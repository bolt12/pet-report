-- | The two vision tasks: analyse camera frames into a 'Scene', and draft a pet's physical
-- description from one photo. Each is a 'Call', bundling prompt, schema, sampling, budget
-- and decoder, run through 'run'. For a structured reply the schema and decoder both come
-- from the output type's codec.
module PetReport.Analysis.Vision
  ( analyze
  , describePet
  ) where

import           Autodocodec            (HasCodec (..), object, requiredField, (.=))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE

import           LLM.Call                    (Call (..), Sampling (..), mapReply,
                                              note, refine, runMaybe, structured,
                                              userParts)
import           PetReport.Domain.Perception (Scene, normalizeScene)
import           PetReport.Domain.Types      (Species, speciesText)
import qualified PetReport.Effect.Llm        as Llm
import           PetReport.Util              (nonBlank, paragraphs)

-- | Analyse one or more JPEG frames into a 'Scene', or 'Nothing' if the model returns no
-- usable JSON. @brief@ is the identification guide steering the prompt.
analyze :: Llm.Handle -> Text -> [ByteString] -> IO (Maybe Scene)
analyze llm brief frames = runMaybe llm (sceneCall brief frames)

-- | The vision-analysis task. 'analyze' runs on the serial batch worker draining the queue
-- and events, so it takes the background budget: a generous cap plus one retry to ride out
-- a transient blip, all under the batch wall-clock. Sampling is near-deterministic, since
-- this is structured extraction rather than prose.
sceneCall :: Text -> [ByteString] -> Call Scene
sceneCall brief frames =
  Call
    { callMessages = [userParts (Llm.TextPart prompt : map imagePart frames)]
    , callSampling = Sampling {samplingTemperature = 0.15, samplingMaxTokens = 700, samplingThinking = False}
    , callBudget = Llm.backgroundBudget
    , callReply = mapReply normalizeScene (structured @Scene "scene")
    }
  where
    prompt = visionInstructions brief <> clipNote
    clipNote
      | length frames > 1 = "\nThese frames are consecutive moments from one short clip; base your single answer on the action across them."
      | otherwise = ""

-- | The per-frame vision instruction. @brief@ is the identification guide: either the
-- curated roster brief or the raw 'PetReport.Analysis.Vocabulary.petBrief' fallback. The
-- field-level semantics and the few-shot examples live here, beside the 'Scene' schema they
-- illustrate, so the two stay in step.
visionInstructions :: Text -> Text
visionInstructions brief =
  paragraphs
    [ "You are a home pet-monitoring assistant analysing one or more camera frames."
    , brief
    , "Report ONLY what is actually visible; never guess. Produce one entry in \"appearances\" per visible animal or person."
    , "Identify a household pet only when the visible traits make it unambiguous. Under night vision or infrared the image is black and white, so DO NOT rely on colour: identify by size, build, shape, ears, tail, gait, and the guide above. If two or more pets could match, describe it by species and lower the confidence."
    , "If a pet's note explains a normal condition (a missing limb, a healed scar, a permanent squint or tremor), treat it as normal for that pet: never report it as an injury, a limp, or a concern; you may use it to identify the pet."
    , "If no animal or person is visible, use an empty \"appearances\" list."
    , "If the frame is dark, blurred, in night/IR mode, or shows only part of an animal, lower the confidence and prefer the activity \"unclear\" over a confident guess."
    , "Always fill \"confidence\": how sure you are of both the identification and the activity, from 0.0 to 1.0. Never omit it and never leave it null. Use the whole range, not just a few high values: a clear daylight view of a distinctive pet belongs near 0.95, a dim or partial view near 0.3."
    , "Keep the behaviour flags consistent with the activity. A crouching or still animal is NOT necessarily toileting; report elimination only when clearly visible, and set its place: litter_box, outdoors, or inappropriate (an indoor accident). Set \"ate\"/\"drank\" only when the animal is visibly consuming food or water, not merely near a bowl."
    , "Fill \"description\" with one neutral, specific sentence for the whole scene (never blank or null); vary the wording and do not begin with \"The image shows\"."
    , "Examples of the expected JSON:"
    , "- Empty room: {\"appearances\": [], \"description\": \"The room is empty.\", \"wellbeing\": \"normal\", \"confidence\": 0.95}"
    , "- A cat asleep: {\"appearances\": [{\"who\": \"cat\", \"activity\": \"sleeping\", \"behaviors\": {\"ate\": false, \"drank\": false, \"slept\": true, \"played\": false, \"groomed\": false, \"concerns\": []}, \"where\": \"on the sofa\"}], \"description\": \"A cat is asleep on the sofa.\", \"wellbeing\": \"normal\", \"confidence\": 0.9}"
    , "- Two animals in night vision, unsure which pets: {\"appearances\": [{\"who\": \"dog\", \"activity\": \"walking\", \"behaviors\": {\"ate\": false, \"drank\": false, \"slept\": false, \"played\": false, \"groomed\": false, \"concerns\": []}}, {\"who\": \"cat\", \"activity\": \"standing\", \"behaviors\": {\"ate\": false, \"drank\": false, \"slept\": false, \"played\": false, \"groomed\": false, \"concerns\": []}}], \"description\": \"A dog and a cat move through a dim, infrared-lit room.\", \"wellbeing\": \"normal\", \"confidence\": 0.4}"
    ]

-- | Draft an identification-grade physical description of one pet from a single
-- owner-supplied JPEG, folding in their current draft (@mExisting@) when enhancing. Runs on
-- the interactive budget, since the owner is waiting. 'Nothing' when the model returns no
-- usable text.
describePet :: Llm.Handle -> Species -> Maybe Text -> ByteString -> IO (Maybe Text)
describePet llm sp mExisting jpg = runMaybe llm (describeCall sp mExisting jpg)

-- | The pet-describe task: one photo plus the describe instruction, on the interactive
-- budget. Low temperature, since this is short prose rather than a wide extraction. The
-- one-field 'PetDescription' reply is projected to its text, or 'Nothing' when blank.
describeCall :: Species -> Maybe Text -> ByteString -> Call Text
describeCall sp mExisting jpg =
  Call
    { callMessages = [userParts [Llm.TextPart (describeInstructions (speciesText sp) mExisting), imagePart jpg]]
    , callSampling = Sampling {samplingTemperature = 0.2, samplingMaxTokens = 400, samplingThinking = False}
    , callBudget = Llm.interactiveBudget
    , callReply = refine (\(PetDescription d) -> note "the model returned an empty description" (nonBlank d)) (structured @PetDescription "pet_description")
    }

-- | The describe instruction. What it produces goes verbatim into 'petLine' and the vision
-- identification brief, which runs on black-and-white infrared footage as well as daylight
-- colour, so the text has to identify the pet under both: structural cues that survive
-- greyscale first, then colour. @sp@ is the declared species, a hint rather than gospel, and
-- @mExisting@ is the owner's current draft to enhance if there is one.
describeInstructions :: Text -> Maybe Text -> Text
describeInstructions sp mExisting =
  paragraphs
    [ "You are helping a pet owner write a physical description of ONE pet from a single photo."
    , "This description is used by home cameras to tell this pet apart, including at night when the picture is black-and-white infrared with no colour. So lead with traits that still read in greyscale, then add colour."
    , "The owner says this is a " <> sp <> " (treat that as a hint; describe what you actually see)."
    , "Write ONE or TWO plain sentences describing only this animal. Cover, in this order: species and rough size or build; coat length and texture; ear shape and set; tail length and shape; muzzle or face shape and any distinctive proportions; THEN colour and any high-contrast markings (a collar, a bib, socks, patches, a blaze) that also show up in greyscale as light-vs-dark."
    , "Describe ONLY what is visible in the photo; never invent breed, colour, or markings you cannot see. Do not mention the background, the pose, the camera, or the photo itself, and do not begin with \"The image shows\" or \"This is\"."
    , enhanceNote mExisting
    ]
  where
    enhanceNote m = case m >>= nonBlank of
      Nothing -> ""
      Just prev ->
        "The owner already wrote: \""
          <> prev
          <> "\". Keep any correct detail they mention that is hard to see in a photo (a collar colour, a healed scar, a normal missing limb), fix anything the photo plainly contradicts, and fold it all into one improved, complete description."

-- | The one-field reply the describe call is constrained to. Its 'HasCodec' supplies both
-- the @response_format@ schema and the decoder.
newtype PetDescription = PetDescription Text
  deriving stock (Eq, Show)

instance HasCodec PetDescription where
  codec =
    object "PetDescription" $
      PetDescription <$> requiredField "description" "the one or two sentence physical description" .= unwrap
    where
      unwrap (PetDescription t) = t

-- | One frame as an OpenAI image part: a @data:image/jpeg;base64,...@ URL.
imagePart :: ByteString -> Llm.ContentPart
imagePart jpg = Llm.ImagePart ("data:image/jpeg;base64," <> TE.decodeUtf8 (B64.encode jpg))
