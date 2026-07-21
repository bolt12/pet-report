-- | The per-pet weekly recap: one short, warm sentence written by the text model and
-- grounded in the pet's observations, cached in @pet_summaries@. The caller computes the
-- deterministic stats and the wellbeing verdict; this module only produces the prose.
--
-- Expressed as a 'Call' whose schema is the codec-derived @{recap}@, with a decoder that
-- tolerates a model answering in plain prose instead.
module PetReport.Analysis.Recap
  ( petWeekly
  ) where

import           Autodocodec        (HasCodec (..), object,
                                     parseJSONViaCodec, requiredField, (.=))
import           Data.Aeson         (Value, decodeStrict)
import           Data.Aeson.Types   (parseMaybe)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)

import           LLM.Call                      (Call (..), DecodeError (..),
                                                Reply (..), Sampling (..), note,
                                                runMaybe, structured, system,
                                                userText)
import           PetReport.Analysis.Vocabulary (petLine)
import           PetReport.Domain.PetReport    (WellbeingKind, wellbeingKindText)
import           PetReport.Domain.Profile      (Pet)
import           PetReport.Effect.Db           (PetSummary (..))
import qualified PetReport.Effect.Llm          as Llm
import           PetReport.Util                (nonBlank, paragraphs)

-- | Ask the model for a warm one-line recap for one pet. @kind@ is the deterministic
-- wellbeing verdict, @stats@ the factual pairs to attach, @body@ the pet's observation
-- lines. A model failure propagates, for the caller to log and skip.
petWeekly :: Llm.Handle -> Pet -> WellbeingKind -> [(Text, Text)] -> Text -> IO PetSummary
petWeekly llm pet kind stats body = do
  recap <- runMaybe llm (recapCall pet kind body)
  pure (PetSummary kind recap stats)

-- | The recap task. 'petWeekly' runs on the serial batch worker with nobody waiting, so it
-- takes the background budget, at a warmer temperature than extraction since this is prose.
-- The schema is the codec-derived @{recap}@, but 'parseRecap' decodes it, so a model that
-- ignores the format and answers in plain prose is kept anyway.
recapCall :: Pet -> WellbeingKind -> Text -> Call Text
recapCall pet kind body =
  Call
    { callMessages = [system (recapInstructions pet kind), userText userBody]
    , callSampling = Sampling {samplingTemperature = 0.7, samplingMaxTokens = 400, samplingThinking = False}
    , callBudget = Llm.backgroundBudget
    , callReply =
        Reply
          { replyFormat = replyFormat (structured @RecapReply "recap")
          , replyDecode = parseRecap . fromMaybe "" . Llm.amContent
          }
    }
  where
    userBody =
      if T.null (T.strip body)
        then "There were very few sightings this week."
        else "This week's observations:\n" <> body

recapInstructions :: Pet -> WellbeingKind -> Text
recapInstructions pet kind =
  paragraphs
    [ "You write ONE short, warm, evocative sentence about a pet's week for its owner, grounded ONLY in the observations given."
    , "The pet is " <> petLine pet
    , "This week overall reads as: " <> wellbeingKindText kind <> " (good = settled and healthy; watch = one gentle thing to keep an eye on)."
    , "Never invent facts. If little was seen, say so gently; cameras are often off."
    ]

-- | The one-field reply the recap call is constrained to. Its 'HasCodec' supplies the
-- @response_format@ schema, while 'parseRecap' does the decoding.
newtype RecapReply = RecapReply Text
  deriving stock (Eq, Show)

instance HasCodec RecapReply where
  codec =
    object "RecapReply" $
      RecapReply <$> requiredField "recap" "one evocative sentence about the pet's week" .= unwrap
    where
      unwrap (RecapReply t) = t

-- | Pull the recap sentence out of the model's reply. A reply that is not JSON at all is
-- kept verbatim, since the model ignored the format and its prose is the recap. A JSON
-- object of the wrong shape is a 'DecodeError' rather than having its raw text surfaced.
parseRecap :: Text -> Either DecodeError Text
parseRecap raw = case decodeStrict (encodeUtf8 raw) :: Maybe Value of
  Nothing -> note "the recap was empty" (nonBlank raw)
  Just v -> case parseMaybe parseJSONViaCodec v of
    Just (RecapReply r) -> note "the recap was empty" (nonBlank r)
    Nothing             -> Left (DecodeError "the recap JSON was not of the expected shape")
