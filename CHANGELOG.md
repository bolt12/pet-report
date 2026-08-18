# Changelog

Notable changes to pet-report, newest first.

## Unreleased

### Breaking changes

- Ingest resumes from Frigate's retention horizon on the first batch after
  upgrading. Anything older is never picked up, its clips having expired already.
- Corrections and edits address one sighting: `POST /api/moments/{id}/correction`
  and `.../edit` become `POST /api/moments/{id}/sightings/{ix}/correction` and
  `.../edit`. The correction body is tagged (`{ kind: "pet", petId }` and so on)
  instead of four optional fields settled by precedence.
- The moments browse takes `subject` in place of `pet`, accepting `pet:{id}`,
  `species:{name}`, `person` and `visiting`. It is repeatable and AND-ed. New
  `behaviour`, `wellbeing` and `camera` facets; `activity`, `media`, `timeOfDay`,
  `review` and `sort` now reject an unrecognised value with a 400 naming the legal
  set instead of dropping the clause. A `pet` id absent from the roster is
  likewise a 400.
- `Spot` (a pet's favourite place) gains `cameras`; `SubjectRef` gains `ix` and
  `person`.
- New `POST /api/moments/{id}/sightings` and
  `DELETE /api/moments/{id}/sightings/{ix}` for a subject the model missed or
  invented.
- "Visitor" meant two different things and no longer exists as a word. A human is
  a `person`; an animal that is not yours is `visiting`. The wire values, the card
  labels and the status pills all follow that split, so a card saying "A person"
  and a pill saying "not my pet" can no longer be confused for each other.

### New features

- Frigate `person` events are ingested, so a sitter or a visitor shows up in the
  day. A person never counts towards a pet's meals or rest. Set
  `PET_REPORT_PERSON_LABELS` empty to turn it off.
- A "still catching up" notice on a day whose events have not all been looked at.
  Tap it to work through them; a busy day can take more than one go.
- The check-in interval is settable in Settings, with no restart.

### Minor changes

- An opened moment shows its date beside the time, so a keepsake or a moment
  reached from search says which day it belongs to.
- A batch is bounded by time, not by a count of jobs. Ingest gets the first 70% of
  the window, frame analysis the rest. A batch that is catching up now runs the
  full twenty minutes.
- Check-in frames are only taken for a camera Frigate has been quiet about for an
  hour. Expect far fewer near-identical moments overnight.
- A flagged pet says why ("no meals seen today", "a concern this week") instead of
  "needs a peek".
- Ingest logging: what each pass fetched, and why any event was dropped.

### Bug fixes

- A moment can hold a pet AND a person. The model misses subjects, and until now the
  only thing an owner could do was retarget what it did report, so a frame read as
  one cat could be called a cat or a person and never both. Subjects can now be
  added and removed, and each is named on its own.
- The Moments "Who" filter takes more than one subject at a time, so "Mochi and a
  person" is a question you can ask. Several subjects AND together.
- Naming one animal in a frame that holds several no longer renames the rest.
  Identity has always been stored per sighting, but the writer took only a moment
  id and fanned one identity across every animal in it, so correcting the cat in a
  cat-and-dog photo relabelled the dog too. Corrections and edits now say which
  sighting they mean, and the lightbox asks when there is more than one.
- "Someone was home today" opens the moments it refers to. It used to navigate to
  a pet filter carrying the literal id `visitor`, which no roster holds, so the
  server answered with an empty page. There is now a real subject facet for a
  person, which the stored `is_person` flag could always have served.
- A pet's stat tiles open the moments they counted. Every tile counts a behaviour
  flag but linked on the closest activity word, which is a different set: "Meals"
  counts `ate` and opened `activity=eating`, "Rest" counts sleeping, resting and
  sitting and opened `activity=resting`. Behaviour is now a facet of its own.
- The "N to check" badge opens exactly the moments it counted, through a wellbeing
  facet, rather than the wider needs-a-look backlog.
- A favourite spot opens its moments even when its room label is a fallback. The
  label can be a title-cased camera id when a camera is missing from the profile
  or its room was left blank, and no saved room ever matches that, so the link
  landed on nothing. Spots now carry the cameras they counted.
- A person's behaviour no longer shows on a pet's card. The model fills a
  behaviour record for every sighting it reports, and the card folded all of them
  together, so a sitter eating lunch put an "ate" pill on the pet beside them.
- The day view names a lone pet rather than its species. Its stats come from the
  projection, which is species-level until the owner corrects something, and it
  was read without the roster resolution every other screen applies.
- A shutdown no longer has to wait out the pipeline. Six exception handlers in the
  batch and scheduler loops caught everything, cancellation included, so a
  cancelled step carried on. They now rethrow async exceptions, which is what the
  codebase's own `catchSync` helper was already for elsewhere.
- A moment flagged concerning now enters the needs-a-look queue, and the "N to check" badge
  opens it. The queue had only ever held moments the model was unsure about, which is a
  different thing: a safety sound carries no confidence to be unsure about, and a scene can
  be worrying and unambiguous at once. So the badge counted moments no filter could retrieve.
  Tapping it did nothing, the card below it read "All caught up", and the moment itself
  turned up only if you scrolled the whole day. Reviewing one now clears it from the count.
  Existing databases are backfilled on upgrade.
- The vision model is asked for a confidence on every scene, and told to use the whole range
  rather than a handful of high values. It was optional and usually left out, and a missing
  confidence counted as a confident one, so the review threshold never fired.
- An empty room keeps the confidence the model gave it. The stored value came from the
  per-animal projection, which has no row when nothing is visible, so it was recorded as
  unknown while the moment itself showed a figure.
- The "still catching up" notice on today no longer shows all day long. It compared the
  ingest watermark against the clock, but the watermark only reaches the present for an
  instant when a batch runs, so today read as behind for the hours between batches. It now
  reflects whether the last ingest left a backlog, so the normal wait for the next batch is
  not mistaken for falling behind.
- The Today "moments to review" badge counts only that day now, like every other day. It
  showed the global needs-a-look backlog, so a single old uncertain moment made today read
  as having one to review, and tapping opened that months-old moment. The whole backlog is
  still reachable in Review by pairing the needs-look filter with a date range.
- Frigate events were never ingested. Check-in frames took the whole batch budget
  and froze the event watermark. Every install was affected.
- Queued frames are analysed round-robin across cameras. The last camera used to
  be starved by whichever camera sorted first.
- A day's story is rewritten when its moments arrive late. Only its timeline and
  stats used to heal.
- Ingest keeps going until the work runs out, not after 500 events. Events that
  cost nothing (already stored, deduped, false positives) advanced the cursor for
  free but still counted against the page, so a backlog with no work left in it
  took one batch per 500 to walk past, twelve hours apart.
- Rebuilding a past day now finishes it: it works through the whole day, uses the
  full window rather than the 70% reserved for a frame queue it never touches,
  and clears the catch-up notice when done.
- The refresh spinner gave up after three minutes, less than a batch may take, so
  it cleared mid-run and reported the previous run's outcome.
- The wellbeing line no longer claims a moment is flagged when the flag came from
  a missed meal.

## 0.1.0 - 2026-07-29

First public release.

### Added

- Twice-daily analysis. A batch works through every Frigate event since the last
  run, asks a local vision model what is happening in each, and stores the answer
  as structured per-pet observations. Between events, a capture loop queues a
  frame per online camera so quiet stretches are not blank.
- A written daily report with per-pet stats, habits, and a wellbeing read, paged
  back day by day.
- A browsable timeline of every moment with the still or clip behind it,
  filterable by pet, room, activity, media kind, time of day, and review state.
- Owner corrections. Any reading can be corrected, and the correction sits next to
  the model's original, so nothing is overwritten and you can revert it. Stats and
  the written summary are rebuilt from the corrected record.
- Questions in plain language, answered from stored observations with the moments
  used as proof, and an explicit "cannot tell" when the record does not support an
  answer.
- Keepsakes. Keeping a moment copies its still and clip into pet-report's own
  storage, so it outlives Frigate's retention.
- Guided setup on first visit: the pet roster, how to tell each pet apart
  (optionally drafted by the model from a photo), cameras auto-discovered from
  Frigate, and connection tests.
- Sound events and speech. Frigate audio labels are ingested as sound events.
  Recordings from Frigate 0.17+ can be transcribed on demand and cached.
- Push notifications via ntfy after each batch, tapping through to the UI.
- Storage lifecycle. Summaries and stats are permanent. Un-kept stills and clips
  go when the keep-window runs out, or when Frigate drops that camera's recording,
  whichever happens first.
- Deployment: a NixOS module (`nixosModules.pet-report`) that runs the service
  behind nginx with systemd sandboxing, a Docker image buildable either from the
  `Dockerfile` (Docker alone) or with Nix (`nix build .#docker`), and a manual
  path for everything else.

### Notes

- Identification is species-level. With one pet of a species, sightings are
  labelled automatically. With two, you assign the individual by hand. There is no
  visual re-identification, and a correction applies only to the sighting it is
  made on.
- Stats are sighting-based counts and proportions, never inferred durations. A
  zero means "not seen", not "did not happen".
- There is no login and no per-request authentication. The app trusts its network.
  Run it on a LAN or behind a VPN.
- The database starts at schema version 1. The versioned migration runner is in
  place for future evolution.
