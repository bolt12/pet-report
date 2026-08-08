# Changelog

Notable changes to pet-report, newest first.

## Unreleased

### Breaking changes

- Ingest resumes from Frigate's retention horizon on the first batch after
  upgrading. Anything older is never picked up, its clips having expired already.

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
