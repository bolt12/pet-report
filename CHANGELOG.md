# Changelog

Notable changes to pet-report. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

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
