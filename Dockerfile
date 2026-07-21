# syntax=docker/dockerfile:1

# A single image running just pet-report: the `serve` process (the API plus the
# capture and batch loops) behind nginx, which serves the built SPA and proxies
# /api and /proof to it. Frigate and the OpenAI-compatible model server are not
# bundled; they run elsewhere and are reached over the network via FRIGATE_URL
# and LLAMA_SWAP_URL.
#
# This is the Nix-free route to what `nix build .#docker` produces from
# flake.nix. The two build the same container by different means, so a change to
# one belongs in the other.

# --------------------------------------------------------------------------- #
# Frontend: the Svelte SPA, built to a static dist/.
# --------------------------------------------------------------------------- #
FROM node:22-bookworm-slim AS frontend

WORKDIR /src

# Dependencies first, so editing a component does not reinstall node_modules.
COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci

COPY frontend/ ./
RUN npm run build

# --------------------------------------------------------------------------- #
# Backend: the Haskell `pet-report` executable. The GHC here matches the dev
# shell's, so the image compiles what `cabal build` compiles.
# --------------------------------------------------------------------------- #
FROM haskell:9.10.3-slim-bookworm AS backend

# Pin Hackage so this build is reproducible. Without it the dependency set is
# whatever Hackage looks like on the day you build, and since the project is
# -Werror, a new warning in a bumped dependency becomes a build failure that is
# hard to diagnose from the outside. Bump this deliberately.
ARG HACKAGE_INDEX_STATE=2026-07-29T00:00:00Z

WORKDIR /src

# The dependency layer is the expensive one, so resolve and build it from the
# cabal files alone. A source edit then reuses it instead of recompiling the
# whole package set.
COPY backend/pet-report.cabal backend/cabal.project ./
RUN cabal update "hackage.haskell.org,${HACKAGE_INDEX_STATE}" \
 && cabal build --only-dependencies --disable-tests --index-state="${HACKAGE_INDEX_STATE}"

COPY backend/ ./
RUN cabal install exe:pet-report \
      --index-state="${HACKAGE_INDEX_STATE}" \
      --installdir=/out --install-method=copy

# --------------------------------------------------------------------------- #
# Runtime.
# --------------------------------------------------------------------------- #
FROM debian:bookworm-slim

# ffmpeg brings ffmpeg and ffprobe, which the serve process shells out to by
# name for clip frames; without them analysis degrades to no frames. Then CA
# certificates for https model and ntfy URLs, tzdata for PET_REPORT_TZ day
# boundaries, and the four libraries a GHC-linked binary loads at startup.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      nginx \
      ffmpeg \
      ca-certificates \
      tzdata \
      libgmp10 \
      libffi8 \
      zlib1g \
      libnuma1 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=backend /out/pet-report /usr/local/bin/pet-report
COPY --from=frontend /src/dist /srv/www
COPY docker/nginx.conf /etc/nginx/nginx.conf
COPY docker/entrypoint.sh /usr/local/bin/pet-report-entrypoint
RUN chmod +x /usr/local/bin/pet-report-entrypoint

# Everything the app owns lives under /data, so the container itself is
# disposable and that one volume is what you back up.
ENV PET_REPORT_LISTEN=127.0.0.1:8116 \
    PET_REPORT_DB=/data/pet-report.db \
    PET_REPORT_QUEUE=/data/queue \
    PET_REPORT_PROOF_DIR=/data/proof \
    PET_REPORT_MEDIA_DIR=/data/media

EXPOSE 8115
VOLUME /data
WORKDIR /data

ENTRYPOINT ["/usr/local/bin/pet-report-entrypoint"]
