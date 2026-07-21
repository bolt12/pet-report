#!/usr/bin/env bash
# PID 1 for the Docker image. Mirrors the Nix image's entrypoint in flake.nix;
# keep the two in step.
#
# bash rather than sh: `wait -n` is a bashism, and Debian's /bin/sh is dash.
set -euo pipefail

mkdir -p /data/queue /data/proof /data/media \
  /tmp/nginx-body /tmp/nginx-proxy /tmp/nginx-fastcgi \
  /tmp/nginx-uwsgi /tmp/nginx-scgi

pet-report serve &
backend=$!

# -e /dev/stderr: point the *early* error log at stderr too, so nginx never
# touches its compiled-in /var/log path before reading the config.
nginx -e /dev/stderr -c /etc/nginx/nginx.conf &
web=$!

# This shell is PID 1, so forward `docker stop`'s SIGTERM to both children for a
# graceful shutdown, rather than leaving them to be SIGKILLed when the stop
# grace period ends.
trap 'kill -TERM "$backend" "$web" 2>/dev/null || true' TERM INT

# If either process exits, stop the container so the orchestrator's restart
# policy recreates it rather than limping on half-up.
wait -n
exit 1
