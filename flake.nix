{
  description = "pet-report: VLM-driven pet-activity monitor (Haskell backend + Svelte frontend)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, ... }:
        {
          # Backend: the Haskell `pet-report` package (library + executable + tests).
          haskellProjects.default = {
            projectRoot = ./backend;
            devShell = {
              tools = hp: {
                inherit (hp) stylish-haskell cabal-fmt;
                inherit (pkgs) hlint;
              };
            };
          };

          packages.backend = self'.packages.pet-report;
          packages.frontend = import ./nix/frontend.nix {
            inherit pkgs;
            src = ./frontend;
          };
          packages.default = self'.packages.pet-report;

          # A single OCI image running just pet-report: the Haskell `serve` process
          # (API + capture/batch loops) behind nginx, which serves the built SPA and
          # proxies /api and /proof to it. Frigate and the OpenAI-compatible model
          # server are assumed to already run elsewhere and are reached over the
          # network via FRIGATE_URL / LLAMA_SWAP_URL. Build with `nix build .#docker`
          # then `docker load < result`; see the README's Docker section to run it.
          packages.docker =
            let
              backend = self'.packages.pet-report;
              frontend = self'.packages.frontend;
              # nginx runs its workers as root so it can write buffering temp files
              # under /tmp (the created-at-boot dirs below); harmless in a
              # single-purpose container that binds only the high UI port.
              nginxConf = pkgs.writeText "pet-report-nginx.conf" ''
                daemon off;
                user root;
                pid /tmp/nginx.pid;
                error_log /dev/stderr warn;
                events { }
                http {
                  access_log /dev/stdout;
                  include ${pkgs.nginx}/conf/mime.types;
                  default_type application/octet-stream;
                  sendfile on;
                  client_max_body_size 25m;
                  client_body_temp_path /tmp/nginx-body;
                  proxy_temp_path /tmp/nginx-proxy;
                  fastcgi_temp_path /tmp/nginx-fastcgi;
                  uwsgi_temp_path /tmp/nginx-uwsgi;
                  scgi_temp_path /tmp/nginx-scgi;
                  server {
                    listen 8115;
                    root ${frontend};
                    location / {
                      try_files $uri /index.html;
                    }
                    # These proxy timeouts mirror nix/module.nix's nginx vhost;
                    # keep the two in step (/api's 300s covers the ~100s
                    # interactive LLM budget, /proof's 120s a local image read).
                    location /api/ {
                      proxy_pass http://127.0.0.1:8116;
                      proxy_read_timeout 300s;
                      proxy_send_timeout 300s;
                    }
                    location /proof/ {
                      proxy_pass http://127.0.0.1:8116;
                      proxy_read_timeout 120s;
                    }
                  }
                }
              '';
              entrypoint = pkgs.writeShellScript "pet-report-entrypoint" ''
                set -euo pipefail
                mkdir -p /data/queue /data/proof /data/media \
                  /tmp/nginx-body /tmp/nginx-proxy /tmp/nginx-fastcgi \
                  /tmp/nginx-uwsgi /tmp/nginx-scgi
                ${backend}/bin/pet-report serve &
                backend=$!
                # -e /dev/stderr: point the *early* error log at stderr too, so
                # nginx never touches its compiled-in /var/log path (absent here).
                ${pkgs.nginx}/bin/nginx -e /dev/stderr -c ${nginxConf} &
                web=$!
                # This shell is PID 1, so forward `docker stop`'s SIGTERM to both
                # children for a graceful shutdown, rather than leaving them to be
                # SIGKILLed when the stop grace period ends.
                trap 'kill -TERM "$backend" "$web" 2>/dev/null || true' TERM INT
                # If either process exits, stop the container so the orchestrator's
                # restart policy recreates it rather than limping on half-up.
                wait -n
                exit 1
              '';
            in
            pkgs.dockerTools.buildLayeredImage {
              name = "pet-report";
              tag = "latest";
              # ffmpeg (the serve process shells out to it for clip frames), nginx,
              # a shell for the entrypoint, CA certs (https model/ntfy URLs), the tz
              # database (PET_REPORT_TZ day boundaries), and fakeNss so glibc can
              # resolve hostnames and nginx has a root user to run as.
              contents = [
                pkgs.bashInteractive
                pkgs.coreutils
                pkgs.ffmpeg-headless
                pkgs.nginx
                pkgs.cacert
                pkgs.tzdata
                pkgs.dockerTools.fakeNss
              ];
              config = {
                Entrypoint = [ "${entrypoint}" ];
                ExposedPorts = {
                  "8115/tcp" = { };
                };
                Volumes = {
                  "/data" = { };
                };
                WorkingDir = "/data";
                Env = [
                  "PATH=/bin"
                  "PET_REPORT_LISTEN=127.0.0.1:8116"
                  "PET_REPORT_DB=/data/pet-report.db"
                  "PET_REPORT_QUEUE=/data/queue"
                  "PET_REPORT_PROOF_DIR=/data/proof"
                  "PET_REPORT_MEDIA_DIR=/data/media"
                  "TZDIR=${pkgs.tzdata}/share/zoneinfo"
                  "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                ];
              };
            };
        };

      # Consumer-facing outputs: an overlay that builds both packages against the
      # consumer's nixpkgs, and the NixOS module that wires the services + nginx.
      flake = {
        overlays.default = final: _prev: {
          pet-report-backend =
            let
              raw = final.haskell.lib.justStaticExecutables (
                final.haskellPackages.callCabal2nix "pet-report" ./backend { }
              );
            in
            raw.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ final.makeWrapper ];
              postInstall = (old.postInstall or "") + ''
                wrapProgram $out/bin/pet-report \
                  --prefix PATH : ${final.lib.makeBinPath [ final.ffmpeg-headless ]}
              '';
            });

          pet-report-frontend = import ./nix/frontend.nix {
            pkgs = final;
            src = ./frontend;
          };
        };

        nixosModules.pet-report = import ./nix/module.nix;
      };
    };
}
