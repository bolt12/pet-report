# NixOS module for pet-report. Expects `overlays.default` to be applied so
# `pkgs.pet-report-backend` and `pkgs.pet-report-frontend` exist.
{ config, lib, pkgs, ... }:
let
  cfg = config.services.pet-report;
  bin = "${pkgs.pet-report-backend}/bin/pet-report";

  env = {
    PET_REPORT_DB = "/var/lib/pet-report/pet-report.db";
    PET_REPORT_QUEUE = "/var/lib/pet-report/queue";
    PET_REPORT_PROOF_DIR = "/var/lib/pet-report/proof";
    PET_REPORT_MEDIA_DIR = "/var/lib/pet-report/media";
    PET_REPORT_LISTEN = "127.0.0.1:${toString cfg.backendPort}";
    FRIGATE_URL = cfg.frigateUrl;
    LLAMA_SWAP_URL = cfg.llamaUrl;
    NTFY_URL = cfg.ntfyUrl;
    VISION_MODEL = cfg.visionModel;
    PET_REPORT_TZ = cfg.timeZone;
    PET_CAMERAS = lib.concatStringsSep "," cfg.cameras;
    PET_LABELS = lib.concatStringsSep "," cfg.petLabels;
    PET_REPORT_CAPTURE_SECS = toString cfg.captureSecs;
    PET_REPORT_BATCH_HOURS = lib.concatMapStringsSep "," toString cfg.batchHours;
    PET_REPORT_LOG_LEVEL = cfg.logLevel;
  } // lib.optionalAttrs (cfg.audioLabels != null) {
    PET_AUDIO_LABELS = lib.concatStringsSep "," cfg.audioLabels;
  } // lib.optionalAttrs (cfg.publicUrl != null) {
    PET_REPORT_PUBLIC_URL = cfg.publicUrl;
  };

  baseService = {
    after = [ "network-online.target" ]
      ++ lib.optional (cfg.frigateService != null) cfg.frigateService;
    wants = [ "network-online.target" ]
      ++ lib.optional (cfg.frigateService != null) cfg.frigateService;
    environment = env;
  };
  # systemd sandboxing shared by the service(s). The process needs only outbound
  # network (Frigate, the model, ntfy), its StateDirectory under /var/lib, a
  # private /tmp for ffmpeg's scratch frames, and permission to exec ffmpeg;
  # everything below denies the rest. AF_NETLINK stays allowed so glibc's resolver
  # can enumerate interfaces (dropping it breaks DNS to a hostname Frigate/model URL).
  baseSC = {
    User = "pet-report";
    Group = "pet-report";
    StateDirectory = "pet-report";
    StateDirectoryMode = "0700";

    # Filesystem
    ProtectSystem = "strict";
    ProtectHome = true;
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectProc = "invisible";
    ProcSubset = "pid";
    UMask = "0077";

    # Kernel and process
    NoNewPrivileges = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectKernelLogs = true;
    ProtectClock = true;
    ProtectControlGroups = true;
    ProtectHostname = true;
    RestrictNamespaces = true;
    RestrictRealtime = true;
    RestrictSUIDSGID = true;
    LockPersonality = true;
    # GHC-compiled binaries and ffmpeg do not JIT, so W^X is safe here.
    MemoryDenyWriteExecute = true;

    # Capabilities: it binds only a high port, so it needs none.
    CapabilityBoundingSet = "";
    AmbientCapabilities = "";

    # Syscalls and address families
    SystemCallArchitectures = "native";
    SystemCallFilter = [
      "@system-service"
      "~@privileged"
      "~@resources"
    ];
    RestrictAddressFamilies = [
      "AF_INET"
      "AF_INET6"
      "AF_UNIX"
      "AF_NETLINK"
    ];
  };
in
{
  options.services.pet-report = {
    enable = lib.mkEnableOption "pet-report VLM pet-activity monitor";
    port = lib.mkOption {
      type = lib.types.port;
      default = 8115;
      description = "Public nginx port for the web UI.";
    };
    backendPort = lib.mkOption {
      type = lib.types.port;
      default = 8116;
      description = "Internal port the Haskell API listens on.";
    };
    cameras = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Frigate camera names to monitor.";
    };
    petLabels = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "dog" "cat" ];
      description = "Frigate object labels ingested as pet sightings.";
    };
    captureSecs = lib.mkOption {
      type = lib.types.ints.positive;
      default = 600;
      description = "Seconds between capture passes (each queues a frame per online camera).";
    };
    batchHours = lib.mkOption {
      type = lib.types.listOf (lib.types.ints.between 0 23);
      default = [ 8 20 ];
      description = "Local hours at which the batch (analysis, report, cleanup) runs.";
    };
    logLevel = lib.mkOption {
      type = lib.types.enum [ "debug" "info" "warn" "error" ];
      default = "info";
      description = "Minimum severity written to the journal. \"debug\" adds a line per HTTP request, every model round-trip with its latency, and routine pipeline detail.";
    };
    frigateUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://localhost:8114";
      description = "Base URL of the Frigate NVR. Cameras are auto-discovered from it. The in-app profile overrides this.";
    };
    llamaUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://localhost:8080";
      description = "Base URL of the OpenAI-compatible vision model endpoint. The in-app profile overrides this.";
    };
    ntfyUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://localhost:8106/pet-report";
      description = "ntfy topic URL for the morning and evening push notifications.";
    };
    visionModel = lib.mkOption {
      type = lib.types.str;
      default = "vision-model";
      description = "Model name to request from the endpoint above, exactly as that server names it. The default is a placeholder to replace, not a working value: set it to your own model's name (or leave it and set the name in the app's setup page, which overrides this).";
    };
    timeZone = lib.mkOption {
      type = lib.types.str;
      default = "UTC";
      description = "IANA zone for day boundaries, the morning/evening split, and reminders. The in-app profile overrides this.";
    };
    publicUrl = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Public URL of the web UI, used as the tap-through target for push notifications. Null disables the notification link.";
    };
    frigateService = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "frigate.service";
      description = "If Frigate runs as a systemd unit on this host, its unit name, so the serve unit (which now also captures and batches) orders after it. Null (the default) suits a remote or containerized Frigate.";
    };
    audioLabels = lib.mkOption {
      type = lib.types.nullOr (lib.types.listOf lib.types.str);
      default = null;
      description = "Frigate audio labels to ingest as sound events; null uses the built-in default set.";
    };
    basicAuthFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Optional htpasswd file to require HTTP basic auth for the web UI. Null
        leaves it open (the LAN-only default). Create with:
        `nix run nixpkgs#apacheHttpd -- htpasswd -c /path/file user`.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.pet-report = {
      isSystemUser = true;
      group = "pet-report";
      home = "/var/lib/pet-report";
    };
    users.groups.pet-report = { };

    systemd.services.pet-report-serve = baseService // {
      description = "pet-report web API";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = baseSC // {
        ExecStart = "${bin} serve";
        Restart = "always";
        RestartSec = "5";
      };
    };

    # Frame capture and the twice-daily batch (analysis, report, cleanup) run as
    # background loops inside the serve process, not external timers, so all of
    # Pet Report's automation lives in one place. Tune the cadence with the
    # `captureSecs` and `batchHours` options above.

    # The UI is only reachable through nginx, so the module owns enabling it
    # rather than assuming the host already has it. Setting this is idempotent
    # where something else (e.g. the nextcloud module) already turned it on.
    services.nginx.enable = true;

    services.nginx.virtualHosts."pet-report" = {
      listen = [
        {
          addr = "0.0.0.0";
          port = cfg.port;
        }
      ];
      # Optional HTTP basic auth (null = open, the LAN-only default).
      basicAuthFile = cfg.basicAuthFile;
      # A pet photo posts to /api as base64, so lift nginx's 1m body default.
      # Kept in step with the Docker image's nginx (flake.nix packages.docker).
      extraConfig = "client_max_body_size 25m;";
      locations."/" = {
        root = "${pkgs.pet-report-frontend}";
        tryFiles = "$uri /index.html";
      };
      locations."/api/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.backendPort}";
        extraConfig = ''
          proxy_read_timeout 300s;
          proxy_send_timeout 300s;
        '';
      };
      # Media (event snapshots/clips and proof frames) is served by the backend
      # under /api and /proof; no direct Frigate proxy is needed. Give /proof/ an
      # explicit read timeout instead of inheriting nginx's 60s default: a local
      # image read never needs long, but 120s is a consistent, generous safety net
      # (the /api/ 300s comfortably exceeds the 100s server-side interactive budget).
      locations."/proof/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.backendPort}";
        extraConfig = ''
          proxy_read_timeout 120s;
        '';
      };
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];
  };
}
