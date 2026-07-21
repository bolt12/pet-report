# The Svelte SPA, built to a static directory with buildNpmPackage.
{ pkgs, src }:
pkgs.buildNpmPackage {
  pname = "pet-report-frontend";
  version = "0.1.0";
  inherit src;
  npmDepsHash = "sha256-ffY6FQIsmTDCtx3RB9hUuli2WurHOnt4fQMMX6b1egs=";

  # `npm run build` (vite) produces ./dist; install it as the package output.
  installPhase = ''
    runHook preInstall
    cp -r dist $out
    runHook postInstall
  '';
}
