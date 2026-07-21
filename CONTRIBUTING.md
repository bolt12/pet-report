# Contributing

Thanks for helping improve pet-report.

## Setup

- **Backend** (`backend/`): `nix develop` provides ghc, cabal, HLS, and the
  formatters. Build with `cabal build all`; test with
  `cabal test all --test-show-details=direct`.
- **Frontend** (`frontend/`): `npm install`, then `npm run dev` (dev server),
  `npm run check` (svelte-check), or `npm run build`.

## Before opening a PR

- Backend: `cabal build all` and `cabal test all` pass, and `nix flake check` is
  green. The code is GHC2021 with `-Werror`; format with `stylish-haskell` and
  `cabal-fmt` (both in the dev shell).
- `hlint backend/src backend/llm backend/app backend/test` reports no hints. CI
  gates on this. If a hint is wrong for the code in question, add a scoped
  `ignore` to `.hlint.yaml` with a comment saying why, rather than reformatting
  code that reads better as it is.
- Frontend: `npm run check` and `npm run build` pass.
- Keep changes proportional to the problem, and prefer property or golden tests
  for domain logic.

## License

By contributing you agree that your work is licensed under the GNU
AGPL-3.0-or-later, the same license as the rest of the project.
