# Repository Guidelines

## Project Structure & Module Organization

This repository is a chezmoi source tree for personal workstation configuration.
Files prefixed with `dot_` or `private_` map to files under `$HOME`; `.tmpl`
files are rendered from `.chezmoi.yaml.tmpl`. Platform hooks live in
`.chezmoiscripts/` (including Darwin setup scripts), shared documentation is
under `doc/`, and `install.sh` bootstraps chezmoi and applies the source.
Configuration for fish, Ghostty, Zed, Git, mise, and related tools is under
`dot_config/`.

## Build, Test, and Development Commands

There is no compiled build or dedicated test suite. Use these commands when
changing configuration:

- `./install.sh` — install chezmoi if needed and apply this source tree.
- `chezmoi diff --source .` — inspect the rendered changes before applying.
- `chezmoi apply --source .` — render and apply the current configuration.
- `chezmoi apply --source . --dry-run` — preview application without changing
  the home directory.

Test platform-specific changes on the target OS; scripts under
`.chezmoiscripts/darwin/` should not be assumed to run on Linux.

## Coding Style & Naming Conventions

Follow `.editorconfig`. Keep shell scripts POSIX-compatible unless a file is
explicitly Fish (`.fish`). Use two-tab indentation in shell, lowercase
hyphenated names for chezmoi-managed files, and descriptive `run_onchange_`
script names. Keep machine-specific values in templates or data rather than
duplicating configuration.

## Testing Guidelines

For configuration changes, review `chezmoi diff`, run a dry-run apply, then
apply on the relevant platform and verify the resulting file or command.
Check shell syntax with `sh -n path/to/script.sh` where applicable.

## Commit & Pull Request Guidelines

Recent commits use concise, lowercase, imperative-style summaries, often with
an optional scope such as `darwin:` or `linux(fedora):`. Follow that pattern.
Pull requests should describe the affected platform and files, explain user-
visible effects, include validation commands and results, and call out any
required secrets, external downloads, or manual setup. Do not commit generated
secrets or private machine state.
