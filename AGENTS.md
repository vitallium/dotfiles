# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a [chezmoi](https://chezmoi.io) dotfiles repository that manages system configuration across macOS and Linux (Fedora). The repository uses chezmoi's templating system to handle platform-specific and work/personal configuration differences.

## Core Tools & Stack

- **Shell**: fish (with fisher plugin manager)
- **Terminal**: Ghostty
- **Editors**: Zed (primary), Helix
- **Version Control**: git with git-delta, jj (Jujutsu), lazygit
- **Package Management**: mise (version management for runtimes), Homebrew (macOS), dnf (Fedora)
- **Languages**: Ruby (primary), Go, Python, Node.js, Rust

## Chezmoi Commands

### Applying Changes

```fish
chezmoi apply     # Apply all changes to target system
chezmoi apply -v  # Apply with verbose output
```

### Testing Templates

```fish
chezmoi execute-template < dot_config/file.tmpl  # Test template rendering
```

### Editing Files

```fish
chezmoi edit ~/.config/fish/config.fish  # Edit the source file in chezmoi
chezmoi edit --apply ~/.bashrc           # Edit and immediately apply
```

### Managing State

```fish
chezmoi diff           # Show differences between source and target
chezmoi status         # Show files that would be changed
chezmoi update         # Pull latest changes and apply
chezmoi re-add ~/.file # Re-add a file after manual edits
```

## Repository Structure

### Chezmoi Naming Conventions

Files and directories use special prefixes that control how chezmoi processes them:

- `dot_` → `.` (e.g., `dot_config/` → `~/.config/`)
- `private_` → Creates file with restricted permissions (0600)
- `.tmpl` suffix → Template file (processed with Go templates)
- `run_onchange_*` → Script that runs when its content hash changes
- `run_once_*` → Script that runs only once

### Key Directories

- `dot_config/` → User configuration files (`~/.config/`)
- `.chezmoiscripts/` → Automation scripts
  - `darwin/` → macOS-specific setup scripts
  - `run_onchange_after_*` → Scripts that run after apply when changed
- `doc/` → Documentation (e.g., Fedora setup guide)

### Configuration Profiles

The repository supports two configuration profiles controlled by the `.workconf` template variable:

- **Personal** (`workconf: false`): Includes personal apps (Discord, Steam, etc.)
- **Work** (`workconf: true`): Includes GitLab-specific tools and fonts

Profile is set during initial `chezmoi init` via the `.chezmoi.yaml.tmpl` prompt.

## Development Tools Setup

### mise Configuration

mise is configured in `dot_config/mise/config.toml` and manages:

- Runtime versions (Ruby, Node, Python, Go, Rust)
- CLI tools via multiple backends:
  - **cargo**: ast-grep, jj-cli, topgrade, eza, git-absorb, gitu, and more
  - **npm**: Claude Code CLI, LSPs (typescript, dockerfile, yaml, etc.), prettierd, eslint_d
  - **pipx**: llm, ruff, pgcli, yt-dlp, yubikey-manager
  - **aqua**: hexyl, tlrc, golangci-lint
- Core tools: lazygit, gum, chezmoi, just, kubectl, hadolint, shellcheck, shfmt
- Tools are installed automatically when entering directories with `.tool-versions` or `.mise.toml`

### Helix

Helix is configured in `dot_config/helix/config.toml`:

- Theme: tokyonight
- Relative line numbers with cursorline highlighting
- LSP with inlay hints enabled
- Inline diagnostics for better error visibility
- Git diff integration

### Jujutsu (jj)

Jujutsu is a Git-compatible VCS configured in `dot_config/jj/config.toml.tmpl`:

- Uses delta as the pager and diff formatter
- GPG signing configured
- Custom aliases: `wip`, `setmain`, `sync`, `evolve`, `xl`
- Installed via mise (cargo backend)

### Additional Tools

Other configured tools in `dot_config/`:

- **bat**: cat replacement with syntax highlighting (`dot_config/bat/`)
- **btop**: Resource monitor (`dot_config/btop/`)
- **lazygit**: Terminal UI for git (`dot_config/lazygit/config.yml`)
- **linearmouse**: Mouse customization for macOS (`dot_config/linearmouse/`)
- **ripgrep**: Fast grep alternative (`dot_config/ripgrep/`)
- **topgrade**: System-wide package updater (`dot_config/topgrade/`)
- **yamllint**: YAML linter configuration (`dot_config/yamllint.yaml`)

### Fish Shell

Fish configuration lives in `dot_config/fish/`:

- Plugins managed by fisher (defined in `fish_plugins.tmpl`)
- Custom functions in `functions/` directory:
  - `gc-ai.fish` → Git commit message generation
  - `commit_msg.fish` / `commit_review.fish` → Commit helpers
  - `benchmark_launch.fish` → Launch benchmarking utility

Update fisher plugins:

```fish
fisher update
```

## Working with Templates

Many files use Go templating for cross-platform/profile support. Common template variables:

- `.chezmoi.os` → Operating system ("darwin" or "linux")
- `.workconf` → Boolean for work profile
- `.email` / `.name` → User identity
- `.git.email` / `.git.signing_key` → Git configuration

Example template conditional:

```go
{{- if eq .chezmoi.os "darwin" }}
  # macOS-specific config
{{- else }}
  # Linux-specific config
{{- end }}
```

## macOS Package Management

Packages defined in `dot_Brewfile.tmpl`:

- Core CLI tools (bat, ripgrep, fzf, gh, etc.)
- GUI applications (casks)
- Mac App Store apps (via `mas`)
- Profile-specific packages (personal vs work)

Install/update packages:

```fish
brew bundle --file ~/.Brewfile  # Install from Brewfile
brew bundle cleanup             # Remove packages not in Brewfile
```

## Common Workflows

### Adding New Configuration Files

```fish
chezmoi add ~/.config/newapp/config.yaml
```

### Adding Platform-Specific Config

```fish
chezmoi add --template ~/.config/tool/settings.json
# Then edit to add conditionals based on .chezmoi.os
```

### Updating All Development Tools

```fish
mise upgrade        # Upgrade all mise-managed tools
topgrade           # Upgrade system packages, mise, brew, etc.
```

### Git Operations

This repository tracks dotfiles with git. When committing changes:

```fish
chezmoi cd                     # Enter chezmoi source directory
git add -p                     # Stage changes interactively
git commit -m "description"
git push
exit                           # Return to previous directory
```

**Commit Message Format**: Use [Conventional Commits](https://www.conventionalcommits.org/) format:

- `feat:` - New features
- `fix:` - Bug fixes
- `docs:` - Documentation changes
- `chore:` - Maintenance tasks (package updates, config changes)
- `refactor:` - Code refactoring without behavior changes
- `style:` - Formatting changes

### Jujutsu (jj) Workflows

Jujutsu is available as an alternative to git with improved UX:

```fish
jj wip                         # Quick commit with "wip" message
jj sync                        # Fetch from all remotes
jj evolve                      # Rebase on top of main branch
jj setmain                     # Set main branch reference
jj xl                          # Full log of all commits
```

Common operations:
- Changes are auto-committed in jj (no staging area)
- Use `jj describe` to edit commit messages
- Delta is configured as the pager for better diffs
- GPG signing is enabled by default

## Editor-Specific Notes

### Zed

- Settings are in `dot_config/zed/private_settings.json.tmpl` (templated for fonts)
- Keybindings in `dot_config/zed/private_keymap.json`
- Ruby snippets in `dot_config/zed/snippets/ruby.json`
- Uses `zed@preview` for personal profile, `zed` for work profile

### Helix

- Configuration in `dot_config/helix/config.toml`
- Theme: tokyonight with relative line numbers
- LSP features: inlay hints, inline diagnostics, message display
- Ruler at column 80 for line length guidance
- Git integration with diff support

## Notes

- The repository includes both macOS and Linux (Fedora) support
- Fedora setup instructions are in `doc/fedora.md`
- GPG configuration for Yubikey is included
- Docker is configured for rootless operation on Linux
