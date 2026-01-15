# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a [chezmoi](https://chezmoi.io) dotfiles repository that manages system configuration across macOS and Linux (Fedora). The repository uses chezmoi's templating system to handle platform-specific and work/personal configuration differences.

## Core Tools & Stack

- **Shell**: fish (with fisher plugin manager)
- **Terminal**: Ghostty
- **Editors**: Zed (primary), Neovim (configured with lazy.nvim)
- **Version Control**: git with git-delta, lazygit, Neogit (in nvim)
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
- CLI tools (via cargo, npm, pipx, aqua backends)
- Tools are installed automatically when entering directories with `.tool-versions` or `.mise.toml`

### Neovim

Neovim uses lazy.nvim for plugin management:

- Configuration: Single-file setup in `dot_config/nvim/init.lua` (no lua/ subdirectories)
- LSP servers installed via Mason
- Ruby LSP configured with `nvim-lspconfig-bundler` for proper gem resolution
- Key plugins: fzf-lua, treesitter, conform (formatting), nvim-lint, blink.cmp

Sync plugins:

```fish
nvim --headless "+Lazy! sync" +qa
```

### Fish Shell

Fish configuration lives in `dot_config/fish/`:

- Plugins managed by fisher (defined in `fish_plugins.tmpl`)
- Custom functions in `functions/` directory
- Notable functions:
  - `gc-ai.fish` → Git commit message generation
  - `commit_msg.fish` / `commit_review.fish` → Commit helpers

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

## Editor-Specific Notes

### Zed

- Settings are in `dot_config/zed/private_settings.json.tmpl` (templated for fonts)
- Keybindings in `dot_config/zed/private_keymap.json`
- Ruby snippets in `dot_config/zed/snippets/ruby.json`

### Neovim

- Leader key: Space
- Main fuzzy finder: fzf-lua (mapped to `<leader><leader>`, `<leader>pf`, etc.)
- LSP keybindings: `gr*` prefix (e.g., `grn` rename, `grd` definition, `grr` references)
- File browser: oil.nvim (mapped to `-`)
- Git interface: Neogit (mapped to `<leader>gg`)

## Notes

- The repository includes both macOS and Linux (Fedora) support
- Fedora setup instructions are in `doc/fedora.md`
- GPG configuration for Yubikey is included
- Docker is configured for rootless operation on Linux
