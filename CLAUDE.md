# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal home-manager configuration repository for NixOS/macOS. It defines a reproducible system environment using Nix flakes and home-manager.

## Commands

```bash
make all      # Switch home-manager config and link files
make link     # Run ./link.nu to symlink config files
make up       # Update flake inputs and commit
make prebuild # Build custom Iosevka font variants
```

The development shell is provided via direnv (`use flake`).

## Architecture

**Nix Configuration:**
- `flake.nix` - Flake definition with inputs (nixpkgs 25.11, home-manager, doomemacs, nu-scripts)
- `home.nix` - Main home-manager configuration (programs, packages, shell config)
- `emacs.nix` - Doom Emacs configuration module
- `registry-pins.nix` / `channel-pins.nix` - Nix registry and channel pinning

**Configuration Directories:**
- `conf/` - Dotfiles for various tools (doom.d, nushell, wezterm, etc.)
- `jj-commands/` - Custom Jujutsu VCS commands (Python/Nushell)
- `pkgs/` - Custom Nix package definitions
- `prebuilt/` - Pre-built binaries (Iosevka fonts)

**Multi-user Support:**
- Usernames: `dan`, `dwb`
- Platform detection for darwin vs Linux, GUI vs headless

## Key Tools Configured

- **Editor:** Doom Emacs (config in `conf/doom.d/`)
- **Shell:** Nushell (config in `conf/nushell/`)
- **VCS:** Jujutsu with custom aliases and revset functions (defined in `home.nix`)
- **Terminals:** WezTerm, Ghostty

## Working with this Repo

Configuration changes should be made to the Nix expressions.

Custom commands in `jj-commands/` support megamerge workflows (multiple parent commits). The `link.nu` script handles symlinking these and other config files.
