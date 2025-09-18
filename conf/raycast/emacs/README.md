# Emacs (Raycast extension)

Drive a running Emacs from Raycast through `emacsclient`. This extension never
starts a fresh `emacs`; it only evaluates elisp in the server you already have
running.

Almost all behaviour lives in elisp, in the companion package
`conf/doom.d/lib/raycast-emacs.el`. The TypeScript here is a thin, generic
renderer: it asks Emacs for a list of things, shows them, and calls back into
Emacs when you pick one.

## Commands

- **Emacs Commands** — lists commands registered in elisp and runs the one you
  pick. This is the extension point (see below).
- **Switch Buffer** — lists buffers (with project and major mode) and displays
  the one you pick, focusing its frame.
- **Select Frame** — lists visible frames and raises/focuses the one you pick.

## How it talks to Emacs

Each command calls `raycast-emacs-dispatch` via
`emacsclient --eval`. Arguments are sent as base64-encoded JSON and results come
back as base64-encoded JSON, so nothing has to worry about elisp print escaping.

The `emacsclient` binary is taken from the **emacsclient Path** preference
(default `~/.nix-profile/bin/emacsclient`).

## Setup

1. The elisp companion is loaded by `config.el`
   (`(require 'raycast-emacs)`). Reload your Doom config (or restart Emacs) so
   it is active, and make sure the Emacs server is running (this config starts
   it automatically).
2. Import the extension into Raycast for development:

   ```
   cd conf/raycast/emacs
   npm install   # first time only
   npm run dev   # runs `ray develop`; registers the commands and hot-reloads
   ```

   Leave `npm run dev` running while you use or edit the extension. The three
   commands appear in Raycast immediately.

## Extending from elisp

Add a command with `raycast-emacs-register-command`, or declaratively via the
`raycast-emacs-commands` customization variable. New entries appear in the
**Emacs Commands** list with no rebuild of this extension.

```elisp
(raycast-emacs-register-command
 :id "org-capture-todo"
 :title "Capture TODO"
 :subtitle "Org capture"
 :icon "CheckList"
 :handler (lambda (&optional _args) (org-capture nil "t")))
```

- `:handler` is called with zero arguments, or with a single alist of arguments
  if it accepts one.
- `:icon` is any Raycast icon name; unknown names fall back to a default.

## Notes

- The `modified` indicator in Switch Buffer reflects `buffer-modified-p`, which
  is set for many special buffers; treat it as advisory.
- Frame focus uses `select-frame-set-input-focus`. If bringing Emacs to the
  foreground over Raycast is unreliable on your setup, adjust the focus behaviour
  in `raycast-emacs.el`.
