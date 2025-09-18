---
name: org-jj
description: Write Org documents that link to code in jj repositories and show jj diffs, using the "jj:" and "jj-diff:" link types and the "jj-diff" dynamic block from the user's Emacs library my-org-jj. Use when writing or editing Org notes, change walkthroughs, or review notes that refer to files at jj revisions or show parts of a jj diff.
allowed-tools: Bash(nu ~/.claude/skills/org-jj/org-jj.nu update *), Bash(nu ~/.claude/skills/org-jj/org-jj.nu check *)
---

# Org links and diff blocks for jj

The user's Emacs config loads `conf/doom.d/lib/my-org-jj.el` (in `~/.config/home-manager`). It adds three things to Org:

- `jj:` links to a file at a jj revision, optionally at a line.
- `jj-diff:` links that open a `vc-diff` buffer between two revisions.
- `jj-diff` dynamic blocks, which jj fills with one file's diff, optionally limited to a range of lines.

Use the `jj` skill before running any jj commands.

If this skill and the source disagree, the source is correct. Read it and tell the user.

## `jj:` links

```org
[[jj:PATH::REV]]
[[jj:PATH::REV::LINE][description]]
```

- PATH is resolved like a `file:` link: absolute, `~/...`, or relative to the Org file's directory.
- REV is any revset that resolves to one commit. PATH ends at the first `::`. A final `::` followed only by digits is LINE.
- Write REV as a full or short commit ID, not a relative revset such as `@` or `@-`. A relative revset points at different content later. `@` is replaced at every working-copy snapshot.
- To link to the current content of a file that `@` does not change, use the commit ID of `@-`. This is what `my/org-jj-store-link-at-commit` does. Only do this when `@` has one parent.
- The description the library generates is `FILE@COMMIT12:LINE`, for example `home.nix@3a1cc69f1b86:340`. Use the same form unless another description is more useful.

Get a commit ID:

```bash
jj log --no-graph -r '@-' -T 'commit_id.short(12)'
```

## `jj-diff:` links

```org
[[jj-diff:PATH::FROM::TO]]
```

PATH is resolved like a `file:` link and may be a directory. FROM and TO are revisions. None of the three parts can contain `::`. Following the link opens a `vc-diff` buffer.

## `jj-diff` dynamic blocks

```org
#+BEGIN: jj-diff :from "3f2a91c0" :to "7a564005" :file "src/foo.el" :lines (40 90)
#+END:

#+BEGIN: jj-diff :rev "3a1cc69f1b86" :file "home.nix"
#+END:
```

Parameters:

| Parameter  | Meaning |
|------------|---------|
| `:from`, `:to` | Two revisions, as for `jj diff --from --to`. |
| `:rev`     | One revision, as for `jj diff -r`. Use either `:rev` or both `:from` and `:to`. |
| `:file`    | Path relative to the repository root. Required. |
| `:lines`   | Optional `(START END)`. Keeps only hunks whose lines on the new side overlap START to END. |
| `:context` | Optional number of context lines. |

Rules:

- Quote every revision and the file. Org reads the parameters with `read`, so an unquoted `7a564005` becomes a number and the block reports an error.
- Use commit IDs, not relative revsets, for the same reason as for `jj:` links.
- jj runs in the directory given by the inherited `JJ_REPO` property. Without it, jj runs in the Org file's directory. Set `JJ_REPO` when the Org file is outside the repository.
- For the whole file, put the property drawer on the first lines of the file, before `#+title:` and any other lines. Org ignores a file-level drawer that comes after other lines, and jj then runs in the wrong directory. A drawer under a heading applies to that heading and its subheadings.

```org
:PROPERTIES:
:JJ_REPO: ~/Developer/some-repo
:END:
#+title: Walkthrough of the parser change
```

- When a block is updated, its content is replaced with a `#+begin_src diff` block, or with a line starting `jj-diff:` that gives an error or says that no changes match. Do not write the content yourself. Leave the block empty and fill it as described below.
- `:lines` refers to line numbers on the new side, that is in the file at `:to` or `:rev`. Get them from the `+START,COUNT` part of the hunk headers in `jj diff --git`.

### Showing a whole diff in parts

A common use is to walk through a diff in several blocks, with text between them. Give every block the same revisions and use `:lines` to choose the hunks for each block. Each hunk should be in exactly one block.

`my/org-jj-diff-uncovered` checks this. It groups the blocks in a buffer by repository and revisions, and for each group reports:

- hunks that no block shows (`not shown`),
- hunks that more than one block shows (`shown N times`),
- blocks that show nothing.

## Filling blocks and checking coverage

Use the script in this skill's directory. Run it with exactly this path, so that it matches the allowed tools:

```bash
nu ~/.claude/skills/org-jj/org-jj.nu update path/to/notes.org
nu ~/.claude/skills/org-jj/org-jj.nu check path/to/notes.org
```

- `update` updates every dynamic block in the file, saves it, and prints the coverage report.
- `check` prints the coverage report and does not change the file.
- When there are no problems, both print `Every hunk is shown exactly once`.

The script calls `my/org-jj-diff-file-report` through `emacsclient`. It works in a temporary buffer, so it does not change the user's buffers or windows. Notes:

- If Emacs has the file open with unsaved changes, the script fails and changes nothing. Tell the user. Do not work around it.
- If Emacs has the file open without unsaved changes, Emacs asks the user to reload it after `update`, or reloads it if `auto-revert-mode` is on.
- An error in one block does not stop the update. It appears in that block as a line starting `jj-diff:`. Read the file after `update` and look for such lines.
- Line numbers in the report refer to the file after the update.
- Do not open the Org file in Emacs yourself, and do not run the interactive commands below through `emacsclient`. The user is probably working in Emacs at the same time.

## Checking a diff without Emacs

To choose `:lines` ranges, list the hunks with jj:

```bash
jj diff --git --from FROM --to TO -- 'root:"path/to/file"'
jj-hunk list --rev REV --format text
```

## Commands the user runs in Emacs

These are interactive. Mention them to the user when useful.

- `C-c C-x C-u` on a block, or `org-update-all-dblocks`: update blocks.
- `my/org-jj-diff-uncovered`: show the coverage report.
- `C-c C-o` on a diff line in a block (`my/org-jj-diff-visit`): open the file at that line, at the new revision for added and context lines and at the old revision for removed lines.
- `my/org-jj-store-link-at-commit` in a file buffer: store a `jj:` link to the current line, for `org-insert-link`.
- `org-store-link` in a buffer opened from a `jj:` link: store a `jj:` link with the revision as a commit ID.
