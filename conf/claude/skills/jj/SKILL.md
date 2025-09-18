---
name: jj
description: Read-only navigation and research of Jujutsu VCS repository history. Use when exploring commits, bookmarks, diffs, and history. No mutating operations unless expressly requested.
---

# Jujutsu (jj) VCS Navigation Skill

Use this skill when asked to explore, navigate, or research repository history using jj. Default to read-only operations. Mutating commands may be used when the user expressly requests them.

## Context

If "$ARGUMENTS[0]" is non-empty, it is a change ID that the user is giving as context for the
conversation. Look at it and its relation to `@` to understand how we should proceed.

## User's Configured Aliases

### `jj lt [revset]` - Log to Trunk (PREFERRED)
- Default: logs from `@` back to trunk
- With argument (typically a bookmark): logs between trunk ancestors and `@` where the given revset is involved
- **Prefer this over raw `jj log` when exploring around bookmarks** - it shows the full picture including descendant revisions up to `@`

### `jj dt` - Diff to Trunk
Shows diff from trunk to current state. Always uses git format.

### Diff Flag
Always use `--git` flag with `jj diff` or diff-based commands.

## User's Workflow Patterns

**The workflow varies by project. Check `jj st` and `jj lt` to understand the current repo structure. When in doubt, ASK the user.**

### Megamerge (Large Projects)
- A working change (typically `private: WORKING`) has multiple parents
- Forms a union of several branches in one working copy
- The megamerge change itself is NOT part of any branch
- **Examine its parents (`@-`) to see the actual branches being worked on**
- Identified by: `@` having multiple parents, description like `private: WORKING`

### Simple Linear (Smaller Projects / Alternative Workspaces)
- `@` sits at the head of a single branch/bookmark
- Navigate normally without megamerge considerations

### Detecting the Workflow
Run `jj st` or `jj lt` first. If `@` has:
- Multiple parents with a `private:` description → megamerge workflow
- Single parent → simple linear workflow
- If unclear → **ask the user**

## Gotchas and Non-Obvious Behaviour

### `jj show` does not accept file paths
To filter a revision's diff to specific files, use `jj diff -r <rev> --git -- <paths>`.

### `jj diff`: `-r` vs `--from`/`--to` are mutually exclusive
- `jj diff -r <rev>` — what changed IN that revision (vs its parents)
- `jj diff --from <A> --to <B>` — difference BETWEEN two revisions

**Common mistakes:**
- Using `-r <branch_tip>` when you want the cumulative branch diff. This only shows that single commit's changes. Use `--from`/`--to` instead.
- Using `-r <complex_revset>` with gaps. If you get "Cannot diff revsets with gaps in", use `--from`/`--to`.

### `@-` with merges
When `@` is a merge commit, `@-` returns ALL parents. For example, `branch_tip::@-` may give unexpected results. Be explicit: use `::branch_tip & ~::trunk()` to enumerate branch commits reliably.

### evolog templates use a different type
`jj evolog` uses `CommitEvolutionEntry`, not `Commit`. Access commit properties via `self.commit()`:

```bash
# WRONG:
jj evolog -T 'change_id.short() ...'

# CORRECT:
jj evolog -T 'self.commit().commit_id().short() ++ " " ++ self.commit().committer().timestamp().format("%H:%M") ++ "\n"'
```

Available methods: `self.commit()`, `self.operation()`, `self.predecessors()`, `self.inter_diff([files])`.

## Using `evolog` to Find Historical State

`evolog` shows the evolution of a single change — all historical versions as it was amended, rebased, etc.

**Use when asked about:** "what changed since last session", "history of edits to this commit", "find when a modification was made".

**Typical workflow for "show changes since X":**
1. `jj evolog` — find the commit ID representing the "before" state (look for time gaps or timestamps)
2. `jj diff --from <old-commit-id> --to @ --git` — see all changes since then

## Megamerge Operations

### Extracting Changes for Push
```bash
jj split -A develop -B @ -m "commit message" path/to/file1 path/to/file2
```
- `-A develop` (insert-after): new change becomes child of `develop`
- `-B @` (insert-before): new change becomes parent of the megamerge
- **File paths are relative to your current working directory**, not the repo root

### Recovering from `workspace update-stale` Divergence
When reconciling divergent operations, jj may create a new empty working copy while the version with actual work becomes orphaned.

```bash
jj log -r 'change_id(<change_id>)'  # List all divergent versions
jj edit <change_id>/0               # Edit the version with actual content
```

Do NOT reach for `jj op restore` — `jj edit` is sufficient when the work is in a divergent copy.

### Getting Help
`jj help -k <keyword>` for inline docs. Available topics include: `bookmarks`, `config`, `filesets`, `glossary`, `revsets`, `templates`, `tutorial`. Output is lengthy; filter to headings first:
```bash
jj help -k templates | grep '^#'
```

## Non-Interactive Split Tool

See [split-tool.md](split-tool.md) for the manifest-driven `jj-split-tool.sh`
that enables hunk-level splits without a TUI, via `jj split --tool`.

## Mutating Commands (User Request Required)

The following commands modify repository state. Do not use them on your own initiative — only when the user expressly requests it.

- `jj new`, `jj commit`, `jj describe`
- `jj edit`, `jj squash`, `jj split`
- `jj rebase`, `jj abandon`
- `jj bookmark create/move/delete/track`
- `jj git push/fetch`
- `jj undo`, `jj op restore`
- `jj workspace add/forget`
- Any other command that modifies commits, bookmarks, or working copy
