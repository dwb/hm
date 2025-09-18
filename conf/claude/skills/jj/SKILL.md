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
`jj evolog` uses `CommitEvolutionEntry`, not `Commit`. ALL commit-level fields (`commit_id`, `change_id`, `description`, `author`, `committer`, etc.) must go through `self.commit()`:

```bash
# WRONG (none of these keywords exist at the entry level):
jj evolog -T 'change_id.short() ...'
jj evolog -T 'description ...'
jj evolog -T 'author.name() ...'

# CORRECT:
jj evolog -T 'self.commit().commit_id().short() ++ " " ++ self.commit().committer().timestamp().format("%H:%M") ++ "\n"'
```

Available methods on the entry: `self.commit()`, `self.operation()`, `self.predecessors()`, `self.inter_diff([files])`.

`self.predecessors()` returns a `List<Commit>`, not a single commit. A squash collapses multiple commits into one entry, so any template that assumes a single predecessor (`self.predecessors().commit_id()`) will error. Use `.join(",")` or iterate.

## Using `evolog` to Find Historical State

`evolog` shows the evolution of a **single change** — all historical versions of it as it was amended, rebased, squashed, etc. It does NOT show repo-wide history.

**Pick the right tool:**
- Whole-repo commit graph → `jj log`
- Whole-repo operation history (commands run against the repo) → `jj op log`
- One change's rewrite lineage (this commit's previous versions) → `jj evolog`

**Use when asked about:** "what changed since last session", "history of edits to this commit", "find when a modification was made", "get back the version I had ten minutes ago".

### `-r` semantics

`jj evolog -r <revset>` follows the evolution of revisions matching the revset. This is NOT the same as `jj log -r <revset>`:

- `jj log -r main` — commits reachable from `main` (history)
- `jj evolog -r main` — `main`'s own rewrite history (rare, since `main` usually isn't rewritten)
- `jj evolog` (default `-r @`) — the working-copy change's rewrite history (common)

Pass a change ID or symbolic ref (`@`, `@-`, bookmark), not a commit ID of an already-rewritten version.

### Hidden commits are still usable

Historical commits listed by evolog are "hidden" (abandoned): they don't appear in `jj log` with the default revset. But their commit IDs remain valid arguments to `jj diff`, `jj show`, `jj file show`, `jj restore --from`, etc. This is what makes evolog the right tool for recovering prior working-copy state.

### Patches between versions

`jj evolog -p` shows the diff between each version of the change and its predecessor. It uses `inter_diff`, which rebases the predecessor onto the current parents before diffing, so a rebase or parent change doesn't pollute the patch — you see only what was actually edited.

### Typical workflow for "show changes since X"

1. `jj evolog` — find the commit ID for the "before" state (look for time gaps or timestamps).
2. `jj diff --from <old-commit-id> --to @ --git` — see all changes since then.

## Temporary File Reverts (Don't Copy to /tmp)

When you want to test against an older version of a file and then put it back, do **not** copy files to `/tmp` and back. jj already snapshots the working copy on every command, so the current state is always recoverable.

### Key Facts
- Every jj command (and the user's snapshot hook on each edit) records a new commit ID for `@` in the evolog. No state is lost just because you edited a file.
- `jj restore --from <rev> <paths>` overwrites the named paths in `@` with their content from `<rev>`. Other files are untouched.
- `jj file show -r <rev> <path>` prints the file's content at that revision to stdout — useful for diffing or piping, but `jj restore` is the idiomatic way to put it on disk.
- `jj util snapshot` forces a snapshot now. Rarely needed (almost every command snapshots first), but useful if files were changed outside any jj invocation and you want a recorded checkpoint before doing something else.

### Workflow: Revert a File, Test, Restore

```bash
# 1. Record the current commit ID of @ so we can come back to it.
CURRENT=$(jj log -r @ --no-graph -T 'commit_id')

# 2. Find an older version. evolog lists historical commit IDs of @.
jj evolog -T 'self.commit().commit_id().short() ++ " " ++ self.commit().committer().timestamp() ++ "\n"'
# Or look further back in normal history:
jj log -r 'ancestors(@, 10)' -- path/to/file

# 3. Restore the file from an older revision into the working copy.
jj restore --from <older-commit-id> path/to/file

# 4. Run your test, observe behaviour...

# 5. Put the file back to the state from step 1.
jj restore --from "$CURRENT" path/to/file
```

`<older-commit-id>` may be any revset: a bookmark, `@-`, a trunk ref, an evolog commit ID, etc.

### Workflow: Inspect a File at an Old Revision Without Touching `@`

```bash
jj file show -r <rev> path/to/file        # print to stdout
jj diff --from <rev> --to @ --git -- path/to/file   # see what's changed
```

### Whole-Repo Rollback: `jj op restore`

For undoing a chain of operations (not just file content), use the operation log:

```bash
jj op log               # find the operation to roll back to
jj op restore <op-id>   # restore repo state to that operation
```

This is a mutating operation — only run on explicit user request. For file-level experiments, prefer `jj restore --from` which doesn't rewrite operation history.

### Read-Only Inspection at an Earlier Operation

`jj --at-op=<op-id> <command>` runs a command against the repo as it was at that operation, without mutating anything. Useful for reconstructing diffs or logs from a prior state.

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
