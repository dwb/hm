---
name: jj
description: Read-only navigation and research of Jujutsu VCS repository history. Use when exploring commits, bookmarks, diffs, and history. No mutating operations.
---

# Jujutsu (jj) VCS - Read-Only Navigation Skill

Use this skill when asked to explore, navigate, or research repository history using jj. This skill is for read-only operations only.

**CRITICAL: This skill permits ONLY read-only commands. NO MUTATING COMMANDS are allowed.**

## Core Concepts (Key Differences from Git)

### Change ID vs Commit ID
- **Commit ID**: Like Git's SHA - changes when a commit is rewritten (20 bytes, hex format)
- **Change ID**: Stable identifier for a *change* that persists across rewrites (displayed as 12 letters k-z)
- Prefer change IDs when referencing commits, as they survive rebases and amendments

### Working Copy is a Commit
Unlike Git where the working copy is separate, in jj the working copy IS a commit (`@`). Every edit automatically amends this commit.

### Bookmarks (not Branches)
- jj uses "bookmarks" - named pointers to commits (similar to Git branches)
- Bookmarks do NOT auto-advance when you create new commits
- Bookmarks DO follow when commits are rewritten/rebased
- No concept of "current branch" - you're always on a working-copy commit

### Anonymous Branches
Commits without bookmarks form "anonymous branches" - jj keeps them visible until explicitly abandoned.

### Private Changes
Changes with descriptions prefixed `private:` or `wip:` cannot be pushed to remotes. Example: `private: WORKING`

### Workspaces
Multiple working copies from the same repository (like Git worktrees). Each workspace has its own `@`. Reference another workspace's working copy with `<workspace>@`.

## User's Workflow Patterns

**IMPORTANT: The workflow varies by project. Check `jj st` and `jj lt` to understand the current repo structure. When in doubt about what workflow is in use, ASK the user.**

### Megamerge (Large Projects)
Used in larger projects where multiple features are developed simultaneously:
- A working change (typically `private: WORKING`) has multiple parents
- This forms a union of several branches in one working copy
- The megamerge change itself is NOT part of any branch
- **When navigating, examine its parents (`@-`) to see the actual branches being worked on**
- Identified by: `@` having multiple parents, description like `private: WORKING`

### Simple Linear (Smaller Projects / Alternative Workspaces)
- `@` sits at the head of a single branch/bookmark
- Used for focused work, long-running migrations, or simpler projects
- Navigate normally without megamerge considerations

### Detecting the Workflow
Run `jj st` or `jj lt` first. If `@` has:
- Multiple parents with a `private:` description → megamerge workflow
- Single parent → simple linear workflow
- If unclear → **ask the user**

## User's Configured Aliases

### `jj lt [revset]` - Log to Trunk (PREFERRED)
- Default: logs from `@` back to trunk
- With argument (typically a bookmark): logs between trunk ancestors and `@` where the given revset is involved
- **Prefer this over raw `jj log` when exploring around bookmarks** - it shows the full picture including descendant revisions up to `@`
- Example: `jj lt my-feature` shows all relevant context for the `my-feature` bookmark

### `jj dt` - Diff to Trunk
Shows diff from trunk to current state. Always uses git format.

### Diff Flag
Always use `--git` flag with `jj diff` or diff-based commands.

## Allowed Read-Only Commands

### Status & Current State
```bash
jj st                           # Status of working copy - RUN THIS FIRST
jj lt                           # Log from @ to trunk (preferred default)
jj lt <bookmark>                # Log around a bookmark with full context to @
jj log -r '@-'                  # Parents of @ (useful for megamerge)
```

### Viewing Changes
```bash
jj show                         # Show current commit (@)
jj show <rev>                   # Show specific revision
jj diff --git                   # Diff of @ vs parents
jj diff -r <rev> --git          # Diff of specific revision
jj diff --from <rev1> --to <rev2> --git  # Diff between revisions
```

**`jj show` does NOT accept file path arguments.** To filter a revision's diff
to specific files, use `jj diff -r <rev> --git -- <paths>`.

**IMPORTANT**: `-r` and `--from`/`--to` are mutually exclusive in `jj diff`. Use one or the other:
- `jj diff -r <rev>` — show what changed IN that revision (vs its parents)
- `jj diff --from <A> --to <B>` — show difference BETWEEN two revisions

**Common use cases:**
- "What did this one commit change?" → `jj diff -r <rev> --git`
- "All changes on a branch vs trunk/develop?" → `jj diff --from trunk() --to <branch_tip> --git`
- "Cumulative diff of a branch?" → `jj diff --from <base> --to <tip> --git` (NOT `-r`)

**Common mistakes:**
- Using `-r <branch_tip>` when you want the cumulative branch diff. This only shows that single commit's changes, not all commits since the branch diverged.
- Using `-r <complex_revset>` with a revset that has gaps. `jj diff -r` requires a contiguous range. If you get "Cannot diff revsets with gaps in", use `--from`/`--to` instead.

### Exploring History
```bash
jj evolog                       # How current change evolved over time
jj evolog -r <rev>              # Evolution of specific change
jj op log                       # Operation log (undo history)
```

**`evolog` templates use a different type** — see "evolog Templates" section below.

### When to Use `evolog`

`evolog` shows the **evolution of a single change** — all the historical versions of the same change ID as it was amended, rebased, or otherwise rewritten. Each entry is a snapshot of what the commit looked like at a point in time.

**Use `evolog` when asked about:**
- "What changed since I last worked on this?" / "changes over the last session"
- "Show me the history of edits to this commit/branch"
- "Find when a specific modification was made"
- "How did this working copy evolve?"
- Finding a baseline commit ID to diff against (e.g., "diff since my last edit")

**Typical workflow for "show changes since X":**
1. Run `jj evolog` to see the evolution timeline with commit IDs and timestamps
2. Identify the commit ID that represents the "before" state (look for time gaps, or specific timestamps)
3. Use `jj diff --from <old-commit-id> --to @ --git` to see all changes since then

**Example:** User says "show me what changed since I modified the instructions"
1. `jj evolog` — find the commit ID from when they made their edit (look for timestamp/gap)
2. `jj diff --from <that-commit-id> --to @ --git` — see all changes since

This is different from `jj log` which shows the commit graph, and `jj op log` which shows operations (undo history).

### Bookmarks & Remotes
```bash
jj bookmark list                # List local bookmarks
jj bookmark list --all-remotes  # Include remote bookmarks
jj bookmark list --tracked      # Show tracked remote bookmarks
```

### Files
```bash
jj file list                    # List files in @
jj file list -r <rev>           # List files in specific revision
```

### Workspaces
```bash
jj workspace list               # List all workspaces
jj log -r '<workspace>@'        # See another workspace's working copy
```

## Revset Syntax Quick Reference

### Symbols
- `@` - working copy commit (this workspace)
- `<name>@` - working copy of another workspace
- `@-` - parent(s) of @ (**see warning below**)
- `root()` - virtual root commit
- `trunk()` - trunk bookmark (usually main@origin)

**WARNING about `@-` with merges**: When `@` is a merge commit (multiple parents), `@-` returns ALL parents, which may not be what you want in a revset range. For example, `branch_tip::@-` may give unexpected results if `@-` includes commits not in the branch's ancestry. Be explicit: use `::branch_tip & ~::trunk()` to enumerate branch commits reliably.

### Operators
- `x-` - parents of x
- `x+` - children of x
- `::x` - ancestors of x (inclusive)
- `x::` - descendants of x (inclusive)
- `x..y` - ancestors of y excluding ancestors of x (like Git's x..y)
- `x::y` - ancestors of y that are descendants of x (ancestry-path)
- `x & y` - intersection
- `x | y` - union
- `~x` - not x

### Common Functions
- `bookmarks()` - all bookmark targets
- `remote_bookmarks()` - all remote bookmark targets
- `heads(x)` - heads within x
- `roots(x)` - roots within x
- `description(pattern)` - commits matching description
- `author(pattern)` - commits by author
- `mine()` - commits by current user
- `empty()` - empty commits
- `conflicts()` - commits with conflicts
- `merges()` - merge commits
- `working_copies()` - working copy commits across all workspaces

## Custom Templates

Use `-T` to customize output. Key points:

### Accessing Timestamps
Timestamps are accessed via signature objects, NOT as top-level keywords:
```bash
# WRONG - these don't exist:
jj log -T 'commit_timestamp.ago()'
jj log -T 'format_timestamp(commit_timestamp, "%Y-%m-%d")'

# CORRECT:
jj log -T 'committer.timestamp().ago()'
jj log -T 'author.timestamp().format("%Y-%m-%d")'
```

### Common Template Methods
- `committer.timestamp()` / `author.timestamp()` → Timestamp object
- `.ago()` → relative time string ("2 days ago")
- `.format("%Y-%m-%d")` → custom strftime format
- `.local()` / `.utc()` → timezone conversion

### Useful Template Examples
```bash
# Compact one-line format with date
jj log -T 'change_id.short() ++ " " ++ committer.timestamp().ago() ++ " " ++ description.first_line() ++ "\n"'

# Machine-readable with ISO date
jj log -T 'commit_id ++ " " ++ committer.timestamp().format("%Y-%m-%d") ++ "\n"'
```

### evolog Templates

`jj evolog` uses a different template type (`CommitEvolutionEntry`) than `jj log` (`Commit`). You must use `self.commit()` to access commit properties:

```bash
# WRONG - these don't work in evolog:
jj evolog -T 'change_id.short() ...'           # Error: Keyword doesn't exist
jj evolog -T 'commit_id.short() ...'           # Error: Keyword doesn't exist
jj evolog -T 'commit().commit_id().short()'    # Error: Function doesn't exist

# CORRECT - use self.commit() to access the wrapped commit:
jj evolog -T 'self.commit().commit_id().short() ++ " " ++ self.commit().committer().timestamp().format("%H:%M") ++ "\n"'
```

Available `CommitEvolutionEntry` methods:
- `self.commit()` → the `Commit` object (then use normal commit methods)
- `self.operation()` → the operation where this version was created
- `self.predecessors()` → list of predecessor commits
- `self.inter_diff([files])` → changes between this and predecessor versions

### Getting Help
Use `jj help -k <keyword>` (or `--keyword`) for detailed inline documentation. Running it without a keyword shows available topics (currently: `bookmarks`, `config`, `filesets`, `glossary`, `revsets`, `templates`, `tutorial` — if this list has changed, update this skill file).

Output is markdown and can be lengthy. Filter to headings first to find relevant sections:
```bash
jj help -k templates | grep '^#'      # See section structure
jj help -k revsets | grep -A2 '^## '  # Headings with brief context
```

## Example Queries

### First: Understand the Repo
```bash
jj st                           # What's the current state?
jj lt                           # What's the history to trunk?
```

### Catching Up on a Branch
When asked to "catch up" or "see what changed" on a branch, **always start with `jj lt <bookmark>`** before using `jj diff`:

1. `jj lt <bookmark>` - See commit structure, messages, and context first
2. Then `jj diff --from <base> --to <tip> --git` if you need the full diff

**Why**: The log shows *what* was done and *why* (commit messages), while diff only shows *how*. Understanding the sequence and intent first makes the diff more meaningful.

### Megamerge Navigation (when applicable)
```bash
jj log -r '@-'                  # See what branches the megamerge combines
jj lt my-feature                # Explore a specific bookmark with full context
jj diff --from trunk() --to @ --git  # See all work since trunk
jj log -r '@-' -- path/to/file  # Find which parent branch contains a file change
```

### Extracting Changes from Megamerge for Push
To extract specific files from the megamerge working copy into a new change ready for push:
```bash
jj split -A develop -B @ -m "commit message" path/to/file1 path/to/file2
```

**Flags explained:**
- `-A develop` (insert-after): The new change becomes a child of `develop`
- `-B @` (insert-before): The new change becomes a parent of the megamerge
- Result: `develop → new-change → megamerge`

**IMPORTANT: File paths are relative to your current working directory**, not the repo root. Check `jj st` output for the correct relative paths. If `jj st` shows `../test/file.js`, use that path, not `workspace/@causal/test/file.js`.

### Recovering from `workspace update-stale` Divergence

When `jj workspace update-stale` reconciles divergent operations, it may create a new empty working copy while the version with actual work becomes an orphaned divergent copy. Symptoms: `@` is empty, change ID shows as `<id>/2` (divergent).

**Diagnosis:**
```bash
jj log -r 'change_id(<change_id>)'  # List all divergent versions
```

**Recovery:** The simplest fix is to `jj edit` to the version that has the work:
```bash
jj edit <change_id>/0              # Edit the version with actual content
```

**Do NOT reach for `jj op restore`** — it's overkill when the work is right there in a divergent copy. `op restore` rewinds the entire operation history; `jj edit` just points the working copy at the right version.

### Cross-Workspace
```bash
jj workspace list               # See all workspaces
jj log -r 'working_copies()'    # See what all workspaces are doing
jj diff --from 'other-workspace@' --to @ --git  # Compare workspaces
```

## Non-Interactive Split Tool

See [split-tool.md](split-tool.md) for the manifest-driven `jj-split-tool.sh`
that enables hunk-level splits without a TUI, via `jj split --tool`.

## FORBIDDEN (Mutating Commands)

DO NOT USE any of these commands - they modify repository state:
- `jj new`, `jj commit`, `jj describe`
- `jj edit`, `jj squash`, `jj split`
- `jj rebase`, `jj abandon`
- `jj bookmark create/move/delete/track`
- `jj git push/fetch`
- `jj undo`, `jj op restore`
- `jj workspace add/forget`
- Any other command that modifies commits, bookmarks, or working copy
