---
name: pr-walkthrough
description: Write an Org-mode walkthrough of a GitHub pull request that guides the user through reviewing it, with the diff shown in a sensible reading order using jj-diff dynamic blocks. Use when the user asks for a PR writeup, walkthrough, or guided tour of a PR's code.
argument-hint: "<pr-number-or-url>"
allowed-tools: Bash(gh pr view *), Bash(gh repo view *), Bash(jj git remote list), Bash(jj git fetch --branch *), Bash(jj log *), Bash(jj diff *), Bash(jj file show *), Bash(nu ~/.claude/skills/org-jj/org-jj.nu update *), Bash(nu ~/.claude/skills/org-jj/org-jj.nu check *)
---

# PR walkthrough in Org

The output is an Org file that shows the user around a pull request so that they can review it.
It presents the diff in a better order than the alphabetical order of files, with explanation
between the parts.

**You are not reviewing the code.** Do not give verdicts, report bugs, or suggest changes. You may
point to things the user will want to look at, such as related code, claims made in comments, or
code the PR does not change, but state them as facts or open questions, not as findings.

Load the `jj` skill before running any jj commands, and the `org-jj` skill before writing the Org
file. The `org-jj` skill describes the `jj:` links, `jj-diff:` links and `jj-diff` dynamic blocks
used here, and the script that fills the blocks and checks coverage.

Write in plain language, as in the user's global instructions.

## 1. Get the PR and its commits

The PR is given as `$0`: either a PR number or a full PR URL such as
`https://github.com/<owner>/<repo>/pull/<number>`. If it is empty, use the PR the user named in the conversation, or
ask for one.

Find the repository that the current directory belongs to:

```bash
gh repo view --json nameWithOwner --jq .nameWithOwner
```

If `gh` cannot tell (for example, the jj repository is not colocated with Git), run
`jj git remote list` and take `<owner>/<repo>` from the GitHub remote, preferring `origin`. If
there is no GitHub remote, or more than one and none is `origin`, ask the user.

If `$0` is a URL, check that its `<owner>/<repo>` is the same as the current repository. If it is
not, stop and tell the user: the PR's commits cannot be fetched into this repository.

```bash
gh pr view '<number-or-url>' --repo <owner>/<repo> --json number,title,body,author,headRefName,baseRefName,headRefOid,baseRefOid,files,commits,url
```

Use the `number` field from this output wherever `<number>` appears below.

The user has asked you to fetch, so fetching the PR's branches is allowed here:

```bash
jj git fetch --branch '<headRefName>' --branch '<baseRefName>'
```

Check that both commits exist locally, and list the PR's commits:

```bash
jj log --no-graph -r 'commit_id(<headRefOid>) | commit_id(<baseRefOid>)' -T 'commit_id.short(12) ++ " " ++ description.first_line() ++ "\n"'
jj log --no-graph -r '<base>::<head>' -T 'commit_id.short(12) ++ " " ++ description.first_line() ++ "\n"'
```

If the head is not a descendant of the base, stop and tell the user: the diff from base to head
would include unrelated changes.

Use 12-character commit IDs everywhere in the document, never change IDs or relative revsets, so
the links keep working.

If the PR is stacked on another PR (the base branch is not the trunk), look at the base PR's diff
too, from the parent of its first commit to the base commit. It often defines the data model the
PR depends on. Summarise it and link to its files at the base commit.

## 2. Read the whole diff

```bash
jj diff --git --stat --from <base> --to <head>
jj diff --git --from <base> --to <head>
jj diff --git --from <base> --to <head> | grep -E '^(diff --git|@@)'
```

Read every hand-written file's diff in full. Generated files (for example GraphQL types) only need
their hunk list. The hunk-header list gives the new-side line numbers you need for `:lines`.

The working copy `@` is usually not at the PR head. It is often a megamerge of other work. So:

- Get line numbers in files at the head commit with
  `jj file show -r <head> '<path>' | grep -n '<pattern>'`, not from the working copy.
- Before using `Read` or `rg` on a working-copy file for context, check that it matches the head:
  `jj diff --git --from <head> --to @ --stat -- '<path>' ...` must report no changes.

## 3. Plan the reading order

Group the changes by what they do, and order the groups so that each one only depends on the
ones before it. An order that usually works:

1. **About this PR**: link, author, base and head branches and commits, a `jj-diff:` link for the
   whole diff, a table of commits, and a short summary in your own words. If the PR is stacked,
   a subsection on the base PR.
2. Shared definitions: constants, types, feature flags, permission predicates.
3. The contract: API schemas, GraphQL operations.
4. New helper modules.
5. The main implementation.
6. Changes to existing code, grouped by purpose (for example "keeping X out of Y"), with the
   question the reviewer should ask of each group.
7. Tests, with the test files near the end unless a test is best read next to what it tests
   (for example a small unit test for a predicate).
8. Small leftovers, such as test ids for a later PR.
9. Generated code, last.
10. An appendix of related code the PR does not change, if relevant. For example, other callers
    of a query the PR changes in some places. Say how you found them and which ones you did not
    look at.

Use Org headings for the groups. Use tables where there is real tabular information, such as the
list of commits or of new operations.

## 4. Write the Org file

Put the file at `.tmp/pr-<number>-walkthrough.org` in the repository root. The user does not want
to keep it.

Start the file with the property drawer, before `#+title:`:

```org
:PROPERTIES:
:JJ_REPO: ~/Developer/<repo>
:END:
#+title: PR <number> walkthrough: <short description>
#+startup: content
```

Links are resolved relative to the Org file, so paths in `jj:` links start with `../`. The
`:file` parameter of a block is relative to the repository root.

Every block uses the same `:from "<base>" :to "<head>"`. Leave blocks empty; the script fills
them.

```org
#+BEGIN: jj-diff :from "<base>" :to "<head>" :file "path/from/repo/root.ts"
#+END:
```

Rules for dividing the diff into blocks:

- Every hunk must be in exactly one block.
- To show different hunks of one file in different sections, give each block a `:lines (START END)`
  range on the new side that covers only its hunks.
- One hunk cannot be split. When a single hunk contains several things (for example several new
  functions added together, or a whole new file), use one block and put a list of `jj:` links to
  the start of each part before it, with a line on what each part does.

Around the blocks, add `jj:` links at the head commit to related code the reader will want to
compare with, such as the existing functions a new one is modelled on, or the code a comment's
claim depends on. Check each line number with `jj file show -r <head>`.

Only write claims you have checked. If you have not read some code, say so rather than describe
it.

## 5. Fill the blocks and check coverage

```bash
nu ~/.claude/skills/org-jj/org-jj.nu update .tmp/pr-<number>-walkthrough.org
grep -n '^jj-diff:' .tmp/pr-<number>-walkthrough.org
```

`update` must print `Every hunk is shown exactly once`, and the `grep` must find nothing. If a hunk
is not shown or is shown twice, fix the `:lines` ranges and run `update` again.

## 6. Report

Tell the user where the file is, that coverage passed, and the reading order in a short list.
Mention anything that the user should know about but that is not in the document, such as tools
that were not available.
