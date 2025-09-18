---
name: gh-pr
description: Fetch GitHub PR comments, identify the corresponding jj change, and assess each comment's validity.
argument-hint: "<pr-number>"
disable-model-invocation: true
effort: max
---

# GitHub PR Comment Review

Review comments on a GitHub pull request, assess their validity against the actual code, and present findings for the user to act on.

## Workflow

1. **Fetch PR data**: Run `nu ${CLAUDE_SKILL_DIR}/fetch-pr.nu $0` and read the output. This returns the PR title, branch name, description, and all comments (numbered, without author names).

2. **Find the jj change**: Invoke the `jj` skill, then run `jj bookmark list` to find a bookmark matching the PR's branch name (shown as `**Branch:**` in the output). If no bookmark matches, **ASK the user** which jj change corresponds to this PR — do not guess.

3. **Read the code**: Run `jj show <change> --git` to get the change's diff. For each file mentioned in inline comments, read the full file at the commented locations to understand surrounding context.

4. **Assess each comment**: For every numbered comment from the PR output:
   - Determine whether the comment is **Valid**, **Invalid**, or **Partially Valid** against the current state of the code.
   - If valid: sketch what the fix or improvement would look like.
   - If invalid: explain concisely why (e.g., already addressed, based on misunderstanding, out of scope).
   - If partially valid: identify which part holds and which doesn't.

5. **Handle stale line references**: PR comments reference line numbers from the diff at review time, which may no longer match. Search for the referenced code by content (grep/read) rather than trusting line numbers blindly.

6. **Present findings and STOP**: Format your assessment per the output format below, then **UNCONDITIONALLY STOP**. Do not make any code changes. Do not ask if you should fix things. Wait for the user to direct you.

## Output Format

```
## PR Comment Review: [PR title]

### Summary
[1-2 sentences: what the PR does and the overall tone of the review comments]

### Findings

**Comment 1** — [Valid/Invalid/Partially Valid]
[Assessment: why, and what to do about it if valid]

**Comment 2** — [Valid/Invalid/Partially Valid]
...

### Recommendations
[Which comments to address, suggested order, any that conflict with each other]
```

## After Presenting Findings

The user will then decide which comments to act on:
- They may dismiss some ("skip 3, that's intentional")
- They may ask for clarification ("explain 2 further")
- They may direct you to address specific ones ("fix 1 and 4")

Only after the user explicitly directs you should you make any code changes. Address only the comments the user has approved.
