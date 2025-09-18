---
name: code-review
description: Structured code review of a jj change with numbered findings for discussion.
argument-hint: "[change-id] [extra-intructions]"
disable-model-invocation: true
effort: max
---

# Code Review

Review the code change at jj change `$0`. If no change was specified, the target is `@`.

## Pre-Review Setup

1. **Invoke the jj skill** before running any jj commands.
2. **Fetch the change**: Run `jj show <change> --git` on the target change (the argument `$0`, or `@` if none was given). Also run `jj lt <change>` (or `jj log` if `jj lt` fails) to see the change in context.
3. **Identify the change**: The diff from `jj show` is the code to review. If the description is empty or is a megamerge marker (e.g. `private: WORKING`), that is normal — it just means the work hasn't been described yet. In a megamerge at `@`, the diff may contain changes across unrelated areas. Focus on changes in the main source directories and ignore peripheral concerns (repository setup, meta-settings, config boilerplate, etc.).
4. **Ticket resolution**: Look for ticket identifiers in the change description (patterns like `ENG-123`, `PROJ-456`, or similar `LETTERS-DIGITS` patterns).
   - **If a ticket is found**: Use MCP tools (try `linear-server`, then Atlassian MCP servers) to fetch the issue details (title, description, acceptance criteria), the project it belongs to (including the project description), and follow enough references (linked issues, parent issues, epics) to understand the requirements. Review the code against these requirements in addition to general code quality. **If you cannot fetch the ticket** (MCP server unavailable, not authenticated, ticket not found, or tools don't return the expected data), **STOP and report the problem to the user.** A ticket reference that can't be resolved needs to be fixed before the review can proceed — the user may need to connect the MCP server, authenticate, or correct the ticket number.
   - **If no ticket is found**: Review the code on its own terms. A code review without ticket context is valuable — focus entirely on code quality, correctness, and codebase fit. Do not treat the absence of a ticket as a problem.

## Review Methodology

Perform a thorough, adversarial review in two passes. Read the changed files in full (not just the diff hunks) to understand surrounding context. Use Grep and Glob to find callers, consumers, tests, and related code.

### Pass 1: Line-by-Line

For each file in the diff, review for:
- **Logic errors**: off-by-ones, null/undefined handling, incorrect conditionals, race conditions
- **Security**: injection vulnerabilities (SQL, command, XSS), unsafe deserialization, secrets in code, path traversal — OWASP top 10 where applicable
- **Error handling**: unhandled errors, swallowed exceptions, missing validation at system boundaries
- **Resource management**: leaks, unclosed handles, missing cleanup
- **Naming and style**: inconsistency with surrounding code, misleading names, violations of the codebase's conventions
- **Comments**: misleading comments, comments that just restate the code, missing comments where the logic is non-obvious

### Pass 2: Architectural

Zoom out and consider:
- **Codebase fit**: Does the change follow existing patterns? Does it introduce a new pattern where an existing one would serve? Are there existing utilities that should have been reused?
- **Missing updates**: If an interface changed, have all consumers been updated? Are there tests that should have been added or updated? Are there documentation references that are now stale?
- **Scope**: Is the change appropriately sized? Does it mix unrelated concerns that should be separate commits? Is a large change justified by the task, or could it be smaller?
- **API and contract changes**: Are public interfaces treated with appropriate care? Are breaking changes flagged? Is backward compatibility handled (or deliberately not, with justification)?
- **Lifecycle stage**: Is this code pre-merge (where refactoring is cheap) or post-merge (where stability matters more)? Adjust review strictness accordingly.

### Pass 3: Validation

Is there enough verification that the changes are correct?

On tests:
- Are there automated tests?
- Are the tests comprehensive enough?
- It may be acceptable to not write tests if something is very hard to test, or the tests that could be practically written are not valuable enough, but this should be well justifyable.

The type system is also a vital part of validation:
- Is the type system being well used?
- Are there any unsafe constructs like unchecked casts, or unjustified escape-hatches?

### Trade-Off Analysis

Identify any design trade-offs in the change. For each:
- Is the trade-off reasonable given the context?
- Is it documented or obvious from the code?
- If not obvious, suggest either a code comment or a note for the commit message.

## Output Format

Structure your review as follows. Every finding MUST have a number so it can be referenced in discussion.

```
## Code Review: [brief description of the change]

### Summary
[1-2 sentences: what the change does and why]

### Ticket
[Ticket reference and brief summary of requirements, or "No ticket associated" — either is fine]

### Findings

**1. [Category: Severity] Title**
File: path/to/file.ext:NN
[Description of the finding, why it matters, and a concrete suggestion]

**2. [Category: Severity] Title**
...

### Observations
[Things that are fine but worth noting: trade-offs understood, good patterns, scope notes, etc.]

### Commit Message Notes
[Key facts for writing a good commit message later:
 - What changed (files, interfaces, behaviour)
 - Why it changed (ticket, bug, feature, refactor)
 - Notable trade-offs or decisions
 - Anything a future reader should know]
```

**Categories:** Logic, Style, Security, Performance, Consistency, Missing, Scope, Documentation
**Severities:**
- `must-fix` — correctness or security issue; should not be committed as-is
- `should-fix` — significant quality issue; strong recommendation to address
- `consider` — reasonable suggestion; may be dismissed with justification
- `nit` — minor style or preference issue

If there are no findings, say so explicitly. An empty findings section is a valid and good outcome.

## After the Review

**CRITICAL: After presenting the review, UNCONDITIONALLY STOP.** Do not proceed to fix anything. Do not ask if you should fix things. Simply present the review and wait.

The user will then discuss the findings:
- They may dismiss some findings (e.g. "dismiss 3, that's intentional")
- They may ask for clarification (e.g. "explain 2 further")
- They may ask you to elaborate on alternatives
- They may eventually sign off (e.g. "fix 1 and 4", or "looks good, address the must-fixes")

Only after the user explicitly directs you to address findings should you make any code changes. Address only the findings the user has approved.

## Extra Instructions

If the user has any extra instructions for this particular code review, they will be here. They can override any of the above, but will most likely just include particular things to watch out for:

<extra-instructions>
$1
</extra-instructions>
