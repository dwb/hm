#!/usr/bin/env bash
#
# Tests for bash-policy.yml and bash-policy.sh.
#
# Section 1 asserts which rule ids a command triggers, which catches rules that
# match for the wrong reason as well as rules that fail to match.
# Section 2 asserts the severity-to-decision mapping through the hook itself.
#
# Run: ./bash-policy.test.sh

set -uo pipefail

here=$(unset CDPATH && cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P) || exit 1
readonly RULESET="$here/../bash-policy.yml"
readonly HOOK="$here/bash-policy.sh"

failures=0
ruleset_text=$(<"$RULESET")

# rule_ids COMMAND -> sorted, comma-separated ids of the rules that matched
rule_ids() {
	ast-grep scan --inline-rules "$ruleset_text" --stdin --json=compact <<<"$1" 2>/dev/null |
		jq -r '[.[].ruleId] | unique | join(",")'
}

expect_rules() {
	local cmd=$1 want=$2 got
	got=$(rule_ids "$cmd")
	if [[ $got != "$want" ]]; then
		printf 'FAIL rules: %s\n  want: %s\n  got:  %s\n' "$cmd" "${want:-<none>}" "${got:-<none>}" >&2
		((failures++))
	fi
}

# expect_decision EVENT COMMAND WANT   (WANT is deny, ask, nudge, or none)
expect_decision() {
	local event=$1 cmd=$2 want=$3 out got
	out=$(jq -cn --arg e "$event" --arg c "$cmd" \
		'{hook_event_name: $e, tool_name: "Bash", tool_input: {command: $c}}' |
		"$HOOK")
	if [[ -z $out ]]; then
		got=none
	else
		got=$(jq -r '.hookSpecificOutput
			| if has("additionalContext") then "nudge" else .permissionDecision end' <<<"$out")
	fi
	if [[ $got != "$want" ]]; then
		printf 'FAIL decision (%s): %s\n  want: %s\n  got:  %s\n' "$event" "$cmd" "$want" "$got" >&2
		((failures++))
	fi
}

# --- Section 1: rule matching ------------------------------------------------

expect_rules 'cat foo.txt' 'read-file-with-cat'
expect_rules 'cat foo.txt | jq .' 'read-file-with-cat'
expect_rules 'sort x | cat -A' ''
expect_rules 'head -20 file.txt' 'read-file-with-pager'
expect_rules 'tail -f log.txt' 'read-file-with-pager'
expect_rules 'sort x | head -20' 'truncating-pipeline-output'
expect_rules 'ls | grep foo' 'pipe-from-ls'
expect_rules 'ls -la' ''
expect_rules "find . -name '*.ts' | xargs grep foo" 'pipe-from-find'
expect_rules 'find . -type f -exec grep foo {} +' ''
expect_rules 'fd -x rm' 'fd-exec'
expect_rules 'fd -X echo' 'fd-exec'
expect_rules 'fd --exec-batch echo' 'fd-exec'
expect_rules 'fd --exec=rm .' 'fd-exec'
expect_rules 'fd -e ts src' ''
expect_rules 'rg --pre=cat foo' 'rg-exec'
expect_rules 'rg --hostname-bin uname foo' 'rg-exec'
expect_rules 'rg foo src' ''
expect_rules 'cd /tmp' 'cd-unguarded'
expect_rules 'cd /tmp || exit 1' 'cd-guarded'
expect_rules 'cd /tmp && ls -la' 'cd-guarded'
expect_rules "python3 -c 'print(1)'" 'inline-python'
expect_rules 'python --version' ''
expect_rules 'grep -rn foo .' 'grep-for-code-search'
expect_rules 'grep -Rl foo .' 'grep-for-code-search'
expect_rules 'grep -in foo x' ''
expect_rules 'fd -Hx rm .' 'fd-exec'
expect_rules 'grep -m 5 foo x' 'truncating-search-output'
expect_rules 'make test' 'missing-or-true'
expect_rules 'make test || true' ''
expect_rules 'echo hi > out.txt' 'write-file-with-shell'
expect_rules 'echo hi > /dev/null' ''
expect_rules 'jj log -r @' ''
expect_rules 'ast-grep run --pattern foo --lang bash .' ''

# --- Section 2: severity to decision ----------------------------------------

expect_decision PreToolUse 'cat foo.txt' deny
expect_decision PreToolUse 'fd -x rm' deny
expect_decision PreToolUse 'cd /tmp' ask
expect_decision PreToolUse 'sort x | head -5' none
expect_decision PreToolUse 'jj log -r @' none
expect_decision PostToolUse 'sort x | head -5' nudge
expect_decision PostToolUse 'make test' nudge
expect_decision PostToolUse 'cat foo.txt' none
expect_decision PostToolUse 'jj log -r @' none

# --- Result ------------------------------------------------------------------

if ((failures > 0)); then
	printf '\n%d failure(s)\n' "$failures" >&2
	exit 1
fi
printf 'all bash-policy tests passed\n'
