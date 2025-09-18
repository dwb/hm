#!/usr/bin/env bash
#
# Matches a proposed Bash command against ../bash-policy.yml and turns ast-grep
# severities into hook decisions:
#
#   PreToolUse   error -> deny, warning -> ask
#   PostToolUse  info/hint -> additionalContext nudge, after the command has run
#
# PostToolUse carries the nudges because additionalContext is ignored on
# PreToolUse. Producing no output means "no decision", which leaves the call to
# the normal permission flow.

set -uo pipefail

# Commands whose allowlist entry in settings.json relies on this hook to close
# an arbitrary-execution hole. If the ruleset cannot be evaluated, these must
# not fall through to the allowlist.
readonly GUARDED_COMMANDS='fd|rg'

emit_deny() {
	jq -cn --arg reason "$1" \
		'{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: $reason}}'
}

main() {
	local here ruleset candidate input event cmd ruleset_text findings

	here=$(unset CDPATH && cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P) || here=
	ruleset=
	for candidate in "$here/../bash-policy.yml" "$HOME/.claude/bash-policy.yml"; do
		if [[ -n $candidate && -r $candidate ]]; then
			ruleset=$candidate
			break
		fi
	done

	input=$(cat)
	event=$(jq -r '.hook_event_name // ""' <<<"$input")
	cmd=$(jq -r '.tool_input.command // ""' <<<"$input")
	[[ -n $cmd ]] || exit 0

	if ! command -v ast-grep >/dev/null 2>&1 || [[ -z $ruleset ]]; then
		# Fail open in general, but fail closed for the allowlisted commands whose
		# safety depends on this ruleset running. Report the breakage either way:
		# silently disabling every rule is worse than being noisy about it.
		if [[ $event == PreToolUse ]]; then
			if [[ $cmd =~ ^[[:space:]]*($GUARDED_COMMANDS)[[:space:]] ]]; then
				emit_deny 'bash-policy could not evaluate this command: ast-grep or bash-policy.yml is missing. Commands that rely on it are blocked.'
			else
				jq -cn '{systemMessage: "bash-policy is inactive: ast-grep or bash-policy.yml could not be found."}'
			fi
		fi
		exit 0
	fi

	ruleset_text=$(<"$ruleset")
	findings=$(ast-grep scan --inline-rules "$ruleset_text" --stdin --json=compact <<<"$cmd" 2>/dev/null) || true
	[[ -n $findings && $findings != '[]' ]] || exit 0

	case $event in
	PreToolUse)
		jq -c '
			[.[] | select(.severity == "error" or .severity == "warning")]
			| unique_by(.ruleId) as $found
			| if ($found | length) == 0 then empty else
					(if any($found[]; .severity == "error") then "deny" else "ask" end) as $decision
					| ($found
							| map("- " + .message + (if .note then " " + .note else "" end))
							| join("\n")) as $detail
					| {hookSpecificOutput: {
							hookEventName: "PreToolUse",
							permissionDecision: $decision,
							permissionDecisionReason: ("bash-policy (CLAUDE.md shell rules):\n" + $detail)
						}}
				end
		' <<<"$findings"
		;;
	PostToolUse)
		jq -c '
			[.[] | select(.severity == "info" or .severity == "hint")]
			| unique_by(.ruleId) as $found
			| if ($found | length) == 0 then empty else
					($found
						| map("- " + .message + (if .note then " " + .note else "" end))
						| join("\n")) as $detail
					| {hookSpecificOutput: {
							hookEventName: "PostToolUse",
							additionalContext: ("bash-policy: this command was allowed to run, but it went against the CLAUDE.md shell rules below. Do not repeat the pattern; do not reply about this message.\n" + $detail)
						}}
				end
		' <<<"$findings"
		;;
	esac
}

main "$@"
