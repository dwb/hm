# EAT Terminal Integration for Nushell
#
# This module provides comprehensive integration between Nushell and the Emacs EAT
# terminal emulator, implementing the full EAT communication protocol.
#
# Usage: Add `source eat-integration.nu` to your Nushell config file
# or run `use eat-integration.nu` to import the module

export module eat {

    # Check if we're running in an EAT terminal
    export def is-eat-terminal [] -> bool {
        let term = ($env.TERM? | default "")
        $term starts-with "eat-"
    }

    # Encode a string as base64 for safe transmission
    def encode-safe [text: string] -> string {
        $text | encode base64 | str trim
    }

    # Build an EAT escape sequence string
    def build-eat-sequence [code: string, ...args: string] -> string {
        mut sequence = $"\e]51;e;($code)"
        for arg in $args {
            $sequence = $sequence + $";($arg)"
        }
        $sequence + "\e\\"
    }

    # Send an EAT escape sequence
    def send-eat-sequence [code: string, ...args: string] {
        build-eat-sequence $code ...$args | print -n $in
    }

    # Send exit status to EAT terminal
    export def send-exit-status [status: int] {
        send-eat-sequence "H" $status
    }

    # Signal new prompt to EAT terminal
    export def send-new-prompt [] {
        send-eat-sequence "J"
    }

    # Send current directory info to EAT terminal
    export def send-directory-info [] {
        let hostname = (sys host | get hostname)
        let pwd = $env.PWD
        send-eat-sequence "A" (encode-safe $hostname) (encode-safe $pwd)
    }

    # Send current command to EAT terminal
    export def send-current-command [command: string] {
        send-eat-sequence "F" (encode-safe $command)
    }

    # Send pre-execution signal
    export def send-pre-execution [] {
        send-eat-sequence "G"
    }

    # Update terminal title with current state
    export def update-title [command?: string] {
        let username = ($env.USER? | default (whoami))
        let hostname = (sys host | get hostname)
        let pwd_display = ($env.PWD | str replace $env.HOME "~")
        let prompt_char = (if ($env.UID? | default 1000) == 0 { "#" } else { "$" })

        let title = if ($command | is-empty) {
            $"($username)@($hostname):($pwd_display)($prompt_char)"
        } else {
            $"($username)@($hostname):($pwd_display)($prompt_char) ($command)"
        }

        print -n $"\e]2;($title)\e\\"
    }

    # Send history to EAT terminal
    export def send-history [] {
        let hostname = (sys host | get hostname)
        let histfile = ($nu.history-path | path expand)

        # Initial history request
        send-eat-sequence "I" "0" "nushell" (encode-safe $hostname) (encode-safe $histfile)

        # For now, we'll send a simple implementation
        # A more sophisticated implementation would handle the response
        try {
            let recent_history = (history | last 100 | get command | str join "\n")
            send-eat-sequence "I" "1" "nushell" (encode-safe $recent_history)
        } catch {
            # If history fails, continue silently
        }
    }

    # Create prompt markers for EAT
    export def get-prompt-start [] -> string {
        build-eat-sequence "B"
    }

    export def get-prompt-end [] -> string {
        build-eat-sequence "C"
    }

    export def get-continuation-start [] -> string {
        build-eat-sequence "D"
    }

    export def get-continuation-end [] -> string {
        build-eat-sequence "E"
    }

    # Send a message to EAT (for debugging/extensions)
    export def send-message [...messages: string] {
        mut args = []
        for msg in $messages {
            $args = ($args | append (encode-safe $msg))
        }
        send-eat-sequence "M" ...$args
    }

    # Pre-prompt hook function
    export def pre-prompt-hook [] {
        if not (is-eat-terminal) { return }

        # Send exit status if we have a previous command
        if ($env.EAT_LAST_COMMAND? | is-not-empty) {
            let exit_status = ($env.LAST_EXIT_CODE? | default 0)
            send-exit-status $exit_status
        }

        # Clear the last command marker
        $env.EAT_LAST_COMMAND = ""

        # Send new prompt signal
        send-new-prompt

        # Send current directory info
        send-directory-info

        # Update terminal title
        update-title
    }

    # Pre-execution hook function
    export def pre-execution-hook [] {
        if not (is-eat-terminal) { return }

        # Get the current command from commandline
        let current_command = (commandline | str trim)
        if ($current_command | is-empty) { return }

        # Store it for later use
        $env.EAT_LAST_COMMAND = $current_command

        # Send current command to terminal
        send-current-command $current_command

        # Send pre-execution signal
        send-pre-execution

        # Update terminal title with running command
        update-title $current_command
    }

    # Environment change hook for PWD
    export def pwd-change-hook [before: string, after: string] {
        if not (is-eat-terminal) { return }

        # Send updated directory info
        send-directory-info

        # Update terminal title if no command is running
        if ($env.EAT_LAST_COMMAND? | is-empty) {
            update-title
        }
    }

    # Enhanced prompt command that includes EAT markers
    export def create-eat-prompt [] -> string {
        if not (is-eat-terminal) {
            return ($env.PROMPT_COMMAND_RIGHT? | default "")
        }

        let start_marker = get-prompt-start
        let end_marker = get-prompt-end

        # Get the base prompt (you can customize this)
        let base_prompt = (
            create_left_prompt 2>/dev/null | complete | get stdout | default (
                $"(ansi green_bold)($env.PWD | path basename)(ansi reset) (ansi blue)>(ansi reset) "
            )
        )

        $"($start_marker)($base_prompt)($end_marker)"
    }

    # Initialize EAT integration
    export def setup [] {
        if not (is-eat-terminal) {
            print "EAT terminal not detected. Integration not enabled."
            return
        }

        print "Setting up EAT terminal integration..."

        # Initialize environment variables
        $env.EAT_INTEGRATION_ENABLED = "yes"
        $env.EAT_LAST_COMMAND = ""

        # Set up hooks in config
        $env.config = ($env.config | upsert hooks.pre_prompt [
            { eat pre-prompt-hook }
        ])

        $env.config = ($env.config | upsert hooks.pre_execution [
            { eat pre-execution-hook }
        ])

        $env.config = ($env.config | upsert hooks.env_change.PWD [
            {|before, after| eat pwd-change-hook $before $after }
        ])

        # Enhanced prompt with EAT markers
        # Note: This is optional - users may prefer their existing prompt
        # $env.PROMPT_COMMAND = { eat create-eat-prompt }

        # Send initial history
        send-history

        print "EAT integration enabled successfully!"
    }

    # Disable EAT integration
    export def disable [] {
        $env.EAT_INTEGRATION_ENABLED = ""

        # Note: Removing hooks requires more sophisticated config manipulation
        # For now, we just set the flag to disable the hook functions
        print "EAT integration disabled."
    }

    # Status check
    export def status [] -> record<enabled: bool, term: string, version: string> {
        {
            enabled: ($env.EAT_INTEGRATION_ENABLED? | default "" | is-not-empty),
            term: ($env.TERM? | default ""),
            version: "1.0.0"
        }
    }

    # Manual directory sync (useful for testing)
    export def sync-directory [] {
        if not (is-eat-terminal) {
            print "Not in EAT terminal"
            return
        }
        send-directory-info
        update-title
    }

    # Manual command simulation (for testing)
    export def simulate-command [cmd: string] {
        if not (is-eat-terminal) {
            print "Not in EAT terminal"
            return
        }
        send-current-command $cmd
        send-pre-execution
        update-title $cmd
    }
}

# Auto-setup when sourced (not when used as module)
export def main [] {
    if (eat is-eat-terminal) {
        eat setup
    }
}

# Helper commands at top level for convenience
export def e [filepath: path] {
    if not (eat is-eat-terminal) {
        print "EAT integration not available"
        return
    }
    eat send-message "find-file" $filepath
}

export def eat-clear [] {
    if (eat is-eat-terminal) {
        eat send-message "vterm-clear-scrollback"
        clear
    } else {
        clear
    }
}
