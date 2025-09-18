# export module eat {

    # Check if we're running in an EAT terminal
    export def is-eat-terminal []: nothing -> bool {
        let term = ($env.TERM? | default "")
        $term starts-with "eat-"
    }

    # Encode a string as base64 for safe transmission
    def encode-safe [text: string]: nothing -> string {
        $text | encode base64 | str trim
    }

    # Build an EAT escape sequence string
    def build-eat-sequence [code: string, ...args: string]: nothing -> string {
        mut sequence = $"\e]51;e;($code)"
        for arg in $args {
            $sequence = $sequence + $";($arg)"
        }
        $sequence + "\e\\"
    }

    # Send an EAT escape sequence
    def send-eat-sequence [code: string, ...args: string]: nothing -> nothing {
        build-eat-sequence $code ...$args | print -n $in
    }

    # Send exit status to EAT terminal
    export def send-exit-status [status: int]: nothing -> nothing {
      send-eat-sequence "H" ($status | format number | get display)
    }

    # Signal new prompt to EAT terminal
    export def send-new-prompt []: nothing -> nothing {
        send-eat-sequence "J"
    }

    # Send current directory info to EAT terminal
    export def send-directory-info []: nothing -> nothing {
        let hostname = (sys host | get hostname)
        let pwd = $env.PWD
        send-eat-sequence "A" (encode-safe $hostname) (encode-safe $pwd)
    }

    # Send current command to EAT terminal
    export def send-current-command [command: string]: nothing -> nothing {
        send-eat-sequence "F" (encode-safe $command)
    }

    # Send pre-execution signal
    export def send-pre-execution []: nothing -> nothing {
        send-eat-sequence "G"
    }

    # Update terminal title with current state
    export def update-title [command?: string]: nothing -> nothing {
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
    export def send-history []: nothing -> nothing {
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
    export def get-prompt-start []: nothing -> string {
        build-eat-sequence "B"
    }

    export def get-prompt-end []: nothing -> string {
        build-eat-sequence "C"
    }

    export def get-continuation-start []: nothing -> string {
        build-eat-sequence "D"
    }

    export def get-continuation-end []: nothing -> string {
        build-eat-sequence "E"
    }

    # Send a message to EAT (for debugging/extensions)
    export def send-message [...messages: string]: nothing -> nothing {
        mut args = []
        for msg in $messages {
            $args = ($args | append (encode-safe $msg))
        }
        send-eat-sequence "M" ...$args
    }

    # Pre-prompt hook function
    export def pre-prompt-hook []: nothing -> nothing {
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
    export def pre-execution-hook []: nothing -> nothing {
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
    export def pwd-change-hook [before: string, after: string]: nothing -> nothing {
        if not (is-eat-terminal) { return }

        # Send updated directory info
        send-directory-info

        # Update terminal title if no command is running
        if ($env.EAT_LAST_COMMAND? | is-empty) {
            update-title
        }
    }

    # Wrap existing prompt with EAT markers
    def wrap-prompt []: nothing -> string {
        if not (is-eat-terminal) {
            # Not in EAT, use original prompt
            if ($env.EAT_ORIGINAL_PROMPT? | is-not-empty) {
                do $env.EAT_ORIGINAL_PROMPT
            } else {
                ""
            }
        } else {
            let start_marker = get-prompt-start
            let end_marker = get-prompt-end

            # Execute the original prompt command and wrap it
            let original_output = if ($env.EAT_ORIGINAL_PROMPT? | is-not-empty) {
                do $env.EAT_ORIGINAL_PROMPT
            } else {
                ""
            }

            $"($start_marker)($original_output)($end_marker)"
        }
    }

    # Wrap existing multiline indicator with EAT markers
    def wrap-multiline-indicator []: nothing -> string {
        if not (is-eat-terminal) {
            # Not in EAT, use original indicator
            $env.EAT_ORIGINAL_MULTILINE_INDICATOR? | default ""
        } else {
            let start_marker = get-continuation-start
            let end_marker = get-continuation-end

            # Get the original multiline indicator
            let original_indicator = $env.EAT_ORIGINAL_MULTILINE_INDICATOR? | default ""

            $"($start_marker)($original_indicator)($end_marker)"
        }
    }

    # Initialize EAT integration
    export def --env setup []: nothing -> nothing {
        if not (is-eat-terminal) {
            print "EAT terminal not detected. Integration not enabled."
            return
        }

        print "Setting up EAT terminal integration..."

        # Initialize environment variables
        $env.EAT_INTEGRATION_ENABLED = true
        $env.EAT_LAST_COMMAND = ""

        # Save existing prompt and wrap it with EAT markers
        if ($env.PROMPT_COMMAND? | is-not-empty) {
            $env.EAT_ORIGINAL_PROMPT = $env.PROMPT_COMMAND
            $env.PROMPT_COMMAND = { wrap-prompt }
        }

        # Save existing multiline indicator and wrap it with EAT markers
        if ($env.PROMPT_MULTILINE_INDICATOR? | is-not-empty) {
            $env.EAT_ORIGINAL_MULTILINE_INDICATOR = $env.PROMPT_MULTILINE_INDICATOR
        } else {
            # Save the default if none is set
            $env.EAT_ORIGINAL_MULTILINE_INDICATOR = "::: "
        }
        $env.PROMPT_MULTILINE_INDICATOR = { wrap-multiline-indicator }

        # Set up hooks in config
        $env.config = ($env.config | upsert hooks.pre_prompt [
            { pre-prompt-hook }
        ])

        $env.config = ($env.config | upsert hooks.pre_execution [
            { pre-execution-hook }
        ])

        $env.config = ($env.config | upsert hooks.env_change.PWD [
            {|before, after| pwd-change-hook ($before | default "") $after }
        ])

        # Send initial history
        # send-history

        print "EAT integration enabled successfully!"
    }

    # Disable EAT integration
    export def --env disable []: nothing -> nothing {
        $env.EAT_INTEGRATION_ENABLED = false

        # Restore original prompt if we saved it
        if ($env.EAT_ORIGINAL_PROMPT? | is-not-empty) {
            $env.PROMPT_COMMAND = $env.EAT_ORIGINAL_PROMPT
            $env.EAT_ORIGINAL_PROMPT = null
        }

        # Restore original multiline indicator if we saved it
        if ($env.EAT_ORIGINAL_MULTILINE_INDICATOR? | is-not-empty) {
            $env.PROMPT_MULTILINE_INDICATOR = $env.EAT_ORIGINAL_MULTILINE_INDICATOR
            $env.EAT_ORIGINAL_MULTILINE_INDICATOR = null
        }

        # Note: Removing hooks requires more sophisticated config manipulation
        # For now, we just set the flag to disable the hook functions
        print "EAT integration disabled. Prompt and multiline indicator restored."
    }

    # Status check
    export def status []: nothing -> record<enabled: bool, term: string, version: string> {
        {
            enabled: ($env.EAT_INTEGRATION_ENABLED? | default false),
            term: ($env.TERM? | default ""),
            version: "1.0.0"
        }
    }

    # Manual directory sync (useful for testing)
    export def sync-directory []: nothing -> nothing {
        if not (is-eat-terminal) {
            print "Not in EAT terminal"
            return
        }
        send-directory-info
        update-title
    }

    # Manual command simulation (for testing)
    export def simulate-command [cmd: string]: nothing -> nothing {
        if not (is-eat-terminal) {
            print "Not in EAT terminal"
            return
        }
        send-current-command $cmd
        send-pre-execution
        update-title $cmd
    }
# }


# # Auto-setup when sourced (not when used as module)
# export def main []: nothing -> nothing {
#     if (eat is-eat-terminal) {
#         eat setup
#     }
# }

# # Helper commands at top level for convenience
# export def e [filepath: path]: nothing -> nothing {
#     if not (eat is-eat-terminal) {
#         print "EAT integration not available"
#         return
#     }
#     eat send-message "find-file" $filepath
# }

# export def eat-clear []: nothing -> nothing {
#     if (eat is-eat-terminal) {
#         eat send-message "vterm-clear-scrollback"
#         clear
#     } else {
#         clear
#     }
# }
