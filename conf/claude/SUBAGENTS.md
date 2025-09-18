# CRITICAL USAGE INSTRUCTIONS

Be VERY DILIGENT in following these, they are CRITICAL for correct operaion.

* When researching source code directly from GitHub, always use the "raw" URLs, that usually start with https://raw.githubusercontent.com/ . To explore public repositories, use the public API at https://api.github.com/
* Always prefer your `Glob` and `Grep` tools over running equivalent Bash commands.
* When writing Bash commands, be sure to write commands that would pass Shellcheck and that give you enough information to proceed. In particular, quote everything properly (including arguments you think are safe), and ensure file names are always handled safely.
  * For example, write `find . -type f -exec grep foo {} +` instead of piping the result of `find` to `grep`.
    * When using `find` be careful of it returning too much! Usually prefer `fd` as it honours VCS ignore lists.
  * Never pipe from `ls`. Use globs, for-loops, `find [...] -exec`, or other construct that deals with unusual file names safely.
  * When redirecting stdout and stderr, remember the order matters! `cmd >output.txt 2>&1` correctly redirects both stdout and stderr to the file. Swapping the redirections will not.
  * When running tests, linters, or other checkers, ALWAYS append `|| true` to the command. This will mean that the Claude Code harness handles arbitrarily large output for you and you won't need to filter it yourself or re-run the (probably expensive) command.
  * Combined with this, STRONGLY PREFER returning the whole output of a command. The Claude Code harness automatically handles large outputs for you, as long as the command exits successfully. Prefer to get the full output and then filter it down later if necessary - otherwise you end up just re-running expensive processes.
* When writing ad-hoc commands to process data, STRONGLY prefer small, compositional tools like `jq` and the standard core utils and friends, over writing programs in general-purpose lanuages like Python. This makes it much easier to verify that your commands are safe, and may mean they are auto-accepted, resulting in a much more pleasant workflow for all.
* When running Bash commands, note that a command running successfully with no output is, by very strong convention, a signal of success. For a typecheck, for example, this means no type errors. The Claude Code harness will tell you if a command exited with a non-zero return code; there is no need to print it out explicitly unless you are looking for a specific code.
