#!/usr/bin/env nu

# Pin each GitHub flake input to its latest commit before a given date, then
# run `nix flake update` with --override-input flags to lock to those commits.
def main [
    --days-ago: int = 7               # Days in the past to pin to (ignored if --date given)
    --date: string = ""               # Target datetime in ISO 8601 (e.g. 2026-05-01T00:00:00Z)
    --dry-run                         # Print the nix command without running it
    --lock-file: string = "flake.lock"
] {
    let target = if ($date | is-not-empty) {
        $date
    } else {
        (date now) - ($days_ago * 1day) | format date "%Y-%m-%dT%H:%M:%SZ"
    }

    print $"Pinning GitHub inputs to commits before ($target)"

    let lock = open $lock_file | from json

    let github_inputs = $lock.nodes
        | items { |name, node| {name: $name, node: $node} }
        | where { |it| $it.name != "root" and ($it.node | get -o locked | get -o type) == "github" }

    let overrides = $github_inputs | each { |it|
        let owner = $it.node.locked.owner
        let repo  = $it.node.locked.repo
        let ref   = ($it.node.original | get -o ref)

        let query = if ($ref != null) {
            $"sha=($ref)&until=($target)&per_page=1"
        } else {
            $"until=($target)&per_page=1"
        }
        let url = $"https://api.github.com/repos/($owner)/($repo)/commits?($query)"

        let ref_display = $ref | default "default branch"
        print $"  ($it.name): ($owner)/($repo) \(($ref_display)\)"

        let commits = http get -H [
            Accept 'application/vnd.github+json'
            X-GitHub-Api-Version '2022-11-28'
        ] $url

        if ($commits | is-empty) {
            error make { msg: $"No commits found for ($owner)/($repo) before ($target)" }
        }

        let sha = $commits | get 0.sha
        print $"    -> ($sha)"

        {name: $it.name, spec: $"github:($owner)/($repo)/($sha)"}
    }

    let override_args = $overrides | each { |it|
        ["--override-input" $it.name $it.spec]
    } | flatten

    let input_names = $overrides | get name

    if $dry_run {
        print $"nix flake update ($override_args | str join ' ') ($input_names | str join ' ')"
    } else {
        run-external "nix" "flake" "update" ...$override_args ...$input_names
    }
}
