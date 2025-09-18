#!/usr/bin/env nu

# Fetch GitHub PR data (title, description, comments) for review.
# Comments are numbered sequentially and author names are omitted to avoid bias.
# Inline comments come from GraphQL review threads with resolved threads filtered out.
# Limitation: first/per_page=100 without pagination; sufficient for virtually all PRs.

def parse-github-remote [url: string]: nothing -> record<owner: string, repo: string> {
  $url
  | parse -r '(?:https://github\.com/|git@github\.com:|ssh://git@github\.com/)(?P<owner>[^/]+)/(?P<repo>[^/.\s]+)'
  | get -o 0
  | default null
  | if $in != null {
      { owner: $in.owner, repo: ($in.repo | str replace -r '\.git$' '') }
    } else {
      null
    }
}

def find-github-remote []: nothing -> record<owner: string, repo: string> {
  let remotes = (^git remote -v | lines | parse -r '(?P<name>\S+)\s+(?P<url>\S+)\s+\(fetch\)')

  # Try origin first
  let origin = ($remotes | where name == origin | get -o 0)
  if $origin != null {
    let parsed = (parse-github-remote $origin.url)
    if $parsed != null {
      return $parsed
    }
  }

  # Fall back to any GitHub remote; require exactly one
  let github_remotes = ($remotes | each { |r|
    let parsed = (parse-github-remote $r.url)
    if $parsed != null { $parsed } else { null }
  } | compact)

  if ($github_remotes | length) == 0 {
    error make { msg: "No GitHub remote found" }
  }
  if ($github_remotes | length) > 1 {
    error make { msg: "Multiple GitHub remotes found; cannot determine which to use" }
  }
  $github_remotes.0
}

def gh-api [endpoint: string]: nothing -> any {
  ^gh api $endpoint -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" | from json
}

def fetch-unresolved-inline-comments [owner: string, repo: string, pr_num: int]: nothing -> list<any> {
  let query = "query($owner: String!, $repo: String!, $number: Int!) { repository(owner: $owner, name: $repo) { pullRequest(number: $number) { reviewThreads(first: 100) { nodes { isResolved comments(first: 100) { nodes { body path line originalLine startLine originalStartLine replyTo { databaseId } } } } } } } }"
  let result = (^gh api graphql -f $"query=($query)" -f $"owner=($owner)" -f $"repo=($repo)" -F $"number=($pr_num)" | from json)
  $result.data.repository.pullRequest.reviewThreads.nodes
  | where isResolved == false
  | each { |thread|
      $thread.comments.nodes | each { |c|
        {
          body: $c.body,
          path: $c.path,
          line: $c.line,
          original_line: $c.originalLine,
          start_line: $c.startLine,
          original_start_line: $c.originalStartLine,
          in_reply_to_id: ($c.replyTo | get -o databaseId),
        }
      }
    }
  | flatten
}

def format-location [comment: record]: nothing -> string {
  let line = ($comment | get -o line | default ($comment | get -o original_line))
  let start = ($comment | get -o start_line | default ($comment | get -o original_start_line))
  if $line == null {
    $comment.path
  } else if $start != null and $start != $line {
    $"($comment.path):($start)-($line)"
  } else {
    $"($comment.path):($line)"
  }
}

def main [pr_num: int] {
  let remote = (find-github-remote)
  let base = $"/repos/($remote.owner)/($remote.repo)"

  let pr = (gh-api $"($base)/pulls/($pr_num)")
  let inline_comments = (fetch-unresolved-inline-comments $remote.owner $remote.repo $pr_num)
  let discussion_comments = (gh-api $"($base)/issues/($pr_num)/comments?per_page=100")
  let reviews = (gh-api $"($base)/pulls/($pr_num)/reviews?per_page=100"
    | where { ($in | get -o body | default "") != "" })

  mut counter = 0
  let body = ($pr | get -o body | default "" | str trim)

  # Header
  print $"# PR #($pr_num): ($pr.title)\n"
  print $"**Branch:** ($pr.head.ref)\n"

  # Description
  print "## Description\n"
  if $body == "" {
    print "(No description)\n"
  } else {
    print $"($body)\n"
  }

  # Review bodies
  if ($reviews | length) > 0 {
    print "## Review Bodies\n"
    for review in $reviews {
      $counter += 1
      print $"**($counter).** ($review.body)\n"
    }
  }

  # Inline comments
  if ($inline_comments | length) > 0 {
    print "## Inline Comments\n"
    for comment in $inline_comments {
      $counter += 1
      let reply_marker = if ($comment | get -o in_reply_to_id) != null { " (reply)" } else { "" }
      let location = (format-location $comment)
      print $"**($counter).($reply_marker)** `($location)`"
      print $"($comment.body)\n"
    }
  }

  # Discussion comments
  if ($discussion_comments | length) > 0 {
    print "## Discussion Comments\n"
    for comment in $discussion_comments {
      $counter += 1
      print $"**($counter).** ($comment.body)\n"
    }
  }
}
