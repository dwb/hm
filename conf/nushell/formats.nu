export def "from ndjson" []: string -> list<record> {
  from json -o
}

export def "to ndjson" []: list<record> -> string {
  each { |r| $"($r | to json --raw)\n" } | str join
}

export def "from jsonl" []: string -> list<record> {
  from json -o
}

export def "to jsonl" []: list<record> -> string {
  each { |r| $"($r | to json --raw)\n" } | str join
}
