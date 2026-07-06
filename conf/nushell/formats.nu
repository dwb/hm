def "from ndjson" []: string -> list<record> {
  from json -o
}

def "to ndjson" []: list<record> -> string {
  each { |r| $"($r | to json --raw)\n" } | str join
}

def "from jsonl" []: string -> list<record> {
  from json -o
}

def "to jsonl" []: list<record> -> string {
  each { |r| $"($r | to json --raw)\n" } | str join
}
