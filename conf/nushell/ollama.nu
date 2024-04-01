export def "ollama list" [] {
  ^ollama list | from tsv --flexible |
    transpose | each {
        update column0 { str downcase | str trim }} | transpose -i -r -d |
        each {
            update name { str trim | str replace --regex ':latest$' '' } |
                update size { into filesize } |
                update modified { into datetime  }
        }
}

export def "ollama ls" [] {
  ollama list
}

def complete-model [] {
  ollama list | get name
}

export extern "ollama run" [
  model: string@complete-model # repository:tag of model to use
  prompt?: string # Prompt to give, and then exit. Omit for an interactive session.
  --format: string # Respose format (e.g. json)
  --nowordwrap # Don't wrap words to the next line automatically
  --insecure # Use an insecure registry
  --verbose # Show timings for response
]

export extern "ollama create" [
  model: string@complete-model # repository:tag of model, can be new
  --file (-f): path # Path to Modelfile (default ./Modelfile)
]

export extern "ollama show" [
  model: string@complete-model # repository:tag of model
  --license
  --modelfile
  --parameters
  --system
  --template
]

export extern "ollama rm" [
  ...model: string@complete-model # repository:tag of model
]

export extern "ollama cp" [
  source: string@complete-model # repository:tag of source model
  target: string # repository:tag of target
]
