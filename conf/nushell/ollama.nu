# List models
export def "ollama list" [] {
  ^ollama list | from ssv --aligned-columns |
    rename --block { str downcase | str trim } |
    update name { str trim | str replace --regex ':latest$' '' } |
    update size { into filesize } |
    update modified { into datetime  }
}

# List models
export def "ollama ls" [] {
  ollama list
}

def complete-model [] {
  ollama list | get name
}

# Run a model
export extern "ollama run" [
  model: string@complete-model # repository:tag of model to use
  prompt?: string # Prompt to give, and then exit. Omit for an interactive session.
  --format: string # Respose format (e.g. json)
  --nowordwrap # Don't wrap words to the next line automatically
  --insecure # Use an insecure registry
  --verbose # Show timings for response
]

# Create a model
export extern "ollama create" [
  model: string@complete-model # repository:tag of model, can be new
  --file (-f): path # Path to Modelfile (default ./Modelfile)
  --quantize (-q): string # Quantize model to this level (e.g. q4_0)
]

# Show information for a model
export extern "ollama show" [
  model: string@complete-model # repository:tag of model
  --license
  --modelfile
  --parameters
  --system
  --template
]

# Stop running a model
export extern "ollama stop" [
  model: string@complete-model # repository:tag of model
]

# List running models
export extern "ollama ps" []

# Remove a model
export extern "ollama rm" [
  ...model: string@complete-model # repository:tag of model
]

# Copy a model
export extern "ollama cp" [
  source: string@complete-model # repository:tag of source model
  target: string # repository:tag of target
]

# Pull a model from a registry
export extern "ollama pull" [
  model: string # repository:tag of model
  --insecure # Use an insecure registry
]

# Push a model to a registry
export extern "ollama push" [
  model: string # repository:tag of model
  --insecure # Use an insecure registry
]
