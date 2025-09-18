export def "awsprofile list" [] {
  aws configure list-profiles | lines
}

export def --env "awsprofile set" [name: string@"awsprofile list"] {
  $env.AWS_PROFILE = $name
}

export def --env "awsprofile clear" [] {
  $env.AWS_PROFILE = null
}
