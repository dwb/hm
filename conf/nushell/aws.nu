export def "awsprofile list" [] {
  aws configure list-profiles | lines
}

export def --env "awsprofile set" [name: string@"awsprofile list"] {
  $env.AWS_PROFILE = $name
}

export def --env "awsprofile clear" [] {
  $env.AWS_PROFILE = null
}

export def "aws ssox list-accounts" [--region: string] {
  aws sso list-accounts --access-token (ls ~/.aws/sso/cache | sort-by -r modified | get 0.name | open $in | get accessToken) --region $region | from json | get accountList
}
