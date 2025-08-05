$env.config.plugins.nupsql = {
  databases: [
    {
      name: xpadev
      host: localhost
      db: causal_dev
      user: causal_postgres_user
      query_alias: [
        {
          name: users
          query: "SELECT * FROM users;"
        }
      ]
      # Optional path to ssl certificate if needed.
      # cert: /home/user/.../cert.crt
      # ssl: true
    }
  ]

}

def "nuql login xpadev" [] {
  "very_secret_password" | nuql password xpadev
}
