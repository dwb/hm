{
  fetchFromGitHub,
  melpaBuild,
}:

melpaBuild {
  pname = "mysql";
  version = "0.2.4";

  # The repository has no release tags.
  src = fetchFromGitHub {
    owner = "LuciusChen";
    repo = "mysql.el";
    rev = "0f8f3c0fff6d9016c9c04ab6094d9354cca82c2c";
    hash = "sha256-B7+ZdjGQPN3iodz6WxuNYGspyGuLTDV0G8KmK4+fHCo=";
  };
}
