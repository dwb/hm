{
  fetchFromGitHub,
  melpaBuild,
}:

melpaBuild {
  pname = "pgsql";
  version = "0.1.0";

  # Tag v0.1.0 is older than this commit; the version header has not changed.
  src = fetchFromGitHub {
    owner = "LuciusChen";
    repo = "pgsql.el";
    rev = "9dbf135d16393c9d849ebd85543a6143fc42a8f9";
    hash = "sha256-NBTlzezCwR/QJ6Cmn3pILQLYNF1WLKjHKkd2x/qQlAs=";
  };
}
