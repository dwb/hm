{
  fetchFromGitHub,
  melpaBuild,
}:

melpaBuild {
  pname = "pretty-tables";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "alberti42";
    repo = "pretty-tables.el";
    rev = "b502b9d95c8a8e938c6b38b716fe065e71acc2c4";
    hash = "sha256-qoy8WalJqHySucUjzonG4b/8ncrwYM4yVF9/82gAZeU=";
  };
}
