{
  fetchFromGitHub,
  melpaBuild,
  transient,
}:

melpaBuild {
  pname = "clutch";
  version = "0.5.1";

  src = fetchFromGitHub {
    owner = "LuciusChen";
    repo = "clutch";
    # Tag v0.5.1.
    rev = "34ede39231d95806daf2888de33e11493a4a6b86";
    hash = "sha256-CPnz1KeHWGu63UmJx4kNpWYOoW08yj/NpECN9Y66C4c=";
  };

  packageRequires = [ transient ];
}
