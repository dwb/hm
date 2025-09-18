{
  lib,
  fetchFromGitHub,
  melpaBuild,
  compat,
  consult,
  magit,
  plz,
  transient,
  with-editor,
}:

melpaBuild {
  pname = "majutsu";
  version = "0.6.0-unstable-2026-10-29";

  src = fetchFromGitHub {
    owner = "0WD0";
    repo = "majutsu";
    rev = "a21d0a9905a0f68d1b969b1199611e13ae9f9c40";
    hash = "sha256-H8U1y7XKfzefZtWFABvzPHFo3lXrItivneEp0R8xTM4=";
  };

  packageRequires = [
    compat
    consult
    magit
    plz
    transient
    with-editor
  ];

  meta = {
    description = "Magit-like interface for Jujutsu";
    homepage = "https://github.com/0WD0/majutsu";
    license = lib.licenses.gpl3Plus;
  };
}
