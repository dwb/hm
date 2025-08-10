{
  lib,
  rustPlatform,
  fetchFromGitLab,
  nushell,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "nu_plugin_nupsql";
  inherit (nushell) version;

  src = fetchFromGitLab {
    owner = "HertelP";
    repo = "nu_plugin_nupsql";
    rev = "e1ef346ac66b763fe9ba7cda7c640e1eaa719d83";
    hash = "sha256-I4ym9v/AYu1EOhEh4N91vT+Ut8c4qLAAidWOlI/lEdY=";
  };

  cargoHash = "";

  meta = {
    description = "PostgreSQL for Nushell";
    mainProgram = "nu_plugin_nupsql";
    homepage = "https://gitlab.com/HertelP/nu_plugin_nupsql";
    license = lib.licenses.mit;
  };
})
