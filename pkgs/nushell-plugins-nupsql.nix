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
    hash = "";
  };

  cargoHash = "sha256-dalAdZ9gszBni81agGFuTsHpn2/eku/26dvlGZdU7ek=";

  meta = {
    description = "PostgreSQL for Nushell";
    mainProgram = "nu_plugin_nupsql";
    homepage = "https://gitlab.com/HertelP/nu_plugin_nupsql";
    license = lib.licenses.mit;
  };
})
