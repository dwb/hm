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
    rev = "70849a68d04094de98ffa3bfd4ba134a50d9bdae";
    hash = "sha256-zbqen1CmQCmNEh5jFqnMvtN8U17Lo8gdTg26H1/DxJ0=";
  };

  cargoHash = "sha256-dalAdZ9gszBni81agGFuTsHpn2/eku/26dvlGZdU7ek=";

  meta = {
    description = "PostgreSQL for Nushell";
    mainProgram = "nu_plugin_nupsql";
    homepage = "https://gitlab.com/HertelP/nu_plugin_nupsql";
    license = lib.licenses.mit;
  };
})
