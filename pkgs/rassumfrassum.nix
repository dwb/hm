{
  lib,
  fetchFromGitHub,
  buildPythonApplication,
  setuptools,
}:

buildPythonApplication rec {
  pname = "rassumfrassum";
  version = "0.3.3";

  src = fetchFromGitHub {
    owner = "joaotavora";
    repo = "rassumfrassum";
    rev = "8f0fab307fa79e339375ef413e03c46428fcadbe";
    hash = "sha256-9jYHWFtKurZz1GXR+CRRuVA3nQZzk0RAx2mzstZN53E=";
  };

  pyproject = true;
  build-system = [ setuptools ];

  meta = {
    description = "LSP/JSONRPC multiplexer for connecting one LSP client to multiple servers";
    homepage = "https://github.com/joaotavora/rassumfrassum";
    license = lib.licenses.gpl3Plus;
    mainProgram = "rass";
  };
}
