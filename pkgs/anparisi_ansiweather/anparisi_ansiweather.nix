{config, pkgs, ...}:
let
  inherit (pkgs) stdenv lib fetchFromGitHub ;
  inherit (lib) makeBinPath ;

  pname = "anparisi_ansiweather" ;
  version = "1.19.0" ;
in
stdenv.mkDerivation rec {
  name = pname ;
  src = fetchFromGitHub {
    owner = "fcambus";
    repo = "ansiweather";
    rev = version;
    sha256 = "3F0ZuCUccwh6al0MTD921tfY3Yt6posZQPMnx1t933M=" ;
  };

  buildInputs = with pkgs ;[
    curl
    jq
    bc
    makeWrapper
  ];

  installPhase = ''
    install -Dm755 ansiweather $out/bin/ansiweather
    wrapProgram $out/bin/ansiweather --prefix PATH : '${makeBinPath buildInputs}'
  '';

  postInstall = ''
    wrapProgram $out/bin/ansiweather --prefix PATH : "${lib.makeBinPath [ pkgs.jq ]} --prefix PATH : "${lib.makeBinPath [ pkgs.bc ]}
  '';
}
