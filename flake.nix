{
  description = "weather in a simple format ";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable" ;
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system} ;
        src = ./. ;
        prod-sbcl = pkgs.sbcl.withPackages (packages: with packages ;
          [cl-json dexador local-time]) ;
        dev-sbcl = pkgs.sbcl.withPackages (packages: with packages ;
          [cl-json dexador local-time swank]);
        appNativeBuildInputs = with pkgs ; [
          pkg-config makeWrapper
        ];
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "medard" ;
          dontStrip = true ;
          version = "0.0.5" ;
          src = src ;
          nativeBuildInputs = appNativeBuildInputs ;
          buildInputs = with pkgs; [ prod-sbcl ];
          buildPhase = builtins.concatStringsSep "\n"
            [
              "export HOME=$(pwd)"
              "${prod-sbcl}/bin/sbcl --load build.lisp"
            ] ;
          installPhase = builtins.concatStringsSep "\n"
            [
              "mkdir -p $out/bin"
              "cp -v medard $out/bin"
              "wrapProgram $out/bin/medard --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH"
            ] ;
        };
        devShells.default =
          with import nixpkgs { inherit system ; } ;
          let sbcl-start = {forms}:
                builtins.foldl'
                  (acc: form: acc + " --eval " + "'" + form + "'")
                  "${dev-sbcl}/bin/sbcl" forms ;
              fn = {name, commands}:
                "function " + name + "(){\n" +
                (builtins.foldl' (acc: command: acc + " " + command + "\n") "" commands)
                + "}\n\n" ;
          in
            pkgs.mkShell {
              buildInputs = with pkgs; [ dev-sbcl ] ;
              shellHook = builtins.concatStringsSep "\n"
                [(fn {
                  name = "run";
                  commands = [
                    (sbcl-start {
                      forms = [
                        ''(require :asdf)''
                        ''(asdf:load-system "dexador")''
                        ''(asdf:load-system "local-time")''
                        ''(asdf:load-system "cl-json")''
                        ''(asdf:load-system "swank")''
                        ''(swank:create-server)''
                      ];
                    })
                  ];
                })
                 ''echo " 󰅟    󰖝   󰖒  󰼶"''
                ] ;
            } ;
      });
}
