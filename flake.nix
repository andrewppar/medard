{
  description = "see what it feels like outside";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable" ;
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let cwd = builtins.toString self ;
          pkgs = nixpkgs.legacyPackages.${system}.pkgs;
          shell-fn = {name, commands}:
            "function " + name + "(){" +
            (builtins.foldl' (acc: elem: acc + " " + elem + "\n") "" commands) +
            "}\n\n" ;
          development-functions =
            [(shell-fn {
              name = "setup";
              commands =
                [ "curl -O https://beta.quicklisp.org/quicklisp.lisp"
                  (builtins.foldl'
                    (acc: elem: acc + " " + elem)
                    ""
                    [''sbcl --load quicklisp.lisp''
                     '' --eval "(quicklisp-quickstart:install)"''
                     '' --eval "(ql:add-to-init-file)"''])];})
             (shell-fn {
               name = "run" ;
               commands =
                 [
                   ''sbcl --eval "(ql:quickload :swank)" --eval "(swank:create-server)"''
                 ];})];
          ansiweather = (pkgs.callPackage ./pkgs/anparisi_ansiweather/anparisi_ansiweather.nix {});
      in  {
        packages.default =
          with import nixpkgs {inherit system ;} ;
          stdenv.mkDerivation {
            name = "weather-widget" ;
            src = self ;
            buildInputs = [
              ansiweather
              sbcl
              which
            ];
            ASDF_OUTPUT_TRANSLATIONS = "/:/";
            dontStrip = true ;
            buildPhase = ''${sbcl}/bin/sbcl --load build.lisp'' ;
            # this should use makewrapper?
            installPhase = ''mkdir -p $out/bin; install -t $out/bin weather-widget'' ;
          } ;
        devShells.default =
          with import nixpkgs {inherit system;} ;
          pkgs.mkShell {
            buildInputs = [
              sbcl
              ansiweather
              curl
            ] ;
            shellHook = (builtins.foldl' (acc: fun: acc + fun + "\n") "" development-functions) +
                        ''echo "welcome to weather widget...";'' ;
          } ;
      }) ;
}
