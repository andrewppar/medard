{
  description = "see what it feels like outside";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable" ;

  outputs = { self, nixpkgs }:
    let cwd = builtins.toString self ;
        pkgs = nixpkgs.legacyPackages.x86_64-darwin.pkgs;
        shell-fn = {name, commands}:
          "function " + name + "(){" +
          (builtins.foldl' (acc: elem: acc + " " + elem + "\n") "" commands) +
          "}\n\n" ;
        development-functions =
          [(
            shell-fn {
              name = "setup";
              commands =
                [ "curl -O https://beta.quicklisp.org/quicklisp.lisp"
                  (builtins.foldl'
                    (acc: elem: acc + " " + elem)
                    ""
                    [''sbcl --load quicklisp.lisp''
                     '' --eval "(quicklisp-quickstart:install)"''
                     '' --eval "(ql:add-to-init-file)"''])];
            })
           (
             shell-fn {
               name = "run" ;
               commands =
                 [
                   ''sbcl --eval "(ql:quickload :swank)" --eval "(swank:create-server)"''
                 ];
             })];
    in  {

      packages.x86_64-darwin.default =
        with import nixpkgs { system = "x86_64-darwin"; } ;
        stdenv.mkDerivation {
          name = "feels-like" ;
          src = self ;
          buildInputs = [
            sbcl
            which
            (callPackage ./pkgs/anparisi_ansiweather/anparisi_ansiweather.nix {})
          ];
          ASDF_OUTPUT_TRANSLATIONS = "/:/";
          dontStrip = true ;
          buildPhase = ''${sbcl}/bin/sbcl --load build.lisp'' ;
          installPhase = ''mkdir -p $out/bin; install -t $out/bin feels-like'' ;
        } ;
      devShells.x86_64-darwin.default =
        with import nixpkgs {system = "x86_64-darwin"; } ;
        pkgs.mkShell {
          buildInputs = [
            sbcl
            (callPackage ./pkgs/anparisi_ansiweather/anparisi_ansiweather.nix {})
            curl
          ] ;
          shellHook = (builtins.foldl' (acc: fun: acc + fun + "\n") "" development-functions) +
                      ''echo "welcome to feels-like...";'' ;
        } ;
    } ;
}
