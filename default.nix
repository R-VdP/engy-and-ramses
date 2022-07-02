{ nixpkgs ? import <nixpkgs> {}
, version ? "dev"
, production ? false
}:

with nixpkgs;
with lib;

let
  dist = "dist";
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , pname
    , srcdir ? "./src"
    , targets ? []
    , registryDat ? ./registry.dat
    }:
    stdenv.mkDerivation {
      inherit pname version src;

      buildInputs = [ elmPackages.elm nodePackages.uglify-js ];

      preBuildPhases = [ "setupElmStuffPhase" ];

      setupElmStuffPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion  = "0.19.1";
        inherit registryDat;
      };

      buildPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
        build_module = out: module: let
          out_file = "${out}/generated/${module}.js";
        in ''
          echo "compiling ${elmfile module}"
          elm make ${optionalString production "--optimize"} ${elmfile module} --output ${out_file}
          ${optionalString production ''
            echo "minifying ${out_file}"
            uglifyjs ${out_file} \
                     --compress \
                     'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe,passes=2' \
              | uglifyjs --mangle --output ${out_file}
          ''}
        '';
      in ''
        mkdir -p ${dist}/
        ${concatMapStrings (build_module dist) targets}
      '';

      installPhase = ''
        mkdir -p $out
        echo "copying generated code"
        cp --verbose --recursive ${dist}/generated $out
        echo "copying assets"
        cp --verbose --recursive index.html assets $out
      '';
    };
in mkDerivation {
  pname   = "ramses_engy_marriage_frontend";
  src     = builtins.path { path = ./.; name = "frontend"; };
  targets = [ "Main" ];
}

