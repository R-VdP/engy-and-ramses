{ nixpkgs ? import <nixpkgs> {}
, version ? "dev"
, production ? false
}:

with nixpkgs;
with lib;

let
  dist = "dist";
  mkDerivation =
    { srcs ? ./nix/elm-srcs.nix
    , src
    , pname
    , srcdir ? "./src"
    , targets ? []
    , registryDat ? ./nix/registry.dat
    }:
    stdenv.mkDerivation {
      inherit pname version src;

      buildInputs = [ elmPackages.elm nodePackages.uglify-js minify ];

      preBuildPhases = [ "setupElmStuffPhase" ];

      setupElmStuffPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion  = "0.19.1";
        inherit registryDat;
      };

      buildPhase = let
        build_module = out: module: let
          elmfile = module:
            "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
          elm_target = "${out}/elm-app.js";
          init_script = "${src}/js/app.js";
          out_dir = "${out}/generated";
          out_file = "${out_dir}/app.js";
          # We calculate the cartesian product to get to a list like
          # F2,A2,F3,A3,...
          pure_funcs = concatMapStringsSep ","
                         ( {type, index}: type + toString index )
                         ( cartesianProductOfSets
                             { type = ["A" "F"]; index = range 2 9; } );
          elm_compress_options =
            concatStringsSep "," [
              ''pure_funcs="A,F,${pure_funcs}"''
              ''pure_getters''
              ''keep_fargs=false''
              ''unsafe_comps''
              ''unsafe''
              ''passes=2''
            ];
        in ''
          echo "compiling ${elmfile module}"
          elm make ${optionalString production "--optimize"} \
                   --output ${elm_target} \
                   ${elmfile module}
          echo "minifying JS"
          mkdir -p "${out_dir}"
          ${if production
            then ''
              uglifyjs \
                <(uglifyjs ${elm_target} --compress '${elm_compress_options}') \
                <(uglifyjs ${init_script} --compress) \
                --mangle \
                --toplevel \
                --output ${out_file}''
            else ''
              uglifyjs ${elm_target} ${init_script} \
                       --warn \
                       --beautify \
                       --output ${out_file}
            ''
          }
        '';
      in ''
        mkdir -p ${dist}
        ${concatMapStrings (build_module dist) targets}
      '';

      installPhase = ''
        mkdir --parents $out
        echo "copying generated code..."
        cp --verbose --recursive ${dist}/generated $out
        echo "copying assets..."
        cp --verbose --recursive assets $out
        ${if production
          then ''
            echo "minifying index.html..."
            minify \
              --html-keep-document-tags \
              --html-keep-end-tags \
              --html-keep-quotes \
              --output $out/ \
              index.html''
          else
            "cp --verbose index.html $out"
        }
      '';
    };
in mkDerivation {
  pname   = "ramses_engy_marriage_frontend";
  src     = builtins.path { path = ./.; name = "frontend"; };
  targets = [ "Main" ];
}

