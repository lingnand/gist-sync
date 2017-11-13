{ pkgs, ghcAndroidPkgs, androidenv }:

with ghcAndroidPkgs;

# let
#   reflex = mkDerivation {
#     pname = "reflex";
#     version = "0.5.0";
#     libraryHaskellDepends = [
#       base containers dependent-map dependent-sum exception-transformers
#       mtl primitive ref-tf semigroups MemoTrie lens monad-control
#       syb these transformers transformers-compat data-default prim-uniq
#     ];
#     testHaskellDepends = [
#       base containers dependent-map MemoTrie mtl ref-tf
#     ];
#     src = pkgs.fetchFromGitHub {
#       owner = "reflex-frp";
#       repo = "reflex";
#       rev = "c617057bc4b9f6b75e4f4ebe2a4f2f818bd7d1a0";
#       sha256 = "0mgjmv1n1faa4r6kcw13b9hvxfsyrrwv24nan24yxwpjzn9psa2p";
#     };
#     configureFlags = [ "-f-use-template-haskell" ];
#     homepage = "https://github.com/reflex-frp/reflex";
#     description = "Higher-order Functional Reactive Programming";
#     license = pkgs.stdenv.lib.licenses.bsd3;
#   };
# in
[ ]