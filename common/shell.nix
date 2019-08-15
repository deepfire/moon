let default-compiler = import ../nix/default-compiler.nix; in
{ compiler         ? default-compiler
}@args:
(import ./packages.nix args).shell
