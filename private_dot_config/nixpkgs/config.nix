# https://nixos.org/manual/nixpkgs/stable/#sec-declarative-package-management
{
  packageOverrides = pkgs: with pkgs; rec {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        # Allows configuration of nix-shell to use fish
        any-nix-shell
        lorri
        rnix-lsp
        cachix
        nixFlakes
        nix-direnv
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" "/share/nix-direnv" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}
