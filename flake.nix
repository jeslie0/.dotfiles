{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;

    # For things like fonts, which we don't want to constantly update.
    pinnedNixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      rev = "872fceeed60ae6b7766cc0a4cd5bf5901b9098ec";
    };

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Version 2.6.3 on my Cachix
    agdaGitHub = {
      type = "github";
      owner = "agda";
      repo = "agda";
      rev = "022837331ad3c782e2bf915fda88e090b4d117dd";
    };

    flakes = {
      url = github:jeslie0/flakes;
      inputs.emacs-overlay = {
        url = github:jeslie0/emacs-overlay;
        inputs.nixpkgs = {
          type = "github";
          owner = "nixos";
          repo = "nixpkgs";
          rev = "093268502280540a7f5bf1e2a6330a598ba3b7d0"; };

        inputs.emacs-overlay = {
          type = "github";
          owner = "nix-community";
          repo = "emacs-overlay";
          rev = "e58b5f1dac80f717f41121a0e4008b3050d79b9d"; };
      };
    };
  };

  outputs = { self, nixpkgs, home-manager,  ... }:
    let
      system = "x86_64-linux"; #current system
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
      };

      lib = nixpkgs.lib;

    in {
      nixosConfigurations = {
        James-Nix = lib.nixosSystem {
          inherit system;
          modules = [ (import ./.config/NixSystem/configuration.nix self system)
                    ];
        };
      };

      homeConfigurations = {
        james = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ (import ./.config/NixSystem/home.nix self)];
        };
      };
    };
}
