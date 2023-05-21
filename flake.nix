{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;

    # For things like fonts, which we don't want to constantly update.
    pinnedNixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      rev = "eea79d584eff53bf7a76aeb63f8845da6d386129";
    };

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      rev = "25cbd5b0f32cab75356a0a8e73aa2913529db36a";
    };

    emacs-pgtk = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      rev = "25cbd5b0f32cab75356a0a8e73aa2913529db36a";
      inputs.nixpkgs = {
        type = "github";
        owner = "nixos";
        repo = "nixpkgs";
        rev = "3005f20ce0aaa58169cdee57c8aa12e5f1b6e1b3";
      };
    };

    flakes.url = github:jeslie0/flakes;

    compdb.url = github:jeslie0/compdb;
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
          modules = [ (import ./.config/NixSystem/configuration.nix self system) ];
        };
      };

      homeConfigurations = {
        james = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ (import ./.config/NixSystem/home.nix self) ];
        };
      };
    };
}
