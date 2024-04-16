{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # For things like fonts, which we don't want to constantly update.
    pinnedNixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      rev = "eea79d584eff53bf7a76aeb63f8845da6d386129";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      rev = "25cbd5b0f32cab75356a0a8e73aa2913529db36a";
    };

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flakes.url = "github:jeslie0/flakes";

    compdb.url = "github:jeslie0/compdb";

    agda.url = "github:agda/agda/abf7388900e9c94d94879185d7ec09e847b5fef5";

    grub-themes.url = "github:jeslie0/nixos-grub-themes";
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager,  ... }:
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
          inherit pkgs system;
          modules = [ (import ./.config/NixSystem/configuration.nix self system)
                      nixos-hardware.nixosModules.dell-xps-15-9570-intel
                    ];
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
