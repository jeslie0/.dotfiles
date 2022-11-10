{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    pinnedNixpkgs.url = "github:nixos/nixpkgs/872fceeed60ae6b7766cc0a4cd5bf5901b9098ec";

    home-manager = {
      url = "github:nix-community/home-manager"; #Maybe change to unstable!!!
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agdaGitHub = {
      url = "github:agda/agda/022837331ad3c782e2bf915fda88e090b4d117dd";
    };

    passbemenuGitHub = {
      url = "github:jeslie0/passbemenu";
    };

    swaybgchangerGitHub = {
      url = "github:jeslie0/swaybgchanger";
    };

    bemenuFocusGitHub = {
      url = "github:jeslie0/bemenuFocus";
    };

    swaylockeffectsGitHub = {
      url = "github:jeslie0/swaylock-effects-git";
    };

    myfonts = {
      url = "github:jeslie0/fonts";
    };

    spotifyd = {
      url = "github:jeslie0/spotifyd";
    };

    emacs-overlay = {
      # url = "github:jeslie0/emacs-overlay";
      url = "/home/james/Documents/Nix/emacs-overlay";
      inputs.nixpkgs.url = "github:nixos/nixpkgs/093268502280540a7f5bf1e2a6330a598ba3b7d0";
      inputs.emacs-overlay.url = "github:nix-community/emacs-overlay/e58b5f1dac80f717f41121a0e4008b3050d79b9d";
    };
  };

  outputs = { self, nixpkgs, home-manager,  ... }:
    let
      system = "x86_64-linux"; #current system
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [ self.inputs.emacs-overlay.overlays.default
                     (final: prev: {
                       virtualbox = self.inputs.pinnedNixpkgs.legacyPackages."x86_64-linux".virtualbox;
                       spotifyd = self.inputs.spotifyd.packages."x86_64-linux".default;
                     })
                   ];
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
