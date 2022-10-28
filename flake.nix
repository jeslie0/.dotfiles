{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    pinnedNixpkgs.url = "github:nixos/nixpkgs/c481b497d5c3754c50e89795c6d903dd0d130baa";

    home-manager = {
      url = "github:nix-community/home-manager"; #Maybe change to unstable!!!
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agdaGitHub = {
      url = "github:agda/agda/022837331ad3c782e2bf915fda88e090b4d117dd";
    };

    # hlsGitHub = {
    #   url = "github:haskell/haskell-language-server/745ef26f406dbdd5e4a538585f8519af9f1ccb09";
    # };

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
      inputs.nixpkgs.url = "github:nixos/nixpkgs/c5924154f000e6306030300592f4282949b2db6c";
      inputs.emacs-overlay.url = "github:nix-community/emacs-overlay/d04558957e2bef64780144486becbdc1c731fe7f";
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
