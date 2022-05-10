{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/master"; #Maybe change to unstable!!!
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agdaGitHub.url = "github:agda/agda/022837331ad3c782e2bf915fda88e090b4d117dd";

    # hlsGitHub.url = "github:haskell/haskell-language-server/745ef26f406dbdd5e4a538585f8519af9f1ccb09";

    passbemenuGitHub.url = "github:jeslie0/passbemenu";
    passbemenuGitHub.inputs.nixpkgs.follows = "nixpkgs";

    swaybgchangerGitHub.url = "github:jeslie0/swaybgchanger";
    swaybgchangerGitHub.inputs.nixpkgs.follows = "nixpkgs";

    bemenuFocusGitHub.url = "github:jeslie0/bemenuFocus";
    bemenuFocusGitHub.inputs.nixpkgs.follows = "nixpkgs";

    swaylockeffectsGitHub.url = "github:jeslie0/swaylock-effects-git";
    swaylockeffectsGitHub.inputs.nixpkgs.follows = "nixpkgs";

    myfonts.url = "github:jeslie0/fonts";
    myfonts.inputs.nixpkgs.follows = "nixpkgs";

    # This is a bleeding edge version of emacs. Sometimes, it doesn't get pulled from Cachix...
    emacs-overlay.url = "github:nix-community/emacs-overlay/ef3088863916b0604dbd5c3ba402b7f52c89c53d";
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
      homeManagerConfigurations = {
        james = home-manager.lib.homeManagerConfiguration {
          inherit system pkgs;
          username = "james";
          homeDirectory = "/home/james";
          configuration = {
            imports = [
              ./.config/NixSystem/home.nix
            ];
          };
        };
      };

      nixosConfigurations = {
        James-Nix = lib.nixosSystem {
          inherit system;
          modules = [ (import ./.config/NixSystem/configuration.nix { inherit self; }) ];
        };
      };


    };
}
