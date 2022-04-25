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

    myfonts.url = "github:jeslie0/my-fonts-flake";
    myfonts.inputs.nixpkgs.follows = "nixpkgs";

    # This is a bleeding edge version of emacs. Sometimes, it doesn't get pulled from Cachix...
    emacs-overlay.url = "github:nix-community/emacs-overlay/c43afd748147e2bf631e5b37f7a68e93a98f89c5";
    # "da2f552d133497abd434006e0cae996c0a282394";
    # emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    # emacs-overlay.inputs.nixpkgs.url = "github:nixos/nixpkgs/2128d0aa28edef51fd8fef38b132ffc0155595df";
  };

  outputs = { self, nixpkgs, home-manager, nur, ... }:
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
