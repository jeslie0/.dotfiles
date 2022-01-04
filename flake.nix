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

    swayBGChangerGitHub.url = "github:jeslie0/swayBGChanger";
    swayBGChangerGitHub.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # This is a nice working version of emacs \/.
    # emacs-overlay.url = "github:nix-community/emacs-overlay/c77eefc7683c6c56694e4516f6bd5fc6b3b0cf48";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }:
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

        modules = [
          (import ./.config/NixSystem/configuration.nix { inherit self; })
        ];
      };
    };


  };
}
