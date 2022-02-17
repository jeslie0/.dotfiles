{
  description = "James' NixOS system configuration Flake";

  nixConfig.extra-substituters = "https://nix-community.cachix.org https://jeslie0.cachix.org";
  nixConfig.extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= jeslie0.cachix.org-1:orKPykG+p5gEbLe/ETPIQdAbK5WtUl2C6CZ+aVn0dy8=";

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

    # This is a bleeding edge version of emacs. Sometimes, it doesn't get pulled from Cachix...
    # emacs-overlay.url = "github:nix-community/emacs-overlay";
    # This is a nice working version of emacs \/.
    emacs-overlay.url = "github:nix-community/emacs-overlay/9b87f32ac912eeadc444f3920d41105ab697a123";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
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
