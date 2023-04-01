{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    # nixpkgs = {
    #   type = "github";
    #   owner = "nixos";
    #   repo = "nixpkgs";
    #    rev = "6e51c97f1c849efdfd4f3b78a4870e6aa2da4198";
    # };

    # For things like fonts, which we don't want to constantly update.
    pinnedNixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      rev = "28319deb5ab05458d9cd5c7d99e1a24ec2e8fc4b";
    };

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      rev = "49e3c66d211d5110909375fe48d85c3c43753d61";
    };

    emacs-pgtk = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      rev = "c595b26be85c85c352ac3f058f2726686ca3a1ee";
      inputs.nixpkgs = {
        type = "github";
        owner = "nixos";
        repo = "nixpkgs";
        rev = "9b97ad7b4330aacda9b2343396eb3df8a853b4fc";
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
