{
  description = "James' NixOS system configuration Flake";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      rev = "6e51c97f1c849efdfd4f3b78a4870e6aa2da4198";
    };

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

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      rev = "49e3c66d211d5110909375fe48d85c3c43753d61";
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
