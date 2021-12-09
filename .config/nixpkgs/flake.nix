{
  description = "Home Manager configurations";

  inputs = {
    nixpkgs.url = "flake:nixpkgs";
    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, homeManager }: {
    homeConfigurations = {
      "someuser@somecomputer" = homeManager.lib.homeManagerConfiguration {
        configuration = ./home.nix;

        system = "x86_64-linux";
        homeDirectory = "/home/james";
        username = "james";
        stateVersion = "21.05";
      };
    };
  };
}
