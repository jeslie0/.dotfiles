{ config, pkgs, ... }:

{ # imports = [  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "james";
  home.homeDirectory = "/home/james";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.

  home.stateVersion = "21.11";

  nixpkgs.config.allowUnfree = true;

  home.packages =   with pkgs; [
                    pinentry_emacs
                    pciutils
                    chromium
                    zoom-us
                    (import (fetchFromGitHub {
                      owner = "jeslie0";
                      repo = "passbemenu";
                      rev = "ec7adda34eeb23216211dc94c2c2c0d74c1fee8e";
                      sha256 = "06ayhdkd2km66k06gn65sfnyajg8spvnspaqgba8djcqq0gvwvc2";
                    }))
                    obs-studio
                    pcmanfm
  ];

}
