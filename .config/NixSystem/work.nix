self:
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

  # home.stateVersion = "21.11";

  nixpkgs.config.allowUnfree = true;

 # * Sway

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };



  # * Fish


  programs.fish.enable = true;

  # * Emacs
  services.emacs = {
  enable = true;
  package = with pkgs;
    ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages (epkgs: [ epkgs.vterm ]));
  };

# * Binary Caches


  # * Packages
nixpkgs.overlays = [ (import self.inputs.emacs-overlay)
                   ];
  home.packages = with pkgs; [
    vim
    firefox

    mpv
    imv


    git
    git-crypt
    cryptsetup
    wget


    isync
    aspell
    aspellDicts.en

    powertop
    pass
    magic-wormhole
    pavucontrol
    sqlite
    stow

    texlive.combined.scheme-full
    zathura

    ripgrep
    direnv
    pandoc


    ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages (epkgs: [ epkgs.vterm ]))



      swayidle
      wl-clipboard
      alacritty
      xfce.xfce4-terminal
      magic-wormhole
      dmenu
      dmenu-wayland
      bemenu
      i3status
      starship
      waybar
      swaynotificationcenter
      gammastep
      wlroots
      slurp
      grim
      self.inputs.passbemenuGitHub.defaultPackage.${system}
      self.inputs.swaybgchangerGitHub.defaultPackage.${system}
      self.inputs.bemenuFocusGitHub.defaultPackage.${system}
      self.inputs.swaylockeffectsGitHub.defaultPackage.${system}
  ];
}
