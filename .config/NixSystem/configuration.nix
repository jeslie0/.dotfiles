# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self }:
{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true; # Use this to use the UEFI bootloader, not GRUB.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  # boot.loader.grub.useOSProber = true;
  boot.loader.grub.gfxmodeEfi = "1920x1080";
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "James-Nix"; # Define your hostname.
  networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # networking.interfaces.virbr0.useDHCP = true;
  # networking.interfaces.virbr0-nic.useDHCP = true;
  networking.interfaces.wlp59s0.useDHCP = true;

  ##Bluetooth
  services.blueman.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
    General = {
      Enable = "Source,Sink,Media,Socket";
    };
  };


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      swaylock-effects
      swayidle
      wl-clipboard
      xfce.xfce4-terminal
      magic-wormhole
      dmenu
      dmenu-wayland
      bemenu
      i3status
      rofi
      waybar
      # (import (fetchFromGitHub {
      #   owner = "jeslie0";
      #   repo = "swayBGChanger";
      #   rev = "b78ac142e9c668830cda9b2786c94fb56742a534";
      #   sha256 = "18nbvj2w5vdw1n6awba0yasq9jdzg980bmhss0zldz3f739apb61";
      # }))
      # swayBGChangerGitHub
    ];
  };

  # Emacs PGTK GCC
  services.emacs.package = pkgs.emacsPgtkGcc;
  services.emacs.enable = true;

  # # For bleeding edge emacs.
  nixpkgs.overlays = [
    (import self.inputs.emacs-overlay)
  ];

  # For an emacs 28 build.
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = "https://github.com/nix-community/emacs-overlay/archive/de00abcd9631b104ef2868e1e0a877b1ec6b5633.tar.gz";
  #   }))
  # ];

  # Fonts
  fonts.fonts = with pkgs; [
    cantarell-fonts
    emacs-all-the-icons-fonts
    dejavu_fonts
    fira-code
    font-awesome
    liberation_ttf
    noto-fonts
    noto-fonts-emoji
    source-code-pro
    terminus_font
  ];

  # Firefox screensharing
  # services.pipewire.enable = true;

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      gtkUsePortal = true;
    };
  };

  environment.sessionVariables = {
    MOZ_ENABLE_WAYLAND = "1";
    XDG_CURRENT_DESKTOP = "sway";
  };

  environment.variables = {
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
  };

  # Binaries
  nix = {
    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://jeslie0.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "jeslie0.cachix.org-1:orKPykG+p5gEbLe/ETPIQdAbK5WtUl2C6CZ+aVn0dy8="
    ];

    # Flakes
    package = pkgs.nixUnstable;
    extraOptions = "experimental-features = nix-command flakes";
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };


  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.james = {
    isNormalUser = true;
    initialPassword = "james";
    extraGroups = [ "wheel" "networkmanager" "video" ]; # Enable ‘sudo’ for the user.
  };


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    git-crypt
    wget
    autoconf
    nix-index
    pulseaudioFull
    cachix
    # emacsPgtkGcc
    gcc
    isync
    aspell
    aspellDicts.en
    # firefox-wayland
    firefox
    powertop
    pass
    magic-wormhole
    pavucontrol
    signal-desktop
    sqlite
    stow
    texlive.combined.scheme-full
    zathura
    neofetch
    zulip-term
    netcat

    ghc
    haskell-language-server
    nix-prefetch-git
    cabal2nix
    cabal-install
    haskellPackages.apply-refact
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.hasktags
    haskellPackages.hoogle
    haskellPackages.hindent
    # haskellPackages.zlib
    # zlib # Remove and put into a nix shell

    # pipewire
    playerctl
    wlroots
    slurp
    gammastep
    grim
    syncthing
    home-manager
    spotify
    ripgrep
    python39

    mu
    coq
    direnv
    unzip
    gnome3.adwaita-icon-theme
    self.inputs.passbemenuGitHub.defaultPackage.${system}
    self.inputs.swayBGChangerGitHub.defaultPackage.${system}
    self.inputs.agdaGitHub.packages.${system}.Agda
    # self.inputs.hlsGitHub.defaultPackage.${system}

    nixfmt
  ];

  programs.light.enable = true;



  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    #  enableSSHSupport = true;
  };

  # Enable unfree software
  nixpkgs.config.allowUnfree = true;

  # Clean /tmp/ folder?
  # boot.cleanTmpDir = true;

  # Automatically optimise the store.
  # nix.autoOptimiseStore = true;

  # GC configuration
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 2000 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}