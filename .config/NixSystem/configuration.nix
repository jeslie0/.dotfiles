self: system:
{ config, pkgs, ... }:

let
  myEmacs = ((pkgs.emacsPackagesFor pkgs.emacsPgtkNativeComp).emacsWithPackages (epkgs: [ epkgs.vterm epkgs.pdf-tools epkgs.emacsql-sqlite epkgs.emacsql]));
  # Use features from emacs-overlay. I don't need this just yet, as it includes your config file.
  # myEmacs = pkgs.emacsWithPackagesFromUsePackage {
  #   config = "";
  #   package = self.inputs.flakes.emacs-overlay.packages.${system}.emacsPgtkNativeComp;
  #   extraEmacsPackages = epkgs: [ epkgs.vterm epkgs.pdf-tools epkgs.emacsql-sqlite epkgs.emacsql ];
  # };
in

{
 imports =
   [ # Include the results of the hardware scan.
     ./hardware-configuration.nix
   ];

nixpkgs.overlays =
  [ self.inputs.emacs-overlay.overlays.default
    (final: prev: {
      virtualbox = self.inputs.pinnedNixpkgs.legacyPackages.${system}.virtualbox;
      spotifyd = self.inputs.myFlakes.spotifyd.packages.${system}.default;
    })
  ];

# Use the systemd-boot EFI boot loader.
# boot.loader.systemd-boot.enable = true; # Use this to use the UEFI bootloader, not GRUB.
boot.loader.grub.enable = true;
boot.loader.grub.version = 2;
boot.loader.grub.device = "nodev";
boot.loader.grub.efiSupport = true;
# boot.loader.grub.useOSProber = true; # Allows other operating systems to be found, but takes a long time to reload.
# boot.loader.grub.gfxmodeEfi = "1920x1080";
boot.loader.efi.canTouchEfiVariables = true;

networking.hostName = "James-Nix"; # Define your hostname.
networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.

# The global useDHCP flag is deprecated, therefore explicitly set to false here.
# Per-interface useDHCP will be mandatory in the future, so this generated config
# replicates the default behaviour.
networking.useDHCP = false;
# networking.interfaces.virbr0.useDHCP = true;
# networking.interfaces.virbr0-nic.useDHCP = true;
networking.interfaces.wlp59s0.useDHCP = true;

# Enable the OpenSSH daemon.
services.openssh.enable = true;
# services.openssh.extraConfig = ''
# AllowAgentForwarding yes
# '';

# Disable ipv6 for vpn
# networking.enableIPv6 = false;
# boot.kernelParams = [ "ipv6.disable=1" ];

# Open ports in the firewall.
networking.firewall.allowedTCPPorts = [ 2000 ];
# networking.firewall.allowedUDPPorts = [ ... ];
# Or disable the firewall altogether.
networking.firewall.enable = true;

# Bluetooth
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

# Set your time zone.
# time.timeZone = "America/Toronto";
time.timeZone = "Europe/London";

programs.sway = {
  enable = true;
  wrapperFeatures.gtk = true;
  extraPackages = with pkgs; [
    # swaylock-effects # using nix flake
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
    rofi
    waybar
    swaynotificationcenter
    gammastep
    wlroots
    slurp
    grim
    self.inputs.flakes.passbemenu.defaultPackage.${system}
    self.inputs.flakes.swaybgchanger.defaultPackage.${system}
    self.inputs.flakes.bemenuFocus.defaultPackage.${system}
    self.inputs.flakes.swaylock-effects.defaultPackage.${system}
  ];
};

programs.fish = {
  enable = true;
  shellAliases = { ls = "lsd"; cat = "bat"; };
  shellInit = ''
              starship init fish | source
              set $fish_color_command cyan
              '';
  };

services.emacs = {
  enable = true;
  defaultEditor = true;
  package = myEmacs;
};

fonts.fonts = with (self.inputs.pinnedNixpkgs.legacyPackages.x86_64-linux);
  [ cantarell-fonts
    emacs-all-the-icons-fonts
    dejavu_fonts
    fira-code
    font-awesome
    liberation_ttf
    noto-fonts
    noto-fonts-emoji
    source-code-pro
    terminus_font
    ubuntu_font_family
    nerdfonts
    (self.inputs.flakes.fonts.defaultPackage.${system})
  ];

# Firefox screensharing
xdg.portal = {
  enable = true;
  # gtkUsePortal = true;
  extraPortals = with pkgs; [ xdg-desktop-portal-wlr
                              xdg-desktop-portal-gtk ];
};

environment = {
  sessionVariables = {
    QT_SCALE_FACTOR="1";
    # QT_QPA_PLATFORM="wayland";
    # XDG_SESSION_TYPE="wayland";
    # GDK_BACKEND="wayland";
    MOZ_ENABLE_WAYLAND = "1";
    # XDG_CURRENT_DESKTOP = "sway";
    WEBKIT_FORCE_SANDBOX= "0";
    WLR_DRM_NO_MODIFIERS="1";
  };
  variables = {
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
    LSP_USE_PLISTS="true";
  };
};

nix.settings = {
  substituters = [ "https://nix-community.cachix.org"
                   "https://jeslie0.cachix.org" ];

  trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                          "jeslie0.cachix.org-1:orKPykG+p5gEbLe/ETPIQdAbK5WtUl2C6CZ+aVn0dy8=" ];
};

nix = {
  package = pkgs.nixUnstable;
  extraOptions = "experimental-features = nix-command flakes";
  registry ={
    self.flake = self;
    james = {
      from = { id = "james"; type = "indirect"; };
      to = { owner = "jeslie0";
             repo = "flake-templates";
             type = "github";
           };
    };
  };
};

# Define a user account. Don't forget to set a password with ‘passwd’.
users.users.james = {
  isNormalUser = true;
  initialPassword = "james";
  extraGroups = [ "wheel" "networkmanager" "video" ];
};

programs.steam = {
  enable = true;
  remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
  dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
};

virtualisation.virtualbox.host = {
  enable = true;
  enableExtensionPack = true;
};

users.extraGroups.vboxusers.members = ["james"];

environment.systemPackages = with pkgs;
  [ # Editors
    vim

    # Browsers
    firefox
    chromium
    nyxt
    qutebrowser

    # Communication
    signal-desktop
    discord
    zulip
    zoom-us

    # Media
    mpv
    imv
    youtube-dl
    spotify
    spotify-tui
    kodi
    playerctl

    # Nix
    cachix
    nix-index
    nix-prefetch-git
    cabal2nix
    home-manager
    nixfmt
    rnix-lsp

    # Tools
    shfmt
    git
    git-crypt
    cryptsetup
    wget
    autoconf
    pulseaudioFull
    gcc
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
    neofetch
    netcat
    openvpn
    python3
    nfs-utils



    ripgrep
    bat
    exa
    lsd
    du-dust
    fd
    tldr
    gping
    zoxide
    fd

    mu
    coq
    direnv
    unzip
    gnome.adwaita-icon-theme
    self.inputs.agdaGitHub.packages.${system}.Agda

    mkvtoolnix
    sbcl
    pandoc

    myEmacs

    # From home-manager
    pinentry-emacs
    pciutils
    pcmanfm
    gnuplot
    nmap
    nnn

    # Games
    # obs-studio
    # polymc
    # steam-run
    # protontricks
    # lutris

    ghc
    cabal-install
    haskell-language-server
    clang-tools
  ];


programs.light.enable = true;

services.dbus.enable = true;

services.printing.enable = true;
services.printing.drivers = [ pkgs.gutenprint ];
services.avahi.enable = true;
services.avahi.nssmdns = true;

# Enable sound.
sound.enable = true;
# hardware.pulseaudio = {
#   enable = true;
  # systemWide = true;
  # extraConfig = ''
  #               unload-module module-native-protocol-unix
  #               load-module module-native-protocol-unix auth-anonymous=1
  #               '';
# };

security.rtkit.enable = true;
services.pipewire = {
  enable = true;
  alsa.enable = true;
  pulse.enable = true;
};
# Enable touchpad support (enabled default in most desktopManager).
services.xserver.libinput.enable = true;

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
programs.mtr.enable = true;

programs.gnupg.agent = {
  enable = true;
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

# This value determines the NixOS release from which the default
# settings for stateful data, like file locations and database versions
# on your system were taken. It‘s perfectly fine and recommended to leave
# this value at the release version of the first install of this system.
# Before changing this value read the documentation for this option
# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
system.stateVersion = "21.05"; # Did you read the comment?
}
