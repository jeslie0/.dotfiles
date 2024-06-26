self: system:
{ config, pkgs, ... }:

let

# emacsPgtk = self.inputs.emacs-pgtk.packages.x86_64-linux.emacsPgtk.overrideAttrs (prev: {
#   postFixup = builtins.replaceStrings [ "/bin/emacs" ] [ "/bin/.emacs-*-wrapped" ] prev.postFixup;
# });
emacsPgtk = pkgs.emacs29-pgtk.overrideAttrs (prev: {
# emacsPgtk = self.inputs.emacs-pgtk.packages.x86_64-linux.emacsPgtk.overrideAttrs (prev: {
  passthru = prev.passthru // {
    treeSitter = true;
  };
});
myEmacs = ((pkgs.emacsPackagesFor emacsPgtk).emacsWithPackages (epkgs: with epkgs;
  [ vterm
    # treeSitterPkgs
    mu4e
    treesit-grammars.with-all-grammars
    epkgs.pdf-tools
  ]));

treeSitterPkgs = pkgs.tree-sitter.withPlugins (p: [ p.tree-sitter-cpp ]);

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
    })
  ];

# Use the systemd-boot EFI boot loader.
# boot.loader.systemd-boot.enable = true; # Use this to use the UEFI bootloader, not GRUB.
boot.loader.grub.enable = true;
boot.loader.grub.device = "nodev";
boot.loader.grub.efiSupport = true;
# boot.loader.grub.useOSProber = true; # Allows other operating systems to be found, but takes a long time to reload.
# boot.loader.grub.gfxmodeEfi = "1920x1080";
boot.loader.efi.canTouchEfiVariables = true;
boot.loader.grub.theme = self.inputs.grub-themes.packages.${system}.big-sur;

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

specialisation = {
  nvidia.configuration = {
    system.nixos.tags = ["nvidia"];

    boot.extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
    boot.initrd.kernelModules = [ "nvidia" ];


    # Enable openGL
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    # Load nvidia driver for Xorg and Wayland
    services.xserver = {
      # Enable nvidia drivers
      videoDrivers = [ "nvidia" ];
      # Enable the X11 windowing system.
      enable = true;

      # Enable i3 window manager
      windowManager.i3.enable = true;

      # use GB keyboard layout
      xkb.layout = "gb";

      # Swap Capslock and Escape keys
      xkb.options = "caps:swapescape";

      # Use lightDM
      displayManager.lightdm.enable = true;
    };

    hardware.nvidia = {
      # Modesetting is required
      modesetting.enable = true;

      package = config.boot.kernelPackages.nvidiaPackages.latest;
    #   powerManagement.enable = false;
      nvidiaSettings = true;

      prime = {
        # sync.enable = true;
        offload = {
          enable = true;
          enableOffloadCmd = true; # Provides `nvidia-offload` command.
        };
        nvidiaBusId = "PCI:1:0:0";
        intelBusId = "PCI:0:2:0";
      };
    };

  };
};

programs.sway = {
  enable = true;
  wrapperFeatures.gtk = true;
  extraPackages = with pkgs; [
    swaylock-effects # using nix flake
    swaybg
    rofi-pass-wayland
    rofi-wayland-unwrapped
    swayidle
    wl-clipboard
    alacritty
    magic-wormhole
    i3status
    starship
    waybar
    swaynotificationcenter
    gammastep
    wlroots
    slurp
    grim
  ];
};

security.pam.loginLimits = [
  { domain = "@users"; item = "rtprio"; type = "-"; value = 1; }
];

programs.fish = {
  enable = true;
  shellAliases = { ls = "lsd"; cat = "bat"; };
  shellInit = ''
              starship init fish | source
              set $fish_color_command cyan
              '';
  };

# services.emacs = {
#   enable = true;
#   defaultEditor = true;
#   package = myEmacs;
# };

fonts.packages = with (self.inputs.pinnedNixpkgs.legacyPackages.x86_64-linux);
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
    iosevka
    cascadia-code
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
  substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://jeslie0.cachix.org"
    "https://cache.iog.io"
  ];

  trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "jeslie0.cachix.org-1:orKPykG+p5gEbLe/ETPIQdAbK5WtUl2C6CZ+aVn0dy8="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
};

nix = {
  package = pkgs.nixVersions.latest;
  extraOptions = "experimental-features = nix-command flakes";
  registry ={
    nixpkgs.flake = self.inputs.nixpkgs;
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
  extraGroups = [ "wheel" "networkmanager" "video" "wireshark" ];
};

programs.steam = {
  enable = true;
  remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
  dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
};

virtualisation.docker.rootless ={
  enable = true;
  setSocketVariable = true;
};

programs.wireshark.enable = true;

environment.systemPackages = with pkgs;
  [ # Editors
    myEmacs
    emacs-lsp-booster
    vim

    # Browsers
    firefox
    chromium
    # nyxt
    qutebrowser

    # Communication
    signal-desktop
    zulip

    # Media
    mpv
    imv
    youtube-dl
    spotify
    kodi
    playerctl
    maestral

    # Nix
    cachix
    nix-index
    nix-prefetch-git
    cabal2nix
    home-manager
    nixfmt-classic

    # Tools
    shfmt
    git
    git-crypt
    subversion
    cryptsetup
    wget
    autoconf
    pulseaudioFull
    isync
    mu
    (aspellWithDicts (d: [ d.en ]))
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
    nfs-utils
    haskellPackages.eventlog2html
    ripgrep
    bat
    lsd
    du-dust
    fd
    tldr
    gping
    zoxide
    fd
    htop
    compdb
    direnv
    unzip
    gnome.adwaita-icon-theme
    ghostscript

    mkvtoolnix
    sbcl
    pandoc
    calibre



    # Programming languages
    # self.inputs.agda.defaultPackage.${system}
    agda
    gcc
    python3
    ghc
    cabal-install
    coq
    elmPackages.elm

    # Language servers
    # self.inputs.hls.packages.${system}.default
    haskell-language-server
    clang-tools
    lua53Packages.digestif
    cmake-language-server
    nil
    nodePackages.typescript
    nodePackages.typescript-language-server

    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-live
    elmPackages.elm-doc-preview



    # Treesitter languages

    # From home-manager
    pinentry-emacs
    pciutils
    pcmanfm
    gnuplot
    nmap
    # nnn

    # Games
    obs-studio
    prismlauncher
    steam-run
    protontricks
    lutris
    # minecraft
  ];


programs.light.enable = true;

services.dbus.enable = true;

services.printing.enable = true;
services.printing.drivers = [ pkgs.gutenprint ];
services.avahi.enable = true;
services.avahi.nssmdns4 = true;

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
services.libinput.enable = true;

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
programs.mtr.enable = true;

programs.gnupg ={
  package = self.inputs.pinnedNixpkgs.legacyPackages.x86_64-linux.gnupg;
  agent = {
    enable = true;
  };
};

# Enable unfree software
# This is set in the flake.
# nixpkgs.config = {
#   allowUnfree = true;
#   allowBroken = false;
# };
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
