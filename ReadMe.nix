services.xserver.videoDrivers = ["nvidia"];
hardware.nvidia = {
  modesetting.enable = true;
  powerManagement.enable = false;
  nvidiaSettings = true;
  prime = {
    # offload = {
    #   enable = true;
    #   enableOffloadCmd = true;
    # };

    nvidiaBusId = "PCI:1:0:0";
    intelBusId = "PCI:0:2:0";
  };
};

hardware.opengl = {
  enable = true;
  driSupport = true;
  driSupport32Bit = true;
}
