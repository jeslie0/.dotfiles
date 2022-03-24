if status is-interactive
   set $fish_color_command cyan
   alias ls "lsd"
   alias cat "bat"
   alias nix-fish "nix develop . --command \"fish\""
   starship init fish | source
end
