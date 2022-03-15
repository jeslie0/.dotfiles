if status is-interactive
   set $fish_color_command cyan
   alias ls "lsd"
   alias cat "bat"
   starship init fish | source
end
