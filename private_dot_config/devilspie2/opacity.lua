-- the debug_print command only prints to stdout
-- if devilspie2 is run using the --debug option
debug_print("Window Name: " .. get_window_name())
debug_print("Application name: " .. get_application_name())
local wc = get_window_class()
debug_print("Window class: " .. wc)

-- Put the window class of the to-be-translucent window as the key
-- Use devilspie2 --debug --emulate and open the application to see the window
-- class in the debug output
local windows = {
    Code=true,
    Zathura=true,
    ["nvim-qt"]=true,
    Emacs=true,
    ["jetbrains-clion"]=true,
}

if (windows[wc]) then
   set_window_opacity(0.95)
end
