function wacom_reset --description 'Reset wacom tablet/pen buttons'
    xsetwacom set "Wacom Intuos BT S Pen stylus" Button 1
    xsetwacom set "Wacom Intuos BT S Pen stylus" Button 2
    xsetwacom set "Wacom Intuos BT S Pen stylus" Button 3
end
