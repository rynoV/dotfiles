function wacom_inkscape --description 'Configure wacom pen/tablet bindings for inkscape'
    # Default for placing pen on tablet
    xsetwacom set "Wacom Intuos BT S Pen stylus" Button 1
    # Shift+e for switching to eraser with bottom button
    xsetwacom set "Wacom Intuos BT S Pen stylus" Button 2 key shift e shift
    # p for switching to freehand drawing
    xsetwacom set "Wacom Intuos BT S Pen stylus" Button 3 key p
end
