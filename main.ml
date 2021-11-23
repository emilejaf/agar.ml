open Graphics;;
let open_window = 
  open_graph "";;
  resize_window 800 600;;
  set_window_title "Agar.ml"

let clear_window color = 
  let fg = foreground 
  in
      set_color color;
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color fg;;

let () = 
  open_window