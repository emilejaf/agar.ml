open Graphics;;

type coordonees = {x: int; y: int};;
type entity = { coordonees: coordonees; color: color; masse: int };;
type player = { entities: entity list; score: int };;
let players = ref [];;

let open_window = 
  open_graph "";
  resize_window 800 600;
  set_window_title "Agar.ml";;

let init ()= 
  let entity = { coordonees = {x = size_x()/2; y = size_y()/2}; color = rgb 0 0 255; masse = 1 } in
  let player = { entities = [entity]; score = 0 } in
  players := player::(!players);
  set_color entity.color;
  fill_circle entity.coordonees.x entity.coordonees.y (entity.masse * 50);;


let rec event_loop () = 
  let (mousex, mousey) = mouse_pos () in
  let mouse_description = Printf.sprintf "Mouse position: %d,%d" mousex mousey in
  clear_graph();
  moveto 0 0; draw_string mouse_description;
  init ();
  moveto 0 100; draw_string (string_of_int (List.length !players));
  Unix.sleepf 0.05;
  event_loop ();;

let () = open_window; event_loop ();; 