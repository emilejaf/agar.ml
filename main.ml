open Graphics;;

type coordonees = {x: int; y: int};;
type entity = { coordonees: coordonees; color: color; masse: int };;
type player = { entities: entity list; score: int };;
let map = ({x = 0; y = 0;}, {x = 2000; y = 2000});;
let generate_cord () = 
  let min, max = map in
  let x = Random.int (max.x - min.x) + min.x
  and y = Random.int (max.y - min.y) + min.y
  in
  {x = x; y = y};;  

let generate_color () = 
  let r = Random.int (255) + 1
  and g = Random.int (255) + 1  
  and b = Random.int (255) + 1
in rgb r g b;;

let generate_player () = 
  let entity = { coordonees = generate_cord (); color = generate_color (); masse = 10 } in
  { entities = [entity]; score = 10 };;

 (* let players = ref [];; *)
  let mainPlayer = generate_player ();;


let open_window () = 
  open_graph "";
  resize_window 800 600;
  set_window_title "Agar.ml";;


let drawMainPlayer player =
  let mainEntity = (List.hd (player.entities)) in
  let radius = mainEntity.masse * 2 in
  set_color mainEntity.color;
  fill_circle (size_x() / 2) (size_y () / 2) radius;; 

let rec event_loop () = 
  clear_graph();
  drawMainPlayer mainPlayer;
  let (mousex, mousey) = mouse_pos () in
  let mouse_description = Printf.sprintf "Mouse position: %d,%d" mousex mousey in
  moveto 0 0; draw_string mouse_description;
  Unix.sleepf 0.05;
  event_loop ();;

let () = open_window (); event_loop ();; 