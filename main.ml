open Graphics;;

Random.self_init

type coordonees = {x: int; y: int};;
let map = ({x = 0; y = 0;}, {x = 2000; y = 2000});;
let generate_cord = 
  let min, max = map in
  let x = Random.int (max.x - min.x) + min.x
  and y = Random.int (max.y - min.y) + min.y
  in
  {x = x; y = y};;  

let generate_color = 
  let r = Random.int (255) + 1
  and g = Random.int (255) + 1  
  and b = Random.int (255) + 1
in rgb r g b;;

class entity coord color masse = 
  object 
    val mutable coordonees = (coord : coordonees)
    val mutable color = (color : color)
    val mutable masse = (masse : int)
    method getCoordonees = coordonees;
    method getColor = color;
    method getRadius = masse * 3;
    method addCoordonees (c : coordonees) = coordonees <- { x = coordonees.x + c.x; y = coordonees.y + c.y};
end;;

class player = 
  object (self)
    val mutable entities = ([new entity generate_cord generate_color 10 ] : entity list);
    val mutable score = (0 : int);
    method getMainEntity = List.hd entities;
    method updateCoords mousex mousey = 
      let mainEntity = self#getMainEntity
    in let currentCord = mainEntity#getCoordonees
      and min, max = map
    in let newX = if mousex > (size_x ()/ 2) then (if currentCord.x = max.x then 0 else 1) else (if(currentCord.x = min.x) then 0 else -1)
    and newY = if mousey > (size_y ()/ 2) then (if currentCord.y = max.y then 0 else 1) else (if(currentCord.y = min.y) then 0 else -1)
    in mainEntity#addCoordonees ({x = newX; y = newY});
end;;

 let player = new player;;


let open_window () = 
  open_graph "";
  resize_window 800 600;
  set_window_title "Agar.ml";;


let drawMainPlayer player =
  let mainEntity = player#getMainEntity in
  set_color mainEntity#getColor;
  fill_circle (size_x() / 2) (size_y () / 2) mainEntity#getRadius;; 


let drawGrid player =
  let gridSpacing = 80 
  and mainEntity = player#getMainEntity in
  let currentCord = mainEntity#getCoordonees
in let gridStartX = gridSpacing - currentCord.x mod gridSpacing
  and gridStartY = gridSpacing - currentCord.y mod gridSpacing
in let i = ref gridStartX and j = ref gridStartY
in set_color (rgb 10 10 10);
while !i < size_x () do
  moveto !i 0;
  lineto !i (size_y ());
  i := !i + gridSpacing;  
done;
while !j < size_y () do
  moveto 0 !j;
  lineto (size_x ()) !j;
  j := !j + gridSpacing;
done;;

let rec event_loop () = 
  clear_graph();
  drawGrid player;
  drawMainPlayer player;
  let (mousex, mousey) = mouse_pos () in
  player#updateCoords mousex mousey;
  moveto 0 0; draw_string (Printf.sprintf "Mouse position: %d,%d" mousex mousey);
  let playerCord = player#getMainEntity#getCoordonees in
  moveto 0 100; draw_string (Printf.sprintf "Player position: %d,%d" playerCord.x  playerCord.y);
  Unix.sleepf 0.05;
  event_loop ();;

let () = open_window (); event_loop ();; 