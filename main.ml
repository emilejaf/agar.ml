open Graphics;;

Random.init (int_of_float(Unix.time ()))

type coordonees = {x: float; y: float};;
let map = ({x = 0.; y = 0.;}, {x = 2000.; y = 2000.});;

let generate_cord () = 
  let min, max = map in
  let x = Random.float (max.x -. min.x) +. min.x
  and y = Random.float (max.y -. min.y) +. min.y
  in
  {x = x; y = y};;  

let generate_color () = 
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
    method addCoordonees (c : coordonees) = coordonees <- { x = coordonees.x +. c.x; y = coordonees.y +. c.y};
end;;

class player = 
  object (self)
    val mutable entities = ([new entity (generate_cord ()) (generate_color ()) 10 ] : entity list);
    val mutable score = (0 : int);
    val mutable foods = ([]: entity list);
    method getMainEntity = List.hd entities;
    method updateCoords mousex mousey = 
      let mainEntity = self#getMainEntity
    in let currentCord = mainEntity#getCoordonees
      and min, max = map
    in let vecteur = {x = float_of_int (mousex - size_x () / 2); y = float_of_int (mousey - size_y () / 2)}
  in let norme = sqrt(vecteur.x *. vecteur.x +. vecteur.y *. vecteur.y)
  in let vecteurNormal = {x = vecteur.x *. 3. /. norme; y = vecteur.y *. 3. /. norme}
  in let x = if currentCord.x +. vecteurNormal.x > min.x then if currentCord.x +. vecteurNormal.x < max.x then vecteurNormal.x else max.x -. currentCord.x else min.x -. currentCord.x 
  and y = if currentCord.y +. vecteurNormal.y > min.y then if currentCord.y +. vecteurNormal.y < max.y then vecteurNormal.y else max.y -. currentCord.y else min.y -. currentCord.y
    in mainEntity#addCoordonees {x = x; y = y};
  method generateFoods = 
    let i = ref 0
    and min, max = map in
    while !i < 1 do
      let playerCord = self#getMainEntity#getCoordonees in
      let x = (if Random.bool () then 1. else -1.) *. Random.float(200.) +. playerCord.x
      and y = (if Random.bool () then 1. else -1.) *. Random.float(200.) +. playerCord.y in
    if x > min.x && x < max.x && y > min.y && y < max.y then
      let newFood = new entity {x = x; y = y;} (generate_color ()) 2;
      in foods <- newFood :: foods;
    i := !i + 1;
    done;
  method getFoods = foods;
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

let drawFood player = 
  let foods = player#getFoods
  and mainEntity = player#getMainEntity in
  let mainEntityCord = mainEntity#getCoordonees
  and ratioX = float_of_int (size_x () / 2) /. float_of_int (mainEntity#getRadius * 2)
  and ratioY = float_of_int (size_y () / 2) /. float_of_int (mainEntity#getRadius * 2)in
  List.iter (fun food ->
    let xFood = food#getCoordonees.x
    and yFood = food#getCoordonees.y
  in let realX = int_of_float((xFood -. mainEntityCord.x) *. ratioX) +size_x () / 2
  and realY = int_of_float((yFood -. mainEntityCord.y) *. ratioY) + size_y() / 2 in
  if realX > 0 && realX < 800 && realY > 0 && realY < 600 then
    set_color food#getColor; fill_circle realX realY food#getRadius; )
  foods;; 

let drawGrid player =
  let gridSpacing = 40 
  and mainEntity = player#getMainEntity in
  let currentCord = mainEntity#getCoordonees
in let gridStartX = gridSpacing - int_of_float currentCord.x mod gridSpacing
  and gridStartY = gridSpacing - int_of_float currentCord.y mod gridSpacing
in let i = ref gridStartX and j = ref gridStartY
in set_color (rgb 208 208 208);
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
  player#generateFoods;
  drawFood player;
  drawMainPlayer player;
  let (mousex, mousey) = mouse_pos () in
  player#updateCoords mousex mousey;
  moveto 0 0; draw_string (Printf.sprintf "Mouse position: %d,%d" mousex mousey);
  let playerCord = player#getMainEntity#getCoordonees in
  moveto 0 100; draw_string (Printf.sprintf "Player position: %f,%f" playerCord.x  playerCord.y);
  Unix.sleepf 0.05;
  event_loop ();;

let () = open_window (); event_loop ();; 
