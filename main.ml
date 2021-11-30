open Graphics;;

Random.init (int_of_float(Unix.time ()))

type coordonees = {x: float; y: float};;
let map = ({x = 0.; y = 0.;}, {x = 2000.; y = 2000.});;
let generate_cord = 
  let min, max = map in
  let x = Random.float (max.x -. min.x) +. min.x
  and y = Random.float (max.y -. min.y) +. min.y
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
    method addCoordonees (c : coordonees) = coordonees <- { x = coordonees.x +. c.x; y = coordonees.y +. c.y};
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
    in let vecteur = {x = float_of_int (mousex - size_x () / 2); y = float_of_int (mousey - size_y () / 2)}
  in let norme = sqrt(vecteur.x *. vecteur.x +. vecteur.y *. vecteur.y)
  in let vecteurNormal = {x = vecteur.x *. 10. /. norme; y = vecteur.y *. 10. /. norme}
  in let x = if currentCord.x +. vecteurNormal.x > min.x then if currentCord.x +. vecteurNormal.x < max.x then vecteurNormal.x else max.x -. currentCord.x else min.x -. currentCord.x 
  and y = if currentCord.y +. vecteurNormal.y > min.y then if currentCord.y +. vecteurNormal.y < max.y then vecteurNormal.y else max.y -. currentCord.y else min.y -. currentCord.y
    in mainEntity#addCoordonees {x = x; y = y};
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
  drawMainPlayer player;
  let (mousex, mousey) = mouse_pos () in
  player#updateCoords mousex mousey;
  moveto 0 0; draw_string (Printf.sprintf "Mouse position: %d,%d" mousex mousey);
  let playerCord = player#getMainEntity#getCoordonees in
  moveto 0 100; draw_string (Printf.sprintf "Player position: %f,%f" playerCord.x  playerCord.y);
  Unix.sleepf 0.05;
  event_loop ();;

let () = open_window (); event_loop ();; 
