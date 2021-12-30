open Graphics;;

exception InvalidResponseType of string;;

Random.init (int_of_float(Unix.time ()))

type coordonees = {x: float; y: float};;

type nature = Player | Food | Bush;;

let map = ({x = 0.; y = 0.;}, {x = 2000.; y = 2000.});;

let maxFood = 250;;
let spawnFoodRadius = 300.;;
let despawnFoodRadius = 350.;;

let maxBushes = 15;;
let maxBushesFood = 7;; 

(* Cette liste stocke toutes les informations reçus entre chaque frame *)
let incomingData = ref [];;


let bushes = ref [];;

let other_players = ref [];;
let bushesFood = ref [];;

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

let collision entity1 entity2 scaleRatio = 
  if (sqrt((entity1#getCoordonees.x -. entity2#getCoordonees.x) *. (entity1#getCoordonees.x -. entity2#getCoordonees.x)+.
    (entity1#getCoordonees.y -. entity2#getCoordonees.y) *. (entity1#getCoordonees.y -. entity2#getCoordonees.y)) *. scaleRatio) < float_of_int ((entity1#getRadius + entity2#getRadius))
  then true
  else false;;

class entity coord color masse nature = 
  object 
    val nature = (nature: nature)
    val mutable coordonees = (coord : coordonees)
    val mutable speed = ({ x = 0.; y = 0.;} : coordonees)
    val mutable color = (color : color)
    val mutable masse = (masse : int)
    method getCoordonees = coordonees;
    method getColor = color;
    method getRadius = match nature with
      | Player -> 30 + int_of_float ((float_of_int masse)**0.5)
      | Food -> 8
      | Bush -> masse;

    method getSpeed = speed;

    (* on notifie les autres clients si le joueur change de direction *)
    method setSpeed (newSpeed: coordonees) =  let needUpdate = newSpeed.x <> speed.x || newSpeed.y <> speed.y in speed <- newSpeed; needUpdate;

    (* modifie les coordonnées de l'entité sans sortir de la map *)
    method updateCoords deltat = 
      let min, max = map in
      let newX = match coordonees.x +. speed.x *. deltat with
        | x when x < min.x -> min.x
        | x when x > max.x -> max.x
        | x -> x 
      and newY = match coordonees.y +. speed.y *. deltat with
        | y when y < min.y -> min.y
        | y when y > max.y -> max.y
        | y -> y 
    in coordonees <- {x = newX; y = newY};

    method addMasse quantity = masse <- masse + quantity;

    method getMasse = masse;
end;;

class player id entities = 
  object (self)
    val mutable id = (id: int)
    val mutable entities = (entities : entity list);
    val mutable score = (0 : int);
    val mutable foods = ([]: entity list);
    val mutable needUpdate = (true: bool);

    method getId = id;

    method getMainEntity = List.hd entities;

    method getEntities = entities;

    method getScore = score;

    method getSpeed = 60. *. exp (-. float_of_int (score) /. 750.) +. 45.;

    method getFoods = foods;
  
    method getRatio = 4.;
  
    method updateCoords deltat = List.iter(fun entity -> entity#updateCoords deltat) entities;

    method updatePlayerSpeed mousex mousey = 
      let mainEntity = self#getMainEntity
      and vector = {x = float_of_int (mousex - size_x () / 2); y = float_of_int (mousey - size_y () / 2)}
    in let norme = sqrt(vector.x *. vector.x +. vector.y *. vector.y)
    in let normalizedVector = match norme with
    | norme when norme  < float_of_int mainEntity#getRadius *. 0.15 -> {x = 0.; y = 0.}
    | norme when norme < float_of_int mainEntity#getRadius ->  {x = vector.x /. 100.; y = vector.y /. 100.}
    | _ -> {x = vector.x  /. norme; y = vector.y /. norme}
      in 
      let result = List.exists (fun entity -> entity#setSpeed { x = normalizedVector.x *. self#getSpeed; y = normalizedVector.y *. self#getSpeed }) entities
    in needUpdate <- result;

  method generateFoods = 
    let i = ref (List.length foods);
    and min, max = map in
    while !i < maxFood do
      let playerCord = self#getMainEntity#getCoordonees in
      let x = (if Random.bool () then 1. else -1.) *. Random.float(spawnFoodRadius) +. playerCord.x
      and y = (if Random.bool () then 1. else -1.) *. Random.float(spawnFoodRadius) +. playerCord.y in
    if x > min.x && x < max.x && y > min.y && y < max.y then
      let newFood = new entity {x = x; y = y;} (generate_color ()) 1 Food;
      in foods <- newFood :: foods;
    i := !i + 1;
    done;

  method destroyFoods = 
    foods <- List.filter (fun food -> 
      let foodCord = food#getCoordonees
      and playerCord = self#getMainEntity#getCoordonees
        in if abs_float (foodCord.x -. playerCord.x) > despawnFoodRadius || abs_float (foodCord.y -. playerCord.y) > despawnFoodRadius then false else true) foods;

  method handleFoodCollisions = foods <- List.filter (fun food -> 
    let mainEntity = self#getMainEntity
    in if collision mainEntity food self#getRatio then
      let () = self#updateScore mainEntity food#getMasse
      in false
    else true
    ) foods;

  method updateScore entity quantity = score <- score + quantity; entity#addMasse quantity;

  method updateServer incomingData output = 
    (* SENDING DATA *)
    begin
    if needUpdate then
    let dataToSend = ref "" in
    List.iter (fun entity -> let speed = entity#getSpeed and coordonees = entity#getCoordonees in dataToSend := !dataToSend ^ (Printf.sprintf "%0.2f %0.2f %0.2f %0.2f %d %d," coordonees.x coordonees.y speed.x speed.y entity#getMasse entity#getColor)) entities;
    output_string output (!dataToSend ^ "\n");
    flush output; 
    needUpdate <- false;
    end;

    (* RECEIVING DATA *)
    List.iter (fun data -> 
      match (String.split_on_char ',' data) with
      | [] -> ()
      | playerData::entitesData -> 
        let (responseType, id) = Scanf.sscanf playerData "%s %d" (fun x y -> (x, y))
        in match responseType with 
          | "UPDATE" -> let entities = List.map (fun entityData -> 
            let (coordonees, speed, masse, color) = Scanf.sscanf entityData "%f %f %f %f %d %d" (fun x y sx sy masse color -> ({x = x; y = y;}, {x = sx; y = sy}, masse, color))
              in let entity = new entity coordonees color masse Player in ignore (entity#setSpeed speed); entity) (List.filter (fun str -> str <> "") entitesData)
            in other_players := (new player id entities)::(List.filter (fun player -> player#getId <> id) !other_players);
          | "DISCONNECT" -> other_players := List.filter (fun p -> p#getId <> id) !other_players;
          | _ -> raise (InvalidResponseType responseType);
      )
      (!incomingData);
    incomingData := [];
    
end;;


let drawMainPlayer player =
  let mainEntity = player#getMainEntity in
  set_color mainEntity#getColor;
  fill_circle (size_x() / 2) (size_y () / 2) (mainEntity#getRadius);; 

(*  
let spawn_buisson_feed coordonees rayon = 
  let theta = Random.float (2.*.Float.pi) in
    let entity ( coord = {x = float_of_int(rayon)*.cos(theta) ; y = float_of_int(rayon)*.sin(theta)}; color := rgb generate_color ; masse := 1)
    and marge = Random.int 20 in 
    for i = 0 to 40 + marge do
      entity#getCoordonees := {x = coordonees.x +. int_of_float(rayon+i) *. cos(theta); y= int_of_float(rayon+i)*.sin(theta)}; 
      set_color entity#getColor;
      fill_circle (int_of_float(entity#getCoordonees.x)) (int_of_float(entity#getCoordonees.y)) (entity#getRadius);
    done;; *)




(* let mange entity1 entity2 =
  if (collision entity1 entity2) then 
    begin
      let r1 = float_of_int(entity1#getRadius) and r2 = float_of_int(entity2#getRadius) and
      distance = sqrt((entity1#getCoordonees.x -. entity2#getCoordonees.x) *. (entity1#getCoordoneess.x -. entity2#getCoordonees.x) +.
      (entity1#getCoordonees.y -. entity2#getCoordonees.y) *. (entity1#getCoordonees.y -. entity2#getCoordonees.y)) in 
      let d = (r1*.r1 -. r2*.r2 +. distance *. distance) /. (2. *. distance) in
      let h = sqrt(r1*.r1 -. d*.d) in
      let aire = r1 *. r1 *. acos(d/.r1) +. r2*.r2*.acos((distance-.d)/.r2) -. h*.distance in
      if (r1<r2) then 
        if (((Float.pi *. r1 *. r1) /. aire) >= 0.80) then ()(* supprime entity1*)
        else ()
      else
        if (((Float.pi *. r2 *. r2) /. aire) >= 0.80) then () (* Supprime entity2*)
        else ()
    end;; 
*)

let drawBush x y radius color = let tableau_coord = Array.make 80 (0,0) and tableau_coord2 = Array.make 80 (0,0) and cote = 6. and compteur = ref 0 and alpha = 2.*.Float.pi/.40. in
  for k=0 to 40-1 do
    let i = float_of_int k in
    tableau_coord.(!compteur) <- (x + int_of_float(radius *. cos(i *. alpha)), y + int_of_float(radius*.sin(i *. alpha)));
    tableau_coord2.(!compteur) <- ( x +int_of_float((radius -. 5.) *. cos(i *. alpha)), y + int_of_float((radius -. 5.)*.sin(i *. alpha)));
    compteur := !compteur + 1;
    let distance = (radius*.cos(alpha/.2.) +. sqrt(cote*.cote -. (radius*.sin(alpha/.2.))*.(radius*.sin(alpha/.2.)))) in
    tableau_coord.(!compteur) <- (x + int_of_float (cos(i *. alpha +. alpha/.2.)*.distance),
                                  y + int_of_float (sin(i *. alpha +. alpha/.2.)*.distance));
    tableau_coord2.(!compteur) <- (x + int_of_float (cos(i *. alpha +. alpha/.2.)*.(distance -. 5.)),
                                   y + int_of_float (sin(i *. alpha +. alpha/.2.)*.(distance -. 5.)));
    compteur := !compteur + 1
  done;
  if (color = "green")
    then
      begin
        set_color (rgb 57 230 20);
        fill_poly (tableau_coord);
        set_color (rgb 57 255 20);
        fill_poly (tableau_coord2)
      end
    else
      begin
        set_color (rgb 240 125 115);
        fill_poly (tableau_coord)
      end;;

let drawEntities player entities drawFunc = 
  let mainEntityCord = player#getMainEntity#getCoordonees in
  List.iter (fun entity ->
  let radius = entity#getRadius in
  let realX = int_of_float((entity#getCoordonees.x -. mainEntityCord.x) *. player#getRatio) + size_x () / 2
  and realY = int_of_float((entity#getCoordonees.y -. mainEntityCord.y) *. player#getRatio) + size_y() / 2 
  in if realX > 0 - radius && realX < size_x () + radius && realY > 0 - radius && realY < size_y () + radius then
    drawFunc realX realY entity#getColor radius )
  entities;; 

let drawGrid player =
  let gridSpacing = 60
  and mainEntity = player#getMainEntity in
  let currentCord = mainEntity#getCoordonees
in let gridStartX = gridSpacing - int_of_float (currentCord.x *. player#getRatio) mod gridSpacing
  and gridStartY = gridSpacing - int_of_float (currentCord.y *. player#getRatio) mod gridSpacing
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

(*let spawn_buisson_feed player bushesFood = 
  let theta = Random.float (2.*.Float.pi) in
    let newFood = new entity {x = float_of_int(bushesFood#getRadius)*.cos(theta); y = float_of_int(bushesFood#getRadius)*.sin(theta);} (generate_color ()) 9 Food
    and marge = Random.int 20 in 
    for i = 0 to 40 + marge do
      newFood#addCoordonees ({x = float_of_int((bushesFood#getRadius+ i)) *. cos(theta); y = float_of_int((bushesFood#getRadius+i))*.sin(theta)}); 
      drawEntities player [newFood] player#getDrawFoodFun
    done;; *)

let generate_bushes = 
  for _i = 0 to maxBushes do
    bushes := (new entity (generate_cord ()) 0 60 Bush)::!bushes
  done;;

let player = new player (-1) [new entity (generate_cord ()) (generate_color ()) 10 Player];;

let generate_bushesFood =
  for _i = 0 to maxBushesFood do
    bushesFood := (new entity (generate_cord ()) 0 60 Bush)::!bushesFood
  done;;


let rec event_loop time serverData = 
  clear_graph();

  (* UPDATES *)
  let newTime = Unix.gettimeofday () and (mousex, mousey) = mouse_pos () in
  let deltaTime = newTime -. time in
  player#updatePlayerSpeed mousex mousey;
  player#updateCoords deltaTime;

  (* MONITORING *)
  moveto 5 (size_y () - 15);
  set_color black;
  (match serverData with 
  | None -> draw_string "Singleplayer";
  | Some (input, output) -> player#updateServer input output; draw_string "Multiplayer");

  let playerCord = player#getMainEntity#getCoordonees in
  moveto 5 (size_y () - 30); draw_string (Printf.sprintf "Position: %0.0f, %0.0f" playerCord.x  playerCord.y);

  (match serverData with
    | None -> ()
    | Some _  -> moveto 5 (size_y () - 45);
      draw_string (Printf.sprintf "Connected : %d " (List.length (!other_players) + 1)));

  (* DRAWING *)
  player#destroyFoods;
  player#handleFoodCollisions;
  player#generateFoods;

  drawGrid player;

  drawEntities player player#getFoods (fun x y color radius -> (set_color color; fill_circle x y radius));

  drawMainPlayer player;
  
  drawEntities player !bushes (fun x y _color radius -> drawBush x y (float_of_int radius) "green");
  drawEntities player !bushesFood (fun x y _color radius -> drawBush x y (float_of_int radius) "red"); 
  (*spawn_buisson_feed player (List.hd (!bushesFood)); *)

  (match serverData with 
    | None -> ();
    | Some (_) -> List.iter (fun otherPlayer -> otherPlayer#updateCoords deltaTime; drawEntities player (otherPlayer#getEntities) (fun x y color radius -> set_color color; fill_circle x y radius)) (!other_players));

  synchronize ();
  event_loop newTime serverData;;

let open_connection () = 
    let server_address = 
      let addr = Unix.inet_addr_of_string "129.151.252.122" and port = 8086 
      in Unix.ADDR_INET(addr, port)
      in Unix.open_connection server_address;;

let open_window () = 
  open_graph "";
  resize_window 800 600;
  set_window_title "Agar.ml";
  display_mode false;
  auto_synchronize false;;

let handle_incoming_data input = 
  try 
  while true do
    let data = input_line input in 
    incomingData := data::!incomingData;
  done;
  with End_of_file -> ();;


let () = 
  open_window (); 
  try
    (* On ouvre une connection avec le serveur *)
    let (input, output) = open_connection () in
    ignore (Thread.create handle_incoming_data input);
    event_loop (Unix.gettimeofday ()) (Some (incomingData, output));
  with Unix.Unix_error (Unix.ECONNREFUSED, "connect", "") -> event_loop (Unix.gettimeofday ()) None;;
