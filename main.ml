open Graphics;;

exception InvalidResponseType of string;;

exception YouLose;;

Random.init (int_of_float(Unix.time ()));;

type coordonees = {x: float; y: float};;
type nature = Player | Food | Bush;;
type state = Splitting | Assembling | Idle;;
(* La taille de la map *)
let map = ({x = 0.; y = 0.;}, {x = 2000.; y = 2000.});;
(* Le nombre maximal de nourriture que le joueur peut afficher *)
let maxFood = 250;;
(* La rayon maximale d'apparition de la nourriture *)
let spawnFoodRadius = 300.;;
(* La rayon maximale de disparition de la nourriture *)
let despawnFoodRadius = 350.;;
let maxBushes = 20;;

(* alphabet *)
let array_tracer_lettre = [|
[|(0.,0.,0.75,2.);(0.75,2.,1.5,0.);(0.35,1.,1.,1.)|];
[|(0.,0.,0.,2.);(0.,2.,1.,2.);(1.,2.,1.,1.);(0.,0.,1.5,0.);(1.5,1.,1.5,0.);(0.,1.,1.5,1.)|];
[|(1.5,2.,0.,2.);(0.,0.,0.,2.);(1.5,0.,0.,0.)|];
[|(0.,0.,0.,2.);(0.,2.,0.8,2.);(0.8,2.,1.5,1.4);(1.5,0.6,1.5,1.4);(1.5,0.6,0.8,0.);(0.8,0.,0.,0.)|];
[|(1.5,2.,0.,2.);(0.,2.,0.,0.);(0.,0.,1.5,0.);(0.7,1.,0.,1.)|];
[|(0.,0.,0.,2.);(0.,2.,1.5,2.);(0.75,1.,0.,1.)|];
[|(1.5,2.,0.,2.);(0.,2.,0.,0.);(0.,0.,1.5,0.);(1.5,0.,1.5,1.);(1.5,1.,0.8,1.)|];
[|(0.,2.,0.,0.);(1.5,2.,1.5,0.);(0.,1.,1.5,1.)|];
[|(0.75,1.75,0.75,0.)|];
[|(0.,2.,1.5,2.);(0.75,2.,0.75,0.);(0.,0.,0.75,0.)|];
[|(0.,2.,0.,0.);(1.5,2.,0.22,1.);(0.22,1.,1.5,0.);(0.,1.,0.22,1.)|];
[|(0.,2.,0.,0.);(0.,0.,1.5,0.)|];
[|(0.,0.,0.,2.);(0.,2.,0.75,1.1);(0.75,1.1,1.5,2.);(1.5,2.,1.5,0.)|];
[|(0.,0.,0.,2.);(0.,2.,1.5,0.);(1.5,0.,1.5,2.)|];
[|(0.,0.,1.5,0.);(1.5,0.,1.5,2.);(1.5,2.,0.,2.);(0.,2.,0.,0.)|];
[|(0.,0.,0.,2.);(0.,2.,1.5,2.);(1.5,2.,1.5,1.);(1.5,1.,0.,1.)|];
[|(0.,0.,0.,2.);(1.5,2.,0.,2.);(1.5,2.,1.5,0.);(0.,0.,1.5,0.);(0.88,0.59,1.4,-.0.3)|];
[|(0.,0.,0.,2.);(0.,2.,1.5,2.);(1.5,1.,1.5,2.);(1.5,1.,0.,1.);(0.7,1.,1.5,0.)|];
[|(1.5,2.,0.,2.);(0.,2.,0.,1.);(0.,1.,1.5,1.);(1.5,1.,1.5,0.);(1.5,0.,0.,0.)|];
[|(0.,2.,1.5,2.);(0.75,2.,0.75,0.)|];
[|(0.,2.,0.,0.);(0.,0.,1.5,0.);(1.5,0.,1.5,2.)|];
[|(0.,2.,0.75,0.);(1.5,2.,0.75,0.)|];
[|(0.,2.,0.3,0.);(0.3,0.,0.75,0.5);(0.75,0.5,1.2,0.);(1.5,2.,1.2,0.)|];
[|(0.,2.,1.5,0.);(1.5,2.,0.,0.)|];
[|(1.5,2.,0.,0.);(0.,2.,0.75,1.)|];
[|(0.,2.,1.5,2.);(0.,0.,1.5,2.);(0.,0.,1.5,0.)|];
|];;

(* Cette liste stocke toutes les informations reçus entre chaque frame par le joueur *)
let incomingData = ref [];;
let bushes = ref [];;
(* Stocke les autres joueurs *)
let other_players = ref [];;

(* Permet de générer des coordonnées aléatoires *)
let generate_cord () = 
  let min, max = map in
  let x = Random.float (max.x -. min.x) +. min.x
  and y = Random.float (max.y -. min.y) +. min.y
  in
  {x = x; y = y};;  
(* Permet de générer une couleur aléatoire *)
let generate_color () = 
  let r = Random.int (255) + 1
  and g = Random.int (255) + 1  
  and b = Random.int (255) + 1
in rgb r g b;;

let string_to_entityName s = List.map (fun x -> if x = ' ' then -1 else int_of_char x - 65 ) (List.init (String.length s) (String.get s))
(* détécte la collision entre deux entités *)
let collision entity1 entity2 scaleRatio = sqrt((entity1#getCoordonees.x -. entity2#getCoordonees.x)**2. +. (entity1#getCoordonees.y -. entity2#getCoordonees.y)**2.) *. scaleRatio < float_of_int (entity1#getRadius + entity2#getRadius);;

let mange entity target scaleRatio =
  let distance = sqrt ((entity#getCoordonees.x -. target#getCoordonees.x)**2. +. (entity#getCoordonees.y -. target#getCoordonees.y)**2.) in
  float_of_int entity#getRadius >= distance *. scaleRatio && float_of_int entity#getRadius >= float_of_int target#getRadius *. 1.3

class entity id name coord color masse nature state = 
  object (self)
    val id = (id: int)
    val name = (name: int list)
    val nature = (nature: nature)
    val mutable coordonees = (coord : coordonees)
    val mutable speed = ({ x = 0.; y = 0.;} : coordonees)
    val mutable color = (color : color)
    val mutable masse = (masse : int)
    val mutable state = (state: state);
    (* temps en seconde avant le prochain changement d'état de l'entité *)
    val mutable timeToNextState = (0. : float);
    (* si on doit notifier les autres joueurs *)
    val mutable needUpdate = (false: bool);

    method getName = name;
    method getState = state;
    method getTimeToNextState = timeToNextState;
    method setTimeToNextState (newTime : float) = timeToNextState <- newTime;
    method getCoordonees = coordonees;
    method getColor = color;
    method getRadius = match nature with
      | Player -> 30 + int_of_float ((float_of_int masse)**0.7)
      | Food -> 8
      | Bush -> masse;
    method getSpeed = speed;
    method setSpeed newSpeed = speed <- newSpeed;
    method getId = id;
    method needUpdate = needUpdate;
    method setState newState = state <- newState;
    (* Permet de mettre à jour l'état de l'entité *)
    method updateState deltat (player: player) = 
      if timeToNextState > deltat then timeToNextState <- timeToNextState -. deltat else timeToNextState <- 0.;
      match state with
      | Idle -> ();
      | Splitting -> if self#getTimeToNextState <= 0. then state <- Assembling;
      | Assembling -> 
        (* l'entité peut se fusioner avec une entité de masse suffisante *)
        try 
          let eater =  List.hd (List.sort (fun e1 e2 -> e2#getMasse - e1#getMasse) 
          (List.filter(fun entity -> entity#getId <> self#getId && mange entity self player#getRatio) player#getEntities));
          in eater#addMasse (self#getMasse);
          player#removeEntityById self#getId;
        with Failure _ -> ();

    (* on notifie les autres clients si le joueur change de direction *)
    method updateSpeed (newSpeed: coordonees) deltat mainEntityPosition = 
      match state with
      | Idle when nature = Player -> needUpdate <- newSpeed.x <> speed.x || newSpeed.y <> speed.y; speed <- newSpeed; 
      | Splitting -> speed <- {x = speed.x +. deltat *. 4.5 *. (mainEntityPosition.x -. coordonees.x); y = speed.y +. deltat *. 4.5 *. ((mainEntityPosition.y -. coordonees.y))};
      | Assembling -> speed <- {x = (mainEntityPosition.x -. coordonees.x) /. 3.; y = (mainEntityPosition.y -. coordonees.y) /. 3.};
      | Idle -> ();

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
    method setMasse newMasse = masse <- newMasse;
end

and player id name score entities = 
  object (self)
    val name = (name: string)
    val id = (id: int)
    val mutable entities = (entities : entity list);
    val mutable score = (score : int);
    val mutable foods = ([]: entity list);
    val mutable needUpdate = (true: bool);
    val mutable entityIdCounter = (1 : int);
    val mutable eaten = [];

    method getId = id
    method getName = name;
    method getMainEntity = List.hd entities
    method getEntities = entities;
    method addEntity entity = entities <- entities@[entity];
    method removeEntityById id = entities <- List.filter (fun entity -> entity#getId <> id) entities; 
    (* permet de mettre à jour l'entité principale lorsque qu'elle s'est faite manger *)
    method updateMainEntity =
      if  entities <> [] then
        begin
        let newEntities = (List.sort (fun e1 e2 -> e2#getMasse - e1#getMasse) entities) in
          let candidate = List.hd newEntities in
          candidate#setState Idle;
          entities <- newEntities;
        end;
    method getScore = score;
    method getSpeed = 60. *. exp (-. float_of_int (score) /. 750.) +. 45.;
    method getFoods = foods;
    (* permet d'harmoniser l'affichage des différentes entités  *)
    method getRatio = 4.;
    method updateCoords deltat = List.iter(fun entity -> entity#updateCoords deltat) entities;
    method updatePlayerSpeed mousex mousey deltat = 
      let mainEntity = self#getMainEntity
      and vector = {x = float_of_int (mousex - size_x () / 2); y = float_of_int (mousey - size_y () / 2)}
    in let norme = sqrt(vector.x *. vector.x +. vector.y *. vector.y)
    in let normalizedVector = match norme with
    | norme when norme < float_of_int mainEntity#getRadius *. 0.15 -> {x = 0.; y = 0.}
    | norme when norme < float_of_int mainEntity#getRadius -> {x = vector.x /. (norme *. 5.); y = vector.y /. (norme *. 5.)}
    | _ -> {x = vector.x /. norme; y = vector.y /. norme}
      in 
      List.iter (fun entity -> entity#updateSpeed { x = normalizedVector.x *. self#getSpeed; y = normalizedVector.y *. self#getSpeed } deltat mainEntity#getCoordonees) entities;
      needUpdate <- List.exists (fun entity -> entity#needUpdate) entities;
  method generateFoods = 
    let i = ref (List.length foods);
    and min, max = map in
    while !i < maxFood do
      let playerCord = self#getMainEntity#getCoordonees in
      let x = (if Random.bool () then 1. else -1.) *. Random.float(spawnFoodRadius) +. playerCord.x
      and y = (if Random.bool () then 1. else -1.) *. Random.float(spawnFoodRadius) +. playerCord.y in
    if x > min.x && x < max.x && y > min.y && y < max.y then
      let newFood = new entity (-1) [] {x = x; y = y;} (generate_color ()) 1 Food Idle;
      in foods <- newFood :: foods;
    i := !i + 1;
    done;
  method destroyFoods = foods <- List.filter (fun food -> 
      let foodCord = food#getCoordonees
      and playerCord = self#getMainEntity#getCoordonees
        in if abs_float (foodCord.x -. playerCord.x) > despawnFoodRadius || abs_float (foodCord.y -. playerCord.y) > despawnFoodRadius then false else true) foods;
  method handleFoodCollisions = foods <- List.filter (fun food -> 
    (List.exists (fun entity -> if collision entity food self#getRatio then let () = self#addScore entity food#getMasse in true else false) entities) <> true
    ) foods;
  method addScore entity quantity = score <- score + quantity; entity#addMasse quantity; needUpdate <- true;
  method removeScore quantity = score <- score - quantity; needUpdate <- true;
  method updateServer incomingData output =
    (* SENDING DATA *)
    begin
      if needUpdate then
      let dataToSend = ref (Printf.sprintf "UPDATE %d %d %s," id score name) in
      List.iter (fun entity -> let id = entity#getId and speed = entity#getSpeed and coordonees = entity#getCoordonees in dataToSend := !dataToSend ^ (Printf.sprintf "%d %0.2f %0.2f %0.2f %0.2f %d %d,"id coordonees.x coordonees.y speed.x speed.y entity#getMasse entity#getColor)) entities;
      output_string output (!dataToSend ^ "\n");
      flush output; 
      needUpdate <- false;
    end;

    (* RECEIVING DATA *)
    List.iter (fun data -> 
      match (String.split_on_char ',' data) with
      | [] -> ()
      | playerData::entitesData -> 
        let (responseType, playerId) = Scanf.sscanf playerData "%s %d" (fun x y -> (x, y))
        in match responseType with 
          | "UPDATE" ->
            let pScore, pName = Scanf.sscanf playerData "%s %d %d %s" (fun _ _ x y -> (x, y)) in
            let entities = List.filter (fun entity -> not (List.exists (fun (pId, eId) -> (pId = playerId && eId = entity#getId)) eaten)) (List.map (fun entityData -> 
            let (entityId, coordonees, speed, masse, color) = Scanf.sscanf entityData "%d %f %f %f %f %d %d" (fun id x y sx sy masse color -> (id, {x = x; y = y;}, {x = sx; y = sy}, masse, color))
              in
              let entity = new entity entityId (string_to_entityName pName) coordonees color masse Player Idle in ignore (entity#setSpeed speed); entity) (List.filter (fun str -> str <> "") entitesData))
            in let player = new player playerId pName pScore entities 
            in other_players := player::(List.filter (fun player -> player#getId <> playerId) !other_players);
          | "DISCONNECT" -> other_players := List.filter (fun p -> p#getId <> playerId) !other_players;
          | "EAT" ->  (* lorsque une entité d'un autre joueur se fait manger se fait manger *)
            let entityId = Scanf.sscanf (List.hd entitesData) "%d" (fun id -> id) in
            (match id with
            (* si un autre joueur a mangé une entité *)
            | id when id = playerId -> let isMainEntityEaten = entityId = self#getMainEntity#getId and masse = (List.find (fun entity -> entity#getId = entityId) entities)#getMasse in
                self#removeEntityById entityId;
                self#removeScore masse;
                if entities = [] then (close_out_noerr output; raise YouLose;) else (if isMainEntityEaten then self#updateMainEntity);
            (* si cette entité appartient à un autre joueur *)
            | _ -> let otherPlayer = List.find (fun p -> p#getId = playerId) !other_players in otherPlayer#removeEntityById entityId)
          | _ -> raise (InvalidResponseType responseType);
      )
      (!incomingData);
    incomingData := [];

  (* separe l'entité principale *)
  method split =
    let mainEntity = self#getMainEntity in
    (if mainEntity#getMasse >= 50 && List.for_all (fun entity -> mainEntity#getMasse * 3 / 4 > entity#getMasse  ) (List.tl entities) then 
      let masse = mainEntity#getMasse / 4 and id = entityIdCounter
      in entityIdCounter <- entityIdCounter + 1;
      mainEntity#setMasse (mainEntity#getMasse - masse);
      let newEntity = new entity id (string_to_entityName self#getName) {x = mainEntity#getCoordonees.x; y = mainEntity#getCoordonees.y} (mainEntity#getColor) masse Player Splitting in
      newEntity#setTimeToNextState 1.;
      let vector = mainEntity#getSpeed in let norme = sqrt(vector.x *. vector.x +. vector.y *. vector.y)
      in let entitySpeed = {x = vector.x *. 300. /. norme; y = vector.y *. 300. /. norme}
      in newEntity#setSpeed entitySpeed;
      self#addEntity newEntity);

    (* manger les autres joueurs *)
    method eat output = 
      List.iter (fun otherPlayer ->
        
        List.iter (fun otherEntity ->

        let candidates = List.filter (fun entity -> mange entity otherEntity self#getRatio) entities 
        in if candidates <> [] then
        let candidate = List.hd (List.sort (fun e1 e2 -> e2#getMasse - e1#getMasse) candidates) 
        in 
          eaten <- (otherPlayer#getId, otherEntity#getId)::eaten;
          self#addScore candidate otherEntity#getMasse;
          otherPlayer#removeEntityById otherEntity#getId;
          (* on notifie les autres joueurs que on mange une entité *)
          output_string output (Printf.sprintf "EAT %d,%d\n" otherPlayer#getId otherEntity#getId);
          flush output;
          ) (List.filter (fun entity -> not (List.exists (fun (pId, eId) -> pId = otherPlayer#getId && eId = entity#getId) eaten)) otherPlayer#getEntities);

        ) !other_players;

end;;

(* permet d'afficher le nom de l'entité *)
let drawEntityName x y name rayon = 
  moveto x y;
  set_line_width 4;
  set_color white;
  let iter = ref 1 
  and nombreLettre = float_of_int (List.length name) in
  let coef = ((1.8 *. (float_of_int rayon)) -. 10. *. (nombreLettre)) /. (nombreLettre *. 1.5) in 
  let debutx = float_of_int x -. (float_of_int rayon *. 0.9) -. 1.5 *. coef -. 5.
  and debuty = float_of_int y -. coef in

   List.iter (fun letter -> (match letter with 
  | -1 -> ()
  | k -> let decallage = debutx +. float_of_int(!iter)*.(1.5*.coef +. 10.) in
          draw_segments (Array.map (fun (a, b, c, d) -> (int_of_float(coef*.a +. decallage), int_of_float(coef*.b +. debuty), int_of_float(coef*.c +. decallage), int_of_float(coef*.d +. debuty))) array_tracer_lettre.(k) ));
          incr iter;   
          ) (List.rev name);;

let drawMainPlayer player =
  let mainEntity = player#getMainEntity in
  set_color mainEntity#getColor;
  fill_circle (size_x() / 2) (size_y () / 2) (mainEntity#getRadius);
  drawEntityName (size_x () / 2) (size_y () / 2) (mainEntity#getName) (mainEntity#getRadius);;

(* dessine un buisson *)
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
        fill_poly (tableau_coord);
      end;;

(* permet de tracer une liste d'entités au bonne endroit sur l'écran par rapport à sa position la position relative entre chaque entités et le joueur *)
let drawEntities player entities drawFunc = 
  let mainEntityCord = player#getMainEntity#getCoordonees in
  List.iter (fun entity ->
  let radius = entity#getRadius in
  let realX = int_of_float((entity#getCoordonees.x -. mainEntityCord.x) *. player#getRatio) + size_x () / 2
  and realY = int_of_float((entity#getCoordonees.y -. mainEntityCord.y) *. player#getRatio) + size_y() / 2 
  in if realX > 0 - radius && realX < size_x () + radius && realY > 0 - radius && realY < size_y () + radius then
    drawFunc realX realY entity#getColor radius; 
    if entity#getName <> [] then drawEntityName realX realY entity#getName entity#getRadius;
  )
  entities;; 

(* trace la grille en fond *)
let drawGrid player =
  set_line_width 0;
  let gridSpacing = int_of_float (15. *. player#getRatio)
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

let generate_bushes () = 
  for i = 0 to maxBushes do
    bushes := (new entity i [] (generate_cord ()) 0 60 Bush Idle)::!bushes
  done;;

let getOtherPlayersEntites () = List.flatten (List.map (fun otherPlayer -> otherPlayer#getEntities) !other_players);;

(* recupere toutes les entités issues d'un joueur *)
let getPlayersEntities player func = List.sort (fun e1 e2 -> e2#getMasse - e1#getMasse) (List.filter func ((getOtherPlayersEntites ())@(List.tl player#getEntities)));;

(* boucle principale de jeu *)
let rec event_loop player time serverData = 
  clear_graph();

  (* mise a jour *)
  let newTime = Unix.gettimeofday () and (mousex, mousey) = mouse_pos () in
  let deltaTime = newTime -. time in

  List.iter (fun entity -> entity#updateState deltaTime player) player#getEntities;

  player#updatePlayerSpeed mousex mousey deltaTime;
  player#updateCoords deltaTime;

  (if key_pressed () then 
      let key = read_key () in 
      match int_of_char key with 
      | 32 -> player#split; (* press space bar *)
      | _ -> ());

  (* affichage des differents textes *)
  moveto 5 (size_y () - 15);
  set_color black;
  (match serverData with 
  | None -> draw_string "Singleplayer";
  | Some (input, output) -> player#updateServer input output; List.iter (fun otherPlayer -> otherPlayer#updateCoords deltaTime) !other_players; player#eat output;  draw_string "Multiplayer");

  let playerCord = player#getMainEntity#getCoordonees in
  moveto 5 (size_y () - 30); draw_string (Printf.sprintf "Position : %0.0f, %0.0f" playerCord.x  playerCord.y);

  moveto (size_x () - 125) (size_y () - 15);
  draw_string "Press SPACE to split";

  moveto (size_x () - 125) (size_y () - 30);
  draw_string (Printf.sprintf "Votre score : %d" player#getScore);

  (match serverData with
    | None -> ()
    | Some _  -> moveto 5 (size_y () - 45);
      draw_string (Printf.sprintf "Connected : %d " (List.length (!other_players) + 1));
      
      let i = ref 0 in
      List.iter (fun p -> moveto (size_x () - 125) (size_y () - 45 - !i * 15);
        draw_string (Printf.sprintf "%s score : %d" p#getName p#getScore);
        incr i;
      ) (List.sort (fun p1 p2 -> p2#getScore - p1#getScore) !other_players);
      );



  
  (* affichage des entités *)
  player#destroyFoods;
  player#handleFoodCollisions;
  player#generateFoods;

  drawGrid player;

  drawEntities player player#getFoods (fun x y color radius -> (set_color color; fill_circle x y radius));

  drawEntities player (getPlayersEntities player (fun entity -> entity#getMasse <= player#getMainEntity#getMasse)) (fun x y color radius -> (set_color color; fill_circle x y radius));
  drawMainPlayer player;
  drawEntities player (getPlayersEntities player (fun entity -> entity#getMasse > player#getMainEntity#getMasse)) (fun x y color radius -> (set_color color; fill_circle x y radius));
  if serverData = None then drawEntities player !bushes (fun x y _color radius -> drawBush x y (float_of_int radius) "green");

  synchronize ();
  event_loop player newTime serverData;;


let open_connection () = 
    let server_address = let addr = Unix.inet_addr_of_string "129.151.252.122" and port = 8086 in Unix.ADDR_INET(addr, port)
    in Unix.open_connection server_address;;

let handle_incoming_data input = 
  try 
  while true do
    let data = input_line input in 
    incomingData := data::!incomingData;
  done;
  with End_of_file -> ();;

(* permet d'afficher une lettre sur le menu principal *)
let affiche_lettre list_lettre = 
  set_line_width 4;
  set_color white;
  fill_rect (size_x()*5/16) (size_y()*7/24) (size_x()*6/16) (size_y()*1/12);
  set_color black;
  draw_rect (size_x()*5/16) (size_y()*7/24) (size_x()*6/16) (size_y()*1/12);
  let iter = ref 1 
  and debutx = float_of_int(size_x())*.5./.16.
  and debuty = float_of_int(size_y())*.15./.48. 
  and coef = (float_of_int(size_y())*.1./.12.)/.5.5 in 
  List.iter (fun letter -> (match letter with 
  | -1 -> ()
  | k -> let decallage = debutx +. float_of_int(!iter)*.(1.5*.coef +. 10.) in
          draw_segments (Array.map (fun (a, b, c, d) -> (int_of_float(coef*.a +. decallage), int_of_float(coef*.b +. debuty), int_of_float(coef*.c +. decallage), int_of_float(coef*.d +. debuty))) array_tracer_lettre.(k) ));
          incr iter;   
          ) (List.rev list_lettre);
  synchronize ();;


let circles = Array.init 50 (fun _i -> let x = Random.float 1. and y = Random.float 1. and color = generate_color () in (x, y, color, Random.int (10)+20) );;
(* boucle de menu *)
let rec menu_loop windowX windowY pseudo = 
  (* on stocke windowX et windowY afin de détécter les redimensionnements de la fenêtre *)
  if((!windowX) <> size_x () || (!windowY) <> size_y ()) then
    begin
      (* on doit réafficher le menu *)
      windowX := size_x ();
      windowY := size_y ();
      clear_graph ();

      (* on trace la grille *)
      set_line_width 0;
        let gridSpacing = 40
      in let gridStartX = 0
        and gridStartY = 0
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
      done;
      
      (* on trace les cercles *)
      Array.iter (fun (x, y, color, radius) -> set_color color; fill_circle (int_of_float(x *. float_of_int (size_x ()))) (int_of_float(y *. float_of_int (size_y ()))) radius) circles;
      (* on trace AGAR.ML *)
      let lettre1 = [|(0.,0.);(1.,3.);(2.289,3.005);(2.903,0.);(2.,0.);(1.811,1.001);(1.245,0.980);(1.,0.)|]
      and lettre2 = [|(1.397,2.421);(1.320,1.692);(1.870,1.816);(1.893,2.439)|]
      and lettre3 = [|(2.495,1.999);(3.,3.);(4.296,3.010);(4.5,2.5);(3.5,2.);(3.5,1.);(4.370,1.277);(3.949,1.568);(4.413,1.865);(4.834,1.488);(4.791,0.498);(4.299,0.);(2.904,0.)|]
      and lettre4 = [|(4.296,0.);(5.5,3.);(6.177,3.019);(7.,0.);(6.098,0.);(6.,1.);(5.499,0.816);(5.279,0.)|]
      and lettre5 = [|(5.703,2.385);(5.592,1.664);(6.016,1.844);(6.,2.4)|]
      and lettre6 = [|(6.668,3.011);(8.,3.);(8.485,2.168);(7.738,1.475);(8.345,0.833);(8.626,0.);(7.862,0.);(7.475,0.772);(7.,0.);(6.453,2.007)|]
      and lettre7 = [|(7.203,2.493);(7.669,2.256);(7.089,1.817)|]
      and lettre8 = [|(8.626,0.);(8.837,0.711);(9.361,0.741);(9.636,0.)|]
      and lettre9 = [|(9.452,1.683);(9.832,2.996);(10.396,3.006);(10.683,1.714);(10.467,0.);(9.636,0.)|]
      and lettre10 = [|(9.636,0.);(9.761,0.741);(9.5,2.);(10.,3.);(10.496,2.61);(10.970,1.608);(11.22,2.663);(11.81,2.985);(12.051,1.725);(12.328,0.);(11.613,0.);(11.363,1.036);(10.755,0.724);(10.263,1.331);(10.228,0.598);(10.63,0.)|]
      and lettre11 = [|(12.051,1.725);(12.355,2.985);(13.133,3.);(12.945,1.724);(13.061,0.715);(14.170,1.01);(14.679,0.);(12.328,0.)|]
      and coef1 = (float_of_int(size_x())*.7./.8. -. float_of_int(size_x())/.8.)/.14.68
      and coef2 = (float_of_int(size_y())/.2. -. float_of_int(size_y())/.6.)/.3.
      in 
      let coef = if coef1<coef2 then coef1 else coef2 in 

      set_color black;
      Array.iter 
        (fun lettre -> fill_poly (Array.map (fun (x, y) -> (int_of_float (coef*.x +. float_of_int(size_x())/.8.), int_of_float (coef*.y +. float_of_int(size_y())/.2.))) lettre))
        [|lettre1; lettre3; lettre4; lettre6; lettre8; lettre9; lettre10; lettre11 |];
      set_color white;
      Array.iter 
        (fun lettre -> fill_poly (Array.map (fun (x, y) -> (int_of_float (coef*.x +. float_of_int(size_x())/.8.), int_of_float ((coef*.y +. float_of_int(size_y())/.2.)))) lettre))
        [| lettre2; lettre5; lettre7 |];
  
      (* on trace les bouttons *)
      set_line_width 4;
      set_color green;
      fill_rect (size_x()/4) (size_y()*3/24) (size_x()*3/16) (size_y()*1/12);

      set_color red;
      fill_rect (size_x()*9/16) (size_y()*3/24) (size_x()*3/16) (size_y()*1/12);

      set_color white;
      let iter = ref 0 
      and debutx1 = float_of_int(size_x())*.21./.80.
      and debuty1 = float_of_int(size_y())*.7./.48. 
      and coef = (float_of_int(size_y())*.1./.12.)/.4. in 
      Array.iter (fun letter -> let decallage = debutx1 +. float_of_int(!iter)*.(1.5*.coef +. 20.) in
              draw_segments (Array.map (fun (a, b, c, d) -> (int_of_float(coef*.a +. decallage), int_of_float(coef*.b +. debuty1), int_of_float(coef*.c +. decallage), int_of_float(coef*.d +. debuty1))) array_tracer_lettre.((int_of_char letter)-97)  );
              incr iter;   
        )([|'p';'l';'a';'y'|]);
      
      iter := 0; 
      let debutx2 = float_of_int(size_x())*.46./.80.
      and debuty2 = float_of_int(size_y())*.7./.48. 
      and coef = (float_of_int(size_y())*.1./.12.)/.4. in 
      Array.iter (fun letter -> let decallage = debutx2 +. float_of_int(!iter)*.(1.5*.coef +. 20.) in
              draw_segments (Array.map (fun (a, b, c, d) -> (int_of_float(coef*.a +. decallage), int_of_float(coef*.b +. debuty2), int_of_float(coef*.c +. decallage), int_of_float(coef*.d +. debuty2))) array_tracer_lettre.((int_of_char letter)-97)  );
              incr iter;   
        )([|'q';'u';'i';'t'|]);

      (* on trace la zone de texte *)
      set_color white;
      fill_rect (size_x()*5/16) (size_y()*7/24) (size_x()*6/16) (size_y()*1/12);

      set_color black;
      draw_rect (size_x()*5/16) (size_y()*7/24) (size_x()*6/16) (size_y()*1/12);

      if !pseudo <> [] then affiche_lettre (!pseudo);
    
      synchronize ();
    end;

    (* gestion de l'interface *)
    if button_down () then
      begin match mouse_pos () with
      | mouse_x, mouse_y when mouse_x>=size_x()*5/16 && mouse_x<=size_x()*11/16 && mouse_y>= size_y()*7/24 && mouse_y<= size_y()*9/24
        -> while key_pressed () do ignore (read_key ()) done; (* gestion de la zone de texte *)
          let buttonPressedOut = ref false in
          while( not !buttonPressedOut) do
            if key_pressed () then
              begin
                let lettre = read_key () in
                let intlettre = int_of_char lettre in
                match intlettre with 
                  | 8 -> if !pseudo <> [] then (pseudo := List.tl (!pseudo); affiche_lettre (!pseudo))
                  | 32 when List.length !pseudo < 8 -> (pseudo := (-1)::(!pseudo);) 
                  | intlettre when List.length !pseudo < 8 
                                  && ((intlettre >=65 && intlettre <= 90) || (intlettre >= 97 && intlettre <= 122)) 
                                  -> pseudo := (((int_of_char lettre)-65) mod 32)::(!pseudo);
                                  affiche_lettre (!pseudo);
                  | _ -> ()
              end;
          let (mouse_x, mouse_y) = mouse_pos() in
          if (button_down () && not (mouse_x>=size_x()*5/16 && mouse_x<=size_x()*11/16 && mouse_y>= size_y()*7/24 && mouse_y<= size_y()*9/24))
            then (buttonPressedOut := true)
          done;
      | mouse_x, mouse_y when mouse_x >=size_x()/4 && mouse_x<=size_x()*7/16 && mouse_y>= size_y()*3/24 && mouse_y<= size_y()*5/24
        -> begin (* on commence la partie *)
          try 
            let defaultEntityMasse = 0 and playerName = String.of_seq (List.to_seq (List.map (fun c -> if c = -1 then ' ' else char_of_int (c+65)) !pseudo))  in 
              try
                (* On ouvre une connection avec le serveur *)
                let (input, output) = open_connection () in
                (* on recupere l'identifiant du joueur auprès du serveur *)
                output_string output "GETPLAYERID\n";
                flush output;
                let id = Scanf.sscanf (input_line input) "%d" (fun x -> x) in
                let player = new player id playerName defaultEntityMasse [new entity 0 !pseudo (generate_cord ()) (generate_color ()) defaultEntityMasse Player Idle] in
                ignore (Thread.create handle_incoming_data input);
                event_loop player (Unix.gettimeofday ()) (Some (incomingData, output));
              with Unix.Unix_error _ -> 
                generate_bushes (); 
                let player = new player (-1) playerName defaultEntityMasse [new entity 0 !pseudo (generate_cord ()) (generate_color ()) defaultEntityMasse Player Idle] in 
                event_loop player (Unix.gettimeofday ()) None 
              with YouLose -> windowX := 0; windowY := 0;
            end;
      | mouse_x, mouse_y when mouse_x >=size_x()*9/16 && mouse_x<=size_x()*12/16 && mouse_y>= size_y()*3/24 && mouse_y<= size_y()*5/24 
        -> close_graph ();
      | _ -> ()
      end;
  menu_loop windowX windowY pseudo;;


let open_window () = 
  open_graph "";
  resize_window 800 600;
  set_window_title "Agar.ml";
  display_mode false;
  remember_mode false;
  auto_synchronize false;;

let () = 
  open_window ();
  menu_loop (ref 0) (ref 0) (ref []);;
