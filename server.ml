#load "unix.cma";;
open Unix;;

let connections = ref [];;
let taille = ref 0;;

let execute input output =
  let connectionID = !taille in 
  connections := (connectionID, (input, output))::(!connections);
  incr taille;
  try 
  while true do
    let incomingData = input_line input in
    List.iter (fun (id, (input, output)) -> 
      if connectionID <> id then 
        begin 
          output_string output ((Printf.sprintf "%d," connectionID) ^ incomingData ^ "\n"); 
          flush output;
        end;) !connections;
  done;
with End_of_file -> () | Sys_error "Connection reset by peer" -> connections := List.filter (fun (id, _) -> id <> connectionID) !connections;;
  
let port = 8086 in
let socket_address = ADDR_INET (inet_addr_of_string "0.0.0.0", port) in
establish_server execute socket_address;;
