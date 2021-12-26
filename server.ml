#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

open Unix;;

let connections = ref [];;
let taille = ref 0;;

let execute (input, output) =
  let connectionID = !taille in 
  connections := (connectionID, (input, output))::!connections;
  taille := !taille + 1;
  try 
  while true do
    let incomingData = input_line input in
    List.iter (fun (id, (clientInput, clientOutput)) -> 
      if connectionID <> id then 
        begin 
          output_string clientOutput ((Printf.sprintf "%d " connectionID) ^ incomingData ^ "\n"); 
          flush clientOutput;
        end;) !connections;
  done;
with End_of_file -> () | Sys_error "Connection reset by peer" -> connections := List.filter (fun (id, _) -> id <> connectionID) !connections; close_out output; Thread.exit ();;
let init () = 
  let port = 8086 in
  let socket_address = ADDR_INET (inet_addr_of_string "0.0.0.0", port) in
  let sckt = socket PF_INET SOCK_STREAM 0 in
  bind sckt socket_address;
  listen sckt 256;
  sckt;;

let accept_connection sckt =
  while true do 
    let (s_descr,_) = (accept sckt) in
    let input = in_channel_of_descr s_descr and output = out_channel_of_descr s_descr in
    ignore (Thread.create execute (input, output));
  done;;


accept_connection (init ());;