

type t = Csv.t

let load_from_file ~file =
  try
    Csv.load ~backslash_escape:true file
  with Csv.Failure (nrecord, nfield, msg) ->
    !Geneweb.GWPARAM.syslog
      `LOG_ERR
      ("failed to load csv from :" ^ file
       ^ " " ^ string_of_int nrecord
       ^ " " ^ string_of_int nfield
       ^ " " ^ msg);
    []
    
(* TODO catch exceptions *)
let of_string s =
  let in_chan = Csv.of_string ~backslash_escape:true s in
  let csv = Csv.input_all in_chan in
  csv

let row_of_string s =  match of_string s with
  | [l] -> l
  | _   -> []

let row_of_string_opt s = match row_of_string s with
  | []        -> None
  | _  as row -> Some row
       
  
let fold_left = List.fold_left
