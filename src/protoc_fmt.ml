type t = Protobuf | Json
let protobuf = Protobuf
let json = Json

let to_piqi = function
  | Protobuf -> `pb
  | Json -> `json
