type 'a t = 'a -> Protoc_fmt.t -> string

let encode translate encode_pb encode_json data = function
  | Protoc_fmt.Protobuf ->
    let encoder = Pbrt.Encoder.create () in
    encode_pb (translate data) encoder;
    Pbrt.Encoder.to_string encoder    
  | Protoc_fmt.Json ->
    let yojson = encode_json (translate data) in
    Yojson.Basic.to_string yojson

let encode f g d = encode (fun _ -> assert false) f g d                
module Api = struct
  let encode_error =
    encode Api_protoc.encode_pb_error Api_protoc.encode_json_error

  let encode_infos_base =
    encode Api_protoc.encode_pb_infos_base Api_protoc.encode_json_infos_base

  let encode_reference_person =
    encode Api_protoc.encode_pb_reference_person Api_protoc.encode_json_reference_person

  let encode_base_warnings =
    encode Api_protoc.encode_pb_base_warnings Api_protoc.encode_json_base_warnings

  let encode_list_persons =
    encode Api_protoc.encode_pb_list_persons Api_protoc.encode_json_list_persons
  let encode_list_full_persons =
    encode Api_protoc.encode_pb_list_full_persons Api_protoc.encode_json_list_full_persons
  let encode_event_query_result_list = assert false
  let encode_graph = assert false
  let encode_full_graph = assert false
  let encode_person = assert false
  let encode_full_person = assert false
  let encode_list_images = assert false
  let encode_image_address = assert false
  let encode_list_full_families = assert false
  let encode_history = assert false
end

module Api_saisie_write = struct
  let encode_auto_complete_result =
    encode Api_saisie_write_protoc.encode_pb_auto_complete_result Api_saisie_write_protoc.encode_json_auto_complete_result

  let encode_person_search_list = encode Api_saisie_write_protoc.encode_pb_person_search_list Api_saisie_write_protoc.encode_json_person_search_list

  let encode_person_search_info = encode Api_saisie_write_protoc.encode_pb_person_search_info Api_saisie_write_protoc.encode_json_person_search_info

  let encode_config = encode Api_saisie_write_protoc.encode_pb_config Api_saisie_write_protoc.encode_json_config

  let encode_modification_status = encode Api_saisie_write_protoc.encode_pb_modification_status Api_saisie_write_protoc.encode_json_modification_status

  let encode_person = encode Api_saisie_write_protoc.encode_pb_person Api_saisie_write_protoc.encode_json_person

  let encode_add_family =
    encode Api_saisie_write_protoc.encode_pb_add_family Api_saisie_write_protoc.encode_json_add_family

  let encode_edit_family_request = assert false
  let encode_edit_family = assert false
  let encode_edit_family_ok = assert false
  let encode_add_parents = assert false
  let encode_add_child = assert false
  let encode_add_sibling = assert false
  
end

module Api_stats = struct
  let encode_stats = assert false
end

module Api_saisie_read = struct
  let encode_person = assert false
  let encode_graph_tree = assert false
  let encode_nb_ancestors = assert false
end
