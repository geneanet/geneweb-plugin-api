type 'a t = 'a -> Protoc_fmt.t -> string

let encode translate encode_pb encode_json data = function
  | Protoc_fmt.Protobuf ->
      let encoder = Pbrt.Encoder.create () in
      encode_pb (translate data) encoder;
      Pbrt.Encoder.to_string encoder
  | Protoc_fmt.Json ->
      let yojson = encode_json (translate data) in
      Yojson.Basic.to_string yojson

module Api = struct
  let encode_error =
    encode Translate.Api.PiqiToProtoc.error Api_protoc.encode_pb_error
      Api_protoc.encode_json_error

  let encode_infos_base =
    encode Translate.Api.PiqiToProtoc.infos_base Api_protoc.encode_pb_infos_base
      Api_protoc.encode_json_infos_base

  let encode_reference_person =
    encode Translate.Api.PiqiToProtoc.reference_person
      Api_protoc.encode_pb_reference_person
      Api_protoc.encode_json_reference_person

  let encode_base_warnings =
    encode Translate.Api.PiqiToProtoc.base_warnings
      Api_protoc.encode_pb_base_warnings Api_protoc.encode_json_base_warnings

  let encode_list_persons =
    encode Translate.Api.PiqiToProtoc.list_persons
      Api_protoc.encode_pb_list_persons Api_protoc.encode_json_list_persons

  let encode_list_full_persons =
    encode Translate.Api.PiqiToProtoc.list_full_persons
      Api_protoc.encode_pb_list_full_persons
      Api_protoc.encode_json_list_full_persons

  let encode_event_query_result_list =
    encode Translate.Api.PiqiToProtoc.event_query_result_list
      Api_protoc.encode_pb_event_query_result_list
      Api_protoc.encode_json_event_query_result_list

  let encode_graph =
    encode Translate.Api.PiqiToProtoc.graph Api_protoc.encode_pb_graph
      Api_protoc.encode_json_graph

  let encode_full_graph =
    encode Translate.Api.PiqiToProtoc.full_graph Api_protoc.encode_pb_full_graph
      Api_protoc.encode_json_full_graph

  let encode_person =
    encode Translate.Api.PiqiToProtoc.person Api_protoc.encode_pb_person
      Api_protoc.encode_json_person

  let encode_full_person =
    encode Translate.Api.PiqiToProtoc.full_person
      Api_protoc.encode_pb_full_person Api_protoc.encode_json_full_person

  let encode_list_images =
    encode Translate.Api.PiqiToProtoc.list_images
      Api_protoc.encode_pb_list_images Api_protoc.encode_json_list_images

  let encode_image_address =
    encode Translate.Api.PiqiToProtoc.image_address
      Api_protoc.encode_pb_image_address Api_protoc.encode_json_image_address

  let encode_list_full_families =
    encode Translate.Api.PiqiToProtoc.list_full_families
      Api_protoc.encode_pb_list_full_families
      Api_protoc.encode_json_list_full_families

  let encode_history =
    encode Translate.Api.PiqiToProtoc.history Api_protoc.encode_pb_history
      Api_protoc.encode_json_history
end

module Api_saisie_write = struct
  let encode_auto_complete_result =
    encode Translate.Api_saisie_write.PiqiToProtoc.auto_complete_result
      Api_saisie_write_protoc.encode_pb_auto_complete_result
      Api_saisie_write_protoc.encode_json_auto_complete_result

  let encode_person_search_list =
    encode Translate.Api_saisie_write.PiqiToProtoc.person_search_list
      Api_saisie_write_protoc.encode_pb_person_search_list
      Api_saisie_write_protoc.encode_json_person_search_list

  let encode_person_search_info =
    encode Translate.Api_saisie_write.PiqiToProtoc.person_search_info
      Api_saisie_write_protoc.encode_pb_person_search_info
      Api_saisie_write_protoc.encode_json_person_search_info

  let encode_config =
    encode Translate.Api_saisie_write.PiqiToProtoc.config
      Api_saisie_write_protoc.encode_pb_config
      Api_saisie_write_protoc.encode_json_config

  let encode_modification_status =
    encode Translate.Api_saisie_write.PiqiToProtoc.modification_status
      Api_saisie_write_protoc.encode_pb_modification_status
      Api_saisie_write_protoc.encode_json_modification_status

  let encode_person =
    encode Translate.Api_saisie_write.PiqiToProtoc.person
      Api_saisie_write_protoc.encode_pb_person
      Api_saisie_write_protoc.encode_json_person

  let encode_add_family =
    encode Translate.Api_saisie_write.PiqiToProtoc.add_family
      Api_saisie_write_protoc.encode_pb_add_family
      Api_saisie_write_protoc.encode_json_add_family

  let encode_edit_family_request =
    encode Translate.Api_saisie_write.PiqiToProtoc.edit_family_request
      Api_saisie_write_protoc.encode_pb_edit_family_request
      Api_saisie_write_protoc.encode_json_edit_family_request

  let encode_edit_family =
    encode Translate.Api_saisie_write.PiqiToProtoc.edit_family
      Api_saisie_write_protoc.encode_pb_edit_family
      Api_saisie_write_protoc.encode_json_edit_family

  let encode_add_parents =
    encode Translate.Api_saisie_write.PiqiToProtoc.add_parents
      Api_saisie_write_protoc.encode_pb_add_parents
      Api_saisie_write_protoc.encode_json_add_parents

  let encode_add_child =
    encode Translate.Api_saisie_write.PiqiToProtoc.add_child
      Api_saisie_write_protoc.encode_pb_add_child
      Api_saisie_write_protoc.encode_json_add_child

  let encode_add_sibling =
    encode Translate.Api_saisie_write.PiqiToProtoc.add_sibling
      Api_saisie_write_protoc.encode_pb_add_sibling
      Api_saisie_write_protoc.encode_json_add_sibling
end

module Api_stats = struct
  let encode_stats =
    encode Translate.Api_stats.PiqiToProtoc.stats
      Api_stats_protoc.encode_pb_stats Api_stats_protoc.encode_json_stats
end

module Api_saisie_read = struct
  let encode_person =
    encode Translate.Api_saisie_read.PiqiToProtoc.person
      Api_saisie_read_protoc.encode_pb_person
      Api_saisie_read_protoc.encode_json_person

  let encode_graph_tree =
    encode Translate.Api_saisie_read.PiqiToProtoc.graph_tree
      Api_saisie_read_protoc.encode_pb_graph_tree
      Api_saisie_read_protoc.encode_json_graph_tree

  let encode_nb_ancestors =
    encode Translate.Api_saisie_read.PiqiToProtoc.nb_ancestors
      Api_saisie_read_protoc.encode_pb_nb_ancestors
      Api_saisie_read_protoc.encode_json_nb_ancestors
end

let encode_int32 i = function
  | Protoc_fmt.Json ->
    let json = `Assoc ["value", `String (Int32.to_string i)] in
    Yojson.Basic.to_string json
  | Protobuf ->
    let encoder = Pbrt.Encoder.create () in
    Pbrt.Encoder.wrapper_int32_value (Some i) encoder;
    Pbrt.Encoder.to_string encoder
