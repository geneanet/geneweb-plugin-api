type 'a t = string -> Protoc_fmt.t -> 'a

let decode translate decode_pb decode_json s fmt = match fmt with
  | Protoc_fmt.Protobuf -> translate (decode_pb (Pbrt.Decoder.of_string s))
  | Json -> translate (decode_json (Yojson.Basic.from_string s))

module Api = struct
  let decode_reference_person =
    decode
      Translate.Api.ProtocToPiqi.reference_person
      Api_protoc.decode_pb_reference_person
      Api_protoc.decode_json_reference_person

  let decode_search_params =
    decode Translate.Api.ProtocToPiqi.search_params
      Api_protoc.decode_pb_search_params
      Api_protoc.decode_json_search_params

  let decode_list_reference_persons =
    decode Translate.Api.ProtocToPiqi.list_reference_person
      Api_protoc.decode_pb_list_reference_persons
      Api_protoc.decode_json_list_reference_persons

  let decode_index =
    decode
      Translate.Api.ProtocToPiqi.index
      Api_protoc.decode_pb_index
      Api_protoc.decode_json_index

  let decode_close_persons_params =
    decode
      Translate.Api.ProtocToPiqi.close_persons_params
      Api_protoc.decode_pb_close_persons_params
      Api_protoc.decode_json_close_persons_params

  let decode_person_start =
    decode
      Translate.Api.ProtocToPiqi.person_start
      Api_protoc.decode_pb_person_start
      Api_protoc.decode_json_person_start

  let decode_events_query_params =
    decode
      Translate.Api.ProtocToPiqi.events_query_params
      Api_protoc.decode_pb_events_query_params
      Api_protoc.decode_json_events_query_params 

  let decode_graph_params = assert false
  let decode_graph_rel_params = assert false
  let decode_cpl_rel_params = assert false
  let decode_reference_person_i = assert false
  let decode_last_modifications = assert false
  let decode_last_visits = assert false
  let decode_all_persons_params = assert false
  let decode_all_families_params = assert false
  let decode_list_pers_img = assert false
  let decode_history_request = assert false
end

module Api_saisie_write = struct
  let decode_auto_complete =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.auto_complete
      Api_saisie_write_protoc.decode_pb_auto_complete
      Api_saisie_write_protoc.decode_json_auto_complete
  let decode_person_search_list_params =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.person_search_list_params
      Api_saisie_write_protoc.decode_pb_person_search_list_params
      Api_saisie_write_protoc.decode_json_person_search_list_params
  let decode_index_person =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.index_person
      Api_saisie_write_protoc.decode_pb_index_person
      Api_saisie_write_protoc.decode_json_index_person
  let decode_person =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.person
      Api_saisie_write_protoc.decode_pb_person
      Api_saisie_write_protoc.decode_json_person
  let decode_index_person_and_family =
    decode Translate.Api_saisie_write.ProtocToPiqi.index_person_and_family
      Api_saisie_write_protoc.decode_pb_index_person_and_family
      Api_saisie_write_protoc.decode_json_index_person_and_family

  let decode_add_family_ok = assert false
  let decode_add_child_request = assert false
  let decode_edit_family_ok = assert false
  let decode_add_child_ok = assert false
  let decode_add_parents_ok = assert false
  let decode_add_sibling_request = assert false
  let decode_add_sibling_ok = assert false
  let decode_add_first_fam = assert false
end

module Api_stats = struct
  let decode_stats_params = assert false
end

module Api_saisie_read = struct
  let decode_graph_tree_params = assert false
  let decode_index_person = assert false
  let decode_fiche_parameters = assert false
  let decode_identifier_person = assert false
end
