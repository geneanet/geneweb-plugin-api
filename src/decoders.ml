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

  let decode_graph_params =
    decode
      Translate.Api.ProtocToPiqi.graph_params
      Api_protoc.decode_pb_graph_params
      Api_protoc.decode_json_graph_params

  let decode_graph_rel_params =
    decode
      Translate.Api.ProtocToPiqi.graph_rel_params
      Api_protoc.decode_pb_graph_rel_params
      Api_protoc.decode_json_graph_rel_params
  let decode_cpl_rel_params =
    decode
      Translate.Api.ProtocToPiqi.cpl_rel_params
      Api_protoc.decode_pb_cpl_rel_params
      Api_protoc.decode_json_cpl_rel_params
  let decode_reference_person_i =
    decode
      Translate.Api.ProtocToPiqi.reference_person_i
      Api_protoc.decode_pb_reference_person_i
      Api_protoc.decode_json_reference_person_i
  let decode_last_modifications =
    decode
      Translate.Api.ProtocToPiqi.last_modifications
      Api_protoc.decode_pb_last_modifications
      Api_protoc.decode_json_last_modifications
  let decode_last_visits =
    decode
      Translate.Api.ProtocToPiqi.last_visits
      Api_protoc.decode_pb_last_visits
      Api_protoc.decode_json_last_visits
  let decode_all_persons_params =
    decode
      Translate.Api.ProtocToPiqi.all_persons_params
      Api_protoc.decode_pb_all_persons_params
      Api_protoc.decode_json_all_persons_params
  let decode_all_families_params =
    decode
      Translate.Api.ProtocToPiqi.all_families_params
      Api_protoc.decode_pb_all_families_params
      Api_protoc.decode_json_all_families_params
  let decode_list_pers_img =
    decode
      Translate.Api.ProtocToPiqi.list_pers_img
      Api_protoc.decode_pb_list_pers_img
      Api_protoc.decode_json_list_pers_img
  let decode_history_request =
    decode
      Translate.Api.ProtocToPiqi.history_request
      Api_protoc.decode_pb_history_request
      Api_protoc.decode_json_history_request
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

  let decode_add_family_ok =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_family_ok
      Api_saisie_write_protoc.decode_pb_add_family_ok
      Api_saisie_write_protoc.decode_json_add_family_ok
  let decode_add_child_request =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_child_request
      Api_saisie_write_protoc.decode_pb_add_child_request
      Api_saisie_write_protoc.decode_json_add_child_request
  let decode_edit_family_ok =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.edit_family_ok
      Api_saisie_write_protoc.decode_pb_edit_family_ok
      Api_saisie_write_protoc.decode_json_edit_family_ok
  let decode_add_child_ok =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_child_ok
      Api_saisie_write_protoc.decode_pb_add_child_ok
      Api_saisie_write_protoc.decode_json_add_child_ok
  let decode_add_parents_ok =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_parents_ok
      Api_saisie_write_protoc.decode_pb_add_parents_ok
      Api_saisie_write_protoc.decode_json_add_parents_ok
  let decode_add_sibling_request =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_sibling_request
      Api_saisie_write_protoc.decode_pb_add_sibling_request
      Api_saisie_write_protoc.decode_json_add_sibling_request
  let decode_add_sibling_ok =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_sibling_ok
      Api_saisie_write_protoc.decode_pb_add_sibling_ok
      Api_saisie_write_protoc.decode_json_add_sibling_ok
  let decode_add_first_fam =
    decode
      Translate.Api_saisie_write.ProtocToPiqi.add_first_fam
      Api_saisie_write_protoc.decode_pb_add_first_fam
      Api_saisie_write_protoc.decode_json_add_first_fam
end

module Api_stats = struct
  let decode_stats_params =
    decode
      Translate.Api_stats.ProtocToPiqi.stats_params
      Api_stats_protoc.decode_pb_stats_params
      Api_stats_protoc.decode_json_stats_params
end

module Api_saisie_read = struct
  let decode_graph_tree_params =
    decode
      Translate.Api_saisie_read.ProtocToPiqi.graph_tree_params
      Api_saisie_read_protoc.decode_pb_graph_tree_params
      Api_saisie_read_protoc.decode_json_graph_tree_params
  let decode_index_person =
    decode
      Translate.Api_saisie_read.ProtocToPiqi.index_person
      Api_saisie_read_protoc.decode_pb_index_person
      Api_saisie_read_protoc.decode_json_index_person
  let decode_fiche_parameters =
    decode
      Translate.Api_saisie_read.ProtocToPiqi.fiche_parameters
      Api_saisie_read_protoc.decode_pb_fiche_parameters
      Api_saisie_read_protoc.decode_json_fiche_parameters
  let decode_identifier_person =
    decode
      Translate.Api_saisie_read.ProtocToPiqi.identifier_person
      Api_saisie_read_protoc.decode_pb_identifier_person
      Api_saisie_read_protoc.decode_json_identifier_person
end
