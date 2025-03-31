type 'a t = 'a -> Protoc_fmt.t -> string

module Api : sig
  val encode_error : Api_piqi.error t
  val encode_infos_base : Api_piqi.infos_base t
  val encode_reference_person : Api_piqi.reference_person t
  val encode_base_warnings : Api_piqi.base_warnings t
  val encode_list_persons : Api_piqi.list_persons t
  val encode_list_full_persons : Api_piqi.list_full_persons t
  val encode_event_query_result_list : Api_piqi.event_query_result_list t
  val encode_graph : Api_piqi.graph t
  val encode_full_graph : Api_piqi.full_graph t
  val encode_person : Api_piqi.person t
  val encode_full_person : Api_piqi.full_person t
  val encode_list_images : Api_piqi.list_images t
  val encode_image_address : Api_piqi.image_address t
  val encode_list_full_families : Api_piqi.list_full_families t
  val encode_history : Api_piqi.history t
end

module Api_saisie_write : sig
  val encode_auto_complete_result : Api_saisie_write_piqi.auto_complete_result t
  val encode_person_search_list : Api_saisie_write_piqi.person_search_list t
  val encode_person_search_info : Api_saisie_write_piqi.person_search_info t
  val encode_config : Api_saisie_write_piqi.config t
  val encode_modification_status : Api_saisie_write_piqi.modification_status t
  val encode_person : Api_saisie_write_piqi.person t
  val encode_add_family : Api_saisie_write_piqi.add_family t
  val encode_edit_family_request : Api_saisie_write_piqi.edit_family_request t
  val encode_edit_family : Api_saisie_write_piqi.edit_family t
  val encode_add_parents : Api_saisie_write_piqi.add_parents t
  val encode_add_child : Api_saisie_write_piqi.add_child t
  val encode_add_sibling : Api_saisie_write_piqi.add_sibling t
end

module Api_stats : sig
  val encode_stats : Api_stats_piqi.stats t
end

module Api_saisie_read : sig
  val encode_person : Api_saisie_read_piqi.person t
  val encode_graph_tree : Api_saisie_read_piqi.graph_tree t
  val encode_nb_ancestors : Api_saisie_read_piqi.nb_ancestors t
end

val encode_int32 : int32 t
