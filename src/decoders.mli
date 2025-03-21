type 'a t = string -> Protoc_fmt.t -> 'a

module Api : sig
  val decode_reference_person : Api_piqi.reference_person t
  val decode_reference_person_i : Api_piqi.reference_person_i t
  val decode_search_params : Api_piqi.search_params t
  val decode_list_reference_persons : Api_piqi.list_reference_persons t
  val decode_index : Api_piqi.index t
  val decode_close_persons_params : Api_piqi.close_persons_params t
  val decode_person_start : Api_piqi.person_start t
  val decode_events_query_params : Api_piqi.events_query_params t
  val decode_graph_params : Api_piqi.graph_params t
  val decode_graph_rel_params : Api_piqi.graph_rel_params t
  val decode_cpl_rel_params : Api_piqi.cpl_rel_params t
  val decode_last_modifications : Api_piqi.last_modifications t
  val decode_last_visits : Api_piqi.last_visits t
  val decode_list_pers_img : Api_piqi.list_pers_img t
  val decode_all_persons_params : Api_piqi.all_persons_params t
  val decode_all_families_params : Api_piqi.all_families_params t
  val decode_history_request : Api_piqi.history_request t
end

module Api_saisie_write : sig
  val decode_auto_complete : Api_saisie_write_piqi.auto_complete t

  val decode_person_search_list_params :
    Api_saisie_write_piqi.person_search_list_params t

  val decode_index_person : Api_saisie_write_piqi.index_person t
  val decode_person : Api_saisie_write_piqi.person t

  val decode_index_person_and_family :
    Api_saisie_write_piqi.index_person_and_family t

  val decode_add_family_ok : Api_saisie_write_piqi.add_family_ok t
  val decode_add_child_request : Api_saisie_write_piqi.add_child_request t
  val decode_edit_family_ok : Api_saisie_write_piqi.edit_family_ok t
  val decode_add_child_ok : Api_saisie_write_piqi.add_child_ok t
  val decode_add_parents_ok : Api_saisie_write_piqi.add_parents_ok t
  val decode_add_sibling_request : Api_saisie_write_piqi.add_sibling_request t
  val decode_add_sibling_ok : Api_saisie_write_piqi.add_sibling_ok t
  val decode_add_first_fam : Api_saisie_write_piqi.add_first_fam t
end

module Api_stats : sig
  val decode_stats_params : Api_stats_piqi.stats_params t
end

module Api_saisie_read : sig
  val decode_graph_tree_params : Api_saisie_read_piqi.graph_tree_params t
  val decode_index_person : Api_saisie_read_piqi.index_person t
  val decode_fiche_parameters : Api_saisie_read_piqi.fiche_parameters t
  val decode_identifier_person : Api_saisie_read_piqi.identifier_person t
end
