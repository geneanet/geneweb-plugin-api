module Api : sig
  module PiqiToProtoc : sig
    val error : Api_piqi.error -> Api_protoc.error
    val infos_base : Api_piqi.infos_base -> Api_protoc.infos_base

    val reference_person :
      Api_piqi.reference_person -> Api_protoc.reference_person

    val base_warnings : Api_piqi.base_warnings -> Api_protoc.base_warnings
    val list_persons : Api_piqi.list_persons -> Api_protoc.list_persons

    val list_full_persons :
      Api_piqi.list_full_persons -> Api_protoc.list_full_persons

    val event_query_result_list :
      Api_piqi.event_query_result_list -> Api_protoc.event_query_result_list

    val graph : Api_piqi.graph -> Api_protoc.graph
    val full_graph : Api_piqi.full_graph -> Api_protoc.full_graph
    val person : Api_piqi.person -> Api_protoc.person
    val full_person : Api_piqi.full_person -> Api_protoc.full_person
    val list_images : Api_piqi.list_images -> Api_protoc.list_images
    val image_address : Api_piqi.image_address -> Api_protoc.image_address

    val list_full_families :
      Api_piqi.list_full_families -> Api_protoc.list_full_families

    val history : Api_piqi.history -> Api_protoc.history
  end

  (* module PiqiToProtoc : sig
     (* val truc : Api_piqi.truc -> Api_protoc.truc *)

     (*    val search_params : Api_piqi.search_params -> Api_protoc.search_params*)
     val list_reference_person : Api_piqi.list_reference_persons -> Api_protoc.list_reference_persons
     val index : Api_piqi.index -> Api_protoc.index
     val close_persons_params : Api_piqi.close_persons_params -> Api_protoc.close_persons_params
     val person_start : Api_piqi.person_start -> Api_protoc.person_start

     val list_persons : Api_piqi.list_persons -> Api_protoc.list_persons
     val list_full_persons : Api_piqi.list_full_persons -> Api_protoc.list_full_persons
     end*)
  module ProtocToPiqi : sig
    (* val truc : Api_protoc.truc -> Api_piqi.truc *)
    val reference_person :
      Api_protoc.reference_person -> Api_piqi.reference_person

    val reference_person_i :
      Api_protoc.reference_person_i -> Api_piqi.reference_person_i

    val search_params : Api_protoc.search_params -> Api_piqi.search_params

    val list_reference_person :
      Api_protoc.list_reference_persons -> Api_piqi.list_reference_persons

    val index : Api_protoc.index -> Api_piqi.index

    val close_persons_params :
      Api_protoc.close_persons_params -> Api_piqi.close_persons_params

    val person_start : Api_protoc.person_start -> Api_piqi.person_start

    val events_query_params :
      Api_protoc.events_query_params -> Api_piqi.events_query_params

    val graph_params : Api_protoc.graph_params -> Api_piqi.graph_params

    val graph_rel_params :
      Api_protoc.graph_rel_params -> Api_piqi.graph_rel_params

    val cpl_rel_params : Api_protoc.cpl_rel_params -> Api_piqi.cpl_rel_params

    val last_modifications :
      Api_protoc.last_modifications -> Api_piqi.last_modifications

    val last_visits : Api_protoc.last_visits -> Api_piqi.last_visits

    val all_persons_params :
      Api_protoc.all_persons_params -> Api_piqi.all_persons_params

    val all_families_params :
      Api_protoc.all_families_params -> Api_piqi.all_families_params

    val list_pers_img : Api_protoc.list_pers_img -> Api_piqi.list_pers_img
    val history_request : Api_protoc.history_request -> Api_piqi.history_request
  end
end

module Api_saisie_write : sig
  module PiqiToProtoc : sig
    val auto_complete_result :
      Api_saisie_write_piqi.auto_complete_result ->
      Api_saisie_write_protoc.auto_complete_result

    val person_search_list :
      Api_saisie_write_piqi.person_search_list ->
      Api_saisie_write_protoc.person_search_list

    val person_search_info :
      Api_saisie_write_piqi.person_search_info ->
      Api_saisie_write_protoc.person_search_info

    val config : Api_saisie_write_piqi.config -> Api_saisie_write_protoc.config

    val modification_status :
      Api_saisie_write_piqi.modification_status ->
      Api_saisie_write_protoc.modification_status

    val person : Api_saisie_write_piqi.person -> Api_saisie_write_protoc.person

    val add_family :
      Api_saisie_write_piqi.add_family -> Api_saisie_write_protoc.add_family

    val edit_family_request :
      Api_saisie_write_piqi.edit_family_request ->
      Api_saisie_write_protoc.edit_family_request

    val edit_family :
      Api_saisie_write_piqi.edit_family -> Api_saisie_write_protoc.edit_family

    val edit_family_ok :
      Api_saisie_write_piqi.edit_family_ok ->
      Api_saisie_write_protoc.edit_family_ok

    val add_parents :
      Api_saisie_write_piqi.add_parents -> Api_saisie_write_protoc.add_parents

    val add_child :
      Api_saisie_write_piqi.add_child -> Api_saisie_write_protoc.add_child

    val add_sibling :
      Api_saisie_write_piqi.add_sibling -> Api_saisie_write_protoc.add_sibling
    (* val truc : Api_saisie_write_piqi.truc -> Api_saisie_write_protoc.truc *)
    (* val auto_complete : Api_saisie_write_piqi.auto_complete -> Api_saisie_write_protoc.auto_complete
       val person_search_list : Api_saisie_write_piqi.person_search_list -> Api_saisie_write_protoc.person_search_list
       val person_search_list_params : Api_saisie_write_piqi.person_search_list_params -> Api_saisie_write_protoc.person_search_list_params
       val index_person : Api_saisie_write_piqi.index_person -> Api_saisie_write_protoc.index_person
       val person : Api_saisie_write_piqi.person -> Api_saisie_write_protoc.person
         val index_person_and_family : Api_saisie_write_piqi.index_person_and_family -> Api_saisie_write_protoc.index_person_and_family*)
  end

  module ProtocToPiqi : sig
    (* val truc : Api_saisie_write_protoc.truc -> Api_saisie_write_piqi.truc *)
    val auto_complete :
      Api_saisie_write_protoc.auto_complete ->
      Api_saisie_write_piqi.auto_complete

    (*    val person_search_list : Api_saisie_write_protoc.person_search_list -> Api_saisie_write_piqi.person_search_list*)
    val person_search_list_params :
      Api_saisie_write_protoc.person_search_list_params ->
      Api_saisie_write_piqi.person_search_list_params

    val index_person :
      Api_saisie_write_protoc.index_person -> Api_saisie_write_piqi.index_person

    val person : Api_saisie_write_protoc.person -> Api_saisie_write_piqi.person

    val index_person_and_family :
      Api_saisie_write_protoc.index_person_and_family ->
      Api_saisie_write_piqi.index_person_and_family

    val add_family_ok :
      Api_saisie_write_protoc.add_family_ok ->
      Api_saisie_write_piqi.add_family_ok

    val add_child_request :
      Api_saisie_write_protoc.add_child_request ->
      Api_saisie_write_piqi.add_child_request

    val edit_family_ok :
      Api_saisie_write_protoc.edit_family_ok ->
      Api_saisie_write_piqi.edit_family_ok

    val add_child_ok :
      Api_saisie_write_protoc.add_child_ok -> Api_saisie_write_piqi.add_child_ok

    val add_parents_ok :
      Api_saisie_write_protoc.add_parents_ok ->
      Api_saisie_write_piqi.add_parents_ok

    val add_sibling_request :
      Api_saisie_write_protoc.add_sibling_request ->
      Api_saisie_write_piqi.add_sibling_request

    val add_sibling_ok :
      Api_saisie_write_protoc.add_sibling_ok ->
      Api_saisie_write_piqi.add_sibling_ok

    val add_first_fam :
      Api_saisie_write_protoc.add_first_fam ->
      Api_saisie_write_piqi.add_first_fam
  end
end

module Api_stats : sig
  module PiqiToProtoc : sig
    val stats : Api_stats_piqi.stats -> Api_stats_protoc.stats
  end

  module ProtocToPiqi : sig
    val stats_params :
      Api_stats_protoc.stats_params -> Api_stats_piqi.stats_params
  end
end

module Api_saisie_read : sig
  module PiqiToProtoc : sig
    val person : Api_saisie_read_piqi.person -> Api_saisie_read_protoc.person

    val graph_tree :
      Api_saisie_read_piqi.graph_tree -> Api_saisie_read_protoc.graph_tree

    val nb_ancestors :
      Api_saisie_read_piqi.nb_ancestors -> Api_saisie_read_protoc.nb_ancestors
  end

  module ProtocToPiqi : sig
    val graph_tree_params :
      Api_saisie_read_protoc.graph_tree_params ->
      Api_saisie_read_piqi.graph_tree_params

    val index_person :
      Api_saisie_read_protoc.index_person -> Api_saisie_read_piqi.index_person

    val fiche_parameters :
      Api_saisie_read_protoc.fiche_parameters ->
      Api_saisie_read_piqi.fiche_parameters

    val identifier_person :
      Api_saisie_read_protoc.identifier_person ->
      Api_saisie_read_piqi.identifier_person
  end
end
