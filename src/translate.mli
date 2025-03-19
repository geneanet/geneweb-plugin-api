
module Api : sig
  module PiqiToProtoc : sig
    (* val truc : Api_piqi.truc -> Api_protoc.truc *)
    val reference_person : Api_piqi.reference_person -> Api_protoc.reference_person
    val search_params : Api_piqi.search_params -> Api_protoc.search_params
    val list_reference_person : Api_piqi.list_reference_persons -> Api_protoc.list_reference_persons
    val index : Api_piqi.index -> Api_protoc.index
    val close_persons_params : Api_piqi.close_persons_params -> Api_protoc.close_persons_params
    val person_start : Api_piqi.person_start -> Api_protoc.person_start
    val error : Api_piqi.error -> Api_protoc.error
    val list_persons : Api_piqi.list_persons -> Api_protoc.list_persons
    val list_full_persons : Api_piqi.list_full_persons -> Api_protoc.list_full_persons
  end
  module ProtocToPiqi : sig
    (* val truc : Api_protoc.truc -> Api_piqi.truc *)
    val reference_person : Api_protoc.reference_person -> Api_piqi.reference_person
    val search_params : Api_protoc.search_params -> Api_piqi.search_params
    val list_reference_person : Api_protoc.list_reference_persons -> Api_piqi.list_reference_persons
    val index : Api_protoc.index -> Api_piqi.index
    val close_persons_params : Api_protoc.close_persons_params -> Api_piqi.close_persons_params
    val person_start : Api_protoc.person_start -> Api_piqi.person_start
    val events_query_params : Api_protoc.events_query_params -> Api_piqi.events_query_params
  end
end
module Api_saisie_write : sig
  module PiqiToProtoc : sig
    (* val truc : Api_saisie_write_piqi.truc -> Api_saisie_write_protoc.truc *)
    val auto_complete : Api_saisie_write_piqi.auto_complete -> Api_saisie_write_protoc.auto_complete
    val person_search_list : Api_saisie_write_piqi.person_search_list -> Api_saisie_write_protoc.person_search_list
    val person_search_list_params : Api_saisie_write_piqi.person_search_list_params -> Api_saisie_write_protoc.person_search_list_params
    val index_person : Api_saisie_write_piqi.index_person -> Api_saisie_write_protoc.index_person
    val person : Api_saisie_write_piqi.person -> Api_saisie_write_protoc.person
    val index_person_and_family : Api_saisie_write_piqi.index_person_and_family -> Api_saisie_write_protoc.index_person_and_family
  end
  module ProtocToPiqi : sig
    (* val truc : Api_saisie_write_protoc.truc -> Api_saisie_write_piqi.truc *)
    val auto_complete : Api_saisie_write_protoc.auto_complete -> Api_saisie_write_piqi.auto_complete
    val person_search_list : Api_saisie_write_protoc.person_search_list -> Api_saisie_write_piqi.person_search_list
    val person_search_list_params : Api_saisie_write_protoc.person_search_list_params -> Api_saisie_write_piqi.person_search_list_params
    val index_person : Api_saisie_write_protoc.index_person -> Api_saisie_write_piqi.index_person
    val person : Api_saisie_write_protoc.person -> Api_saisie_write_piqi.person
    val index_person_and_family : Api_saisie_write_protoc.index_person_and_family -> Api_saisie_write_piqi.index_person_and_family
  end
end
