let piqi = Api_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _infos_base_piqi_type = Piqirun_ext.find_piqi_type "api/infos-base"
let _reference_person_piqi_type = Piqirun_ext.find_piqi_type "api/reference-person"
let _reference_person_i_piqi_type = Piqirun_ext.find_piqi_type "api/reference-person-i"
let _list_reference_persons_piqi_type = Piqirun_ext.find_piqi_type "api/list-reference-persons"
let _relation_parent_piqi_type = Piqirun_ext.find_piqi_type "api/relation-parent"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api/title"
let _spouse_piqi_type = Piqirun_ext.find_piqi_type "api/spouse"
let _person_piqi_type = Piqirun_ext.find_piqi_type "api/person"
let _full_person_piqi_type = Piqirun_ext.find_piqi_type "api/full-person"
let _full_family_piqi_type = Piqirun_ext.find_piqi_type "api/full-family"
let _internal_int32_piqi_type = Piqirun_ext.find_piqi_type "api/internal-int32"
let _list_persons_piqi_type = Piqirun_ext.find_piqi_type "api/list-persons"
let _list_full_persons_piqi_type = Piqirun_ext.find_piqi_type "api/list-full-persons"
let _list_full_families_piqi_type = Piqirun_ext.find_piqi_type "api/list-full-families"
let _search_params_piqi_type = Piqirun_ext.find_piqi_type "api/search-params"
let _image_piqi_type = Piqirun_ext.find_piqi_type "api/image"
let _list_images_piqi_type = Piqirun_ext.find_piqi_type "api/list-images"
let _pers_img_piqi_type = Piqirun_ext.find_piqi_type "api/pers-img"
let _list_pers_img_piqi_type = Piqirun_ext.find_piqi_type "api/list-pers-img"
let _index_piqi_type = Piqirun_ext.find_piqi_type "api/index"
let _image_address_piqi_type = Piqirun_ext.find_piqi_type "api/image-address"
let _close_persons_params_piqi_type = Piqirun_ext.find_piqi_type "api/close-persons-params"
let _anniversary_params_piqi_type = Piqirun_ext.find_piqi_type "api/anniversary-params"
let _graph_params_piqi_type = Piqirun_ext.find_piqi_type "api/graph-params"
let _graph_rel_params_piqi_type = Piqirun_ext.find_piqi_type "api/graph-rel-params"
let _cpl_rel_params_piqi_type = Piqirun_ext.find_piqi_type "api/cpl-rel-params"
let _node_piqi_type = Piqirun_ext.find_piqi_type "api/node"
let _full_node_piqi_type = Piqirun_ext.find_piqi_type "api/full-node"
let _edge_piqi_type = Piqirun_ext.find_piqi_type "api/edge"
let _graph_piqi_type = Piqirun_ext.find_piqi_type "api/graph"
let _full_graph_piqi_type = Piqirun_ext.find_piqi_type "api/full-graph"
let _all_persons_params_piqi_type = Piqirun_ext.find_piqi_type "api/all-persons-params"
let _all_families_params_piqi_type = Piqirun_ext.find_piqi_type "api/all-families-params"
let _warning_event_piqi_type = Piqirun_ext.find_piqi_type "api/warning-event"
let _warning_person_piqi_type = Piqirun_ext.find_piqi_type "api/warning-person"
let _warning_already_defined_piqi_type = Piqirun_ext.find_piqi_type "api/warning-already-defined"
let _warning_own_ancestor_piqi_type = Piqirun_ext.find_piqi_type "api/warning-own-ancestor"
let _warning_bad_sex_of_married_person_piqi_type = Piqirun_ext.find_piqi_type "api/warning-bad-sex-of-married-person"
let _warning_birth_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-birth-after-death"
let _warning_incoherent_sex_piqi_type = Piqirun_ext.find_piqi_type "api/warning-incoherent-sex"
let _warning_changed_order_of_children_piqi_type = Piqirun_ext.find_piqi_type "api/warning-changed-order-of-children"
let _warning_changed_order_of_marriages_piqi_type = Piqirun_ext.find_piqi_type "api/warning-changed-order-of-marriages"
let _warning_children_not_in_order_piqi_type = Piqirun_ext.find_piqi_type "api/warning-children-not-in-order"
let _warning_dead_too_early_to_be_father_piqi_type = Piqirun_ext.find_piqi_type "api/warning-dead-too-early-to-be-father"
let _warning_incoherent_ancestor_date_piqi_type = Piqirun_ext.find_piqi_type "api/warning-incoherent-ancestor-date"
let _warning_marriage_date_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-marriage-date-after-death"
let _warning_marriage_date_before_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-marriage-date-before-birth"
let _warning_mother_dead_before_child_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-mother-dead-before-child-birth"
let _warning_parent_born_after_child_piqi_type = Piqirun_ext.find_piqi_type "api/warning-parent-born-after-child"
let _warning_parent_too_young_piqi_type = Piqirun_ext.find_piqi_type "api/warning-parent-too-young"
let _warning_possible_duplicate_fam_piqi_type = Piqirun_ext.find_piqi_type "api/warning-possible-duplicate-fam"
let _warning_possible_duplicate_fam_homonymous_piqi_type = Piqirun_ext.find_piqi_type "api/warning-possible-duplicate-fam-homonymous"
let _warning_title_dates_error_piqi_type = Piqirun_ext.find_piqi_type "api/warning-title-dates-error"
let _warning_undefined_sex_piqi_type = Piqirun_ext.find_piqi_type "api/warning-undefined-sex"
let _warning_young_for_marriage_piqi_type = Piqirun_ext.find_piqi_type "api/warning-young-for-marriage"
let _warning_old_for_marriage_piqi_type = Piqirun_ext.find_piqi_type "api/warning-old-for-marriage"
let _warning_parent_too_old_piqi_type = Piqirun_ext.find_piqi_type "api/warning-parent-too-old"
let _warning_close_children_piqi_type = Piqirun_ext.find_piqi_type "api/warning-close-children"
let _warning_distant_children_piqi_type = Piqirun_ext.find_piqi_type "api/warning-distant-children"
let _warning_big_age_between_spouses_piqi_type = Piqirun_ext.find_piqi_type "api/warning-big-age-between-spouses"
let _warning_dead_old_piqi_type = Piqirun_ext.find_piqi_type "api/warning-dead-old"
let _warning_witness_date_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-witness-date-after-death"
let _warning_witness_date_before_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-witness-date-before-birth"
let _warning_event_order_piqi_type = Piqirun_ext.find_piqi_type "api/warning-event-order"
let _base_warnings_piqi_type = Piqirun_ext.find_piqi_type "api/base-warnings"
let _filter_date_piqi_type = Piqirun_ext.find_piqi_type "api/filter-date"
let _filter_date_range_piqi_type = Piqirun_ext.find_piqi_type "api/filter-date-range"
let _filters_piqi_type = Piqirun_ext.find_piqi_type "api/filters"
let _modification_status_piqi_type = Piqirun_ext.find_piqi_type "api/modification-status"
let _person_start_piqi_type = Piqirun_ext.find_piqi_type "api/person-start"
let _last_modifications_piqi_type = Piqirun_ext.find_piqi_type "api/last-modifications"
let _last_visits_piqi_type = Piqirun_ext.find_piqi_type "api/last-visits"
let _dmy_piqi_type = Piqirun_ext.find_piqi_type "api/dmy"
let _date_piqi_type = Piqirun_ext.find_piqi_type "api/date"
let _events_query_params_piqi_type = Piqirun_ext.find_piqi_type "api/events-query-params"
let _event_query_result_piqi_type = Piqirun_ext.find_piqi_type "api/event-query-result"
let _event_query_result_list_piqi_type = Piqirun_ext.find_piqi_type "api/event-query-result-list"
let _name_frequency_result_piqi_type = Piqirun_ext.find_piqi_type "api/name-frequency-result"
let _name_frequency_result_list_piqi_type = Piqirun_ext.find_piqi_type "api/name-frequency-result-list"
let _name_frequency_params_piqi_type = Piqirun_ext.find_piqi_type "api/name-frequency-params"
let _name_frequency_params_type_piqi_type = Piqirun_ext.find_piqi_type "api/name-frequency-params-type"
let _error_piqi_type = Piqirun_ext.find_piqi_type "api/error"
let _error_code_piqi_type = Piqirun_ext.find_piqi_type "api/error-code"
let _time_piqi_type = Piqirun_ext.find_piqi_type "api/time"
let _history_request_piqi_type = Piqirun_ext.find_piqi_type "api/history-request"
let _history_person_piqi_type = Piqirun_ext.find_piqi_type "api/history-person"
let _history_note_piqi_type = Piqirun_ext.find_piqi_type "api/history-note"
let _history_entry_piqi_type = Piqirun_ext.find_piqi_type "api/history-entry"
let _history_piqi_type = Piqirun_ext.find_piqi_type "api/history"
let _sex_piqi_type = Piqirun_ext.find_piqi_type "api/sex"
let _death_type_piqi_type = Piqirun_ext.find_piqi_type "api/death-type"
let _marriage_type_piqi_type = Piqirun_ext.find_piqi_type "api/marriage-type"
let _divorce_type_piqi_type = Piqirun_ext.find_piqi_type "api/divorce-type"
let _relation_parent_type_piqi_type = Piqirun_ext.find_piqi_type "api/relation-parent-type"
let _title_type_piqi_type = Piqirun_ext.find_piqi_type "api/title-type"
let _visibility_piqi_type = Piqirun_ext.find_piqi_type "api/visibility"
let _search_type_piqi_type = Piqirun_ext.find_piqi_type "api/search-type"
let _pevent_name_piqi_type = Piqirun_ext.find_piqi_type "api/pevent-name"
let _fevent_name_piqi_type = Piqirun_ext.find_piqi_type "api/fevent-name"
let _witness_type_piqi_type = Piqirun_ext.find_piqi_type "api/witness-type"
let _calendar_piqi_type = Piqirun_ext.find_piqi_type "api/calendar"
let _precision_piqi_type = Piqirun_ext.find_piqi_type "api/precision"
let _modification_type_piqi_type = Piqirun_ext.find_piqi_type "api/modification-type"


let parse_infos_base ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _infos_base_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_infos_base buf

let parse_reference_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _reference_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_reference_person buf

let parse_reference_person_i ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _reference_person_i_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_reference_person_i buf

let parse_list_reference_persons ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_reference_persons_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_reference_persons buf

let parse_relation_parent ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_relation_parent buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_title buf

let parse_spouse ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _spouse_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_spouse buf

let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_person buf

let parse_full_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_person buf

let parse_full_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_family buf

let parse_internal_int32 ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _internal_int32_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_internal_int32 buf

let parse_list_persons ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_persons_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_persons buf

let parse_list_full_persons ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_full_persons_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_full_persons buf

let parse_list_full_families ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_full_families_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_full_families buf

let parse_search_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _search_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_search_params buf

let parse_image ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _image_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_image buf

let parse_list_images ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_images_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_images buf

let parse_pers_img ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pers_img_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_pers_img buf

let parse_list_pers_img ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_pers_img_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_pers_img buf

let parse_index ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_index buf

let parse_image_address ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _image_address_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_image_address buf

let parse_close_persons_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _close_persons_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_close_persons_params buf

let parse_anniversary_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _anniversary_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_anniversary_params buf

let parse_graph_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_graph_params buf

let parse_graph_rel_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_rel_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_graph_rel_params buf

let parse_cpl_rel_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _cpl_rel_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_cpl_rel_params buf

let parse_node ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_node buf

let parse_full_node ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_node_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_node buf

let parse_edge ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edge_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_edge buf

let parse_graph ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_graph buf

let parse_full_graph ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_graph_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_graph buf

let parse_all_persons_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _all_persons_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_all_persons_params buf

let parse_all_families_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _all_families_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_all_families_params buf

let parse_warning_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_event buf

let parse_warning_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_person buf

let parse_warning_already_defined ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_already_defined_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_already_defined buf

let parse_warning_own_ancestor ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_own_ancestor_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_own_ancestor buf

let parse_warning_bad_sex_of_married_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_bad_sex_of_married_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_bad_sex_of_married_person buf

let parse_warning_birth_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_birth_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_birth_after_death buf

let parse_warning_incoherent_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_incoherent_sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_incoherent_sex buf

let parse_warning_changed_order_of_children ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_changed_order_of_children_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_changed_order_of_children buf

let parse_warning_changed_order_of_marriages ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_changed_order_of_marriages_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_changed_order_of_marriages buf

let parse_warning_children_not_in_order ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_children_not_in_order_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_children_not_in_order buf

let parse_warning_dead_too_early_to_be_father ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_dead_too_early_to_be_father_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_dead_too_early_to_be_father buf

let parse_warning_incoherent_ancestor_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_incoherent_ancestor_date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_incoherent_ancestor_date buf

let parse_warning_marriage_date_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_marriage_date_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_marriage_date_after_death buf

let parse_warning_marriage_date_before_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_marriage_date_before_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_marriage_date_before_birth buf

let parse_warning_mother_dead_before_child_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_mother_dead_before_child_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_mother_dead_before_child_birth buf

let parse_warning_parent_born_after_child ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_parent_born_after_child_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_parent_born_after_child buf

let parse_warning_parent_too_young ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_parent_too_young_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_parent_too_young buf

let parse_warning_possible_duplicate_fam ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_possible_duplicate_fam_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_possible_duplicate_fam buf

let parse_warning_possible_duplicate_fam_homonymous ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_possible_duplicate_fam_homonymous_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_possible_duplicate_fam_homonymous buf

let parse_warning_title_dates_error ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_title_dates_error_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_title_dates_error buf

let parse_warning_undefined_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_undefined_sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_undefined_sex buf

let parse_warning_young_for_marriage ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_young_for_marriage_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_young_for_marriage buf

let parse_warning_old_for_marriage ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_old_for_marriage_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_old_for_marriage buf

let parse_warning_parent_too_old ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_parent_too_old_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_parent_too_old buf

let parse_warning_close_children ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_close_children_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_close_children buf

let parse_warning_distant_children ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_distant_children_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_distant_children buf

let parse_warning_big_age_between_spouses ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_big_age_between_spouses_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_big_age_between_spouses buf

let parse_warning_dead_old ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_dead_old_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_dead_old buf

let parse_warning_witness_date_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_witness_date_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_witness_date_after_death buf

let parse_warning_witness_date_before_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_witness_date_before_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_witness_date_before_birth buf

let parse_warning_event_order ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_event_order_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_event_order buf

let parse_base_warnings ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _base_warnings_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_base_warnings buf

let parse_filter_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filter_date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_filter_date buf

let parse_filter_date_range ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filter_date_range_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_filter_date_range buf

let parse_filters ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filters_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_filters buf

let parse_modification_status ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _modification_status_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_modification_status buf

let parse_person_start ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_start_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_person_start buf

let parse_last_modifications ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _last_modifications_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_last_modifications buf

let parse_last_visits ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _last_visits_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_last_visits buf

let parse_dmy ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_dmy buf

let parse_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_date buf

let parse_events_query_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _events_query_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_events_query_params buf

let parse_event_query_result ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_query_result_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_event_query_result buf

let parse_event_query_result_list ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_query_result_list_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_event_query_result_list buf

let parse_name_frequency_result ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _name_frequency_result_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_name_frequency_result buf

let parse_name_frequency_result_list ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _name_frequency_result_list_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_name_frequency_result_list buf

let parse_name_frequency_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _name_frequency_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_name_frequency_params buf

let parse_name_frequency_params_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _name_frequency_params_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_name_frequency_params_type buf

let parse_error ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _error_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_error buf

let parse_error_code ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _error_code_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_error_code buf

let parse_time ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _time_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_time buf

let parse_history_request ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _history_request_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_history_request buf

let parse_history_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _history_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_history_person buf

let parse_history_note ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _history_note_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_history_note buf

let parse_history_entry ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _history_entry_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_history_entry buf

let parse_history ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _history_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_history buf

let parse_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_sex buf

let parse_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_death_type buf

let parse_marriage_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_marriage_type buf

let parse_divorce_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_divorce_type buf

let parse_relation_parent_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_relation_parent_type buf

let parse_title_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_title_type buf

let parse_visibility ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _visibility_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_visibility buf

let parse_search_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _search_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_search_type buf

let parse_pevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_pevent_name buf

let parse_fevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_fevent_name buf

let parse_witness_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_witness_type buf

let parse_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_calendar buf

let parse_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_precision buf

let parse_modification_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _modification_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_modification_type buf


let gen_infos_base ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_infos_base x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _infos_base_piqi_type `pb format x_pb ?opts

let gen_reference_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_reference_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _reference_person_piqi_type `pb format x_pb ?opts

let gen_reference_person_i ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_reference_person_i x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _reference_person_i_piqi_type `pb format x_pb ?opts

let gen_list_reference_persons ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_reference_persons x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_reference_persons_piqi_type `pb format x_pb ?opts

let gen_relation_parent ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_spouse ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_spouse x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _spouse_piqi_type `pb format x_pb ?opts

let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_full_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_person_piqi_type `pb format x_pb ?opts

let gen_full_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_family_piqi_type `pb format x_pb ?opts

let gen_internal_int32 ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_internal_int32 x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _internal_int32_piqi_type `pb format x_pb ?opts

let gen_list_persons ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_persons x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_persons_piqi_type `pb format x_pb ?opts

let gen_list_full_persons ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_persons x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_full_persons_piqi_type `pb format x_pb ?opts

let gen_list_full_families ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_families x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_full_families_piqi_type `pb format x_pb ?opts

let gen_search_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_search_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _search_params_piqi_type `pb format x_pb ?opts

let gen_image ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_image x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _image_piqi_type `pb format x_pb ?opts

let gen_list_images ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_images x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_images_piqi_type `pb format x_pb ?opts

let gen_pers_img ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_pers_img x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _pers_img_piqi_type `pb format x_pb ?opts

let gen_list_pers_img ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_pers_img x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_pers_img_piqi_type `pb format x_pb ?opts

let gen_index ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_index x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _index_piqi_type `pb format x_pb ?opts

let gen_image_address ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_image_address x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _image_address_piqi_type `pb format x_pb ?opts

let gen_close_persons_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_close_persons_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _close_persons_params_piqi_type `pb format x_pb ?opts

let gen_anniversary_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_anniversary_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _anniversary_params_piqi_type `pb format x_pb ?opts

let gen_graph_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_params_piqi_type `pb format x_pb ?opts

let gen_graph_rel_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph_rel_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_rel_params_piqi_type `pb format x_pb ?opts

let gen_cpl_rel_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_cpl_rel_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _cpl_rel_params_piqi_type `pb format x_pb ?opts

let gen_node ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_node x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _node_piqi_type `pb format x_pb ?opts

let gen_full_node ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_node x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_node_piqi_type `pb format x_pb ?opts

let gen_edge ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_edge x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edge_piqi_type `pb format x_pb ?opts

let gen_graph ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_piqi_type `pb format x_pb ?opts

let gen_full_graph ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_graph x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_graph_piqi_type `pb format x_pb ?opts

let gen_all_persons_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_all_persons_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _all_persons_params_piqi_type `pb format x_pb ?opts

let gen_all_families_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_all_families_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _all_families_params_piqi_type `pb format x_pb ?opts

let gen_warning_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_event_piqi_type `pb format x_pb ?opts

let gen_warning_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_person_piqi_type `pb format x_pb ?opts

let gen_warning_already_defined ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_already_defined x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_already_defined_piqi_type `pb format x_pb ?opts

let gen_warning_own_ancestor ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_own_ancestor x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_own_ancestor_piqi_type `pb format x_pb ?opts

let gen_warning_bad_sex_of_married_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_bad_sex_of_married_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_bad_sex_of_married_person_piqi_type `pb format x_pb ?opts

let gen_warning_birth_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_birth_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_birth_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_incoherent_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_incoherent_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_incoherent_sex_piqi_type `pb format x_pb ?opts

let gen_warning_changed_order_of_children ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_changed_order_of_children x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_changed_order_of_children_piqi_type `pb format x_pb ?opts

let gen_warning_changed_order_of_marriages ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_changed_order_of_marriages x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_changed_order_of_marriages_piqi_type `pb format x_pb ?opts

let gen_warning_children_not_in_order ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_children_not_in_order x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_children_not_in_order_piqi_type `pb format x_pb ?opts

let gen_warning_dead_too_early_to_be_father ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_dead_too_early_to_be_father x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_dead_too_early_to_be_father_piqi_type `pb format x_pb ?opts

let gen_warning_incoherent_ancestor_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_incoherent_ancestor_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_incoherent_ancestor_date_piqi_type `pb format x_pb ?opts

let gen_warning_marriage_date_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_marriage_date_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_marriage_date_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_marriage_date_before_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_marriage_date_before_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_marriage_date_before_birth_piqi_type `pb format x_pb ?opts

let gen_warning_mother_dead_before_child_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_mother_dead_before_child_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_mother_dead_before_child_birth_piqi_type `pb format x_pb ?opts

let gen_warning_parent_born_after_child ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_born_after_child x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_parent_born_after_child_piqi_type `pb format x_pb ?opts

let gen_warning_parent_too_young ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_too_young x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_parent_too_young_piqi_type `pb format x_pb ?opts

let gen_warning_possible_duplicate_fam ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_possible_duplicate_fam x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_possible_duplicate_fam_piqi_type `pb format x_pb ?opts

let gen_warning_possible_duplicate_fam_homonymous ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_possible_duplicate_fam_homonymous x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_possible_duplicate_fam_homonymous_piqi_type `pb format x_pb ?opts

let gen_warning_title_dates_error ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_title_dates_error x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_title_dates_error_piqi_type `pb format x_pb ?opts

let gen_warning_undefined_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_undefined_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_undefined_sex_piqi_type `pb format x_pb ?opts

let gen_warning_young_for_marriage ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_young_for_marriage x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_young_for_marriage_piqi_type `pb format x_pb ?opts

let gen_warning_old_for_marriage ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_old_for_marriage x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_old_for_marriage_piqi_type `pb format x_pb ?opts

let gen_warning_parent_too_old ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_too_old x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_parent_too_old_piqi_type `pb format x_pb ?opts

let gen_warning_close_children ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_close_children x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_close_children_piqi_type `pb format x_pb ?opts

let gen_warning_distant_children ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_distant_children x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_distant_children_piqi_type `pb format x_pb ?opts

let gen_warning_big_age_between_spouses ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_big_age_between_spouses x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_big_age_between_spouses_piqi_type `pb format x_pb ?opts

let gen_warning_dead_old ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_dead_old x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_dead_old_piqi_type `pb format x_pb ?opts

let gen_warning_witness_date_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_witness_date_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_witness_date_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_witness_date_before_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_witness_date_before_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_witness_date_before_birth_piqi_type `pb format x_pb ?opts

let gen_warning_event_order ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_event_order x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_event_order_piqi_type `pb format x_pb ?opts

let gen_base_warnings ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_base_warnings x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _base_warnings_piqi_type `pb format x_pb ?opts

let gen_filter_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filter_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _filter_date_piqi_type `pb format x_pb ?opts

let gen_filter_date_range ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filter_date_range x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _filter_date_range_piqi_type `pb format x_pb ?opts

let gen_filters ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filters x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _filters_piqi_type `pb format x_pb ?opts

let gen_modification_status ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _modification_status_piqi_type `pb format x_pb ?opts

let gen_person_start ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person_start x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_start_piqi_type `pb format x_pb ?opts

let gen_last_modifications ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_last_modifications x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _last_modifications_piqi_type `pb format x_pb ?opts

let gen_last_visits ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_last_visits x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _last_visits_piqi_type `pb format x_pb ?opts

let gen_dmy ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _dmy_piqi_type `pb format x_pb ?opts

let gen_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _date_piqi_type `pb format x_pb ?opts

let gen_events_query_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_events_query_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _events_query_params_piqi_type `pb format x_pb ?opts

let gen_event_query_result ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_event_query_result x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_query_result_piqi_type `pb format x_pb ?opts

let gen_event_query_result_list ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_event_query_result_list x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_query_result_list_piqi_type `pb format x_pb ?opts

let gen_name_frequency_result ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_name_frequency_result x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _name_frequency_result_piqi_type `pb format x_pb ?opts

let gen_name_frequency_result_list ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_name_frequency_result_list x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _name_frequency_result_list_piqi_type `pb format x_pb ?opts

let gen_name_frequency_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_name_frequency_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _name_frequency_params_piqi_type `pb format x_pb ?opts

let gen_name_frequency_params_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_name_frequency_params_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _name_frequency_params_type_piqi_type `pb format x_pb ?opts

let gen_error ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_error x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _error_piqi_type `pb format x_pb ?opts

let gen_error_code ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_error_code x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _error_code_piqi_type `pb format x_pb ?opts

let gen_time ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_time x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _time_piqi_type `pb format x_pb ?opts

let gen_history_request ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_history_request x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _history_request_piqi_type `pb format x_pb ?opts

let gen_history_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_history_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _history_person_piqi_type `pb format x_pb ?opts

let gen_history_note ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_history_note x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _history_note_piqi_type `pb format x_pb ?opts

let gen_history_entry ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_history_entry x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _history_entry_piqi_type `pb format x_pb ?opts

let gen_history ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_history x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _history_piqi_type `pb format x_pb ?opts

let gen_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sex_piqi_type `pb format x_pb ?opts

let gen_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _death_type_piqi_type `pb format x_pb ?opts

let gen_marriage_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _marriage_type_piqi_type `pb format x_pb ?opts

let gen_divorce_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _divorce_type_piqi_type `pb format x_pb ?opts

let gen_relation_parent_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_type_piqi_type `pb format x_pb ?opts

let gen_title_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_type_piqi_type `pb format x_pb ?opts

let gen_visibility ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_visibility x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _visibility_piqi_type `pb format x_pb ?opts

let gen_search_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_search_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _search_type_piqi_type `pb format x_pb ?opts

let gen_pevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_pevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _pevent_name_piqi_type `pb format x_pb ?opts

let gen_fevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_fevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _fevent_name_piqi_type `pb format x_pb ?opts

let gen_witness_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_type_piqi_type `pb format x_pb ?opts

let gen_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _calendar_piqi_type `pb format x_pb ?opts

let gen_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _precision_piqi_type `pb format x_pb ?opts

let gen_modification_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_modification_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _modification_type_piqi_type `pb format x_pb ?opts


let print_infos_base ?opts x =
  Stdlib.print_endline (gen_infos_base x `piq ?opts)
let prerr_infos_base ?opts x =
  Stdlib.prerr_endline (gen_infos_base x `piq ?opts)

let print_reference_person ?opts x =
  Stdlib.print_endline (gen_reference_person x `piq ?opts)
let prerr_reference_person ?opts x =
  Stdlib.prerr_endline (gen_reference_person x `piq ?opts)

let print_reference_person_i ?opts x =
  Stdlib.print_endline (gen_reference_person_i x `piq ?opts)
let prerr_reference_person_i ?opts x =
  Stdlib.prerr_endline (gen_reference_person_i x `piq ?opts)

let print_list_reference_persons ?opts x =
  Stdlib.print_endline (gen_list_reference_persons x `piq ?opts)
let prerr_list_reference_persons ?opts x =
  Stdlib.prerr_endline (gen_list_reference_persons x `piq ?opts)

let print_relation_parent ?opts x =
  Stdlib.print_endline (gen_relation_parent x `piq ?opts)
let prerr_relation_parent ?opts x =
  Stdlib.prerr_endline (gen_relation_parent x `piq ?opts)

let print_title ?opts x =
  Stdlib.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Stdlib.prerr_endline (gen_title x `piq ?opts)

let print_spouse ?opts x =
  Stdlib.print_endline (gen_spouse x `piq ?opts)
let prerr_spouse ?opts x =
  Stdlib.prerr_endline (gen_spouse x `piq ?opts)

let print_person ?opts x =
  Stdlib.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Stdlib.prerr_endline (gen_person x `piq ?opts)

let print_full_person ?opts x =
  Stdlib.print_endline (gen_full_person x `piq ?opts)
let prerr_full_person ?opts x =
  Stdlib.prerr_endline (gen_full_person x `piq ?opts)

let print_full_family ?opts x =
  Stdlib.print_endline (gen_full_family x `piq ?opts)
let prerr_full_family ?opts x =
  Stdlib.prerr_endline (gen_full_family x `piq ?opts)

let print_internal_int32 ?opts x =
  Stdlib.print_endline (gen_internal_int32 x `piq ?opts)
let prerr_internal_int32 ?opts x =
  Stdlib.prerr_endline (gen_internal_int32 x `piq ?opts)

let print_list_persons ?opts x =
  Stdlib.print_endline (gen_list_persons x `piq ?opts)
let prerr_list_persons ?opts x =
  Stdlib.prerr_endline (gen_list_persons x `piq ?opts)

let print_list_full_persons ?opts x =
  Stdlib.print_endline (gen_list_full_persons x `piq ?opts)
let prerr_list_full_persons ?opts x =
  Stdlib.prerr_endline (gen_list_full_persons x `piq ?opts)

let print_list_full_families ?opts x =
  Stdlib.print_endline (gen_list_full_families x `piq ?opts)
let prerr_list_full_families ?opts x =
  Stdlib.prerr_endline (gen_list_full_families x `piq ?opts)

let print_search_params ?opts x =
  Stdlib.print_endline (gen_search_params x `piq ?opts)
let prerr_search_params ?opts x =
  Stdlib.prerr_endline (gen_search_params x `piq ?opts)

let print_image ?opts x =
  Stdlib.print_endline (gen_image x `piq ?opts)
let prerr_image ?opts x =
  Stdlib.prerr_endline (gen_image x `piq ?opts)

let print_list_images ?opts x =
  Stdlib.print_endline (gen_list_images x `piq ?opts)
let prerr_list_images ?opts x =
  Stdlib.prerr_endline (gen_list_images x `piq ?opts)

let print_pers_img ?opts x =
  Stdlib.print_endline (gen_pers_img x `piq ?opts)
let prerr_pers_img ?opts x =
  Stdlib.prerr_endline (gen_pers_img x `piq ?opts)

let print_list_pers_img ?opts x =
  Stdlib.print_endline (gen_list_pers_img x `piq ?opts)
let prerr_list_pers_img ?opts x =
  Stdlib.prerr_endline (gen_list_pers_img x `piq ?opts)

let print_index ?opts x =
  Stdlib.print_endline (gen_index x `piq ?opts)
let prerr_index ?opts x =
  Stdlib.prerr_endline (gen_index x `piq ?opts)

let print_image_address ?opts x =
  Stdlib.print_endline (gen_image_address x `piq ?opts)
let prerr_image_address ?opts x =
  Stdlib.prerr_endline (gen_image_address x `piq ?opts)

let print_close_persons_params ?opts x =
  Stdlib.print_endline (gen_close_persons_params x `piq ?opts)
let prerr_close_persons_params ?opts x =
  Stdlib.prerr_endline (gen_close_persons_params x `piq ?opts)

let print_anniversary_params ?opts x =
  Stdlib.print_endline (gen_anniversary_params x `piq ?opts)
let prerr_anniversary_params ?opts x =
  Stdlib.prerr_endline (gen_anniversary_params x `piq ?opts)

let print_graph_params ?opts x =
  Stdlib.print_endline (gen_graph_params x `piq ?opts)
let prerr_graph_params ?opts x =
  Stdlib.prerr_endline (gen_graph_params x `piq ?opts)

let print_graph_rel_params ?opts x =
  Stdlib.print_endline (gen_graph_rel_params x `piq ?opts)
let prerr_graph_rel_params ?opts x =
  Stdlib.prerr_endline (gen_graph_rel_params x `piq ?opts)

let print_cpl_rel_params ?opts x =
  Stdlib.print_endline (gen_cpl_rel_params x `piq ?opts)
let prerr_cpl_rel_params ?opts x =
  Stdlib.prerr_endline (gen_cpl_rel_params x `piq ?opts)

let print_node ?opts x =
  Stdlib.print_endline (gen_node x `piq ?opts)
let prerr_node ?opts x =
  Stdlib.prerr_endline (gen_node x `piq ?opts)

let print_full_node ?opts x =
  Stdlib.print_endline (gen_full_node x `piq ?opts)
let prerr_full_node ?opts x =
  Stdlib.prerr_endline (gen_full_node x `piq ?opts)

let print_edge ?opts x =
  Stdlib.print_endline (gen_edge x `piq ?opts)
let prerr_edge ?opts x =
  Stdlib.prerr_endline (gen_edge x `piq ?opts)

let print_graph ?opts x =
  Stdlib.print_endline (gen_graph x `piq ?opts)
let prerr_graph ?opts x =
  Stdlib.prerr_endline (gen_graph x `piq ?opts)

let print_full_graph ?opts x =
  Stdlib.print_endline (gen_full_graph x `piq ?opts)
let prerr_full_graph ?opts x =
  Stdlib.prerr_endline (gen_full_graph x `piq ?opts)

let print_all_persons_params ?opts x =
  Stdlib.print_endline (gen_all_persons_params x `piq ?opts)
let prerr_all_persons_params ?opts x =
  Stdlib.prerr_endline (gen_all_persons_params x `piq ?opts)

let print_all_families_params ?opts x =
  Stdlib.print_endline (gen_all_families_params x `piq ?opts)
let prerr_all_families_params ?opts x =
  Stdlib.prerr_endline (gen_all_families_params x `piq ?opts)

let print_warning_event ?opts x =
  Stdlib.print_endline (gen_warning_event x `piq ?opts)
let prerr_warning_event ?opts x =
  Stdlib.prerr_endline (gen_warning_event x `piq ?opts)

let print_warning_person ?opts x =
  Stdlib.print_endline (gen_warning_person x `piq ?opts)
let prerr_warning_person ?opts x =
  Stdlib.prerr_endline (gen_warning_person x `piq ?opts)

let print_warning_already_defined ?opts x =
  Stdlib.print_endline (gen_warning_already_defined x `piq ?opts)
let prerr_warning_already_defined ?opts x =
  Stdlib.prerr_endline (gen_warning_already_defined x `piq ?opts)

let print_warning_own_ancestor ?opts x =
  Stdlib.print_endline (gen_warning_own_ancestor x `piq ?opts)
let prerr_warning_own_ancestor ?opts x =
  Stdlib.prerr_endline (gen_warning_own_ancestor x `piq ?opts)

let print_warning_bad_sex_of_married_person ?opts x =
  Stdlib.print_endline (gen_warning_bad_sex_of_married_person x `piq ?opts)
let prerr_warning_bad_sex_of_married_person ?opts x =
  Stdlib.prerr_endline (gen_warning_bad_sex_of_married_person x `piq ?opts)

let print_warning_birth_after_death ?opts x =
  Stdlib.print_endline (gen_warning_birth_after_death x `piq ?opts)
let prerr_warning_birth_after_death ?opts x =
  Stdlib.prerr_endline (gen_warning_birth_after_death x `piq ?opts)

let print_warning_incoherent_sex ?opts x =
  Stdlib.print_endline (gen_warning_incoherent_sex x `piq ?opts)
let prerr_warning_incoherent_sex ?opts x =
  Stdlib.prerr_endline (gen_warning_incoherent_sex x `piq ?opts)

let print_warning_changed_order_of_children ?opts x =
  Stdlib.print_endline (gen_warning_changed_order_of_children x `piq ?opts)
let prerr_warning_changed_order_of_children ?opts x =
  Stdlib.prerr_endline (gen_warning_changed_order_of_children x `piq ?opts)

let print_warning_changed_order_of_marriages ?opts x =
  Stdlib.print_endline (gen_warning_changed_order_of_marriages x `piq ?opts)
let prerr_warning_changed_order_of_marriages ?opts x =
  Stdlib.prerr_endline (gen_warning_changed_order_of_marriages x `piq ?opts)

let print_warning_children_not_in_order ?opts x =
  Stdlib.print_endline (gen_warning_children_not_in_order x `piq ?opts)
let prerr_warning_children_not_in_order ?opts x =
  Stdlib.prerr_endline (gen_warning_children_not_in_order x `piq ?opts)

let print_warning_dead_too_early_to_be_father ?opts x =
  Stdlib.print_endline (gen_warning_dead_too_early_to_be_father x `piq ?opts)
let prerr_warning_dead_too_early_to_be_father ?opts x =
  Stdlib.prerr_endline (gen_warning_dead_too_early_to_be_father x `piq ?opts)

let print_warning_incoherent_ancestor_date ?opts x =
  Stdlib.print_endline (gen_warning_incoherent_ancestor_date x `piq ?opts)
let prerr_warning_incoherent_ancestor_date ?opts x =
  Stdlib.prerr_endline (gen_warning_incoherent_ancestor_date x `piq ?opts)

let print_warning_marriage_date_after_death ?opts x =
  Stdlib.print_endline (gen_warning_marriage_date_after_death x `piq ?opts)
let prerr_warning_marriage_date_after_death ?opts x =
  Stdlib.prerr_endline (gen_warning_marriage_date_after_death x `piq ?opts)

let print_warning_marriage_date_before_birth ?opts x =
  Stdlib.print_endline (gen_warning_marriage_date_before_birth x `piq ?opts)
let prerr_warning_marriage_date_before_birth ?opts x =
  Stdlib.prerr_endline (gen_warning_marriage_date_before_birth x `piq ?opts)

let print_warning_mother_dead_before_child_birth ?opts x =
  Stdlib.print_endline (gen_warning_mother_dead_before_child_birth x `piq ?opts)
let prerr_warning_mother_dead_before_child_birth ?opts x =
  Stdlib.prerr_endline (gen_warning_mother_dead_before_child_birth x `piq ?opts)

let print_warning_parent_born_after_child ?opts x =
  Stdlib.print_endline (gen_warning_parent_born_after_child x `piq ?opts)
let prerr_warning_parent_born_after_child ?opts x =
  Stdlib.prerr_endline (gen_warning_parent_born_after_child x `piq ?opts)

let print_warning_parent_too_young ?opts x =
  Stdlib.print_endline (gen_warning_parent_too_young x `piq ?opts)
let prerr_warning_parent_too_young ?opts x =
  Stdlib.prerr_endline (gen_warning_parent_too_young x `piq ?opts)

let print_warning_possible_duplicate_fam ?opts x =
  Stdlib.print_endline (gen_warning_possible_duplicate_fam x `piq ?opts)
let prerr_warning_possible_duplicate_fam ?opts x =
  Stdlib.prerr_endline (gen_warning_possible_duplicate_fam x `piq ?opts)

let print_warning_possible_duplicate_fam_homonymous ?opts x =
  Stdlib.print_endline (gen_warning_possible_duplicate_fam_homonymous x `piq ?opts)
let prerr_warning_possible_duplicate_fam_homonymous ?opts x =
  Stdlib.prerr_endline (gen_warning_possible_duplicate_fam_homonymous x `piq ?opts)

let print_warning_title_dates_error ?opts x =
  Stdlib.print_endline (gen_warning_title_dates_error x `piq ?opts)
let prerr_warning_title_dates_error ?opts x =
  Stdlib.prerr_endline (gen_warning_title_dates_error x `piq ?opts)

let print_warning_undefined_sex ?opts x =
  Stdlib.print_endline (gen_warning_undefined_sex x `piq ?opts)
let prerr_warning_undefined_sex ?opts x =
  Stdlib.prerr_endline (gen_warning_undefined_sex x `piq ?opts)

let print_warning_young_for_marriage ?opts x =
  Stdlib.print_endline (gen_warning_young_for_marriage x `piq ?opts)
let prerr_warning_young_for_marriage ?opts x =
  Stdlib.prerr_endline (gen_warning_young_for_marriage x `piq ?opts)

let print_warning_old_for_marriage ?opts x =
  Stdlib.print_endline (gen_warning_old_for_marriage x `piq ?opts)
let prerr_warning_old_for_marriage ?opts x =
  Stdlib.prerr_endline (gen_warning_old_for_marriage x `piq ?opts)

let print_warning_parent_too_old ?opts x =
  Stdlib.print_endline (gen_warning_parent_too_old x `piq ?opts)
let prerr_warning_parent_too_old ?opts x =
  Stdlib.prerr_endline (gen_warning_parent_too_old x `piq ?opts)

let print_warning_close_children ?opts x =
  Stdlib.print_endline (gen_warning_close_children x `piq ?opts)
let prerr_warning_close_children ?opts x =
  Stdlib.prerr_endline (gen_warning_close_children x `piq ?opts)

let print_warning_distant_children ?opts x =
  Stdlib.print_endline (gen_warning_distant_children x `piq ?opts)
let prerr_warning_distant_children ?opts x =
  Stdlib.prerr_endline (gen_warning_distant_children x `piq ?opts)

let print_warning_big_age_between_spouses ?opts x =
  Stdlib.print_endline (gen_warning_big_age_between_spouses x `piq ?opts)
let prerr_warning_big_age_between_spouses ?opts x =
  Stdlib.prerr_endline (gen_warning_big_age_between_spouses x `piq ?opts)

let print_warning_dead_old ?opts x =
  Stdlib.print_endline (gen_warning_dead_old x `piq ?opts)
let prerr_warning_dead_old ?opts x =
  Stdlib.prerr_endline (gen_warning_dead_old x `piq ?opts)

let print_warning_witness_date_after_death ?opts x =
  Stdlib.print_endline (gen_warning_witness_date_after_death x `piq ?opts)
let prerr_warning_witness_date_after_death ?opts x =
  Stdlib.prerr_endline (gen_warning_witness_date_after_death x `piq ?opts)

let print_warning_witness_date_before_birth ?opts x =
  Stdlib.print_endline (gen_warning_witness_date_before_birth x `piq ?opts)
let prerr_warning_witness_date_before_birth ?opts x =
  Stdlib.prerr_endline (gen_warning_witness_date_before_birth x `piq ?opts)

let print_warning_event_order ?opts x =
  Stdlib.print_endline (gen_warning_event_order x `piq ?opts)
let prerr_warning_event_order ?opts x =
  Stdlib.prerr_endline (gen_warning_event_order x `piq ?opts)

let print_base_warnings ?opts x =
  Stdlib.print_endline (gen_base_warnings x `piq ?opts)
let prerr_base_warnings ?opts x =
  Stdlib.prerr_endline (gen_base_warnings x `piq ?opts)

let print_filter_date ?opts x =
  Stdlib.print_endline (gen_filter_date x `piq ?opts)
let prerr_filter_date ?opts x =
  Stdlib.prerr_endline (gen_filter_date x `piq ?opts)

let print_filter_date_range ?opts x =
  Stdlib.print_endline (gen_filter_date_range x `piq ?opts)
let prerr_filter_date_range ?opts x =
  Stdlib.prerr_endline (gen_filter_date_range x `piq ?opts)

let print_filters ?opts x =
  Stdlib.print_endline (gen_filters x `piq ?opts)
let prerr_filters ?opts x =
  Stdlib.prerr_endline (gen_filters x `piq ?opts)

let print_modification_status ?opts x =
  Stdlib.print_endline (gen_modification_status x `piq ?opts)
let prerr_modification_status ?opts x =
  Stdlib.prerr_endline (gen_modification_status x `piq ?opts)

let print_person_start ?opts x =
  Stdlib.print_endline (gen_person_start x `piq ?opts)
let prerr_person_start ?opts x =
  Stdlib.prerr_endline (gen_person_start x `piq ?opts)

let print_last_modifications ?opts x =
  Stdlib.print_endline (gen_last_modifications x `piq ?opts)
let prerr_last_modifications ?opts x =
  Stdlib.prerr_endline (gen_last_modifications x `piq ?opts)

let print_last_visits ?opts x =
  Stdlib.print_endline (gen_last_visits x `piq ?opts)
let prerr_last_visits ?opts x =
  Stdlib.prerr_endline (gen_last_visits x `piq ?opts)

let print_dmy ?opts x =
  Stdlib.print_endline (gen_dmy x `piq ?opts)
let prerr_dmy ?opts x =
  Stdlib.prerr_endline (gen_dmy x `piq ?opts)

let print_date ?opts x =
  Stdlib.print_endline (gen_date x `piq ?opts)
let prerr_date ?opts x =
  Stdlib.prerr_endline (gen_date x `piq ?opts)

let print_events_query_params ?opts x =
  Stdlib.print_endline (gen_events_query_params x `piq ?opts)
let prerr_events_query_params ?opts x =
  Stdlib.prerr_endline (gen_events_query_params x `piq ?opts)

let print_event_query_result ?opts x =
  Stdlib.print_endline (gen_event_query_result x `piq ?opts)
let prerr_event_query_result ?opts x =
  Stdlib.prerr_endline (gen_event_query_result x `piq ?opts)

let print_event_query_result_list ?opts x =
  Stdlib.print_endline (gen_event_query_result_list x `piq ?opts)
let prerr_event_query_result_list ?opts x =
  Stdlib.prerr_endline (gen_event_query_result_list x `piq ?opts)

let print_name_frequency_result ?opts x =
  Stdlib.print_endline (gen_name_frequency_result x `piq ?opts)
let prerr_name_frequency_result ?opts x =
  Stdlib.prerr_endline (gen_name_frequency_result x `piq ?opts)

let print_name_frequency_result_list ?opts x =
  Stdlib.print_endline (gen_name_frequency_result_list x `piq ?opts)
let prerr_name_frequency_result_list ?opts x =
  Stdlib.prerr_endline (gen_name_frequency_result_list x `piq ?opts)

let print_name_frequency_params ?opts x =
  Stdlib.print_endline (gen_name_frequency_params x `piq ?opts)
let prerr_name_frequency_params ?opts x =
  Stdlib.prerr_endline (gen_name_frequency_params x `piq ?opts)

let print_name_frequency_params_type ?opts x =
  Stdlib.print_endline (gen_name_frequency_params_type x `piq ?opts)
let prerr_name_frequency_params_type ?opts x =
  Stdlib.prerr_endline (gen_name_frequency_params_type x `piq ?opts)

let print_error ?opts x =
  Stdlib.print_endline (gen_error x `piq ?opts)
let prerr_error ?opts x =
  Stdlib.prerr_endline (gen_error x `piq ?opts)

let print_error_code ?opts x =
  Stdlib.print_endline (gen_error_code x `piq ?opts)
let prerr_error_code ?opts x =
  Stdlib.prerr_endline (gen_error_code x `piq ?opts)

let print_time ?opts x =
  Stdlib.print_endline (gen_time x `piq ?opts)
let prerr_time ?opts x =
  Stdlib.prerr_endline (gen_time x `piq ?opts)

let print_history_request ?opts x =
  Stdlib.print_endline (gen_history_request x `piq ?opts)
let prerr_history_request ?opts x =
  Stdlib.prerr_endline (gen_history_request x `piq ?opts)

let print_history_person ?opts x =
  Stdlib.print_endline (gen_history_person x `piq ?opts)
let prerr_history_person ?opts x =
  Stdlib.prerr_endline (gen_history_person x `piq ?opts)

let print_history_note ?opts x =
  Stdlib.print_endline (gen_history_note x `piq ?opts)
let prerr_history_note ?opts x =
  Stdlib.prerr_endline (gen_history_note x `piq ?opts)

let print_history_entry ?opts x =
  Stdlib.print_endline (gen_history_entry x `piq ?opts)
let prerr_history_entry ?opts x =
  Stdlib.prerr_endline (gen_history_entry x `piq ?opts)

let print_history ?opts x =
  Stdlib.print_endline (gen_history x `piq ?opts)
let prerr_history ?opts x =
  Stdlib.prerr_endline (gen_history x `piq ?opts)

let print_sex ?opts x =
  Stdlib.print_endline (gen_sex x `piq ?opts)
let prerr_sex ?opts x =
  Stdlib.prerr_endline (gen_sex x `piq ?opts)

let print_death_type ?opts x =
  Stdlib.print_endline (gen_death_type x `piq ?opts)
let prerr_death_type ?opts x =
  Stdlib.prerr_endline (gen_death_type x `piq ?opts)

let print_marriage_type ?opts x =
  Stdlib.print_endline (gen_marriage_type x `piq ?opts)
let prerr_marriage_type ?opts x =
  Stdlib.prerr_endline (gen_marriage_type x `piq ?opts)

let print_divorce_type ?opts x =
  Stdlib.print_endline (gen_divorce_type x `piq ?opts)
let prerr_divorce_type ?opts x =
  Stdlib.prerr_endline (gen_divorce_type x `piq ?opts)

let print_relation_parent_type ?opts x =
  Stdlib.print_endline (gen_relation_parent_type x `piq ?opts)
let prerr_relation_parent_type ?opts x =
  Stdlib.prerr_endline (gen_relation_parent_type x `piq ?opts)

let print_title_type ?opts x =
  Stdlib.print_endline (gen_title_type x `piq ?opts)
let prerr_title_type ?opts x =
  Stdlib.prerr_endline (gen_title_type x `piq ?opts)

let print_visibility ?opts x =
  Stdlib.print_endline (gen_visibility x `piq ?opts)
let prerr_visibility ?opts x =
  Stdlib.prerr_endline (gen_visibility x `piq ?opts)

let print_search_type ?opts x =
  Stdlib.print_endline (gen_search_type x `piq ?opts)
let prerr_search_type ?opts x =
  Stdlib.prerr_endline (gen_search_type x `piq ?opts)

let print_pevent_name ?opts x =
  Stdlib.print_endline (gen_pevent_name x `piq ?opts)
let prerr_pevent_name ?opts x =
  Stdlib.prerr_endline (gen_pevent_name x `piq ?opts)

let print_fevent_name ?opts x =
  Stdlib.print_endline (gen_fevent_name x `piq ?opts)
let prerr_fevent_name ?opts x =
  Stdlib.prerr_endline (gen_fevent_name x `piq ?opts)

let print_witness_type ?opts x =
  Stdlib.print_endline (gen_witness_type x `piq ?opts)
let prerr_witness_type ?opts x =
  Stdlib.prerr_endline (gen_witness_type x `piq ?opts)

let print_calendar ?opts x =
  Stdlib.print_endline (gen_calendar x `piq ?opts)
let prerr_calendar ?opts x =
  Stdlib.prerr_endline (gen_calendar x `piq ?opts)

let print_precision ?opts x =
  Stdlib.print_endline (gen_precision x `piq ?opts)
let prerr_precision ?opts x =
  Stdlib.prerr_endline (gen_precision x `piq ?opts)

let print_modification_type ?opts x =
  Stdlib.print_endline (gen_modification_type x `piq ?opts)
let prerr_modification_type ?opts x =
  Stdlib.prerr_endline (gen_modification_type x `piq ?opts)


