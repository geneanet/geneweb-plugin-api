module rec Api_piqi:
  sig
    type protobuf_int64 = int64
    type protobuf_int32 = int32
    type name_frequency_params_type =
      [
        | `last_name
        | `first_name
      ]
    type error_code =
      [
        | `bad_request
        | `unauthorized
        | `forbidden
        | `not_found
        | `conflict
      ]
    type sex =
      [
        | `male
        | `female
        | `unknown
      ]
    type death_type =
      [
        | `not_dead
        | `dead
        | `dead_young
        | `dead_dont_know_when
        | `dont_know_if_dead
        | `of_course_dead
      ]
    type marriage_type =
      [
        | `married
        | `not_married
        | `engaged
        | `no_sexes_check_not_married
        | `no_mention
        | `no_sexes_check_married
        | `marriage_bann
        | `marriage_contract
        | `marriage_license
        | `pacs
        | `residence
      ]
    type divorce_type =
      [
        | `not_divorced
        | `divorced
        | `separated
      ]
    type relation_parent_type =
      [
        | `rpt_adoption
        | `rpt_recognition
        | `rpt_candidate_parent
        | `rpt_god_parent
        | `rpt_foster_parent
      ]
    type title_type =
      [
        | `title_main
        | `title_name
        | `title_none
      ]
    type visibility =
      [
        | `visibility_public
        | `visibility_semi_public
        | `visibility_private
      ]
    type search_type =
      [
        | `starting_with
        | `approximative
        | `lastname_or_firstname
      ]
    type pevent_name =
      [
        | `epers_birth
        | `epers_baptism
        | `epers_death
        | `epers_burial
        | `epers_cremation
        | `epers_accomplishment
        | `epers_acquisition
        | `epers_adhesion
        | `epers_baptismlds
        | `epers_barmitzvah
        | `epers_batmitzvah
        | `epers_benediction
        | `epers_changename
        | `epers_circumcision
        | `epers_confirmation
        | `epers_confirmationlds
        | `epers_decoration
        | `epers_demobilisationmilitaire
        | `epers_diploma
        | `epers_distinction
        | `epers_dotation
        | `epers_dotationlds
        | `epers_education
        | `epers_election
        | `epers_emigration
        | `epers_excommunication
        | `epers_familylinklds
        | `epers_firstcommunion
        | `epers_funeral
        | `epers_graduate
        | `epers_hospitalisation
        | `epers_illness
        | `epers_immigration
        | `epers_listepassenger
        | `epers_militarydistinction
        | `epers_militarypromotion
        | `epers_militaryservice
        | `epers_mobilisationmilitaire
        | `epers_naturalisation
        | `epers_occupation
        | `epers_ordination
        | `epers_property
        | `epers_recensement
        | `epers_residence
        | `epers_retired
        | `epers_scellentchildlds
        | `epers_scellentparentlds
        | `epers_scellentspouselds
        | `epers_ventebien
        | `epers_will
      ]
    type fevent_name =
      [
        | `efam_marriage
        | `efam_no_marriage
        | `efam_no_mention
        | `efam_engage
        | `efam_divorce
        | `efam_separated
        | `efam_annulation
        | `efam_marriage_bann
        | `efam_marriage_contract
        | `efam_marriage_license
        | `efam_pacs
        | `efam_residence
      ]
    type witness_type =
      [
        | `witness
        | `witness_godparent
        | `witness_civilofficer
        | `witness_religiousofficer
        | `witness_informant
        | `witness_attending
        | `witness_mentioned
        | `witness_other
      ]
    type calendar =
      [
        | `gregorian
        | `julian
        | `french
        | `hebrew
      ]
    type precision =
      [
        | `sure
        | `about
        | `maybe
        | `before
        | `after
        | `oryear
        | `yearint
      ]
    type modification_type =
      [
        | `person_added
        | `person_modified
        | `person_deleted
        | `person_merged
        | `image_received
        | `image_deleted
        | `family_added
        | `family_modified
        | `family_deleted
        | `family_inverted
        | `family_merged
        | `changed_children_names
        | `parents_added
        | `notes_modified
        | `place_modified
        | `source_modified
        | `occupation_modified
      ]
    type infos_base = Infos_base.t
    type reference_person = Reference_person.t
    type reference_person_i = Reference_person_i.t
    type list_reference_persons = List_reference_persons.t
    type relation_parent = Relation_parent.t
    type title = Title.t
    type spouse = Spouse.t
    type person = Person.t
    type full_person = Full_person.t
    type full_family = Full_family.t
    type internal_int32 = Internal_int32.t
    type list_persons = List_persons.t
    type list_full_persons = List_full_persons.t
    type list_full_families = List_full_families.t
    type search_params = Search_params.t
    type image = Image.t
    type list_images = List_images.t
    type pers_img = Pers_img.t
    type list_pers_img = List_pers_img.t
    type index = Index.t
    type image_address = Image_address.t
    type close_persons_params = Close_persons_params.t
    type anniversary_params = Anniversary_params.t
    type graph_params = Graph_params.t
    type graph_rel_params = Graph_rel_params.t
    type cpl_rel_params = Cpl_rel_params.t
    type node = Node.t
    type full_node = Full_node.t
    type edge = Edge.t
    type graph = Graph.t
    type full_graph = Full_graph.t
    type all_persons_params = All_persons_params.t
    type all_families_params = All_families_params.t
    type warning_event = Warning_event.t
    type warning_person = Warning_person.t
    type warning_already_defined = Warning_already_defined.t
    type warning_own_ancestor = Warning_own_ancestor.t
    type warning_bad_sex_of_married_person = Warning_bad_sex_of_married_person.t
    type warning_birth_after_death = Warning_birth_after_death.t
    type warning_incoherent_sex = Warning_incoherent_sex.t
    type warning_changed_order_of_children = Warning_changed_order_of_children.t
    type warning_changed_order_of_marriages = Warning_changed_order_of_marriages.t
    type warning_children_not_in_order = Warning_children_not_in_order.t
    type warning_dead_too_early_to_be_father = Warning_dead_too_early_to_be_father.t
    type warning_incoherent_ancestor_date = Warning_incoherent_ancestor_date.t
    type warning_marriage_date_after_death = Warning_marriage_date_after_death.t
    type warning_marriage_date_before_birth = Warning_marriage_date_before_birth.t
    type warning_mother_dead_before_child_birth = Warning_mother_dead_before_child_birth.t
    type warning_parent_born_after_child = Warning_parent_born_after_child.t
    type warning_parent_too_young = Warning_parent_too_young.t
    type warning_possible_duplicate_fam = Warning_possible_duplicate_fam.t
    type warning_possible_duplicate_fam_homonymous = Warning_possible_duplicate_fam_homonymous.t
    type warning_title_dates_error = Warning_title_dates_error.t
    type warning_undefined_sex = Warning_undefined_sex.t
    type warning_young_for_marriage = Warning_young_for_marriage.t
    type warning_old_for_marriage = Warning_old_for_marriage.t
    type warning_parent_too_old = Warning_parent_too_old.t
    type warning_close_children = Warning_close_children.t
    type warning_distant_children = Warning_distant_children.t
    type warning_big_age_between_spouses = Warning_big_age_between_spouses.t
    type warning_dead_old = Warning_dead_old.t
    type warning_witness_date_after_death = Warning_witness_date_after_death.t
    type warning_witness_date_before_birth = Warning_witness_date_before_birth.t
    type warning_event_order = Warning_event_order.t
    type base_warnings = Base_warnings.t
    type filter_date = Filter_date.t
    type filter_date_range = Filter_date_range.t
    type filters = Filters.t
    type modification_status = Modification_status.t
    type person_start = Person_start.t
    type last_modifications = Last_modifications.t
    type last_visits = Last_visits.t
    type dmy = Dmy.t
    type date = Date.t
    type events_query_params = Events_query_params.t
    type event_query_result = Event_query_result.t
    type event_query_result_list = Event_query_result_list.t
    type name_frequency_result = Name_frequency_result.t
    type name_frequency_result_list = Name_frequency_result_list.t
    type name_frequency_params = Name_frequency_params.t
    type error = Error.t
    type time = Time.t
    type history_request = History_request.t
    type history_person = History_person.t
    type history_note = History_note.t
    type history_entry = History_entry.t
    type history = History.t
  end = Api_piqi
and Infos_base:
  sig
    type t = {
      mutable nb_persons: Api_piqi.protobuf_int64;
      mutable nb_families: Api_piqi.protobuf_int64;
      mutable sosa: Api_piqi.reference_person option;
      mutable last_modified_person: Api_piqi.protobuf_int64 option;
      mutable real_nb_persons: Api_piqi.protobuf_int64 option;
      mutable has_ignored_duplicates: bool option;
    }
  end = Infos_base
and Reference_person:
  sig
    type t = {
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
    }
  end = Reference_person
and Reference_person_i:
  sig
    type t = {
      mutable key: Api_piqi.reference_person option;
      mutable i: string option;
    }
  end = Reference_person_i
and List_reference_persons:
  sig
    type t = {
      mutable list_ref_persons: Api_piqi.reference_person list;
    }
  end = List_reference_persons
and Relation_parent:
  sig
    type t = {
      mutable father: Api_piqi.protobuf_int32 option;
      mutable mother: Api_piqi.protobuf_int32 option;
      mutable source: string option;
      mutable rpt_type: Api_piqi.relation_parent_type;
    }
  end = Relation_parent
and Title:
  sig
    type t = {
      mutable title_type: Api_piqi.title_type;
      mutable name: string option;
      mutable title: string option;
      mutable fief: string option;
      mutable date_begin: string option;
      mutable date_end: string option;
      mutable nth: Api_piqi.protobuf_int32 option;
    }
  end = Title
and Spouse:
  sig
    type t = {
      mutable sosa: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable sex: Api_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable image: string;
      mutable birth_date: string;
      mutable birth_place: string;
      mutable baptism_date: string;
      mutable baptism_place: string;
      mutable death_date: string;
      mutable death_place: string;
      mutable death_type: Api_piqi.death_type;
      mutable burial_date: string;
      mutable burial_place: string;
      mutable marriage_date: string;
      mutable marriage_place: string;
      mutable divorce_type: Api_piqi.divorce_type;
      mutable visible_for_visitors: Api_piqi.visibility;
      mutable index: Api_piqi.protobuf_int32;
    }
  end = Spouse
and Person:
  sig
    type t = {
      mutable sosa: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable sex: Api_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable image: string;
      mutable birth_date: string;
      mutable birth_place: string;
      mutable baptism_date: string;
      mutable baptism_place: string;
      mutable death_date: string;
      mutable death_place: string;
      mutable death_type: Api_piqi.death_type;
      mutable burial_date: string;
      mutable burial_place: string;
      mutable spouses: Api_piqi.spouse list;
      mutable ascend: bool;
      mutable descend: bool;
      mutable visible_for_visitors: Api_piqi.visibility;
      mutable baseprefix: string;
      mutable index: Api_piqi.protobuf_int32;
      mutable is_contemporary: bool;
      mutable name_is_hidden: bool;
      mutable name_is_restricted: bool;
    }
  end = Person
and Full_person:
  sig
    type t = {
      mutable sosa: string;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable index: Api_piqi.protobuf_int32;
      mutable sex: Api_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable image: string option;
      mutable birth_date: string option;
      mutable birth_place: string option;
      mutable birth_src: string option;
      mutable baptism_date: string option;
      mutable baptism_place: string option;
      mutable baptism_src: string option;
      mutable death_date: string option;
      mutable death_place: string option;
      mutable death_src: string option;
      mutable death_type: Api_piqi.death_type;
      mutable burial_date: string option;
      mutable burial_place: string option;
      mutable burial_src: string option;
      mutable occupation: string option;
      mutable psources: string option;
      mutable titles: Api_piqi.title list;
      mutable related: Api_piqi.protobuf_int32 list;
      mutable rparents: Api_piqi.relation_parent list;
      mutable visible_for_visitors: Api_piqi.visibility;
      mutable parents: Api_piqi.protobuf_int32 option;
      mutable families: Api_piqi.protobuf_int32 list;
      mutable baseprefix: string;
      mutable is_contemporary: bool;
      mutable name_is_hidden: bool;
      mutable name_is_restricted: bool;
    }
  end = Full_person
and Full_family:
  sig
    type t = {
      mutable fsources: string option;
      mutable marriage_date: string option;
      mutable marriage_place: string option;
      mutable marriage_src: string option;
      mutable marriage_type: Api_piqi.marriage_type;
      mutable divorce_type: Api_piqi.divorce_type;
      mutable divorce_date: string option;
      mutable witnesses: Api_piqi.protobuf_int32 list;
      mutable father: Api_piqi.protobuf_int32;
      mutable mother: Api_piqi.protobuf_int32;
      mutable children: Api_piqi.protobuf_int32 list;
      mutable index: Api_piqi.protobuf_int32;
    }
  end = Full_family
and Internal_int32:
  sig
    type t = {
      mutable value: Api_piqi.protobuf_int32;
    }
  end = Internal_int32
and List_persons:
  sig
    type t = {
      mutable list_persons: Api_piqi.person list;
    }
  end = List_persons
and List_full_persons:
  sig
    type t = {
      mutable persons: Api_piqi.full_person list;
    }
  end = List_full_persons
and List_full_families:
  sig
    type t = {
      mutable families: Api_piqi.full_family list;
    }
  end = List_full_families
and Search_params:
  sig
    type t = {
      mutable search_type: Api_piqi.search_type;
      mutable lastname: string option;
      mutable firstname: string option;
      mutable only_sosa: bool;
      mutable only_recent: bool;
      mutable maiden_name: bool;
    }
  end = Search_params
and Image:
  sig
    type t = {
      mutable person: Api_piqi.reference_person;
      mutable img: string;
    }
  end = Image
and List_images:
  sig
    type t = {
      mutable list_images: Api_piqi.image list;
    }
  end = List_images
and Pers_img:
  sig
    type t = {
      mutable person: Api_piqi.reference_person;
      mutable img: string;
    }
  end = Pers_img
and List_pers_img:
  sig
    type t = {
      mutable list_pers_img: Api_piqi.pers_img list;
    }
  end = List_pers_img
and Index:
  sig
    type t = {
      mutable index: Api_piqi.protobuf_int32;
    }
  end = Index
and Image_address:
  sig
    type t = {
      mutable img: string;
    }
  end = Image_address
and Close_persons_params:
  sig
    type t = {
      mutable person: Api_piqi.reference_person;
      mutable nb_gen_asc: Api_piqi.protobuf_int32 option;
      mutable nb_gen_desc: Api_piqi.protobuf_int32 option;
      mutable spouse_ascend: bool;
      mutable only_recent: bool;
    }
  end = Close_persons_params
and Anniversary_params:
  sig
    type t = {
      mutable month: Api_piqi.protobuf_int32 option;
    }
  end = Anniversary_params
and Graph_params:
  sig
    type t = {
      mutable generation: Api_piqi.protobuf_int32 option;
      mutable person: Api_piqi.reference_person;
    }
  end = Graph_params
and Graph_rel_params:
  sig
    type t = {
      mutable person1: Api_piqi.reference_person;
      mutable person2: Api_piqi.reference_person;
    }
  end = Graph_rel_params
and Cpl_rel_params:
  sig
    type t = {
      mutable person1: Api_piqi.reference_person;
      mutable person2: Api_piqi.reference_person;
    }
  end = Cpl_rel_params
and Node:
  sig
    type t = {
      mutable id: Api_piqi.protobuf_int64;
      mutable person: Api_piqi.person;
    }
  end = Node
and Full_node:
  sig
    type t = {
      mutable id: Api_piqi.protobuf_int64;
      mutable person: Api_piqi.full_person;
    }
  end = Full_node
and Edge:
  sig
    type t = {
      mutable from_node: Api_piqi.protobuf_int64;
      mutable to_node: Api_piqi.protobuf_int64;
    }
  end = Edge
and Graph:
  sig
    type t = {
      mutable nodes: Api_piqi.node list;
      mutable edges: Api_piqi.edge list;
    }
  end = Graph
and Full_graph:
  sig
    type t = {
      mutable nodes: Api_piqi.full_node list;
      mutable edges: Api_piqi.edge list;
      mutable families: Api_piqi.full_family list;
    }
  end = Full_graph
and All_persons_params:
  sig
    type t = {
      mutable from: Api_piqi.protobuf_int32 option;
      mutable limit: Api_piqi.protobuf_int32 option;
    }
  end = All_persons_params
and All_families_params:
  sig
    type t = {
      mutable from: Api_piqi.protobuf_int32 option;
      mutable limit: Api_piqi.protobuf_int32 option;
    }
  end = All_families_params
and Warning_event:
  sig
    type t = {
      mutable pevent: Api_piqi.pevent_name option;
      mutable fevent: Api_piqi.fevent_name option;
    }
  end = Warning_event
and Warning_person:
  sig
    type t = {
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable lastname: string;
      mutable firstname: string;
      mutable birth_date: string option;
      mutable death_date: string option;
      mutable iper: string;
    }
  end = Warning_person
and Warning_already_defined:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_already_defined
and Warning_own_ancestor:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_own_ancestor
and Warning_bad_sex_of_married_person:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_bad_sex_of_married_person
and Warning_birth_after_death:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_birth_after_death
and Warning_incoherent_sex:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_incoherent_sex
and Warning_changed_order_of_children:
  sig
    type t = {
      mutable father: Api_piqi.warning_person;
      mutable mother: Api_piqi.warning_person;
    }
  end = Warning_changed_order_of_children
and Warning_changed_order_of_marriages:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_changed_order_of_marriages
and Warning_children_not_in_order:
  sig
    type t = {
      mutable father: Api_piqi.warning_person;
      mutable mother: Api_piqi.warning_person;
    }
  end = Warning_children_not_in_order
and Warning_dead_too_early_to_be_father:
  sig
    type t = {
      mutable son: Api_piqi.warning_person;
      mutable father: Api_piqi.warning_person;
    }
  end = Warning_dead_too_early_to_be_father
and Warning_incoherent_ancestor_date:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable ancestor: Api_piqi.warning_person;
    }
  end = Warning_incoherent_ancestor_date
and Warning_marriage_date_after_death:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_marriage_date_after_death
and Warning_marriage_date_before_birth:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_marriage_date_before_birth
and Warning_mother_dead_before_child_birth:
  sig
    type t = {
      mutable mother: Api_piqi.warning_person;
      mutable child: Api_piqi.warning_person;
    }
  end = Warning_mother_dead_before_child_birth
and Warning_parent_born_after_child:
  sig
    type t = {
      mutable parent: Api_piqi.warning_person;
      mutable child: Api_piqi.warning_person;
    }
  end = Warning_parent_born_after_child
and Warning_parent_too_young:
  sig
    type t = {
      mutable parent: Api_piqi.warning_person;
      mutable date: string;
      mutable child: Api_piqi.warning_person;
    }
  end = Warning_parent_too_young
and Warning_possible_duplicate_fam:
  sig
    type t = {
      mutable father1: Api_piqi.warning_person;
      mutable mother1: Api_piqi.warning_person;
      mutable father2: Api_piqi.warning_person;
      mutable mother2: Api_piqi.warning_person;
    }
  end = Warning_possible_duplicate_fam
and Warning_possible_duplicate_fam_homonymous:
  sig
    type t = {
      mutable father1: Api_piqi.warning_person;
      mutable mother1: Api_piqi.warning_person;
      mutable father2: Api_piqi.warning_person;
      mutable mother2: Api_piqi.warning_person;
      mutable homonymous: Api_piqi.warning_person;
    }
  end = Warning_possible_duplicate_fam_homonymous
and Warning_title_dates_error:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable title: Api_piqi.title;
    }
  end = Warning_title_dates_error
and Warning_undefined_sex:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
    }
  end = Warning_undefined_sex
and Warning_young_for_marriage:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable date: string;
    }
  end = Warning_young_for_marriage
and Warning_old_for_marriage:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable date: string;
    }
  end = Warning_old_for_marriage
and Warning_parent_too_old:
  sig
    type t = {
      mutable parent: Api_piqi.warning_person;
      mutable date: string;
      mutable child: Api_piqi.warning_person;
    }
  end = Warning_parent_too_old
and Warning_close_children:
  sig
    type t = {
      mutable father: Api_piqi.warning_person;
      mutable mother: Api_piqi.warning_person;
      mutable child1: Api_piqi.warning_person;
      mutable child2: Api_piqi.warning_person;
    }
  end = Warning_close_children
and Warning_distant_children:
  sig
    type t = {
      mutable father: Api_piqi.warning_person;
      mutable mother: Api_piqi.warning_person;
      mutable child1: Api_piqi.warning_person;
      mutable child2: Api_piqi.warning_person;
    }
  end = Warning_distant_children
and Warning_big_age_between_spouses:
  sig
    type t = {
      mutable father: Api_piqi.warning_person;
      mutable mother: Api_piqi.warning_person;
      mutable date: string;
    }
  end = Warning_big_age_between_spouses
and Warning_dead_old:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable date: string;
    }
  end = Warning_dead_old
and Warning_witness_date_after_death:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable event: Api_piqi.warning_event;
      mutable origin: Api_piqi.warning_person list;
    }
  end = Warning_witness_date_after_death
and Warning_witness_date_before_birth:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable event: Api_piqi.warning_event;
      mutable origin: Api_piqi.warning_person list;
    }
  end = Warning_witness_date_before_birth
and Warning_event_order:
  sig
    type t = {
      mutable person: Api_piqi.warning_person;
      mutable pevents: Api_piqi.pevent_name list;
      mutable fevents: Api_piqi.fevent_name list;
    }
  end = Warning_event_order
and Base_warnings:
  sig
    type t = {
      mutable warning_already_defined: Api_piqi.warning_already_defined list;
      mutable warning_own_ancestor: Api_piqi.warning_own_ancestor list;
      mutable warning_bad_sex_of_married_person: Api_piqi.warning_bad_sex_of_married_person list;
      mutable warning_birth_after_death: Api_piqi.warning_birth_after_death list;
      mutable warning_incoherent_sex: Api_piqi.warning_incoherent_sex list;
      mutable warning_changed_order_of_children: Api_piqi.warning_changed_order_of_children list;
      mutable warning_children_not_in_order: Api_piqi.warning_children_not_in_order list;
      mutable warning_dead_too_early_to_be_father: Api_piqi.warning_dead_too_early_to_be_father list;
      mutable warning_incoherent_ancestor_date: Api_piqi.warning_incoherent_ancestor_date list;
      mutable warning_marriage_date_after_death: Api_piqi.warning_marriage_date_after_death list;
      mutable warning_marriage_date_before_birth: Api_piqi.warning_marriage_date_before_birth list;
      mutable warning_mother_dead_before_child_birth: Api_piqi.warning_mother_dead_before_child_birth list;
      mutable warning_parent_born_after_child: Api_piqi.warning_parent_born_after_child list;
      mutable warning_parent_too_young: Api_piqi.warning_parent_too_young list;
      mutable warning_possible_duplicate_fam: Api_piqi.warning_possible_duplicate_fam list;
      mutable warning_title_dates_error: Api_piqi.warning_title_dates_error list;
      mutable warning_undefined_sex: Api_piqi.warning_undefined_sex list;
      mutable warning_young_for_marriage: Api_piqi.warning_young_for_marriage list;
      mutable warning_close_children: Api_piqi.warning_close_children list;
      mutable warning_parent_too_old: Api_piqi.warning_parent_too_old list;
      mutable warning_changed_order_of_marriages: Api_piqi.warning_changed_order_of_marriages list;
      mutable warning_big_age_between_spouses: Api_piqi.warning_big_age_between_spouses list;
      mutable warning_dead_old: Api_piqi.warning_dead_old list;
      mutable warning_witness_date_after_death: Api_piqi.warning_witness_date_after_death list;
      mutable warning_witness_date_before_birth: Api_piqi.warning_witness_date_before_birth list;
      mutable warning_old_for_marriage: Api_piqi.warning_old_for_marriage list;
      mutable warning_distant_children: Api_piqi.warning_distant_children list;
      mutable warning_event_order: Api_piqi.warning_event_order list;
      mutable warning_possible_duplicate_fam_homonymous: Api_piqi.warning_possible_duplicate_fam_homonymous list;
    }
  end = Base_warnings
and Filter_date:
  sig
    type t = {
      mutable day: Api_piqi.protobuf_int32;
      mutable month: Api_piqi.protobuf_int32;
      mutable year: Api_piqi.protobuf_int32;
    }
  end = Filter_date
and Filter_date_range:
  sig
    type t = {
      mutable date_begin: Api_piqi.filter_date;
      mutable date_end: Api_piqi.filter_date;
      mutable only_exact: bool;
    }
  end = Filter_date_range
and Filters:
  sig
    type t = {
      mutable only_sosa: bool;
      mutable only_recent: bool;
      mutable sex: Api_piqi.sex option;
      mutable nb_results: bool;
      mutable date_birth: Api_piqi.filter_date_range option;
      mutable date_death: Api_piqi.filter_date_range option;
    }
  end = Filters
and Modification_status:
  sig
    type t = {
      mutable status: bool;
      mutable base_warnings: Api_piqi.base_warnings;
      mutable index: Api_piqi.protobuf_int32 option;
    }
  end = Modification_status
and Person_start:
  sig
    type t = {
      mutable lastname: string;
      mutable firstname: string;
      mutable sex: Api_piqi.sex;
      mutable birth_date_day: Api_piqi.protobuf_int32 option;
      mutable birth_date_month: Api_piqi.protobuf_int32 option;
      mutable birth_date_year: Api_piqi.protobuf_int32 option;
    }
  end = Person_start
and Last_modifications:
  sig
    type t = {
      mutable wizard: string option;
      mutable max_res: Api_piqi.protobuf_int32 option;
      mutable range: Api_piqi.filter_date_range option;
    }
  end = Last_modifications
and Last_visits:
  sig
    type t = {
      mutable user: string;
    }
  end = Last_visits
and Dmy:
  sig
    type t = {
      mutable day: Api_piqi.protobuf_int32;
      mutable month: Api_piqi.protobuf_int32;
      mutable year: int32;
      mutable delta: Api_piqi.protobuf_int32;
    }
  end = Dmy
and Date:
  sig
    type t = {
      mutable cal: Api_piqi.calendar option;
      mutable prec: Api_piqi.precision option;
      mutable dmy: Api_piqi.dmy option;
      mutable dmy2: Api_piqi.dmy option;
      mutable text: string option;
    }
  end = Date
and Events_query_params:
  sig
    type t = {
      mutable close_persons_params: Api_piqi.close_persons_params option;
      mutable start_date: Api_piqi.date option;
      mutable stop_date: Api_piqi.date option;
      mutable pevents: Api_piqi.pevent_name list;
      mutable fevents: Api_piqi.fevent_name list;
    }
  end = Events_query_params
and Event_query_result:
  sig
    type t = {
      mutable p: Api_piqi.person;
      mutable sp: Api_piqi.person option;
      mutable pevent_name: Api_piqi.pevent_name option;
      mutable fevent_name: Api_piqi.fevent_name option;
      mutable date: Api_piqi.date;
      mutable place: string;
      mutable note: string;
      mutable src: string;
    }
  end = Event_query_result
and Event_query_result_list:
  sig
    type t = {
      mutable events: Api_piqi.event_query_result list;
    }
  end = Event_query_result_list
and Name_frequency_result:
  sig
    type t = {
      mutable key: string;
      mutable name: string;
      mutable count: Api_piqi.protobuf_int32;
    }
  end = Name_frequency_result
and Name_frequency_result_list:
  sig
    type t = {
      mutable result: Api_piqi.name_frequency_result list;
      mutable total: Api_piqi.protobuf_int32;
    }
  end = Name_frequency_result_list
and Name_frequency_params:
  sig
    type t = {
      mutable type_: Api_piqi.name_frequency_params_type;
      mutable from: Api_piqi.protobuf_int32 option;
      mutable to_: Api_piqi.protobuf_int32 option;
    }
  end = Name_frequency_params
and Error:
  sig
    type t = {
      mutable code: Api_piqi.error_code;
      mutable message: string option;
    }
  end = Error
and Time:
  sig
    type t = {
      mutable year: Api_piqi.protobuf_int32;
      mutable month: Api_piqi.protobuf_int32;
      mutable day: Api_piqi.protobuf_int32;
      mutable hour: Api_piqi.protobuf_int32;
      mutable minute: Api_piqi.protobuf_int32;
      mutable second: Api_piqi.protobuf_int32;
    }
  end = Time
and History_request:
  sig
    type t = {
      mutable page: Api_piqi.protobuf_int32;
      mutable elements_per_page: Api_piqi.protobuf_int32;
      mutable filter_user: string option;
    }
  end = History_request
and History_person:
  sig
    type t = {
      mutable n: string;
      mutable p: string;
      mutable oc: Api_piqi.protobuf_int32;
      mutable firstname: string;
      mutable lastname: string;
      mutable year1: Api_piqi.protobuf_int32 option;
      mutable year2: Api_piqi.protobuf_int32 option;
      mutable exists_in_base: bool;
      mutable has_history: bool;
    }
  end = History_person
and History_note:
  sig
    type t = {
      mutable link_parameters: string;
      mutable link_txt: string;
    }
  end = History_note
and History_entry:
  sig
    type t = {
      mutable modification_type: Api_piqi.modification_type;
      mutable time: Api_piqi.time;
      mutable editor: string;
      mutable person: Api_piqi.history_person option;
      mutable note: Api_piqi.history_note option;
    }
  end = History_entry
and History:
  sig
    type t = {
      mutable entries: Api_piqi.history_entry list;
      mutable page: Api_piqi.protobuf_int32;
      mutable total_elements: Api_piqi.protobuf_int32;
    }
  end = History


let rec parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x

and parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_protobuf_int64 x = Piqirun.int64_of_signed_varint x
and packed_parse_protobuf_int64 x = Piqirun.int64_of_packed_signed_varint x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_infos_base x =
  let x = Piqirun.parse_record x in
  let _nb_persons, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _nb_families, x = Piqirun.parse_required_field 2 parse_protobuf_int64 x in
  let _sosa, x = Piqirun.parse_optional_field 3 parse_reference_person x in
  let _last_modified_person, x = Piqirun.parse_optional_field 4 parse_protobuf_int64 x in
  let _real_nb_persons, x = Piqirun.parse_optional_field 5 parse_protobuf_int64 x in
  let _has_ignored_duplicates, x = Piqirun.parse_optional_field 6 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    Infos_base.nb_persons = _nb_persons;
    Infos_base.nb_families = _nb_families;
    Infos_base.sosa = _sosa;
    Infos_base.last_modified_person = _last_modified_person;
    Infos_base.real_nb_persons = _real_nb_persons;
    Infos_base.has_ignored_duplicates = _has_ignored_duplicates;
  }

and parse_reference_person x =
  let x = Piqirun.parse_record x in
  let _n, x = Piqirun.parse_required_field 1 parse_string x in
  let _p, x = Piqirun.parse_required_field 2 parse_string x in
  let _oc, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Reference_person.n = _n;
    Reference_person.p = _p;
    Reference_person.oc = _oc;
  }

and parse_reference_person_i x =
  let x = Piqirun.parse_record x in
  let _key, x = Piqirun.parse_optional_field 1 parse_reference_person x in
  let _i, x = Piqirun.parse_optional_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Reference_person_i.key = _key;
    Reference_person_i.i = _i;
  }

and parse_list_reference_persons x =
  let x = Piqirun.parse_record x in
  let _list_ref_persons, x = Piqirun.parse_repeated_field 1 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    List_reference_persons.list_ref_persons = _list_ref_persons;
  }

and parse_relation_parent x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _mother, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _source, x = Piqirun.parse_optional_field 3 parse_string x in
  let _rpt_type, x = Piqirun.parse_required_field 4 parse_relation_parent_type x in
  Piqirun.check_unparsed_fields x;
  {
    Relation_parent.father = _father;
    Relation_parent.mother = _mother;
    Relation_parent.source = _source;
    Relation_parent.rpt_type = _rpt_type;
  }

and parse_title x =
  let x = Piqirun.parse_record x in
  let _title_type, x = Piqirun.parse_required_field 1 parse_title_type x in
  let _name, x = Piqirun.parse_optional_field 2 parse_string x in
  let _title, x = Piqirun.parse_optional_field 3 parse_string x in
  let _fief, x = Piqirun.parse_optional_field 4 parse_string x in
  let _date_begin, x = Piqirun.parse_optional_field 5 parse_string x in
  let _date_end, x = Piqirun.parse_optional_field 6 parse_string x in
  let _nth, x = Piqirun.parse_optional_field 7 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Title.title_type = _title_type;
    Title.name = _name;
    Title.title = _title;
    Title.fief = _fief;
    Title.date_begin = _date_begin;
    Title.date_end = _date_end;
    Title.nth = _nth;
  }

and parse_spouse x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_string x in
  let _n, x = Piqirun.parse_required_field 2 parse_string x in
  let _p, x = Piqirun.parse_required_field 3 parse_string x in
  let _oc, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 5 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 6 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 7 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 8 parse_string x in
  let _image, x = Piqirun.parse_required_field 9 parse_string x in
  let _birth_date, x = Piqirun.parse_required_field 10 parse_string x in
  let _birth_place, x = Piqirun.parse_required_field 11 parse_string x in
  let _baptism_date, x = Piqirun.parse_required_field 12 parse_string x in
  let _baptism_place, x = Piqirun.parse_required_field 13 parse_string x in
  let _death_date, x = Piqirun.parse_required_field 14 parse_string x in
  let _death_place, x = Piqirun.parse_required_field 15 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 16 parse_death_type x in
  let _burial_date, x = Piqirun.parse_required_field 17 parse_string x in
  let _burial_place, x = Piqirun.parse_required_field 18 parse_string x in
  let _marriage_date, x = Piqirun.parse_required_field 19 parse_string x in
  let _marriage_place, x = Piqirun.parse_required_field 20 parse_string x in
  let _divorce_type, x = Piqirun.parse_required_field 21 parse_divorce_type x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 22 parse_visibility x in
  let _index, x = Piqirun.parse_required_field 23 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Spouse.sosa = _sosa;
    Spouse.n = _n;
    Spouse.p = _p;
    Spouse.oc = _oc;
    Spouse.sex = _sex;
    Spouse.lastname = _lastname;
    Spouse.firstname = _firstname;
    Spouse.public_name = _public_name;
    Spouse.image = _image;
    Spouse.birth_date = _birth_date;
    Spouse.birth_place = _birth_place;
    Spouse.baptism_date = _baptism_date;
    Spouse.baptism_place = _baptism_place;
    Spouse.death_date = _death_date;
    Spouse.death_place = _death_place;
    Spouse.death_type = _death_type;
    Spouse.burial_date = _burial_date;
    Spouse.burial_place = _burial_place;
    Spouse.marriage_date = _marriage_date;
    Spouse.marriage_place = _marriage_place;
    Spouse.divorce_type = _divorce_type;
    Spouse.visible_for_visitors = _visible_for_visitors;
    Spouse.index = _index;
  }

and parse_person x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_string x in
  let _n, x = Piqirun.parse_required_field 2 parse_string x in
  let _p, x = Piqirun.parse_required_field 3 parse_string x in
  let _oc, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 5 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 6 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 7 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 8 parse_string x in
  let _image, x = Piqirun.parse_required_field 9 parse_string x in
  let _birth_date, x = Piqirun.parse_required_field 10 parse_string x in
  let _birth_place, x = Piqirun.parse_required_field 11 parse_string x in
  let _baptism_date, x = Piqirun.parse_required_field 12 parse_string x in
  let _baptism_place, x = Piqirun.parse_required_field 13 parse_string x in
  let _death_date, x = Piqirun.parse_required_field 14 parse_string x in
  let _death_place, x = Piqirun.parse_required_field 15 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 16 parse_death_type x in
  let _burial_date, x = Piqirun.parse_required_field 17 parse_string x in
  let _burial_place, x = Piqirun.parse_required_field 18 parse_string x in
  let _spouses, x = Piqirun.parse_repeated_field 19 parse_spouse x in
  let _ascend, x = Piqirun.parse_required_field 20 parse_bool x in
  let _descend, x = Piqirun.parse_required_field 21 parse_bool x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 22 parse_visibility x in
  let _baseprefix, x = Piqirun.parse_required_field 23 parse_string x in
  let _index, x = Piqirun.parse_required_field 24 parse_protobuf_int32 x in
  let _is_contemporary, x = Piqirun.parse_required_field 25 parse_bool x in
  let _name_is_hidden, x = Piqirun.parse_required_field 26 parse_bool x in
  let _name_is_restricted, x = Piqirun.parse_required_field 27 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    Person.sosa = _sosa;
    Person.n = _n;
    Person.p = _p;
    Person.oc = _oc;
    Person.sex = _sex;
    Person.lastname = _lastname;
    Person.firstname = _firstname;
    Person.public_name = _public_name;
    Person.image = _image;
    Person.birth_date = _birth_date;
    Person.birth_place = _birth_place;
    Person.baptism_date = _baptism_date;
    Person.baptism_place = _baptism_place;
    Person.death_date = _death_date;
    Person.death_place = _death_place;
    Person.death_type = _death_type;
    Person.burial_date = _burial_date;
    Person.burial_place = _burial_place;
    Person.spouses = _spouses;
    Person.ascend = _ascend;
    Person.descend = _descend;
    Person.visible_for_visitors = _visible_for_visitors;
    Person.baseprefix = _baseprefix;
    Person.index = _index;
    Person.is_contemporary = _is_contemporary;
    Person.name_is_hidden = _name_is_hidden;
    Person.name_is_restricted = _name_is_restricted;
  }

and parse_full_person x =
  let x = Piqirun.parse_record x in
  let _sosa, x = Piqirun.parse_required_field 1 parse_string x in
  let _n, x = Piqirun.parse_required_field 2 parse_string x in
  let _p, x = Piqirun.parse_required_field 3 parse_string x in
  let _oc, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _index, x = Piqirun.parse_required_field 5 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 6 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 7 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 8 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 9 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 10 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 12 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 13 parse_string x in
  let _image, x = Piqirun.parse_optional_field 15 parse_string x in
  let _birth_date, x = Piqirun.parse_optional_field 16 parse_string x in
  let _birth_place, x = Piqirun.parse_optional_field 17 parse_string x in
  let _birth_src, x = Piqirun.parse_optional_field 18 parse_string x in
  let _baptism_date, x = Piqirun.parse_optional_field 19 parse_string x in
  let _baptism_place, x = Piqirun.parse_optional_field 20 parse_string x in
  let _baptism_src, x = Piqirun.parse_optional_field 21 parse_string x in
  let _death_date, x = Piqirun.parse_optional_field 22 parse_string x in
  let _death_place, x = Piqirun.parse_optional_field 23 parse_string x in
  let _death_src, x = Piqirun.parse_optional_field 24 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 25 parse_death_type x in
  let _burial_date, x = Piqirun.parse_optional_field 26 parse_string x in
  let _burial_place, x = Piqirun.parse_optional_field 27 parse_string x in
  let _burial_src, x = Piqirun.parse_optional_field 28 parse_string x in
  let _occupation, x = Piqirun.parse_optional_field 30 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 31 parse_string x in
  let _titles, x = Piqirun.parse_repeated_field 32 parse_title x in
  let _related, x = Piqirun.parse_repeated_field 33 parse_protobuf_int32 x in
  let _rparents, x = Piqirun.parse_repeated_field 34 parse_relation_parent x in
  let _visible_for_visitors, x = Piqirun.parse_required_field 35 parse_visibility x in
  let _parents, x = Piqirun.parse_optional_field 36 parse_protobuf_int32 x in
  let _families, x = Piqirun.parse_repeated_field 37 parse_protobuf_int32 x in
  let _baseprefix, x = Piqirun.parse_required_field 38 parse_string x in
  let _is_contemporary, x = Piqirun.parse_required_field 39 parse_bool x in
  let _name_is_hidden, x = Piqirun.parse_required_field 40 parse_bool x in
  let _name_is_restricted, x = Piqirun.parse_required_field 41 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    Full_person.sosa = _sosa;
    Full_person.n = _n;
    Full_person.p = _p;
    Full_person.oc = _oc;
    Full_person.index = _index;
    Full_person.sex = _sex;
    Full_person.lastname = _lastname;
    Full_person.firstname = _firstname;
    Full_person.public_name = _public_name;
    Full_person.aliases = _aliases;
    Full_person.qualifiers = _qualifiers;
    Full_person.firstname_aliases = _firstname_aliases;
    Full_person.surname_aliases = _surname_aliases;
    Full_person.image = _image;
    Full_person.birth_date = _birth_date;
    Full_person.birth_place = _birth_place;
    Full_person.birth_src = _birth_src;
    Full_person.baptism_date = _baptism_date;
    Full_person.baptism_place = _baptism_place;
    Full_person.baptism_src = _baptism_src;
    Full_person.death_date = _death_date;
    Full_person.death_place = _death_place;
    Full_person.death_src = _death_src;
    Full_person.death_type = _death_type;
    Full_person.burial_date = _burial_date;
    Full_person.burial_place = _burial_place;
    Full_person.burial_src = _burial_src;
    Full_person.occupation = _occupation;
    Full_person.psources = _psources;
    Full_person.titles = _titles;
    Full_person.related = _related;
    Full_person.rparents = _rparents;
    Full_person.visible_for_visitors = _visible_for_visitors;
    Full_person.parents = _parents;
    Full_person.families = _families;
    Full_person.baseprefix = _baseprefix;
    Full_person.is_contemporary = _is_contemporary;
    Full_person.name_is_hidden = _name_is_hidden;
    Full_person.name_is_restricted = _name_is_restricted;
  }

and parse_full_family x =
  let x = Piqirun.parse_record x in
  let _fsources, x = Piqirun.parse_optional_field 1 parse_string x in
  let _marriage_date, x = Piqirun.parse_optional_field 2 parse_string x in
  let _marriage_place, x = Piqirun.parse_optional_field 3 parse_string x in
  let _marriage_src, x = Piqirun.parse_optional_field 4 parse_string x in
  let _marriage_type, x = Piqirun.parse_required_field 5 parse_marriage_type x in
  let _divorce_type, x = Piqirun.parse_required_field 6 parse_divorce_type x in
  let _divorce_date, x = Piqirun.parse_optional_field 7 parse_string x in
  let _witnesses, x = Piqirun.parse_repeated_field 8 parse_protobuf_int32 x in
  let _father, x = Piqirun.parse_required_field 9 parse_protobuf_int32 x in
  let _mother, x = Piqirun.parse_required_field 10 parse_protobuf_int32 x in
  let _children, x = Piqirun.parse_repeated_field 11 parse_protobuf_int32 x in
  let _index, x = Piqirun.parse_required_field 12 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Full_family.fsources = _fsources;
    Full_family.marriage_date = _marriage_date;
    Full_family.marriage_place = _marriage_place;
    Full_family.marriage_src = _marriage_src;
    Full_family.marriage_type = _marriage_type;
    Full_family.divorce_type = _divorce_type;
    Full_family.divorce_date = _divorce_date;
    Full_family.witnesses = _witnesses;
    Full_family.father = _father;
    Full_family.mother = _mother;
    Full_family.children = _children;
    Full_family.index = _index;
  }

and parse_internal_int32 x =
  let x = Piqirun.parse_record x in
  let _value, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Internal_int32.value = _value;
  }

and parse_list_persons x =
  let x = Piqirun.parse_record x in
  let _list_persons, x = Piqirun.parse_repeated_field 1 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    List_persons.list_persons = _list_persons;
  }

and parse_list_full_persons x =
  let x = Piqirun.parse_record x in
  let _persons, x = Piqirun.parse_repeated_field 1 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    List_full_persons.persons = _persons;
  }

and parse_list_full_families x =
  let x = Piqirun.parse_record x in
  let _families, x = Piqirun.parse_repeated_field 1 parse_full_family x in
  Piqirun.check_unparsed_fields x;
  {
    List_full_families.families = _families;
  }

and parse_search_params x =
  let x = Piqirun.parse_record x in
  let _search_type, x = Piqirun.parse_required_field 1 parse_search_type x ~default:"\b\000" in
  let _lastname, x = Piqirun.parse_optional_field 2 parse_string x in
  let _firstname, x = Piqirun.parse_optional_field 3 parse_string x in
  let _only_sosa, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _only_recent, x = Piqirun.parse_required_field 5 parse_bool x ~default:"\b\000" in
  let _maiden_name, x = Piqirun.parse_required_field 6 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Search_params.search_type = _search_type;
    Search_params.lastname = _lastname;
    Search_params.firstname = _firstname;
    Search_params.only_sosa = _only_sosa;
    Search_params.only_recent = _only_recent;
    Search_params.maiden_name = _maiden_name;
  }

and parse_image x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _img, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Image.person = _person;
    Image.img = _img;
  }

and parse_list_images x =
  let x = Piqirun.parse_record x in
  let _list_images, x = Piqirun.parse_repeated_field 1 parse_image x in
  Piqirun.check_unparsed_fields x;
  {
    List_images.list_images = _list_images;
  }

and parse_pers_img x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _img, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Pers_img.person = _person;
    Pers_img.img = _img;
  }

and parse_list_pers_img x =
  let x = Piqirun.parse_record x in
  let _list_pers_img, x = Piqirun.parse_repeated_field 1 parse_pers_img x in
  Piqirun.check_unparsed_fields x;
  {
    List_pers_img.list_pers_img = _list_pers_img;
  }

and parse_index x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Index.index = _index;
  }

and parse_image_address x =
  let x = Piqirun.parse_record x in
  let _img, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Image_address.img = _img;
  }

and parse_close_persons_params x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _nb_gen_asc, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _nb_gen_desc, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  let _spouse_ascend, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _only_recent, x = Piqirun.parse_required_field 5 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Close_persons_params.person = _person;
    Close_persons_params.nb_gen_asc = _nb_gen_asc;
    Close_persons_params.nb_gen_desc = _nb_gen_desc;
    Close_persons_params.spouse_ascend = _spouse_ascend;
    Close_persons_params.only_recent = _only_recent;
  }

and parse_anniversary_params x =
  let x = Piqirun.parse_record x in
  let _month, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Anniversary_params.month = _month;
  }

and parse_graph_params x =
  let x = Piqirun.parse_record x in
  let _generation, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _person, x = Piqirun.parse_required_field 2 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_params.generation = _generation;
    Graph_params.person = _person;
  }

and parse_graph_rel_params x =
  let x = Piqirun.parse_record x in
  let _person1, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _person2, x = Piqirun.parse_required_field 2 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    Graph_rel_params.person1 = _person1;
    Graph_rel_params.person2 = _person2;
  }

and parse_cpl_rel_params x =
  let x = Piqirun.parse_record x in
  let _person1, x = Piqirun.parse_required_field 1 parse_reference_person x in
  let _person2, x = Piqirun.parse_required_field 2 parse_reference_person x in
  Piqirun.check_unparsed_fields x;
  {
    Cpl_rel_params.person1 = _person1;
    Cpl_rel_params.person2 = _person2;
  }

and parse_node x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _person, x = Piqirun.parse_required_field 2 parse_person x in
  Piqirun.check_unparsed_fields x;
  {
    Node.id = _id;
    Node.person = _person;
  }

and parse_full_node x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _person, x = Piqirun.parse_required_field 2 parse_full_person x in
  Piqirun.check_unparsed_fields x;
  {
    Full_node.id = _id;
    Full_node.person = _person;
  }

and parse_edge x =
  let x = Piqirun.parse_record x in
  let _from_node, x = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let _to_node, x = Piqirun.parse_required_field 2 parse_protobuf_int64 x in
  Piqirun.check_unparsed_fields x;
  {
    Edge.from_node = _from_node;
    Edge.to_node = _to_node;
  }

and parse_graph x =
  let x = Piqirun.parse_record x in
  let _nodes, x = Piqirun.parse_repeated_field 1 parse_node x in
  let _edges, x = Piqirun.parse_repeated_field 2 parse_edge x in
  Piqirun.check_unparsed_fields x;
  {
    Graph.nodes = _nodes;
    Graph.edges = _edges;
  }

and parse_full_graph x =
  let x = Piqirun.parse_record x in
  let _nodes, x = Piqirun.parse_repeated_field 1 parse_full_node x in
  let _edges, x = Piqirun.parse_repeated_field 2 parse_edge x in
  let _families, x = Piqirun.parse_repeated_field 3 parse_full_family x in
  Piqirun.check_unparsed_fields x;
  {
    Full_graph.nodes = _nodes;
    Full_graph.edges = _edges;
    Full_graph.families = _families;
  }

and parse_all_persons_params x =
  let x = Piqirun.parse_record x in
  let _from, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _limit, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    All_persons_params.from = _from;
    All_persons_params.limit = _limit;
  }

and parse_all_families_params x =
  let x = Piqirun.parse_record x in
  let _from, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _limit, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    All_families_params.from = _from;
    All_families_params.limit = _limit;
  }

and parse_warning_event x =
  let x = Piqirun.parse_record x in
  let _pevent, x = Piqirun.parse_optional_field 1 parse_pevent_name x in
  let _fevent, x = Piqirun.parse_optional_field 2 parse_fevent_name x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_event.pevent = _pevent;
    Warning_event.fevent = _fevent;
  }

and parse_warning_person x =
  let x = Piqirun.parse_record x in
  let _n, x = Piqirun.parse_required_field 1 parse_string x in
  let _p, x = Piqirun.parse_required_field 2 parse_string x in
  let _oc, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _lastname, x = Piqirun.parse_required_field 6 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 7 parse_string x in
  let _birth_date, x = Piqirun.parse_optional_field 8 parse_string x in
  let _death_date, x = Piqirun.parse_optional_field 9 parse_string x in
  let _iper, x = Piqirun.parse_required_field 10 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_person.n = _n;
    Warning_person.p = _p;
    Warning_person.oc = _oc;
    Warning_person.lastname = _lastname;
    Warning_person.firstname = _firstname;
    Warning_person.birth_date = _birth_date;
    Warning_person.death_date = _death_date;
    Warning_person.iper = _iper;
  }

and parse_warning_already_defined x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_already_defined.person = _person;
  }

and parse_warning_own_ancestor x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_own_ancestor.person = _person;
  }

and parse_warning_bad_sex_of_married_person x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_bad_sex_of_married_person.person = _person;
  }

and parse_warning_birth_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_birth_after_death.person = _person;
  }

and parse_warning_incoherent_sex x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_incoherent_sex.person = _person;
  }

and parse_warning_changed_order_of_children x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_changed_order_of_children.father = _father;
    Warning_changed_order_of_children.mother = _mother;
  }

and parse_warning_changed_order_of_marriages x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_changed_order_of_marriages.person = _person;
  }

and parse_warning_children_not_in_order x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_children_not_in_order.father = _father;
    Warning_children_not_in_order.mother = _mother;
  }

and parse_warning_dead_too_early_to_be_father x =
  let x = Piqirun.parse_record x in
  let _son, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _father, x = Piqirun.parse_required_field 2 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_dead_too_early_to_be_father.son = _son;
    Warning_dead_too_early_to_be_father.father = _father;
  }

and parse_warning_incoherent_ancestor_date x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _ancestor, x = Piqirun.parse_required_field 2 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_incoherent_ancestor_date.person = _person;
    Warning_incoherent_ancestor_date.ancestor = _ancestor;
  }

and parse_warning_marriage_date_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_marriage_date_after_death.person = _person;
  }

and parse_warning_marriage_date_before_birth x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_marriage_date_before_birth.person = _person;
  }

and parse_warning_mother_dead_before_child_birth x =
  let x = Piqirun.parse_record x in
  let _mother, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _child, x = Piqirun.parse_required_field 2 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_mother_dead_before_child_birth.mother = _mother;
    Warning_mother_dead_before_child_birth.child = _child;
  }

and parse_warning_parent_born_after_child x =
  let x = Piqirun.parse_record x in
  let _parent, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _child, x = Piqirun.parse_required_field 2 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_parent_born_after_child.parent = _parent;
    Warning_parent_born_after_child.child = _child;
  }

and parse_warning_parent_too_young x =
  let x = Piqirun.parse_record x in
  let _parent, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  let _child, x = Piqirun.parse_required_field 3 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_parent_too_young.parent = _parent;
    Warning_parent_too_young.date = _date;
    Warning_parent_too_young.child = _child;
  }

and parse_warning_possible_duplicate_fam x =
  let x = Piqirun.parse_record x in
  let _father1, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother1, x = Piqirun.parse_required_field 2 parse_warning_person x in
  let _father2, x = Piqirun.parse_required_field 3 parse_warning_person x in
  let _mother2, x = Piqirun.parse_required_field 4 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_possible_duplicate_fam.father1 = _father1;
    Warning_possible_duplicate_fam.mother1 = _mother1;
    Warning_possible_duplicate_fam.father2 = _father2;
    Warning_possible_duplicate_fam.mother2 = _mother2;
  }

and parse_warning_possible_duplicate_fam_homonymous x =
  let x = Piqirun.parse_record x in
  let _father1, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother1, x = Piqirun.parse_required_field 2 parse_warning_person x in
  let _father2, x = Piqirun.parse_required_field 3 parse_warning_person x in
  let _mother2, x = Piqirun.parse_required_field 4 parse_warning_person x in
  let _homonymous, x = Piqirun.parse_required_field 5 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_possible_duplicate_fam_homonymous.father1 = _father1;
    Warning_possible_duplicate_fam_homonymous.mother1 = _mother1;
    Warning_possible_duplicate_fam_homonymous.father2 = _father2;
    Warning_possible_duplicate_fam_homonymous.mother2 = _mother2;
    Warning_possible_duplicate_fam_homonymous.homonymous = _homonymous;
  }

and parse_warning_title_dates_error x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _title, x = Piqirun.parse_required_field 2 parse_title x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_title_dates_error.person = _person;
    Warning_title_dates_error.title = _title;
  }

and parse_warning_undefined_sex x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_undefined_sex.person = _person;
  }

and parse_warning_young_for_marriage x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_young_for_marriage.person = _person;
    Warning_young_for_marriage.date = _date;
  }

and parse_warning_old_for_marriage x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_old_for_marriage.person = _person;
    Warning_old_for_marriage.date = _date;
  }

and parse_warning_parent_too_old x =
  let x = Piqirun.parse_record x in
  let _parent, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _date, x = Piqirun.parse_required_field 2 parse_string x in
  let _child, x = Piqirun.parse_required_field 3 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_parent_too_old.parent = _parent;
    Warning_parent_too_old.date = _date;
    Warning_parent_too_old.child = _child;
  }

and parse_warning_close_children x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_warning_person x in
  let _child1, x = Piqirun.parse_required_field 3 parse_warning_person x in
  let _child2, x = Piqirun.parse_required_field 4 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_close_children.father = _father;
    Warning_close_children.mother = _mother;
    Warning_close_children.child1 = _child1;
    Warning_close_children.child2 = _child2;
  }

and parse_warning_distant_children x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_warning_person x in
  let _child1, x = Piqirun.parse_required_field 3 parse_warning_person x in
  let _child2, x = Piqirun.parse_required_field 4 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_distant_children.father = _father;
    Warning_distant_children.mother = _mother;
    Warning_distant_children.child1 = _child1;
    Warning_distant_children.child2 = _child2;
  }

and parse_warning_big_age_between_spouses x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _mother, x = Piqirun.parse_required_field 2 parse_warning_person x in
  let _date, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_big_age_between_spouses.father = _father;
    Warning_big_age_between_spouses.mother = _mother;
    Warning_big_age_between_spouses.date = _date;
  }

and parse_warning_dead_old x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _date, x = Piqirun.parse_required_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_dead_old.person = _person;
    Warning_dead_old.date = _date;
  }

and parse_warning_witness_date_after_death x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _event, x = Piqirun.parse_required_field 2 parse_warning_event x in
  let _origin, x = Piqirun.parse_repeated_field 3 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_witness_date_after_death.person = _person;
    Warning_witness_date_after_death.event = _event;
    Warning_witness_date_after_death.origin = _origin;
  }

and parse_warning_witness_date_before_birth x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _event, x = Piqirun.parse_required_field 2 parse_warning_event x in
  let _origin, x = Piqirun.parse_repeated_field 3 parse_warning_person x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_witness_date_before_birth.person = _person;
    Warning_witness_date_before_birth.event = _event;
    Warning_witness_date_before_birth.origin = _origin;
  }

and parse_warning_event_order x =
  let x = Piqirun.parse_record x in
  let _person, x = Piqirun.parse_required_field 1 parse_warning_person x in
  let _pevents, x = Piqirun.parse_repeated_field 2 parse_pevent_name x in
  let _fevents, x = Piqirun.parse_repeated_field 3 parse_fevent_name x in
  Piqirun.check_unparsed_fields x;
  {
    Warning_event_order.person = _person;
    Warning_event_order.pevents = _pevents;
    Warning_event_order.fevents = _fevents;
  }

and parse_base_warnings x =
  let x = Piqirun.parse_record x in
  let _warning_already_defined, x = Piqirun.parse_repeated_field 1 parse_warning_already_defined x in
  let _warning_own_ancestor, x = Piqirun.parse_repeated_field 2 parse_warning_own_ancestor x in
  let _warning_bad_sex_of_married_person, x = Piqirun.parse_repeated_field 3 parse_warning_bad_sex_of_married_person x in
  let _warning_birth_after_death, x = Piqirun.parse_repeated_field 4 parse_warning_birth_after_death x in
  let _warning_incoherent_sex, x = Piqirun.parse_repeated_field 5 parse_warning_incoherent_sex x in
  let _warning_changed_order_of_children, x = Piqirun.parse_repeated_field 6 parse_warning_changed_order_of_children x in
  let _warning_children_not_in_order, x = Piqirun.parse_repeated_field 7 parse_warning_children_not_in_order x in
  let _warning_dead_too_early_to_be_father, x = Piqirun.parse_repeated_field 8 parse_warning_dead_too_early_to_be_father x in
  let _warning_incoherent_ancestor_date, x = Piqirun.parse_repeated_field 9 parse_warning_incoherent_ancestor_date x in
  let _warning_marriage_date_after_death, x = Piqirun.parse_repeated_field 10 parse_warning_marriage_date_after_death x in
  let _warning_marriage_date_before_birth, x = Piqirun.parse_repeated_field 11 parse_warning_marriage_date_before_birth x in
  let _warning_mother_dead_before_child_birth, x = Piqirun.parse_repeated_field 12 parse_warning_mother_dead_before_child_birth x in
  let _warning_parent_born_after_child, x = Piqirun.parse_repeated_field 13 parse_warning_parent_born_after_child x in
  let _warning_parent_too_young, x = Piqirun.parse_repeated_field 14 parse_warning_parent_too_young x in
  let _warning_title_dates_error, x = Piqirun.parse_repeated_field 15 parse_warning_title_dates_error x in
  let _warning_undefined_sex, x = Piqirun.parse_repeated_field 16 parse_warning_undefined_sex x in
  let _warning_young_for_marriage, x = Piqirun.parse_repeated_field 17 parse_warning_young_for_marriage x in
  let _warning_close_children, x = Piqirun.parse_repeated_field 18 parse_warning_close_children x in
  let _warning_parent_too_old, x = Piqirun.parse_repeated_field 19 parse_warning_parent_too_old x in
  let _warning_changed_order_of_marriages, x = Piqirun.parse_repeated_field 20 parse_warning_changed_order_of_marriages x in
  let _warning_big_age_between_spouses, x = Piqirun.parse_repeated_field 21 parse_warning_big_age_between_spouses x in
  let _warning_dead_old, x = Piqirun.parse_repeated_field 22 parse_warning_dead_old x in
  let _warning_witness_date_after_death, x = Piqirun.parse_repeated_field 24 parse_warning_witness_date_after_death x in
  let _warning_witness_date_before_birth, x = Piqirun.parse_repeated_field 25 parse_warning_witness_date_before_birth x in
  let _warning_possible_duplicate_fam, x = Piqirun.parse_repeated_field 26 parse_warning_possible_duplicate_fam x in
  let _warning_old_for_marriage, x = Piqirun.parse_repeated_field 27 parse_warning_old_for_marriage x in
  let _warning_distant_children, x = Piqirun.parse_repeated_field 28 parse_warning_distant_children x in
  let _warning_event_order, x = Piqirun.parse_repeated_field 29 parse_warning_event_order x in
  let _warning_possible_duplicate_fam_homonymous, x = Piqirun.parse_repeated_field 30 parse_warning_possible_duplicate_fam_homonymous x in
  Piqirun.check_unparsed_fields x;
  {
    Base_warnings.warning_already_defined = _warning_already_defined;
    Base_warnings.warning_own_ancestor = _warning_own_ancestor;
    Base_warnings.warning_bad_sex_of_married_person = _warning_bad_sex_of_married_person;
    Base_warnings.warning_birth_after_death = _warning_birth_after_death;
    Base_warnings.warning_incoherent_sex = _warning_incoherent_sex;
    Base_warnings.warning_changed_order_of_children = _warning_changed_order_of_children;
    Base_warnings.warning_children_not_in_order = _warning_children_not_in_order;
    Base_warnings.warning_dead_too_early_to_be_father = _warning_dead_too_early_to_be_father;
    Base_warnings.warning_incoherent_ancestor_date = _warning_incoherent_ancestor_date;
    Base_warnings.warning_marriage_date_after_death = _warning_marriage_date_after_death;
    Base_warnings.warning_marriage_date_before_birth = _warning_marriage_date_before_birth;
    Base_warnings.warning_mother_dead_before_child_birth = _warning_mother_dead_before_child_birth;
    Base_warnings.warning_parent_born_after_child = _warning_parent_born_after_child;
    Base_warnings.warning_parent_too_young = _warning_parent_too_young;
    Base_warnings.warning_title_dates_error = _warning_title_dates_error;
    Base_warnings.warning_undefined_sex = _warning_undefined_sex;
    Base_warnings.warning_young_for_marriage = _warning_young_for_marriage;
    Base_warnings.warning_close_children = _warning_close_children;
    Base_warnings.warning_parent_too_old = _warning_parent_too_old;
    Base_warnings.warning_changed_order_of_marriages = _warning_changed_order_of_marriages;
    Base_warnings.warning_big_age_between_spouses = _warning_big_age_between_spouses;
    Base_warnings.warning_dead_old = _warning_dead_old;
    Base_warnings.warning_witness_date_after_death = _warning_witness_date_after_death;
    Base_warnings.warning_witness_date_before_birth = _warning_witness_date_before_birth;
    Base_warnings.warning_possible_duplicate_fam = _warning_possible_duplicate_fam;
    Base_warnings.warning_old_for_marriage = _warning_old_for_marriage;
    Base_warnings.warning_distant_children = _warning_distant_children;
    Base_warnings.warning_event_order = _warning_event_order;
    Base_warnings.warning_possible_duplicate_fam_homonymous = _warning_possible_duplicate_fam_homonymous;
  }

and parse_filter_date x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Filter_date.day = _day;
    Filter_date.month = _month;
    Filter_date.year = _year;
  }

and parse_filter_date_range x =
  let x = Piqirun.parse_record x in
  let _date_begin, x = Piqirun.parse_required_field 1 parse_filter_date x in
  let _date_end, x = Piqirun.parse_required_field 2 parse_filter_date x in
  let _only_exact, x = Piqirun.parse_required_field 3 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Filter_date_range.date_begin = _date_begin;
    Filter_date_range.date_end = _date_end;
    Filter_date_range.only_exact = _only_exact;
  }

and parse_filters x =
  let x = Piqirun.parse_record x in
  let _only_sosa, x = Piqirun.parse_required_field 1 parse_bool x ~default:"\b\000" in
  let _only_recent, x = Piqirun.parse_required_field 2 parse_bool x ~default:"\b\000" in
  let _sex, x = Piqirun.parse_optional_field 3 parse_sex x in
  let _nb_results, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _date_birth, x = Piqirun.parse_optional_field 5 parse_filter_date_range x in
  let _date_death, x = Piqirun.parse_optional_field 6 parse_filter_date_range x in
  Piqirun.check_unparsed_fields x;
  {
    Filters.only_sosa = _only_sosa;
    Filters.only_recent = _only_recent;
    Filters.sex = _sex;
    Filters.nb_results = _nb_results;
    Filters.date_birth = _date_birth;
    Filters.date_death = _date_death;
  }

and parse_modification_status x =
  let x = Piqirun.parse_record x in
  let _status, x = Piqirun.parse_required_field 1 parse_bool x in
  let _base_warnings, x = Piqirun.parse_required_field 2 parse_base_warnings x in
  let _index, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Modification_status.status = _status;
    Modification_status.base_warnings = _base_warnings;
    Modification_status.index = _index;
  }

and parse_person_start x =
  let x = Piqirun.parse_record x in
  let _lastname, x = Piqirun.parse_required_field 1 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 2 parse_string x in
  let _sex, x = Piqirun.parse_required_field 3 parse_sex x in
  let _birth_date_day, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  let _birth_date_month, x = Piqirun.parse_optional_field 5 parse_protobuf_int32 x in
  let _birth_date_year, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Person_start.lastname = _lastname;
    Person_start.firstname = _firstname;
    Person_start.sex = _sex;
    Person_start.birth_date_day = _birth_date_day;
    Person_start.birth_date_month = _birth_date_month;
    Person_start.birth_date_year = _birth_date_year;
  }

and parse_last_modifications x =
  let x = Piqirun.parse_record x in
  let _wizard, x = Piqirun.parse_optional_field 1 parse_string x in
  let _max_res, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _range, x = Piqirun.parse_optional_field 3 parse_filter_date_range x in
  Piqirun.check_unparsed_fields x;
  {
    Last_modifications.wizard = _wizard;
    Last_modifications.max_res = _max_res;
    Last_modifications.range = _range;
  }

and parse_last_visits x =
  let x = Piqirun.parse_record x in
  let _user, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Last_visits.user = _user;
  }

and parse_dmy x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_required_field 3 parse_int32 x in
  let _delta, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Dmy.day = _day;
    Dmy.month = _month;
    Dmy.year = _year;
    Dmy.delta = _delta;
  }

and parse_date x =
  let x = Piqirun.parse_record x in
  let _cal, x = Piqirun.parse_optional_field 1 parse_calendar x in
  let _prec, x = Piqirun.parse_optional_field 2 parse_precision x in
  let _dmy, x = Piqirun.parse_optional_field 3 parse_dmy x in
  let _dmy2, x = Piqirun.parse_optional_field 4 parse_dmy x in
  let _text, x = Piqirun.parse_optional_field 5 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Date.cal = _cal;
    Date.prec = _prec;
    Date.dmy = _dmy;
    Date.dmy2 = _dmy2;
    Date.text = _text;
  }

and parse_events_query_params x =
  let x = Piqirun.parse_record x in
  let _close_persons_params, x = Piqirun.parse_optional_field 1 parse_close_persons_params x in
  let _start_date, x = Piqirun.parse_optional_field 2 parse_date x in
  let _stop_date, x = Piqirun.parse_optional_field 3 parse_date x in
  let _pevents, x = Piqirun.parse_repeated_field 4 parse_pevent_name x in
  let _fevents, x = Piqirun.parse_repeated_field 5 parse_fevent_name x in
  Piqirun.check_unparsed_fields x;
  {
    Events_query_params.close_persons_params = _close_persons_params;
    Events_query_params.start_date = _start_date;
    Events_query_params.stop_date = _stop_date;
    Events_query_params.pevents = _pevents;
    Events_query_params.fevents = _fevents;
  }

and parse_event_query_result x =
  let x = Piqirun.parse_record x in
  let _p, x = Piqirun.parse_required_field 1 parse_person x in
  let _sp, x = Piqirun.parse_optional_field 2 parse_person x in
  let _pevent_name, x = Piqirun.parse_optional_field 3 parse_pevent_name x in
  let _fevent_name, x = Piqirun.parse_optional_field 4 parse_fevent_name x in
  let _date, x = Piqirun.parse_required_field 5 parse_date x in
  let _place, x = Piqirun.parse_required_field 6 parse_string x in
  let _note, x = Piqirun.parse_required_field 7 parse_string x in
  let _src, x = Piqirun.parse_required_field 8 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Event_query_result.p = _p;
    Event_query_result.sp = _sp;
    Event_query_result.pevent_name = _pevent_name;
    Event_query_result.fevent_name = _fevent_name;
    Event_query_result.date = _date;
    Event_query_result.place = _place;
    Event_query_result.note = _note;
    Event_query_result.src = _src;
  }

and parse_event_query_result_list x =
  let x = Piqirun.parse_record x in
  let _events, x = Piqirun.parse_repeated_field 1 parse_event_query_result x in
  Piqirun.check_unparsed_fields x;
  {
    Event_query_result_list.events = _events;
  }

and parse_name_frequency_result x =
  let x = Piqirun.parse_record x in
  let _key, x = Piqirun.parse_required_field 1 parse_string x in
  let _name, x = Piqirun.parse_required_field 2 parse_string x in
  let _count, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Name_frequency_result.key = _key;
    Name_frequency_result.name = _name;
    Name_frequency_result.count = _count;
  }

and parse_name_frequency_result_list x =
  let x = Piqirun.parse_record x in
  let _result, x = Piqirun.parse_repeated_field 1 parse_name_frequency_result x in
  let _total, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Name_frequency_result_list.result = _result;
    Name_frequency_result_list.total = _total;
  }

and parse_name_frequency_params x =
  let x = Piqirun.parse_record x in
  let _type_, x = Piqirun.parse_required_field 1 parse_name_frequency_params_type x in
  let _from, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _to_, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Name_frequency_params.type_ = _type_;
    Name_frequency_params.from = _from;
    Name_frequency_params.to_ = _to_;
  }

and parse_name_frequency_params_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `last_name
    | 2l -> `first_name
    | x -> Piqirun.error_enum_const x
and packed_parse_name_frequency_params_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `last_name
    | 2l -> `first_name
    | x -> Piqirun.error_enum_const x

and parse_error x =
  let x = Piqirun.parse_record x in
  let _code, x = Piqirun.parse_required_field 998 parse_error_code x in
  let _message, x = Piqirun.parse_optional_field 999 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Error.code = _code;
    Error.message = _message;
  }

and parse_error_code x =
  match Piqirun.int32_of_signed_varint x with
    | 400l -> `bad_request
    | 401l -> `unauthorized
    | 403l -> `forbidden
    | 404l -> `not_found
    | 409l -> `conflict
    | x -> Piqirun.error_enum_const x
and packed_parse_error_code x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 400l -> `bad_request
    | 401l -> `unauthorized
    | 403l -> `forbidden
    | 404l -> `not_found
    | 409l -> `conflict
    | x -> Piqirun.error_enum_const x

and parse_time x =
  let x = Piqirun.parse_record x in
  let _year, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _day, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _hour, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _minute, x = Piqirun.parse_required_field 5 parse_protobuf_int32 x in
  let _second, x = Piqirun.parse_required_field 6 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Time.year = _year;
    Time.month = _month;
    Time.day = _day;
    Time.hour = _hour;
    Time.minute = _minute;
    Time.second = _second;
  }

and parse_history_request x =
  let x = Piqirun.parse_record x in
  let _page, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _elements_per_page, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _filter_user, x = Piqirun.parse_optional_field 3 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    History_request.page = _page;
    History_request.elements_per_page = _elements_per_page;
    History_request.filter_user = _filter_user;
  }

and parse_history_person x =
  let x = Piqirun.parse_record x in
  let _n, x = Piqirun.parse_required_field 1 parse_string x in
  let _p, x = Piqirun.parse_required_field 2 parse_string x in
  let _oc, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _lastname, x = Piqirun.parse_required_field 5 parse_string x in
  let _year1, x = Piqirun.parse_optional_field 6 parse_protobuf_int32 x in
  let _year2, x = Piqirun.parse_optional_field 7 parse_protobuf_int32 x in
  let _exists_in_base, x = Piqirun.parse_required_field 8 parse_bool x in
  let _has_history, x = Piqirun.parse_required_field 9 parse_bool x in
  Piqirun.check_unparsed_fields x;
  {
    History_person.n = _n;
    History_person.p = _p;
    History_person.oc = _oc;
    History_person.firstname = _firstname;
    History_person.lastname = _lastname;
    History_person.year1 = _year1;
    History_person.year2 = _year2;
    History_person.exists_in_base = _exists_in_base;
    History_person.has_history = _has_history;
  }

and parse_history_note x =
  let x = Piqirun.parse_record x in
  let _link_parameters, x = Piqirun.parse_required_field 1 parse_string x in
  let _link_txt, x = Piqirun.parse_required_field 2 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    History_note.link_parameters = _link_parameters;
    History_note.link_txt = _link_txt;
  }

and parse_history_entry x =
  let x = Piqirun.parse_record x in
  let _modification_type, x = Piqirun.parse_required_field 1 parse_modification_type x in
  let _time, x = Piqirun.parse_required_field 2 parse_time x in
  let _editor, x = Piqirun.parse_required_field 3 parse_string x in
  let _person, x = Piqirun.parse_optional_field 4 parse_history_person x in
  let _note, x = Piqirun.parse_optional_field 5 parse_history_note x in
  Piqirun.check_unparsed_fields x;
  {
    History_entry.modification_type = _modification_type;
    History_entry.time = _time;
    History_entry.editor = _editor;
    History_entry.person = _person;
    History_entry.note = _note;
  }

and parse_history x =
  let x = Piqirun.parse_record x in
  let _entries, x = Piqirun.parse_repeated_field 1 parse_history_entry x in
  let _page, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _total_elements, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    History.entries = _entries;
    History.page = _page;
    History.total_elements = _total_elements;
  }

and parse_sex x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x
and packed_parse_sex x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x

and parse_death_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x
and packed_parse_death_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x

and parse_marriage_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | 6l -> `marriage_bann
    | 7l -> `marriage_contract
    | 8l -> `marriage_license
    | 9l -> `pacs
    | 10l -> `residence
    | x -> Piqirun.error_enum_const x
and packed_parse_marriage_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | 6l -> `marriage_bann
    | 7l -> `marriage_contract
    | 8l -> `marriage_license
    | 9l -> `pacs
    | 10l -> `residence
    | x -> Piqirun.error_enum_const x

and parse_divorce_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x
and packed_parse_divorce_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x

and parse_relation_parent_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `rpt_adoption
    | 1l -> `rpt_recognition
    | 2l -> `rpt_candidate_parent
    | 3l -> `rpt_god_parent
    | 4l -> `rpt_foster_parent
    | x -> Piqirun.error_enum_const x
and packed_parse_relation_parent_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `rpt_adoption
    | 1l -> `rpt_recognition
    | 2l -> `rpt_candidate_parent
    | 3l -> `rpt_god_parent
    | 4l -> `rpt_foster_parent
    | x -> Piqirun.error_enum_const x

and parse_title_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x
and packed_parse_title_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x

and parse_visibility x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `visibility_public
    | 1l -> `visibility_semi_public
    | 2l -> `visibility_private
    | x -> Piqirun.error_enum_const x
and packed_parse_visibility x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `visibility_public
    | 1l -> `visibility_semi_public
    | 2l -> `visibility_private
    | x -> Piqirun.error_enum_const x

and parse_search_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `starting_with
    | 1l -> `approximative
    | 2l -> `lastname_or_firstname
    | x -> Piqirun.error_enum_const x
and packed_parse_search_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `starting_with
    | 1l -> `approximative
    | 2l -> `lastname_or_firstname
    | x -> Piqirun.error_enum_const x

and parse_pevent_name x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `epers_birth
    | 1l -> `epers_baptism
    | 2l -> `epers_death
    | 3l -> `epers_burial
    | 4l -> `epers_cremation
    | 5l -> `epers_accomplishment
    | 6l -> `epers_acquisition
    | 7l -> `epers_adhesion
    | 8l -> `epers_baptismlds
    | 9l -> `epers_barmitzvah
    | 10l -> `epers_batmitzvah
    | 11l -> `epers_benediction
    | 12l -> `epers_changename
    | 13l -> `epers_circumcision
    | 14l -> `epers_confirmation
    | 15l -> `epers_confirmationlds
    | 16l -> `epers_decoration
    | 17l -> `epers_demobilisationmilitaire
    | 18l -> `epers_diploma
    | 19l -> `epers_distinction
    | 20l -> `epers_dotation
    | 21l -> `epers_dotationlds
    | 22l -> `epers_education
    | 23l -> `epers_election
    | 24l -> `epers_emigration
    | 25l -> `epers_excommunication
    | 26l -> `epers_familylinklds
    | 27l -> `epers_firstcommunion
    | 28l -> `epers_funeral
    | 29l -> `epers_graduate
    | 30l -> `epers_hospitalisation
    | 31l -> `epers_illness
    | 32l -> `epers_immigration
    | 33l -> `epers_listepassenger
    | 34l -> `epers_militarydistinction
    | 35l -> `epers_militarypromotion
    | 36l -> `epers_militaryservice
    | 37l -> `epers_mobilisationmilitaire
    | 38l -> `epers_naturalisation
    | 39l -> `epers_occupation
    | 40l -> `epers_ordination
    | 41l -> `epers_property
    | 42l -> `epers_recensement
    | 43l -> `epers_residence
    | 44l -> `epers_retired
    | 45l -> `epers_scellentchildlds
    | 46l -> `epers_scellentparentlds
    | 47l -> `epers_scellentspouselds
    | 48l -> `epers_ventebien
    | 49l -> `epers_will
    | x -> Piqirun.error_enum_const x
and packed_parse_pevent_name x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `epers_birth
    | 1l -> `epers_baptism
    | 2l -> `epers_death
    | 3l -> `epers_burial
    | 4l -> `epers_cremation
    | 5l -> `epers_accomplishment
    | 6l -> `epers_acquisition
    | 7l -> `epers_adhesion
    | 8l -> `epers_baptismlds
    | 9l -> `epers_barmitzvah
    | 10l -> `epers_batmitzvah
    | 11l -> `epers_benediction
    | 12l -> `epers_changename
    | 13l -> `epers_circumcision
    | 14l -> `epers_confirmation
    | 15l -> `epers_confirmationlds
    | 16l -> `epers_decoration
    | 17l -> `epers_demobilisationmilitaire
    | 18l -> `epers_diploma
    | 19l -> `epers_distinction
    | 20l -> `epers_dotation
    | 21l -> `epers_dotationlds
    | 22l -> `epers_education
    | 23l -> `epers_election
    | 24l -> `epers_emigration
    | 25l -> `epers_excommunication
    | 26l -> `epers_familylinklds
    | 27l -> `epers_firstcommunion
    | 28l -> `epers_funeral
    | 29l -> `epers_graduate
    | 30l -> `epers_hospitalisation
    | 31l -> `epers_illness
    | 32l -> `epers_immigration
    | 33l -> `epers_listepassenger
    | 34l -> `epers_militarydistinction
    | 35l -> `epers_militarypromotion
    | 36l -> `epers_militaryservice
    | 37l -> `epers_mobilisationmilitaire
    | 38l -> `epers_naturalisation
    | 39l -> `epers_occupation
    | 40l -> `epers_ordination
    | 41l -> `epers_property
    | 42l -> `epers_recensement
    | 43l -> `epers_residence
    | 44l -> `epers_retired
    | 45l -> `epers_scellentchildlds
    | 46l -> `epers_scellentparentlds
    | 47l -> `epers_scellentspouselds
    | 48l -> `epers_ventebien
    | 49l -> `epers_will
    | x -> Piqirun.error_enum_const x

and parse_fevent_name x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `efam_marriage
    | 1l -> `efam_no_marriage
    | 2l -> `efam_no_mention
    | 3l -> `efam_engage
    | 4l -> `efam_divorce
    | 5l -> `efam_separated
    | 6l -> `efam_annulation
    | 7l -> `efam_marriage_bann
    | 8l -> `efam_marriage_contract
    | 9l -> `efam_marriage_license
    | 10l -> `efam_pacs
    | 11l -> `efam_residence
    | x -> Piqirun.error_enum_const x
and packed_parse_fevent_name x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `efam_marriage
    | 1l -> `efam_no_marriage
    | 2l -> `efam_no_mention
    | 3l -> `efam_engage
    | 4l -> `efam_divorce
    | 5l -> `efam_separated
    | 6l -> `efam_annulation
    | 7l -> `efam_marriage_bann
    | 8l -> `efam_marriage_contract
    | 9l -> `efam_marriage_license
    | 10l -> `efam_pacs
    | 11l -> `efam_residence
    | x -> Piqirun.error_enum_const x

and parse_witness_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `witness
    | 1l -> `witness_godparent
    | 2l -> `witness_civilofficer
    | 3l -> `witness_religiousofficer
    | 4l -> `witness_informant
    | 5l -> `witness_attending
    | 6l -> `witness_mentioned
    | 7l -> `witness_other
    | x -> Piqirun.error_enum_const x
and packed_parse_witness_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `witness
    | 1l -> `witness_godparent
    | 2l -> `witness_civilofficer
    | 3l -> `witness_religiousofficer
    | 4l -> `witness_informant
    | 5l -> `witness_attending
    | 6l -> `witness_mentioned
    | 7l -> `witness_other
    | x -> Piqirun.error_enum_const x

and parse_calendar x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x
and packed_parse_calendar x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x

and parse_precision x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x
and packed_parse_precision x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x

and parse_modification_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `person_added
    | 2l -> `person_modified
    | 3l -> `person_deleted
    | 4l -> `person_merged
    | 5l -> `image_received
    | 6l -> `image_deleted
    | 7l -> `family_added
    | 8l -> `family_modified
    | 9l -> `family_deleted
    | 10l -> `family_inverted
    | 11l -> `family_merged
    | 12l -> `changed_children_names
    | 13l -> `parents_added
    | 14l -> `notes_modified
    | 15l -> `place_modified
    | 16l -> `source_modified
    | 17l -> `occupation_modified
    | x -> Piqirun.error_enum_const x
and packed_parse_modification_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `person_added
    | 2l -> `person_modified
    | 3l -> `person_deleted
    | 4l -> `person_merged
    | 5l -> `image_received
    | 6l -> `image_deleted
    | 7l -> `family_added
    | 8l -> `family_modified
    | 9l -> `family_deleted
    | 10l -> `family_inverted
    | 11l -> `family_merged
    | 12l -> `changed_children_names
    | 13l -> `parents_added
    | 14l -> `notes_modified
    | 15l -> `place_modified
    | 16l -> `source_modified
    | 17l -> `occupation_modified
    | x -> Piqirun.error_enum_const x


let rec gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x

and gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__infos_base code x =
  let _nb_persons = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Infos_base.nb_persons in
  let _nb_families = Piqirun.gen_required_field 2 gen__protobuf_int64 x.Infos_base.nb_families in
  let _sosa = Piqirun.gen_optional_field 3 gen__reference_person x.Infos_base.sosa in
  let _last_modified_person = Piqirun.gen_optional_field 4 gen__protobuf_int64 x.Infos_base.last_modified_person in
  let _real_nb_persons = Piqirun.gen_optional_field 5 gen__protobuf_int64 x.Infos_base.real_nb_persons in
  let _has_ignored_duplicates = Piqirun.gen_optional_field 6 gen__bool x.Infos_base.has_ignored_duplicates in
  Piqirun.gen_record code (_nb_persons :: _nb_families :: _sosa :: _last_modified_person :: _real_nb_persons :: _has_ignored_duplicates :: [])

and gen__reference_person code x =
  let _n = Piqirun.gen_required_field 1 gen__string x.Reference_person.n in
  let _p = Piqirun.gen_required_field 2 gen__string x.Reference_person.p in
  let _oc = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Reference_person.oc in
  Piqirun.gen_record code (_n :: _p :: _oc :: [])

and gen__reference_person_i code x =
  let _key = Piqirun.gen_optional_field 1 gen__reference_person x.Reference_person_i.key in
  let _i = Piqirun.gen_optional_field 2 gen__string x.Reference_person_i.i in
  Piqirun.gen_record code (_key :: _i :: [])

and gen__list_reference_persons code x =
  let _list_ref_persons = Piqirun.gen_repeated_field 1 gen__reference_person x.List_reference_persons.list_ref_persons in
  Piqirun.gen_record code (_list_ref_persons :: [])

and gen__relation_parent code x =
  let _father = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Relation_parent.father in
  let _mother = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Relation_parent.mother in
  let _source = Piqirun.gen_optional_field 3 gen__string x.Relation_parent.source in
  let _rpt_type = Piqirun.gen_required_field 4 gen__relation_parent_type x.Relation_parent.rpt_type in
  Piqirun.gen_record code (_father :: _mother :: _source :: _rpt_type :: [])

and gen__title code x =
  let _title_type = Piqirun.gen_required_field 1 gen__title_type x.Title.title_type in
  let _name = Piqirun.gen_optional_field 2 gen__string x.Title.name in
  let _title = Piqirun.gen_optional_field 3 gen__string x.Title.title in
  let _fief = Piqirun.gen_optional_field 4 gen__string x.Title.fief in
  let _date_begin = Piqirun.gen_optional_field 5 gen__string x.Title.date_begin in
  let _date_end = Piqirun.gen_optional_field 6 gen__string x.Title.date_end in
  let _nth = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.Title.nth in
  Piqirun.gen_record code (_title_type :: _name :: _title :: _fief :: _date_begin :: _date_end :: _nth :: [])

and gen__spouse code x =
  let _sosa = Piqirun.gen_required_field 1 gen__string x.Spouse.sosa in
  let _n = Piqirun.gen_required_field 2 gen__string x.Spouse.n in
  let _p = Piqirun.gen_required_field 3 gen__string x.Spouse.p in
  let _oc = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Spouse.oc in
  let _sex = Piqirun.gen_required_field 5 gen__sex x.Spouse.sex in
  let _lastname = Piqirun.gen_required_field 6 gen__string x.Spouse.lastname in
  let _firstname = Piqirun.gen_required_field 7 gen__string x.Spouse.firstname in
  let _public_name = Piqirun.gen_optional_field 8 gen__string x.Spouse.public_name in
  let _image = Piqirun.gen_required_field 9 gen__string x.Spouse.image in
  let _birth_date = Piqirun.gen_required_field 10 gen__string x.Spouse.birth_date in
  let _birth_place = Piqirun.gen_required_field 11 gen__string x.Spouse.birth_place in
  let _baptism_date = Piqirun.gen_required_field 12 gen__string x.Spouse.baptism_date in
  let _baptism_place = Piqirun.gen_required_field 13 gen__string x.Spouse.baptism_place in
  let _death_date = Piqirun.gen_required_field 14 gen__string x.Spouse.death_date in
  let _death_place = Piqirun.gen_required_field 15 gen__string x.Spouse.death_place in
  let _death_type = Piqirun.gen_required_field 16 gen__death_type x.Spouse.death_type in
  let _burial_date = Piqirun.gen_required_field 17 gen__string x.Spouse.burial_date in
  let _burial_place = Piqirun.gen_required_field 18 gen__string x.Spouse.burial_place in
  let _marriage_date = Piqirun.gen_required_field 19 gen__string x.Spouse.marriage_date in
  let _marriage_place = Piqirun.gen_required_field 20 gen__string x.Spouse.marriage_place in
  let _divorce_type = Piqirun.gen_required_field 21 gen__divorce_type x.Spouse.divorce_type in
  let _visible_for_visitors = Piqirun.gen_required_field 22 gen__visibility x.Spouse.visible_for_visitors in
  let _index = Piqirun.gen_required_field 23 gen__protobuf_int32 x.Spouse.index in
  Piqirun.gen_record code (_sosa :: _n :: _p :: _oc :: _sex :: _lastname :: _firstname :: _public_name :: _image :: _birth_date :: _birth_place :: _baptism_date :: _baptism_place :: _death_date :: _death_place :: _death_type :: _burial_date :: _burial_place :: _marriage_date :: _marriage_place :: _divorce_type :: _visible_for_visitors :: _index :: [])

and gen__person code x =
  let _sosa = Piqirun.gen_required_field 1 gen__string x.Person.sosa in
  let _n = Piqirun.gen_required_field 2 gen__string x.Person.n in
  let _p = Piqirun.gen_required_field 3 gen__string x.Person.p in
  let _oc = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Person.oc in
  let _sex = Piqirun.gen_required_field 5 gen__sex x.Person.sex in
  let _lastname = Piqirun.gen_required_field 6 gen__string x.Person.lastname in
  let _firstname = Piqirun.gen_required_field 7 gen__string x.Person.firstname in
  let _public_name = Piqirun.gen_optional_field 8 gen__string x.Person.public_name in
  let _image = Piqirun.gen_required_field 9 gen__string x.Person.image in
  let _birth_date = Piqirun.gen_required_field 10 gen__string x.Person.birth_date in
  let _birth_place = Piqirun.gen_required_field 11 gen__string x.Person.birth_place in
  let _baptism_date = Piqirun.gen_required_field 12 gen__string x.Person.baptism_date in
  let _baptism_place = Piqirun.gen_required_field 13 gen__string x.Person.baptism_place in
  let _death_date = Piqirun.gen_required_field 14 gen__string x.Person.death_date in
  let _death_place = Piqirun.gen_required_field 15 gen__string x.Person.death_place in
  let _death_type = Piqirun.gen_required_field 16 gen__death_type x.Person.death_type in
  let _burial_date = Piqirun.gen_required_field 17 gen__string x.Person.burial_date in
  let _burial_place = Piqirun.gen_required_field 18 gen__string x.Person.burial_place in
  let _spouses = Piqirun.gen_repeated_field 19 gen__spouse x.Person.spouses in
  let _ascend = Piqirun.gen_required_field 20 gen__bool x.Person.ascend in
  let _descend = Piqirun.gen_required_field 21 gen__bool x.Person.descend in
  let _visible_for_visitors = Piqirun.gen_required_field 22 gen__visibility x.Person.visible_for_visitors in
  let _baseprefix = Piqirun.gen_required_field 23 gen__string x.Person.baseprefix in
  let _index = Piqirun.gen_required_field 24 gen__protobuf_int32 x.Person.index in
  let _is_contemporary = Piqirun.gen_required_field 25 gen__bool x.Person.is_contemporary in
  let _name_is_hidden = Piqirun.gen_required_field 26 gen__bool x.Person.name_is_hidden in
  let _name_is_restricted = Piqirun.gen_required_field 27 gen__bool x.Person.name_is_restricted in
  Piqirun.gen_record code (_sosa :: _n :: _p :: _oc :: _sex :: _lastname :: _firstname :: _public_name :: _image :: _birth_date :: _birth_place :: _baptism_date :: _baptism_place :: _death_date :: _death_place :: _death_type :: _burial_date :: _burial_place :: _spouses :: _ascend :: _descend :: _visible_for_visitors :: _baseprefix :: _index :: _is_contemporary :: _name_is_hidden :: _name_is_restricted :: [])

and gen__full_person code x =
  let _sosa = Piqirun.gen_required_field 1 gen__string x.Full_person.sosa in
  let _n = Piqirun.gen_required_field 2 gen__string x.Full_person.n in
  let _p = Piqirun.gen_required_field 3 gen__string x.Full_person.p in
  let _oc = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Full_person.oc in
  let _index = Piqirun.gen_required_field 5 gen__protobuf_int32 x.Full_person.index in
  let _sex = Piqirun.gen_required_field 6 gen__sex x.Full_person.sex in
  let _lastname = Piqirun.gen_required_field 7 gen__string x.Full_person.lastname in
  let _firstname = Piqirun.gen_required_field 8 gen__string x.Full_person.firstname in
  let _public_name = Piqirun.gen_optional_field 9 gen__string x.Full_person.public_name in
  let _aliases = Piqirun.gen_repeated_field 10 gen__string x.Full_person.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 11 gen__string x.Full_person.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 12 gen__string x.Full_person.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 13 gen__string x.Full_person.surname_aliases in
  let _image = Piqirun.gen_optional_field 15 gen__string x.Full_person.image in
  let _birth_date = Piqirun.gen_optional_field 16 gen__string x.Full_person.birth_date in
  let _birth_place = Piqirun.gen_optional_field 17 gen__string x.Full_person.birth_place in
  let _birth_src = Piqirun.gen_optional_field 18 gen__string x.Full_person.birth_src in
  let _baptism_date = Piqirun.gen_optional_field 19 gen__string x.Full_person.baptism_date in
  let _baptism_place = Piqirun.gen_optional_field 20 gen__string x.Full_person.baptism_place in
  let _baptism_src = Piqirun.gen_optional_field 21 gen__string x.Full_person.baptism_src in
  let _death_date = Piqirun.gen_optional_field 22 gen__string x.Full_person.death_date in
  let _death_place = Piqirun.gen_optional_field 23 gen__string x.Full_person.death_place in
  let _death_src = Piqirun.gen_optional_field 24 gen__string x.Full_person.death_src in
  let _death_type = Piqirun.gen_required_field 25 gen__death_type x.Full_person.death_type in
  let _burial_date = Piqirun.gen_optional_field 26 gen__string x.Full_person.burial_date in
  let _burial_place = Piqirun.gen_optional_field 27 gen__string x.Full_person.burial_place in
  let _burial_src = Piqirun.gen_optional_field 28 gen__string x.Full_person.burial_src in
  let _occupation = Piqirun.gen_optional_field 30 gen__string x.Full_person.occupation in
  let _psources = Piqirun.gen_optional_field 31 gen__string x.Full_person.psources in
  let _titles = Piqirun.gen_repeated_field 32 gen__title x.Full_person.titles in
  let _related = Piqirun.gen_repeated_field 33 gen__protobuf_int32 x.Full_person.related in
  let _rparents = Piqirun.gen_repeated_field 34 gen__relation_parent x.Full_person.rparents in
  let _visible_for_visitors = Piqirun.gen_required_field 35 gen__visibility x.Full_person.visible_for_visitors in
  let _parents = Piqirun.gen_optional_field 36 gen__protobuf_int32 x.Full_person.parents in
  let _families = Piqirun.gen_repeated_field 37 gen__protobuf_int32 x.Full_person.families in
  let _baseprefix = Piqirun.gen_required_field 38 gen__string x.Full_person.baseprefix in
  let _is_contemporary = Piqirun.gen_required_field 39 gen__bool x.Full_person.is_contemporary in
  let _name_is_hidden = Piqirun.gen_required_field 40 gen__bool x.Full_person.name_is_hidden in
  let _name_is_restricted = Piqirun.gen_required_field 41 gen__bool x.Full_person.name_is_restricted in
  Piqirun.gen_record code (_sosa :: _n :: _p :: _oc :: _index :: _sex :: _lastname :: _firstname :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _image :: _birth_date :: _birth_place :: _birth_src :: _baptism_date :: _baptism_place :: _baptism_src :: _death_date :: _death_place :: _death_src :: _death_type :: _burial_date :: _burial_place :: _burial_src :: _occupation :: _psources :: _titles :: _related :: _rparents :: _visible_for_visitors :: _parents :: _families :: _baseprefix :: _is_contemporary :: _name_is_hidden :: _name_is_restricted :: [])

and gen__full_family code x =
  let _fsources = Piqirun.gen_optional_field 1 gen__string x.Full_family.fsources in
  let _marriage_date = Piqirun.gen_optional_field 2 gen__string x.Full_family.marriage_date in
  let _marriage_place = Piqirun.gen_optional_field 3 gen__string x.Full_family.marriage_place in
  let _marriage_src = Piqirun.gen_optional_field 4 gen__string x.Full_family.marriage_src in
  let _marriage_type = Piqirun.gen_required_field 5 gen__marriage_type x.Full_family.marriage_type in
  let _divorce_type = Piqirun.gen_required_field 6 gen__divorce_type x.Full_family.divorce_type in
  let _divorce_date = Piqirun.gen_optional_field 7 gen__string x.Full_family.divorce_date in
  let _witnesses = Piqirun.gen_repeated_field 8 gen__protobuf_int32 x.Full_family.witnesses in
  let _father = Piqirun.gen_required_field 9 gen__protobuf_int32 x.Full_family.father in
  let _mother = Piqirun.gen_required_field 10 gen__protobuf_int32 x.Full_family.mother in
  let _children = Piqirun.gen_repeated_field 11 gen__protobuf_int32 x.Full_family.children in
  let _index = Piqirun.gen_required_field 12 gen__protobuf_int32 x.Full_family.index in
  Piqirun.gen_record code (_fsources :: _marriage_date :: _marriage_place :: _marriage_src :: _marriage_type :: _divorce_type :: _divorce_date :: _witnesses :: _father :: _mother :: _children :: _index :: [])

and gen__internal_int32 code x =
  let _value = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Internal_int32.value in
  Piqirun.gen_record code (_value :: [])

and gen__list_persons code x =
  let _list_persons = Piqirun.gen_repeated_field 1 gen__person x.List_persons.list_persons in
  Piqirun.gen_record code (_list_persons :: [])

and gen__list_full_persons code x =
  let _persons = Piqirun.gen_repeated_field 1 gen__full_person x.List_full_persons.persons in
  Piqirun.gen_record code (_persons :: [])

and gen__list_full_families code x =
  let _families = Piqirun.gen_repeated_field 1 gen__full_family x.List_full_families.families in
  Piqirun.gen_record code (_families :: [])

and gen__search_params code x =
  let _search_type = Piqirun.gen_required_field 1 gen__search_type x.Search_params.search_type in
  let _lastname = Piqirun.gen_optional_field 2 gen__string x.Search_params.lastname in
  let _firstname = Piqirun.gen_optional_field 3 gen__string x.Search_params.firstname in
  let _only_sosa = Piqirun.gen_required_field 4 gen__bool x.Search_params.only_sosa in
  let _only_recent = Piqirun.gen_required_field 5 gen__bool x.Search_params.only_recent in
  let _maiden_name = Piqirun.gen_required_field 6 gen__bool x.Search_params.maiden_name in
  Piqirun.gen_record code (_search_type :: _lastname :: _firstname :: _only_sosa :: _only_recent :: _maiden_name :: [])

and gen__image code x =
  let _person = Piqirun.gen_required_field 1 gen__reference_person x.Image.person in
  let _img = Piqirun.gen_required_field 2 gen__string x.Image.img in
  Piqirun.gen_record code (_person :: _img :: [])

and gen__list_images code x =
  let _list_images = Piqirun.gen_repeated_field 1 gen__image x.List_images.list_images in
  Piqirun.gen_record code (_list_images :: [])

and gen__pers_img code x =
  let _person = Piqirun.gen_required_field 1 gen__reference_person x.Pers_img.person in
  let _img = Piqirun.gen_required_field 2 gen__string x.Pers_img.img in
  Piqirun.gen_record code (_person :: _img :: [])

and gen__list_pers_img code x =
  let _list_pers_img = Piqirun.gen_repeated_field 1 gen__pers_img x.List_pers_img.list_pers_img in
  Piqirun.gen_record code (_list_pers_img :: [])

and gen__index code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index.index in
  Piqirun.gen_record code (_index :: [])

and gen__image_address code x =
  let _img = Piqirun.gen_required_field 1 gen__string x.Image_address.img in
  Piqirun.gen_record code (_img :: [])

and gen__close_persons_params code x =
  let _person = Piqirun.gen_required_field 1 gen__reference_person x.Close_persons_params.person in
  let _nb_gen_asc = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Close_persons_params.nb_gen_asc in
  let _nb_gen_desc = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Close_persons_params.nb_gen_desc in
  let _spouse_ascend = Piqirun.gen_required_field 4 gen__bool x.Close_persons_params.spouse_ascend in
  let _only_recent = Piqirun.gen_required_field 5 gen__bool x.Close_persons_params.only_recent in
  Piqirun.gen_record code (_person :: _nb_gen_asc :: _nb_gen_desc :: _spouse_ascend :: _only_recent :: [])

and gen__anniversary_params code x =
  let _month = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Anniversary_params.month in
  Piqirun.gen_record code (_month :: [])

and gen__graph_params code x =
  let _generation = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Graph_params.generation in
  let _person = Piqirun.gen_required_field 2 gen__reference_person x.Graph_params.person in
  Piqirun.gen_record code (_generation :: _person :: [])

and gen__graph_rel_params code x =
  let _person1 = Piqirun.gen_required_field 1 gen__reference_person x.Graph_rel_params.person1 in
  let _person2 = Piqirun.gen_required_field 2 gen__reference_person x.Graph_rel_params.person2 in
  Piqirun.gen_record code (_person1 :: _person2 :: [])

and gen__cpl_rel_params code x =
  let _person1 = Piqirun.gen_required_field 1 gen__reference_person x.Cpl_rel_params.person1 in
  let _person2 = Piqirun.gen_required_field 2 gen__reference_person x.Cpl_rel_params.person2 in
  Piqirun.gen_record code (_person1 :: _person2 :: [])

and gen__node code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Node.id in
  let _person = Piqirun.gen_required_field 2 gen__person x.Node.person in
  Piqirun.gen_record code (_id :: _person :: [])

and gen__full_node code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Full_node.id in
  let _person = Piqirun.gen_required_field 2 gen__full_person x.Full_node.person in
  Piqirun.gen_record code (_id :: _person :: [])

and gen__edge code x =
  let _from_node = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Edge.from_node in
  let _to_node = Piqirun.gen_required_field 2 gen__protobuf_int64 x.Edge.to_node in
  Piqirun.gen_record code (_from_node :: _to_node :: [])

and gen__graph code x =
  let _nodes = Piqirun.gen_repeated_field 1 gen__node x.Graph.nodes in
  let _edges = Piqirun.gen_repeated_field 2 gen__edge x.Graph.edges in
  Piqirun.gen_record code (_nodes :: _edges :: [])

and gen__full_graph code x =
  let _nodes = Piqirun.gen_repeated_field 1 gen__full_node x.Full_graph.nodes in
  let _edges = Piqirun.gen_repeated_field 2 gen__edge x.Full_graph.edges in
  let _families = Piqirun.gen_repeated_field 3 gen__full_family x.Full_graph.families in
  Piqirun.gen_record code (_nodes :: _edges :: _families :: [])

and gen__all_persons_params code x =
  let _from = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.All_persons_params.from in
  let _limit = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.All_persons_params.limit in
  Piqirun.gen_record code (_from :: _limit :: [])

and gen__all_families_params code x =
  let _from = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.All_families_params.from in
  let _limit = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.All_families_params.limit in
  Piqirun.gen_record code (_from :: _limit :: [])

and gen__warning_event code x =
  let _pevent = Piqirun.gen_optional_field 1 gen__pevent_name x.Warning_event.pevent in
  let _fevent = Piqirun.gen_optional_field 2 gen__fevent_name x.Warning_event.fevent in
  Piqirun.gen_record code (_pevent :: _fevent :: [])

and gen__warning_person code x =
  let _n = Piqirun.gen_required_field 1 gen__string x.Warning_person.n in
  let _p = Piqirun.gen_required_field 2 gen__string x.Warning_person.p in
  let _oc = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Warning_person.oc in
  let _lastname = Piqirun.gen_required_field 6 gen__string x.Warning_person.lastname in
  let _firstname = Piqirun.gen_required_field 7 gen__string x.Warning_person.firstname in
  let _birth_date = Piqirun.gen_optional_field 8 gen__string x.Warning_person.birth_date in
  let _death_date = Piqirun.gen_optional_field 9 gen__string x.Warning_person.death_date in
  let _iper = Piqirun.gen_required_field 10 gen__string x.Warning_person.iper in
  Piqirun.gen_record code (_n :: _p :: _oc :: _lastname :: _firstname :: _birth_date :: _death_date :: _iper :: [])

and gen__warning_already_defined code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_already_defined.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_own_ancestor code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_own_ancestor.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_bad_sex_of_married_person code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_bad_sex_of_married_person.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_birth_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_birth_after_death.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_incoherent_sex code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_incoherent_sex.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_changed_order_of_children code x =
  let _father = Piqirun.gen_required_field 1 gen__warning_person x.Warning_changed_order_of_children.father in
  let _mother = Piqirun.gen_required_field 2 gen__warning_person x.Warning_changed_order_of_children.mother in
  Piqirun.gen_record code (_father :: _mother :: [])

and gen__warning_changed_order_of_marriages code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_changed_order_of_marriages.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_children_not_in_order code x =
  let _father = Piqirun.gen_required_field 1 gen__warning_person x.Warning_children_not_in_order.father in
  let _mother = Piqirun.gen_required_field 2 gen__warning_person x.Warning_children_not_in_order.mother in
  Piqirun.gen_record code (_father :: _mother :: [])

and gen__warning_dead_too_early_to_be_father code x =
  let _son = Piqirun.gen_required_field 1 gen__warning_person x.Warning_dead_too_early_to_be_father.son in
  let _father = Piqirun.gen_required_field 2 gen__warning_person x.Warning_dead_too_early_to_be_father.father in
  Piqirun.gen_record code (_son :: _father :: [])

and gen__warning_incoherent_ancestor_date code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_incoherent_ancestor_date.person in
  let _ancestor = Piqirun.gen_required_field 2 gen__warning_person x.Warning_incoherent_ancestor_date.ancestor in
  Piqirun.gen_record code (_person :: _ancestor :: [])

and gen__warning_marriage_date_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_marriage_date_after_death.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_marriage_date_before_birth code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_marriage_date_before_birth.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_mother_dead_before_child_birth code x =
  let _mother = Piqirun.gen_required_field 1 gen__warning_person x.Warning_mother_dead_before_child_birth.mother in
  let _child = Piqirun.gen_required_field 2 gen__warning_person x.Warning_mother_dead_before_child_birth.child in
  Piqirun.gen_record code (_mother :: _child :: [])

and gen__warning_parent_born_after_child code x =
  let _parent = Piqirun.gen_required_field 1 gen__warning_person x.Warning_parent_born_after_child.parent in
  let _child = Piqirun.gen_required_field 2 gen__warning_person x.Warning_parent_born_after_child.child in
  Piqirun.gen_record code (_parent :: _child :: [])

and gen__warning_parent_too_young code x =
  let _parent = Piqirun.gen_required_field 1 gen__warning_person x.Warning_parent_too_young.parent in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_parent_too_young.date in
  let _child = Piqirun.gen_required_field 3 gen__warning_person x.Warning_parent_too_young.child in
  Piqirun.gen_record code (_parent :: _date :: _child :: [])

and gen__warning_possible_duplicate_fam code x =
  let _father1 = Piqirun.gen_required_field 1 gen__warning_person x.Warning_possible_duplicate_fam.father1 in
  let _mother1 = Piqirun.gen_required_field 2 gen__warning_person x.Warning_possible_duplicate_fam.mother1 in
  let _father2 = Piqirun.gen_required_field 3 gen__warning_person x.Warning_possible_duplicate_fam.father2 in
  let _mother2 = Piqirun.gen_required_field 4 gen__warning_person x.Warning_possible_duplicate_fam.mother2 in
  Piqirun.gen_record code (_father1 :: _mother1 :: _father2 :: _mother2 :: [])

and gen__warning_possible_duplicate_fam_homonymous code x =
  let _father1 = Piqirun.gen_required_field 1 gen__warning_person x.Warning_possible_duplicate_fam_homonymous.father1 in
  let _mother1 = Piqirun.gen_required_field 2 gen__warning_person x.Warning_possible_duplicate_fam_homonymous.mother1 in
  let _father2 = Piqirun.gen_required_field 3 gen__warning_person x.Warning_possible_duplicate_fam_homonymous.father2 in
  let _mother2 = Piqirun.gen_required_field 4 gen__warning_person x.Warning_possible_duplicate_fam_homonymous.mother2 in
  let _homonymous = Piqirun.gen_required_field 5 gen__warning_person x.Warning_possible_duplicate_fam_homonymous.homonymous in
  Piqirun.gen_record code (_father1 :: _mother1 :: _father2 :: _mother2 :: _homonymous :: [])

and gen__warning_title_dates_error code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_title_dates_error.person in
  let _title = Piqirun.gen_required_field 2 gen__title x.Warning_title_dates_error.title in
  Piqirun.gen_record code (_person :: _title :: [])

and gen__warning_undefined_sex code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_undefined_sex.person in
  Piqirun.gen_record code (_person :: [])

and gen__warning_young_for_marriage code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_young_for_marriage.person in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_young_for_marriage.date in
  Piqirun.gen_record code (_person :: _date :: [])

and gen__warning_old_for_marriage code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_old_for_marriage.person in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_old_for_marriage.date in
  Piqirun.gen_record code (_person :: _date :: [])

and gen__warning_parent_too_old code x =
  let _parent = Piqirun.gen_required_field 1 gen__warning_person x.Warning_parent_too_old.parent in
  let _date = Piqirun.gen_required_field 2 gen__string x.Warning_parent_too_old.date in
  let _child = Piqirun.gen_required_field 3 gen__warning_person x.Warning_parent_too_old.child in
  Piqirun.gen_record code (_parent :: _date :: _child :: [])

and gen__warning_close_children code x =
  let _father = Piqirun.gen_required_field 1 gen__warning_person x.Warning_close_children.father in
  let _mother = Piqirun.gen_required_field 2 gen__warning_person x.Warning_close_children.mother in
  let _child1 = Piqirun.gen_required_field 3 gen__warning_person x.Warning_close_children.child1 in
  let _child2 = Piqirun.gen_required_field 4 gen__warning_person x.Warning_close_children.child2 in
  Piqirun.gen_record code (_father :: _mother :: _child1 :: _child2 :: [])

and gen__warning_distant_children code x =
  let _father = Piqirun.gen_required_field 1 gen__warning_person x.Warning_distant_children.father in
  let _mother = Piqirun.gen_required_field 2 gen__warning_person x.Warning_distant_children.mother in
  let _child1 = Piqirun.gen_required_field 3 gen__warning_person x.Warning_distant_children.child1 in
  let _child2 = Piqirun.gen_required_field 4 gen__warning_person x.Warning_distant_children.child2 in
  Piqirun.gen_record code (_father :: _mother :: _child1 :: _child2 :: [])

and gen__warning_big_age_between_spouses code x =
  let _father = Piqirun.gen_required_field 1 gen__warning_person x.Warning_big_age_between_spouses.father in
  let _mother = Piqirun.gen_required_field 2 gen__warning_person x.Warning_big_age_between_spouses.mother in
  let _date = Piqirun.gen_required_field 3 gen__string x.Warning_big_age_between_spouses.date in
  Piqirun.gen_record code (_father :: _mother :: _date :: [])

and gen__warning_dead_old code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_dead_old.person in
  let _date = Piqirun.gen_required_field 3 gen__string x.Warning_dead_old.date in
  Piqirun.gen_record code (_person :: _date :: [])

and gen__warning_witness_date_after_death code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_witness_date_after_death.person in
  let _event = Piqirun.gen_required_field 2 gen__warning_event x.Warning_witness_date_after_death.event in
  let _origin = Piqirun.gen_repeated_field 3 gen__warning_person x.Warning_witness_date_after_death.origin in
  Piqirun.gen_record code (_person :: _event :: _origin :: [])

and gen__warning_witness_date_before_birth code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_witness_date_before_birth.person in
  let _event = Piqirun.gen_required_field 2 gen__warning_event x.Warning_witness_date_before_birth.event in
  let _origin = Piqirun.gen_repeated_field 3 gen__warning_person x.Warning_witness_date_before_birth.origin in
  Piqirun.gen_record code (_person :: _event :: _origin :: [])

and gen__warning_event_order code x =
  let _person = Piqirun.gen_required_field 1 gen__warning_person x.Warning_event_order.person in
  let _pevents = Piqirun.gen_repeated_field 2 gen__pevent_name x.Warning_event_order.pevents in
  let _fevents = Piqirun.gen_repeated_field 3 gen__fevent_name x.Warning_event_order.fevents in
  Piqirun.gen_record code (_person :: _pevents :: _fevents :: [])

and gen__base_warnings code x =
  let _warning_already_defined = Piqirun.gen_repeated_field 1 gen__warning_already_defined x.Base_warnings.warning_already_defined in
  let _warning_own_ancestor = Piqirun.gen_repeated_field 2 gen__warning_own_ancestor x.Base_warnings.warning_own_ancestor in
  let _warning_bad_sex_of_married_person = Piqirun.gen_repeated_field 3 gen__warning_bad_sex_of_married_person x.Base_warnings.warning_bad_sex_of_married_person in
  let _warning_birth_after_death = Piqirun.gen_repeated_field 4 gen__warning_birth_after_death x.Base_warnings.warning_birth_after_death in
  let _warning_incoherent_sex = Piqirun.gen_repeated_field 5 gen__warning_incoherent_sex x.Base_warnings.warning_incoherent_sex in
  let _warning_changed_order_of_children = Piqirun.gen_repeated_field 6 gen__warning_changed_order_of_children x.Base_warnings.warning_changed_order_of_children in
  let _warning_children_not_in_order = Piqirun.gen_repeated_field 7 gen__warning_children_not_in_order x.Base_warnings.warning_children_not_in_order in
  let _warning_dead_too_early_to_be_father = Piqirun.gen_repeated_field 8 gen__warning_dead_too_early_to_be_father x.Base_warnings.warning_dead_too_early_to_be_father in
  let _warning_incoherent_ancestor_date = Piqirun.gen_repeated_field 9 gen__warning_incoherent_ancestor_date x.Base_warnings.warning_incoherent_ancestor_date in
  let _warning_marriage_date_after_death = Piqirun.gen_repeated_field 10 gen__warning_marriage_date_after_death x.Base_warnings.warning_marriage_date_after_death in
  let _warning_marriage_date_before_birth = Piqirun.gen_repeated_field 11 gen__warning_marriage_date_before_birth x.Base_warnings.warning_marriage_date_before_birth in
  let _warning_mother_dead_before_child_birth = Piqirun.gen_repeated_field 12 gen__warning_mother_dead_before_child_birth x.Base_warnings.warning_mother_dead_before_child_birth in
  let _warning_parent_born_after_child = Piqirun.gen_repeated_field 13 gen__warning_parent_born_after_child x.Base_warnings.warning_parent_born_after_child in
  let _warning_parent_too_young = Piqirun.gen_repeated_field 14 gen__warning_parent_too_young x.Base_warnings.warning_parent_too_young in
  let _warning_title_dates_error = Piqirun.gen_repeated_field 15 gen__warning_title_dates_error x.Base_warnings.warning_title_dates_error in
  let _warning_undefined_sex = Piqirun.gen_repeated_field 16 gen__warning_undefined_sex x.Base_warnings.warning_undefined_sex in
  let _warning_young_for_marriage = Piqirun.gen_repeated_field 17 gen__warning_young_for_marriage x.Base_warnings.warning_young_for_marriage in
  let _warning_close_children = Piqirun.gen_repeated_field 18 gen__warning_close_children x.Base_warnings.warning_close_children in
  let _warning_parent_too_old = Piqirun.gen_repeated_field 19 gen__warning_parent_too_old x.Base_warnings.warning_parent_too_old in
  let _warning_changed_order_of_marriages = Piqirun.gen_repeated_field 20 gen__warning_changed_order_of_marriages x.Base_warnings.warning_changed_order_of_marriages in
  let _warning_big_age_between_spouses = Piqirun.gen_repeated_field 21 gen__warning_big_age_between_spouses x.Base_warnings.warning_big_age_between_spouses in
  let _warning_dead_old = Piqirun.gen_repeated_field 22 gen__warning_dead_old x.Base_warnings.warning_dead_old in
  let _warning_witness_date_after_death = Piqirun.gen_repeated_field 24 gen__warning_witness_date_after_death x.Base_warnings.warning_witness_date_after_death in
  let _warning_witness_date_before_birth = Piqirun.gen_repeated_field 25 gen__warning_witness_date_before_birth x.Base_warnings.warning_witness_date_before_birth in
  let _warning_possible_duplicate_fam = Piqirun.gen_repeated_field 26 gen__warning_possible_duplicate_fam x.Base_warnings.warning_possible_duplicate_fam in
  let _warning_old_for_marriage = Piqirun.gen_repeated_field 27 gen__warning_old_for_marriage x.Base_warnings.warning_old_for_marriage in
  let _warning_distant_children = Piqirun.gen_repeated_field 28 gen__warning_distant_children x.Base_warnings.warning_distant_children in
  let _warning_event_order = Piqirun.gen_repeated_field 29 gen__warning_event_order x.Base_warnings.warning_event_order in
  let _warning_possible_duplicate_fam_homonymous = Piqirun.gen_repeated_field 30 gen__warning_possible_duplicate_fam_homonymous x.Base_warnings.warning_possible_duplicate_fam_homonymous in
  Piqirun.gen_record code (_warning_already_defined :: _warning_own_ancestor :: _warning_bad_sex_of_married_person :: _warning_birth_after_death :: _warning_incoherent_sex :: _warning_changed_order_of_children :: _warning_children_not_in_order :: _warning_dead_too_early_to_be_father :: _warning_incoherent_ancestor_date :: _warning_marriage_date_after_death :: _warning_marriage_date_before_birth :: _warning_mother_dead_before_child_birth :: _warning_parent_born_after_child :: _warning_parent_too_young :: _warning_title_dates_error :: _warning_undefined_sex :: _warning_young_for_marriage :: _warning_close_children :: _warning_parent_too_old :: _warning_changed_order_of_marriages :: _warning_big_age_between_spouses :: _warning_dead_old :: _warning_witness_date_after_death :: _warning_witness_date_before_birth :: _warning_possible_duplicate_fam :: _warning_old_for_marriage :: _warning_distant_children :: _warning_event_order :: _warning_possible_duplicate_fam_homonymous :: [])

and gen__filter_date code x =
  let _day = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Filter_date.day in
  let _month = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Filter_date.month in
  let _year = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Filter_date.year in
  Piqirun.gen_record code (_day :: _month :: _year :: [])

and gen__filter_date_range code x =
  let _date_begin = Piqirun.gen_required_field 1 gen__filter_date x.Filter_date_range.date_begin in
  let _date_end = Piqirun.gen_required_field 2 gen__filter_date x.Filter_date_range.date_end in
  let _only_exact = Piqirun.gen_required_field 3 gen__bool x.Filter_date_range.only_exact in
  Piqirun.gen_record code (_date_begin :: _date_end :: _only_exact :: [])

and gen__filters code x =
  let _only_sosa = Piqirun.gen_required_field 1 gen__bool x.Filters.only_sosa in
  let _only_recent = Piqirun.gen_required_field 2 gen__bool x.Filters.only_recent in
  let _sex = Piqirun.gen_optional_field 3 gen__sex x.Filters.sex in
  let _nb_results = Piqirun.gen_required_field 4 gen__bool x.Filters.nb_results in
  let _date_birth = Piqirun.gen_optional_field 5 gen__filter_date_range x.Filters.date_birth in
  let _date_death = Piqirun.gen_optional_field 6 gen__filter_date_range x.Filters.date_death in
  Piqirun.gen_record code (_only_sosa :: _only_recent :: _sex :: _nb_results :: _date_birth :: _date_death :: [])

and gen__modification_status code x =
  let _status = Piqirun.gen_required_field 1 gen__bool x.Modification_status.status in
  let _base_warnings = Piqirun.gen_required_field 2 gen__base_warnings x.Modification_status.base_warnings in
  let _index = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Modification_status.index in
  Piqirun.gen_record code (_status :: _base_warnings :: _index :: [])

and gen__person_start code x =
  let _lastname = Piqirun.gen_required_field 1 gen__string x.Person_start.lastname in
  let _firstname = Piqirun.gen_required_field 2 gen__string x.Person_start.firstname in
  let _sex = Piqirun.gen_required_field 3 gen__sex x.Person_start.sex in
  let _birth_date_day = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Person_start.birth_date_day in
  let _birth_date_month = Piqirun.gen_optional_field 5 gen__protobuf_int32 x.Person_start.birth_date_month in
  let _birth_date_year = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.Person_start.birth_date_year in
  Piqirun.gen_record code (_lastname :: _firstname :: _sex :: _birth_date_day :: _birth_date_month :: _birth_date_year :: [])

and gen__last_modifications code x =
  let _wizard = Piqirun.gen_optional_field 1 gen__string x.Last_modifications.wizard in
  let _max_res = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Last_modifications.max_res in
  let _range = Piqirun.gen_optional_field 3 gen__filter_date_range x.Last_modifications.range in
  Piqirun.gen_record code (_wizard :: _max_res :: _range :: [])

and gen__last_visits code x =
  let _user = Piqirun.gen_required_field 1 gen__string x.Last_visits.user in
  Piqirun.gen_record code (_user :: [])

and gen__dmy code x =
  let _day = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Dmy.day in
  let _month = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Dmy.month in
  let _year = Piqirun.gen_required_field 3 gen__int32 x.Dmy.year in
  let _delta = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Dmy.delta in
  Piqirun.gen_record code (_day :: _month :: _year :: _delta :: [])

and gen__date code x =
  let _cal = Piqirun.gen_optional_field 1 gen__calendar x.Date.cal in
  let _prec = Piqirun.gen_optional_field 2 gen__precision x.Date.prec in
  let _dmy = Piqirun.gen_optional_field 3 gen__dmy x.Date.dmy in
  let _dmy2 = Piqirun.gen_optional_field 4 gen__dmy x.Date.dmy2 in
  let _text = Piqirun.gen_optional_field 5 gen__string x.Date.text in
  Piqirun.gen_record code (_cal :: _prec :: _dmy :: _dmy2 :: _text :: [])

and gen__events_query_params code x =
  let _close_persons_params = Piqirun.gen_optional_field 1 gen__close_persons_params x.Events_query_params.close_persons_params in
  let _start_date = Piqirun.gen_optional_field 2 gen__date x.Events_query_params.start_date in
  let _stop_date = Piqirun.gen_optional_field 3 gen__date x.Events_query_params.stop_date in
  let _pevents = Piqirun.gen_repeated_field 4 gen__pevent_name x.Events_query_params.pevents in
  let _fevents = Piqirun.gen_repeated_field 5 gen__fevent_name x.Events_query_params.fevents in
  Piqirun.gen_record code (_close_persons_params :: _start_date :: _stop_date :: _pevents :: _fevents :: [])

and gen__event_query_result code x =
  let _p = Piqirun.gen_required_field 1 gen__person x.Event_query_result.p in
  let _sp = Piqirun.gen_optional_field 2 gen__person x.Event_query_result.sp in
  let _pevent_name = Piqirun.gen_optional_field 3 gen__pevent_name x.Event_query_result.pevent_name in
  let _fevent_name = Piqirun.gen_optional_field 4 gen__fevent_name x.Event_query_result.fevent_name in
  let _date = Piqirun.gen_required_field 5 gen__date x.Event_query_result.date in
  let _place = Piqirun.gen_required_field 6 gen__string x.Event_query_result.place in
  let _note = Piqirun.gen_required_field 7 gen__string x.Event_query_result.note in
  let _src = Piqirun.gen_required_field 8 gen__string x.Event_query_result.src in
  Piqirun.gen_record code (_p :: _sp :: _pevent_name :: _fevent_name :: _date :: _place :: _note :: _src :: [])

and gen__event_query_result_list code x =
  let _events = Piqirun.gen_repeated_field 1 gen__event_query_result x.Event_query_result_list.events in
  Piqirun.gen_record code (_events :: [])

and gen__name_frequency_result code x =
  let _key = Piqirun.gen_required_field 1 gen__string x.Name_frequency_result.key in
  let _name = Piqirun.gen_required_field 2 gen__string x.Name_frequency_result.name in
  let _count = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Name_frequency_result.count in
  Piqirun.gen_record code (_key :: _name :: _count :: [])

and gen__name_frequency_result_list code x =
  let _result = Piqirun.gen_repeated_field 1 gen__name_frequency_result x.Name_frequency_result_list.result in
  let _total = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Name_frequency_result_list.total in
  Piqirun.gen_record code (_result :: _total :: [])

and gen__name_frequency_params code x =
  let _type_ = Piqirun.gen_required_field 1 gen__name_frequency_params_type x.Name_frequency_params.type_ in
  let _from = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Name_frequency_params.from in
  let _to_ = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Name_frequency_params.to_ in
  Piqirun.gen_record code (_type_ :: _from :: _to_ :: [])

and gen__name_frequency_params_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `last_name -> 1l
    | `first_name -> 2l
  )
and packed_gen__name_frequency_params_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `last_name -> 1l
    | `first_name -> 2l
  )

and gen__error code x =
  let _code = Piqirun.gen_required_field 998 gen__error_code x.Error.code in
  let _message = Piqirun.gen_optional_field 999 gen__string x.Error.message in
  Piqirun.gen_record code (_code :: _message :: [])

and gen__error_code code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `bad_request -> 400l
    | `unauthorized -> 401l
    | `forbidden -> 403l
    | `not_found -> 404l
    | `conflict -> 409l
  )
and packed_gen__error_code x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `bad_request -> 400l
    | `unauthorized -> 401l
    | `forbidden -> 403l
    | `not_found -> 404l
    | `conflict -> 409l
  )

and gen__time code x =
  let _year = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Time.year in
  let _month = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Time.month in
  let _day = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Time.day in
  let _hour = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Time.hour in
  let _minute = Piqirun.gen_required_field 5 gen__protobuf_int32 x.Time.minute in
  let _second = Piqirun.gen_required_field 6 gen__protobuf_int32 x.Time.second in
  Piqirun.gen_record code (_year :: _month :: _day :: _hour :: _minute :: _second :: [])

and gen__history_request code x =
  let _page = Piqirun.gen_required_field 1 gen__protobuf_int32 x.History_request.page in
  let _elements_per_page = Piqirun.gen_required_field 2 gen__protobuf_int32 x.History_request.elements_per_page in
  let _filter_user = Piqirun.gen_optional_field 3 gen__string x.History_request.filter_user in
  Piqirun.gen_record code (_page :: _elements_per_page :: _filter_user :: [])

and gen__history_person code x =
  let _n = Piqirun.gen_required_field 1 gen__string x.History_person.n in
  let _p = Piqirun.gen_required_field 2 gen__string x.History_person.p in
  let _oc = Piqirun.gen_required_field 3 gen__protobuf_int32 x.History_person.oc in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.History_person.firstname in
  let _lastname = Piqirun.gen_required_field 5 gen__string x.History_person.lastname in
  let _year1 = Piqirun.gen_optional_field 6 gen__protobuf_int32 x.History_person.year1 in
  let _year2 = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.History_person.year2 in
  let _exists_in_base = Piqirun.gen_required_field 8 gen__bool x.History_person.exists_in_base in
  let _has_history = Piqirun.gen_required_field 9 gen__bool x.History_person.has_history in
  Piqirun.gen_record code (_n :: _p :: _oc :: _firstname :: _lastname :: _year1 :: _year2 :: _exists_in_base :: _has_history :: [])

and gen__history_note code x =
  let _link_parameters = Piqirun.gen_required_field 1 gen__string x.History_note.link_parameters in
  let _link_txt = Piqirun.gen_required_field 2 gen__string x.History_note.link_txt in
  Piqirun.gen_record code (_link_parameters :: _link_txt :: [])

and gen__history_entry code x =
  let _modification_type = Piqirun.gen_required_field 1 gen__modification_type x.History_entry.modification_type in
  let _time = Piqirun.gen_required_field 2 gen__time x.History_entry.time in
  let _editor = Piqirun.gen_required_field 3 gen__string x.History_entry.editor in
  let _person = Piqirun.gen_optional_field 4 gen__history_person x.History_entry.person in
  let _note = Piqirun.gen_optional_field 5 gen__history_note x.History_entry.note in
  Piqirun.gen_record code (_modification_type :: _time :: _editor :: _person :: _note :: [])

and gen__history code x =
  let _entries = Piqirun.gen_repeated_field 1 gen__history_entry x.History.entries in
  let _page = Piqirun.gen_required_field 2 gen__protobuf_int32 x.History.page in
  let _total_elements = Piqirun.gen_required_field 3 gen__protobuf_int32 x.History.total_elements in
  Piqirun.gen_record code (_entries :: _page :: _total_elements :: [])

and gen__sex code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )
and packed_gen__sex x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )

and gen__death_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )
and packed_gen__death_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )

and gen__marriage_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
    | `marriage_bann -> 6l
    | `marriage_contract -> 7l
    | `marriage_license -> 8l
    | `pacs -> 9l
    | `residence -> 10l
  )
and packed_gen__marriage_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
    | `marriage_bann -> 6l
    | `marriage_contract -> 7l
    | `marriage_license -> 8l
    | `pacs -> 9l
    | `residence -> 10l
  )

and gen__divorce_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )
and packed_gen__divorce_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )

and gen__relation_parent_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `rpt_adoption -> 0l
    | `rpt_recognition -> 1l
    | `rpt_candidate_parent -> 2l
    | `rpt_god_parent -> 3l
    | `rpt_foster_parent -> 4l
  )
and packed_gen__relation_parent_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `rpt_adoption -> 0l
    | `rpt_recognition -> 1l
    | `rpt_candidate_parent -> 2l
    | `rpt_god_parent -> 3l
    | `rpt_foster_parent -> 4l
  )

and gen__title_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )
and packed_gen__title_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )

and gen__visibility code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `visibility_public -> 0l
    | `visibility_semi_public -> 1l
    | `visibility_private -> 2l
  )
and packed_gen__visibility x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `visibility_public -> 0l
    | `visibility_semi_public -> 1l
    | `visibility_private -> 2l
  )

and gen__search_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `starting_with -> 0l
    | `approximative -> 1l
    | `lastname_or_firstname -> 2l
  )
and packed_gen__search_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `starting_with -> 0l
    | `approximative -> 1l
    | `lastname_or_firstname -> 2l
  )

and gen__pevent_name code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `epers_birth -> 0l
    | `epers_baptism -> 1l
    | `epers_death -> 2l
    | `epers_burial -> 3l
    | `epers_cremation -> 4l
    | `epers_accomplishment -> 5l
    | `epers_acquisition -> 6l
    | `epers_adhesion -> 7l
    | `epers_baptismlds -> 8l
    | `epers_barmitzvah -> 9l
    | `epers_batmitzvah -> 10l
    | `epers_benediction -> 11l
    | `epers_changename -> 12l
    | `epers_circumcision -> 13l
    | `epers_confirmation -> 14l
    | `epers_confirmationlds -> 15l
    | `epers_decoration -> 16l
    | `epers_demobilisationmilitaire -> 17l
    | `epers_diploma -> 18l
    | `epers_distinction -> 19l
    | `epers_dotation -> 20l
    | `epers_dotationlds -> 21l
    | `epers_education -> 22l
    | `epers_election -> 23l
    | `epers_emigration -> 24l
    | `epers_excommunication -> 25l
    | `epers_familylinklds -> 26l
    | `epers_firstcommunion -> 27l
    | `epers_funeral -> 28l
    | `epers_graduate -> 29l
    | `epers_hospitalisation -> 30l
    | `epers_illness -> 31l
    | `epers_immigration -> 32l
    | `epers_listepassenger -> 33l
    | `epers_militarydistinction -> 34l
    | `epers_militarypromotion -> 35l
    | `epers_militaryservice -> 36l
    | `epers_mobilisationmilitaire -> 37l
    | `epers_naturalisation -> 38l
    | `epers_occupation -> 39l
    | `epers_ordination -> 40l
    | `epers_property -> 41l
    | `epers_recensement -> 42l
    | `epers_residence -> 43l
    | `epers_retired -> 44l
    | `epers_scellentchildlds -> 45l
    | `epers_scellentparentlds -> 46l
    | `epers_scellentspouselds -> 47l
    | `epers_ventebien -> 48l
    | `epers_will -> 49l
  )
and packed_gen__pevent_name x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `epers_birth -> 0l
    | `epers_baptism -> 1l
    | `epers_death -> 2l
    | `epers_burial -> 3l
    | `epers_cremation -> 4l
    | `epers_accomplishment -> 5l
    | `epers_acquisition -> 6l
    | `epers_adhesion -> 7l
    | `epers_baptismlds -> 8l
    | `epers_barmitzvah -> 9l
    | `epers_batmitzvah -> 10l
    | `epers_benediction -> 11l
    | `epers_changename -> 12l
    | `epers_circumcision -> 13l
    | `epers_confirmation -> 14l
    | `epers_confirmationlds -> 15l
    | `epers_decoration -> 16l
    | `epers_demobilisationmilitaire -> 17l
    | `epers_diploma -> 18l
    | `epers_distinction -> 19l
    | `epers_dotation -> 20l
    | `epers_dotationlds -> 21l
    | `epers_education -> 22l
    | `epers_election -> 23l
    | `epers_emigration -> 24l
    | `epers_excommunication -> 25l
    | `epers_familylinklds -> 26l
    | `epers_firstcommunion -> 27l
    | `epers_funeral -> 28l
    | `epers_graduate -> 29l
    | `epers_hospitalisation -> 30l
    | `epers_illness -> 31l
    | `epers_immigration -> 32l
    | `epers_listepassenger -> 33l
    | `epers_militarydistinction -> 34l
    | `epers_militarypromotion -> 35l
    | `epers_militaryservice -> 36l
    | `epers_mobilisationmilitaire -> 37l
    | `epers_naturalisation -> 38l
    | `epers_occupation -> 39l
    | `epers_ordination -> 40l
    | `epers_property -> 41l
    | `epers_recensement -> 42l
    | `epers_residence -> 43l
    | `epers_retired -> 44l
    | `epers_scellentchildlds -> 45l
    | `epers_scellentparentlds -> 46l
    | `epers_scellentspouselds -> 47l
    | `epers_ventebien -> 48l
    | `epers_will -> 49l
  )

and gen__fevent_name code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `efam_marriage -> 0l
    | `efam_no_marriage -> 1l
    | `efam_no_mention -> 2l
    | `efam_engage -> 3l
    | `efam_divorce -> 4l
    | `efam_separated -> 5l
    | `efam_annulation -> 6l
    | `efam_marriage_bann -> 7l
    | `efam_marriage_contract -> 8l
    | `efam_marriage_license -> 9l
    | `efam_pacs -> 10l
    | `efam_residence -> 11l
  )
and packed_gen__fevent_name x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `efam_marriage -> 0l
    | `efam_no_marriage -> 1l
    | `efam_no_mention -> 2l
    | `efam_engage -> 3l
    | `efam_divorce -> 4l
    | `efam_separated -> 5l
    | `efam_annulation -> 6l
    | `efam_marriage_bann -> 7l
    | `efam_marriage_contract -> 8l
    | `efam_marriage_license -> 9l
    | `efam_pacs -> 10l
    | `efam_residence -> 11l
  )

and gen__witness_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `witness -> 0l
    | `witness_godparent -> 1l
    | `witness_civilofficer -> 2l
    | `witness_religiousofficer -> 3l
    | `witness_informant -> 4l
    | `witness_attending -> 5l
    | `witness_mentioned -> 6l
    | `witness_other -> 7l
  )
and packed_gen__witness_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `witness -> 0l
    | `witness_godparent -> 1l
    | `witness_civilofficer -> 2l
    | `witness_religiousofficer -> 3l
    | `witness_informant -> 4l
    | `witness_attending -> 5l
    | `witness_mentioned -> 6l
    | `witness_other -> 7l
  )

and gen__calendar code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )
and packed_gen__calendar x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )

and gen__precision code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )
and packed_gen__precision x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )

and gen__modification_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `person_added -> 1l
    | `person_modified -> 2l
    | `person_deleted -> 3l
    | `person_merged -> 4l
    | `image_received -> 5l
    | `image_deleted -> 6l
    | `family_added -> 7l
    | `family_modified -> 8l
    | `family_deleted -> 9l
    | `family_inverted -> 10l
    | `family_merged -> 11l
    | `changed_children_names -> 12l
    | `parents_added -> 13l
    | `notes_modified -> 14l
    | `place_modified -> 15l
    | `source_modified -> 16l
    | `occupation_modified -> 17l
  )
and packed_gen__modification_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `person_added -> 1l
    | `person_modified -> 2l
    | `person_deleted -> 3l
    | `person_merged -> 4l
    | `image_received -> 5l
    | `image_deleted -> 6l
    | `family_added -> 7l
    | `family_modified -> 8l
    | `family_deleted -> 9l
    | `family_inverted -> 10l
    | `family_merged -> 11l
    | `changed_children_names -> 12l
    | `parents_added -> 13l
    | `notes_modified -> 14l
    | `place_modified -> 15l
    | `source_modified -> 16l
    | `occupation_modified -> 17l
  )


let gen_int64 x = gen__int64 (-1) x
let gen_int32 x = gen__int32 (-1) x
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
let gen_bool x = gen__bool (-1) x
let gen_string x = gen__string (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_infos_base x = gen__infos_base (-1) x
let gen_reference_person x = gen__reference_person (-1) x
let gen_reference_person_i x = gen__reference_person_i (-1) x
let gen_list_reference_persons x = gen__list_reference_persons (-1) x
let gen_relation_parent x = gen__relation_parent (-1) x
let gen_title x = gen__title (-1) x
let gen_spouse x = gen__spouse (-1) x
let gen_person x = gen__person (-1) x
let gen_full_person x = gen__full_person (-1) x
let gen_full_family x = gen__full_family (-1) x
let gen_internal_int32 x = gen__internal_int32 (-1) x
let gen_list_persons x = gen__list_persons (-1) x
let gen_list_full_persons x = gen__list_full_persons (-1) x
let gen_list_full_families x = gen__list_full_families (-1) x
let gen_search_params x = gen__search_params (-1) x
let gen_image x = gen__image (-1) x
let gen_list_images x = gen__list_images (-1) x
let gen_pers_img x = gen__pers_img (-1) x
let gen_list_pers_img x = gen__list_pers_img (-1) x
let gen_index x = gen__index (-1) x
let gen_image_address x = gen__image_address (-1) x
let gen_close_persons_params x = gen__close_persons_params (-1) x
let gen_anniversary_params x = gen__anniversary_params (-1) x
let gen_graph_params x = gen__graph_params (-1) x
let gen_graph_rel_params x = gen__graph_rel_params (-1) x
let gen_cpl_rel_params x = gen__cpl_rel_params (-1) x
let gen_node x = gen__node (-1) x
let gen_full_node x = gen__full_node (-1) x
let gen_edge x = gen__edge (-1) x
let gen_graph x = gen__graph (-1) x
let gen_full_graph x = gen__full_graph (-1) x
let gen_all_persons_params x = gen__all_persons_params (-1) x
let gen_all_families_params x = gen__all_families_params (-1) x
let gen_warning_event x = gen__warning_event (-1) x
let gen_warning_person x = gen__warning_person (-1) x
let gen_warning_already_defined x = gen__warning_already_defined (-1) x
let gen_warning_own_ancestor x = gen__warning_own_ancestor (-1) x
let gen_warning_bad_sex_of_married_person x = gen__warning_bad_sex_of_married_person (-1) x
let gen_warning_birth_after_death x = gen__warning_birth_after_death (-1) x
let gen_warning_incoherent_sex x = gen__warning_incoherent_sex (-1) x
let gen_warning_changed_order_of_children x = gen__warning_changed_order_of_children (-1) x
let gen_warning_changed_order_of_marriages x = gen__warning_changed_order_of_marriages (-1) x
let gen_warning_children_not_in_order x = gen__warning_children_not_in_order (-1) x
let gen_warning_dead_too_early_to_be_father x = gen__warning_dead_too_early_to_be_father (-1) x
let gen_warning_incoherent_ancestor_date x = gen__warning_incoherent_ancestor_date (-1) x
let gen_warning_marriage_date_after_death x = gen__warning_marriage_date_after_death (-1) x
let gen_warning_marriage_date_before_birth x = gen__warning_marriage_date_before_birth (-1) x
let gen_warning_mother_dead_before_child_birth x = gen__warning_mother_dead_before_child_birth (-1) x
let gen_warning_parent_born_after_child x = gen__warning_parent_born_after_child (-1) x
let gen_warning_parent_too_young x = gen__warning_parent_too_young (-1) x
let gen_warning_possible_duplicate_fam x = gen__warning_possible_duplicate_fam (-1) x
let gen_warning_possible_duplicate_fam_homonymous x = gen__warning_possible_duplicate_fam_homonymous (-1) x
let gen_warning_title_dates_error x = gen__warning_title_dates_error (-1) x
let gen_warning_undefined_sex x = gen__warning_undefined_sex (-1) x
let gen_warning_young_for_marriage x = gen__warning_young_for_marriage (-1) x
let gen_warning_old_for_marriage x = gen__warning_old_for_marriage (-1) x
let gen_warning_parent_too_old x = gen__warning_parent_too_old (-1) x
let gen_warning_close_children x = gen__warning_close_children (-1) x
let gen_warning_distant_children x = gen__warning_distant_children (-1) x
let gen_warning_big_age_between_spouses x = gen__warning_big_age_between_spouses (-1) x
let gen_warning_dead_old x = gen__warning_dead_old (-1) x
let gen_warning_witness_date_after_death x = gen__warning_witness_date_after_death (-1) x
let gen_warning_witness_date_before_birth x = gen__warning_witness_date_before_birth (-1) x
let gen_warning_event_order x = gen__warning_event_order (-1) x
let gen_base_warnings x = gen__base_warnings (-1) x
let gen_filter_date x = gen__filter_date (-1) x
let gen_filter_date_range x = gen__filter_date_range (-1) x
let gen_filters x = gen__filters (-1) x
let gen_modification_status x = gen__modification_status (-1) x
let gen_person_start x = gen__person_start (-1) x
let gen_last_modifications x = gen__last_modifications (-1) x
let gen_last_visits x = gen__last_visits (-1) x
let gen_dmy x = gen__dmy (-1) x
let gen_date x = gen__date (-1) x
let gen_events_query_params x = gen__events_query_params (-1) x
let gen_event_query_result x = gen__event_query_result (-1) x
let gen_event_query_result_list x = gen__event_query_result_list (-1) x
let gen_name_frequency_result x = gen__name_frequency_result (-1) x
let gen_name_frequency_result_list x = gen__name_frequency_result_list (-1) x
let gen_name_frequency_params x = gen__name_frequency_params (-1) x
let gen_name_frequency_params_type x = gen__name_frequency_params_type (-1) x
let gen_error x = gen__error (-1) x
let gen_error_code x = gen__error_code (-1) x
let gen_time x = gen__time (-1) x
let gen_history_request x = gen__history_request (-1) x
let gen_history_person x = gen__history_person (-1) x
let gen_history_note x = gen__history_note (-1) x
let gen_history_entry x = gen__history_entry (-1) x
let gen_history x = gen__history (-1) x
let gen_sex x = gen__sex (-1) x
let gen_death_type x = gen__death_type (-1) x
let gen_marriage_type x = gen__marriage_type (-1) x
let gen_divorce_type x = gen__divorce_type (-1) x
let gen_relation_parent_type x = gen__relation_parent_type (-1) x
let gen_title_type x = gen__title_type (-1) x
let gen_visibility x = gen__visibility (-1) x
let gen_search_type x = gen__search_type (-1) x
let gen_pevent_name x = gen__pevent_name (-1) x
let gen_fevent_name x = gen__fevent_name (-1) x
let gen_witness_type x = gen__witness_type (-1) x
let gen_calendar x = gen__calendar (-1) x
let gen_precision x = gen__precision (-1) x
let gen_modification_type x = gen__modification_type (-1) x


let rec default_int64 () = 0L
and default_int32 () = 0l
and default_protobuf_int64 () = default_int64 ()
and default_bool () = false
and default_string () = ""
and default_protobuf_int32 () = default_int32 ()
and default_infos_base () =
  {
    Infos_base.nb_persons = default_protobuf_int64 ();
    Infos_base.nb_families = default_protobuf_int64 ();
    Infos_base.sosa = None;
    Infos_base.last_modified_person = None;
    Infos_base.real_nb_persons = None;
    Infos_base.has_ignored_duplicates = None;
  }
and default_reference_person () =
  {
    Reference_person.n = default_string ();
    Reference_person.p = default_string ();
    Reference_person.oc = default_protobuf_int32 ();
  }
and default_reference_person_i () =
  {
    Reference_person_i.key = None;
    Reference_person_i.i = None;
  }
and default_list_reference_persons () =
  {
    List_reference_persons.list_ref_persons = [];
  }
and default_relation_parent () =
  {
    Relation_parent.father = None;
    Relation_parent.mother = None;
    Relation_parent.source = None;
    Relation_parent.rpt_type = default_relation_parent_type ();
  }
and default_title () =
  {
    Title.title_type = default_title_type ();
    Title.name = None;
    Title.title = None;
    Title.fief = None;
    Title.date_begin = None;
    Title.date_end = None;
    Title.nth = None;
  }
and default_spouse () =
  {
    Spouse.sosa = default_string ();
    Spouse.n = default_string ();
    Spouse.p = default_string ();
    Spouse.oc = default_protobuf_int32 ();
    Spouse.sex = default_sex ();
    Spouse.lastname = default_string ();
    Spouse.firstname = default_string ();
    Spouse.public_name = None;
    Spouse.image = default_string ();
    Spouse.birth_date = default_string ();
    Spouse.birth_place = default_string ();
    Spouse.baptism_date = default_string ();
    Spouse.baptism_place = default_string ();
    Spouse.death_date = default_string ();
    Spouse.death_place = default_string ();
    Spouse.death_type = default_death_type ();
    Spouse.burial_date = default_string ();
    Spouse.burial_place = default_string ();
    Spouse.marriage_date = default_string ();
    Spouse.marriage_place = default_string ();
    Spouse.divorce_type = default_divorce_type ();
    Spouse.visible_for_visitors = default_visibility ();
    Spouse.index = default_protobuf_int32 ();
  }
and default_person () =
  {
    Person.sosa = default_string ();
    Person.n = default_string ();
    Person.p = default_string ();
    Person.oc = default_protobuf_int32 ();
    Person.sex = default_sex ();
    Person.lastname = default_string ();
    Person.firstname = default_string ();
    Person.public_name = None;
    Person.image = default_string ();
    Person.birth_date = default_string ();
    Person.birth_place = default_string ();
    Person.baptism_date = default_string ();
    Person.baptism_place = default_string ();
    Person.death_date = default_string ();
    Person.death_place = default_string ();
    Person.death_type = default_death_type ();
    Person.burial_date = default_string ();
    Person.burial_place = default_string ();
    Person.spouses = [];
    Person.ascend = default_bool ();
    Person.descend = default_bool ();
    Person.visible_for_visitors = default_visibility ();
    Person.baseprefix = default_string ();
    Person.index = default_protobuf_int32 ();
    Person.is_contemporary = default_bool ();
    Person.name_is_hidden = default_bool ();
    Person.name_is_restricted = default_bool ();
  }
and default_full_person () =
  {
    Full_person.sosa = default_string ();
    Full_person.n = default_string ();
    Full_person.p = default_string ();
    Full_person.oc = default_protobuf_int32 ();
    Full_person.index = default_protobuf_int32 ();
    Full_person.sex = default_sex ();
    Full_person.lastname = default_string ();
    Full_person.firstname = default_string ();
    Full_person.public_name = None;
    Full_person.aliases = [];
    Full_person.qualifiers = [];
    Full_person.firstname_aliases = [];
    Full_person.surname_aliases = [];
    Full_person.image = None;
    Full_person.birth_date = None;
    Full_person.birth_place = None;
    Full_person.birth_src = None;
    Full_person.baptism_date = None;
    Full_person.baptism_place = None;
    Full_person.baptism_src = None;
    Full_person.death_date = None;
    Full_person.death_place = None;
    Full_person.death_src = None;
    Full_person.death_type = default_death_type ();
    Full_person.burial_date = None;
    Full_person.burial_place = None;
    Full_person.burial_src = None;
    Full_person.occupation = None;
    Full_person.psources = None;
    Full_person.titles = [];
    Full_person.related = [];
    Full_person.rparents = [];
    Full_person.visible_for_visitors = default_visibility ();
    Full_person.parents = None;
    Full_person.families = [];
    Full_person.baseprefix = default_string ();
    Full_person.is_contemporary = default_bool ();
    Full_person.name_is_hidden = default_bool ();
    Full_person.name_is_restricted = default_bool ();
  }
and default_full_family () =
  {
    Full_family.fsources = None;
    Full_family.marriage_date = None;
    Full_family.marriage_place = None;
    Full_family.marriage_src = None;
    Full_family.marriage_type = default_marriage_type ();
    Full_family.divorce_type = default_divorce_type ();
    Full_family.divorce_date = None;
    Full_family.witnesses = [];
    Full_family.father = default_protobuf_int32 ();
    Full_family.mother = default_protobuf_int32 ();
    Full_family.children = [];
    Full_family.index = default_protobuf_int32 ();
  }
and default_internal_int32 () =
  {
    Internal_int32.value = default_protobuf_int32 ();
  }
and default_list_persons () =
  {
    List_persons.list_persons = [];
  }
and default_list_full_persons () =
  {
    List_full_persons.persons = [];
  }
and default_list_full_families () =
  {
    List_full_families.families = [];
  }
and default_search_params () =
  {
    Search_params.search_type = parse_search_type (Piqirun.parse_default "\b\000");
    Search_params.lastname = None;
    Search_params.firstname = None;
    Search_params.only_sosa = parse_bool (Piqirun.parse_default "\b\000");
    Search_params.only_recent = parse_bool (Piqirun.parse_default "\b\000");
    Search_params.maiden_name = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_image () =
  {
    Image.person = default_reference_person ();
    Image.img = default_string ();
  }
and default_list_images () =
  {
    List_images.list_images = [];
  }
and default_pers_img () =
  {
    Pers_img.person = default_reference_person ();
    Pers_img.img = default_string ();
  }
and default_list_pers_img () =
  {
    List_pers_img.list_pers_img = [];
  }
and default_index () =
  {
    Index.index = default_protobuf_int32 ();
  }
and default_image_address () =
  {
    Image_address.img = default_string ();
  }
and default_close_persons_params () =
  {
    Close_persons_params.person = default_reference_person ();
    Close_persons_params.nb_gen_asc = None;
    Close_persons_params.nb_gen_desc = None;
    Close_persons_params.spouse_ascend = parse_bool (Piqirun.parse_default "\b\000");
    Close_persons_params.only_recent = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_anniversary_params () =
  {
    Anniversary_params.month = None;
  }
and default_graph_params () =
  {
    Graph_params.generation = None;
    Graph_params.person = default_reference_person ();
  }
and default_graph_rel_params () =
  {
    Graph_rel_params.person1 = default_reference_person ();
    Graph_rel_params.person2 = default_reference_person ();
  }
and default_cpl_rel_params () =
  {
    Cpl_rel_params.person1 = default_reference_person ();
    Cpl_rel_params.person2 = default_reference_person ();
  }
and default_node () =
  {
    Node.id = default_protobuf_int64 ();
    Node.person = default_person ();
  }
and default_full_node () =
  {
    Full_node.id = default_protobuf_int64 ();
    Full_node.person = default_full_person ();
  }
and default_edge () =
  {
    Edge.from_node = default_protobuf_int64 ();
    Edge.to_node = default_protobuf_int64 ();
  }
and default_graph () =
  {
    Graph.nodes = [];
    Graph.edges = [];
  }
and default_full_graph () =
  {
    Full_graph.nodes = [];
    Full_graph.edges = [];
    Full_graph.families = [];
  }
and default_all_persons_params () =
  {
    All_persons_params.from = None;
    All_persons_params.limit = None;
  }
and default_all_families_params () =
  {
    All_families_params.from = None;
    All_families_params.limit = None;
  }
and default_warning_event () =
  {
    Warning_event.pevent = None;
    Warning_event.fevent = None;
  }
and default_warning_person () =
  {
    Warning_person.n = default_string ();
    Warning_person.p = default_string ();
    Warning_person.oc = default_protobuf_int32 ();
    Warning_person.lastname = default_string ();
    Warning_person.firstname = default_string ();
    Warning_person.birth_date = None;
    Warning_person.death_date = None;
    Warning_person.iper = default_string ();
  }
and default_warning_already_defined () =
  {
    Warning_already_defined.person = default_warning_person ();
  }
and default_warning_own_ancestor () =
  {
    Warning_own_ancestor.person = default_warning_person ();
  }
and default_warning_bad_sex_of_married_person () =
  {
    Warning_bad_sex_of_married_person.person = default_warning_person ();
  }
and default_warning_birth_after_death () =
  {
    Warning_birth_after_death.person = default_warning_person ();
  }
and default_warning_incoherent_sex () =
  {
    Warning_incoherent_sex.person = default_warning_person ();
  }
and default_warning_changed_order_of_children () =
  {
    Warning_changed_order_of_children.father = default_warning_person ();
    Warning_changed_order_of_children.mother = default_warning_person ();
  }
and default_warning_changed_order_of_marriages () =
  {
    Warning_changed_order_of_marriages.person = default_warning_person ();
  }
and default_warning_children_not_in_order () =
  {
    Warning_children_not_in_order.father = default_warning_person ();
    Warning_children_not_in_order.mother = default_warning_person ();
  }
and default_warning_dead_too_early_to_be_father () =
  {
    Warning_dead_too_early_to_be_father.son = default_warning_person ();
    Warning_dead_too_early_to_be_father.father = default_warning_person ();
  }
and default_warning_incoherent_ancestor_date () =
  {
    Warning_incoherent_ancestor_date.person = default_warning_person ();
    Warning_incoherent_ancestor_date.ancestor = default_warning_person ();
  }
and default_warning_marriage_date_after_death () =
  {
    Warning_marriage_date_after_death.person = default_warning_person ();
  }
and default_warning_marriage_date_before_birth () =
  {
    Warning_marriage_date_before_birth.person = default_warning_person ();
  }
and default_warning_mother_dead_before_child_birth () =
  {
    Warning_mother_dead_before_child_birth.mother = default_warning_person ();
    Warning_mother_dead_before_child_birth.child = default_warning_person ();
  }
and default_warning_parent_born_after_child () =
  {
    Warning_parent_born_after_child.parent = default_warning_person ();
    Warning_parent_born_after_child.child = default_warning_person ();
  }
and default_warning_parent_too_young () =
  {
    Warning_parent_too_young.parent = default_warning_person ();
    Warning_parent_too_young.date = default_string ();
    Warning_parent_too_young.child = default_warning_person ();
  }
and default_warning_possible_duplicate_fam () =
  {
    Warning_possible_duplicate_fam.father1 = default_warning_person ();
    Warning_possible_duplicate_fam.mother1 = default_warning_person ();
    Warning_possible_duplicate_fam.father2 = default_warning_person ();
    Warning_possible_duplicate_fam.mother2 = default_warning_person ();
  }
and default_warning_possible_duplicate_fam_homonymous () =
  {
    Warning_possible_duplicate_fam_homonymous.father1 = default_warning_person ();
    Warning_possible_duplicate_fam_homonymous.mother1 = default_warning_person ();
    Warning_possible_duplicate_fam_homonymous.father2 = default_warning_person ();
    Warning_possible_duplicate_fam_homonymous.mother2 = default_warning_person ();
    Warning_possible_duplicate_fam_homonymous.homonymous = default_warning_person ();
  }
and default_warning_title_dates_error () =
  {
    Warning_title_dates_error.person = default_warning_person ();
    Warning_title_dates_error.title = default_title ();
  }
and default_warning_undefined_sex () =
  {
    Warning_undefined_sex.person = default_warning_person ();
  }
and default_warning_young_for_marriage () =
  {
    Warning_young_for_marriage.person = default_warning_person ();
    Warning_young_for_marriage.date = default_string ();
  }
and default_warning_old_for_marriage () =
  {
    Warning_old_for_marriage.person = default_warning_person ();
    Warning_old_for_marriage.date = default_string ();
  }
and default_warning_parent_too_old () =
  {
    Warning_parent_too_old.parent = default_warning_person ();
    Warning_parent_too_old.date = default_string ();
    Warning_parent_too_old.child = default_warning_person ();
  }
and default_warning_close_children () =
  {
    Warning_close_children.father = default_warning_person ();
    Warning_close_children.mother = default_warning_person ();
    Warning_close_children.child1 = default_warning_person ();
    Warning_close_children.child2 = default_warning_person ();
  }
and default_warning_distant_children () =
  {
    Warning_distant_children.father = default_warning_person ();
    Warning_distant_children.mother = default_warning_person ();
    Warning_distant_children.child1 = default_warning_person ();
    Warning_distant_children.child2 = default_warning_person ();
  }
and default_warning_big_age_between_spouses () =
  {
    Warning_big_age_between_spouses.father = default_warning_person ();
    Warning_big_age_between_spouses.mother = default_warning_person ();
    Warning_big_age_between_spouses.date = default_string ();
  }
and default_warning_dead_old () =
  {
    Warning_dead_old.person = default_warning_person ();
    Warning_dead_old.date = default_string ();
  }
and default_warning_witness_date_after_death () =
  {
    Warning_witness_date_after_death.person = default_warning_person ();
    Warning_witness_date_after_death.event = default_warning_event ();
    Warning_witness_date_after_death.origin = [];
  }
and default_warning_witness_date_before_birth () =
  {
    Warning_witness_date_before_birth.person = default_warning_person ();
    Warning_witness_date_before_birth.event = default_warning_event ();
    Warning_witness_date_before_birth.origin = [];
  }
and default_warning_event_order () =
  {
    Warning_event_order.person = default_warning_person ();
    Warning_event_order.pevents = [];
    Warning_event_order.fevents = [];
  }
and default_base_warnings () =
  {
    Base_warnings.warning_already_defined = [];
    Base_warnings.warning_own_ancestor = [];
    Base_warnings.warning_bad_sex_of_married_person = [];
    Base_warnings.warning_birth_after_death = [];
    Base_warnings.warning_incoherent_sex = [];
    Base_warnings.warning_changed_order_of_children = [];
    Base_warnings.warning_children_not_in_order = [];
    Base_warnings.warning_dead_too_early_to_be_father = [];
    Base_warnings.warning_incoherent_ancestor_date = [];
    Base_warnings.warning_marriage_date_after_death = [];
    Base_warnings.warning_marriage_date_before_birth = [];
    Base_warnings.warning_mother_dead_before_child_birth = [];
    Base_warnings.warning_parent_born_after_child = [];
    Base_warnings.warning_parent_too_young = [];
    Base_warnings.warning_title_dates_error = [];
    Base_warnings.warning_undefined_sex = [];
    Base_warnings.warning_young_for_marriage = [];
    Base_warnings.warning_close_children = [];
    Base_warnings.warning_parent_too_old = [];
    Base_warnings.warning_changed_order_of_marriages = [];
    Base_warnings.warning_big_age_between_spouses = [];
    Base_warnings.warning_dead_old = [];
    Base_warnings.warning_witness_date_after_death = [];
    Base_warnings.warning_witness_date_before_birth = [];
    Base_warnings.warning_possible_duplicate_fam = [];
    Base_warnings.warning_old_for_marriage = [];
    Base_warnings.warning_distant_children = [];
    Base_warnings.warning_event_order = [];
    Base_warnings.warning_possible_duplicate_fam_homonymous = [];
  }
and default_filter_date () =
  {
    Filter_date.day = default_protobuf_int32 ();
    Filter_date.month = default_protobuf_int32 ();
    Filter_date.year = default_protobuf_int32 ();
  }
and default_filter_date_range () =
  {
    Filter_date_range.date_begin = default_filter_date ();
    Filter_date_range.date_end = default_filter_date ();
    Filter_date_range.only_exact = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_filters () =
  {
    Filters.only_sosa = parse_bool (Piqirun.parse_default "\b\000");
    Filters.only_recent = parse_bool (Piqirun.parse_default "\b\000");
    Filters.sex = None;
    Filters.nb_results = parse_bool (Piqirun.parse_default "\b\000");
    Filters.date_birth = None;
    Filters.date_death = None;
  }
and default_modification_status () =
  {
    Modification_status.status = default_bool ();
    Modification_status.base_warnings = default_base_warnings ();
    Modification_status.index = None;
  }
and default_person_start () =
  {
    Person_start.lastname = default_string ();
    Person_start.firstname = default_string ();
    Person_start.sex = default_sex ();
    Person_start.birth_date_day = None;
    Person_start.birth_date_month = None;
    Person_start.birth_date_year = None;
  }
and default_last_modifications () =
  {
    Last_modifications.wizard = None;
    Last_modifications.max_res = None;
    Last_modifications.range = None;
  }
and default_last_visits () =
  {
    Last_visits.user = default_string ();
  }
and default_dmy () =
  {
    Dmy.day = default_protobuf_int32 ();
    Dmy.month = default_protobuf_int32 ();
    Dmy.year = default_int32 ();
    Dmy.delta = default_protobuf_int32 ();
  }
and default_date () =
  {
    Date.cal = None;
    Date.prec = None;
    Date.dmy = None;
    Date.dmy2 = None;
    Date.text = None;
  }
and default_events_query_params () =
  {
    Events_query_params.close_persons_params = None;
    Events_query_params.start_date = None;
    Events_query_params.stop_date = None;
    Events_query_params.pevents = [];
    Events_query_params.fevents = [];
  }
and default_event_query_result () =
  {
    Event_query_result.p = default_person ();
    Event_query_result.sp = None;
    Event_query_result.pevent_name = None;
    Event_query_result.fevent_name = None;
    Event_query_result.date = default_date ();
    Event_query_result.place = default_string ();
    Event_query_result.note = default_string ();
    Event_query_result.src = default_string ();
  }
and default_event_query_result_list () =
  {
    Event_query_result_list.events = [];
  }
and default_name_frequency_result () =
  {
    Name_frequency_result.key = default_string ();
    Name_frequency_result.name = default_string ();
    Name_frequency_result.count = default_protobuf_int32 ();
  }
and default_name_frequency_result_list () =
  {
    Name_frequency_result_list.result = [];
    Name_frequency_result_list.total = default_protobuf_int32 ();
  }
and default_name_frequency_params () =
  {
    Name_frequency_params.type_ = default_name_frequency_params_type ();
    Name_frequency_params.from = None;
    Name_frequency_params.to_ = None;
  }
and default_name_frequency_params_type () = `last_name
and default_error () =
  {
    Error.code = default_error_code ();
    Error.message = None;
  }
and default_error_code () = `bad_request
and default_time () =
  {
    Time.year = default_protobuf_int32 ();
    Time.month = default_protobuf_int32 ();
    Time.day = default_protobuf_int32 ();
    Time.hour = default_protobuf_int32 ();
    Time.minute = default_protobuf_int32 ();
    Time.second = default_protobuf_int32 ();
  }
and default_history_request () =
  {
    History_request.page = default_protobuf_int32 ();
    History_request.elements_per_page = default_protobuf_int32 ();
    History_request.filter_user = None;
  }
and default_history_person () =
  {
    History_person.n = default_string ();
    History_person.p = default_string ();
    History_person.oc = default_protobuf_int32 ();
    History_person.firstname = default_string ();
    History_person.lastname = default_string ();
    History_person.year1 = None;
    History_person.year2 = None;
    History_person.exists_in_base = default_bool ();
    History_person.has_history = default_bool ();
  }
and default_history_note () =
  {
    History_note.link_parameters = default_string ();
    History_note.link_txt = default_string ();
  }
and default_history_entry () =
  {
    History_entry.modification_type = default_modification_type ();
    History_entry.time = default_time ();
    History_entry.editor = default_string ();
    History_entry.person = None;
    History_entry.note = None;
  }
and default_history () =
  {
    History.entries = [];
    History.page = default_protobuf_int32 ();
    History.total_elements = default_protobuf_int32 ();
  }
and default_sex () = `male
and default_death_type () = `not_dead
and default_marriage_type () = `married
and default_divorce_type () = `not_divorced
and default_relation_parent_type () = `rpt_adoption
and default_title_type () = `title_main
and default_visibility () = `visibility_public
and default_search_type () = `starting_with
and default_pevent_name () = `epers_birth
and default_fevent_name () = `efam_marriage
and default_witness_type () = `witness
and default_calendar () = `gregorian
and default_precision () = `sure
and default_modification_type () = `person_added


let piqi = "\226\202\2304\003api\226\231\249\238\001\014api.proto.piqi\162\244\146\155\011)Geneanet.Geneweb.Api.Proto.Standard.Model\218\244\134\182\012\182\004\138\233\142\251\014\175\004\210\203\242$Q\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\nnb-persons\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$R\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011nb-families\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004sosa\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$[\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\020last-modified-person\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$V\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\015real-nb-persons\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$`\232\146\150q\012\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\022has-ignored-duplicates\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\ninfos-base\218\244\134\182\012\245\001\138\233\142\251\014\238\001\210\203\242$@\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001n\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002oc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\016reference-person\218\244\134\182\012\181\001\138\233\142\251\014\174\001\210\203\242$L\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\003key\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\001i\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\018reference-person-i\218\244\134\182\012\128\001\138\233\142\251\014z\210\203\242$Y\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\016list-ref-persons\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\022list-reference-persons\218\244\134\182\012\228\002\138\233\142\251\014\221\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$E\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006source\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$U\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\brpt-type\208\215\133\174\005\000\210\171\158\194\006\020relation-parent-type\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\015relation-parent\218\244\134\182\012\166\004\138\233\142\251\014\159\004\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ntitle-type\208\215\133\174\005\000\210\171\158\194\006\ntitle-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004name\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$D\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005title\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004fief\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\ndate-begin\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\bdate-end\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\003nth\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\005title\218\244\134\182\012\154\014\138\233\142\251\014\147\014\210\203\242$C\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004sosa\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001n\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002oc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$?\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003sex\208\215\133\174\005\000\210\171\158\194\006\003sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011public-name\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$D\232\146\150q\018\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005image\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\020\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\nbirth-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\022\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011birth-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\024\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\012baptism-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\026\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\rbaptism-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\028\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\030\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011death-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q \152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-type\208\215\133\174\005\000\210\171\158\194\006\ndeath-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\"\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011burial-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q$\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\012burial-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q&\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\rmarriage-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q(\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\014marriage-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Q\232\146\150q*\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\012divorce-type\208\215\133\174\005\000\210\171\158\194\006\012divorce-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$W\232\146\150q,\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\020visible-for-visitors\208\215\133\174\005\000\210\171\158\194\006\nvisibility\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q.\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005index\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\006spouse\218\244\134\182\012\129\017\138\233\142\251\014\250\016\210\203\242$C\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004sosa\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001n\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002oc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$?\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003sex\208\215\133\174\005\000\210\171\158\194\006\003sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011public-name\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$D\232\146\150q\018\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005image\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\020\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\nbirth-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\022\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011birth-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\024\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\012baptism-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\026\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\rbaptism-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\028\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\030\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011death-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q \152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-type\208\215\133\174\005\000\210\171\158\194\006\ndeath-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\"\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011burial-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q$\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\012burial-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$F\232\146\150q&\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007spouses\208\215\133\174\005\000\210\171\158\194\006\006spouse\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$P\232\146\150q(\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006ascend\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Q\232\146\150q*\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007descend\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$W\232\146\150q,\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\020visible-for-visitors\208\215\133\174\005\000\210\171\158\194\006\nvisibility\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q.\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\nbaseprefix\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q0\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005index\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Y\232\146\150q2\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\015is-contemporary\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$X\232\146\150q4\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\014name-is-hidden\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$\\\232\146\150q6\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\018name-is-restricted\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\006person\218\244\134\182\012\186\024\138\233\142\251\014\179\024\210\203\242$C\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004sosa\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001n\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002oc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005index\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$?\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003sex\208\215\133\174\005\000\210\171\158\194\006\003sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\018\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011public-name\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$F\232\146\150q\020\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007aliases\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\022\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\nqualifiers\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$P\232\146\150q\024\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\017firstname-aliases\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\026\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\015surname-aliases\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$D\232\146\150q\030\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005image\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q \152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nbirth-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\"\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011birth-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q$\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\tbirth-src\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q&\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\012baptism-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q(\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\rbaptism-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q*\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011baptism-src\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q,\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q.\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011death-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q0\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\tdeath-src\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q2\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-type\208\215\133\174\005\000\210\171\158\194\006\ndeath-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q4\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011burial-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q6\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\012burial-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q8\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nburial-src\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q<\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\noccupation\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q>\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\bpsources\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$D\232\146\150q@\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\006titles\208\215\133\174\005\000\210\171\158\194\006\005title\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150qB\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007related\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$P\232\146\150qD\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\brparents\208\215\133\174\005\000\210\171\158\194\006\015relation-parent\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$W\232\146\150qF\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\020visible-for-visitors\208\215\133\174\005\000\210\171\158\194\006\nvisibility\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150qH\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\007parents\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150qJ\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\bfamilies\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150qL\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\nbaseprefix\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Y\232\146\150qN\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\015is-contemporary\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$X\232\146\150qP\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\014name-is-hidden\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$\\\232\146\150qR\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\018name-is-restricted\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\011full-person\218\244\134\182\012\243\007\138\233\142\251\014\236\007\210\203\242$G\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\bfsources\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\rmarriage-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\014marriage-place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\012marriage-src\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$S\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\rmarriage-type\208\215\133\174\005\000\210\171\158\194\006\rmarriage-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Q\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\012divorce-type\208\215\133\174\005\000\210\171\158\194\006\012divorce-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\012divorce-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$P\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\twitnesses\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\018\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\020\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150q\022\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\bchildren\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\024\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005index\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\011full-family\218\244\134\182\012k\138\233\142\251\014e\210\203\242$L\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005value\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\014internal-int32\218\244\134\182\012h\138\233\142\251\014b\210\203\242$K\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\012list-persons\208\215\133\174\005\000\210\171\158\194\006\006person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\012list-persons\218\244\134\182\012m\138\233\142\251\014g\210\203\242$K\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007persons\208\215\133\174\005\000\210\171\158\194\006\011full-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\017list-full-persons\218\244\134\182\012o\138\233\142\251\014i\210\203\242$L\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\bfamilies\208\215\133\174\005\000\210\171\158\194\006\011full-family\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\018list-full-families\218\244\134\182\012\199\004\138\233\142\251\014\192\004\210\203\242$\\\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011search-type\208\215\133\174\005\000\210\171\158\194\006\011search-type\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$`\232\146\150q\b\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\tonly-sosa\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$b\232\146\150q\n\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011only-recent\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$b\232\146\150q\012\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011maiden-name\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\218\164\238\191\004\rsearch-params\218\244\134\182\012\173\001\138\233\142\251\014\166\001\210\203\242$O\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$B\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003img\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\005image\218\244\134\182\012e\138\233\142\251\014_\210\203\242$I\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\011list-images\208\215\133\174\005\000\210\171\158\194\006\005image\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\011list-images\218\244\134\182\012\176\001\138\233\142\251\014\169\001\210\203\242$O\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$B\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003img\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\bpers-img\218\244\134\182\012l\138\233\142\251\014f\210\203\242$N\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\rlist-pers-img\208\215\133\174\005\000\210\171\158\194\006\bpers-img\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\rlist-pers-img\218\244\134\182\012b\138\233\142\251\014\\\210\203\242$L\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005index\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\005index\218\244\134\182\012`\138\233\142\251\014Z\210\203\242$B\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003img\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\rimage-address\218\244\134\182\012\242\003\138\233\142\251\014\235\003\210\203\242$O\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Q\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nnb-gen-asc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$R\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011nb-gen-desc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$d\232\146\150q\b\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\rspouse-ascend\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$b\232\146\150q\n\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011only-recent\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\218\164\238\191\004\020close-persons-params\218\244\134\182\012o\138\233\142\251\014i\210\203\242$L\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005month\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\018anniversary-params\218\244\134\182\012\195\001\138\233\142\251\014\188\001\210\203\242$Q\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\ngeneration\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\012graph-params\218\244\134\182\012\199\001\138\233\142\251\014\192\001\210\203\242$P\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007person1\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$P\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007person2\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\016graph-rel-params\218\244\134\182\012\197\001\138\233\142\251\014\190\001\210\203\242$P\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007person1\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$P\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007person2\208\215\133\174\005\000\210\171\158\194\006\016reference-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\014cpl-rel-params\218\244\134\182\012\169\001\138\233\142\251\014\162\001\210\203\242$I\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002id\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$E\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\006person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\004node\218\244\134\182\012\179\001\138\233\142\251\014\172\001\210\203\242$I\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002id\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\011full-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\tfull-node\218\244\134\182\012\185\001\138\233\142\251\014\178\001\210\203\242$P\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfrom-node\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007to-node\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int64\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\004edge\218\244\134\182\012\160\001\138\233\142\251\014\153\001\210\203\242$B\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\005nodes\208\215\133\174\005\000\210\171\158\194\006\004node\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$B\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\005edges\208\215\133\174\005\000\210\171\158\194\006\004edge\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\005graph\218\244\134\182\012\251\001\138\233\142\251\014\244\001\210\203\242$G\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\005nodes\208\215\133\174\005\000\210\171\158\194\006\tfull-node\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$B\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\005edges\208\215\133\174\005\000\210\171\158\194\006\004edge\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\bfamilies\208\215\133\174\005\000\210\171\158\194\006\011full-family\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\nfull-graph\218\244\134\182\012\192\001\138\233\142\251\014\185\001\210\203\242$K\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004from\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005limit\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\018all-persons-params\218\244\134\182\012\193\001\138\233\142\251\014\186\001\210\203\242$K\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004from\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005limit\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\019all-families-params\218\244\134\182\012\184\001\138\233\142\251\014\177\001\210\203\242$J\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006pevent\208\215\133\174\005\000\210\171\158\194\006\011pevent-name\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006fevent\208\215\133\174\005\000\210\171\158\194\006\011fevent-name\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\rwarning-event\218\244\134\182\012\240\004\138\233\142\251\014\233\004\210\203\242$@\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001n\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002oc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nbirth-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\018\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\ndeath-date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\020\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004iper\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\014warning-person\218\244\134\182\012u\138\233\142\251\014o\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\023warning-already-defined\218\244\134\182\012r\138\233\142\251\014l\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\020warning-own-ancestor\218\244\134\182\012\127\138\233\142\251\014y\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004!warning-bad-sex-of-married-person\218\244\134\182\012w\138\233\142\251\014q\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\025warning-birth-after-death\218\244\134\182\012t\138\233\142\251\014n\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\022warning-incoherent-sex\218\244\134\182\012\210\001\138\233\142\251\014\203\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004!warning-changed-order-of-children\218\244\134\182\012\128\001\138\233\142\251\014z\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\"warning-changed-order-of-marriages\218\244\134\182\012\206\001\138\233\142\251\014\199\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\029warning-children-not-in-order\218\244\134\182\012\209\001\138\233\142\251\014\202\001\210\203\242$J\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003son\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004#warning-dead-too-early-to-be-father\218\244\134\182\012\211\001\138\233\142\251\014\204\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\bancestor\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004 warning-incoherent-ancestor-date\218\244\134\182\012\127\138\233\142\251\014y\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004!warning-marriage-date-after-death\218\244\134\182\012\128\001\138\233\142\251\014z\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\"warning-marriage-date-before-birth\218\244\134\182\012\214\001\138\233\142\251\014\207\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005child\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004&warning-mother-dead-before-child-birth\218\244\134\182\012\207\001\138\233\142\251\014\200\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006parent\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005child\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\031warning-parent-born-after-child\218\244\134\182\012\144\002\138\233\142\251\014\137\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006parent\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005child\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\024warning-parent-too-young\218\244\134\182\012\247\002\138\233\142\251\014\240\002\210\203\242$N\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007father1\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007mother1\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007father2\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007mother2\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\030warning-possible-duplicate-fam\218\244\134\182\012\216\003\138\233\142\251\014\209\003\210\203\242$N\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007father1\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007mother1\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007father2\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\007mother2\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Q\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\nhomonymous\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004)warning-possible-duplicate-fam-homonymous\218\244\134\182\012\192\001\138\233\142\251\014\185\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005title\208\215\133\174\005\000\210\171\158\194\006\005title\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\025warning-title-dates-error\218\244\134\182\012s\138\233\142\251\014m\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\021warning-undefined-sex\218\244\134\182\012\193\001\138\233\142\251\014\186\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\026warning-young-for-marriage\218\244\134\182\012\191\001\138\233\142\251\014\184\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\024warning-old-for-marriage\218\244\134\182\012\142\002\138\233\142\251\014\135\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006parent\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005child\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\022warning-parent-too-old\218\244\134\182\012\235\002\138\233\142\251\014\228\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006child1\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006child2\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\022warning-close-children\218\244\134\182\012\237\002\138\233\142\251\014\230\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006child1\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006child2\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\024warning-distant-children\218\244\134\182\012\152\002\138\233\142\251\014\145\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006father\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006mother\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\031warning-big-age-between-spouses\218\244\134\182\012\183\001\138\233\142\251\014\176\001\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\016warning-dead-old\218\244\134\182\012\161\002\138\233\142\251\014\154\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005event\208\215\133\174\005\000\210\171\158\194\006\rwarning-event\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\006origin\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004 warning-witness-date-after-death\218\244\134\182\012\162\002\138\233\142\251\014\155\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005event\208\215\133\174\005\000\210\171\158\194\006\rwarning-event\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\006origin\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004!warning-witness-date-before-birth\218\244\134\182\012\146\002\138\233\142\251\014\139\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014warning-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007pevents\208\215\133\174\005\000\210\171\158\194\006\011pevent-name\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007fevents\208\215\133\174\005\000\210\171\158\194\006\011fevent-name\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\019warning-event-order\218\244\134\182\012\250\026\138\233\142\251\014\243\026\210\203\242$g\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\023warning-already-defined\208\215\133\174\005\000\210\171\158\194\006\023warning-already-defined\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$a\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\020warning-own-ancestor\208\215\133\174\005\000\210\171\158\194\006\020warning-own-ancestor\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242${\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004!warning-bad-sex-of-married-person\208\215\133\174\005\000\210\171\158\194\006!warning-bad-sex-of-married-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$k\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\025warning-birth-after-death\208\215\133\174\005\000\210\171\158\194\006\025warning-birth-after-death\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$e\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\022warning-incoherent-sex\208\215\133\174\005\000\210\171\158\194\006\022warning-incoherent-sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242${\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004!warning-changed-order-of-children\208\215\133\174\005\000\210\171\158\194\006!warning-changed-order-of-children\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$s\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\029warning-children-not-in-order\208\215\133\174\005\000\210\171\158\194\006\029warning-children-not-in-order\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$\127\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004#warning-dead-too-early-to-be-father\208\215\133\174\005\000\210\171\158\194\006#warning-dead-too-early-to-be-father\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$y\232\146\150q\018\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004 warning-incoherent-ancestor-date\208\215\133\174\005\000\210\171\158\194\006 warning-incoherent-ancestor-date\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242${\232\146\150q\020\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004!warning-marriage-date-after-death\208\215\133\174\005\000\210\171\158\194\006!warning-marriage-date-after-death\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$}\232\146\150q\022\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\"warning-marriage-date-before-birth\208\215\133\174\005\000\210\171\158\194\006\"warning-marriage-date-before-birth\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$\133\001\232\146\150q\024\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004&warning-mother-dead-before-child-birth\208\215\133\174\005\000\210\171\158\194\006&warning-mother-dead-before-child-birth\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$w\232\146\150q\026\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\031warning-parent-born-after-child\208\215\133\174\005\000\210\171\158\194\006\031warning-parent-born-after-child\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$i\232\146\150q\028\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\024warning-parent-too-young\208\215\133\174\005\000\210\171\158\194\006\024warning-parent-too-young\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$u\232\146\150q4\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\030warning-possible-duplicate-fam\208\215\133\174\005\000\210\171\158\194\006\030warning-possible-duplicate-fam\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$k\232\146\150q\030\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\025warning-title-dates-error\208\215\133\174\005\000\210\171\158\194\006\025warning-title-dates-error\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$c\232\146\150q \152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\021warning-undefined-sex\208\215\133\174\005\000\210\171\158\194\006\021warning-undefined-sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$m\232\146\150q\"\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\026warning-young-for-marriage\208\215\133\174\005\000\210\171\158\194\006\026warning-young-for-marriage\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$e\232\146\150q$\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\022warning-close-children\208\215\133\174\005\000\210\171\158\194\006\022warning-close-children\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$e\232\146\150q&\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\022warning-parent-too-old\208\215\133\174\005\000\210\171\158\194\006\022warning-parent-too-old\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$}\232\146\150q(\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\"warning-changed-order-of-marriages\208\215\133\174\005\000\210\171\158\194\006\"warning-changed-order-of-marriages\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$w\232\146\150q*\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\031warning-big-age-between-spouses\208\215\133\174\005\000\210\171\158\194\006\031warning-big-age-between-spouses\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$Y\232\146\150q,\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\016warning-dead-old\208\215\133\174\005\000\210\171\158\194\006\016warning-dead-old\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$y\232\146\150q0\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004 warning-witness-date-after-death\208\215\133\174\005\000\210\171\158\194\006 warning-witness-date-after-death\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242${\232\146\150q2\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004!warning-witness-date-before-birth\208\215\133\174\005\000\210\171\158\194\006!warning-witness-date-before-birth\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$i\232\146\150q6\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\024warning-old-for-marriage\208\215\133\174\005\000\210\171\158\194\006\024warning-old-for-marriage\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$i\232\146\150q8\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\024warning-distant-children\208\215\133\174\005\000\210\171\158\194\006\024warning-distant-children\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$_\232\146\150q:\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\019warning-event-order\208\215\133\174\005\000\210\171\158\194\006\019warning-event-order\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$\139\001\232\146\150q<\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004)warning-possible-duplicate-fam-homonymous\208\215\133\174\005\000\210\171\158\194\006)warning-possible-duplicate-fam-homonymous\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\rbase-warnings\218\244\134\182\012\136\002\138\233\142\251\014\129\002\210\203\242$J\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003day\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005month\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004year\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\011filter-date\218\244\134\182\012\168\002\138\233\142\251\014\161\002\210\203\242$N\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\ndate-begin\208\215\133\174\005\000\210\171\158\194\006\011filter-date\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\bdate-end\208\215\133\174\005\000\210\171\158\194\006\011filter-date\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$a\232\146\150q\006\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nonly-exact\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\218\164\238\191\004\017filter-date-range\218\244\134\182\012\188\004\138\233\142\251\014\181\004\210\203\242$`\232\146\150q\002\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\tonly-sosa\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$b\232\146\150q\004\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011only-recent\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$?\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\003sex\208\215\133\174\005\000\210\171\158\194\006\003sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$a\232\146\150q\b\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nnb-results\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\138\140\251\240\r\007\218\148\211\024\002\b\000\136\158\147\199\014\000\210\203\242$T\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\ndate-birth\208\215\133\174\005\000\210\171\158\194\006\017filter-date-range\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$T\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\ndate-death\208\215\133\174\005\000\210\171\158\194\006\017filter-date-range\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\007filters\218\244\134\182\012\158\002\138\233\142\251\014\151\002\210\203\242$P\232\146\150q\002\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006status\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$S\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\rbase-warnings\208\215\133\174\005\000\210\171\158\194\006\rbase-warnings\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005index\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\019modification-status\218\244\134\182\012\135\004\138\233\142\251\014\128\004\210\203\242$G\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$?\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003sex\208\215\133\174\005\000\210\171\158\194\006\003sex\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$U\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\014birth-date-day\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$W\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\016birth-date-month\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$V\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\015birth-date-year\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\012person-start\218\244\134\182\012\144\002\138\233\142\251\014\137\002\210\203\242$E\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006wizard\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$N\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\007max-res\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005range\208\215\133\174\005\000\210\171\158\194\006\017filter-date-range\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\018last-modifications\218\244\134\182\012_\138\233\142\251\014Y\210\203\242$C\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004user\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\011last-visits\218\244\134\182\012\200\002\138\233\142\251\014\193\002\210\203\242$J\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003day\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005month\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$B\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004year\208\215\133\174\005\000\210\171\158\194\006\005int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005delta\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\003dmy\218\244\134\182\012\246\002\138\233\142\251\014\239\002\210\203\242$D\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\003cal\208\215\133\174\005\000\210\171\158\194\006\bcalendar\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$F\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004prec\208\215\133\174\005\000\210\171\158\194\006\tprecision\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$?\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\003dmy\208\215\133\174\005\000\210\171\158\194\006\003dmy\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004dmy2\208\215\133\174\005\000\210\171\158\194\006\003dmy\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004text\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\004date\218\244\134\182\012\189\003\138\233\142\251\014\182\003\210\203\242$a\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\020close-persons-params\208\215\133\174\005\000\210\171\158\194\006\020close-persons-params\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\nstart-date\208\215\133\174\005\000\210\171\158\194\006\004date\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$F\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\tstop-date\208\215\133\174\005\000\210\171\158\194\006\004date\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007pevents\208\215\133\174\005\000\210\171\158\194\006\011pevent-name\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007fevents\208\215\133\174\005\000\210\171\158\194\006\011fevent-name\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\019events-query-params\218\244\134\182\012\240\004\138\233\142\251\014\233\004\210\203\242$@\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$A\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\002sp\208\215\133\174\005\000\210\171\158\194\006\006person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011pevent-name\208\215\133\174\005\000\210\171\158\194\006\011pevent-name\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$O\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011fevent-name\208\215\133\174\005\000\210\171\158\194\006\011fevent-name\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$A\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004date\208\215\133\174\005\000\210\171\158\194\006\004date\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$D\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005place\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004note\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$B\232\146\150q\016\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003src\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\018event-query-result\218\244\134\182\012y\138\233\142\251\014s\210\203\242$Q\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\006events\208\215\133\174\005\000\210\171\158\194\006\018event-query-result\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\023event-query-result-list\218\244\134\182\012\130\002\138\233\142\251\014\251\001\210\203\242$B\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003key\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$C\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004name\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005count\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\021name-frequency-result\218\244\134\182\012\209\001\138\233\142\251\014\202\001\210\203\242$T\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\006result\208\215\133\174\005\000\210\171\158\194\006\021name-frequency-result\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005total\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\026name-frequency-result-list\218\244\134\182\012\156\002\138\233\142\251\014\149\002\210\203\242$W\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004type\208\215\133\174\005\000\210\171\158\194\006\026name-frequency-params-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004from\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\002to\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\021name-frequency-params\218\244\134\182\012\179\001\138\176\205\197\001\172\001\218\164\238\191\004\026name-frequency-params-type\170\183\218\222\005?\232\146\150q\002\152\247\223\136\002\000\234\188\204\215\002\031name_frequency_params_last_name\218\164\238\191\004\tlast-name\170\183\218\222\005A\232\146\150q\004\152\247\223\136\002\000\234\188\204\215\002 name_frequency_params_first_name\218\164\238\191\004\nfirst-name\218\244\134\182\012\171\001\138\233\142\251\014\164\001\210\203\242$H\232\146\150q\204\015\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004code\208\215\133\174\005\000\210\171\158\194\006\nerror-code\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\206\015\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\007message\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\005error\218\244\134\182\012\173\002\138\176\205\197\001\166\002\218\164\238\191\004\nerror-code\170\183\218\222\0054\232\146\150q\160\006\152\247\223\136\002\000\234\188\204\215\002\017error_bad_request\218\164\238\191\004\011bad-request\170\183\218\222\0056\232\146\150q\162\006\152\247\223\136\002\000\234\188\204\215\002\018error_unauthorized\218\164\238\191\004\012unauthorized\170\183\218\222\0050\232\146\150q\166\006\152\247\223\136\002\000\234\188\204\215\002\015error_forbidden\218\164\238\191\004\tforbidden\170\183\218\222\0050\232\146\150q\168\006\152\247\223\136\002\000\234\188\204\215\002\015error_not_found\218\164\238\191\004\tnot-found\170\183\218\222\005.\232\146\150q\178\006\152\247\223\136\002\000\234\188\204\215\002\014error_conflict\218\164\238\191\004\bconflict\218\244\134\182\012\245\003\138\233\142\251\014\238\003\210\203\242$K\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004year\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\005month\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\003day\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004hour\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006minute\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006second\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\004time\218\244\134\182\012\152\002\138\233\142\251\014\145\002\210\203\242$K\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004page\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$X\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\017elements-per-page\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$J\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\011filter-user\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\015history-request\218\244\134\182\012\229\005\138\233\142\251\014\222\005\210\203\242$@\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001n\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$@\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\001p\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\002oc\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$H\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\tfirstname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blastname\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\012\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005year1\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$L\232\146\150q\014\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\005year2\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$X\232\146\150q\016\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\014exists-in-base\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$U\232\146\150q\018\152\247\223\136\002\000\170\131\252\172\003\007\218\148\211\024\002\b\001\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\011has-history\208\215\133\174\005\000\210\171\158\194\006\004bool\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\014history-person\218\244\134\182\012\184\001\138\233\142\251\014\177\001\210\203\242$N\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\015link-parameters\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$G\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\blink-txt\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\012history-note\218\244\134\182\012\170\003\138\233\142\251\014\163\003\210\203\242$[\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\017modification-type\208\215\133\174\005\000\210\171\158\194\006\017modification-type\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$A\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004time\208\215\133\174\005\000\210\171\158\194\006\004time\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$E\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\006editor\208\215\133\174\005\000\210\171\158\194\006\006string\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$M\232\146\150q\b\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\006person\208\215\133\174\005\000\210\171\158\194\006\014history-person\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$I\232\146\150q\n\152\247\223\136\002\000\152\182\154\152\004\160\223\186\243\001\232\243\204\157\004\000\218\164\238\191\004\004note\208\215\133\174\005\000\210\171\158\194\006\012history-note\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\rhistory-entry\218\244\134\182\012\144\002\138\233\142\251\014\137\002\210\203\242$M\232\146\150q\002\152\247\223\136\002\000\152\182\154\152\004\250\248\214\130\001\232\243\204\157\004\000\218\164\238\191\004\007entries\208\215\133\174\005\000\210\171\158\194\006\rhistory-entry\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$K\232\146\150q\004\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\004page\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\210\203\242$U\232\146\150q\006\152\247\223\136\002\000\152\182\154\152\004\223\162\138\147\001\232\243\204\157\004\000\218\164\238\191\004\014total-elements\208\215\133\174\005\000\210\171\158\194\006\014protobuf-int32\192\139\160\247\t\000\136\158\147\199\014\000\218\164\238\191\004\007history\218\244\134\182\012e\138\176\205\197\001_\218\164\238\191\004\003sex\170\183\218\222\005\021\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\004male\170\183\218\222\005\023\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\006female\170\183\218\222\005\024\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\007unknown\218\244\134\182\012\233\001\138\176\205\197\001\226\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\025\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\021\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\004dead\170\183\218\222\005\027\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\ndead-young\170\183\218\222\005$\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\"\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\031\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\014of-course-dead\218\244\134\182\012\165\003\138\176\205\197\001\158\003\218\164\238\191\004\rmarriage-type\170\183\218\222\005\024\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\007married\170\183\218\222\005\028\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\011not-married\170\183\218\222\005\024\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\007engaged\170\183\218\222\005+\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\026no-sexes-check-not-married\170\183\218\222\005\027\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\nno-mention\170\183\218\222\005'\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\022no-sexes-check-married\170\183\218\222\005\030\232\146\150q\012\152\247\223\136\002\000\218\164\238\191\004\rmarriage-bann\170\183\218\222\005\"\232\146\150q\014\152\247\223\136\002\000\218\164\238\191\004\017marriage-contract\170\183\218\222\005!\232\146\150q\016\152\247\223\136\002\000\218\164\238\191\004\016marriage-license\170\183\218\222\005\021\232\146\150q\018\152\247\223\136\002\000\218\164\238\191\004\004pacs\170\183\218\222\005\026\232\146\150q\020\152\247\223\136\002\000\218\164\238\191\004\tresidence\218\244\134\182\012z\138\176\205\197\001t\218\164\238\191\004\012divorce-type\170\183\218\222\005\029\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\012not-divorced\170\183\218\222\005\025\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\bdivorced\170\183\218\222\005\026\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\tseparated\218\244\134\182\012\226\001\138\176\205\197\001\219\001\218\164\238\191\004\020relation-parent-type\170\183\218\222\005\029\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\012rpt-adoption\170\183\218\222\005 \232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\015rpt-recognition\170\183\218\222\005%\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\020rpt-candidate-parent\170\183\218\222\005\031\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\014rpt-god-parent\170\183\218\222\005\"\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\017rpt-foster-parent\218\244\134\182\012y\138\176\205\197\001s\218\164\238\191\004\ntitle-type\170\183\218\222\005\027\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\ntitle-main\170\183\218\222\005\027\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\ntitle-name\170\183\218\222\005\027\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\ntitle-none\218\244\134\182\012\149\001\138\176\205\197\001\142\001\218\164\238\191\004\nvisibility\170\183\218\222\005\"\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\017visibility-public\170\183\218\222\005'\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\022visibility-semi-public\170\183\218\222\005#\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\018visibility-private\218\244\134\182\012\140\001\138\176\205\197\001\133\001\218\164\238\191\004\011search-type\170\183\218\222\005\030\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\rstarting-with\170\183\218\222\005\030\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\rapproximative\170\183\218\222\005&\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\021lastname-or-firstname\218\244\134\182\012\242\015\138\176\205\197\001\235\015\218\164\238\191\004\011pevent-name\170\183\218\222\005\028\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\011epers-birth\170\183\218\222\005\030\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\repers-baptism\170\183\218\222\005\028\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\011epers-death\170\183\218\222\005\029\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\012epers-burial\170\183\218\222\005 \232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\015epers-cremation\170\183\218\222\005%\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\020epers-accomplishment\170\183\218\222\005\"\232\146\150q\012\152\247\223\136\002\000\218\164\238\191\004\017epers-acquisition\170\183\218\222\005\031\232\146\150q\014\152\247\223\136\002\000\218\164\238\191\004\014epers-adhesion\170\183\218\222\005!\232\146\150q\016\152\247\223\136\002\000\218\164\238\191\004\016epers-baptismlds\170\183\218\222\005!\232\146\150q\018\152\247\223\136\002\000\218\164\238\191\004\016epers-barmitzvah\170\183\218\222\005!\232\146\150q\020\152\247\223\136\002\000\218\164\238\191\004\016epers-batmitzvah\170\183\218\222\005\"\232\146\150q\022\152\247\223\136\002\000\218\164\238\191\004\017epers-benediction\170\183\218\222\005!\232\146\150q\024\152\247\223\136\002\000\218\164\238\191\004\016epers-changename\170\183\218\222\005#\232\146\150q\026\152\247\223\136\002\000\218\164\238\191\004\018epers-circumcision\170\183\218\222\005#\232\146\150q\028\152\247\223\136\002\000\218\164\238\191\004\018epers-confirmation\170\183\218\222\005&\232\146\150q\030\152\247\223\136\002\000\218\164\238\191\004\021epers-confirmationlds\170\183\218\222\005!\232\146\150q \152\247\223\136\002\000\218\164\238\191\004\016epers-decoration\170\183\218\222\005.\232\146\150q\"\152\247\223\136\002\000\218\164\238\191\004\029epers-demobilisationmilitaire\170\183\218\222\005\030\232\146\150q$\152\247\223\136\002\000\218\164\238\191\004\repers-diploma\170\183\218\222\005\"\232\146\150q&\152\247\223\136\002\000\218\164\238\191\004\017epers-distinction\170\183\218\222\005\031\232\146\150q(\152\247\223\136\002\000\218\164\238\191\004\014epers-dotation\170\183\218\222\005\"\232\146\150q*\152\247\223\136\002\000\218\164\238\191\004\017epers-dotationlds\170\183\218\222\005 \232\146\150q,\152\247\223\136\002\000\218\164\238\191\004\015epers-education\170\183\218\222\005\031\232\146\150q.\152\247\223\136\002\000\218\164\238\191\004\014epers-election\170\183\218\222\005!\232\146\150q0\152\247\223\136\002\000\218\164\238\191\004\016epers-emigration\170\183\218\222\005&\232\146\150q2\152\247\223\136\002\000\218\164\238\191\004\021epers-excommunication\170\183\218\222\005$\232\146\150q4\152\247\223\136\002\000\218\164\238\191\004\019epers-familylinklds\170\183\218\222\005%\232\146\150q6\152\247\223\136\002\000\218\164\238\191\004\020epers-firstcommunion\170\183\218\222\005\030\232\146\150q8\152\247\223\136\002\000\218\164\238\191\004\repers-funeral\170\183\218\222\005\031\232\146\150q:\152\247\223\136\002\000\218\164\238\191\004\014epers-graduate\170\183\218\222\005&\232\146\150q<\152\247\223\136\002\000\218\164\238\191\004\021epers-hospitalisation\170\183\218\222\005\030\232\146\150q>\152\247\223\136\002\000\218\164\238\191\004\repers-illness\170\183\218\222\005\"\232\146\150q@\152\247\223\136\002\000\218\164\238\191\004\017epers-immigration\170\183\218\222\005%\232\146\150qB\152\247\223\136\002\000\218\164\238\191\004\020epers-listepassenger\170\183\218\222\005*\232\146\150qD\152\247\223\136\002\000\218\164\238\191\004\025epers-militarydistinction\170\183\218\222\005(\232\146\150qF\152\247\223\136\002\000\218\164\238\191\004\023epers-militarypromotion\170\183\218\222\005&\232\146\150qH\152\247\223\136\002\000\218\164\238\191\004\021epers-militaryservice\170\183\218\222\005,\232\146\150qJ\152\247\223\136\002\000\218\164\238\191\004\027epers-mobilisationmilitaire\170\183\218\222\005%\232\146\150qL\152\247\223\136\002\000\218\164\238\191\004\020epers-naturalisation\170\183\218\222\005!\232\146\150qN\152\247\223\136\002\000\218\164\238\191\004\016epers-occupation\170\183\218\222\005!\232\146\150qP\152\247\223\136\002\000\218\164\238\191\004\016epers-ordination\170\183\218\222\005\031\232\146\150qR\152\247\223\136\002\000\218\164\238\191\004\014epers-property\170\183\218\222\005\"\232\146\150qT\152\247\223\136\002\000\218\164\238\191\004\017epers-recensement\170\183\218\222\005 \232\146\150qV\152\247\223\136\002\000\218\164\238\191\004\015epers-residence\170\183\218\222\005\030\232\146\150qX\152\247\223\136\002\000\218\164\238\191\004\repers-retired\170\183\218\222\005'\232\146\150qZ\152\247\223\136\002\000\218\164\238\191\004\022epers-scellentchildlds\170\183\218\222\005(\232\146\150q\\\152\247\223\136\002\000\218\164\238\191\004\023epers-scellentparentlds\170\183\218\222\005(\232\146\150q^\152\247\223\136\002\000\218\164\238\191\004\023epers-scellentspouselds\170\183\218\222\005 \232\146\150q`\152\247\223\136\002\000\218\164\238\191\004\015epers-ventebien\170\183\218\222\005\027\232\146\150qb\152\247\223\136\002\000\218\164\238\191\004\nepers-will\218\244\134\182\012\224\003\138\176\205\197\001\217\003\218\164\238\191\004\011fevent-name\170\183\218\222\005\030\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\refam-marriage\170\183\218\222\005!\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\016efam-no-marriage\170\183\218\222\005 \232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\015efam-no-mention\170\183\218\222\005\028\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\011efam-engage\170\183\218\222\005\029\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\012efam-divorce\170\183\218\222\005\031\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\014efam-separated\170\183\218\222\005 \232\146\150q\012\152\247\223\136\002\000\218\164\238\191\004\015efam-annulation\170\183\218\222\005#\232\146\150q\014\152\247\223\136\002\000\218\164\238\191\004\018efam-marriage-bann\170\183\218\222\005'\232\146\150q\016\152\247\223\136\002\000\218\164\238\191\004\022efam-marriage-contract\170\183\218\222\005&\232\146\150q\018\152\247\223\136\002\000\218\164\238\191\004\021efam-marriage-license\170\183\218\222\005\026\232\146\150q\020\152\247\223\136\002\000\218\164\238\191\004\tefam-pacs\170\183\218\222\005\031\232\146\150q\022\152\247\223\136\002\000\218\164\238\191\004\014efam-residence\218\244\134\182\012\213\002\138\176\205\197\001\206\002\218\164\238\191\004\012witness-type\170\183\218\222\005\024\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\007witness\170\183\218\222\005\"\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\017witness-godparent\170\183\218\222\005%\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\020witness-civilofficer\170\183\218\222\005)\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\024witness-religiousofficer\170\183\218\222\005\"\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\017witness-informant\170\183\218\222\005\"\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\017witness-attending\170\183\218\222\005\"\232\146\150q\012\152\247\223\136\002\000\218\164\238\191\004\017witness-mentioned\170\183\218\222\005\030\232\146\150q\014\152\247\223\136\002\000\218\164\238\191\004\rwitness-other\218\244\134\182\012\140\001\138\176\205\197\001\133\001\218\164\238\191\004\bcalendar\170\183\218\222\005\026\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\tgregorian\170\183\218\222\005\023\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\006julian\170\183\218\222\005\023\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\006french\170\183\218\222\005\023\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\006hebrew\218\244\134\182\012\221\001\138\176\205\197\001\214\001\218\164\238\191\004\tprecision\170\183\218\222\005\021\232\146\150q\000\152\247\223\136\002\000\218\164\238\191\004\004sure\170\183\218\222\005\022\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\005about\170\183\218\222\005\022\232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\005maybe\170\183\218\222\005\023\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\006before\170\183\218\222\005\022\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\005after\170\183\218\222\005\023\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\006oryear\170\183\218\222\005\024\232\146\150q\012\152\247\223\136\002\000\218\164\238\191\004\007yearint\218\244\134\182\012\156\005\138\176\205\197\001\149\005\218\164\238\191\004\017modification-type\170\183\218\222\005\029\232\146\150q\002\152\247\223\136\002\000\218\164\238\191\004\012person-added\170\183\218\222\005 \232\146\150q\004\152\247\223\136\002\000\218\164\238\191\004\015person-modified\170\183\218\222\005\031\232\146\150q\006\152\247\223\136\002\000\218\164\238\191\004\014person-deleted\170\183\218\222\005\030\232\146\150q\b\152\247\223\136\002\000\218\164\238\191\004\rperson-merged\170\183\218\222\005\031\232\146\150q\n\152\247\223\136\002\000\218\164\238\191\004\014image-received\170\183\218\222\005\030\232\146\150q\012\152\247\223\136\002\000\218\164\238\191\004\rimage-deleted\170\183\218\222\005\029\232\146\150q\014\152\247\223\136\002\000\218\164\238\191\004\012family-added\170\183\218\222\005 \232\146\150q\016\152\247\223\136\002\000\218\164\238\191\004\015family-modified\170\183\218\222\005\031\232\146\150q\018\152\247\223\136\002\000\218\164\238\191\004\014family-deleted\170\183\218\222\005 \232\146\150q\020\152\247\223\136\002\000\218\164\238\191\004\015family-inverted\170\183\218\222\005\030\232\146\150q\022\152\247\223\136\002\000\218\164\238\191\004\rfamily-merged\170\183\218\222\005'\232\146\150q\024\152\247\223\136\002\000\218\164\238\191\004\022changed-children-names\170\183\218\222\005\030\232\146\150q\026\152\247\223\136\002\000\218\164\238\191\004\rparents-added\170\183\218\222\005\031\232\146\150q\028\152\247\223\136\002\000\218\164\238\191\004\014notes-modified\170\183\218\222\005\031\232\146\150q\030\152\247\223\136\002\000\218\164\238\191\004\014place-modified\170\183\218\222\005 \232\146\150q \152\247\223\136\002\000\218\164\238\191\004\015source-modified\170\183\218\222\005$\232\146\150q\"\152\247\223\136\002\000\218\164\238\191\004\019occupation-modified"
include Api_piqi
