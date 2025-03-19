module Api = struct
  (*module PiqiToProtoc = struct
    let reference_person (Api_piqi.Reference_person.{n;p;oc}) = Api_protoc.({n;p;oc})
(*
    let transl_search_type = function
      | `starting_with -> Api_protoc.Starting_with
      | `approximative -> Approximative
      | `lastname_or_firstname -> Lastname_or_firstname
    
    let search_params (Api_piqi.Search_params.{search_type; lastname; firstname; only_sosa; only_recent; maiden_name}) =
      let search_type = Some (transl_search_type search_type) in
      Api_protoc.({search_type; lastname; firstname; only_sosa; only_recent; maiden_name})*)
    let index _ = assert false
    let list_reference_person _ = assert false
    let close_persons_params _ = assert false
    let person_start _ = assert false
    let error _ = assert false
    let list_persons _ = assert false
    let list_full_persons _ = assert false
    end*)
  module ProtocToPiqi = struct
    open Api_protoc

    let required = function
      | Some v -> v
      | None -> failwith "required"
    
    let reference_person ({n;p;oc} : reference_person) = Api_piqi.Reference_person.{n;p;oc}

    let reference_person_i = function
      | Key key -> Api_piqi.Reference_person_i.{key = Some (reference_person key); i = None}
      | I i -> Api_piqi.Reference_person_i.{key = None; i = Some i}

    let graph_params ({generation; person} : graph_params) =
      let person = reference_person (required person) in
      Api_piqi.Graph_params.{generation; person}

    let graph_rel_params ({person1; person2} : graph_rel_params) =
      let person1 = reference_person (required person1) in
      let person2 = reference_person (required person2) in
      Api_piqi.Graph_rel_params.{person1; person2}
      
    let cpl_rel_params ({person1; person2} : cpl_rel_params) =
      let person1 = reference_person (required person1) in
      let person2 = reference_person (required person2) in
      Api_piqi.Cpl_rel_params.{person1; person2}

    let filter_date ({day; month; year} : filter_date) =
      Api_piqi.Filter_date.{day; month; year} 

    let filter_date_range ({date_begin; date_end; only_exact} : filter_date_range) =
      let date_begin = filter_date (required date_begin) in
      let date_end = filter_date (required date_end) in
      let only_exact = Option.value ~default:false only_exact in
      Api_piqi.Filter_date_range.{date_begin; date_end; only_exact}
    
    let last_modifications ({wizard; max_res; range} : last_modifications) =
      let range = Option.map filter_date_range range in
      Api_piqi.Last_modifications.{wizard; max_res; range}
    
    let last_visits ({user} : last_visits) = Api_piqi.Last_visits.{user}

    let all_persons_params ({from; limit} : all_persons_params) =
      Api_piqi.All_persons_params.{from; limit}

    let all_families_params ({from; limit} : all_families_params) =
      Api_piqi.All_families_params.{from; limit}

    let pers_img ({person; img} : pers_img) =
      let person = reference_person (required person) in
      Api_piqi.Pers_img.{person; img}

    let list_pers_img ({list_pers_img} : list_pers_img) =
      let list_pers_img = List.map pers_img list_pers_img in
      Api_piqi.List_pers_img.{list_pers_img}
      

    let translate_search_type = function
      | Starting_with -> `starting_with
      | Approximative -> `approximative
      | Lastname_or_firstname  -> `lastname_or_firstname

    let search_params ({search_type; lastname; firstname; only_sosa; only_recent; maiden_name} : search_params) =
      let search_type = translate_search_type (Option.value ~default:Starting_with search_type) in
      let only_sosa = Option.value ~default:false only_sosa in
      let only_recent = Option.value ~default:false only_recent in
      let maiden_name = Option.value ~default:false maiden_name in
      Api_piqi.Search_params.{search_type; lastname; firstname; only_sosa; only_recent; maiden_name}
    let history_request ({page; elements_per_page; filter_user} : history_request) =
      Api_piqi.History_request.{page; elements_per_page; filter_user}

    let index ({index} : index) = Api_piqi.Index.{index}

    let list_reference_person ({list_ref_persons} : list_reference_persons) =
      let list_ref_persons = List.map reference_person list_ref_persons in
      Api_piqi.List_reference_persons.{list_ref_persons}

    let close_persons_params ({person; nb_gen_asc; nb_gen_desc; spouse_ascend; only_recent} : close_persons_params) =
      let person = reference_person (required person) in
      let spouse_ascend = Option.value ~default:false spouse_ascend in
      let only_recent = Option.value ~default:false only_recent in
      Api_piqi.Close_persons_params.{person; nb_gen_asc; nb_gen_desc; spouse_ascend; only_recent}

    let translate_sex = function
      | Male -> `male
      | Female -> `female
      | Unknown -> `unknown
    
    let person_start ({lastname; firstname; sex; birth_date_day; birth_date_month; birth_date_year}: person_start) =
      let sex = translate_sex sex in
      Api_piqi.Person_start.{lastname; firstname; sex; birth_date_day; birth_date_month; birth_date_year}

    let calendar = function
      | Gregorian -> `gregorian
      | Julian -> `julian
      | French -> `french
      | Hebrew -> `hebrew

    let precision = function
      | Sure -> `sure
      | About -> `about
      | Maybe -> `maybe
      | Before -> `before
      | After -> `after
      | Oryear -> `oryear
      | Yearint -> `yearint

    let day_month_year ({day; month; year; delta} : dmy) =
      Api_piqi.Dmy.{day; month; year; delta}
    
    let translate_date ({cal; prec; dmy; dmy2; text} : date) =
      let cal = Option.map calendar cal in
      let prec = Option.map precision prec in
      let dmy = Option.map day_month_year dmy in
      let dmy2 = Option.map day_month_year dmy2 in
      Api_piqi.Date.{cal; prec; dmy; dmy2; text}

    let pevent_name = function
      | Epers_birth -> `epers_birth 
      | Epers_baptism -> `epers_baptism
      | Epers_death -> `epers_death
      | Epers_burial  -> `epers_burial
      | Epers_cremation  -> `epers_cremation
      | Epers_accomplishment  -> `epers_accomplishment
      | Epers_acquisition  -> `epers_acquisition
      | Epers_adhesion  -> `epers_adhesion
      | Epers_baptismlds -> `epers_baptismlds
      | Epers_barmitzvah -> `epers_barmitzvah
      | Epers_batmitzvah -> `epers_batmitzvah 
      | Epers_benediction -> `epers_benediction
      | Epers_changename  -> `epers_changename
      | Epers_circumcision -> `epers_circumcision 
      | Epers_confirmation  -> `epers_confirmation
      | Epers_confirmationlds  -> `epers_confirmationlds 
      | Epers_decoration  -> `epers_decoration 
      | Epers_demobilisationmilitaire  -> `epers_demobilisationmilitaire 
      | Epers_diploma  -> `epers_diploma 
      | Epers_distinction  -> `epers_distinction 
      | Epers_dotation  -> `epers_dotation 
      | Epers_dotationlds  -> `epers_dotationlds 
      | Epers_education  -> `epers_education 
      | Epers_election  -> `epers_election 
      | Epers_emigration  -> `epers_emigration 
      | Epers_excommunication  -> `epers_excommunication 
      | Epers_familylinklds  -> `epers_familylinklds 
      | Epers_firstcommunion  -> `epers_firstcommunion 
      | Epers_funeral  -> `epers_funeral 
      | Epers_graduate  -> `epers_graduate 
      | Epers_hospitalisation  -> `epers_hospitalisation 
      | Epers_illness  -> `epers_illness 
      | Epers_immigration  -> `epers_immigration 
      | Epers_listepassenger  -> `epers_listepassenger 
      | Epers_militarydistinction  -> `epers_militarydistinction 
      | Epers_militarypromotion  -> `epers_militarypromotion 
      | Epers_militaryservice  -> `epers_militaryservice 
      | Epers_mobilisationmilitaire  -> `epers_mobilisationmilitaire 
      | Epers_naturalisation  -> `epers_naturalisation 
      | Epers_occupation  -> `epers_occupation 
      | Epers_ordination  -> `epers_ordination 
      | Epers_property  -> `epers_property 
      | Epers_recensement  -> `epers_recensement 
      | Epers_residence  -> `epers_residence 
      | Epers_retired  -> `epers_retired 
      | Epers_scellentchildlds  -> `epers_scellentchildlds 
      | Epers_scellentparentlds  -> `epers_scellentparentlds 
      | Epers_scellentspouselds  -> `epers_scellentspouselds 
      | Epers_ventebien  -> `epers_ventebien 
      | Epers_will -> `epers_will

    let fevent_name = assert false
    
    let events_query_params  (d : events_query_params) =
      let close_persons_params = Option.map close_persons_params d.close_persons_params in
      let start_date = Option.map translate_date d.start_date in
      let stop_date = Option.map translate_date d.stop_date in
      let pevents = List.map pevent_name d.pevents in
      let fevents = List.map fevent_name d.fevents in
      Api_piqi.Events_query_params.{close_persons_params;
                                    start_date;
                                    stop_date;
                                    pevents;
                                    fevents}
  end
end
module Api_saisie_write = struct
  module PiqiToProtoc = struct
    let auto_complete _ = assert false
    let person_search_list _ = assert false
    let person_search_list_params _ = assert false
    let index_person _ = assert false
    let person _ = assert false
    let index_person_and_family _ = assert false
  end
  module ProtocToPiqi = struct
    let auto_complete _ = assert false
    let person_search_list _ = assert false
    let person_search_list_params _ = assert false
    let index_person _ = assert false
    let person _ = assert false
    let index_person_and_family _ = assert false
    let add_family_ok _ = assert false
    let add_child_request _ = assert false
    let edit_family_ok _ = assert false
    let add_child_ok _ = assert false
    let add_parents_ok _ = assert false
    let add_sibling_request _ = assert false
    let add_sibling_ok _ = assert false
    let add_first_fam _ = assert false
    end
end
module Api_stats = struct
  module ProtocToPiqi = struct
    let stats_params _ = assert false
  end
end
module Api_saisie_read = struct
  module ProtocToPiqi = struct
    let graph_tree_params _ = assert false
    let index_person _ = assert false
    let fiche_parameters _ = assert false
    let identifier_person _ = assert false
  end
end
