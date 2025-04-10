let required = function Some v -> v | None -> failwith "required"

module Api = struct
  module PiqiToProtoc = struct
    let error_code = function
      | `bad_request -> Api_protoc.Bad_request
      | `unauthorized -> Unauthorized
      | `forbidden -> Forbidden
      | `not_found -> Not_found
      | `conflict -> Conflict

    let error Api_piqi.Error.{ code; message } =
      let code = error_code code in
      Api_protoc.(({ code; message } : error))

    let reference_person Api_piqi.Reference_person.{ n; p; oc } =
      Api_protoc.(({ n; p; oc } : reference_person))

    let infos_base
        Api_piqi.Infos_base.
          {
            nb_persons;
            nb_families;
            sosa;
            last_modified_person;
            real_nb_persons;
            has_ignored_duplicates;
          } =
      let sosa = Option.map reference_person sosa in
      Api_protoc.(
        ({
           nb_persons;
           nb_families;
           sosa;
           last_modified_person;
           real_nb_persons;
           has_ignored_duplicates;
         }
          : infos_base))

    let warning_person
        Api_piqi.Warning_person.
          { n; p; oc; lastname; firstname; birth_date; death_date; iper } =
      Api_protoc.(
        ({ n; p; oc; lastname; firstname; birth_date; death_date; iper }
          : warning_person))

    let translate_warning_own_ancestor Api_piqi.Warning_own_ancestor.{ person }
        =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_own_ancestor))

    let translate_warning_already_defined
        Api_piqi.Warning_already_defined.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_already_defined))

    let translate_warning_bad_sex_of_married_person
        Api_piqi.Warning_bad_sex_of_married_person.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_bad_sex_of_married_person))

    let translate_warning_birth_after_death
        Api_piqi.Warning_birth_after_death.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_birth_after_death))

    let translate_warning_incoherent_sex
        Api_piqi.Warning_incoherent_sex.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_incoherent_sex))

    let translate_warning_changed_order_of_children
        Api_piqi.Warning_changed_order_of_children.{ father; mother } =
      let father = Some (warning_person father) in
      let mother = Some (warning_person mother) in
      Api_protoc.(({ father; mother } : warning_changed_order_of_children))

    let translate_warning_children_not_in_order
        Api_piqi.Warning_children_not_in_order.{ father; mother } =
      let father = Some (warning_person father) in
      let mother = Some (warning_person mother) in
      Api_protoc.(({ father; mother } : warning_children_not_in_order))

    let translate_warning_dead_too_early_to_be_father
        Api_piqi.Warning_dead_too_early_to_be_father.{ son; father } =
      let son = Some (warning_person son) in
      let father = Some (warning_person father) in
      Api_protoc.(({ son; father } : warning_dead_too_early_to_be_father))

    let translate_warning_incoherent_ancestor_date
        Api_piqi.Warning_incoherent_ancestor_date.{ person; ancestor } =
      let person = Some (warning_person person) in
      let ancestor = Some (warning_person ancestor) in
      Api_protoc.(({ person; ancestor } : warning_incoherent_ancestor_date))

    let translate_warning_marriage_date_after_death
        Api_piqi.Warning_marriage_date_after_death.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_marriage_date_after_death))

    let translate_warning_marriage_date_before_birth
        Api_piqi.Warning_marriage_date_before_birth.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_marriage_date_before_birth))

    let translate_warning_mother_dead_before_child_birth
        Api_piqi.Warning_mother_dead_before_child_birth.{ mother; child } =
      let mother = Some (warning_person mother) in
      let child = Some (warning_person child) in
      Api_protoc.(({ mother; child } : warning_mother_dead_before_child_birth))

    let translate_warning_parent_born_after_child
        Api_piqi.Warning_parent_born_after_child.{ parent; child } =
      let parent = Some (warning_person parent) in
      let child = Some (warning_person child) in
      Api_protoc.(({ parent; child } : warning_parent_born_after_child))

    let translate_warning_parent_too_young
        Api_piqi.Warning_parent_too_young.{ parent; child; date } =
      let parent = Some (warning_person parent) in
      let child = Some (warning_person child) in
      Api_protoc.(({ parent; child; date } : warning_parent_too_young))

    let translate_warning_possible_duplicate_fam
        Api_piqi.Warning_possible_duplicate_fam.
          { father1; mother1; father2; mother2 } =
      let father1 = Some (warning_person father1) in
      let mother1 = Some (warning_person mother1) in
      let father2 = Some (warning_person father2) in
      let mother2 = Some (warning_person mother2) in
      Api_protoc.(
        ({ father1; mother1; father2; mother2 }
          : warning_possible_duplicate_fam))

    let translate_warning_possible_duplicate_fam_homonymous
        Api_piqi.Warning_possible_duplicate_fam_homonymous.
          { father1; mother1; father2; mother2; homonymous } =
      let father1 = Some (warning_person father1) in
      let mother1 = Some (warning_person mother1) in
      let father2 = Some (warning_person father2) in
      let mother2 = Some (warning_person mother2) in
      let homonymous = Some (warning_person homonymous) in
      Api_protoc.(
        ({ father1; mother1; father2; mother2; homonymous }
          : warning_possible_duplicate_fam_homonymous))

    let translate_title_type = function
      | `title_main -> Api_protoc.Title_main
      | `title_name -> Title_name
      | `title_none -> Title_none

    let translate_title
        Api_piqi.Title.
          { title_type; name; title; fief; date_begin; date_end; nth } =
      let title_type = translate_title_type title_type in
      Api_protoc.(
        ({ title_type; name; title; fief; date_begin; date_end; nth } : title))

    let translate_warning_title_dates_error
        Api_piqi.Warning_title_dates_error.{ person; title } =
      let person = Some (warning_person person) in
      let title = Some (translate_title title) in
      Api_protoc.(({ person; title } : warning_title_dates_error))

    let translate_warning_undefined_sex
        Api_piqi.Warning_undefined_sex.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_undefined_sex))

    let translate_warning_young_for_marriage
        Api_piqi.Warning_young_for_marriage.{ person; date } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person; date } : warning_young_for_marriage))

    let translate_warning_close_children
        Api_piqi.Warning_close_children.{ father; mother; child1; child2 } =
      let father = Some (warning_person father) in
      let mother = Some (warning_person mother) in
      let child1 = Some (warning_person child1) in
      let child2 = Some (warning_person child2) in
      Api_protoc.(({ father; mother; child1; child2 } : warning_close_children))

    let translate_warning_distant_children
        Api_piqi.Warning_distant_children.{ father; mother; child1; child2 } =
      let father = Some (warning_person father) in
      let mother = Some (warning_person mother) in
      let child1 = Some (warning_person child1) in
      let child2 = Some (warning_person child2) in
      Api_protoc.(
        ({ father; mother; child1; child2 } : warning_distant_children))

    let translate_warning_parent_too_old
        Api_piqi.Warning_parent_too_old.{ parent; date; child } =
      let parent = Some (warning_person parent) in
      let child = Some (warning_person child) in
      Api_protoc.(({ parent; date; child } : warning_parent_too_old))

    let translate_warning_changed_order_of_marriages
        Api_piqi.Warning_changed_order_of_marriages.{ person } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person } : warning_changed_order_of_marriages))

    let translate_warning_big_age_between_spouses
        Api_piqi.Warning_big_age_between_spouses.{ father; mother; date } =
      let father = Some (warning_person father) in
      let mother = Some (warning_person mother) in
      Api_protoc.(({ father; mother; date } : warning_big_age_between_spouses))

    let translate_warning_dead_old Api_piqi.Warning_dead_old.{ person; date } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person; date } : warning_dead_old))

    let pevent_name = function
      | `epers_birth -> Api_protoc.Epers_birth
      | `epers_baptism -> Epers_baptism
      | `epers_death -> Epers_death
      | `epers_burial -> Epers_burial
      | `epers_cremation -> Epers_cremation
      | `epers_accomplishment -> Epers_accomplishment
      | `epers_acquisition -> Epers_acquisition
      | `epers_adhesion -> Epers_adhesion
      | `epers_baptismlds -> Epers_baptismlds
      | `epers_barmitzvah -> Epers_barmitzvah
      | `epers_batmitzvah -> Epers_batmitzvah
      | `epers_benediction -> Epers_benediction
      | `epers_changename -> Epers_changename
      | `epers_circumcision -> Epers_circumcision
      | `epers_confirmation -> Epers_confirmation
      | `epers_confirmationlds -> Epers_confirmationlds
      | `epers_decoration -> Epers_decoration
      | `epers_demobilisationmilitaire -> Epers_demobilisationmilitaire
      | `epers_diploma -> Epers_diploma
      | `epers_distinction -> Epers_distinction
      | `epers_dotation -> Epers_dotation
      | `epers_dotationlds -> Epers_dotationlds
      | `epers_education -> Epers_education
      | `epers_election -> Epers_election
      | `epers_emigration -> Epers_emigration
      | `epers_excommunication -> Epers_excommunication
      | `epers_familylinklds -> Epers_familylinklds
      | `epers_firstcommunion -> Epers_firstcommunion
      | `epers_funeral -> Epers_funeral
      | `epers_graduate -> Epers_graduate
      | `epers_hospitalisation -> Epers_hospitalisation
      | `epers_illness -> Epers_illness
      | `epers_immigration -> Epers_immigration
      | `epers_listepassenger -> Epers_listepassenger
      | `epers_militarydistinction -> Epers_militarydistinction
      | `epers_militarypromotion -> Epers_militarypromotion
      | `epers_militaryservice -> Epers_militaryservice
      | `epers_mobilisationmilitaire -> Epers_mobilisationmilitaire
      | `epers_naturalisation -> Epers_naturalisation
      | `epers_occupation -> Epers_occupation
      | `epers_ordination -> Epers_ordination
      | `epers_property -> Epers_property
      | `epers_recensement -> Epers_recensement
      | `epers_residence -> Epers_residence
      | `epers_retired -> Epers_retired
      | `epers_scellentchildlds -> Epers_scellentchildlds
      | `epers_scellentparentlds -> Epers_scellentparentlds
      | `epers_scellentspouselds -> Epers_scellentspouselds
      | `epers_ventebien -> Epers_ventebien
      | `epers_will -> Epers_will

    let fevent_name = function
      | `efam_marriage -> Api_protoc.Efam_marriage
      | `efam_no_marriage -> Efam_no_marriage
      | `efam_no_mention -> Efam_no_mention
      | `efam_engage -> Efam_engage
      | `efam_divorce -> Efam_divorce
      | `efam_separated -> Efam_separated
      | `efam_annulation -> Efam_annulation
      | `efam_marriage_bann -> Efam_marriage_bann
      | `efam_marriage_contract -> Efam_marriage_contract
      | `efam_marriage_license -> Efam_marriage_license
      | `efam_pacs -> Efam_pacs
      | `efam_residence -> Efam_residence

    let translate_warning_event Api_piqi.Warning_event.{ pevent; fevent } =
      let fevent = Option.map fevent_name fevent in
      let pevent = Option.map pevent_name pevent in
      Api_protoc.(({ fevent; pevent } : warning_event))

    let translate_warning_witness_date_after_death
        Api_piqi.Warning_witness_date_after_death.{ person; event; origin } =
      let person = Some (warning_person person) in
      let event = Some (translate_warning_event event) in
      let origin = List.map warning_person origin in
      Api_protoc.(
        ({ person; event; origin } : warning_witness_date_after_death))

    let translate_warning_witness_date_before_birth
        Api_piqi.Warning_witness_date_before_birth.{ person; event; origin } =
      let person = Some (warning_person person) in
      let event = Some (translate_warning_event event) in
      let origin = List.map warning_person origin in
      Api_protoc.(
        ({ person; event; origin } : warning_witness_date_before_birth))

    let translate_warning_event_order
        Api_piqi.Warning_event_order.{ person; fevents; pevents } =
      let person = Some (warning_person person) in
      let fevents = List.map fevent_name fevents in
      let pevents = List.map pevent_name pevents in
      Api_protoc.(({ person; fevents; pevents } : warning_event_order))

    let translate_warning_old_for_marriage
        Api_piqi.Warning_old_for_marriage.{ person; date } =
      let person = Some (warning_person person) in
      Api_protoc.(({ person; date } : warning_old_for_marriage))

    (*
    let translate_warning_truc Api_piqi.Warning_truc.{person} =
      let person = Some (warning_person person) in
      Api_protoc.(({person} : warning_truc))
*)

    let base_warnings
        Api_piqi.Base_warnings.
          {
            warning_already_defined;
            warning_own_ancestor;
            warning_bad_sex_of_married_person;
            warning_birth_after_death;
            warning_incoherent_sex;
            warning_changed_order_of_children;
            warning_children_not_in_order;
            warning_dead_too_early_to_be_father;
            warning_incoherent_ancestor_date;
            warning_marriage_date_after_death;
            warning_marriage_date_before_birth;
            warning_mother_dead_before_child_birth;
            warning_parent_born_after_child;
            warning_parent_too_young;
            warning_possible_duplicate_fam;
            warning_title_dates_error;
            warning_undefined_sex;
            warning_young_for_marriage;
            warning_close_children;
            warning_parent_too_old;
            warning_changed_order_of_marriages;
            warning_big_age_between_spouses;
            warning_dead_old;
            warning_witness_date_after_death;
            warning_witness_date_before_birth;
            warning_old_for_marriage;
            warning_distant_children;
            warning_event_order;
            warning_possible_duplicate_fam_homonymous;
          } =
      let warning_already_defined =
        List.map translate_warning_already_defined warning_already_defined
      in
      let warning_own_ancestor =
        List.map translate_warning_own_ancestor warning_own_ancestor
      in
      let warning_bad_sex_of_married_person =
        List.map translate_warning_bad_sex_of_married_person
          warning_bad_sex_of_married_person
      in
      (*
      let warning_truc = List.map translate_warning_truc warning_truc in
*)
      let warning_old_for_marriage =
        List.map translate_warning_old_for_marriage warning_old_for_marriage
      in
      let warning_event_order =
        List.map translate_warning_event_order warning_event_order
      in
      let warning_witness_date_after_death =
        List.map translate_warning_witness_date_after_death
          warning_witness_date_after_death
      in
      let warning_witness_date_before_birth =
        List.map translate_warning_witness_date_before_birth
          warning_witness_date_before_birth
      in
      let warning_birth_after_death =
        List.map translate_warning_birth_after_death warning_birth_after_death
      in
      let warning_incoherent_sex =
        List.map translate_warning_incoherent_sex warning_incoherent_sex
      in
      let warning_changed_order_of_children =
        List.map translate_warning_changed_order_of_children
          warning_changed_order_of_children
      in
      let warning_children_not_in_order =
        List.map translate_warning_children_not_in_order
          warning_children_not_in_order
      in
      let warning_dead_too_early_to_be_father =
        List.map translate_warning_dead_too_early_to_be_father
          warning_dead_too_early_to_be_father
      in
      let warning_incoherent_ancestor_date =
        List.map translate_warning_incoherent_ancestor_date
          warning_incoherent_ancestor_date
      in
      let warning_marriage_date_after_death =
        List.map translate_warning_marriage_date_after_death
          warning_marriage_date_after_death
      in
      let warning_marriage_date_before_birth =
        List.map translate_warning_marriage_date_before_birth
          warning_marriage_date_before_birth
      in
      let warning_mother_dead_before_child_birth =
        List.map translate_warning_mother_dead_before_child_birth
          warning_mother_dead_before_child_birth
      in
      let warning_parent_born_after_child =
        List.map translate_warning_parent_born_after_child
          warning_parent_born_after_child
      in
      let warning_parent_too_young =
        List.map translate_warning_parent_too_young warning_parent_too_young
      in
      let warning_possible_duplicate_fam =
        List.map translate_warning_possible_duplicate_fam
          warning_possible_duplicate_fam
      in
      let warning_possible_duplicate_fam_homonymous =
        List.map translate_warning_possible_duplicate_fam_homonymous
          warning_possible_duplicate_fam_homonymous
      in
      let warning_title_dates_error =
        List.map translate_warning_title_dates_error warning_title_dates_error
      in
      let warning_undefined_sex =
        List.map translate_warning_undefined_sex warning_undefined_sex
      in
      let warning_young_for_marriage =
        List.map translate_warning_young_for_marriage warning_young_for_marriage
      in
      let warning_close_children =
        List.map translate_warning_close_children warning_close_children
      in
      let warning_distant_children =
        List.map translate_warning_distant_children warning_distant_children
      in
      let warning_parent_too_old =
        List.map translate_warning_parent_too_old warning_parent_too_old
      in
      let warning_changed_order_of_marriages =
        List.map translate_warning_changed_order_of_marriages
          warning_changed_order_of_marriages
      in
      let warning_big_age_between_spouses =
        List.map translate_warning_big_age_between_spouses
          warning_big_age_between_spouses
      in
      let warning_dead_old =
        List.map translate_warning_dead_old warning_dead_old
      in
      Api_protoc.(
        ({
           warning_already_defined;
           warning_own_ancestor;
           warning_bad_sex_of_married_person;
           warning_birth_after_death;
           warning_incoherent_sex;
           warning_changed_order_of_children;
           warning_children_not_in_order;
           warning_dead_too_early_to_be_father;
           warning_incoherent_ancestor_date;
           warning_marriage_date_after_death;
           warning_marriage_date_before_birth;
           warning_mother_dead_before_child_birth;
           warning_parent_born_after_child;
           warning_parent_too_young;
           warning_possible_duplicate_fam;
           warning_title_dates_error;
           warning_undefined_sex;
           warning_young_for_marriage;
           warning_close_children;
           warning_parent_too_old;
           warning_changed_order_of_marriages;
           warning_big_age_between_spouses;
           warning_dead_old;
           warning_witness_date_after_death;
           warning_witness_date_before_birth;
           warning_old_for_marriage;
           warning_distant_children;
           warning_event_order;
           warning_possible_duplicate_fam_homonymous;
         }
          : base_warnings))

    let translate_sex = function
      | `male -> Api_protoc.Male
      | `female -> Female
      | `unknown -> Unknown

    let translate_divorce_type = function
      | `not_divorced -> Api_protoc.Not_divorced
      | `divorced -> Divorced
      | `separated -> Separated

    let translate_death_type = function
      | `not_dead -> Api_protoc.Not_dead
      | `dead -> Dead
      | `dead_young -> Dead_young
      | `dead_dont_know_when -> Dead_dont_know_when
      | `dont_know_if_dead -> Dont_know_if_dead
      | `of_course_dead -> Of_course_dead

    let translate_visibility = function
      | `visibility_public -> Api_protoc.Visibility_public
      | `visibility_semi_public -> Visibility_semi_public
      | `visibility_private -> Visibility_private

    let translate_spouse
        Api_piqi.Spouse.
          {
            sosa;
            n;
            p;
            oc;
            sex;
            lastname;
            firstname;
            public_name;
            image;
            birth_date;
            birth_place;
            baptism_date;
            baptism_place;
            death_date;
            death_place;
            death_type;
            burial_date;
            burial_place;
            marriage_date;
            marriage_place;
            divorce_type;
            index;
            visible_for_visitors;
          } =
      let sex = translate_sex sex in
      let death_type = translate_death_type death_type in
      let divorce_type = translate_divorce_type divorce_type in
      let visible_for_visitors = translate_visibility visible_for_visitors in
      Api_protoc.(
        ({
           sosa;
           n;
           p;
           oc;
           sex;
           lastname;
           firstname;
           public_name;
           image;
           birth_date;
           birth_place;
           baptism_date;
           baptism_place;
           death_date;
           death_place;
           death_type;
           burial_date;
           burial_place;
           marriage_date;
           marriage_place;
           divorce_type;
           index;
           visible_for_visitors;
         }
          : spouse))

    let translate_person
        Api_piqi.Person.
          {
            sosa;
            n;
            p;
            oc;
            sex;
            lastname;
            firstname;
            public_name;
            image;
            birth_date;
            birth_place;
            baptism_date;
            baptism_place;
            death_date;
            death_place;
            death_type;
            burial_date;
            burial_place;
            spouses;
            ascend;
            descend;
            visible_for_visitors;
            baseprefix;
            index;
            is_contemporary;
            name_is_hidden;
            name_is_restricted;
          } =
      let sex = translate_sex sex in
      let death_type = translate_death_type death_type in
      let spouses = List.map translate_spouse spouses in
      let visible_for_visitors = translate_visibility visible_for_visitors in
      Api_protoc.(
        ({
           sosa;
           n;
           p;
           oc;
           sex;
           lastname;
           firstname;
           public_name;
           image;
           birth_date;
           birth_place;
           baptism_date;
           baptism_place;
           death_date;
           death_place;
           death_type;
           burial_date;
           burial_place;
           spouses;
           ascend;
           descend;
           visible_for_visitors;
           baseprefix;
           index;
           is_contemporary;
           name_is_hidden;
           name_is_restricted;
         }
          : person))

    let list_persons Api_piqi.List_persons.{ list_persons } =
      let list_persons = List.map translate_person list_persons in
      Api_protoc.(({ list_persons } : list_persons))

    let translate_relation_parent_type = function
      | `rpt_adoption -> Api_protoc.Rpt_adoption
      | `rpt_recognition -> Rpt_recognition
      | `rpt_candidate_parent -> Rpt_candidate_parent
      | `rpt_god_parent -> Rpt_god_parent
      | `rpt_foster_parent -> Rpt_foster_parent

    let translate_relation_parent
        Api_piqi.Relation_parent.{ father; mother; source; rpt_type } =
      let rpt_type = translate_relation_parent_type rpt_type in
      Api_protoc.(({ father; mother; source; rpt_type } : relation_parent))

    let translate_full_person
        Api_piqi.Full_person.
          {
            sosa;
            n;
            p;
            oc;
            sex;
            lastname;
            firstname;
            public_name;
            image;
            birth_date;
            birth_place;
            baptism_date;
            baptism_place;
            death_date;
            death_place;
            death_type;
            burial_date;
            burial_place;
            visible_for_visitors;
            baseprefix;
            index;
            is_contemporary;
            name_is_hidden;
            name_is_restricted;
            aliases;
            qualifiers;
            firstname_aliases;
            surname_aliases;
            birth_src;
            baptism_src;
            death_src;
            burial_src;
            occupation;
            psources;
            titles;
            related;
            rparents;
            parents;
            families;
          } =
      let sex = translate_sex sex in
      let death_type = translate_death_type death_type in
      let visible_for_visitors = translate_visibility visible_for_visitors in
      let titles = List.map translate_title titles in
      let rparents = List.map translate_relation_parent rparents in
      Api_protoc.(
        ({
           sosa;
           n;
           p;
           oc;
           sex;
           lastname;
           firstname;
           public_name;
           image;
           birth_date;
           birth_place;
           baptism_date;
           baptism_place;
           death_date;
           death_place;
           death_type;
           burial_date;
           burial_place;
           visible_for_visitors;
           baseprefix;
           index;
           is_contemporary;
           name_is_hidden;
           name_is_restricted;
           aliases;
           qualifiers;
           firstname_aliases;
           surname_aliases;
           birth_src;
           baptism_src;
           death_src;
           burial_src;
           occupation;
           psources;
           titles;
           related;
           rparents;
           parents;
           families;
         }
          : full_person))

    let list_full_persons Api_piqi.List_full_persons.{ persons } =
      let persons = List.map translate_full_person persons in
      Api_protoc.(({ persons } : list_full_persons))

    let calendar = function
      | `gregorian -> Api_protoc.Gregorian
      | `julian -> Julian
      | `french -> French
      | `hebrew -> Hebrew

    let precision = function
      | `sure -> Api_protoc.Sure
      | `about -> About
      | `maybe -> Maybe
      | `before -> Before
      | `after -> After
      | `oryear -> Oryear
      | `yearint -> Yearint

    let day_month_year Api_piqi.Dmy.{ day; month; year; delta } =
      Api_protoc.(({ day; month; year; delta } : dmy))

    let translate_date Api_piqi.Date.{ cal; prec; dmy; dmy2; text } =
      let cal = Option.map calendar cal in
      let prec = Option.map precision prec in
      let dmy = Option.map day_month_year dmy in
      let dmy2 = Option.map day_month_year dmy2 in
      Api_protoc.(({ cal; prec; dmy; dmy2; text } : date))

    let event_query_result e =
      let p = Some (translate_person e.Api_piqi.Event_query_result.p) in
      let fevent_name = Option.map fevent_name e.fevent_name in
      let pevent_name = Option.map pevent_name e.pevent_name in
      let sp = Option.map translate_person e.sp in
      let date = Some (translate_date e.date) in
      Api_protoc.(
        ({
           p;
           sp;
           pevent_name;
           fevent_name;
           date;
           place = e.place;
           note = e.note;
           src = e.src;
         }
          : event_query_result))

    let event_query_result_list Api_piqi.Event_query_result_list.{ events } =
      let events = List.map event_query_result events in
      Api_protoc.(({ events } : event_query_result_list))

    let translate_node Api_piqi.Node.{ id; person } =
      let person = Some (translate_person person) in
      Api_protoc.(({ id; person } : node))

    let translate_edge Api_piqi.Edge.{ from_node; to_node } =
      Api_protoc.(({ from_node; to_node } : edge))

    let graph Api_piqi.Graph.{ nodes; edges } =
      let nodes = List.map translate_node nodes in
      let edges = List.map translate_edge edges in
      Api_protoc.(({ nodes; edges } : graph))

    let translate_full_node Api_piqi.Full_node.{ id; person } =
      let person = Some (translate_full_person person) in
      Api_protoc.(({ id; person } : full_node))

    let translate_marriage_type = function
      | `married -> Api_protoc.Married
      | `not_married -> Not_married
      | `engaged -> Engaged
      | `no_sexes_check_not_married -> No_sexes_check_not_married
      | `no_mention -> No_mention
      | `no_sexes_check_married -> No_sexes_check_married
      | `marriage_bann -> Marriage_bann
      | `marriage_contract -> Marriage_contract
      | `marriage_license -> Marriage_license
      | `pacs -> Pacs
      | `residence -> Residence

    let translate_full_family
        Api_piqi.Full_family.
          {
            fsources;
            marriage_date;
            marriage_place;
            marriage_src;
            marriage_type;
            divorce_type;
            divorce_date;
            witnesses;
            father;
            mother;
            children;
            index;
          } =
      let marriage_type = translate_marriage_type marriage_type in
      let divorce_type = translate_divorce_type divorce_type in
      Api_protoc.(
        ({
           fsources;
           marriage_date;
           marriage_place;
           marriage_src;
           marriage_type;
           divorce_type;
           divorce_date;
           witnesses;
           father;
           mother;
           children;
           index;
         }
          : full_family))

    let full_graph Api_piqi.Full_graph.{ nodes; edges; families } =
      let edges = List.map translate_edge edges in
      let nodes = List.map translate_full_node nodes in
      let families = List.map translate_full_family families in
      Api_protoc.(({ nodes; edges; families } : full_graph))

    let person = translate_person
    let full_person = translate_full_person

    let translate_image Api_piqi.Image.{ person; img } =
      let person = Some (reference_person person) in
      Api_protoc.(({ person; img } : image))

    let list_images Api_piqi.List_images.{ list_images } =
      let list_images = List.map translate_image list_images in
      Api_protoc.(({ list_images } : list_images))

    let image_address Api_piqi.Image_address.{ img } =
      Api_protoc.(({ img } : image_address))

    let list_full_families Api_piqi.List_full_families.{ families } =
      let families = List.map translate_full_family families in
      Api_protoc.(({ families } : list_full_families))

    let translate_modification_type = function
      | `person_added -> Api_protoc.Person_added
      | `person_modified -> Person_modified
      | `person_deleted -> Person_deleted
      | `person_merged -> Person_merged
      | `image_received -> Image_received
      | `image_deleted -> Image_deleted
      | `family_added -> Family_added
      | `family_modified -> Family_modified
      | `family_deleted -> Family_deleted
      | `family_inverted -> Family_inverted
      | `family_merged -> Family_merged
      | `changed_children_names -> Changed_children_names
      | `parents_added -> Parents_added
      | `notes_modified -> Notes_modified
      | `place_modified -> Place_modified
      | `source_modified -> Source_modified
      | `occupation_modified -> Occupation_modified

    let translate_history_person
        Api_piqi.History_person.
          {
            n;
            p;
            oc;
            firstname;
            lastname;
            year1;
            year2;
            exists_in_base;
            has_history;
          } =
      Api_protoc.(
        ({
           n;
           p;
           oc;
           firstname;
           lastname;
           year1;
           year2;
           exists_in_base;
           has_history;
         }
          : history_person))

    let translate_time Api_piqi.Time.{ year; month; day; hour; minute; second }
        =
      Api_protoc.(({ year; month; day; hour; minute; second } : time))

    let translate_history_note
        Api_piqi.History_note.{ link_parameters; link_txt } =
      Api_protoc.(({ link_parameters; link_txt } : history_note))

    let translate_history_entry
        Api_piqi.History_entry.{ modification_type; time; editor; person; note }
        =
      let modification_type = translate_modification_type modification_type in
      let time = Some (translate_time time) in
      let person = Option.map translate_history_person person in
      let note = Option.map translate_history_note note in
      Api_protoc.(
        ({ modification_type; time; editor; person; note } : history_entry))

    let history Api_piqi.History.{ entries; page; total_elements } =
      let entries = List.map translate_history_entry entries in
      Api_protoc.(({ entries; page; total_elements } : history))
  end

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

        end*)
  module ProtocToPiqi = struct
    let reference_person Api_protoc.(({ n; p; oc } : reference_person)) =
      Api_piqi.Reference_person.{ n; p; oc }

    let reference_person_i = function
      | Api_protoc.Key key ->
          Api_piqi.Reference_person_i.
            { key = Some (reference_person key); i = None }
      | I i -> Api_piqi.Reference_person_i.{ key = None; i = Some i }

    let graph_params Api_protoc.(({ generation; person } : graph_params)) =
      let person = reference_person (required person) in
      Api_piqi.Graph_params.{ generation; person }

    let graph_rel_params Api_protoc.(({ person1; person2 } : graph_rel_params))
        =
      let person1 = reference_person (required person1) in
      let person2 = reference_person (required person2) in
      Api_piqi.Graph_rel_params.{ person1; person2 }

    let cpl_rel_params Api_protoc.(({ person1; person2 } : cpl_rel_params)) =
      let person1 = reference_person (required person1) in
      let person2 = reference_person (required person2) in
      Api_piqi.Cpl_rel_params.{ person1; person2 }

    let filter_date Api_protoc.(({ day; month; year } : filter_date)) =
      Api_piqi.Filter_date.{ day; month; year }

    let filter_date_range
        Api_protoc.(({ date_begin; date_end; only_exact } : filter_date_range))
        =
      let date_begin = filter_date (required date_begin) in
      let date_end = filter_date (required date_end) in
      let only_exact = Option.value ~default:false only_exact in
      Api_piqi.Filter_date_range.{ date_begin; date_end; only_exact }

    let last_modifications
        Api_protoc.(({ wizard; max_res; range } : last_modifications)) =
      let range = Option.map filter_date_range range in
      Api_piqi.Last_modifications.{ wizard; max_res; range }

    let last_visits Api_protoc.(({ user } : last_visits)) =
      Api_piqi.Last_visits.{ user }

    let all_persons_params Api_protoc.(({ from; limit } : all_persons_params)) =
      Api_piqi.All_persons_params.{ from; limit }

    let all_families_params Api_protoc.(({ from; limit } : all_families_params))
        =
      Api_piqi.All_families_params.{ from; limit }

    let pers_img Api_protoc.(({ person; img } : pers_img)) =
      let person = reference_person (required person) in
      Api_piqi.Pers_img.{ person; img }

    let list_pers_img Api_protoc.(({ list_pers_img } : list_pers_img)) =
      let list_pers_img = List.map pers_img list_pers_img in
      Api_piqi.List_pers_img.{ list_pers_img }

    let translate_search_type = function
      | Api_protoc.Starting_with -> `starting_with
      | Approximative -> `approximative
      | Lastname_or_firstname -> `lastname_or_firstname

    let search_params
        Api_protoc.(
          ({
             search_type;
             lastname;
             firstname;
             only_sosa;
             only_recent;
             maiden_name;
           } :
            search_params)) =
      let search_type =
        translate_search_type (Option.value ~default:Starting_with search_type)
      in
      let only_sosa = Option.value ~default:false only_sosa in
      let only_recent = Option.value ~default:false only_recent in
      let maiden_name = Option.value ~default:false maiden_name in
      Api_piqi.Search_params.
        {
          search_type;
          lastname;
          firstname;
          only_sosa;
          only_recent;
          maiden_name;
        }

    let history_request
        Api_protoc.(
          ({ page; elements_per_page; filter_user } : history_request)) =
      Api_piqi.History_request.{ page; elements_per_page; filter_user }

    let index Api_protoc.(({ index } : index)) = Api_piqi.Index.{ index }

    let list_reference_person
        Api_protoc.(({ list_ref_persons } : list_reference_persons)) =
      let list_ref_persons = List.map reference_person list_ref_persons in
      Api_piqi.List_reference_persons.{ list_ref_persons }

    let close_persons_params
        Api_protoc.(
          ({ person; nb_gen_asc; nb_gen_desc; spouse_ascend; only_recent } :
            close_persons_params)) =
      let person = reference_person (required person) in
      let spouse_ascend = Option.value ~default:false spouse_ascend in
      let only_recent = Option.value ~default:false only_recent in
      Api_piqi.Close_persons_params.
        { person; nb_gen_asc; nb_gen_desc; spouse_ascend; only_recent }

    let translate_sex = function
      | Api_protoc.Male -> `male
      | Female -> `female
      | Unknown -> `unknown

    let person_start
        Api_protoc.(
          ({
             lastname;
             firstname;
             sex;
             birth_date_day;
             birth_date_month;
             birth_date_year;
           } :
            person_start)) =
      let sex = translate_sex sex in
      Api_piqi.Person_start.
        {
          lastname;
          firstname;
          sex;
          birth_date_day;
          birth_date_month;
          birth_date_year;
        }

    let calendar = function
      | Api_protoc.Gregorian -> `gregorian
      | Julian -> `julian
      | French -> `french
      | Hebrew -> `hebrew

    let precision = function
      | Api_protoc.Sure -> `sure
      | About -> `about
      | Maybe -> `maybe
      | Before -> `before
      | After -> `after
      | Oryear -> `oryear
      | Yearint -> `yearint

    let day_month_year Api_protoc.(({ day; month; year; delta } : dmy)) =
      Api_piqi.Dmy.{ day; month; year; delta }

    let translate_date Api_protoc.(({ cal; prec; dmy; dmy2; text } : date)) =
      let cal = Option.map calendar cal in
      let prec = Option.map precision prec in
      let dmy = Option.map day_month_year dmy in
      let dmy2 = Option.map day_month_year dmy2 in
      Api_piqi.Date.{ cal; prec; dmy; dmy2; text }

    let pevent_name = function
      | Api_protoc.Epers_birth -> `epers_birth
      | Epers_baptism -> `epers_baptism
      | Epers_death -> `epers_death
      | Epers_burial -> `epers_burial
      | Epers_cremation -> `epers_cremation
      | Epers_accomplishment -> `epers_accomplishment
      | Epers_acquisition -> `epers_acquisition
      | Epers_adhesion -> `epers_adhesion
      | Epers_baptismlds -> `epers_baptismlds
      | Epers_barmitzvah -> `epers_barmitzvah
      | Epers_batmitzvah -> `epers_batmitzvah
      | Epers_benediction -> `epers_benediction
      | Epers_changename -> `epers_changename
      | Epers_circumcision -> `epers_circumcision
      | Epers_confirmation -> `epers_confirmation
      | Epers_confirmationlds -> `epers_confirmationlds
      | Epers_decoration -> `epers_decoration
      | Epers_demobilisationmilitaire -> `epers_demobilisationmilitaire
      | Epers_diploma -> `epers_diploma
      | Epers_distinction -> `epers_distinction
      | Epers_dotation -> `epers_dotation
      | Epers_dotationlds -> `epers_dotationlds
      | Epers_education -> `epers_education
      | Epers_election -> `epers_election
      | Epers_emigration -> `epers_emigration
      | Epers_excommunication -> `epers_excommunication
      | Epers_familylinklds -> `epers_familylinklds
      | Epers_firstcommunion -> `epers_firstcommunion
      | Epers_funeral -> `epers_funeral
      | Epers_graduate -> `epers_graduate
      | Epers_hospitalisation -> `epers_hospitalisation
      | Epers_illness -> `epers_illness
      | Epers_immigration -> `epers_immigration
      | Epers_listepassenger -> `epers_listepassenger
      | Epers_militarydistinction -> `epers_militarydistinction
      | Epers_militarypromotion -> `epers_militarypromotion
      | Epers_militaryservice -> `epers_militaryservice
      | Epers_mobilisationmilitaire -> `epers_mobilisationmilitaire
      | Epers_naturalisation -> `epers_naturalisation
      | Epers_occupation -> `epers_occupation
      | Epers_ordination -> `epers_ordination
      | Epers_property -> `epers_property
      | Epers_recensement -> `epers_recensement
      | Epers_residence -> `epers_residence
      | Epers_retired -> `epers_retired
      | Epers_scellentchildlds -> `epers_scellentchildlds
      | Epers_scellentparentlds -> `epers_scellentparentlds
      | Epers_scellentspouselds -> `epers_scellentspouselds
      | Epers_ventebien -> `epers_ventebien
      | Epers_will -> `epers_will

    let fevent_name = function
      | Api_protoc.Efam_marriage -> `efam_marriage
      | Efam_no_marriage -> `efam_no_marriage
      | Efam_no_mention -> `efam_no_mention
      | Efam_engage -> `efam_engage
      | Efam_divorce -> `efam_divorce
      | Efam_separated -> `efam_separated
      | Efam_annulation -> `efam_annulation
      | Efam_marriage_bann -> `efam_marriage_bann
      | Efam_marriage_contract -> `efam_marriage_contract
      | Efam_marriage_license -> `efam_marriage_license
      | Efam_pacs -> `efam_pacs
      | Efam_residence -> `efam_residence

    let events_query_params Api_protoc.((d : events_query_params)) =
      let close_persons_params =
        Option.map close_persons_params d.close_persons_params
      in
      let start_date = Option.map translate_date d.start_date in
      let stop_date = Option.map translate_date d.stop_date in
      let pevents = List.map pevent_name d.pevents in
      let fevents = List.map fevent_name d.fevents in
      Api_piqi.Events_query_params.
        { close_persons_params; start_date; stop_date; pevents; fevents }
  end
end

module Api_saisie_write = struct
  module PiqiToProtoc = struct
    let auto_complete_result
        Api_saisie_write_piqi.Auto_complete_result.{ result } =
      Api_saisie_write_protoc.(({ result } : auto_complete_result))

    let translate_sex = function
      | `male -> Api_saisie_write_protoc.Male
      | `female -> Female
      | `unknown -> Unknown

    let translate_sosa = function
      | `sosa_ref -> Api_saisie_write_protoc.Sosa_ref
      | `sosa -> Sosa
      | `no_sosa -> No_sosa

    let translate_person_search
        Api_saisie_write_piqi.Person_search.
          { index; sex; lastname; firstname; dates; image; sosa; family } =
      let sex = translate_sex sex in
      let sosa = translate_sosa sosa in
      Api_saisie_write_protoc.(
        ({ index; sex; lastname; firstname; dates; image; sosa; family }
          : person_search))

    let person_search_list Api_saisie_write_piqi.Person_search_list.{ persons }
        =
      let persons = List.map translate_person_search persons in
      Api_saisie_write_protoc.(({ persons } : person_search_list))

    let calendar = function
      | `gregorian -> Api_saisie_write_protoc.Gregorian
      | `julian -> Julian
      | `french -> French
      | `hebrew -> Hebrew

    let simple_person
        Api_saisie_write_piqi.Simple_person.
          {
            index;
            sex;
            lastname;
            firstname;
            birth_short_date;
            birth_place;
            death_short_date;
            death_place;
            image;
            sosa;
          } =
      let sex = translate_sex sex in
      let sosa = translate_sosa sosa in
      Api_saisie_write_protoc.(
        ({
           index;
           sex;
           lastname;
           firstname;
           birth_short_date;
           birth_place;
           death_short_date;
           death_place;
           image;
           sosa;
         }
          : simple_person))

    let translate_witness_type = function
      | `witness -> Api_saisie_write_protoc.Witness
      | `witness_godparent -> Witness_godparent
      | `witness_civilofficer -> Witness_civilofficer
      | `witness_religiousofficer -> Witness_religiousofficer
      | `witness_informant -> Witness_informant
      | `witness_attending -> Witness_attending
      | `witness_mentioned -> Witness_mentioned
      | `witness_other -> Witness_other

    let witness_event
        Api_saisie_write_piqi.Witness_event.
          { witness_type; witness; witness_note } =
      let witness_type = translate_witness_type witness_type in
      let witness = Some (simple_person witness) in
      Api_saisie_write_protoc.(
        ({ witness_type; witness; witness_note } : witness_event))

    let translate_event
        Api_saisie_write_piqi.Event.
          {
            name;
            date;
            date_conv;
            date_cal;
            place;
            reason;
            note;
            src;
            spouse;
            witnesses;
          } =
      let date_cal = Option.map calendar date_cal in
      let spouse = Option.map simple_person spouse in
      let witnesses = List.map witness_event witnesses in
      Api_saisie_write_protoc.(
        ({
           name;
           date;
           date_conv;
           date_cal;
           place;
           reason;
           note;
           src;
           spouse;
           witnesses;
         }
          : event))

    let translate_relation_type = function
      | `rparent_adoption -> Api_saisie_write_protoc.Rparent_adoption
      | `rparent_recognition -> Rparent_recognition
      | `rparent_candidate_parent -> Rparent_candidate_parent
      | `rparent_god_parent -> Rparent_god_parent
      | `rparent_foster_parent -> Rparent_foster_parent
      | `rchild_adoption -> Rchild_adoption
      | `rchild_recognition -> Rchild_recognition
      | `rchild_candidate_parent -> Rchild_candidate_parent
      | `rchild_god_parent -> Rchild_god_parent
      | `rchild_foster_parent -> Rchild_foster_parent

    let relation_person Api_saisie_write_piqi.Relation_person.{ r_type; person }
        =
      let r_type = translate_relation_type r_type in
      let person = Some (simple_person person) in
      Api_saisie_write_protoc.(({ r_type; person } : relation_person))

    let translate_was_witness
        Api_saisie_write_piqi.Was_witness.{ husband; wife } =
      Api_saisie_write_protoc.(({ husband; wife } : was_witness))

    let person_search_info
        Api_saisie_write_piqi.Person_search_info.
          {
            index;
            sex;
            lastname;
            firstname;
            public_name;
            aliases;
            qualifiers;
            firstname_aliases;
            surname_aliases;
            image;
            events;
            occupation;
            notes;
            psources;
            has_sources;
            titles;
            related;
            rparents;
            was_witness;
            sosa;
          } =
      let sex = translate_sex sex in
      let events = List.map translate_event events in
      let related = List.map relation_person related in
      let rparents = List.map relation_person rparents in
      let was_witness = List.map translate_was_witness was_witness in
      let sosa = translate_sosa sosa in
      Api_saisie_write_protoc.(
        ({
           index;
           sex;
           lastname;
           firstname;
           public_name;
           aliases;
           qualifiers;
           firstname_aliases;
           surname_aliases;
           image;
           events;
           occupation;
           notes;
           psources;
           has_sources;
           titles;
           related;
           rparents;
           was_witness;
           sosa;
         }
          : person_search_info))

    let transl_calendar Api_saisie_write_piqi.Transl_calendar.{ pos; sval } =
      let pos = calendar pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_calendar))

    let config_transl_calendar
        Api_saisie_write_piqi.Config_transl_calendar.{ msg } =
      let msg = List.map transl_calendar msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_calendar))

    let transl_witness_type
        Api_saisie_write_piqi.Transl_witness_type.{ pos; sval } =
      let pos = translate_witness_type pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_witness_type))

    let config_transl_witness_type
        Api_saisie_write_piqi.Config_transl_witness_type.{ msg } =
      let msg = List.map transl_witness_type msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_witness_type))

    let precision = function
      | `sure -> Api_saisie_write_protoc.Sure
      | `about -> About
      | `maybe -> Maybe
      | `before -> Before
      | `after -> After
      | `oryear -> Oryear
      | `yearint -> Yearint

    let transl_precision Api_saisie_write_piqi.Transl_precision.{ pos; sval } =
      let pos = precision pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_precision))

    let config_transl_precision
        Api_saisie_write_piqi.Config_transl_precision.{ msg } =
      let msg = List.map transl_precision msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_precision))

    let translate_death_type = function
      | `not_dead -> Api_saisie_write_protoc.Not_dead
      | `dead -> Dead
      | `dead_young -> Dead_young
      | `dead_dont_know_when -> Dead_dont_know_when
      | `dont_know_if_dead -> Dont_know_if_dead
      | `of_course_dead -> Of_course_dead

    let transl_death_type Api_saisie_write_piqi.Transl_death_type.{ pos; sval }
        =
      let pos = translate_death_type pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_death_type))

    let config_transl_death_type
        Api_saisie_write_piqi.Config_transl_death_type.{ msg } =
      let msg = List.map transl_death_type msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_death_type))

    let relation_parent_type = function
      | `rpt_adoption_father -> Api_saisie_write_protoc.Rpt_adoption_father
      | `rpt_adoption_mother -> Rpt_adoption_mother
      | `rpt_recognition_father -> Rpt_recognition_father
      | `rpt_recognition_mother -> Rpt_recognition_mother
      | `rpt_candidate_parent_father -> Rpt_candidate_parent_father
      | `rpt_candidate_parent_mother -> Rpt_candidate_parent_mother
      | `rpt_god_parent_father -> Rpt_god_parent_father
      | `rpt_god_parent_mother -> Rpt_god_parent_mother
      | `rpt_foster_parent_father -> Rpt_foster_parent_father
      | `rpt_foster_parent_mother -> Rpt_foster_parent_mother

    let transl_relation_parent_type
        Api_saisie_write_piqi.Transl_relation_parent_type.{ pos; sval } =
      let pos = relation_parent_type pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_relation_parent_type))

    let config_transl_relation_parent_type
        Api_saisie_write_piqi.Config_transl_relation_parent_type.{ msg } =
      let msg = List.map transl_relation_parent_type msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_relation_parent_type))

    let fevent_name = function
      | `efam_marriage -> Api_saisie_write_protoc.Efam_marriage
      | `efam_no_marriage -> Efam_no_marriage
      | `efam_no_mention -> Efam_no_mention
      | `efam_engage -> Efam_engage
      | `efam_divorce -> Efam_divorce
      | `efam_separated -> Efam_separated
      | `efam_annulation -> Efam_annulation
      | `efam_marriage_bann -> Efam_marriage_bann
      | `efam_marriage_contract -> Efam_marriage_contract
      | `efam_marriage_license -> Efam_marriage_license
      | `efam_pacs -> Efam_pacs
      | `efam_residence -> Efam_residence

    let transl_fevent_name
        Api_saisie_write_piqi.Transl_fevent_name.{ pos; sval } =
      let pos = fevent_name pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_fevent_name))

    let config_transl_fevent_name
        Api_saisie_write_piqi.Config_transl_fevent_name.{ msg } =
      let msg = List.map transl_fevent_name msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_fevent_name))

    let pevent_name = function
      | `epers_birth -> Api_saisie_write_protoc.Epers_birth
      | `epers_baptism -> Epers_baptism
      | `epers_death -> Epers_death
      | `epers_burial -> Epers_burial
      | `epers_cremation -> Epers_cremation
      | `epers_accomplishment -> Epers_accomplishment
      | `epers_acquisition -> Epers_acquisition
      | `epers_adhesion -> Epers_adhesion
      | `epers_baptismlds -> Epers_baptismlds
      | `epers_barmitzvah -> Epers_barmitzvah
      | `epers_batmitzvah -> Epers_batmitzvah
      | `epers_benediction -> Epers_benediction
      | `epers_changename -> Epers_changename
      | `epers_circumcision -> Epers_circumcision
      | `epers_confirmation -> Epers_confirmation
      | `epers_confirmationlds -> Epers_confirmationlds
      | `epers_decoration -> Epers_decoration
      | `epers_demobilisationmilitaire -> Epers_demobilisationmilitaire
      | `epers_diploma -> Epers_diploma
      | `epers_distinction -> Epers_distinction
      | `epers_dotation -> Epers_dotation
      | `epers_dotationlds -> Epers_dotationlds
      | `epers_education -> Epers_education
      | `epers_election -> Epers_election
      | `epers_emigration -> Epers_emigration
      | `epers_excommunication -> Epers_excommunication
      | `epers_familylinklds -> Epers_familylinklds
      | `epers_firstcommunion -> Epers_firstcommunion
      | `epers_funeral -> Epers_funeral
      | `epers_graduate -> Epers_graduate
      | `epers_hospitalisation -> Epers_hospitalisation
      | `epers_illness -> Epers_illness
      | `epers_immigration -> Epers_immigration
      | `epers_listepassenger -> Epers_listepassenger
      | `epers_militarydistinction -> Epers_militarydistinction
      | `epers_militarypromotion -> Epers_militarypromotion
      | `epers_militaryservice -> Epers_militaryservice
      | `epers_mobilisationmilitaire -> Epers_mobilisationmilitaire
      | `epers_naturalisation -> Epers_naturalisation
      | `epers_occupation -> Epers_occupation
      | `epers_ordination -> Epers_ordination
      | `epers_property -> Epers_property
      | `epers_recensement -> Epers_recensement
      | `epers_residence -> Epers_residence
      | `epers_retired -> Epers_retired
      | `epers_scellentchildlds -> Epers_scellentchildlds
      | `epers_scellentparentlds -> Epers_scellentparentlds
      | `epers_scellentspouselds -> Epers_scellentspouselds
      | `epers_ventebien -> Epers_ventebien
      | `epers_will -> Epers_will

    let transl_pevent_name
        Api_saisie_write_piqi.Transl_pevent_name.{ pos; sval } =
      let pos = pevent_name pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_pevent_name))

    let config_transl_pevent_name
        Api_saisie_write_piqi.Config_transl_pevent_name.{ msg } =
      let msg = List.map transl_pevent_name msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_pevent_name))

    let translate_access = function
      | `access_iftitles -> Api_saisie_write_protoc.Access_iftitles
      | `access_public -> Access_public
      | `access_private -> Access_private

    let transl_access Api_saisie_write_piqi.Transl_access.{ pos; sval } =
      let pos = translate_access pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_access))

    let config_transl_access Api_saisie_write_piqi.Config_transl_access.{ msg }
        =
      let msg = List.map transl_access msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_access))

    let update_warning_js = function
      | `empty_index -> Api_saisie_write_protoc.Empty_index
      | `empty_surname -> Empty_surname
      | `empty_first_name -> Empty_first_name
      | `empty_sex -> Empty_sex
      | `required_field -> Required_field
      | `birth_date_after_event -> Birth_date_after_event
      | `death_date_before_event -> Death_date_before_event

    let transl_update_warning_js
        Api_saisie_write_piqi.Transl_update_warning_js.{ pos; sval } =
      let pos = update_warning_js pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_update_warning_js))

    let config_transl_update_warning_js
        Api_saisie_write_piqi.Config_transl_update_warning_js.{ msg } =
      let msg = List.map transl_update_warning_js msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_update_warning_js))

    let short_greg_month = function
      | `janv -> Api_saisie_write_protoc.Janv
      | `fevr -> Fevr
      | `mars -> Mars
      | `avr -> Avr
      | `mai -> Mai
      | `juin -> Juin
      | `juil -> Juil
      | `aout -> Aout
      | `sept -> Sept
      | `oct -> Oct
      | `nov -> Nov
      | `dec -> Dec

    let transl_short_greg_month
        Api_saisie_write_piqi.Transl_short_greg_month.{ pos; sval } =
      let pos = short_greg_month pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_short_greg_month))

    let config_transl_short_greg_month
        Api_saisie_write_piqi.Config_transl_short_greg_month.{ msg } =
      let msg = List.map transl_short_greg_month msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_short_greg_month))

    let french_month = function
      | `vendemiaire -> Api_saisie_write_protoc.Vendemiaire
      | `brumaire -> Brumaire
      | `frimaire -> Frimaire
      | `nivose -> Nivose
      | `pluviose -> Pluviose
      | `ventose -> Ventose
      | `germinal -> Germinal
      | `floreal -> Floreal
      | `prairial -> Prairial
      | `messidor -> Messidor
      | `thermidor -> Thermidor
      | `fructidor -> Fructidor
      | `complementaire -> Complementaire

    let transl_french_month
        Api_saisie_write_piqi.Transl_french_month.{ pos; sval } =
      let pos = french_month pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_french_month))

    let config_transl_french_month
        Api_saisie_write_piqi.Config_transl_french_month.{ msg } =
      let msg = List.map transl_french_month msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_french_month))

    let hebrew_month = function
      | `tichri -> Api_saisie_write_protoc.Tichri
      | `marhechvan -> Marhechvan
      | `kislev -> Kislev
      | `tevet -> Tevet
      | `chevat -> Chevat
      | `adar_1 -> Adar_1
      | `adar_2 -> Adar_2
      | `nissan -> Nissan
      | `iyar -> Iyar
      | `sivan -> Sivan
      | `tamouz -> Tamouz
      | `av -> Av
      | `eloul -> Eloul

    let transl_hebrew_month
        Api_saisie_write_piqi.Transl_hebrew_month.{ pos; sval } =
      let pos = hebrew_month pos in
      Api_saisie_write_protoc.(({ pos; sval } : transl_hebrew_month))

    let config_transl_hebrew_month
        Api_saisie_write_piqi.Config_transl_hebrew_month.{ msg } =
      let msg = List.map transl_hebrew_month msg in
      Api_saisie_write_protoc.(({ msg } : config_transl_hebrew_month))

    let config
        Api_saisie_write_piqi.Config.
          {
            transl_cal;
            transl_wit;
            transl_prec;
            transl_death;
            transl_rel;
            transl_fevents;
            transl_pevents;
            transl_access;
            transl_warning;
            transl_short_greg_month;
            transl_french_month;
            transl_hebrew_month;
            gwf_place_format;
            gwf_place_format_placeholder;
          } =
      let transl_cal = Some (config_transl_calendar transl_cal) in
      let transl_wit = Some (config_transl_witness_type transl_wit) in
      let transl_prec = Some (config_transl_precision transl_prec) in
      let transl_death = Some (config_transl_death_type transl_death) in
      let transl_rel = Some (config_transl_relation_parent_type transl_rel) in
      let transl_fevents = Some (config_transl_fevent_name transl_fevents) in
      let transl_pevents = Some (config_transl_pevent_name transl_pevents) in
      let transl_access = Some (config_transl_access transl_access) in
      let transl_warning =
        Some (config_transl_update_warning_js transl_warning)
      in
      let transl_short_greg_month =
        Some (config_transl_short_greg_month transl_short_greg_month)
      in
      let transl_french_month =
        Some (config_transl_french_month transl_french_month)
      in
      let transl_hebrew_month =
        Some (config_transl_hebrew_month transl_hebrew_month)
      in
      Api_saisie_write_protoc.(
        ({
           transl_cal;
           transl_wit;
           transl_prec;
           transl_death;
           transl_rel;
           transl_fevents;
           transl_pevents;
           transl_access;
           transl_warning;
           transl_short_greg_month;
           transl_french_month;
           transl_hebrew_month;
           gwf_place_format;
           gwf_place_format_placeholder;
         }
          : config))

    let person_or_family = function
      | `person_form1 -> Api_saisie_write_protoc.Person_form1
      | `person_form2 -> Person_form2
      | `family_form -> Family_form

    let create_conflict
        Api_saisie_write_piqi.Create_conflict.
          {
            form;
            witness;
            rparents;
            event;
            pos;
            pos_witness;
            lastname;
            firstname;
          } =
      let form = Option.map person_or_family form in
      Api_saisie_write_protoc.(
        ({
           form;
           witness;
           rparents;
           event;
           pos;
           pos_witness;
           lastname;
           firstname;
         }
          : create_conflict))

    let translate_created_person
        Api_saisie_write_piqi.Created_person.{ n; p; oc } =
      Api_saisie_write_protoc.(({ n; p; oc } : created_person))

    let modification_status
        Api_saisie_write_piqi.Modification_status.
          {
            is_base_updated;
            base_warnings;
            base_miscs;
            index_person;
            lastname;
            firstname;
            occ;
            index_family;
            conflict;
            lastname_str;
            firstname_str;
            n;
            p;
            created_person;
          } =
      let conflict = Option.map create_conflict conflict in
      let created_person = Option.map translate_created_person created_person in
      Api_saisie_write_protoc.(
        ({
           is_base_updated;
           base_warnings;
           base_miscs;
           index_person;
           lastname;
           firstname;
           occ;
           index_family;
           conflict;
           lastname_str;
           firstname_str;
           n;
           p;
           created_person;
         }
          : modification_status))

    let create_or_link = function
      | `create -> Api_saisie_write_protoc.Create
      | `link -> Link
      | `create_default_occ -> Create_default_occ

    let precision = function
      | `sure -> Api_saisie_write_protoc.Sure
      | `about -> About
      | `maybe -> Maybe
      | `before -> Before
      | `after -> After
      | `oryear -> Oryear
      | `yearint -> Yearint

    let day_month_year Api_saisie_write_piqi.Dmy.{ day; month; year; delta } =
      Api_saisie_write_protoc.(({ day; month; year; delta } : dmy))

    let translate_date Api_saisie_write_piqi.Date.{ cal; prec; dmy; dmy2; text }
        =
      let cal = Option.map calendar cal in
      let prec = Option.map precision prec in
      let dmy = Option.map day_month_year dmy in
      let dmy2 = Option.map day_month_year dmy2 in
      Api_saisie_write_protoc.(({ cal; prec; dmy; dmy2; text } : date))

    let translate_title
        Api_saisie_write_piqi.Title.
          { name; title; fief; date_begin; date_end; nth } =
      let date_begin = Option.map translate_date date_begin in
      let date_end = Option.map translate_date date_end in
      Api_saisie_write_protoc.(
        ({ name; title; fief; date_begin; date_end; nth } : title))

    let person_link
        Api_saisie_write_piqi.Person_link.
          { create_link; index; sex; lastname; firstname; occ; dates } =
      let create_link = create_or_link create_link in
      let sex = translate_sex sex in
      Api_saisie_write_protoc.(
        ({ create_link; index; sex; lastname; firstname; occ; dates }
          : person_link))

    let translate_witness
        Api_saisie_write_piqi.Witness.{ witness_type; person; witness_note } =
      let witness_type = translate_witness_type witness_type in
      let person = Option.map person_link person in
      Api_saisie_write_protoc.(
        ({ witness_type; person; witness_note } : witness))

    let translate_pevent
        Api_saisie_write_piqi.Pevent.
          {
            pevent_type;
            date;
            place;
            reason;
            note;
            src;
            witnesses;
            event_perso;
          } =
      let pevent_type = Option.map pevent_name pevent_type in
      let date = Option.map translate_date date in
      let witnesses = List.map translate_witness witnesses in
      Api_saisie_write_protoc.(
        ({ pevent_type; date; place; reason; note; src; witnesses; event_perso }
          : pevent))

    let translate_relation_parent
        Api_saisie_write_piqi.Relation_parent.{ person; source; rpt_type } =
      let rpt_type = relation_parent_type rpt_type in
      let person = Option.map person_link person in
      Api_saisie_write_protoc.(({ person; source; rpt_type } : relation_parent))

    let person
        Api_saisie_write_piqi.Person.
          {
            digest;
            create_link;
            index;
            sex;
            lastname;
            firstname;
            occ;
            public_name;
            aliases;
            qualifiers;
            firstname_aliases;
            surname_aliases;
            image;
            death_type;
            occupation;
            psources;
            notes;
            titles;
            pevents;
            related;
            rparents;
            access;
            parents;
            families;
            is_contemporary;
            name_is_hidden;
            name_is_restricted;
          } =
      let create_link = create_or_link create_link in
      let sex = translate_sex sex in
      let death_type = translate_death_type death_type in
      let titles = List.map translate_title titles in
      let pevents = List.map translate_pevent pevents in
      let rparents = List.map translate_relation_parent rparents in
      let access = Some (translate_access access) in
      Api_saisie_write_protoc.(
        ({
           digest;
           create_link;
           index;
           sex;
           lastname;
           firstname;
           occ;
           public_name;
           aliases;
           qualifiers;
           firstname_aliases;
           surname_aliases;
           image;
           death_type;
           occupation;
           psources;
           notes;
           titles;
           pevents;
           related;
           rparents;
           access;
           parents;
           families;
           is_contemporary;
           name_is_hidden;
           name_is_restricted;
         }
          : person))

    let translate_fevent
        Api_saisie_write_piqi.Fevent.
          {
            fevent_type;
            date;
            place;
            reason;
            note;
            src;
            witnesses;
            event_perso;
          } =
      let fevent_type = Option.map fevent_name fevent_type in
      let date = Option.map translate_date date in
      let witnesses = List.map translate_witness witnesses in
      Api_saisie_write_protoc.(
        ({ fevent_type; date; place; reason; note; src; witnesses; event_perso }
          : fevent))

    let translate_family
        Api_saisie_write_piqi.Family.
          {
            digest;
            index;
            fevents;
            fsources;
            origin_file;
            comment;
            father;
            mother;
            children;
            old_witnesses;
          } =
      let fevents = List.map translate_fevent fevents in
      let father = Some (person father) in
      let mother = Some (person mother) in
      let children = List.map person_link children in
      Api_saisie_write_protoc.(
        ({
           digest;
           index;
           fevents;
           fsources;
           origin_file;
           comment;
           father;
           mother;
           children;
           old_witnesses;
         }
          : family))

    let add_family
        Api_saisie_write_piqi.Add_family.
          { person_lastname; person_firstname; family } =
      let family = Some (translate_family family) in
      Api_saisie_write_protoc.(
        ({ person_lastname; person_firstname; family } : add_family))

    let edit_family
        Api_saisie_write_piqi.Edit_family.
          { person_lastname; person_firstname; family } =
      let family = Some (translate_family family) in
      Api_saisie_write_protoc.(
        ({ person_lastname; person_firstname; family } : edit_family))

    let translate_family_spouse
        Api_saisie_write_piqi.Family_spouse.
          {
            index_family;
            index_person;
            sex;
            lastname;
            firstname;
            dates;
            image;
            sosa;
          } =
      let sex = translate_sex sex in
      let sosa = translate_sosa sosa in
      Api_saisie_write_protoc.(
        ({
           index_family;
           index_person;
           sex;
           lastname;
           firstname;
           dates;
           image;
           sosa;
         }
          : family_spouse))

    let edit_family_request
        Api_saisie_write_piqi.Edit_family_request.{ spouses; first_family } =
      let spouses = List.map translate_family_spouse spouses in
      let first_family = Option.map edit_family first_family in
      Api_saisie_write_protoc.(
        ({ spouses; first_family } : edit_family_request))

    let edit_family_ok
        Api_saisie_write_piqi.Edit_family_ok.{ index_person; family } =
      let family = Some (translate_family family) in
      Api_saisie_write_protoc.(({ index_person; family } : edit_family_ok))

    let add_parents
        Api_saisie_write_piqi.Add_parents.
          { person_lastname; person_firstname; family } =
      let family = Some (translate_family family) in
      Api_saisie_write_protoc.(
        ({ person_lastname; person_firstname; family } : add_parents))

    let add_child
        Api_saisie_write_piqi.Add_child.
          { person_lastname; person_firstname; family_spouse; child } =
      let family_spouse = List.map translate_family_spouse family_spouse in
      let child = Some (person child) in
      Api_saisie_write_protoc.(
        ({ person_lastname; person_firstname; family_spouse; child }
          : add_child))

    let add_sibling
        Api_saisie_write_piqi.Add_sibling.
          { person_lastname; person_firstname; sibling } =
      let sibling = Some (person sibling) in
      Api_saisie_write_protoc.(
        ({ person_lastname; person_firstname; sibling } : add_sibling))
  end

  module ProtocToPiqi = struct
    let auto_complete_field = function
      | Api_saisie_write_protoc.Lastname -> `lastname
      | Firstname -> `firstname
      | Place -> `place
      | Source -> `source
      | Occupation -> `occupation

    let auto_complete_place_field = function
      | Api_saisie_write_protoc.Subdivision -> `subdivision
      | Town -> `town
      | Area_code -> `area_code
      | County -> `county
      | Region -> `region
      | Country -> `country

    let auto_complete
        Api_saisie_write_protoc.(
          ({ field; place_field; input; limit } : auto_complete)) =
      let field = auto_complete_field field in
      let place_field = Option.map auto_complete_place_field place_field in
      Api_saisie_write_piqi.Auto_complete.{ field; place_field; input; limit }

    (*


    let translate_sosa = function
      | Api_saisie_write_protoc.Sosa_ref -> `sosa_ref
      | Sosa -> `sosa 
      | No_sosa  -> `no_sosa
    
    let translate_person_search Api_saisie_write_protoc.(({
        index;
        sex;
        lastname;
        firstname;
        dates;
        image;
        sosa;
        family
      } : person_search))=
      let sex = translate_sex sex in
      let sosa = translate_sosa sosa in
      Api_saisie_write_piqi.Person_search.{
          index;
          sex;
          lastname;
          firstname;
          dates;
          image;
          sosa;
          family
        }
  *)
    (*let person_search_list  Api_saisie_write_protoc.(({persons} : person_search_list)) =
      let persons = List.map translate_person_search persons in
      Api_saisie_write_piqi.Person_search_list.{persons}
    *)
    let person_search_list_params
        Api_saisie_write_protoc.(
          ({ lastname; firstname; limit } : person_search_list_params)) =
      Api_saisie_write_piqi.Person_search_list_params.
        { lastname; firstname; limit }

    let index_person Api_saisie_write_protoc.(({ index } : index_person)) =
      Api_saisie_write_piqi.Index_person.{ index }

    let translate_sex = function
      | Api_saisie_write_protoc.Male -> `male
      | Female -> `female
      | Unknown -> `unknown

    let translate_death_type = function
      | Api_saisie_write_protoc.Not_dead -> `not_dead
      | Dead -> `dead
      | Dead_young -> `dead_young
      | Dead_dont_know_when -> `dead_dont_know_when
      | Dont_know_if_dead -> `dont_know_if_dead
      | Of_course_dead -> `of_course_dead

    let create_or_link = function
      | Api_saisie_write_protoc.Create -> `create
      | Link -> `link
      | Create_default_occ -> `create_default_occ

    let precision = function
      | Api_saisie_write_protoc.Sure -> `sure
      | About -> `about
      | Maybe -> `maybe
      | Before -> `before
      | After -> `after
      | Oryear -> `oryear
      | Yearint -> `yearint

    let calendar = function
      | Api_saisie_write_protoc.Gregorian -> `gregorian
      | Julian -> `julian
      | French -> `french
      | Hebrew -> `hebrew

    let day_month_year
        Api_saisie_write_protoc.(({ day; month; year; delta } : dmy)) =
      Api_saisie_write_piqi.Dmy.{ day; month; year; delta }

    let translate_date
        Api_saisie_write_protoc.(({ cal; prec; dmy; dmy2; text } : date)) =
      let cal = Option.map calendar cal in
      let prec = Option.map precision prec in
      let dmy = Option.map day_month_year dmy in
      let dmy2 = Option.map day_month_year dmy2 in
      Api_saisie_write_piqi.Date.{ cal; prec; dmy; dmy2; text }

    let translate_title
        Api_saisie_write_protoc.(
          ({ name; title; fief; date_begin; date_end; nth } : title)) =
      let date_begin = Option.map translate_date date_begin in
      let date_end = Option.map translate_date date_end in
      Api_saisie_write_piqi.Title.
        { name; title; fief; date_begin; date_end; nth }

    let pevent_name = function
      | Api_saisie_write_protoc.Epers_birth -> `epers_birth
      | Epers_baptism -> `epers_baptism
      | Epers_death -> `epers_death
      | Epers_burial -> `epers_burial
      | Epers_cremation -> `epers_cremation
      | Epers_accomplishment -> `epers_accomplishment
      | Epers_acquisition -> `epers_acquisition
      | Epers_adhesion -> `epers_adhesion
      | Epers_baptismlds -> `epers_baptismlds
      | Epers_barmitzvah -> `epers_barmitzvah
      | Epers_batmitzvah -> `epers_batmitzvah
      | Epers_benediction -> `epers_benediction
      | Epers_changename -> `epers_changename
      | Epers_circumcision -> `epers_circumcision
      | Epers_confirmation -> `epers_confirmation
      | Epers_confirmationlds -> `epers_confirmationlds
      | Epers_decoration -> `epers_decoration
      | Epers_demobilisationmilitaire -> `epers_demobilisationmilitaire
      | Epers_diploma -> `epers_diploma
      | Epers_distinction -> `epers_distinction
      | Epers_dotation -> `epers_dotation
      | Epers_dotationlds -> `epers_dotationlds
      | Epers_education -> `epers_education
      | Epers_election -> `epers_election
      | Epers_emigration -> `epers_emigration
      | Epers_excommunication -> `epers_excommunication
      | Epers_familylinklds -> `epers_familylinklds
      | Epers_firstcommunion -> `epers_firstcommunion
      | Epers_funeral -> `epers_funeral
      | Epers_graduate -> `epers_graduate
      | Epers_hospitalisation -> `epers_hospitalisation
      | Epers_illness -> `epers_illness
      | Epers_immigration -> `epers_immigration
      | Epers_listepassenger -> `epers_listepassenger
      | Epers_militarydistinction -> `epers_militarydistinction
      | Epers_militarypromotion -> `epers_militarypromotion
      | Epers_militaryservice -> `epers_militaryservice
      | Epers_mobilisationmilitaire -> `epers_mobilisationmilitaire
      | Epers_naturalisation -> `epers_naturalisation
      | Epers_occupation -> `epers_occupation
      | Epers_ordination -> `epers_ordination
      | Epers_property -> `epers_property
      | Epers_recensement -> `epers_recensement
      | Epers_residence -> `epers_residence
      | Epers_retired -> `epers_retired
      | Epers_scellentchildlds -> `epers_scellentchildlds
      | Epers_scellentparentlds -> `epers_scellentparentlds
      | Epers_scellentspouselds -> `epers_scellentspouselds
      | Epers_ventebien -> `epers_ventebien
      | Epers_will -> `epers_will

    let translate_witness_type = function
      | Api_saisie_write_protoc.Witness -> `witness
      | Witness_godparent -> `witness_godparent
      | Witness_civilofficer -> `witness_civilofficer
      | Witness_religiousofficer -> `witness_religiousofficer
      | Witness_informant -> `witness_informant
      | Witness_attending -> `witness_attending
      | Witness_mentioned -> `witness_mentioned
      | Witness_other -> `witness_other

    let person_link
        Api_saisie_write_protoc.(
          ({ create_link; index; sex; lastname; firstname; occ; dates } :
            person_link)) =
      let create_link = create_or_link create_link in
      let sex = translate_sex sex in
      Api_saisie_write_piqi.Person_link.
        { create_link; index; sex; lastname; firstname; occ; dates }

    let translate_witness
        Api_saisie_write_protoc.(
          ({ witness_type; person; witness_note } : witness)) =
      let witness_type = translate_witness_type witness_type in
      let person = Option.map person_link person in
      Api_saisie_write_piqi.Witness.{ witness_type; person; witness_note }

    let translate_pevent
        Api_saisie_write_protoc.(
          ({
             pevent_type;
             date;
             place;
             reason;
             note;
             src;
             witnesses;
             event_perso;
           } :
            pevent)) =
      let pevent_type = Option.map pevent_name pevent_type in
      let date = Option.map translate_date date in
      let witnesses = List.map translate_witness witnesses in
      Api_saisie_write_piqi.Pevent.
        { pevent_type; date; place; reason; note; src; witnesses; event_perso }

    let relation_parent_type = function
      | Api_saisie_write_protoc.Rpt_adoption_father -> `rpt_adoption_father
      | Rpt_adoption_mother -> `rpt_adoption_mother
      | Rpt_recognition_father -> `rpt_recognition_father
      | Rpt_recognition_mother -> `rpt_recognition_mother
      | Rpt_candidate_parent_father -> `rpt_candidate_parent_father
      | Rpt_candidate_parent_mother -> `rpt_candidate_parent_mother
      | Rpt_god_parent_father -> `rpt_god_parent_father
      | Rpt_god_parent_mother -> `rpt_god_parent_mother
      | Rpt_foster_parent_father -> `rpt_foster_parent_father
      | Rpt_foster_parent_mother -> `rpt_foster_parent_mother

    let translate_relation_parent
        Api_saisie_write_protoc.(
          ({ person; source; rpt_type } : relation_parent)) =
      let rpt_type = relation_parent_type rpt_type in
      let person = Option.map person_link person in
      Api_saisie_write_piqi.Relation_parent.{ person; source; rpt_type }

    let translate_access = function
      | Api_saisie_write_protoc.Access_iftitles -> `access_iftitles
      | Access_public -> `access_public
      | Access_private -> `access_private

    let person
        Api_saisie_write_protoc.(
          ({
             digest;
             create_link;
             index;
             sex;
             lastname;
             firstname;
             occ;
             public_name;
             aliases;
             qualifiers;
             firstname_aliases;
             surname_aliases;
             image;
             death_type;
             occupation;
             psources;
             notes;
             titles;
             pevents;
             related;
             rparents;
             access;
             parents;
             families;
             is_contemporary;
             name_is_hidden;
             name_is_restricted;
           } :
            person)) =
      let sex = translate_sex sex in
      let death_type = translate_death_type death_type in
      let create_link = create_or_link create_link in
      let titles = List.map translate_title titles in
      let pevents = List.map translate_pevent pevents in
      let rparents = List.map translate_relation_parent rparents in
      let access = translate_access (required access) in
      Api_saisie_write_piqi.Person.
        {
          digest;
          create_link;
          index;
          sex;
          lastname;
          firstname;
          occ;
          public_name;
          aliases;
          qualifiers;
          firstname_aliases;
          surname_aliases;
          image;
          death_type;
          occupation;
          psources;
          notes;
          titles;
          pevents;
          related;
          rparents;
          access;
          parents;
          families;
          is_contemporary;
          name_is_hidden;
          name_is_restricted;
        }

    let index_person_and_family
        Api_saisie_write_protoc.(
          ({ index_person; index_family } : index_person_and_family)) =
      Api_saisie_write_piqi.Index_person_and_family.
        { index_person; index_family }

    let fevent_name = function
      | Api_saisie_write_protoc.Efam_marriage -> `efam_marriage
      | Efam_no_marriage -> `efam_no_marriage
      | Efam_no_mention -> `efam_no_mention
      | Efam_engage -> `efam_engage
      | Efam_divorce -> `efam_divorce
      | Efam_separated -> `efam_separated
      | Efam_annulation -> `efam_annulation
      | Efam_marriage_bann -> `efam_marriage_bann
      | Efam_marriage_contract -> `efam_marriage_contract
      | Efam_marriage_license -> `efam_marriage_license
      | Efam_pacs -> `efam_pacs
      | Efam_residence -> `efam_residence

    let translate_fevent
        Api_saisie_write_protoc.(
          ({
             fevent_type;
             date;
             place;
             reason;
             note;
             src;
             witnesses;
             event_perso;
           } :
            fevent)) =
      let fevent_type = Option.map fevent_name fevent_type in
      let date = Option.map translate_date date in
      let witnesses = List.map translate_witness witnesses in
      Api_saisie_write_piqi.Fevent.
        { fevent_type; date; place; reason; note; src; witnesses; event_perso }

    let translate_family
        Api_saisie_write_protoc.(
          ({
             digest;
             index;
             fevents;
             fsources;
             origin_file;
             comment;
             father;
             mother;
             children;
             old_witnesses;
           } :
            family)) =
      let fevents = List.map translate_fevent fevents in
      let father = person (required father) in
      let mother = person (required mother) in
      let children = List.map person_link children in
      Api_saisie_write_piqi.Family.
        {
          digest;
          index;
          fevents;
          fsources;
          origin_file;
          comment;
          father;
          mother;
          children;
          old_witnesses;
        }

    let add_family_ok
        Api_saisie_write_protoc.(({ index_person; family } : add_family_ok)) =
      let family = translate_family (required family) in
      Api_saisie_write_piqi.Add_family_ok.{ index_person; family }

    let add_child_request
        Api_saisie_write_protoc.(
          ({ index; index_family; sex } : add_child_request)) =
      let sex = Option.map translate_sex sex in
      Api_saisie_write_piqi.Add_child_request.{ index; index_family; sex }

    let edit_family_ok
        Api_saisie_write_protoc.(({ index_person; family } : edit_family_ok)) =
      let family = translate_family (required family) in
      Api_saisie_write_piqi.Edit_family_ok.{ index_person; family }

    let add_child_ok
        Api_saisie_write_protoc.(
          ({ index_person; index_family; new_family; child } : add_child_ok)) =
      let child = person (required child) in
      Api_saisie_write_piqi.Add_child_ok.
        { index_person; index_family; new_family; child }

    let add_parents_ok
        Api_saisie_write_protoc.(({ index_person; family } : add_parents_ok)) =
      let family = translate_family (required family) in
      Api_saisie_write_piqi.Add_parents_ok.{ index_person; family }

    let add_sibling_request
        Api_saisie_write_protoc.(({ index; sex } : add_sibling_request)) =
      let sex = Option.map translate_sex sex in
      Api_saisie_write_piqi.Add_sibling_request.{ index; sex }

    let add_sibling_ok
        Api_saisie_write_protoc.(({ index_person; sibling } : add_sibling_ok)) =
      let sibling = person (required sibling) in
      Api_saisie_write_piqi.Add_sibling_ok.{ index_person; sibling }

    let add_first_fam
        Api_saisie_write_protoc.(
          ({ sosa; father; mother; spouse; children } : add_first_fam)) =
      let sosa = person (required sosa) in
      let father = person (required father) in
      let mother = person (required mother) in
      let spouse = person (required spouse) in
      let children = List.map person children in
      Api_saisie_write_piqi.Add_first_fam.
        { sosa; father; mother; spouse; children }
  end
end

module Api_stats = struct
  module PiqiToProtoc = struct
    let translate_title = function
      | `st_ind_longevity -> Api_stats_protoc.St_ind_longevity
      | `st_ind_birth_month -> St_ind_birth_month
      | `st_ind_parent_age -> St_ind_parent_age
      | `st_ind_lastname -> St_ind_lastname
      | `st_ind_firstname -> St_ind_firstname
      | `st_ind_occupation -> St_ind_occupation
      | `st_ind_younguest_parent -> St_ind_younguest_parent
      | `st_ind_oldest -> St_ind_oldest
      | `st_ind_astro -> St_ind_astro
      | `st_ind_moon -> St_ind_moon
      | `st_fam_first_marr_parent_age -> St_fam_first_marr_parent_age
      | `st_fam_marr_day -> St_fam_marr_day
      | `st_fam_marr_month -> St_fam_marr_month
      | `st_fam_avg_marr_nb -> St_fam_avg_marr_nb
      | `st_fam_avg_marr_duration -> St_fam_avg_marr_duration
      | `st_fam_avg_nb_children -> St_fam_avg_nb_children
      | `st_fam_int_btw_children -> St_fam_int_btw_children
      | `st_fam_diff_age_btw_children -> St_fam_diff_age_btw_children
      | `st_fam_diff_age_btw_cpl -> St_fam_diff_age_btw_cpl
      | `st_fam_longuest_marr -> St_fam_longuest_marr
      | `st_fam_shortest_marr -> St_fam_shortest_marr
      | `st_asc -> St_asc
      | `st_desc -> St_desc
      | `st_desc_man_woman -> St_desc_man_woman
      | `st_asc_lastname -> St_asc_lastname
      | `st_asc_firstname -> St_asc_firstname
      | `st_asc_occupation -> St_asc_occupation
      | `st_desc_lastname -> St_desc_lastname
      | `st_desc_firstname -> St_desc_firstname
      | `st_desc_occupation -> St_desc_occupation

    let translate_serie = function
      | `serie_male -> Api_stats_protoc.Serie_male
      | `serie_female -> Serie_female
      | `serie_month_1 -> Serie_month_1
      | `serie_month_2 -> Serie_month_2
      | `serie_month_3 -> Serie_month_3
      | `serie_month_4 -> Serie_month_4
      | `serie_month_5 -> Serie_month_5
      | `serie_month_6 -> Serie_month_6
      | `serie_month_7 -> Serie_month_7
      | `serie_month_8 -> Serie_month_8
      | `serie_month_9 -> Serie_month_9
      | `serie_month_10 -> Serie_month_10
      | `serie_month_11 -> Serie_month_11
      | `serie_month_12 -> Serie_month_12
      | `serie_day_1 -> Serie_day_1
      | `serie_day_2 -> Serie_day_2
      | `serie_day_3 -> Serie_day_3
      | `serie_day_4 -> Serie_day_4
      | `serie_day_5 -> Serie_day_5
      | `serie_day_6 -> Serie_day_6
      | `serie_day_7 -> Serie_day_7
      | `serie_male_age_first_child -> Serie_male_age_first_child
      | `serie_male_age_last_child -> Serie_male_age_last_child
      | `serie_female_age_first_child -> Serie_female_age_first_child
      | `serie_female_age_last_child -> Serie_female_age_last_child
      | `serie_all -> Serie_all
      | `serie_asc_found -> Serie_asc_found
      | `serie_asc_uniq -> Serie_asc_uniq
      | `serie_desc_found -> Serie_desc_found
      | `serie_desc_uniq -> Serie_desc_uniq
      | `serie_top_10_1 -> Serie_top_10_1
      | `serie_top_10_2 -> Serie_top_10_2
      | `serie_top_10_3 -> Serie_top_10_3
      | `serie_top_10_4 -> Serie_top_10_4
      | `serie_top_10_5 -> Serie_top_10_5
      | `serie_top_10_6 -> Serie_top_10_6
      | `serie_top_10_7 -> Serie_top_10_7
      | `serie_top_10_8 -> Serie_top_10_8
      | `serie_top_10_9 -> Serie_top_10_9
      | `serie_top_10_10 -> Serie_top_10_10
      | `serie_aries -> Serie_aries
      | `serie_taurus -> Serie_taurus
      | `serie_gemini -> Serie_gemini
      | `serie_cancer -> Serie_cancer
      | `serie_leo -> Serie_leo
      | `serie_virgo -> Serie_virgo
      | `serie_libra -> Serie_libra
      | `serie_scorpio -> Serie_scorpio
      | `serie_sagittarius -> Serie_sagittarius
      | `serie_capricorn -> Serie_capricorn
      | `serie_aquarius -> Serie_aquarius
      | `serie_pisces -> Serie_pisces
      | `serie_moon_new -> Serie_moon_new
      | `serie_moon_first_quarter -> Serie_moon_first_quarter
      | `serie_moon_full -> Serie_moon_full
      | `serie_moon_last_quarter -> Serie_moon_last_quarter

    let translate_data Api_stats_piqi.Data.{ nb; value } =
      Api_stats_protoc.(({ nb; value } : data))

    let translate_data_l Api_stats_piqi.Data_l.{ data } =
      let data = List.map translate_data data in
      Api_stats_protoc.(({ data } : data_l))

    let translate_stat
        Api_stats_piqi.Stat.{ title; labels; series; series_string; datas } =
      let title = translate_title title in
      let series = List.map translate_serie series in
      let datas = List.map translate_data_l datas in
      Api_stats_protoc.(
        ({ title; labels; series; series_string; datas } : stat))

    let stats Api_stats_piqi.Stats.{ stats } =
      let stats = List.map translate_stat stats in
      Api_stats_protoc.(({ stats } : stats))
  end

  module ProtocToPiqi = struct
    let stats_params Api_stats_protoc.(({ i } : stats_params)) =
      Api_stats_piqi.Stats_params.{ i }
  end
end

module Api_saisie_read = struct
  module PiqiToProtoc = struct
    let translate_sex = function
      | `male -> Api_saisie_read_protoc.Male
      | `female -> Female
      | `unknown -> Unknown

    let person_type = function
      | `simple -> Api_saisie_read_protoc.Simple
      | `full -> Full
      | `fiche -> Fiche

    let calendar = function
      | `gregorian -> Api_saisie_read_protoc.Gregorian
      | `julian -> Julian
      | `french -> French
      | `hebrew -> Hebrew

    let translate_death_type = function
      | `not_dead -> Api_saisie_read_protoc.Not_dead
      | `dead -> Dead
      | `dead_young -> Dead_young
      | `dead_dont_know_when -> Dead_dont_know_when
      | `dont_know_if_dead -> Dont_know_if_dead
      | `of_course_dead -> Of_course_dead

    let translate_relation_type = function
      | `rparent_adoption -> Api_saisie_read_protoc.Rparent_adoption
      | `rparent_recognition -> Rparent_recognition
      | `rparent_candidate_parent -> Rparent_candidate_parent
      | `rparent_god_parent -> Rparent_god_parent
      | `rparent_foster_parent -> Rparent_foster_parent
      | `rchild_adoption -> Rchild_adoption
      | `rchild_recognition -> Rchild_recognition
      | `rchild_candidate_parent -> Rchild_candidate_parent
      | `rchild_god_parent -> Rchild_god_parent
      | `rchild_foster_parent -> Rchild_foster_parent

    let translate_sosa = function
      | `sosa_ref -> Api_saisie_read_protoc.Sosa_ref
      | `sosa -> Sosa
      | `no_sosa -> No_sosa

    let translate_visibility = function
      | `visibility_public -> Api_saisie_read_protoc.Visibility_public
      | `visibility_semi_public -> Visibility_semi_public
      | `visibility_private -> Visibility_private

    let simple_person
        Api_saisie_read_piqi.Simple_person.
          {
            index;
            sex;
            lastname;
            firstname;
            n;
            p;
            occ;
            birth_short_date;
            birth_date_raw;
            birth_place;
            death_short_date;
            death_date_raw;
            death_place;
            image;
            sosa;
            baseprefix;
            sosa_nb;
            visible_for_visitors;
            has_parent;
            has_spouse;
            has_child;
            is_contemporary;
            name_is_hidden;
            name_is_restricted;
          } =
      let sex = translate_sex sex in
      let sosa = translate_sosa sosa in
      let visible_for_visitors = translate_visibility visible_for_visitors in
      Api_saisie_read_protoc.(
        ({
           index;
           sex;
           lastname;
           firstname;
           n;
           p;
           occ;
           birth_short_date;
           birth_date_raw;
           birth_place;
           death_short_date;
           death_date_raw;
           death_place;
           image;
           sosa;
           baseprefix;
           sosa_nb;
           visible_for_visitors;
           has_parent;
           has_spouse;
           has_child;
           is_contemporary;
           name_is_hidden;
           name_is_restricted;
         }
          : simple_person))

    let relation_person Api_saisie_read_piqi.Relation_person.{ r_type; person }
        =
      let r_type = translate_relation_type r_type in
      let person = Some (simple_person person) in
      Api_saisie_read_protoc.(({ r_type; person } : relation_person))

    let translate_marriage_type = function
      | `married -> Api_saisie_read_protoc.Married
      | `not_married -> Not_married
      | `engaged -> Engaged
      | `no_sexes_check_not_married -> No_sexes_check_not_married
      | `no_mention -> No_mention
      | `no_sexes_check_married -> No_sexes_check_married
      | `marriage_bann -> Marriage_bann
      | `marriage_contract -> Marriage_contract
      | `marriage_license -> Marriage_license
      | `pacs -> Pacs
      | `residence -> Residence

    let translate_divorce_type = function
      | `not_divorced -> Api_saisie_read_protoc.Not_divorced
      | `divorced -> Divorced
      | `separated -> Separated

    let translate_witness_type = function
      | `witness -> Api_saisie_read_protoc.Witness
      | `witness_godparent -> Witness_godparent
      | `witness_civilofficer -> Witness_civilofficer
      | `witness_religiousofficer -> Witness_religiousofficer
      | `witness_informant -> Witness_informant
      | `witness_attending -> Witness_attending
      | `witness_mentioned -> Witness_mentioned
      | `witness_other -> Witness_other

    let witness_event
        Api_saisie_read_piqi.Witness_event.
          { witness_type; witness; witness_note } =
      let witness_type = translate_witness_type witness_type in
      let witness = Some (simple_person witness) in
      Api_saisie_read_protoc.(
        ({ witness_type; witness; witness_note } : witness_event))

    let translate_family
        Api_saisie_read_piqi.Family.
          {
            index;
            spouse;
            marriage_date;
            marriage_date_long;
            marriage_date_raw;
            marriage_date_conv;
            marriage_date_conv_long;
            marriage_date_text;
            marriage_date_cal;
            marriage_place;
            marriage_src;
            marriage_type;
            divorce_type;
            divorce_date;
            divorce_date_long;
            divorce_date_raw;
            divorce_date_conv;
            divorce_date_conv_long;
            divorce_date_cal;
            witnesses;
            notes;
            fsources;
            children;
          } =
      let spouse = Some (simple_person spouse) in
      let marriage_date_cal = Option.map calendar marriage_date_cal in
      let marriage_type = translate_marriage_type marriage_type in
      let divorce_type = translate_divorce_type divorce_type in
      let divorce_date_cal = Option.map calendar divorce_date_cal in
      let witnesses = List.map witness_event witnesses in
      let children = List.map simple_person children in
      Api_saisie_read_protoc.(
        ({
           index;
           spouse;
           marriage_date;
           marriage_date_long;
           marriage_date_raw;
           marriage_date_conv;
           marriage_date_conv_long;
           marriage_date_text;
           marriage_date_cal;
           marriage_place;
           marriage_src;
           marriage_type;
           divorce_type;
           divorce_date;
           divorce_date_long;
           divorce_date_raw;
           divorce_date_conv;
           divorce_date_conv_long;
           divorce_date_cal;
           witnesses;
           notes;
           fsources;
           children;
         }
          : family))

    let event_type = function
      | `epers_birth -> Api_saisie_read_protoc.Epers_birth
      | `epers_baptism -> Epers_baptism
      | `epers_death -> Epers_death
      | `epers_burial -> Epers_burial
      | `epers_cremation -> Epers_cremation
      | `epers_accomplishment -> Epers_accomplishment
      | `epers_acquisition -> Epers_acquisition
      | `epers_adhesion -> Epers_adhesion
      | `epers_baptismlds -> Epers_baptismlds
      | `epers_barmitzvah -> Epers_barmitzvah
      | `epers_batmitzvah -> Epers_batmitzvah
      | `epers_benediction -> Epers_benediction
      | `epers_changename -> Epers_changename
      | `epers_circumcision -> Epers_circumcision
      | `epers_confirmation -> Epers_confirmation
      | `epers_confirmationlds -> Epers_confirmationlds
      | `epers_decoration -> Epers_decoration
      | `epers_demobilisationmilitaire -> Epers_demobilisationmilitaire
      | `epers_diploma -> Epers_diploma
      | `epers_distinction -> Epers_distinction
      | `epers_dotation -> Epers_dotation
      | `epers_dotationlds -> Epers_dotationlds
      | `epers_education -> Epers_education
      | `epers_election -> Epers_election
      | `epers_emigration -> Epers_emigration
      | `epers_excommunication -> Epers_excommunication
      | `epers_familylinklds -> Epers_familylinklds
      | `epers_firstcommunion -> Epers_firstcommunion
      | `epers_funeral -> Epers_funeral
      | `epers_graduate -> Epers_graduate
      | `epers_hospitalisation -> Epers_hospitalisation
      | `epers_illness -> Epers_illness
      | `epers_immigration -> Epers_immigration
      | `epers_listepassenger -> Epers_listepassenger
      | `epers_militarydistinction -> Epers_militarydistinction
      | `epers_militarypromotion -> Epers_militarypromotion
      | `epers_militaryservice -> Epers_militaryservice
      | `epers_mobilisationmilitaire -> Epers_mobilisationmilitaire
      | `epers_naturalisation -> Epers_naturalisation
      | `epers_occupation -> Epers_occupation
      | `epers_ordination -> Epers_ordination
      | `epers_property -> Epers_property
      | `epers_recensement -> Epers_recensement
      | `epers_residence -> Epers_residence
      | `epers_retired -> Epers_retired
      | `epers_scellentchildlds -> Epers_scellentchildlds
      | `epers_scellentparentlds -> Epers_scellentparentlds
      | `epers_scellentspouselds -> Epers_scellentspouselds
      | `epers_ventebien -> Epers_ventebien
      | `epers_will -> Epers_will
      | `epers_custom -> Epers_custom
      | `efam_marriage -> Efam_marriage
      | `efam_no_marriage -> Efam_no_marriage
      | `efam_no_mention -> Efam_no_mention
      | `efam_engage -> Efam_engage
      | `efam_divorce -> Efam_divorce
      | `efam_separated -> Efam_separated
      | `efam_annulation -> Efam_annulation
      | `efam_marriage_bann -> Efam_marriage_bann
      | `efam_marriage_contract -> Efam_marriage_contract
      | `efam_marriage_license -> Efam_marriage_license
      | `efam_pacs -> Efam_pacs
      | `efam_residence -> Efam_residence
      | `efam_custom -> Efam_custom

    let translate_event
        Api_saisie_read_piqi.Event.
          {
            name;
            type_;
            date;
            date_long;
            date_raw;
            date_conv;
            date_conv_long;
            date_cal;
            place;
            reason;
            note;
            src;
            spouse;
            witnesses;
          } =
      let type_ = event_type type_ in
      let date_cal = Option.map calendar date_cal in
      let spouse = Option.map simple_person spouse in
      let witnesses = List.map witness_event witnesses in
      Api_saisie_read_protoc.(
        ({
           name;
           type_;
           date;
           date_long;
           date_raw;
           date_conv;
           date_conv_long;
           date_cal;
           place;
           reason;
           note;
           src;
           spouse;
           witnesses;
         }
          : event))

    let event_witness
        Api_saisie_read_piqi.Event_witness.
          { event_witness_type; husband; wife; witness_note } =
      let husband = Some (simple_person husband) in
      let wife = Option.map simple_person wife in
      Api_saisie_read_protoc.(
        ({ event_witness_type; husband; wife; witness_note } : event_witness))

    let translate_burial_type = function
      | `dont_know -> Api_saisie_read_protoc.Dont_know
      | `buried -> Buried
      | `cremated -> Cremated

    let fiche_person person_protoc
        Api_saisie_read_piqi.Fiche_person.
          {
            birth_date_raw;
            birth_text;
            baptism_date_raw;
            baptism_text;
            death_date_raw;
            death_text;
            burial_date_raw;
            burial_text;
            cremation_text;
            burial_type;
            titles_links;
            sosa_nb;
            has_history;
            has_possible_duplications;
            linked_page_biblio;
            linked_page_bnote;
            linked_page_death;
            linked_page_head;
            linked_page_occu;
            visible_for_visitors;
            is_contemporary;
          } =
      let burial_type = Some (translate_burial_type burial_type) in
      let visible_for_visitors = translate_visibility visible_for_visitors in
      let has_history = Some has_history in
      let has_possible_duplications = Some has_possible_duplications in
      let linked_page_biblio = Some linked_page_biblio in
      let linked_page_bnote = Some linked_page_bnote in
      let linked_page_death = Some linked_page_death in
      let linked_page_head = Some linked_page_head in
      let linked_page_occu = Some linked_page_occu in
      let visible_for_visitors = Some visible_for_visitors in
      Api_saisie_read_protoc.(
        ({
           person_protoc with
           birth_date_raw;
           birth_text;
           baptism_date_raw;
           baptism_text;
           death_date_raw;
           death_text;
           burial_date_raw;
           burial_text;
           cremation_text;
           burial_type;
           titles_links;
           sosa_nb;
           has_history;
           has_possible_duplications;
           linked_page_biblio;
           linked_page_bnote;
           linked_page_death;
           linked_page_head;
           linked_page_occu;
           visible_for_visitors;
           is_contemporary;
         }
          : person))

    let person
        Api_saisie_read_piqi.Person.
          {
            type_;
            index;
            sex;
            lastname;
            firstname;
            n;
            p;
            occ;
            public_name;
            aliases;
            qualifiers;
            firstname_aliases;
            surname_aliases;
            image;
            birth_date;
            birth_date_conv;
            birth_date_cal;
            birth_place;
            birth_src;
            baptism_date;
            baptism_date_conv;
            baptism_date_cal;
            baptism_place;
            baptism_src;
            death_date;
            death_date_conv;
            death_date_cal;
            death_place;
            death_src;
            death_type;
            burial_date;
            burial_date_conv;
            burial_date_cal;
            burial_place;
            burial_src;
            occupation;
            notes;
            psources;
            has_sources;
            titles;
            related;
            rparents;
            father;
            mother;
            families;
            sosa;
            events;
            events_witnesses;
            baseprefix;
            is_contemporary;
            name_is_hidden;
            name_is_restricted;
            fiche_person_person;
          } =
      let type_ = person_type type_ in
      let sex = translate_sex sex in
      let birth_date_cal = Option.map calendar birth_date_cal in
      let baptism_date_cal = Option.map calendar baptism_date_cal in
      let death_date_cal = Option.map calendar death_date_cal in
      let burial_date_cal = Option.map calendar burial_date_cal in
      let death_type = translate_death_type death_type in
      let related = List.map relation_person related in
      let rparents = List.map relation_person rparents in
      let father = Option.map simple_person father in
      let mother = Option.map simple_person mother in
      let families = List.map translate_family families in
      let sosa = translate_sosa sosa in
      let events = List.map translate_event events in
      let events_witnesses = List.map event_witness events_witnesses in
      let person =
        Api_saisie_read_protoc.(
          ({
             type_;
             index;
             sex;
             lastname;
             firstname;
             n;
             p;
             occ;
             public_name;
             aliases;
             qualifiers;
             firstname_aliases;
             surname_aliases;
             image;
             birth_date;
             birth_date_conv;
             birth_date_cal;
             birth_place;
             birth_src;
             baptism_date;
             baptism_date_conv;
             baptism_date_cal;
             baptism_place;
             baptism_src;
             death_date;
             death_date_conv;
             death_date_cal;
             death_place;
             death_src;
             death_type;
             burial_date;
             burial_date_conv;
             burial_date_cal;
             burial_place;
             burial_src;
             occupation;
             notes;
             psources;
             has_sources;
             titles;
             related;
             rparents;
             father;
             mother;
             families;
             sosa;
             events;
             events_witnesses;
             baseprefix;
             is_contemporary;
             name_is_hidden;
             name_is_restricted;
             birth_date_raw = None;
             birth_text = None;
             baptism_date_raw = None;
             baptism_text = None;
             death_date_raw = None;
             death_text = None;
             burial_date_raw = None;
             burial_text = None;
             cremation_text = None;
             burial_type = None;
             titles_links = [];
             sosa_nb = None;
             has_history = None;
             has_possible_duplications = None;
             linked_page_biblio = None;
             linked_page_bnote = None;
             linked_page_death = None;
             linked_page_head = None;
             linked_page_occu = None;
             visible_for_visitors = None;
           }
            : person))
      in
      match fiche_person_person with
      | Some fp -> fiche_person person fp
      | None -> person

    let person_tree
        Api_saisie_read_piqi.Person_tree.
          {
            index;
            sex;
            lastname;
            firstname;
            n;
            p;
            occ;
            dates;
            image;
            sosa;
            has_more_infos;
            baseprefix;
            name_is_hidden;
            name_is_restricted;
          } =
      let sex = translate_sex sex in
      let sosa = translate_sosa sosa in
      Api_saisie_read_protoc.(
        ({
           index;
           sex;
           lastname;
           firstname;
           n;
           p;
           occ;
           dates;
           image;
           sosa;
           has_more_infos;
           baseprefix;
           name_is_hidden;
           name_is_restricted;
         }
          : person_tree))

    let node Api_saisie_read_piqi.Node.{ id; person; ifam } =
      let person = Some (person_tree person) in
      Api_saisie_read_protoc.(({ id; person; ifam } : node))

    let edge Api_saisie_read_piqi.Edge.{ from_node; to_node } =
      Api_saisie_read_protoc.(({ from_node; to_node } : edge))

    let graph_tree
        Api_saisie_read_piqi.Graph_tree.
          {
            nodes_asc;
            edges_asc;
            nodes_desc;
            edges_desc;
            nodes_siblings;
            nodes_siblings_before;
            nodes_siblings_after;
          } =
      let nodes_asc = List.map node nodes_asc in
      let nodes_desc = List.map node nodes_desc in
      let nodes_siblings = List.map node nodes_siblings in
      let nodes_siblings_before = List.map node nodes_siblings_before in
      let nodes_siblings_after = List.map node nodes_siblings_after in
      let edges_asc = List.map edge edges_asc in
      let edges_desc = List.map edge edges_desc in
      Api_saisie_read_protoc.(
        ({
           nodes_asc;
           edges_asc;
           nodes_desc;
           edges_desc;
           nodes_siblings;
           nodes_siblings_before;
           nodes_siblings_after;
         }
          : graph_tree))

    let nb_ancestors Api_saisie_read_piqi.Nb_ancestors.{ nb } =
      Api_saisie_read_protoc.(({ nb } : nb_ancestors))
  end

  module ProtocToPiqi = struct
    let identifier_person'
        Api_saisie_read_protoc.(
          ({ index; n; p; oc; track_visit } : identifier_person)) =
      Api_saisie_read_piqi.Identifier_person.{ index; n; p; oc; track_visit }

    let graph_tree_params
        Api_saisie_read_protoc.(
          ({ identifier_person; nb_asc; nb_desc; indexz } : graph_tree_params))
        =
      let identifier_person = identifier_person' (required identifier_person) in
      Api_saisie_read_piqi.Graph_tree_params.
        { identifier_person; nb_asc; nb_desc; indexz }

    let identifier_person = identifier_person'

    let index_person
        Api_saisie_read_protoc.(
          ({ index; indexz; events_limit; events_witnesses_limit } :
            index_person)) =
      Api_saisie_read_piqi.Index_person.
        { index; indexz; events_limit; events_witnesses_limit }

    let fiche_parameters
        Api_saisie_read_protoc.(
          ({
             identifier_person;
             nb_asc_max;
             nb_desc_max;
             simple_graph_info;
             no_event;
           } :
            fiche_parameters)) =
      let identifier_person = identifier_person' (required identifier_person) in
      Api_saisie_read_piqi.Fiche_parameters.
        {
          identifier_person;
          nb_asc_max;
          nb_desc_max;
          simple_graph_info;
          no_event;
        }
  end
end
