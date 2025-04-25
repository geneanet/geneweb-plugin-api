let empty : Api_piqi.Base_warnings.t =
  { warning_already_defined = []
  ; warning_own_ancestor = []
  ; warning_bad_sex_of_married_person = []
  ; warning_big_age_between_spouses = []
  ; warning_birth_after_death = []
  ; warning_incoherent_sex = []
  ; warning_changed_order_of_children = []
  ; warning_changed_order_of_marriages = []
  ; warning_children_not_in_order = []
  ; warning_close_children = []
  ; warning_dead_old = []
  ; warning_dead_too_early_to_be_father = []
  ; warning_incoherent_ancestor_date = []
  ; warning_marriage_date_after_death = []
  ; warning_marriage_date_before_birth = []
  ; warning_mother_dead_before_child_birth = []
  ; warning_parent_born_after_child = []
  ; warning_parent_too_old = []
  ; warning_parent_too_young = []
  ; warning_possible_duplicate_fam = []
  ; warning_title_dates_error = []
  ; warning_undefined_sex = []
  ; warning_witness_date_after_death = []
  ; warning_witness_date_before_birth = []
  ; warning_young_for_marriage = []
  ; warning_old_for_marriage = []
  ; warning_distant_children = []
  ; warning_event_order = []
  ; warning_possible_duplicate_fam_homonymous = []
  }

(** [add_error_to_piqi_warning_list base error]
    Convert [error] and add it to corresponding error list
*)
let add_error_to_piqi_warning_list base (w : Api_piqi.Base_warnings.t) = function
  | Def.AlreadyDefined p ->
    { w with warning_already_defined =
               Api_piqi.Warning_already_defined.{person = Api_util.person_to_warning_person base p }
               :: w.warning_already_defined }
  | OwnAncestor p ->
    { w with warning_own_ancestor =
               Api_piqi.Warning_own_ancestor.{person = Api_util.person_to_warning_person base p }
               :: w.warning_own_ancestor }
  | BadSexOfMarriedPerson p ->
    { w with warning_bad_sex_of_married_person =
               Api_piqi.Warning_bad_sex_of_married_person.{person = Api_util.person_to_warning_person base p }
               :: w.warning_bad_sex_of_married_person }

let fevent_to_warning_event e =
  let get_fevent_name e = e.Def.efam_name in
  { Api_piqi.Warning_event.pevent = None
  ; fevent =
      try Some (Api_piqi_util.piqi_fevent_name_of_fevent_name (get_fevent_name e))
      with _ -> None
  }

let pevent_to_warning_event e =
  let get_pevent_name e = e.Def.epers_name in
  { Api_piqi.Warning_event.fevent = None
  ; pevent =
      try Some (Api_piqi_util.piqi_pevent_name_of_pevent_name (get_pevent_name e))
      with _ -> None
  }

let add_warning_to_piqi_warning_list conf base =
  let _ = conf in
  let get_pevent_name e = e.Def.epers_name in
  let get_fevent_name e = e.Def.efam_name in
  let p2wp = Api_util.person_to_warning_person in
  fun (w : Api_piqi.Base_warnings.t) (warn : Geneweb.Warning.base_warning) -> match warn with
    | BigAgeBetweenSpouses (fath, moth, dmy) ->
      { w with warning_big_age_between_spouses =
                 Api_piqi.Warning_big_age_between_spouses.{
                   father = p2wp base fath
                 ; mother = p2wp base moth
                 ; date = Api_util.string_of_prec_dmy dmy
                 } :: w.warning_big_age_between_spouses }
    | BirthAfterDeath p ->
      { w with warning_birth_after_death =
                 Api_piqi.Warning_birth_after_death.{ person = p2wp base p }
                 :: w.warning_birth_after_death }
    | IncoherentSex (p, _, _) ->
      { w with warning_incoherent_sex =
                 Api_piqi.Warning_incoherent_sex.{ person = p2wp base p }
                 :: w.warning_incoherent_sex }
    | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = Gwdb.foi base ifam in
      { w with warning_changed_order_of_children =
                 Api_piqi.Warning_changed_order_of_children.{
                   father = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl
                 ; mother = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl
                 } :: w.warning_changed_order_of_children }
    | ChangedOrderOfMarriages (p, _, _) ->
      { w with warning_changed_order_of_marriages =
                 Api_piqi.Warning_changed_order_of_marriages.{ person = p2wp base p }
                 :: w.warning_changed_order_of_marriages }
    | ChildrenNotInOrder (ifam, _, _, _) ->
      let cpl = Gwdb.foi base ifam in
      { w with warning_children_not_in_order =
                 Api_piqi.Warning_children_not_in_order.{
                   father = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl
                 ; mother = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl
                 } :: w.warning_children_not_in_order }
    | CloseChildren (ifam, c1, c2) ->
      let cpl = Gwdb.foi base ifam in
      { w with warning_close_children =
                 Api_piqi.Warning_close_children.{
                   father = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl
                 ; mother = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl
                 ; child1 = p2wp base c1
                 ; child2 = p2wp base c2
                 } :: w.warning_close_children }
    | DeadOld (p, dmy) ->
      { w with warning_dead_old =
                 Api_piqi.Warning_dead_old.{
                   person = p2wp base p
                 ; date = Api_util.string_of_prec_dmy dmy ;
                 } :: w.warning_dead_old }
    | DeadTooEarlyToBeFather (f, s) ->
      { w with warning_dead_too_early_to_be_father =
                 Api_piqi.Warning_dead_too_early_to_be_father.{
                   father = p2wp base f
                 ; son = p2wp base s;
                 } :: w.warning_dead_too_early_to_be_father }
    | DistantChildren (ifam, c1, c2) ->
      let cpl = Gwdb.foi base ifam in
      { w with warning_distant_children =
                 Api_piqi.Warning_distant_children.{
                   father = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl
                 ; mother = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl
                 ; child1 = p2wp base c1
                 ; child2 = p2wp base c2
                 } :: w.warning_distant_children }
    | FWitnessEventAfterDeath (p, e, ifam) ->
      let cpl = Gwdb.foi base ifam in
      { w with warning_witness_date_after_death =
                 Api_piqi.Warning_witness_date_after_death.{
                   person = p2wp base p
                 ; event = fevent_to_warning_event e
                 ; origin = [ p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl
                            ; p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl ]
                 } :: w.warning_witness_date_after_death }
    | FWitnessEventBeforeBirth (p, e, ifam) ->
      let cpl = Gwdb.foi base ifam in
      { w with warning_witness_date_before_birth =
                 Api_piqi.Warning_witness_date_before_birth.{
                   person = p2wp base p
                 ; event = fevent_to_warning_event e
                 ; origin = [ p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl
                            ; p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl ]
                 } :: w.warning_witness_date_before_birth }
    | IncoherentAncestorDate (a, p) ->
      { w with warning_incoherent_ancestor_date =
                 Api_piqi.Warning_incoherent_ancestor_date.{
                   person = p2wp base p
                 ; ancestor = p2wp base a
                 } :: w.warning_incoherent_ancestor_date }
    | MarriageDateAfterDeath p ->
      { w with warning_marriage_date_after_death =
                 Api_piqi.Warning_marriage_date_after_death.{person = p2wp base p}
                 :: w.warning_marriage_date_after_death }
    | MarriageDateBeforeBirth p ->
      { w with warning_marriage_date_before_birth =
                 Api_piqi.Warning_marriage_date_before_birth.{person = p2wp base p}
                 :: w.warning_marriage_date_before_birth }
    | MotherDeadBeforeChildBirth (m, c) ->
      { w with warning_mother_dead_before_child_birth =
                 Api_piqi.Warning_mother_dead_before_child_birth.{
                   mother = p2wp base m
                 ; child = p2wp base c
                 } :: w.warning_mother_dead_before_child_birth }
    | ParentBornAfterChild (p, c) ->
      { w with warning_parent_born_after_child =
                 Api_piqi.Warning_parent_born_after_child.{
                   parent = p2wp base p
                 ; child = p2wp base c
                 } :: w.warning_parent_born_after_child }
    | ParentTooOld (p, dmy, c) ->
      { w with warning_parent_too_old =
                 Api_piqi.Warning_parent_too_old.{
                   parent = p2wp base p
                 ; date = Api_util.string_of_prec_dmy dmy
                 ; child = p2wp base c
                 } :: w.warning_parent_too_old }
    | ParentTooYoung (p, dmy, c) ->
      { w with warning_parent_too_young =
                 Api_piqi.Warning_parent_too_young.{
                   parent = p2wp base p
                 ; date = Api_util.string_of_prec_dmy dmy
                 ; child = p2wp base c
                 } :: w.warning_parent_too_young }
    | PossibleDuplicateFam (f1, f2) ->
       let cpl1 = Gwdb.foi base f1 in
       let cpl2 = Gwdb.foi base f2 in
       let father1 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl1 in
       let mother1 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl1 in
       let father2 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl2 in
       let mother2 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl2 in
      { w with warning_possible_duplicate_fam =
                 Api_piqi.Warning_possible_duplicate_fam.{
                   father1;
                   mother1;
                   father2;
                   mother2;
                 } :: w.warning_possible_duplicate_fam }
    | PossibleDuplicateFamHomonymous (f1, f2, p) ->
       let cpl1 = Gwdb.foi base f1 in
       let cpl2 = Gwdb.foi base f2 in
       let father1 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl1 in
       let mother1 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl1 in
       let father2 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_father cpl2 in
       let mother2 = p2wp base @@ Gwdb.poi base @@ Gwdb.get_mother cpl2 in
       { w with warning_possible_duplicate_fam_homonymous =
                  Api_piqi.Warning_possible_duplicate_fam_homonymous.{
                    father1;
                    mother1;
                    father2;
                    mother2;
                    homonymous = p2wp base p;
                  } :: w.warning_possible_duplicate_fam_homonymous }
    | PWitnessEventAfterDeath (p, e, origin) ->
      { w with warning_witness_date_after_death =
                 Api_piqi.Warning_witness_date_after_death.{
                   person = p2wp base p
                 ; event = pevent_to_warning_event e
                 ; origin = [ p2wp base origin ]
                 } :: w.warning_witness_date_after_death }
    | PWitnessEventBeforeBirth (p, e, origin) ->
      { w with warning_witness_date_before_birth =
                 Api_piqi.Warning_witness_date_before_birth.{
                   person = p2wp base p
                 ; event = pevent_to_warning_event e
                 ; origin = [ p2wp base origin ]
                 } :: w.warning_witness_date_before_birth }
    | TitleDatesError (p, t) ->
      let t = Futil.map_title_strings (Gwdb.sou base) t in
      { w with warning_title_dates_error =
                 Api_piqi.Warning_title_dates_error.{ person = p2wp base p
                                             ; title = Api_util.title_to_piqi_title t }
                 :: w.warning_title_dates_error }
    | UndefinedSex p ->
      { w with warning_undefined_sex =
                 Api_piqi.Warning_undefined_sex.{ person = p2wp base p }
                 :: w.warning_undefined_sex }
    | YoungForMarriage (p, dmy, _) ->
      { w with warning_young_for_marriage =
                 Api_piqi.Warning_young_for_marriage.{
                   person = p2wp base p
                 ; date = Api_util.string_of_prec_dmy dmy
                 } :: w.warning_young_for_marriage }
    | OldForMarriage (p, dmy, _) ->
      { w with warning_old_for_marriage =
                 Api_piqi.Warning_old_for_marriage.{
                   person = p2wp base p
                 ; date = Api_util.string_of_prec_dmy dmy
                 } :: w.warning_old_for_marriage }
    | FEventOrder (p, e1, e2) ->
      { w with warning_event_order =
                 Api_piqi.Warning_event_order.{
                   person = p2wp base p
                 ; pevents = []
                 ; fevents = [ Api_piqi_util.piqi_fevent_name_of_fevent_name (get_fevent_name e1)
                             ; Api_piqi_util.piqi_fevent_name_of_fevent_name (get_fevent_name e2) ]
                 } :: w.warning_event_order }
      [@warning "-45"]
    | PEventOrder (p, e1, e2) ->
      { w with warning_event_order =
                 Api_piqi.Warning_event_order.{
                   person = p2wp base p
                 ; pevents = [ Api_piqi_util.piqi_pevent_name_of_pevent_name (get_pevent_name e1)
                             ; Api_piqi_util.piqi_pevent_name_of_pevent_name (get_pevent_name e2) ]
                 ; fevents = []
                 } :: w.warning_event_order }
      [@warning "-45"]
    (* Not included in api *)
    | ChangedOrderOfFamilyEvents (_, _, _) -> w
    | ChangedOrderOfPersonEvents (_, _, _) -> w
