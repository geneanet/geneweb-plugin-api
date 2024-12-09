type person_update = {
    first_name : string;
    surname : string;
    occurrence_number : int;
    kind : Geneweb.Update.create;
    force : bool;
  }

type created_person = {
  n : string;
  p : string;
  oc : int32
}

type update_base_status =
  | UpdateSuccess of
      Geneweb.Warning.base_warning list
      * Geneweb.Warning.base_misc list
      * (unit -> unit) list
      * created_person option
  | UpdateError of Geneweb.Update.update_error
  | UpdateErrorConflict of Api_saisie_write_piqi.Create_conflict.t

exception ModErrApiConflict of Api_saisie_write_piqi.Create_conflict.t

val created_person_of_person :
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  created_person

val created_person : n:string -> p:string -> oc:Int32.t -> created_person

val created_person_is_unnamed : created_person -> bool

val reserve_occurrence_number :
  first_name:string ->
  surname:string ->
  int ->
  unit

val find_free_occ :
  base:Gwdb.base ->
  first_name:string ->
  surname:string ->
  int

val check_person_conflict :
  Gwdb.base ->
  ( person_update,
    string )
  Def.gen_pers_event
  list ->
  ( Gwdb.iper,
    person_update,
    string )
  Def.gen_person ->
  unit

val check_family_conflict :
  Gwdb.base ->
  ( person_update,
    _,
    _ )
  Def.gen_family ->
  person_update Def.gen_couple ->
  person_update Def.gen_descend ->
  unit

val date_of_piqi_date :
  Geneweb.Config.config -> Api_saisie_write_piqi.date -> Date.date option

val pers_to_piqi_person_search :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person_search
(** [Description] : Retourne une personne qui sert lors de la recherche pour
                    relier un individu dans la saisie.                        *)

val pers_to_piqi_person_search_info :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person_search_info
(** [Description] : Retourne une personne qui sert lors de la recherche pour
                    relier un individu dans la saisie (affichage des
                    informations détaillées).                                 *)

val pers_to_piqi_person_link :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person_link
(** [Description] : Retourne une personne qui sert lors de la recherche pour
                    relier un individu dans la saisie.                        *)

val pers_to_piqi_mod_person :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person

val fam_to_piqi_mod_family :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.ifam ->
  Gwdb.family ->
  Api_saisie_write_piqi.family

val piqi_mod_person_of_person_start :
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_piqi.person_start ->
  Api_saisie_write_piqi.person
(** [Description] : Converti une personne start pour la première saisie en
                    Person afin de suivre le chemin classique de modification
                    de la base.                                               *)

val piqi_empty_family :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.ifam ->
  Api_saisie_write_piqi.family

val reconstitute_somebody :
  Gwdb.base -> Api_saisie_write_piqi.person_link -> person_update

val to_update_key : person_update -> Geneweb.Update.key
