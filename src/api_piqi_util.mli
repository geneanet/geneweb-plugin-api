val p_getenvbin : ('a * Adef.encoded_string) list -> 'a -> string option

module Date : functor
  (M : sig
     module Dmy : sig
       type t = {
         mutable day : int32;
         mutable month : int32;
         mutable year : int32;
         mutable delta : int32;
       }
     end

     module Date : sig
       type t = {
         mutable cal : [ `gregorian | `julian | `french | `hebrew | `islamic ] option;
         mutable prec :
           [ `sure | `about | `maybe | `before | `after | `oryear | `yearint ]
           option;
         mutable dmy : Dmy.t option;
         mutable dmy2 : Dmy.t option;
         mutable text : string option;
       }
     end
   end)
  -> sig
  val piqi_date_of_date : Date.date -> M.Date.t
end

module Filter : functor
  (M : sig
     module Filter_date : sig
       type t = {
         mutable day : int32;
         mutable month : int32;
         mutable year : int32;
       }
     end

     module Filter_date_range : sig
       type t = {
         mutable date_begin : Filter_date.t;
         mutable date_end : Filter_date.t;
         mutable only_exact : bool;
       }
     end

     module Filters : sig
       type t = {
         mutable only_sosa : bool;
         mutable only_recent : bool;
         mutable sex : [ `female | `male | `unknown ] option;
         mutable nb_results : bool;
         mutable date_birth : Filter_date_range.t option;
         mutable date_death : Filter_date_range.t option;
       }
     end
   end)
  (Mext : sig
     val parse_filters :
       ?opts:Piqirun_ext.options ->
       string ->
       Piqirun_ext.input_format ->
       M.Filters.t
   end)
  -> sig
  val get_filters : Geneweb.Config.config -> Api_def.filters
end

module ReferencePerson : functor
  (M : sig
     module Reference_person : sig
       type t = { mutable n : string; mutable p : string; mutable oc : int32 }
     end
   end)
  -> sig
  val person_to_reference_person :
    Gwdb.base -> Gwdb.person -> M.Reference_person.t

  val empty_reference_person : M.Reference_person.t
end

val print_result :
  Geneweb.Config.config -> ([> `json | `pb | `xml ] -> string) -> unit

val print_error :
  Geneweb.Config.config -> Api_piqi.Api_piqi.error_code -> string -> _

val get_params :
  Geneweb.Config.config -> (string -> [> `json | `pb | `xml ] -> 'a) -> 'a

val piqi_fevent_name_of_fevent_name :
  _ Def.gen_fam_event_name ->
  [> `efam_annulation
  | `efam_divorce
  | `efam_engage
  | `efam_marriage
  | `efam_marriage_bann
  | `efam_marriage_contract
  | `efam_marriage_license
  | `efam_no_marriage
  | `efam_no_mention
  | `efam_pacs
  | `efam_residence
  | `efam_separated ]

val piqi_pevent_name_of_pevent_name :
  _ Def.gen_pers_event_name ->
  [> `epers_accomplishment
  | `epers_acquisition
  | `epers_adhesion
  | `epers_baptism
  | `epers_baptismlds
  | `epers_barmitzvah
  | `epers_batmitzvah
  | `epers_benediction
  | `epers_birth
  | `epers_burial
  | `epers_changename
  | `epers_circumcision
  | `epers_confirmation
  | `epers_confirmationlds
  | `epers_cremation
  | `epers_death
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
  | `epers_adoption ]

val pevent_name_of_piqi_pevent_name :
  [< `epers_accomplishment
  | `epers_acquisition
  | `epers_adhesion
  | `epers_baptism
  | `epers_baptismlds
  | `epers_barmitzvah
  | `epers_batmitzvah
  | `epers_benediction
  | `epers_birth
  | `epers_burial
  | `epers_changename
  | `epers_circumcision
  | `epers_confirmation
  | `epers_confirmationlds
  | `epers_cremation
  | `epers_death
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
  | `epers_adoption ] ->
  _ Def.gen_pers_event_name

val fevent_name_of_piqi_fevent_name :
  [< `efam_annulation
  | `efam_divorce
  | `efam_engage
  | `efam_marriage
  | `efam_marriage_bann
  | `efam_marriage_contract
  | `efam_marriage_license
  | `efam_no_marriage
  | `efam_no_mention
  | `efam_pacs
  | `efam_residence
  | `efam_separated ] ->
  _ Def.gen_fam_event_name

val piqi_access_to_access :
  [< `access_iftitles | `access_private | `access_public ] -> Def.access
