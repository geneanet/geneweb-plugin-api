val apply_filters_p :
  Geneweb.Config.config ->
  Api_def.filters ->
  (Gwdb.person -> Sosa.t) ->
  Gwdb.person ->
  bool

val ( !! ) : _ Adef.astring -> string

val get_params :
  Geneweb.Config.config -> (string -> [> `json | `pb | `xml ] -> 'a) -> 'a

val person_to_warning_person :
  Gwdb.base -> Gwdb.person -> Api_piqi.warning_person

val get_filters : Geneweb.Config.config -> Api_def.filters
val chop_base_prefix : string -> string

val conv_data_list_person :
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_def.filters ->
  Gwdb.person list ->
  Piqirun_ext.output_format ->
  string

val print_result :
  Geneweb.Config.config -> ([> `json | `pb | `xml ] -> string) -> unit

val get_portrait :
  Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> string option

val string_of_prec_dmy : Date.dmy -> string

val piqi_of_witness_kind :
  Def.witness_kind ->
  [> `witness
  | `witness_attending
  | `witness_civilofficer
  | `witness_godparent
  | `witness_informant
  | `witness_mentioned
  | `witness_other
  | `witness_religiousofficer ]

val title_to_piqi_title : string Def.gen_title -> Api_piqi.title

val get_visibility :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  [> `visibility_private | `visibility_public | `visibility_semi_public ]

val pers_to_piqi_person_light :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (Gwdb.person -> Sosa.t) ->
  Api_piqi.person

val piqi_date_of_date : Date.date -> Api_piqi.date

val piqi_death_type_of_death :
  Def.death ->
  [> `dead | `dead_young | `dont_know_if_dead | `not_dead | `of_course_dead ]

val person_to_reference_person :
  Gwdb.base -> Gwdb.person -> Api_piqi.reference_person

val print_error :
  Geneweb.Config.config -> Api_piqi.Api_piqi.error_code -> string -> _

val calendar_of_piqi_calendar : Api_piqi.calendar -> Date.calendar

val date_of_piqi_date : Api_piqi.date -> Date.date

val pers_to_piqi_person :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (Gwdb.person -> Sosa.t) ->
  (Api_piqi.person, Api_piqi.full_person) Api_def.pb_person

val witness_kind_of_piqi :
  [< `witness
  | `witness_attending
  | `witness_civilofficer
  | `witness_godparent
  | `witness_informant
  | `witness_mentioned
  | `witness_other
  | `witness_religiousofficer ] ->
  Def.witness_kind

val load_image_ht : Geneweb.Config.config -> unit
val empty_reference_person : Api_piqi.reference_person

val empty_piqi_person :
  Geneweb.Config.config ->
  Api_piqi.reference_person ->
  (Api_piqi.person, Api_piqi.full_person) Api_def.pb_person

val translate_witness :
  Geneweb.Config.config -> Def.witness_kind -> Adef.safe_string

val fam_to_piqi_family :
  Geneweb.Config.config -> Gwdb.base -> Gwdb.ifam -> Api_piqi.full_family

val data_person :
  (Api_piqi.person, Api_piqi.full_person) Api_def.pb_person ->
  Piqirun_ext.output_format ->
  string

val witness_kinds : Def.witness_kind list

val piqi_ref_person_to_person :
  Gwdb.base -> Api_piqi.reference_person -> Gwdb.person option

val compute_sosa :
  Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> Sosa.t

val opt_of_string : string -> string option

val person_node_map :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person list ->
  (Api_piqi.node list, Api_piqi.full_node list) Api_def.pb_person

val data_list_person_option :
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_def.filters ->
  (Api_piqi.reference_person, Gwdb.person) Api_def.pb_person list ->
  Piqirun_ext.output_format ->
  string

val is_empty_or_quest_name : Gwdb.person -> bool
val is_date_included : bool -> Date.date option -> Date.dmy -> Date.dmy -> bool
val p_getenvbin : ('a * Adef.encoded_string) list -> 'a -> string option

val pers_to_piqi_person_full :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (Gwdb.person -> Sosa.t) ->
  Api_piqi.full_person

val set_sosa_ref : Geneweb.Config.config -> Gwdb.iper -> Geneweb.Config.config

module Page : sig
  type t

  val first : element_count:int -> t
end

module Paginated_data : sig
  type 'element t =
    private {elements : 'element list; page_number : int; total_count : int}

  val extract : Page.t -> 'element list -> 'element t
end
