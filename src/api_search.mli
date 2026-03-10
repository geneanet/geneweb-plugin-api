val print_search : Geneweb.Config.config -> Gwdb.base -> unit

type dico = string array

val dico_fname :
  assets:string ->
  lang:string ->
  data_type:[< Api_saisie_write_piqi.auto_complete_place_field | `profession ] ->
  string option

val complete_with_dico :
  string ->
  Geneweb.Config.config ->
  int ->
  int ->
  [< Api_saisie_write_piqi.auto_complete_place_field | `profession]
  option ->
  string ->
  string list ->
  string list
(** [complete_with_dico _ _ _ _ _ ini list]:
    [ini] must be in the form of [Name.lower @@ Mutil.tr '_' ' ' ini]
    Assume that [list] is already sorted, but reversed.
*)

val search_auto_complete :
  assets:string ->
  conf:Geneweb.Config.config ->
  base:Gwdb.base ->
  mode:Api_saisie_write_piqi.auto_complete_field ->
  place_mode:Api_saisie_write_piqi.auto_complete_place_field option ->
  max:int ->
  ini:string ->
  string list
