val has_cache :
  conf:Geneweb.Config.config ->
  mode:Api_saisie_write_piqi.auto_complete_field ->
  bool

val get_list_from_cache :
  conf:Geneweb.Config.config ->
  base:Gwdb.base ->
  mode:Api_saisie_write_piqi.auto_complete_field ->
  place_mode:Api_saisie_write_piqi.auto_complete_place_field option ->
  n:int ->
  ini:string ->
  string list

val is_valid_suggestion :
  mode:Api_saisie_write_piqi.auto_complete_field ->
  place_mode:Api_saisie_write_piqi.auto_complete_place_field option ->
  ini:string ->
  candidate:string ->
  bool
