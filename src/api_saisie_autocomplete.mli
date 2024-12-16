val has_cache :
  Geneweb.Config.config ->
  [< `firstname | `lastname | `place | `source | `occupation ] ->
  bool

val get_list_from_cache :
  Geneweb.Config.config ->
  Api_saisie_write_piqi.auto_complete_field ->
  Api_saisie_write_piqi.auto_complete_place_field option ->
  int ->
  string ->
  string list

val is_valid_suggestion :
  Api_saisie_write_piqi.auto_complete_field ->
  Api_saisie_write_piqi.auto_complete_place_field option ->
  string ->
  string ->
  bool
