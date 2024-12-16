val has_cache :
  Geneweb.Config.config ->
  [< `firstname | `lastname | `place | `source | `occupation ] ->
  bool

val get_list_from_cache :
  Geneweb.Config.config ->
  [< `firstname | `lastname | `place | `source | `occupation ] ->
  [> `area_code | `country | `county | `region | `town ] option ->
  int ->
  string ->
  string list
