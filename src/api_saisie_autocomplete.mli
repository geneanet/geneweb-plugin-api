val has_cache :
  Geneweb.Config.config ->
  [< `firstname | `lastname | `place | `source | `occupation ] ->
  bool

val write_caches : Geneweb.Config.config -> Gwdb.base -> unit

val get_list_from_cache :
  Geneweb.Config.config ->
  [< `firstname | `lastname | `place | `source | `occupation ] ->
  int ->
  string ->
  string list
