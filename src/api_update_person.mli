val print_add :
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_saisie_write_piqi.person ->
  Api_update_util.update_base_status

val print_mod :
  ?no_check_name:bool ->
  ?fexclude:Gwdb.ifam list ->
  ?with_history:bool ->
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_saisie_write_piqi.person ->
  Api_update_util.update_base_status

val print_add_nobase :
  Geneweb.Config.config ->
  Api_saisie_write_piqi.person ->
  Api_update_util.update_base_status
