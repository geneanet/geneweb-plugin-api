val empty : Api_piqi.Base_warnings.t

val add_warning_to_piqi_warning_list :
  Gwdb.base ->
  Api_piqi.base_warnings ->
  Geneweb.Warning.base_warning ->
  Api_piqi.base_warnings

val add_error_to_piqi_warning_list :
  Gwdb.base ->
  Api_piqi.base_warnings ->
  Gwdb.person Def.error ->
  Api_piqi.base_warnings
