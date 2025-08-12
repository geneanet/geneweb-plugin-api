type t

val get_person : Geneweb.Config.config -> Gwdb.base -> Common.person_select -> Common.requested_fields -> t option

val get_index : t -> Common.index option
val get_npocc : t -> Common.npocc option

