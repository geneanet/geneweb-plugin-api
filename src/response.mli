type t
val response : Geneweb.Config.config -> Gwdb.base -> Request.t -> t option
val to_piqi : t -> Api_v2_piqi.Person.t
