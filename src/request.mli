type t
val request_of_piqi_request : Api_v2_piqi.Request.t -> t option
val get_select : t -> Common.person_select
val get_fields : t -> Common.requested_fields
