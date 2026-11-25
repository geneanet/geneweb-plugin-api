(** Request parsing and field selection for API v2. *)

type t
(** Parsed person query request including target selector and requested fields. *)

val request_of_piqi_request : Api_v2_piqi.Request.t -> t option
(** [request_of_piqi_request piqi_req] extracts the person selector (by index or npocc)
    and field selection masks from a decoded Piqi request. Returns [None] if no
    valid person selector is provided. *)

val get_fields : t -> Common.requested_fields
(** [get_fields req] returns the set of requested fields. *)

val get_select : t -> Common.person_select
(** [get_select req] returns the target person selector. *)
