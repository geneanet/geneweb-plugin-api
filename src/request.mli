(** Request parsing and field selection for API v2. *)

type t
(** Parsed person query request including target selector and requested fields. *)

val make :
  person_select:Common.person_select ->
  requested_fields:Common.requested_fields ->
  t
(** [make ~person_select ~requested_fields] constructs a [Request.t]. *)

val request_of_piqi_request : Api_v2_piqi.Request.t -> t option
(** [request_of_piqi_request piqi_req] extracts the person selector (by index or npocc)
    and field selection masks from a decoded Piqi request. Returns [None] if no
    valid person selector is provided. *)

val get_fields : t -> Common.requested_fields
(** [get_fields req] returns the set of requested fields. *)

val get_select : t -> Common.person_select
(** [get_select req] returns the target person selector. *)

val simple_fields : Common.requested_fields
(** Minimal field selection mask (index, npocc, names, sex). *)

val full_fields : Common.requested_fields
(** Comprehensive field selection mask with all fields and relations enabled. *)

val full_page : 'a -> 'a Common.paginated_request
(** [full_page req] wraps [req] in an unpaginated request ([elements_per_page = `All]). *)
