(** API v2 endpoint handler for individual person retrieval. *)

val handler : Geneweb.Config.config -> Gwdb.base -> unit
(** [handler conf base] processes incoming [API_PERSON] requests.
    Parses Protocol Buffer request parameters, retrieves the requested person
    and fields from [base] respecting privacy filtering, and serializes the
    resulting [Api_v2_piqi.Person.t] protobuf payload to standard output. *)
