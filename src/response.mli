(** Response transformation and serialization for API v2. *)

type t
(** Intermediate response record holding resolved person and event data. *)

val response : Geneweb.Config.config -> Gwdb.base -> Request.t -> t option
(** [response conf base req] resolves the person specified by [req] in [base]
    and builds an intermediate response record. Returns [None] if the person
    is not found or inaccessible. *)

val to_piqi : t -> Api_v2_piqi.Person.t
(** [to_piqi resp] converts an intermediate response into a Piqi-compatible
    Person record for Protobuf serialization. *)
