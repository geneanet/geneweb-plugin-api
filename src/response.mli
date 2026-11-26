(** Response transformation and serialization for API v2. *)

type t = {
  index : Common.index option;
  npocc : Common.npocc option;
  lastname : string option;
  firstname : string option;
  sex : Common.sex option;
  image : string option;
  public_name : string option;
  name_aliases : Common.name_aliases option;
  father : t option;
  mother : t option;
  sosa : string option;
  occupation : string option;
  notes : string option;
  notes_raw : string option;
  sources : string option;
  titles : string list option;
  events : Common.paginated_events option;
  birth : Common.event option;
  baptism : Common.event option;
  death : Common.event option;
  burial : Common.event option;
  related : (Def.relation_type * t) list option;
  rparents : (Def.relation_type * t) list option;
  families : Common.family list option;
  visibility : Common.visibility option;
  is_contemporary : bool option;
  has_parent : bool;
  has_spouse : bool;
  has_child : bool;
}
(** Intermediate response record holding resolved person and event data. *)

val response_of_person : Common.person -> t
(** [response_of_person person] converts a resolved [Common.person] entity
    into an intermediate response record. *)

val response : Geneweb.Config.config -> Gwdb.base -> Request.t -> t option
(** [response conf base req] resolves the person specified by [req] in [base]
    and builds an intermediate response record. Returns [None] if the person
    is not found or inaccessible. *)

val to_piqi : t -> Api_v2_piqi.Person.t
(** [to_piqi resp] converts an intermediate response into a Piqi-compatible
    Person record for Protobuf serialization. *)
