(** Core domain models, field selection specifications, and privacy-filtered entity extraction for API v2. *)

type npocc = {
  n : string;  (** First name (nom de baptême) *)
  p : string;  (** Surname (patronyme) *)
  occ : Int32.t;  (** Occurrence number *)
}
(** Person lookup key based on name and occurrence. *)

type index = Int32.t
(** Numeric database entity index. *)

(** Sex of an individual. *)
type sex =
  | Unknown  (** Unknown sex *)
  | Male  (** Male *)
  | Female  (** Female *)

type name_aliases = {
  aliases : string list option;
  qualifiers : string list option;
  firstname_aliases : string list option;
  surname_aliases : string list option;
}
(** Set of aliases and name qualifiers for a person. *)

type 'a paginated_request = {
  page_number : int;
  elements_per_page : [ `Int of int | `All ];
  elements_request : 'a;
}
(** Pagination specification for nested collection requests. *)

type event_request = {
  spouse : requested_fields option;
  witnesses : paginated_witness_request option;
}
(** Nested field selection for event entities. *)

and witness_request = { witness : requested_fields option; note : bool }
(** Field selection for event witnesses. *)

and paginated_witness_request = witness_request paginated_request
and paginated_event_request = event_request paginated_request

and requested_fields = {
  index : bool;
  npocc : bool;
  lastname : bool;
  firstname : bool;
  sex : bool;
  image : bool;
  public_name : bool;
  name_aliases : bool;
  father : requested_fields option;
  mother : requested_fields option;
  sosa : bool;
  occupation : bool;
  notes : bool;
  sources : bool;
  titles : bool;
  events : paginated_event_request option;
  birth : bool;
  baptism : bool;
  death : bool;
  burial : bool;
  related : requested_fields option;
  rparents : requested_fields option;
}
(** Complete mask specifying which fields and sub-entities to populate. *)

type person_select =
  | Index of index
  | Npocc of npocc
      (** Query selector: either direct database index or name occurrence key. *)

type person
(** Encapsulated person entity holding base handle, fields mask, and precomputed privacy token. *)

type 'a paginated = { elements : 'a list; page_number : int; total_count : int }
(** Generic paginated collection container. *)

type event_type = string Geneweb.Event.event_name
(** Event type discriminator (personal or familial). *)

type date = Date.date
(** GeneWeb calendar date representation. *)

type witness = {
  witness_type : Def.witness_kind;
  witness : person;
  note : string option;
}
(** An individual witnessing an event. *)

type paginated_witnesses = witness paginated

type event = {
  event_type : event_type;
  date : date option;
  place : string option;
  notes : string option;
  sources : string option;
  spouse : person option;
  death_type : Def.death option;
  burial_type : Def.burial option;
  witnesses : paginated_witnesses option;
}
(** A personal or familial event. *)

type paginated_events = event paginated

module Person : sig
  type t = person

  val get_person :
    Geneweb.Config.config ->
    Gwdb.base ->
    person_select ->
    requested_fields ->
    t option
  (** [get_person conf base select fields] looks up a person in [base] matching [select],
      precomputing privacy visibility and wrapping it with requested [fields].
      Returns [None] if the person does not exist. *)

  val get_index : t -> index option
  (** Returns the database index if requested and not hidden by privacy rules. *)

  val get_npocc : t -> npocc option
  (** Returns the name-occurrence key if requested and not hidden by privacy rules. *)

  val get_sex : t -> sex option
  (** Returns the sex if requested and person is not restricted. *)

  val get_lastname : t -> string option
  (** Returns the surname if requested and not hidden by privacy rules. *)

  val get_firstname : t -> string option
  (** Returns the first name if requested and not hidden by privacy rules. *)

  val get_public_name : t -> string option
  (** Returns the public name if requested, non-empty, and not hidden by privacy rules. *)

  val get_image : t -> string option
  (** Returns the portrait image filename if requested. *)

  val get_name_aliases : t -> name_aliases option
  (** Returns name aliases and qualifiers if requested and not hidden. *)

  val get_father : t -> person option
  (** Returns the father person entity if requested and not restricted. *)

  val get_mother : t -> person option
  (** Returns the mother person entity if requested and not restricted. *)

  val get_sosa : t -> string option
  (** Returns the Sosa number relative to the reference individual if requested. *)

  val get_occupation : t -> string option
  (** Returns the occupation string if requested and visible. *)

  val get_notes : t -> string option
  (** Returns person notes if requested, visible, and notes are not globally disabled. *)

  val get_sources : t -> string option
  (** Returns person sources if requested and visible. *)

  val get_titles : t -> string list option
  (** Returns nobility titles formatted according to privacy visibility. *)

  val get_events : t -> paginated_events option
  (** Returns sorted, paginated personal and family events if requested and visible. *)

  val get_birth : t -> event option
  (** Returns the birth event if requested, populated, and visible. *)

  val get_baptism : t -> event option
  (** Returns the baptism event if requested, populated, and visible. *)

  val get_death : t -> event option
  (** Returns the death event if requested, populated, and visible. *)

  val get_burial : t -> event option
  (** Returns the burial event if requested, populated, and visible. *)

  val get_related : t -> (Def.relation_type * person) list option
  (** Returns other related persons if requested. *)

  val get_rparents : t -> (Def.relation_type * person) list option
  (** Returns non-traditional / adoptive / foster parents if requested. *)
end
