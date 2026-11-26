(** Core domain models, field selection specifications, and privacy-filtered entity extraction for API v2. *)

type npocc = {
  first_name : string;  (** First name (nom de baptême / prénom) *)
  surname : string;  (** Surname (patronyme / nom de famille) *)
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

type marriage_type =
  [ `married
  | `not_married
  | `engaged
  | `no_sexes_check_not_married
  | `no_mention
  | `no_sexes_check_married
  | `marriage_bann
  | `marriage_contract
  | `marriage_license
  | `pacs
  | `residence ]
(** Marital union relationship kind. *)

type divorce_type = [ `not_divorced | `divorced | `separated ]
(** Divorce status of a family union. *)

type visibility =
  [ `visibility_public | `visibility_semi_public | `visibility_private ]
(** Public visibility classification for visitors. *)

type event_request = {
  spouse : requested_fields option;
  witnesses : paginated_witness_request option;
  event_notes_raw : bool;
  date_str : bool;
}
(** Nested field selection for event entities. *)

and witness_request = { witness : requested_fields option; note : bool }
(** Field selection for event witnesses. *)

and paginated_witness_request = witness_request paginated_request
and paginated_event_request = event_request paginated_request

and family_request = {
  fam_spouse : requested_fields option;
  marriage_date : bool;
  marriage_place : bool;
  marriage_note : bool;
  marriage_note_raw : bool;
  marriage_source : bool;
  marriage_type : bool;
  divorce_type : bool;
  divorce_date : bool;
  children : requested_fields option;
  fam_witnesses : paginated_witness_request option;
  marriage_date_str : bool;
  divorce_date_str : bool;
}
(** Field selection for family unions. *)

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
  notes_raw : bool;
  sources : bool;
  titles : bool;
  events : paginated_event_request option;
  birth : bool;
  baptism : bool;
  death : bool;
  burial : bool;
  related : requested_fields option;
  rparents : requested_fields option;
  families : family_request option;
  visibility : bool;
  is_contemporary : bool;
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
  date_str : string option;
  date_long : string option;
  date_conv : string option;
  date_conv_long : string option;
  date_raw : string option;
  place : string option;
  notes : string option;
  notes_raw : string option;
  sources : string option;
  spouse : person option;
  death_type : Def.death option;
  burial_type : Def.burial option;
  witnesses : paginated_witnesses option;
}
(** A personal or familial event. *)

type paginated_events = event paginated

type family = {
  index : int;
  fam_spouse : person option;
  marriage_date : date option;
  marriage_date_str : string option;
  marriage_date_long : string option;
  marriage_date_conv : string option;
  marriage_date_conv_long : string option;
  marriage_date_raw : string option;
  marriage_place : string option;
  marriage_note : string option;
  marriage_note_raw : string option;
  marriage_source : string option;
  marriage_type : marriage_type option;
  divorce_type : divorce_type option;
  divorce_date : date option;
  divorce_date_str : string option;
  divorce_date_long : string option;
  divorce_date_conv : string option;
  divorce_date_conv_long : string option;
  divorce_date_raw : string option;
  children : person list option;
  fam_witnesses : witness list option;
}
(** A family union between individuals. *)

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

  val get_notes_raw : t -> string option
  (** Returns raw uninterpreted person notes if requested, visible, and not disabled. *)

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

  val get_families : t -> family list option
  (** Returns family unions (spouses, marriage details, children) if requested. *)

  val get_visibility : t -> visibility option
  (** Returns the visitor visibility level if requested. *)

  val get_is_contemporary : t -> bool option
  (** Returns whether the person is contemporary if requested. *)

  val has_parent : t -> bool
  (** Returns whether the person has parents in the database. *)

  val has_spouse : t -> bool
  (** Returns whether the person has at least one spouse/union in the database. *)

  val has_child : t -> bool
  (** Returns whether the person has at least one child in the database. *)
end
