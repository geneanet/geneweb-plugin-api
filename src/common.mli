type npocc = {
  n : string;
  p : string;
  occ : Int32.t;
}

type index = Int32.t

type sex = Unknown | Male | Female

type name_aliases = {
  aliases : string list option;
  qualifiers : string list option;
  firstname_aliases : string list option;
  surname_aliases : string list option;
}

type requested_fields = {
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
}

type person_select = Index of index | Npocc of npocc

type person
type family

module Person : sig
  type t = person
  val get_person : Geneweb.Config.config -> Gwdb.base -> person_select -> requested_fields -> t option
  val get_index : t -> index option
  val get_npocc : t -> npocc option
  val get_sex : t -> sex option
  val get_lastname : t -> string option
  val get_firstname : t -> string option
  val get_public_name: t -> string option
  val get_image : t -> string option
  val get_name_aliases : t -> name_aliases option
  val get_father : t -> person option
  val get_mother : t -> person option
  val get_sosa : t -> string option
  val get_occupation : t -> string option
  val get_notes : t -> string option
  val get_sources : t -> string option
end

module Family : sig
  type t = family
end
