type npocc = {
  n : string;
  p : string;
  occ : Int32.t;
}

type index = Int32.t

type sex = Unknown | Male | Female

type basic_infos = {
  lastname : string option;
  firstname : string option;
  sex : sex option;
  image : string option;
  public_name : string option;
  aliases : string list option;
  qualifiers : string list option;
  firstname_aliases : string list option;
  surname_aliases : string list option;
}

type requested_fields = {
  index : bool;
  npocc : bool;
  basic_infos : bool;
  parents : bool;
}

type person_select = Index of index | Npocc of npocc

type person
type family

module Person : sig
  type t = person
  val get_person : Geneweb.Config.config -> Gwdb.base -> person_select -> requested_fields -> t option
  val get_index : t -> index option
  val get_npocc : t -> npocc option
  val get_basic_infos : t -> basic_infos option
  val get_father : t -> person option
  val get_mother : t -> person option
end

module Family : sig
  type t = family
end
