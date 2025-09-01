type npocc = {
  n : string;
  p : string;
  occ : Int32.t;
}

type index = Int32.t

type requested_fields = {
  index : bool;
  npocc : bool;
}

type person_select = Index of index | Npocc of npocc

type person
type family

module Person : sig
  type t = person
  val get_person : Geneweb.Config.config -> Gwdb.base -> person_select -> requested_fields -> t option
  val get_index : t -> index option
  val get_npocc : t -> npocc option
end

module Family : sig
  type t = family
end
