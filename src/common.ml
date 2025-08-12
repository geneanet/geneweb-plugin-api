
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
