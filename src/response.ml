
type t = {
  index : Common.index option;
  npocc : Common.npocc option;
}

let npocc_to_piqi npocc : Api_v2_piqi.Npocc.t = {
  Api_v2_piqi.Npocc.n = Some npocc.Common.n;
  p = Some npocc.Common.p;
  occ = Some npocc.Common.occ;
}

let to_piqi response =
  let index = response.index in
  let npocc = Option.map npocc_to_piqi response.npocc in
  {
    Api_v2_piqi.Person.index;
    npocc;
  }

let response conf base request =
  Option.map (fun person ->
      {
        index = Common.Person.get_index person;
        npocc = Common.Person.get_npocc person;
      }
    )
    (Common.Person.get_person conf base
       (Request.get_select request)
       (Request.get_fields request))
