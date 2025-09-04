type t = {
  person_select : Common.person_select;
  requested_fields : Common.requested_fields;
}

let full_fields = {
  Common.index = true;
  npocc = true;
  lastname = true;
  firstname = true;
  sex = true;
  image = true;
  public_name = true;
  name_aliases = true;
  father = None;
  mother = None;
  sosa = true;
  occupation = true;
  notes = true;
  sources = true;
}

let get_fields {person_select = _; requested_fields} = requested_fields

let get_select {person_select; requested_fields = _} = person_select

let person_select_of_piqi_request piqi_request =
  let requested_index = piqi_request.Api_v2_piqi.Request.index in
  let requested_npocc = piqi_request.Api_v2_piqi.Request.npocc in
  match requested_index, requested_npocc with
  | Some index, _ -> Some (Common.Index index)
  | _, Some Api_v2_piqi.Npocc.{n = Some n; p = Some p; occ = Some occ} -> Some (Common.Npocc {n; p; occ})
  | _, _ -> None

let optional = Option.value ~default:false

let rec requested_fields person_request = {
  Common.index = optional person_request.Api_v2_piqi.Person_request.index;
  npocc = optional person_request.npocc;
  lastname = optional person_request.lastname;
  firstname = optional person_request.firstname;
  sex = optional person_request.sex;
  image = optional person_request.image;
  public_name = optional person_request.public_name;
  name_aliases = optional person_request.name_aliases;
  father = Option.map requested_fields person_request.father;
  mother = Option.map requested_fields person_request.mother;
  sosa = optional person_request.sosa;
  occupation = optional person_request.occupation;
  notes = optional person_request.notes;
  sources = optional person_request.sources;
}

let request_of_piqi_request piqi_request : t option =
  let person_select = person_select_of_piqi_request piqi_request in
  let person_request = piqi_request.Api_v2_piqi.Request.person_request in
  Option.map (fun person_select -> {
        person_select;
        requested_fields = Option.value ~default:full_fields (Option.map requested_fields person_request)
      }) person_select
