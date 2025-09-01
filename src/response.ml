type t = {
  index : Common.index option;
  npocc : Common.npocc option;
  basic_infos : Common.basic_infos option;
}

let npocc_to_piqi npocc : Api_v2_piqi.Npocc.t = {
  Api_v2_piqi.Npocc.n = Some npocc.Common.n;
  p = Some npocc.Common.p;
  occ = Some npocc.Common.occ;
}

let sex_to_piqi = function
  | Common.Unknown -> `unknown
  | Male -> `male
  | Female -> `female

let basic_infos_to_piqi basic_infos : Api_v2_piqi.Basic_infos.t = {
  Api_v2_piqi.Basic_infos.lastname = basic_infos.Common.lastname;
  firstname = basic_infos.firstname;
  sex = Option.map sex_to_piqi basic_infos.sex;
  image = basic_infos.image;
  public_name = basic_infos.public_name;
  aliases = Option.value ~default:[] basic_infos.aliases;
  qualifiers = Option.value ~default:[] basic_infos.qualifiers;
  firstname_aliases = Option.value ~default:[] basic_infos.firstname_aliases;
  surname_aliases = Option.value ~default:[] basic_infos.surname_aliases;
}

let to_piqi response =
  let index = response.index in
  let npocc = Option.map npocc_to_piqi response.npocc in
  let basic_infos = Option.map basic_infos_to_piqi response.basic_infos in
  {
    Api_v2_piqi.Person.index;
    npocc;
    basic_infos;
  }

let response conf base request =
  Option.map (fun person ->
      {
        index = Common.Person.get_index person;
        npocc = Common.Person.get_npocc person;
        basic_infos = Common.Person.get_basic_infos person;
      }
    )
    (Common.Person.get_person conf base
       (Request.get_select request)
       (Request.get_fields request))
