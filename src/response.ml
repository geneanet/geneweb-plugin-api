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
  sources : string option;
  titles : string list option;
  events : Common.paginated_events option;
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

let name_aliases_to_piqi name_aliases : Api_v2_piqi.Name_aliases.t = {
  aliases = Option.value ~default:[] name_aliases.Common.aliases;
  qualifiers = Option.value ~default:[] name_aliases.qualifiers;
  firstname_aliases = Option.value ~default:[] name_aliases.firstname_aliases;
  surname_aliases = Option.value ~default:[] name_aliases.surname_aliases;
}

module DateConv = Api_util.Date_converter.Make (Api_v2_piqi)

let events_to_piqi event =
  let event_type, name = match event.Common.event_type with
    | Geneweb.Event.Pevent (Def.Epers_Name name) -> Some `epers_custom, Some name
    | Geneweb.Event.Pevent pe -> Some (Api_piqi_util.piqi_pevent_name_of_pevent_name pe), None
    | Geneweb.Event.Fevent (Def.Efam_Name name) -> Some `efam_custom, Some name
    | Geneweb.Event.Fevent fe -> Some (Api_piqi_util.piqi_fevent_name_of_fevent_name fe), None
  in
  {
  Api_v2_piqi.Event.event_type = event_type;
  name;
  date = Option.map DateConv.piqi_date_of_date event.date;
  place = event.place;
  notes = event.notes;
  sources = event.sources;
}
let paginated_events_to_piqi events = {
  Api_v2_piqi.Paginated_events.elements = List.map events_to_piqi events.Common.elements;
  page_number = Int32.of_int events.page_number;
  total_count = Int32.of_int events.total_count;
}

let rec to_piqi response =
  {
    Api_v2_piqi.Person.index = response.index;
    npocc = Option.map npocc_to_piqi response.npocc;
    lastname = response.lastname;
    firstname = response.firstname;
    sex = Option.map sex_to_piqi response.sex;
    image = response.image;
    public_name = response.public_name;
    name_aliases = Option.map name_aliases_to_piqi response.name_aliases;
    father = Option.map to_piqi response.father;
    mother = Option.map to_piqi response.mother;
    sosa = response.sosa;
    occupation = response.occupation;
    notes = response.notes;
    sources = response.sources;
    titles = Option.value ~default:[] response.titles;
    events = Option.map paginated_events_to_piqi response.events;
  }

let rec response_of_person person = {
  index = Common.Person.get_index person;
  npocc = Common.Person.get_npocc person;
  lastname = Common.Person.get_lastname person;
  firstname = Common.Person.get_firstname person;
  sex = Common.Person.get_sex person;
  image = Common.Person.get_image person;
  public_name = Common.Person.get_public_name person;
  name_aliases = Common.Person.get_name_aliases person;
  father = Option.map response_of_person (Common.Person.get_father person);
  mother = Option.map response_of_person (Common.Person.get_mother person);
  sosa = Common.Person.get_sosa person;
  occupation = Common.Person.get_occupation person;
  notes = Common.Person.get_notes person;
  sources = Common.Person.get_sources person;
  titles = Common.Person.get_titles person;
  events = Common.Person.get_events person;
}

let response conf base request =
  Option.map response_of_person
    (Common.Person.get_person conf base
       (Request.get_select request)
       (Request.get_fields request))
