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
  birth : Common.event option;
  baptism : Common.event option;
  death : Common.event option;
  burial : Common.event option;
  related : (Def.relation_type * t) list option;
  rparents : (Def.relation_type * t) list option;
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
  birth = Common.Person.get_birth person;
  baptism = Common.Person.get_baptism person;
  death = Common.Person.get_death person;
  burial = Common.Person.get_burial person;
  related = Option.map relations_to_piqi (Common.Person.get_related person);
  rparents = Option.map relations_to_piqi (Common.Person.get_rparents person);
}

and relations_to_piqi relations =
  List.map (fun (relation_type, person) ->
      relation_type, response_of_person person
    ) relations

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

let death_type_to_piqi = function
  | Def.NotDead -> `not_dead
  | Death (_,_) -> `dead
  | DeadYoung -> `dead_young
  | DeadDontKnowWhen -> `dead_dont_know_when
  | DontKnowIfDead -> `dont_know_if_dead
  | OfCourseDead -> `of_course_dead

let burial_type_to_piqi = function
    Def.UnknownBurial -> `dont_know
  | Buried _ -> `buried
  | Cremated _ -> `cremated

module DateConv = Api_util.Date_converter.Make (Api_v2_piqi)

let rec events_to_piqi event =
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
    spouse = Option.map (fun spouse_response -> to_piqi (response_of_person spouse_response)) event.spouse;
    witnesses = Option.map paginated_witnesses_to_piqi event.witnesses;
    death_type = Option.map death_type_to_piqi event.death_type;
    burial_type = Option.map burial_type_to_piqi event.burial_type;
  }

and paginated_events_to_piqi events = {
  Api_v2_piqi.Paginated_events.elements = List.map events_to_piqi events.Common.elements;
  page_number = Int32.of_int events.page_number;
  total_count = Int32.of_int events.total_count;
}

and paginated_witnesses_to_piqi (witnesses : Common.paginated_witnesses) = {
  Api_v2_piqi.Paginated_witnesses.elements = List.map witness_to_piqi witnesses.Common.elements;
  page_number = Int32.of_int witnesses.page_number;
  total_count = Int32.of_int witnesses.total_count;
}

and witness_to_piqi witness = {
  Api_v2_piqi.Witness.witness_type = Some (Api_util.piqi_of_witness_kind witness.witness_type);
  person = Some (to_piqi (response_of_person witness.witness));
  note = witness.note;
}

and relation_type_to_piqi = function
  | Def.Adoption -> `rchild_adoption
  | Recognition -> `rchild_recognition
  | CandidateParent -> `rchild_candidate_parent
  | GodParent -> `rchild_god_parent
  | FosterParent -> `rchild_foster_parent

and rparents_relation_type_to_piqi = function
  | Def.Adoption -> `rparent_adoption
  | Recognition -> `rparent_recognition
  | CandidateParent -> `rparent_candidate_parent
  | GodParent -> `rparent_god_parent
  | FosterParent -> `rparent_foster_parent

and relation_to_piqi (relation_type, person) = {
  Api_v2_piqi.Relation.relation_type = Some (relation_type_to_piqi relation_type);
  person = Some (to_piqi person);
}

and rparent_to_piqi (relation_type, person) = {
  Api_v2_piqi.Relation.relation_type = Some (rparents_relation_type_to_piqi relation_type);
  person = Some (to_piqi person);
}

and to_piqi response =
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
    birth = Option.map events_to_piqi response.birth;
    baptism = Option.map events_to_piqi response.baptism;
    death = Option.map events_to_piqi response.death;
    burial = Option.map events_to_piqi response.burial;
    related = Option.value ~default:[] (Option.map (List.map relation_to_piqi) response.related);
    rparents = Option.value ~default:[] (Option.map (List.map rparent_to_piqi) response.rparents);
  }

let response conf base request =
  Option.map response_of_person
    (Common.Person.get_person conf base
       (Request.get_select request)
       (Request.get_fields request))
