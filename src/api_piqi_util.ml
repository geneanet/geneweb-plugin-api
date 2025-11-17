let p_getenvbin env label =
  let decode_varenv = Mutil.gen_decode false in
  Option.map decode_varenv (List.assoc_opt label env)

module Geneweb_date = Date

module Date

    (M : sig
       module Dmy : sig
         type t = { mutable day : int32
                  ; mutable month : int32
                  ; mutable year : int32
                  ; mutable delta : int32
                  }
       end
       module Date : sig
         type t = { mutable cal : [ `gregorian | `julian | `french | `hebrew ] option
                  ; mutable prec : [ `sure | `about | `maybe | `before | `after | `oryear | `yearint ] option
                  ; mutable dmy : Dmy.t option
                  ; mutable dmy2 : Dmy.t option
                  ; mutable text : string option
                  }
       end
     end)

= struct

  let piqi_date_of_date = function
    | Date.Dgreg (dmy, cal) ->
      let cal =
        match cal with
        | Dgregorian -> `gregorian
        | Djulian -> `julian
        | Dfrench -> `french
        | Dhebrew -> `hebrew
      in
      let (prec, dmy, dmy2) =
        let (d, m, y, delta) =
          (Int32.of_int dmy.day, Int32.of_int dmy.month,
           Int32.of_int dmy.year, Int32.of_int dmy.delta)
        in
        let dmy1 = {M.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear d2 ->
            let dmy2 =
              {
                M.Dmy.day = Int32.of_int 0;
                month = Int32.of_int 0;
                year = Int32.of_int d2.year2;
                delta = Int32.of_int 0;
              }
            in
            (`oryear, Some dmy2)
          | YearInt d2 ->
            let dmy2 =
              {
                M.Dmy.day = Int32.of_int 0;
                month = Int32.of_int 0;
                year = Int32.of_int d2.year2;
                delta = Int32.of_int 0;
              }
            in
            (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        M.Date.cal = Some cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
    | Dtext txt ->
      {
        M.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }
end

module Filter

    (M : sig
       module Filter_date : sig
         type t = { mutable day : int32
                  ; mutable month : int32
                  ; mutable year : int32
                  }
       end
       module Filter_date_range : sig
         type t = { mutable date_begin : Filter_date.t
                  ; mutable date_end : Filter_date.t
                  ; mutable only_exact : bool
                  }
       end
       module Filters : sig
         type t = { mutable only_sosa : bool
                  ; mutable only_recent : bool
                  ; mutable sex: [ `male | `female | `unknown ] option
                  ; mutable nb_results : bool
                  ; mutable date_birth : Filter_date_range.t option
                  ; mutable date_death : Filter_date_range.t option
                  }
       end
     end)

    (Mext : sig
       val parse_filters
         : ?opts:Piqirun_ext.options
         -> string
         -> Piqirun_ext.input_format
         -> M.Filters.t
     end)

= struct

  let get_filters conf =
    let filters =
      match (Geneweb.Util.p_getenv conf.Geneweb.Config.env "filters", Geneweb.Util.p_getenv conf.Geneweb.Config.env "input") with
      | (Some d, Some "pb") -> Mext.parse_filters d `pb
      | (Some d, Some "json") -> Mext.parse_filters d `json
      | (Some d, Some "xml") -> Mext.parse_filters d `xml
      | _ -> Mext.parse_filters "" `pb
    in
    let dmy d =
      { Geneweb_date.day = Int32.to_int d.M.Filter_date.day
      ; month = Int32.to_int d.M.Filter_date.month
      ; year = Int32.to_int d.M.Filter_date.year
      ; prec = Sure
      ; delta = 0
      }
    in
    let range = function
      | Some range ->
        let dmy1 = dmy range.M.Filter_date_range.date_begin in
        let dmy2 = dmy range.M.Filter_date_range.date_end in
        let prec = range.M.Filter_date_range.only_exact in
        Some (dmy1, dmy2, prec)
      | None -> None
    in
    { Api_def.only_sosa = filters.M.Filters.only_sosa
    ; only_recent = filters.M.Filters.only_recent
    ; filter_sex =
        (match filters.M.Filters.sex with
         | Some `male -> Some Male
         | Some `female -> Some Female
         | Some `unknown -> Some Neuter
         | _ -> None)
    ; nb_results = filters.M.Filters.nb_results
    ; date_birth = range filters.M.Filters.date_birth
    ; date_death = range filters.M.Filters.date_death
    }

end

module ReferencePerson

    (M : sig
       module Reference_person : sig
         type t = { mutable n : string
                  ; mutable p : string
                  ; mutable oc : int32
                  }
       end
     end)

 = struct

  let person_to_reference_person base p =
    { M.Reference_person.n = Name.lower @@ Gwdb.sou base @@ Gwdb.get_surname p
    ; p = Name.lower @@ Gwdb.sou base @@ Gwdb.get_first_name p
    ; oc = Int32.of_int (Gwdb.get_occ p)
    }

  let empty_reference_person =
    { M.Reference_person.n = "" ; p = "" ; oc = 0l }
end

let print_result conf data =
  let (content_type, output) =
    match p_getenvbin conf.Geneweb.Config.env "output" with
    | Some "pb" -> ("application/octet-stream", Protoc_fmt.protobuf)
    | Some "json" -> ("application/json", Protoc_fmt.json)
    | _ -> exit (-2)
  in
  let data = data output in
  Geneweb.Util.html ~content_type conf ;
  Geneweb.Output.print_sstring conf data

let from_piqi_status = function
  | `bad_request -> Def.Bad_Request
  | `unauthorized -> Def.Unauthorized
  | `forbidden -> Def.Forbidden
  | `not_found -> Def.Not_Found
  | `conflict -> Def.Conflict

(** [print_error conf code]
    Print error code and [raise Exit]
*)
let print_error conf code msg =
  let piqi_error = Api_piqi.default_error () in
  piqi_error.Api_piqi.Error.code <- code ;
  piqi_error.Api_piqi.Error.message <- (match msg with | "" -> None | s -> Some s);
  let data = Encoders.Api.encode_error piqi_error in
  Geneweb.Output.status conf (from_piqi_status code) ;
  print_result conf data ;
  raise Exit

let get_params conf parse =
  match (p_getenvbin conf.Geneweb.Config.env "data", p_getenvbin conf.Geneweb.Config.env "input") with
  | (Some d, Some "pb") -> parse d Protoc_fmt.protobuf
  | (Some d, Some "json") -> parse d Protoc_fmt.json
  | _ -> assert false

let get_params conf parse =
  try get_params conf parse
  with e -> print_error conf `bad_request (Printexc.to_string e)

let piqi_fevent_name_of_fevent_name = function
  | Def.Efam_Marriage -> `efam_marriage
  | Efam_NoMarriage -> `efam_no_marriage
  | Efam_NoMention -> `efam_no_mention
  | Efam_Engage -> `efam_engage
  | Efam_Divorce -> `efam_divorce
  | Efam_Separated -> `efam_separated
  | Efam_Annulation -> `efam_annulation
  | Efam_MarriageBann -> `efam_marriage_bann
  | Efam_MarriageContract -> `efam_marriage_contract
  | Efam_MarriageLicense -> `efam_marriage_license
  | Efam_PACS -> `efam_pacs
  | Efam_Residence -> `efam_residence
  | Efam_Name _ -> failwith __LOC__

let piqi_pevent_name_of_pevent_name = function
  | Def.Epers_Birth -> `epers_birth
  | Epers_Baptism -> `epers_baptism
  | Epers_Death -> `epers_death
  | Epers_Burial -> `epers_burial
  | Epers_Cremation -> `epers_cremation
  | Epers_Accomplishment -> `epers_accomplishment
  | Epers_Acquisition -> `epers_acquisition
  | Epers_Adhesion -> `epers_adhesion
  | Epers_BaptismLDS -> `epers_baptismlds
  | Epers_BarMitzvah -> `epers_barmitzvah
  | Epers_BatMitzvah -> `epers_batmitzvah
  | Epers_Benediction -> `epers_benediction
  | Epers_ChangeName -> `epers_changename
  | Epers_Circumcision-> `epers_circumcision
  | Epers_Confirmation -> `epers_confirmation
  | Epers_ConfirmationLDS -> `epers_confirmationlds
  | Epers_Decoration -> `epers_decoration
  | Epers_DemobilisationMilitaire -> `epers_demobilisationmilitaire
  | Epers_Diploma -> `epers_diploma
  | Epers_Distinction -> `epers_distinction
  | Epers_Dotation -> `epers_dotation
  | Epers_DotationLDS -> `epers_dotationlds
  | Epers_Education -> `epers_education
  | Epers_Election -> `epers_election
  | Epers_Emigration -> `epers_emigration
  | Epers_Excommunication -> `epers_excommunication
  | Epers_FamilyLinkLDS -> `epers_familylinklds
  | Epers_FirstCommunion -> `epers_firstcommunion
  | Epers_Funeral -> `epers_funeral
  | Epers_Graduate -> `epers_graduate
  | Epers_Hospitalisation -> `epers_hospitalisation
  | Epers_Illness -> `epers_illness
  | Epers_Immigration-> `epers_immigration
  | Epers_ListePassenger -> `epers_listepassenger
  | Epers_MilitaryDistinction -> `epers_militarydistinction
  | Epers_MilitaryPromotion -> `epers_militarypromotion
  | Epers_MilitaryService -> `epers_militaryservice
  | Epers_MobilisationMilitaire -> `epers_mobilisationmilitaire
  | Epers_Naturalisation -> `epers_naturalisation
  | Epers_Occupation -> `epers_occupation
  | Epers_Ordination -> `epers_ordination
  | Epers_Property -> `epers_property
  | Epers_Recensement -> `epers_recensement
  | Epers_Residence -> `epers_residence
  | Epers_Retired -> `epers_retired
  | Epers_ScellentChildLDS -> `epers_scellentchildlds
  | Epers_ScellentParentLDS -> `epers_scellentparentlds
  | Epers_ScellentSpouseLDS -> `epers_scellentspouselds
  | Epers_VenteBien -> `epers_ventebien
  | Epers_Will -> `epers_will
  | Epers_Name _ -> failwith __LOC__

let pevent_name_of_piqi_pevent_name = function
  | `epers_birth -> Def.Epers_Birth
  | `epers_baptism -> Epers_Baptism
  | `epers_death -> Epers_Death
  | `epers_burial -> Epers_Burial
  | `epers_cremation -> Epers_Cremation
  | `epers_accomplishment -> Epers_Accomplishment
  | `epers_acquisition -> Epers_Acquisition
  | `epers_adhesion -> Epers_Adhesion
  | `epers_baptismlds -> Epers_BaptismLDS
  | `epers_barmitzvah -> Epers_BarMitzvah
  | `epers_batmitzvah -> Epers_BatMitzvah
  | `epers_benediction -> Epers_Benediction
  | `epers_changename -> Epers_ChangeName
  | `epers_circumcision -> Epers_Circumcision
  | `epers_confirmation -> Epers_Confirmation
  | `epers_confirmationlds -> Epers_ConfirmationLDS
  | `epers_decoration -> Epers_Decoration
  | `epers_demobilisationmilitaire -> Epers_DemobilisationMilitaire
  | `epers_diploma -> Epers_Diploma
  | `epers_distinction -> Epers_Distinction
  | `epers_dotation -> Epers_Dotation
  | `epers_dotationlds -> Epers_DotationLDS
  | `epers_education -> Epers_Education
  | `epers_election -> Epers_Election
  | `epers_emigration -> Epers_Emigration
  | `epers_excommunication -> Epers_Excommunication
  | `epers_familylinklds -> Epers_FamilyLinkLDS
  | `epers_firstcommunion -> Epers_FirstCommunion
  | `epers_funeral -> Epers_Funeral
  | `epers_graduate -> Epers_Graduate
  | `epers_hospitalisation -> Epers_Hospitalisation
  | `epers_illness -> Epers_Illness
  | `epers_immigration -> Epers_Immigration
  | `epers_listepassenger -> Epers_ListePassenger
  | `epers_militarydistinction -> Epers_MilitaryDistinction
  | `epers_militarypromotion -> Epers_MilitaryPromotion
  | `epers_militaryservice -> Epers_MilitaryService
  | `epers_mobilisationmilitaire -> Epers_MobilisationMilitaire
  | `epers_naturalisation -> Epers_Naturalisation
  | `epers_occupation -> Epers_Occupation
  | `epers_ordination -> Epers_Ordination
  | `epers_property -> Epers_Property
  | `epers_recensement -> Epers_Recensement
  | `epers_residence -> Epers_Residence
  | `epers_retired -> Epers_Retired
  | `epers_scellentchildlds -> Epers_ScellentChildLDS
  | `epers_scellentparentlds -> Epers_ScellentParentLDS
  | `epers_scellentspouselds -> Epers_ScellentSpouseLDS
  | `epers_ventebien -> Epers_VenteBien
  | `epers_will -> Epers_Will

let fevent_name_of_piqi_fevent_name = function
  | `efam_marriage -> Def.Efam_Marriage
  | `efam_no_marriage -> Efam_NoMarriage
  | `efam_no_mention -> Efam_NoMention
  | `efam_engage -> Efam_Engage
  | `efam_divorce -> Efam_Divorce
  | `efam_separated -> Efam_Separated
  | `efam_annulation -> Efam_Annulation
  | `efam_marriage_bann -> Efam_MarriageBann
  | `efam_marriage_contract -> Efam_MarriageContract
  | `efam_marriage_license -> Efam_MarriageLicense
  | `efam_pacs -> Efam_PACS
  | `efam_residence -> Efam_Residence

let piqi_access_to_access = function
  | `access_iftitles -> Def.IfTitles
  | `access_public -> Public
  | `access_private -> Private
