(** Compatibility and response emulation adapter bridging legacy API (v1)
    and API v2 engines. *)

val request_of_fiche_parameters :
  Api_saisie_read_piqi.Fiche_parameters.t -> Request.t option
(** [request_of_fiche_parameters params] maps legacy [Fiche_parameters]
    to an API v2 [Request.t] with a synthesized [PersonRequest] field mask
    that populates all fields required by a [Fiche_person] view. *)

val simple_person_of_response :
  Geneweb.Config.config -> Response.t -> Api_saisie_read_piqi.Simple_person.t
(** [simple_person_of_response conf resp] converts an API v2 [Response.t]
    into a legacy [Simple_person.t] record. *)

val fiche_person_of_response :
  Geneweb.Config.config ->
  Gwdb.base ->
  Response.t ->
  Api_saisie_read_piqi.Fiche_person.t
(** [fiche_person_of_response conf base resp] converts an API v2 [Response.t]
    into a legacy [Fiche_person.t] record for Protobuf serialization. *)

val person_of_response :
  Geneweb.Config.config ->
  Gwdb.base ->
  Response.t ->
  Api_saisie_read_piqi.Person.t
(** [person_of_response conf base resp] wraps the translated [Fiche_person.t]
    into a top-level [Api_saisie_read_piqi.Person.t] with [type_ = `fiche]. *)

val print_fiche_person_emulated : Geneweb.Config.config -> Gwdb.base -> unit
(** [print_fiche_person_emulated conf base] executes the legacy [API_FICHE_PERSON]
    workflow using the API v2 engine and translates the output back to legacy format. *)
