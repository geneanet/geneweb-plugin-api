(** Functions to filter and mask person fields according to GeneWeb privacy rules. *)

type t
(** Abstract token representing the precomputed privacy visibility state for a person. *)

val evaluate : Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> t
(** [evaluate conf base p] computes whether a person is hidden and visible according to GeneWeb privacy rules. *)

val is_hidden : t -> bool
(** [is_hidden t] returns whether the person is hidden. *)

val is_visible : t -> bool
(** [is_visible t] returns whether the person is visible (authenticated or public access). *)

val surname_lower : t -> Gwdb.base -> Gwdb.person -> string
(** [surname_lower visibility base p] returns the lowercased surname of person [p]
    or an empty string if hidden. *)

val first_name_lower : t -> Gwdb.base -> Gwdb.person -> string
(** [first_name_lower visibility base p] returns the lowercased first name of person [p]
    or an empty string if hidden. *)

val surname : ?mask:string -> t -> ('a, 'b, string) Def.gen_person -> string
(** [surname ?mask visibility gen_p] returns the surname of person [p] or [mask]
    (defaults to ["x"]) if hidden. *)

val first_name : ?mask:string -> t -> ('a, 'b, string) Def.gen_person -> string
(** [first_name ?mask visibility gen_p] returns the first name of person [p] or [mask]
    (defaults to ["x"]) if hidden. *)

val public_name : t -> ('a, 'b, string) Def.gen_person -> string option
(** [public_name visibility gen_p] returns the public name if names are not hidden and non-empty. *)

val aliases : t -> ('a, 'b, string) Def.gen_person -> string list
(** [aliases visibility gen_p] returns the list of aliases if names are not hidden,
    or empty list if hidden. *)

val qualifiers : t -> ('a, 'b, string) Def.gen_person -> string list
(** [qualifiers visibility gen_p] returns the list of qualifiers if names are not hidden,
    or empty list if hidden. *)

val first_names_aliases : t -> ('a, 'b, string) Def.gen_person -> string list
(** [first_names_aliases visibility gen_p] returns the first name aliases if names are not hidden. *)

val surnames_aliases : t -> ('a, 'b, string) Def.gen_person -> string list
(** [surnames_aliases visibility gen_p] returns the surname aliases if names are not hidden. *)

val titles :
  t -> Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> string list
(** [titles visibility conf base p] returns the nobility titles if authenticated. *)

val titles_with_links :
  t -> Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> string list
(** [titles_with_links visibility conf base p] returns the nobility titles formatted with links if authenticated. *)
