val print_info_base : Geneweb.Config.config -> Gwdb.base -> unit

val print_loop : Geneweb.Config.config -> Gwdb.base -> unit
(** [print_loop conf base]
    If there is a loop in the base print a person being its own ancestor.
    Otherwise, print a dummy (empty) person instead. **)

val print_info_ind : Geneweb.Config.config -> Gwdb.base -> unit
val print_list_ref_person : Geneweb.Config.config -> Gwdb.base -> unit
val print_ref_person_from_ip : Geneweb.Config.config -> Gwdb.base -> unit

val print_first_available_person : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Retourne la "première" personne accessible d'un arbre
                    et visible. *)

val print_find_sosa : Geneweb.Config.config -> Gwdb.base -> unit

val print_last_modified_persons : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Retourne la liste des dernières personnes modifiées
                    par le magicien. Si aucun magicien n'est donné, alors
                    c'est les dernières personnes. *)

val print_last_visited_persons : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Retourne la liste des dernières personnes visités
                    par le user donné en paramètre. *)

val print_max_ancestors : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Recherche la personne qui a le plus d'ancêtres. *)

val print_img_all : Geneweb.Config.config -> Gwdb.base -> unit
val print_img_person : Geneweb.Config.config -> Gwdb.base -> unit
val print_updt_image : Geneweb.Config.config -> Gwdb.base -> unit
val print_base_warnings : Geneweb.Config.config -> Gwdb.base -> unit
val print_person_warnings : Geneweb.Config.config -> Gwdb.base -> unit
val print_all_persons : Geneweb.Config.config -> Gwdb.base -> unit
val print_all_families : Geneweb.Config.config -> Gwdb.base -> unit
val history : Geneweb.Config.config -> Gwdb.base -> unit
