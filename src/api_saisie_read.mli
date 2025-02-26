val short_dates_text :
  Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> string

val string_of_date_and_conv :
  Geneweb.Config.config ->
  Date.date ->
  string
  * string
  * string
  * string
  * [> `french | `gregorian | `hebrew | `julian ] option
(** [Description] : Renvoie la date, la date traduite et le calendrier au
                    format texte.
      *)

val person_firstname_surname_txt : Gwdb.base -> Gwdb.person -> string * string

val print_person_tree : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie un objet personne qui servira à afficher
      toutes les informations sur le panneau latéral de l'arbre de
      navigation.
 *)

val print_fiche_person : Geneweb.Config.config -> Gwdb.base -> unit
val print_nb_ancestors : Geneweb.Config.config -> Gwdb.base -> unit

val print_graph_tree : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Retourne un graphe d'ascendance et de descendance
       d'une personne
 *)
