val print_auto_complete : string -> Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie la liste unique d'un champ. Par exemple la liste
                    de nom de famille en fonction de ce qui est tapé.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - result : la liste de la recherche.
                                                                           *)

val print_person_search_list_switch : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie la liste des personnes qui ont ce nom ou prénom.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
    - result : la liste de la recherche.                                   *)

val print_person_search_info : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Affiche les informations telles que sur le panneau
                    droit dans l'arbre.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : PersonSearchInfo
                                                                           *)

val print_config : Geneweb.Config.config -> unit
(** [Description] : Renvoi un message contenant la configuration, i.e. la
                    traduction de plusieurs mots clés : pevent, fevent, ...
                    ainsi que le découpage des lieux ...
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - Config
                                                                           *)

val print_add_ind_start_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Fonction qui ajoute une personne à la base lors de la
                    création d'un arbre et de la première saisie.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_mod_ind : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Fonction qui renvoi les informations d'une personne
                    afin d'afficher le formulaire de modification.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_mod_ind_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Fonction qui réalise les modifications d'une personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_add_ind_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Fonction qui ajoute une personne à la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_del_ind_ok : Geneweb.Config.config -> Gwdb.base -> unit
val print_del_fam_ok : Geneweb.Config.config -> Gwdb.base -> unit

val print_add_family : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie le conjoint dive où on a calculé le décès pour
                    le conjoint, ainsi qu'une famille vide.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddFamily : les informations du template.
                                                                           *)

val print_add_family_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Enregistre l'ajout d'une famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_mod_family_request : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] :
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - EditFamily : les informations du template.
*)

val print_mod_family : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] :
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - EditFamily : les informations du template.
                                                                           *)

val print_mod_family_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Enregistre l'ajout d'une famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_add_parents : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie les parents vides où on a calculé le nom pour
                    le père et le décès pour les parents, ainsi qu'une
                    famille vide.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddChild : les informations du template.
                                                                           *)

val print_add_child_ok : Geneweb.Config.config -> Gwdb.base -> unit

val print_add_parents_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Enregistre les modifications de l'ajout de parents.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_add_child : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie un enfant vide pour lequel on calcul son nom
                    et s'il est potentiellement décédé ou pas.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddChild : les informations du template.
                                                                           *)

val print_add_sibling : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Renvoie un frère/sœur vide pour lequel on calcul son
                    nom et s'il est potentiellement décédé ou pas.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - AddSibling : les informations du template.
                                                                           *)

val print_add_sibling_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Enregistre en base les informations envoyées.
      2 cas de figures :
        - ajout d'un conjoint et d'un enfant
        - ajout d'un enfant
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)

val print_add_first_fam : Geneweb.Config.config -> unit
(** [Description] : Permet de vérifier qu'à partir de l'objet AddFirstFam,
      la saisie va bien se passer. C'est elle qui calcul les occ des
      homonymes pour ne pas avoir de conflit et vérifie que les champs
      requis sont bien renseignés.
    [Args] :
      - conf : configuration de la base
    [Retour] :
      - AddFirstFam, ModificationStatus :
         L'objet AddFirstFam modifié afain de ne pas avoir de conflit de
         occ et le status de la réponse si l'utilisateur a fait une mauvaise
         saisie.
                                                                           *)

val print_add_first_fam_ok : Geneweb.Config.config -> Gwdb.base -> unit
(** [Description] : Enregistre en base les informations envoyées.
      On reconstitue toutes les liaisons à la main pour pouvoir les
      enregistrer en base, i.e. ascendants de la personne (famille) et
      descendants de la personne (famille), et éventuelle modification des
      personnes ensuite.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - status : les informations si la modification s'est bien passée.
                                                                           *)
