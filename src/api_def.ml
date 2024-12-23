type ('a, 'b) pb_person =
  | PLight of 'a
  | PFull of 'b
;;

type filters =
  { only_sosa : bool;
    only_recent : bool;
    filter_sex : Def.sex option;
    nb_results : bool;
    date_birth : (Geneweb_util.Date.dmy * Geneweb_util.Date.dmy * bool) option;
    date_death : (Geneweb_util.Date.dmy * Geneweb_util.Date.dmy * bool) option;
  }
;;
