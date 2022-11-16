#' Calcul du montant des mensualités
#'
#' @param montant_emprunte nombre eu euros
#' @param taux_annuel nombre en % (ex : 2.54 pour 2.54 %)
#' @param duree nombre d'années
#'
#' @return montant des mensualités : nombre (en euros)
#' @export
#'
#' @examples calculerMensualite(10000, 5, 1)
#' 

calculerMensualite <- function(montant_emprunte, taux_annuel, duree){
  taux_mensuel <- taux_annuel / (100*12)
  nb_mensualite <- duree * 12
  mensualite <- (montant_emprunte * taux_mensuel * (1+taux_mensuel)^nb_mensualite) / ((1+taux_mensuel)^nb_mensualite-1)
  mensualite
}



