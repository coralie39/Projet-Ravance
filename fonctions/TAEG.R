#' Calcul du TAEG
#'
#' @param montant_credit nombre en euros 
#' @param frais nombre en euros
#' @param mensualite nombre en euros (récupéré à partir du tableau d'amortissement)
#' @param duree_credit nombre d'années
#'
#' @return
#' @export
#'
#' @examples TAEG(20000, 1000, 220.46, 10)
#' 

TAEG <- function(montant_credit, frais, mensualite, duree_credit){
  taux_mensuel_effectif <- uniroot(function(t) montant_credit - frais - mensualite * (1-(1+t)^(-duree_credit*12))/t, 
                                   lower = 1e-15, upper = 1)$root
  taeg <- round((1 + taux_mensuel_effectif)^12 - 1, 4)
  taeg
}
