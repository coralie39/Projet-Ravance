#' Calcul de la capacité d'emprunt
#'
#' @param mensualite nombre (en euros / mois)
#' @param taux taux moyen (en %) correspondant à la durée du crédit
#' @param taux taux assurance moyen (en %)
#' @param duree nombre d'années 
#'
#' @return calcul de la capacité d'emprunt : nombre (en euros)
#' @export
#'
#' @examples
calculCapaciteEmprunt <- function(mensualite, taux, taux_ass, duree){
  taux <- taux / (100*12)
  nb_mensualite <- duree * 12
  denominateur <- ((taux * (1+taux)^nb_mensualite) / ((1+taux)^nb_mensualite-1)) + taux_ass / (12*100)
  capacite_emprunt <- mensualite / denominateur
  capacite_emprunt
}