#' Création du tableau d'amortissement
#'
#' @param taux_annuel nombre en % 
#' @param montant_emprunte nombre en euros
#' @param taux_assurance nombre en %
#' @param duree nombre d'années
#'
#' @return tableau d'amortissement
#' @export
#'
#' @examples tableauAmortissement(5,10000,0.35,1)
#' 
#' 

tableauAmortissement <- function(taux_annuel, montant_emprunte, taux_assurance, duree){
  vecteur_mois <- seq(1,duree*12,1)
  taux_mensuel <- taux_annuel / (12*100)
  montant_mensualite <- calculerMensualite(montant_emprunte, taux_annuel, duree)
  montant_assurance <- montant_emprunte * taux_assurance / (12*100)
  
  capital_restant <- montant_emprunte
  interets <- c()
  principal <- c()
  capital_restant_du <- c()
  
  for (i in vecteur_mois){
    interets[i] <- capital_restant * taux_mensuel
    principal[i] <- montant_mensualite - interets[i]
    capital_restant_du[i] <- capital_restant - principal[i]
    capital_restant <- capital_restant_du[i]
  }
  
  
  amortissement <- data.frame(numero = vecteur_mois,
                              capital_restant_du = round(capital_restant_du,2),
                              interets = round(interets,2),
                              principal = round(principal,2),
                              assurance = round(montant_assurance,2),
                              mensualite = round(montant_mensualite,2))
  amortissement
}