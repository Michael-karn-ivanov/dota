Kelly <- function(probs, team1_id, team2_id, coeff1, coeff2) {
  est1 = probs[probs$X1 == team1_id & probs$X2 == team2_id, "value"]
  est2 = probs[probs$X1 == team2_id & probs$X2 == team1_id, "value"]
  answ1 <- (est1 * coeff1 - 1)/(coeff1 - 1)
  answ2 <- (est2 * coeff2 - 1)/(coeff2 - 1)
  result <- c(answ1, answ2)
  return(result)
}