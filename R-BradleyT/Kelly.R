Kelly <- function(Win1, Win2, Value) {
  answ1 <- (Value * Win1 - 1)/(Win1 - 1)
  answ2 <- ((1-Value) * Win2 - 1)/(Win2 - 1)
  return (answ1, answ2)
}