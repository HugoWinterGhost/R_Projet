default.payment.next.month <- read.csv(
  "C:/UCI_Credit_Card.csv", 
  h = TRUE
)

default.payment.next.month[1:5, ]
dim(default.payment.next.month)

summary(default.payment.next.month)

cat("\014")
