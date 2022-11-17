credit_card <- read.csv(
  "~/Documents/Cours/Machine Learning/R_Projet/UCI_Credit_Card.csv",
  h = TRUE
)

credit_card[1:5, ]
renamed.data <- rename(
  credit_card,
  DEFAULT_PAYMENT = default.payment.next.month
)

summary(renamed.data)
dim(renamed.data)

sapply(renamed.data, mean, na.rm = TRUE)
sapply(renamed.data, sd, na.rm = TRUE)
sapply(renamed.data, median, na.rm = TRUE)

cat("\014")
