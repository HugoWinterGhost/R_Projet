################# Projet R #################

############################ Question 1 et 2 ############################

default.payment.next.month <- read.csv(
  "~/Documents/Cours/Machine Learning/R_Projet/UCI_Credit_Card.csv", 
  h = TRUE
)

default.payment.next.month[1:5, ]

dim(default.payment.next.month)

############################ Question 3 ############################

summary(default.payment.next.month)

############################ Question 4 ############################

sapply(default.payment.next.month, mean, na.rm = TRUE)
sapply(default.payment.next.month, sd, na.rm = TRUE)
sapply(default.payment.next.month, median, na.rm = TRUE)

############################ Question 5 ############################

# install.packages("funModeling")
library(funModeling)

df_status(default.payment.next.month)

# 0 : pas de crédit ou crédit payé avec pas de retards

plot(default.payment.next.month$EDUCATION)

default.payment.next.month$EDUCATION[
  default.payment.next.month$EDUCATION == 0
] <- 4
default.payment.next.month$MARRIAGE[
  default.payment.next.month$MARRIAGE == 0
] <- 3

default.payment.next.month$EDUCATION

default.payment.next.month$EDUCATION[
  default.payment.next.month$MARRIAGE == 0
]

plot(default.payment.next.month$EDUCATION, 
     default.payment.next.month$MARRIAGE
)

cat("\014")
