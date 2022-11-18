credit_card <- read.csv(
  "~/Documents/Cours/Machine Learning/R_Projet/UCI_Credit_Card.csv", 
  h = TRUE
)

credit_card[1:5, ]


# install.packages("dplyr")
library(dplyr)

EDUCATION_Count <- credit_card %>% group_by(EDUCATION) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

SEXE_Count <- credit_card %>% group_by(SEX) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

MARRIAGE_Count <- credit_card %>% group_by(MARRIAGE) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

PAYMENT_Count <- credit_card %>% group_by(default.payment.next.month) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

##################################################################

education_additional_others <- EDUCATION_Count$total_count[5] + EDUCATION_Count$total_count[6] + EDUCATION_Count$total_count[7]
education_values_list <- c(
  EDUCATION_Count$total_count[2], EDUCATION_Count$total_count[3],
  EDUCATION_Count$total_count[4], education_additional_others
)
education_labels_list <- c(
  "Graduate School", "University", "High School", "Others"
)
education_piepercent <- round(100 * education_values_list / sum(education_values_list), 1)
education_labels <- paste(education_labels_list, education_piepercent, '%')
pie(
  education_values_list, education_labels, 
  col = rainbow(length(education_values_list))
)

##############################

sexe_values_list <- c(
  SEXE_Count$total_count[1], SEXE_Count$total_count[2]
)
sexe_labels_list <- c("Male", "Female")
sexe_piepercent <- round(100 * sexe_values_list / sum(sexe_values_list), 1)
sexe_labels <- paste(sexe_labels_list, sexe_piepercent, '%')
pie(
  sexe_values_list, sexe_labels, 
  col = rainbow(length(sexe_values_list))
)

##############################

marriage_values_list <- c(
  MARRIAGE_Count$total_count[2], MARRIAGE_Count$total_count[3],
  MARRIAGE_Count$total_count[4]
)
marriage_labels_list <- c("Married", "Single", "Others")
marriage_piepercent <- round(100 * marriage_values_list / sum(marriage_values_list), 1)
marriage_labels <- paste(marriage_labels_list, marriage_piepercent, '%')
pie(
  marriage_values_list, marriage_labels,
  col = rainbow(length(marriage_values_list))
)

##############################

payment_values_list <- c(
  PAYMENT_Count$total_count[1], PAYMENT_Count$total_count[2]
)
payment_labels_list <- c("Bon Payeur", "Mauvais Payeur")
payment_piepercent <- round(100 * payment_values_list / sum(payment_values_list), 1)
payment_labels <- paste(payment_labels_list, payment_piepercent, '%')
pie(
  payment_values_list, payment_labels,
  col = rainbow(length(payment_values_list))
)


##################################################################

Bad_Payor <- credit_card[which(credit_card$default.payment.next.month == 0),]
Good_Payor <- credit_card[which(credit_card$default.payment.next.month == 1),]

##############################

marriage_table <- table(credit_card$MARRIAGE, credit_card$default.payment.next.month)
marriage_table

library("corrplot")
corrplot(
  cor(credit_card), type = "upper", order = "hclust", 
  tl.col = "black", tl.srt = 45
)

##################################################################

LIMIT_BAL_Count <- credit_card %>% group_by(LIMIT_BAL) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

limit_bal_values_list <- c(
  LIMIT_BAL_Count$total_count[1], LIMIT_BAL_Count$total_count[2],
  LIMIT_BAL_Count$total_count[3], LIMIT_BAL_Count$total_count[4],
  LIMIT_BAL_Count$total_count[5], LIMIT_BAL_Count$total_count[6],
  LIMIT_BAL_Count$total_count[7], LIMIT_BAL_Count$total_count[8],
  LIMIT_BAL_Count$total_count[9], LIMIT_BAL_Count$total_count[10]
)
limit_bal_labels_list <- c(
  LIMIT_BAL_Count$LIMIT_BAL[1], LIMIT_BAL_Count$LIMIT_BAL[2],
  LIMIT_BAL_Count$LIMIT_BAL[3], LIMIT_BAL_Count$LIMIT_BAL[4],
  LIMIT_BAL_Count$LIMIT_BAL[5], LIMIT_BAL_Count$LIMIT_BAL[6],
  LIMIT_BAL_Count$LIMIT_BAL[7], LIMIT_BAL_Count$LIMIT_BAL[8],
  LIMIT_BAL_Count$LIMIT_BAL[9], LIMIT_BAL_Count$LIMIT_BAL[10]
)
limit_bal_piepercent <- round(100 * limit_bal_values_list / sum(limit_bal_values_list), 1)
limit_bal_labels <- paste("Prêt de", limit_bal_labels_list, "€ => ", limit_bal_piepercent, '%')
pie(
  limit_bal_values_list, limit_bal_labels,
  col = rainbow(length(limit_bal_values_list))
)

bill_table <- table(credit_card$LIMIT_BAL, credit_card$AGE)
bill_table

hist(
  credit_card$AGE, xlab = "Age", ylab = "Prêt", 
  col = "green", border = "black", main = "Histogramme"
)

barplot(
  table(credit_card$AGE), main="Répartion des âges", xlab="Age", 
  ylab="Nombre de personnes", col = rainbow(length(payment_values_list)), 
  ylim = c(0,2000)
)

sort(unique(credit_card$AGE))

GET_BILL_AGE <- function(number) {
  FILTER_AGE <- credit_card[which(credit_card$AGE == number),]
  BILL_AGE <- round(mean(FILTER_AGE$LIMIT_BAL))
  return (BILL_AGE)
}

A21 <- GET_BILL_AGE(21)
A22<- GET_BILL_AGE(22)
A23 <- GET_BILL_AGE(23)
A24 <- GET_BILL_AGE(24)
A25 <- GET_BILL_AGE(25)
A26 <- GET_BILL_AGE(26)
A27 <- GET_BILL_AGE(27)
A28 <- GET_BILL_AGE(28)
A29 <- GET_BILL_AGE(29)
A30 <- GET_BILL_AGE(30)
A31 <- GET_BILL_AGE(31)
A32 <- GET_BILL_AGE(32)
A33 <- GET_BILL_AGE(33)
A34 <- GET_BILL_AGE(34)
A35 <- GET_BILL_AGE(35)
A36 <- GET_BILL_AGE(36)
A37 <- GET_BILL_AGE(37)
A38 <- GET_BILL_AGE(38)
A39 <- GET_BILL_AGE(39)
A40 <- GET_BILL_AGE(40)
A41 <- GET_BILL_AGE(41)
A42 <- GET_BILL_AGE(42)
A43 <- GET_BILL_AGE(43)
A44 <- GET_BILL_AGE(44)
A45 <- GET_BILL_AGE(45)
A46 <- GET_BILL_AGE(46)
A47 <- GET_BILL_AGE(47)
A48 <- GET_BILL_AGE(48)
A49 <- GET_BILL_AGE(49)
A50 <- GET_BILL_AGE(50)
A51 <- GET_BILL_AGE(51)
A52 <- GET_BILL_AGE(52)
A53 <- GET_BILL_AGE(53)
A54 <- GET_BILL_AGE(54)
A55 <- GET_BILL_AGE(55)
A56 <- GET_BILL_AGE(56)
A57 <- GET_BILL_AGE(57)
A58 <- GET_BILL_AGE(58)
A59 <- GET_BILL_AGE(59)
A60 <- GET_BILL_AGE(60)
A61 <- GET_BILL_AGE(61)
A62 <- GET_BILL_AGE(62)
A63 <- GET_BILL_AGE(63)
A64 <- GET_BILL_AGE(64)
A65 <- GET_BILL_AGE(65)
A66 <- GET_BILL_AGE(66)
A67 <- GET_BILL_AGE(67)
A68 <- GET_BILL_AGE(68)
A69 <- GET_BILL_AGE(69)
A70 <- GET_BILL_AGE(70)
A71 <- GET_BILL_AGE(71)
A72 <- GET_BILL_AGE(72)
A73 <- GET_BILL_AGE(73)
A74 <- GET_BILL_AGE(74)
A75 <- GET_BILL_AGE(75)
A79 <- GET_BILL_AGE(79)

BILL_MEAN_LIST <- c(
  A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34,
  A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48,
  A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62,
  A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A79
)
AGE_SORT <- sort(unique(credit_card$AGE))

BILL_MEAN_LIST

backup_option <- options()

options(scipen = 1000000)
# options(backup_option)
plot(AGE_SORT, BILL_MEAN_LIST)
cat("\014")
