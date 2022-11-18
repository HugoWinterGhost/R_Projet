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

cat("\014")
