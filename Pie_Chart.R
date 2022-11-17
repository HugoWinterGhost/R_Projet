default.payment.next.month <- read.csv(
  "~/Documents/Cours/Machine Learning/R_Projet/UCI_Credit_Card.csv", 
  h = TRUE
)

default.payment.next.month[1:5, ]


# install.packages("dplyr")
library(dplyr)

EDUCATION_Count <- default.payment.next.month %>% group_by(EDUCATION) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

SEXE_Count <- default.payment.next.month %>% group_by(SEX) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

MARRIAGE_Count <- default.payment.next.month %>% group_by(MARRIAGE) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )


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
pie(education_values_list, education_labels)


sexe_values_list <- c(
  SEXE_Count$total_count[1], SEXE_Count$total_count[2]
)
sexe_labels_list <- c("Male", "Female")
sexe_piepercent <- round(100 * sexe_values_list / sum(sexe_values_list), 1)
sexe_labels <- paste(sexe_labels_list, sexe_piepercent, '%')
pie(sexe_values_list, sexe_labels)


marriage_values_list <- c(
  MARRIAGE_Count$total_count[2], MARRIAGE_Count$total_count[3],
  MARRIAGE_Count$total_count[4]
)
marriage_labels_list <- c("Married", "Single", "Others")
marriage_piepercent <- round(100 * marriage_values_list / sum(marriage_values_list), 1)
marriage_labels <- paste(marriage_labels_list, marriage_piepercent, '%')
pie(marriage_values_list, marriage_labels)


cat("\014")
