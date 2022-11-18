################# Projet R #################

############################ Packages ############################

# install.packages("funModeling")
# install.packages("pROC")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("MASS")
# install.packages("rpart")
library(funModeling)
library(pROC)
library(dplyr)
library(corrplot)
library(MASS)
library(rpart)

set.seed(1234)

credit_card <- read.csv(
  "~/Documents/Cours/Machine Learning/R_Projet/UCI_Credit_Card.csv",
  h = TRUE
)

############################ Question 1 ############################

dim(credit_card)

credit_card[1:5, ]

############################ Question 2 ############################

renamed.data <- rename(
  credit_card,
  DEFAULT_PAYMENT = default.payment.next.month
)

############################ Question 3 ############################

summary(renamed.data)

############################ Question 4 ############################

sapply(renamed.data, mean, na.rm = TRUE)
sapply(renamed.data, sd, na.rm = TRUE)
sapply(renamed.data, median, na.rm = TRUE)

############################ Question 5 ############################

# La colonne MARRIAGE possède des données où la valeur est 0, ce qui n'est pas possible selon la description du dataset. #nolint
# De même pour la colonne EDUCATION, où la valeur 0 et 5 ne sont pas dans la description du dataset. #nolint

renamed.data$EDUCATION[renamed.data$EDUCATION == 0] <- 4
renamed.data$MARRIAGE[renamed.data$MARRIAGE == 0] <- 3
df_status(renamed.data)

############################ Question 6 ############################

EDUCATION_Count <- renamed.data %>%
  group_by(EDUCATION) %>%
  summarise(
    total_count = n(),
    .groups = "drop"
  )

SEXE_Count <- renamed.data %>%
  group_by(SEX) %>%
  summarise(
    total_count = n(),
    .groups = "drop"
  )

MARRIAGE_Count <- renamed.data %>%
  group_by(MARRIAGE) %>%
  summarise(
    total_count = n(),
    .groups = "drop"
  )

PAYMENT_Count <- renamed.data %>% 
  group_by(DEFAULT_PAYMENT) %>% 
  summarise(
    total_count=n(),
    .groups = 'drop'
  )

education_additional_others <- EDUCATION_Count$total_count[4] + EDUCATION_Count$total_count[5] + EDUCATION_Count$total_count[6]
education_values_list <- c(EDUCATION_Count$total_count[1], EDUCATION_Count$total_count[2], EDUCATION_Count$total_count[3], education_additional_others)
education_labels_list <- c("Graduate School", "University", "High School", "Others")
education_piepercent <- round(100 * education_values_list / sum(education_values_list), 1)
education_labels <- paste(education_labels_list, education_piepercent, '%')
pie(education_values_list, education_labels, col = rainbow(length(education_values_list)))

payment_values_list <- c(PAYMENT_Count$total_count[1], PAYMENT_Count$total_count[2])
payment_labels_list <- c("Bon Payeur", "Mauvais Payeur")
payment_piepercent <- round(100 * payment_values_list / sum(payment_values_list), 1)
payment_labels <- paste(payment_labels_list, payment_piepercent, '%')
pie(payment_values_list, payment_labels, col = rainbow(length(payment_values_list)))

Bad_Payor <- credit_card[which(credit_card$default.payment.next.month == 0),]
Good_Payor <- credit_card[which(credit_card$default.payment.next.month == 1),]

marriage_table <- table(credit_card$MARRIAGE, credit_card$default.payment.next.month)
marriage_table

corrplot(
  cor(credit_card), type = "upper", order = "hclust", 
  tl.col = "black", tl.srt = 45
)

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

GET_MEAN_BILL_AGE <- function(number) {
  FILTER_AGE <- credit_card[which(credit_card$AGE == number),]
  BILL_AGE <- round(mean(FILTER_AGE$LIMIT_BAL))
  return (BILL_AGE)
}

A21 <- GET_MEAN_BILL_AGE(21)
A21


############################ Question 7 ############################

cor.data <- cor(renamed.data)
corrplot(cor.data, method = "square")
# On remarque que les variables BILL_AMT1, BILL_AMT2, BILL_AMT3, BILL_AMT4, BILL_AMT5, BILL_AMT6 sont fortement corrélées entre elles. #nolint
# On remarque enfin que les variables PAY_1, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6 sont fortement corrélées entre elles. #nolint
renamed.data$SEX <- as.factor(renamed.data$SEX)
renamed.data$EDUCATION <- as.factor(renamed.data$EDUCATION)
renamed.data$MARRIAGE <- as.factor(renamed.data$MARRIAGE)
typeof(renamed.data$SEX)
df_status(renamed.data)

split.data <- sample(
  c(TRUE, FALSE),
  nrow(renamed.data),
  replace = TRUE,
  prob = c(0.7, 0.3)
)

############################ Question 11 ############################

train <- renamed.data[split.data, ]
test <- renamed.data[!split.data, ]
dim(train)
dim(test)
# Quand on sépare les données en train et test, on permet de travailler sur les données de train et ensuite on teste ces donnes sur test afin de juger la précision de notre modèle. #nolint

glm.fitl <- glm(
  DEFAULT_PAYMENT ~ . +
    SEX + EDUCATION + MARRIAGE + AGE + PAY_5 + BILL_AMT1 + PAY_AMT1,
  data = train, family = binomial
)

glm.test.fitl <- glm(
  DEFAULT_PAYMENT ~ . +
    SEX + EDUCATION + MARRIAGE + AGE + PAY_5 + BILL_AMT1 + PAY_AMT1,
  data = test, family = binomial
)

summary(glm.fitl)
boxplot(glm.fitl$residuals)

############################ Question 13 ############################

train_auc <- predict(glm.fitl, type = "response")
test_auc <- predict(glm.test.fitl, type = "response")

gini_train <- 2 * auc(train$DEFAULT_PAYMENT, train_auc) - 1
gini_train

gini_test <- 2 * auc(test$DEFAULT_PAYMENT, test_auc) - 1
gini_test

# L'indice de Gini est un indice qui permet de mesurer la qualité d'un modèle de classification où tous les salaires, revenus sont supérieurs à 0. #nolint
# On voit que les indices de Gini ont une valeur proche de 0.4, ce qui indique que les inégalités de salaire, revenus et niveaux sont à peu près égale  entre eux. #nolint

############################ Question 14 ############################

glm.aic.fitl <- glm(
  DEFAULT_PAYMENT ~ .,
  data = test, family = binomial(link = logit)
)
summary(glm.aic.fitl)

data_aic <- stepAIC(glm.aic.fitl, stepwise = TRUE)

############################ Question 15 ############################

data_aic_auc <- predict(glm.aic.fitl, type = "response")

gini_aic <- 2 * auc(test$DEFAULT_PAYMENT, data_aic_auc) - 1
gini_aic

############################ Partie B ############################

# Un arbre de décision est un schéma représentant les résultats possibles d'une série de choix interconnectés et les probabilités associées à chaque résultat. #nolint
# Les paramètres à varier pour améliorer la précision de l'arbre de décision sont le nombre de noeuds, la profondeur de l'arbre, le nombre de variables à prendre en compte pour la construction de l'arbre. #nolint

# Une forêt aléatoire est un ensemble d'arbres de décision. Chaque arbre est construit à partir d'un échantillon aléatoire de données. #nolint

data.tree <- rpart(
  DEFAULT_PAYMENT ~ .,
  data = train,
  method = "class",
  control = rpart.control(minsplit = 5, cp = 0)
)

# text(data.tree, use.n = TRUE, all = FALSE, cex = 0.8)
plotcp(data.tree)
printcp(data.tree)

print(data.tree$cptable[which.min(data.tree$cptable[, "xerror"]), "CP"])
cat("\014")
