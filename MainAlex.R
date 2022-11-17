set.seed(1234)

credit_card <- read.csv(
  "~/Documents/Cours/Machine Learning/R_Projet/UCI_Credit_Card.csv",
  h = TRUE
)

# Question 1
dim(credit_card)

credit_card[1:5, ]
# Question 2
renamed.data <- rename(
  credit_card,
  DEFAULT_PAYMENT = default.payment.next.month
)
# Question 3
summary(renamed.data)

# question 4
sapply(renamed.data, mean, na.rm = TRUE)
sapply(renamed.data, sd, na.rm = TRUE)
sapply(renamed.data, median, na.rm = TRUE)

# Question 5
# La colonne MARRIAGE possède des données où la valeur est 0, ce qui n'est pas possible selon la description du dataset. #nolint
# De même pour la colonne EDUCATION, où la valeur 0 et 5 ne sont pas dans la description du dataset. #nolint

renamed.data$EDUCATION[renamed.data$EDUCATION == 0] <- 4
renamed.data$MARRIAGE[renamed.data$MARRIAGE == 0] <- 3
df_status(renamed.data)

# Question 6

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

EDUCATION_Count
SEXE_Count
MARRIAGE_Count
# Question 7
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

# Question 11
train <- renamed.data[split.data, ]
test <- renamed.data[!split.data, ]
dim(train)
dim(test)
# Quand on sépare les données en train et test, on permet de travailler sur les données de train et ensuite on teste ces donnes sur test afin de juger la précision de notre modèle. #nolint

glm.fitl <- glm(
  DEFAULT_PAYMENT ~ . +
    SEX + EDUCATION + MARRIAGE + AGE + PAY_5 + BILL_AMT1 + PAY_AMT1,
  data = train, family = binomial(link = logit)
)

glm.test.fitl <- glm(
  DEFAULT_PAYMENT ~ . +
    SEX + EDUCATION + MARRIAGE + AGE + PAY_5 + BILL_AMT1 + PAY_AMT1,
  data = test, family = binomial(link = logit)
)

summary(glm.fitl)
plot(glm.fitl, which = 1)
train_auc <- predict(glm.fitl, type = "response")
test_auc <- predict(glm.test.fitl, newdata = test, type = "response")

gini_train <- 2 * auc(train$DEFAULT_PAYMENT, train_auc) - 1
gini_train

gini_test <- 2 * auc(test$DEFAULT_PAYMENT, test_auc) - 1
gini_test

# L'indice de Gini est un indice qui permet de mesurer la qualité d'un modèle de classification où tous les salaires, revenus sont supérieurs à 0. #nolint
# On voit que les indices de Gini ont une valeur proche de 0.4, ce qui est un bon indice d'égalité entre notre modèles.

# B

# Un arbre de décision est un schéma représentant les résultats possibles d'une série de choix interconnectés et les probabilités associées à chaque résultat. #nolint
# Les paramètres à varier pour améliorer la précision de l'arbre de décision sont le nombre de noeuds, la profondeur de l'arbre, le nombre de variables à prendre en compte pour la construction de l'arbre. #nolint

# Une forêt aléatoire est un ensemble d'arbres de décision. Chaque arbre est construit à partir d'un échantillon aléatoire de données. #nolint
cat("\014")
