#hearing_2122
#REPEATED
#SC_RACE_R
#SC_HISPANIC_R
#age3_2122
#sex_2122
#povlev4_2122
library(yacca)
library(foreign)
library(party)
library(randomForest)
library(gbm)
library(gam)

install.packages("rpart")       # For CART
install.packages("rpart.plot")  # To visualize the CART tree
install.packages("randomForest") # For Random Forest

NSCH.data<-read.spss("C:\\Users\\johnh\\Desktop\\Research Project\\2017-2022 NSCH_Final_Dataset.sav", to.data.frame=TRUE, use.value.labels=FALSE)

NSCH.data.nomiss<-na.omit(NSCH.data)

NSCH.data <- NSCH.data[, c("hearing", "REPEATED", "Race", "Ethnicity", "age_group", "sex", "covid_period", "povlev")]

NSCH.data <- na.omit(NSCH.data)

colnames(NSCH.data)
#RECODING VARIABLES INTO BINARY


#HEARING
# Remove N/A cases 
NSCH.data_clean <- NSCH.data[NSCH.data$hearing != 90, ]

# Recode the variable into binary (1 for Yes, 0 for No)
NSCH.data_clean$hearing_binary <- ifelse(NSCH.data_clean$hearing == 1, 1, 0)

# Check the distribution of the recoded variable
table(NSCH.data_clean$hearing_binary)


#SEX
# Recode sex: 1 (Male) -> 0, 2 (Female) -> 1
NSCH.data_clean$sex_binary <- ifelse(NSCH.data_clean$sex == 1, 0, 1)

# Check the distribution of the recoded variable
table(NSCH.data_clean$sex_binary)


#REPEATED

# Remove N/A cases
NSCH.data_clean <- NSCH.data[NSCH.data$REPEATED != 90, ]

# Recode the variable into binary (1 for Yes, 0 for No)
NSCH.data_clean$REPEATED_binary <- ifelse(NSCH.data_clean$REPEATED == 1, 1, 0)

# Check the distribution of the recoded variable
table(NSCH.data_clean$REPEATED_binary)




#logistic regression

#AGE GROUPS
# Convert age_groups to a factor if it's not already
NSCH.data_clean$age <- as.factor(NSCH.data_clean$age_group)

# Check the levels to ensure they are properly coded
levels(NSCH.data_clean$age)


#race
# Convert race to a factor if it's not already
NSCH.data_clean$race <- factor(NSCH.data_clean$race,
                               levels = c(1, 2, 3, 4, 5, 7),
                               labels = c("White", "Black/African American", "American Indian or Alaska Native", "Asian", "Native Hawaiian and Other Pacific Islander", "Two or More Races"))

# Check the levels to ensure they are properly coded
levels(NSCH.data_clean$race)


#ethnicity
# Convert ethnicity to a factor if it's not already
NSCH.data_clean$ethnicity <- as.factor(NSCH.data_clean$Ethnicity)

# Check the levels to ensure they are properly coded
levels(NSCH.data_clean$ethnicity)


#INCOME
# Convert income to a factor if it's not already
NSCH.data_clean$income <- as.factor(NSCH.data_clean$povlev)

# Check the levels to ensure they are properly coded
levels(NSCH.data_clean$income)


#Covid Period
NSCH.data_clean$covid <- factor(NSCH.data_clean$covid_period,
                                       levels = c(0, 1),
                                       labels = c("Pre-COVID", "Post-COVID"))

#Age group
NSCH.data_clean$age <- factor(NSCH.data_clean$age_group,
                                levels = c(2, 3),
                                labels = c("6-11", "12-17"))

#LOGISTIC MODEL

logit_model <- glm(REPEATED_binary ~ hearing_binary + age + sex_binary + race + ethnicity + income + covid, 
                   data = NSCH.data_clean, 
                   family = binomial)

summary(logit_model)


#with interactions
logit_model2 <- glm(REPEATED_binary ~ hearing_binary * covid + age * covid + sex_binary + race*covid + ethnicity + income * covid + covid, 
                   data = NSCH.data_clean, 
                   family = binomial)

summary(logit_model2)





# Calculating McFadden's Pseudo R-squared
null_deviance <- 25218
residual_deviance <- 24290

r_squared <- 1 - (residual_deviance / null_deviance)
r_squared
