setwd("E:/R/titanic")

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE , header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE , header = TRUE)

median(titanic.train$Age, na.rm = TRUE)  #it's 28
median(titanic.test$Age, na.rm = TRUE)   #it's 27

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA #added column as we want to combine both the data sets

titanic.full <- rbind(titanic.train , titanic.test) #vertical join basically

names(titanic.train)
names(titanic.test)

titanic.full[titanic.full$Embarked == '' , "Embarked"] <- 'S' #mode of table(titanic.full$Embarked) to replace the missing values

table(is.na(titanic.full$Age)) #to check the missing values

#clean missing values of age
age.median <- median(titanic.full$Age, na.rm = TRUE) #it's 28
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median #assigned median value 28 to missing values

upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker

fare.euqation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
                  formula = fare.euqation,
                  data = titanic.full[outlier.filter, ]
                )
fare.row <- titanic.full[
                        is.na(titanic.full$Fare),
                        c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
                        ]

fare.predictions <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions

#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#split data set back out into train and test
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE, ]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE, ]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerID <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerID)
output.df$Survived <- Survived

write.csv(output.df, file = "E:/R/titanic/kaggle_submission.csv", row.names = FALSE)
