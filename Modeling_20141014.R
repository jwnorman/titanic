# Modeling

#### Logistic regression
SurvivedFactor <- as.factor(train$Survived)
trainfit <- glm(formula = SurvivedFactor ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title
			+ Sex*Age + Pclass*Sex + Pclass*Age,
			family = "binomial", data = train)
stepfit <- step(trainfit, direction="both")
summary(trainfit)
trainfit <- glm(formula = SurvivedFactor ~ Pclass + Age + SibSp + Sex*Age + Pclass*Sex,
			family = "binomial", data = train)
testfit <- predict(stepfit, test, type="response")
guess <- round(testfit)

# Embarked, TicketPrefix, TicketNumber, TicketPrefixPrefix, TicketPrefixSuffix, AgeType still have NAs so I got rid of it
# glm doesn't like missing, naturally removes it. i think it should just treat it as another factor, is that an option?

#### Submit (something similar to)
library(rpart)
library(rpart.plot)
trainfit <- rpart(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
			Embarked + TicketPrefix + TicketNumber + TicketPrefixPrefix + 
			TicketPrefixSuffix + AgeType + Title, data = train, 
			method = "class", control=rpart.control(minsplit = 50, cp = .01))
prp(trainfit)
testfit <- predict(trainfit, test)
test[153, ] # has a fare of NA, deal with this... actually deal with the possibility that any variable could be NA in the test dataset
guess <- ifelse(testfit[,1] > testfit[,2], 0, 1)
submit <- data.frame(PassengerId = test$PassengerId, Survived = guess)
write.csv(submit, file = "~/Desktop/Kaggle/Titanic/titanic/submit141002.csv", row.names=FALSE)
write.csv(submit, file = "C:/Kaggle/Titanic/submit141014.csv", row.names=FALSE)