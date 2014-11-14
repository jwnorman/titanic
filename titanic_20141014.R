# load data pc
train = read.csv("C:/Kaggle/Titanic/train.csv")
test = read.csv("C:/Kaggle/Titanic/test.csv")

# load data mac
train <- read.csv("~/Desktop/Kaggle/Titanic/titanic/train.csv")
test <- read.csv("~/Desktop/Kaggle/Titanic/titanic/test.csv")

# save data pc
save(train, file = "C:/Kaggle/Titanic/train.Rda")

#### Raw Data Missing Data
sapply(train, function(x) any(is.na(x)))
sapply(test, function(x) any(is.na(x)))

# variables with no missing data
# train: PassengerId, Survived, Pclass, Name, Sex, SibSp, Parch, Ticket, Fare

# some missing:
# train: Age, Cabin, Embarked

#### After first manipulation, Missing Data
sapply(train, function(x) any(is.na(x)))
sapply(test, function(x) any(is.na(x)))

# variables with no missing data
# train: Survived, PassengerId, Pclass, Name, Sex, Age, SibSp, Parch, Ticket, Fare, Title, Embarked,               hasNickname, AgeType
# test:            PassengerId, Pclass, Name, Sex, Age, SibSp, Parch, Ticket, Fare, Title, Embarked, TicketNumber, hasNickname, AgeType

# some missing:
# train: Cabin, TicketPrefix, TicketNumber, TicketPrefixPrefix, TicketPrefixSuffix
# test:  Cabin, TicketPrefix,             , TicketPrefixPrefix, TicketPrefixSuffix


# to do
#	- in Cabin, note if there are multiple cabins, what levels they are on,
#		and what numbers they are, or if it's missing
#		what about when one ticket is one level, the other another
#	- what to do when factor for testing doesn't exist in train (other than a hack)
#	- in ticket prefix, there are things like C.A. and CA and CA... I think these should count as the same
#		A./5. == A.5. == A/5 == A/5. == A/S (?)
#		A4. == A/4
#		C.A. == CA == CA.
#		WE/P == W.E.P.
#		A lot have STON followed by "/" and then some more characters.. the "/" must indicate something.
# 
#	- basic tree assigns title of Cpt,Don, etc. to 0, but there is only one instance of both Capt and Don, so that is not reliable! fixixxixit

#######################################################################
# TICKET
#######################################################################

editTicket <- function(dataset) {
	require(rpart)
	# create TicketPrefix and TicketNumber
	dataset$Ticket = as.character(dataset$Ticket)
	dataset$TicketPrefix = gsub("([^ ]*) ([0-9]+)", "\\1", dataset$Ticket)
	dataset$TicketNumber = gsub("([^ ]*) ([0-9]+)", "\\2", dataset$Ticket) # FIXME, not catching the numbers perfectly, see 180, 272, 303, 474
	dataset$TicketPrefix = ifelse(dataset$TicketPrefix == dataset$TicketNumber, NA, dataset$TicketPrefix)
	
	# create TicketPrefixPrefix
	dataset$TicketPrefixPrefix <- gsub("([^\\/]+)(\\/).*", "\\1", dataset$TicketPrefix)
	dataset$TicketPrefixPrefix <- gsub("\\.", "", dataset$TicketPrefixPrefix)
	dataset$TicketPrefixSuffix <- gsub("([^\\/]+)\\/(.*)", "\\2", dataset$TicketPrefix)
	dataset$TicketPrefixSuffix <- gsub("\\.", "", dataset$TicketPrefixSuffix)
	dataset$TicketPrefixSuffix[setdiff(1:nrow(dataset), grep("\\/", dataset$TicketPrefix))] = NA

	# alter the classes of the ticket variables
	dataset$Ticket = as.character(dataset$Ticket)
	dataset$TicketPrefix = as.factor(dataset$TicketPrefix)
	dataset$TicketPrefixPrefix = as.factor(dataset$TicketPrefixPrefix)
	dataset$TicketPrefixSuffix = as.factor(dataset$TicketPrefixSuffix)
	dataset$TicketNumber = as.numeric(as.character(dataset$TicketNumber)) # FIXME, turns TicketNumber 180, 272, 303, 474 to NA
	return(dataset)
}

#######################################################################
# AGE
#######################################################################

editAge <- function(dataset) {
	# if age ends in .5, the age is "Estimated"
	# else it's "Real" or NA (Missing)
	dataset$AgeType = rep("Real", nrow(dataset))
	dataset$AgeType = ifelse(is.na(dataset$Age), "Missing", "Real")
	dataset$AgeType = ifelse(dataset$Age %% 1 == .5, "Estimated", dataset$AgeType)
	dataset$AgeType = ifelse(is.na(dataset$AgeType), "Missing", dataset$AgeType)
	dataset$AgeType = as.factor(dataset$AgeType)
	return(dataset)
}

replaceMissingAge <- function(train, test) {
	require(rpart)
	# combine train and test
	survNum <- which(names(train)=="Survived")
	combined <- as.data.frame(rbind(train[,-survNum], test))
	Survived <- train[,survNum]
	trainLength <- nrow(train)
	testLength <- nrow(test)

	# to be run after all manipulating is done.
	missingNums <- which(is.na(combined$Age))
	combinedWONAage <- combined[-missingNums,]
	combinedWNAage  <- combined[missingNums,]

	# grow tree
	agefit <- rpart(formula = Age ~ Pclass + Sex + Fare + Embarked + Title + AgeType, data = combinedWONAage, method = "anova", minsplit = 50, maxdepth = 3)
	testfit <- predict(agefit, combinedWNAage)
	combined[names(testfit), "Age"] <- testfit	

	# put back as train/test
	traintemp <- combined[1:trainLength, ]
	train <<- as.data.frame(cbind(Survived, traintemp))
	test <<- combined[(trainLength+1):(trainLength+testLength), ]
	rownames(test) <<- 1:nrow(test)
}

#######################################################################
# CABIN
#######################################################################
editCabin <- function(dataset) {
	dataset$Cabin = as.character(dataset$Cabin)
	dataset$Cabin = ifelse(dataset$Cabin == "", NA, dataset$Cabin)
	return(dataset)
}

#######################################################################
# EMBARKED
#######################################################################
editEmbarked <- function(dataset) {
	dataset$Embarked = as.character(dataset$Embarked)
	dataset$Embarked = ifelse(dataset$Embarked == "", NA, dataset$Embarked)
	dataset$Embarked = as.factor(dataset$Embarked)
	return(dataset)
}

replaceMissingEmbarked <- function(train, test) {
	require(rpart)
	# combine train and test
	survNum <- which(names(train)=="Survived")
	combined <- as.data.frame(rbind(train[,-survNum], test))
	Survived <- train[,survNum]
	trainLength <- nrow(train)
	testLength <- nrow(test)

	# to be run after all manipulating is done.
	missingNums <- which(is.na(combined$Embarked))
	combinedWONAemb <- combined[-missingNums,]
	combinedWNAemb  <- combined[missingNums,]

	# grow tree
	embfit <- rpart(formula = Embarked ~ Pclass + Sex + Fare + Age + Title + AgeType, data = combinedWONAemb, method = "class", minsplit = 50, maxdepth = 3)
	testfit <- as.data.frame(predict(embfit, combinedWNAemb))
	for (i in 1:nrow(testfit)) {
		for (j in 1:ncol(testfit)) {
			if (testfit[i,j] == max(testfit[i,])) {
				if (j == 1) j = "C"
				if (j == 2) j = "Q"
				if (j == 3) j = "S"
				combined[rownames(testfit[i,]), "Embarked"] <- j
			}
		}
	}

	# put back as train/test
	traintemp <- combined[1:trainLength, ]
	train <<- as.data.frame(cbind(Survived, traintemp))
	test <<- combined[(trainLength+1):(trainLength+testLength), ]
	rownames(test) <<- 1:nrow(test)
}

train[c(62,830),] # yes I could manually replace with C,C but that’s too hacky

tapply(train$Fare, train$Embarked, mean, na.rm=TRUE)
table(train$Embarked, train$Title)
table(train$Embarked, train$Pclass)

#######################################################################
# FARE
#######################################################################

replaceMissingFare <- function(train, test) {
	# combine train and test
	survNum <- which(names(train)=="Survived")
	combined <- as.data.frame(rbind(train[,-survNum], test))
	Survived <- train[,survNum]
	trainLength <- nrow(train)
	testLength <- nrow(test)

	# to be run after all manipulating is done.
	missingNums <- which(is.na(combined$Fare))
	combinedWONAfare <- combined[-missingNums,]
	combinedWNAfare  <- combined[missingNums,]

	# grow tree
	fareFit <- rpart(formula = Fare ~ Pclass + Sex + Age + Embarked + Title + AgeType, data = combinedWONAfare, method = "anova", minsplit = 50, maxdepth = 3)
	testfit <- predict(fareFit, combinedWNAfare)
	combined[names(testfit), "Fare"] <- testfit	

	# put back as train/test
	traintemp <- combined[1:trainLength, ]
	train <<- as.data.frame(cbind(Survived, traintemp))
	test <<- combined[(trainLength+1):(trainLength+testLength), ]
	rownames(test) <<- 1:nrow(test)
}

#######################################################################
# NAMES
#######################################################################

# Name formats:
# Dooley, Mr. Patrick
# Najib, Miss. Adele Kiamie "Jane"
# Potter, Mrs. Thomas Jr (Lily Alexenia Wilson)
# van Melkebeke, Mr. Philemon
# Bystrom, Mrs. (Karolina)
# Simonius-Blumer, Col. Oberst Alfons

editNames <- function(dataset) {
	# turn names to character
	dataset$Name = as.character(dataset$Name)

	# grab title of name
	dataset$Title = gsub("[^,]+, ([^\\.]+).*", "\\1", dataset$Name)
	dataset$Title = as.factor(dataset$Title)

	# note names that have "nicknames"
	nicknameNums = union(grep("\\(", dataset$Name), grep('\"', dataset$Name))
	dataset$hasNickname = integer(nrow(dataset))
	dataset$hasNickname[nicknameNums] = 1

	return(dataset)
}

# look at average age by title, hasNickname
tapply(train$Age, train$Title, mean, na.rm = TRUE)
table(train$Title)

tapply(train$Age, train$hasNickname, mean, na.rm = TRUE)
table(train$hasNickname)

#######################################################################
# train, test, model
#######################################################################

manipulate <- function(dataset) {
	dataset <- editTicket(dataset)
	dataset <- editAge(dataset)
	dataset <- editCabin(dataset)
	dataset <- editEmbarked(dataset)
	dataset <- editNames(dataset)
	return(dataset)
}

learn <- function() {
	train <<- manipulate(train)
	test  <<- manipulate(test)
	replaceMissingAge(train, test)
	replaceMissingFare(train, test)
	replaceMissingEmbarked(train, test)

	# now for the factors that exist in test but not in train, 
	# convert to NA until further plan of action!
	# factor variables: Sex, Embarked, TicketPrefix, TicketPrefixPrefix, TicketPrefixSuffix, AgeType, Title
	# factor variables of concern: Title
	lookForNA <- ifelse(test$Title %in% train$Title, test$Title, NA)
	NAnum <- which(is.na(lookForNA))
	#test$Title[NAnum] <<- NA 
	#test$Title[NAnum] <<- "Missing"
	test$Title[NAnum] <<- "Mr"
}

learn()

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