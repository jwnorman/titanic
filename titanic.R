# load data
train = read.csv("C:/Kaggle/Titanic/train.csv")
test = read.csv("C:/Kaggle/Titanic/test.csv")
train = current
train <- read.csv("~/Desktop/Kaggle/Titanic/titanic/train.csv")
test <- read.csv("~/Desktop/Kaggle/Titanic/titanic/test.csv")
temp <- read.csv("~/Desktop/Kaggle/Titanic/titanic/test.csv")
# save data
current = train
save(current, file = "C:/Kaggle/Titanic/current.Rda")


# variables with no missing data
# PassengerId, Survived, Pclass, Name, Sex, SibSp, Parch, Ticket, Fare

# some missing:
# Age, Cabin, Embarked

# to do
#	- deal with the blanks of Age
#	- in Cabin, note if there are multiple cabins, what levels they are on,
#		and what numbers they are, or if it's missing
#		what about when one ticket is one level, the other another
#	- what to do when factor for testing doesn't exist in train (other than a hack)

#######################################################################
# TICKET
#######################################################################
train$Ticket = as.character(train$Ticket)
train$TicketPrefix = gsub("([^ ]*) ([0-9]+)", "\\1", train$Ticket)
train$TicketNumber = gsub("([^ ]*) ([0-9]+)", "\\2", train$Ticket)
train$TicketPrefix = ifelse(train$TicketPrefix == train$TicketNumber, NA, train$TicketPrefix)
table(train$TicketPrefix)

# test
test$Ticket = as.character(test$Ticket)
test$TicketPrefix = gsub("([^ ]*) ([0-9]+)", "\\1", test$Ticket)
test$TicketNumber = gsub("([^ ]*) ([0-9]+)", "\\2", test$Ticket)
test$TicketPrefix = ifelse(test$TicketPrefix == test$TicketNumber, NA, test$TicketPrefix)
table(test$TicketPrefix)

#######################################################################
# AGE
#######################################################################
train$Age %% 1 == .5
train$AgeType = rep("Real", nrow(train))
train$AgeType = ifelse(train$Age %% 1 == .5, "Estimated", train$AgeType)
current$AgeType = as.factor(current$AgeType)

# test
test$Age %% 1 == .5
test$AgeType = rep("Real", nrow(test))
test$AgeType = ifelse(test$Age %% 1 == .5, "Estimated", test$AgeType)
test$AgeType = as.factor(test$AgeType)

#######################################################################
# CABIN
#######################################################################
train$Cabin = as.character(train$Cabin)
train$Cabin = ifelse(train$Cabin == "", NA, train$Cabin)

# test
test$Cabin = as.character(test$Cabin)
test$Cabin = ifelse(test$Cabin == "", NA, test$Cabin)

#######################################################################
# EMBARKED
#######################################################################
train$Embarked = as.character(train$Embarked)
train$Embarked = ifelse(train$Embarked == "", NA, train$Embarked)
current$Embarked = as.factor(current$Embarked)
# find which variables to use if missing
tapply(current$Fare, current$Embarked, mean, na.rm=TRUE)
table(current$Embarked, current$Title)
table(current$Embarked, current$Pclass)

# test
test$Embarked = as.character(test$Embarked)
test$Embarked = ifelse(test$Embarked == "", NA, test$Embarked)
test$Embarked = as.factor(test$Embarked)
# find which variables to use if missing
tapply(test$Fare, test$Embarked, mean, na.rm=TRUE)
table(test$Embarked, test$Title)
table(test$Embarked, test$Pclass)

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

# turn names to character
train$Name = as.character(train$Name)

# grab title of name
train$Title = gsub("[^,]+, ([^\\.]+).*", "\\1", train$Name)
current$Title = as.factor(current$Title)

# note names that have nicknames
nicknameNums = union(grep("\\(", train$Name), grep('\"', train$Name))
train$hasNickname = integer(nrow(train))
train$hasNickname[nicknameNums] = 1

# look at average age by title, hasNickname
tapply(train$Age, train$Title, mean, na.rm = TRUE)
table(train$Title)

tapply(train$Age, train$hasNickname, mean, na.rm = TRUE)
table(train$hasNickname)

# test
# turn names to character
test$Name = as.character(test$Name)

# grab title of name
test$Title = gsub("[^,]+, ([^\\.]+).*", "\\1", test$Name)
# hack, fixme? (problem if factor doesn't exist in training tree fit)
test$Title = ifelse(test$Title %in% current$Title, test$Title, "Mr")
test$Title = as.factor(test$Title)

# note names that have nicknames
nicknameNums = union(grep("\\(", test$Name), grep('\"', test$Name))
test$hasNickname = integer(nrow(test))
test$hasNickname[nicknameNums] = 1

# look at average age by title, hasNickname
tapply(test$Age, test$Title, mean, na.rm = TRUE)
table(test$Title)

tapply(test$Age, test$hasNickname, mean, na.rm = TRUE)
table(test$hasNickname)





#### Submit (something similar to)
trainfit <- rpart(formula = Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + AgeType, data = current, method = "class", minsplit = 50, maxdepth = 3)
testfit <- predict(trainfit, test) # test, which has been manipulated
guess <- ifelse(testsol[,1] > testsol[,2], 0, 1)
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = submit)
write.csv(submit2, file = "~/Desktop/Kaggle/Titanic/titanic/submit141002.csv", row.names=FALSE)