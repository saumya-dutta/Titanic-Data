train_data <- read.csv(file = "C:/Users/dsaum/RStudioProjects/TitanicML/titanic/train.csv", header = T)
test_data <- read.csv(file = "C:/Users/dsaum/RStudioProjects/TitanicML/titanic/test.csv", header = T)

# survival by sex (stacked bar chart)
table_1 <- table(train_data$Survived, train_data$Sex)
sexSurvival <- barplot(table_1,
        main = "Survival by Sex", 
        xlab = "Sex", ylab = "Survival",
        col = c("skyblue", "coral"),
        legend = c("Did Not Survive", "Survived"),
        beside = T)
text(sexSurvival, table_1 -10, labels = table_1)
graphics.off()

# survival by age (histogram plot)
train_data1 <- na.omit(train_data)
train_data1$NewAge <- cut(train_data1$Age, c(0, 10, 20, 30, 40, 50, 60, 70, 80))
table_2 <- table(train_data1$Survived, train_data1$NewAge)
ageSurvival <- barplot(table_2, main = "Survival by Sex", 
                       xlab = "Age", ylab = "Number Survived",
                       col = c("azure3", "plum"),
                       legend = c("Did Not Survive", "Survived"),
                       beside = T)
text(ageSurvival, table_2 - 5, labels = table_2)
graphics.off()

# survival by Pclass (grouped bar chart)
table_3 <- table(train_data$Pclass, train_data$Survived)
classSurvival <- barplot(table_3,
                       main = "Survival by Class", 
                       xlab = "Survival", ylab = "Number Survived",
                       col = c("pink", "seashell2", "tan"),
                       legend = c("Class 1", "Class 2", "Class 3"),
                       beside = T)
text(classSurvival, table_3 -10, labels = table_3)
graphics.off()

# survival by family
train_data$lastName <- sapply(strsplit(train_data$Name,"[,]"), `[`, 1)
train_data$familySize <- train_data$SibSp + train_data$Parch + 1
train_data$familyType <- ifelse(train_data$familySize >=3, "MEDIUM", "SMALL")
train_data$familyType <- ifelse(train_data$familySize >5, "LARGE", "MEDIUM")

table_4 <- table(train_data$familyType, train_data$Survived)
familySurvival <- barplot(table_4,
                         main = "Survival by Family Size", 
                         xlab = "Survival", ylab = "Number Survived",
                         col = c("khaki1", "thistle", "lightsalmon"),
                         legend = c("SMALL", "MEDIUM", "LARGE"),
                         beside = T)
text(familySurvival, table_4 -10, labels = table_4)

# survival by destination embarked
table_5 <- table(train_data$Embarked, train_data$Survived)
destinationSurvival <- barplot(table_5,
                         main = "Survival by Destination Embarked", 
                         xlab = "Survival", ylab = "Number Survived",
                         col = c("lightgray", "lightgreen", "indianred"),
                         legend = c("Cherbourg", "Queenstown", "Southampton"),
                         beside = T)
text(destinationSurvival, table_5 -10, labels = table_5)
graphics.off()

# tree diagram
library(rpart)
tree_model <- rpart(Survived~ Sex + Pclass + Age + familySize, data = train_data, method = 'class' )
install.packages("rpart.plot")
library("rpart.plot")
pdf("titanic_tree_diagram.pdf,", width = 11, height = 8)
rpart.plot(tree_model)
dev.off()
