library(ggplot2)
library(dplyr)

#Load dataset
salary <- read.csv('Salary.csv')
head(salary)
summary(salary)
str(salary)

#Data cleaning and preprossesing

#Handle missing values
sum(is.na(salary))
colSums(is.na(salary))

#Remove missing values
salary <- na.omit(salary)
sum(is.na(salary))
colSums(is.na(salary))

#Convert data types
salary$Education.Level <- as.factor(salary$Education.Level)
salary$Years.of.Experience <- as.numeric(salary$Years.of.Experience)
salary$Salary <- as.numeric(salary$Salary)

#Remove duplicates
salary <- salary[!duplicated(salary),]

#Rename column names
colnames(salary) <- c("education","experience","salary")

#Filter outliers and remove outliers
boxplot(salary$salary,main = "Boxplot for salary")
q1 <- quantile(salary$salary, 0.25)
q3 <- quantile(salary$salary, 0.75)
IQR <- q3-q1
salary<- salary[salary$salary >= (q1- 1.5*IQR) & salary$salary <= (q3+1.5*IQR),]

boxplot(salary$salary,main="Boxplot for salary")

boxplot(salary$experience, main =" Boxplot for experience") 

boxplot(salary$education, main = "Boxplot for education")


#Exploratory  Data Analysis(EDA)

pl <- ggplot(data = salary, aes(x=experience, y=salary,colour = education)) 
+ geom_point(size=3) 
+ geom_smooth(method = "lm",se = FALSE)
+ labs(title = "Salary vs Experience", x="experience",y = "salary") 
+ theme_minimal()
print(pl)

#Build the multiple linear regression model
model <- lm(salary~ experience +education,data = salary)
summary(model)

#Predict salaries 
new_employee <- data.frame(experience=c(5,10,3,7),education = c("Master's","PhD","PhD","Bachelor's"))
new_salary <-predict(model, newdata = new_employee)
print(new_salary)

#Residual Plot(Check model Assumptions)
plot(model$residuals, main = "Residual Plot",ylab = "Residuals",xlab = "Index" , col = "green", pch=19)
abline(h=0,col="red")

#Histogram of Residuals
histogram <- hist(model$residuals, main = "Histogram of Residuals",col = "lightblue",xlab = "residuals",breaks = 20)
print(histogram)

#Line plot(Salary growth Projection)
line_chart <- ggplot(data = salary, aes(x = experience, y = salary, color = education, group = education)) +stat_summary(fun = mean, geom = "line", linewidth = 1.2)  +labs(title = "Salary Growth Projection", x = "Years of Experience", y = "Average Salary") +theme_minimal()
print(line_chart)













