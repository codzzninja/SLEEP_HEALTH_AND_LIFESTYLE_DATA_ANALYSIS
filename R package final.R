library(ggplot2)
library(readr)

file_path <-  "C:/Users/KUBER/Downloads/Sleep_health_and_lifestyle_dataset (3).csv"
data <- read_csv(file_path,show_col_types = F)
#View(data)

# Extract variables
id <- data$`Person ID`
gender <- data$`Gender`
stress <- data$`Stress Level`
bmi <- data$`BMI Category`
steps <- data$`Daily Steps`
sleep_dur <- data$`Sleep Duration`
qos <- data$`Quality of Sleep`
hr <- data$`Heart Rate`
phy_act <- data$`Physical Activity Level`
steps <- data$`Daily Steps`
disorder <- data$`Sleep Disorder`
age <- data$`Age`
prof <- data$`Occupation`

print("**********************************")
cat("\n\t\t SLEEP HEALTH AND LIFESTYLE DATA ANALYSIS \n\n")

print("A sample dataset of a population is taken for analysis")
print(paste("Sample size : ",length(id)))


cat("\n=====================================================================================================\n\n")

print("*Plotting*")
cat("\nPie chart for no. of people in each occupation")
cat("\n\nBar graph indicating BMI of the people")
cat("\n\nBar graph indicating Sleep Disorders of the people")
cat("\n\nMultibar graph indicating the gender along with sleep disorders affecting which age groups")
cat("\n\nBox plot for Heart rate vs Gender")
cat("\n\nBar graph indicating Age distribution")
cat("\n\nMultibar graph indicating distribution of gender along with sleep disorder")


pie(table(prof), main = "People's Occupation", radius = 0.8)

barplot(table(bmi), col='green', main='Health status', xlab='Body mass index(BMI)', ylab='No. of persons')

barplot(table(disorder), col='purple', main='Distribution Sleep disorders', xlab='sleep disorders', ylab='No. of persons')

df <- data.frame(age,gender,disorder)
gen_age_dis <- ggplot(df, aes(gender,age,fill=disorder))+
  geom_bar(position='dodge',stat='identity')+
  labs(title="Most affected ages in each type of Sleep Disorder",
       x="Gender", y="Age")
plot(gen_age_dis)

boxplot(hr ~ gender, data=data, main="Heart Rate Vs Gender", xlab="Gender", ylab="Heart Rate",col="red")

hist(age, main="Age Distribution", xlab="Age", col="lightblue")

df_gender_disorder <- table(gender, disorder)
barplot(df_gender_disorder, beside=TRUE, col=c("magenta", "blue"), main="Sleep Disorders and Gender distribution", xlab="Sleep disorder", ylab="Count", legend=rownames(df_gender_disorder))

cat("\n=====================================================================================================\n\n")

print("*Concept of Confidence Interval*")

#sleep duration ci
xbar <- mean(sleep_dur)
sd <- sd(sleep_dur)
n <- length(sleep_dur)
alpha <- 0.05
z <- qnorm(alpha/2, lower.tail = FALSE)
lb <- xbar - ((sd * z) / sqrt(n))
ub <- xbar + ((sd * z) / sqrt(n))
cat("\n\n")

cat("Level of significance     : " , alpha,"\n")
cat("z_alpha                   : " , z, "\n")
cat("sample avg sleep duration : ", mean(sleep_dur),"\n")
cat("sample avg stress level   : ", mean(stress),"\n")

cat("\n95% Confidence interval that: \n")

cat("\nThe Mean Sleep Duration of the population is ",xbar,": \n")
cat(c(lb, ub))

#stress ci
xbar1 <- mean(stress)
sd1 <- sd(stress)
n1 <- length(stress)
alpha <- 0.05
z1 <- qnorm(alpha/2, lower.tail = FALSE)
lb1 <- xbar1 - ((sd1 * z1) / sqrt(n1))
ub1 <- xbar1 + ((sd1 * z1) / sqrt(n1))
cat("\n\n")

cat("\nThe mean Stress Level is ",xbar1,": \n")
cat(c(lb1, ub1))
cat("\n\n")


cat("\n=====================================================================================================\n\n")


print("*Concept of Hypothesis Testing for single mean*")
cat("\n")
print("In general, it is claimed that the 'average sleep duration for a person is 7.5 hrs' ")
cat("\n")
print("Null Hypothesis (H0)       : The mean sleep duration is equal 7.5 hrs")
print("Alternative Hypothesis (H1): The mean sleep duration is less than 7.5 hrs")

alpha2 <- 0.05
mu <- 7.5
xbar <- mean(sleep_dur)
s <- sd(sleep_dur)

z <- (xbar - mu) / (s/sqrt(length(sleep_dur)))
p_value <- pnorm(z)
cr <- qnorm(alpha2,lower.tail = F)

cat("\n\n")
cat("Level of significance: " ,alpha2,"\n")
cat("Z-Statistic          : " , z, "\n")
cat("P-Value   : " , p_value, "\n")
cat("Critical region      : " , "(-Inf," , -cr , ")","U","(" , cr , ",Inf)" , "\n\n")

cat("From, the data obtained from the sample, \n")

if(p_value < alpha2) {
  cat(" 'Reject H0' \n")
  cat("The mean sleep duration a person in the given population is less than 7.5 hrs.\n")
} else {
  cat("Do not reject H0\n")
  cat("The mean sleep duration of a person int the given population is not less than 7.5 hrs.\n")
}


cat("\n=====================================================================================================\n\n")


print("*Concept of Test for Independence*")
cat("\n")
print("To check if the gender and level of stress are dependent or independent of each other ?")
cat("\n")
print("Null hypothesis(H0)      : Level of stress and gender is independent of each other")
print("Alternate hypothesis(H1) : Level of stress and gender is not independent of each other")
cat("\n")

contingency_table <- table(gender, stress)
contingency_table_with_margins <- addmargins(contingency_table)
contingency_table_with_margins <- addmargins(contingency_table)
print(contingency_table_with_margins)

result <- chisq.test(contingency_table)
print(result)
chi_sq <- result$statistic

cr <- qchisq(0.05, df = (2-1)*(6-1), lower.tail = F)
cat("chi_sq_alpha : ",cr,"\n")
if(chi_sq > cr){
  print("Reject H0.")
  print("Level of stress and gender is not independent of each other")
} else{
  print("Do not reject H0.")
  print("Level of stress and gender is independent of eact other")
}


cat("\n=====================================================================================================\n\n")


print("*Linear Regression*")
cat("\n")

print("Modeling a linear regression for sleep duration and quality of sleep")
print("Sleep duration  : independent variable")
print("Quality of sleep: dependent variable")

model <- lm(qos~sleep_dur)
plot(qos~sleep_dur, main="plot for Regression model of Sleep duration and quality of sleep",xlab="Duration of sleep",ylab="Quality of sleep")
abline(coef(model),col="darkblue")

a <- model$coefficients[1]
b <- model$coefficients[2]
cat("\n")
print("Regression line equation: ")
cat("\t y = ",a,"+",b,"x",'\n\n')

print("With the 'Sleep Duration' of 7.75, 8.5, 9.125 hrs")
print("The Predicted 'Quality of Sleep' values are: ")
new_data <- data.frame(sleep_dur = c(7.75, 8.5, 9.125))
predicted_quality_of_sleep <- predict(model, newdata = new_data)
print(predicted_quality_of_sleep)


cat("\n=====================================================================================================\n\n")


print("*Multiple Regression*")
cat("\n")
print("Modeling a multiple regression for")
cat("\nsleep duration vs stress level\n")
cat("                  amount of physical activity(mins/per)\n")
cat("                  heart rate(bpm)\n\n")

model <- lm(sleep_dur ~ stress+phy_act+hr)
#print(model)
a = model$coefficients[1]
b1 = model$coefficients[2]
b2 = model$coefficients[3]
b3 = model$coefficients[4]

print("Regression line equation: ")
cat("\t y = ",a,"+",b1,"x1","+",b2,"X2","+",b3,"X3","\n")


cat("\n=====================================================================================================\n\n")
