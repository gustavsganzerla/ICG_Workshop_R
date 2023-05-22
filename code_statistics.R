####REQUIREMENTS

.libPaths("/project/def-sponsor00/R/lib")
library(rio)
library(ggpubr)
library(pROC)
library(ggplot2)


######1 - Basic statistical tests
View(ToothGrowth)
head(ToothGrowth)
tail(ToothGrowth)
colnames(ToothGrowth)
class(ToothGrowth$len)
class(ToothGrowth$supp)
class(ToothGrowth$dose)
mean(ToothGrowth$len)
sd(ToothGrowth$len)
min(ToothGrowth$len)
max(ToothGrowth$len)

table(ToothGrowth$supp)
table(ToothGrowth$dose,ToothGrowth$supp)

aggregate(ToothGrowth$len~ToothGrowth$supp, FUN = 'mean')
aggregate(ToothGrowth$len~ToothGrowth$supp+ToothGrowth$dose, FUN = 'mean')

ggplot(data = ToothGrowth, aes(x = supp, y = len))+
  geom_boxplot()
ggplot(data = ToothGrowth, aes(x = supp, y = len, color = supp))+
  geom_boxplot()+
  theme_pubr()
ggplot(data = ToothGrowth, aes(x = supp, y = len, color = supp))+
  geom_boxplot()+
  theme_pubr()+
  facet_grid(.~dose)

shapiro.test(ToothGrowth$len)

ggplot(data = ToothGrowth, aes(x = supp, y = len, color = supp))+
  geom_boxplot()+
  theme_pubr()+
  facet_grid(.~dose)+
  stat_compare_means(method = "t.test")

######2 - Classification
data_covid <- import("https://raw.githubusercontent.com/gustavsganzerla/ICG_Workshop_R/main/data_covid.csv")

shapiro.test(data_covid$neutro)

class(data_covid$MOF)
data_covid$MOF <- as.factor(data_covid$MOF)

ggplot(data = data_covid, aes(x = Diagnosis, y = neutro))+
  geom_boxplot()+
  facet_grid(.~MOF)+
  stat_compare_means(comparisons = list(c("sepsis", "sepsis+covid"),
                                        c("sepsis", "septic shock"),
                                        c("sepsis+covid", "septic shock")),
                     method = "wilcox.test",
                     label = "p.signif")

data_covid_mof_0 <- subset(data_covid, data_covid$MOF == 0)

model <- glm(Diagnosis == "septic shock" ~ neutro, data = data_covid_mof_0)

roc_obj <- roc(data_covid_mof_0$Diagnosis=="septic shock", predict(model))

roc_df <- data.frame(TPR = roc_obj$sensitivities, FPR = roc_obj$specificities)

roc_obj$auc

ggplot(data = roc_df, aes(x = 1-FPR, y = TPR))+
  geom_line()+
  annotate("text", x = 0.8, y = 0.2, label = paste("AUC = ", round(roc_obj$auc, 2)))+
  theme_pubr()

######3 - Regression
data_diabetes <- import("https://raw.githubusercontent.com/gustavsganzerla/ICG_Workshop_R/main/diabetes.csv")


# Fit the linear regression model
model <- lm(BMI ~ Glucose, data = data_diabetes)

# Print the model summary
summary(model)


#
intercept <- coef(model)[1]
slope <- coef(model)[2]

equation <- paste("BMI =", round(intercept, 2), "+", round(slope, 2), "* Glucose")
print(equation)


data_diabetes$predicted_BMI <- predict(model, newdata = data_diabetes)

# Calculate residuals (difference between actual and predicted BMI)
residuals <- data_diabetes$BMI - data_diabetes$predicted_BMI
rmse <- sqrt(mean(residuals^2))

#





