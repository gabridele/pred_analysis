# SYNTACTIC PREDICT
# Install and load required packages
install.packages(c("ordinal", "lmerTest", "pbkrtest", "multcomp", "graphics", "ggplot2", "gmodels"))
library(ordinal)
library(lmerTest)
library(multcomp)
library(graphics)
library(ggplot2)
library(gmodels)


#choose file
results=read.csv(file.choose(),header = TRUE, sep = ";", comment.char = "#")
# create a df with columns you need
results.x = results[,c(1, 3, 5, 6, 10, 12, 17, 19, 20, 22, 23, 25, 28, 29)]

# Type col needs to be a factor
results.x$Type <- factor(results.x$Type)
results.x$List <- factor(results.x$List)
results.x$Accuracy_W1 <- factor(results.x$Accuracy_W1)
results.x$Accuracy_W2 <- factor(results.x$Accuracy_W2)

# Check the levels of variable Type
levels(results.x$Type)
# check if everything is fine
summary(results.x)

results_filteredW1 <- subset(results.x, Accuracy_W1 == 1)
results_filteredW2 <- subset(results.x, Accuracy_W2 == 1)


# -- -- -- -- -- -- -- -- --

RT_start_model <- lmer(RT_Start ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_start_model_contrast_result <- glht(RT_start_model, linfct = mcp(Type = contrast_matrix))
summary(RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

RT_start_model <- lmer(RT_Start ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_start_model_contrast_result <- glht(RT_start_model, linfct = mcp(Type = contrast_matrix))
summary(RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

ln_RT_start_model <- lmer(log(RT_Start) ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_start_model_contrast_result <- glht(ln_RT_start_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

ln_RT_start_model <- lmer(log(RT_Start) ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_start_model_contrast_result <- glht(ln_RT_start_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for RT_W1
RT_W1_model <- lmer(RT_W1 ~ Type + LengthW1 + Type:LengthW1 + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(RT_W1_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_W1_model_contrast_result <- glht(RT_W1_model, linfct = mcp(Type = contrast_matrix))
summary(RT_W1_model_contrast_result)

# warning message tells me that a contrast is not suitable for this kind of interaction Type:Length. So I added this line of 
# code that builds an interaction plot 
interaction.plot(x.factor = results_filteredW1$Type, trace.factor = results_filteredW1$LengthW1, response = results_filteredW1$RT_W1, type = "b")

#another kind of interaction plot
results_filteredW1$predicted <- predict(RT_W1_model)

ggplot(data = results_filteredW1, aes(x = Type, y = RT_W1)) +
  geom_point() +  # Add points
  geom_line(aes(y = predicted), color = "red") +  # Add the predicted values as a line
  facet_grid(~ LengthW1) +
  labs(x = "Type", y = "RT_W1")
  
# -- -- -- -- -- -- -- -- --
  
# lmer for RT_W2
RT_W2_model <- lmer(RT_W2 ~ Type + LengthW2 + Type:LengthW2 + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(RT_W2_model)
  
# contrast test
contrast_matrix <- c(-1, 1)
RT_W2_model_contrast_result <- glht(RT_W2_model, linfct = mcp(Type = contrast_matrix))
summary(RT_W2_model_contrast_result)
  
# warning message tells me that a contrast is not suitable for this kind of interaction Type:Length. So I added this line of 
# code that builds an interaction plot 
interaction.plot(x.factor = results_filteredW2$Type, trace.factor = results_filteredW2$LengthW2, response = results_filteredW2$RT_W2, type = "b")
  
#another kind of interaction plot
results_filteredW2$predicted <- predict(RT_W2_model)
  
ggplot(data = results.x, aes(x = Type, y = RT_W2)) +
  geom_point() +  # Add points
  geom_line(aes(y = predicted), color = "red") +  # Add the predicted values as a line
  facet_grid(~ LengthW2) +
  labs(x = "Type", y = "RT_W2")

# -- -- -- -- -- -- -- -- --

# lmer for ln(RT_W1)
ln_RT_W1_model <- lmer(log(RT_W1) ~ Type + LengthW1 + Type:LengthW1 + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(ln_RT_W1_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_W1_model_contrast_result <- glht(ln_RT_W1_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_W1_model_contrast_result)

# PS: ln() function in R is log(). For log base 10, you specify an argument

#plot
logarithm_word = log(results_filteredW2$RT_W1)
interaction.plot(x.factor = results_filteredW1$Type, trace.factor = results_filteredW1$LengthW1, response = logarithm_word, type = "b")

#another kind of interaction plot
results_filteredW1$predicted_log <- predict(ln_RT_W1_model)

ggplot(data = results_filteredW1, aes(x = Type, y = log(RT_W1))) +
  geom_point() +  # Add points
  geom_line(aes(y = predicted_log), color = "red") +  # Add the predicted values as a line
  facet_grid(~ LengthW1) + 
  labs(x = "Type", y = "ln(RT_W1)")

# -- -- -- -- -- -- -- -- --

# lmer for ln(RT_W2)
ln_RT_W2_model <- lmer(log(RT_W2) ~ Type + LengthW2 + Type:LengthW2 + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(ln_RT_W2_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_W2_model_contrast_result <- glht(ln_RT_W2_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_W2_model_contrast_result)

# PS: ln() function in R is log(). For log base 10, you specify an argument

#plot
logarithm_word = log(results_filteredW2$RT_W2)
interaction.plot(x.factor = results_filteredW2$Type, trace.factor = results_filteredW2$LengthW2, response = logarithm_word, type = "b")

#another kind of interaction plot
results.x$predicted_log <- predict(ln_RT_W2_model)

ggplot(data = results.x, aes(x = Type, y = log(RT_W2))) +
  geom_point() +  # Add points
  geom_line(aes(y = predicted_log), color = "red") +  # Add the predicted values as a line
  facet_grid(~ LengthW2) + 
  labs(x = "Type", y = "ln(RT_W2)")

# -- -- -- -- -- -- -- -- --

RT_end_model <- lmer(RT_End ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_end_model_contrast_result <- glht(RT_end_model, linfct = mcp(Type = contrast_matrix))
summary(RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

RT_end_model <- lmer(RT_End ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_end_model_contrast_result <- glht(RT_end_model, linfct = mcp(Type = contrast_matrix))
summary(RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

ln_RT_end_model <- lmer(log(RT_End) ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(ln_RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_end_model_contrast_result <- glht(ln_RT_end_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

ln_RT_end_model <- lmer(log(RT_End) ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(ln_RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_end_model_contrast_result <- glht(ln_RT_end_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# just to check variable types
columntype <- sapply(results.x, class) 
print(columntype)

# -- -- -- -- -- -- -- -- --

# lmer for RT_likert
RT_likert_model <- lmer(RT_Likert ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(RT_likert_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_likert_model_contrast_result <- glht(RT_likert_model, linfct = mcp(Type = contrast_matrix))
summary(RT_likert_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for RT_likert
RT_likert_model <- lmer(RT_Likert ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(RT_likert_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_likert_model_contrast_result <- glht(RT_likert_model, linfct = mcp(Type = contrast_matrix))
summary(RT_likert_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for ln(RT_Likert)
ln_RT_likert_model <- lmer(log(RT_Likert) ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(ln_RT_likert_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_likert_model_contrast_result <- glht(ln_RT_likert_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_likert_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for ln(RT_Likert)
ln_RT_likert_model <- lmer(log(RT_Likert) ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(ln_RT_likert_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_likert_model_contrast_result <- glht(ln_RT_likert_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_likert_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# LIST DIFFERENCES

# lmer for list_RT_start
list_RT_start_model <- lmer(RT_Start ~ List + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(list_RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
list_RT_start_model_contrast_result <- glht(list_RT_start_model, linfct = mcp(List = contrast_matrix))
summary(list_RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for list_RT_start
list_RT_start_model <- lmer(RT_Start ~ List + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(list_RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
list_RT_start_model_contrast_result <- glht(list_RT_start_model, linfct = mcp(List = contrast_matrix))
summary(list_RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for list_RT_W1
list_RT_W1_model <- lmer(RT_W1 ~ List + LengthW1 + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(list_RT_W1_model)

# contrast test
contrast_matrix <- c(-1, 1)
list_RT_W1_model_contrast_result <- glht(list_RT_W1_model, linfct = mcp(List = contrast_matrix))
summary(list_RT_W1_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for list_RT_W2
list_RT_W2_model <- lmer(RT_W2 ~ List + LengthW2 + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(list_RT_W2_model)

# contrast test
contrast_matrix <- c(-1, 1)
list_RT_W2_model_contrast_result <- glht(list_RT_W2_model, linfct = mcp(List = contrast_matrix))
summary(list_RT_W2_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for list_RT_end
list_RT_end_model <- lmer(RT_End ~ List + (1 | Item) + (1 | Participant), data = results_filteredW1)
summary(list_RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
list_RT_end_model_contrast_result <- glht(list_RT_end_model, linfct = mcp(List = contrast_matrix))
summary(list_RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for list_RT_end
list_RT_end_model <- lmer(RT_End ~ List + (1 | Item) + (1 | Participant), data = results_filteredW2)
summary(list_RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
list_RT_end_model_contrast_result <- glht(list_RT_end_model, linfct = mcp(List = contrast_matrix))
summary(list_RT_end_model_contrast_result)


# -- -- -- -- -- -- -- -- --

# now Likert_raw needs to be a factor
results.x$Likert_raw <- factor(results.x$Likert_raw, levels = 1:7)
# ordinal regression for Likert_raw
ordinal_model <- clmm(Likert_raw ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW1, link = "probit")
summary(ordinal_model)

# Likert_raw - list difference
list_ordinal_model <- clmm(Likert_raw ~ List + (1 | Item) + (1 | Participant), data = results_filteredW1, link = "probit")
summary(list_ordinal_model)

# -- -- -- -- -- -- -- -- --

# now Likert_raw needs to be a factor
results.x$Likert_raw <- factor(results.x$Likert_raw, levels = 1:7)
# ordinal regression for Likert_raw
ordinal_model <- clmm(Likert_raw ~ Type + (1 | Item) + (1 | Participant), data = results_filteredW2, link = "probit")
summary(ordinal_model)

# Likert_raw - list difference
list_ordinal_model <- clmm(Likert_raw ~ List + (1 | Item) + (1 | Participant), data = results_filteredW2, link = "probit")
summary(list_ordinal_model)

# -- -- -- -- -- -- -- -- --

# Type - AccuracyW1 contingency table

CrossTable(results.x$Type, results.x$Accuracy_W1, prop.chisq = TRUE)

# chi test W1
contingency_table <- table(results.x$Type, results.x$Accuracy_W1)
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)


# -- -- -- -- -- -- -- -- --

# Type - AccuracyW2 contingency table

CrossTable(results.x$Type, results.x$Accuracy_W2, prop.chisq = TRUE)

# chi test W2
contingency_table <- table(results.x$Type, results.x$Accuracy_W2)
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

