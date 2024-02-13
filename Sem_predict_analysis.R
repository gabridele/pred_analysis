# SEMANTIC PREDICT
# Install and load required packages
install.packages(c("ordinal", "lmerTest", "pbkrtest", "multcomp", "graphics", "ggplot2"))
library(ordinal)
library(lmerTest)
library(multcomp)
library(graphics)
library(ggplot2)

#choose file
results=read.csv(file.choose(),header = TRUE, sep = ";", comment.char = "#")
# create a df with columns you need
results.x = results[,c(1, 2, 3, 4, 5, 9, 15, 17, 19, 21, 23)]
# check if everything is fine
summary(results.x)
# Type col needs to be a factor
results.x$Type <- factor(results.x$Type)
# Check the levels of variable Type
levels(results.x$Type)

# -- -- -- -- -- -- -- -- --

RT_start_model <- lmer(RT_Start ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_start_model_contrast_result <- glht(RT_start_model, linfct = mcp(Type = contrast_matrix))
summary(RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

ln_RT_start_model <- lmer(log(RT_Start) ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(RT_start_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_start_model_contrast_result <- glht(ln_RT_start_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_start_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for RT_word
RT_word_model <- lmer(RT_Word ~ Type + Length + Type:Length + (1 | Item) + (1 | Participant), data = results.x)
summary(RT_word_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_word_model_contrast_result <- glht(RT_word_model, linfct = mcp(Type = contrast_matrix))
summary(RT_word_model_contrast_result)

# warning message tells me that a contrast is not suitable for this kind of interaction Type:Length. So I added this line of 
# code that builds an interaction plot 
interaction.plot(x.factor = results.x$Type, trace.factor = results.x$Length, response = results.x$RT_Word, type = "b")

#another kind of interaction plot
results.x$predicted <- predict(RT_word_model)

ggplot(data = results.x, aes(x = Type, y = RT_Word)) +
  geom_point() +  # Add points
  geom_line(aes(y = predicted), color = "red") +  # Add the predicted values as a line
  facet_grid(~ Length)
  labs(x = "Type", y = "RT_Word")

# -- -- -- -- -- -- -- -- --

#lmer for ln(RT_Word)
ln_RT_word_model <- lmer(log(RT_Word) ~ Type + Length + Type:Length + (1 | Item) + (1 | Participant), data = results.x)
summary(ln_RT_word_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_word_model_contrast_result <- glht(ln_RT_word_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_word_model_contrast_result)

# PS: ln() function in R is log(). For log base 10, you specify an argument

#plot
logarithm_word = log(results.x$RT_Word)
interaction.plot(x.factor = results.x$Type, trace.factor = results.x$Length, response = logarithm_word, type = "b")

# another kind of interaction plot
results.x$predicted_log <- predict(ln_RT_word_model)

ggplot(data = results.x, aes(x = Type, y = log(RT_Word))) +  # Added closing parenthesis here
  geom_point() +  # Add points
  geom_line(aes(y = predicted_log), color = "red") +  # Add the predicted values as a line
  facet_grid(~ Length) +
  labs(x = "Type", y = "ln_RT_Word")

# -- -- -- -- -- -- -- -- --

RT_end_model <- lmer(RT_End ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_end_model_contrast_result <- glht(RT_end_model, linfct = mcp(Type = contrast_matrix))
summary(RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

ln_RT_end_model <- lmer(log(RT_End) ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(ln_RT_end_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_end_model_contrast_result <- glht(ln_RT_end_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_end_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# To compute z-score, Likert_raw needs to be a number
results.x$Likert_raw <- as.numeric(results.x$Likert_raw)
# lmer for Likert_z_score
Likert_z_score_model <- lmer(scale(Likert_raw) ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(Likert_z_score_model)

# contrast test
contrast_matrix <- c(-1, 1)
Likert_z_score_model_contrast_result <- glht(Likert_z_score_model, linfct = mcp(Type = contrast_matrix))
summary(Likert_z_score_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# just to check variable types
columntype <- sapply(results.x, class) 
print(columntype)

# -- -- -- -- -- -- -- -- --

# lmer for RT_likert
RT_likert_model <- lmer(RT_Likert ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(RT_likert_model)

# contrast test
contrast_matrix <- c(-1, 1)
RT_likert_model_contrast_result <- glht(RT_likert_model, linfct = mcp(Type = contrast_matrix))
summary(RT_likert_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# lmer for ln(RT_Likert)
ln_RT_likert_model <- lmer(log(RT_Likert) ~ Type + (1 | Item) + (1 | Participant), data = results.x)
summary(ln_RT_likert_model)

# contrast test
contrast_matrix <- c(-1, 1)
ln_RT_likert_model_contrast_result <- glht(ln_RT_likert_model, linfct = mcp(Type = contrast_matrix))
summary(ln_RT_likert_model_contrast_result)

# -- -- -- -- -- -- -- -- --

# now Likert_raw needs to be a factor
results.x$Likert_raw <- factor(results.x$Likert_raw, levels = 1:7)
# ordinal regression for Likert_raw
ordinal_model <- clmm(Likert_raw ~ Type + (1 | Item) + (1 | Participant), data = results.x, link = "probit")
summary(ordinal_model)

