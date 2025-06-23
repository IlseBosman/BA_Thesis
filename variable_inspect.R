##Loading packages
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmice)
library(naniar)
library(lavaan)
library(tinytex)
library(tidyverse)
library(psych)
library(aod)
library(gridExtra)

#Checking descriptive statistics
summary(youth15combined) 
describe(youth15combined)

#Checking frequency distributions within relevant categorical variables from the youth datafile
#Socio-demographic variables
youth15combined %>% count(sex)
youth15combined %>% count(ethnicity)
youth15combined %>% count(household_size)

#parent-child variables
youth15combined %>% count(feeling_supported_by_family)
youth15combined %>% count(quarrel_with_mother)
youth15combined %>% count(important_talks_with_mother)
youth15combined %>% count(happiness_about_family)
youth15combined %>% count(parents_interest_in_school)

#Psychosocial variables
youth15combined %>% count(future_plans)
youth15combined %>% count(importance_school_exams)
youth15combined %>% count(frequency_physical_bullying)
youth15combined %>% count(frequency_bullying_otherways)
youth15combined %>% count(misbehaviour_school)

ggplot(youth15combined, aes(x = ethnicity)) + 
  geom_histogram(bins= 15, fill = "gray", color = "black") + 
  labs(title = "Histogram of ethnicity", x = "Category", y = "Frequency (N)") +
  theme_minimal()

plotfutpla <- ggplot(youth15combined, aes(x = future_plans)) + 
  geom_histogram(bins= 6, fill = "gray", color = "black") + 
  labs(title = "Histogram of future plans", x = "Category", y = "Frequency (N)") +
  theme_minimal()

plotimpscho <- ggplot(youth15combined, aes(x = importance_school_exams)) + 
  geom_histogram(bins= 6, fill = "gray", color = "black") + 
  labs(title = "Histogram of importance school exams", x = "Category", y = "Frequency (N)") +
  theme_minimal()

grid.arrange(plotfutpla, plotimpscho, ncol = 2)

plotphybul <- ggplot(youth15combined, aes(x = frequency_physical_bullying)) + 
  geom_histogram(bins= 6, fill = "gray", color = "black") + 
  labs(title = "Histogram of frequency physical bullying", x = "Category", y = "Frequency (N)") +
  theme_minimal()

plotothbul <- ggplot(youth15combined, aes(x = frequency_bullying_otherways)) + 
  geom_histogram(bins= 6, fill = "gray", color = "black") + 
  labs(title = "Histogram of frequency bullying in other ways", x = "Category", y = "Frequency (N)") +
  theme_minimal()

grid.arrange(plotphybul, plotothbul, ncol = 2)

#Creating dataset for analysis
youthfinalvar <- youth15combined[ ,c("sex", "householdnumber", "age","ethnicity", "feeling_supported_by_family", "quarrel_with_mother", "important_talks_with_mother", "happiness_about_family", "parents_interest_in_school", "importance_school_exams", "misbehaviour_school", "SDQemotional_symptoms", "SDQconduct_problems", "SDQhyperactivity", "SDQpeer_problems", "SDQprosocial","mean_authoritative", "mean_authorarian", "mean_permissive", "household_size", "response")]
describe(youthfinalvar)
summary(youthfinalvar)

#Respondents who do not have a mother get NA on that variable
youthfinalvar <- youthfinalvar %>%
  mutate(quarrel_with_mother = ifelse(quarrel_with_mother == 5, NA, quarrel_with_mother)) %>%
  mutate(important_talks_with_mother = ifelse(important_talks_with_mother == 5, NA, important_talks_with_mother))

youthfinalvar[youthfinalvar == -9] <- NA
youthfinalvar[youthfinalvar == -8] <- NA
youthfinalvar[youthfinalvar == -1] <- NA

#Making initial frequency distributions
youthfinalvar %>% count(sex)
youthfinalvar %>% count(ethnicity)
youthfinalvar %>% count(household_size)

#parent-child variables
youthfinalvar %>% count(feeling_supported_by_family)
youthfinalvar %>% count(quarrel_with_mother)
youthfinalvar %>% count(important_talks_with_mother)
youthfinalvar %>% count(happiness_about_family)
youthfinalvar %>% count(parents_interest_in_school)

#Psychosocial variables
youthfinalvar %>% count(importance_school_exams)
youthfinalvar %>% count(misbehaviour_school)

summary(youthfinalvar)
describe(youthfinalvar)

#inspecting in variables with missing
percentages <- youthfinalvar |> 
  select(sex, feeling_supported_by_family, quarrel_with_mother, important_talks_with_mother, misbehaviour_school, SDQemotional_symptoms, SDQconduct_problems, SDQhyperactivity, SDQpeer_problems, SDQprosocial, mean_authoritative, mean_authorarian, mean_permissive, household_size)

pm <- colMeans(is.na(percentages))
pm[]

#Further inspecting missingness
md.pattern(percentages, rotate.names = TRUE)
vis_miss(percentages)

#Listwise deletion for all variables (except parenting style)
youthfinalvar <- youthfinalvar[ ,c("sex", "householdnumber", "feeling_supported_by_family", "quarrel_with_mother", "important_talks_with_mother", "parents_interest_in_school", "misbehaviour_school", "SDQemotional_symptoms", "SDQconduct_problems", "SDQhyperactivity", "SDQpeer_problems", "SDQprosocial","mean_authoritative", "mean_authorarian", "mean_permissive", "household_size", "response")]

youthfinalvar[youthfinalvar == -9] <- NA
youthfinalvar[youthfinalvar == -8] <- NA
youthfinalvar[youthfinalvar == -1] <- NA

youthfinalvar <- youthfinalvar %>%
  drop_na(feeling_supported_by_family, quarrel_with_mother, important_talks_with_mother, misbehaviour_school)

#Making initial frequency distributions
youthfinalvar %>% count(sex)
youthfinalvar %>% count(household_size)

#parent-child variables
youthfinalvar %>% count(feeling_supported_by_family)
youthfinalvar %>% count(quarrel_with_mother)
youthfinalvar %>% count(important_talks_with_mother)
youthfinalvar %>% count(misbehaviour_school)

# Count occurrences of each household_id
household_counts <- youthfinalvar %>%
  group_by(householdnumber) %>%
  summarise(count = n())

print(household_counts)

# Find households with more than one person
non_unique_households <- household_counts %>%
  filter(count > 1)

# Boxplot to check outliers
ggplot(youthfinalvar, aes(y = household_size)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 19, outlier.size = 2) +
  labs(title = "Household size", y = "Value") + theme_classic()

ggplot(youthfinalvar, aes(y = SDQemotional_symptoms)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 19, outlier.size = 2) +
  labs(title = "SDQ emotional symptoms", y = "Value") + theme_classic()

ggplot(youthfinalvar, aes(y = SDQconduct_problems)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 19, outlier.size = 2) +
  labs(title = "SDQ conduct problems", y = "Value") + theme_classic()

ggplot(youthfinalvar, aes(y = SDQhyperactivity)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 19, outlier.size = 2) +
  labs(title = "SDQ hyperactivity", y = "Value") + theme_classic()

ggplot(youthfinalvar, aes(y = SDQpeer_problems)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 19, outlier.size = 2) +
  labs(title = "SDQ peer problems", y = "Value") + theme_classic()

ggplot(youthfinalvar, aes(y = SDQprosocial)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 19, outlier.size = 2) +
  labs(title = "SDQ prosocial", y = "Value") + theme_classic()

describe(youthfinalvar)
summary(youthfinalvar)

#Re-ordering variables as distributions are very skewed + making original frequency distributions

#Plot and transform misbehaviour in school
plot_misscho <- data.frame(category = c("In most or all classes", "More than half of the classes", "Half of the classes", "Now and then", "No problem at all"),
                           frequency = c(4, 25, 30, 252, 355))

plot_misscho$category <- factor(plot_misscho$category,
                                levels = c("In most or all classes", "More than half of the classes", "Half of the classes", "Now and then", "No problem at all"))
plotmisscho1 <- ggplot(plot_misscho, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency") +
  ggtitle("Misbehaviour in school - Pre") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 10, vjust=1, size=10))

youthfinalvar <- youthfinalvar %>%
  mutate(misbehaviour_school_1 = factor(case_when(misbehaviour_school == 5 ~ "No problem at all",
                                                  misbehaviour_school == 4 ~ "Now and then", 
                                                  misbehaviour_school %in% c(1, 2, 3) ~ "At least half of the classes")))

youthfinalvar %>% count(misbehaviour_school_1)

plot2_misscho <- data.frame(category = c("At least half of the classes", "No problem at all", "Now and then"),
                            frequency = c(59, 355, 253))

plot2_misscho$category <- factor(plot2_misscho$category,
                                 levels = c("At least half of the classes", "No problem at all", "Now and then"))
plotmisscho2 <- ggplot(plot2_misscho, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency") +
  ggtitle("Misbehaviour in school - Post") +
  theme_minimal() + theme(axis.text.x = element_text(size=12, vjust = 1))

grid.arrange(plotmisscho1, plotmisscho2, ncol = 2)

#Plot important talks with mother
plot_imptalk <- data.frame(category = c("Most days", "More than once a week", "Less than once a week", "Hardly ever"),
                           frequency = c(219, 192, 135, 121))

plot_imptalk$category <- factor(plot_imptalk$category,
                                levels = c("Most days", "More than once a week", "Less than once a week", "Hardly ever"))
plotimptalk1 <- ggplot(plot_imptalk, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Important talks with mother", y = "Frequency") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 25, vjust=1, hjust=1))

grid.arrange(plotimptalk1)

#Plot and transform quarrel with mother
plot_quar <- data.frame(category = c("Most days", "More than once a week", "Less than once a week", "Hardly ever"),
                        frequency = c(34, 90, 197, 346))

plot_quar$category <- factor(plot_quar$category,
                             levels = c("Most days", "More than once a week", "Less than once a week", "Hardly ever"))

plotquar1 <- ggplot(plot_quar, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency (N)") + 
  ggtitle("Quarrel with mother - Pre") +
  theme_minimal() + theme(axis.text.x = element_text(size=10))

youthfinalvar <- youthfinalvar %>%
  mutate(quarrel_with_mother_1 = factor(case_when(quarrel_with_mother %in% c(1, 2) ~ "More than once a week",
                                                  quarrel_with_mother %in% c(3, 4) ~ "Less than once  a week")))

youthfinalvar %>% count(quarrel_with_mother_1)

plot2_quar <- data.frame(category = c("Less than once a week", "More than once a week"),
                         frequency = c(543, 124))

plot2_quar$category <- factor(plot2_quar$category,
                              levels = c("Less than once a week", "More than once a week"))

plotquar2 <- ggplot(plot2_quar, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency (N)") +
  ggtitle("Quarrel with mother - Post") + 
  theme_minimal() + theme(axis.text.x = element_text(size=12))

grid.arrange(plotquar1, plotquar2, ncol = 2)


#Plot and transform feeling supported by family 
plot_famsup <- data.frame(category = c("In most or all of the things I do", "In some of the things I do", "In none of the things I do"),
                          frequency = c(501, 154, 12))

plot_famsup$category <- factor(plot_famsup$category,
                               levels = c("In most or all of the things I do", "In some of the things I do", "In none of the things I do"))
plotfamsup1 <- ggplot(plot_famsup, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency (N)") +
  ggtitle("Feeling supported by family - Pre") +
  theme_minimal() + theme(axis.text.x = element_text(size=11))

youthfinalvar <- youthfinalvar %>%
  mutate(feeling_supported_by_family_1 = factor(case_when(feeling_supported_by_family == 1 ~ "In most or all the things I do",
                                                          feeling_supported_by_family %in% c(2, 3) ~ "In some to none of the things I do")))

youthfinalvar %>% count(feeling_supported_by_family_1)

plot2_famsup <- data.frame(category = c("Most or all of the things I do", "Some to none of the things I do"),
                           frequency = c(501, 166))

plot2_famsup$category <- factor(plot2_famsup$category,
                                levels = c("Most or all of the things I do", "Some to none of the things I do"))
plotfamsup2 <- ggplot(plot2_famsup, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency (N)") +
  ggtitle("Feeling supported by family - Post") +
  theme_minimal() + theme(axis.text.x = element_text(size=12, vjust = 1))

grid.arrange(plotfamsup1, plotfamsup2, ncol = 2)

#Transform and plot sex
youthfinalvar <- youthfinalvar %>%
  mutate(sex_1 = ifelse(sex == 1, 
                        "Male", 
                        "Female")) %>%
  mutate(sex_1 = fct_relevel(sex_1, "Male", "Female"))

plot_sex <- data.frame(category = c("Male", "Female"),
                       frequency = c(291, 376))

plot_sex$category <- factor(plot_sex$category,
                            levels = c("Male", "Female"))

plotsex <- ggplot(plot_sex, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency (N)") +
  theme_minimal() + theme(axis.text.x = element_text(size=12)) +
  ggtitle("Sex")

grid.arrange(plotsex)

#Plot response
youthfinalvar %>% count(response)
plot_resp <- data.frame(category = c("No", "Yes"),
                        frequency = c(134, 533))

plot_resp$category <- factor(plot_resp$category,
                             levels = c("No", "Yes"))

plotresp <- ggplot(plot_resp, aes(x = category, y = frequency)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  labs(x = "Categories", y = "Frequency (N)") +
  theme_minimal() + theme(axis.text.x = element_text(size=12)) +
  ggtitle("Succesful response to first invite to the adult questionnaire?")

grid.arrange(plotresp)
