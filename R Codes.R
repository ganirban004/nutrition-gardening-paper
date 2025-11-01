library(readxl)
library(tidyverse)
library(pscl)
other_data <- read_excel("I:/New folder/Statistics/ISI/Rooftop Gardening/Data.xlsx", 
                         sheet = "Other Data")
logistic_data <- read_excel("I:/New folder/Statistics/ISI/Rooftop Gardening/Data.xlsx", 
                            sheet = "Logistic Data")

## Descriptive Statistics
logistic_data %>%
  select(all_of(c("Age", "Gender", "Education", "Occupation"))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  mutate(Percentage = round(100 * Count / sum(Count), 2)) %>%
  arrange(Variable, desc(Percentage))


## Awareness
# Level
other_data %>% filter(!is.na(Awareness)) %>% 
  mutate(Awareness = factor(Awareness, levels = c("Yes", "Maybe", "No"))) %>%
  ggplot(aes(x = Awareness)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5) + coord_cartesian(ylim = c(0, 60)) + 
  labs(x = "Awareness Level", y = "Percentage") + theme_minimal() + 
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# Source
other_data %>% filter(!is.na(Source)) %>% 
  ggplot(aes(x = Source)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5, size = 3) + coord_cartesian(ylim = c(0, 50)) + 
  labs(x = "Source of Awareness", y = "Percentage") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.7, vjust = 0.9), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


## Invovlvment
#Level
other_data %>% filter(!is.na(Involvement)) %>% 
  mutate(Involvement = factor(Involvement, levels = c("School Gardening", "Rooftop Gardening", 
                                                      "Kitchen Gardening", "None"))) %>%
  ggplot(aes(x = Involvement)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5, size = 3) + coord_cartesian(ylim = c(0, 60)) + 
  labs(x = "Type of Gardening Involvement", y = "Percentage") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.7, vjust = 0.9), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#Nutrition
other_data %>% filter(!is.na(Is.Nutrition)) %>% 
  mutate(Is.Nutrition = factor(Is.Nutrition, levels = c("Yes", "Maybe", "No"))) %>%
  ggplot(aes(x = Is.Nutrition)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5) + coord_cartesian(ylim = c(0, 80)) + 
  labs(x = "Engagement in Nutrition Gardening", y = "Percentage") + theme_minimal() + 
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


## Willingness
logistic_data %>% filter(!is.na(Willingness)) %>% 
  mutate(Willingness = factor(Willingness, levels = c("Strongly yes", "Somewhat yes", "Not at all"))) %>%
  ggplot(aes(x = Willingness)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5) + coord_cartesian(ylim = c(0, 50)) + 
  labs(x = "Level of Willingness to Establish a Nutrition Garden", y = "Percentage") + theme_minimal() + 
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


## Problems
#Barriers
other_data %>% filter(!is.na(Barriers)) %>% 
  mutate(Barriers = factor(Barriers, levels = c("Constraints", "Enthusiasm", "Soil", 
                                                "Health", "Time", "Others"))) %>%
  ggplot(aes(x = Barriers)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5, size = 3) + coord_cartesian(ylim = c(0, 50)) + 
  labs(x = "Barriers to Do Nutrition Gardening", y = "Percentage") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.7, vjust = 0.9),
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#Support
other_data %>% filter(!is.na(Support)) %>% 
  mutate(Support = factor(Support, levels = c("Awareness", "Subsidization", "Equipmental", 
                                              "Govt. Incentive", "Financial", "None"))) %>%
  ggplot(aes(x = Support)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5, size = 3) + coord_cartesian(ylim = c(0, 40)) + 
  labs(x = "Types of Support Needed", y = "Percentage") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 20, hjust = 0.7, vjust = 0.9),
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


## Benefits
#Daily Nutrition
other_data %>% filter(!is.na(Is.DailyNutrition)) %>% 
  mutate(Is.DailyNutrition = factor(Is.DailyNutrition, levels = c("Yes", "No"))) %>%
  ggplot(aes(x = Is.DailyNutrition)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5) + coord_cartesian(ylim = c(0, 100)) + 
  labs(x = "Contribution to Daily Nutrition", y = "Percentage") + theme_minimal() + 
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#Climate & Pollution
other_data %>% filter(!is.na(Climate.Pollution)) %>% 
  mutate(Climate.Pollution = factor(Climate.Pollution, levels = c("Yes", "Maybe", "No"))) %>%
  ggplot(aes(x = Climate.Pollution)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5) + coord_cartesian(ylim = c(0, 70)) + 
  labs(x = "Climate & Air Pollution Mitigation", y = "Percentage") + theme_minimal() + 
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#Health
other_data %>% filter(!is.na(Impact.Health)) %>% 
  mutate(Impact.Health = factor(Impact.Health, levels = c("Significantly yes", "Somewhat yes", 
                                                          "No noticeable impact"))) %>%
  ggplot(aes(x = Impact.Health)) +
  geom_bar(aes(y = (after_stat(count) / sum(after_stat(count))) * 100), 
           fill = "skyblue", color = "black", width = 0.5) +
  geom_text(aes(y = (after_stat(count) / sum(after_stat(count))) * 100, 
                label = paste0(round((after_stat(count) / sum(after_stat(count))) * 100, 2), "%")), 
            stat = "count", vjust = -0.5) + coord_cartesian(ylim = c(0, 70)) + 
  labs(x = "Impact on Health", y = "Percentage") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 10, hjust = 0.7, vjust = 0.9), 
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


## Logistic Regression
model_data <- logistic_data %>%
  mutate(Age = factor(Age, levels = c("Under 18", "18-45", "45-65", "65 and above")), 
         Gender = factor(Gender, levels = c("Male", "Female")), 
         Education = factor(ifelse(Education %in% c("Less than high school", 
                                                    "High school diploma or equivalent"), 0, 1)), 
         Occupation = factor(ifelse(Occupation == "Student", 1, 0)), 
         Awareness = factor(ifelse(Awareness == "Yes", 1, 0)), 
         Support = factor(ifelse(Support == "None", 0, 1)),
         Willingness = factor(ifelse(Willingness == "Not at all", 0, 1)))
log_model <- glm(Willingness ~ ., data = model_data, family = "binomial")
summary(log_model)


model_summary <- summary(log_model)$coefficients
results_table <- data.frame(
  Variable = rownames(model_summary),
  Coefficient = round(model_summary[, "Estimate"], 4),
  `Std. Error` = round(model_summary[, "Std. Error"], 4),
  `p-value` = round(model_summary[, "Pr(>|z|)"], 4),
  `Odds Ratio` = round(exp(model_summary[, "Estimate"]), 4)
)
results_table

pR2(log_model)