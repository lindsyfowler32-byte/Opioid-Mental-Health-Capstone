library(tidyverse)
library(polycor) # Excellent for correlations between categorical variables

# 1. Filter and Recode
analysis_df <- ds %>%
  select(
    # Opioid Misuse (Pain Relievers)
    opioid_misuse = PNRNMREC, 
    # Health Indicators
    gen_health = HEALTH2,
    cog_difficulty = LVLDIFMEM2,
    # Demographic Controls
    age = CATAG6,
    poverty = POVERTY3
  ) %>%
  mutate(
    # Recode Opioid Use: 1 = Past Month, 2 = Past Year, 3 = Ever, 8/9/NA = Never/Missing
    opioid_binary = ifelse(opioid_misuse %in% c(1, 2), 1, 0),
    
    # Recode Health: Higher values usually mean worse health in survey codes
    health_score = as.numeric(gen_health),
    
    # Recode Cognitive Difficulty: 1 = Yes, 2 = No
    mental_fog = ifelse(cog_difficulty == 1, 1, 0)
  ) %>%
  filter(!is.na(opioid_binary), !is.na(health_score))


# Calculate Correlation Matrix for Categorical Data
# This looks at the strength of the link between Opioid use and Health Status
cor_matrix <- polychor(analysis_df$opioid_binary, analysis_df$health_score)
print(paste("Polychoric Correlation:", round(cor_matrix, 3)))

# 3. Visualization: Health Status by Opioid Misuse
ggplot(analysis_df, aes(x = factor(opioid_binary), fill = factor(gen_health))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "Impact of Opioid Misuse on Self-Reported Health",
    x = "Opioid Misuse in Past Year (0 = No, 1 = Yes)",
    y = "Proportion of Population",
    fill = "Health Rating"
  ) +
  theme_minimal()


# Logistic Regression Model
# Outcome: Mental/Cognitive Difficulty (mental_fog)
# Predictor: Opioid Misuse (opioid_binary)
capstone_model <- glm(mental_fog ~ opioid_binary + age + poverty, 
                      data = analysis_df, 
                      family = binomial)

# Summarize results
model_summary <- summary(capstone_model)
print(model_summary)

# Calculate Odds Ratios (This is what you report in your project)
# e.g., "Those misusing opioids are X times more likely to report cognitive distress"
odds_ratios <- exp(coef(capstone_model))
print(odds_ratios)

# Load necessary libraries
library(tidyverse)
library(polycor) # For categorical correlations

# Filter and Clean Data
opioid_mh_data <- ds %>%
  select(
    # Opioid variables
    PNRNMREC, HERREC, 
    # Mental Health proxies
    HEALTH2, LVLDIFMEM2,
    # Contextual variables
    AGE3, INCOME, ANYHLTI2
  ) %>%
  mutate(
    # Create a binary "Opioid User" variable (1 = Used in past year, 0 = Not)
    opioid_user = ifelse(PNRNMREC %in% c(1, 2) | HERREC %in% c(1, 2), 1, 0),
    
    # Create a binary "Mental Health Distress" variable (using memory/cog difficulty)
    # 1 = Yes difficulty, 0 = No difficulty
    mh_distress = ifelse(LVLDIFMEM2 == 1, 1, 0),
    
    # Clean General Health (1=Excellent to 5=Poor)
    health_score = as.numeric(HEALTH2)
  ) %>%
  drop_na(opioid_user, mh_distress)

# 1. Correlation between Opioid Use and Cognitive/Mental Distress
binary_cor <- tetrachoric(table(opioid_mh_data$opioid_user, opioid_mh_data$mh_distress))
print(paste("Tetrachoric Correlation:", round(binary_cor$rho, 3)))

# 2. Correlation between Opioid Use and General Health Score
# A positive correlation here suggests opioid use is linked to lower health ratings
health_cor <- polychor(opioid_mh_data$opioid_user, opioid_mh_data$health_score)
print(paste("Polychoric Correlation (Health):", round(health_cor, 3)))

# Percentage of individuals with Mental/Cognitive Distress by Opioid Use
opioid_mh_data %>%
  group_by(opioid_user) %>%
  summarize(pct_distress = mean(mh_distress) * 100) %>%
  ggplot(aes(x = factor(opioid_user, labels = c("Non-User", "Opioid User")), 
             y = pct_distress, fill = factor(opioid_user))) +
  geom_col(width = 0.6) +
  labs(
    title = "Prevalence of Cognitive/Mental Distress by Opioid Use Status",
    subtitle = "Data source: NSDUH 2024",
    x = "User Group",
    y = "Percentage Reporting Distress (%)",
    fill = "Group"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("gray70", "firebrick"))

# Model: Does Opioid Use predict Mental Health Distress?
# We control for INCOME and AGE3
model <- glm(mh_distress ~ opioid_user + INCOME + AGE3, 
             data = opioid_mh_data, 
             family = binomial)

# 1. Get the summary
summary(model)

# 2. Extract Odds Ratios (Crucial for your report)
# This tells you: "Opioid users are X times more likely to have MH distress"
exp(coef(model))
