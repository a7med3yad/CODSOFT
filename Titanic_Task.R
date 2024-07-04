---
title: "Titanic predictive model"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
# Load required libraries
library(readxl)
library(ggplot2)
library(patchwork)
library(flexdashboard)


# Load Titanic dataset
data <- read.csv("E:\\Codsoft\\Titanic-Dataset.csv")

# Check for duplicates
duplicate_count <- sum(duplicated(data))
print(paste("Number of duplicates: ", duplicate_count))

# Check for missing values
missing_value_count <- sum(is.na(data))
print(paste("Number of missing values: ", missing_value_count))

# Calculate survival rates
survived_count <- length(which(data$Survived == 1))
not_survived_count <- length(which(data$Survived == 0))
survival_rates <- c(survived_count, not_survived_count)
survival_percentages <- round(survival_rates / sum(survival_rates) * 100, 1)

# Create a data frame for plotting
df_survival <- data.frame(
  category = c("Survived", "Not Survived"),
  count = survival_rates,
  percentage = survival_percentages
)

# Create a pie chart using ggplot2
p6 <- ggplot(df_survival, aes(x = "", y = count, fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Titanic Survival Rates by Outcome",
       subtitle = "Note: Survival rates based on 891 passengers") +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  theme(legend.position = "bottom")

# Calculate passenger counts by class
first_class_count <- length(which(data$Pclass == 1))
second_class_count <- length(which(data$Pclass == 2))
third_class_count <- length(which(data$Pclass == 3))
class_counts <- c(first_class_count, second_class_count, third_class_count)

# Create a data frame for plotting
df_class <- data.frame(
  class = c("1st class", "2nd class", "3rd class"),
  count = class_counts
)

# Create a pie chart using ggplot2
p5 <- ggplot(df_class, aes(x = "", y = count, fill = class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Distribution of Passenger Classes on the Titanic",
       subtitle = "Note: Based on 891 passengers") +
  scale_fill_manual(values = c("#4e79a7", "#f28e2c", "#e15759")) +
  theme(legend.position = "bottom")

# Calculate survival counts by class
first_class_survived <- sum(data$Pclass == 1 & data$Survived == 1)
first_class_not_survived <- sum(data$Pclass == 1 & data$Survived == 0)
second_class_survived <- sum(data$Pclass == 2 & data$Survived == 1)
second_class_not_survived <- sum(data$Pclass == 2 & data$Survived == 0)
third_class_survived <- sum(data$Pclass == 3 & data$Survived == 1)
third_class_not_survived <- sum(data$Pclass == 3 & data$Survived == 0)

# Create data frame for survival by class bar plot
survival_by_class_df <- data.frame(
  Class = c("1st", "1st", "2nd", "2nd", "3rd", "3rd"),
  Survival = c("Survived", "Did not survive", "Survived", "Did not survive", "Survived", "Did not survive"),
  Count = c(first_class_survived, first_class_not_survived, second_class_survived, second_class_not_survived, third_class_survived, third_class_not_survived)
)

# Create bar plot for survival by class
p1 <- ggplot(survival_by_class_df, aes(x = Class, y = Count, fill = Survival)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Survival by Class", x = "Class", y = "Number of Passengers") +
  scale_fill_manual(values = c("skyblue", "black"),
                    labels = c("Survived", "Did not survive"))

# Calculate mean age
mean_age <- mean(data$Age, na.rm = TRUE)  # Calculate mean age, ignoring NA values

# Calculate passenger counts by age
passengers_under_mean_age <- length(which(data$Age <= mean_age))  # Count passengers under mean age
passengers_over_mean_age <- length(which(data$Age > mean_age))  # Count passengers over mean age

# Calculate survival counts by age
survivors_under_mean_age <- length(which(data$Age <= mean_age & data$Survived == 1))  # Count survivors under mean age
non_survivors_under_mean_age <- passengers_under_mean_age - survivors_under_mean_age  # Count non-survivors under mean age
survivors_over_mean_age <- length(which(data$Age > mean_age & data$Survived == 1))  # Count survivors over mean age
non_survivors_over_mean_age <- passengers_over_mean_age - survivors_over_mean_age  # Count non-survivors over mean age

# Create data frame for survival by age bar plot
survival_by_age_df <- data.frame(
  Age = c("Under Mean Age", "Under Mean Age", "Over Mean Age", "Over Mean Age"),
  Survival = c("Survived", "Did not survive", "Survived", "Did not survive"),
  Count = c(survivors_under_mean_age, non_survivors_under_mean_age, survivors_over_mean_age, non_survivors_over_mean_age)
)

# Create bar plot for survival by age
p2 <- ggplot(survival_by_age_df, aes(x = Age, y = Count, fill = Survival)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Survival by Age", x = "Age", y = "Number of Passengers") +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("Survived", "Did not survive"))

# Create data frame for survival by sex
survival_by_sex_df <- data.frame(
  Sex = data$Sex,
  Survived = data$Survived
)

# Create bar plot for survival by sex
p3 <- ggplot(survival_by_sex_df, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Sex", x = "Sex", y = "Number of Passengers") +
  scale_fill_manual(values = c("yellow", "purple"),
                    labels = c("Did not survive", "Survived")) +
  theme_classic()

# Calculate passenger counts by travel companions
passengers_traveling_alone <- sum(data$SibSp == 0 & data$Parch == 0)  # Count passengers traveling alone
passengers_with_family_or_friends <- sum(data$SibSp > 0 & data$Parch > 0)  # Count passengers with family or friends

# Calculate survival counts by travel companions
survivors_traveling_alone <- sum(data$SibSp == 0 & data$Parch == 0 & data$Survived == 1)  # Count survivors traveling alone
non_survivors_traveling_alone <- passengers_traveling_alone - survivors_traveling_alone  # Count non-survivors traveling alone
survivors_with_family_or_friends <- sum(data$Parch > 0 & data$SibSp > 0 & data$Survived == 1)  # Count survivors with family or friends
non_survivors_with_family_or_friends <- passengers_with_family_or_friends - survivors_with_family_or_friends  # Count non-survivors with family or friends

# Create data frame for survival by travel companions bar plot
survival_by_companions_df <- data.frame(
  Travel_Companions = c("Alone", "Alone", "With Family/Friends", "With Family/Friends"),
  Survival = c("Survived", "Did not survive", "Survived", "Did not survive"),
  Count = c(survivors_traveling_alone, non_survivors_traveling_alone, survivors_with_family_or_friends, non_survivors_with_family_or_friends)
)

# Create bar plot for survival by travel companions
p4 <- ggplot(survival_by_companions_df, aes(x = Travel_Companions, y = Count, fill = Survival)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Survival by Travel Companions", x = "Travel Companions", y = "Number of Passengers") +
  scale_fill_manual(values = c("blue", "orange"),
                    labels = c("Survived", "Did not survive"))
```
1stPage
=======================================================================

Column {data-width=650}
-----------------------------------------------------------------------

```{r}
p6

```


```{r}
p5 

```


```{r}
p1
```

2ndPage
=======================================================================

Column {data-width=650}
-----------------------------------------------------------------------

```{r}
p2
```


```{r}
p3
```


```{r}
p4
```








