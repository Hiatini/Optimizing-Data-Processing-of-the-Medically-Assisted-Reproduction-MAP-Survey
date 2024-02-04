#Hiatini Tekohuotetua

# Install and load the necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
install.packages("tidyr", dependencies=TRUE)

library(ggplot2)
library(dplyr)
library(tidyr)

# First step, read our database
data <- read.csv2("data.csv")

# The next step is crucial to understand the data we have
head(data, 5)
summary(data)

# We will start directly with data cleaning after capturing the necessary content
cleaned_data <- data %>%
  gather(key = "attempt", value = "treatment", -id) %>%
  separate(attempt, into = c("variable", "attempt_num"), sep = "_") %>%
  mutate(attempt_num = as.numeric(stringr::str_extract(attempt_num, "\\d+"))) %>%
  arrange(id, attempt_num) %>%
  group_by(id) %>%
  pivot_wider(names_from = variable, values_from = treatment) %>%
  filter(!is.na(year) | !is.na(treatment))

View(cleaned_data)

# Visualization of the data after cleaning
ggplot(cleaned_data, aes(x = treatment)) +
  geom_bar(fill = "darkred") +
  labs(title = "Distribution of treatments after cleaning",
       x = "Treatment",
       y = "Frequency")

ggplot(cleaned_data, aes(x = attempt_num)) +
  geom_bar(fill = "darkred") +
  labs(title = "Distribution of attempts after cleaning",
       x = "Attempt",
       y = "Frequency")
