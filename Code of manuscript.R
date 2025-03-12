data <- read_excel(file_path)
library(dplyr)

# Here's cleaning the data as stated in the manuscript(MPAI)
data <- data %>% filter(!(Number %in% c("B16","G15","J12","M17","E11","F14","H12","L9","M3","Q10",
                                         "G7","T5","A15","B14","G5","J20","T12","T4","C3","D13","E4","T2",
                                         "R20","P17","S3")))

# Here's cleaning the data as stated in the manuscript(MST)
data <- data %>% filter(!(Number %in% c("E11","L9","M3","Q10","A15","J20","T4","C3","E4","D13","P17","S3")))


data$MPAI	 <- as.numeric(data$MPAI)
data$MST	 <- as.numeric(data$MST)
data$RI <- as.numeric(data$RI)
data$AX <- as.numeric(data$AX)
data <- data %>%
  group_by(Number) %>%
  mutate(
    RI_centered = RI	- mean(RI, na.rm = TRUE),
    AX_centered = AX	- mean(AX, na.rm = TRUE)
  ) %>%
  ungroup()

# Here's cleaning the data as stated in the manuscript
data_no_na <- data %>%
  filter(!is.na(RI)) 

data_no_na <- data_no_na %>%
  group_by(Number) %>%
  mutate(RI_check = ifelse(all(RI == first(RI)), "same", "different")) %>%
  ungroup()

invalid_subjects <- data_no_na %>%
  filter(RI_check == "same") %>%
  select(Number) %>%
  distinct()
data <- data %>%
  filter(!Number %in% invalid_subjects$Number)

data_no_na <- data %>%
  filter(!is.na(AX))  

data_no_na <- data_no_na %>%
  group_by(Number) %>%
  mutate(AX_check = ifelse(all(AX == first(AX)), "same", "different")) %>%
  ungroup()

invalid_subjects <- data_no_na %>%
  filter(AX_check == "same") %>%
  select(Number) %>%
  distinct()

data <- data %>%
  filter(!Number %in% invalid_subjects$Number)


data <- data %>%
  arrange(Number, Night) %>%
  group_by(Number) %>%
  mutate(RI_lag = lag(RI_centered)) %>%
  ungroup()

# Incorporation of holiday variables
data <- data %>%
  mutate(holiday = ifelse(Night >= 1 & Night <= 7, 0, ifelse(Night >= 8 & Night <= 14, 1, NA))) %>%
  mutate(holiday = factor(holiday, levels = c(0, 1)))

# Perform linear mixed model tests
library(lmerTest)
data_lag <- data %>% filter(!is.na(RI_lag))

model_RI <- lmer(use_time_hours~ RI_centered+RI_lag+Night+holiday +(1|Number), data_lag)
summary(model_RI)

model_null <- lmer(MPAI~ RI_centered+Night+holiday+(1|总Number), data_lag)
summary(model_null)

model_randomslope <- lmer(MPAI~ RI_centered+RI_lag+Night+holiday +(1+RI_centered|Number), data_lag)
summary(model_randomslope)

library(performance)
compare_performance(model_RI, model_null)
compare_performance(model_RI, model_randomslope)
anova(model_RI, model_null)
anova(model_RI, model_randomslope)

#No further code is shown here for MST
#as this is consistent with MPAI, although the dependent variable is changed in the model













