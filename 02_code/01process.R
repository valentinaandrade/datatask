# Code 1: Process ---------------------------------------------------------

# 1. load packages --------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse, readxl, plm, pglm)

# 2. load data ------------------------------------------------------------
data <- read_csv("01_input/src/PSID_subset.csv")

# 3. Restructure data -----------------------------------------------------
dlong <- data %>% 
  pivot_longer(cols = -c(FID_68, IID_68)) %>%  
  mutate(
    year = str_extract(name, "[0-9]+"),
    year = 2000 + as.numeric(year),
    name = str_remove(name, "[0-9]+"),
    name = str_remove(name, "_$"),
  ) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  janitor::clean_names() %>% 
  mutate(person_id = paste0(fid_68, "-", iid_68))

# 4. Cohorts --------------------------------------------------------------
# Identify individuals for pre and post cohorts based on 2007 and 2009 criteria
pre_ids <- dlong  %>%
  filter(year == 2007 & age >= 18 & age <= 22) %>%
  distinct(person_id) %>%
  pull(person_id)

post_ids <- dlong %>%
  filter(year == 2009 & age >= 18 & age <= 22) %>%
  distinct(person_id) %>%
  pull(person_id)


# Assign 'pre' and 'post' labels to these individuals across the entire dataset
cohorts <- dlong %>%
  filter(person_id %in% c(pre_ids, post_ids)) %>% 
  mutate(cohort = case_when(
    person_id %in% pre_ids ~ "pre",
    person_id %in% post_ids ~ "post",
    TRUE ~ NA_character_
  )) %>%

# Recode EMPL_ into the work_status column based on the provided criteria 
  mutate(work_status = case_when(
    empl == 1 ~ "working",
    empl == 3 ~ "unemployed",
    empl == 7 ~ "student",
    TRUE ~ NA_character_  # Set all other categories as NA
  ))


  

# Question 5: Plot --------------------------------------------------------
# Calculate proportions of 'working' and 'unemployed' for each year
work_status_proportions <- cohorts %>%
  filter(!is.na(work_status), age >= 17) %>% 
  group_by(year,  cohort, work_status) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count))

# Plot for 'working' status
work_status_proportions %>%
  filter(work_status == "working") %>%
  ggplot(aes(x = year, y = prop, color = cohort)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "red") +
  labs(title = "Proportion of Individuals in 'Working' Status Over Time",
       x = "Year",
       y = "Proportion",
       subtitle = "Red dashed line indicates 2008 financial crisis") +
  theme_minimal() +
  scale_x_continuous(breaks = 2002:2015)

# Plot for 'unemployed' status
work_status_proportions %>%
  filter(work_status == "unemployed") %>%
  ggplot(aes(x = year, y = prop, color = cohort)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "red") +
  labs(title = "Proportion of Individuals in 'Unemployed' Status Over Time",
       x = "Year",
       y = "Proportion",
       subtitle = "Red dashed line indicates 2008 financial crisis") +
  theme_minimal() + scale_x_continuous(breaks = 2002:2015)

# Plot for 'students' status
work_status_proportions %>%
  filter(work_status == "student") %>%
  ggplot(aes(x = year, y = prop, color = cohort)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "red") +
  labs(title = "Proportion of Individuals in 'Students' Status Over Time",
       x = "Year",
       y = "Proportion",
       subtitle = "Red dashed line indicates 2008 financial crisis") +
  theme_minimal() + scale_x_continuous(breaks = 2002:2015)


# Question 6: Model -------------------------------------------------------

#𝑌 = 𝛿 + 𝛿1𝑐𝑜ℎ𝑜𝑟𝑡𝑖 + 𝛿2𝑎𝑔𝑒𝑖𝑡 + 𝛿3𝑎𝑔𝑒𝑖𝑡 × 𝑔𝑟𝑎𝑑𝑖𝑡 × 𝑐𝑜ℎ𝑜𝑟𝑡𝑖 + 𝛾𝑡 + 𝜇𝑖𝑡

# Step 1: Create the 'grad' variable
cohorts <- cohorts %>%
  filter(!is.na(work_status)) %>% 
  mutate(grad = ifelse(age > 22, 1, 0),
         emp = if_else(empl == 1, 1,0),
         unemp = if_else(empl == 3,1,0)) 


# Linear model ------------------------------------------------------------

# Convert the dataset into a panel data frame
pdata <- pdata.frame(cohorts, index = c("person_id", "year"))

# Estimate the fixed-effects model
model_fe1 <- plm(emp ~ cohort + age + age:grad:cohort + as.factor(year),
                data = pdata, model = "within")
summary(model_fe1)

model_fe2 <- plm(unemp ~ cohort + age + age:grad:cohort + as.factor(year),
                data = pdata, model = "within")
summary(model_fe2)

# Logit -------------------------------------------------------------------
model_logit_fe1 <- pglm(emp ~ cohort + age + age:grad:cohort + as.factor(year), 
                      data = pdata, 
                      family = binomial(link="logit"), 
                      effect = "individual")
summary(model_logit_fe1)

model_logit_fe2 <- pglm(unemp ~ cohort + age + age:grad:cohort + as.factor(year), 
                       data = pdata, 
                       family = binomial(link="logit"), 
                       effect = "individual")
summary(model_logit_fe2)

