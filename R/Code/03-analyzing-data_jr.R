# Reproducible Research Fundamentals 
# 03. Data Analysis

# Install packages

# install.packages("modelsummary")
# install.packages("stargazer")
# install.packages("ggplot2")

# Libraries -----
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data -----
#household level data
data_path <- "C:/Users/wb637898/OneDrive - WBG/Course materials/DataWork/Data"
hh_data   <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta")) %>%
    rename(district = adm2_en) %>% 
    mutate(district = as_factor(district))

# Exercise 1 and 2: Create graph of area by district -----

# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = as_factor(district))

# Create the bar plot

ggplot(hh_data_plot, aes(x = district,
                         y = area_acre_w,
                         fill = treatment)) +
    geom_bar(stat = "summary",
             fun = "mean",
             position = position_dodge(width = 0.9)) +
    geom_text(stat = "summary",
              fun = "mean",
              aes(label = round(..y.., 1)),   # texto con el promedio
              position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3.5) +
    labs(title = "Average cultivated area by treatment across districts",
         x = "District",
         y = "Average area cultivated (acres)") +
    theme_minimal(base_size = 12)

       
       ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)
       
       
# Exercise 3: Create a density plot of non-food consumption -----
       
# Calculate mean non-food consumption for female and male-headed households
mean_female <- hh_data %>% 
   filter(female_head == 1) %>% 
   summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
   pull(mean)

mean_male <- hh_data %>% 
   filter(female_head == 0) %>% 
   summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
   pull(mean)
       

# Exercise 4: Summary statistics ----

# Create summary statistics by district and export to CSV

summary_table <- datasummary(
    (hh_size + food_cons_usd +female_head + crop_damage + n_child_5) ~ as_factor(district) * (Mean + SD),
    data = hh_data,
    title = "Summary Statistics by District",
    output = file.path("Outputs", "summary_table.csv")
)

# Exercise 5: Balance table ----
balance_table <- datasummary_balance(
    (female_head + crop_damage + food_cons_usd + hh_size) ~ treatment,
    data = hh_data,
    stars = TRUE,
    title = "Balance by Treatment Status",
    note = "Includes HHS with observations for baseline and endline",
    output = file.path("Outputs", "balance_table.csv")  # Change to CSV
)

# Exercise 6: Regressions ----

# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w ~ treatment, data = hh_data)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, data = hh_data)

# Model 3: Add FE by district
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood + factor(district), 
             data = hh_data)

# Create regression table using stargazer
stargazer(
    model1, model2, model3,
    title = "Food Consumption Effects",
    keep = c("treatment", "crop_damage", "drought_flood"),
    covariate.labels = c("Treatment",
                         "Crop Damage",
                         "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    dep.var.caption = "",
    add.lines = list(c("District Fixed Effects", "No", "No", "Yes")),
    header = FALSE,
    keep.stat = c("n", "adj.rsq"),
    notes = "Standard errors in parentheses",
    out = file.path("Outputs","regression_table.tex")
)

# Exercise 7: Combining two plots ----

long_data <- secondary_data %>%
    ungroup() %>% 
    select(-c(n_hospital, n_clinic)) %>% 
    pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
    mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
           in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))

