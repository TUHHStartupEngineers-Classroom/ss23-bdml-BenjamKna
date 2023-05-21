library(h2o)

# To launch H2O locally with default initialization arguments, use the following: 
h2o.init()


library(tidyverse)
hp_by_cyl <- mtcars %>% 
  group_by(cyl) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
hp_by_cyl
## # A tibble: 3 x 3
##     cyl min_hp max_hp
##   <dbl>  <dbl>  <dbl>
## 1     4     52    113
## 2     6    105    175
## 3     8    150    335

groupby_var <- quo(vs)

hp_by_vs <- mtcars %>% 
  group_by(!!groupby_var) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
hp_by_vs
## # A tibble: 2 x 3
## vs min_hp max_hp
## <dbl>  <dbl>  <dbl>
## 1     0     91    335
## 2     1     52    123



car_stats <- function(groupby_var, measure_var) {
  
  groupby_var <- enquo(groupby_var)
  measure_var <- enquo(measure_var)
  
  ret <- mtcars %>% 
    
    group_by(!!groupby_var) %>%
    summarize(min = min(!!measure_var), max = max(!!measure_var)) %>%
    
    # Optional: as_label() and "walrus operator" :=
    mutate(
      measure_var = as_label(measure_var), !!measure_var := "test"
    )
  
  return(ret)
  
}
car_stats(am,hp)
## # A tibble: 2 x 5
##      am   min   max measure_var hp   
##   <dbl> <dbl> <dbl> <chr>       <chr>
## 1     0    62   245 hp          test 
## 2     1    52   335 hp          test 

car_stats(gear,cyl)
## # A tibble: 3 x 5
##    gear   min   max measure_var cyl  
##   <dbl> <dbl> <dbl> <chr>       <chr>
## 1     3     4     8 cyl         test 
## 2     4     4     6 cyl         test 
## 3     5     4     8 cyl         test 


scatter_plot <- function(data, x_var, y_var) {
  
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  
  ret <- data %>% 
    ggplot(aes(x = !!x_var, y = !!y_var)) + 
    geom_point() + 
    geom_smooth() +
    ggtitle(str_c(as_label(y_var), " vs. ",as_label(x_var)))
  
  return(ret)
}
scatter_plot(mtcars, disp, hp)



######### Business case ############


employee_attrition_tbl <- read_csv("datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Business & Data Understanding: Department and Job Role

# Data subset
dept_job_role_tbl <- employee_attrition_tbl %>%
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dept_job_role_tbl %>%
  
  group_by(Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))
## # A tibble: 2 x 3
##   Attrition     n   pct
##   <chr>     <int> <dbl>
## 1 No         1233 0.839
## 2 Yes         237 0.161


# Attrition by department
dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2: Caution: It's easy to inadvertently miss grouping when creating counts & percents within groups
  group_by(Department) %>%
  mutate(pct = n / sum(n))
## # A tibble: 6 x 4
## # Groups:   Department [3]
##   Department             Attrition     n   pct
##   <chr>                  <chr>     <int> <dbl>
## 1 Human Resources        No           51 0.810
## 2 Human Resources        Yes          12 0.190
## 3 Research & Development No          828 0.862
## 4 Research & Development Yes         133 0.138
## 5 Sales                  No          354 0.794
## 6 Sales                  Yes          92 0.206


# Attrition by job role
dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") 
  


## # A tibble: 10 x 5
##    Department             JobRole                   Attrition     n    pct
##    <chr>                  <chr>                     <chr>     <int>  <dbl>
##  1 Human Resources        Human Resources           Yes          12 0.231 
##  2 Research & Development Healthcare Representative Yes           9 0.0687
##  3 Research & Development Laboratory Technician     Yes          62 0.239 
##  4 Research & Development Manager                   Yes           3 0.0556
##  5 Research & Development Manufacturing Director    Yes          10 0.0690
##  6 Research & Development Research Director         Yes           2 0.025 
##  7 Research & Development Research Scientist        Yes          47 0.161 
##  8 Sales                  Manager                   Yes           2 0.0541
##  9 Sales                  Sales Executive           Yes          57 0.175 
## 10 Sales                  Sales Representative      Yes          33 0.398

# Develop KPI

dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  )


# 
calculate_attrition_cost()
## [1] 78483.33
calculate_attrition_cost(200)
## [1] 15696667


dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  
  # Block 4. Set salaray to 80000 for now
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )


# Instead of
dept_job_role_tbl %>%
  
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) 
# Use this
dept_job_role_tbl %>%
  
  count(Department, JobRole, Attrition)




# This is way shorter and more flexibel
dept_job_role_tbl %>%
  count(JobRole, Attrition) %>%
  count_to_pct(JobRole)

dept_job_role_tbl %>%
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole)  



source("assess_attrition.R")
dept_job_role_tbl %>%
  
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )


dept_job_role_tbl %>%
  
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )



#############Visualizing###################

dept_job_role_tbl %>%
  
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>%
  
  # Data Manipulation
  mutate(name = str_c(Department, JobRole, sep = ": ") %>% as_factor()) %>%
  
  # Check levels
  # pull(name) %>%
  # levels()
  
  mutate(name      = fct_reorder(name, cost_of_attrition)) %>%
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2),
                           "M", sep = "")) %>%
  
  #Plotting
  ggplot(aes(cost_of_attrition, y = name)) +
  geom_segment(aes(xend = 0, yend = name),    color = "#2dc6d6") +
  geom_point(  aes(size = cost_of_attrition), color = "#2dc6d6") +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_attrition),
             hjust = "inward", color = "#2dc6d6") +
  scale_size(range = c(3, 5)) +
  labs(title = "Estimated cost of Attrition: By Dept and Job Role",
       y = "",
       x = "Cost of attrition") +
  theme(legend.position = "none")





# turn this into function


# This will return a quoted result
colnames(dept_job_role_tbl)[[1]]
## "EmployeeNumber"

# This will become an unquoted expression
rlang::sym(colnames(dept_job_role_tbl)[[1]])
## EmployeeNumber

# quos() captures it and turns it into a quosure, which is a list
# Will be evaluated at the time we use the double !! later on in the code.
# Then it will turn it into EmployeeNumber
quos(rlang::sym(colnames(employee_attrition_tbl)[[1]]))
## <list_of<quosure>>
##
## [[1]]
## <quosure>
## expr: ^rlang::sym(colnames(employee_attrition_tbl)[[1]])
## env:  global

# If the user supplies two different columns such as Department and Job Role
# or if the user does not supply a column the length will be different
quos(Department, JobRole) 
quos(Department, JobRole) %>% length()
## 2
quos() %>% length
## 0





# Function to plot attrition
plot_attrition <- function(data, 
                           ..., 
                           .value,
                           fct_reorder = TRUE,
                           fct_rev     = FALSE,
                           include_lbl = TRUE,
                           color       = "#2dc6d6",
                           units       = c("0", "K", "M")) {
  
  ### Inputs
  group_vars_expr   <- quos(...)
  
  # If the user does not supply anything, 
  # this takes the first column of the supplied data
  if (length(group_vars_expr) == 0) {
    group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
  }
  
  value_expr <- enquo(.value)
  
  units_val  <- switch(units[[1]],
                       "M" = 1e6,
                       "K" = 1e3,
                       "0" = 1)
  if (units[[1]] == "0") units <- ""
  
  # Data Manipulation
  # This is a so called Function Factory (a function that produces a function)
  usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
  
  # Create the axis labels and values for the plot
  data_manipulated <- data %>%
    mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>%
    mutate(value_text = str_c(usd(!! value_expr / units_val),
                              units[[1]], sep = ""))
  
  
  # Order the labels on the y-axis according to the input
  if (fct_reorder) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
      arrange(name)
  }
  
  if (fct_rev) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_rev(name)) %>%
      arrange(name)
  }
  
  # Visualization
  g <- data_manipulated %>%
    
    # "name" is a column name generated by our function internally as part of the data manipulation task
    ggplot(aes(x = (!! value_expr), y = name)) +
    geom_segment(aes(xend = 0, yend = name), color = color) +
    geom_point(aes(size = !! value_expr), color = color) +
    scale_x_continuous(labels = scales::dollar) +
    scale_size(range = c(3, 5)) +
    theme(legend.position = "none")
  
  # Plot labels if TRUE
  if (include_lbl) {
    g <- g +
      geom_label(aes(label = value_text, size = !! value_expr),
                 hjust = "inward", color = color)
  }
  
  return(g)
  
}  

dept_job_role_tbl %>%
  
  # Select columnns
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>%
  
  # Select columnns
  plot_attrition(Department, JobRole, .value = cost_of_attrition,
                 units = "M") +
  labs(
    title = "Estimated Cost of Attrition by Job Role",
    x = "Cost of Attrition",
    subtitle = "Looks like Sales Executive and Labaratory Technician are the biggest drivers of cost"
  )


# Now step two oif CRISP : Data understanding




# Libraries 
library(tidyverse)
library(readxl)
library(skimr)
library(GGally)

# Load Data data definitions

path_data_definitions <- "data_definitions.xlsx"
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

employee_attrition_tbl




# Descriptive Features
employee_attrition_tbl %>% select(Age, DistanceFromHome, Gender, MaritalStatus, NumCompaniesWorked, Over18)

# Employment Features
employee_attrition_tbl %>% select(Department, EmployeeCount, EmployeeNumber, JobInvolvement, JobLevel, JobRole, JobSatisfaction)

# Compensation Features
employee_attrition_tbl %>% select(DailyRate, HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel)

# Survery Results
employee_attrition_tbl %>% select(EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance)

# Performance Data
employee_attrition_tbl %>% select(JobInvolvement, PerformanceRating)

# Work-Life Features
employee_attrition_tbl %>% select(BusinessTravel, OverTime)

# Training & Education
employee_attrition_tbl %>% select(Education, EducationField, TrainingTimesLastYear)

# Time-Based Features
employee_attrition_tbl %>% select(TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)



# Step 1: Data Summarization -----

skim(employee_attrition_tbl)

# Character Data Type
employee_attrition_tbl %>%
  select_if(is.character) %>%
  glimpse()

# Get "levels"
employee_attrition_tbl %>%
  select_if(is.character) %>%
  map(unique)

# Proportions    
employee_attrition_tbl %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

# Numeric Data
employee_attrition_tbl %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())

employee_attrition_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  # Select all columns
  pivot_longer(everything()) %>%
  arrange(value) %>%
  filter(value <= 10)


#Exploratory data analysis (EDA): Part 2

library(GGally)
# Step 2: Data Visualization ----

employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  ggpairs() 


employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
          diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")



# Create data tibble, to potentially debug the plot_ggpairs function (because it has a data argument)
data <- employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome)

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}

employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  plot_ggpairs(color = Attrition)




# Explore Features by Category

#   1. Descriptive features: age, gender, marital status 
employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  plot_ggpairs(Attrition)

#   2. Employment features: department, job role, job level
employee_attrition_tbl %>%
  select(Attrition, contains("employee"), contains("department"), contains("job")) %>%
  plot_ggpairs(Attrition) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
employee_attrition_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)

#   5. Performance Data: Job Involvment, Performance Rating
employee_attrition_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

#   6. Work-Life Features 
employee_attrition_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

#   7. Training and Education 
employee_attrition_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

#   8. Time-Based Features: Years at company, years in current role
employee_attrition_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)


# 1. Compensation Features
# What can you deduce about the interaction between Monthly Income and Attrition?
# c. Those that are leaving have a lower Monthly Income


# 2. Compensation Features
# What can you deduce about the interaction between Percent Salary Hike and Attrition?

# d. It's difficult to deduce anything based on the visualization. 
# ... but rather c than a or b.

# 3. Compensation Features
# What can you deduce about the interaction between Stock Option Level and Attrition?
# b. Those that are staying have a higher stock option level

# 4. Survey Results
# What can you deduce about the interaction between Environment Satisfaction and Attrition?
# a. A higher proportion of those leaving have a low environment satisfaction level
  
# 5. Survey Results
# What can you deduce about the interaction between Work Life Balance and Attrition
# b. Those that are staying have a higher density of 2's and 3's

# 6. Performance Data
# What Can you deduce about the interaction between Job Involvement and Attrition?
# a. Those that are leaving have a lower density of 3's and 4's

# 7. Work-Life Features
# What can you deduce about the interaction between Over Time and Attrition?
# a. The proportion of those leaving that are working Over Time are high compared to those that are not leaving


# 8. Training and Education
# What can you deduce about the interaction between Training Times Last Year and Attrition
# b. People that leave tend to have less annual trainings

# 9. Time-Based Features
# What can you deduce about the interaction between Years At Company and Attrition
# b. People that leave tend to have less working years at the company

# 10. Time-Based Features
# What can you deduce about the interaction between Years Since Last Promotion and Attrition?
# c. It's difficult to deduce anything based on the visualization
# ... but rather b than a.

