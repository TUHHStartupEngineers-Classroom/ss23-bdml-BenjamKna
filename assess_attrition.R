


#Function to calculate attrition cost
calculate_attrition_cost <- function(
    
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year *
    (workdays_position_open + workdays_onboarding * onboarding_efficiency)
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}

# Use this
# Function to convert counts to percentages. 
count_to_pct <- function(data, ..., col = n) {
  
  # capture the dots
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()
  
  return(ret)
  
}

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
  
  attrition_col_expr <- enquo(attrition_col)
  
  data %>%
    
    # Use parenthesis () to give tidy eval evaluation priority
    filter((!! attrition_col_expr) %in% attrition_value) %>%
    arrange(desc(pct)) %>%
    mutate(
      # Function inputs in numeric format (e.g. baseline_pct = 0.088 don't require tidy eval)
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )
  
}

