library(rdecision)
library(readxl)
library(dplyr)
library(ggplot2)

# Define the Markov Model parameters, societal perspective
markov_variables_societal = list(
  # ds costs
  cost_ds_patient = 544.27,
  cost_ds_provider = 102.55, # average of BU and TD
  # mdr costs
  cost_mdr_patient = 2999.50,
  cost_mdr_provider = 1439.48, # average of BU and TD
  # Transition probabilities
  p_success_ds_6m = 0.9719,
  p_died_ds_6m = 0.0254,
  p_failure_ds_6m = 0.0027,
  p_success_mdr_9m = 0.84877,
  p_died_mdr_9m = 0.15123
)

# Define the Markov Model parameters, provider perspective
markov_variables_provider = list(
  # ds costs
  cost_ds_patient = 0,
  cost_ds_provider = 102.55, # average of BU and TD
  # mdr costs
  cost_mdr_patient = 0,
  cost_mdr_provider = 1439.48, # average of BU and TD
  # Transition probabilities
  p_success_ds_6m = 0.9719,
  p_died_ds_6m = 0.0254,
  p_failure_ds_6m = 0.0027,
  p_success_mdr_9m = 0.84877,
  p_died_mdr_9m = 0.15123
)

# Function to convert to monthly probabilities, taken from (32.)
monthly_prob <- function(p, n) {
  1 - (1 - p)^(1 / n)
}

# Load the Philippines natural death rate data
philippines_p_dying_by_age <- read_excel("phillipines p_dying by age.xlsx") %>%
  filter(age >= 40)

# Creating Markov model function with respect to the variable list (perspective)
markov_model_ds <- function(variable_list) {
  cost_ds_patient <- variable_list$cost_ds_patient
  cost_ds_provider <- variable_list$cost_ds_provider
  cost_ds_societal <- cost_ds_patient + cost_ds_provider
  cost_mdr_patient <- variable_list$cost_mdr_patient
  cost_mdr_provider <- variable_list$cost_mdr_provider
  cost_mdr_societal <- cost_mdr_patient + cost_mdr_provider
  p_success_ds_6m <- variable_list$p_success_ds_6m
  p_died_ds_6m <- variable_list$p_died_ds_6m
  p_failure_ds_6m <- variable_list$p_failure_ds_6m
  p_success_mdr_9m <- variable_list$p_success_mdr_9m
  p_died_mdr_9m <- variable_list$p_died_mdr_9m
  
  # Converting to monthly probabilities from probabilities with respect to regimen length
  p_success_ds_1m <- monthly_prob(p_success_ds_6m, 6)
  p_died_ds_1m <- monthly_prob(p_died_ds_6m, 6)
  p_failure_ds_1m <- monthly_prob(p_failure_ds_6m, 6)
  
  p_success_mdr_1m <- monthly_prob(p_success_mdr_9m, 9)
  p_died_mdr_1m <- monthly_prob(p_died_mdr_9m, 9)
  
  # Converting Philippines natural 12-month death probability to a monthly probability
  philippines_p_dying_by_age$p_natural_death_1m <- monthly_prob(philippines_p_dying_by_age$p_dying_12m, 12)
  
  # Markov states
  s.ds_tb_treatment <- MarkovState$new("ds_tb_treatment", cost = 0, utility = 0.69)
  s.mdr_tb_treatment <- MarkovState$new("mdr_tb_treatment", cost = 0, utility = 0.51)
  s.cured <- MarkovState$new("cured", cost = 0, utility = 0.88)
  s.death <- MarkovState$new("death", cost = 0, utility = 0)
  
  # Define transitions
  E <- list(
    Transition$new(s.ds_tb_treatment, s.ds_tb_treatment),
    Transition$new(s.ds_tb_treatment, s.mdr_tb_treatment, cost = cost_mdr_societal),
    Transition$new(s.ds_tb_treatment, s.death),
    Transition$new(s.ds_tb_treatment, s.cured),
    Transition$new(s.mdr_tb_treatment, s.mdr_tb_treatment),
    Transition$new(s.mdr_tb_treatment, s.death),
    Transition$new(s.mdr_tb_treatment, s.cured),
    Transition$new(s.cured, s.death),
    Transition$new(s.death, s.death),
    Transition$new(s.cured, s.cured)
  )
  
  # Define the state names
  snames <- c("ds_tb_treatment", "mdr_tb_treatment", "cured", "death")
  
  # The package does not allow for dynamic transition matrices, so we define a new matrix every 5 years which is a function of the natural death rate.
  # Creating function to generate the transition matrix
  generate_transition_matrix <- function(p_natural_death) {
    Pt <- matrix(
      data = c(
        1 - (p_failure_ds_1m + p_success_ds_1m + p_died_ds_1m + p_natural_death), p_failure_ds_1m, p_success_ds_1m, p_died_ds_1m + p_natural_death,
        0, 1 - (p_success_mdr_1m + p_died_mdr_1m + p_natural_death), p_success_mdr_1m, p_died_mdr_1m + p_natural_death,
        0, 0, (1 - p_natural_death), p_natural_death,
        0, 0, 0, 1
      ),
      nrow = 4, byrow = TRUE
    )
    dimnames(Pt) <- list(source = snames, target = snames)
    return(Pt)
  }
  
  # Generate transition matrices for different age groups up to 90 years old
  transition_matrices <- lapply(1:11, function(i) generate_transition_matrix(philippines_p_dying_by_age$p_natural_death_1m[i]))
  
  # Define a function to generate the Markov model as a function of the transition matrix
  generate_markov_model <- function(matrix) {
    M <- SemiMarkovModel$new(
      V = list(s.ds_tb_treatment, s.mdr_tb_treatment, s.cured, s.death),
      E,
      tcycle = as.difftime(30.44, units = "days"),
      discount.cost = 0,
      discount.utility = 0
    )
    M$set_probabilities(matrix)
    return(M)
  }
  
  # Create the Markov models for each age group
  models <- lapply(transition_matrices, generate_markov_model)
  
  # Functin calculating the discount rate for a given period in months
  calculate_discount_rate <- function(annual_rate, months) {
    return((1 + annual_rate)^(months / 12) - 1)
  }
  
  # Empty dataframe to store the results
  results_df_ds <- data.frame(
    Year = integer(),
    Discounted_Costs = numeric(),
    Discounted_QALYs = numeric(),
    ds_tb_treatment = numeric(),
    mdr_tb_treatment = numeric(),
    cured = numeric(),
    death = numeric()
  )
  
  # Function to process the Markov model for one year, applying a mean 6-month discount rate to all monthly cycle in the given year.
  process_markov_model <- function(M, cycles, discount_rate, year, results_df_ds) {
    MT <- M$cycles(cycles, hcc.pop = FALSE, hcc.cost = FALSE)
    # Extract final values, which will be the starting values for the next cycle.
    final_values <- MT[nrow(MT), ]
    discounted_qalys <- sum(MT$QALY) / (1 + discount_rate)
    discounted_costs <- sum(MT$Cost) / (1 + discount_rate)
    final_ds_tb_treatment <- final_values$ds_tb_treatment
    final_mdr_tb_treatment <- final_values$mdr_tb_treatment
    final_cured <- final_values$cured
    final_death <- final_values$death
    
    # Add the results as a new row to the results dataframe
    new_row <- data.frame(
      Year = year,
      Discounted_Costs = discounted_costs,
      Discounted_QALYs = discounted_qalys,
      ds_tb_treatment = final_ds_tb_treatment,
      mdr_tb_treatment = final_mdr_tb_treatment,
      cured = final_cured,
      death = final_death
    )
    
    results_df_ds <- rbind(results_df_ds, new_row)
    
    return(list(results_df_ds = results_df_ds, final_population = c(ds_tb_treatment = final_ds_tb_treatment, mdr_tb_treatment = final_mdr_tb_treatment, cured = final_cured, death = final_death)))
  }
  
  # Setting starting population (we consider 1 DS-TB patient aged 40)
  starting_population <- c(ds_tb_treatment = 1, mdr_tb_treatment = 0, death = 0, cured = 0)
  current_population <- starting_population
  age <- 40
  
  # Run the simulation for 50 years with each model
  for (year in 1:50) {
    model <- models[[ceiling(year / 5)]]
    
    # Calculate the discount rate for the current year
    discount_rate <- if (year == 1) {
      calculate_discount_rate(0.07, 6)  # 6-month rate for the first year, 7% as per HTA guidelines for philippines
    } else {
      months <- 6 + (year - 1) * 12
      calculate_discount_rate(0.07, months)  # 1.5-year discount rate, 2.5-year, etc. (This was believed to be the only way to account for the chosen set up, with different transition matrices every 5 years)
    }
    
    # Resetting the model with the current population
    model$reset(current_population)
    
    # Process the Markov model for one year and append the results to the dataframe
    result <- process_markov_model(model, 12, discount_rate, year, results_df_ds)
    results_df_ds <- result$results_df_ds
    current_population <- result$final_population
    
    # Increase age by one year
    age <- age + 1
  }
  
  print(results_df_ds)
  
  qalys_ds <- sum(results_df_ds$Discounted_QALYs) # Must add up QALYs from each year, as the output is an average per year.
  costs_ds <- sum(results_df_ds$Discounted_Costs) # Same for costs
  
  return(list(qalys_ds = qalys_ds, costs_ds = costs_ds))
}

# Run the Markov model with the provided variables
markov_model_ds_societal <- markov_model_ds(markov_variables_societal)
markov_model_ds_provider <- markov_model_ds(markov_variables_provider)

#
#
#

# MARKOV MODEL - MDR PATIENTS

# Redefining the Markov Model function for one mdr patient, instead of one DS-TB. Everything else is the same.
# Only difference in line 344.
markov_model_mdr <- function(variable_list) {
  cost_ds_patient <- variable_list$cost_ds_patient
  cost_ds_provider <- variable_list$cost_ds_provider
  cost_ds_societal <- cost_ds_patient + cost_ds_provider
  cost_mdr_patient <- variable_list$cost_mdr_patient
  cost_mdr_provider <- variable_list$cost_mdr_provider
  cost_mdr_societal <- cost_mdr_patient + cost_mdr_provider
  p_success_ds_6m <- variable_list$p_success_ds_6m
  p_died_ds_6m <- variable_list$p_died_ds_6m
  p_failure_ds_6m <- variable_list$p_failure_ds_6m
  p_success_mdr_9m <- variable_list$p_success_mdr_9m
  p_died_mdr_9m <- variable_list$p_died_mdr_9m
  
  p_success_ds_1m <- monthly_prob(p_success_ds_6m, 6)
  p_died_ds_1m <- monthly_prob(p_died_ds_6m, 6)
  p_failure_ds_1m <- monthly_prob(p_failure_ds_6m, 6)
  
  p_success_mdr_1m <- monthly_prob(p_success_mdr_9m, 9)
  p_died_mdr_1m <- monthly_prob(p_died_mdr_9m, 9)
  
  philippines_p_dying_by_age$p_natural_death_1m <- monthly_prob(philippines_p_dying_by_age$p_dying_12m, 12)
  
  s.ds_tb_treatment <- MarkovState$new("ds_tb_treatment", cost = 0, utility = 0.69)
  s.mdr_tb_treatment <- MarkovState$new("mdr_tb_treatment", cost = 0, utility = 0.51)
  s.cured <- MarkovState$new("cured", cost = 0, utility = 0.88)
  s.death <- MarkovState$new("death", cost = 0, utility = 0)
  
  E <- list(
    Transition$new(s.ds_tb_treatment, s.ds_tb_treatment),
    Transition$new(s.ds_tb_treatment, s.mdr_tb_treatment, cost = cost_mdr_societal),
    Transition$new(s.ds_tb_treatment, s.death),
    Transition$new(s.ds_tb_treatment, s.cured),
    Transition$new(s.mdr_tb_treatment, s.mdr_tb_treatment),
    Transition$new(s.mdr_tb_treatment, s.death),
    Transition$new(s.mdr_tb_treatment, s.cured),
    Transition$new(s.cured, s.death),
    Transition$new(s.death, s.death),
    Transition$new(s.cured, s.cured)
  )
  
  snames <- c("ds_tb_treatment", "mdr_tb_treatment", "cured", "death")
  
  generate_transition_matrix <- function(p_natural_death) {
    Pt <- matrix(
      data = c(
        1 - (p_failure_ds_1m + p_success_ds_1m + p_died_ds_1m + p_natural_death), p_failure_ds_1m, p_success_ds_1m, p_died_ds_1m + p_natural_death,
        0, 1 - (p_success_mdr_1m + p_died_mdr_1m + p_natural_death), p_success_mdr_1m, p_died_mdr_1m + p_natural_death,
        0, 0, (1 - p_natural_death), p_natural_death,
        0, 0, 0, 1
      ),
      nrow = 4, byrow = TRUE
    )
    dimnames(Pt) <- list(source = snames, target = snames)
    return(Pt)
  }
  
  transition_matrices <- lapply(1:11, function(i) generate_transition_matrix(philippines_p_dying_by_age$p_natural_death_1m[i]))
  
  generate_markov_model <- function(matrix) {
    M <- SemiMarkovModel$new(
      V = list(s.ds_tb_treatment, s.mdr_tb_treatment, s.cured, s.death),
      E,
      tcycle = as.difftime(30.44, units = "days"),
      discount.cost = 0,
      discount.utility = 0
    )
    M$set_probabilities(matrix)
    return(M)
  }
  
  models <- lapply(transition_matrices, generate_markov_model)
  
  calculate_discount_rate <- function(annual_rate, months) {
    return((1 + annual_rate)^(months / 12) - 1)
  }
  
  results_df_mdr <- data.frame(
    Year = integer(),
    Discounted_Costs = numeric(),
    Discounted_QALYs = numeric(),
    ds_tb_treatment = numeric(),
    mdr_tb_treatment = numeric(),
    cured = numeric(),
    death = numeric()
  )

  process_markov_model <- function(M, cycles, discount_rate, year, results_df_mdr) {
    MT <- M$cycles(cycles, hcc.pop = FALSE, hcc.cost = FALSE)
    
    final_values <- MT[nrow(MT), ]
    
    discounted_qalys <- sum(MT$QALY) / (1 + discount_rate)
    discounted_costs <- sum(MT$Cost) / (1 + discount_rate)
    final_ds_tb_treatment <- final_values$ds_tb_treatment
    final_mdr_tb_treatment <- final_values$mdr_tb_treatment
    final_cured <- final_values$cured
    final_death <- final_values$death
    
    new_row <- data.frame(
      Year = year,
      Discounted_Costs = discounted_costs,
      Discounted_QALYs = discounted_qalys,
      ds_tb_treatment = final_ds_tb_treatment,
      mdr_tb_treatment = final_mdr_tb_treatment,
      cured = final_cured,
      death = final_death
    )
    
    results_df_mdr <- rbind(results_df_mdr, new_row)
    
    return(list(results_df_mdr = results_df_mdr, final_population = c(ds_tb_treatment = final_ds_tb_treatment, mdr_tb_treatment = final_mdr_tb_treatment, cured = final_cured, death = final_death)))
  }
  
  starting_population <- c(ds_tb_treatment = 0, mdr_tb_treatment = 1, death = 0, cured = 0)
  current_population <- starting_population
  age <- 40
  
  for (year in 1:50) {
    model <- models[[ceiling(year / 5)]]
    
    discount_rate <- if (year == 1) {
      calculate_discount_rate(0.07, 6)  
    } else {
      months <- 6 + (year - 1) * 12
      calculate_discount_rate(0.07, months)  
    }
    
    model$reset(current_population)
    
    result <- process_markov_model(model, 12, discount_rate, year, results_df_mdr)
    results_df_mdr <- result$results_df_mdr
    current_population <- result$final_population
    
    age <- age + 1
  }
  
  print(results_df_mdr)
  
  qalys_mdr <- sum(results_df_mdr$Discounted_QALYs)
  costs_mdr <- sum(results_df_mdr$Discounted_Costs)
  
  return(list(qalys_mdr = qalys_mdr, costs_mdr = costs_mdr))
}

markov_model_mdr_societal <- markov_model_mdr(markov_variables_societal)
markov_model_mdr_provider <- markov_model_mdr(markov_variables_provider)

# No costs here as costs are only derived from failure (DS-TB -> MDR-TB) in the Markov Model

#
#
#
#
#

# Modelling the False Negatives - Delay to treatment initiation with worse survival probabilities 

# Increasing probability of death by 50%, inspired by (44).

# Define the False Negative Markov Model parameters, societal perspective
markov_variables_societal_fn = list(
  # ds costs
  cost_ds_patient = 544.27,
  cost_ds_provider = 102.55, # average of BU and TD
  # mdr costs
  cost_mdr_patient = 2999.50,
  cost_mdr_provider = 1439.48, # average of BU and TD
  # Transition probabilities, INCREASE IN MORTALITY
  p_died_ds_6m = 1.5 * 0.0254,
  p_failure_ds_6m = 0.0027,
  p_success_ds_6m = 1 - (1.5 * 0.0254) - 0.0027,
  p_died_mdr_9m = 1.5 * 0.15123,
  p_success_mdr_9m = 1 - (1.5 * 0.15123)
)

# Define the False Negative Markov Model parameters, provider perspective
markov_variables_provider_fn = list(
  # ds costs
  cost_ds_patient = 0,
  cost_ds_provider = 102.55, # average of BU and TD
  # mdr costs
  cost_mdr_patient = 0,
  cost_mdr_provider = 1439.48, # average of BU and TD
  # Transition probabilities
  p_died_ds_6m = 1.5 * 0.0254,
  p_failure_ds_6m = 0.0027,
  p_success_ds_6m = 1 - (1.5 * 0.0254) - 0.0027,
  p_died_mdr_9m = 1.5 * 0.15123,
  p_success_mdr_9m = 1 - 1.5 * 0.15123
)

# Run the DS-TB and MDR-TB models for false negatives from both perspectives
markov_model_ds_societal_fn <- markov_model_ds(markov_variables_societal_fn)
markov_model_ds_provider_fn <- markov_model_ds(markov_variables_provider_fn)

markov_model_mdr_societal_fn <- markov_model_mdr(markov_variables_societal_fn)
markov_model_mdr_provider_fn <- markov_model_mdr(markov_variables_provider_fn)




