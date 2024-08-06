library(rdecision)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
library(grid)

# Scenario 1: Standard model + Assumptions 

# Model Parameters from Societal Perspective 
model_variables_societal <- list(
  prev = BetaModVar$new("Prevalence", "", 70.003, 165.6973),
  sens_gx = BetaModVar$new("Sensitivity of GeneXpert", "", 222.24, 30.31),
  spec_gx = BetaModVar$new("Sensitivity of GeneXpert", "", 1505.07, 15.20),
  sens_rdt = BetaModVar$new("Sensitivity of nLRDT", "", 19.2, 4.8),
  spec_rdt = BetaModVar$new("Specificity of nLRDT", "", 1.02, 0.0208),
  p_naive = BetaModVar$new("Proportion naive", "", 11.4096, 1.5982),
  phi = BetaModVar$new("Proportion negative nLRDT further investigated", "", 84.85, 162.6333),
  p_resistant_in_experienced = BetaModVar$new("Proportion resistant given experienced", "" , 83.335, 480.8167),
  # Treatment Costs
  cost_ds_patient = GammaModVar$new("Patient cost of 6 month ds-TB treatment regimen","2024 USD", 100, 5.4427),
  cost_ds_provider = GammaModVar$new("Provider cost of 6 month ds-TB treatment regimen","2024 USD", 100, 1.0254),
  cost_mdr_patient = GammaModVar$new("Patient cost of 9 month mdr-TB treatment regimen","2024 USD", 100, 29.9950),
  cost_mdr_provider = GammaModVar$new("Provider cost of 9 month mdr-TB treatment regimen","2024 USD", 100, 14.3948),
  # Test Associated Costs
  cost_diagnostic_visit_provider = GammaModVar$new("Provider cost of an outpatient diagnostic visit","2024 USD", 100, 0.041111),
  cost_treatment_visit_provider = GammaModVar$new("Provider cost of an outpatient treatment visit", "2024 USD", 100, 0.032889),
  cost_sputum_collection_provider = GammaModVar$new("Provider cost of sputum collection", "2024 USD", 100, 0.072825),
  cost_shipping_provider = GammaModVar$new("Provider cost of shipping sputum sample to lab", "2024 USD", 100, 0.05), 
  cost_gx_test_provider = GammaModVar$new("Provider cost of GeneXpert test","2024 USD", 100, 0.2612),
  cost_rdt_test_provider = GammaModVar$new("Provider cost of nLRDT test", "2024 USD", 100, 0.1),
  cost_food_accom_travel_one_visit_patient = GammaModVar$new("Patient cost of food, accom, travel - one healthcare utilization", "2024 USD", 100, 0.0029),
  cost_income_loss_one_visit_patient = GammaModVar$new("Patient cost of income loss - one healthcare utilization", "2024 USD", 100, 0.1131),
  # Markov Model Outputs
  ds_qalys = GammaModVar$new("QALYs from Markov Model ds", "QALYs", 100, markov_model_ds_provider$qalys_ds/100),
  ds_costs = GammaModVar$new("Costs from Markov Model ds", "2024 USD", 100, markov_model_ds_provider$costs_ds/100),
  mdr_qalys = GammaModVar$new("QALYs from Markov Model mdr", "QALYs", 100, markov_model_mdr_provider$qalys_mdr/100),
  fn_ds_qalys = GammaModVar$new("QALYs from Markov Model ds False Negatives", "QALYs", 100, markov_model_ds_provider_fn$qalys_ds/100),
  fn_ds_costs = GammaModVar$new("Costs from Markov Model ds False Negatives", "2024 USD", 100, markov_model_ds_provider_fn$costs_ds/100),
  fn_mdr_qalys = GammaModVar$new("QALYs from Markov Model mdr False Negatives", "QALYs", 100, markov_model_mdr_provider_fn$qalys_mdr/100)
)


# Model Parameters from Societal Perspective - setting all patient cost variables to zero
model_variables_provider <- list(
  prev = BetaModVar$new("Prevalence", "", 70.003, 165.6973),
  sens_gx = BetaModVar$new("Sensitivity of GeneXpert", "", 222.24, 30.31),
  spec_gx = BetaModVar$new("Sensitivity of GeneXpert", "", 1505.07, 15.20),
  sens_rdt = BetaModVar$new("Sensitivity of nLRDT", "", 19.2, 4.8),
  spec_rdt = BetaModVar$new("Specificity of nLRDT", "", 1.02, 0.0208),
  p_naive = BetaModVar$new("Proportion naive", "", 11.4096, 1.5982),
  phi = BetaModVar$new("Proportion negative nLRDT further investigated", "", 84.85, 162.6333),
  p_resistant_in_experienced = BetaModVar$new("Proportion resistant given experienced", "" , 83.335, 480.8167),
  # Treatment Costs
  cost_ds_patient = ConstModVar$new("Patient cost of 6 month ds-TB treatment regimen","2024 USD", 0),
  cost_ds_provider = GammaModVar$new("Provider cost of 6 month ds-TB treatment regimen","2024 USD", 100, 1.0254),
  cost_mdr_patient = ConstModVar$new("Patient cost of 9 month mdr-TB treatment regimen","2024 USD", 0),
  cost_mdr_provider = GammaModVar$new("Provider cost of 9 month mdr-TB treatment regimen","2024 USD", 100, 14.3948),
  # Test Associated Costs
  cost_diagnostic_visit_provider = GammaModVar$new("Provider cost of an outpatient diagnostic visit","2024 USD", 100, 0.041111),
  cost_treatment_visit_provider = GammaModVar$new("Provider cost of an outpatient treatment visit", "2024 USD", 100, 0.032889),
  cost_sputum_collection_provider = GammaModVar$new("Provider cost of sputum collection", "2024 USD", 100, 0.072825),
  cost_shipping_provider = GammaModVar$new("Provider cost of shipping sputum sample to lab", "2024 USD", 100, 0.05), 
  cost_gx_test_provider = GammaModVar$new("Provider cost of GeneXpert test","2024 USD", 100, 0.2612),
  cost_rdt_test_provider = GammaModVar$new("Provider cost of nLRDT test", "2024 USD", 100, 0.1),
  cost_food_accom_travel_one_visit_patient = ConstModVar$new("Patient cost of food, accommodation and travel - one healthcare utilization", "2024 USD", 0),
  cost_income_loss_one_visit_patient = ConstModVar$new("Patient lost income cost - one healthcare utilization", "2024 USD", 0),
  # Markov Model Outputs
  ds_qalys = GammaModVar$new("QALYs from Markov Model ds", "QALYs", 100, markov_model_ds_provider$qalys_ds/100),
  ds_costs = GammaModVar$new("Costs from Markov Model ds", "2024 USD", 100, markov_model_ds_provider$costs_ds/100),
  mdr_qalys = GammaModVar$new("QALYs from Markov Model mdr", "QALYs", 100, markov_model_mdr_provider$qalys_mdr/100),
  fn_ds_qalys = GammaModVar$new("QALYs from Markov Model ds False Negatives", "QALYs", 100, markov_model_ds_provider_fn$qalys_ds/100),
  fn_ds_costs = GammaModVar$new("Costs from Markov Model ds False Negatives", "2024 USD", 100, markov_model_ds_provider_fn$costs_ds/100),
  fn_mdr_qalys = GammaModVar$new("QALYs from Markov Model mdr False Negatives", "QALYs", 100, markov_model_mdr_provider_fn$qalys_mdr/100)
)

# Creating decision tree model as a function of variable list (i.e societal or provider)
# This follows the 'rdecision' package reference manual
run_decision_model <- function(variable_list) {
  prev <- variable_list$prev
  sens_gx <- variable_list$sens_gx
  spec_gx <- variable_list$spec_gx
  sens_rdt <- variable_list$sens_rdt
  spec_rdt <- variable_list$spec_rdt
  p_naive <- variable_list$p_naive
  phi <- variable_list$phi
  p_resistant_in_experienced <- variable_list$p_resistant_in_experienced
  cost_ds_patient <- variable_list$cost_ds_patient
  cost_ds_provider <- variable_list$cost_ds_provider
  cost_mdr_patient <- variable_list$cost_mdr_patient
  cost_mdr_provider <- variable_list$cost_mdr_provider
  cost_diagnostic_visit_provider <- variable_list$cost_diagnostic_visit_provider
  cost_treatment_visit_provider <- variable_list$cost_treatment_visit_provider
  cost_sputum_collection_provider <- variable_list$cost_sputum_collection_provider
  cost_shipping_provider <- variable_list$cost_shipping_provider
  cost_gx_test_provider <- variable_list$cost_gx_test_provider
  cost_rdt_test_provider <- variable_list$cost_rdt_test_provider
  cost_food_accom_travel_one_visit_patient <- variable_list$cost_food_accom_travel_one_visit_patient
  cost_income_loss_one_visit_patient <- variable_list$cost_income_loss_one_visit_patient
  ds_qalys <- variable_list$ds_qalys
  ds_costs <- variable_list$ds_costs
  mdr_qalys <- variable_list$mdr_qalys
  fn_ds_qalys <- variable_list$fn_ds_qalys
  fn_ds_costs <- variable_list$fn_ds_costs
  fn_mdr_qalys <- variable_list$fn_mdr_qalys
  ds_healthy <- variable_list$ds_healthy
  
  # Define pathways costs (Table 2 in thesis)
  gx_pathway_cost_provider <- ExprModVar$new("GeneXpert provider pathway total cost", "", rlang::quo(
    cost_diagnostic_visit_provider +
      cost_sputum_collection_provider +
      cost_shipping_provider +
      cost_gx_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  rdt_pathway_cost_provider <- ExprModVar$new("nLRDT provider pathway total cost", "", rlang::quo(
    cost_diagnostic_visit_provider +
      cost_sputum_collection_provider +
      cost_rdt_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  gx_pathway_cost_patient <- ExprModVar$new("GeneXpert patient pathway total cost", "", rlang::quo(
    3 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  rdt_pathway_cost_patient <- ExprModVar$new("nLRDT patient pathway total cost", "", rlang::quo(
    2 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  gx_pathway_cost_societal <- ExprModVar$new("GX pathway societal total cost", "", rlang::quo(
    gx_pathway_cost_patient + gx_pathway_cost_provider), nemp = 1000L)
  
  rdt_pathway_cost_societal <- ExprModVar$new("nLRDT pathway societal total cost", "", rlang::quo(
    rdt_pathway_cost_patient + rdt_pathway_cost_provider), nemp = 1000L)
  
  # Further investigation after initial nLRDT pathway costs (Xpert)
  further_test_rdt_cost_provider <- ExprModVar$new("Further GX test after initial nLRDT provider cost", "", rlang::quo(
    cost_sputum_collection_provider +
      cost_shipping_provider +
      cost_gx_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  further_test_rdt_cost_patient <- ExprModVar$new("Further GX test after initial nLRDT patient cost", "", rlang::quo(
    2 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  further_test_rdt_cost_societal <- ExprModVar$new("Further GX test after initial nLRDT societal cost", "", rlang::quo(
    further_test_rdt_cost_patient + further_test_rdt_cost_provider), nemp = 1000L)
  
  # Treatment costs
  cost_ds_societal <- ExprModVar$new("Societal cost of 6 month ds-TB treatment regimen", "", rlang::quo(
    cost_ds_patient + cost_ds_provider), nemp = 1000L)
  
  cost_mdr_societal <- ExprModVar$new("Societal cost of 9 month mdr-TB treatment regimen", "", rlang::quo(
    cost_mdr_patient + cost_mdr_provider), nemp = 1000L)
  
  # Total costs for DS-TB treatments
  total_costs_ds_treatment_no_delay <- ExprModVar$new("Total cost of 6 month ds, no delay (including costs from Markov Model Failures)", "", rlang::quo(
    cost_ds_societal + ds_costs), nemp = 1000L)
  total_costs_ds_treatment_delay <- ExprModVar$new("Total cost of 6 month ds, including delay for false negatives (including costs from Markov Model Failures)", "", rlang::quo(
    cost_ds_societal + fn_ds_costs), nemp = 1000L)
  
  # Creating other variables as functions of existing variables
  p_experienced <- ExprModVar$new("Proportion experienced", "", rlang::quo(1 - p_naive), nemp = 1000L)
  p_sensitive_in_experienced <- ExprModVar$new("Proportion sensitive", "", rlang::quo(1 - p_resistant_in_experienced))
  p_resistant_pooled <- ExprModVar$new("Proportion resistant pooled", "", rlang::quo(p_resistant_in_experienced * p_experienced))
  p_sensitive_pooled <- ExprModVar$new("Proportion sensitive pooled", "", rlang::quo(1 - p_resistant_pooled))

# Defining branch probabilities for world without nLRDT (current pathway) (These follow top to bottom from Figure 1)
edge_1_p  <- ExprModVar$new("Probability for edge 1", "" , rlang::quo(prev * sens_gx), nemp = 1000L)
edge_2_p  <- ExprModVar$new("Probability for edge 2", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
edge_3_p  <- ExprModVar$new("Probability for edge 3", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
edge_4_p  <- ExprModVar$new("Probability for edge 4", "" , rlang::quo((1 - prev)*(1 - spec_gx)), nemp = 1000L)
edge_5_p  <- ExprModVar$new("Probability for edge 5", "" , rlang::quo((1 - prev)*spec_gx), nemp = 1000L)
edge_6_p  <- ExprModVar$new("Probability for edge 6", "" , rlang::quo(prev * (1 - sens_gx)), nemp = 1000L)
edge_7_p  <- ExprModVar$new("Probability for edge 7", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
edge_8_p  <- ExprModVar$new("Probability for edge 8", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)

# Defining branch probabilities world with nLRDT (hypothetical pathway)
edge_9_p  <- ExprModVar$new("Probability for edge 9", "" , rlang::quo(prev * p_naive * sens_rdt), nemp = 1000L)
edge_10_p <- ExprModVar$new("Probability for edge 10", "" , rlang::quo((1 - prev) * p_naive * (1 - spec_rdt)), nemp = 1000L)
edge_11_p <- ExprModVar$new("Probability for edge 11", "" , rlang::quo(prev * p_experienced * sens_rdt * sens_gx), nemp = 1000L)
edge_12_p  <- ExprModVar$new("Probability for edge 12", "" , rlang::quo(p_sensitive_in_experienced), nemp = 1000L)
edge_13_p  <- ExprModVar$new("Probability for edge 13", "" , rlang::quo(p_resistant_in_experienced), nemp = 1000L) 
edge_14_p <- ExprModVar$new("Probability for edge 14", "" , rlang::quo((1 - prev) * p_experienced * (1 - spec_rdt) * (1 - spec_gx)), nemp = 1000L)
edge_15_p <- ExprModVar$new("Probability for edge 15", "" , rlang::quo((1 - prev) * p_experienced * (1 - spec_rdt) * spec_gx), nemp = 1000L)
edge_16_p <- ExprModVar$new("Probability for edge 16", "" , rlang::quo(prev * p_experienced * sens_rdt * (1 - sens_gx)), nemp = 1000L)
edge_17_p  <- ExprModVar$new("Probability for edge 17", "" , rlang::quo(p_sensitive_in_experienced), nemp = 1000L)
edge_18_p  <- ExprModVar$new("Probability for edge 18", "" , rlang::quo(p_resistant_in_experienced), nemp = 1000L) 
edge_19_p  <- ExprModVar$new("Probability for edge 19", "" , rlang::quo(phi * prev * (1 - sens_rdt) * sens_gx), nemp = 1000L) 
edge_20_p  <- ExprModVar$new("Probability for edge 20", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
edge_21_p  <- ExprModVar$new("Probability for edge 21", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
edge_22_p  <- ExprModVar$new("Probability for edge 22", "" , rlang::quo(phi * (1 - prev) * spec_rdt * (1 - spec_gx)), nemp = 1000L) 
edge_23_p  <- ExprModVar$new("Probability for edge 23", "" , rlang::quo(phi * (1 - prev) * spec_rdt * spec_gx), nemp = 1000L) 
edge_24_p  <- ExprModVar$new("Probability for edge 24", "" , rlang::quo(phi * prev * (1 - sens_rdt) * (1 - sens_gx)), nemp = 1000L) 
edge_25_p  <- ExprModVar$new("Probability for edge 25", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
edge_26_p  <- ExprModVar$new("Probability for edge 26", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
edge_27_p  <- ExprModVar$new("Probability for edge 27", "" , rlang::quo((1 - phi) * (1 - prev) * spec_rdt), nemp = 1000L)
edge_28_p  <- ExprModVar$new("Probability for edge 28", "" , rlang::quo((1 - phi) * prev * (1 - sens_rdt)), nemp = 1000L)
edge_29_p  <- ExprModVar$new("Probability for edge 29", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
edge_30_p  <- ExprModVar$new("Probability for edge 30", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)

# Formulating decision tree

decision_node <- DecisionNode$new(label = "Decision Node") 
# Nodes for world without
chance_node_gx <- ChanceNode$new("Test Result")
chance_node_gx_true_pos <- ChanceNode$new("GX True Positive")
leaf_node_gx_true_pos_ds <- LeafNode$new("GX True Positive ds", utility = ds_qalys)
leaf_node_gx_true_pos_mdr <- LeafNode$new("GX True Positive mdr", utility = mdr_qalys)
leaf_node_gx_false_pos <- LeafNode$new("GX False Positive")
leaf_node_gx_true_neg <- LeafNode$new("GX True Negative")
chance_node_gx_false_neg <- ChanceNode$new("GX False Negative")
leaf_node_gx_false_neg_ds <- LeafNode$new("GX False Negative ds TB", utility = fn_ds_qalys)
leaf_node_gx_false_neg_mdr <- LeafNode$new("GX False Negative mdr TB", utility = fn_mdr_qalys)
# Nodes for world with 
chance_node_rdt <- ChanceNode$new("Test Result")
leaf_node_rdt_pos_naive_true_pos <- LeafNode$new("nLRDT Positive, Naive, True Positive", utility = ds_qalys)
leaf_node_rdt_pos_naive_false_pos <- LeafNode$new("nLRDT Positive, Naive, False Positive")
chance_node_rdt_pos_experienced_gx_true_pos <- ChanceNode$new("nLRDT Positive, Experienced, GX True Positive")
leaf_node_rdt_pos_experienced_gx_true_pos_ds <- LeafNode$new("nLRDT Positive, Experienced, GX True Positive ds", utility = ds_qalys)
leaf_node_rdt_pos_experienced_gx_true_pos_mdr <- LeafNode$new("nLRDT Positive, Experienced, GX True Positive mdr", utility = mdr_qalys)
leaf_node_rdt_pos_experienced_gx_false_pos <- LeafNode$new("nLRDT Positive, Experienced, GX False Positive")
leaf_node_rdt_pos_experienced_gx_true_neg <- LeafNode$new("nLRDT Positive, Experienced, GX True Negative")
chance_node_rdt_pos_experienced_gx_false_neg <- ChanceNode$new("nLRDT Positive, Experienced, GX False Negative")
leaf_node_rdt_pos_experienced_gx_false_neg_ds <- LeafNode$new("nLRDT Positive, Experienced, GX False Negative ds", utility = fn_ds_qalys)
leaf_node_rdt_pos_experienced_gx_false_neg_mdr <- LeafNode$new("nLRDT Positive, Experienced, GX False Negative mdr", utility = fn_mdr_qalys)
chance_node_rdt_neg_further_gx_true_pos <- ChanceNode$new("nLRDT Negative, Further GX investigation True Positive")
leaf_node_rdt_neg_further_gx_true_pos_ds <- LeafNode$new("nLRDT Negative, Further GX investigation True Positive ds", utility = ds_qalys)
leaf_node_rdt_neg_further_gx_true_pos_mdr <- LeafNode$new("nLRDT Negative, Further GX investigation True Positive mdr", utility = mdr_qalys)
leaf_node_rdt_neg_further_gx_false_pos <- LeafNode$new("nLRDT Negative, Further GX investigation False Positive")
leaf_node_rdt_neg_further_gx_true_neg <- LeafNode$new("nLRDT Negative, Further GX investigation True Negative")
chance_node_rdt_neg_further_gx_false_neg <- ChanceNode$new("nLRDT Negative, Further GX investigation False Negative")
leaf_node_rdt_neg_further_gx_false_neg_ds <- LeafNode$new("nLRDT Negative, Further GX investigation False Negative ds", utility = fn_ds_qalys)
leaf_node_rdt_neg_further_gx_false_neg_mdr <- LeafNode$new("nLRDT Negative, Further GX investigation False Negative mdr", utility = fn_mdr_qalys)
leaf_node_rdt_neg_no_further_gx_true_neg <- LeafNode$new("nLRDT Negative, No Further investigation, True Negative")
chance_node_rdt_neg_no_further_gx_false_neg <- ChanceNode$new("nLRDT Negative, No Further GX investigation False Negative")
leaf_node_rdt_neg_no_further_gx_false_neg_ds <- LeafNode$new("nLRDT Negative, No Further GX investigation False Negative ds", utility = fn_ds_qalys)
leaf_node_rdt_neg_no_further_gx_false_neg_mdr <- LeafNode$new("nLRDT Negative, No Further GX investigation False Negative mdr", utility = fn_mdr_qalys)

# Actions and Reactions (aka branches)

# world without
action_gx <- Action$new(decision_node, chance_node_gx, cost = gx_pathway_cost_societal, label = "GX")
reaction_1 <- Reaction$new(chance_node_gx, 
                           chance_node_gx_true_pos, p = edge_1_p, 
                           cost = 0.0, label = "True Positive GX"
)
reaction_2 <- Reaction$new(chance_node_gx_true_pos, 
                           leaf_node_gx_true_pos_ds, p = edge_2_p, 
                           cost = total_costs_ds_treatment_no_delay, label = "True Positive GX - ds"
)
reaction_3 <- Reaction$new(chance_node_gx_true_pos, 
                           leaf_node_gx_true_pos_mdr, p = edge_3_p, 
                           cost = cost_mdr_societal, label = "True Positive GX - mdr"
)
reaction_4 <- Reaction$new(chance_node_gx, 
                           leaf_node_gx_false_pos, p = edge_4_p, 
                           cost = cost_ds_societal, label = "False Positive GX"
)
reaction_5 <- Reaction$new(chance_node_gx, 
                           leaf_node_gx_true_neg, p = edge_5_p, 
                           cost = 0.0, label = "True Negative GX"
)
reaction_6 <- Reaction$new(chance_node_gx, 
                           chance_node_gx_false_neg, p = edge_6_p, 
                           cost = 0.0, label = "False Negative GX"
)
reaction_7 <- Reaction$new(chance_node_gx_false_neg, 
                           leaf_node_gx_false_neg_ds, p = edge_7_p, 
                           cost = total_costs_ds_treatment_delay, label = "False Negative GX - ds"
)
reaction_8 <- Reaction$new(chance_node_gx_false_neg, 
                           leaf_node_gx_false_neg_mdr, p = edge_8_p, 
                           cost = cost_mdr_societal, label = "False Negative GX - mdr"
)

# world with
action_rdt <- Action$new(decision_node, chance_node_rdt, cost = rdt_pathway_cost_societal, label = "nLRDT") 
reaction_9 <- Reaction$new(chance_node_rdt, 
                           leaf_node_rdt_pos_naive_true_pos, p = edge_9_p, 
                           cost = total_costs_ds_treatment_no_delay, label = "Positive nLRDT, naive true pos"
)
reaction_10 <- Reaction$new(chance_node_rdt, 
                            leaf_node_rdt_pos_naive_false_pos, p = edge_10_p, 
                            cost = cost_ds_societal, label = "Positive nLRDT, naive false pos"
)
reaction_11 <- Reaction$new(chance_node_rdt, 
                            chance_node_rdt_pos_experienced_gx_true_pos, p = edge_11_p, 
                            cost = 0.0, label = "Positive nLRDT, experienced -> GX true pos"
)
reaction_12 <- Reaction$new(chance_node_rdt_pos_experienced_gx_true_pos , 
                            leaf_node_rdt_pos_experienced_gx_true_pos_ds, p = edge_12_p, 
                            cost = total_costs_ds_treatment_no_delay, label = "Positive nLRDT, experienced -> GX true pos ds"
)
reaction_13 <- Reaction$new(chance_node_rdt_pos_experienced_gx_true_pos , 
                            leaf_node_rdt_pos_experienced_gx_true_pos_mdr, p = edge_13_p, 
                            cost = cost_mdr_societal, label = "Positive nLRDT, experienced -> GX true pos mdr"
)
reaction_14 <- Reaction$new(chance_node_rdt, 
                           leaf_node_rdt_pos_experienced_gx_false_pos, p = edge_14_p, 
                           cost = cost_ds_societal, label = "Positive nLRDT, experienced -> GX false pos"
)
reaction_15 <- Reaction$new(chance_node_rdt, 
                            leaf_node_rdt_pos_experienced_gx_true_neg, p = edge_15_p, 
                            cost = 0.0, label = "Positive nLRDT, experienced -> GX true neg"
)
reaction_16 <- Reaction$new(chance_node_rdt, 
                            chance_node_rdt_pos_experienced_gx_false_neg, p = edge_16_p, 
                            cost = 0.0, label = "Positive nLRDT, experienced -> GX false neg"
)
reaction_17 <- Reaction$new(chance_node_rdt_pos_experienced_gx_false_neg,
                            leaf_node_rdt_pos_experienced_gx_false_neg_ds, p = edge_17_p, 
                            cost = total_costs_ds_treatment_delay, label = "Positive nLRDT, experienced -> GX false neg ds"
)
reaction_18 <- Reaction$new(chance_node_rdt_pos_experienced_gx_false_neg,
                            leaf_node_rdt_pos_experienced_gx_false_neg_mdr, p = edge_18_p, 
                            cost = cost_mdr_societal, label = "Positive nLRDT, experienced -> GX false neg mdr"
)
reaction_19 <- Reaction$new(chance_node_rdt, 
                            chance_node_rdt_neg_further_gx_true_pos, p = edge_19_p, 
                            cost = 0.0, label = "Negative nLRDT further test GX true pos"
)
reaction_20 <- Reaction$new(chance_node_rdt_neg_further_gx_true_pos, 
                            leaf_node_rdt_neg_further_gx_true_pos_ds, p = edge_20_p, 
                            cost = total_costs_ds_treatment_no_delay, label = "Negative nLRDT further test GX true pos - ds"
)
reaction_21 <- Reaction$new(chance_node_rdt_neg_further_gx_true_pos, 
                            leaf_node_rdt_neg_further_gx_true_pos_mdr, p = edge_21_p, 
                            cost = cost_mdr_societal, label = "Negative nLRDT further test GX true pos - mdr"
)
reaction_22 <- Reaction$new(chance_node_rdt, 
                            leaf_node_rdt_neg_further_gx_false_pos, p = edge_22_p, 
                            cost = cost_ds_societal, label = "Negative nLRDT further test GX false pos"
)
reaction_23 <- Reaction$new(chance_node_rdt, 
                            leaf_node_rdt_neg_further_gx_true_neg, p = edge_23_p, 
                            cost = 0.0, label = "Negative nLRDT further test GX true neg"
)
reaction_24 <- Reaction$new(chance_node_rdt, 
                            chance_node_rdt_neg_further_gx_false_neg, p = edge_24_p, 
                            cost = 0.0, label = "Negative nLRDT further test GX false neg"
)
reaction_25 <- Reaction$new(chance_node_rdt_neg_further_gx_false_neg, 
                            leaf_node_rdt_neg_further_gx_false_neg_ds, p = edge_25_p, 
                            cost = total_costs_ds_treatment_delay, label = "Negative nLRDT further test GX false neg ds"
)
reaction_26 <- Reaction$new(chance_node_rdt_neg_further_gx_false_neg, 
                            leaf_node_rdt_neg_further_gx_false_neg_mdr, p = edge_26_p, 
                            cost = cost_mdr_societal, label = "Negative nLRDT further test GX false neg mdr"
)
reaction_27 <- Reaction$new(chance_node_rdt, 
                            leaf_node_rdt_neg_no_further_gx_true_neg, p = edge_27_p, 
                            cost = 0.0, label = "True Negative nLRDT"
)
reaction_28 <- Reaction$new(chance_node_rdt, 
                            chance_node_rdt_neg_no_further_gx_false_neg, p = edge_28_p, 
                            cost = 0.0, label = "False Negative nLRDT"
)
reaction_29 <- Reaction$new(chance_node_rdt_neg_no_further_gx_false_neg, 
                            leaf_node_rdt_neg_no_further_gx_false_neg_ds, p = edge_29_p, 
                            cost = total_costs_ds_treatment_delay, label = "False Negative nLRDT ds"
)
reaction_30 <- Reaction$new(chance_node_rdt_neg_no_further_gx_false_neg, 
                            leaf_node_rdt_neg_no_further_gx_false_neg_mdr, p = edge_30_p, 
                            cost = cost_mdr_societal, label = "False Negative nLRDT mdr"
)

# Create tree

dt <- DecisionTree$new(
V = list(
  decision_node,
  # world without
  chance_node_gx,
  chance_node_gx_true_pos,
  leaf_node_gx_true_pos_ds,
  leaf_node_gx_true_pos_mdr,
  leaf_node_gx_false_pos,
  leaf_node_gx_true_neg,
  chance_node_gx_false_neg,
  leaf_node_gx_false_neg_ds,
  leaf_node_gx_false_neg_mdr,
  # world with 
  chance_node_rdt,
  leaf_node_rdt_pos_naive_true_pos,
  leaf_node_rdt_pos_naive_false_pos,
  chance_node_rdt_pos_experienced_gx_true_pos,
  leaf_node_rdt_pos_experienced_gx_true_pos_ds,
  leaf_node_rdt_pos_experienced_gx_true_pos_mdr,
  leaf_node_rdt_pos_experienced_gx_false_pos,
  leaf_node_rdt_pos_experienced_gx_true_neg,
  chance_node_rdt_pos_experienced_gx_false_neg,
  leaf_node_rdt_pos_experienced_gx_false_neg_ds,
  leaf_node_rdt_pos_experienced_gx_false_neg_mdr,
  chance_node_rdt_neg_further_gx_true_pos,
  leaf_node_rdt_neg_further_gx_true_pos_ds,
  leaf_node_rdt_neg_further_gx_true_pos_mdr,
  leaf_node_rdt_neg_further_gx_false_pos,
  leaf_node_rdt_neg_further_gx_true_neg,
  chance_node_rdt_neg_further_gx_false_neg,
  leaf_node_rdt_neg_further_gx_false_neg_ds,
  leaf_node_rdt_neg_further_gx_false_neg_mdr,
  leaf_node_rdt_neg_no_further_gx_true_neg,
  chance_node_rdt_neg_no_further_gx_false_neg,
  leaf_node_rdt_neg_no_further_gx_false_neg_ds,
  leaf_node_rdt_neg_no_further_gx_false_neg_mdr
),
E = list(
  # world without
  action_gx,
  reaction_1,
  reaction_2,
  reaction_3,
  reaction_4,
  reaction_5,
  reaction_6,
  reaction_7,
  reaction_8,
  # world with
  action_rdt,
  reaction_9,
  reaction_10,
  reaction_11,
  reaction_12,
  reaction_13,
  reaction_14,
  reaction_15,
  reaction_16,
  reaction_17,
  reaction_18,
  reaction_19,
  reaction_20,
  reaction_21,
  reaction_22,
  reaction_23,
  reaction_24,
  reaction_25,
  reaction_26,
  reaction_27,
  reaction_28,
  reaction_29,
  reaction_30
)
)

# Defining outputs of the function 

results <- dt$evaluate(setvars = "expected") # Evaluate using mean parameter values

path <- dt$evaluate(by = "path") # Evaluate proportion of cohort, costs and utilities at each terminal node

incremental_qalys <- (results[2,7] - results[1,7])
incremental_costs <- (results[2,4] - results[1,4])

ICER <- (results[2,4] - results[1,4]) /
        (results[2,7] - results[1,7])

NMB <- ((results[2,7] - results[1,7]) * 1357.47) - (results[2,4] - results[1,4]) # Using estimate of WTP

output <- list(results = results, 
               path = path, 
               incrementsl_qalys = incremental_qalys,
               incremental_costs = incremental_costs,
               ICER = ICER, 
               NMB = NMB)

return(output)
}

# Scenario 1: Base Case (societal)

base_case_societal <- run_decision_model(model_variables_societal)

# Scenario Analysis - Varying sens and spec of nLRDT (societal Perspective) for the six combinations

sensitivity_values <- c(0.6, 0.7, 0.8, 0.9,  0.8,  0.9)
specificity_values <- c(0.9, 0.8, 0.7, 0.6, 0.98, 0.98)

for (i in 1:length(sensitivity_values)) {
  model_variables_societal$sens_rdt <- ConstModVar$new("", "", sensitivity_values[i])
  model_variables_societal$spec_rdt <- ConstModVar$new("", "", specificity_values[i])
  output <- run_decision_model(model_variables_societal)
  print(output)
}

# Scenario 1: Base Case (provider)

base_case_provider <- run_decision_model(model_variables_provider)

# Scenario Analysis - Varying sens and spec of nLRDT (provider perspective) for the six combinations

for (i in 1:length(sensitivity_values)) {
  model_variables_provider$sens_rdt <- ConstModVar$new("", "", sensitivity_values[i])
  model_variables_provider$spec_rdt <- ConstModVar$new("", "", specificity_values[i])
  output <- run_decision_model(model_variables_provider)
  print(output)
}

#
#
#
#
#

# Scenario 2 - Tongue Swabs + One Day nLRDT Visit

# Defining new function for scenario 2, once more a function of variable list (e.g. societal or provider)

run_decision_model_scenario_2 <- function(variable_list) {
  prev <- variable_list$prev
  sens_gx <- variable_list$sens_gx
  spec_gx <- variable_list$spec_gx
  sens_rdt <- variable_list$sens_rdt
  spec_rdt <- variable_list$spec_rdt
  p_naive <- variable_list$p_naive
  phi <- variable_list$phi
  p_resistant_in_experienced <- variable_list$p_resistant_in_experienced
  cost_ds_patient <- variable_list$cost_ds_patient
  cost_ds_provider <- variable_list$cost_ds_provider
  cost_mdr_patient <- variable_list$cost_mdr_patient
  cost_mdr_provider <- variable_list$cost_mdr_provider
  cost_diagnostic_visit_provider <- variable_list$cost_diagnostic_visit_provider
  cost_treatment_visit_provider <- variable_list$cost_treatment_visit_provider
  cost_sputum_collection_provider <- variable_list$cost_sputum_collection_provider
  cost_shipping_provider <- variable_list$cost_shipping_provider
  cost_gx_test_provider <- variable_list$cost_gx_test_provider
  cost_rdt_test_provider <- variable_list$cost_rdt_test_provider
  cost_food_accom_travel_one_visit_patient <- variable_list$cost_food_accom_travel_one_visit_patient
  cost_income_loss_one_visit_patient <- variable_list$cost_income_loss_one_visit_patient
  ds_qalys <- variable_list$ds_qalys
  ds_costs <- variable_list$ds_costs
  mdr_qalys <- variable_list$mdr_qalys
  fn_ds_qalys <- variable_list$fn_ds_qalys
  fn_ds_costs <- variable_list$fn_ds_costs
  fn_mdr_qalys <- variable_list$fn_mdr_qalys
  ds_healthy <- variable_list$ds_healthy
  
  # Define pathways costs
  gx_pathway_cost_provider <- ExprModVar$new("GeneXpert provider pathway total cost", "", rlang::quo(
    cost_diagnostic_visit_provider +
      cost_sputum_collection_provider +
      cost_shipping_provider +
      cost_gx_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  rdt_pathway_cost_provider <- ExprModVar$new("nLRDT provider pathway total cost", "", rlang::quo(
    cost_diagnostic_visit_provider +
      # Here, we remove sputum collection cost
      cost_rdt_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  gx_pathway_cost_patient <- ExprModVar$new("GeneXpert patient pathway total cost", "", rlang::quo(
    3 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  rdt_pathway_cost_patient <- ExprModVar$new("nLRDT patient pathway total cost", "", rlang::quo(
    1 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  # Here, we change for 1 day visit
  
  gx_pathway_cost_societal <- ExprModVar$new("GX pathway societal total cost", "", rlang::quo(
    gx_pathway_cost_patient + gx_pathway_cost_provider), nemp = 1000L)
  
  rdt_pathway_cost_societal <- ExprModVar$new("nLRDT pathway societal total cost", "", rlang::quo(
    rdt_pathway_cost_patient + rdt_pathway_cost_provider), nemp = 1000L)
  
  # Further investigation after initial nLRDT pathway costs (Xpert)
  further_test_rdt_cost_provider <- ExprModVar$new("Further GX test after initial nLRDT provider cost", "", rlang::quo(
    cost_sputum_collection_provider +
      cost_shipping_provider +
      cost_gx_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  further_test_rdt_cost_patient <- ExprModVar$new("Further GX test after initial nLRDT patient cost", "", rlang::quo(
    2 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  further_test_rdt_cost_societal <- ExprModVar$new("Further GX test after initial nLRDT societal cost", "", rlang::quo(
    further_test_rdt_cost_patient + further_test_rdt_cost_provider), nemp = 1000L)
  
  # Treatment costs
  cost_ds_societal <- ExprModVar$new("Societal cost of 6 month ds-TB treatment regimen", "", rlang::quo(
    cost_ds_patient + cost_ds_provider), nemp = 1000L)
  
  cost_mdr_societal <- ExprModVar$new("Societal cost of 9 month mdr-TB treatment regimen", "", rlang::quo(
    cost_mdr_patient + cost_mdr_provider), nemp = 1000L)
  
  # Total costs for DS-TB treatments
  total_costs_ds_treatment_no_delay <- ExprModVar$new("Total cost of 6 month ds, no delay (including costs from Markov Model Failures)", "", rlang::quo(
    cost_ds_societal + ds_costs), nemp = 1000L)
  total_costs_ds_treatment_delay <- ExprModVar$new("Total cost of 6 month ds, including delay for false negatives (including costs from Markov Model Failures)", "", rlang::quo(
    cost_ds_societal + fn_ds_costs), nemp = 1000L)
  
  # Creating other variables as function of existing variables
  p_experienced <- ExprModVar$new("Proportion experienced", "", rlang::quo(1 - p_naive), nemp = 1000L)
  p_sensitive_in_experienced <- ExprModVar$new("Proportion sensitive", "", rlang::quo(1 - p_resistant_in_experienced))
  p_resistant_pooled <- ExprModVar$new("Proportion resistant pooled", "", rlang::quo(p_resistant_in_experienced * p_experienced))
  p_sensitive_pooled <- ExprModVar$new("Proportion sensitive pooled", "", rlang::quo(1 - p_resistant_pooled))
  
  # world without nLRDT (current pathway)
  edge_1_p  <- ExprModVar$new("Probability for edge 1", "" , rlang::quo(prev * sens_gx), nemp = 1000L)
  edge_2_p  <- ExprModVar$new("Probability for edge 2", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_3_p  <- ExprModVar$new("Probability for edge 3", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  edge_4_p  <- ExprModVar$new("Probability for edge 4", "" , rlang::quo((1 - prev)*(1 - spec_gx)), nemp = 1000L)
  edge_5_p  <- ExprModVar$new("Probability for edge 5", "" , rlang::quo((1 - prev)*spec_gx), nemp = 1000L)
  edge_6_p  <- ExprModVar$new("Probability for edge 6", "" , rlang::quo(prev * (1 - sens_gx)), nemp = 1000L)
  edge_7_p  <- ExprModVar$new("Probability for edge 7", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_8_p  <- ExprModVar$new("Probability for edge 8", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  
  # world with nLRDT (hypothetical pathway)
  edge_9_p  <- ExprModVar$new("Probability for edge 9", "" , rlang::quo(prev * p_naive * sens_rdt), nemp = 1000L)
  edge_10_p <- ExprModVar$new("Probability for edge 10", "" , rlang::quo((1 - prev) * p_naive * (1 - spec_rdt)), nemp = 1000L)
  edge_11_p <- ExprModVar$new("Probability for edge 11", "" , rlang::quo(prev * p_experienced * sens_rdt * sens_gx), nemp = 1000L)
  edge_12_p  <- ExprModVar$new("Probability for edge 12", "" , rlang::quo(p_sensitive_in_experienced), nemp = 1000L)
  edge_13_p  <- ExprModVar$new("Probability for edge 13", "" , rlang::quo(p_resistant_in_experienced), nemp = 1000L) 
  edge_14_p <- ExprModVar$new("Probability for edge 14", "" , rlang::quo((1 - prev) * p_experienced * (1 - spec_rdt) * (1 - spec_gx)), nemp = 1000L)
  edge_15_p <- ExprModVar$new("Probability for edge 15", "" , rlang::quo((1 - prev) * p_experienced * (1 - spec_rdt) * spec_gx), nemp = 1000L)
  edge_16_p <- ExprModVar$new("Probability for edge 16", "" , rlang::quo(prev * p_experienced * sens_rdt * (1 - sens_gx)), nemp = 1000L)
  edge_17_p  <- ExprModVar$new("Probability for edge 17", "" , rlang::quo(p_sensitive_in_experienced), nemp = 1000L)
  edge_18_p  <- ExprModVar$new("Probability for edge 18", "" , rlang::quo(p_resistant_in_experienced), nemp = 1000L) 
  edge_19_p  <- ExprModVar$new("Probability for edge 19", "" , rlang::quo(phi * prev * (1 - sens_rdt) * sens_gx), nemp = 1000L) 
  edge_20_p  <- ExprModVar$new("Probability for edge 20", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_21_p  <- ExprModVar$new("Probability for edge 21", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  edge_22_p  <- ExprModVar$new("Probability for edge 22", "" , rlang::quo(phi * (1 - prev) * spec_rdt * (1 - spec_gx)), nemp = 1000L) 
  edge_23_p  <- ExprModVar$new("Probability for edge 23", "" , rlang::quo(phi * (1 - prev) * spec_rdt * spec_gx), nemp = 1000L) 
  edge_24_p  <- ExprModVar$new("Probability for edge 24", "" , rlang::quo(phi * prev * (1 - sens_rdt) * (1 - sens_gx)), nemp = 1000L) 
  edge_25_p  <- ExprModVar$new("Probability for edge 25", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_26_p  <- ExprModVar$new("Probability for edge 26", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  edge_27_p  <- ExprModVar$new("Probability for edge 27", "" , rlang::quo((1 - phi) * (1 - prev) * spec_rdt), nemp = 1000L)
  edge_28_p  <- ExprModVar$new("Probability for edge 28", "" , rlang::quo((1 - phi) * prev * (1 - sens_rdt)), nemp = 1000L)
  edge_29_p  <- ExprModVar$new("Probability for edge 29", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_30_p  <- ExprModVar$new("Probability for edge 30", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  
  # Formulating decision tree - same as Scenario 1
  
  decision_node <- DecisionNode$new(label = "Decision Node") 
  # world without
  chance_node_gx <- ChanceNode$new("Test Result")
  chance_node_gx_true_pos <- ChanceNode$new("GX True Positive")
  leaf_node_gx_true_pos_ds <- LeafNode$new("GX True Positive ds", utility = ds_qalys)
  leaf_node_gx_true_pos_mdr <- LeafNode$new("GX True Positive mdr", utility = mdr_qalys)
  leaf_node_gx_false_pos <- LeafNode$new("GX False Positive")
  leaf_node_gx_true_neg <- LeafNode$new("GX True Negative")
  chance_node_gx_false_neg <- ChanceNode$new("GX False Negative")
  leaf_node_gx_false_neg_ds <- LeafNode$new("GX False Negative ds TB", utility = fn_ds_qalys)
  leaf_node_gx_false_neg_mdr <- LeafNode$new("GX False Negative mdr TB", utility = fn_mdr_qalys)
  # world with 
  chance_node_rdt <- ChanceNode$new("Test Result")
  leaf_node_rdt_pos_naive_true_pos <- LeafNode$new("nLRDT Positive, Naive, True Positive", utility = ds_qalys)
  leaf_node_rdt_pos_naive_false_pos <- LeafNode$new("nLRDT Positive, Naive, False Positive")
  chance_node_rdt_pos_experienced_gx_true_pos <- ChanceNode$new("nLRDT Positive, Experienced, GX True Positive")
  leaf_node_rdt_pos_experienced_gx_true_pos_ds <- LeafNode$new("nLRDT Positive, Experienced, GX True Positive ds", utility = ds_qalys)
  leaf_node_rdt_pos_experienced_gx_true_pos_mdr <- LeafNode$new("nLRDT Positive, Experienced, GX True Positive mdr", utility = mdr_qalys)
  leaf_node_rdt_pos_experienced_gx_false_pos <- LeafNode$new("nLRDT Positive, Experienced, GX False Positive")
  leaf_node_rdt_pos_experienced_gx_true_neg <- LeafNode$new("nLRDT Positive, Experienced, GX True Negative")
  chance_node_rdt_pos_experienced_gx_false_neg <- ChanceNode$new("nLRDT Positive, Experienced, GX False Negative")
  leaf_node_rdt_pos_experienced_gx_false_neg_ds <- LeafNode$new("nLRDT Positive, Experienced, GX False Negative ds", utility = fn_ds_qalys)
  leaf_node_rdt_pos_experienced_gx_false_neg_mdr <- LeafNode$new("nLRDT Positive, Experienced, GX False Negative mdr", utility = fn_mdr_qalys)
  chance_node_rdt_neg_further_gx_true_pos <- ChanceNode$new("nLRDT Negative, Further GX investigation True Positive")
  leaf_node_rdt_neg_further_gx_true_pos_ds <- LeafNode$new("nLRDT Negative, Further GX investigation True Positive ds", utility = ds_qalys)
  leaf_node_rdt_neg_further_gx_true_pos_mdr <- LeafNode$new("nLRDT Negative, Further GX investigation True Positive mdr", utility = mdr_qalys)
  leaf_node_rdt_neg_further_gx_false_pos <- LeafNode$new("nLRDT Negative, Further GX investigation False Positive")
  leaf_node_rdt_neg_further_gx_true_neg <- LeafNode$new("nLRDT Negative, Further GX investigation True Negative")
  chance_node_rdt_neg_further_gx_false_neg <- ChanceNode$new("nLRDT Negative, Further GX investigation False Negative")
  leaf_node_rdt_neg_further_gx_false_neg_ds <- LeafNode$new("nLRDT Negative, Further GX investigation False Negative ds", utility = fn_ds_qalys)
  leaf_node_rdt_neg_further_gx_false_neg_mdr <- LeafNode$new("nLRDT Negative, Further GX investigation False Negative mdr", utility = fn_mdr_qalys)
  leaf_node_rdt_neg_no_further_gx_true_neg <- LeafNode$new("nLRDT Negative, No Further investigation, True Negative")
  chance_node_rdt_neg_no_further_gx_false_neg <- ChanceNode$new("nLRDT Negative, No Further GX investigation False Negative")
  leaf_node_rdt_neg_no_further_gx_false_neg_ds <- LeafNode$new("nLRDT Negative, No Further GX investigation False Negative ds", utility = fn_ds_qalys)
  leaf_node_rdt_neg_no_further_gx_false_neg_mdr <- LeafNode$new("nLRDT Negative, No Further GX investigation False Negative mdr", utility = fn_mdr_qalys)
  
  # Actions and Reactions 
  
  # world without
  action_gx <- Action$new(decision_node, chance_node_gx, cost = gx_pathway_cost_societal, label = "GX")
  reaction_1 <- Reaction$new(chance_node_gx, 
                             chance_node_gx_true_pos, p = edge_1_p, 
                             cost = 0.0, label = "True Positive GX"
  )
  reaction_2 <- Reaction$new(chance_node_gx_true_pos, 
                             leaf_node_gx_true_pos_ds, p = edge_2_p, 
                             cost = total_costs_ds_treatment_no_delay, label = "True Positive GX - ds"
  )
  reaction_3 <- Reaction$new(chance_node_gx_true_pos, 
                             leaf_node_gx_true_pos_mdr, p = edge_3_p, 
                             cost = cost_mdr_societal, label = "True Positive GX - mdr"
  )
  reaction_4 <- Reaction$new(chance_node_gx, 
                             leaf_node_gx_false_pos, p = edge_4_p, 
                             cost = cost_ds_societal, label = "False Positive GX"
  )
  reaction_5 <- Reaction$new(chance_node_gx, 
                             leaf_node_gx_true_neg, p = edge_5_p, 
                             cost = 0.0, label = "True Negative GX"
  )
  reaction_6 <- Reaction$new(chance_node_gx, 
                             chance_node_gx_false_neg, p = edge_6_p, 
                             cost = 0.0, label = "False Negative GX"
  )
  reaction_7 <- Reaction$new(chance_node_gx_false_neg, 
                             leaf_node_gx_false_neg_ds, p = edge_7_p, 
                             cost = total_costs_ds_treatment_delay, label = "False Negative GX - ds"
  )
  reaction_8 <- Reaction$new(chance_node_gx_false_neg, 
                             leaf_node_gx_false_neg_mdr, p = edge_8_p, 
                             cost = cost_mdr_societal, label = "False Negative GX - mdr"
  )
  
  # world with
  action_rdt <- Action$new(decision_node, chance_node_rdt, cost = rdt_pathway_cost_societal, label = "nLRDT") 
  reaction_9 <- Reaction$new(chance_node_rdt, 
                             leaf_node_rdt_pos_naive_true_pos, p = edge_9_p, 
                             cost = total_costs_ds_treatment_no_delay, label = "Positive nLRDT, naive true pos"
  )
  reaction_10 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_pos_naive_false_pos, p = edge_10_p, 
                              cost = cost_ds_societal, label = "Positive nLRDT, naive false pos"
  )
  reaction_11 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_pos_experienced_gx_true_pos, p = edge_11_p, 
                              cost = 0.0, label = "Positive nLRDT, experienced -> GX true pos"
  )
  reaction_12 <- Reaction$new(chance_node_rdt_pos_experienced_gx_true_pos , 
                              leaf_node_rdt_pos_experienced_gx_true_pos_ds, p = edge_12_p, 
                              cost = total_costs_ds_treatment_no_delay, label = "Positive nLRDT, experienced -> GX true pos ds"
  )
  reaction_13 <- Reaction$new(chance_node_rdt_pos_experienced_gx_true_pos , 
                              leaf_node_rdt_pos_experienced_gx_true_pos_mdr, p = edge_13_p, 
                              cost = cost_mdr_societal, label = "Positive nLRDT, experienced -> GX true pos mdr"
  )
  reaction_14 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_pos_experienced_gx_false_pos, p = edge_14_p, 
                              cost = cost_ds_societal, label = "Positive nLRDT, experienced -> GX false pos"
  )
  reaction_15 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_pos_experienced_gx_true_neg, p = edge_15_p, 
                              cost = 0.0, label = "Positive nLRDT, experienced -> GX true neg"
  )
  reaction_16 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_pos_experienced_gx_false_neg, p = edge_16_p, 
                              cost = 0.0, label = "Positive nLRDT, experienced -> GX false neg"
  )
  reaction_17 <- Reaction$new(chance_node_rdt_pos_experienced_gx_false_neg,
                              leaf_node_rdt_pos_experienced_gx_false_neg_ds, p = edge_17_p, 
                              cost = total_costs_ds_treatment_delay, label = "Positive nLRDT, experienced -> GX false neg ds"
  )
  reaction_18 <- Reaction$new(chance_node_rdt_pos_experienced_gx_false_neg,
                              leaf_node_rdt_pos_experienced_gx_false_neg_mdr, p = edge_18_p, 
                              cost = cost_mdr_societal, label = "Positive nLRDT, experienced -> GX false neg mdr"
  )
  reaction_19 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_neg_further_gx_true_pos, p = edge_19_p, 
                              cost = 0.0, label = "Negative nLRDT further test GX true pos"
  )
  reaction_20 <- Reaction$new(chance_node_rdt_neg_further_gx_true_pos, 
                              leaf_node_rdt_neg_further_gx_true_pos_ds, p = edge_20_p, 
                              cost = total_costs_ds_treatment_no_delay, label = "Negative nLRDT further test GX true pos - ds"
  )
  reaction_21 <- Reaction$new(chance_node_rdt_neg_further_gx_true_pos, 
                              leaf_node_rdt_neg_further_gx_true_pos_mdr, p = edge_21_p, 
                              cost = cost_mdr_societal, label = "Negative nLRDT further test GX true pos - mdr"
  )
  reaction_22 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_neg_further_gx_false_pos, p = edge_22_p, 
                              cost = cost_ds_societal, label = "Negative nLRDT further test GX false pos"
  )
  reaction_23 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_neg_further_gx_true_neg, p = edge_23_p, 
                              cost = 0.0, label = "Negative nLRDT further test GX true neg"
  )
  reaction_24 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_neg_further_gx_false_neg, p = edge_24_p, 
                              cost = 0.0, label = "Negative nLRDT further test GX false neg"
  )
  reaction_25 <- Reaction$new(chance_node_rdt_neg_further_gx_false_neg, 
                              leaf_node_rdt_neg_further_gx_false_neg_ds, p = edge_25_p, 
                              cost = total_costs_ds_treatment_delay, label = "Negative nLRDT further test GX false neg ds"
  )
  reaction_26 <- Reaction$new(chance_node_rdt_neg_further_gx_false_neg, 
                              leaf_node_rdt_neg_further_gx_false_neg_mdr, p = edge_26_p, 
                              cost = cost_mdr_societal, label = "Negative nLRDT further test GX false neg mdr"
  )
  reaction_27 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_neg_no_further_gx_true_neg, p = edge_27_p, 
                              cost = 0.0, label = "True Negative nLRDT"
  )
  reaction_28 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_neg_no_further_gx_false_neg, p = edge_28_p, 
                              cost = 0.0, label = "False Negative nLRDT"
  )
  reaction_29 <- Reaction$new(chance_node_rdt_neg_no_further_gx_false_neg, 
                              leaf_node_rdt_neg_no_further_gx_false_neg_ds, p = edge_29_p, 
                              cost = total_costs_ds_treatment_delay, label = "False Negative nLRDT ds"
  )
  reaction_30 <- Reaction$new(chance_node_rdt_neg_no_further_gx_false_neg, 
                              leaf_node_rdt_neg_no_further_gx_false_neg_mdr, p = edge_30_p, 
                              cost = cost_mdr_societal, label = "False Negative nLRDT mdr"
  )
  
  # Create tree
  
  dt <- DecisionTree$new(
    V = list(
      decision_node,
      # world without
      chance_node_gx,
      chance_node_gx_true_pos,
      leaf_node_gx_true_pos_ds,
      leaf_node_gx_true_pos_mdr,
      leaf_node_gx_false_pos,
      leaf_node_gx_true_neg,
      chance_node_gx_false_neg,
      leaf_node_gx_false_neg_ds,
      leaf_node_gx_false_neg_mdr,
      # world with 
      chance_node_rdt,
      leaf_node_rdt_pos_naive_true_pos,
      leaf_node_rdt_pos_naive_false_pos,
      chance_node_rdt_pos_experienced_gx_true_pos,
      leaf_node_rdt_pos_experienced_gx_true_pos_ds,
      leaf_node_rdt_pos_experienced_gx_true_pos_mdr,
      leaf_node_rdt_pos_experienced_gx_false_pos,
      leaf_node_rdt_pos_experienced_gx_true_neg,
      chance_node_rdt_pos_experienced_gx_false_neg,
      leaf_node_rdt_pos_experienced_gx_false_neg_ds,
      leaf_node_rdt_pos_experienced_gx_false_neg_mdr,
      chance_node_rdt_neg_further_gx_true_pos,
      leaf_node_rdt_neg_further_gx_true_pos_ds,
      leaf_node_rdt_neg_further_gx_true_pos_mdr,
      leaf_node_rdt_neg_further_gx_false_pos,
      leaf_node_rdt_neg_further_gx_true_neg,
      chance_node_rdt_neg_further_gx_false_neg,
      leaf_node_rdt_neg_further_gx_false_neg_ds,
      leaf_node_rdt_neg_further_gx_false_neg_mdr,
      leaf_node_rdt_neg_no_further_gx_true_neg,
      chance_node_rdt_neg_no_further_gx_false_neg,
      leaf_node_rdt_neg_no_further_gx_false_neg_ds,
      leaf_node_rdt_neg_no_further_gx_false_neg_mdr
    ),
    E = list(
      # world without
      action_gx,
      reaction_1,
      reaction_2,
      reaction_3,
      reaction_4,
      reaction_5,
      reaction_6,
      reaction_7,
      reaction_8,
      # world with
      action_rdt,
      reaction_9,
      reaction_10,
      reaction_11,
      reaction_12,
      reaction_13,
      reaction_14,
      reaction_15,
      reaction_16,
      reaction_17,
      reaction_18,
      reaction_19,
      reaction_20,
      reaction_21,
      reaction_22,
      reaction_23,
      reaction_24,
      reaction_25,
      reaction_26,
      reaction_27,
      reaction_28,
      reaction_29,
      reaction_30
    )
  )
  
  # Defining output of function
  results <- dt$evaluate(setvars = "expected")
  
  path <- dt$evaluate(by = "path")
  
  incremental_qalys <- (results[2,7] - results[1,7])
  incremental_costs <- (results[2,4] - results[1,4])
  
  ICER <- (results[2,4] - results[1,4]) /
    (results[2,7] - results[1,7])
  
  NMB <- ((results[2,7] - results[1,7]) * 1357.47) - (results[2,4] - results[1,4])
  
  output <- list(results = results, 
                 path = path, 
                 incrementsl_qalys = incremental_qalys,
                 incremental_costs = incremental_costs,
                 ICER = ICER, 
                 NMB = NMB)
  
  return(output)
}

# Scenario Analysis - Varying Sens and Spec of nLRDT (Societal Perspective)

sensitivity_values_sputum <- c(0.6, 0.7, 0.8, 0.9,  0.7,  0.9)
specificity_values_sputum <- c(0.9, 0.8, 0.7, 0.6, 0.98, 0.98)

for (i in 1:length(sensitivity_values_sputum)) {
  model_variables_societal$sens_rdt <- ConstModVar$new("", "", sensitivity_values_sputum[i])
  model_variables_societal$spec_rdt <- ConstModVar$new("", "", specificity_values_sputum[i])
  output <- run_decision_model_scenario_2(model_variables_societal)
  print(output)
}

# Scenario 2: Varying Sens and Spec of nLRDT (Provider Perspective)

for (i in 1:length(sensitivity_values_sputum)) {
  model_variables_provider$sens_rdt <- ConstModVar$new("", "", sensitivity_values_sputum[i])
  model_variables_provider$spec_rdt <- ConstModVar$new("", "", specificity_values_sputum[i])
  output <- run_decision_model_scenario_2(model_variables_provider)
  print(output)
}

#
#
#
#
#

# TORNADO PLOT (where QALY parameters are treated collecticely)

# Here, must reset model variables from societal and provider perspective.
# Therefore, run lines 13 and 47 again.


# Creating function to calculate changes in Net Monetary Benefit (NMB) as a function of the variable list, and its respective base case.
nmb_results <- function(variable_list, base_case_result) {
  
  # Nested function to calculate the changes in NMB when varying a parameter
  calculate_changes <- function(model_variables, param_name, base_case_result) {
    # Store the original value of the parameter
    original_value <- model_variables[[param_name]]
    
    # Get the lower and upper quantiles (2.5% and 97.5%) of the parameter
    lower_quantile <- model_variables[[param_name]]$quantile(0.025)
    upper_quantile <- model_variables[[param_name]]$quantile(0.975)
    
    # Assign the lower quantile value as the 'constant' value of the parameter
    model_variables[[param_name]] <- ConstModVar$new("", "", lower_quantile)
    # Run standard model with the new constant (lower estimate) value, all other parameters with their mean value.
    lower_result <- run_decision_model(model_variables)
    # Calculate the change in the NMB, which is an output of the model
    lower_change <- lower_result$NMB - base_case_result$NMB
    
    # Resetting the parameter to its original value
    model_variables[[param_name]] <- original_value
    
    # Same as above, but for upper estimate 
    model_variables[[param_name]] <- ConstModVar$new("", "", upper_quantile)
    upper_result <- run_decision_model(model_variables)
    upper_change <- upper_result$NMB - base_case_result$NMB
    
    # Reset the parameter to its original value
    model_variables[[param_name]] <- original_value
    
    # Return the changes in NMB for the lower and upper bounds
    return(list(Lower = lower_change, Upper = upper_change))
  }
  
  # Names of all parameters to be analyzed
  param_names <- names(variable_list)
  
  # Creating data frame to store the results of NMB changes
  nmb_results <- data.frame(Parameter = param_names, 
                            Lower_NMB_Change = NA, 
                            Upper_NMB_Change = NA)
  
  # Loop through all parameters, calculating NMB changes for lower and upper estimates
  for (param_name in param_names) {
    changes <- calculate_changes(variable_list, param_name, base_case_result)
    nmb_results[nmb_results$Parameter == param_name, ]$Lower_NMB_Change <- changes$Lower
    nmb_results[nmb_results$Parameter == param_name, ]$Upper_NMB_Change <- changes$Upper
    nmb_results[nmb_results$Parameter == param_name, ]$Change <- changes$Upper - changes$Lower
  }
  
  # Return the data frame with NMB changes for all parameters
  return(nmb_results)
}

# Running NMB changes data frame for both perspectives, relative to their respective base cases.
nmb_results_societal <- nmb_results(model_variables_societal, base_case_societal)
nmb_results_provider <- nmb_results(model_variables_provider, base_case_provider)

# We do this for ALL parameters, however we want to vary the QALY-parameters simultaneously
# We remove the four QALY parameters from the NMB results dataframe
qaly_params <- c("ds_qalys", "mdr_qalys", "fn_ds_qalys", "fn_mdr_qalys")
nmb_results_societal_without_qalys <- nmb_results_societal %>%
  filter(!(Parameter %in% qaly_params))
nmb_results_provider_without_qalys <- nmb_results_provider %>%
  filter(!(Parameter %in% qaly_params))

# Societal Perspective
# Calculate lower and upper estimates for these parameters
ds_qalys_lower <- model_variables_societal$ds_qalys$quantile(0.025)
mdr_qalys_lower <- model_variables_societal$mdr_qalys$quantile(0.025)
fn_ds_qalys_lower <- model_variables_societal$fn_ds_qalys$quantile(0.025)
fn_mdr_qalys_lower <- model_variables_societal$fn_mdr_qalys$quantile(0.025)

ds_qalys_upper <- model_variables_societal$ds_qalys$quantile(0.975)
mdr_qalys_upper <- model_variables_societal$mdr_qalys$quantile(0.975)
fn_ds_qalys_upper <- model_variables_societal$fn_ds_qalys$quantile(0.975)
fn_mdr_qalys_upper <- model_variables_societal$fn_mdr_qalys$quantile(0.975)

# For societal perspective, set QALY parameters to their lower bounds and calculate NMB change
model_variables_societal$ds_qalys <- ConstModVar$new("","", ds_qalys_lower)
model_variables_societal$mdr_qalys <- ConstModVar$new("","", mdr_qalys_lower)
model_variables_societal$fn_ds_qalys <- ConstModVar$new("","", fn_ds_qalys_lower)
model_variables_societal$fn_mdr_qalys <- ConstModVar$new("","", fn_mdr_qalys_lower)
change_nmb_qalys_lower_societal <- run_decision_model(model_variables_societal)$NMB - base_case_societal$NMB

# Set QALY parameters to their upper bounds and calculate NMB change
model_variables_societal$ds_qalys <- ConstModVar$new("","", ds_qalys_upper)
model_variables_societal$mdr_qalys <- ConstModVar$new("","", mdr_qalys_upper)
model_variables_societal$fn_ds_qalys <- ConstModVar$new("","", fn_ds_qalys_upper)
model_variables_societal$fn_mdr_qalys <- ConstModVar$new("","", fn_mdr_qalys_upper)
change_nmb_qalys_upper_societal <- run_decision_model(model_variables_societal)$NMB - base_case_societal$NMB

# Store changes in a new data frame
change_nmb_societal <- data.frame(
  Parameter = "model_qalys",
  Lower_NMB_Change = change_nmb_qalys_lower_societal,  
  Upper_NMB_Change = change_nmb_qalys_upper_societal)

# Combine results without QALY parameters individually, and with QALY changes varied simultaneously 
nmb_results_societal_with_qalys <- rbind(nmb_results_societal_without_qalys, change_nmb_societal)

# Repeat for provider perspective
model_variables_provider$ds_qalys <- ConstModVar$new("","", ds_qalys_lower)
model_variables_provider$mdr_qalys <- ConstModVar$new("","", mdr_qalys_lower)
model_variables_provider$fn_ds_qalys <- ConstModVar$new("","", fn_ds_qalys_lower)
model_variables_provider$fn_mdr_qalys <- ConstModVar$new("","", fn_mdr_qalys_lower)
change_nmb_qalys_lower_provider <- run_decision_model(model_variables_provider)$NMB - base_case_provider$NMB

model_variables_provider$ds_qalys <- ConstModVar$new("","", ds_qalys_upper)
model_variables_provider$mdr_qalys <- ConstModVar$new("","", mdr_qalys_upper)
model_variables_provider$fn_ds_qalys <- ConstModVar$new("","", fn_ds_qalys_upper)
model_variables_provider$fn_mdr_qalys <- ConstModVar$new("","", fn_mdr_qalys_upper)
change_nmb_qalys_upper_provider <- run_decision_model(model_variables_provider)$NMB - base_case_provider$NMB

change_nmb_provider <- data.frame(
  Parameter = "model_qalys",
  Lower_NMB_Change = change_nmb_qalys_lower_provider,  
  Upper_NMB_Change = change_nmb_qalys_upper_provider)

nmb_results_provider_with_qalys <- rbind(nmb_results_provider_without_qalys, change_nmb_provider)

# Function to create tornado plot
tornado_plot <- function(nmb_results, plot_title, param_names_mapping) {
  
  # Map parameter codes to descriptive names, rather than their variable names in the code appearing on y-axis
  nmb_results$Parameter <- param_names_mapping[nmb_results$Parameter]
  
  # Calculate the absolute maximum change in NMB, as we want to sort by most influential parameter
  nmb_results$Max_Abs_Change <- pmax(abs(nmb_results$Lower_NMB_Change), abs(nmb_results$Upper_NMB_Change))
  
  # Sort parameters in descending order by absolute maximum change
  nmb_results <- nmb_results[order(-nmb_results$Max_Abs_Change), ]
  
  # Keep the top 8 parameters with the highest changes
  nmb_results <- head(nmb_results, 8)
  
  # Create the tornado plot with a legend for lower and upper estimates
  plot <- ggplot(nmb_results, aes(x = reorder(Parameter, Max_Abs_Change))) +
    geom_bar(aes(y = Lower_NMB_Change, fill = "Lower estimate"), 
             stat = "identity", position = position_dodge(width = 0.8), width = 0.4) +
    geom_bar(aes(y = Upper_NMB_Change, fill = "Upper estimate"), 
             stat = "identity", position = position_dodge(width = 0.8), width = 0.4) +
    coord_flip() +
    scale_fill_manual(name = "Estimate",
                      values = c("Lower estimate" = "blue", "Upper estimate" = "red")) +
    labs(title = paste("Tornado Plot -", plot_title), 
         x = "Parameter", 
         y = "Change to Base Case Net Monetary Benefit (2024 USD)") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  return(plot)
}

# Mapping parameter names to more descriptive labels for y-axis plotting
param_names_mapping_collective_qalys <- c(
  prev = "Prevalence",
  sens_gx = "Sensitivity of GeneXpert",
  spec_gx = "Specificity of GeneXpert",
  sens_rdt = "Sensitivity of nLRDT",
  spec_rdt = "Specificity of nLRDT",
  p_naive = "Proportion naive",
  phi = "Proportion negative nLRDT further investigated",
  p_resistant_in_experienced = "Proportion resistant",
  cost_ds_patient = "Patient cost of 6 month DS-TB treatment regimen",
  cost_ds_provider = "Provider cost of 6 month DS-TB treatment regimen",
  cost_mdr_patient = "Patient cost of 9 month MDR-TB treatment regimen",
  cost_mdr_provider = "Provider cost of 9 month MDR-TB treatment regimen",
  cost_diagnostic_visit_provider = "Provider cost of an outpatient diagnostic visit",
  cost_treatment_visit_provider = "Provider cost of an outpatient treatment visit",
  cost_sputum_collection_provider = "Provider cost of sputum collection",
  cost_shipping_provider = "Provider cost of shipping sputum", 
  cost_gx_test_provider = "Provider cost of GeneXpert test",
  cost_rdt_test_provider = "Provider cost of nLRDT test",
  cost_food_accom_travel_one_visit_patient = "Patient cost of food, accommodation and travel - one healthcare utilization",
  cost_income_loss_one_visit_patient = "Patient lost income cost - one healthcare utilization",
  model_qalys = "Model QALYs"
)

# Creating plots
tornado_plot_societal <- tornado_plot(nmb_results_societal_with_qalys, "Societal", param_names_mapping_collective_qalys)
tornado_plot_provider <- tornado_plot(nmb_results_provider_with_qalys, "Provider", param_names_mapping_collective_qalys)

#
#
#
#
#

# Probabilistic Sensitivity Analysis

# Create similar function to the scenario analysis previously.
# The difference is in the output.
# We evaluate 1000 random samplings of the model, where parameters are drawn from their probability distributions
# The package necessitates recreating the whole function code for this.

# Reset model variable lists for societal / provider (re-run lines 13 and 47)

psa <- function(variable_list, perspective) {
  
  prev <- variable_list$prev
  sens_gx <- variable_list$sens_gx
  spec_gx <- variable_list$spec_gx
  sens_rdt <- variable_list$sens_rdt
  spec_rdt <- variable_list$spec_rdt
  p_naive <- variable_list$p_naive
  phi <- variable_list$phi
  p_resistant_in_experienced <- variable_list$p_resistant_in_experienced
  cost_ds_patient <- variable_list$cost_ds_patient
  cost_ds_provider <- variable_list$cost_ds_provider
  cost_mdr_patient <- variable_list$cost_mdr_patient
  cost_mdr_provider <- variable_list$cost_mdr_provider
  cost_diagnostic_visit_provider <- variable_list$cost_diagnostic_visit_provider
  cost_treatment_visit_provider <- variable_list$cost_treatment_visit_provider
  cost_sputum_collection_provider <- variable_list$cost_sputum_collection_provider
  cost_shipping_provider <- variable_list$cost_shipping_provider
  cost_gx_test_provider <- variable_list$cost_gx_test_provider
  cost_rdt_test_provider <- variable_list$cost_rdt_test_provider
  cost_food_accom_travel_one_visit_patient <- variable_list$cost_food_accom_travel_one_visit_patient
  cost_income_loss_one_visit_patient <- variable_list$cost_income_loss_one_visit_patient
  ds_qalys <- variable_list$ds_qalys
  ds_costs <- variable_list$ds_costs
  mdr_qalys <- variable_list$mdr_qalys
  fn_ds_qalys <- variable_list$fn_ds_qalys
  fn_ds_costs <- variable_list$fn_ds_costs
  fn_mdr_qalys <- variable_list$fn_mdr_qalys
  ds_healthy <- variable_list$ds_healthy
  
  # Define pathways costs
  gx_pathway_cost_provider <- ExprModVar$new("GeneXpert provider pathway total cost", "", rlang::quo(
    cost_diagnostic_visit_provider +
      cost_sputum_collection_provider +
      cost_shipping_provider +
      cost_gx_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  rdt_pathway_cost_provider <- ExprModVar$new("nLRDT provider pathway total cost", "", rlang::quo(
    cost_diagnostic_visit_provider +
      cost_sputum_collection_provider +
      cost_rdt_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  gx_pathway_cost_patient <- ExprModVar$new("GeneXpert patient pathway total cost", "", rlang::quo(
    3 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  rdt_pathway_cost_patient <- ExprModVar$new("nLRDT patient pathway total cost", "", rlang::quo(
    2 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  # standard assumptions again
  
  gx_pathway_cost_societal <- ExprModVar$new("GX pathway societal total cost", "", rlang::quo(
    gx_pathway_cost_patient + gx_pathway_cost_provider), nemp = 1000L)
  
  rdt_pathway_cost_societal <- ExprModVar$new("nLRDT pathway societal total cost", "", rlang::quo(
    rdt_pathway_cost_patient + rdt_pathway_cost_provider), nemp = 1000L)
  
  # Further investigation after negative nLRDT pathway costs
  further_test_rdt_cost_provider <- ExprModVar$new("Further GX test after initial nLRDT provider cost", "", rlang::quo(
    cost_sputum_collection_provider +
      cost_shipping_provider +
      cost_gx_test_provider +
      cost_treatment_visit_provider), nemp = 1000L)
  
  further_test_rdt_cost_patient <- ExprModVar$new("Further GX test after initial nLRDT patient cost", "", rlang::quo(
    2 * (cost_income_loss_one_visit_patient + cost_food_accom_travel_one_visit_patient)), nemp = 1000L)
  
  further_test_rdt_cost_societal <- ExprModVar$new("Further GX test after initial nLRDT societal cost", "", rlang::quo(
    further_test_rdt_cost_patient + further_test_rdt_cost_provider), nemp = 1000L)
  
  # Treatment costs
  cost_ds_societal <- ExprModVar$new("Societal cost of 6 month ds-TB treatment regimen", "", rlang::quo(
    cost_ds_patient + cost_ds_provider), nemp = 1000L)
  
  cost_mdr_societal <- ExprModVar$new("Societal cost of 9 month mdr-TB treatment regimen", "", rlang::quo(
    cost_mdr_patient + cost_mdr_provider), nemp = 1000L)
  
  # Total costs for ds-TB treatments
  total_costs_ds_treatment_no_delay <- ExprModVar$new("Total cost of 6 month ds, no delay (including costs from Markov Model Failures)", "", rlang::quo(
    cost_ds_societal + ds_costs), nemp = 1000L)
  total_costs_ds_treatment_delay <- ExprModVar$new("Total cost of 6 month ds, including delay for false negatives (including costs from Markov Model Failures)", "", rlang::quo(
    cost_ds_societal + fn_ds_costs), nemp = 1000L)
  
  # Other Model Variables
  p_experienced <- ExprModVar$new("Proportion experienced", "", rlang::quo(1 - p_naive), nemp = 1000L)
  p_sensitive_in_experienced <- ExprModVar$new("Proportion sensitive", "", rlang::quo(1 - p_resistant_in_experienced))
  p_resistant_pooled <- ExprModVar$new("Proportion resistant pooled", "", rlang::quo(p_resistant_in_experienced * p_experienced))
  p_sensitive_pooled <- ExprModVar$new("Proportion sensitive pooled", "", rlang::quo(1 - p_resistant_pooled))
  
  # world without
  edge_1_p  <- ExprModVar$new("Probability for edge 1", "" , rlang::quo(prev * sens_gx), nemp = 1000L)
  edge_2_p  <- ExprModVar$new("Probability for edge 2", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_3_p  <- ExprModVar$new("Probability for edge 3", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  edge_4_p  <- ExprModVar$new("Probability for edge 4", "" , rlang::quo((1 - prev)*(1 - spec_gx)), nemp = 1000L)
  edge_5_p  <- ExprModVar$new("Probability for edge 5", "" , rlang::quo((1 - prev)*spec_gx), nemp = 1000L)
  edge_6_p  <- ExprModVar$new("Probability for edge 6", "" , rlang::quo(prev * (1 - sens_gx)), nemp = 1000L)
  edge_7_p  <- ExprModVar$new("Probability for edge 7", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_8_p  <- ExprModVar$new("Probability for edge 8", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  
  # world with
  edge_9_p  <- ExprModVar$new("Probability for edge 9", "" , rlang::quo(prev * p_naive * sens_rdt), nemp = 1000L)
  edge_10_p <- ExprModVar$new("Probability for edge 10", "" , rlang::quo((1 - prev) * p_naive * (1 - spec_rdt)), nemp = 1000L)
  edge_11_p <- ExprModVar$new("Probability for edge 11", "" , rlang::quo(prev * p_experienced * sens_rdt * sens_gx), nemp = 1000L)
  edge_12_p  <- ExprModVar$new("Probability for edge 12", "" , rlang::quo(p_sensitive_in_experienced), nemp = 1000L)
  edge_13_p  <- ExprModVar$new("Probability for edge 13", "" , rlang::quo(p_resistant_in_experienced), nemp = 1000L) 
  edge_14_p <- ExprModVar$new("Probability for edge 14", "" , rlang::quo((1 - prev) * p_experienced * (1 - spec_rdt) * (1 - spec_gx)), nemp = 1000L)
  edge_15_p <- ExprModVar$new("Probability for edge 15", "" , rlang::quo((1 - prev) * p_experienced * (1 - spec_rdt) * spec_gx), nemp = 1000L)
  edge_16_p <- ExprModVar$new("Probability for edge 16", "" , rlang::quo(prev * p_experienced * sens_rdt * (1 - sens_gx)), nemp = 1000L)
  edge_17_p  <- ExprModVar$new("Probability for edge 17", "" , rlang::quo(p_sensitive_in_experienced), nemp = 1000L)
  edge_18_p  <- ExprModVar$new("Probability for edge 18", "" , rlang::quo(p_resistant_in_experienced), nemp = 1000L) 
  edge_19_p  <- ExprModVar$new("Probability for edge 19", "" , rlang::quo(phi * prev * (1 - sens_rdt) * sens_gx), nemp = 1000L) 
  edge_20_p  <- ExprModVar$new("Probability for edge 20", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_21_p  <- ExprModVar$new("Probability for edge 21", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  edge_22_p  <- ExprModVar$new("Probability for edge 22", "" , rlang::quo(phi * (1 - prev) * spec_rdt * (1 - spec_gx)), nemp = 1000L) 
  edge_23_p  <- ExprModVar$new("Probability for edge 23", "" , rlang::quo(phi * (1 - prev) * spec_rdt * spec_gx), nemp = 1000L) 
  edge_24_p  <- ExprModVar$new("Probability for edge 24", "" , rlang::quo(phi * prev * (1 - sens_rdt) * (1 - sens_gx)), nemp = 1000L) 
  edge_25_p  <- ExprModVar$new("Probability for edge 25", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_26_p  <- ExprModVar$new("Probability for edge 26", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  edge_27_p  <- ExprModVar$new("Probability for edge 27", "" , rlang::quo((1 - phi) * (1 - prev) * spec_rdt), nemp = 1000L)
  edge_28_p  <- ExprModVar$new("Probability for edge 28", "" , rlang::quo((1 - phi) * prev * (1 - sens_rdt)), nemp = 1000L)
  edge_29_p  <- ExprModVar$new("Probability for edge 29", "" , rlang::quo(p_sensitive_pooled), nemp = 1000L)
  edge_30_p  <- ExprModVar$new("Probability for edge 30", "" , rlang::quo(p_resistant_pooled), nemp = 1000L)
  
  # Formulating decision tree
  
  decision_node <- DecisionNode$new(label = "Decision Node") 
  # world without
  chance_node_gx <- ChanceNode$new("Test Result")
  chance_node_gx_true_pos <- ChanceNode$new("GX True Positive")
  leaf_node_gx_true_pos_ds <- LeafNode$new("GX True Positive ds", utility = ds_qalys)
  leaf_node_gx_true_pos_mdr <- LeafNode$new("GX True Positive mdr", utility = mdr_qalys)
  leaf_node_gx_false_pos <- LeafNode$new("GX False Positive")
  leaf_node_gx_true_neg <- LeafNode$new("GX True Negative")
  chance_node_gx_false_neg <- ChanceNode$new("GX False Negative")
  leaf_node_gx_false_neg_ds <- LeafNode$new("GX False Negative ds TB", utility = fn_ds_qalys)
  leaf_node_gx_false_neg_mdr <- LeafNode$new("GX False Negative mdr TB", utility = fn_mdr_qalys)
  # world with 
  chance_node_rdt <- ChanceNode$new("Test Result")
  leaf_node_rdt_pos_naive_true_pos <- LeafNode$new("nLRDT Positive, Naive, True Positive", utility = ds_qalys)
  leaf_node_rdt_pos_naive_false_pos <- LeafNode$new("nLRDT Positive, Naive, False Positive")
  chance_node_rdt_pos_experienced_gx_true_pos <- ChanceNode$new("nLRDT Positive, Experienced, GX True Positive")
  leaf_node_rdt_pos_experienced_gx_true_pos_ds <- LeafNode$new("nLRDT Positive, Experienced, GX True Positive ds", utility = ds_qalys)
  leaf_node_rdt_pos_experienced_gx_true_pos_mdr <- LeafNode$new("nLRDT Positive, Experienced, GX True Positive mdr", utility = mdr_qalys)
  leaf_node_rdt_pos_experienced_gx_false_pos <- LeafNode$new("nLRDT Positive, Experienced, GX False Positive")
  leaf_node_rdt_pos_experienced_gx_true_neg <- LeafNode$new("nLRDT Positive, Experienced, GX True Negative")
  chance_node_rdt_pos_experienced_gx_false_neg <- ChanceNode$new("nLRDT Positive, Experienced, GX False Negative")
  leaf_node_rdt_pos_experienced_gx_false_neg_ds <- LeafNode$new("nLRDT Positive, Experienced, GX False Negative ds", utility = fn_ds_qalys)
  leaf_node_rdt_pos_experienced_gx_false_neg_mdr <- LeafNode$new("nLRDT Positive, Experienced, GX False Negative mdr", utility = fn_mdr_qalys)
  chance_node_rdt_neg_further_gx_true_pos <- ChanceNode$new("nLRDT Negative, Further GX investigation True Positive")
  leaf_node_rdt_neg_further_gx_true_pos_ds <- LeafNode$new("nLRDT Negative, Further GX investigation True Positive ds", utility = ds_qalys)
  leaf_node_rdt_neg_further_gx_true_pos_mdr <- LeafNode$new("nLRDT Negative, Further GX investigation True Positive mdr", utility = mdr_qalys)
  leaf_node_rdt_neg_further_gx_false_pos <- LeafNode$new("nLRDT Negative, Further GX investigation False Positive")
  leaf_node_rdt_neg_further_gx_true_neg <- LeafNode$new("nLRDT Negative, Further GX investigation True Negative")
  chance_node_rdt_neg_further_gx_false_neg <- ChanceNode$new("nLRDT Negative, Further GX investigation False Negative")
  leaf_node_rdt_neg_further_gx_false_neg_ds <- LeafNode$new("nLRDT Negative, Further GX investigation False Negative ds", utility = fn_ds_qalys)
  leaf_node_rdt_neg_further_gx_false_neg_mdr <- LeafNode$new("nLRDT Negative, Further GX investigation False Negative mdr", utility = fn_mdr_qalys)
  leaf_node_rdt_neg_no_further_gx_true_neg <- LeafNode$new("nLRDT Negative, No Further investigation, True Negative")
  chance_node_rdt_neg_no_further_gx_false_neg <- ChanceNode$new("nLRDT Negative, No Further GX investigation False Negative")
  leaf_node_rdt_neg_no_further_gx_false_neg_ds <- LeafNode$new("nLRDT Negative, No Further GX investigation False Negative ds", utility = fn_ds_qalys)
  leaf_node_rdt_neg_no_further_gx_false_neg_mdr <- LeafNode$new("nLRDT Negative, No Further GX investigation False Negative mdr", utility = fn_mdr_qalys)
  
  # Actions and Reactions 
  
  # world without
  action_gx <- Action$new(decision_node, chance_node_gx, cost = gx_pathway_cost_societal, label = "GX")
  reaction_1 <- Reaction$new(chance_node_gx, 
                             chance_node_gx_true_pos, p = edge_1_p, 
                             cost = 0.0, label = "True Positive GX"
  )
  reaction_2 <- Reaction$new(chance_node_gx_true_pos, 
                             leaf_node_gx_true_pos_ds, p = edge_2_p, 
                             cost = total_costs_ds_treatment_no_delay, label = "True Positive GX - ds"
  )
  reaction_3 <- Reaction$new(chance_node_gx_true_pos, 
                             leaf_node_gx_true_pos_mdr, p = edge_3_p, 
                             cost = cost_mdr_societal, label = "True Positive GX - mdr"
  )
  reaction_4 <- Reaction$new(chance_node_gx, 
                             leaf_node_gx_false_pos, p = edge_4_p, 
                             cost = cost_ds_societal, label = "False Positive GX"
  )
  reaction_5 <- Reaction$new(chance_node_gx, 
                             leaf_node_gx_true_neg, p = edge_5_p, 
                             cost = 0.0, label = "True Negative GX"
  )
  reaction_6 <- Reaction$new(chance_node_gx, 
                             chance_node_gx_false_neg, p = edge_6_p, 
                             cost = 0.0, label = "False Negative GX"
  )
  reaction_7 <- Reaction$new(chance_node_gx_false_neg, 
                             leaf_node_gx_false_neg_ds, p = edge_7_p, 
                             cost = total_costs_ds_treatment_delay, label = "False Negative GX - ds"
  )
  reaction_8 <- Reaction$new(chance_node_gx_false_neg, 
                             leaf_node_gx_false_neg_mdr, p = edge_8_p, 
                             cost = cost_mdr_societal, label = "False Negative GX - mdr"
  )
  
  # world with
  action_rdt <- Action$new(decision_node, chance_node_rdt, cost = rdt_pathway_cost_societal, label = "nLRDT") 
  reaction_9 <- Reaction$new(chance_node_rdt, 
                             leaf_node_rdt_pos_naive_true_pos, p = edge_9_p, 
                             cost = total_costs_ds_treatment_no_delay, label = "Positive nLRDT, naive true pos"
  )
  reaction_10 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_pos_naive_false_pos, p = edge_10_p, 
                              cost = cost_ds_societal, label = "Positive nLRDT, naive false pos"
  )
  reaction_11 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_pos_experienced_gx_true_pos, p = edge_11_p, 
                              cost = 0.0, label = "Positive nLRDT, experienced -> GX true pos"
  )
  reaction_12 <- Reaction$new(chance_node_rdt_pos_experienced_gx_true_pos , 
                              leaf_node_rdt_pos_experienced_gx_true_pos_ds, p = edge_12_p, 
                              cost = total_costs_ds_treatment_no_delay, label = "Positive nLRDT, experienced -> GX true pos ds"
  )
  reaction_13 <- Reaction$new(chance_node_rdt_pos_experienced_gx_true_pos , 
                              leaf_node_rdt_pos_experienced_gx_true_pos_mdr, p = edge_13_p, 
                              cost = cost_mdr_societal, label = "Positive nLRDT, experienced -> GX true pos mdr"
  )
  reaction_14 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_pos_experienced_gx_false_pos, p = edge_14_p, 
                              cost = cost_ds_societal, label = "Positive nLRDT, experienced -> GX false pos"
  )
  reaction_15 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_pos_experienced_gx_true_neg, p = edge_15_p, 
                              cost = 0.0, label = "Positive nLRDT, experienced -> GX true neg"
  )
  reaction_16 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_pos_experienced_gx_false_neg, p = edge_16_p, 
                              cost = 0.0, label = "Positive nLRDT, experienced -> GX false neg"
  )
  reaction_17 <- Reaction$new(chance_node_rdt_pos_experienced_gx_false_neg,
                              leaf_node_rdt_pos_experienced_gx_false_neg_ds, p = edge_17_p, 
                              cost = total_costs_ds_treatment_delay, label = "Positive nLRDT, experienced -> GX false neg ds"
  )
  reaction_18 <- Reaction$new(chance_node_rdt_pos_experienced_gx_false_neg,
                              leaf_node_rdt_pos_experienced_gx_false_neg_mdr, p = edge_18_p, 
                              cost = cost_mdr_societal, label = "Positive nLRDT, experienced -> GX false neg mdr"
  )
  reaction_19 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_neg_further_gx_true_pos, p = edge_19_p, 
                              cost = 0.0, label = "Negative nLRDT further test GX true pos"
  )
  reaction_20 <- Reaction$new(chance_node_rdt_neg_further_gx_true_pos, 
                              leaf_node_rdt_neg_further_gx_true_pos_ds, p = edge_20_p, 
                              cost = total_costs_ds_treatment_no_delay, label = "Negative nLRDT further test GX true pos - ds"
  )
  reaction_21 <- Reaction$new(chance_node_rdt_neg_further_gx_true_pos, 
                              leaf_node_rdt_neg_further_gx_true_pos_mdr, p = edge_21_p, 
                              cost = cost_mdr_societal, label = "Negative nLRDT further test GX true pos - mdr"
  )
  reaction_22 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_neg_further_gx_false_pos, p = edge_22_p, 
                              cost = cost_ds_societal, label = "Negative nLRDT further test GX false pos"
  )
  reaction_23 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_neg_further_gx_true_neg, p = edge_23_p, 
                              cost = 0.0, label = "Negative nLRDT further test GX true neg"
  )
  reaction_24 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_neg_further_gx_false_neg, p = edge_24_p, 
                              cost = 0.0, label = "Negative nLRDT further test GX false neg"
  )
  reaction_25 <- Reaction$new(chance_node_rdt_neg_further_gx_false_neg, 
                              leaf_node_rdt_neg_further_gx_false_neg_ds, p = edge_25_p, 
                              cost = total_costs_ds_treatment_delay, label = "Negative nLRDT further test GX false neg ds"
  )
  reaction_26 <- Reaction$new(chance_node_rdt_neg_further_gx_false_neg, 
                              leaf_node_rdt_neg_further_gx_false_neg_mdr, p = edge_26_p, 
                              cost = cost_mdr_societal, label = "Negative nLRDT further test GX false neg mdr"
  )
  reaction_27 <- Reaction$new(chance_node_rdt, 
                              leaf_node_rdt_neg_no_further_gx_true_neg, p = edge_27_p, 
                              cost = 0.0, label = "True Negative nLRDT"
  )
  reaction_28 <- Reaction$new(chance_node_rdt, 
                              chance_node_rdt_neg_no_further_gx_false_neg, p = edge_28_p, 
                              cost = 0.0, label = "False Negative nLRDT"
  )
  reaction_29 <- Reaction$new(chance_node_rdt_neg_no_further_gx_false_neg, 
                              leaf_node_rdt_neg_no_further_gx_false_neg_ds, p = edge_29_p, 
                              cost = total_costs_ds_treatment_delay, label = "False Negative nLRDT ds"
  )
  reaction_30 <- Reaction$new(chance_node_rdt_neg_no_further_gx_false_neg, 
                              leaf_node_rdt_neg_no_further_gx_false_neg_mdr, p = edge_30_p, 
                              cost = cost_mdr_societal, label = "False Negative nLRDT mdr"
  )
  
  # Create tree
  
  dt <- DecisionTree$new(
    V = list(
      decision_node,
      # world without
      chance_node_gx,
      chance_node_gx_true_pos,
      leaf_node_gx_true_pos_ds,
      leaf_node_gx_true_pos_mdr,
      leaf_node_gx_false_pos,
      leaf_node_gx_true_neg,
      chance_node_gx_false_neg,
      leaf_node_gx_false_neg_ds,
      leaf_node_gx_false_neg_mdr,
      # world with 
      chance_node_rdt,
      leaf_node_rdt_pos_naive_true_pos,
      leaf_node_rdt_pos_naive_false_pos,
      chance_node_rdt_pos_experienced_gx_true_pos,
      leaf_node_rdt_pos_experienced_gx_true_pos_ds,
      leaf_node_rdt_pos_experienced_gx_true_pos_mdr,
      leaf_node_rdt_pos_experienced_gx_false_pos,
      leaf_node_rdt_pos_experienced_gx_true_neg,
      chance_node_rdt_pos_experienced_gx_false_neg,
      leaf_node_rdt_pos_experienced_gx_false_neg_ds,
      leaf_node_rdt_pos_experienced_gx_false_neg_mdr,
      chance_node_rdt_neg_further_gx_true_pos,
      leaf_node_rdt_neg_further_gx_true_pos_ds,
      leaf_node_rdt_neg_further_gx_true_pos_mdr,
      leaf_node_rdt_neg_further_gx_false_pos,
      leaf_node_rdt_neg_further_gx_true_neg,
      chance_node_rdt_neg_further_gx_false_neg,
      leaf_node_rdt_neg_further_gx_false_neg_ds,
      leaf_node_rdt_neg_further_gx_false_neg_mdr,
      leaf_node_rdt_neg_no_further_gx_true_neg,
      chance_node_rdt_neg_no_further_gx_false_neg,
      leaf_node_rdt_neg_no_further_gx_false_neg_ds,
      leaf_node_rdt_neg_no_further_gx_false_neg_mdr
    ),
    E = list(
      # world without
      action_gx,
      reaction_1,
      reaction_2,
      reaction_3,
      reaction_4,
      reaction_5,
      reaction_6,
      reaction_7,
      reaction_8,
      # world with
      action_rdt,
      reaction_9,
      reaction_10,
      reaction_11,
      reaction_12,
      reaction_13,
      reaction_14,
      reaction_15,
      reaction_16,
      reaction_17,
      reaction_18,
      reaction_19,
      reaction_20,
      reaction_21,
      reaction_22,
      reaction_23,
      reaction_24,
      reaction_25,
      reaction_26,
      reaction_27,
      reaction_28,
      reaction_29,
      reaction_30
    )
  )
  
  # Simulate 1000 Monte Carlo observations
  results <- dt$evaluate(setvars = "random", N = 1000L)
  
  # Initialize a data frame to store incremental QALYs and costs for each run
  incremental_results <- data.frame(
    Run = integer(),
    Incremental_QALYs = numeric(),
    Incremental_Costs = numeric()
  )
  
  # Calculating costs and QALYs for each simulation
  for (i in seq(1, nrow(results), by = 2)) {
    gx_result <- results[i, ]
    rdt_result <- results[i + 1, ]
    incremental_qalys <- rdt_result$QALY - gx_result$QALY
    incremental_costs <- rdt_result$Cost - gx_result$Cost
    # storing in created dataframe
    incremental_results <- rbind(incremental_results, data.frame(
      Run = gx_result$Run,
      Incremental_QALYs = incremental_qalys,
      Incremental_Costs = incremental_costs
    ))
  }
  # calculating mean of Incremental QALYs and costs for visualisation purposes
  mean_result <- data.frame(
    Incremental_QALYs = mean(incremental_results$Incremental_QALYs),
    Incremental_Costs = mean(incremental_results$Incremental_Costs)
  )
  # Creating plot on the cost-effectiveness plane
  psa_plot <- ggplot(incremental_results, aes(x = Incremental_QALYs, y = Incremental_Costs)) +
    geom_point(color = "blue") +
    geom_point(data = mean_result, aes(x = Incremental_QALYs, y = Incremental_Costs), color = "orange", size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    # adding wtp threshold estimate for philippines
    geom_abline(slope = 1357.47, intercept = 0, linetype = "solid", color = "red", linewidth = 1) +
    labs(title = paste("Probabilistic Sensitivity Analysis -", perspective, "Perspective"), 
         x = "Incremental QALYs", y = "Incremental costs (2024 USD)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    # setting reasonable, fixed limits for societal/provider comparison
    xlim(-0.125, 0.125) +
    ylim(-50, 50)
  
# Creating cost-effectiveness acceptability curve plot
  # Define WTP thresholds to evaluate at
  wtp_thresholds <- seq(0, 10000, by = 100)
  
  # Creating data frame to store probabilities
  ce_probabilities <- data.frame(
    WTP = wtp_thresholds,
    Probability_GX = numeric(length(wtp_thresholds)),
    Probability_nLRDT = numeric(length(wtp_thresholds))
  )
  
  # Loop through each WTP thresholds
  for (i in seq_along(wtp_thresholds)) {
    wtp <- wtp_thresholds[i]
    
    # Calculate Net Monetary Benefit (NMB) for each run at each threshold
    incremental_results$NMB <- (incremental_results$Incremental_QALYs * wtp) - (incremental_results$Incremental_Costs)
    
    # Calculate the proportion of runs where nLRDT is cost-effective (NMB >0)
    ce_probabilities$Probability_nLRDT[i] <- mean(incremental_results$NMB > 0)
    
    # Calculate the proportion of runs where GX is cost-effective, 1 - above
    ce_probabilities$Probability_GX[i] <- 1 - ce_probabilities$Probability_nLRDT[i]
  }
  
  # Creating plot
  ce_plot <- ggplot(ce_probabilities, aes(x = WTP)) +
    geom_line(aes(y = Probability_nLRDT, color = "nLRDT")) +
    geom_line(aes(y = Probability_GX, color = "GeneXpert")) +
    labs(x = "Willingness-to-Pay Threshold (2024 USD)", y = "Probability Cost-Effective", 
         title = paste("Cost-Effectiveness Acceptability Curve -", perspective, "Perspective")) +
    scale_color_manual(name = "Intervention", values = c("nLRDT" = "blue", "GeneXpert" = "green")) +
    theme_minimal() +
    # Adding vertical lines showing estimated WTP threshold for the philippines, and 10% of this value either side
    geom_vline(xintercept = 1357.47, linetype = "solid", color = "red") +
    geom_vline(xintercept = 1357.47 * 1.1, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 1357.47 * 0.9, linetype = "dotted", color = "red") +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 14)  # Increase legend text size
    )
  
  # Output of function
  output <- list(
    results = results,
    incremental_results = incremental_results,
    psa_plot = psa_plot,
    ce_plot = ce_plot,
    ce_probabilities = ce_probabilities
  )
  
  return(output)
}

psa_societal <- psa(model_variables_societal, "Societal")
psa_provider <- psa(model_variables_provider, "Provider")

#
#
#
#
#

#MVA 

# Reset model variable lists for societal / provider (re-run lines 13 and 47)

# Define the range of parameter values of interest
prev_range <- seq(0.1, 0.4, by = 0.1)
sens_rdt_range <- seq(0.6, 1, by = 0.1)
spec_rdt_range <- seq(0.8, 1, by = 0.1)
p_naive_range <- seq(0.6, 1, by = 0.1)

# Create a matrix with all combinations of parameter values
mva_combinations <- expand.grid(prev = prev_range, 
                                sens_rdt = sens_rdt_range, 
                                spec_rdt = spec_rdt_range, 
                                p_naive = p_naive_range)

# Initialize an empty data frame to store the results
mva_results <- data.frame(prev = numeric(),
                          sens_rdt = numeric(),
                          spec_rdt = numeric(),
                          p_naive = numeric(),
                          nmb_societal = numeric(),
                          nmb_provider = numeric())

# Function to run the model and calculate NMB, a function of each combination of the parameter set and the variable list (societal/provider)
mva_run <- function(row, variable_list) {
  
  # Parameter values of current row
  prev_val <- row$prev
  sens_rdt_val <- row$sens_rdt
  spec_rdt_val <- row$spec_rdt
  p_naive_val <- row$p_naive
  
  # Copy the variable list
  model_variables <- variable_list
  
  # Assign constant parameter values from the current row
  model_variables$prev <- ConstModVar$new("Prevalence", '', prev_val)
  model_variables$sens_rdt <- ConstModVar$new("Sensitivity of nLRDT", '', sens_rdt_val)
  model_variables$spec_rdt <- ConstModVar$new("Specificity of nLRDT", '', spec_rdt_val)
  model_variables$p_naive <- ConstModVar$new("Proportion naive", '', p_naive_val)
  
  # Run the model with these parameter values values (standard model)
  results <- run_decision_model(model_variables)
  
  # Take net monetary benefit from model output
  return(results$NMB)
}

# Iterate over all parameter value combinations in the matrix
for (i in 1:nrow(mva_combinations)) {
  row <- mva_combinations[i, ]
  
  # Run the above function from both societal and provider perspectives
  nmb_societal <- mva_run(row, mva_list_societal)
  nmb_provider <- mva_run(row, mva_list_provider)
  
  # Add the results to the data frame, one row for each row in the combination matrix
  mva_results <- rbind(mva_results, cbind(row, nmb_societal, nmb_provider))
}

# converting the results to long format 
mva_results_long <- mva_results %>%
  pivot_longer(cols = c(nmb_societal, nmb_provider), names_to = "Perspective", values_to = "NMB")

# prrepare data for the societal perspective plot, filtering for societal only
societal_data <- mva_results_long %>%
  filter(Perspective == "nmb_societal") %>%
  pivot_longer(cols = c(prev, sens_rdt, p_naive, spec_rdt), names_to = "Parameter", values_to = "Value") %>%
  # Creating new column with actual parameter names.
  mutate(parameter_group = case_when(
    str_detect(Parameter, "prev") ~ "Prevalence",
    str_detect(Parameter, "sens_rdt") ~ "Sensitivity nLRDT",
    str_detect(Parameter, "spec_rdt") ~ "Specificity nLRDT",
    str_detect(Parameter, "p_naive") ~ "Proportion Naive"
  ))

# Reorder factor levels 
societal_data$parameter_group <- factor(societal_data$parameter_group, levels = c("Prevalence", "Sensitivity nLRDT", "Specificity nLRDT", "Proportion Naive"))

# Create the societal perspective plot
mva_societal_plot <- ggplot(societal_data, aes(x = NMB, y = factor(Value), fill = parameter_group)) +
  geom_boxplot() +
  facet_grid(parameter_group ~ ., scales = "free_y", space = "free") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better readability
    strip.background = element_blank(),  # Remove background from strip labels
    strip.placement = "outside"  # Place strip labels outside the plot area
  ) +
  # adding labels for the title and axes
  labs(
    title = "Multivariate Sensitivity Analysis - Societal Perspective",  # Title of the plot
    x = "Net Monetary Benefit (2024 USD)",  # Label for x-axis
    y = "Parameter Values"  # Label for y-axis
  ) +
  # setting y-axis to 1 decimal place
  scale_y_discrete(labels = function(x) sprintf("%.1f", as.numeric(x))) +
  scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
  scale_fill_manual(values = c(
    "Prevalence" = "blue",  # Blue for Prevalence
    "Sensitivity nLRDT" = "red",  # Red for Sensitivity nLRDT
    "Specificity nLRDT" = "green",  # Green for Specificity nLRDT
    "Proportion Naive" = "purple"  # Purple for Proportion Naive
  ))

# Repeat for provider perspective
provider_data <- mva_results_long %>%
  filter(Perspective == "nmb_provider") %>%
  pivot_longer(cols = c(prev, sens_rdt, p_naive, spec_rdt), names_to = "Parameter", values_to = "Value") %>%
  mutate(parameter_group = case_when(
    str_detect(Parameter, "prev") ~ "Prevalence",
    str_detect(Parameter, "sens_rdt") ~ "Sensitivity nLRDT",
    str_detect(Parameter, "spec_rdt") ~ "Specificity nLRDT",
    str_detect(Parameter, "p_naive") ~ "Proportion Naive"
  ))

# Reorder factor levels for better visualization
provider_data$parameter_group <- factor(provider_data$parameter_group, levels = c("Prevalence", "Sensitivity nLRDT", "Specificity nLRDT", "Proportion Naive"))

# Create the provider perspective plot
mva_provider_plot <- ggplot(provider_data, aes(x = NMB, y = factor(Value), fill = parameter_group)) +
  geom_boxplot() +
  facet_grid(parameter_group ~ ., scales = "free_y", space = "free") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  labs(
    title = "Multivariate Sensitivity Analysis - Provider Perspective",
    x = "Net Monetary Benefit (2024 USD)",
    y = "Parameter Values"
  ) +
  scale_y_discrete(labels = function(x) sprintf("%.1f", as.numeric(x))) +
  scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
  scale_fill_manual(values = c("Prevalence" = "blue", "Sensitivity nLRDT" = "red", "Specificity nLRDT" = "green", "Proportion Naive" = "purple"))

# Create plots
mva_societal_plot
mva_provider_plot

# Further analysis, want to facet for different values of the specificity. 

# Create a function that filters the long data for a fixed spec value, societal.
facet_societal_plot <- function(spec_value) {
  mva_results_long_fixed_spec_societal <- mva_results_long %>%
    filter(Perspective == "nmb_societal", spec_rdt == spec_value) %>%
    pivot_longer(cols = c(prev, sens_rdt, p_naive), names_to = "Parameter", values_to = "Value") %>%
    mutate(parameter_group = case_when(
      str_detect(Parameter, "prev") ~ "Prevalence",
      str_detect(Parameter, "sens_rdt") ~ "Sensitivity nLRDT",
      str_detect(Parameter, "p_naive") ~ "Proportion Naive"
    ))
  
  # Reorder factors as before for better comparison
  mva_results_long_fixed_spec_societal$parameter_group <- factor(mva_results_long_fixed_spec_societal$parameter_group, levels = c("Prevalence", "Sensitivity nLRDT", "Proportion Naive"))
  
  plot <- ggplot(mva_results_long_fixed_spec_societal, aes(x = NMB, y = factor(Value), fill = parameter_group)) +
    geom_boxplot() +
    facet_grid(parameter_group ~ ., scales = "free_y", space = "free") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14), # Increase y-axis font size
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 12, face = "bold") # Increase individual plot title font size
    ) +
    labs(
      title = paste("Fixed specificity of the nLRDT:", spec_value),
      x = "Net Monetary Benefit (2024 USD)",
      y = "Parameter Values"
    ) +
    scale_y_discrete(labels = function(x) sprintf("%.1f", as.numeric(x))) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_fill_manual(values = c("Prevalence" = "blue", "Sensitivity nLRDT" = "red", "Proportion Naive" = "purple"), name = "Parameter")
  
  return(plot)
}

# Repeat for provider

facet_provider_plot <- function(spec_value) {
  # Filter data for provider perspective with a fixed specificity value
  mva_results_long_fixed_spec_provider <- mva_results_long %>%
    filter(Perspective == "nmb_provider", spec_rdt == spec_value) %>%
    pivot_longer(cols = c(prev, sens_rdt, p_naive), names_to = "Parameter", values_to = "Value") %>%
    mutate(parameter_group = case_when(
      str_detect(Parameter, "prev") ~ "Prevalence",
      str_detect(Parameter, "sens_rdt") ~ "Sensitivity nLRDT",
      str_detect(Parameter, "p_naive") ~ "Proportion Naive"
    ))
  
  # Reorder factor levels for better visualization
  mva_results_long_fixed_spec_provider$parameter_group <- factor(mva_results_long_fixed_spec_provider$parameter_group, levels = c("Prevalence", "Sensitivity nLRDT", "Proportion Naive"))
  
  # Create the plot
  plot <- ggplot(mva_results_long_fixed_spec_provider, aes(x = NMB, y = factor(Value), fill = parameter_group)) +
    geom_boxplot() +
    facet_grid(parameter_group ~ ., scales = "free_y", space = "free") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14), # Increase y-axis font size
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 12, face = "bold") # Increase individual plot title font size
    ) +
    labs(
      title = paste("Fixed specificity of the nLRDT:", spec_value),
      x = "Net Monetary Benefit (2024 USD)",
      y = "Parameter Values"
    ) +
    scale_y_discrete(labels = function(x) sprintf("%.1f", as.numeric(x))) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    scale_fill_manual(values = c("Prevalence" = "blue", "Sensitivity nLRDT" = "red", "Proportion Naive" = "purple"), name = "Parameter")
  
  return(plot)
}

# Create plots for each value of spec (0.8, 0.9 ,1 in our case) from both perspectives
spec_values <- unique(mva_combinations$spec_rdt)
societal_plots <- lapply(spec_values, facet_societal_plot)
provider_plots <- lapply(spec_values, facet_provider_plot)

# Arrange the plots into a grid for societal perspective
grid_plot_societal <- grid.arrange(
  grobs = societal_plots,
  ncol = 1,  # Arrange plots in a single column
  top = "Multivariate Sensitivity Analysis - Comparison of Specificity nLRDT Values - Societal Perspective",
)

# Arrange the plots into a grid for provider perspective
grid_plot_provider <- grid.arrange(
  grobs = provider_plots,
  ncol = 1,  # Arrange plots in a single column
  top = "Multivariate Sensitivity Analysis - Comparison of Specificity nLRDT Values - Provider Perspective",
)
