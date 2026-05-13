library(dplyr)
library(sf)
library(ggplot2)
library(spdep)
library(spatialreg)
library(stargazer)

#setwd(...)

# 1. load data
master_data <- readRDS("07_grid_MASTER_FOR_REGRESSION.rds")
municipality_map <- readRDS("02_municipality_map_with_income.rds")

# define target categories and their corresponding clean names for the tables
categories <- c("Entropy_Weekend", "Entropy_Evening", "Entropy_Daytime_Leisure", "Entropy_Work")
category_names <- c("Weekend Leisure", "Evening Leisure", "Daytime Leisure", "Work")

# initialize lists to store regression models
ols_models <- list()
spatial_models <- list()

# create an empty dataframe to store the diagnostic test results
diagnostic_results <- data.frame(
  Category = character(),
  Observations = integer(),
  Morans_I = numeric(),
  Morans_p = numeric(),
  LM_Error = numeric(),
  LM_Lag = numeric(),
  Robust_LM_Error = numeric(),
  Robust_LM_Lag = numeric(),
  Selected_Model = character(),
  stringsAsFactors = FALSE
)

# 2. start loop for all 4 categories
for (i in 1:length(categories)) {
  
  CURRENT_TARGET <- categories[i]
  CURRENT_NAME <- category_names[i]
  
  print(paste("PROCESSING:", CURRENT_NAME))
  
  # filter data (keep only cells with valid entropy data, i.e., 15+ visitors)
  model_data <- master_data %>%
    filter(!is.na(!!sym(CURRENT_TARGET))) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  n_obs <- nrow(model_data)
  print(paste("Number of valid grid cells in model:", n_obs))
  
  # generate and save spatial coverage map
  coverage_map <- ggplot() +
    geom_sf(data = municipality_map, fill = "gray95", color = "white", linewidth = 0.2) +
    geom_sf(data = model_data, color = "steelblue", size = 1.2, alpha = 0.8) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank(), 
          axis.title = element_blank(), plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    labs(title = paste("Spatial Coverage:", CURRENT_NAME),
         subtitle = paste("Valid grid cells: n =", n_obs))
  
  ggsave(paste0("08_coverage_", CURRENT_TARGET, ".png"), coverage_map, width = 10, height = 8, bg = "white")
  
  # construct the regression formula dynamically
  formula_str <- paste(CURRENT_TARGET, "~ Velocity_Score_Smoothed + Livability_Score_Smoothed + Attractivity_Score_Smoothed + AAQI_Score")
  model_formula <- as.formula(formula_str)
  
  # run standard OLS model and save to list
  ols_model <- lm(model_formula, data = model_data)
  ols_models[[CURRENT_NAME]] <- ols_model
  
  # construct spatial weights matrix (800m distance band)
  model_data_projected <- st_transform(model_data, 3857) # pseudo-mercator for meter-based distance
  coords_proj <- st_coordinates(model_data_projected)
  nb_dist <- dnearneigh(coords_proj, d1 = 0, d2 = 800)
  listw_dist <- nb2listw(nb_dist, style = "B", zero.policy = TRUE)
  
  # run spatial diagnostics tests
  moran_test <- lm.morantest(ols_model, listw_dist, zero.policy = TRUE)
  lm_tests <- lm.RStests(ols_model, listw_dist, test = c("RSerr", "RSlag", "adjRSerr", "adjRSlag"), zero.policy = TRUE)
  
  # extract test statistics for the diagnostic table
  m_stat <- round(moran_test$statistic, 3)
  m_p <- moran_test$p.value
  lme_stat <- round(lm_tests[["RSerr"]]$statistic, 1)
  lml_stat <- round(lm_tests[["RSlag"]]$statistic, 1)
  rlme_stat <- round(lm_tests[["adjRSerr"]]$statistic, 1)
  rlml_stat <- round(lm_tests[["adjRSlag"]]$statistic, 1)
  
  # dynamic model selection based on temporal category
  if (CURRENT_TARGET == "Entropy_Work") {
    selected_mod <- "SLM"
    sp_model <- lagsarlm(model_formula, data = model_data, listw = listw_dist, method = "Matrix", zero.policy = TRUE)
  } else {
    selected_mod <- "SEM"
    sp_model <- errorsarlm(model_formula, data = model_data, listw = listw_dist, method = "Matrix", zero.policy = TRUE)
  }
  
  # save the selected spatial model to list
  spatial_models[[CURRENT_NAME]] <- sp_model
  
  # append diagnostic results to the dataframe
  diagnostic_results <- rbind(diagnostic_results, data.frame(
    Category = CURRENT_NAME,
    Observations = n_obs,
    Morans_I = m_stat,
    Morans_p = m_p,
    LM_Error = lme_stat,
    LM_Lag = lml_stat,
    Robust_LM_Error = rlme_stat,
    Robust_LM_Lag = rlml_stat,
    Selected_Model = selected_mod
  ))
}


# 3. export combined tables

# combined OLS table
stargazer(
  ols_models, 
  type = "text", 
  title = "OLS Regression Results Across All Time Periods",
  column.labels = category_names,
  covariate.labels = c("Accessibility", "Livability", "Attractivity", "Environmental Quality", "Constant"),
  out = "09_COMBINED_OLS_Results.txt"
)

# combined spatial models table (3 SEM + 1 SLM)
stargazer(
  spatial_models, 
  type = "text", 
  title = "Spatial Regression Results (SEM for Leisure, SLM for Work)",
  column.labels = category_names,
  covariate.labels = c("Accessibility", "Livability", "Attractivity", "Environmental Quality", "Constant"),
  out = "09_COMBINED_Spatial_Results.txt"
)

# export diagnostic tests table (text format for preview)
stargazer(
  diagnostic_results, 
  type = "text", 
  summary = FALSE, 
  title = "Diagnostic Tests for Spatial Dependence",
  rownames = FALSE,
  out = "09_COMBINED_Diagnostics.txt"
)

# export diagnostic tests as CSV (easier for formatting in excel/word)
write.csv(diagnostic_results, "09_COMBINED_Diagnostics.csv", row.names = FALSE)

# loop through saved models and extract spatial parameters
for (name in names(spatial_models)) {
  mod_sum <- summary(spatial_models[[name]])
  
  if (name == "Work") {
    param <- mod_sum$rho
    se <- mod_sum$rho.se
    z_val <- param / se
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    cat(name, "- SLM (Rho):   \t Value:", round(param, 3), "\t Error:", round(se, 4), "\t p-value:", p_val, "\n")
  } else {
    param <- mod_sum$lambda
    se <- mod_sum$lambda.se
    z_val <- param / se
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    cat(name, "- SEM (Lambda):\t Value:", round(param, 3), "\t Error:", round(se, 4), "\t p-value:", p_val, "\n")
  }
}



# appendix: stepwise spatial models for all categories

library(dplyr)
library(sf)
library(spdep)
library(spatialreg)
library(stargazer)

print("STARTING FULL STEPWISE SPATIAL ANALYSIS")

# define categories and clean names for plotting/saving
categories <- c("Entropy_Weekend", "Entropy_Evening", "Entropy_Daytime_Leisure", "Entropy_Work")
category_names <- c("Weekend Leisure", "Evening Leisure", "Daytime Leisure", "Work")

# list to store the AIC summary dataframes
all_aic_results <- list()

# loop through all 4 categories
for (i in seq_along(categories)) {
  CURRENT_TARGET <- categories[i]
  CAT_NAME <- category_names[i]
  
  print(paste(">>> Processing:", CAT_NAME, "<<<"))
  
  # filter dataset for current category
  current_data <- master_data %>%
    filter(!is.na(!!sym(CURRENT_TARGET))) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # re-create spatial weights matrix for subset
  current_data_proj <- st_transform(current_data, 3857) 
  coords_w <- st_coordinates(current_data_proj)
  nb_w <- dnearneigh(coords_w, d1 = 0, d2 = 800)
  listw_w <- nb2listw(nb_w, style = "B", zero.policy = TRUE)
  
  # define formulas for the 4 steps
  form_1 <- as.formula(paste(CURRENT_TARGET, "~ Velocity_Score_Smoothed"))
  form_2 <- as.formula(paste(CURRENT_TARGET, "~ Velocity_Score_Smoothed + Livability_Score_Smoothed"))
  form_3 <- as.formula(paste(CURRENT_TARGET, "~ Velocity_Score_Smoothed + Livability_Score_Smoothed + Attractivity_Score_Smoothed"))
  form_4 <- as.formula(paste(CURRENT_TARGET, "~ Velocity_Score_Smoothed + Livability_Score_Smoothed + Attractivity_Score_Smoothed + AAQI_Score"))
  
  # fit models (SEM for leisure, SLM for work)
  # note: using method = "Matrix" here (can be changed to Chebyshev to drastically reduce computation time)
  if (CURRENT_TARGET == "Entropy_Work") {
    mod_type <- "SLM"
    m1 <- lagsarlm(form_1, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
    m2 <- lagsarlm(form_2, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
    m3 <- lagsarlm(form_3, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
    m4 <- lagsarlm(form_4, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
  } else {
    mod_type <- "SEM"
    m1 <- errorsarlm(form_1, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
    m2 <- errorsarlm(form_2, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
    m3 <- errorsarlm(form_3, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
    m4 <- errorsarlm(form_4, data = current_data, listw = listw_w, method = "Matrix", zero.policy = TRUE)
  }
  
  # calculate AIC improvement for console log
  aic_res <- data.frame(
    Step = c(paste("1.", mod_type, "(+ Accessibility)"), 
             paste("2.", mod_type, "(+ Livability)"), 
             paste("3.", mod_type, "(+ Attractivity)"), 
             paste("4. Full", mod_type, "(+ Environment)")),
    AIC_Value = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4))
  )
  aic_res$Improvement <- c(NA, diff(aic_res$AIC_Value))
  all_aic_results[[CAT_NAME]] <- aic_res
  
  # export full stepwise table
  out_file <- paste0("10_Appendix_Stepwise_", gsub(" ", "_", CAT_NAME), ".txt")
  stargazer(
    m1, m2, m3, m4,
    type = "text",
    title = paste("Appendix Table: Stepwise Spatial Regression -", CAT_NAME, "(", mod_type, ")"),
    column.labels = c("Step 1", "Step 2", "Step 3", "Full Model"),
    covariate.labels = c("Accessibility", "Livability", "Attractivity", "Environmental Quality", "Constant"),
    out = out_file
  )
}

# print summary
print(all_aic_results)
print("Check your working directory for the 4 Appendix .txt files!")