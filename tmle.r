# Load the necessary packages
if (!require(tmle)) install.packages("tmle")
if (!require(SuperLearner)) install.packages("SuperLearner")
if (!require(SparkR)) install.packages("SparkR")

library(tmle)
library(SuperLearner)
library(SparkR)

# read the all_data_full
data_tem <- read.df("all_data_full.csv", source = "csv", inferSchema = "true", header = "true")
all_data_full <- collect(data_tem)

all_data_full$gender_b <- ifelse(all_data_full$gender == "male", 1, 0)
all_data_full$shoe_type_b <- ifelse(all_data_full$shoe_type == "Super Shoe", 1, 0)

all_data_full_q1 <- subset(all_data_full, quartile == 1)
all_data_full_q2 <- subset(all_data_full, quartile == 2)
all_data_full_q3 <- subset(all_data_full, quartile == 3)
all_data_full_q4 <- subset(all_data_full, quartile == 4)

datasets <- list(
  all_data_full = all_data_full,
  all_data_full_q1 = all_data_full_q1,
  all_data_full_q2 = all_data_full_q2,
  all_data_full_q3 = all_data_full_q3,
  all_data_full_q4 = all_data_full_q4
)

# Define the tmle function
run_tmle <- function(data) {
  SL.library <- c("SL.glm", "SL.gam")
  tt <- tmle(Y = data$speed, 
             A = data$shoe_type_b, 
             W = data %>% select(weight_kg, critical_power, grade, env_temperature, 
                                 env_humidity, env_wind_speed, env_wind_gust, env_wind_bearing, 
                                 elevation, age, gender_b),
             Q.SL.library = SL.library, 
             g.SL.library = SL.library)
  return(tt)
}

library(dplyr)

# Function to run effect modification
run_effect_modification <- function(data, dataset_name, modifier_col, modifier_bins = NULL) {
  results <- data.frame()
  
  if (!is.null(modifier_bins)) {
    data$modifier_bin <- cut(data[[modifier_col]], breaks = modifier_bins)
  } else {
    data$modifier_bin <- data[[modifier_col]]
  }
  
  for (group in unique(data$modifier_bin)) {
    subset_data <- data %>% filter(modifier_bin == group)
    
    if (nrow(subset_data) < 10) {
      cat("Skipping group", group, "in", dataset_name, "due to insufficient data\n")
      next
    }

    if (length(unique(subset_data$shoe_type_b)) < 2) {

      cat("Skipping group", group, "in", dataset_name, "due to insufficient variety in treatment variable\n")
      next
    }
    
    if (min(table(subset_data$shoe_type_b)) < 5) {
      cat(sprintf("Skipping group %s in %s due to insufficient treatment variation\n", group, dataset_name))
      next
    }

    # Run TMLE
    est <- run_tmle(subset_data)
    
    # Compute ATE and confidence intervals
    ate <- est$estimates$ATE$psi
    ate_percentage <- (ate / mean(subset_data$speed)) * 100
    ate_ci <- est$estimates$ATE$CI
    ate_ci_percentage <- (ate_ci / mean(subset_data$speed)) * 100
    
    # Store results
    results <- rbind(results, data.frame(
      dataset = dataset_name,
      modifier_col = modifier_col,
      modifier_group = group,
      ATE = ate,
      ATE_percentage = ate_percentage,
      ATE_ci_lower = ate_ci[1],
      ATE_ci_upper = ate_ci[2],
      ATE_ci_percentage_lower = ate_ci_percentage[1],
      ATE_ci_percentage_upper = ate_ci_percentage[2]
    ))
    
    cat("ATE for", modifier_col, "group", group, "in", dataset_name, ":", ate_percentage, "%\n")
  }
  
  return(results)
}

# Run effect modification for all datasets
# Initialize an empty data frame to store all results
all_results <- data.frame()

# Loop through each dataset and run effect modification
for (dataset_name in names(datasets)) {
  cat("\nRunning effect modification for", dataset_name, ":\n")
  dataset_data <- datasets[[dataset_name]]
  
  # Run effect modification for gender
  gender_results <- run_effect_modification(dataset_data, dataset_name, "gender_b")
  
  # Run for continuous variables with bins
  weight_bins <- c(40, 50, 60, 70, 80, 90)
  weight_results <- run_effect_modification(dataset_data, dataset_name, "weight_kg", weight_bins)
  
  age_bins <- c(15, 20, 30, 40, 50, 60)
  age_results <- run_effect_modification(dataset_data, dataset_name, "age", age_bins)
  
  shoe_size_bins <- c(5, 7, 9, 11, 13, 15, 17, 19)
  shoe_size_results <- run_effect_modification(dataset_data, dataset_name, "normalised_shoe_size", shoe_size_bins)
  
  intensity_bins <- c(0.4, 0.7, 1.0, 1.3)
  intensity_results <- run_effect_modification(dataset_data, dataset_name, "intensity", intensity_bins)
  
  total_power_bins <- c(100, 200, 300, 400)
  total_power_results <- run_effect_modification(dataset_data, dataset_name, "total_power", total_power_bins)
  
  heart_rate_bins <- c(100, 130, 160, 190)
  heart_rate_results <- run_effect_modification(dataset_data, dataset_name, "heart_rate", heart_rate_bins)
  
  cadence_bins <- c(140, 160, 180, 200)
  cadence_results <- run_effect_modification(dataset_data, dataset_name, "cadence", cadence_bins)
  
  stride_length_bins <- c(0.5, 1, 1.5, 2)
  stride_length_results <- run_effect_modification(dataset_data, dataset_name, "stride_length", stride_length_bins)
  
  peak_vertical_grf_bins <- c(1000, 1500, 2000, 2500)
  peak_vertical_grf_results <- run_effect_modification(dataset_data, dataset_name, "peak_vertical_grf", peak_vertical_grf_bins)
  
  leg_spring_bins <- c(5, 8, 12, 16)
  leg_spring_results <- run_effect_modification(dataset_data, dataset_name, "leg_spring", leg_spring_bins)
  
  flight_time_ms_bins <- c(0, 30, 60, 100)
  flight_time_ms_results <- run_effect_modification(dataset_data, dataset_name, "flight_time_ms", flight_time_ms_bins)
  
  ground_time_bins <- c(150, 200, 250, 300, 400)
  ground_time_results <- run_effect_modification(dataset_data, dataset_name, "ground_time", ground_time_bins)
  
  # Combine results for the current dataset
  combined_results <- rbind(gender_results, weight_results, age_results, shoe_size_results, intensity_results, 
                            total_power_results, heart_rate_results, cadence_results, stride_length_results, 
                            peak_vertical_grf_results, leg_spring_results, flight_time_ms_results, ground_time_results)
  
  # Append to all results
  all_results <- rbind(all_results, combined_results)
}

print(all_results)
# Save results to a CSV file
# write.csv(all_results, "effect_modification_results_all_data_full_tmle.csv", row.names = FALSE)