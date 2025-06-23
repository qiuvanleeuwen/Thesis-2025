library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble) 

input_data <- read.csv("PrimingPredictors.csv")

# Define the dependent variable and predictor sets
dependent_variable <- "zRT_Priming_Effect"
all_predictors <- c(
  "RelatedCos", "DifferenceCos", "A.RelatedPrompt", "A.DifferencePrompt", "Fb.RelatedPrompt", "Fb.DifferencePrompt"
)
related_predictors <- c("RelatedCos", "A.RelatedPrompt", "Fb.RelatedPrompt")
difference_predictors <- c("DifferenceCos", "A.DifferencePrompt", "Fb.DifferencePrompt")

# Calculate descriptive statistics for the dependent variable
if (dependent_variable %in% names(input_data)) {
  desc_zRT <- input_data %>%
    summarise(
      Observations = sum(!is.na(!!sym(dependent_variable))),
      Mean = mean(!!sym(dependent_variable), na.rm = TRUE),
      SD = sd(!!sym(dependent_variable), na.rm = TRUE),
      Min = min(!!sym(dependent_variable), na.rm = TRUE),
      Max = max(!!sym(dependent_variable), na.rm = TRUE)
    )
  if(nrow(desc_zRT) > 0){
    print(paste0("For ", dependent_variable, ": Mean = ", round(desc_zRT$Mean, 2),
                 ", SD = ", round(desc_zRT$SD, 2),
                 ", Min = ", round(desc_zRT$Min, 2),
                 ", Max = ", round(desc_zRT$Max, 2),
                 ", based on ", desc_zRT$Observations, " target words/observations."))
  }
}

# Calculate descriptive statistics for the predictors
existing_predictors <- intersect(all_predictors, names(input_data))
if (length(existing_predictors) > 0) {
  desc_predictors_table <- input_data %>%
    select(all_of(existing_predictors)) %>%
    summarise(
      across(everything(), list(Mean=~mean(.,na.rm=T), SD=~sd(.,na.rm=T), Min=~min(.,na.rm=T), Max=~max(.,na.rm=T), N=~sum(!is.na(.))), .names="{.col}__{.fn}")
    ) %>%
    pivot_longer(cols=everything(), names_to=c("Predictor", "Statistic"), names_sep="__") %>%
    pivot_wider(names_from=Statistic, values_from=value)
  print("Descriptive Statistics for Predictors:")
  print(desc_predictors_table)
}

# Create individual plots
output_individual <- "individual_scatter_plots"
if (!dir.exists(output_individual)) {dir.create(output_individual)} 
for (predictor_name in all_predictors) {
  if (predictor_name %in% names(input_data)) {
    plot_data_single <- input_data %>% select(Y = all_of(dependent_variable), X = all_of(predictor_name))
    correlation_value <- cor(plot_data_single$X, plot_data_single$Y, use = "complete.obs")
    correlation_text <- paste("Pearson correlation =", round(correlation_value, 5))
    p_individual <- ggplot(plot_data_single, aes(x = X, y = Y)) +
      geom_point(color = "blue", shape = 18, size = 2.5, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
      labs(title = paste(dependent_variable, "vs.", predictor_name), subtitle = correlation_text, x = predictor_name, y = dependent_variable) +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(file.path(output_individual, paste0("scatter_", predictor_name, ".png")), plot = p_individual, width = 6, height = 4, dpi = 300)
  }
}
print(paste("Individual scatter plots saved to:", output_individual))

# Create grouped plots
output_grouped <- "grouped_plots"
if (!dir.exists(output_grouped)) {dir.create(output_grouped)} 

create_grouped_plot <- function(data, predictors_group, group_title, filename) {
  plot_data_long <- data %>%
    select(all_of(dependent_variable), all_of(predictors_group)) %>%
    pivot_longer(cols = all_of(predictors_group), names_to = "PredictorName", values_to = "PredictorValue") %>%
    rename(OutcomeValue = !!sym(dependent_variable))
  
  p_grouped <- ggplot(plot_data_long, aes(x = PredictorValue, y = OutcomeValue)) +
    geom_point(alpha = 0.3, size = 1, color="blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +
    facet_wrap(~ PredictorName, scales = "free_x") +
    labs(title = paste(dependent_variable, "vs.", group_title, "Predictors"), x = "Predictor Value", y = dependent_variable) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), strip.text = element_text(face="bold", size=9), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(filename, plot = p_grouped, width = 8, height = 3, dpi = 300)
}

create_grouped_plot(input_data, related_predictors, "Related", file.path(output_grouped, "group_related_predictors.png"))
create_grouped_plot(input_data, difference_predictors, "Difference Score", file.path(output_grouped, "group_difference_predictors.png"))
print(paste("Grouped plots saved to:", output_grouped))

# Regression Analysis
run_and_save_regression <- function(formula_str, data, filename_prefix) {
  model_formula <- as.formula(formula_str)
  print(paste("Fitting model with formula:", formula_str))
  
  model <- lm(model_formula, data = data, na.action = na.omit)
  summary_model <- summary(model)
  
  # Print the summary to the console
  print(paste("--- Summary for model:", filename_prefix, "---"))
  print(summary_model)
  
  # Create histogram of residuals
  diag_plots_dir <- "histogram_plots"
  if (!dir.exists(diag_plots_dir)) {dir.create(diag_plots_dir)}
  residuals_df <- data.frame(Residuals = residuals(model))
  
  range_residuals <- range(residuals_df$Residuals, na.rm = TRUE)
  binwidth_val <- (range_residuals[2] - range_residuals[1]) / 30
  if(is.na(binwidth_val) || binwidth_val == 0) binwidth_val <- 0.05
  
  p_residuals_hist <- ggplot(residuals_df, aes(x = Residuals)) +
    geom_histogram(binwidth = binwidth_val, fill = "blue", color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = mean(Residuals, na.rm = TRUE)), color = "red", linetype = "dotted", linewidth = 1) +
    labs(
      title = paste("Histogram of Residuals - Model:", gsub("_", " ", filename_prefix)),
      x = "Residuals", 
      y = "Frequency"
    ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave(file.path(diag_plots_dir, paste0(filename_prefix, "_histogram.png")), plot = p_residuals_hist, width = 7, height = 5, dpi = 300)
  print(paste("Histogram for", filename_prefix, "saved to", diag_plots_dir))
}
predictors_formula_all <- paste(paste0("`", all_predictors, "`"), collapse = " + ") 
formula_str_all <- paste0("`", dependent_variable, "` ~ ", predictors_formula_all)
run_and_save_regression(formula_str_all, input_data, "regression_all_predictors")

predictors_formula_difference <- paste(paste0("`", difference_predictors, "`"), collapse = " + ")
formula_str_difference <- paste0("`", dependent_variable, "` ~ ", predictors_formula_difference)
run_and_save_regression(formula_str_difference, input_data, "regression_difference_predictors")

predictors_formula_related <- paste(paste0("`", related_predictors, "`"), collapse = " + ")
formula_str_related <- paste0("`", dependent_variable, "` ~ ", predictors_formula_related)
run_and_save_regression(formula_str_related, input_data, "regression_related_predictors")

print("All regression analyses completed.")