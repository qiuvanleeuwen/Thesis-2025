library(dplyr)
library(tidyr)

calculate_predictors_from_average <- function(input_avg_csv_filename, output_predictors_csv_filename, analysis_type_label) {
  
print(paste("--- Starten:", toupper(analysis_type_label), "Predictor calculation ---"))
print(paste("   Input average results file:", input_avg_csv_filename))
print(paste("   Output predictors file:", output_predictors_csv_filename))
  
average_results_data <- read.csv(input_avg_csv_filename)
average_results_sep <- average_results_data %>%
  separate(pair, into = c("cue", "target"), sep = "-", remove = FALSE, extra = "merge", fill = "right") %>%
  filter(!is.na(target) & target != "")

# Calculate Cosine Similarity Difference Score & Only Related
cosine_components <- average_results_sep %>%
  filter(type %in% c("related", "unrelated"), !is.na(mean_cosine_similarity)) %>%
  select(target, type, mean_cosine_similarity) %>%
  pivot_wider(names_from = type, values_from = mean_cosine_similarity, names_prefix = "cosine_") %>%
  mutate(diff_score_cosine = cosine_related - cosine_unrelated) %>%
  select(target, diff_score_cosine, cosine_related, cosine_unrelated) %>%
  filter(!is.na(diff_score_cosine))
  
# Calculate Prompt Similarity Difference Score & Only Related
api_components <- average_results_sep %>%
  filter(type %in% c("related", "unrelated"), !is.na(mean_prompt_similarity)) %>%
  select(target, type, mean_prompt_similarity) %>%
  pivot_wider(names_from = type, values_from = mean_prompt_similarity, names_prefix = "prompt_") %>%
  mutate(diff_score_api = prompt_related - prompt_unrelated) %>%
  select(target, diff_score_api, prompt_related, prompt_unrelated) %>%
  filter(!is.na(diff_score_api))
  
# Add Scores
final_data_prep <- average_results_sep
final_data_with_scores <- left_join(final_data_prep, cosine_components, by = "target")
final_data_with_scores <- left_join(final_data_with_scores, api_components, by = "target")
  
# Create columns
predictors_final_df <- final_data_with_scores %>%
  mutate(
    Only_Related_cosine = ifelse(type == "related", mean_cosine_similarity, NA_real_),
    DifferenceScore_cosine = diff_score_cosine,
    Related_score_cosine = cosine_related,
    Unrelated_score_cosine = cosine_unrelated,
    Only_Related_Prompt = ifelse(type == "related", mean_prompt_similarity, NA_real_),
    DifferenceScore_Prompt = diff_score_api,
    Related_score_Prompt = prompt_related,
    Unrelated_score_Prompt = prompt_unrelated
  ) %>%
  select(
    pair,
    type,
    Only_Related_cosine,
    DifferenceScore_cosine,
    Related_score_cosine,
    Unrelated_score_cosine,
    Only_Related_Prompt,
    DifferenceScore_Prompt,
    Related_score_Prompt,
    Unrelated_score_Prompt
  )
  
# Save results
write.csv(predictors_final_df, output_predictors_csv_filename, row.names = FALSE)
} 

# ASSOCIATION data
calculate_predictors_from_average(
  input_avg_csv_filename = "average_results_association.csv",
  output_predictors_csv_filename = "predictors_priming_association.csv",
  analysis_type_label = "Association"
)

# FEATURE-BASED data
calculate_predictors_from_average(
  input_avg_csv_filename = "average_results_feature_based.csv",
  output_predictors_csv_filename = "predictors_priming_feature_based.csv",
  analysis_type_label = "Feature-Based"
)