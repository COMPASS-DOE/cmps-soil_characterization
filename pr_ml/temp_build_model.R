

source("2-code/0-packages.R")

require(pacman)
p_load(janitor, #I compulsively use clean_names()... sorry to add dependencie
       ranger, #actual RF algorithm
       PNWcolors, #change to your preferred color scheme, just using something familiar
       cowplot,
       tidymodels) #make modeling tidy again

df_raw <- read_csv("1-data/processed/chemistry_combined_PCA.csv") %>% 
  clean_names() %>% 
  mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1)) %>% 
  dplyr::select(-c(sample_label, tree_number)) 

df <- df_raw %>% 
  drop_na()

make_rf_model <- function(var){
  
  # 1) Dynamically create formula
  formula <- as.formula(paste(var, "~ ."))
  
  # 2) Tune mtry and ntree hyperparameters
  rf_spec <- rand_forest() %>%
    set_engine("ranger", importance = "impurity") %>% #using the ranger pkg instead of rf; importance sets up for graph
    set_mode("classification")
  
  # 3) Define a recipe
  recipe <- recipe(formula, data = df) %>%
    #step_dummy(all_nominal(), -all_outcomes()) %>% # One-hot encoding was recommended. I'm not gonna lie, I don't know what this is or what it does...
    step_integer(all_nominal_predictors()) %>%
    step_corr(all_predictors()) %>%
    step_normalize(all_predictors(), -all_outcomes())
  
  # 4) Define a workflow
  wf <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(recipe)
  
  # 5) Run the  model
  rf_model <- wf %>%
    update_model(rf_spec) %>%
    fit(data = df)
  
  ## Predictions
  predictions <- predict(rf_model, df) %>% rename("predicted" = .pred_class)
  
  ### Feature Importance
  ## Set vectors for renaming stuff
  var_names <- rf_model$fit$fit$fit$variable.importance
  
  ## Convert feature importance to a tibble with variables as a column
  fi0 <- as.data.frame(var_names) %>%
    tibble::rownames_to_column() %>%
    as_tibble() %>% 
    rename("predictor" = rowname, 
           "raw_fi" = var_names) %>% 
    mutate(fi_n = raw_fi / sum(raw_fi))
  
  prediction_df <- bind_cols(df, predictions)
  
  return(list(predictions = prediction_df,
                feature_importance = fi0))
}

x <- make_rf_model("region")
#x <- make_rf_model("site")
#x <- make_rf_model("transect")

# Wrapper function for plotting
plot_rf_results <- function(var, plot_title) {
  # Call the make_rf_model function
  model_results <- make_rf_model(var)
  
  p1 <- ggplot(model_results$predictions, aes_string(x = var, y = "predicted")) + 
    geom_jitter(width = 0.2, 
                height = 0.2,
                alpha = 0.5) + 
    ggtitle(paste("Predictions:", var))
  
  # Set up a color palette
  var_colors <- PNWColors::pnw_palette("Bay", n = length(unique(model_results$feature_importance$predictor)))
  
  p2 <- ggplot(model_results$feature_importance, aes(x = fi_n * 100, 
                                                     y = reorder(predictor, fi_n), 
                                                     fill = predictor)) + 
    geom_col(alpha = 0.8, show.legend = F, width = 0.7) + 
    scale_fill_manual(values = var_colors) + 
    labs(x = "Feature Importance (%)", 
         y = "", fill = "") + 
    ggtitle(paste("Feature Imp.:", var))
  
  plot_grid(p1, p2, nrow = 1)
}

plot_rf_results("transect")







# ggplot(x$predictions, aes(region, predicted)) + 
#   geom_jitter(width = 0.2, 
#               height = 0.2,
#               alpha = 0.5)
# 
# ## Set up a color palette
# var_colors <- PNWColors::pnw_palette("Bay", n = length(unique(x$feature_importance$predictor)))
# 
# ## Make plots
# ggplot(x$feature_importance, aes(fi_n * 100, 
#                reorder(predictor, fi_n), fill = predictor)) + 
#   geom_col(alpha = 0.8, show.legend = F, width = 0.7) + 
#   scale_fill_manual(values = var_colors) + 
#   labs(x = "Feature Importance (%)", 
#        y = "", fill = "")

#interrogate_model <- function(var)