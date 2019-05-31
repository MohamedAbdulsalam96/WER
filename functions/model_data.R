library(rsample)
library(parsnip)



df <- df3 %>% 
  select(-fut5_ret, -fut21_ret, -fut63_ret, -fut10_ret, -fut_max_ret, 
         -ticker, -ticker_name, -ticker_stock_exchange, -ticker_industry, -ticker_subsector) %>% 
  filter(Index <= today() - 189) %>% #filter out the last 9 months for testing 
  filter(ema9_above_sma50 == 1) %>% 
  select(-ema9_above_sma50, -Index) %>% na.omit()

x_validation <- vfold_cv(data = df, v = 10, repeats = 1)



prep_ema9_above_sma50 <- function(mtry = 10, trees = 500, split, id){ 
  # yo is a data frame
  
  analysis_set <- analysis(split)
  assessment_set <- assessment(split)
  assessment_sett <- assessment_set %>% select(-fut_max_ret_bin)
  
  model <- rand_forest(mtry = 10, trees = 500) %>%
    set_engine("ranger", importance = 'impurity') %>%
    fit(fut_max_ret_bin ~ ., data = analysis_set)
  
  df_results <- tibble("id" = id, 
                       "truth" = assessment_set$fut_max_ret_bin, 
                       "prediction" = unlist(predict(model, new_data = assessment_sett)))
  df_results
}


results_example <- map2_df(.x = x_validation$splits, .y = x_validation$id, 
                           ~prep_ema9_above_sma50(mtry = 9, trees = 500, split = .x, id = .y))




dff <- df3 %>% 
  select(-fut5_ret, -fut21_ret, -fut63_ret, -fut10_ret, -fut_max_ret, 
         -ticker, -ticker_name, -ticker_stock_exchange, -ticker_industry, -ticker_subsector) %>% 
  filter(Index > today() - 189) %>% #filter out the last 9 months for testing 
  filter(ema9_above_sma50 == 1) %>% 
  select(-ema9_above_sma50, -Index) %>% na.omit()

yo <- unlist(predict(model, new_data = dff))
