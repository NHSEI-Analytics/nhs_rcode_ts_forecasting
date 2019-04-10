
######################################################### ACTIVITY PLANNING FORECASTING CODE #######################################
### NHSI Information and Analytics December 2018
### SINGLE ORGANISATION FORECASTER
### DERIVED FROM a simplified version of prov_jar_v8.r (National code to run all organisations in 1 go)
### Monthly Activity forecasts based on monthly historical data
### Running time: 
############################################################

start_time <- Sys.time() 
cat(paste("This is the single trust tool: ", Sys.time(), "\n"))  

### Users will need to make sure the below packages are installed
### Imports ###
suppressPackageStartupMessages(library("readxl")) 
suppressPackageStartupMessages(library("xts")) 
suppressPackageStartupMessages(library("forecast")) 
suppressPackageStartupMessages(library("lubridate")) 
suppressPackageStartupMessages(library("stringr")) 
suppressPackageStartupMessages(library("tidyr")) 
suppressPackageStartupMessages(library("dplyr")) 
suppressPackageStartupMessages(library("ggplot2")) 

############################################################## PARAMETERS ##########################################################

### Users will need to set to local working directories, below are examples
### remember to replace back slashes with forward slashes  

path_target <- "Z:/Planning/Forecasting Approach/Forecast Distributed/Final Organisational level code for circulation"

### different folders can be set up and applied here
path_data <- path_target
path_output <- path_target

### Input Data File, example used below ### 
file_data <- "sample_data.csv"

### if tuncate is TRUE then only use historical data in the  
### "months_to_use" parameter otherwise use all history 
truncate_ts <- FALSE 
months_to_use <- 36 # 36 is the min as the arimas suffer with less 

### set to TRUE to test oos models on best MAPE 
### FALSE to score on RMSE 
score_on_mape <- TRUE 

### how far out to forecast, 18 is used on M6 data for planning deadline 14/01/19 
forecast_horiz_months <- 12 
### months to hold out as a test set, recommended as 12 for monthly seasonality
hold_out_months <- 12 
### and the forecast frequency 
forecast_frequency <- 12 

### save the outputs as csv 
save_csv <- TRUE 

### Generate Plots 
plot_forecasts <- TRUE

########################################################## FUNCTIONS TO RUN ########################################################

fun_trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
  return(x)
} 

fun_fc_model_is <- function(x_vals, x_dates, modl, horiz, freq) { 
  modl = modl[1] 
  ts_this = xts(x = x_vals, order.by = x_dates)
  if (modl == "ARIMA") {
    this_fit = auto.arima(ts_this) 
    fc_model = forecast(this_fit, h = horiz)
  } else if(modl == "ETS") {
    this_fit = ets(ts_this) 
    fc_model = forecast(this_fit, h = horiz)
  } else if(modl == "TBATS") {
    ts_this = ts(x_vals, start = c(year(min(x_dates)), month(min(x_dates))), frequency = freq) 
    this_fit = tbats(ts_this) 
    fc_model = forecast(this_fit, h = horiz)
  } else if(modl == "XREG") {
    this_fit = Arima(ts_this, order = c(0, 0, 0),  seasonal = c(0, 0, 0), xreg = fourier(x = ts_this, K = 3))
    fc_model =  forecast(this_fit, xreg = fourier(x = ts_this, K = 3, h = horiz), h = horiz) 
  } 
  acc_model = accuracy(fc_model)
  fc_dates = tail(seq(max(x_dates), by = "month", length = horiz + 1), horiz) 
  df_model = data.frame(metric_month = x_dates, metric_value = x_vals, metric_type = "data", 
                        lo80 = NA, up80 = NA, lo95 = NA, up95 = NA, model = NA, stringsAsFactors = FALSE)
  df_fc = data.frame(metric_month = fc_dates, metric_value = unclass(fc_model$mean), metric_type = "fc",  
                     lo80 = unclass(fc_model$lower)[,1], up80 = unclass(fc_model$upper)[,1], 
                     lo95 = unclass(fc_model$lower)[,2], up95 = unclass(fc_model$upper)[,2], 
                     model = NA, stringsAsFactors = FALSE) 
  df_model=bind_rows(df_model, df_fc) 
  df_model$model=as.character(this_fit)
  df_model$rmse = acc_model["Training set", "RMSE"]
  df_model$mape = acc_model["Training set", "MAPE"]  
  return(df_model)
}

fun_fc_model_oos <- function(x_vals, x_dates, modl, hold_out, freq) { 
  x_train = head(x_vals, length(x_vals) - hold_out) 
  x_test = tail(x_vals, hold_out) 
  x_train_dates = head(x_dates, length(x_dates) -  hold_out) 
  x_test_dates = tail(x_dates, hold_out) 
  ts_train = xts(x = x_train, order.by = x_train_dates) 
  ts_test = xts(x = x_test, order.by = x_test_dates)
  if (modl == "ARIMA") {
    this_fit = auto.arima(ts_train) 
    fc_model = forecast(this_fit, h = hold_out) 
  } else if(modl == "ETS") {
    this_fit = ets(ts_train) 
    fc_model = forecast(this_fit, h = hold_out) 
  } else if(modl == "TBATS") { 
    date_train_start = c(year(min(x_train_dates)), month(min(x_train_dates))) 
    ts_train = ts(x_train, start = date_train_start, frequency = freq)
    this_fit = suppressWarnings(tbats(ts_train)) ### suppress warning: optim() did not converge
    fc_model = forecast(this_fit, h = hold_out) 
  } else if(modl == "XREG") {
    this_fit = Arima(ts_train, order = c(0, 0, 0),  seasonal = c(0, 0, 0), xreg = fourier(x = ts_train, K = 3))
    fc_model =  forecast(this_fit, xreg = fourier(x = ts_test, K = 3, h = hold_out), h = hold_out) 
  } 
  
  acc_model = accuracy(fc_model, x_test) 
  df_train = data.frame(metric_month = x_train_dates, actual_value = x_train, fc_value = NA, 
                        lo80 = NA,up80 = NA,lo95 = NA,up95 = NA, model = NA, stringsAsFactors = FALSE)
  df_test = data.frame(metric_month = x_test_dates, actual_value = x_test, fc_value = unclass(fc_model$mean),  
                       lo80 = unclass(fc_model$lower)[,1], up80 = unclass(fc_model$upper)[,1],
                       lo95 = unclass(fc_model$lower)[,2], up95 = unclass(fc_model$upper)[,2], 
                       model = NA, stringsAsFactors = FALSE) 
  df_model = bind_rows(df_train, df_test) 
  df_model$model = as.character(this_fit)
  df_model$rmse = acc_model["Test set", "RMSE"]
  df_model$mape = acc_model["Test set", "MAPE"] 
  return(df_model)
} 

fun_plot_fc <- function(df_data, this_page, this_pages) {
  fc_cols <- c("data" = "black", "fc" = "orangered")
  plot_fc <- ggplot(data = df_data, aes(x = metric_month)) +
    geom_line(aes(y = metric_value, group = metric_type, color = metric_type), size = 1.1, linetype = "solid") +
    scale_color_manual(values = fc_cols) +
    geom_ribbon(aes(ymin = lo80, ymax = up80, group = 1), fill = "steelblue", alpha = 0.150, na.rm = TRUE) +
    geom_ribbon(aes(ymin = lo95, ymax = up95, group = 1), fill = "steelblue", alpha = 0.150, na.rm = TRUE) +
    scale_x_date(date_breaks = "3 month", date_minor_breaks = "3 month", date_labels = "%Y %B") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    theme(legend.position = "none") + 
    facet_wrap(~metric_name, ncol = 2, scale = "free") + 
    labs(title = "Forecast Charts",
         subtitle = paste0("Page ", this_page, " of ", this_pages, "."), 
         caption = "source: Supplied csv",
         x = "Date", y = "Activity")
  return(plot_fc)
} 

########################################################### DATA PREPARATION #######################################################
### Fetch source data linkins to initial pathways set at the beginning
df_import_data <- read.csv(file = paste(path_data, file_data, sep = "/"), header = TRUE, stringsAsFactors = FALSE,
                          colClasses = c("character", "character", "numeric")) 
names(df_import_data) <- c("metric_month", "metric_name", "metric_value") 


date_test <- as.numeric(substr(df_import_data$metric_month[1], 1, 2)) 
if(date_test > 2) {
  df_import_data$metric_month <- as.Date(df_import_data$metric_month) 
} else {
  df_import_data$metric_month <- as.Date(df_import_data$metric_month, "%d/%m/%Y") 
}

# df_import_data <- left_join(df_import_data, df_podrefs, by = "metric_name") %>% filter(!is.na(pod_type))

###################################################### Cross Join Safety Net #######################################################

date_end <- max(df_import_data$metric_month) 

if (truncate_ts) { 
  date_start <- date_end - months(months_to_use - 1)
} else {
  date_start <- min(df_import_data$metric_month)
}
date_range <- seq(date_start, date_end, by="month") 
periods <- length(date_range) 

metric_names <- unique(df_import_data$metric_name)

df_data <- expand.grid(metric_name = metric_names, 
                   metric_month = date_range, 
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) 

df_data <- left_join(df_data, df_import_data, by = c("metric_name", "metric_month")) 


df_allzeros_id <- df_data %>% group_by(metric_name) %>% 
  summarise(total = sum(metric_value)) %>% ungroup() %>% filter(total == 0) 

### Filter IN
df_fc_as_zeros <- df_data %>% filter(metric_name %in% df_allzeros_id$metric_name) 

### Filter OUT
df_fc_data <- df_data %>% filter(!(metric_name %in% df_allzeros_id$metric_name)) 

###################################################### FORECAST ####################################################################
################## Forecast on training Data then score on the held out test set, ############
################## Use the best scoring model on the entire dataset #########################

cat("\n", "Forecasting Arima","\n")
df_fc_bymetric1 <- df_fc_data  %>% group_by(metric_name) %>%
  do(data.frame(as.list(fun_fc_model_oos(.$metric_value, .$metric_month, modl = "ARIMA", 
                                         hold_out = hold_out_months, freq = forecast_frequency)), 
                check.names = TRUE, stringsAsFactors = FALSE)) %>% ungroup()  

cat("\n", "Forecasting ETS","\n")
df_fc_bymetric2 <- df_fc_data %>% group_by(metric_name) %>%
  do(data.frame(as.list(fun_fc_model_oos(.$metric_value, .$metric_month, modl = "ETS", 
                                         hold_out = hold_out_months, freq = forecast_frequency)), 
                check.names = FALSE, stringsAsFactors = FALSE)) %>% ungroup()  

cat("\n", "Forecasting TBATS","\n")
df_fc_bymetric3 <- df_fc_data %>% group_by(metric_name) %>%
  do(data.frame(as.list(fun_fc_model_oos(.$metric_value, .$metric_month, modl = "TBATS", 
                                         hold_out = hold_out_months, freq = forecast_frequency)), 
                check.names = FALSE, stringsAsFactors = FALSE)) %>% ungroup() 

df_fc_oos <- bind_rows(df_fc_bymetric1, df_fc_bymetric2, df_fc_bymetric3)
rm(df_fc_bymetric1, df_fc_bymetric2, df_fc_bymetric3)

df_fc_oos <- df_fc_oos %>% mutate(model_type = str_extract(model, "^[^\\(]+"), 
                                  model_type = fun_trim(model_type), 
                                  model_type = if_else(model_type == " BATS", "TBATS", model_type))

################################### Score the Models and Select the Least Worst with the Minimum OOS Error #########################

if (score_on_mape) {
  df_scored <- df_fc_oos %>% group_by(metric_name) %>% slice(which.min(mape)) %>% ungroup() %>% 
    select(metric_name, best_model = model, best_fit = mape)
} else {
  df_scored <- df_fc_oos %>% group_by(metric_name) %>% slice(which.min(rmse)) %>% ungroup() %>% 
    select(metric_name, best_model = model, best_fit = rmse)
}

df_scored <- df_scored %>%  mutate(best_model_type = str_extract(best_model,"^[^\\(]+"), 
         best_model_type = fun_trim(best_model_type), 
         best_model_type = if_else(best_model_type == "BATS", "TBATS",best_model_type)) %>% 
  arrange(desc(best_fit)) %>% mutate(best_fit = round(best_fit, 2))

df_fc_data <- left_join(df_fc_data, df_scored, by = c("metric_name")) 

########################## generate the final forecast based on the OOS winner using the whole data set ############################

df_fc_final <- df_fc_data  %>% group_by(metric_name) %>%
  do(data.frame(as.list(fun_fc_model_is(.$metric_value, .$metric_month, modl = .$best_model_type, 
                                        horiz = forecast_horiz_months, 
                                        freq = forecast_frequency)), 
                check.names = TRUE, stringsAsFactors = FALSE)) %>% ungroup() 

df_fc_final <-  df_fc_final %>% mutate(model_type = str_extract(model,"^[^\\(]+"), 
                                       model_type = fun_trim(model_type), 
                                       model_type = if_else(model_type=="BATS", "TBATS", model_type))
 
df_final_score <- df_fc_final %>% group_by(metric_name) %>% filter(row_number() == 1) %>% ungroup() %>% 
  select(metric_name, model_type, model, rmse, mape) %>% mutate(mape = round(mape, 2), rmse = round(rmse, 2))

### Rounding and setting negs to zero 
df_fc_final <- df_fc_final %>% mutate(metric_value = round(metric_value, 0), 
                                      lo80 = round(lo80, 0), up80 = round(up80, 0), lo95 = round(lo95, 0), up95 = round(up95, 0),
                                      lo80 = ifelse(lo80 < 0, 0, lo80), lo95 = ifelse(lo95 < 0, 0, lo95), 
                                      mape = round(mape, 3), rmse = round(rmse, 3))

if (save_csv) { ### Outputs

  write.csv(df_fc_final, ### the output from the Final Forecast
            paste(path_output, paste0(format(Sys.time(), "%Y%m%d_%H_%M_%S"), "_", "final_fc_DATA_SINGLE.csv"), sep = "/"), 
            row.names = FALSE) 
  
  write.csv(df_final_score, ### the output of the scores
            paste(path_output, paste0(format(Sys.time(), "%Y%m%d_%H_%M_%S"), "_", "final_fc_SCORES_SINGLE.csv"), sep = "/"), 
            row.names = FALSE) 
} 

View(df_final_score) 

print_pods <- c(unique(df_fc_final$metric_name), rep(NA, 16))
if(plot_forecasts) {  
  page_no <- 1 
  pages_all <- ceiling(nrow(df_final_score) / 4)
  for(i in seq(from = 1, to = nrow(df_final_score), by = 4)) {
    print_batch <- print_pods[i:(i + 3)]
    df_fc_printers <- df_fc_final[df_fc_final$metric_name %in% print_batch, ]
    print(fun_plot_fc(df_fc_printers, page_no, pages_all))
    page_no = page_no + 1 
  }  
} 

cat("\n", "Done, Output Data Saved in", path_output, ". Run time:", format(Sys.time() - start_time, digits = 4), "\n") 





