# data frame preparation -----
require(tidyverse)
require(tsibble)
require(lubridate)
require(feasts) #tsibbledata, feasts, fable
require(cowplot)
require(ggwrap)
require(fable)
require(fable.prophet)
require(fabletools)

# include Te, Tc into dataframe -----
prepare_df_fun <- function(Data_struct_fun) map(Data_struct_fun$Out_DF_list_outdoors, function (outdoor_df){
  
  outdoor_inc_tec_df <- outdoor_df %>%
    mutate(OpeMode=if_else(Cooling.capacity==0 & Heating.capacity==0 & Operation.control.mode == "Heat recovery normal control", "HeRec", OpeMode)) %>% 
    select(TimeStampPosixct, consumption = Watt.hour.amount.Corrected., ambient = Outside.air.temp., OpeMode, Operation.control.mode, UniqueIdentifier, UnitType, SystemIdentifier, ope_time_out = Outdoor.unit.operation.time, matches('^Te.mean.value|^Tc.mean.value|^Tes.mean.value|^Tcs.mean.value')) %>%   #'^T.*\\.mean\\.value' %>% 
    mutate(consumption = replace(consumption, consumption == 0, 0.015), dummy.worktime_out = if_else(ope_time_out == 0, 0, 1)) %>% 
    arrange(TimeStampPosixct)
  
  sys_ident <- unique(outdoor_inc_tec_df$SystemIdentifier)
  
  # indoor average sepotints -----
  indoors_average_values <- Data_struct_fun %>% 
    pluck("Merged_Indoor_Units") %>%
    arrange(TimeStampPosixct) %>% 
    filter(SystemIdentifier %in% sys_ident) %>% 
    select(TimeStampPosixct, Avg..setting.temp., Indoor.unit.operation.time.accumulation, Suction.air.temp., UnitType, SystemIdentifier, UniqueIdentifier) %>% 
    mutate(Suction.air.temp. = replace(Suction.air.temp., Suction.air.temp. == 0, NA)) %>% 
    group_by(UniqueIdentifier) %>% 
    fill(Avg..setting.temp., .direction = "downup") %>% 
    ungroup() %>% 
    mutate(unit_size = get_unit_size(UnitType), 
           setpoint_factor = Avg..setting.temp.*unit_size, 
           ope_time_factor = Indoor.unit.operation.time.accumulation/60 * unit_size,
           suction_factor = Suction.air.temp. * unit_size,
           suction_factor_opetime = Suction.air.temp. * unit_size * Indoor.unit.operation.time.accumulation/60) %>% 
    group_by(TimeStampPosixct) %>% 
    summarise(setpoint_factor_sum = sum(setpoint_factor, na.rm = TRUE), 
              unit_size_sum = sum(unit_size, na.rm = TRUE), 
              average_setpoint=setpoint_factor_sum/unit_size_sum,
              ope_time_factor_sum = sum(ope_time_factor, na.rm = TRUE), 
              average_ope_time = ope_time_factor_sum/unit_size_sum, 
              suction_factor_sum = sum(suction_factor, na.rm = TRUE),
              suction_factor_opetime_sum = sum(suction_factor_opetime, na.rm = TRUE),
              average_suction = suction_factor_sum/unit_size_sum,
              average_suction_opetime = if_else(ope_time_factor_sum == 0, average_suction ,suction_factor_opetime_sum/ope_time_factor_sum)) %>% 
    ungroup() %>% 
    mutate(average_suction = replace_na(average_suction, 0), dummy.worktime = if_else(average_ope_time == 0, 0, 1))%>% 
    select(-contains("sum"))
  
  total_inc_tec_tsbl <- outdoor_inc_tec_df %>% 
    left_join(indoors_average_values, by="TimeStampPosixct") %>% 
    group_by(TimeStampPosixct) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    arrange(TimeStampPosixct) %>% 
    as_tsibble(index = TimeStampPosixct, key = UniqueIdentifier) %>% 
    fill_gaps() %>% 
    fill(everything(), .direction = "downup") %>% 
    mutate(hd = hour(TimeStampPosixct), my = month(TimeStampPosixct), wd = wday(TimeStampPosixct, week_start = getOption("lubridate.week.start", 1)), OpeMode = factor(OpeMode, levels = c("Cooling", "Heating", "HeRec", "Both", "Idle"))) %>% 
    mutate(dummy.OpeMode = OpeMode)
  
  # add dummmy variables for OpeMode ---------
  # which dummy has lowest count - exclude counts = 0 and minimum
  OpeMode_counts <- table(total_inc_tec_tsbl$OpeMode)
  nonzero_counts <- OpeMode_counts[which(OpeMode_counts > 0)]
  names_to_exclude <- str_c("dummy.", ifelse("Idle" %in% names(nonzero_counts), "Idle", names(which.min(nonzero_counts))) )
  
  hourly_inc_tec_tsbl <- total_inc_tec_tsbl %>% 
    # select(TimeStampPosixct, OpeMode) %>% 
    mutate(dummy_value_1 = 1) %>% 
    pivot_wider(names_from = dummy.OpeMode, values_from = dummy_value_1, names_prefix = "dummy.", values_fill = list(dummy_value_1 = 0)) %>% 
    select(-all_of(names_to_exclude)) %>% 
    arrange(TimeStampPosixct) %>% 
    as_tsibble(key = UniqueIdentifier, index = TimeStampPosixct)

  return (hourly_inc_tec_tsbl)
})

possibly_prepare_df_fun <- possibly(prepare_df_fun, otherwise = NULL)


# correlation coefficients -------
corr_coef <- function(df) as_tibble(df) %>% 
  select_if(is.numeric) %>% 
  corrr::correlate() %>% 
  filter(rowname == "consumption") %>% 
  select_if(~any(!is.na(.)))

# add CV to the model accuracy  -------
accuracy_cv_fun <- function(outdoor_df, out_model) mutate(accuracy(out_model), mean_cons = mean(outdoor_df$consumption), CV = RMSE/mean_cons*100)

# add CV to the forecast accuracy -----
fcst_accuracy <- function(forecast_df, new_data_df) {
  accuracy_df <- accuracy(forecast_df, data = new_data_df) %>% 
    mutate(mean_cons = mean(new_data_df$consumption), CV = RMSE/mean_cons*100)
  return(accuracy_df)
}

test_df <- tibble(rowid = rep(seq(1, 12, 1), 2), suction_temp = rep(seq(12, 35, 2), 2), OpeMode = c(rep("Cooling", 12), rep("Heating", 12)), Te_target = rep(seq(-5, 17, 2), 2), Tc_target = rep(seq(33, 55, 2), 2))

# temp_setback_fun <- function(df, temperature_var, setback = 2, Tsopt_cool = 26, Tsopt_heat = 22) {
#   ret_df <- mutate(df, temperature = case_when(OpeMode == "Cooling" & is.null(Tsopt_cool) ~ (!!temperature_var) + setback))
#             # OpeMode == "Cooling" & (Tsopt_cool - temperature_var) > setback ~ temperature_var + setback,
#             # OpeMode == "Cooling" & between(Tsopt_cool - !!temperature_var, 0, setback) ~ Tsopt_cool,
#             # OpeMode == "Cooling" & (Tsopt_cool - temperature_var) < 0 ~ temperature_var,
#             # OpeMode == "Heating" & is.null(Tsopt_heat) ~ !!temperature_var - setback,
#             # OpeMode == "Heating" & (Tsopt_heat - temperature_var) < -1 * setback ~ temperature_var - setback,
#             # OpeMode == "Heating" & between(Tsopt_heat - !!temperature_var, -1 * setback, 0) ~ Tsopt_heat,
#             # OpeMode == "Heating" & (Tsopt_heat - temperature_var) > 0 ~ temperature_var,
#             # TRUE ~ (!!temperature_var)))
#   return(ret_df)
# }
# 
# # temp_setback_fun(test_df, temp)
# 
# setback_fun_vec1 <- function(OpeMode, temperature_var, setback = 2, Tsopt_cool = 26, Tsopt_heat = 22) {
#   browser()  
#   x <- case_when(OpeMode == "Cooling" & (Tsopt_cool - temperature_var) > setback ~ temperature_var + setback,
#                  OpeMode == "Cooling" & between(Tsopt_cool - temperature_var, 0, setback) ~ Tsopt_cool,
#                  OpeMode == "Cooling" & (Tsopt_cool - temperature_var) < 0 ~ temperature_var,
#                  OpeMode == "Heating" & (Tsopt_heat - temperature_var) < -1 * setback ~ temperature_var - setback,
#                  OpeMode == "Heating" & between(Tsopt_heat - temperature_var, -1 * setback, 0) ~ Tsopt_heat,
#                  OpeMode == "Heating" & (Tsopt_heat - temperature_var) > 0 ~ temperature_var,
#                  TRUE ~ temperature_var)
#     return(x)
# }

setback_fun_vec <- function(cut_dir = "up", temperature_var, setback = 2, Tsopt = 26) {
  x <- case_when(cut_dir == "up" & (Tsopt - temperature_var) > setback ~ temperature_var + setback,
                 cut_dir == "up" & between(Tsopt - temperature_var, 0, setback) ~ Tsopt,
                 cut_dir == "up" & (Tsopt - temperature_var) < 0 ~ temperature_var,
                 cut_dir == "down" & (Tsopt - temperature_var) < -1 * setback ~ temperature_var - setback,
                 cut_dir == "down" & between(Tsopt - temperature_var, -1 * setback, 0) ~ Tsopt,
                 cut_dir == "down" & (Tsopt - temperature_var) > 0 ~ temperature_var,
                 TRUE ~ temperature_var)
  return(x)
}

# mutate(test_df, temp_shift = case_when(OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = suction_temp, Tsopt = 26),
#                                  OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", setback = Inf, temperature_var = suction_temp, Tsopt = 18),
#                                  TRUE ~ suction_temp),
#        Te_shift = case_when(OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = Te_target, Tsopt = 11),
#                             TRUE  ~ Te_target),
#        Tc_shift = case_when(OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", temperature_var = Tc_target, Tsopt = 41),
#                             TRUE  ~ Tc_target)) %>% 
# ggplot(aes(x=rowid, group = OpeMode, color = OpeMode))+
#   geom_line(aes(y = suction_temp))+
#   geom_line(aes(y = temp_shift))


add_non_working_time <- function(df) {
  ret_df <- add_calendar_fields_fun(df) %>%
    mutate(non_working_time = case_when(
      workday == FALSE ~ TRUE,
      Night_Time == "nighttime" ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    select(-Day_Hour, -Night_Time, -Week_Day, -Week_Day_Name, -workday)
  return(ret_df)
}

apply_eco_shut_down_cooling <- function(df){
  if (!("non_working_time" %in% names(df))) df <- add_non_working_time(df)
  setback_df <- mutate(df, dummy.worktime_out = if_else(OpeMode == "Cooling" & non_working_time, 0, dummy.worktime_out))
  return(setback_df)
} 

apply_eco_non_work_time_setback_heating <- function(df, Tstbk_heat = 18) {
  if (!("non_working_time" %in% names(df))) df <- add_non_working_time(df)
  setback_df <- mutate(df,
    average_setpoint = if_else(OpeMode == "Heating" & non_working_time & average_setpoint > Tstbk_heat, Tstbk_heat, average_setpoint),
    average_suction = if_else(OpeMode == "Heating" & non_working_time & average_suction > Tstbk_heat, Tstbk_heat, average_suction)
  )
  return(setback_df)
}

apply_eco_shift_setpoint <- function(df, setback = 2, Tsopt_cool = 26, Tsopt_heat = 22) {
  setback_df <- mutate(df, average_setpoint = case_when(
    OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = average_setpoint, setback = setback, Tsopt = Tsopt_cool),
    OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", temperature_var = average_setpoint, setback = setback, Tsopt = Tsopt_heat),
    TRUE ~ average_setpoint
  ))
  setback_df <- mutate(setback_df, average_suction = case_when(
    OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = average_suction, setback = setback, Tsopt = Tsopt_cool),
    OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", temperature_var = average_suction, setback = setback, Tsopt = Tsopt_heat),
    TRUE ~ average_suction
  ))
  return(setback_df)
}

apply_eco_te_shift <- function(df, Te_setback = 2, Te_limit = 11) {
  setback_df <- mutate(df, Tes.mean.value = case_when(
    OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = Tes.mean.value, setback = Te_setback, Tsopt = Te_limit),
    TRUE ~ Tes.mean.value
  ))
  return(setback_df)
}

apply_eco_tc_shift <- function(df, Tc_setback = 2, Tc_limit = 41) {
  setback_df <- mutate(df, Tcs.mean.value = case_when(
    OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", temperature_var = Tcs.mean.value, setback = Tc_setback, Tsopt = Tc_limit),
    TRUE ~ Tcs.mean.value
  ))
  return(setback_df)
}

# function to create dataframe with setback
setback_fun <- function(df, suction_stbck = 2, Te_setback = 5, Tc_setback = 5, ambient_shift = 0) {
  setback_df <- df
  # setback_df <- add_calendar_fields_fun(df)
  # setback_df <- mutate(setback_df, non_working_time = case_when(
  #   workday == FALSE ~ TRUE,
  #   Night_Time == "nighttime" ~ TRUE,
  #   TRUE ~ FALSE
  # )) %>% 
  # select(-Day_Hour, -Night_Time, -Week_Day, -Week_Day_Name, -workday)
  # 
  # setback_df <- mutate(setback_df, dummy.worktime_out = if_else(OpeMode == "Cooling" & non_working_time, 0, dummy.worktime_out))

  setback_df <- mutate(setback_df, average_setpoint = case_when(
    OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = average_setpoint, setback = suction_stbck, Tsopt = Inf),
    OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", temperature_var = average_setpoint, setback = suction_stbck, Tsopt = -Inf),
    TRUE ~ average_setpoint
  ))
  setback_df <- mutate(setback_df, average_suction = case_when(
    OpeMode == "Cooling" ~ setback_fun_vec(cut_dir = "up", temperature_var = average_suction, setback = suction_stbck, Tsopt = Inf),
    OpeMode == "Heating" ~ setback_fun_vec(cut_dir = "down", temperature_var = average_suction, setback = suction_stbck, Tsopt = -Inf),
    TRUE ~ average_suction
  ))
  
    # average_suction_opetime = case_when(
    #   OpeMode == "Cooling" & is.null(Tsopt_cool) ~ average_suction_opetime + suction_stbck,
    #   OpeMode == "Heating" & is.null(Tsopt_heat) ~ average_suction_opetime - suction_stbck,
    #   TRUE ~ average_suction
    # ),
    setback_df <- mutate(setback_df, ambient = ambient + ambient_shift)
  
  # heating setback during non working times
  # if(!is.null(Tstbk_heat)) setback_df <- mutate(setback_df,
  #                      average_setpoint = if_else(OpeMode == "Heating" & non_working_time & average_setpoint > Tstbk_heat, Tstbk_heat, average_setpoint),
  #                      average_suction = if_else(OpeMode == "Heating" & non_working_time & average_suction > Tstbk_heat, Tstbk_heat, average_suction)) 
  # 
  if (Te_setback > 0) {
    setback_df <- mutate(setback_df,
      Tes.mean.value = if_else(OpeMode == "Cooling", Tes.mean.value + Te_setback, Tes.mean.value),
      Tes.mean.value = if_else(Tes.mean.value > 11, 11, Tes.mean.value),
      Te.mean.value = if_else(OpeMode == "Cooling", Te.mean.value + Te_setback, Te.mean.value),
      Te.mean.value = if_else(Te.mean.value > 11, 11, Te.mean.value)
    )
  }
  if (Tc_setback > 0) {
    setback_df <- mutate(setback_df,
      Tcs.mean.value = if_else(OpeMode == "Heating", Tcs.mean.value - Tc_setback, Tcs.mean.value),
      Tcs.mean.value = if_else(Tcs.mean.value < 41, 41, Tcs.mean.value),
      Tc.mean.value = if_else(OpeMode == "Heating", Tc.mean.value - Tc_setback, Tc.mean.value),
      Tc.mean.value = if_else(Tc.mean.value < 41, 41, Tc.mean.value)
    )
  }
  return(setback_df)
}

# function to compare real, forecast from training and setback consumption -------
compare_cons_fun <- function (training_df, forecast_real, forecast_stbck){
  ope_mode <- map_dfr(list(training_df, forecast_real, forecast_stbck), ~distinct(.x, OpeMode)) %>% 
    distinct(OpeMode) %>% 
    pull(OpeMode)
  
  real_cons_sum <- sum(training_df$consumption)
  # browser()
  comparison_df <- as_tibble(forecast_real) %>% 
    group_by(.model) %>% 
    summarise(fcst_sum = sum(consumption, na.rm = T)) %>% 
    full_join(forecast_stbck %>% 
                as_tibble() %>% 
                group_by(.model) %>% 
                summarise(fcst_setback_sum = sum(consumption, na.rm = T)) ,
              by = ".model") %>% 
    mutate(OpeMode = ope_mode, real_cons = real_cons_sum, saving_kwh = fcst_sum - fcst_setback_sum, saving_perc = saving_kwh/fcst_sum*100)
  return(comparison_df)
}


# function to rename the column for particular mode ------
rename_mode <- function(orig_name, ope_mode) {
  renamed <- str_c(orig_name, "_", str_sub(ope_mode, 1, 4))
  return(renamed)
}

# function to create the forecast dataframe per mode -------
add_mode_columns <- function (df, ope_mode, model_formula){
  # browser()
  orig_names <- names(df)
  df <- mutate(df, data = map(hourly_df, ~filter(.x, OpeMode == ope_mode & dummy.worktime == 1)))
  df <- mutate(df, corr_coef = map(data, possibly(~corr_coef(.x), otherwise = NA_real_)))
  df <- mutate(df, out_model = map(data, possibly(~model(.x, TSLM = TSLM(model_formula)), otherwise = NA_real_)))
  df <- mutate(df, regr_coef = map(out_model, possibly(~coef(.x), otherwise = NA_real_)))
  df <- mutate(df, n_rows = map_dbl(data, ~nrow(.x)))
  df <- mutate(df, mean_consumption = map_dbl(data, ~mean(.x$consumption)))
  df <- mutate(df, sum_consumption = map_dbl(data, ~sum(.x$consumption)))
  df <- mutate(df, model_accuracy = map2(data, out_model, possibly(~accuracy_cv_fun(.x, .y), otherwise = NA_real_)))
  df <- mutate(df, forecast_training = map2(out_model, data, possibly(~forecast(.x, new_data = .y, times = 100), otherwise = NA_real_)))
  df <- mutate(df, fcst_training_accuracy = map2(forecast_training, data, possibly(~fcst_accuracy(.x, .y), otherwise = NA_real_)))
  df <- mutate(df, data_stbck = map(data, setback_fun))
  df <- mutate(df, forecast_setback =map2(out_model, data_stbck, possibly(~forecast(.x, new_data = .y, times = 100), otherwise = NA_real_))) 
  df <- mutate(df, comparison = pmap(list(data, forecast_training, forecast_setback), possibly(~compare_cons_fun(..1, ..2, ..3), otherwise = NA_real_)))
  df <- rename_at(df, .vars = vars(-all_of(orig_names)), ~rename_mode(.x, ope_mode))
}

unnest_df_fun <- function (df) {
  unnest_results_df <- df %>% 
    unnest(contains("corr_coef"), names_repair = "unique", names_sep = ".") %>%
    unnest(contains("model_accuracy"), names_repair = "unique", names_sep = ".") %>% 
    unnest(contains("comparison"), names_repair = "unique", names_sep = ".") %>% 
    select_if(negate(is.list))
  return(unnest_results_df)
}

create_tbl_fun <- function (unnested_df){
  require(flextable)  
  # browser()
  header_df <- tibble(orig_names = names(unnested_df)) %>% 
    mutate(first_row = str_extract(orig_names, "^[^.]*"), 
           second_row = str_replace(orig_names, str_c(first_row,"."), ""), 
           second_row = str_replace_all(second_row, "_|\\.", "\n"),
           first_row = str_replace_all(first_row, "corr_coef", "correlation coefficients"),
           first_row = str_replace_all(first_row, "_|\\.", " ")
    ) 
  
  numeric_cols <- names(select_if(unnested_df, is.numeric))
  perc_cols <- names(select(unnested_df, contains(".CV"), contains(".setback_save")))
  
  tbl <- flextable(unnested_df) %>% 
    set_header_df(mapping = header_df, key = "orig_names") %>% 
    fontsize(size = 10, part = "all") %>% 
    width(width = 1) %>% 
    merge_h(part = "header") %>% 
    theme_box() 
  
  for (i in seq_along(numeric_cols)){
    eval(parse(text= paste0("tbl <-", "color (tbl, i = ~", numeric_cols[i], "< 0, j = ~", numeric_cols[i], " , color = \"red\")")))
    eval(parse(text= paste0("tbl <-", "width (tbl, j = ~", numeric_cols[i], " , width = 0.7)")))
    eval(parse(text= paste0("tbl <-", "set_formatter (tbl, ", numeric_cols[i], " = function(x) prettyNum(x, digits = 2))")))
  }
  
  for (i in seq_along(perc_cols)) {
    eval(parse(text= paste0("tbl <-", "set_formatter (tbl, ", perc_cols[i], " = function(x) str_c(prettyNum(x, digits = 2), \" %\"))")))
  }
  
  return(tbl)  
}

sample_plt <- function(model_fit, model_df, model_setback_df) {result_df <- augment(model_fit)
samples_time <- model_df %>%
  as_tibble() %>% 
  sample_n(10) %>% 
  pull(TimeStampPosixct)

days_range <- case_when(is.POSIXct(samples_time[1]) ~ 2,
                        is.Date(samples_time[1]) ~ 6)

samples_df_lst <- map(samples_time, ~result_df %>% 
                        filter(TimeStampPosixct %within% (.x %--% (.x + days(days_range)))) %>% 
                        ggplot(aes(x = TimeStampPosixct, color = .model, group = .model))+
                        geom_line(aes(y = consumption), color = "black", alpha = 0.5) +
                        geom_line(aes(y = .fitted), alpha = 0.5) + 
                        geom_line(data=filter(model_setback_df, TimeStampPosixct %within% (.x %--% (.x + days(days_range)))), aes(y = consumption), linetype = "dashed")+
                        labs(title = .x)+
                        theme(legend.position = "bottom")
)
# geom_line(data = tsbox::ts_tsibble(forecast::tsclean(tsbox::ts_ts(select(training_set_daily, consumption)))), aes(x = time, y = value), color = "green")

grid_plt <- cowplot::plot_grid(plotlist = samples_df_lst) 
return (grid_plt)
}

# move this into main markdown ------
weekly_plt_comparison_fun <- function (forecast_real, forecast_stbck, consumption_var) {
  consumption_var <- enquo(consumption_var)
  
  df_lst <- list(forecast_real, forecast_stbck) %>% 
    set_names(c("forecast_real", "forecast_stbck"))
  
  df <- imap(df_lst, function (df, df_name) {
    if (!("Week_Day_Name" %in% names(df))) df <- add_calendar_fields_fun(df)
    if (!("Week_day_consumption" %in% names(df))) df <- as_tibble(df) %>% 
        group_by(Week_Day, Week_Day_Name, OpeMode, workday) %>%
        summarise(Week_day_consumption = sum(!!consumption_var, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(df_name = df_name) %>% 
        mutate(Week_day_consumption = if_else(Week_day_consumption < 0, 0, Week_day_consumption))
    return(df)
  }) %>% 
    bind_rows()
  # main plt
 plt <- weekly_plt_fun(df)
 plt <- plt +
    facet_wrap(~ df_name, ncol = 2)
  
  return(plt)
}

daily_plt_comparison_fun <- function (forecast_real, forecast_stbck, consumption_var) {
  consumption_var <- enquo(consumption_var)
  
  df_lst <- list(forecast_real, forecast_stbck) %>% 
    set_names(c("forecast_real", "forecast_stbck"))
  
  df <- imap(df_lst, function (df, df_name) {
    if (!("Week_Day_Name" %in% names(df))) df <- add_calendar_fields_fun(df)
    if (!("Week_day_consumption" %in% names(df))) df <- as_tibble(df) %>% 
        group_by(Day_Hour, OpeMode, Night_Time) %>%
        summarise(Day_hour_consumption = sum(!!consumption_var, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(df_name = df_name) %>% 
        mutate(Day_hour_consumption = if_else(Day_hour_consumption < 0, 0, Day_hour_consumption))
    return(df)
  }) %>% 
    bind_rows()
  
  # main plt
  plt <- daily_plt_fun(df)
  plt <- plt +
    facet_wrap(~ df_name, ncol = 2)
  
  return(plt)
}

setpoint_plt_comparison_fun <- function (forecast_real, forecast_stbck, consumption_var, histogram_var) {
  consumption_var <- enquo(consumption_var)
  histogram_var <- enquo(histogram_var)
  
  df_lst <- list(forecast_real, forecast_stbck) %>% 
    set_names(c("forecast_real", "forecast_stbck"))
  
  df <- imap(df_lst, function (df, df_name) as_tibble(df) %>% 
               select(OpeMode, !!consumption_var, !!histogram_var) %>% 
               mutate(df_name = df_name)) %>% 
    bind_rows()
  ope_mode <- pull(distinct(df, OpeMode), OpeMode)
  
  # main plt
  plt <- ggplot(data = df, aes(x = !!histogram_var, weights = !!consumption_var))+
    geom_histogram(fill = Color_Mode_for_plot[ope_mode]) +
    labs(title=str_c(ope_mode , ""))+
    My_ggplot_opts+
    facet_wrap(~ df_name, ncol = 2)
  
  return(plt)
}





  