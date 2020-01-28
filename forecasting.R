library(tidyverse)
getwd()
working_path <- "C:/Users/sk0007/OneDrive - Daikin Europe N.V/Documents/R scripts/DCS/DCS/DM_all/DM PodBisk 01012018-31122019"
source("C:/Users/sk0007/OneDrive - Daikin Europe N.V/Documents/R scripts/DCS/DCS/MasterSctripts/Functions.R")
load(file=file.path(working_path, "Data_structure.Rda"))
Evaluated_data_outdoors <- Data_structure$Merged_Outdoor_Units
Evaluated_data_outdoors <- Evaluated_data_outdoors %>% 
  mutate(OpeMode=if_else(Cooling.capacity==0&Heating.capacity==0&Operation.control.mode=="Heat recovery normal control", "HeRec", OpeMode))
Evaluated_data_indoors <- Data_structure$Merged_Indoor_Units

# data frame preparation -----
outdoor_dframe <- Data_structure$Out_DF_list_outdoors[[1]] %>%
  select(Data.create.date, TimeStampPosixct, Watt.hour.amount.Corrected., O.U.1.outside.air.temp., 
         Cooling.capacity, Heating.capacity, OpeMode)

# indoor average sepotints -----
indoors_average_setpoints <- Evaluated_data_indoors %>% select(Data.create.date, TimeStampPosixct, Avg..setting.temp., Indoor.unit.operation.time.accumulation, UnitType, SystemIdentifier) %>% 
  fill(Avg..setting.temp., .direction = "downup") %>% 
  mutate(unit_size=get_unit_size(UnitType), setpoint_factor=Avg..setting.temp.*unit_size, 
         ope_time_factor = Indoor.unit.operation.time.accumulation * unit_size) %>% 
  group_by(TimeStampPosixct) %>% 
  summarise(setpoint_factor_sum=sum(setpoint_factor, na.rm = TRUE), unit_size_sum = sum(unit_size, na.rm = TRUE), average_setpoint=setpoint_factor_sum/unit_size_sum,
            ope_time_factor_sum = sum(ope_time_factor, na.rm = TRUE), average_ope_time = ope_time_factor_sum/unit_size_sum) %>% 
  select(-setpoint_factor_sum, -unit_size_sum, -ope_time_factor_sum)

# data frAme with indoor average setpoints -----
total_df <- outdoor_dframe %>% 
  left_join(indoors_average_setpoints, by="TimeStampPosixct") %>% 
  select(-Data.create.date) %>% 
  group_by(TimeStampPosixct) %>% 
  filter(row_number()==1) %>% 
  ungroup()


library(fpp2)
library(tsbox)
library(forecast)
# a little bit of conversions -------
forecasted_df <- total_df
forecasted_tsbl <- tsibble::as_tsibble(forecasted_df, index = TimeStampPosixct)
forecasted_ts <- ts_ts(forecasted_tsbl)
consumption_ts <- forecasted_ts[,"Watt.hour.amount.Corrected."]
occupancy_ts <- forecasted_ts[,"average_ope_time"]
consumption_clean_ts <- forecast::tsclean(consumption_ts)

# forecasting ----
autoplot(snaive(window(occupancy_ts, start=2018.215, end=2019)))+
           autolayer(window(occupancy_ts, start=2019))
         
autoplot(snaive(window(consumption_ts, start=2018.215, end=2019)))+
  autolayer(window(consumption_ts, start=2019))

fit1 <- meanf(consumption_ts) 
fit2 <- rwf(consumption_ts)
fit3 <- snaive(consumption_ts)
autoplot(consumption_ts) +
  autolayer(fit1, series="Mean", PI=FALSE) +
  autolayer(fit2, series="Naïve", PI=FALSE) +
  autolayer(fit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("kWh") +
  guides(colour=guide_legend(title="Forecast"))
# https://otexts.com/fpp2/accuracy.html ------

GGally::ggpairs(filter(forecasted_df, OpeMode=="Cooling"))
GGally::ggpairs(filter(forecasted_df, OpeMode=="Heating"))


# test with Rqafael ----
cooling_hourly_df <- total_df %>% 
  filter(OpeMode == "Cooling")
cooling_model_hourly <- lm(formula = Watt.hour.amount.Corrected. ~ O.U.1.outside.air.temp. + average_setpoint, data = cooling_hourly_df)
summary(cooling_model_hourly)

cooling_daily_df <- cooling_hourly_df %>% 
  mutate(day_date=date(TimeStampPosixct)) %>% 
  group_by(day_date) %>% 
  summarise(day_consumption=sum(Watt.hour.amount.Corrected., na.rm = TRUE),
            day_cooling_prod = sum(Cooling.capacity, na.rm = TRUE),
            day_avg_ambient_tem = mean(O.U.1.outside.air.temp., na.rm = TRUE),
            day_avg_setpoint = mean(average_setpoint, na.rm = TRUE),
            day_avg_ope_time = mean(average_ope_time, na.rm = TRUE)) %>% 
  mutate(day_avg_ambient_tem_sq = day_avg_ambient_tem ** 2,
         day_avg_setpoint_sq = day_avg_setpoint ** 2,
         abproduct = day_avg_ambient_tem * day_avg_setpoint)
# write_csv2(cooling_daily_df, "C:/Users/sk0007/OneDrive - Daikin Europe N.V/Documents/R scripts/DCS/DCS/DM_all/01112018-30102019/daily_dataframe.csv") 

cooling_model_formula <- formula(day_consumption ~ day_avg_ambient_tem + day_avg_setpoint + day_avg_setpoint_sq + day_avg_ope_time)
cooling_model_daily <- lm(formula = cooling_model_formula, data = cooling_daily_df)
summary(cooling_model_daily)

# test model
library(modelr)

cooling_daily_df %>%
     add_predictions(cooling_model_daily) %>%
  ggplot(aes(x=day_date))+
  geom_point(aes(y=day_consumption), color = "grey")+
  geom_point(aes(y=pred), color = "red")
  
# training on the sample----
training_sample <- sample_n(cooling_daily_df, 100)
test_sample <- cooling_daily_df %>% anti_join(training_sample, by = "day_date")

cooling_model_daily_sample <- lm(formula = cooling_model_formula, data = training_sample)
summary(cooling_model_daily_sample)
test_sample %>% 
  add_predictions(cooling_model_daily_sample) %>% 
  summarise(sum(day_consumption), sum(pred), mean(day_consumption), sigma(cooling_model_daily_sample), 
            variance=sigma(cooling_model_daily_sample)/mean(day_consumption))

# the same for heating ----
heating_hourly_df <- total_df %>% 
  filter(OpeMode == "Heating")
heating_model_hourly <- lm(formula = Watt.hour.amount.Corrected. ~ O.U.1.outside.air.temp. + average_setpoint, 
                           data = heating_hourly_df)
summary(heating_model_hourly)
heating_hourly_df %>% 
add_predictions(heating_model_hourly) %>% 
  ggplot(aes(x=TimeStampPosixct))+
  geom_point(aes(y=Watt.hour.amount.Corrected.), color = "grey")+
  geom_point(aes(y=pred), color = "red")

heating_daily_df <- heating_hourly_df %>% 
  mutate(day_date=date(TimeStampPosixct)) %>% 
  group_by(day_date) %>% 
  summarise(day_consumption=sum(Watt.hour.amount.Corrected., na.rm = TRUE),
            day_heating_prod = sum(Heating.capacity, na.rm = TRUE),
            day_avg_ambient_tem = mean(O.U.1.outside.air.temp., na.rm = TRUE),
            day_avg_setpoint = mean(average_setpoint, na.rm = TRUE),
            day_avg_ope_time = mean(average_ope_time, na.rm = TRUE)) %>% 
  mutate(day_avg_ambient_tem_sq = day_avg_ambient_tem ** 2,
         day_avg_setpoint_sq = day_avg_setpoint ** 2,
         abproduct = day_avg_ambient_tem * day_avg_setpoint)

heating_model_formula <- cooling_model_formula

heating_model_daily <- lm(formula = heating_model_formula, data = heating_daily_df)
summary(heating_model_daily)

heating_daily_df %>% 
  mutate(day_avg_setpoint = day_avg_setpoint + 2)
  add_predictions(heating_model_daily) %>% 
  ggplot(aes(x=day_date))+
  geom_point(aes(y=day_consumption), color = "grey")+
  geom_point(aes(y=pred), color = "red")

heating_daily_df %>% 
  add_predictions(heating_model_daily) %>% 
  summarise(sum(day_consumption), sum(pred), mean(day_consumption), sigma(heating_model_daily), 
            variance=sigma(heating_model_daily)/mean(day_consumption))

# train on sample -----
training_sample_heating <- sample_n(heating_daily_df, 100)
test_sample_heating <- heating_daily_df %>% anti_join(training_sample, by = "day_date")

heating_model_daily_sample <- lm(formula = day_consumption ~ day_avg_ambient_tem + day_avg_setpoint + day_avg_setpoint_sq + day_avg_ope_time, 
                                 data = training_sample_heating)
summary(heating_model_daily_sample)

test_sample_heating %>% 
  add_predictions(heating_model_daily_sample) %>% 
  summarise(sum(day_consumption), sum(pred), mean(day_consumption), sigma(heating_model_daily_sample), 
                         variance=sigma(heating_model_daily_sample)/mean(day_consumption))

test_sample_heating %>% 
  add_predictions(heating_model_daily_sample) %>% 
  ggplot(aes(x=day_date))+
  geom_point(aes(y=day_consumption), color = "grey")+
  geom_point(aes(y=pred), color = "red")


# better workflow
outdoor_base_df <- Data_structure$Out_DF_list_outdoors[[1]] %>%
  select(Data.create.date, TimeStampPosixct, Watt.hour.amount.Corrected., O.U.1.outside.air.temp., 
         Cooling.capacity, Heating.capacity, OpeMode)

indoors_base_df <- Evaluated_data_indoors %>% select(Data.create.date, TimeStampPosixct, Avg..setting.temp., Indoor.unit.operation.time.accumulation, UnitType, SystemIdentifier) %>% 
  fill(Avg..setting.temp., .direction = "downup") %>% 
  mutate(unit_size=get_unit_size(UnitType), setpoint_factor=Avg..setting.temp.*unit_size, 
         ope_time_factor = Indoor.unit.operation.time.accumulation * unit_size) %>% 
  group_by(TimeStampPosixct) %>% 
  summarise(setpoint_factor_sum=sum(setpoint_factor, na.rm = TRUE), unit_size_sum = sum(unit_size, na.rm = TRUE), average_setpoint=setpoint_factor_sum/unit_size_sum,
            ope_time_factor_sum = sum(ope_time_factor, na.rm = TRUE), average_ope_time = ope_time_factor_sum/unit_size_sum) %>% 
  select(-setpoint_factor_sum, -unit_size_sum, -ope_time_factor_sum)

base_df <- outdoor_base_df %>% 
  left_join(indoors_base_df, by="TimeStampPosixct") 

# models


renamed_base_df <- base_df %>%
  rename(time_stamp = "TimeStampPosixct", consumption = "Watt.hour.amount.Corrected.", tem = "O.U.1.outside.air.temp.")

hourly_formula <- formula(consumption ~ tem + average_setpoint + tem_sq + average_setpoint_sq)

model_ope_modes <- c("Cooling", "Heating")

walk(model_ope_modes,  function (ope_mode) {
      model_df <- renamed_base_df %>% 
      filter(OpeMode == ope_mode) %>% 
      mutate(average_setpoint_sq = average_setpoint ** 2, tem_sq = tem ** 2, abproduct=average_setpoint*tem)
      
      training_sample <- sample_n(model_df, length(model_df)*2/3)
      test_sample <- model_df %>% 
        anti_join(training_sample, by="time_stamp")
      
      model <- lm(formula = hourly_formula, data = model_df)
      print(summary(model))
      
      training_sample %>% 
        summarise(mean(consumption), sigma(model), variance=sprintf("%.0f %%",sigma(model)/mean(consumption)*100)) %>% 
        print()
      
      test_sample %>% 
      add_predictions(model) %>% 
        summarise(sum(consumption), sum(pred)) %>% 
        print()
      
      plt <- test_sample %>% 
        add_predictions(model) %>% 
        ggplot(aes(x=time_stamp))+
        geom_point(aes(y=consumption), color = "grey")+
        geom_point(aes(y=pred), color = "red")
      plt
}) %>% 
  set_names(model_ope_modes)


# resampling


summary(cooling_model_hourly)

cooling_daily_df <- cooling_hourly_df %>% 
  mutate(day_date=date(TimeStampPosixct)) %>% 
  group_by(day_date) %>% 
  summarise(day_consumption=sum(Watt.hour.amount.Corrected., na.rm = TRUE),
            day_cooling_prod = sum(Cooling.capacity, na.rm = TRUE),
            day_avg_ambient_tem = mean(O.U.1.outside.air.temp., na.rm = TRUE),
            day_avg_setpoint = mean(average_setpoint, na.rm = TRUE),
            day_avg_ope_time = mean(average_ope_time, na.rm = TRUE)) %>% 
  mutate(day_avg_ambient_tem_sq = day_avg_ambient_tem ** 2,
         day_avg_setpoint_sq = day_avg_setpoint ** 2,
         abproduct = day_avg_ambient_tem * day_avg_setpoint)


# degreedays <- read_csv2("C:/Users/sk0007/OneDrive - Daikin Europe N.V/Documents/R scripts/DCS/DCS/DM_all/01112018-30102019/DD.csv") 

# daily_total_with_dd <- daily_total_df %>% 
#   full_join(degreedays, by = c("day_date" = "HDD"))
# write_csv2(daily_total_with_dd, path = file.path(working_path, "daily_total_with_dd.csv"))
# 
# 
# outdoor_dframe %>% 
#   filter(Heating.capacity != 0 | Cooling.capacity != 0) %>%
#   summarise(min(Watt.hour.amount.Corrected.), max(Watt.hour.amount.Corrected.), mean(Watt.hour.amount.Corrected.),
#             min(Stand.by.power.consumption), max(Stand.by.power.consumption), mean(Stand.by.power.consumption))
  
