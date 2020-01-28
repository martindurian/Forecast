require(fpp3)
○
# moj df
total_df_ts <- total_df %>% 
  as_tsibble(index = TimeStampPosixct) %>% 
  fill_gaps() %>% 
  fill(Watt.hour.amount.Corrected., average_setpoint, O.U.1.outside.air.temp.) %>% 
  mutate(consumption = if_else(Watt.hour.amount.Corrected.==0, 0.015, Watt.hour.amount.Corrected.), temp_diff=average_setpoint - O.U.1.outside.air.temp.)

summary(total_df_ts)

total_df_ts %>%
  gather("var", "value", Watt.hour.amount.Corrected., O.U.1.outside.air.temp.) %>%
  ggplot(aes(x = TimeStampPosixct, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  xlab("Time") + ylab(NULL) +
  ggtitle("VRV consumption")

# model hourly temp diff -----
fit <- total_df_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-03-20 14:00:00") %--% ymd_hms("2018-04-20 14:00:00"))) %>% 
  model(ARIMA(box_cox(consumption, lambda = 0) ~ temp_diff))
report(fit)

bind_rows(
  `Regression Errors` = residuals(fit, type="regression"),
  `ARIMA Errors` = residuals(fit, type="innovation"),
  .id = "type"
) %>%
  ggplot(aes(x = TimeStampPosixct, y = .resid)) +
  geom_line() +
  facet_grid(vars(type), scales = "free_y") +
  xlab("Year") + ylab(NULL)

fit %>% 
  gg_tsresiduals()

augment(fit) %>%
  features(.resid, ljung_box, dof = 5, lag = 8)

augment(fit) %>%
gather("var", "value", consumption, .fitted) %>%
  ggplot(aes(x = TimeStampPosixct, y = value, color = var)) +
  geom_line() +
  xlab("Time") + ylab(NULL) +
  ggtitle("VRV consumption")

augment(fit) %>% 
  as_tibble() %>% 
  summarise(sum(consumption), sum(.fitted))

# forecast with limits ------
# Bounds
a <- min(total_df_ts$consumption)*0.9
b <- max(total_df_ts$consumption)*1.1
# Transform data
total_df_ts <- total_df_ts %>% 
  mutate(tr.consumption = log((consumption-a)/(b-consumption)))

fit2 <- total_df_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-03-20 14:00:00") %--% ymd_hms("2018-04-20 14:00:00"))) %>% 
  model(ARIMA(tr.consumption ~ temp_diff))
report(fit2)

# Back-transform forecasts
result <- augment(fit2) %>% 
  mutate(res.fitted = (b-a)*exp(.fitted)/(1+exp(.fitted)) + a, res.consumption = (b-a)*exp(tr.consumption)/(1+exp(tr.consumption)) + a)
# Plot result on original scale
result %>% 
gather("var", "value", res.fitted, res.consumption) %>%
  ggplot(aes(x = TimeStampPosixct, y = value, color = var)) +
  geom_line() +
  xlab("Time") + ylab(NULL) +
  ggtitle("VRV consumption")

result %>% 
  as_tibble() %>% 
  summarise(sum(res.fitted), sum(res.consumption))

# try daily with limits (no need for limits) -------
total_df_hr_ts <- total_df %>% 
  as_tsibble(index = TimeStampPosixct) %>% 
  index_by(date.stamp = date(TimeStampPosixct)) %>% 
  summarise(hr.consumption = sum(Watt.hour.amount.Corrected.), average_setpoint = mean(average_setpoint), ambient.temp = mean(O.U.1.outside.air.temp.)) %>% 
  fill_gaps() %>% 
  fill(hr.consumption, average_setpoint, ambient.temp) %>% 
  mutate(consumption = if_else(hr.consumption==0, 0.015, hr.consumption), temp_diff=average_setpoint - ambient.temp)

# Bounds
a <- min(total_df_hr_ts$consumption)*0.9
b <- max(total_df_hr_ts$consumption)*1.1
# Transform data
total_df_hr_ts <- total_df_hr_ts %>% 
  mutate(tr.consumption = log((consumption-a)/(b-consumption)))  
  # filter(is.na(tr.consumption))

fit_hr <- total_df_hr_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-03-20 14:00:00") %--% ymd_hms("2018-04-20 14:00:00"))) %>% 
  model(ARIMA(tr.consumption ~ temp_diff))
report(fit_hr)

result_hr <- augment(fit_hr) %>% 
  mutate(res.fitted = (b-a)*exp(.fitted)/(1+exp(.fitted)) + a, res.consumption = (b-a)*exp(tr.consumption)/(1+exp(tr.consumption)) + a)
# Plot result on original scale
result_hr %>% 
  gather("var", "value", res.fitted, res.consumption) %>%
  ggplot(aes(x = date.stamp, y = value, color = var)) +
  geom_line() +
  xlab("Time") + ylab(NULL) +
  ggtitle("VRV consumption")

result_hr %>% 
  as_tibble() %>% 
  summarise(sum(res.fitted), sum(res.consumption))

# without transformation ------

fit_hr2 <- total_df_hr_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-03-20 14:00:00") %--% ymd_hms("2018-04-20 14:00:00"))) %>% 
  model(ARIMA(consumption ~ temp_diff))
report(fit_hr2)

# plot
augment(fit_hr2) %>% 
  gather("var", "value", .fitted, consumption) %>%
  ggplot(aes(x = date.stamp, y = value, color = var)) +
  geom_line() +
  xlab("Time") + ylab(NULL) +
  ggtitle("VRV consumption")

augment(fit_hr2) %>% 
  as_tibble() %>% 
  summarise(sum(.fitted), sum(consumption))

# forecast
setback <- -4
future_df_hr_ts <- total_df_hr_ts %>% 
  select(temp_diff) %>% 
  mutate(temp_diff = if_else(temp_diff>0, temp_diff-setback, temp_diff+setback))
forecast(fit_hr2, new_data = future_df_hr_ts) %>% 
as_tibble() %>% 
    summarise(sum(consumption))

# include dummy variables ------
total_df_hr_ts <- total_df %>% 
  mutate(OpeMode = replace(OpeMode, OpeMode == "Idle", NA)) %>% 
  fill(OpeMode, .direction = "downup") %>% 
  mutate(OpeMode = as.factor(OpeMode)) %>% 
  as_tsibble(index = TimeStampPosixct) %>% 
  index_by(date.stamp = date(TimeStampPosixct)) %>% 
  summarise(most.freq.opemode = names(table(OpeMode))[which.max(table(OpeMode))], hr.consumption = sum(Watt.hour.amount.Corrected.), 
            average_setpoint = mean(average_setpoint), ambient.temp = mean(O.U.1.outside.air.temp.)) %>% 
  fill_gaps() %>% 
  fill(most.freq.opemode, hr.consumption, average_setpoint, ambient.temp) %>% 
  mutate(hr.consumption = replace(hr.consumption, hr.consumption==0, 0.36), temp_diff=average_setpoint - ambient.temp) %>% 
  mutate(dummy.cooling = if_else(most.freq.opemode == "Cooling", 1, 0))

# check if mode only cooling or heating
total_df_hr_ts %>% distinct(most.freq.opemode)

GGally::ggpairs(total_df_hr_ts)

# subset dataframe
window_df_hr_ts <- total_df_hr_ts 
 # filter(date.stamp %within% (ymd("2018-05-01") %--% ymd("2018-08-31")))
window_df_hr_ts %>% distinct(most.freq.opemode)

GGally::ggpairs(window_df_hr_ts)

# arima
fit_hr3 <- window_df_hr_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-03-20 14:00:00") %--% ymd_hms("2018-04-20 14:00:00"))) %>% 
  model(ARIMA(hr.consumption ~ ambient.temp + average_setpoint + dummy.cooling))
report(fit_hr3)

# linear regression
fit_tslm_hr3 <- window_df_hr_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-03-20 14:00:00") %--% ymd_hms("2018-04-20 14:00:00"))) %>% 
  model(TSLM(hr.consumption ~ ambient.temp + average_setpoint + dummy.cooling))
report(fit_tslm_hr3)

# plot
arima_plot <- augment(fit_hr3) %>% 
  gather("var", "value", .fitted, hr.consumption) %>%
  ggplot(aes(x = date.stamp, y = value, color = var)) +
  geom_line() +
  xlab("Time") + ylab(NULL) +
  ggtitle("ARIMA")
arima_plot

augment(fit_hr3) %>% 
  as_tibble() %>% 
  summarise(sum(.fitted), sum(hr.consumption))

augment(fit_hr3) %>% 
  pull(.resid) %>% 
RMSE()

tslm_plot <- augment(fit_tslm_hr3) %>% 
  gather("var", "value", .fitted, hr.consumption) %>%
  ggplot(aes(x = date.stamp, y = value, color = var)) +
  geom_line() +
  xlab("Time") + ylab(NULL) +
  ggtitle("TSLM")
tslm_plot

augment(fit_tslm_hr3) %>% 
  as_tibble() %>% 
  summarise(sum(.fitted), sum(hr.consumption))

augment(fit_tslm_hr3) %>% 
  pull(.resid) %>% 
  RMSE()

# forecast
setback <- 0
future_df_hr_ts <- window_df_hr_ts %>% 
  select(average_setpoint, ambient.temp, dummy.cooling) %>% 
  mutate(average_setpoint = if_else(dummy.cooling == 1, average_setpoint+setback, average_setpoint-setback))

forecast(fit_hr3, new_data = future_df_hr_ts) %>% 
  as_tibble() %>% 
  summarise(sum(hr.consumption))

forecast(fit_tslm_hr3, new_data = future_df_hr_ts) %>% 
  as_tibble() %>% 
  summarise(sum(hr.consumption))

# prophet ------
require(prophet)
window_df_hourly_ts <- total_df_ts %>%
  # filter(TimeStampPosixct %within% (ymd_hms("2018-12-01 00:00:00") %--% ymd_hms("2018-12-31 23:00:00"))) %>% 
  select(ds = TimeStampPosixct, y = consumption, ambient = O.U.1.outside.air.temp., average_setpoint, OpeMode) %>% 
  mutate(y=log(y))

# dummy cooling
window_df_hourly_ts <- window_df_hourly_ts %>% 
  mutate(OpeMode = replace(OpeMode, OpeMode == "Idle", NA)) %>% 
  fill(OpeMode, .direction = "downup") %>% 
  mutate(OpeMode = as.factor(OpeMode)) %>% 
  mutate(dummy.cooling = if_else(OpeMode == "Cooling", 1, 0))

prophet_fit <- prophet::prophet()
prophet_fit <- add_regressor(prophet_fit, "ambient")
prophet_fit <- add_regressor(prophet_fit, "average_setpoint")
prophet_fit <- add_regressor(prophet_fit, "dummy.cooling")
prophet_fit <- fit.prophet(prophet_fit, window_df_hourly_ts)

future <- prophet::make_future_dataframe(prophet_fit, periods = 1)

future <- future %>% right_join(select(window_df_hourly_ts, ambient, average_setpoint, dummy.cooling), by="ds")

fcst <- predict(prophet_fit, future)
fcst_backtransformed <- fcst %>% mutate(yhat = exp (yhat))
window_df_hourly_ts %>% as_tibble() %>%  mutate(y=exp(y)) %>% summarise(sum(y))
fcst_backtransformed %>% summarise(sum(yhat))

window_df_hourly_ts %>% 
  right_join(select(fcst_backtransformed, ds, yhat), by = "ds") %>% mutate(y_orig=exp(y)) %>% 
  ggplot(aes(x=ds))+
  geom_line(aes(y=y_orig), color="blue")+
  geom_line(aes(y=yhat), color="red")


plot(prophet_fit, fcst_backtransformed)

# prophet daily
window_df_hr_ts <- total_df_hr_ts %>% 
  filter(date.stamp %within% (ymd("2019-01-01") %--% ymd("2019-12-31"))) %>% 
  select(ds = date.stamp, y = hr.consumption, ambient = ambient.temp, average_setpoint, dummy.cooling) %>% 
  mutate(y=log(y))

prophet_fit <- prophet::prophet()
prophet_fit <- add_regressor(prophet_fit, "ambient")
prophet_fit <- add_regressor(prophet_fit, "average_setpoint")
prophet_fit <- add_regressor(prophet_fit, "dummy.cooling")
prophet_fit <- fit.prophet(prophet_fit, window_df_hr_ts)

future <- prophet::make_future_dataframe(prophet_fit, periods = 1)

future <- future %>% 
  mutate(ds = date(ds)) %>% 
  right_join(select(window_df_hr_ts, ambient, average_setpoint, dummy.cooling), by="ds")

fcst <- predict(prophet_fit, future)
fcst_backtransformed <- fcst %>% 
  mutate(yhat = exp (yhat))%>% 
  mutate(ds = date(ds)) 
window_df_hr_ts %>% as_tibble() %>%  mutate(y=exp(y)) %>% summarise(sum(y))
fcst_backtransformed %>% summarise(sum(yhat))

window_df_hr_ts %>% 
  right_join(select(fcst_backtransformed, ds, yhat), by = "ds") %>% mutate(y_orig=exp(y)) %>% 
  ggplot(aes(x=ds))+
  geom_line(aes(y=y_orig), color="blue")+
  geom_line(aes(y=yhat), color="red")


