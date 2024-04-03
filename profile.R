x = matrix(runif(1e5*500), ncol = 500)


Rprof()
x_pr = prcomp(x, retx = FALSE)
# data 

library(readr)
library(tseries)
library(forecast)
library(dplyr)

library(MTS)
library(lmtest)
#making time series 

fKinexon<-read_csv("fake_kinexon_data.csv")

kdata <- fKinexon %>% 
  filter( date >= "2023-08-20") %>%
  select(date, metabolic_work)%>% group_by(date) %>%
  summarise(total_metabolic_work = sum(metabolic_work)) %>% arrange(date )


# Create a sequence of dates from the minimum to the maximum date in your data
full_dates <- seq(min(kdata$date), max(kdata$date), by = 'day')

# Create a tibble with full dates
full_dates_df <- tibble(date = full_dates)

# Left join the full dates tibble with your original data, filling NAs with 0
result <- full_dates_df %>%
  left_join(kdata, by = 'date') %>%
  mutate(total_metabolic_work = ifelse(is.na(total_metabolic_work), 1000, total_metabolic_work))

#time series 1 
result_ts_meta<- ts(result$total_metabolic_work, frequency = 7)

plot.ts(result_ts_meta)

plot(result_ts_meta, 
     main = "Time Series Plot",  # Set the main title
     xlab = "Time",               # Set the x-axis label
     ylab = "Metabolic Load",             # Set the y-axis label
     col = "black",                # Set the color of the lines
     lwd = 2)   

sum(result$total_metabolic_work==0)/nrow(result)


kdata <- fKinexon %>% 
  filter( date >= "2023-08-20") %>%
  select(date, accel_load_accum)%>% group_by(date) %>%
  summarise(total_accel_load_accum = sum(accel_load_accum)) %>% arrange(date )


# Create a sequence of dates from the minimum to the maximum date in your data
full_dates <- seq(min(kdata$date), max(kdata$date), by = 'day')

# Create a tibble with full dates
full_dates_df <- tibble(date = full_dates)

# Left join the full dates tibble with your original data, filling NAs with 0
result <- full_dates_df %>%
  left_join(kdata, by = 'date') %>%
  mutate(total_accel_load_accum = ifelse(is.na(total_accel_load_accum), 1000, total_accel_load_accum))

#time series 2
result_ts_acum<- ts(result$total_accel_load_accum, frequency = 7)

plot.ts(result_ts_acum)

plot(result_ts_acum, 
     main = "Time Series Plot",  # Set the main title
     xlab = "Time",               # Set the x-axis label
     ylab = "Accumulation Load",             # Set the y-axis label
     col = "black",                # Set the color of the lines
     lwd = 2)   


sum(result_ts_acum==0)/nrow(result)


Rprof(NULL)
summaryRprof()
