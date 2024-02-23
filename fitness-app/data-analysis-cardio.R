



library(tidyverse)
library(lubridate)
library(readxl)



cardio_df = read_xlsx('fitness-data.xlsx', sheet = 'cardio',col_types = NULL) 



cardio_by_week_df <- cardio_df %>% 
  filter(Date >= (Sys.Date() - 30)) %>%
  mutate(
    Week = case_when(
      Date >= lubridate::floor_date(Sys.Date(), "week") ~ "Current Week",
      Date < lubridate::floor_date(Sys.Date(), "week") & Date >= lubridate::floor_date(Sys.Date() - 7, "week") ~ "Previous Week",
      Date < lubridate::floor_date(Sys.Date() - 7, "week") & Date >= lubridate::floor_date(Sys.Date() - 14, "week") ~ "Two Weeks Ago",
      TRUE ~ "Three Weeks Ago"
    ),
    DayOfWeek = wday(Date, label = TRUE, abbr = FALSE) 
  )