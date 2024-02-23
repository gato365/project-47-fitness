

##-------------------------------------------
## Name: calculatePower
## Purpose: Calculate Power when lifting weights
## Input: Weight as a vector
## Output: Power Exerted for an exercise
## Notes: NA
##-------------------------------------------

calculatePower = function(weight_rep_vec) {
  tmp_vec_df = data.frame(tmp_col = weight_rep_vec)
  
  
  pwr_vec = separate(data = tmp_vec_df,
                     col = tmp_col,
                     into = c('weight','reps'),
                     sep = '-') %>%  ## separate weight and rep
    mutate(weight = as.numeric(str_trim(weight)),
           reps = as.numeric(str_trim(reps)),
           power_wt_r = weight*reps ) %>%  ## calculate power
    pull(power_wt_r)
  
  return(pwr_vec)
  
}





suppressWarnings({wo_df = read_xlsx('fitness-data.xlsx', sheet = 'workout',col_types = NULL) %>% 
  mutate(Date = as.Date(Date),
         month = month(Date, label = TRUE, abbr = TRUE),
         year = year(Date)) 
})




## Find Power per exercise
power_by_ex_df = wo_df %>% 
  filter(is_power == 1,`completion of workout` == "Yes") %>% 
  mutate_at(vars(contains("set")), ~ calculatePower(.)) %>% 
  mutate(pwr_ex = rowSums(select(., starts_with("set") ),na.rm = TRUE ))

## Find Power per day
power_day_df = power_by_ex_df %>% 
  group_by(Date) %>% 
  summarise(num_of_ex = n(),sum_pwr = as.numeric(sum(pwr_ex)))




# Assuming power_day_df is your dataframe and it has columns for Date and sum_pwr.
summarize_wo_df <- power_day_df %>%
  filter(Date >= (Sys.Date() - 30)) %>%
  mutate(
    Week = case_when(
      Date >= lubridate::floor_date(Sys.Date(), "week") ~ "Current Week",
      Date < lubridate::floor_date(Sys.Date(), "week") & Date >= lubridate::floor_date(Sys.Date() - 7, "week") ~ "Previous Week",
      Date < lubridate::floor_date(Sys.Date() - 7, "week") & Date >= lubridate::floor_date(Sys.Date() - 14, "week") ~ "Two Weeks Ago",
      TRUE ~ "Three Weeks Ago"
    ),
    DayOfWeek = wday(Date, label = TRUE, abbr = FALSE)  # Using full names for days
  ) %>%
  group_by(Week, DayOfWeek) %>%
  summarise(sum_pwr = sum(sum_pwr, na.rm = TRUE)) %>%
  mutate(DayOfWeek = factor(DayOfWeek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ungroup()