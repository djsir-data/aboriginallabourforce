library(dplyr)
library(readr)
library(stringr)
library(tidyr)

census <- read_csv("data-raw/Vic ATSI Employment SA3 .csv", skip = 10, )
names(census) <- c("drop1", "drop2", "ind", "lf_stat", "sa3", "count")

census <- census %>% 
  select(-contains("drop")) %>% 
  filter(!is.na(count)) %>%
  mutate(
    ind = ifelse(str_detect(ind, "Aboriginal"), "Aboriginal", "Non-Aboriginal"),
    lf_stat = str_extract(
      lf_stat, 
      "Employed|Unemployed|Not in the labour force"
      ) %>% 
      str_to_lower() %>% 
      str_replace("not in the labour force", "nilf")
  ) %>%
  pivot_wider(names_from = lf_stat, values_from = count, values_fn = sum) 


census_total <- census %>% 
  group_by(sa3) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  ungroup() %>% 
  mutate(ind = "total")


census <- census %>% 
  bind_rows(census_total) %>% 
  mutate(
    unemp_rate = unemployed / (employed + unemployed),
    part_rate = (employed + unemployed) / (employed + unemployed + nilf),
    emp_pop = employed / (employed + unemployed + nilf)
    )

write_csv(census, "data/census.csv")
