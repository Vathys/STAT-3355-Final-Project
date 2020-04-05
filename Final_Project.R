# Questions we need to answer:
# Did Texas become a more diverse state from 2000-2010? 
# What is the distribution of race between urban and rural areas?
# Is there a correlation between multigenerational living and the number of children in a household?
# Which race is more likely to live in a multigenerational household?

library(tidyverse)
library(tidycensus)
library(gridExtra)

# Use census_api_key(FULL_KEY_FIRST_TIME, install = TRUE)
# to store key in .Renviron
# Used to activate Census API
census_api_key(Sys.getenv("CENSUS_API_KEY"))

var_10 <- load_variables(year = "2010", dataset = 'sf1', cache = TRUE)
var_10_sf2 <- load_variables(year = "2010", dataset = 'sf2', cache = TRUE)
var_00 <- load_variables(year = "2000", dataset = 'sf1', cache = TRUE)

# ANSWERING QUESTION 1

race_vars_10 <- c("P003002", "P003003", "P003004", "P003005", "P003006", "P003007", "P003008")
race_vars_00 <- c("P003003", "P003004", "P003005", "P003006", "P003007", "P003008", "P003009")

# Diversity 
p_3_10 <- get_decennial(geography = "county", variables = race_vars_10, 
                        summary_var = "P003001", year = 2010, 
                        state = "TX", geometry = TRUE, keep_geo_vars = TRUE)

p_3_10 <- p_3_10 %>% 
  select(NAME.x, variable, value, summary_value) %>% 
  spread(variable, value) %>% 
  mutate(nonwhite = P003003 + P003004 + P003005 + P003006 + P003007 + P003008, white = P003002) %>%
  select(NAME.x, white, nonwhite, summary_value) %>%
  gather(key = "variable", value = "value", white, nonwhite) %>%
  mutate(pct = 100 * (value / summary_value))

p_3_00 <- get_decennial(geography = "county", variables = race_vars_00,
                        summary_var = "P003002", year = 2000,
                        state = "TX", show_call = TRUE) 
p_3_00 <- full_join(p_3_00, county_laea, by = "GEOID")

p_3_00 <- p_3_00 %>% 
  select(NAME, variable, value, summary_value, geometry) %>% 
  spread(variable, value) %>% 
  mutate(nonwhite = P003004 + P003005 + P003006 + P003007 + P003008 + P003009, white = P003003) %>%
  select(NAME, white, nonwhite, summary_value, geometry) %>%
  gather(key = "variable", value = "value", white, nonwhite) %>%
  mutate(pct = 100 * (value / summary_value))

race_plot_10 <- ggplot(p_3_10, aes(fill = pct)) + 
  geom_sf() + 
  facet_wrap(~variable)

race_plot_00 <- ggplot(p_3_00, aes(fill = pct)) +
  geom_sf() + 
  facet_wrap(~variable)

grid.arrange(race_plot_00, race_plot_10, nrow = 1)

# ANSWERING QUESTION 2

urban_vars <- c("P002002", "P002005", "P002006")

p_2_10 <- get_decennial(geography = "county", variables = urban_vars,
                        summary_var = "P002001", year = 2010,
                        state = "TX", geometry = TRUE) %>%
  mutate(pct = 100 * (value / summary_value))

urban_plot_10 <- ggplot(p_2_10, aes(fill = pct)) + 
  geom_sf() +
  facet_wrap(~variable)

grid.arrange(race_plot_00, race_plot_10, nrow = 1)


# ANSWER QUESTION 4

p_16_10 <- get_decennial(geography = "county", table = "P016",
                         year = 2010, state = "TX")
p_17_10 <- get_decennial(geography = "county", table = "P017",
                         year = 2010, state = "TX", show_call = TRUE)

var_names <- c("White", "Black/African American", "American Indian/Native Alaskan", "Asian", "Native Hawaiian/Pacific Islander", "Other Race", "Two or More")

multi_sum <- c("PCT014A001", "PCT014B001", "PCT014C001", "PCT014D001", "PCT014E001","PCT014F001", "PCT014G001")

multi_g_002 <- c("PCT014A002", "PCT014B002", "PCT014C002", "PCT014D002", "PCT014E002","PCT014F002", "PCT014G002")

pct_14_002_10 <- get_decennial(geography = "county", variables = multi_g_002[1], 
                               summary_var = multi_sum[1],
                               year = 2010, state = "TX", show_call = TRUE) %>%
  mutate(percent = value / summary_value)

for(i in seq(from = 2, to = length(multi_g_002))){
  pct_14_002_10 <- pct_14_002_10 %>% bind_rows(get_decennial(geography = "county", variables = multi_g_002[i], 
                                 summary_var = multi_sum[i],
                                 year = 2010, state = "TX", show_call = TRUE) %>%
    mutate(percent = value / summary_value))
}

three_or_more <- ggplot(pct_14_002_10, mapping = aes(x = variable, y = percent)) + 
  geom_violin(draw_quantiles = 0.5) + labs(title = "Three or more generations") + ylim (0, 0.5)

multi_g_003 <- c("PCT014A003", "PCT014B003", "PCT014C003", "PCT014D003", "PCT014E003","PCT014F003", "PCT014G003")

pct_14_003_10 <- get_decennial(geography = "county", variables = multi_g_003[1], 
                               summary_var = multi_sum[1],
                               year = 2010, state = "TX", show_call = TRUE) %>%
  mutate(percent = value / summary_value)

for(i in seq(from = 2, to = length(multi_g_003))){
  pct_14_003_10 <- pct_14_003_10 %>% bind_rows(get_decennial(geography = "county", variables = multi_g_003[i], 
                                                             summary_var = multi_sum[i],
                                                             year = 2010, state = "TX", show_call = TRUE) %>%
                                                 mutate(percent = value / summary_value))
}


two_or_less <- ggplot(pct_14_003_10, mapping = aes(x = variable, y = percent)) + 
  geom_violin(draw_quantiles = 0.5) + labs(title = "Two or less generations") + ylim(0.5, 1)

grid.arrange(two_or_less, three_or_more, nrow = 2)
