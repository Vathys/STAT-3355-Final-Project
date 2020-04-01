# Questions we need to answer:
# Did Texas become a more diverse state from 2000-2010? 
# What is the distribution of race between urban and rural areas?
# Can we accurately predict the increase in population by the number of children?
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
pct_14_10 <- 