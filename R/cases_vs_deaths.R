# usporedivi po smrtima

deaths <- df %>%
  group_by(countriesAndTerritories) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_deaths = cumsum(deaths)) %>% 
  summarise(max_cum_deaths = max(cum_deaths)) %>% 
  arrange(max_cum_deaths)

cases_deaths <- df %>%
  group_by(countriesAndTerritories) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_cases = cumsum(cases)) %>% 
  summarise(max_cum_cases = max(cum_cases)) %>%
  left_join(deaths) %>% 
  filter(max_cum_deaths > 0) %>% 
  # filter(max_cum_deaths %in% c(3:7)) %>% 
  arrange(max_cum_cases)

cases_deaths %>%
  filter(max_cum_cases < 20000) %>% 
  ggplot(aes(max_cum_cases, max_cum_deaths)) +
  geom_point()

library(broom)

cor_fun <- function(x) {
  cases_deaths %>%
  filter(max_cum_deaths > x) %>% 
  cor.test(~ max_cum_cases + max_cum_deaths, data = .) %>% 
  tidy()
}

cors <- lapply(5:2000, cor_fun)

cors.df <- bind_rows(cors)

cors.df %>%
  unique() %>% 
  ggplot() +
  geom_pointrange(aes(x = 1:41, y = estimate, ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous(limits = c(0,1))

             