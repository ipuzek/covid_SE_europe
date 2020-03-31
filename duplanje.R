# doubling rate #

library(tibble)
library(broom)
library(magrittr)

zemlje <- c("HR", "SI", "AT", "SK")
# zemlje <- c("IT", "ES")


df.cum_cases_days <- df %>%
  arrange(geoId, dateRep) %>% 
  filter(geoId %in% zemlje, cases > 0) %>%
  group_by(geoId) %>% 
  mutate(cum_cases = cumsum(cases)) %>% 
  ungroup() %>% 
  split(.$geoId) %>% 
  lapply(function(x) add_column(x, days = 1:nrow(x)))

model_n_extract <- function(df, remove_intercept) {
  m <- lm(log2(cum_cases) ~ days, data = df)
  if (remove_intercept) m <- lm(log2(cum_cases) ~ days - 1, data = df)
  m %>%   
    tidy %>%
    filter(term == "days") %>% 
    pull(estimate) %>%
    raise_to_power(-1) %>%
    round(2)
}

doubling_average <- df.cum_cases_days %>%
  sapply(model_n_extract, remove_intercept = FALSE) %>% 
  enframe(name = "geoId")

doubling_last_10_days <- df.cum_cases_days %>%
  lapply(function(x) slice(x, tail(row_number(), 10))) %>% 
  sapply(model_n_extract, remove_intercept = FALSE) %>% 
  enframe(name = "geoId")


###


df.sliced <- df.cum_cases_days %>% 
  lapply(function(x) slice(x, tail(row_number(), 10))) %>% 
  bind_rows()

df.cum_cases_days %>% 
  bind_rows() %>% 
  ggplot(aes(x = days, y = cum_cases)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(data = df.sliced, method = "lm", se = FALSE, colour = "red") +
  geom_text(data = doubling_average, aes(label=value), 
            x = -Inf, y = Inf, hjust=-2, vjust=2,
            inherit.aes = FALSE, colour = "blue") +
  geom_text(data = doubling_last_10_days, aes(label=value), 
            x = -Inf, y = Inf, hjust=-2, vjust=3.5,
            inherit.aes = FALSE, colour = "red") +
  scale_y_continuous(trans = "log2") +
  facet_grid(cols = vars(geoId)) +
  labs(title = "duplanje",
       subtitle = "plavo = duplanje (u danima) od 1. slučaja; crveno = duplanje u zadnjih 10 dana")

