# doubling rate #

library(tibble)
library(broom)
library(magrittr)

zemlje <- c("HR", "SI", "AT", "SK")
# zemlje <- c("HR", "SI", "DK", "AT", "DE")
# zemlje <- c("IT", "ES", "UK", "US")


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

# model_n_extract(df.cum_cases_days$AT, remove_intercept = TRUE)

doubling_average <- df.cum_cases_days %>%
  sapply(model_n_extract, remove_intercept = TRUE) %>% 
  enframe(name = "geoId")

df.cum_cases_days %>% 
  bind_rows() %>% 
  ggplot(aes(x = days, y = cum_cases)) +
  geom_abline(intercept = 0, slope = 1/5, size = .2, colour = "grey")  + #, linetype = 2) +
  geom_abline(intercept = 0, slope = 1/4, size = .2, colour = "grey")  + #, linetype = 2) +
  geom_abline(intercept = 0, slope = 1/3, size = .2, colour = "grey")  + #, linetype = 2) +
  geom_abline(intercept = 0, slope = 1/2, size = .2, colour = "grey")  + #, linetype = 2) +
  geom_abline(intercept = 0, slope = 1/1, size = .2, colour = "grey")  + #, linetype = 2) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula=y~x-1) +
  scale_y_continuous(trans = "log2") +
  geom_text(data = doubling_average, aes(label=value), 
            x = -Inf, y = Inf, hjust=-.2, vjust=1.8,
            inherit.aes = FALSE, colour = "blue") +
  facet_grid(cols = vars(geoId)) +
  labs(title = "Duplanje",
       subtitle = "- prosječni broj dana za duplanje slučajeva od prvog slučaja")
