# library(readxl)
library(here)
library(httr)
library(lubridate)
library(purrr)
library(broom)
library(tidyr)
library(ggplot2); library(ggrepel)
library(tibble)
library(dplyr)


add_numbers <- function(df){
  df.split <- split(df, df$geoId)
  df.arr_by_date <- lapply(df.split, arrange, dateRep)
  df.split.nos <- lapply(df.arr_by_date, function(x) tibble::add_column(x, nos = 1:nrow(x)))
  dplyr::bind_rows(df.split.nos)
}

# get data ----------------------------------------------------------------

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
httr::GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- readxl::read_xlsx(tf)


# graf 1 ------------------------------------------------------------------
# broj smrti

zemlje <- c("HR", "SI", "AT", "BA")

g1 <- df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(
    cum_deaths = cumsum(deaths)
    ) %>% 
  filter(geoId %in% zemlje, cum_deaths > 0) %>%
  ggplot(aes(x = dateRep, y = cum_deaths)) +
  geom_point(alpha = .3) +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm") +
  facet_grid(cols = vars(geoId), scales = "free_y") +
  labs(title = "smrti",
       subtitle = format(Sys.time(), "%Y-%m-%d"), x = "", y = "")

ggsave("figs_out/deaths.svg", plot = g1)

# graf 2 ------------------------------------------------------------------
# kretanje novopotvrđenih - ukupno vs zadnja 2 tjedna

df_wks <- df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_cases = cumsum(cases),
         week = as.numeric(strftime(dateRep, format = "%V"))) %>% 
  filter(week %in% c(max(week) - 1, max(week))) %>%
  add_numbers() %>% 
  filter(geoId %in% zemlje, cum_cases > 0) %>% 
  ungroup()

# model

dbl_df <- df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_cases = cumsum(cases),
         weeks = strftime(dateRep, format = "%V")) %>% 
  filter(geoId %in% zemlje, dateRep > "2020-02-29 UTC") %>%
  add_numbers() %>% 
  split(.$geoId) %>% 
  map(~ lm(log2(cum_cases) ~ nos, data = .x)) %>%
  map_dfr(tidy, .id = "geoId") %>% 
  filter(term == "nos") %>% 
  mutate(doubling = 1/estimate)

dbl_wks <- split(df_wks, list(df_wks$geoId, df_wks$week)) %>%
  map(~ lm(log2(cum_cases) ~ nos, data = .x)) %>%
  map_dfr(tidy, .id = "cntr.week") %>% 
  separate(col = cntr.week, into = c("geoId", "week")) %>% 
  filter(term == "nos") %>% 
  mutate(doubling = 1/estimate)
  
  
g2 <- df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_cases = cumsum(cases),
         weeks = strftime(dateRep, format = "%V")) %>% 
  filter(geoId %in% zemlje, dateRep > "2020-02-29 UTC") %>% 
  ggplot(aes(x = dateRep, y = cum_cases)) +
  geom_point(alpha = .3) +
    scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm") +
  geom_smooth(data = df_wks, aes(colour = as.character(week)), method = "lm") +
  geom_text_repel(data = dbl_df, aes(label=round(doubling,1)), 
            x = -Inf, y = Inf, #  hjust=-.5, vjust=2,
            inherit.aes = FALSE, colour = "blue") +
  geom_text_repel(data = dbl_wks, aes(label=round(doubling,1), colour = as.character(week)),
            x = Inf, y = Inf, #  hjust=-.5, vjust=3,
            inherit.aes = FALSE) +
  facet_grid(cols = vars(geoId)) +
  labs(title = "kumulativni slučajevi",
       subtitle = format(Sys.time(), "%Y-%m-%d"), x = "", y = "") +
  guides(colour=FALSE)

ggsave("figs_out/dynamics.svg", plot = g2)


# graf 3 ------------------------------------------------------------------
# Broj slučajeva - Prosječno kretanje od prvog zabilježenog slučaja, iz ishodišta

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
    magrittr::raise_to_power(-1) %>%
    round(2)
}

# model_n_extract(df.cum_cases_days$AT, remove_intercept = TRUE)

doubling_average <- df.cum_cases_days %>%
  sapply(model_n_extract, remove_intercept = TRUE) %>% 
  enframe(name = "geoId")

g3 <- df.cum_cases_days %>% 
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

ggsave("figs_out/doubling.svg", plot = g3)

# graf 4 ------------------------------------------------------------------
# Broj slučajeva vs kretanje (Vlejd spika)


# graf 5 ------------------------------------------------------------------
# Broj kumulativnih slučajeva vs broj novih slučajeva - animacija po uzoru na https://youtu.be/54XLXg4fYsc

