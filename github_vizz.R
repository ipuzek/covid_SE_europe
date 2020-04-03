# library(readxl)
library(httr)
library(lubridate)
library(purrr)
library(broom)
library(tidyr)
library(ggplot2); library(ggrepel)
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

df %>%
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
  
  
df %>%
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
