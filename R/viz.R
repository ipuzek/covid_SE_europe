# library(readxl)
library(httr)
library(dplyr)
library(ggplot2)

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
httr::GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- readxl::read_xlsx(tf)


# VIZZ --------------------------------------------------------------------

zemlje <- c("HR", "SI", "AT", "SK")

df %>% 
  arrange(dateRep) %>% 
  filter(geoId %in% zemlje, dateRep > "2020-03-01 UTC") %>% 
  ggplot(aes(x = dateRep, y = cases)) +
  geom_point() +
  facet_grid(cols = vars(geoId), scales = "free_y") +
  labs(title = "novi slučajevi",
       subtitle = format(Sys.time(), "%Y-%m-%d"), x = "", y = "")

df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cumsum = cumsum(cases)) %>% 
  filter(geoId %in% zemlje, dateRep > "2020-03-01 UTC") %>% 
  ggplot(aes(x = dateRep, y = cumsum)) +
  geom_point(alpha = .3) +
  geom_smooth(method="glm",
             method.args=list(family=gaussian(link="log")), se = FALSE) +
  scale_y_continuous(trans = "log2") +
  facet_grid(cols = vars(geoId)) +
  labs(title = "kumulativni slučajevi",
       subtitle = format(Sys.time(), "%Y-%m-%d"), x = "", y = "")

nalje <- df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_deaths = cumsum(deaths)) %>% 
  filter(geoId %in% zemlje) %>% 
  group_by(geoId, dateRep) %>%
  summarise(cum_deaths = max(cum_deaths)) %>% 
  arrange(desc(dateRep)) %>%
  ungroup() %>% 
  slice(1:length(zemlje))

df %>%
  group_by(geoId) %>% 
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_deaths = cumsum(deaths)) %>% 
  filter(geoId %in% zemlje, cum_deaths > 0) %>%
  ggplot(aes(x = dateRep, y = cum_deaths)) +
  geom_point(alpha = .3) +
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")), se = FALSE) +
  geom_text(data = nalje, aes(label=cum_deaths), 
             x = -Inf, y = Inf, hjust=-2, vjust=2,
             inherit.aes = FALSE) +
  facet_grid(cols = vars(geoId), scales = "free_y") +
  labs(title = "smrti",
       subtitle = format(Sys.time(), "%Y-%m-%d"), x = "", y = "")
