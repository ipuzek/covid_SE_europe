library(zoo)
library(gganimate)

# zemlje <- c("HR", "SI")
# zemlje <- c("IT", "ES", "KR", "IR")
# zemlje <- c("HR", "SI", "DK", "AT", "DE")
zemlje <- c("HR", "SI", "AT", "SK")

add_numbers <- function(df){
  df.split <- split(df, df$geoId)
  df.arr_by_date <- lapply(df.split, arrange, dateRep)
  df.split.nos <- lapply(df.arr_by_date, function(x) tibble::add_column(x, nos = 1:nrow(x)))
  dplyr::bind_rows(df.split.nos)
}

vizz_anim <- df %>%
  add_numbers() %>%
  group_by(geoId) %>%
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(cum_cases > 0) %>%
  mutate(
    ma3 = rollapply(cases, 3, mean, align = 'right', fill = NA),
    ma5 = rollapply(cases, 5, mean, align = 'right', fill = NA),
    ma7 = rollapply(cases, 7, mean, align = 'right', fill = NA)
  ) %>%
  ungroup() %>% 
  filter(geoId %in% zemlje) %>%
  filter(dateRep > "2020-02-26") %>% 
  ggplot(aes(x = cum_cases, y = ma5, colour = geoId)) +
  geom_line(aes(y = ma5, colour = geoId)) +
  # geom_line(aes(y = ma7, colour = geoId), size = .1, linetype = 2) +
  scale_x_continuous(limits = c(10, NA), trans = "log10") +
  scale_y_continuous(limits = c(10, NA), trans = "log10") +
  transition_reveal(along = dateRep)

animate(vizz_anim, fps = 20, duration = 30)
animate(vizz_anim, fps = 20, duration = 30, renderer = ffmpeg_renderer())
