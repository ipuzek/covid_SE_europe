# ajmo skužite exp

library(magrittr)
library(broom)
library(ggplot2)
library(dplyr)

data_exp <- readxl::read_xlsx("exp_data_za_test.xlsx")

data_exp %>% 
  ggplot(aes(x = br_dana)) +
  geom_point(aes(y = dana_5)) +
  geom_point(aes(y = dana_4))

lm(log2(dana_5) ~ br_dana - 1, data = data_exp) %>% 
  tidy %>% 
  pull(estimate) %>% 
  extract(1) %>% 
  # raise_to_power(-1) %>% 
  round(2)
