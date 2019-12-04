library(tidyverse)

res <-
  tibble(number = 0:999999) %>%
  mutate(
    digit_1 = floor(number / 10^5 %/% 1),
    digit_2 = floor((number - floor(number / 10^5)*10^5) / 10^4 %/% 1),
    digit_3 = floor((number - floor(number / 10^4)*10^4) / 10^3 %/% 1),
    digit_4 = floor((number - floor(number / 10^3)*10^3) / 10^2 %/% 1),
    digit_5 = floor((number - floor(number / 10^2)*10^2) / 10^1 %/% 1),
    digit_6 = floor((number - floor(number / 10^1)*10^1) %/% 1),
  ) %>%
  mutate(
    eq_12 = digit_1 == digit_2,
    eq_23 = digit_2 == digit_3,
    eq_34 = digit_3 == digit_4,
    eq_45 = digit_4 == digit_5,
    eq_56 = digit_5 == digit_6,
  ) %>%
  mutate(
    any_pair = eq_12 | eq_23 | eq_34 | eq_45 | eq_56
  ) %>%
  filter(
    any_pair == TRUE
  ) %>%
  filter(
    digit_1 <= digit_2,
    digit_2 <= digit_3,
    digit_3 <= digit_4,
    digit_4 <= digit_5,
    digit_5 <= digit_6,
  ) %>%
  filter(
    number > 307237,
    number < 769058
  ) %>%
  nrow()

