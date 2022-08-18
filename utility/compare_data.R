require(tidyverse)

new <- read_csv("cty_data_downloaded.csv")
old <- read_csv("raw_country_indices.csv")

old_ds  <- old %>%
    select(-f_gdp_growth_percap) %>%
                                        #    mutate(unemp = 100-f_employment) %>%
    rename(emp = f_employment,
           gdp_growth = f_gdp_growth,
           share_growth = f_shares_growth,
           credit_gdp = c_credit,
           ca_gdp = c_ca_deficit,
           cpi = o_inflation,
           hpnomg = o_house_price) %>%
    mutate(ca_gdp = 1- ca_gdp) %>%
    select(-f_employment) %>%
    pivot_longer(!c(year, country), names_to = "series") %>%
    filter(!is.na(value)) %>%
    mutate(value = 0.01 * value,
    mutate(origin = "old",
           source = NA,
           iso2 = recode(country,
                         us = "US",
                         uk = "GB",
                         canada = "CA",
                         france = "FR",
                         germany = "DE",
                         italy = "IT",
                         japan = "JP")) %>%
    select(-country)

both <- new %>%
    mutate(origin = "new") %>%
    rbind(old_ds)

ggplot(filter(both, year>=1950),
       aes(x=year, y=value, colour=origin)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(vars(series), vars(iso2), scales="free_y") +
    labs(x = "", y = "")
