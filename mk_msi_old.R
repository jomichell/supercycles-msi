require(tidyverse)
require(TTR)

MOV_YEARS  <- 5

                                        # Drop Germany and GDP per capita
                                        # from the dataset -- these are #
                                        # currently unused.

df <- read_csv("raw_country_indices.csv") %>%
    filter(country!="germany") %>%
    select(-f_gdp_growth_percap) %>%
    arrange(country, year)

                                        # function to normalise a vector.
                                        # the formula depends on the
                                        # type of the variable:
                                        # (f)loor, (c)eiling or c(o)rridor

 z_index <- function(x, type) {
    max_x  <- max(x,na.rm = TRUE)
    min_x  <- min(x,na.rm = TRUE)
    median_x  <- median(x,na.rm = TRUE)    
    
    numerator  <- case_when (
        type == "f" ~ max_x - x,
        type == "c" ~ min_x - x,
        type == "o" ~ median_x - x        
    )

    abs(numerator)/abs(max_x - min_x)
}

                                        # Pivot to longer format,
                                        # use first character of 
                                        # variable name to determine 
                                        # type (see comment above)
                                        # calculate normalised
                                        # 'z-index' for each variable,
                                        # average over each variable type,
                                        # then average these sub-averages.
                                        # Finally apply moving average.

msi_df  <- df %>%
    pivot_longer(c(-year, -country)) %>%
    mutate(type = substr(name, 1, 1)) %>%
    group_by(country, name) %>%
    mutate(z_index = z_index(value, type)) %>%
    group_by(year, country, type) %>% 
    summarise(sub_avg = mean(z_index, na.rm = TRUE)) %>%
    summarise(msi_raw = 1 - mean(sub_avg, na.rm = TRUE)) %>%
    group_by(country) %>%
    mutate(msi_smoothed = SMA(msi_raw, n=MOV_YEARS)) %>%
    ungroup %>%
    arrange(country, year)

ggplot(filter(msi_df, year>=1950),
       aes(x=year, y=msi_smoothed)) +
    geom_line() +
    facet_grid(vars(country), scales="free_y") +
    labs(x = "", y = "")

## write_csv(msi_df, "msi.csv")


