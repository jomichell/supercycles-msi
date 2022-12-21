require(tidyverse)

                                        # Years for moving average calcuation
MOV_YEARS  <- 5

CEILINGS  <- c("cred_gdp", "ca_def", "ass_cap")
FLOORS    <- c("gdp_gr", "shr_gr", "emp")
CORRIDORS <- c("cpi_gr", "hpg_gr")
                                        # Prepare to calculate the MSI
                                        # Some indicators need transforming to
                                        # match our ceiling/floor definitions.
                                        # Drop any years which don't have all indicators


cty_data  <- read_csv("country_data.csv")


ds_trans  <- cty_data %>%
    select(-source) %>%
    pivot_wider(names_from = "series") %>%
    mutate(emp = 1-unemp,
           ca_def = -ca_gdp,
           ass_cap = 1/lev) %>%
    drop_na() %>%
    pivot_longer(!c(year, iso2), names_to = "series") %>%
    filter(!is.na(value))


add_labels  <- function(df,
                        ceilings = CEILINGS,
                        floors = FLOORS,
                        corridors = CORRIDORS) {

    ceilings = df %>%
        filter(series %in% CEILINGS) %>%
        mutate(type = "C")
    
    floors = df %>%
        filter(series %in% FLOORS) %>%
        mutate(type = "F")
    
    corridors = df %>%
        filter(series %in% CORRIDORS) %>%
        mutate(type = "O")

    ds_labelled  <- rbind(ceilings, floors, corridors) %>%
        filter(!is.na(type))

    return(ds_labelled)
}

                                        # Function to calculate the z index of a
                                        # vector
z_index <- function(x, type) {
    max_x  <- max(x,na.rm = TRUE)
    min_x  <- min(x,na.rm = TRUE)
    median_x  <- median(x,na.rm = TRUE)    
    
    numerator  <- case_when (
        type == "F" ~ max_x - x,
        type == "C" ~ min_x - x,
        type == "O" ~ median_x - x        
    )

    abs(numerator)/abs(max_x - min_x)
}


calc_msi  <- function(df) {
    msi <- df %>%
        group_by(iso2, series) %>%
        mutate(z_index = z_index(value, type)) %>%
        group_by(year, iso2, type) %>% 
        summarise(sub_avg = mean(z_index)) %>%
        summarise(msi_raw = 1 - mean(sub_avg)) %>%
        group_by(iso2) %>%
        mutate(msi_smoothed = TTR::SMA(msi_raw, n=MOV_YEARS)) %>%
        ungroup %>%
        arrange(iso2, year) %>%
        filter(!is.na(msi_smoothed))
    
    return(msi)
}


                                        # Raw dataset with variables labelled as
                                        # ceiling/floor/corridor

msi_raw  <- ds_trans %>% add_labels

                                        # Calculate the MSI

msi_df  <- filter(msi_raw, year<=2019) %>%
    calc_msi

                                        # Eyeball plot

ggplot(filter(msi_df, year>=1950),
       aes(x=year, y=msi_smoothed)) +
    geom_line() +
    facet_grid(vars(iso2), scales="free_y") +
    labs(x = "", y = "")


write_csv(msi_df, "msi.csv")



                                        # Calculate split MSI by year



msi_ic  <-msi_raw %>% filter((year <=1986 & iso2 != "GB") | (year <=1992 & iso2 == "GB")) %>%
    calc_msi %>%
    mutate(sc = "ic")

msi_fg  <- msi_raw %>% filter((year >=1983 & iso2 != "GB") | (year >=1989 & iso2 == "GB"),
                              year<=2019) %>%
    calc_msi %>%
    mutate(sc = "fg")

msi_split <- rbind(msi_ic, msi_fg)

write_csv(msi_split, "msi_split.csv")

                                        # eyeball plot

ggplot(filter(msi_split, year>=1950),
       aes(x=year, y=msi_smoothed, colour=sc)) +
    geom_line() +
    facet_grid(vars(iso2), scales="free_y") +
    labs(x = "", y = "")



                                        # Alternate specification:

msi_alt <- ds_trans %>%
    add_labels(ceilings = cat(CEILINGS, "ass_cap")) %>%
    filter(year<=2019) %>%
    calc_msi

write_csv(msi_alt, "msi_alt.csv")
                                        # eyeball plot

ggplot(filter(msi_df, year>=1950),
       aes(x=year, y=msi_smoothed)) +
    geom_line() +
    facet_grid(vars(iso2), scales="free_y") +
    labs(x = "", y = "")

