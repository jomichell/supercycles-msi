require(tidyverse)
require(readxl)
require(httr)
                                        # needs 'rsdmx' and 'fredr' packages installed

                                        # List of countries we want to download data for

iso3_ctys  <- c("USA", "GBR", "CAN", "FRA", "DEU", "ITA", "JPN")
iso2_ctys  <- c("US", "GB", "CA", "FR", "DE", "IT", "JP")


                                        # Set the Fred API key

                                        # Insert your Fred API key here


fredr::fredr_set_key("")


                                        #
                                        # Load the Jorda et al dataset
                                        # and calculate ratios
                                        #


JST_URL = "https://www.macrohistory.net/app/download/9834512569/JSTdatasetR6.xlsx"

httr::GET(JST_URL,
          write_disk(tf <- tempfile(fileext = ".xlsx")))

jst <- read_excel(tf) %>%
    filter(iso %in% iso3_ctys) %>%
    mutate(cred_gdp = tloans/gdp,
           ca_gdp = ca/gdp,
           unemp = unemp/100,
           lev = lev/100,
           hpg_gr = hpnom/lag(hpnom)-1,
           source = "jorda")  %>%
#    select(year, iso, gdp, cpi, ca, unemp, tloans, lev) %>%
    select(year, iso, source, unemp,
           ca_gdp, cred_gdp, lev, hpg_gr) %>%    
    pivot_longer(!c(year, iso, source), names_to = "series") %>%
    rename(iso3 = iso) %>%
    arrange(iso3, series, year) %>%
    filter(!(iso3=="ITA" & series=="cred_gdp" & year >=2018))

                                        # Helper function to do request
                                        # from an SDMX API and return as df

get_data  <- function(base_url, cty_codes) {
    url  <-  sprintf(base_url, paste(cty_codes, collapse="+"))
    rsdmx::readSDMX(url) %>% as_tibble()
}

clean_oecd <- function(df) {
    df %>%
        rename(iso3 = LOCATION,
               year = obsTime,
               value = obsValue) %>%
        mutate(source = "oecd") %>%
        select(any_of(c("iso3", "year", "value", "VARIABLE", "source")))
}

clean_fred  <- function(df) {
    df %>% select(date, value) %>%
        mutate(year = format(date, format="%Y"),
               source = "fred") %>%
        select(!date)
    }

                                        # gdp growth
GDP_URL = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/SNA_TABLE1/%1$s.B1_GE.G/all"

gdp_growth_oecd  <- get_data(GDP_URL, iso3_ctys) %>%
    clean_oecd %>%
    mutate(series = "gdp_gr") 

                                        # add USA from FRED
gdp_growth_fred  <- fredr::fredr(series_id = "A191RL1A225NBEA", frequency="a") %>%
    select(date, value) %>%
    clean_fred %>%
    mutate(series = "gdp_gr",
           iso3 = "USA")

gdp_growth  <- 
    rbind(gdp_growth_oecd, gdp_growth_fred) %>%
    mutate(value = 0.01 * value)

                                        # labour force
                                        # calculate unemployment rate and
                                        # employment rate

LFS_URL = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EO/%1$s.ET+UNR+POP1574.A/all"

lfs_oecd_raw  <- get_data(LFS_URL, iso3_ctys)

lfs_oecd <- lfs_oecd_raw %>%
    clean_oecd %>%
    pivot_wider(names_from = VARIABLE) %>%
    mutate(unemp = UNR/100,
           emp = ET/POP1574) %>%
    select(iso3, year, source, unemp, emp) %>%
    pivot_longer(!c(iso3, year, source), names_to = "series") %>%
    arrange(iso3, series, year)
    
us_epop  <- fredr::fredr(series_id = "LNS12300060", frequency="a") %>%
    clean_fred %>%
    mutate(series = "emp",
           iso3 = "USA",
           value = value/100)

us_unemp  <- fredr::fredr(series_id = "UNRATE", frequency="a") %>%
    clean_fred %>%
    mutate(series = "unemp",
           iso3 = "USA",
           value = value/100)


lfs <- lfs_oecd %>%
    rbind(us_epop) %>%
    rbind(us_unemp)

                                        # share price growth

SHARES_URL = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/KEI/SPASTT01.%1$s.GP.A/all"

shares <- get_data(SHARES_URL, iso3_ctys) %>%
    clean_oecd %>%
    mutate(series = "shr_gr",
           value = value * 0.01)

                                        # Credit to GDP from BIS
                                        # library(rsdmx)

                                        # to get list of SDMX dataflows from BIS use:
                                        # https://stats.bis.org/api/v1/dataflow/all/all/latest?references=none&detail=full

                                        # revelant dataflow is "WS_TC" (webstats total credit"?)
                                        # options are countr(y/ies).borrowing sector.lendint sector.valuation.unit_type.tc_adjust

CREDIT_URL = "https://stats.bis.org/api/v1/data/WS_TC/Q.%1$s.P.A.M.770.A/all?detail=dataonly"

credit_raw  <- get_data(CREDIT_URL, iso2_ctys)

credit  <- credit_raw %>%
    separate(TIME_PERIOD, into = c("year", "quarter")) %>%
    filter(quarter=="Q4") %>%
    rename(iso2 = BORROWERS_CTY,
           value = OBS_VALUE) %>%
    select(iso2, year, value) %>%
    mutate(series = "cred_gdp",
           source = "bis",
           value = as.double(value)/100)

                                        # Current account as % of GDP
CA_URL = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_BOP6/B6BLTT02.%1$s.STSA.A/all"

ca <- get_data(CA_URL,
               iso3_ctys) %>%
    clean_oecd %>%
    mutate(series = "ca_gdp",
           value = value/100)

INFLATION_URL = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/PRICES_CPI/%1$s.CPALTT01.GY.A/all"

inflation  <- get_data(INFLATION_URL,iso3_ctys) %>%
    clean_oecd %>%
    mutate(series = "cpi_gr",
           value = value/100)

HOUSE_PRICE_URL = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HOUSE_PRICES/%1$s.HPI/all"

house_idx_raw  <-  get_data(HOUSE_PRICE_URL, iso3_ctys)

house_price_gr  <- house_idx_raw %>%
    rename(LOCATION = COU) %>%
    clean_oecd %>%
    separate(year, into = c("year", "quarter")) %>%
    filter(is.na(quarter)) %>%
    mutate(series = "hpg_gr") %>%
    select(!quarter) %>%
    group_by(iso3) %>%
    arrange(year) %>%
    mutate(growth = value/lag(value)-1) %>%
    ungroup %>%
    select(!value) %>%
    rename(value = growth)

                                        # Truncating the ISO3 code is naughty
                                        # but works for our sample of countries
dataset  <- jst %>%
    rbind(gdp_growth, lfs, shares, ca, inflation, house_price_gr) %>%
    mutate(iso2 = substr(iso3, 1,2)) %>%
        select(!iso3) %>%
    rbind(credit) %>%
    mutate(year = as.integer(year),
           value = as.double(value))



                                        #
                                        # Plot to eyeball the series
                                        #


ggplot(filter(dataset, year>=1950),
       aes(x=year, y=value, colour=source)) +
    geom_line() +
    facet_grid(vars(series), vars(iso2), scales="free_y") +
    labs(x = "", y = "")

##ggsave("/tmp/dataset.png", type = "cairo")


                                        #
                                        # Filter down to single observations
                                        #

                                        # OECD plus Jorda

ca_unemp_hpg_gr  <- dataset %>%
    filter(series %in% c("ca_gdp", "unemp", "emp", "hpg_gr", "lev")) %>%
    pivot_wider(names_from = source) %>%
    mutate(value = ifelse(is.na(oecd), jorda, oecd)) %>%
    mutate(source = ifelse(is.na(oecd), "jorda", "oecd")) %>%
    select(-jorda, -oecd, -fred) %>%
    filter(!is.na(value))

                                        # OECD plus Fred
gdp_growth  <- dataset %>%
    filter(series == "gdp_gr") %>%
           pivot_wider(names_from = source) %>%
    mutate(value = ifelse(is.na(fred), oecd, fred),
           source = ifelse(is.na(fred), "oecd", "fred")) %>%
    select(-fred, -oecd) %>%
    filter(!is.na(value))

                                        # BIS
credit <- dataset %>%
    filter(series == "cred_gdp", source == "bis")

dataset_filtered  <-
    jst %>%
    filter(series %in% c("cpi_gr", "shr_gr")) %>%
    rbind(shares) %>%
    rbind(inflation) %>%
    mutate(iso2 = substr(iso3, 1,2)) %>%
        select(!iso3) %>%
    rbind(gdp_growth) %>%
    rbind(ca_unemp_hpg_gr) %>%
    rbind(credit) %>%
    mutate(year = as.integer(year)) %>%
    select(year, iso2, everything())



ggplot(filter(dataset_filtered, year>=1950),
       aes(x=year, y=value, colour=source)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(vars(series), vars(iso2), scales="free_y") +
    labs(x = "", y = "")


## ggsave("/tmp/dataset_filtered.png", type = "cairo")

write_csv(dataset_filtered, "country_data.csv")


