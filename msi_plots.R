require(tidyverse)
require(lubridate)
require(ggalt)
require(gridExtra)


msi_df  <- read_csv("msi.csv") %>% mutate(sc = "both")

#    mutate(country = recode(iso2,
#                            CA = "canada",
#                            DE = "germany",
#                            FR = 
#
cycle_names = c("Industrial Capitalism","Financial Globalisation")
phase_names = c("Expansion","Maturity", "Crisis", "Genesis")

                                        # Manually code the dates for each phase

phases_data = tibble(cycle = rep(cycle_names, each = 4),
                     phase = rep(phase_names, times = 2),
                     "US"      = c(1962, 1969, 1974, 1979, 1986, 1999, 2008, 2013),
                     "GB"      = c(1963, 1969, 1974, 1979, 1992, 1998, 2008, 2013),
                     "CA"  = c(1967, 1969, 1974, 1979, 1986, 2004, 2008, 2013),
                     "FR"  = c(1967, 1969, 1974, 1979, 1986, 2000, 2008, 2013),
##                   "DE" = c(1971, 1971, 1974, 1979, 1986, 1999, 2008, 2013),
                     "IT"   = c(NA,   NA,   1974, 1979, 1986, 2000, 2008, 2013),
                     "JP"   = c(NA,   NA,   1974, 1979, 1985, 1988, 2008, 2013))

                                        # Dataframes with the cycle
                                        # and phase starts, ends and midpoints

phases <- phases_data %>%
    pivot_longer(c(-cycle, -phase), names_to = "iso2", values_to = "start_year") %>%
    group_by(iso2) %>%
    mutate(end_year = lead(start_year, default=2018),
           midpoint_phase = start_year + (end_year - start_year)/2) %>%    
    ungroup()

cycles <- phases %>%
    group_by(iso2, cycle) %>%
    mutate(start_year = min(start_year, na.rm=TRUE),
           midpoint_cycle = start_year + (max(end_year, na.rm=TRUE) - start_year)/2) %>%
    ungroup %>%
    select(cycle, iso2, start_year, midpoint_cycle) %>%
    distinct()

mk_graph  <- function(iso2_list,
                      year_labs = subset(phases, iso2=="US")$start_year,
                      y_limits = c(1965, 2019),
                      phase_label_y = 0.93,
                      cycle_label_y = 0.973) {
    
    msi_df_c <- msi_df %>% filter(iso2 %in% iso2_list)
    phases_c <- phases %>% filter(iso2 %in% iso2_list)
    cycles_c <- cycles %>% filter(iso2 %in% iso2_list)

    ggplot(data = msi_df_c,
           aes(x = year, y = msi_smoothed, colour = sc)) +
        geom_rect(data = phases_c,
                  mapping = aes(xmin = start_year, xmax = end_year,
                                ymin = YMIN, ymax = YMAX,
                                fill=phase),
                  alpha=0.3,
                  inherit.aes = FALSE) +
        geom_text(data = phases_c,
                  mapping = aes(x=midpoint_phase, y=phase_label_y,
                                label=phase),
                  size=2.2,
                  inherit.aes = FALSE) +
        geom_label(data = cycles_c,
                   mapping = aes(x=midpoint_cycle, y=cycle_label_y,
                                 label=cycle),
                   size=2.2,
                   inherit.aes = FALSE) +
        geom_segment(data = subset(cycles_c, cycle=="Financial Globalisation"),
                     mapping = aes(x=start_year, xend=start_year, y=YMIN, yend=YMAX), size=0.5,
                     colour="black", linetype="dotted")+
        ##        geom_line(colour="darkred", lwd=0.8) +
        geom_line(lwd=0.8) +        
        scale_x_continuous(expand = c(0, 0), breaks =  year_labs, limits = y_limits) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .01)),
                           limits = c(YMIN, YMAX),
                           breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) +
        scale_fill_manual(values=pallette) +
        scale_colour_manual(values=c("darkred", "black")) +
        labs(x = "", y = "Macrofinancial Stability Index") +
        theme_light(base_size=8) +
        theme(plot.title=element_text(color="black"),  # title
              plot.subtitle=element_text(color="gray20"),  # subtitle
              plot.caption=element_text(color="gray20",
                                        size=6),
              axis.title.x=element_text(color="gray10"),
              axis.title.y=element_text(color="gray20"),
              axis.ticks = element_line(color="gray30", size=0.5),
              strip.text.x = element_text(size = 9, color = "grey10"),
              strip.background = element_rect(color="white", fill="white", size=0.1),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),          
              legend.position = "none",
              panel.border = element_blank(),
              axis.title=element_text(size=8))
}


YMIN  <- 0.4
YMAX  <- 1.0
                                        # Construct the timeline

ymind  <- ymd(19620101)
ymaxd  <- ymd(20180101)
limits = c(1962, 2019)

data <- tribble( ~start_date, ~event, ~displ, ~hjust, ~cty,
                ymd(19710101), "1971 BoE Competition and Credit Control", 0.7, "inward", "UK",
                ymd(19730101), "1973 Dollar gold convertibility ends", 0.55, "inward", "US",
                ymd(19740101), "1975 US bank failures", 0.4, "inward", "US",
                ymd(19800301), "1980 Volker rate rise", 0.3, "inward", "US",
                ymd(19800331), "1980 Depository Institutions \nDeregulation and Monetary Control Act", -0.3, "outward", "US",
                ymd(19830301), "1983 London Big Bang", 0.15, "inward", "UK",
                ymd(19920916), "1992 Pound exits ERM\n", -0.3, "inward", "UK",
##              ymd(19840101), "Continental Illinois", 0.5, "inward",
##              ymd(19870101), "Stock market crash", -0.5, "inward",
                ymd(19910101), "1991 Salomon Brothers scandal", 0.55, "outward", "US",
                ymd(19970701), "1997 Asian crisis", 0.15, "inward", "global",               
                ymd(19980817), "1998 Russian default", 0.35, "inward", "global",
                ymd(19980923), "1998 LTCM Bailout\n", -0.4, "inward", "US",           
                ymd(19991112), "1999 Repeal of Glass-Steagall Act", 0.35,  "outward", "US",
##              ymd(20070901), "Northern Rock", -0.5, "inward",
                ymd(20080815), "2008 Lehman Brothers failure", 0.2, "outward", "US",
                ymd(20091101), "2009 Bank of England starts\nquantitative easing", -0.3, "inward", "UK")
##              ymd(20200101), "Corona", 0.5)

date_breaks  <- c(ymd(19620101),
                  ymd(19690101),                  
                  ymd(19740101),
                  ymd(19790101),
                  ymd(19860101),
                  ymd(20000101),
                  ymd(20080101),
                  ymd(20130101),
                  ymd(20190101))

                                        # Function to shift x-axis to 0
shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, colour = "grey30") +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
}

                                        # Conditionally set whether text
                                        # will be above or below the point
vjust = ifelse(data$displ > 0, -1, 1.4)

tl_raw <- data %>% 
    ggplot(aes(start_date, displ, colour=cty)) +
    geom_lollipop(point.size = 1) +
    geom_text(aes(x = start_date, y = displ, label = event), data = data,
              hjust = data$hjust, vjust = vjust, size = 1.7, lineheight = 1.0) +
    scale_x_date(limits = c(ymind, ymaxd),
                 date_labels = "%Y",
                 breaks = date_breaks,                 
                 expand = c(0,0)) +
    scale_y_continuous(limits = c(-0.7, 0.8)) +
    scale_colour_manual(values=c("darkblue", "darkred", "black")) +
    labs(x = "", y = "") +
    theme(legend.position = "none",
        axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(size = 7),
          plot.margin = margin(0, 0.14, 0, 0.85, "cm"),
          panel.background = element_blank())
tl <- shift_axis(tl_raw, ymind, ymaxd)

pallette <- c("Expansion" = "grey85",
              "Maturity" = "grey70",
              "Crisis" = "red4",
              "Genesis" = "grey95",
              " " = "grey90",
              "Exp" = "grey90") ## <- fudge to cope with non-labelled start


                                        # make the UK and US graphs and
                                        # combine them with the timeline

uk_graph  <-  mk_graph("GB", y_limits=limits,
                       year_labs = subset(phases, iso2=="GB")$start_year) +
    annotate("text", x = 1966, y = 0.65, label = "UK", size=10, alpha = 0.2)

us_graph  <- mk_graph("US", y_limits=limits) + 
    annotate("text", x = 1966, y = 0.65, label = "US", size=10, alpha = 0.2)

g1 <- arrangeGrob(us_graph, tl, uk_graph, nrow = 3, heights = c(2,0.8,2))

ggsave("Fig1-US-UK.png", g1, width=6, height=6, type="cairo", dpi=300)

                                        # graph of four countries


phases  <- phases %>%
    mutate(phase = replace(phase, phase=="Expansion" &
                                  cycle == "Industrial Capitalism" &
                                  iso2 %in% c("FR", "CA"), " "),
           phase = replace(phase, phase=="Expansion" &
                                  cycle == "Financial Globalisation" &
                                  iso2 %in% c("JP"), "Exp"))

cty_labs <- c("Canada", "Italy", "France", "UK", "Japan", "US")

names(cty_labs) <- c("CA", "IT", "FR", "GB", "JP", "US")

g2  <- mk_graph(c("CA", "IT", "FR", "JP"),
                y_limits = c(1967, 2019),
                cycle_label_y = 0.45) +
    facet_wrap(~iso2,
               labeller = labeller(iso2 = cty_labs),
               nrow=4)

ggsave("Fig2.png", g2, width=6, height=6, type="cairo", dpi=300)

# Split MSI

msi_df  <- read_csv("msi_split.csv")

a1  <- mk_graph(c("US", "GB", "CA", "IT", "FR", "JP"),
                y_limits = c(1967, 2019),
                cycle_label_y = 1.1) +
    facet_wrap(~iso2,
               labeller = labeller(iso2 = cty_labs),
               nrow=4)

ggsave("FigA1.png", a1, width=6, height=6, type="cairo", dpi=300)

# Alternative MSI
msi_df  <- read_csv("msi_alt.csv") %>% mutate(sc = "both")

a2  <- mk_graph(c("US", "GB", "CA", "IT", "FR", "JP"),
                y_limits = c(1967, 2019),
                cycle_label_y = 1.1) +
    facet_wrap(~iso2,
               labeller = labeller(iso2 = cty_labs),
               nrow=4)

ggsave("FigA2.png", a2, width=6, height=6, type="cairo", dpi=300)
