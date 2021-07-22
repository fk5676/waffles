# data wrangling
library(tidyverse)

# waffle plot
library(waffle)

# combining graphs
library(patchwork)

# text magic
library(ggtext)

# set rows
rows = 50

# Change your decimal , to . or set R to deal with it!!!

# data should be int as they are counts
# in the processing below values below unity
# will not be "seen" in this setup unless you
# apply a multiplier 10x 100x and to scale
# the graphs accordingly. Values are rounded
# and used as such (not multiplied)
df <- read.table("referentiedosis.csv",
                 sep = ",",
                 header = TRUE,
                 stringsAsFactors = FALSE) %>%
  mutate(
    across(starts_with("PF"), ~ 1 *.x), # multiplier
    across(starts_with("PF"), round)
  )

# fill in NA values again NA counts doesn't work
df[is.na(df)] <- 0

waffleplot <- function(df){
  
  # split out two datasets
  efsa2008 <- df %>%
    filter(RfD == "EFSA 2008") %>%
    select(-RfD) %>%
    as.vector()
  
  efsa2018 <- df %>%
    filter(RfD == "EFSA 2018") %>%
    select(-RfD) %>%
    as.vector()
  
  epa <- df %>%
    filter(RfD == "EPA 2016") %>%
    select(-RfD) %>%
    as.vector()
  
  rivm <- df %>%
    filter(RfD == "RIVM 2016") %>%
    select(-RfD) %>%
    as.vector()
  
  efsa2020 <- df %>%
    filter(RfD == "EFSA 2020") %>%
    select(-RfD) %>%
    as.vector()
  
  colours <- c("black","grey","yellow")
  
  # keep rows the same, play with the SIZE
  # of the squares to make things proportional
  # in combination with a patchwork composite
  p1 <- waffle::waffle(
    efsa2008,
    rows = rows,
    size = 2,
    keep = TRUE,
    colors = colours,
    legend_pos = "none",
    title = "EFSA 2008",
    flip = TRUE)
  
  p2 <- waffle::waffle(
    efsa2018,
    rows = rows,
    size = 2,
    keep = TRUE,
    colors = colours,
    legend_pos = "none",
    title = "EFSA 2018",
    flip = TRUE)
  
  p3 <- waffle::waffle(
    epa,
    rows = rows,
    size = 2,
    keep = TRUE,
    colors = colours,
    legend_pos = "none",
    title = "EPA 2016",
    flip = TRUE)
  
  p4 <- waffle::waffle(
    rivm,
    rows = rows,
    size = 2,
    keep = TRUE,
    colors = colours,
    legend_pos = "none",
    title = "RIVM 2016",
    flip = TRUE)
  
  p5 <- waffle::waffle(
    efsa2020,
    rows = rows,
    size = 2,
    keep = TRUE,
    colors = colours,
    legend_pos = "none",
    title = "EFSA 2020",
    flip = TRUE)
  
  print("creating plot")
  
  # patchwork composite including some filler
  # values for titles and subtitles
  p <- p1 + p3 + p4 + p2 + p5 + plot_layout(nrow = 5,
                                            guides = "auto") +
    plot_annotation(title = "Exposure standards over time",
                    subtitle = "<span>
                    Decreasing values for PFOS
                    (<span style='color:#000000;'>\u25a0</span>) and 
                    PFOA
                    (<span style='color:#dddddd;'>\u25a0</span>) over time,<br> with
                    the EFSA 2020 expressed as a pooled value of total loading
                    of PFOS + PFOA + PFHxS + PFNA 
                    (<span style='color:#fff000;'>\u25a0</span>)
                    </span>",
                    caption = "Data Source: where you got it from"
    ) &
    theme(plot.margin = margin(5, 5, 5, 5),
          legend.position = "none",
          plot.title = element_markdown(size = 30),
          plot.subtitle = element_markdown(size = 18),
          plot.caption = element_text(
            size = 8,
            color = "grey"
          ))
  }

p <- waffleplot(df = df)

ggsave("waffleplot.png", width = 15, height = 15)
