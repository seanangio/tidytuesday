library(tidyverse)
library(readxl)
library(here)
library(grid)

# download and read in file
if (!file.exists("week2/tidy_tuesday_week2.xlsx")) {
    url <- "https://github.com/rfordatascience/tidytuesday/raw/master/data/tidy_tuesday_week2.xlsx"
    download.file(url, dest = "week2/tidy_tuesday_week2.xlsx", mode = "wb") 
}
nfl_raw <- read_xlsx(here("week2", "tidy_tuesday_week2.xlsx"))

positions_abb <- tribble(
    ~position, ~abb,
    "Running Back", "RB",
    "Quarterback", "QB",
    "Offensive Lineman", "OL",
    "Tight End", "TE",
    "Wide Receiver", "WR",
    "Cornerback", "CB",
    "Defensive Lineman", "DE",
    "Linebacker", "LB",
    "Safety", "S",
    "Special Teamer", "ST"
)

nfl_tidy <- nfl_raw %>%
    gather(key = "position", value = "salary", -year) %>%
    left_join(positions_abb, by = "position") %>%
    mutate(abb = fct_relevel(abb, positions_abb$abb),
           year = year %% 100,
           salary = salary / 1e6,
           off_def = factor(case_when(
               abb %in% c("OL","QB","RB","TE","WR") ~ "Offense",
               TRUE ~ "Defense"), level = c("Offense", "Defense")))

# filter only top 16 in each position
top16 <- nfl_tidy %>%
    group_by(year, position) %>%
    top_n(16, salary) %>%
    ungroup()

# dataframe for Trend annotation
ann_text <- data.frame(
    label = c("Trend", rep("")),
    abb = factor("RB", levels = c("RB","QB","OL","TE","WR","CB","DE","LB","S","ST"))
)

# dataframe for line segment annotation
segm_df <- data.frame(
    x = 12, y = 13, xend = 12, yend = 8,
    abb = factor("RB", levels = c("RB","QB","OL","TE","WR","CB","DE","LB","S","ST"))
)

p <- ggplot(top16, aes(x = year, y = salary)) +
    geom_jitter(alpha = 0.3) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    geom_smooth(data = filter(top16, abb == "RB"), 
                method = "loess", color = "black", lwd = 1.5, se = FALSE) +
    facet_wrap(~ abb, nrow = 2) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "'")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(-1, 26), 
                       labels = c("0", "5", "10", "15", "20", "$25m")) +
    labs(x = "", y = "Average cap value",
         title = "The average pay for top running backs has stalled",
         subtitle = "Average cap value of 16 highest-paid players in each position",
         caption = "Source: ESPN Stats & Information Center") +
    geom_text(data = ann_text, aes(x = -Inf, y = -Inf, label = label), 
              hjust = -0.8, vjust = -16) +
    geom_segment(data = segm_df, aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

# make RB facet bold: https://stackoverflow.com/questions/46905774/ggplot-using-strip-text-x-element-text-for-making-only-one-element-of-the-fac
grob <- ggplotGrob(p)
k <- 57
grob$grobs[[k]]$grobs[[1]]$children[[2]]$children[[1]]$gp$font <- as.integer(2)
attr(grob$grobs[[k]]$grobs[[1]]$children[[2]]$children[[1]]$gp$font,"names") <- "bold"

# save output
png("week2/week2.png",width = 1000, height = 600, units = "px") 
grid.draw(grob) 
dev.off()

# find missing data in each column
#top16 %>%
#    map_df(function(x) sum(is.null(x))) %>%
#    gather(feature, num_nulls) 
