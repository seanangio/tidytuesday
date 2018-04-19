library(tidyverse)
library(ggrepel)
library(readxl)
library(here)

# download file
if (!file.exists("week1/us_avg_tuition.xlsx")) {
    url <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/us_avg_tuition.xlsx"
    download.file(url, dest = "week1/us_avg_tuition.xlsx", mode = "wb") 
}
tuition <- read_xlsx(here("week1/us_avg_tuition.xlsx"))

# run get_regions.R first
states <- read_csv("week1/us_states_regions.csv")

# add abb and region variables
tuition <- tuition %>%
    left_join(states, by = c("State" = "state"))

# calculate and order region by mean tuition
region_ranks <- tuition %>%
    gather(key = "year", value = "tuition", -State, -abb, -region) %>%
    group_by(region) %>%
    summarize(tuition = mean(tuition)) %>%
    mutate(rank = min_rank(tuition)) %>%
    select(-tuition) %>%
    arrange(rank) %>%
    pull(region)

tuition$region <- factor(tuition$region, levels = region_ranks)

tidy_tuition <- tuition %>%
    gather(key = "year", value = "tuition", -State, -abb, -region)

last_year <- tidy_tuition %>%
    filter(year == "2015-16")

# mean tuition over time faceted by region
ggplot(tidy_tuition, 
       aes(x = year, y = tuition, group = abb, label = abb, color = abb)) +
        geom_line(alpha = 0.5) +
        facet_grid(~ region) +
        geom_text_repel(data = last_year, size = 3) +
        scale_y_continuous(labels = scales::dollar) +
        guides(color = FALSE) +
        theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.x = element_text(size = 7)) +
        labs(title = "Mean Tuition Over Time Faceted by Region", 
             x = "2004-05 to 2015-16", y = "",
             caption = "Source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/")

ggsave("week1/week1.png")    
