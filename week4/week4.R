library(tidyverse)
library(modelr)
library(here)
library(ggrepel)
library(scales)

# download and read in file
if (!file.exists("week4/week4_australian_salary.csv")) {
    url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week4_australian_salary.csv"
    download.file(url, dest = "week4/week4_australian_salary.csv", mode = "wb") 
}
salary <- read_csv(here("week4", "week4_australian_salary.csv"))

salary$gender <- as.factor(salary$gender)

# spread income into male and female variables; mutate pay_gap
spread_sal <- salary %>%
    select(occupation, gender, average_taxable_income) %>%
    spread(gender, average_taxable_income) %>%
    filter(!is.na(Female), !is.na(Male)) %>%
    mutate(pay_gap = Male - Female) %>%
    arrange(pay_gap) %>%
    mutate(pay_gap_d = dollar(pay_gap))

# add predictions of simple best fit line
m1 <- lm(Male ~ Female, data = spread_sal)
spread_sal <- spread_sal %>%
    add_predictions(m1, "m1_pred") 

ggplot(data = spread_sal) +
    geom_point(aes(x = Female, y = Male), alpha = 0.1) +
    geom_line(aes(x = Female, y = Female), color = "red") +
    geom_line(aes(x = Female, y = m1_pred), color = "blue") +
    geom_text_repel(data = filter(spread_sal, pay_gap > 200000 |
                                      pay_gap < -50000), 
                    aes(x = Female, y = Male, label = occupation), size = 3) +
    geom_text(x = 350000, y = 365000, label = "Equal Pay Line", size = 3) +
    geom_text(x = 350000, y = 565000, label = "Best Fit Line", size = 3) +
    geom_segment(data = tail(spread_sal,1), 
                 aes(x = Female, y = Male, xend = Female, yend = Female)) +
    geom_label(data = tail(spread_sal,1), 
               x = 220000, y = 250000, aes(label = pay_gap_d), size = 3) +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Comparison of Average Taxable Income in Australia between Sexes by Occupation",
         subtitle = "Points above the red line indicate occupations where male average taxable income exceeded that of females",
         x = "Average Female Salary", y = "Average Male Salary", caption = "Data: data.gov.au")

ggsave("week4/week4.png")
