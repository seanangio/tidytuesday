library(readr)
library(here)

# download and unzip file
if (!file.exists("week1/us_states.zip")) {
    url <- "http://www.whypad.com/wp-content/uploads/us_states.zip"
    download.file(url, dest = "week1/us_states.zip", mode = "wb") 
    unzip("week1/us_states.zip", exdir = "./")
}

states <- read_csv(here("week1/us_states.csv"), col_names = FALSE)
states <- states[,-1]
names(states) <- c("state", "abb")

# define regions
new_england <- c("CT","ME","MA","NH","RI","VT")
mid_atlantic <- c("NJ","NY","PA")
en_central <- c("IL","IN","MI","OH","WI")
wn_central <- c("IA","KS","MN","MO","NE","ND","SD")
south_atlantic <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV")
es_central <- c("AL","KY","MI","TN")
ws_central <- c("AK","LA","OK","TX")
mountain <- c("AZ","CO","ID","MT","NV","NM","UT","WY")
pacific <- c("AK","CA","HW","OR","WA")

# assign regions to abbreviations
states <- states %>%
    mutate(
        region = case_when(
            states$abb %in% new_england ~ "New England",
            states$abb %in% mid_atlantic ~ "Mid-Atlantic",
            states$abb %in% en_central ~ "East North Central",
            states$abb %in% wn_central ~ "West North Central",
            states$abb %in% south_atlantic ~ "South Atlantic",
            states$abb %in% es_central ~ "East South Central",
            states$abb %in% ws_central ~ "West South Central",
            states$abb %in% mountain ~ "Mountain",
            TRUE ~ "Pacific"
        )
    )

# output a csv
write_csv(states, path = "week1/us_states_regions.csv")
rm(list = ls())
