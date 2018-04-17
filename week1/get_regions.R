library(readr)

# download and unzip file
if (!file.exists("us_states.zip")) {
    url <- "http://www.whypad.com/wp-content/uploads/us_states.zip"
    download.file(url, dest = "us_states.zip", mode = "wb") 
    unzip("us_states.zip", exdir = "./")
}

states <- read_csv("us_states.csv", col_names = FALSE)
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
states$region <- vector("character", length = nrow(states))
states$region <- ifelse(
    states$abb %in% new_england, "New England",
      ifelse(states$abb %in% mid_atlantic, "Mid-Atlantic",
        ifelse(states$abb %in% en_central, "East North Central",
          ifelse(states$abb %in% wn_central, "West North Central",
            ifelse(states$abb %in% south_atlantic, "South Atlantic",
              ifelse(states$abb %in% south_atlantic, "South Atlantic",
                ifelse(states$abb %in% es_central, "East South Central",
                  ifelse(states$abb %in% ws_central, "West South Central",
                    ifelse(states$abb %in% mountain, "Mountain",
                      "Pacific")))))))))

# output a csv
write_csv(states, path = "us_states_regions.csv")
rm(list = ls())
