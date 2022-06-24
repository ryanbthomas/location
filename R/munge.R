
library(readxl)
library(dplyr)
library(tidyr)
library(rvest)

loc_file <- "raw-data/SUB-IP-EST2021-ANNRNK.xlsx"

viable_states <- c(
    "California", "Colorado", "Connecticut", "Illinois", "Indiana", "Maine", 
    "Massachusetts", "Michigan", "Minnesota", "New Hampshire", "New Jersey", 
    "New York", "Ohio", "Oregon", "Pennsylvania", "Rhode Island", "Vermont", 
    "Washington", "Wisconsin"
    )


all_locations <- read_xlsx(
    loc_file, 
    sheet = 1, 
    skip = 4, 
    col_names = c("rank", "city_state", "pop2019", "pop2020", "pop2021")
    )

viable_locations <- all_locations %>%
    tidyr::separate(
        col = "city_state", 
        into = c("city", "state"), 
        sep = ", "
        ) %>%
    dplyr::filter(state %in% viable_states) %>%
    mutate(
        state_abb = state.abb[match(state, state.name)]
    )

make_walkable_url <- function(city, state_abb) {
    # remove trailing city
    # replace spaces with underscores
    
    formatted_city <- city %>% 
        stringr::str_remove(" city$") %>%
        stringr::str_replace_all(" ", "_")
    
    paste0("https://www.walkscore.com/", state_abb, "/", formatted_city)
}

tmp <- viable_locations %>% 
    mutate(walkable_url = make_walkable_url(city, state_abb))
    
