
library(rvest)

loc_file <- "raw-data/SUB-IP-EST2021-ANNRNK.xlsx"

load_viable_states <- function(viable_states_file, sheet = 1) {
    tmp <- read_xlsx(
        viable_states_file,
        sheet = sheet,
        col_names = c("state", "society_risk", "climate_risk", "overall_risk")
    )
    
    tmp[, "state", drop = TRUE]
}

clean_up_city_name <- function(city) {
    city %>% 
        stringr::str_remove(" city$") %>%
        stringr::str_replace_all(" ", "_")
}

make_walkable_url <- function(city, state_abb) {
    # remove trailing city
    # replace spaces with underscores
    
    formatted_city <- clean_up_city_name(city)
    
    paste0(state_abb, "/", formatted_city)
}


create_viable_cities <-  function(all_cities_file, viable_states_file) {

    viable_states <- load_viable_states(viable_states_file)
    
    all_cities <- read_xlsx(
        all_cities_file, 
        sheet = 1, 
        skip = 4, 
        col_names = c("rank", "city_state", "pop2019", "pop2020", "pop2021")
    )
    
    all_cities %>%
        tidyr::separate(
            col = "city_state", 
            into = c("city", "state"), 
            sep = ", "
        ) %>%
        dplyr::filter(state %in% viable_states) %>%
        mutate(
            state_abb = state.abb[match(state, state.name)],
            walkable_url_path = make_walkable_url(city, state_abb),
            id = dplyr::row_number()
        ) |>
        dplyr::select(
            c("id", 
              "rank", 
              "city", 
              "state", 
              paste0("pop", 2019:2021), 
              "state_abb", 
              "walkable_url_path"
             )
        )
    
}

