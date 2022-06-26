
find_redfin_city_url <- function(links, city_path) {
    city_path <- stringr::str_replace_all(city_path, "_", "-")
    pattern <- paste0("redfin\\.com/city/[0-9]+/",city_path, "/apartments-for-rent")
    city_url_for_rent_links <- links |>
        stringr::str_subset(pattern)
    
    stringr::str_remove(city_url_for_rent_links[1], "apartments-for-rent$")
}

find_redfin_neighborhood_url <- function(nbrd_paths) {
    result <- rep(NA_character_, length(nbrd_paths))
    #browser()
    for (i in seq_along(nbrd_paths)) {
        walkable_nbhrd <- read_html(paste0("https://www.walkscore.com/", nbrd_paths[i]))
    
        pattern <- paste0(
            "redfin\\.com/neighborhood/[0-9]+", 
            stringr::str_sub(nbrd_paths[i], 1, 2)
            )
    
        tmp <- walkable_nbhrd |>
            html_elements("a") |>
            html_attr("href") |>
            stringr::str_subset(pattern)
    
        result[i] <- stringr::str_remove(tmp[1], "apartments-for-rent$")
    }
    
    result
}

fetch_neighborhoods_row <- function(row_of_df) {
    fetch_neighborhoods(row_of_df[["walkable_url_path"]])
}

fetch_neighborhoods <- function(url_path) {
    print(url_path)
    walkable <- rvest::read_html(paste0("https://www.walkscore.com/", url_path))
    
    neighborhoods <- walkable |>
        html_element(".stripe-table") 
     
    all_links <- walkable |>
        html_elements("a") |>
        html_attr("href")
    
    if(is.na(neighborhoods)) {
        tmp <- walkable |> 
            html_elements("img") |> 
            html_attr("src") |> 
            stringr::str_match("badge/(walk|bike|transit)/score/([0-9]{2})\\.svg")
        
        tmp2 <- tmp[!is.na(tmp[, 1]), c(2, 3), drop = FALSE]
        
        tmp3 <- as.data.frame(tmp2) |> 
            unique() |>
            mutate(V2 = as.integer(V2)) |> 
            tidyr::pivot_wider(
                names_from = "V1", 
                names_glue = "{stringr::str_to_title(V1)}_Score", 
                values_from = "V2"
                ) |>
            mutate(
                Neighborhood = NA_character_,
                Rank = NA_integer_,
                Population = NA_integer_,
                Redfin_url = find_redfin_city_url(all_links, url_path)
            ) |>
            select(
                c("Neighborhood", 
                  "Rank", 
                  dplyr::ends_with("Score"), 
                  "Population", 
                  "Redfin_url")
            )
        
        return(tmp3)    
    }
    
    nbrhd_paths <- all_links %>%
        stringr::str_subset(paste0(url_path, "/")) 
    
    #picked extra url in San Diego. You can rely on exact number in exact order
    browser()
    neighborhoods %>%
        html_table() %>%
        rename(
            Neighborhood = Name,
            Rank = `Rank#`, 
            Walk_Score = `Walk Score`, 
            Transit_Score = `Transit Score`, 
            Bike_Score = `Bike Score`) %>%
        mutate(
            dplyr::across(.cols = dplyr::ends_with("Score"), as.integer),
            Population = as.integer(stringr::str_remove_all(Population, ",")),
            Redfin_url = find_redfin_neighborhood_url(nbrhd_paths)
                                    
        )
    
}

