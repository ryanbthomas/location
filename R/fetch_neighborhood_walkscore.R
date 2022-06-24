
library(rvest)

fetch_neighborhoods <- function(url) {
    walkable <- rvest::read_html(url)
    
    neighborhoods <- walkable %>%
        html_element(".stripe-table") 
    
    if(is.na(neighborhoods)) {
        tmp <- walkable %>% 
            html_elements("img") %>% 
            html_attr("src") %>% 
            stringr::str_match("badge/(walk|bike|transit)/score/([0-9]{2})\\.svg") %>% 
            {.[!is.na(.[,1]), c(2,3)]} %>%
            {.[1:3, ]}
        
        tmp2 <- tmp |>
            as.data.frame() |> 
            mutate(V2 = as.integer(V2)) |> 
            tidyr::pivot_wider(
                names_from = "V1", 
                names_glue = "{stringr::str_to_title(V1)}_Score", 
                values_from = "V2"
                ) |>
            mutate(
                Neighborhood = NA_character_,
                Rank = NA_integer_,
                Population = NA_integer_
            ) |>
            select(
                c("Neighborhood", "Rank", "Walk_Score", "Transit_Score", 
                  "Bike_Score", "Population")
            )
        return(tmp2)    
    }
    
    neighborhoods %>%
        html_table() %>%
        rename(
            Neighborhood = Name,
            Rank = `Rank#`, 
            Walk_Score = `Walk Score`, 
            Transit_Score = `Transit Score`, 
            Bike_Score = `Bike Score`) %>%
        mutate(
            Population = as.integer(stringr::str_remove_all(Population, ","))
        )
    
}


system.time({
    nbrhds <- purrr::map(tmp$walkable_url, purrr::safely(fetch_neighborhoods))
})
