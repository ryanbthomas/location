# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(readxl)
library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(future)
library(future.callr)


# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "readxl", "dplyr", "tidyr", "rvest", "purrr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

#future execution plan
plan(callr)

# Replace the target list below with your own:
list(
    tar_target(
        all_cities,
        "raw-data/SUB-IP-EST2021-ANNRNK.xlsx",
        format = "file"
    ),
    tar_target(
        viable_states,
        "raw-data/viable-states.xlsx",
        format = "file"
    ),
    tar_target(
        name = viable_cities,
        command = create_viable_cities(all_cities, viable_states),
        deployment = "main"
    ),
    tar_target(
        name = viable_neighborhoods,
        command = fetch_neighborhoods_row(viable_cities),
        pattern = map(viable_cities),
        iteration = "vector" 
    )
)
