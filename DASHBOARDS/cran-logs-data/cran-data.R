# Load required libraries
library(tools)
library(dplyr)

# Function to retrieve CRAN package release dates
get_release_dates <- function(packages) {
  cran_db <- as.data.frame(CRAN_package_db()) # Get CRAN package database
  release_dates <- cran_db %>%
    filter(Package %in% packages) %>% # Filter for the packages of interest
    select(Package, Published) %>% # Select relevant columns
    group_by(Package) %>%
    summarise(release_date = min(as.Date(Published, format = "%Y-%m-%d"))) %>% # Get earliest release date
    rename(package = Package)
  return(release_dates)
}

# Function to retrieve CRAN downloads
get_download_data <- function(packages, start_date, end_date) {
  library(cranlogs) # Load the cranlogs package
  download_data <- cran_downloads(packages = packages, from = start_date, to = end_date)
  return(download_data)
}

# Example usage
packages <- c("serofoi", "sivirep", "vaccineff")
start_date <- "2023-01-01"
end_date <- "2023-12-31"

# Get release dates
release_dates <- get_release_dates(packages)
print("Release Dates:")
print(release_dates)

# Get download data
download_data <- get_download_data(packages, start_date, end_date)
print("Download Data:")
print(head(download_data))

