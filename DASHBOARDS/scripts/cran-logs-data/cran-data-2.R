get_earliest_release_date <- function(package) {
  # Construct the URL to the CRAN package archive
  url <- paste0("https://cran.r-project.org/src/contrib/Archive/", package, "/")
  tryCatch({
    # Read the HTML of the archive page
    archive_page <- readLines(url, warn = FALSE)
    
    # Extract dates and filenames from the HTML
    archive_info <- regmatches(archive_page, gregexpr("\\d{4}-\\d{2}-\\d{2}", archive_page))
    dates <- as.Date(unlist(archive_info), format = "%Y-%m-%d")
    
    # Return the earliest date (if available)
    if (length(dates) > 0) {
      return(min(dates))
    } else {
      return(NA) # If no archive exists, return NA
    }
  }, error = function(e) {
    return(NA) # If the archive doesn't exist, return NA
  })
}

# Function to retrieve release dates for multiple packages
get_release_dates_from_archive <- function(packages) {
  release_dates <- sapply(packages, get_earliest_release_date)
  data.frame(package = packages, release_date = release_dates, stringsAsFactors = FALSE)
}

# Example usage
packages <- c("sivirep", "vaccineff", "serofoi")
release_dates <- get_release_dates_from_archive(packages)
print(release_dates)
