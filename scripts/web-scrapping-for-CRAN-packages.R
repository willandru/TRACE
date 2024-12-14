# Load necessary package
library(rvest)

# Function to get all available versions of a package from the CRAN Archive
get_package_versions <- function(package) {
  url <- paste0("https://cran.r-project.org/src/contrib/Archive/", package, "/")
  
  # Read the archive page
  tryCatch({
    page <- read_html(url)
    # Extract file names from the links
    versions <- page %>%
      html_elements("a") %>%
      html_text() %>%
      grep(paste0("^", package, "_"), ., value = TRUE)
    
    # Extract and return version numbers
    sub(paste0(package, "_([0-9.]+)\\.tar\\.gz"), "\\1", versions)
  }, error = function(e) {
    message(paste("No archive found for package:", package))
    return(NULL)
  })
}

# Packages of interest
packages <- c("sivirep", "vaccineff", "serofoi")

# Get versions for each package
all_versions <- lapply(packages, get_package_versions)
names(all_versions) <- packages

# Print results
all_versions
