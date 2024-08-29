#' Load an RDS File from a Private GitHub Repository
#'
#' This function loads an RDS file from a private GitHub repository. It uses a provided or interactive GitHub 
#' Personal Access Token (PAT). If the PAT is not provided, the function will try to retrieve it using `gitcreds` 
#' or prompt the user to enter it manually. It also checks if the user has access to the repository and provides 
#' instructions for requesting access if needed.
#'
#' @param url A character string specifying the raw URL to the RDS file in the private GitHub repository.
#' @param pat A character string containing the GitHub Personal Access Token (PAT) with access to the repository. 
#'            If not provided, the function will attempt to retrieve it using `gitcreds` or prompt the user to enter it.
#'
#' @return The content of the RDS file as an R object, or `NULL` if access is denied or if the file cannot be loaded.
#'
#' @details
#' If the PAT is not provided, the function will attempt to retrieve it from `gitcreds`. If `gitcreds` does not 
#' provide a PAT, the user will be prompted to enter one manually. If the function encounters a 404 status code, 
#' it will inform the user to contact Brooke Gibbons at brooke.gibbons@uwa.edu.au with their GitHub username to request 
#' access to the repository.
#'
#' @examples
#' # Define the URL to the RDS file in your private GitHub repository
#' rds_url <- "https://raw.githubusercontent.com/GlobalArchiveManual/australia-synthesis-2024/main/data/tidy/australian-synthesis_covariates.RDS"
#'
#' # Call the function to load the RDS file
#' data <- load_rds_from_github(rds_url)
#'
#' # Print or work with the data
#' if (!is.null(data)) {
#'   print(data)
#' }
#'
#' @import httr
#' @import gitcreds
#' @export
load_rds_from_github <- function(url, pat = NULL) {
  # Check if PAT is provided; if not, try to get it from gitcreds
  if (is.null(pat)) {
    pat <- tryCatch({
      gitcreds::gitcreds_get()$password
    }, error = function(e) {
      message("GitHub PAT not found in gitcreds.")
      message("To get a GitHub PAT, follow these steps:")
      message("1. Go to GitHub and log in to your account.")
      message("2. Navigate to Settings > Developer settings > Personal access tokens.")
      message("3. Click 'Generate new token'.")
      message("4. Select the 'repo' scope to access private repositories.")
      message("5. Click 'Generate token' at the bottom of the page.")
      message("6. Copy the token and paste it below.")
      readline(prompt = "Enter your GitHub Personal Access Token (PAT): ")
    })
  }
  
  # Set up the GET request with the PAT for authentication
  response <- GET(url, authenticate("", pat, type = "basic"))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Check if the response content is valid
    if (length(content(response, "raw")) > 0) {
      # Read the raw content from the response and load it as an RDS file
      rds_data <- readRDS(gzcon(rawConnection(content(response, "raw"))))
      return(rds_data)
    } else {
      stop("The content of the RDS file is empty.")
    }
  } else if (status_code(response) == 404) {
    message("Access to the repository or file not found.")
    message("Please contact Brooke Gibbons at brooke.gibbons@uwa.edu.au with your GitHub username to request access.")
    return(NULL)
  } else {
    stop("Failed to download the RDS file. Status code: ", status_code(response))
  }
}
