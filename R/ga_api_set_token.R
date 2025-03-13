#' Set GlobalArchive API Token
#'
#' This function prompts the user to enter their GlobalArchive API token, saves it in an RDS file 
#' within a "secrets" directory, and optionally adds the "secrets" directory to 
#' the `.gitignore` file if the function is run inside a Git repository.
#'
#' @details 
#' The function performs the following steps:
#' \itemize{
#'   \item Prompts the user to input their GlobalArchive API token via the console.
#'   \item Creates a "secrets" directory in the current working directory if it doesn't already exist.
#'   \item Saves the token as an RDS file named `api_token.rds` inside the "secrets" directory.
#'   \item Checks if the current directory is part of a Git repository (i.e., it contains a `.git` folder).
#'   \item If in a Git repository, the function adds the "secrets/" directory to the `.gitignore` file if it's not already listed.
#' }
#'
#' @note 
#' This function is intended to be used in projects involving GlobalArchive, where sensitive information 
#' like API tokens should not be committed to version control. Ensure that the working directory is set 
#' correctly before running this function.
#'
#' @examples 
#' \dontrun{
#'   # Run the function to set a GlobalArchive API token
#'   ga_api_set_token()
#' }
#'
#' @export
ga_api_set_token <- function() {
  # Define the path for the secrets directory and the token file
  secrets_dir <- "secrets"
  token_file <- file.path(secrets_dir, "api_token.rds")
  
  # Check if the secrets directory exists, if not, create it
  if (!dir.exists(secrets_dir)) {
    dir.create(secrets_dir)
  }
  
  # If the token file exists, ask for user confirmation before overwriting
  if (file.exists(token_file)) {
    cat("A token file already exists. Do you want to overwrite it? (y/n): ")
    response <- readline()
    
    if (tolower(response) != "y") {
      message("Token was not overwritten.")
      return(invisible(NULL))
    }
  }
  
  # Prompt the user to enter the GlobalArchive API token
  cat("Please enter your GlobalArchive API token:\n")
  token <- readline(prompt = "API Token: ")
  
  # Save the token as an RDS file
  saveRDS(token, file = token_file)
  
  # Check if the current directory is part of a Git repository
  if (dir.exists(".git")) {
    # Define the path to the .gitignore file
    gitignore_path <- ".gitignore"
    
    # Check if .gitignore exists and read its contents if it does
    if (file.exists(gitignore_path)) {
      gitignore_contents <- readLines(gitignore_path)
    } else {
      gitignore_contents <- character(0)
    }
    
    # Ensure the file has a newline at the end
    if (length(gitignore_contents) > 0 && gitignore_contents[length(gitignore_contents)] != "") {
      gitignore_contents <- c(gitignore_contents, "")
    }
    
    # Check if the "secrets" directory is already in .gitignore
    if (!"secrets/" %in% gitignore_contents) {
      # Add "secrets/" to .gitignore
      writeLines(c(gitignore_contents, "secrets/"), con = gitignore_path)
      message("Added 'secrets/' to .gitignore.")
    } else {
      message("'secrets/' is already in .gitignore.")
    }
  } else {
    message("Not in a Git repository. Skipping .gitignore update.")
  }
  
  message("GlobalArchive API token has been saved successfully.")
}
