# Set GlobalArchive API Token

This function prompts the user to enter their GlobalArchive API token,
saves it in an RDS file within a "secrets" directory, and optionally
adds the "secrets" directory to the `.gitignore` file if the function is
run inside a Git repository.

## Usage

``` r
ga_api_set_token()
```

## Details

The function performs the following steps:

- Prompts the user to input their GlobalArchive API token via the
  console.

- Creates a "secrets" directory in the current working directory if it
  doesn't already exist.

- Saves the token as an RDS file named `api_token.rds` inside the
  "secrets" directory.

- Checks if the current directory is part of a Git repository (i.e., it
  contains a `.git` folder).

- If in a Git repository, the function adds the "secrets/" directory to
  the `.gitignore` file if it's not already listed.

## Note

This function is intended to be used in projects involving
GlobalArchive, where sensitive information like API tokens should not be
committed to version control. Ensure that the working directory is set
correctly before running this function.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Run the function to set a GlobalArchive API token
  ga_api_set_token()
} # }
```
