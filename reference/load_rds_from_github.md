# Load an RDS File from a Private GitHub Repository

This function loads an RDS file from a private GitHub repository. It
uses a provided or interactive GitHub Personal Access Token (PAT). If
the PAT is not provided, the function will try to retrieve it using
`gitcreds` or prompt the user to enter it manually. It also checks if
the user has access to the repository and provides instructions for
requesting access if needed.

## Usage

``` r
load_rds_from_github(url, pat = NULL)
```

## Arguments

- url:

  A character string specifying the raw URL to the RDS file in the
  private GitHub repository.

- pat:

  A character string containing the GitHub Personal Access Token (PAT)
  with access to the repository. If not provided, the function will
  attempt to retrieve it using `gitcreds` or prompt the user to enter
  it.

## Value

The content of the RDS file as an R object, or `NULL` if access is
denied or if the file cannot be loaded.

## Details

If the PAT is not provided, the function will attempt to retrieve it
from `gitcreds`. If `gitcreds` does not provide a PAT, the user will be
prompted to enter one manually. If the function encounters a 404 status
code, it will inform the user to contact Brooke Gibbons at
brooke.gibbons@uwa.edu.au with their GitHub username to request access
to the repository.

## Examples

``` r
# Define the URL to the RDS file in your private GitHub repository
rds_url <- "https://raw.githubusercontent.com/GlobalArchiveManual/australia-synthesis-2024/main/data/tidy/australian-synthesis_covariates.RDS"

# Call the function to load the RDS file
data <- load_rds_from_github(rds_url)
#> Error in GET(url, authenticate("", pat, type = "basic")): could not find function "GET"

# Print or work with the data
if (!is.null(data)) {
  print(data)
}
#> function (..., list = character(), package = NULL, lib.loc = NULL, 
#>     verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) 
#> {
#>     fileExt <- function(x) {
#>         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
#>         ans <- sub(".*\\.", "", x)
#>         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
#>             x[db])
#>         ans
#>     }
#>     my_read_table <- function(...) {
#>         lcc <- Sys.getlocale("LC_COLLATE")
#>         on.exit(Sys.setlocale("LC_COLLATE", lcc))
#>         Sys.setlocale("LC_COLLATE", "C")
#>         read.table(...)
#>     }
#>     stopifnot(is.character(list))
#>     names <- c(as.character(substitute(list(...))[-1L]), list)
#>     if (!is.null(package)) {
#>         if (!is.character(package)) 
#>             stop("'package' must be a character vector or NULL")
#>     }
#>     paths <- find.package(package, lib.loc, verbose = verbose)
#>     if (is.null(lib.loc)) 
#>         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
#>             paths)
#>     paths <- unique(normalizePath(paths[file.exists(paths)]))
#>     paths <- paths[dir.exists(file.path(paths, "data"))]
#>     dataExts <- tools:::.make_file_exts("data")
#>     if (length(names) == 0L) {
#>         db <- matrix(character(), nrow = 0L, ncol = 4L)
#>         for (path in paths) {
#>             entries <- NULL
#>             packageName <- if (file_test("-f", file.path(path, 
#>                 "DESCRIPTION"))) 
#>                 basename(path)
#>             else "."
#>             if (file_test("-f", INDEX <- file.path(path, "Meta", 
#>                 "data.rds"))) {
#>                 entries <- readRDS(INDEX)
#>             }
#>             else {
#>                 dataDir <- file.path(path, "data")
#>                 entries <- tools::list_files_with_type(dataDir, 
#>                   "data")
#>                 if (length(entries)) {
#>                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
#>                   entries <- cbind(entries, "")
#>                 }
#>             }
#>             if (NROW(entries)) {
#>                 if (is.matrix(entries) && ncol(entries) == 2L) 
#>                   db <- rbind(db, cbind(packageName, dirname(path), 
#>                     entries))
#>                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
#>                   sQuote(packageName)), domain = NA, call. = FALSE)
#>             }
#>         }
#>         colnames(db) <- c("Package", "LibPath", "Item", "Title")
#>         footer <- if (missing(package)) 
#>             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
#>                 "\n", "to list the data sets in all *available* packages.")
#>         else NULL
#>         y <- list(title = "Data sets", header = NULL, results = db, 
#>             footer = footer)
#>         class(y) <- "packageIQR"
#>         return(y)
#>     }
#>     paths <- file.path(paths, "data")
#>     for (name in names) {
#>         found <- FALSE
#>         for (p in paths) {
#>             tmp_env <- if (overwrite) 
#>                 envir
#>             else new.env()
#>             if (file_test("-f", file.path(p, "Rdata.rds"))) {
#>                 rds <- readRDS(file.path(p, "Rdata.rds"))
#>                 if (name %in% names(rds)) {
#>                   found <- TRUE
#>                   if (verbose) 
#>                     message(sprintf("name=%s:\t found in Rdata.rds", 
#>                       name), domain = NA)
#>                   objs <- rds[[name]]
#>                   lazyLoad(file.path(p, "Rdata"), envir = tmp_env, 
#>                     filter = function(x) x %in% objs)
#>                   break
#>                 }
#>                 else if (verbose) 
#>                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
#>                     name, paste(names(rds), collapse = ",")), 
#>                     domain = NA)
#>             }
#>             files <- list.files(p, full.names = TRUE)
#>             files <- files[grep(name, files, fixed = TRUE)]
#>             if (length(files) > 1L) {
#>                 o <- match(fileExt(files), dataExts, nomatch = 100L)
#>                 paths0 <- dirname(files)
#>                 paths0 <- factor(paths0, levels = unique(paths0))
#>                 files <- files[order(paths0, o)]
#>             }
#>             if (length(files)) {
#>                 for (file in files) {
#>                   if (verbose) 
#>                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
#>                       basename(file), "::\t", appendLF = FALSE, 
#>                       domain = NA)
#>                   ext <- fileExt(file)
#>                   if (basename(file) != paste0(name, ".", ext)) 
#>                     found <- FALSE
#>                   else {
#>                     found <- TRUE
#>                     switch(ext, R = , r = {
#>                       library("utils")
#>                       sys.source(file, chdir = TRUE, envir = tmp_env)
#>                     }, RData = , rdata = , rda = load(file, envir = tmp_env), 
#>                       TXT = , txt = , tab = , tab.gz = , tab.bz2 = , 
#>                       tab.xz = , txt.gz = , txt.bz2 = , txt.xz = assign(name, 
#>                         my_read_table(file, header = TRUE, as.is = FALSE), 
#>                         envir = tmp_env), CSV = , csv = , csv.gz = , 
#>                       csv.bz2 = , csv.xz = assign(name, my_read_table(file, 
#>                         header = TRUE, sep = ";", as.is = FALSE), 
#>                         envir = tmp_env), found <- FALSE)
#>                   }
#>                   if (found) 
#>                     break
#>                 }
#>                 if (verbose) 
#>                   message(if (!found) 
#>                     "*NOT* ", "found", domain = NA)
#>             }
#>             if (found) 
#>                 break
#>         }
#>         if (!found) {
#>             warning(gettextf("data set %s not found", sQuote(name)), 
#>                 domain = NA)
#>         }
#>         else if (!overwrite) {
#>             for (o in ls(envir = tmp_env, all.names = TRUE)) {
#>                 if (exists(o, envir = envir, inherits = FALSE)) 
#>                   warning(gettextf("an object named %s already exists and will not be overwritten", 
#>                     sQuote(o)))
#>                 else assign(o, get(o, envir = tmp_env, inherits = FALSE), 
#>                   envir = envir)
#>             }
#>             rm(tmp_env)
#>         }
#>     }
#>     invisible(names)
#> }
#> <bytecode: 0x563255168a70>
#> <environment: namespace:utils>
```
