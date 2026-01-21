#' Australia life history
#'
#' A dataset containing life-history and related metadata for Australian fish species.
#'
#' @format A data frame with one row per species and the following variables (selected):
#' \describe{
#'   \item{family}{Family name.}
#'   \item{genus}{Genus name.}
#'   \item{species}{Species epithet.}
#'   \item{scientific_name}{Full scientific name (genus + species).}
#'   \item{length_max_cm}{Maximum length (cm).}
#'   \item{length_max_source}{Source of the maximum length record.}
#'   \item{length_max_type}{Type of length measurement (e.g., TL/FL/SL if available).}
#'   \item{rls_thermal_niche}{Thermal niche category from Reef Life Survey (if present).}
#'   \item{iucn_ranking}{IUCN category (if present).}
#'   \item{epbc_threat_status}{EPBC threat listing (if present).}
#' }
#'
#' @details
#' This dataset contains additional columns not listed above. Use
#' \code{names(australia_life_history)} to view all available fields.
#'
#' @source Compiled from multiple sources including FishBase/SeaLifeBase, Reef Life Survey,
#' and Australian reference datasets (see package documentation for details).
"australia_life_history"
