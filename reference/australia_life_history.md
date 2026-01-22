# Australia life history

A dataset containing life-history, ecological, conservation, and
fisheries management information for Australian fish species.

## Usage

``` r
australia_life_history
```

## Format

A data frame with one row per species and the following variables:

- australian_source:

  Source of Australian-specific information.

- caab_code:

  CAAB (Codes for Australian Aquatic Biota) identifier.

- class:

  Taxonomic class.

- order:

  Taxonomic order.

- family:

  Taxonomic family.

- subfamily:

  Taxonomic subfamily (if available).

- genus:

  Genus name.

- species:

  Species epithet.

- scientific_name:

  Full scientific name (genus + species).

- australian_common_name:

  Common name used in Australia.

- global_region:

  Broad global biogeographic region.

- marine_region:

  Marine bioregion.

- imcra_region:

  IMCRA (Integrated Marine and Coastal Regionalisation of Australia)
  region.

- global_source:

  Source of global distribution information.

- fb_code:

  FishBase species code.

- fb_length_at_maturity_cm:

  Length at maturity (cm) from FishBase.

- fb_length_at_maturity_type:

  Type of length measurement used for maturity (e.g. TL, FL).

- fb_length_at_maturity_source:

  Source of FishBase maturity information.

- fb_length_weight_measure:

  Length–weight measurement type used in FishBase.

- fb_a:

  FishBase length–weight parameter \\a\\.

- fb_b:

  FishBase length–weight parameter \\b\\.

- fb_a_ll:

  Lower-limit estimate of FishBase \\a\\.

- fb_b_ll:

  Lower-limit estimate of FishBase \\b\\.

- fb_ll_equation_type:

  Equation type used for lower-limit estimates.

- fb_length_weight_source:

  Source of FishBase length–weight relationship.

- fb_vulnerability:

  FishBase vulnerability index.

- fb_countries:

  Countries in which the species is recorded in FishBase.

- fb_status:

  FishBase conservation or population status.

- length_max_cm:

  Maximum recorded length (cm).

- length_max_type:

  Type of maximum length measurement (e.g. TL, FL, SL).

- length_max_source:

  Source of maximum length information.

- fb_trophic_level:

  Mean trophic level from FishBase.

- fb_trophic_level_se:

  Standard error of FishBase trophic level estimate.

- fb_trophic_level_source:

  Source of FishBase trophic level information.

- foa_min_depth:

  Minimum recorded depth (m) from Fish of Australia.

- foa_max_depth:

  Maximum recorded depth (m) from Fish of Australia.

- rls_trophic_group:

  Trophic group classification from Reef Life Survey.

- rls_water_column:

  Water-column position from Reef Life Survey.

- rls_substrate_type:

  Primary substrate association from Reef Life Survey.

- rls_thermal_niche:

  Thermal niche category from Reef Life Survey.

- local_source:

  Source of locally derived information.

- epbc_threat_status:

  Threat listing under the EPBC Act (if applicable).

- iucn_ranking:

  IUCN Red List category.

- fishing_mortality:

  Qualitative or quantitative indicator of fishing mortality.

- fishing_type:

  Primary fishing method or sector.

- min_legal_nt:

  Minimum legal size in the Northern Territory.

- max_legal_nt:

  Maximum legal size in the Northern Territory.

- min_legal_wa:

  Minimum legal size in Western Australia.

- max_legal_wa:

  Maximum legal size in Western Australia.

- min_legal_qld:

  Minimum legal size in Queensland.

- max_legal_qld:

  Maximum legal size in Queensland.

- min_legal_nsw:

  Minimum legal size in New South Wales.

- max_legal_nsw:

  Maximum legal size in New South Wales.

- min_legal_vic:

  Minimum legal size in Victoria.

- max_legal_vic:

  Maximum legal size in Victoria.

- min_legal_sa:

  Minimum legal size in South Australia.

- max_legal_sa:

  Maximum legal size in South Australia.

- min_legal_tas:

  Minimum legal size in Tasmania.

- max_legal_tas:

  Maximum legal size in Tasmania.

## Source

Compiled from multiple sources including FishBase/SeaLifeBase, Fish of
Australia, Reef Life Survey, and Australian fisheries and conservation
datasets.
