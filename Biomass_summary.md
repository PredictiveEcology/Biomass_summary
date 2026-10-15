---
title: "Biomass_summary"
author: "Alex Chubaty"
date: "08 October 2026"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Summarizes the results of multiple LandR Biomass simulations, across multiple study areas, climate scenarios, and replicates.

# Usage

Intended to be used for post-simulation processing of multiple LandR Biomass simulations, following a `LandR-fs` project structure and workflow described and templated in the [`SpaDES.project`](https://github.com/PredictiveEcology/SpaDES.project) package.

# Parameters

Provide a summary of user-visible parameters.


|paramName       |paramClass |default      |min |max |paramDesc                                                                                                                                                             |
|:---------------|:----------|:------------|:---|:---|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|climateScenario |character  |NA           |NA  |NA  |name of CIMP6 climate scenarios including SSP, formatted as in ClimateNA, using underscores as separator. E.g., 'CanESM5_SSP370'.                                     |
|mode            |character  |single       |NA  |NA  |use 'single' to run part of a simulation; use 'multi' to run as part of postprocessing multiple runs.                                                                 |
|simOutputPath   |character  |/tmp/Rtm.... |NA  |NA  |Directory specifying the location of the simulation outputs.                                                                                                          |
|.studyAreaName  |character  |NA           |NA  |NA  |Human-readable name for the study area used. If `NA`, a hash of `studyAreaReporting` will be used.                                                                    |
|reps            |integer    |1, 2, 3,.... |1   |NA  |number of replicates/runs per study area and climate scenario. NOTE: `mclapply` is used internally, so you should set `options(mc.cores = nReps)` to run in parallel. |
|years           |integer    |2011, 2100   |NA  |NA  |Which two simulation years should be compared? Typically start and end years.                                                                                         |

## Plotting and saving

Several figures are produced, as `.png` files, and summary rasters are written to disk.

## Uploading

Figures can optionally be uploaded to Google Drive.

# Data dependencies

## Input data

Description of the module inputs.


|objectName         |objectClass |desc                                                                                                                                     |sourceURL |
|:------------------|:-----------|:----------------------------------------------------------------------------------------------------------------------------------------|:---------|
|cohortData         |data.table  |                                                                                                                                         |NA        |
|pixelGroupMap      |SpatRaster  |                                                                                                                                         |NA        |
|rasterToMatch      |SpatRaster  |template raster used for simulations                                                                                                     |NA        |
|studyAreaReporting |SpatVector  |Optional; multi mode. Reporting area: the leading-change map is masked to it, and it names the study area when `.studyAreaName` is `NA`. |NA        |
|treeSpecies        |data.table  |species name and deciduous/conifer type                                                                                                  |NA        |

## Output data

Description of the module outputs.


|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

Originally developed for *post hoc* use with the LandR Biomass suite of modules.
