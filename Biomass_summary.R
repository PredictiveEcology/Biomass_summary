defineModule(sim, list(
  name = "Biomass_summary",
  description = paste("Summarizes the results of multiple LandR Biomass simulations,",
                      "across multiple study areas, climate scenarios, and replicates."),
  keywords = "LandR Biomass",
  authors = c(
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = "aut"),
    person("Ian MS", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(Biomass_summary = "1.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "Biomass_summary.Rmd"), ## README generated from module Rmd
  reqdPkgs = list(
    "arrow", "assertthat", "cowplot", "data.table", "fs", "ggplot2", "googledrive",
    "purrr", "qs2", "RColorBrewer", "terra", "tidyterra",
    "PredictiveEcology/LandR@development (>= 1.1.5.9100)",
    "PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003)",
    "PredictiveEcology/SpaDES.tools@development (>= 2.1.1.9000)"
  ),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("climateScenario", "character", NA, NA, NA,
                    desc = paste("name of CIMP6 climate scenarios including SSP,",
                                 "formatted as in ClimateNA, using underscores as separator.",
                                 "E.g., 'CanESM5_SSP370'.")),
    defineParameter("mode", "character", "single", NA, NA,
                    paste("use 'single' to run part of a simulation;",
                          "use 'multi' to run as part of postprocessing multiple runs.")),
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    desc = "Directory specifying the location of the simulation outputs."),
    defineParameter("studyAreaNames", "character", NA, NA, NA,
                    desc = "names of study areas simulated."),
    defineParameter("reps", "integer", 1L:10L, 1L, NA_integer_,
                    desc = paste("number of replicates/runs per study area and climate scenario.",
                                 "NOTE: `mclapply` is used internally, so you should set",
                                 "`options(mc.cores = nReps)` to run in parallel.")),
    defineParameter("years", "integer", c(2011L, 2100L), NA, NA,
                    desc = "Which two simulation years should be compared? Typically start and end years.")
  ),
  inputObjects = bindrows(
    expectsInput("cohortData", "data.table", "", sourceURL = NA), ## TODO: description needed
    expectsInput("pixelGroupMap", "SpatRaster", "", sourceURL = NA), ## TODO: description needed
    expectsInput("rasterToMatch", "SpatRaster", "template raster used for simulations", sourceURL = NA),
    expectsInput("treeSpecies", "data.table", "species name and deciduous/conifer type", sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Biomass_summary = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      if (P(sim)$mode == "single") {
        sim <- scheduleEvent(sim, P(sim)$years[1], "Biomass_summary", "save_single", .last())
        sim <- scheduleEvent(sim, P(sim)$years[2], "Biomass_summary", "save_single", .last())
      } else if (P(sim)$mode == "multi") {
        sim <- InitMulti(sim)

        f_leading_plot <- LandR::plotLeadingSpecies(
          studyAreaName = P(sim)$studyAreaName,
          climateScenario = P(sim)$climateScenario,
          Nreps = max(P(sim)$reps),
          years = P(sim)$years,
          outputDir = P(sim)$simOutputPath,
          treeSpecies = sim$treeSpecies,
          defineLeading = LandR:::.defineLeading, ## TODO: allow user override?
          leadingPercentage = 0.8, ## TODO: allow user override?
          treeType = NULL, ## TODO: allow user override?
          rasterToMatch = sim$rasterToMatch
        )
        sim <- registerOutputs(f_leading_plot, sim)
      }
    },
    save_single = {
      padYear <- paddedFloatToChar(time(sim), padL = ceiling(log10(end(sim) + 1)))

      f_cohortData <- file.path(outputPath(sim), paste0("cohortData_year", padYear, ".qs2"))
      qs2::qs_save(sim$cohortData, f_cohortData)
      sim <- registerOutputs(f_cohortData, sim)

      f_pixelGroupMap <- file.path(outputPath(sim), paste0("pixelGroupMap_year", padYear, ".tif"))
      terra::writeRaster(sim$pixelGroupMap, f_pixelGroupMap, datatype = "INT4U", overwrite = TRUE)
      sim <- registerOutputs(f_pixelGroupMap, sim)
    },
    noEventWarning(sim)
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
InitMulti <- function(sim) {
  ## check for necessary output files -----------------------------------------------
  ## NOTE: don't load simLists -- slow and unreliable
  allReps <- sprintf("rep%02d", P(sim)$reps)
  padL <- ceiling(log10(P(sim)$years[2] + 1))
  padYearStart <- paddedFloatToChar(P(sim)$years[1], padL = padL)
  padYearEnd <- paddedFloatToChar(P(sim)$years[2], padL = padL)

  checkPath(file.path(P(sim)$simOutputPath, "figures", currentModule(sim)), create = TRUE)

  cdpgm <- fs::dir_ls(
    P(sim)$simOutputPath,
    regexp = "cohortData|pixelGroupMap",
    recurse = 1,
    type = "file"
  ) |>
    grep(paste0("(", paste0(P(sim)$reps, collapse = "|"), ")"), x = _, value = TRUE) |>
    grep(paste0("_year(", paste0(P(sim)$years, collapse = "|"), ")"), x = _, value = TRUE)

  filesUserHas <- c(cdpgm)

  dirsExpected <- file.path(P(sim)$simOutputPath, allReps)
  filesExpected <- as.character(sapply(dirsExpected, function(d) {
    c(
      file.path(d, sprintf("cohortData_year%04d.qs2", P(sim)$years)),
      file.path(d, sprintf("pixelGroupMap_year%04d.tif", P(sim)$years))
    )
  }))

  filesNeeded <- data.frame(file = filesExpected, exists = filesExpected %in% filesUserHas)

  if (!all(filesNeeded$exists)) {
    missing <- filesNeeded[filesNeeded$exists == FALSE, ]$file
    stop(
      sum(!filesNeeded$exists),
      " simulation files appear to be missing:\n",
      paste(missing, collapse = "\n")
    )
  }

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  if (!suppliedElsewhere("treeSpecies", sim)) {
    stop("treeSpecies must be supplied.")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
