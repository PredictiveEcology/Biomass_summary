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
  version = list(Biomass_summary = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "Biomass_summary.Rmd"), ## README generated from module Rmd
  reqdPkgs = list("assertthat", "cowplot", "data.table", "disk.frame", "fs", "ggplot2", "googledrive",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9026)",
                  "purrr", "raster", "rasterVis", "RColorBrewer",
                  "SpaDES.core (>= 1.0.10)", "SpaDES.tools", "qs"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("climateScenarios", "character", NA, NA, NA,
                    desc = paste("names of CIMP6 climate scenarios including SSP,",
                                 "formatted as in ClimateNA, using underscores as separator.",
                                 "E.g., 'CanESM5_SSP370'.")),
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    desc = "Directory specifying the location of the simulation outputs."),
    defineParameter("studyAreaNames", "character", NA, NA, NA,
                    desc = "names of study areas simulated."),
    defineParameter("reps", "integer", 1:10, 1, NA,
                    desc = paste("number of replicates/runs per study area and climate scenario.",
                                 "NOTE: `mclapply` is used internally, so you should set",
                                 "`options(mc.cores = nReps)` to run in parallel.")),
    defineParameter("upload", "logical", FALSE, NA, NA,
                    desc = "if TRUE, uses the `googledrive` package to upload figures."),
    defineParameter("years", "integer", c(2011, 2100), NA, NA,
                    desc = "Which two simulation years should be compared? Typically start and end years.")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("rasterToMatch", "RasterLayer", "DESCRIPTION NEEDED", sourceURL = NA),
    expectsInput("treeSpecies", "data.table", "DESCRIPTION NEEDED", sourceURL = NA),
    expectsInput("uploadTo", "character",
                 desc = paste("if `upload = TRUE`, a named list of Google Drive folder ids,",
                              "corresponding to `studyAreaNames`."),
                 sourceURL = NA)
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
      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "Biomass_summary", "plot_leadingSpecies")

      if (isTRUE(P(sim)$upload)) {
        sim <- scheduleEvent(sim, end(sim), "Biomass_summary", "upload", .last())
      }
    },
    plot_leadingSpecies = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      files2upload <- lapply(P(sim)$studyAreaNames, function(studyAreaName) {
        ## get rasterToMatch for each studyArea
        tmp <- loadSimList(file.path(P(sim)$simOutputPath, studyAreaName,
                                     paste0("simOutPreamble_", studyAreaName, "_",
                                            gsub("SSP", "", P(sim)$climateScenarios[1]), ".qs")))
        sim$rasterToMatch <- tmp$rasterToMatchReporting
        rm(tmp)

        lapply(P(sim)$climateScenarios, function(climateScenario) {
          message(paste("Leading species plot: ", studyAreaName, climateScenario))

          ## TODO: ensure cohortData_2001 is created in sim
          cd2001 <- file.path(P(sim)$simOutputPath, paste0(studyAreaName, "_", climateScenario),
                              "rep01", "cohortData_2001_year2001.qs")
          if (!file.exists(cd2001)) {
            tmp <- loadSimList(file.path(P(sim)$simOutputPath, paste0(studyAreaName, "_", climateScenario),
                                         "rep01", paste0("biomassMaps2001_", studyAreaName, ".qs")))
            qs::qsave(tmp$cohortData, cd2001)
            rm(tmp)
          }

          plotLeadingSpecies(
            studyAreaName = studyAreaName,
            climateScenario = climateScenario,
            Nreps = max(P(sim)$reps),
            years = P(sim)$years,
            outputDir = P(sim)$simOutputPath,
            treeSpecies = sim$treeSpecies,
            defineLeading = LandR:::.defineLeading, ## TODO: allow user override?
            leadingPercentage = 0.8,                ## TODO: allow user override?
            treeType = NULL,                        ## TODO: allow user override?
            rasterToMatch = sim$rasterToMatch
          )
        })
      })
      files2upload <- unlist(files2upload, recursive = TRUE)

      mod$files2upload <- c(mod$files2upload, files2upload)

      # ! ----- STOP EDITING ----- ! #
    },
    upload = {
      # ! ----- EDIT BELOW ----- ! #
      mod$files2upload <- set_names(mod$files2upload, basename(mod$files2upload))

      gid <- as_id(sim$uploadTo[[P(sim)$studyAreaName]])
      prevUploaded <- drive_ls(gid)
      toUpload <- mod$files2upload[!(basename(mod$files2upload) %in% prevUploaded$name)]
      uploaded <- map(toUpload, ~ drive_upload(.x, path = gid))
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  checkPath(file.path(P(sim)$simOutputPath, P(sim)$studyAreaNames, "figures"), create = TRUE)

  ## TODO: inventory all files to ensure correct dir structure? compare against expected files?
  #filesUserHas <- fs::dir_ls(P(sim)$simOutputPath, recurse = TRUE, type = "file", glob = "*.qs")

  filesUserExpects <- rbindlist(lapply(P(sim)$studyAreaNames, function(studyAreaName) {
    rbindlist(lapply(P(sim)$climateScenarios, function(climateScenario) {
      rbindlist(lapply(P(sim)$reps, function(rep) {
        runName <- sprintf("%s_%s", studyAreaName, climateScenario)
        f <- file.path(P(sim)$simOutputPath, runName, sprintf("rep%02d", as.integer(rep)),
                       paste0(runName, "_", sprintf("rep%02d", as.integer(rep)), ".qs"))

        data.table(file = f, exists = file.exists(f))
      }))
    }))
  }))

  if (!all(filesUserExpects$exists)) {
    missing <- filesUserExpects[exists == FALSE, ]$file
    stop("Some simulation files missing:\n", paste(missing, collapse = "\n"))
  }

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  if (!suppliedElsewhere("treeSpecies", sim)) {
    stop("treeSpecies must be supplied.") ## TODO: use Eliot's new function to get kNN spp for the studyArea
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
