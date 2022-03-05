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
  reqdPkgs = list("assertthat", "cowplot", "data.table", "fs", "ggplot2", "googledrive",
                  "PredictiveEcology/LandR@development (>= 1.0.7.9005)",
                  "raster", "rasterVis", "RColorBrewer", "SpaDES.core (>= 1.0.10)", "SpaDES.tools", "qs"),
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
                    desc = "number of replicates/runs per study area and climate scenario."),
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
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "Biomass_summary", "plot_leadingSpecies")
      sim <- scheduleEvent(sim, start(sim), "Biomass_summary", "upload", .last())
    },
    plot_leadingSpecies = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
browser()
      files2upload <- lapply(P(sim)$studyAreaNames, function(studyAreaName) {
        lapply(P(sim)$climateScenarios, function(climateScenario) {
          tmp <- loadSimList(file.path("outputs", studyAreaName,
                                       paste0("simOutPreamble_", studyAreaName, "_",
                                              gsub("SSP", "", climateScenario), ".qs")))
          rasterToMatch <- tmp$rasterToMatchReporting
          rm(tmp)

          if (grepl("ROF", studyAreaName)) {
            if (unique(res(rasterToMatch)) == 250) {
              rasterToMatch <- disaggregate(rasterToMatch, fact = 2) ## 125 m pixels from sims
            }
          }

          plotLeadingSpecies(
            studyAreaName = studyAreaName,
            climateScenario = climateScenario,
            Nreps = P(sim)$reps,
            years = P(sim)$years,
            outputDir = P(sim)$simOutputPath,
            treeSpecies = sim$treeSpecies,
            defineLeading = .defineLeading, ## TODO: allow user override?
            leadingPercentage = 0.8,        ## TODO: allow user override?
            treeType = NULL,                ## TODO: allow user override?
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
browser()
      lapply(mod$files2upload, function(f) {
        drive_put(f, sim$uploadTo[[P(sim)$studyAreaName]], basename(f))
      })

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

  ## TODO: inventory all files to ensure correct dir structure? compare against expected files?
  #filesUserHas <- fs::dir_ls(P(sim)$simOutputPath, recurse = TRUE, type = "file", glob = "*.qs")

  filesUserExpects <- rbindlist(lapply(P(sim)$studyAreaNames, function(studyAreaName) {
    rbindlist(lapply(P(sim)$climateScenarios, function(climateScenario) {
      rbindlist(lapply(P(sim)$reps, function(rep) {
        runName <- sprintf("%s_%s_run%02d", studyAreaName, climateScenario, as.integer(rep))
        f <- file.path(P(sim)$simOutputPath, runName, paste0(runName, ".qs"))

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
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
