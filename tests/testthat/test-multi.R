## `multi` mode on one replicate of a 2 x 2 map where every pixel goes from conifer- to
## deciduous-leading, so the leading-change map is 1 wherever it isn't masked.

rtm <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, vals = 1)

runMulti <- function(objects, outputPath) {
  repDir <- file.path(outputPath, "rep01")
  dir.create(repDir)
  pixelGroupMap <- terra::rast(rtm, vals = 1:4)
  leading <- c("2011" = "Pice_mar", "2100" = "Popu_tre")
  for (year in names(leading)) {
    cohortData <- data.table::data.table(pixelGroup = 1:4, speciesCode = leading[[year]], B = 100L)
    qs2::qs_save(cohortData, file.path(repDir, paste0("cohortData_year", year, ".qs2")))
    terra::writeRaster(pixelGroupMap, file.path(repDir, paste0("pixelGroupMap_year", year, ".tif")))
  }
  treeSpecies <- data.table::data.table(
    Species = c("Pice_mar", "Popu_tre"), Type = c("Conifer", "Deciduous")
  )

  paths <- testPaths
  paths$outputPath <- outputPath
  sim <- SpaDES.core::simInit(
    times = list(start = 2011, end = 2100), modules = moduleName,
    params = list(Biomass_summary = list(mode = "multi", reps = 1L, climateScenario = "CanESM5_SSP370")),
    objects = c(list(rasterToMatch = rtm, treeSpecies = treeSpecies), objects), paths = paths
  )
  suppressMessages(SpaDES.core::spades(sim, events = list(Biomass_summary = "init"), debug = FALSE))
}

test_that("multi mode masks to studyAreaReporting, names by it, and saves to figurePath()", {
  withr::local_options(mc.cores = 1L)
  outputPath <- withr::local_tempdir()
  ## the left column of pixels
  studyAreaReporting <- terra::as.polygons(terra::ext(0, 1, 0, 2), crs = terra::crs(rtm))
  sim <- runMulti(list(studyAreaReporting = studyAreaReporting), outputPath)

  name <- reproducible::studyAreaName(studyAreaReporting)
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName, name)

  fileStem <- paste0("leadingChange_", name, "_CanESM5_SSP370")
  expect_identical(list.files(file.path(outputPath, "figures")), moduleName)
  expect_identical(list.files(file.path(outputPath, "figures", moduleName)), paste0(fileStem, ".png"))
  expect_contains(basename(SpaDES.core::outputs(sim)$file), paste0(fileStem, c(".tif", ".png")))

  leadingChange <- terra::rast(file.path(outputPath, paste0(fileStem, ".tif")))
  expect_equal(as.vector(terra::values(leadingChange)), c(1, NA, 1, NA))
})

test_that("multi mode without studyAreaReporting leaves the map unmasked and the name NA", {
  withr::local_options(mc.cores = 1L)
  outputPath <- withr::local_tempdir()
  sim <- runMulti(list(), outputPath)

  expect_true(is.na(SpaDES.core::P(sim, module = moduleName)$.studyAreaName))
  leadingChange <- terra::rast(file.path(outputPath, "leadingChange_NA_CanESM5_SSP370.tif"))
  expect_identical(as.vector(terra::values(leadingChange)), c(1, 1, 1, 1))
})
