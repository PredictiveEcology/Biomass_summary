## `.studyAreaName` names the study area in figure titles and file names.

rtm <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3, vals = 1:9)

initOnly <- function(params) {
  sim <- SpaDES.core::simInit(times = list(start = 2011, end = 2100), modules = moduleName,
                              params = params, objects = list(rasterToMatch = rtm), paths = testPaths)
  SpaDES.core::spades(sim, events = list(Biomass_summary = "init"), debug = FALSE)
}

test_that("a supplied .studyAreaName is used", {
  sim <- initOnly(list(Biomass_summary = list(.studyAreaName = "4.2.1")))
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName, "4.2.1")
})

test_that("an NA .studyAreaName becomes a hash of rasterToMatch", {
  sim <- initOnly(list())
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName,
                   reproducible::.robustDigest(rtm, algo = "xxhash64"))
})
