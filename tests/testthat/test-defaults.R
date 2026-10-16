## `.studyAreaName` names the study area in the file names `multi` mode writes.

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

test_that("the old studyAreaName parameter is used, with a warning", {
  expect_warning(
    sim <- initOnly(list(Biomass_summary = list(studyAreaName = "4.2.1"))),
    "now `.studyAreaName`"
  )
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName, "4.2.1")
})

test_that("the old studyAreaName parameter does not override .studyAreaName", {
  expect_warning(
    sim <- initOnly(list(Biomass_summary = list(studyAreaName = "old", .studyAreaName = "new"))),
    "now `.studyAreaName`"
  )
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName, "new")
})
