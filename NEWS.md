# Biomass_summary 1.1.0

This release brings Biomass_summary in line with the other LandR and fireSense modules. The study area name setting now has the same name it has everywhere else, and when no name is given the module makes one automatically from the study area map. The module also states that it runs after Biomass_core, so it is scheduled in the right order without extra setup.

The module now has automated tests that run on every change, checking that its inputs, outputs and settings stay as documented. Projects that set `studyAreaName` for this module need to rename it to `.studyAreaName`.

- The parameter `studyAreaName` is renamed `.studyAreaName`, the name every other fireSense and Biomass module
  uses for it. A project passing `studyAreaName` to this module must rename it. If it is `NA` (the default), a
  hash of `rasterToMatch` is used, as other modules use a hash of `studyArea`.
