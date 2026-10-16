# Biomass_summary (development version)

- `multi` mode reads the replicate outputs from the `outputsDF` input (`outputs(sim)` of the sims being
  summarised) when it is supplied, rather than globbing `simOutputPath`. Without it, the replicate folder
  names (`rep1` or `rep01`) are taken from the files found on disk.
- The `years` parameter defaults to `NA`, meaning the simulation's own start and end times.
- Needs SpaDES.core 3.2.1.9038 or later (`dirnamesFromSet()`, `padYears()`, `resolveSimYears()`).

# Biomass_summary 1.1.0

This release brings Biomass_summary in line with the other LandR and fireSense modules. The study area name setting now has the same name it has everywhere else, and when no name is set, the module makes one from the reporting area, if it is given one. The module also states that it runs after Biomass_core, so it is scheduled in the right order without extra setup.

The module now has automated tests that run on every change, checking that its inputs, outputs and settings stay as documented. Projects that set `studyAreaName` for this module should rename it to `.studyAreaName`; the old name still works, with a warning.

- The parameter `studyAreaName` is renamed `.studyAreaName`, the name every other fireSense and Biomass module
  uses for it. A project passing `studyAreaName` still works, with a warning, but should rename it.
  If it is `NA` (the default), `multi` mode names the study area with `reproducible::studyAreaName()` on
  `studyAreaReporting`, as other modules do with `studyArea`.
- New optional input `studyAreaReporting`, as in NRV_summary: in `multi` mode, the leading-change map is
  masked to it. Without it, the map covers `rasterToMatch`, as before.
- The leading-species figure is saved in `figures/Biomass_summary/` under the output path
  (`SpaDES.core::figurePath()`), not in `figures/` under `simOutputPath`, and the module no longer creates an
  empty `figures/Biomass_summary/` directory. This needs LandR 1.2.0.9051 or later.
