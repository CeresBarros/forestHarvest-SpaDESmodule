defineModule(sim, list(
  name = "forestHarvest-SpaDESmodule",
  description = "A toy model of forest harvest by forest age and type", 
  keywords = c("forest", "age", "composition"), 
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9000", forestHarvest-SpaDESmodule = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "forestHarvest-SpaDESmodule.Rmd"),
  reqdPkgs = list("raster", "RColorBrewer", "dplyr"),
  parameters = rbind(
    defineParameter("harvAge", "numeric", 50, NA, NA, desc = "Tree age for harvesting"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "Time interval between succession events"),
    defineParameter("startTime", "numeric", start(sim), NA, NA, desc = ""),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, desc = "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc = "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc = "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc = "Interval between save events")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Map of vegetation age", sourceURL = NA),
    expectsInput(objectName = "vegMap", objectClass = "RasterLayer", desc = "Map of vegetation types", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "harvMap", objectClass = "RasterLayer", desc = "Map of areas that are suitable for harvesting", sourceURL = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.forestHarvest-SpaDESmodule = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- forestHarvest-SpaDESmoduleInit(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "forestHarvest-SpaDESmodule", "forestHarvest-SpaDESmoduleMaps")
      sim <- scheduleEvent(sim, P(sim)$startTime + P(sim)$.plotInitialTime, "forestHarvest-SpaDESmodule", "plot")

    },
    forestHarvest-SpaDESmoduleMaps = {
      sim <- forestHarvest-SpaDESmoduleMapFun(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "forestHarvest-SpaDESmodule", "forestHarvest-SpaDESmoduleMaps")
    },
    plot = {
      Plot(sim$harvMap)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "forestHarvest-SpaDESmodule", "plot")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
forestHarvest-SpaDESmoduleInit <- function(sim) {
  sim$harvMap <- sim$vegMap
  return(invisible(sim))
}


### template for your event1
forestHarvest-SpaDESmoduleMapFun <- function(sim) {
  ## only harvest coniferous forest
  harVeg <- raster::levels(sim$vegMap)[[1]]$ID[grep("coniferous", raster::levels(sim$vegMap)[[1]]$Class)]

  ## make a mask of the areas to harvest
  sim$harvMap[!sim$harvMap[] %in% harVeg] <- 9999
  sim$harvMap[sim$harvMap[] %in% harVeg] <- 1
  sim$harvMap[is.na(sim$vegMap[])] <- NA

  levels(sim$harvMap)[[1]] <- data.frame(ID = c(1,9999), Class = c("Harvest", "Non-harvest"))

  setColors(sim$harvMap, n = 2) <- RColorBrewer::brewer.pal(n = 2, name = "BrBG")[c(1,3)]

  ## make a mask of vegetation that is older than the harvest age threshold
  age4harvest <- sim$ageMap
  age4harvest[age4harvest[] <  P(sim)$harvAge] <- NA
  age4harvest[age4harvest[] >=  P(sim)$harvAge] <- 1

  ## condition harvested areas to age
  sim$harvMap[is.na(age4harvest[])] <- 9999
  sim$harvMap[is.na(sim$vegMap[])] <- NA

  return(invisible(sim))
}

