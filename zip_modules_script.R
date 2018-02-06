library(SpaDES)

# check all module versions, and rezip if needed
omit <- ""

allModules <- list.dirs("modules/", recursive = FALSE) %>% basename()
currentModules <- if (length(omit)) {
  allModules[!allModules %in% omit]
} else {
  allModules
}
rezipped <- list()

sim <- simInit() ## workaround: need dummy simList to get metadata below

## overwrite existing zip?
overwrite = TRUE

out <- lapply(currentModules, function(x) {
  version <- SpaDES.core::moduleVersion(module = x, path = "modules") %>% as.character()
  zipFile <- paste0("modules/", x, "/", x, "_", version, ".zip")
  if (!file.exists(zipFile) | overwrite == TRUE) {
    SpaDES.core::zipModule(x, "modules")
    rezipped <<- append(rezipped, x)
  }
})
rezipped <- unlist(rezipped)
