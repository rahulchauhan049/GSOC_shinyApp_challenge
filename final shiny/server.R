options(shiny.maxRequestSize = 50 * 1024 ^ 2)
library(bdchecks)

# ------------- Local Data store ------------------------
data_store <-
  list(
    inputData = data.frame(),
    configuredCleaning = FALSE,
    customizedChecks = c(),
    customizedCheck = FALSE,
    flaggedData = data.frame(),
    flaggingDone = FALSE,
    cleanedData = data.frame(),
    cleaningDone = FALSE,
    questionnaire = bdclean::create_default_questionnaire(),
    
    warningData =
      data.frame(
        from = c("Startup"),
        message = c("bdclean Started"),
        time = "Now",
        icon = "rocket"
      ),
    
    cleaningThresholdControl = 7
  )


# ------------- End of Local Data store ------------------------



shinyServer(function(input, output, session) {
  data_store$inputData <- callModule(bdFile, "bdFileInput")
  
})# Server Ends Here