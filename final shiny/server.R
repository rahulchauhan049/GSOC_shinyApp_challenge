options(shiny.maxRequestSize = 5000 * 1024 ^ 2)
library(bdchecks)

inputDataset <- data.frame()
shinyServer(function(input, output, session) {
  
  inputDataset <- callModule(inputTabServer, "input")

  
  # --------- Plotting taxomic plots -------

 callModule(taxonomicTabServer, "taxo", inputDataset())

  #----------End of Taxonomic Tab-----------



  # --------- Plotting spatial plots -------

  callModule(spatialServer, "spatial", inputDataset())

  #----------End of spatial Tab-----------


  # --------- Plotting Temporal plots -------

  callModule(temporalServer, "temporal", inputDataset())

  #----------End of Temporal Tab-----------


})# Server Ends Here