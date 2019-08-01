# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "dataInput" )
golem::add_module( name = "input" ) # Name of the module
golem::add_module( name = "spatial" )
golem::add_module( name = "temporal" )
golem::add_module( name = "taxonomic" )

## 2.2 Add dependencies

usethis::use_package( "data.table" ) # To call each time you need a new package
usethis::use_package( "shinydashboard" )
usethis::use_package( "finch" )
usethis::use_package( "DT" )
usethis::use_package( "bdchecks" )
usethis::use_package( "leaflet" )
usethis::use_package( "shinyjs" )
usethis::use_package( "plotly" )
usethis::use_package( "dplyr" )
usethis::use_package( "bdvis" )
usethis::use_package( "nycflights13" )
usethis::use_package( "ggstat" )
usethis::use_package( "purrr" )
usethis::use_package( "data.tree" )
usethis::use_package( "circlepackeR" )
usethis::use_package( "collapsibleTree" )
usethis::use_package( "networkD3" )
usethis::use_package( "bdchecks" )


## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("bdvisShiny")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
