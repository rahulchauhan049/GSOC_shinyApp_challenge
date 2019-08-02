## code to prepare `dataset` dataset goes here
hyena <- read.csv("data-raw/hyenaData.csv")
mammals <- read.csv("data-raw/smallData.csv")
usethis::use_data(mammals)
