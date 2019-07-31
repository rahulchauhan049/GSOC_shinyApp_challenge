## code to prepare `dataset` dataset goes here

mammalsData <- read.csv("data-raw/smallData.csv")
hyenaData <- read.csv("data-raw/hyenaData.csv")
usethis::use_data(mammalsData)
usethis::use_data(hyenaData)
