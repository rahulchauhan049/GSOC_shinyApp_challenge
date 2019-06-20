library("r2d3")
r2d3(data=jsonlite::read_json("src/json/flare.json"),css="src/css/barchart.css", d3_version = 3, script = "src/js/barchart.js")
