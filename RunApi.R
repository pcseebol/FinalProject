# File for running the API
library(plumber)
r = plumb("API.r")
r$run(port = 8000)
#is hosted at http://localhost:8000
