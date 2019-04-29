
# Update package ----------------------------------------------------------

library(roxygen2)
library(devtools)
load_all()
document()
setwd("..") # install from parent directory
install("TIMI")
library("TIMI")
# devtools::check()

# install_github("th-zelniker/TIMI")
