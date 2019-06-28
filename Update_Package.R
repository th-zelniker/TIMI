
# Update package ----------------------------------------------------------

library(roxygen2)
library(devtools)
load_all()
document()
setwd("..") # install from parent directory
install("TIMI")
library("TIMI")


# devtools::check()
# git remote add TIMI git@github.com:th-zelniker/TIMI.git
# git push -u TIMI master



# install_github("th-zelniker/TIMI")
# here we add
