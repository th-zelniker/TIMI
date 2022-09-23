
# Update package ----------------------------------------------------------
library(roxygen2)
library(devtools)
load_all()
document()
setwd("..") # install from parent directory
install("TIMI")
library("TIMI")

# Test Package:
# devtools::test()

# Push Commits in Shell
# git remote add origin https://github.com/th-zelniker/TIMI
# git remote add TIMI git@github.com:th-zelniker/TIMI.git
# git push -u TIMI master


# Install package from github:
# devtools::install_github("th-zelniker/TIMI")
