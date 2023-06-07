#--------------------------------------#
# Packages file
#--------------------------------------#

# Get the list of packages.

packagelist <- c(
  "dplyr",
  "tidyverse",
  "ggplot2",
  "MASS",
  "scales",
  "lubridate",
  "rlist",
  "distr",
  "units",
  "lme4",
  "haven",
  "merTools",
  "lmerTest",
  "splines",
  "ggeffects",
  "sjPlot",
  "matlib",
  "ggthemes",
  "mfp",
  "reshape2",
  "data.table"
)

# Get packages function.

get.packages <- function(plist) {
  
  #oldw <- getOption("warn") # Suppress annoying warnings.
  #options(warn = -1)
  
  for(i in 1:length(plist)) {
    name <- as.character(plist[i])
    if(!(name %in% installed.packages())) {
      install.packages(name)
      require(name, character.only=TRUE)
    } else {
      require(name, character.only=TRUE)
    }
  }
  
  #options(warn = oldw) # Turn the warnings back on.
  
}

# Get the packages.

get.packages(packagelist)





