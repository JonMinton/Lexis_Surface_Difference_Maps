# Code for figs for EJE paper

# To do:  -----------------------------------------------------------------

# 1 ) Fix e65 variables for country differences 

# Load packages -----------------------------------------------------------

rm(list=ls())

require(plyr)
require(tidyr)
require(dplyr)
require(stringr)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(grid)

require(xtable)

# for smoothing
require(fields) 
require(spatstat)



# Manage data (base and derived) ------------------------------------------


source("scripts/manage_data.R")

  
# Figures and tables -----------------------------------------------------------------



