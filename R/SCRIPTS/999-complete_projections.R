rm(list=ls())

# Load the libraries and clean the workspace
source('R/SCRIPTS/000-Libraries.R') 

# Project fertility rates in Puerto Rico
source('R/SCRIPTS/002-fertility_rate_projections.R') 

# Projections for all 3 scenarios
source('R/SCRIPTS/007-projections_20152100.R')
source('R/SCRIPTS/007-projections_20172100.R')
source('R/SCRIPTS/007-projections_20152100plus.R')

# Figures

source('R/SCRIPTS/figures.R')