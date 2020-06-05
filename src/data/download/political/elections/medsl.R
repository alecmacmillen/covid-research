###################################################
# medsl.R
# Script for downloading MIT Election Data and Science Lab (MEDSL) data
# MEDSL package and data repo: https://github.com/MEDSL/elections
###################################################

# if (!require('devtools', quietly=TRUE)) install.packages('devtools')
# devtools::install_github('MEDSL/elections')

library(elections)
library(dplyr)
data(presidential_precincts_2016)
write.csv(presidential_precincts_2016, "data/raw/political/elections/medsl.csv", row.names=FALSE)
