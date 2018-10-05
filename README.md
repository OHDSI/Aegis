# AEGIS
Application for Epidemiological Geographic Information System (AEGIS): An open source spatial analysis tool based on CDM

# Getting Started
```r
install.packages("devtools")
devtools::install_github("ohdsi/DatabaseConnector")
devtools::install_github("ohdsi/SqlRender")
devtools::install_github("ohdsi/aegis")
```
If you need to use Bayesian mapping (small area estimation), you need to install R-INLA as follows:
```r
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```
