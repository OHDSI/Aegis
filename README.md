# AEGIS
Application for Epidemiological Geographic Information System (AEGIS): An open source spatial analysis tool based on CDM

# Getting Started
```r
install.packages("devtools")
devtools::install_github("cran/raster", ref="2.6-7")
devtools::install_github("ohdsi/DatabaseConnector")
devtools::install_github("ohdsi/SqlRender")
devtools::install_github("ohdsi/aegis")

AEGIS::AEGIS()
```
If you need to use Bayesian mapping (small area estimation), you need to install R-INLA as follows:
```r
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```

# Demo Video

<a href="http://www.youtube.com/watch?feature=player_embedded&v=tExqsZU7qYg
" target="_blank"><img src="http://img.youtube.com/vi/tExqsZU7qYg/0.jpg" 
alt="AEGIS" width="500" height="300" border="10" /></a>