GIS.Indirect.AgeGenderadjust <- function(df, fraction, conf.level = 0.95){
  zv <- qnorm(0.5*(1+conf.level))
  
  #Calculation to indirect age and gender standardization rate
    stdratedf <-data.frame(df %>%
                             group_by(age_cat, sex_cat) %>%
                             summarise(targetsum = sum(target_count),
                                       outcomesum = sum(outcome_count)))
    
    stdratedf$stdrate <- stdratedf$outcomesum / stdratedf$targetsum
    
    
    tmp <- dplyr::left_join(df, stdratedf, by=c("age_cat" = "age_cat", "sex_cat" = "sex_cat"))
    tmp$stdexpected <- tmp$target_count * tmp$stdrate
    
    tmp <- data.frame(tmp%>%
                        group_by(gadm_id) %>%
                        summarise(target_count = sum(target_count),
                                  outcome_count = sum(outcome_count),
                                  expected = sum(stdexpected)))
    
  #Standardized Incidence Ratio calculation by administirative district
  
    tmp$sir <- tmp$outcome_count / tmp$expected
    
    tmp$logsirlower <- log(tmp$sir) - zv * (1/sqrt(tmp$outcome_count))
    tmp$logsirupper <- log(tmp$sir) + zv * (1/sqrt(tmp$outcome_count))
    
    tmp$sirupper <- exp(tmp$logsirupper)
    tmp$sirlower <- exp(tmp$logsirlower)
  
  
  #Proportion calculation 
  
    #Crude proportion
      df$crdprop <- (df$outcome_count / df$target_count) * fraction
    
    #Standaridzation proportion
      tmp$stdprop <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$sir) * fraction 
      tmp$stdproplower <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$sirlower) * fraction 
      tmp$stdpropupper <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$sirupper) * fraction 
      
      return(tmp)
}