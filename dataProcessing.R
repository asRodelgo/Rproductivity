# load global packages ----------------------------------------------
library(plyr) # manipulate data 
library(dplyr) # manipulate data 
library(ggplot2) # charts
library(data.table) # fast operations
library(tidyr) # transform data
library(reshape2) # manipulate data
library(foreign) # read/write Stata (.dta) files
library(readstata13) # read data in version 13 of Stata
library(matrixStats) # calculate weighted median
# library(reldist) # calculate weighted quantiles
library(Hmisc) # calculate weighted quantiles
# --------------------------------------------------------
# Object: load the raw questionnaire data and perform:
# 1. Calculate summary statistics with/without weights.Check with Nona's
# 2. Robustness checks of 10-50 percentile vs. 50-90 percentile for employment. 
#    Check the signs of the covariances
# 3. Aggregate indicators by: 
#   Country
#   Type of firm: age, size, export status, tech. innovation status, foreign ownership
#   Sector: Aggregated, Manufacture and Services
# 4. Look at the ES country profile report for more visualization ideas
# --------------------------------------------------------

###
# 1. Calculate all summary stats as in Nona's excel file
###

# Read data
data <- read.dta13("data/TFPR_and_ratios.dta")

removeOutliers <- TRUE
outlierIQRfactor <- 10
indicator <- "n2a_d2"
indicatorQuantile <- "d2_n2a"
# calculate summary stats All sectors for labor cost (n2a) over sales (d2)
if (removeOutliers){
  
  data2 <- data %>%
    select(idstd,country,wt,sector_MS,l1,n2a_d2,d2_n2a,N_n2a_d2) %>%
    group_by(country) %>% 
    filter(!is.na(n2a_d2)) %>% # remove NAs
    filter((n2a_d2 < wtd.quantile(n2a_d2,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(n2a_d2,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(n2a_d2,100*round(wt,1),0.25,na.rm=TRUE)))
           & (n2a_d2 > wtd.quantile(n2a_d2,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(n2a_d2,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(n2a_d2,100*round(wt,1),0.25,na.rm=TRUE)))
           ) %>% # remove outliers
    mutate(N = sum(N_n2a_d2,na.rm=TRUE),
           #N_effective = sum(ifelse(!(is.na(n2a_d2)),1,0),na.rm=TRUE),
           n2a_d2_mean = weighted.mean(n2a_d2,wt,na.rm=TRUE),
           n2a_d2_median = weightedMedian(n2a_d2,wt,na.rm=TRUE),
           n2a_d2_sd = sqrt(sum(wt*(n2a_d2-n2a_d2_mean)^2,na.rm=TRUE)/sum(wt)),
           n2a_d2_se = n2a_d2_sd/sqrt(sum(wt)),
           n2a_d2_iqr = wtd.quantile(n2a_d2,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(n2a_d2,100*round(wt,1),0.25,na.rm=TRUE),
           n2a_d2_iqratio = wtd.quantile(n2a_d2,100*round(wt,1),0.75)/wtd.quantile(n2a_d2,100*round(wt,1),0.25),
           tot_emp10_50_d2_n2a = sum(ifelse((d2_n2a>=wtd.quantile(d2_n2a,100*round(wt,1),0.1,na.rm=TRUE)) & (d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           tot_emp50_90_d2_n2a = sum(ifelse((d2_n2a>=wtd.quantile(d2_n2a,100*round(wt,1),0.5,na.rm=TRUE)) & (d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           emp10_d2_n2a = ifelse(d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.1,na.rm=TRUE),l1,NA),
           median_emp10_d2_n2a = weightedMedian(emp10_d2_n2a,wt,na.rm=TRUE),
           emp50_d2_n2a = ifelse(d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.5,na.rm=TRUE),l1,NA),
           median_emp50_d2_n2a = weightedMedian(emp50_d2_n2a,wt,na.rm=TRUE),
           emp90_d2_n2a = ifelse(d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.9,na.rm=TRUE),l1,NA),
           median_emp90_d2_n2a = weightedMedian(emp90_d2_n2a,wt,na.rm=TRUE),
           ratio_median_emp10_50_d2_n2a = median_emp10_d2_n2a/median_emp50_d2_n2a,
           ratio_median_emp90_50_d2_n2a = median_emp90_d2_n2a/median_emp50_d2_n2a,
           emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
           emp_unweighted = l1/sum(l1,na.rm=TRUE),
           n2a_d2_OPcov = sum(wt*(n2a_d2-n2a_d2_mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
           n2a_d2_OPcovNoWeights = sum((n2a_d2-mean(n2a_d2,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE)
                    ) %>%
    select(-idstd,-wt,-sector_MS,-l1,-n2a_d2,-d2_n2a,-emp10_d2_n2a,-emp50_d2_n2a,-emp90_d2_n2a,
           -emp_weighted,-emp_unweighted)
  data2 <- data2[!(duplicated(data2)),]
  data2 <- arrange(data2,country)
  
} else {
  
  data2 <- data %>%
    select(idstd,country,wt,sector_MS,l1,n2a_d2,d2_n2a,N_n2a_d2) %>%
    group_by(country) %>% 
    filter(!is.na(n2a_d2)) %>% # remove NAs
    mutate(N = sum(N_n2a_d2,na.rm=TRUE),
           #N_effective = sum(ifelse(!(is.na(n2a_d2)),1,0),na.rm=TRUE),
           n2a_d2_mean = weighted.mean(n2a_d2,wt,na.rm=TRUE),
           n2a_d2_median = weightedMedian(n2a_d2,wt,na.rm=TRUE),
           n2a_d2_sd = sqrt(sum(wt*(n2a_d2-n2a_d2_mean)^2,na.rm=TRUE)/sum(wt)),
           n2a_d2_se = n2a_d2_sd/sqrt(sum(wt)),
           n2a_d2_iqr = wtd.quantile(n2a_d2,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(n2a_d2,100*round(wt,1),0.25,na.rm=TRUE),
           n2a_d2_iqratio = wtd.quantile(n2a_d2,100*round(wt,1),0.75)/wtd.quantile(n2a_d2,100*round(wt,1),0.25),
           tot_emp10_50_d2_n2a = sum(ifelse((d2_n2a>=wtd.quantile(d2_n2a,100*round(wt,1),0.1,na.rm=TRUE)) & (d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           tot_emp50_90_d2_n2a = sum(ifelse((d2_n2a>=wtd.quantile(d2_n2a,100*round(wt,1),0.5,na.rm=TRUE)) & (d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           emp10_d2_n2a = ifelse(d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.1,na.rm=TRUE),l1,NA),
           median_emp10_d2_n2a = weightedMedian(emp10_d2_n2a,wt,na.rm=TRUE),
           emp50_d2_n2a = ifelse(d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.5,na.rm=TRUE),l1,NA),
           median_emp50_d2_n2a = weightedMedian(emp50_d2_n2a,wt,na.rm=TRUE),
           emp90_d2_n2a = ifelse(d2_n2a<=wtd.quantile(d2_n2a,100*round(wt,1),0.9,na.rm=TRUE),l1,NA),
           median_emp90_d2_n2a = weightedMedian(emp90_d2_n2a,wt,na.rm=TRUE),
           ratio_median_emp10_50_d2_n2a = median_emp10_d2_n2a/median_emp50_d2_n2a,
           ratio_median_emp90_50_d2_n2a = median_emp90_d2_n2a/median_emp50_d2_n2a,
           emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
           emp_unweighted = l1/sum(l1,na.rm=TRUE),
           n2a_d2_OPcov = sum(wt*(n2a_d2-n2a_d2_mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
           n2a_d2_OPcovNoWeights = sum((n2a_d2-mean(n2a_d2,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE)
    ) %>%
    select(-idstd,-wt,-sector_MS,-l1,-n2a_d2,-d2_n2a,-emp10_d2_n2a,-emp50_d2_n2a,-emp90_d2_n2a,
           -emp_weighted,-emp_unweighted)
  data2 <- data2[!(duplicated(data2)),]
  data2 <- arrange(data2,country)
}


# plot original data
# Armenia2013
arm13 <- filter(data, country=="Armenia2013")[,c("wt","d2_n2a","l1","sector_MS","n2a_d2","n2a","d2","exrate_d2","d2_orig","n2a_orig")]
plot(arm13$d2_n2a)
quantile(arm13$d2_n2a,0.05,na.rm=TRUE)
iqr(arm13$d2_n2a,na.rm=TRUE)
arm13NoOut <- filter(arm13, n2a < quantile(arm13$n2a,0.75,na.rm=TRUE) + 10*iqr(arm13$n2a,na.rm=TRUE))
boxplot(arm13$d2_n2a)
arm13 <- mutate(arm13, prod = n2a_d2*wt)
write.csv(arm13, "arm13.csv",row.names = FALSE)
# table(filData$sum_wt)
# 
# sum(filter(data, country=="Costarica2010")$wt)/nrow(filter(data, country=="Costarica2010"))

