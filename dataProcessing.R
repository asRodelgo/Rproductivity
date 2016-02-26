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
#     Country
#     Type of firm: age, size, export status, tech. innovation status, foreign ownership
#     Sector: Aggregated, Manufacture and Services
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
N_indicator <- paste0("N_",indicator)
# calculate summary stats All sectors for labor cost (n2a) over sales (d2)
if (removeOutliers){
  
  data2 <- data %>%
    select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicator),
            indicatorQuantile = one_of(indicatorQuantile),N_indicator = one_of(N_indicator)) %>%
    group_by(country) %>% 
    filter(!is.na(indicator)) %>% # remove NAs
    filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
           & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
           ) %>% # remove outliers
    mutate(N = sum(N_indicator,na.rm=TRUE),
           #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
           indicator_mean = weighted.mean(indicator,wt,na.rm=TRUE),
           indicator_median = weightedMedian(indicator,wt,na.rm=TRUE),
           indicator_sd = sqrt(sum(wt*(indicator-indicator_mean)^2,na.rm=TRUE)/sum(wt)),
           indicator_se = indicator_sd/sqrt(sum(wt)),
           indicator_iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
           indicator_iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
           tot_emp10_50_indicatorQuantile = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           tot_emp50_90_indicatorQuantile = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           emp10_indicatorQuantile = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA),
           median_emp10_indicatorQuantile = weightedMedian(emp10_indicatorQuantile,wt,na.rm=TRUE),
           emp50_indicatorQuantile = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA),
           median_emp50_indicatorQuantile = weightedMedian(emp50_indicatorQuantile,wt,na.rm=TRUE),
           emp90_indicatorQuantile = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA),
           median_emp90_indicatorQuantile = weightedMedian(emp90_indicatorQuantile,wt,na.rm=TRUE),
           ratio_median_emp10_50_indicatorQuantile = median_emp10_indicatorQuantile/median_emp50_indicatorQuantile,
           ratio_median_emp90_50_indicatorQuantile = median_emp90_indicatorQuantile/median_emp50_indicatorQuantile,
           emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
           emp_unweighted = l1/sum(l1,na.rm=TRUE),
           indicator_OPcov = sum(wt*(indicator-indicator_mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
           indicator_OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE)
                    ) %>%
    select(-idstd,-wt,-sector_MS,-l1,-indicator,-indicatorQuantile,-emp10_indicatorQuantile,-emp50_indicatorQuantile,-emp90_indicatorQuantile,
           -emp_weighted,-emp_unweighted)
  data2 <- data2[!(duplicated(data2)),]
  data2 <- arrange(data2,country)
  
} else {
  
  data2 <- data %>%
    select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicator),
           indicatorQuantile = one_of(indicatorQuantile),N_indicator = one_of(N_indicator)) %>%
    group_by(country) %>% 
    filter(!is.na(indicator)) %>% # remove NAs
    mutate(N = sum(N_indicator,na.rm=TRUE),
           #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
           indicator_mean = weighted.mean(indicator,wt,na.rm=TRUE),
           indicator_median = weightedMedian(indicator,wt,na.rm=TRUE),
           indicator_sd = sqrt(sum(wt*(indicator-indicator_mean)^2,na.rm=TRUE)/sum(wt)),
           indicator_se = indicator_sd/sqrt(sum(wt)),
           indicator_iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
           indicator_iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
           tot_emp10_50_indicatorQuantile = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           tot_emp50_90_indicatorQuantile = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
           emp10_indicatorQuantile = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA),
           median_emp10_indicatorQuantile = weightedMedian(emp10_indicatorQuantile,wt,na.rm=TRUE),
           emp50_indicatorQuantile = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA),
           median_emp50_indicatorQuantile = weightedMedian(emp50_indicatorQuantile,wt,na.rm=TRUE),
           emp90_indicatorQuantile = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA),
           median_emp90_indicatorQuantile = weightedMedian(emp90_indicatorQuantile,wt,na.rm=TRUE),
           ratio_median_emp10_50_indicatorQuantile = median_emp10_indicatorQuantile/median_emp50_indicatorQuantile,
           ratio_median_emp90_50_indicatorQuantile = median_emp90_indicatorQuantile/median_emp50_indicatorQuantile,
           emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
           emp_unweighted = l1/sum(l1,na.rm=TRUE),
           indicator_OPcov = sum(wt*(indicator-indicator_mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
           indicator_OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE)
    ) %>%
    select(-idstd,-wt,-sector_MS,-l1,-indicator,-indicatorQuantile,-emp10_indicatorQuantile,-emp50_indicatorQuantile,-emp90_indicatorQuantile,
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

