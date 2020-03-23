###################
### DATA PREP
##################
rm(list=ls())
source('R/SCRIPTS/000-Libraries.R')      # loading in the libraries

# Loading in the Human Fertility Database estimates from Hauer & Schmertmann 2020.
HMDHFDtfr <- read_csv("R/DATA-RAW/HMDHFD-allbTFR_06172017.csv") %>%
  dplyr::select(-lagTFR) %>%
  group_by(Code) %>%
  mutate(delta1yearTFR = post_mean/lag(post_mean)) %>% # calculating the 1 year changes.
  na.omit

ggplot(data=HMDHFDtfr, aes(x = delta1yearTFR)) +
  geom_density()

# Calculating the largest single year increase in fertility
increase <- max(HMDHFDtfr$delta1yearTFR)


# Loading in the PR population data
K05_pop <- read_csv("R/DATA-RAW/pr2000_2017.csv")

forecastfertrates <- function(launch_year, increase){
  K05t <- K05_pop %>%
    filter(YEAR <= launch_year) %>%
    group_by(YEAR, STATE, RACE, SEX, AGE) %>%
    dplyr::summarise(POPULATION = sum(POPULATION)) %>%
    ungroup()
  newbornst <- K05t %>%
    filter(AGE == 1, SEX == 0) %>% # AGE 1 = newborns. SEX 0 = Total
    group_by(STATE, RACE, YEAR)  %>%
    dplyr::summarise(Newborns = sum(POPULATION))
  childbearingt <- K05t %>%
    filter(AGE %in% c(4,5,6,7,8,9,10), # women ages 15-49
           SEX == "2" ) %>%
    group_by(STATE, YEAR) %>%
    dplyr::summarise(Women1550 = sum(POPULATION)) %>%
    left_join(., newbornst) %>%
    mutate(fertrat = Newborns/Women1550) 
  
  childbearingt$SEX <- "2"
  childbearingt[mapply(is.infinite, childbearingt)] <- NA
  childbearingt[mapply(is.nan, childbearingt)] <- NA
  childbearingt[is.na(childbearingt)] <-0
  num <- seq(1,FORLEN,5)
  predcwr = function(ccr, sex, x, DF){
    hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
    b <- max(DF[[as.character(ccr)]][which(DF$RACE== x)])*1.01
    a <- -0.00000001
    y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$STATE== x & DF$SEX == sex & DF$RACE == this.race)]))
    
    num <- seq(1,FORLEN,5)
    pred<- tryCatch(round(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),],5)
                    , error=function(e) array(hyndtran(DF$fertrat[which.max(DF$YEAR)]), c(STEPS)))
    pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
    return(round(pred2,6))#
  }
  forecast<- forecast(arima(childbearingt$fertrat, order = arima_order), h= FORLEN)
  mean <- forecast$mean[c(num)]*increase
  lower <- forecast$lower[c(num)]*increase
  upper <- forecast$upper[c(num)]*increase
  forecasst<- as.tibble(t(rbind(mean, lower, upper)))
  
  dat <- data.frame()
  dat<-rbind(dat,forecasst %>%
                             mutate(STATE = 72,
                                    RACE= 0,
                                    SEX = 2))
  return(dat)
}

fertrats_20152100<- forecastfertrates(2015, increase = 1)
fertrats_20172100<- forecastfertrates(2017, increase = 1)
fertrates_20152100plus <- forecastfertrates(2015, increase = increase)


  
write_csv(fertrats_20152100, "R/DATA-PROCESSED/state-level-fert-rates_20152100.csv")
write_csv(fertrats_20172100, "R/DATA-PROCESSED/state-level-fert-rates_20172100.csv")
write_csv(fertrates_20152100plus, "R/DATA-PROCESSED/state-level-fert-rates_20152100plus.csv")