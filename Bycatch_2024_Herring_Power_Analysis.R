#Setting up main datasets
main.data <- read.csv("data/Main_herring_2024_Catch_Bycatch.csv")

########################################################################################################
#Subset
sum.data<-subset(main.data, main.data$Net_group=="Sum")#sum of both nets
names(sum.data)

controls<-sum.data %>% filter(Net_treatment=="24hr") %>% select(Port, Trip_ID, Soak_duration, Bycatch_NOGA)
days<- sum.data %>% filter(Net_treatment=="12_day") %>% select(Port, Trip_ID, Soak_duration, Bycatch_NOGA)
nights<-sum.data %>% filter(Net_treatment=="12_night") %>% select(Port, Trip_ID, Soak_duration, Bycatch_NOGA)


## TAKE 10,000 bootstrap samples from control with sample sizes specified by day and night treatment

day.samples <- matrix(sample(controls$Bycatch_NOGA, size = 10000 * nrow(days), replace = TRUE),
                          10000, nrow(days))
day.statistics <- apply(day.samples, 1, sum)

power.day<-sum(ifelse(day.statistics>0,1,0))/10000


night.samples <- matrix(sample(controls$Bycatch_NOGA, size = 10000 * nrow(nights), replace = TRUE),
                      10000, nrow(nights))
night.statistics <- apply(night.samples, 1, sum)

power.night<-sum(ifelse(night.statistics>0,1,0))/10000


### POWER TO DETECT ANY BYCATCH IN TREATMENT SAMPLES assuming the same bycatch rate as in control samples

(1-power.day)
(1-power.night)