library(estimatr)
library(plyr)
library(readr)
library("xlsx")
library(ggplot2)
library(stargazer)


setwd('/Users/phoebespear/Desktop/MIT_20-21/14.33/Short_Paper')

#load raw voting data
raw_voting <- read.csv('Voting.csv')

#make new voting data set with extra variables to indicate difference in rain from average, and in person voter turnout 
voting <- raw_voting[!is.na(raw_voting$voted),]
voting$prcp_diff <- voting$prcpMm - voting$prcpMm1015
voting$physical_turnout <- voting$votedPhysical/(voting$registered - (voting$voted-voting$votedPhysical))
voting$turnout_final <- ifelse(voting$physical_turnout == 0 , voting$voted/voting$registered,voting$physical_turnout)

#get table of stats on voting dataset, fields of interest
voting_important <- dplyr::select(voting, voted, turnout, physical_turnout, prcp_diff, shareWhite, educCollegeUp)
stat.desc(voting_important)
voting_data_stats <- stat.desc(voting_important)
stargazer(voting_data_stats)

#regression to see affect of rain on voter turnout controlling for close election, share white, and percentage with college education 
rain_regression <- iv_robust(turnout_final ~ prcp_diff  + shareWhite  + educCollegeUp | prcp_diff  + shareWhite  + educCollegeUp , data = voting)
rain_regression_df <- tidy(rain_regression)


#show that education and income are directly related (controlling for one controls for another)
ggplot(data=voting, aes(x=educCollegeUp, y=medianIncome, group=1)) +
  geom_point(color='blue') + 
  geom_smooth(method="lm", color = 'black') 

#new health + county level data 
health_data_dir = "/Users/phoebespear/Desktop/MIT_20-21/14.33/Short_Paper/state_health_data"
health_files = list.files(path=health_data_dir, pattern="*.xls", full.names=TRUE)
health_df = read.xlsx(health_files[1], sheetName='Ranked Measure Data')

# Loop through the remaining files and merge them to the existing data frame
for (file in health_files[-1]) {
  newFile = read.xlsx(file, sheetName ='Ranked Measure Data')
  health_df = rbind(health_df, newFile, all=TRUE)
}

#clean health data so it can be merged with voting data 
names <- lapply(health_df[1,], as.character)
names <- unlist(names)
names <- as.character(names)
colnames(health_df) <- names
health_df <- health_df[-1,]
names(health_df)[names(health_df) == 'FIPs'] <- 'FIPS'
health_df$FIPs <- sub('^+0', '', health_df$FIPs)

#merge datasets 
voting_and_health <- merge(voting,health_df, by="FIPS")

#clean commute data 
names(voting_and_health)[names(voting_and_health) == '% Long Commute - Drives Alone' ] <- 'perc_LongCommute' 
voting_and_health$perc_LongCommute <- sapply(voting_and_health$perc_LongCommute, as.numeric)
voting_and_health$perc_LongCommute_edit <- (voting_and_health$perc_LongCommute)/10

#get stats on commute data  
commute_important <- voting_and_health$perc_LongCommute_edit
stat.desc(commute_important/100)

#calculate sd and mean of commute and subset stat significantly long v short commutes
sd_longCommute <- sd(voting_and_health$perc_LongCommute_edit)
mean_longCommute <- mean(voting_and_health$perc_LongCommute_edit)
long_commutes <- subset(voting_and_health,perc_LongCommute_edit >= mean_longCommute + sd_longCommute )
short_commutes <- subset(voting_and_health,perc_LongCommute_edit <= mean_longCommute - sd_longCommute )
avg_commutes <- subset(voting_and_health, mean_longCommute - sd_longCommute <=  perc_LongCommute_edit & perc_LongCommute_edit <= mean_longCommute + sd_longCommute )

#show that effect of rain is stronger on communities with longer commutes (create table information)
long_commute_regression <- iv_robust(turnout_final ~ prcp_diff  + shareWhite  + educCollegeUp | prcp_diff  + shareWhite  + educTillHS , data = long_commutes)
long_commute_regression_df <- tidy(long_commute_regression)

short_commute_regression <- iv_robust(turnout_final ~ prcp_diff  + shareWhite  + educCollegeUp | prcp_diff  + shareWhite  + educTillHS , data = short_commutes)
short_commute_regression_df <- tidy(short_commute_regression)

average_commute_regression <- iv_robust(turnout_final ~ prcp_diff  + shareWhite  + educCollegeUp | prcp_diff  + shareWhite  + educTillHS , data = avg_commutes)
average_commute_regression_df <- tidy(average_commute_regression)

#create final tables of regression results 
stargazer(rain_regression_df, long_commute_regression_df, short_commute_regression_df, average_commute_regression_df, title="Regression Results", align=TRUE, dep.var.labels=c("Overall Rating","High Rating"))

#graph commute length 
voting_and_health$commute_length <- ifelse(voting_and_health$perc_LongCommute_edit <= mean_longCommute - sd_longCommute, "short_commute", ifelse((voting_and_health$perc_LongCommute_edit >= mean_longCommute + sd_longCommute), "long_commute", "average_commute"))
voting_extreme_commute <- subset(voting_and_health,voting_and_health$commute_length != 'average_commute') 
voting_extreme_commute <- subset(voting_extreme_commute,voting_extreme_commute$prcp_diff != 0) 
voting_extreme_commute$prcp_diff = voting_extreme_commute$prcpMm - voting_extreme_commute$prcpMm1015

ggplot(data=voting_extreme_commute, aes(x=prcp_diff, y=turnout_final, color=commute_length)) +
  geom_point() +
  xlim(-2, 2) + 
  geom_smooth(method="lm", se = FALSE) + 
  xlab("Percipation Difference from Average (mm)") + 
  ylab("Voter Turnout (in person) ") + 
  ggtitle("Voter Turnout in County's where people have Longer Commutes are More Effected by Rain")

write.csv(voting_and_health, "voting_and_health.csv")
write.csv(voting, "voting_edited.csv")


