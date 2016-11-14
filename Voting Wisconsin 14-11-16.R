setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
library(XML); require(rvest); require(stringr);require(xlsx);require(dplyr);require(plyr)
require(agricolae); require(ggplot2); library(gridExtra)

options(scipen=999) # Effectively disables scientific numbering

one.graph.width = 20;one.graph.height = 15

# Function to take rightmost digit
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Reading in vote data ----------------------------------------------------

# Following guide here: https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

url <- "http://wisconsinvote.org/results/President%20-%20General/county-results"
voting <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@class="views-table cols-3"]') %>%
  html_table()

lapply(voting, '[[', 'Candidates')

voting.county <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@class="views-table cols-3"]/caption') %>%
  html_text()

voting.county = gsub("County: ","", voting.county)
as.character(voting.county)

votinglist.test <- mapply(cbind, voting,"voting.county"=as.character(voting.county), SIMPLIFY=F)
voting = votinglist.test

df.test = data.frame(1:504)
df.test$county = unlist(lapply(voting, '[[', 'voting.county'))
df.test$cand= unlist(lapply(voting, '[[', 'Candidates'))
df.test$votes.rec= unlist(lapply(voting, '[[', 'Votes Received'))
df.test$votes.perc= unlist(lapply(voting, '[[', 'Vote Percentage (In County)'))
df.test$X1.504 = NULL
voting.df = df.test

voting.df$votes.rec = as.integer(gsub(",","",voting.df$votes.rec))

## Historical votes
url <- "http://uselectionatlas.org/RESULTS/compare.php?year=2016&fips=55&f=0&off=0&elect=0&type=state"
voting.history <- url %>%
  read_html() %>%
  html_nodes(xpath='//table[@id="data"]') %>%
  html_table(fill=T)



# Downloading polls ------------------------------------------------------
# Loading poll data for Wisconsin 

# Gathering poll data instructions from here: https://pkremp.github.io/report.html

# rm(list = ls())
options(mc.cores = parallel::detectCores())

library(rstan)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(curl)
library(shinystan)
library(rmarkdown)

setwd("C:\\Users\\s_cas\\Documents\\GitHub\\polls")

####################
# Useful functions #
####################

corr_matrix <- function(m){
  (diag(m)^-.5 * diag(nrow = nrow(m))) %*% m %*% (diag(m)^-.5 * diag(nrow = nrow(m))) 
}

cov_matrix <- function(n, sigma2, rho){
  m <- matrix(nrow = n, ncol = n)
  m[upper.tri(m)] <- rho
  m[lower.tri(m)] <- rho
  diag(m) <- 1
  (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) 1/(1 + exp(-x))


start_date <- as.Date("2016-10-01") # Keeping all polls after October 1, 2016.

########################################################
# Downloading poll data from the HuffPost Pollster API #
########################################################

# Creating a vector of URL stubs, to fetch csv files for each state from HuffPost
# Note that FL and CA have different stubs.

state_name <- datasets::state.name
names(state_name) <- datasets::state.abb

stubs <- c("2016-general-election-trump-vs-clinton",
           "2016-wisconsin-president-trump-vs-clinton")

names(stubs) <- c("--","WI")

stubs <- stubs[order(names(stubs))]

download_csv <- function(stub){
  url <- paste("http://elections.huffingtonpost.com/pollster/", stub, ".csv", sep = "")
  connection <- curl(url, "r") 
  df <- read.csv(connection, stringsAsFactors = FALSE)
  close(connection)
  message("Downloaded ", url)
  return(df)
}

# Download the data and put everything in a single df
all_polls <- map_df(stubs, download_csv, .id = "state")
write.csv(all_polls, "all_polls.csv")

# all_polls = read.csv("all_polls.csv", stringsAsFactors = FALSE, header = TRUE)

colnames(all_polls) <- colnames(all_polls) %>% tolower

# I'm only interested in the overall likely voters
str(all_polls$population)
selected_polls = subset(all_polls, population == "Likely Voters" | population == "Registered Voters" |
                          population == "Adults")

selected_polls$start.date = as.Date(selected_polls$start.date)

selected_polls = subset(selected_polls, start.date > as.Date("2016-11-01"))

mean.trump.nov = mean(selected_polls$trump)
mean.clinton.nov = mean(selected_polls$clinton)

# Plotting the polls over time in November
polls.wisc.point = ggplot(selected_polls,aes(x = start.date, y = trump)) +
  geom_smooth(colour = "red") + geom_smooth(aes(y = selected_polls$clinton), colour = "blue")
polls.wisc.point

# Comparing candidate results overall -------------------------------------
# Overall results vs polls
overall.results.group = group_by(voting.df,cand)
  overall.results = dplyr::summarise(overall.results.group,
                                     votes.rec = sum(votes.rec))

  overall.results$votes.perc = (overall.results$votes.rec / sum(overall.results$votes.rec)) * 100

  overall.results.vis = ggplot(overall.results,aes(x = cand, y = votes.perc, fill = cand)) +
    geom_bar(stat = "identity") +
    geom_point(stat = "identity", aes(y=c(rep(mean.clinton.nov,7))),colour = "blue", show.legend = F) +
    geom_point(stat = "identity", aes(y=c(rep(mean.trump.nov,7))),colour = "red",show.legend = F) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  legend.position = "none") +
    scale_fill_manual(values = c("grey","lightcoral","grey","light blue","grey","grey","grey"))
  overall.results.vis
  
result.clinton = overall.results$votes.perc[overall.results$cand == "Hillary Clinton"]
result.trump = overall.results$votes.perc[overall.results$cand == "Donald J. Trump"]

poll.res.compare = data.frame(1:2)
poll.res.compare$cand = c("Hillary Clinton","Donald J. Trump")
poll.res.compare[1] = NULL
poll.res.compare$polls.nov = c(mean.clinton.nov,mean.trump.nov)
poll.res.compare$result = c(result.clinton,result.trump)
poll.res.compare$diff.from.poll = poll.res.compare$result - poll.res.compare$polls.nov

poll.res.compare.vis = ggplot(poll.res.compare,aes(x = cand, y = diff.from.poll, fill = cand)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightcoral","light blue"))
poll.res.compare.vis

voting.df$votes.rec.last.digit = as.integer(substrRight(df.test$votes.rec, 1))

voting.df$votes.digit.is.5 = ifelse(voting.df$votes.rec.last.digit == 5,1,0)
voting.df$votes.digit.is.5 = ifelse(voting.df$votes.rec == 0 ,NA,voting.df$votes.digit.is.5)

voting.df$votes.digit.is.0 = ifelse(voting.df$votes.rec.last.digit == 0,1,0)
voting.df$votes.digit.is.0 = ifelse(voting.df$votes.rec == 0 ,NA,voting.df$votes.digit.is.0)

voting.df$votes.digit.is.5.or.0 = ifelse(voting.df$votes.rec.last.digit == 5 |
                                         voting.df$votes.rec.last.digit == 0,1,0)
voting.df$votes.digit.is.5.or.0 = ifelse(voting.df$votes.rec == 0 ,NA,voting.df$votes.digit.is.5.or.0)


# Summarising the data
summary.df = data.frame(unique(voting.df$cand))
colnames(summary.df) = "cand"
summary.df$perc.5.or.0 = tapply(voting.df$votes.digit.is.5.or.0[!is.na(voting.df$votes.digit.is.0)],voting.df$cand[!is.na(voting.df$votes.digit.is.0)],sum,na.rm=T) /
  tapply(voting.df$votes.digit.is.5.or.0[!is.na(voting.df$votes.digit.is.0)],voting.df$cand[!is.na(voting.df$votes.digit.is.0)],length)

summary.df$perc.5 = tapply(voting.df$votes.digit.is.5[!is.na(voting.df$votes.digit.is.0)],voting.df$cand[!is.na(voting.df$votes.digit.is.0)],sum,na.rm=T) /
  tapply(voting.df$votes.digit.is.5[!is.na(voting.df$votes.digit.is.0)],voting.df$cand[!is.na(voting.df$votes.digit.is.0)],length)

summary.df$perc.0 = tapply(voting.df$votes.digit.is.0[!is.na(voting.df$votes.digit.is.0)],voting.df$cand[!is.na(voting.df$votes.digit.is.0)],sum,na.rm=T) /
  tapply(voting.df$votes.digit.is.0[!is.na(voting.df$votes.digit.is.0)],
         voting.df$cand[!is.na(voting.df$votes.digit.is.0)],length)

voting.df$votes.perc = as.numeric(gsub("%","",voting.df$votes.perc))

## Visualising
last.dig.vis = ggplot(summary.df,aes(x = cand, y = perc.5.or.0*100,fill = cand)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("grey","lightcoral","grey","light blue","grey","grey","grey")) +
  geom_bar(stat = "identity") + theme(legend.position = "none")
last.dig.vis

last.dig.vis.5 = ggplot(summary.df,aes(x = cand, y = perc.5*100,fill = cand)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("grey","lightcoral","grey","light blue","grey","grey","grey")) +
  geom_bar(stat = "identity") + theme(legend.position = "none")
last.dig.vis.5

last.dig.vis.0 = ggplot(summary.df,aes(x = cand, y = perc.0*100,fill = cand)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("grey","lightcoral","grey","light blue","grey","grey","grey")) +
  geom_bar(stat = "identity") + theme(legend.position = "none")
last.dig.vis.0

# Comparing by county -------------------------------------------------------------

# Looking at county data
county.summary.df = data.frame(unique(voting.df$county))
colnames(county.summary.df) = "county"
county.summary.df$trump = as.numeric(voting.df$votes.perc[voting.df$cand == "Donald J. Trump"])
county.summary.df$trump.num = as.numeric(voting.df$votes.rec[voting.df$cand == "Donald J. Trump"])
county.summary.df$clinton = as.numeric(voting.df$votes.perc[voting.df$cand == "Hillary Clinton"])
county.summary.df$clinton.num = as.numeric(voting.df$votes.rec[voting.df$cand == "Hillary Clinton"])
county.summary.df$turnout = tapply(voting.df$votes.rec,voting.df$county,sum)
county.summary.df$turnout = as.integer(county.summary.df$turnout)

county.summary.df$perc.diff = county.summary.df$trump - county.summary.df$clinton
county.summary.df$num.diff = county.summary.df$trump.num - county.summary.df$clinton.num

county.summary.df$winner = ifelse(county.summary.df$perc.diff > 0, "Trump","Clinton")
county.summary.df$winner.party = ifelse(county.summary.df$perc.diff > 0, "Rep","Dem")

county.summary.df$mean.poll.trump = mean.trump.nov
county.summary.df$mean.poll.clinton = mean.clinton.nov

county.summary.df$turnout.all = sum(county.summary.df$turnout)
overall.results$turnout.all = sum(county.summary.df$turnout)

# Calculating turnout by county
# Calculating turnout of registered voters by November 2nd. BUT, this has been seen in past to
# increase by up to 11% on voting day, see 'general_election_voter_turnout...xlsx'. So maybe
# The turnout value here is an over-estimate

## NOTE: Here http://www.wisconsinvote.org/faq it says you can register on the day to vote,
## so maybe this registered to vote value is not accurate?
setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
registered.voters = read.xlsx("registeredvotersbycounty_xlsx_45404MOD.xlsx", sheetIndex = 1,
                              stringsAsFactors = F,header=T)
registered.voters = registered.voters[-c(73,74),]
registered.voters$NA. = NULL
registered.voters$county = gsub( " COUNTY.*$", "", registered.voters$county)
registered.voters$registered.voters = as.integer(registered.voters$registered.voters)

county.summary.df = county.summary.df[order(county.summary.df$county),]
registered.voters = registered.voters[order(registered.voters$county),]

county.summary.df$reg.voters.all = sum(registered.voters$registered.voters)
overall.results$reg.voters.all = sum(registered.voters$registered.voters)

county.summary.df$registered.voters = registered.voters$registered.voters
county.summary.df$turnout.perc.reg = county.summary.df$turnout / county.summary.df$registered.voters
county.summary.df$turnout.state.reg = sum(county.summary.df$turnout) / sum(county.summary.df$registered.voters) 
overall.results$turnout.state.reg = mean(county.summary.df$turnout.state.reg)

## Calculating turnout of voting age population
## I can only estimate the turnout per county based on the 2012 voting age populations
# from file "general_election_voter_registration... this has the 2016 value"
overall.results$voting.age.pop = 4449170
overall.results$turnout.overall = sum(overall.results$votes.rec / overall.results$voting.age.pop)

voting.age.people = read.xlsx("2012_presidential_general_election_turnout_xlsx_11916.xlsx",
                              sheetIndex = 2, stringsAsFactors = F, header = T)
voting.age.people$county = gsub( " COUNTY.*$", "", voting.age.people$County)

total.2012.pop.wi = sum(voting.age.people$Voting.Age.Estimate.2012)

# Estimated 1.01% increase in voting age population from 2012 to 2016. I will multiply the
# 2012 county voter data by 101.01% to try to account for this increase. Yes, this is an assumption,
# but newer data is not yet available!
prop.2016.voting.age.to.2012.est = mean(overall.results$voting.age.pop) / total.2012.pop.wi

voting.age.people$Voting.Age.Estimate.2016 = voting.age.people$Voting.Age.Estimate.2012 * prop.2016.voting.age.to.2012.est
voting.age.people$Voting.Age.Estimate.2016 = ceiling(voting.age.people$Voting.Age.Estimate.2016)

county.summary.df$total.voters.age.est = voting.age.people$Voting.Age.Estimate.2016
county.summary.df$turnout.perc.allage.est = county.summary.df$turnout / county.summary.df$total.voters.age.est

# order.df = data.frame(1:length(county.summary.df$county))

## VISUALISING THE 2016 RESULTS
county.summary.df = county.summary.df[order(county.summary.df$trump),]
county.summary.df$ordered.county.2016.trump = c(1:length(county.summary.df$trump))

# Visualise absolute percentage results
county.perc.winner = ggplot(county.summary.df,
                            aes(x = county.summary.df$ordered.county.2016.trump, y = trump,
                                              fill = winner)) +
  geom_bar(aes(y = county.summary.df$turnout.perc.allage.est*100), stat = "identity", 
           fill = "grey") +
  geom_bar(stat = "identity",alpha = 0.6) +

  scale_y_continuous(
    name = "Percentage vote for Trump in Wisconsin counties.\n
          Grey bar behind indicates percentage turnout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.2016.trump,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner


county.summary.df = county.summary.df[order(county.summary.df$turnout.perc.allage.est),]
county.summary.df$ordered.county.2016.turnout.perc.diff = c(1:length(county.summary.df$turnout.perc.allage.est))

# Visualise county turnout data as percentage
county.perc.turnout.winner = ggplot(county.summary.df,aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est*100,
                                                fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(
    name = "Estimated turnout in Trump and Clinton won counties.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.2016.turnout.perc.diff,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.winner


## VISUALISING THE 2016 RESULTS
county.summary.df = county.summary.df[order(county.summary.df$perc.diff),]
county.summary.df$ordered.county.2016.perc.diff = c(1:length(county.summary.df$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(county.summary.df,aes(x = ordered.county.2016.perc.diff, y = perc.diff,
                                                fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(
          name = "Percentage difference in vote between Trump and Clinton by county.") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.2016.perc.diff,
    labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.diff

county.summary.df = county.summary.df[order(county.summary.df$turnout),]
county.summary.df$ordered.county.2016.turnout = c(1:length(county.summary.df$turnout))

# Votes as numbers
county.2016.trump.num = ggplot(county.summary.df,aes(x = ordered.county.2016.turnout, y = trump.num,
                                                     fill = winner)) +
  geom_bar(stat = "identity", aes(y = county.summary.df$turnout), fill = "grey", alpha = 0.5) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.2016.turnout,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2016.trump.num

votes.2016.compare.plotnames = c("county.perc.diff","county.2016.trump.num")
votes.2016.compare = marrangeGrob(grobs = mget(votes.2016.compare.plotnames), nrow=1, ncol=2,top=NULL)
votes.2016.compare

# Comparing 2012 and 2016 results by county --------------------------------
# Make sure to reorder before adding in new data!
county.summary.df = county.summary.df[order(county.summary.df$county),]

# This data comes from here: http://elections.wi.gov/elections-voting/results/2012/fall-general
# 2012 county data
results.2012 = read.xlsx(file = "County by County_11.6.12MOD.xls",
               sheetIndex = 2, header = TRUE, colClasses = NA, stringsAsFactors = F)
results.2012 = results.2012[-c(73:74),]

county.summary.df$turnout.2012 = results.2012$Total.Votes.Cast
county.summary.df$turnout.2012.perc = voting.age.people$Voter.Turnout
county.summary.df$dem.2012 = results.2012$BARACK.OBAMA
county.summary.df$rep.2012 = results.2012$MITT.ROMNEY

county.summary.df$diff.2012.num = results.2012$MITT.ROMNEY - results.2012$BARACK.OBAMA
county.summary.df$diff.2012.perc = county.summary.df$diff.2012.num / county.summary.df$turnout.2012

# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

county.summary.df = county.summary.df[order(county.summary.df$diff.2012.perc),]
county.summary.df$ordered.county.2012.perc.diff = c(1:length(county.summary.df$diff.2012.perc))

# Visualise county data 2012 vs 2016 - as percentage
county.2012v2016.perc.diff.2016ord = ggplot(county.summary.df,aes(x = ordered.county.2016.perc.diff, y = diff.2012.perc*100)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df$perc.diff),
                                         fill = "red",
                                         alpha = 0.5) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.2012.perc.diff,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.perc.diff.2016ord

# As number instead of percentage
county.differences.years.num = ggplot(county.summary.df,aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df$num.diff),
                                         fill = "red",
                                         alpha = 0.5) +
  scale_y_continuous(
    name = "Number of votes difference in 2012 and 2016 by county. Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.2012.perc.diff,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.differences.years.num

absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
absolute.2016.2012.compare

#### SWING IN VOTES 2012 to 2016
county.summary.df$swing.num = with(county.summary.df, num.diff - diff.2012.num)
county.summary.df$swing.perc = with(county.summary.df, perc.diff - (diff.2012.perc*100))
county.summary.df$swing.turnout = with(county.summary.df, turnout - turnout.2012)
county.summary.df$swing.turnout.perc = with(county.summary.df, turnout.perc.allage.est - (turnout.2012.perc))

# Visualise
county.summary.df = county.summary.df[order(county.summary.df$swing.perc),]
county.summary.df$ordered.county.swing.perc = c(1:length(county.summary.df$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.df,aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = (county.summary.df$swing.turnout.perc*100)),
                                         fill = "grey", alpha = 0.8) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.swing.perc,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.swing.perc

# As number
county.2012v2016.swing.num = ggplot(county.summary.df,aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df$swing.turnout),
                                         fill = "grey", alpha = 0.8) +
scale_y_continuous(
  name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\n
  below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.county.swing.perc,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.swing.num

swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc","county.differences.years.num")
swing.votes.2016.2012.compare = marrangeGrob(grobs = mget(swing.votes.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
swing.votes.2016.2012.compare


# turnout 2016 vs 2012
county.summary.df = county.summary.df[order(county.summary.df$swing.turnout.perc),]
county.summary.df$ordered.turnout.diff = c(1:length(county.summary.df$swing.turnout.perc))

turnout.diff.perc.graph = ggplot(county.summary.df,aes(x = ordered.turnout.diff, y = swing.turnout.perc,
                            fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 as percentage.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.turnout.diff,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
turnout.diff.perc.graph

# THIS IS A GOOD GRAPH
turnout.diff.num.graph = ggplot(county.summary.df,aes(x = ordered.turnout.diff, y = swing.turnout, fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 in voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df$ordered.turnout.diff,
                   labels = as.character(county.summary.df$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
turnout.diff.num.graph

swing.turnout.2016.2012.compare.plotnames = c("turnout.diff.perc.graph","turnout.diff.num.graph")
swing.turnout.2016.2012.compare = marrangeGrob(grobs = mget(swing.turnout.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
swing.turnout.2016.2012.compare

# Voting machines in counties ---------------------------------------------
# This file from here: http://elections.wi.gov/elections-voting/voting-equipment/voting-equipment-use
# Voting machine equipment for 13 September 2016
vot.equip = read.xlsx("voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
                      sheetIndex = 1,header=TRUE, colClasses=NA, stringsAsFactors = F)
colnames(vot.equip) = c("county.long","municipality","machine.vendor.dealer.model","accessible.vendor.dealer.model")

# Follwing from
# http://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
county.split.att2 = gsub( " COUNTY.*$", "", vot.equip$county.long )
vot.equip$county = county.split.att2
vot.equip = vot.equip[!is.na(vot.equip$county.long),]

vot.equip$use.machines = NA
vot.equip$use.machines = ifelse(vot.equip$machine.vendor.dealer.model == "None ",0,1)

vot.equip$machine.vendor.dealer.spec = gsub( "Optech-", "Optech ", vot.equip$machine.vendor.dealer.model)
vot.equip$machine.vendor.dealer.spec = gsub( "Optech/", "Optech ", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub( " .*$", "", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub( "None", NA, vot.equip$machine.vendor.dealer.spec)

vot.equip.county.grouped = group_by(vot.equip,county)
vot.equip.county = dplyr::summarise(vot.equip.county.grouped,
                             use.machines.prop = sum(use.machines,na.rm = TRUE) /
                               length(use.machines)
)

vot.equip.withmachines = subset(vot.equip,use.machines > 0)
 
vot.equip.county.grouped.machines = group_by(vot.equip.withmachines,county)
vot.equip.county.machines = dplyr::summarise(vot.equip.county.grouped.machines,
                             use.machines.prop = sum(use.machines,na.rm = TRUE) /
                               length(use.machines)
)

vot.equip.county.machines$use.machines.prop = NULL

max.vendor = as.character(1:length(vot.equip.county.machines$county))
for (i in 1:length(unique(vot.equip.county.machines$county))){
a = count(vot.equip.withmachines$machine.vendor.dealer.spec[vot.equip.withmachines$county ==
                                                          vot.equip.withmachines$county[i]])
a$x = as.character(a$x)
max.vendor[i] = a$x[a$freq == max(a$freq)]
}

vot.equip.county.machines$machine.most.used = max.vendor
vot.equip.county = join(vot.equip.county, vot.equip.county.machines, by= "county",match = "first")

# NOW, GRAPHING SWING BY PROPORTION OF ELECTRONIC VOTING MACHINES IN COUNTY
# Make sure to reorder before adding in new data!
county.summary.df = county.summary.df[order(county.summary.df$county),]
county.summary.df$use.machines.prop = vot.equip.county$use.machines.prop
county.summary.df$machine.most.used = vot.equip.county$machine.most.used
  
# Now, let's order the counties by proportion of municipalities using voting machines
county.summary.df = county.summary.df[order(county.summary.df$use.machines.prop),]

# Swing percentage on x axis vs the % of municipalities that use voting machines in the county
county.swing.perc.vs.machines.point = ggplot(county.summary.df,aes(x = swing.perc, y = use.machines.prop*100,
                                                                   colour = machine.most.used)) +
  geom_point() +
  scale_y_continuous(
    name = "Use of voting machines in each county as a percentage of total counties that use them") +
  scale_x_continuous(name = "Swing percentage from 2012 to 2016 election - negative is towards\n
                     Democrats, positive is towards republicans") +
  scale_colour_discrete(name = "Primary machine vendor")
county.swing.perc.vs.machines.point
correlation(county.summary.df$swing.perc,county.summary.df$use.machines.prop)
# cor -0.712513 : high correlation

# absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
# absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
# absolute.2016.2012.compare

# Repeating graphs for counties with 100% machines -----------------------------

county.summary.df.machines = subset(county.summary.df, use.machines.prop == 1)

## VISUALISING THE 2016 RESULTS
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$trump),]
county.summary.df.machines$ordered.county.2016.trump = c(1:length(county.summary.df.machines$trump))

# Visualise absolute percentage results
county.perc.winner = ggplot(county.summary.df.machines,
                            aes(x = county.summary.df.machines$ordered.county.2016.trump, y = trump,
                                fill = winner)) +
  geom_bar(aes(y = county.summary.df.machines$turnout.perc.allage.est*100), stat = "identity", 
           fill = "grey") +
  geom_bar(stat = "identity",alpha = 0.6) +
  
  scale_y_continuous(
    name = "Percentage vote for Trump in Wisconsin counties.\n
    Grey bar behind indicates percentage turnout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.2016.trump,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner


county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$turnout.perc.allage.est),]
county.summary.df.machines$ordered.county.2016.turnout.perc.diff = c(1:length(county.summary.df.machines$turnout.perc.allage.est))

# Visualise county turnout data as percentage 2016
county.perc.turnout.winner = ggplot(county.summary.df.machines,aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est*100,
                                                          fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(
    name = "Estimated turnout in Trump and Clinton won counties.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.2016.turnout.perc.diff,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.winner


## VISUALISING THE 2016 RESULTS
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$perc.diff),]
county.summary.df.machines$ordered.county.2016.perc.diff = c(1:length(county.summary.df.machines$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(county.summary.df.machines,aes(x = ordered.county.2016.perc.diff, y = perc.diff,
                                                fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(
    name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.2016.perc.diff,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.diff

county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$turnout),]
county.summary.df.machines$ordered.county.2016.turnout = c(1:length(county.summary.df.machines$turnout))

# Votes as numbers
county.2016.trump.num = ggplot(county.summary.df.machines,aes(x = ordered.county.2016.turnout, y = trump.num,
                                                     fill = winner)) +
  geom_bar(stat = "identity", aes(y = county.summary.df.machines$turnout), fill = "grey", alpha = 0.5) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.2016.turnout,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2016.trump.num

votes.2016.compare.plotnames = c("county.perc.diff","county.2016.trump.num")
votes.2016.compare = marrangeGrob(grobs = mget(votes.2016.compare.plotnames), nrow=1, ncol=2,top=NULL)
votes.2016.compare



# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$diff.2012.perc),]
county.summary.df.machines$ordered.county.2012.perc.diff = c(1:length(county.summary.df.machines$diff.2012.perc))

# Visualise county data 2012 vs 2016 - as percentage
county.2012v2016.perc.diff.2016ord = ggplot(county.summary.df.machines,aes(x = ordered.county.2016.perc.diff, y = diff.2012.perc*100)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df.machines$perc.diff),
                                         fill = "red",
                                         alpha = 0.5) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.2012.perc.diff,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.perc.diff.2016ord

# As number instead of percentage
county.differences.years.num = ggplot(county.summary.df.machines,aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df.machines$num.diff),
                                         fill = "red",
                                         alpha = 0.5) +
  scale_y_continuous(
    name = "Number of votes difference in 2012 and 2016 by county. Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.2012.perc.diff,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.differences.years.num

absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
absolute.2016.2012.compare

#### SWING IN VOTES 2012 to 2016
# Visualise
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$swing.perc),]
county.summary.df.machines$ordered.county.swing.perc = c(1:length(county.summary.df.machines$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.df.machines,aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = (county.summary.df.machines$swing.turnout.perc*100)),
                                         fill = "grey", alpha = 0.8) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.swing.perc,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.swing.perc

# As number
county.2012v2016.swing.num = ggplot(county.summary.df.machines,aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df.machines$swing.turnout),
                                         fill = "grey", alpha = 0.8) +
  scale_y_continuous(
    name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.county.swing.perc,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.swing.num

swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc","county.differences.years.num")
swing.votes.2016.2012.compare = marrangeGrob(grobs = mget(swing.votes.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
swing.votes.2016.2012.compare


# turnout 2016 vs 2012
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$swing.turnout.perc),]
county.summary.df.machines$ordered.turnout.diff = c(1:length(county.summary.df.machines$swing.turnout.perc))

turnout.diff.perc.graph = ggplot(county.summary.df.machines,aes(x = ordered.turnout.diff, y = swing.turnout.perc,
                                                       fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 as percentage.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.turnout.diff,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
turnout.diff.perc.graph

turnout.diff.num.graph = ggplot(county.summary.df.machines,aes(x = ordered.turnout.diff, y = swing.turnout, fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 in voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.machines$ordered.turnout.diff,
                   labels = as.character(county.summary.df.machines$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
turnout.diff.num.graph

swing.turnout.2016.2012.compare.plotnames = c("turnout.diff.perc.graph","turnout.diff.num.graph")
swing.turnout.2016.2012.compare = marrangeGrob(grobs = mget(swing.turnout.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
swing.turnout.2016.2012.compare

# Repeating graphs for counties swing > 20% to Trump -----------------------------

county.summary.df.20.swing = subset(county.summary.df, swing.perc >= 20)

## VISUALISING THE 2016 RESULTS
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$trump),]
county.summary.df.20.swing$ordered.county.2016.trump = c(1:length(county.summary.df.20.swing$trump))

# Visualise absolute percentage results
county.perc.winner = ggplot(county.summary.df.20.swing,
                            aes(x = county.summary.df.20.swing$ordered.county.2016.trump, y = trump,
                                fill = winner)) +
  geom_bar(aes(y = county.summary.df.20.swing$turnout.perc.allage.est*100), stat = "identity", 
           fill = "grey") +
  geom_bar(stat = "identity",alpha = 0.6) +
  
  scale_y_continuous(
    name = "Percentage vote for Trump in Wisconsin counties.\n
    Grey bar behind indicates percentage turnout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.2016.trump,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner


county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$turnout.perc.allage.est),]
county.summary.df.20.swing$ordered.county.2016.turnout.perc.diff = c(1:length(county.summary.df.20.swing$turnout.perc.allage.est))

# Visualise county turnout data as percentage 2016
county.perc.turnout.winner = ggplot(county.summary.df.20.swing,aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est*100,
                                                                   fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(
    name = "Estimated turnout in Trump and Clinton won counties.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.2016.turnout.perc.diff,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.winner


## VISUALISING THE 2016 RESULTS
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$perc.diff),]
county.summary.df.20.swing$ordered.county.2016.perc.diff = c(1:length(county.summary.df.20.swing$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(county.summary.df.20.swing,aes(x = ordered.county.2016.perc.diff, y = perc.diff,
                                                         fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(
    name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.2016.perc.diff,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.diff

county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$turnout),]
county.summary.df.20.swing$ordered.county.2016.turnout = c(1:length(county.summary.df.20.swing$turnout))

# Votes as numbers
county.2016.trump.num = ggplot(county.summary.df.20.swing,aes(x = ordered.county.2016.turnout, y = trump.num,
                                                              fill = winner)) +
  geom_bar(stat = "identity", aes(y = county.summary.df.20.swing$turnout), fill = "grey", alpha = 0.5) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.2016.turnout,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2016.trump.num

votes.2016.compare.plotnames = c("county.perc.diff","county.2016.trump.num")
votes.2016.compare = marrangeGrob(grobs = mget(votes.2016.compare.plotnames), nrow=1, ncol=2,top=NULL)
votes.2016.compare



# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$diff.2012.perc),]
county.summary.df.20.swing$ordered.county.2012.perc.diff = c(1:length(county.summary.df.20.swing$diff.2012.perc))

# Visualise county data 2012 vs 2016 - as percentage
county.2012v2016.perc.diff.2016ord = ggplot(county.summary.df.20.swing,aes(x = ordered.county.2016.perc.diff, y = diff.2012.perc*100)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df.20.swing$perc.diff),
                                         fill = "red",
                                         alpha = 0.5) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.2012.perc.diff,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.perc.diff.2016ord

# As number instead of percentage
county.differences.years.num = ggplot(county.summary.df.20.swing,aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df.20.swing$num.diff),
                                         fill = "red",
                                         alpha = 0.5) +
  scale_y_continuous(
    name = "Number of votes difference in 2012 and 2016 by county. Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.2012.perc.diff,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.differences.years.num

absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
absolute.2016.2012.compare

#### SWING IN VOTES 2012 to 2016
# Visualise
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$swing.perc),]
county.summary.df.20.swing$ordered.county.swing.perc = c(1:length(county.summary.df.20.swing$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.df.20.swing,aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = (county.summary.df.20.swing$swing.turnout.perc*100)),
                                         fill = "grey", alpha = 0.8) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.swing.perc,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.swing.perc

# As number
county.2012v2016.swing.num = ggplot(county.summary.df.20.swing,aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity", aes(y = county.summary.df.20.swing$swing.turnout),
                                         fill = "grey", alpha = 0.8) +
  scale_y_continuous(
    name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.county.swing.perc,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.2012v2016.swing.num

swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc","county.2012v2016.swing.num")
swing.votes.2016.2012.compare = marrangeGrob(grobs = mget(swing.votes.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
swing.votes.2016.2012.compare

# turnout 2016 vs 2012
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$swing.turnout.perc),]
county.summary.df.20.swing$ordered.turnout.diff = c(1:length(county.summary.df.20.swing$swing.turnout.perc))

turnout.diff.perc.graph = ggplot(county.summary.df.20.swing,aes(x = ordered.turnout.diff, y = swing.turnout.perc,
                                                                fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 as percentage.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.turnout.diff,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
turnout.diff.perc.graph

turnout.diff.num.graph = ggplot(county.summary.df.20.swing,aes(x = ordered.turnout.diff, y = swing.turnout, fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 in voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limit =  county.summary.df.20.swing$ordered.turnout.diff,
                   labels = as.character(county.summary.df.20.swing$county), name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral"))
turnout.diff.num.graph

swing.turnout.2016.2012.compare.plotnames = c("turnout.diff.perc.graph","turnout.diff.num.graph")
swing.turnout.2016.2012.compare = marrangeGrob(grobs = mget(swing.turnout.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
swing.turnout.2016.2012.compare


# Comparing with earlier elections ----------------------------------------
setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin\\Historical elections")
# http://elections.wi.gov/elections-voting/results/2000/fall-general
# http://elections.wi.gov/elections-voting/results/2004/fall-general
# http://elections.wi.gov/elections-voting/results/2008/fall-general

county.summary.df = county.summary.df[order(county.summary.df$county),]

county.summary.2012.df = county.summary.df
county.summary.2012.df$dem.2012.perc = county.summary.2012.df$dem.2012 / results.2012$Total.Votes.Cast
county.summary.2012.df$rep.2012.perc = county.summary.2012.df$rep.2012 / results.2012$Total.Votes.Cast
county.summary.2012.df$winner.party = ifelse(county.summary.2012.df$rep.2012.perc > 
                                         county.summary.2012.df$dem.2012.perc,"Rep","Dem")
county.summary.2012.df$winner = ifelse(county.summary.2012.df$rep.2012.perc > 
                                               county.summary.2012.df$dem.2012.perc,"Romney","Obama")
county.summary.2012.df$total.poss.voters = voting.age.people$Voting.Age.Estimate.2012

county.summary.2012.final = with(county.summary.2012.df, 
                data.frame(county,dem.2012,dem.2012.perc,rep.2012, rep.2012.perc,
                           num.diff, perc.diff, winner.party,winner, turnout.2012, turnout.2012.perc,
                           total.poss.voters))
colnames(county.summary.2012.final) = c("county","dem.vote","dem.vote.perc","rep.vote","rep.vote.perc",
                                        "num.diff","perc.diff",
                                        "winner","winning.party","turnout","turnout.perc",
                                        "total.voting.age")
county.summary.2012.final$turnout = as.integer(as.character(county.summary.2012.final$turnout))

county.summary.2016.final = with(county.summary.df, 
                                 data.frame(county,clinton.num,clinton,trump.num, trump,
                                            num.diff,perc.diff,
                                            winner.party,winner, turnout, turnout.perc.allage.est,
                                            total.voters.age.est,
                                            swing.num,swing.perc,swing.turnout,swing.turnout.perc))
colnames(county.summary.2016.final) = c("county","dem.vote","dem.vote.perc","rep.vote.perc",
                                        "num.diff","perc.diff",
                                        "winner","winning.party","turnout","turnout.perc",
                                        "total.voting.age",
                                        "swing.num","swing.perc","swing.turnout","swing.turnout.perc")

setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin\\Historical elections")

election.2008 = read.csv("2008_FallElection_President_WardbyWardMOD.csv")

election.2008.final.group = group_by(election.2008,COUNTY)
election.2008.final = dplyr::summarise(election.2008.final.group,
      dem.vote = sum(Democratic), rep.vote = sum(Republican),
      turnout = sum(Total.votes)
)
election.2008.final = election.2008.final[-c(1),]

election.2008.final$dem.vote.perc = election.2008.final$dem.vote / election.2008.final$turnout
election.2008.final$rep.vote.perc = election.2008.final$rep.vote / election.2008.final$turnout

election.2008.final$num.diff = election.2008.final$rep.vote - election.2008.final$dem.vote
election.2008.final$perc.diff = election.2008.final$rep.vote.perc - election.2008.final$dem.vote.perc

election.2008.final$winner = ifelse(election.2008.final$rep.vote > 
                                            election.2008.final$dem.vote,"McCain","Obama")
election.2008.final$winning.party = ifelse(election.2008.final$rep.vote > 
                                            election.2008.final$dem.vote,"Rep","Dem")
election.2008.final$turnout.perc = NA
election.2008.final$total.voting.age = NA

election.2008.final$turnout = as.integer(as.character(election.2008.final$turnout))
#### SWING IN VOTES 2012 to 2008
county.summary.2012.final$swing.num = county.summary.2012.final$num.diff - election.2008.final$num.diff
county.summary.2012.final$swing.perc =   county.summary.2012.final$perc.diff - election.2008.final$perc.diff
county.summary.2012.final$swing.turnout = county.summary.2012.final$turnout - election.2008.final$turnout
county.summary.2012.final$swing.turnout.perc = NA

election.2008.final$swing.num = NA
election.2008.final$swing.perc =   NA
election.2008.final$swing.turnout = NA
election.2008.final$swing.turnout.perc = NA


# overall.results$voting.age.pop = 4449170
# overall.results$turnout.overall = sum(overall.results$votes.rec / overall.results$voting.age.pop)
# 
# voting.age.people = read.xlsx("2012_presidential_general_election_turnout_xlsx_11916.xlsx",
#                               sheetIndex = 2, stringsAsFactors = F, header = T)
# voting.age.people$county = gsub( " COUNTY.*$", "", voting.age.people$County)
# 
# total.2012.pop.wi = sum(voting.age.people$Voting.Age.Estimate.2012)
# 
# # Estimated 1.01% increase in voting age population from 2012 to 2016. I will multiply the
# # 2012 county voter data by 101.01% to try to account for this increase. Yes, this is an assumption,
# # but newer data is not yet available!
# prop.2016.voting.age.to.2012.est = mean(overall.results$voting.age.pop) / total.2012.pop.wi
# 
# voting.age.people$Voting.Age.Estimate.2016 = voting.age.people$Voting.Age.Estimate.2012 * prop.2016.voting.age.to.2012.est
# voting.age.people$Voting.Age.Estimate.2016 = ceiling(voting.age.people$Voting.Age.Estimate.2016)
# 
# county.summary.df$total.voters.age.est = voting.age.people$Voting.Age.Estimate.2016
# county.summary.df$turnout.perc.allage.est = county.summary.df$turnout / county.summary.df$total.voters.age.est




# Including income --------------------------------------------------------

#C:\Users\s_cas\Documents\GitHub\Voting Data
