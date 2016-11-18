setwd("https://github.com/Sean-Case/WisconsinElection2016")
library(XML); require(rvest); require(stringr);require(xlsx);require(dplyr);require(plyr)
require(agricolae); require(ggplot2); library(gridExtra); library(tidyr); require(corrplot)

# Be careful that rstudio git integration does not work on files with spaces in them...
# http://stackoverflow.com/questions/34105129/using-git-in-r-studio-cannot-stage-modified-code-files

# Information on how to use Plotly can be found here: https://www.r-bloggers.com/plot-with-ggplot2-and-plotly-within-knitr-reports/

options(scipen=999) # Effectively disables scientific numbering
options(mc.cores = parallel::detectCores()) # This is used for the poll downloads

one.graph.width = 20;one.graph.height = 15

# Function to take rightmost digit
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


## Capitalising first letters of sentence
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


# Loading 2016 results ----------------------------------------------------
# Loading in 2016 results
# Following guide here:
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

# 2016 results from here: http://wisconsinvote.org/results/President%20-%20General/county-results

url <-
  "http://wisconsinvote.org/results/President%20-%20General/county-results"
voting <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@class="views-table cols-3"]') %>%
  html_table()

lapply(voting, '[[', 'Candidates')

voting.county <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@class="views-table cols-3"]/caption') %>%
  html_text()


voting.county = gsub("County: ", "", voting.county)
as.character(voting.county)

votinglist.test <-
  mapply(cbind,
         voting,
         "voting.county" = as.character(voting.county),
         SIMPLIFY = F)
voting = votinglist.test

voting.df.2016 = data.frame(1:504)
voting.df.2016$county = unlist(lapply(voting, '[[', 'voting.county'))
voting.df.2016$cand = unlist(lapply(voting, '[[', 'Candidates'))
voting.df.2016$votes.rec = unlist(lapply(voting, '[[', 'Votes Received'))
voting.df.2016$votes.perc = unlist(lapply(voting, '[[', 'Vote Percentage (In County)'))
voting.df.2016$votes.perc = as.numeric(gsub("%", "", voting.df.2016$votes.perc))
voting.df.2016$X1.504 = NULL
voting.df.2016$votes.rec = as.integer(gsub(",", "", voting.df.2016$votes.rec))

voting.df.2016$cand.group = NA
voting.df.2016$cand.group[grep("Hillary",voting.df.2016$cand)] = "Clinton"
voting.df.2016$cand.group[grep("Donald",voting.df.2016$cand)] = "Trump"
voting.df.2016$cand.group[grep("Darrell",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Rocky",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Gary",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Monica",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Jill",voting.df.2016$cand)] = "Other"

# Grouping by other
# Looking at county data
voting.df.2016.votecand.groups = group_by(voting.df.2016,county,cand.group)
voting.df.2016.votecand = dplyr::summarise(voting.df.2016.votecand.groups,
              votes.rec = sum(votes.rec),
              votes.perc = sum(votes.perc)
)

voting.df.2016.votecand$cand.group2 = voting.df.2016.votecand$cand.group

str(voting.df.2016$votecand)
voting.df.2016.test = spread(voting.df.2016.votecand,
                             key = cand.group, value = votes.rec)
colnames(voting.df.2016.test)[4:6] = c("dem.votes","oth.votes","rep.votes")
voting.df.2016.test = spread(voting.df.2016.test,
       key = cand.group2, value = votes.perc)
colnames(voting.df.2016.test)[5:7] = c("dem.votes.perc","oth.votes.perc","rep.votes.perc")

voting.df.2016.votecand.groups = group_by(voting.df.2016.test,county)
voting.df.2016.votecand = dplyr::summarise(voting.df.2016.votecand.groups,
                                           dem.votes = sum(na.rm=T,dem.votes),
                                           dem.votes.perc = sum(na.rm=T,dem.votes.perc),
                                           rep.votes = sum(na.rm=T,rep.votes),
                                           rep.votes.perc = sum(na.rm=T,rep.votes.perc),
                                           oth.votes = sum(na.rm=T,oth.votes),
                                           oth.votes.perc = sum(na.rm=T,oth.votes.perc)
)

voting.df.2016.votecand$turnout = with(voting.df.2016.votecand, dem.votes + rep.votes + oth.votes)

### 2016 results ###
# Overall results vs polls
election.2016.group = group_by(voting.df.2016, cand)
election.2016.grouped = dplyr::summarise(election.2016.group,
                                             votes.rec = sum(votes.rec))

election.2016.grouped$votes.perc = (election.2016.grouped$votes.rec / sum(election.2016.grouped$votes.rec)) * 100

election.2016.grouped$ordered.cands= c(5,1,3,2,4,6,7)
colours = c("grey", "lightcoral", "dark grey", "light blue", "light green","grey","grey")
election.2016.grouped = election.2016.grouped[order(election.2016.grouped$ordered.cands), ]
election.2016.grouped$cand.group = c("Donald J. Trump", "Hillary Clinton",rep("Other",5))

election.2016.grouped$votes.perc.group = with(election.2016.grouped, c(votes.perc[1],votes.perc[2], rep(sum(votes.perc[3:7]), 5)))
election.2016.grouped$votes.rec.group = with(election.2016.grouped, c(votes.rec[1],votes.rec[2], rep(sum(votes.rec[3:7]), 5)))

rm(voting.df.2016.test,voting.df.2016.votecand.groups)

# Reading in historical vote data ----------------------------------------------------
## General overview of Wisconsin elections from 1900 onwards
url <- "http://uselectionatlas.org/RESULTS/compare.php?year=2016&fips=55&f=0&off=0&elect=0&type=state"
voting.history <- url %>%
  read_html() %>%
  html_nodes(xpath='//table[@id="data"]') %>%
  html_table(fill=T)

elections.100.years = data.frame(1:44)
elections.100.years$year = unlist(lapply(voting.history, '[[', 'X3'))
elections.100.years$year = as.integer(elections.100.years$year)
elections.100.years$turnout = unlist(lapply(voting.history, '[[', 'X4'))
elections.100.years$turnout = as.integer(gsub(",", "", elections.100.years$turnout))
elections.100.years$dem.vote.perc = unlist(lapply(voting.history, '[[', 'X10'))
elections.100.years$dem.vote.perc = as.numeric(gsub("%", "", elections.100.years$dem.vote.perc))
elections.100.years$rep.vote.perc = unlist(lapply(voting.history, '[[', 'X11'))
elections.100.years$rep.vote.perc = as.numeric(gsub("%", "", elections.100.years$rep.vote.perc))

# Independents and others
elections.100.years$ind.vote.perc = unlist(lapply(voting.history, '[[', 'X12'))
elections.100.years$ind.vote.perc = as.numeric(gsub("%", "", elections.100.years$ind.vote.perc))
elections.100.years$oth.vote.perc = unlist(lapply(voting.history, '[[', 'X13'))
elections.100.years$oth.vote.perc = as.numeric(gsub("%", "", elections.100.years$oth.vote.perc))

elections.100.years$dem.vote = unlist(lapply(voting.history, '[[', 'X14'))
elections.100.years$dem.vote = as.integer(gsub(",", "", elections.100.years$dem.vote))
elections.100.years$rep.vote = unlist(lapply(voting.history, '[[', 'X15'))
elections.100.years$rep.vote = as.integer(gsub(",", "", elections.100.years$rep.vote))

elections.100.years$ind.vote = unlist(lapply(voting.history, '[[', 'X16'))
elections.100.years$ind.vote = as.integer(gsub(",", "", elections.100.years$ind.vote))
elections.100.years$oth.vote = unlist(lapply(voting.history, '[[', 'X17'))
elections.100.years$oth.vote = as.integer(gsub(",", "", elections.100.years$oth.vote))

elections.100.years$X1.44 = NULL
elections.100.years = elections.100.years[-c(1:3), ]

### Writing the 2016 results from my other input file, because the numbers there aren't quite the same
elections.100.years$rep.vote.perc[1] = election.2016.grouped$votes.perc[1]
elections.100.years$dem.vote.perc[1] = election.2016.grouped$votes.perc[2]
elections.100.years$oth.vote.perc[1] = sum(election.2016.grouped$votes.perc[3:6])
elections.100.years$ind.vote.perc[1] = election.2016.grouped$votes.perc[7]

elections.100.years$rep.vote[1] = election.2016.grouped$votes.rec[1]
elections.100.years$dem.vote[1] = election.2016.grouped$votes.rec[2]
elections.100.years$oth.vote[1] = sum(election.2016.grouped$votes.rec[3:6])
elections.100.years$ind.vote[1] = election.2016.grouped$votes.rec[7]

# Total others = independents + others
elections.100.years$tot.oth.vote.perc = elections.100.years$ind.vote.perc + elections.100.years$oth.vote.perc
elections.100.years$tot.oth.vote = elections.100.years$ind.vote + elections.100.years$oth.vote


elections.100.years$vote.perc.diff = elections.100.years$rep.vote.perc - elections.100.years$dem.vote.perc
elections.100.years$vote.num.diff = elections.100.years$rep.vote - elections.100.years$dem.vote
elections.100.years$winner = with(elections.100.years, ifelse(vote.perc.diff < 0, "Dem", "Rep"))

## File reference not working?
# From here: https://www.google.es/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwjChuux5qfQAhWCrxoKHXcKDqYQFggdMAA&url=http%3A%2F%2Felections.wi.gov%2Fsites%2Fdefault%2Ffiles%2Fpage%2Fvoter_turnout_partisan_nonpartisan_xlsx_13632.xlsx&usg=AFQjCNHNxn4e1xNCCjaLXekHxuAKT_dFxg
historical.turnout = read.xlsx(
  file = "Wisconsin election stats\\voter_turnout_partisan_nonpartisan_xlsx_13632.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(historical.turnout) = tolower(colnames(historical.turnout))
historical.turnout.join = with(historical.turnout, data.frame(year, voting.age.population))
elections.100.years = join(elections.100.years, historical.turnout.join, by = "year")
elections.100.years$turnout.perc = with(elections.100.years, (turnout / voting.age.population)) * 100

elections.100.years$swing.turnout.perc = NA
for (i in (2:(length(elections.100.years$year) - 1))) {
  elections.100.years$swing.turnout.perc[i - 1] =  -(elections.100.years$turnout.perc[i] - elections.100.years$turnout.perc[i -1])
  elections.100.years$swing.vote.num[i - 1] =  -(elections.100.years$vote.num.diff[i] - elections.100.years$vote.num.diff[i - 1])
  elections.100.years$swing.vote.perc[i - 1] =  -(elections.100.years$vote.perc.diff[i] - elections.100.years$vote.perc.diff[i - 1])
  elections.100.years$swing.vote.dem.change.perc[i - 1] =  (elections.100.years$dem.vote.perc[i-1] - elections.100.years$dem.vote.perc[i])
  elections.100.years$swing.vote.dem.change.num[i - 1] =  (elections.100.years$dem.vote.num[i-1] - elections.100.years$dem.vote.num[i])
  elections.100.years$swing.vote.rep.change.perc[i - 1] =  (elections.100.years$rep.vote.perc[i-1] - elections.100.years$rep.vote.perc[i])
  elections.100.years$swing.vote.rep.change.num[i - 1] =  (elections.100.years$rep.vote.num[i-1] - elections.100.years$rep.vote.num[i])
  elections.100.years$swing.vote.tot.oth.change.perc[i - 1] =  (elections.100.years$tot.oth.vote.perc[i-1] - elections.100.years$tot.oth.vote.perc[i])
  elections.100.years$swing.vote.tot.oth.change.num[i - 1] =  (elections.100.years$tot.oth.vote.num[i-1] - elections.100.years$tot.oth.vote.num[i])
}
elections.100.years.postwar = subset(elections.100.years, year >= 1948)




# The following variables will be used when I scrape the polls from HuffPost API later
elections.100.years$mean.rep.poll.oneweek = NA
elections.100.years$mean.dem.poll.oneweek = NA
elections.100.years$mean.rep.poll.oneweek.divergence = NA
elections.100.years$mean.dem.poll.oneweek.divergence = NA

# Downloading 2016 and 2012 polls ------------------------------------------------------
# Loading poll data for Wisconsin

# Gathering poll data scraping instructions from here: https://pkremp.github.io/report.html
# rm(list = ls())
library(rstan)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(curl)
library(shinystan)
library(rmarkdown)

# Useful functions #
corr_matrix <- function(m) {
  (diag(m) ^ -.5 * diag(nrow = nrow(m))) %*% m %*% (diag(m) ^ -.5 * diag(nrow = nrow(m)))
}

cov_matrix <- function(n, sigma2, rho) {
  m <- matrix(nrow = n, ncol = n)
  m[upper.tri(m)] <- rho
  m[lower.tri(m)] <- rho
  diag(m) <- 1
  (sigma2 ^ .5 * diag(n))  %*% m %*% (sigma2 ^ .5 * diag(n))
}

logit <- function(x)
  log(x / (1 - x))
inv_logit <- function(x)
  1 / (1 + exp(-x))

download_csv <- function(stub) {
  url <-
    paste("http://elections.huffingtonpost.com/pollster/",
          stub,
          ".csv",
          sep = "")
  connection <- curl(url, "r")
  df <- read.csv(connection, stringsAsFactors = FALSE)
  close(connection)
  message("Downloaded ", url)
  return(df)
}

## Downloading poll data from the HuffPost Pollster API ##

# Creating a vector of URL stubs, to fetch csv files for each state from HuffPost
# NOTE: I don't need all the states names as I am only interested in Wisconsin
# state_name <- datasets::state.name
# names(state_name) <- datasets::state.abb

{ # Getting the 2016 poll data
stubs_2016 <- c(
  "2016-general-election-trump-vs-clinton",
  "2016-wisconsin-president-trump-vs-clinton"
)
names(stubs_2016) <- c("--", "WI")
stubs_2016 <- stubs_2016[order(names(stubs_2016))]

start_date_2016 <-
  as.Date("2016-10-01") # Keeping all polls after October 1.

# Download the data and put everything in a single df
all_polls_2016 <- map_df(stubs_2016, download_csv, .id = "state")
colnames(all_polls_2016) <- colnames(all_polls_2016) %>% tolower

# I'm only interested in the overall likely voters, not partisan voters
all_polls_2016 = subset(
  all_polls_2016,
  population == "Likely Voters" | population == "Registered Voters" |
    population == "Adults"
)

all_polls_2016$start.date = as.Date(all_polls_2016$start.date)
all_polls_2016$other.nonas = ifelse(is.na(all_polls_2016$other),0,all_polls_2016$other)

all_polls_2016$decided.total = with(all_polls_2016, trump + clinton + other.nonas)
all_polls_2016$trump.defs = with(all_polls_2016, trump / decided.total) * 100
all_polls_2016$clinton.defs = with(all_polls_2016, clinton / decided.total) * 100
all_polls_2016$other.defs = with(all_polls_2016, other / decided.total) * 100

# Subsetting for polls started one week before election day. Election was on 08-11-16.
# THESE ARE ALL LIKELY VOTERS
selected_polls_2016.sub = subset(all_polls_2016, start.date > as.Date("2016-11-01"))

elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$trump)
elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$clinton)
elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$other,na.rm = T)
elections.100.years$mean.und.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$undecided,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2016]
elections.100.years$mean.dem.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2016]
elections.100.years$mean.other.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2016]
elections.100.years$mean.und.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$tot.und.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.und.poll.oneweek[elections.100.years$year == 2016]

### Removing unsure voters from polls and reanalysing
elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$trump.defs,na.rm = T)
elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$clinton.defs,na.rm = T)
elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$other.defs,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.defs.divergence[elections.100.years$year == 2016] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2016]
elections.100.years$mean.dem.poll.oneweek.defs.divergence[elections.100.years$year == 2016] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2016]
elections.100.years$mean.other.defs.poll.oneweek.defs.divergence[elections.100.years$year == 2016] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2016]

# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2016 = ggplot(selected_polls_2016.sub, aes(x = start.date, y = trump)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2016.sub$clinton), colour = "blue") +
  geom_jitter(aes(y = selected_polls_2016.sub$other), colour = "grey") +
  geom_jitter(aes(y = selected_polls_2016.sub$und), colour = "orange")
polls.wisc.point.2016

# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2016.defs = ggplot(selected_polls_2016.sub, aes(x = start.date, y = trump.defs)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2016.sub$clinton.defs), colour = "blue") +
  geom_jitter(aes(y = selected_polls_2016.sub$other.defs), colour = "grey")
polls.wisc.point.2016.defs
} # Getting the 2016 poll data
{ ## Getting 2012 poll data
stubs_2012 <- c(
  "2012-general-election-romney-vs-obama",
  "2012-wisconsin-president-romney-vs-obama"
)
names(stubs_2012) <- c("--", "WI")
stubs_2012 <- stubs_2012[order(names(stubs_2012))]

start_date_2012 <-
  as.Date("2012-10-01") # Keeping all polls after October 1.

# Download the data and put everything in a single df
all_polls_2012 <- map_df(stubs_2012, download_csv, .id = "state")
colnames(all_polls_2012) <- colnames(all_polls_2012) %>% tolower

# I'm only interested in the overall likely voters, not partisan voters
all_polls_2012 = subset(
  all_polls_2012,
  population == "Likely Voters" | population == "Registered Voters" |
    population == "Adults"
)

all_polls_2012$start.date = as.Date(all_polls_2012$start.date)

all_polls_2012$other.nonas = ifelse(is.na(all_polls_2012$other),0,all_polls_2012$other)

all_polls_2012$decided.total = with(all_polls_2012, romney + obama + other.nonas)
all_polls_2012$romney.defs = with(all_polls_2012, romney / decided.total) * 100
all_polls_2012$obama.defs = with(all_polls_2012, obama / decided.total) * 100
all_polls_2012$other.defs = with(all_polls_2012, other / decided.total) * 100

# Subsetting for polls started one week before election day. Election was on 06-11-12
selected_polls_2012.sub = subset(all_polls_2012, start.date > as.Date("2012-10-30"))

elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$romney)
elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$obama)
elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$other, na.rm = T)
elections.100.years$mean.und.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$undecided,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2012]
elections.100.years$mean.dem.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2012]
elections.100.years$mean.other.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2012]
elections.100.years$mean.other.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2012]

###
elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$romney.defs,na.rm = T)
elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$obama.defs,na.rm = T)
elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$other.defs,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.defs.divergence[elections.100.years$year == 2012] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2012]
elections.100.years$mean.dem.poll.oneweek.defs.divergence[elections.100.years$year == 2012] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2012]
elections.100.years$mean.other.defs.poll.oneweek.defs.divergence[elections.100.years$year == 2012] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2012]


# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2012 = ggplot(selected_polls_2012.sub, aes(x = start.date, y = romney)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2012.sub$obama), colour = "blue") +
geom_jitter(aes(y = selected_polls_2012.sub$other), colour = "grey") +
  geom_jitter(aes(y = selected_polls_2012.sub$und), colour = "orange") +
  scale_y_continuous(limits = c(0,55))
polls.wisc.point.2012
} # Getting the 2012 poll data


# Loading historic county data 2016 to 2000 ----------------------------------------
# http://elections.wi.gov/elections-voting/results/2000/fall-general
# http://elections.wi.gov/elections-voting/results/2004/fall-general
# http://elections.wi.gov/elections-voting/results/2008/fall-general

{ # 2016 data
county.summary.df = voting.df.2016.votecand
# colnames(county.summary.df) = "county"
county.summary.df$year = 2016

county.summary.df$perc.diff = county.summary.df$rep.votes.perc - county.summary.df$dem.votes.perc
county.summary.df$num.diff = county.summary.df$rep.votes - county.summary.df$dem.votes

county.summary.df$winner = ifelse(county.summary.df$perc.diff > 0, "Trump", "Clinton")
county.summary.df$winner.party = ifelse(county.summary.df$perc.diff > 0, "Rep", "Dem")

county.summary.df$mean.poll.trump = elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2016]
county.summary.df$mean.poll.clinton = elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2016]

county.summary.df$turnout.all = sum(county.summary.df$turnout)
# elections.100.years$turnout.all = sum(county.summary.df$turnout)

# Calculating turnout by county
# Calculating turnout of registered voters by November 2nd. BUT, this has been seen in past to
# increase by up to 11% on voting day, see 'general_election_voter_turnout...xlsx'. So maybe
# The turnout value here is an over-estimate

## NOTE: Here http://www.wisconsinvote.org/faq it says you can register on the day to vote,
## so maybe this registered to vote value is not accurate?
registered.voters = read.xlsx(
  "Wisconsin election stats\\registeredvotersbycounty_xlsx_45404MOD.xlsx",
  sheetIndex = 1,
  stringsAsFactors = F,
  header = T
)
registered.voters = registered.voters[-c(73, 74), ]
registered.voters$NA. = NULL
registered.voters$county = gsub(" COUNTY.*$", "", registered.voters$county)
registered.voters$registered.voters = as.integer(registered.voters$registered.voters)

county.summary.df = county.summary.df[order(county.summary.df$county), ]
registered.voters = registered.voters[order(registered.voters$county), ]

county.summary.df$reg.voters.all = sum(registered.voters$registered.voters)
# elections.100.years$reg.voters.all = sum(registered.voters$registered.voters)

county.summary.df$registered.voters = registered.voters$registered.voters
county.summary.df$turnout.perc.reg = (county.summary.df$turnout / county.summary.df$registered.voters) * 100
county.summary.df$turnout.state.reg = (sum(county.summary.df$turnout) / sum(county.summary.df$registered.voters)) * 100
# elections.100.years$turnout.state.reg = mean(county.summary.df$turnout.state.reg)

## Calculating turnout of voting age population
## I can only estimate the turnout per county based on the 2012 voting age populations
# from file "general_election_voter_registration... this has the 2016 value"
# elections.100.years$voting.age.pop = 4449170
# elections.100.years$turnout.overall = sum(elections.100.years$votes.rec / elections.100.years$voting.age.pop)

voting.age.people = read.xlsx(
  "Wisconsin election stats\\2012_presidential_general_election_turnout_xlsx_11916.xlsx",
  sheetIndex = 2,
  stringsAsFactors = F,
  header = T
)
voting.age.people$county = gsub(" COUNTY.*$", "", voting.age.people$County)

total.2012.pop.wi = sum(voting.age.people$Voting.Age.Estimate.2012)

# Estimated 1.01% increase in voting age population from 2012 to 2016. I will multiply the
# 2012 county voter data by 101.01% to try to account for this increase. Yes, this is an assumption,
# but newer data is not yet available!
prop.2016.voting.age.to.2012.est = mean(elections.100.years$voting.age.pop[elections.100.years$year == 2016]) / total.2012.pop.wi

voting.age.people$Voting.Age.Estimate.2016 = voting.age.people$Voting.Age.Estimate.2012 * prop.2016.voting.age.to.2012.est
voting.age.people$Voting.Age.Estimate.2016 = ceiling(voting.age.people$Voting.Age.Estimate.2016)

county.summary.df$total.voters.age.est = voting.age.people$Voting.Age.Estimate.2016
county.summary.df$turnout.perc.allage.est = (county.summary.df$turnout / county.summary.df$total.voters.age.est) *100

# Making final data frame to fit in with the other election days
county.summary.2016.final = with(
  county.summary.df,
  data.frame(
    county,
    year,
    dem.votes,
    dem.votes.perc,
    rep.votes,
    rep.votes.perc,
    oth.votes,
    oth.votes.perc,
    num.diff,
    perc.diff,
    winner.party,
    winner,
    turnout,
    turnout.perc.allage.est,
    total.voters.age.est
  )
)
colnames(county.summary.2016.final) = c(
  "county",
  "year",
  "dem.vote",
  "dem.vote.perc",
  "rep.vote",
  "rep.vote.perc",
  "oth.vote",
  "oth.vote.perc",
  "num.diff",
  "perc.diff",
  "winning.party",
  "winner",
  "turnout",
  "turnout.perc",
  "total.voting.age"
)
county.summary.2016.final$county = as.character(county.summary.2016.final$county)

} # 2016 data
{ # 2012 data
  county.summary.df = county.summary.df[order(county.summary.df$county), ]

    # This data comes from here: http://elections.wi.gov/elections-voting/results/2012/fall-general
  # 2012 county data
  results.2012 = read.xlsx(
    file = "Historical elections\\County by County_11.6.12MOD.xls",
    sheetIndex = 2,
    header = TRUE,
    colClasses = NA,
    stringsAsFactors = F
  )
  results.2012 = results.2012[-c(73:74), ]

  county.summary.df$turnout.2012 = results.2012$Total.Votes.Cast
  county.summary.df$turnout.2012.perc = results.2012$Total.Votes.Cast / voting.age.people$Voting.Age.Estimate.2012 * 100
  county.summary.df$dem.2012 = results.2012$BARACK.OBAMA
  county.summary.df$rep.2012 = results.2012$MITT.ROMNEY
  county.summary.df$oth.2012 = results.2012$other.votes

  county.summary.df$diff.2012.num = results.2012$MITT.ROMNEY - results.2012$BARACK.OBAMA
  county.summary.df$diff.2012.perc = (county.summary.df$diff.2012.num / county.summary.df$turnout.2012) * 100

  county.summary.2012.df = county.summary.df
  county.summary.2012.df$year = 2012
  county.summary.2012.df$dem.2012.perc = county.summary.2012.df$dem.2012 / results.2012$Total.Votes.Cast *100
  county.summary.2012.df$rep.2012.perc = county.summary.2012.df$rep.2012 / results.2012$Total.Votes.Cast *100
  county.summary.2012.df$oth.2012.perc = county.summary.2012.df$oth.2012 / results.2012$Total.Votes.Cast *100

  county.summary.2012.df$winner.party = ifelse(
    county.summary.2012.df$rep.2012.perc >
      county.summary.2012.df$dem.2012.perc,
    "Rep",
    "Dem"
  )
  county.summary.2012.df$winner = ifelse(
    county.summary.2012.df$rep.2012.perc >
      county.summary.2012.df$dem.2012.perc,
    "Romney",
    "Obama"
  )
  county.summary.2012.df$total.poss.voters = voting.age.people$Voting.Age.Estimate.2012

  county.summary.2012.final = with(
    county.summary.2012.df,
    data.frame(
      county,
      year,
      dem.2012,
      dem.2012.perc,
      rep.2012,
      rep.2012.perc,
      oth.2012,
      oth.2012.perc,
      diff.2012.num,
      diff.2012.perc,
      winner.party,
      winner,
      turnout.2012,
      turnout.2012.perc,
      total.poss.voters
    )
  )
  colnames(county.summary.2012.final) = c(
    "county",
    "year",
    "dem.vote",
    "dem.vote.perc",
    "rep.vote",
    "rep.vote.perc",
    "oth.vote",
    "oth.vote.perc",
    "num.diff",
    "perc.diff",
    "winning.party",
    "winner",
    "turnout",
    "turnout.perc",
    "total.voting.age"
  )
  county.summary.2012.final$turnout = as.integer(as.character(county.summary.2012.final$turnout))
  county.summary.2012.final$county = as.character(county.summary.2012.final$county)
  ## Swing votes 2016 to 2012
  county.summary.2016.final$dem.change = county.summary.2016.final$dem.vote.perc -
                                        county.summary.2012.final$dem.vote.perc
  county.summary.2016.final$rep.change = county.summary.2016.final$rep.vote.perc -
    county.summary.2012.final$rep.vote.perc
  county.summary.2016.final$oth.change = county.summary.2016.final$oth.vote.perc -
    county.summary.2012.final$oth.vote.perc

  county.summary.2016.final$swing.num = with(county.summary.df, num.diff - diff.2012.num)
  county.summary.2016.final$swing.perc = with(county.summary.df, perc.diff - diff.2012.perc)
  county.summary.2016.final$swing.turnout = with(county.summary.df, turnout - turnout.2012)
  county.summary.2016.final$swing.turnout.perc = with(county.summary.df,
                                              turnout.perc.allage.est - turnout.2012.perc)

  # rbind(county.summary.2012.final,county.summary.2012.final)
} # 2012 county data frame
{ # 2008 county data frame
  election.2008 = read.csv("Historical elections\\2008_FallElection_President_WardbyWardMOD.csv",
                           stringsAsFactors = F)

  election.2008.final.group = group_by(election.2008, COUNTY)

  election.2008.final = dplyr::summarise(
    election.2008.final.group,
    dem.vote = sum(Democrat),
    rep.vote = sum(Republican),
    oth.vote = sum(other.votes),
    turnout = sum(Total.votes)
  )
  election.2008.final = election.2008.final[-c(1), ]
  colnames(election.2008.final)[1] = c("county")
  election.2008.final$county[election.2008.final$county == "LaCrosse"] = "La Crosse"
  election.2008.final$county = as.character(election.2008.final$county)
  election.2008.final$year = 2008

  election.2008.final$dem.vote.perc = (election.2008.final$dem.vote / election.2008.final$turnout) * 100
  election.2008.final$rep.vote.perc = (election.2008.final$rep.vote / election.2008.final$turnout) * 100
  election.2008.final$oth.vote.perc = (election.2008.final$oth.vote / election.2008.final$turnout) * 100

  election.2008.final$num.diff = election.2008.final$rep.vote - election.2008.final$dem.vote
  election.2008.final$perc.diff = election.2008.final$rep.vote.perc - election.2008.final$dem.vote.perc

  election.2008.final$winner = ifelse(election.2008.final$rep.vote >
                                        election.2008.final$dem.vote,
                                      "McCain",
                                      "Obama")
  election.2008.final$winning.party = ifelse(election.2008.final$rep.vote >
                                               election.2008.final$dem.vote,
                                             "Rep",
                                             "Dem")
  election.2008.final$turnout.perc = NA
  election.2008.final$total.voting.age = NA

  election.2008.final$turnout = as.integer(as.character(election.2008.final$turnout))
  #### SWING IN VOTES 2012 to 2008
  county.summary.2012.final$swing.num = county.summary.2012.final$num.diff - election.2008.final$num.diff
  county.summary.2012.final$swing.perc =   county.summary.2012.final$perc.diff - election.2008.final$perc.diff
  county.summary.2012.final$swing.turnout = county.summary.2012.final$turnout - election.2008.final$turnout
  county.summary.2012.final$swing.turnout.perc = NA

  county.summary.2012.final$dem.change = county.summary.2012.final$dem.vote.perc -
    election.2008.final$dem.vote.perc
  county.summary.2012.final$rep.change = county.summary.2012.final$rep.vote.perc -
    election.2008.final$rep.vote.perc
  county.summary.2012.final$oth.change = county.summary.2012.final$oth.vote.perc -
    election.2008.final$oth.vote.perc

  election.2008.final$swing.num = NA
  election.2008.final$swing.perc =   NA
  election.2008.final$swing.turnout = NA
  election.2008.final$swing.turnout.perc = NA
} # 2008 data frame
{ # 2004 county data frame
  election.2004 = read.csv("Historical elections\\2004_FallElection_President_WardbyWardMOD.csv",stringsAsFactors = F)
  election.2004$COUNTY.1 = as.character(election.2004$COUNTY.1)

  election.2004.final.group = group_by(election.2004, COUNTY.1)
  election.2004.final = dplyr::summarise(
    election.2004.final.group,
    dem.vote = sum(Democrat),
    rep.vote = sum(Republican),
    oth.vote = sum(other.votes),
    turnout = sum(Total.votes)
  )
  election.2004.final = election.2004.final[-c(1), ]
  colnames(election.2004.final)[1] = c("county")
  election.2004.final$county[election.2004.final$county == "LaCrosse"] = "La Crosse"
  election.2004.final$county = as.character(election.2004.final$county)
  election.2004.final$year = 2004

  election.2004.final$dem.vote.perc = (election.2004.final$dem.vote / election.2004.final$turnout) * 100
  election.2004.final$rep.vote.perc = (election.2004.final$rep.vote / election.2004.final$turnout) * 100
  election.2004.final$oth.vote.perc = (election.2004.final$oth.vote / election.2004.final$turnout) * 100


  election.2004.final$num.diff = election.2004.final$rep.vote - election.2004.final$dem.vote
  election.2004.final$perc.diff = election.2004.final$rep.vote.perc - election.2004.final$dem.vote.perc

  election.2004.final$winner = ifelse(election.2004.final$rep.vote >
                                        election.2004.final$dem.vote,
                                      "Bush",
                                      "Kerry")
  election.2004.final$winning.party = ifelse(election.2004.final$rep.vote >
                                               election.2004.final$dem.vote,
                                             "Rep",
                                             "Dem")
  election.2004.final$turnout.perc = NA
  election.2004.final$total.voting.age = NA

  election.2004.final$turnout = as.integer(as.character(election.2004.final$turnout))
  #### SWING IN VOTES 2008 to 2004
  election.2008.final$swing.num = election.2008.final$num.diff - election.2004.final$num.diff
  election.2008.final$swing.perc =   election.2008.final$perc.diff - election.2004.final$perc.diff
  election.2008.final$swing.turnout = election.2008.final$turnout - election.2004.final$turnout
  election.2008.final$swing.turnout.perc = NA

  election.2008.final$dem.change = election.2008.final$dem.vote.perc -
    election.2004.final$dem.vote.perc
  election.2008.final$rep.change = election.2008.final$rep.vote.perc -
    election.2004.final$rep.vote.perc
  election.2008.final$oth.change = election.2008.final$oth.vote.perc -
    election.2004.final$oth.vote.perc

  election.2004.final$swing.num = NA
  election.2004.final$swing.perc =   NA
  election.2004.final$swing.turnout = NA
  election.2004.final$swing.turnout.perc = NA
} # 2004 data frame
{ # 2000 county data frame
  election.2000 = read.csv("Historical elections\\2000001107_PRES_SORTMOD.csv", stringsAsFactors = F)

  election.2000.final.group = group_by(election.2000, COUNTY)
  election.2000.final = dplyr::summarise(
    election.2000.final.group,
    dem.vote = sum(Democrat),
    rep.vote = sum(Republican),
    oth.vote = sum(other.votes),
    turnout = sum(Total.votes)
  )
  election.2000.final = election.2000.final[-c(1), ]
  colnames(election.2000.final)[1] = c("county")
  election.2000.final$county[election.2000.final$county == "LaCrosse"] = "La Crosse"
  election.2000.final$county = as.character(election.2000.final$county)
  election.2000.final$year = 2000

  election.2000.final$dem.vote.perc = (election.2000.final$dem.vote / election.2000.final$turnout) * 100
  election.2000.final$rep.vote.perc = (election.2000.final$rep.vote / election.2000.final$turnout) * 100
  election.2000.final$oth.vote.perc = (election.2000.final$oth.vote / election.2000.final$turnout) * 100


  election.2000.final$num.diff = election.2000.final$rep.vote - election.2000.final$dem.vote
  election.2000.final$perc.diff = election.2000.final$rep.vote.perc - election.2000.final$dem.vote.perc

  election.2000.final$winner = ifelse(election.2000.final$rep.vote >
                                        election.2000.final$dem.vote,
                                      "Bush",
                                      "Gore")
  election.2000.final$winning.party = ifelse(election.2000.final$rep.vote >
                                               election.2000.final$dem.vote,
                                             "Rep",
                                             "Dem")
  election.2000.final$turnout.perc = NA
  election.2000.final$total.voting.age = NA

  election.2000.final$turnout = as.integer(as.character(election.2000.final$turnout))
  #### SWING IN VOTES 2008 to 2004
  election.2004.final$swing.num = election.2004.final$num.diff - election.2000.final$num.diff
  election.2004.final$swing.perc =   election.2004.final$perc.diff - election.2000.final$perc.diff
  election.2004.final$swing.turnout = election.2004.final$turnout - election.2000.final$turnout
  election.2004.final$swing.turnout.perc = NA

  election.2004.final$dem.change = election.2004.final$dem.vote.perc -
    election.2000.final$dem.vote.perc
  election.2004.final$rep.change = election.2004.final$rep.vote.perc -
    election.2000.final$rep.vote.perc
  election.2004.final$oth.change = election.2004.final$oth.vote.perc -
    election.2000.final$oth.vote.perc


  election.2000.final$swing.num = NA
  election.2000.final$swing.perc =   NA
  election.2000.final$swing.turnout = NA
  election.2000.final$swing.turnout.perc = NA

  election.2000.final$dem.change = NA
  election.2000.final$rep.change = NA
  election.2000.final$oth.change = NA
} # 2000 data frame

# Making final length data frame
counties.2000.2016 = rbind(county.summary.2016.final,county.summary.2012.final,
                            election.2008.final,election.2004.final,election.2000.final)
counties.2000.2016$county = factor(counties.2000.2016$county)

# Adding extra columns
county.winners = as.data.frame(table(counties.2000.2016$county[counties.2000.2016$winning.party == "Dem"]))
colnames(county.winners) = c("county","dem.wins")

counties.2000.2016 = join(counties.2000.2016, county.winners, by = "county")
counties.2000.2016$mostly.dem = with(counties.2000.2016, ifelse(dem.wins >= 3, "Mostly Dem",
                                                                  "Mostly Rep"))
counties.2000.2016$dem.2016 = with(counties.2000.2016, ifelse(year == 2016 &
                                                                  winning.party == "Dem",
                                                                "Dem2016", "Rep2016"))
low.turnout.quant = quantile(counties.2000.2016$swing.turnout.perc[counties.2000.2016$year == 2016])[2]
low.turnout.quant = unname(low.turnout.quant)

counties.2000.2016$most.turnout.drop = with(counties.2000.2016, ifelse(swing.turnout.perc < low.turnout.quant,
                                                                         "Large turnout change 2016", "Little turnout change 2016"))

counties.2000.2016.2016 = counties.2000.2016
counties.2000.2016.2016 = subset(counties.2000.2016.2016, !is.na(most.turnout.drop))

counties.2000.2016.quant.join = data.frame(counties.2000.2016.2016$county,counties.2000.2016.2016$most.turnout.drop)
colnames(counties.2000.2016.quant.join) = c("county","most.turnout.drop")

counties.2000.2016 = join(counties.2000.2016, counties.2000.2016.quant.join, by = "county")

# Loading voting machine data ---------------------------------------------
# This file from here: http://elections.wi.gov/elections-voting/voting-equipment/voting-equipment-use
# Voting machine equipment for 13 September 2016
vot.equip = read.xlsx(
  "Wisconsin election stats\\voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(vot.equip) = c(
  "county.long",
  "municipality",
  "machine.vendor.dealer.model",
  "accessible.vendor.dealer.model"
)

# Following from
# http://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
county.split.att2 = gsub(" COUNTY.*$", "", vot.equip$county.long)
vot.equip$county = county.split.att2
vot.equip = vot.equip[!is.na(vot.equip$county.long), ]

vot.equip$use.machines = NA
vot.equip$use.machines = ifelse(vot.equip$machine.vendor.dealer.model == "None ", 0, 1)

vot.equip$machine.vendor.dealer.spec = gsub("Optech-", "Optech ", vot.equip$machine.vendor.dealer.model)
vot.equip$machine.vendor.dealer.spec = gsub("Optech/", "Optech ", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub(" .*$", "", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub("None", NA, vot.equip$machine.vendor.dealer.spec)

vot.equip.county.grouped = group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.withmachines = subset(vot.equip, use.machines > 0)

vot.equip.county.grouped.machines = group_by(vot.equip.withmachines, county)
vot.equip.county.machines = dplyr::summarise(
  vot.equip.county.grouped.machines,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.county.machines$use.machines.prop = NULL

max.vendor = as.character(1:length(vot.equip.county.machines$county))
for (i in 1:length(unique(vot.equip.county.machines$county))) {
  a = count(vot.equip.withmachines$machine.vendor.dealer.spec[vot.equip.withmachines$county ==
                                                                vot.equip.withmachines$county[i]])
  a$x = as.character(a$x)
  max.vendor[i] = a$x[a$freq == max(a$freq)]
}

vot.equip.county.machines$machine.most.used = max.vendor
vot.equip.county = join(vot.equip.county,
                        vot.equip.county.machines,
                        by = "county",
                        match = "first")

test = tolower(vot.equip.county$county)
test = capwords(test)
vot.equip.county$county = test


machine.join = data.frame(vot.equip.county$county, vot.equip.county$use.machines.prop, vot.equip.county$machine.most.used)
colnames(machine.join) = c("county","use.machines.prop","machine.most.used")

machine.join$all.machines = with(machine.join, ifelse(use.machines.prop > 0.75, "Mostly voting machines",
                                                      "Some or no voting machines"))

machine.join$county = as.character(machine.join$county)
machine.join$county[machine.join$county == "Fond Du Lac"] = "Fond du Lac"

counties.2000.2016 = join(counties.2000.2016,machine.join,by="county")
county.summary.2016.final = join(county.summary.2016.final,machine.join,by="county")
county.summary.df = join(county.summary.df,machine.join,by="county")

rm(vote.equip.county.machines,vote.equip.county.grouped,voting.age.people,test,
   machine.join, historical.turnout.join,a,)

# Write final csvs --------------------------------------------------------

write.csv(election.2016.grouped,"candidate.2016.summary.csv")
write.csv(voting.df.2016,"counties.2016.bycand.csv")
write.csv(county.summary.df,"county.2016.vs.2012.csv")
write.csv(counties.2000.2016,"counties.2000.2016.csv")
write.csv(all_polls_2012,"all_polls_2012.csv")
write.csv(all_polls_2016,"all_polls_2016.csv")
write.csv(selected_polls_2012.sub,"selected_polls_2012.sub.csv")
write.csv(selected_polls_2016.sub,"selected_polls_2016.sub.csv")
write.csv(elections.100.years,"elections.100.years.csv")



# Read in csvs and subset for use ------------------------------------------------------------
require(ggplot2); library(plotly); require(corrplot)
require("devtools")  # so we can install from GitHub
# devtools::install_github("ropensci/plotly")  # plotly is part of rOpenSci

# I got this from here: http://stackoverflow.com/questions/31337922/adding-italicised-r-with-correlation-coefficient-to-a-scatter-plot-chart-in-ggpl
corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")

candidate.2016.summary = read.csv("candidate.2016.summary.csv")
counties.2016.bycand = read.csv("counties.2016.bycand.csv")
county.2016.vs.2012 = read.csv("county.2016.vs.2012.csv")
counties.2000.2016 = read.csv("counties.2000.2016.csv")
elections.100.years = read.csv("elections.100.years.csv")
all_polls_2012 = read.csv("all_polls_2012.csv")
all_polls_2016 = read.csv("all_polls_2016.csv")
selected_polls_2012.sub = read.csv("selected_polls_2012.sub.csv")
selected_polls_2016.sub = read.csv("selected_polls_2016.sub.csv")

elections.100.years.summary.2016 = subset(elections.100.years, year == 2016)
elections.100.years.postwar = subset(elections.100.years, year >= 1948)

## Comparing the definite poll voters with the final result
poll.summary.2016 = data.frame(c(1:4))
poll.summary.2016$cand.group = c("Donald J. Trump", "Hillary Clinton", "Other","Undecided")
poll.summary.2016$polls.nov = with(selected_polls_2016.sub,
                                   c(mean(trump),mean(clinton),mean(other),mean(undecided,na.rm=T)))
poll.summary.2016$c.1.4. = NULL
poll.summary.2016$polls.nov.def = with(elections.100.years.summary.2016, c(mean.rep.poll.oneweek.defs, mean.dem.poll.oneweek.defs, mean.oth.poll.oneweek.defs,NA))

candidate.2016.summary$polls.nov = with(poll.summary.2016, c(polls.nov[1], polls.nov[2], rep(polls.nov[3],5)))
candidate.2016.summary$polls.nov.def = with(poll.summary.2016, c(polls.nov.def[1], polls.nov.def[2], rep(polls.nov.def[3],5)))

candidate.2016.summary$diff.from.poll = candidate.2016.summary$votes.perc.group - candidate.2016.summary$polls.nov
candidate.2016.summary$diff.from.poll.defs = candidate.2016.summary$votes.perc.group - candidate.2016.summary$polls.nov.def


### 2012 results ###
elections.100.years.summary.2012 = elections.100.years.postwar[2,]

poll.summary.2012 = data.frame(c(1:4))
poll.summary.2012$cand.group = c("Mitt Romney", "Barack Obama", "Other","Undecided")
poll.summary.2012$polls.nov = with(selected_polls_2012.sub,
                                   c(mean(romney),mean(obama),mean(other,na.rm=T),mean(undecided,na.rm=T)))
poll.summary.2012$c.1.4. = NULL
poll.summary.2012$polls.nov.def = with(elections.100.years.summary.2012, c(mean.rep.poll.oneweek.defs, mean.dem.poll.oneweek.defs, mean.oth.poll.oneweek.defs,NA))

poll.summary.2012$vote = with(elections.100.years.summary.2012, c(rep.vote, dem.vote, oth.vote,NA))
poll.summary.2012$vote.perc = with(elections.100.years.summary.2012, c(rep.vote.perc, dem.vote.perc, oth.vote.perc,NA))

poll.summary.2012$diff.from.polls = with(poll.summary.2012, vote.perc - polls.nov)
poll.summary.2012$diff.from.polls.defs = with(poll.summary.2012, vote.perc - polls.nov.def)

poll.summary.2012.nounds = poll.summary.2012[1:3,]

## VISUALISING THE 2016 RESULTS
county.summary.2016.final = subset(counties.2000.2016, year == 2016)
county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$county), ]
added.columns = county.summary.2016.final[,20:28]
county.2016.vs.2012 = cbind(county.2016.vs.2012, added.columns)

county.2016.vs.2012$total.voting.age= as.integer(county.2016.vs.2012$total.voters.age.est)
county.2016.vs.2012.2000ppl = subset(county.2016.vs.2012, turnout > 2000)

county.2016.vs.2012.2000ppl.mildane= subset(county.2016.vs.2012.2000ppl, county != "Milwaukee" & county != "Dane")


county.summary.2016.final$mostly.dem.num = ifelse(county.summary.2016.final$mostly.dem == "Mostly Dem",1,0)
county.summary.2016.final$dem.other.change = county.summary.2016.final$dem.change - county.summary.2016.final$oth.change

county.summary.2016.final.highpop = subset(county.summary.2016.final, total.voting.age > mean(total.voting.age))
county.summary.2016.final$total.voting.age= as.integer(county.summary.2016.final$total.voting.age)
county.summary.2016.final.2000ppl= subset(county.summary.2016.final, turnout > 2000)

county.summary.2016.final.2000ppl.mildane= subset(county.summary.2016.final.2000ppl, county != "Milwaukee" & county != "Dane")

counties.2000.2016.2000ppl= subset(counties.2000.2016, turnout > 2000)

counties.2000.2016.2000ppl.mildane = subset(counties.2000.2016.2000ppl, county != "Milwaukee" & county != "Dane")

# Comparing 2016 candidate results overall -------------------------------------
# The majority of 'other' voters went for Gary Johnson. Hillary massively underperformed compared
# to decided voter polling, and Trump slightly overperformed. This means that more of the 'undecideds'
# went to Trump, or that many undecideds that would have voted Hillary chose not to turn up.

candidate.2016.summary.vis.defs = ggplot(candidate.2016.summary,
                                            aes(x = ordered.cands, y = votes.perc, fill = cand,group =1)) +
  geom_bar(stat = "identity") +
  geom_line(stat = "identity",
            aes(y = elections.100.years.summary.2016$mean.dem.poll.oneweek.defs),
            colour = "blue", linetype = 2,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = elections.100.years.summary.2016$mean.rep.poll.oneweek.defs),
            colour = "red", linetype = 2,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = elections.100.years.summary.2016$mean.oth.poll.oneweek.defs),
            colour = "black", linetype = 2,
            show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(
    name =
      "Percentage votes for the candidates.Lines indicate polling\n of decided voters,excluding those who were unsure."
  ) +
  scale_x_discrete(
    limit =  candidate.2016.summary$ordered.cands,
    labels = as.character(candidate.2016.summary$cand),
    name = NULL) +
  scale_fill_manual(values = colours)
candidate.2016.summary.vis.defs

# This is a cheesy way to remove all the other candidates apart from Johnson
candidate.2016.summary.small = subset(candidate.2016.summary, votes.rec > 100000)

# Comparing polls of ALL with results
# poll.2016.compare.vis.all = ggplot(candidate.2016.summary.small, aes(x = cand.group, y = diff.from.poll, fill = cand.group, group = 1)) +
#   # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
#   geom_bar(stat = "identity", show.legend = F) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   coord_cartesian(ylim = c(-6,6)) +
#   scale_y_continuous(
#     name =
#       "The % difference between all voter polls\n and the final result of the 2016 presidential election."
#   ) +
#   scale_x_discrete(
#     limit =  candidate.2016.summary.small$cand.group,
#     labels = as.character(candidate.2016.summary.small$cand.group),
#     name = NULL) +
#   scale_fill_manual(values = c("lightcoral","light blue","dark grey"))
# poll.2016.compare.vis.all



# The divergence between definite voters in polls and reality was greatest for Clinton,
# and the 'other' votes was not much different from the polls. So, either the turnout dropped
# significantly for Clinton, or the undecideds went to Trump
poll.2016.compare.vis.defs = ggplot(candidate.2016.summary.small, aes(x = cand.group, y = diff.from.poll.defs, fill = cand.group, group = 1)) +
  # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-5,5)) +
  scale_y_continuous(
    name =
      "The % difference between definite voter polls\n and the final result of the 2016 presidential election."
  ) +
  scale_x_discrete(
    limit =  candidate.2016.summary.small$cand.group,
    labels = as.character(candidate.2016.summary.small$cand.group),
    name = NULL) +
  scale_fill_manual(values = c("lightcoral","light blue","dark grey"))
poll.2016.compare.vis.defs


# Comparing the 2012 and 2016 polls ---------------------------------------
# Were the polls in 2012 similarly off ?
## Comparing the definite poll voters with the final result

# Comparing polls of ALL with results
# poll.2012.compare.vis.all = ggplot(poll.summary.2012.nounds, aes(x = cand.group, y = diff.from.polls, fill = cand.group, group = 1)) +
#   # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
#   geom_bar(stat = "identity", show.legend = F) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   coord_cartesian(ylim = c(-6,6)) +
#   scale_y_continuous(
#     name =
#       "The % difference between all voter polls\n and the final result of the 2012 presidential election."
#   ) +
#   scale_x_discrete(
#     limit =  poll.summary.2012.nounds$cand.group,
#     labels = as.character(poll.summary.2012.nounds$cand.group),
#     name = NULL) +
#   scale_fill_manual(values = c("light blue","lightcoral","dark grey"))
# poll.2012.compare.vis.all


# Comparing polls of decideds with results
# This shows that the polls of the decided votes can be off by 3% ish
poll.2012.compare.vis.defs = ggplot(poll.summary.2012.nounds, aes(x = cand.group, y = diff.from.polls.defs, fill = cand.group, group = 1)) +
  # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-5,5)) +
  scale_y_continuous(
    name =
      "The % difference between definite voter polls\n and the final result of the 2012 presidential election."
  ) +
  scale_x_discrete(
    limit =  poll.summary.2012.nounds$cand.group,
    labels = as.character(poll.summary.2012.nounds$cand.group),
    name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral","dark grey"))
poll.2012.compare.vis.defs


# Compare decided voters for 2016 and 2012 with polls. This shows that it has happened before that
# the polls can be ~3% off
compare.2016.2012.polls.plotnames = c("poll.2016.compare.vis.defs",
                                      "poll.2012.compare.vis.defs")
compare.2016.2012.polls = marrangeGrob(
  grobs = mget(compare.2016.2012.polls.plotnames),
  nrow = 1,
  ncol = 2,
  top = NULL
)
compare.2016.2012.polls


# Overview of historical elections 1900 onwards ---------------------------

# This graphs shows that the swing from the previous election in terms of democrat to republican
# was not significant
historical.election.data.swing = ggplot(elections.100.years.postwar,
                                  aes(
                                    x = year,
                                    y = swing.vote.perc,
                                    fill = winner,
                                    group = 1
                                  )) +
  geom_bar(stat = "identity") +
  geom_line(
    aes(y = elections.100.years.postwar$swing.turnout.perc),
    alpha = 0.5,
    stat = "identity",
    colour = "black"
  ) +
  scale_y_continuous(
    name =
      "Swing vote percentage from previous election with time.\nLine indicates change in % turnout compared with previous election."
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(
    "text",
    x = mean(elections.100.years.postwar$year),
    y = min(elections.100.years.postwar$swing.vote.perc),
    label = "Vote swing towards Democrats",
    vjust = 1,
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = mean(elections.100.years.postwar$year),
    y = max(elections.100.years.postwar$swing.vote.perc),
    label = "Vote swing towards Republicans",
    vjust = 1,
    hjust = 0.5
  ) +
  scale_fill_manual(values = c("light blue","lightcoral"))
historical.election.data.swing

## The swing is within historical limits, but perhaps the bigger story is the 'other' vote

# Change in Dem votes compared to last election with turnout swing line
historical.election.data.dem.votes = ggplot(elections.100.years.postwar,
                                  aes(
                                    x = year,
                                    y = dem.vote.perc,
                                    fill = winner,
                                    group = 1
                                  )) +
  geom_bar(stat = "identity") +
  geom_line(
    aes(y = elections.100.years.postwar$turnout.perc),
    alpha = 0.5,
    stat = "identity",
    colour = "black"
  ) +
  geom_bar(stat = "identity",fill = "grey", show.legend = FALSE,
           aes(y = oth.vote.perc)
           ) +
  scale_y_continuous(
    name = "Democrat and other votes with time (%). Grey bars indicate other votes.\nLine indicates % turnout.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1948,2016,4)) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
historical.election.data.dem.votes

# As overall voter numbers instead of percentages. Definitely can see that the 'other' vote
# is substantially larger than previous years. The polls saw this. But who is voting other, and why?
# Maybe I should compare this with country-wide other results to check that the other vote is not out
# of line.

# At time of writing (15-11-16), Trump has: 47.2% votes 60,834,437. Clinton has: 47.9% votes 61,782,016.
# So, other has 100 - (47.2 + 47.9) = 4.9%. Therefore, Wisconsin other vote is higher than average (6.26%).
# Also, the other vote was higher than what the polls predicted by about 1 %.


# Overall voter numbers
historical.election.data.dem.votes.num = ggplot(elections.100.years.postwar,
                                                aes(
                                                  x = year,
                                                  y = dem.vote
                                                )) +
  geom_bar(stat = "identity", aes(fill = winner)) +
  geom_bar(stat = "identity",fill = "grey", show.legend = FALSE,
           aes(y = oth.vote)
  ) +
  geom_line(
    aes(y = elections.100.years.postwar$turnout),group = 1
  ) +

  scale_y_continuous(
    name = "Democrat plus other votes with time (no. of voters).\nLine indicates % turnout."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1948,2016,4)) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
historical.election.data.dem.votes.num

# Comparing by county 2016 -------------------------------------------------------------
county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$dem.votes.perc), ]
county.2016.vs.2012$ordered.county.2016.dem.votes.perc = c(1:length(county.2016.vs.2012$dem.votes.perc))

# Visualising votes for clinton by county and by turnout. Turnout does not seem to differ drastically according to
# Republican won or Clinton won county
county.perc.winner = ggplot(
  county.2016.vs.2012,
  aes(
    x = county.2016.vs.2012$ordered.county.2016.dem.votes.perc,
    y = dem.votes.perc,
    fill = winner
  )) +
  geom_bar(
    aes(y = county.2016.vs.2012$turnout.perc.allage.est ),
    stat = "identity",
    fill = "grey"
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(name =
  paste("Percentage vote for Clinton in Wisconsin counties.\nGrey bar behind indicates percentage turnout")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.dem.votes.perc,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner


county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$oth.votes.perc), ]
county.2016.vs.2012$ordered.county.2016.oth.votes.perc = c(1:length(county.2016.vs.2012$oth.votes.perc))

# Visualising votes for other by county and by turnout. Turnout does not seem to differ drastically according to
# Republican won or Clinton won county
county.perc.winner.other = ggplot(
  county.2016.vs.2012,
  aes(
    x = county.2016.vs.2012$ordered.county.2016.oth.votes.perc,
    y = oth.votes.perc,
    fill = winner
  )) +
  geom_bar(
    aes(y = county.2016.vs.2012$swing.turnout.perc),
    stat = "identity",
    fill = "grey", width = 0.8
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(name =
                       paste("Percentage vote for 'Other' in Wisconsin counties.\nGrey bar behind indicates change in turnout from 2012 (%)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.oth.votes.perc,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner.other


# Visualising votes for clinton against turnout
# Votes for other parties was pretty consistent regardless of being mostly dem or mostly republican
county.perc.turnout.dems = ggplot(
  county.2016.vs.2012.2000ppl,
  aes(
    y = dem.votes.perc,
    x = turnout.perc.allage.est,
    colour = winner
  )
) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.dems

correlation(county.2016.vs.2012.2000ppl$dem.votes.perc,county.2016.vs.2012.2000ppl$turnout.perc.allage.est)
# 0.1840505

# Visualising votes for clinton against other.
# Counties closest to middle most likely to vote other
county.perc.oth.vs.dem = ggplot(
  county.2016.vs.2012.2000ppl,
  aes(
    x = oth.votes.perc,
    y = dem.votes.perc,
    colour = winner
  )) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name ="Percentage vote for Clinton in counties against votes for 'other'.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("light blue","lightcoral"))
  county.perc.oth.vs.dem

# Visualising votes for other against turnout
# Votes for other parties was pretty consistent regardless of being mostly dem or mostly republican
county.perc.turnout.other = ggplot(
  county.2016.vs.2012.2000ppl,
  aes(
    x = turnout.perc.allage.est,
    y = oth.votes.perc,
    colour = winner
  )
) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.other


# Visualising the swing in votes
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$swing.perc), ]
county.summary.2016.final$ordered.county.swing.perc = c(1:length(county.summary.2016.final$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.2016.final,
                                     aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity",
                                         aes(y = (
                                           county.summary.2016.final$swing.turnout.perc
                                         )),
                                         fill = "grey",
                                         width = 0.4) +
  scale_y_continuous(
    name = "Difference in 2016 vote between Democrats and Republicans for counties (%).<br> Grey bars: difference in percentage turnout from the 2012 election.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.county.swing.perc,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.perc
# ggplotly(county.2012v2016.swing.perc, session = "knitr") %>% # , width = 800, height = 600
# layout(bargap = 3, autosize=T, margin = default.margin) # l, r, b, t, pad


# Visualising votes for other against turnout
# Votes for other parties was pretty consistent regardless of being mostly dem or mostly republican
county.num.turnout.other = ggplot(
  county.2016.vs.2012,
  aes(
    x = log10(turnout),
    y = use.machines.prop,
    colour = winner
  )) +
  geom_point() +
  # geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_colour_manual(values = c("light blue","lightcoral"))
county.num.turnout.other


# Visualising votes for other against percentage machines
# Votes for other parties was pretty consistent regardless of the proportion of machines
county.perc.machines.other = ggplot(
  county.2016.vs.2012,
  aes(
    x = use.machines.prop,
    y = oth.votes.perc,
    colour = winner
  )) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for other in counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_colour_manual(values = c("light blue","lightcoral"))
county.perc.machines.other


# Visualising votes for other against percentage machines
# Votes for other parties in total amount seemed to be higher in 100% machine states
county.perc.winner = ggplot(
  county.2016.vs.2012,
  aes(
    x = use.machines.prop,
    y = oth.votes,
    colour = winner
  )
) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name =
                       paste("Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner



county.summary.join = county.2016.vs.2012[c(2,9,23,32:39)]
voting.bycandcounty.2016.joined = join(voting.bycandcounty.2016,county.summary.join,by="county")

voting.bycandcounty.2016.joined.johnson = subset(voting.bycandcounty.2016.joined, cand == "Gary Johnson")

# Visualising votes for johnson against percentage machines
# Votes for johnson in total amount seemed to be higher in 100% machine states
county.perc.winner = ggplot(
  voting.bycandcounty.2016.joined.johnson,
  aes(
    x = use.machines.prop,
    y = votes.perc
  )
) +
  geom_point() +
  scale_y_continuous(name =
                       paste("Percentage vote for Johnson in Wisconsin counties\nagainst machines in county.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner

correlation(voting.bycandcounty.2016.joined.johnson$use.machines.prop,voting.bycandcounty.2016.joined.johnson$votes.perc)

# Visualising votes for johnson against turnout
# Votes for other parties in total amount seemed to be higher in 100% machine states
county.perc.winner = ggplot(
  voting.bycandcounty.2016.joined.johnson,
  aes(
    x = turnout.perc.allage.est,
    y = votes.perc
  )
) +
  geom_point() +
  scale_y_continuous(name =
                       paste("Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner


county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout.perc.allage.est), ]
county.2016.vs.2012$ordered.county.2016.turnout.perc.diff = c(1:length(county.2016.vs.2012$turnout.perc.allage.est))

# Visualise county turnout data as percentage
county.perc.turnout.winner = ggplot(
  county.2016.vs.2012,
  aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est,
      fill = winner), group = 1
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Estimated turnout in Trump and Clinton won counties.") +
  geom_line(aes(y = elections.100.years.summary.2016$turnout.perc), linetype = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.turnout.perc.diff,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.turnout.winner



## VISUALISING THE 2016 RESULTS
county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$perc.diff), ]
county.2016.vs.2012$ordered.county.2016.perc.diff = c(1:length(county.2016.vs.2012$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(county.2016.vs.2012,
                          aes(x = ordered.county.2016.perc.diff, y = perc.diff,
                              fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.perc.diff,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.diff

county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout), ]
county.2016.vs.2012$ordered.county.2016.turnout = c(1:length(county.2016.vs.2012$turnout))




# Votes as numbers
county.2016.trump.num = ggplot(county.2016.vs.2012,
                               aes(x = ordered.county.2016.turnout, y = rep.votes,
                                   fill = winner)) +
  geom_bar(
    stat = "identity",
    aes(y = county.2016.vs.2012$turnout),
    fill = "grey",
    alpha = 0.5
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.turnout,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2016.trump.num

votes.2016.compare.plotnames = c("county.perc.diff", "county.2016.trump.num")
votes.2016.compare = marrangeGrob(
  grobs = mget(votes.2016.compare.plotnames),
  nrow = 1,
  ncol = 2,
  top = NULL
)
votes.2016.compare

# Comparing 2012 and 2016 results by county --------------------------------

# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

# Visualising county data 2012 vs 2016
{
  county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$diff.2012.perc), ]
  county.2016.vs.2012$ordered.county.2012.perc.diff = c(1:length(county.2016.vs.2012$diff.2012.perc))

county.2012v2016.perc.diff.2016ord = ggplot(county.2016.vs.2012,
                                            aes(x = ordered.county.2012.perc.diff, y = diff.2012.perc)) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.2016.vs.2012$perc.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\nred bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2012.perc.diff,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.perc.diff.2016ord
}

# As number instead of percentage
{
# county.differences.years.num = ggplot(county.2016.vs.2012,
#                                       aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)) +
#   geom_bar(stat = "identity") + geom_bar(
#     stat = "identity",
#     aes(y = county.2016.vs.2012$num.diff),
#     fill = "red",
#     alpha = 0.5
#   ) +
#   scale_y_continuous(
#     name = "Number of votes difference between Democrat and Republican parties in 2012 and 2016 by county. Grey bars are 2012 results,\nred bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(
#     limit =  county.2016.vs.2012$ordered.county.2012.perc.diff,
#     labels = as.character(county.2016.vs.2012$county),
#     name = NULL
#   ) +
#   scale_fill_manual(values = c("light blue", "lightcoral"))
# county.differences.years.num
#
# absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord",
#                                          "county.differences.years.num")
# absolute.2016.2012.compare = marrangeGrob(
#   grobs = mget(absolute.2016.2012.compare.plotnames),
#   nrow = 2,
#   ncol = 1,
#   top = NULL
# )
# absolute.2016.2012.compare
}

# Visualising the swing in votes
{
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$swing.perc), ]
county.summary.2016.final$ordered.county.swing.perc = c(1:length(county.summary.2016.final$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.2016.final,
                                     aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity",
                                         aes(y = (
                                           county.summary.2016.final$swing.turnout.perc
                                         )),
                                         fill = "grey",
                                         width = 0.4) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,
below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.county.swing.perc,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.perc

# As number
# county.2012v2016.swing.num = ggplot(county.summary.2016.final,
#                                     aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)) +
#   geom_bar(stat = "identity") + geom_bar(
#     stat = "identity",
#     aes(y = county.summary.2016.final$swing.turnout),
#     fill = "grey",
#     alpha = 0.8
#   ) +
#   scale_y_continuous(
#     name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\nbelow 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers.",
#     breaks = c(seq(-60000,20000,10000))
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(
#     limit =  county.summary.2016.final$ordered.county.swing.perc,
#     labels = as.character(county.summary.2016.final$county),
#     name = NULL
#   ) +
#   scale_fill_manual(values = c("light blue", "lightcoral"))
# county.2012v2016.swing.num
#
# swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc",
#                                             "county.differences.years.num")
# swing.votes.2016.2012.compare = marrangeGrob(
#   grobs = mget(swing.votes.2016.2012.compare.plotnames),
#   nrow = 2,
#   ncol = 1,
#   top = NULL
# )
# swing.votes.2016.2012.compare
}


# Change in those voting 'Democrat' between 2012 and 2016 by turnout
{
  county.2012v2016.change.dem.perc.point = ggplot(county.summary.2016.final.2000ppl,
                                                  aes(x = swing.turnout.perc, y = dem.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in vote for Democrats 2012 - 2016 (%)"
    ) +
    scale_x_continuous(
      name = "Change in turnout 2012 - 2016 (%)"
    ) +
    geom_text(x = -8, y = 2,
              label = corr_eqn(county.summary.2016.final.2000ppl$swing.turnout.perc,
                               county.summary.2016.final.2000ppl$dem.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.dem.perc.point
}

# Change in those voting 'other' between 2012 and 2016 by turnout
{
county.2012v2016.change.oth.perc.point = ggplot(county.summary.2016.final.2000ppl,
                                                aes(x = swing.turnout.perc, y = oth.change, colour = winner)) +
  geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
  geom_point(stat = "identity") +
  scale_y_continuous(
    name = "Change in vote for other 2012 - 2016 (%)"
  ) +
    scale_x_continuous(
      name = "Change in turnout 2012 - 2016 (%)"
    ) +
  geom_text(x = -8, y = 5,
            label = corr_eqn(county.summary.2016.final.2000ppl$swing.turnout.perc,
                              county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
            show.legend = F, colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("light blue", "lightcoral"))
county.2012v2016.change.oth.perc.point
}

# Change in those voting dem - those between 2012 and 2016
{
  # The bigger the change from Democrat, the lower the change in other
  county.2012v2016.change.oth.dem.point = ggplot(county.summary.2016.final.2000ppl,
                                                 aes(x = dem.change, y = oth.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in voting other from 2012 (%)"
    ) +
    scale_x_continuous(
      name = "Change in Democrat from 2012 - 2016 (%)"
    ) +
    geom_text(x = 0, y = 5,
              label = corr_eqn(county.summary.2016.final.2000ppl$dem.change,
                               county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.oth.dem.point
}

# Correlation plot --------------------------------------------------------
plot(log(county.summary.2016.final.2000ppl$total.voting.age))
plot(county.summary.2016.final.2000ppl$total.voting.age)

corr.table = with(county.summary.2016.final.2000ppl, data.frame(dem.change, rep.change,
                                                  oth.change,swing.turnout.perc,log(turnout),
                                                  use.machines.prop,dem.wins))
correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")

require(nlme)
other.vote.lm = lm(log(oth.vote) ~ swing.turnout.perc +log(turnout) + use.machines.prop + dem.wins,data = county.summary.2016.final.2000ppl)
summary(other.vote.lm)

other.vote.perc.lm = lm(oth.vote.perc ~ swing.turnout.perc + log(turnout) + use.machines.prop + dem.wins,data = county.summary.2016.final.2000ppl)
summary(other.vote.perc.lm)

other.vote.perc.change.lm = lm(oth.change ~ swing.turnout.perc + log(turnout*use.machines.prop) + dem.wins,data = county.summary.2016.final.2000ppl)
summary(other.vote.perc.change.lm)
AIC(other.vote.perc.change.lm)
####
{
#Next, comparing residuals against each factor individually
layout.show(layout(matrix(c(1,2,3,4),2,2)))

A <- data.frame(rstandard(other.vote.perc.change.lm),
                other.vote.perc.change.lm$model$swing.turnout.perc,other.vote.perc.change.lm$model$turnout,
                other.vote.perc.change.lm$model$use.machines.prop,
                other.vote.perc.change.lm$model$dem.wins
                )
colnames(A) <- c("resid", "swing.turnout.perc","turnout","use.machines.prop","dem.wins")
plot(A$resid ~ A$swing.turnout.perc, xlab = "swing.turnout.perc",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$use.machines.prop, xlab = "turnout",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$use.machines.prop, xlab = "use.machines.prop",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$use.machines.prop, xlab = "dem.wins",
     ylab = "Residuals"); abline(0,0)

e2nona = A$resid[!is.na(A$resid)]
residlength = length(e2nona)
residsd = sd(e2nona, na.rm = TRUE)
residmean = mean(e2nona, na.rm = TRUE)
normed = (rnorm(10000, mean = residmean, sd = residsd))

#Following gives PROBABILITY DENSITIES instead of frequencies
hist(e2nona,xlab="Residuals", main="Model residuals vs rnorm 10000 (red line)",
     freq=FALSE, ylim = c(0,0.7), breaks = 15)
lines(density(normed), col="red")

shapiro.test(e2nona)
ks.test(e2nona,rnorm)
}
####

other.vote.perc.change.lm.log = lm(oth.change ~ swing.turnout.perc + log(turnout) + use.machines.prop + dem.wins,data = county.summary.2016.final.2000ppl)
summary(other.vote.perc.change.lm.log)
AIC(other.vote.perc.change.lm.log)
plot(other.vote.perc.change.lm.log)
####
{
#Next, comparing residuals against each factor individually
layout.show(layout(matrix(c(1,2,3,4),2,2)))

A <- data.frame(rstandard(other.vote.perc.change.lm.log),
                other.vote.perc.change.lm.log$model$swing.turnout.perc,other.vote.perc.change.lm.log$model$"log(turnout)",
                other.vote.perc.change.lm.log$model$use.machines.prop,
                other.vote.perc.change.lm.log$model$dem.wins
)

colnames(A) <- c("resid", "swing.turnout.perc","turnout","use.machines.prop","dem.wins")
plot(A$resid ~ A$swing.turnout.perc, xlab = "swing.turnout.perc",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$use.machines.prop, xlab = "turnout",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$use.machines.prop, xlab = "use.machines.prop",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$use.machines.prop, xlab = "dem.wins",
     ylab = "Residuals"); abline(0,0)

e2nona = A$resid[!is.na(A$resid)]
residlength = length(e2nona)
residsd = sd(e2nona, na.rm = TRUE)
residmean = mean(e2nona, na.rm = TRUE)
normed = (rnorm(10000, mean = residmean, sd = residsd))

#Following gives PROBABILITY DENSITIES instead of frequencies
hist(e2nona,xlab="Residuals", main="Model residuals vs rnorm 10000 (red line)",
     freq=FALSE, ylim = c(0,0.7), breaks = 15)
lines(density(normed), col="red")

shapiro.test(e2nona)
ks.test(e2nona,rnorm)
}
####

# Change in those voting 'other' between 2012 and 2016 by absolute turnout
{
  county.2012v2016.change.oth.turnout.point = ggplot(county.summary.2016.final.2000ppl,
                                                     aes(x = turnout, y = oth.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in vote for other 2012 - 2016 (%)"
    ) +
    scale_x_continuous(
      name = "Absolute turnout 2012 - 2016"
    ) +
    geom_text(x = 100000, y = 5,
              label = corr_eqn(county.summary.2016.final.2000ppl$turnout,
                               county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.oth.turnout.point
}

# Change in those voting 'other' between 2012 and 2016 by log of turnout
{
  county.2012v2016.change.oth.turnout.log.point = ggplot(county.summary.2016.final.2000ppl,
                                                     aes(x = log(turnout), y = oth.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in vote for other 2012 - 2016 (%)"
    ) +
    scale_x_continuous(
      name = "Log of turnout 2012 - 2016"
    ) +
    geom_text(x = 8, y = 5,
              label = corr_eqn(log(county.summary.2016.final.2000ppl$turnout),
                               county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.oth.turnout.log.point
}

other.turnout.2016.2012.compare.plotnames = c("county.2012v2016.change.oth.turnout.point",
                                              "county.2012v2016.change.oth.turnout.log.point")
other.turnout.2016.2012.compare = marrangeGrob(
  grobs = mget(other.turnout.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL)
other.turnout.2016.2012.compare

# Looking at County data through time -------------------------------------
# counties.2000.2016 = counties.2000.2016[order(counties.2000.2016$turnout), ]
# counties.2000.2016$ordered.turnout = c(1:length(counties.2000.2016$year))

### Other votes

historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = oth.vote.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Other vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.graph

historical.oth.vote.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = oth.change, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in other vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.change.graph

# Other numbers
historical.oth.vote.graph.hist = ggplot(counties.2000.2016.2000ppl[counties.2000.2016.2000ppl$year == 2016,],
                                   aes(x = oth.vote, colour = all.machines,
                                       group = all.machines)) +
  geom_histogram()
historical.oth.vote.graph.hist

historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl,
                                   aes(x = year, y = log(oth.vote), colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Log of average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.graph

# Removing Milwaukee and Dane counties ------------------------------------
corr.table.mildane = with(county.summary.2016.final.2000ppl.mildane, data.frame(dem.change, rep.change,
                                                                oth.change,swing.turnout.perc,log(total.voting.age),
                                                                use.machines.prop,dem.wins))
correlationmatrix.mildane = cor(corr.table.mildane, use = "pairwise.complete.obs")
corrplot.all = corrplot(correlationmatrix.mildane, method = "number", type = "upper")



# Other votes
historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                        aes(x = year, y = oth.vote.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Other vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral")) +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.graph

historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                        aes(x = year, y = oth.change, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in other vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.graph

# Other numbers
historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                   aes(x = year, y = oth.vote, colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.graph

# Other numbers
historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                   aes(x = year, y = oth.vote, colour = all.machines,
                                       group = county)) +
  geom_smooth() +
  scale_y_continuous(name = "Average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral")) +
  facet_wrap(~mostly.dem)
historical.oth.vote.graph



# Voting machines in counties ---------------------------------------------
# NOW, GRAPHING SWING BY PROPORTION OF ELECTRONIC VOTING MACHINES IN COUNTY
# Make sure to reorder before adding in new data!

# Now, let's order the counties by proportion of municipalities using voting machines
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$use.machines.prop), ]
county.summary.2016.final$use.machines.prop.order = c(1:length(county.summary.2016.final$use.machines.prop))

# Machines by county
county.vs.machines.bar = ggplot(
  county.summary.2016.final,
  aes(x = county.summary.2016.final$use.machines.prop.order, y = use.machines.prop * 100,
      fill = machine.most.used)
) +
  geom_bar(stat="identity") +
  scale_y_continuous(name = "Use of voting machines in each county (%)") +
  scale_x_discrete(
    limit =  county.summary.2016.final$use.machines.prop.order,
    labels = as.character(county.summary.2016.final$county),
    name = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
  scale_fill_discrete(name = "Primary machine vendor")
county.vs.machines.bar


# Swing percentage on x axis vs the % of municipalities that use voting machines in the county
county.swing.perc.vs.machines.point = ggplot(
  county.summary.2016.final,
  aes(x = swing.perc, y = use.machines.prop * 100,
      colour = machine.most.used)
) +
  geom_point() +
  scale_y_continuous(name = "Use of voting machines in each county as a percentage of total counties that use them") +
  scale_x_continuous(name = "Swing percentage from 2012 to 2016 election - negative is towards\n
                     Democrats, positive is towards republicans") +
  scale_colour_discrete(name = "Primary machine vendor")
county.swing.perc.vs.machines.point


correlation(county.summary.2016.final$swing.perc,
            county.summary.2016.final$use.machines.prop)
# cor -0.712513 : high correlation

correlation(county.summary.2016.final$turnout.perc,
            county.summary.2016.final$use.machines.prop)
# cor 0.3324792 : not highly correlated

# absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
# absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
# absolute.2016.2012.compare


