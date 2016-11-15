setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
library(XML); require(rvest); require(stringr);require(xlsx);require(dplyr);require(plyr)
require(agricolae); require(ggplot2); library(gridExtra); library(tidyr)

# Be careful that rstudio git integration does not work on files with spaces in them...
# http://stackoverflow.com/questions/34105129/using-git-in-r-studio-cannot-stage-modified-code-files

options(scipen=999) # Effectively disables scientific numbering
options(mc.cores = parallel::detectCores()) # This is used for the poll downloads

one.graph.width = 20;one.graph.height = 15

# Function to take rightmost digit
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
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

voting.df.2016 = voting.df.2016.votecand

voting.df.2016$turnout = with(voting.df.2016, dem.votes + rep.votes + oth.votes)

rm(voting.df.2016.test,voting.df.2016.votecand,voting.df.2016.votecand.groups)

# Reading in historical vote data ----------------------------------------------------
## General overview of Wisconsin elections from 1900 onwards
url <- "http://uselectionatlas.org/RESULTS/compare.php?year=2016&fips=55&f=0&off=0&elect=0&type=state"
voting.history <- url %>%
  read_html() %>%
  html_nodes(xpath='//table[@id="data"]') %>%
  html_table(fill=T)
# write.csv(voting.history,"us.election.wisc.history.csv")

df.elections = data.frame(1:44)
df.elections$year = unlist(lapply(voting.history, '[[', 'X3'))
df.elections$year = as.integer(df.elections$year)
df.elections$turnout = unlist(lapply(voting.history, '[[', 'X4'))
df.elections$turnout = as.integer(gsub(",", "", df.elections$turnout))
df.elections$dem.vote.perc = unlist(lapply(voting.history, '[[', 'X10'))
df.elections$dem.vote.perc = as.numeric(gsub("%", "", df.elections$dem.vote.perc))
df.elections$rep.vote.perc = unlist(lapply(voting.history, '[[', 'X11'))
df.elections$rep.vote.perc = as.numeric(gsub("%", "", df.elections$rep.vote.perc))

# Independents and others
df.elections$ind.vote.perc = unlist(lapply(voting.history, '[[', 'X12'))
df.elections$ind.vote.perc = as.numeric(gsub("%", "", df.elections$ind.vote.perc))
df.elections$oth.vote.perc = unlist(lapply(voting.history, '[[', 'X13'))
df.elections$oth.vote.perc = as.numeric(gsub("%", "", df.elections$oth.vote.perc))

df.elections$dem.vote = unlist(lapply(voting.history, '[[', 'X14'))
df.elections$dem.vote = as.integer(gsub(",", "", df.elections$dem.vote))
df.elections$rep.vote = unlist(lapply(voting.history, '[[', 'X15'))
df.elections$rep.vote = as.integer(gsub(",", "", df.elections$rep.vote))

df.elections$ind.vote = unlist(lapply(voting.history, '[[', 'X16'))
df.elections$ind.vote = as.integer(gsub(",", "", df.elections$ind.vote))
df.elections$oth.vote = unlist(lapply(voting.history, '[[', 'X17'))
df.elections$oth.vote = as.integer(gsub(",", "", df.elections$oth.vote))

df.elections$X1.44 = NULL
df.elections = df.elections[-c(1:3), ]

# Total others = independents + others
df.elections$tot.oth.vote.perc = df.elections$ind.vote.perc + df.elections$oth.vote.perc
df.elections$tot.oth.vote = df.elections$ind.vote + df.elections$oth.vote


df.elections$vote.perc.diff = df.elections$rep.vote.perc - df.elections$dem.vote.perc
df.elections$vote.num.diff = df.elections$rep.vote - df.elections$dem.vote
df.elections$winner = with(df.elections, ifelse(vote.perc.diff < 0, "Dem", "Rep"))

setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
# From here: https://www.google.es/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwjChuux5qfQAhWCrxoKHXcKDqYQFggdMAA&url=http%3A%2F%2Felections.wi.gov%2Fsites%2Fdefault%2Ffiles%2Fpage%2Fvoter_turnout_partisan_nonpartisan_xlsx_13632.xlsx&usg=AFQjCNHNxn4e1xNCCjaLXekHxuAKT_dFxg
historical.turnout = read.xlsx(
  file = "voter_turnout_partisan_nonpartisan_xlsx_13632.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(historical.turnout) = tolower(colnames(historical.turnout))
historical.turnout.join = with(historical.turnout, data.frame(year, voting.age.population))
df.elections = join(df.elections, historical.turnout.join, by = "year")
df.elections$turnout.perc = with(df.elections, (turnout / voting.age.population)) * 100


df.elections$swing.turnout.perc = NA
for (i in (2:(length(df.elections$year) - 1))) {
  df.elections$swing.turnout.perc[i - 1] =  -(df.elections$turnout.perc[i] - df.elections$turnout.perc[i -1])
  df.elections$swing.vote.num[i - 1] =  -(df.elections$vote.num.diff[i] - df.elections$vote.num.diff[i - 1])
  df.elections$swing.vote.perc[i - 1] =  -(df.elections$vote.perc.diff[i] - df.elections$vote.perc.diff[i - 1])
  df.elections$swing.vote.dem.change.perc[i - 1] =  (df.elections$dem.vote.perc[i-1] - df.elections$dem.vote.perc[i])
  df.elections$swing.vote.dem.change.num[i - 1] =  (df.elections$dem.vote.num[i-1] - df.elections$dem.vote.num[i])
  df.elections$swing.vote.rep.change.perc[i - 1] =  (df.elections$rep.vote.perc[i-1] - df.elections$rep.vote.perc[i])
  df.elections$swing.vote.rep.change.num[i - 1] =  (df.elections$rep.vote.num[i-1] - df.elections$rep.vote.num[i])
  df.elections$swing.vote.tot.oth.change.perc[i - 1] =  (df.elections$tot.oth.vote.perc[i-1] - df.elections$tot.oth.vote.perc[i])
  df.elections$swing.vote.tot.oth.change.num[i - 1] =  (df.elections$tot.oth.vote.num[i-1] - df.elections$tot.oth.vote.num[i])
}
df.elections.postwar = subset(df.elections, year >= 1948)


historical.election.data = ggplot(df.elections.postwar,
                                  aes(
                                    x = year,
                                    y = swing.vote.perc,
                                    fill = winner,
                                    group = 1
                                  )) +
  geom_bar(stat = "identity") +
  geom_line(
    aes(y = df.elections.postwar$swing.turnout.perc * 100),
    alpha = 0.5,
    stat = "identity",
    colour = "black"
  ) +
  scale_y_continuous(
    name = paste(
      "Swing vote percentage from previous election with time.\n
      Line indicates change in % turnout compared with previous election.",
      sep = "",
      collapse = "\n"
    )
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(
    "text",
    x = mean(df.elections.postwar$year),
    y = min(df.elections.postwar$swing.vote.perc),
    label = "Vote swing towards Democrats",
    vjust = 1,
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = mean(df.elections.postwar$year),
    y = max(df.elections.postwar$swing.vote.perc),
    label = "Vote swing towards Republicans",
    vjust = 1,
    hjust = 0.5
  ) +
  # scale_x_discrete(limit =  df.elections$ordered.county.2016.trump,
  #                  labels = as.character(df.elections$county), name = NULL) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
historical.election.data

historical.election.data.others = ggplot(df.elections.postwar,
                                  aes(
                                    x = year,
                                    y = swing.vote.tot.oth.change.perc,
                                    fill = winner,
                                    group = 1
                                  )) +
  geom_bar(stat = "identity") +
  geom_line(
    aes(y = df.elections.postwar$swing.turnout.perc * 100),
    alpha = 0.5,
    stat = "identity",
    colour = "black"
  ) +
  scale_y_continuous(
    name = paste(
      "Change in other party votes, previous election with time.\n
      Line indicates change in % turnout compared with previous election.",
      sep = "",
      collapse = "\n"
    )
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(limit =  df.elections$ordered.county.2016.trump,
  #                  labels = as.character(df.elections$county), name = NULL) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
historical.election.data.others


# The following variables will be used when I scrape the polls from HuffPost API later
df.elections$mean.rep.poll.oneweek = NA
df.elections$mean.dem.poll.oneweek = NA
df.elections$mean.rep.poll.oneweek.divergence = NA
df.elections$mean.dem.poll.oneweek.divergence = NA

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

setwd("C:\\Users\\s_cas\\Documents\\GitHub\\polls")

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
write.csv(all_polls_2016, "all_polls_2016.csv")

# all_polls_2016 = read.csv("all_polls_2016.csv", stringsAsFactors = FALSE, header = TRUE)

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

df.elections$mean.rep.poll.oneweek[df.elections$year == 2016] = mean(selected_polls_2016.sub$trump)
df.elections$mean.dem.poll.oneweek[df.elections$year == 2016] = mean(selected_polls_2016.sub$clinton)
df.elections$mean.oth.poll.oneweek[df.elections$year == 2016] = mean(selected_polls_2016.sub$other,na.rm = T)
df.elections$mean.und.poll.oneweek[df.elections$year == 2016] = mean(selected_polls_2016.sub$undecided,na.rm = T)

df.elections$mean.rep.poll.oneweek.divergence[df.elections$year == 2016] =
  df.elections$rep.vote.perc[df.elections$year == 2016] - df.elections$mean.rep.poll.oneweek[df.elections$year == 2016]
df.elections$mean.dem.poll.oneweek.divergence[df.elections$year == 2016] =
  df.elections$dem.vote.perc[df.elections$year == 2016] - df.elections$mean.dem.poll.oneweek[df.elections$year == 2016]
df.elections$mean.other.poll.oneweek.divergence[df.elections$year == 2016] =
  df.elections$tot.oth.vote.perc[df.elections$year == 2016] - df.elections$mean.oth.poll.oneweek[df.elections$year == 2016]
df.elections$mean.und.poll.oneweek.divergence[df.elections$year == 2016] =
  df.elections$tot.und.vote.perc[df.elections$year == 2016] - df.elections$mean.und.poll.oneweek[df.elections$year == 2016]

### Removing unsure voters from polls and reanalysing
df.elections$mean.rep.poll.oneweek.defs[df.elections$year == 2016] = mean(selected_polls_2016.sub$trump.defs,na.rm = T)
df.elections$mean.dem.poll.oneweek.defs[df.elections$year == 2016] = mean(selected_polls_2016.sub$clinton.defs,na.rm = T)
df.elections$mean.oth.poll.oneweek.defs[df.elections$year == 2016] = mean(selected_polls_2016.sub$other.defs,na.rm = T)

df.elections$mean.rep.poll.oneweek.defs.divergence[df.elections$year == 2016] =
  df.elections$rep.vote.perc[df.elections$year == 2016] - df.elections$mean.rep.poll.oneweek.defs[df.elections$year == 2016]
df.elections$mean.dem.poll.oneweek.defs.divergence[df.elections$year == 2016] =
  df.elections$dem.vote.perc[df.elections$year == 2016] - df.elections$mean.dem.poll.oneweek.defs[df.elections$year == 2016]
df.elections$mean.other.defs.poll.oneweek.defs.divergence[df.elections$year == 2016] =
  df.elections$tot.oth.vote.perc[df.elections$year == 2016] - df.elections$mean.oth.poll.oneweek.defs[df.elections$year == 2016]

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

write.csv(all_polls_2016, "all_polls_2016.csv")
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

# all_polls_2012 = read.csv("all_polls_2012.csv", stringsAsFactors = FALSE, header = TRUE)

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

df.elections$mean.rep.poll.oneweek[df.elections$year == 2012] = mean(selected_polls_2012.sub$romney)
df.elections$mean.dem.poll.oneweek[df.elections$year == 2012] = mean(selected_polls_2012.sub$obama)
df.elections$mean.oth.poll.oneweek[df.elections$year == 2012] = mean(selected_polls_2012.sub$other, na.rm = T)
df.elections$mean.und.poll.oneweek[df.elections$year == 2012] = mean(selected_polls_2012.sub$undecided,na.rm = T)

df.elections$mean.rep.poll.oneweek.divergence[df.elections$year == 2012] =
  df.elections$rep.vote.perc[df.elections$year == 2012] - df.elections$mean.rep.poll.oneweek[df.elections$year == 2012]
df.elections$mean.dem.poll.oneweek.divergence[df.elections$year == 2012] =
  df.elections$dem.vote.perc[df.elections$year == 2012] - df.elections$mean.dem.poll.oneweek[df.elections$year == 2012]
df.elections$mean.other.poll.oneweek.divergence[df.elections$year == 2012] =
  df.elections$tot.oth.vote.perc[df.elections$year == 2012] - df.elections$mean.oth.poll.oneweek[df.elections$year == 2012]
df.elections$mean.other.poll.oneweek.divergence[df.elections$year == 2012] =
  df.elections$tot.oth.vote.perc[df.elections$year == 2012] - df.elections$mean.oth.poll.oneweek[df.elections$year == 2012]

###
df.elections$mean.rep.poll.oneweek.defs[df.elections$year == 2012] = mean(selected_polls_2012.sub$romney.defs,na.rm = T)
df.elections$mean.dem.poll.oneweek.defs[df.elections$year == 2012] = mean(selected_polls_2012.sub$obama.defs,na.rm = T)
df.elections$mean.oth.poll.oneweek.defs[df.elections$year == 2012] = mean(selected_polls_2012.sub$other.defs,na.rm = T)

df.elections$mean.rep.poll.oneweek.defs.divergence[df.elections$year == 2012] =
  df.elections$rep.vote.perc[df.elections$year == 2012] - df.elections$mean.rep.poll.oneweek.defs[df.elections$year == 2012]
df.elections$mean.dem.poll.oneweek.defs.divergence[df.elections$year == 2012] =
  df.elections$dem.vote.perc[df.elections$year == 2012] - df.elections$mean.dem.poll.oneweek.defs[df.elections$year == 2012]
df.elections$mean.other.defs.poll.oneweek.defs.divergence[df.elections$year == 2012] =
  df.elections$tot.oth.vote.perc[df.elections$year == 2012] - df.elections$mean.oth.poll.oneweek.defs[df.elections$year == 2012]


# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2012 = ggplot(selected_polls_2012.sub, aes(x = start.date, y = romney)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2012.sub$obama), colour = "blue") +
geom_jitter(aes(y = selected_polls_2012.sub$other), colour = "grey") +
  geom_jitter(aes(y = selected_polls_2012.sub$und), colour = "orange") +
  scale_y_continuous(limits = c(0,55))
polls.wisc.point.2012
write.csv(all_polls_2012, "all_polls_2012.csv")
} # Getting the 2012 poll data


# Comparing 2016 candidate results overall -------------------------------------
# Overall results vs polls
df.elections.summary.2016 = subset(df.elections, year == 2016)

df.elections.2016.group = group_by(voting.df.2016, cand.group)
df.elections.2016.grouped = dplyr::summarise(df.elections.2016.group,
                                   votes.rec = sum(votes.rec))

df.elections.2016.grouped$votes.perc = (df.elections.2016.grouped$votes.rec / sum(df.elections.2016.grouped$votes.rec)) * 100

df.elections.2016.grouped$ordered.cands= c(2,3,1)
df.elections.2016.grouped = df.elections.2016.grouped[order(df.elections.2016.grouped$ordered.cands), ]

df.elections.2016.grouped.vis.defs = ggplot(df.elections.2016.grouped,
                                       aes(x = ordered.cands, y = votes.perc, fill = cand.group,group =1)) +
  geom_bar(stat = "identity") +
  geom_line(stat = "identity",
            aes(y = df.elections.summary.2016$mean.dem.poll.oneweek.defs),
            colour = "blue", linetype = 3,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = df.elections.summary.2016$mean.rep.poll.oneweek.defs),
            colour = "red", linetype = 3, size = 1,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = df.elections.summary.2016$mean.oth.poll.oneweek.defs),
            colour = "black", linetype = 3,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = df.elections.summary.2016$mean.dem.poll.oneweek),
            colour = "blue",
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = df.elections.summary.2016$mean.rep.poll.oneweek),
            colour = "red", size = 1,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = df.elections.summary.2016$mean.oth.poll.oneweek),
            colour = "black",
            show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_x_discrete(
    limit =  df.elections.2016.grouped$ordered.cands,
    labels = as.character(df.elections.2016.grouped$cand.group),
    name = NULL) +
  scale_fill_manual(values = c(
    "light blue",
    "grey",
    "lightcoral"
  ))
df.elections.2016.grouped.vis.defs

## Comparing the definite poll voters with the final result
df.elections.2016.grouped$polls.nov = with(df.elections.summary.2016, c(mean.rep.poll.oneweek, mean.dem.poll.oneweek, mean.oth.poll.oneweek))
df.elections.2016.grouped$polls.nov.def = with(df.elections.summary.2016, c(mean.rep.poll.oneweek.defs, mean.dem.poll.oneweek.defs, mean.oth.poll.oneweek.defs))

df.elections.2016.grouped$diff.from.poll = df.elections.2016.grouped$votes.perc - df.elections.2016.grouped$polls.nov
df.elections.2016.grouped$diff.from.poll.defs = df.elections.2016.grouped$votes.perc - df.elections.2016.grouped$polls.nov.def

# The undecideds went to Trump
poll.res.compare.vis = ggplot(df.elections.2016.grouped, aes(x = cand.group, y = diff.from.poll.defs, fill = cand.group, group = 1)) +
  # geom_line(stat = "identity",show.legend = F, aes(y = df.elections.summary.2016$polls.nov)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_x_discrete(
     limit =  df.elections.2016.grouped$cand.group,
     labels = as.character(df.elections.2016.grouped$cand.group),
     name = NULL) +
  scale_fill_manual(values = c(
    "light blue",
    "grey",
    "lightcoral"))
poll.res.compare.vis

compare.2016.results.with.polls.plotnames = c("df.elections.2016.grouped.vis.defs",
                                              "poll.res.compare.vis")
compare.2016.results.with.polls = marrangeGrob(
  grobs = mget(compare.2016.results.with.polls.plotnames),
  nrow = 1,
  ncol = 2,
  top = NULL
)
compare.2016.results.with.polls

# Loading historic county data 2016 to 2000 ----------------------------------------
setwd(
  "C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin\\Historical elections"
)
# http://elections.wi.gov/elections-voting/results/2000/fall-general
# http://elections.wi.gov/elections-voting/results/2004/fall-general
# http://elections.wi.gov/elections-voting/results/2008/fall-general

{ # 2016 data
county.summary.df = voting.df.2016
# colnames(county.summary.df) = "county"
county.summary.df$year = 2016

county.summary.df$perc.diff = county.summary.df$rep.votes.perc - county.summary.df$dem.votes.perc
county.summary.df$num.diff = county.summary.df$rep.votes - county.summary.df$dem.votes

county.summary.df$winner = ifelse(county.summary.df$perc.diff > 0, "Trump", "Clinton")
county.summary.df$winner.party = ifelse(county.summary.df$perc.diff > 0, "Rep", "Dem")

county.summary.df$mean.poll.trump = df.elections$mean.rep.poll.oneweek[df.elections$year == 2016]
county.summary.df$mean.poll.clinton = df.elections$mean.dem.poll.oneweek[df.elections$year == 2016]

county.summary.df$turnout.all = sum(county.summary.df$turnout)
# df.elections$turnout.all = sum(county.summary.df$turnout)

# Calculating turnout by county
# Calculating turnout of registered voters by November 2nd. BUT, this has been seen in past to
# increase by up to 11% on voting day, see 'general_election_voter_turnout...xlsx'. So maybe
# The turnout value here is an over-estimate

## NOTE: Here http://www.wisconsinvote.org/faq it says you can register on the day to vote,
## so maybe this registered to vote value is not accurate?
setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
registered.voters = read.xlsx(
  "registeredvotersbycounty_xlsx_45404MOD.xlsx",
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
# df.elections$reg.voters.all = sum(registered.voters$registered.voters)

county.summary.df$registered.voters = registered.voters$registered.voters
county.summary.df$turnout.perc.reg = (county.summary.df$turnout / county.summary.df$registered.voters) * 100
county.summary.df$turnout.state.reg = (sum(county.summary.df$turnout) / sum(county.summary.df$registered.voters)) * 100
# df.elections$turnout.state.reg = mean(county.summary.df$turnout.state.reg)

## Calculating turnout of voting age population
## I can only estimate the turnout per county based on the 2012 voting age populations
# from file "general_election_voter_registration... this has the 2016 value"
# df.elections$voting.age.pop = 4449170
# df.elections$turnout.overall = sum(df.elections$votes.rec / df.elections$voting.age.pop)

voting.age.people = read.xlsx(
  "2012_presidential_general_election_turnout_xlsx_11916.xlsx",
  sheetIndex = 2,
  stringsAsFactors = F,
  header = T
)
voting.age.people$county = gsub(" COUNTY.*$", "", voting.age.people$County)

total.2012.pop.wi = sum(voting.age.people$Voting.Age.Estimate.2012)

# Estimated 1.01% increase in voting age population from 2012 to 2016. I will multiply the
# 2012 county voter data by 101.01% to try to account for this increase. Yes, this is an assumption,
# but newer data is not yet available!
prop.2016.voting.age.to.2012.est = mean(df.elections$voting.age.pop[df.elections$year == 2016]) / total.2012.pop.wi

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

  setwd(
    "C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin\\Historical elections"
  )
  county.summary.df = county.summary.df[order(county.summary.df$county), ]

    # This data comes from here: http://elections.wi.gov/elections-voting/results/2012/fall-general
  # 2012 county data
  results.2012 = read.xlsx(
    file = "County by County_11.6.12MOD.xls",
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
setwd(
  "C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin\\Historical elections"
)
{ # 2008 county data frame
  election.2008 = read.csv("2008_FallElection_President_WardbyWardMOD.csv",
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
  election.2004 = read.csv("2004_FallElection_President_WardbyWardMOD.csv",stringsAsFactors = F)
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
  election.2000 = read.csv("2000001107_PRES_SORTMOD.csv", stringsAsFactors = F)

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

colnames(election.2008.final) %in% colnames(election.2004.final)

test = rbind(county.summary.2016.final,county.summary.2012.final, election.2008.final)

test = rbind(election.2004.final,election.2000.final)

historical.counties = rbind(county.summary.2016.final,county.summary.2012.final,
                            election.2008.final,election.2004.final,election.2000.final)

# Loading voting machine data ---------------------------------------------
# This file from here: http://elections.wi.gov/elections-voting/voting-equipment/voting-equipment-use
# Voting machine equipment for 13 September 2016
setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")

vot.equip = read.xlsx(
  "voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
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

# Comparing by county 2016 -------------------------------------------------------------
## VISUALISING THE 2016 RESULTS
county.summary.df = county.summary.df[order(county.summary.df$rep.votes.perc), ]
county.summary.df$ordered.county.2016.rep.votes.perc = c(1:length(county.summary.df$rep.votes.perc))

# Visualise absolute percentage results
county.perc.winner = ggplot(
  county.summary.df,
  aes(
    x = county.summary.df$ordered.county.2016.rep.votes.perc,
    y = rep.votes.perc,
    fill = winner
  )
) +
  geom_bar(
    aes(y = county.summary.df$turnout.perc.allage.est ),
    stat = "identity",
    fill = "grey"
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(name =
  paste("Percentage vote for Trump in Wisconsin counties.\nGrey bar behind indicates percentage turnout")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df$ordered.county.2016.rep.votes.perc,
    labels = as.character(county.summary.df$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner


county.summary.df = county.summary.df[order(county.summary.df$turnout.perc.allage.est), ]
county.summary.df$ordered.county.2016.turnout.perc.diff = c(1:length(county.summary.df$turnout.perc.allage.est))

# Visualise county turnout data as percentage
county.perc.turnout.winner = ggplot(
  county.summary.df,
  aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est,
      fill = winner), group = 1
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Estimated turnout in Trump and Clinton won counties.") +
  geom_line(aes(y = df.elections.summary.2016$turnout.perc), linetype = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df$ordered.county.2016.turnout.perc.diff,
    labels = as.character(county.summary.df$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.turnout.winner


## VISUALISING THE 2016 RESULTS
county.summary.df = county.summary.df[order(county.summary.df$perc.diff), ]
county.summary.df$ordered.county.2016.perc.diff = c(1:length(county.summary.df$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(county.summary.df,
                          aes(x = ordered.county.2016.perc.diff, y = perc.diff,
                              fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df$ordered.county.2016.perc.diff,
    labels = as.character(county.summary.df$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.diff

county.summary.df = county.summary.df[order(county.summary.df$turnout), ]
county.summary.df$ordered.county.2016.turnout = c(1:length(county.summary.df$turnout))

# Votes as numbers
county.2016.trump.num = ggplot(county.summary.df,
                               aes(x = ordered.county.2016.turnout, y = rep.votes,
                                   fill = winner)) +
  geom_bar(
    stat = "identity",
    aes(y = county.summary.df$turnout),
    fill = "grey",
    alpha = 0.5
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df$ordered.county.2016.turnout,
    labels = as.character(county.summary.df$county),
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
  county.summary.df = county.summary.df[order(county.summary.df$diff.2012.perc), ]
  county.summary.df$ordered.county.2012.perc.diff = c(1:length(county.summary.df$diff.2012.perc))

county.2012v2016.perc.diff.2016ord = ggplot(county.summary.df,
                                            aes(x = ordered.county.2016.perc.diff, y = diff.2012.perc)) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df$perc.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\nred bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df$ordered.county.2012.perc.diff,
    labels = as.character(county.summary.df$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.perc.diff.2016ord

# As number instead of percentage
county.differences.years.num = ggplot(county.summary.df,
                                      aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df$num.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Number of votes difference between Democrat and Republican parties in 2012 and 2016 by county. Grey bars are 2012 results,\nred bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df$ordered.county.2012.perc.diff,
    labels = as.character(county.summary.df$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.differences.years.num

absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord",
                                         "county.differences.years.num")
absolute.2016.2012.compare = marrangeGrob(
  grobs = mget(absolute.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
absolute.2016.2012.compare
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
                                         alpha = 0.8) +
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
county.2012v2016.swing.num = ggplot(county.summary.2016.final,
                                    aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.2016.final$swing.turnout),
    fill = "grey",
    alpha = 0.8
  ) +
  scale_y_continuous(
    name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\nbelow 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers.",
    breaks = c(seq(-60000,20000,10000))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.county.swing.perc,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.num

swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc",
                                            "county.differences.years.num")
swing.votes.2016.2012.compare = marrangeGrob(
  grobs = mget(swing.votes.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
swing.votes.2016.2012.compare
}

# Difference in those voting other
{
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$oth.change), ]
county.summary.2016.final$ordered.county.oth.change = c(1:length(county.summary.2016.final$oth.change))

# Increase in those voting 'other' between 2012 and 2016
county.2012v2016.change.oth.perc = ggplot(county.summary.2016.final,
                                    aes(x = ordered.county.oth.change, y = oth.change, fill = winner)) +
   geom_bar(stat = "identity") +
  # geom_bar(
  #   stat = "identity",
  #   aes(y = county.summary.2016.final$swing.turnout),
  #   fill = "grey",
  #   alpha = 0.8
  # ) +
  scale_y_continuous(
    name = "Change in percentage voting other by county."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.county.oth.change,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.change.oth.perc
}

# turnout 2016 vs 2012
{
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$swing.turnout.perc), ]
county.summary.2016.final$ordered.turnout.diff = c(1:length(county.summary.2016.final$swing.turnout.perc))

turnout.diff.perc.graph = ggplot(county.summary.2016.final,
                                 aes(x = ordered.turnout.diff, y = swing.turnout.perc,
                                     fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 as percentage.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.turnout.diff,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.perc.graph

# As number of people
turnout.diff.num.graph = ggplot(county.summary.2016.final,
                                aes(x = ordered.turnout.diff, y = swing.turnout, fill = winner)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 in voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.turnout.diff,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.num.graph

swing.turnout.2016.2012.compare.plotnames = c("turnout.diff.perc.graph", "turnout.diff.num.graph")
swing.turnout.2016.2012.compare = marrangeGrob(
  grobs = mget(swing.turnout.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
swing.turnout.2016.2012.compare
}

# Looking at County data through time -------------------------------------

historical.counties$county = factor(historical.counties$county)

county.winners = as.data.frame(table(historical.counties$county[historical.counties$winning.party == "Dem"]))
county.winners = c("county","dem.wins")

historical.counties = join(historical.counties, county.winners, by = "county")
historical.counties$mostly.dem = with(historical.counties, ifelse(dem.wins >= 3, "Mostly Dem",
                                                                  "Mostly Rep"))
historical.counties$dem.2016 = with(historical.counties, ifelse(year == 2016 &
                                                                  winning.party == "Dem",
                                                                "Dem2016", "Rep2016"))
low.turnout.quant = quantile(historical.counties$swing.turnout.perc[historical.counties$year == 2016])[2]
low.turnout.quant = unname(low.turnout.quant)

historical.counties$most.turnout.drop = with(historical.counties, ifelse(swing.turnout.perc < low.turnout.quant,
                                                                              "Large turnout change 2016", "Little turnout change 2016"))

historical.counties.2016 = historical.counties
historical.counties.2016 = subset(historical.counties.2016, !is.na(most.turnout.drop))

historical.counties.quant.join = data.frame(historical.counties.2016$county,historical.counties.2016$most.turnout.drop)
colnames(historical.counties.quant.join) = c("county","most.turnout.drop")

historical.counties = join(historical.counties, historical.counties.quant.join, by = "county")

historical.counties = historical.counties[order(historical.counties$turnout), ]
historical.counties$ordered.turnout = c(1:length(historical.counties$year))

turnout.diff.num.graph = ggplot(historical.counties,
                                aes(x = year, y = turnout, fill = winning.party)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Overall turnout with time in counties per winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.turnout,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.num.graph

turnout.diff.num.graph = ggplot(historical.counties,
                                aes(x = year, y = turnout, fill = winning.party)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Overall turnout with time in counties per winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.turnout,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~mostly.dem)
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.num.graph

turnout.diff.num.turnout.graph = ggplot(historical.counties,
                                aes(x = year, y = turnout, fill = winning.party)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Overall turnout with time in counties per winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.turnout,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~most.turnout.drop)
scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.num.turnout.graph


turnout.diff.num.graph = ggplot(historical.counties,
                                aes(x = year, y = turnout, colour = winning.party,
                                    group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall turnout with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.turnout,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~mostly.dem)
  scale_fill_manual(values = c("lightcoral", "light blue"))
turnout.diff.num.graph


historical.dem.vote.perc.graph = ggplot(historical.counties,
                                aes(x = year, y = dem.vote.perc, colour = winning.party,
                                    group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall dem.vote.perc with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.dem.vote.perc,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~mostly.dem)
scale_fill_manual(values = c("lightcoral", "light blue"))
historical.dem.vote.perc.graph

historical.dem.vote.perc.graph = ggplot(historical.counties,
                                        aes(x = year, y = dem.vote.perc, colour = winning.party,
                                            group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall dem.vote.perc with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.dem.vote.perc,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~most.turnout.drop)
scale_fill_manual(values = c("lightcoral", "light blue"))
historical.dem.vote.perc.graph

### Other votes

historical.oth.vote.perc.graph = ggplot(historical.counties,
                                        aes(x = year, y = oth.vote.perc, colour = winning.party,
                                            group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall oth.vote.perc with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.oth.vote.perc,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~most.turnout.drop)
scale_fill_manual(values = c("lightcoral", "light blue"))
historical.oth.vote.perc.graph

historical.oth.vote.perc.graph = ggplot(historical.counties,
                                        aes(x = year, y = oth.vote.perc, colour = winning.party,
                                            group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall oth.vote.perc with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.oth.vote.perc,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~mostly.dem)
scale_fill_manual(values = c("lightcoral", "light blue"))
historical.oth.vote.perc.graph

# Other numbers
historical.oth.vote.perc.graph = ggplot(historical.counties,
                                        aes(x = year, y = oth.vote, colour = winning.party,
                                            group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall oth.vote.perc with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.oth.vote.perc,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~mostly.dem)
scale_fill_manual(values = c("lightcoral", "light blue"))
historical.oth.vote.perc.graph

historical.oth.vote.perc.graph = ggplot(historical.counties,
                                        aes(x = year, y = oth.vote.perc, colour = winning.party,
                                            group = county)) +
  geom_line(stat = "identity") +
  scale_y_continuous(name = "Overall oth.vote.perc with time by winning party.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(
  #   limit =  historical.counties$ordered.oth.vote.perc,
  #   labels = as.character(historical.counties$year),
  #   name = NULL
  # ) +
  facet_wrap(~all.machines)
scale_fill_manual(values = c("lightcoral", "light blue"))
historical.oth.vote.perc.graph


# Voting machines in counties ---------------------------------------------
# NOW, GRAPHING SWING BY PROPORTION OF ELECTRONIC VOTING MACHINES IN COUNTY
# Make sure to reorder before adding in new data!
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$county), ]
county.summary.2016.final$use.machines.prop = vot.equip.county$use.machines.prop
county.summary.2016.final$machine.most.used = vot.equip.county$machine.most.used

# Now, let's order the counties by proportion of municipalities using voting machines
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$use.machines.prop), ]

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

machine.join = data.frame(county.summary.2016.final$county, county.summary.2016.final$use.machines.prop, county.summary.2016.final$machine.most.used)
colnames(machine.join) = c("county","use.machines.prop","machine.most.used")

str(machine.join$use.machines.prop)

machine.join$all.machines = with(machine.join, ifelse(use.machines.prop > 0.75, "Mostly voting machines",
                                                      "Some or no voting machines"))

historical.counties = join(historical.counties,machine.join,by="county")

historical.counties$machine.most.used = NULL

# absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
# absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
# absolute.2016.2012.compare


# Repeating graphs for counties with 100% machines -----------------------------

## VISUALISING THE 2016 RESULTS
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$trump), ]
county.summary.df.machines$ordered.county.2016.trump = c(1:length(county.summary.df.machines$trump))

# Visualise absolute percentage results
county.perc.winner = ggplot(
  county.summary.df.machines,
  aes(
    x = county.summary.df.machines$ordered.county.2016.trump,
    y = trump,
    fill = winner
  )
) +
  geom_bar(
    aes(y = county.summary.df.machines$turnout.perc.allage.est * 100),
    stat = "identity",
    fill = "grey"
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +

  scale_y_continuous(name = "Percentage vote for Trump in Wisconsin counties.\n
                     Grey bar behind indicates percentage turnout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.2016.trump,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner


county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$turnout.perc.allage.est), ]
county.summary.df.machines$ordered.county.2016.turnout.perc.diff = c(1:length(county.summary.df.machines$turnout.perc.allage.est))

# Visualise county turnout data as percentage 2016
county.perc.turnout.winner = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est *
        100,
      fill = winner)
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Estimated turnout in Trump and Clinton won counties.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.2016.turnout.perc.diff,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.turnout.winner


## VISUALISING THE 2016 RESULTS
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$perc.diff), ]
county.summary.df.machines$ordered.county.2016.perc.diff = c(1:length(county.summary.df.machines$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.2016.perc.diff, y = perc.diff,
      fill = winner)
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.2016.perc.diff,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.diff

county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$turnout), ]
county.summary.df.machines$ordered.county.2016.turnout = c(1:length(county.summary.df.machines$turnout))

# Votes as numbers
county.2016.trump.num = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.2016.turnout, y = trump.num,
      fill = winner)
) +
  geom_bar(
    stat = "identity",
    aes(y = county.summary.df.machines$turnout),
    fill = "grey",
    alpha = 0.5
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.2016.turnout,
    labels = as.character(county.summary.df.machines$county),
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



# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$diff.2012.perc), ]
county.summary.df.machines$ordered.county.2012.perc.diff = c(1:length(county.summary.df.machines$diff.2012.perc))

# Visualise county data 2012 vs 2016 - as percentage
county.2012v2016.perc.diff.2016ord = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.2016.perc.diff, y = diff.2012.perc * 100)
) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df.machines$perc.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.2012.perc.diff,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.perc.diff.2016ord

# As number instead of percentage
county.differences.years.num = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)
) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df.machines$num.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Number of votes difference in 2012 and 2016 by county. Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.2012.perc.diff,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.differences.years.num

absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord",
                                         "county.differences.years.num")
absolute.2016.2012.compare = marrangeGrob(
  grobs = mget(absolute.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
absolute.2016.2012.compare

#### SWING IN VOTES 2012 to 2016
# Visualise
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$swing.perc), ]
county.summary.df.machines$ordered.county.swing.perc = c(1:length(county.summary.df.machines$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)
) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity",
                                         aes(y = (
                                           county.summary.df.machines$swing.turnout.perc * 100
                                         )),
                                         fill = "grey",
                                         alpha = 0.8) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.swing.perc,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.perc

# As number
county.2012v2016.swing.num = ggplot(
  county.summary.df.machines,
  aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)
) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df.machines$swing.turnout),
    fill = "grey",
    alpha = 0.8
  ) +
  scale_y_continuous(
    name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.county.swing.perc,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.num

swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc",
                                            "county.differences.years.num")
swing.votes.2016.2012.compare = marrangeGrob(
  grobs = mget(swing.votes.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
swing.votes.2016.2012.compare


# turnout 2016 vs 2012
county.summary.df.machines = county.summary.df.machines[order(county.summary.df.machines$swing.turnout.perc), ]
county.summary.df.machines$ordered.turnout.diff = c(1:length(county.summary.df.machines$swing.turnout.perc))

turnout.diff.perc.graph = ggplot(
  county.summary.df.machines,
  aes(x = ordered.turnout.diff, y = swing.turnout.perc,
      fill = winner)
) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 as percentage.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.turnout.diff,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.perc.graph

turnout.diff.num.graph = ggplot(
  county.summary.df.machines,
  aes(x = ordered.turnout.diff, y = swing.turnout, fill = winner)
) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 in voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.machines$ordered.turnout.diff,
    labels = as.character(county.summary.df.machines$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.num.graph

swing.turnout.2016.2012.compare.plotnames = c("turnout.diff.perc.graph", "turnout.diff.num.graph")
swing.turnout.2016.2012.compare = marrangeGrob(
  grobs = mget(swing.turnout.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
swing.turnout.2016.2012.compare

# Repeating graphs for counties swing > 20% to Trump -----------------------------

county.summary.df.20.swing = subset(county.summary.df, swing.perc >= 20)

## VISUALISING THE 2016 RESULTS
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$trump), ]
county.summary.df.20.swing$ordered.county.2016.trump = c(1:length(county.summary.df.20.swing$trump))

# Visualise absolute percentage results
county.perc.winner = ggplot(
  county.summary.df.20.swing,
  aes(
    x = county.summary.df.20.swing$ordered.county.2016.trump,
    y = trump,
    fill = winner
  )
) +
  geom_bar(
    aes(y = county.summary.df.20.swing$turnout.perc.allage.est * 100),
    stat = "identity",
    fill = "grey"
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +

  scale_y_continuous(name = "Percentage vote for Trump in Wisconsin counties.\n
                     Grey bar behind indicates percentage turnout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.2016.trump,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner


county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$turnout.perc.allage.est), ]
county.summary.df.20.swing$ordered.county.2016.turnout.perc.diff = c(1:length(county.summary.df.20.swing$turnout.perc.allage.est))

# Visualise county turnout data as percentage 2016
county.perc.turnout.winner = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est *
        100,
      fill = winner)
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Estimated turnout in Trump and Clinton won counties.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.2016.turnout.perc.diff,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.turnout.winner


## VISUALISING THE 2016 RESULTS
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$perc.diff), ]
county.summary.df.20.swing$ordered.county.2016.perc.diff = c(1:length(county.summary.df.20.swing$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.2016.perc.diff, y = perc.diff,
      fill = winner)
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.2016.perc.diff,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.diff

county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$turnout), ]
county.summary.df.20.swing$ordered.county.2016.turnout = c(1:length(county.summary.df.20.swing$turnout))

# Votes as numbers
county.2016.trump.num = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.2016.turnout, y = trump.num,
      fill = winner)
) +
  geom_bar(
    stat = "identity",
    aes(y = county.summary.df.20.swing$turnout),
    fill = "grey",
    alpha = 0.5
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.2016.turnout,
    labels = as.character(county.summary.df.20.swing$county),
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



# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$diff.2012.perc), ]
county.summary.df.20.swing$ordered.county.2012.perc.diff = c(1:length(county.summary.df.20.swing$diff.2012.perc))

# Visualise county data 2012 vs 2016 - as percentage
county.2012v2016.perc.diff.2016ord = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.2016.perc.diff, y = diff.2012.perc * 100)
) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df.20.swing$perc.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.2012.perc.diff,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.perc.diff.2016ord

# As number instead of percentage
county.differences.years.num = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)
) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df.20.swing$num.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Number of votes difference in 2012 and 2016 by county. Grey bars are 2012 results,\n
    red bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.2012.perc.diff,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.differences.years.num

absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord",
                                         "county.differences.years.num")
absolute.2016.2012.compare = marrangeGrob(
  grobs = mget(absolute.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
absolute.2016.2012.compare

#### SWING IN VOTES 2012 to 2016
# Visualise
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$swing.perc), ]
county.summary.df.20.swing$ordered.county.swing.perc = c(1:length(county.summary.df.20.swing$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)
) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity",
                                         aes(y = (
                                           county.summary.df.20.swing$swing.turnout.perc * 100
                                         )),
                                         fill = "grey",
                                         alpha = 0.8) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.swing.perc,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.perc

# As number
county.2012v2016.swing.num = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)
) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.summary.df.20.swing$swing.turnout),
    fill = "grey",
    alpha = 0.8
  ) +
  scale_y_continuous(
    name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\n
    below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.county.swing.perc,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.num

swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc", "county.2012v2016.swing.num")
swing.votes.2016.2012.compare = marrangeGrob(
  grobs = mget(swing.votes.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
swing.votes.2016.2012.compare

# turnout 2016 vs 2012
county.summary.df.20.swing = county.summary.df.20.swing[order(county.summary.df.20.swing$swing.turnout.perc), ]
county.summary.df.20.swing$ordered.turnout.diff = c(1:length(county.summary.df.20.swing$swing.turnout.perc))

turnout.diff.perc.graph = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.turnout.diff, y = swing.turnout.perc,
      fill = winner)
) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 as percentage.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.turnout.diff,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.perc.graph

turnout.diff.num.graph = ggplot(
  county.summary.df.20.swing,
  aes(x = ordered.turnout.diff, y = swing.turnout, fill = winner)
) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Swing in turnout between 2012 and 2016 in voter numbers.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.df.20.swing$ordered.turnout.diff,
    labels = as.character(county.summary.df.20.swing$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
turnout.diff.num.graph

swing.turnout.2016.2012.compare.plotnames = c("turnout.diff.perc.graph", "turnout.diff.num.graph")
swing.turnout.2016.2012.compare = marrangeGrob(
  grobs = mget(swing.turnout.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL
)
swing.turnout.2016.2012.compare

# Including income --------------------------------------------------------

#C:\Users\s_cas\Documents\GitHub\Voting Data
