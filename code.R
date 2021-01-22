## This file merges ACLED's data on US protests with BLS data on unemployment,
## Census data on demographic characteristics, and data on voting behavior from MIT.
## Here we're just looking for some quick and dirty descriptive statistics
## about the kinds of communities that were most likely to experience protests in the
## aftermath of the the murder of George Floyd.

## NOTE: I would not rely on any of the models in this R file to draw conclusions.
## We're using count data with many 0s, so OLS is probably not appropriate,
## and (relatedly) the data exhibits serious heteroskedasticity.
## For serious statistical investigation I would re-organize the data as panel data
## (or possibly leave it as count data and use tobit).
## Certainly I'm not equipped to make any causal claims.

## The real point if this file is to demonstrate to ITEP that I can clean and merge messy data from a variety of sources.

# Load package manager and some useful packages
require(pacman)
p_load(tidyverse,openxlsx,tidycensus,usmap)

# read in the ACLED's data
acled <- read.xlsx("~/acled/data/acled.xlsx")

# We want observations at the county level, so FIPS codes will be convenient
# Unfortunately, the ACLED data doesn't include FIPS codes, and some naming conventions are inconsistent
# First standardize "saint" vs. "st."

acled$ADMIN2 <- acled$ADMIN2 %>% str_replace("[Ss]aint","St.")

# now get fips codes using the fips function from usmap
# have to use a for-loop since the fips function isn't vectorized :(
# fips is also finnicky about the inclusion of "parish", "city", etc., which ACLED has dropped
# so we'll take care of that in the for-loop

acled$fips <- NA

for (i in 1:nrow(acled)) {
  
  tryCatch({
  acled$fips[i] <- fips(acled$ADMIN1[i],acled$ADMIN2[i])
  }, error=function(e){})
  
  if (is.na(acled$fips[i])) {
    tryCatch({
      acled$fips[i] <- fips(acled$ADMIN1[i],paste0(acled$ADMIN2[i]," city"))
    }, error=function(e){})
  }

  if (is.na(acled$fips[i])) {
    tryCatch({
      acled$fips[i] <- fips(acled$ADMIN1[i],paste0(acled$ADMIN2[i]," parish"))
    }, error=function(e){})
  }

  if (is.na(acled$fips[i])) {
    tryCatch({
      acled$fips[i] <- fips(acled$ADMIN1[i],paste0(acled$ADMIN2[i]," census area"))
    }, error=function(e){})
  }
  
  if (is.na(acled$fips[i])) {
    tryCatch({
      acled$fips[i] <- fips(acled$ADMIN1[i],paste0(acled$ADMIN2[i]," borough"))
    }, error=function(e){})
  }
  
  if (is.na(acled$fips[i])) {
    tryCatch({
      acled$fips[i] <- fips(acled$ADMIN1[i],paste0(acled$ADMIN2[i]," city and borough"))
    }, error=function(e){})
  }
}

# check to see how many observations fips couldn't categorize
sum(is.na(acled$fips))


# read in county-level unemployment data from the BLS
bls <- read.xlsx("~/acled/data/laucntycur14.xlsx",startRow = 5)

# do some cleaning so that we'll be able to merge the BLS data later
names(bls)[9] <- "unemp"
names(bls)[3] <- "countyfips"
bls$GEOID <- paste0(bls$Code,bls$countyfips)
bls$unemp <- bls$unemp %>%  as.character() %>% as.numeric() 
bls <- bls %>% dplyr::filter(Period %in% c("May-20",   "Jun-20",   "Jul-20",   "Aug-20",   "Sep-20",   "Oct-20 p"))
bls <- bls %>% group_by(GEOID) %>% summarise(unemployment = mean(unemp))

## You'll need a Census API key to use the functions below

# you don't have to run the first line below, but it's a convenient way to see available census variables
vars <- load_variables(year=2018,dataset="acs5")

census_data <- get_acs(geography = "county",
                       year=2018,
                       variables=c(
                         total="B01003_001",
                         median_hh_inc = "B19013_001",
                         hh_size= "B08202_001",
                         housing_cost = "B25105_001",
                         renters = "B07013_003",
                         divorced = "B06008_004",
                         lessHS = "B06009_002",
                         college = "B06009_005",
                         grad = "B06009_006",
                         workers = "B08012_001",
                         white="B02001_002",
                         black="B02001_003",
                         tworaces="B02001_008"), 
                       geometry=F,                
                       shift_geo=F,
                       output="wide")


acled$militia <- ifelse(str_detect(acled$ACTOR1,"[Mm]ilitia|III%|Boogaloo|Patriot Prayer|Oath Keepers"),1,0)

acled2 <- acled %>% group_by(fips) %>% summarise(fatalities = sum(FATALITIES),militias=sum(militia),n=n())

acled2 <- left_join(census_data,acled2,by=c("GEOID"="fips"))

acled2$prop_white <- acled2$whiteE/acled2$totalE
acled2$prop_black <- acled2$blackE/acled2$totalE
acled2$prop_college <- acled2$collegeE/acled2$totalE
acled2$prop_renters <- acled2$rentersE/acled2$totalE
acled2$prop_divorced <- acled2$divorcedE/acled2$totalE
acled2$college_black <- acled2$prop_college * acled2$prop_black
acled2$prop_workers <- acled2$workersE/acled2$totalE

acled2$n <- ifelse(is.na(acled2$n),0,acled2$n)
acled2$militias <- ifelse(is.na(acled2$militias),0,acled2$militias)
acled2$fatalities <- ifelse(is.na(acled2$fatalities),0,acled2$fatalities)

# quick sanity check--is number of events correlated with population?
summary(lm(n ~ totalE,data=acled2))
# looks like that's a yes

# how about the proportion of the population that is black?
summary(lm(n ~ totalE  + prop_black,data=acled2))
# also appears to be a yes

# but what if we control for some other characteristics?
summary(lm(n ~ totalE + prop_black + prop_renters + prop_workers + prop_college + median_hh_incE + housing_costE,data=acled2))
# now it looks like prop_black isn't significant


# lets look at ideological lean, as measured by county-level vote shares in 2016

votes <- read.delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ")
votes <- votes %>% dplyr::filter(year == 2016)

votes$party <- if_else(is.na(votes$party),"other",as.character(votes$party))

votes <- votes %>% dplyr::select(-c("year","version","office","candidate"))

votes <- votes %>% pivot_wider(names_from=party,values_from=c(candidatevotes))

votes$FIPS <- ifelse(str_length(votes$FIPS)==4,paste0(0,votes$FIPS),votes$FIPS)

votes$dem_prop <- votes$democrat / votes$totalvotes
votes$rep_prop <- votes$republican / votes$totalvotes
votes$other_prop <- votes$other/votes$totalvotes

acled2 <- left_join(acled2,votes,by=c("GEOID"="FIPS"))

# let's add dem_prop to our previous regressions
summary(lm(n ~ totalE + dem_prop,data=acled2))
# blue counties saw more protests--no surprise there

summary(lm(n ~ totalE  + prop_black + dem_prop,data=acled2))

summary(lm(n ~ totalE + prop_black + prop_renters + prop_workers + prop_college + median_hh_incE + housing_costE + dem_prop,data=acled2))

# interestingly, after controlling for partisanship, prop_black is significant and *negative*

# finally let's merge the bls data on unemployment over the may-oct 2020 period

acled2 <- left_join(acled2,bls)

summary(lm(n ~ totalE + unemployment,data=acled2))
# one theory was that high unemployment rates had freed up folks' time for other activities, like social justice activism
# and at first that looks plausible

summary(lm(n ~ totalE + dem_prop + prop_black +  unemployment,data=acled2))
# but after controlling for race and partisanship, it looks like high unemployment rates are correlated with fewer demonstrations
# In reality, it's likely that both prop_black and and unemployment have non-linear relationships with the number of demonstrations
# see graph below

ggplot(acled2) +
  geom_point(aes(x=prop_black,y=n)) +
  geom_smooth(aes(x=prop_black,y=n),method="lm",formula = 'y~ x + I(x^2)')
