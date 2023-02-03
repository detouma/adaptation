##### To understand climate change adaptation we must characterize climate variability. Here’s how. #####
### Anne Pisor, Danielle Touma, Deepti Singh, James Holland Jones ###


library(foreign) # Bring in .dta data
library(tidyverse) # Various functions useful for data processing

setwd() # Set working directory


##### Download data #####
# Download from https://microdata.worldbank.org/index.php/catalog/95


##### Import data #####

b0 <- as_tibble(read.dta("Burkina Faso/f0w.dta"))
b5 <- as_tibble(read.dta("Burkina Faso/f5iw.dta"))
b6 <- as_tibble(read.dta("Burkina Faso/f6iw.dta"))


##### Subset #####

b0a <- b0 %>%
  select(men, prov, vill) %>% #Select household, province, and village.
  mutate_at(vars(prov, vill), ~ str_to_lower(.)) %>% #Make all names lowercase for ease of merging.
  distinct(men, .keep_all = T) #One row per household in case duplicates.

b5a <- b5 %>%
  select(men, s56, s57, s519, s521) # These questions were posed to households with one or more household members that are currently elsewhere, whether in Burkina or abroad. 56: why live outside HH? 57: where live. 519: 0/1 send money to HH last 12 months. 522: 0/1 send or bring food to HH.

b6a <- b6 %>%
  select(men, s62, s66, s610) # These are the households that have social network connections from outside their household that are currently elsewhere, whether in Burkina or abroad. 62: where live? 66: pres/abs of money, 610: 0/1 receive goods.

bur1 <- bind_rows(b5a, b6a) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere

bur1a <- full_join(b0a, bur1, by = "men") # Combine basic household data with data on presence/absence of remittances, keeping all rows.

bur1a <- bur1a %>%
  group_by(men) %>% #Group by household
  rename(house = men, loc_meso = prov, loc_micro = vill, curr_why = s56, curr_where = s57, curr_money = s519, curr_goods = s521, nothh_where = s62, nothh_money = s66, nothh_goods = s610) #curr variables are for current HH members who are elsewhere; nothh variables are for non-HH members who are elsewhere


##### Clean and recode #####
bur1a <- droplevels(bur1a) # Dropping levels to improve behavior of factors

bur1a$curr_money <- case_when( # Data are continuous (number of times money sent) but very sparse (few received money more than 3 times) and don't specify amounts (amount could be huge and only sent once); given these uncertainties, we code all to presence/absence.
  bur1a$curr_money > 0 ~ TRUE,
  bur1a$curr_money == 0 ~ FALSE)

bur1a$curr_goods <- case_when(
  bur1a$curr_goods == 1 ~ TRUE,
  bur1a$curr_goods == 2 ~ FALSE,
  bur1a$curr_goods == 0 ~ NA
)

bur1a$nothh_money <- case_when(
  bur1a$nothh_money == "yes" ~ TRUE,
  bur1a$nothh_money == "no" ~ FALSE
)

bur1a$nothh_goods <- case_when(
  bur1a$nothh_goods == "yes" ~ TRUE,
  bur1a$nothh_goods == "no" ~ FALSE
)


##### Merge with location data #####

loc_b <- read_csv("Burkina Faso/burkina locations_meso.csv") # Author AP coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances

loc_b <- mutate(loc_b, landmark = NULL, notes = NULL) # Remove AP's notes about how she got lat/long for each

bur1a <- left_join(bur1a, loc_b, by = c("loc_meso" = "location")) # Merge province location with household data


##### Make binary for whether any remittances received #####
# This is combining across source (same-HH, non-HH) and type (money or goods)

bur1b <- bur1a %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

bur1b$cts <- rowSums(bur1b[ , 2:5], na.rm = T) # Since these are all binary, it's okay to sum

baggs <- aggregate(bur1b$cts, by = list(bur1b$house),function(x){ifelse(sum (x) > 0, 1, 0)}) # Record to binary

baggs <- data.frame(house = baggs$Group.1, remit = baggs$x) # Aggregate creates annoying dataframes (still useful function though!), so rename that messy dataframe

bur1c <- bur1a %>% #Go back to original data before this little sideshow aggregate thing; get rid of all columns except HH ID and location.
  select(house, lat, long) %>% 
  distinct(house, .keep_all = T)

bur2 <- inner_join(bur1c, baggs) #Merge with remittance presence/absence data.


##### Output #####

write_csv(bur2, "remittance data_meso.csv")
