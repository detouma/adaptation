# Game plan: include at least current HH and non-HH migrants, since it's a time stamp, and descriptions of why HH person moved (e.g., percentage that mentioned better climate)

setwd("C:\\Users\\anne.pisor\\OneDrive - Washington State University (email.wsu.edu)\\Research\\Characterizing climate variability\\Migration HH survey")

# Data considered: HH wealth? (sections 2 and 3), banking (mobile phone trasactions? section 4), migration and remittances (sect 5; 5.6, 5.8, 5.18, 5.21, 5.22, 5.23, 5.31), remittances (6.1, 6.2, 6.6, .9, 6.11, 6.15), more migration (7.1, 7.2, 7.5, 7.6, 7.12, 7.13, 7.18)

# Top choices: 5.6, 5.8, 5.18, 5.23, 6.1, 6.2, 6.11, 7.2, 7.5, 7.6, 7.13, 7.18 

library(foreign)
library(tidyverse)

## KENYA
# qno = HH unique ID, cunit = census unit, district, hhmnum = HH size, date1 = date of interview

k1_6 <- as_tibble(read.dta("Kenya/Household.dta"))
k5 <- as_tibble(read.dta("Kenya/section5.dta"))
k6 <- as_tibble(read.dta("Kenya/section6.dta"))
k7 <- as_tibble(read.dta("Kenya/section7.dta"))

## Subset
# id_ind not used properly so ignored by me

k1a <- k1_6 %>%
  select(qno, district, cunit) %>% # Removed date because Burkina didn't include.
  mutate_at(vars(district, cunit), ~ str_to_lower(.)) %>%
  distinct(qno, .keep_all = T) # Made it unique by household even though Kenyan data should already be unique by household, just because that's what I did with the other countries.

k5a <- k5 %>%
  select(qno, q5_6, q5_8a, q5_19, q5_23) %>% #5_6: why live outside HH? 5_8_11: where live: area. 5_8_a: where live: locality name. 5_19: money transfers in last 12 mos. 5_23: 0/1 send or bring food to HH in last 12 months.
  mutate(q5_8a = str_to_lower(q5_8a))

# Of 1942 HH, only 279 said yes.
k6a <- k6 %>%
  select(qno, q6_2, q6_6, q6_11) #6_1: last 12 mos, receive money or goods from non-HH migrants? (it's already conditioned on this) 6_2: where live? (supposed to list area within country but doesn't!), 6_6: times received money (will recode as pres/abs) in last 12, 6_11: 0/1 receive goods in last 12.

# IMPORTANT: if used, time of last migration (7_3) needs to be during last interview window.
k7a <- k7 %>%
  select(qno, q7_2, q7_3, q7_4, q7_5, q7_6, q7_13, q7_18, q7_19) #7_2: HH member lived where? (supposed to list area within country but doesn't!) 7_3: when left. 7_4: when returned. 7_5: reason why left HH. 7_6: why chose that place. 7_13: sent money while there? 7_18: sent money for which shock? 7_19: goods (don't mention foods but seems to be roughly isometric).

ken1 <- bind_rows(k5a, k6a) # Counterintuitive but every row is a unique person, so don't want to merge.
ken1a <- full_join(k1a, ken1, by = "qno") # Want every household represented, even if they don't have a migrant or receive from non-household migrants; full_join instead of left because don't want to accidentally exclude a household not in the Household file.

ken1a <- ken1a %>%
  group_by(qno) %>%
  add_column(country = "kenya") %>%
  mutate(id = row_number()) %>%
  rename(house = qno, loc_meso = district, loc_micro = cunit, curr_why = q5_6, curr_where = q5_8a, curr_money = q5_19, curr_goods = q5_23, nothh_where = q6_2, nothh_money = q6_6, nothh_goods = q6_11)
                
k7a <- rename(k7a, past_where = q7_2, past_when_arrive = q7_3, past_when_depart = q7_4, past_why_left = q7_5, past_why_there = q7_6, past_money = q7_13, past_money_shock = q7_18, past_goods = q7_19)

# Cleaning
ken1a$curr_goods <- fauxnaif::na_if_in(ken1a$curr_goods, c(0, 6)) #blanks

ken1a <- droplevels(ken1a) #Removes the baggage of unused levels in a factor; Kenya had some, so I used this on all countries just in case.
k7a <- droplevels(k7a)

# Recoding

ken1a$curr_money <- if_else(ken1a$curr_money > 0, TRUE, NA)

ken1a$curr_goods <- recode(ken1a$curr_goods, `1` = TRUE, `2` = FALSE)

ken1a$nothh_where <- recode(ken1a$nothh_where, `1` = "kenya", `2` = "kenya", `3` = "uk", `4` = "tanzania", `5` = "us", `6` = "uganda", `7` = "canada", `8` = "germany", `9` = "australia", `10` = "india", `11` = "netherlands", `12` = "italy", `13` = "south africa", `14` = "dubai", `15` = "france") # codes 16 as NA, as desired (it's blank)

ken1a$nothh_money <- if_else(ken1a$nothh_money > 0, TRUE, NA)

ken1a$nothh_goods <- recode(ken1a$nothh_goods, `yes` = TRUE, `no` = FALSE)

k7a$past_where <- recode(k7a$past_where, `urban area within the country (place)` = "kenya", `rural area within the country (palce)` = "kenya", `a) uk` = "uk", `b) tanzania` = "tanzania", `c) us` = "us", `d) uganda` = "uganda", `e) canada` = "canada", `f) germany` = "germany", `g) australia` = "australia", `h) india` = "india", `j) italy` = "italy", `k) saudi arabia` = "saudi arabia", `japan` = "japan", `philiphines` = "philippines")

k7a$past_money_shock <- recode(k7a$past_money_shock, `1` = "drought", `2` = "flood", `5` = "none") # codes 9 as NA, as desired

ken1a$curr_where <- str_to_lower(ken1a$curr_where) # make sure everything is lowercase

loc_f_k <- read_csv("Kenya/kenya locations_meso.csv")
loc_f_k <- mutate(loc_f_k, landmark = NULL, notes = NULL)

ken1a <- left_join(ken1a, loc_f_k, by = c("loc_meso" = "location"))
#ken1a <- ken1a %>%
#  rename(lat_ego = lat, long_ego = long)

#ken1a <- left_join(ken1a, loc_f_k, by = c("curr_where" = "location"))
#ken1a <- ken1a %>%
#  rename(lat_curr = lat, long_curr = long)


# Create file with ego only
#ken2 <- ken1a %>%
#  select(house, curr_money, curr_goods, nothh_money, nothh_goods) %>%
#  group_modify(if_else(any(.), 1, 0))

#  with_groups(house, ~ any(.))

#  distinct(house, .keep_all = T)
  
  
ken1b <- ken1a %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods) #%>%
  #mutate(counts = rowSums(.[,2:5])) %>%
  #aggregate() # Could not nail the following in tidy

ken1b$cts <- rowSums(ken1b[ , 2:5], na.rm = T)
kaggs <- aggregate(ken1b$cts, by = list(ken1b$house),function(x){ifelse(sum (x) > 0, 1, 0)})
kaggs <- data.frame(house = kaggs$Group.1, remit = kaggs$x)

ken1c <- ken1a %>%
  select(house, country, lat, long) %>%
  distinct(house, .keep_all = T)

ken2 <- inner_join(ken1c, kaggs)


## BURKINA FASO
# Household data built into files for Burkina.

b0 <- as_tibble(read.dta("Burkina Faso/f0w.dta"))
b5 <- as_tibble(read.dta("Burkina Faso/f5iw.dta")) #Migrant ID does appear to be in order.
b6 <- as_tibble(read.dta("Burkina Faso/f6iw.dta")) #Migrant ID does appear to be in order.
b7 <- as_tibble(read.dta("Burkina Faso/f7iw.dta")) #ID does appear to be in order.


## Subset

b0a <- b0 %>%
  select(men, prov, vill) %>%
  mutate_at(vars(prov, vill), ~ str_to_lower(.)) %>%
  distinct(men, .keep_all = T)

b5a <- b5 %>%
  select(men, s56, s57, s519, s521) #56: why live outside HH? 57: where live. 519: 0/1 send money to HH last 12 months. 522: 0/1 send or bring food to HH.

b6a <- b6 %>%
  select(men, s62, s66, s610) #61: last 12 mos, receive money or goods from non-HH migrants? (it's already conditioned on this) 62: where live? 66: pres/abs of money, 610: 0/1 receive goods.

# IMPORTANT: if used, time of last migration (73) needs to be during last interview window.
b7a <- b7[b7$s71 == "yes", ]
b7b <- b7a %>%
  select(men, prov, vill, s72, s73a, s74a, s75, s76, s713, s7161, s717) #72: HH member lived where? 7_3: year arrived. 7_4: year left. 7_5: reason why left HH. 7_6: why chose that place. 713: sent money while there? 716: sent money for drought (So few people for anything higher than 7161 (flood, earthquake, cyclone, other) not worth it.) 717: goods.

bur1 <- bind_rows(b5a, b6a)
bur1a <- full_join(b0a, bur1, by = "men")

bur1a <- bur1a %>%
  group_by(men) %>%
  add_column(country = "burkina") %>%
  mutate(id = row_number()) %>%
  rename(house = men, loc_meso = prov, loc_micro = vill, curr_why = s56, curr_where = s57, curr_money = s519, curr_goods = s521, nothh_where = s62, nothh_money = s66, nothh_goods = s610)

b7b <- rename(b7b, past_where = s72, past_when_arrive = s73a, past_when_depart = s74a, past_why_left = s75, past_why_there = s76, past_money = s713, past_money_drought = s7161, past_goods = s717)

# Cleaning
bur1a <- droplevels(bur1a)
b7b <- droplevels(b7b)

# Recoding

bur1a$curr_where <- recode(bur1a$curr_where, `Milieu urbain Burkina` = "burkina", `Milieu rural Burkina` = "burkina", `Côte d'Ivoire` = "cote divoire", `Mali`= "mali", `Niger` = "niger", `Ghana` = "ghana", `Togo` = "togo", `Bénin` = "benin", `Nigeria` = "nigeria", `Gabon` = "gabon", `Libye` = "libya", `Italie` = "italy", `France` = "france", `Allemagne` = "germany", `Suisse` = "switzerland", `Autres pays africains` = "other africa", `Autres` = "other")

bur1a$curr_money <- case_when(
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

bur1a$nothh_where <- recode(bur1a$nothh_where, `Urban Burkina` = "burkina", `Rural Burkina` = "burkina", `Cote d'Ivoire` = "cote divoire", `Mali` = "mali", `Ghana` = "ghana", `Togo` = "togo", `Benin` = "benin", `Nigeria` = "nigeria", `Gabon` = "gabon", `Libya` = "libya", `Italia` = "italy", `France` = "france", `Germany` = "germany", `usa` = "usa", `Other African country` = "other africa", `Other` = "other")


# Burkina locations time!

loc_b <- read_csv("Burkina Faso/burkina locations_meso.csv")
loc_b <- mutate(loc_b, landmark = NULL, notes = NULL)

bur1a <- left_join(bur1a, loc_b, by = c("loc_meso" = "location"))


bur1b <- bur1a %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

bur1b$cts <- rowSums(bur1b[ , 2:5], na.rm = T)

baggs <- aggregate(bur1b$cts, by = list(bur1b$house),function(x){ifelse(sum (x) > 0, 1, 0)})

baggs <- data.frame(house = baggs$Group.1, remit = baggs$x)

bur1c <- bur1a %>%
  select(house, country, lat, long) %>%
  distinct(house, .keep_all = T)

bur2 <- inner_join(bur1c, baggs)

## NIGERIA

n1 <- as_tibble(read.dta("Nigeria/consolidated.dta"))

n5 <- as_tibble(read.dta("Nigeria/migrants 2.dta"))

n6 <- as_tibble(read.dta("Nigeria/non hh member migrants 2.dta"))

n7 <- as_tibble(read.dta("Nigeria/individuals 5.dta"))

## Subset

n1a <- n1 %>%
  select(hhno, censusunit, state) %>% # Wait until below for distinct; hhno is unique to town not dataset. #Swapped to state on 9-9 for more comparability to other countries.
  rename("loc_micro" = `censusunit`, "loc_meso" = `state`) %>%
  mutate_at(vars(loc_micro, loc_meso), ~ str_to_lower(.))

n5a <- n5 %>%
  select(HHNo, CensusUnit, Reasonforleaving, Migrantin, Howmanytimes, Sendfoodgoods) %>%
  rename("hhno" = `HHNo`, "loc_micro" = `CensusUnit`, curr_why = Reasonforleaving, curr_where = Migrantin, "curr_money" = `Howmanytimes`, "curr_goods" = `Sendfoodgoods`) %>%
  mutate(loc_micro = str_to_lower(loc_micro))

n6a <- n6 %>%
  select(HHNo, Censusunit, Where, Howmanytimes, Foodgoods) %>%
  rename("hhno" = `HHNo`, "loc_micro" = `Censusunit`, "nothh_money" = `Howmanytimes`, nothh_where = Where, nothh_goods = Foodgoods) %>%
  mutate(loc_micro = str_to_lower(loc_micro))

# IMPORTANT: if used, time of last migration (73) needs to be during last interview window.
n7a <- n7[n7$returnmigrant == "Yes" & !is.na(n7$returnmigrant), ]
n7b <- n7a %>%
  select(hhno, censusunit, where, whenmove, whenreturn, reasonformigration, reasonforplace, sendmoney, sendmoneyforexpense, sendgoods)

# HH IDs are unique to census unit, not to the dataset.
# Make unique for the combo of HHno and loc_micro.

n1b <- n1a %>%
  distinct(hhno, loc_micro, .keep_all = T) %>%
  arrange(loc_micro, hhno) %>%
  mutate(house = seq(1:nrow(.)))

# Merge and tidy up

nga1 <- bind_rows(n5a, n6a)

nga1a <- full_join(n1b, nga1, by = c("hhno", "loc_micro"))

nga1a <- nga1a %>% mutate(hhno = NULL)

nga1a <- nga1a %>% relocate(house) # Move house to first position

nga1a <- nga1a %>%
  add_column(country = "nigeria") %>%
  mutate(id = row_number())

n7b <- rename(n7b, past_where = where, past_when_arrive = whenmove, past_when_depart = whenreturn, past_why_left = reasonformigration, past_why_there = reasonforplace, past_money = sendmoney, past_money_shock = sendmoneyforexpense, past_goods = sendgoods)

# Cleaning

nga1a <- droplevels(nga1a)
n7b <- droplevels(n7b)

# Recoding
nga1a$curr_where <- recode(nga1a$curr_where, `Urban area within Nigeria` = "nigeria", `Rural area within Nigeria` = "nigeria", `Cote d'Ivoire` = "cote divoire", `Mali`= "mali", `Ghana` = "ghana", `Togo` = "togo", `Benin` = "benin", `Italy` = "italy", `France` = "france", `Germany` = "germany", `usa` = "us", `Canada` = "canada", `Holland` = "netherlands", `Spain` = "spain", `Belgium` = "belgium", `South Africa` = "south africa", `Senegal` = "senegal", `Other Africa` = "other africa", `Other` = "other")

nga1a$curr_money <- case_when(
  nga1a$curr_money > 0 ~ TRUE,
  nga1a$curr_money == 0 ~ FALSE)

nga1a$curr_goods <- case_when(
  nga1a$curr_goods == "Yes" ~ TRUE,
  nga1a$curr_goods == "No" ~ FALSE
)

nga1a$nothh_money <- case_when(
  nga1a$nothh_money > 0 ~ TRUE,
  nga1a$nothh_money == 0 ~ FALSE)

nga1a$nothh_goods <- case_when(
  nga1a$nothh_goods == "Yes" ~ TRUE,
  nga1a$nothh_goods == "No" ~ FALSE
)

nga1a$nothh_where <- recode(nga1a$nothh_where, `Urban area within Nigeria` = "nigeria", `Rural area within Nigeria` = "nigeria", `Mali`= "mali", `Ghana` = "ghana", `Togo` = "togo", `Benin` = "benin", `Germany` = "germany", `usa` = "us", `Canada` = "canada", `Holland` = "netherlands", `Spain` = "spain", `South Africa` = "south africa", `Other Africa` = "other africa", `Other` = "other")

# Ready for locations!

loc_n <- read_csv("Nigeria/nigeria locations_meso.csv")
loc_n <- mutate(loc_n, landmark = NULL, notes = NULL)

nga1a <- left_join(nga1a, loc_n, by = c("loc_meso" = "location"))


nga1b <- nga1a %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

nga1b$cts <- rowSums(nga1b[ , 2:5], na.rm = T)

naggs <- aggregate(nga1b$cts, by = list(nga1b$house),function(x){ifelse(sum (x) > 0, 1, 0)})

naggs <- data.frame(house = naggs$Group.1, remit = naggs$x)

nga1c <- nga1a %>%
  select(house, country, lat, long) %>%
  distinct(house, .keep_all = T)

nga2 <- inner_join(nga1c, naggs)


### Senegal

s1 <- as_tibble(read.dta("Senegal/base_menage_21avril2011.dta"))
s5 <- as_tibble(read.dta("Senegal/base_anciens_membres_du_menage_final_21_avril11.dta"))
s6 <- as_tibble(read.dta("Senegal/bases_non_anciens_membres_du_menage_21 avril2011.dta"))
s7 <- as_tibble(read.dta("Senegal/base_individu_21_avril_2011.dta"))

## Subset

s1a <- s1 %>%
  select(numqest, q02, q05) %>%
  mutate_at(vars(q02, q05), ~ str_to_lower(.)) %>%
  distinct(numqest, .keep_all = T) # Doesn't eliminate anyone here; just being careful. Switched from district to department on 9-11 to match other countries.

s5a <- s5 %>%
  select(numqest, q56, q57, q57_27, q57_28, q519, q521) #02: meso. 05: micro. 5_6 why. 5_7: where live. 57_27: where live if "Africa other." 57_28: where live if "other." 5_19: money transfers in last 12 mos. 5_21: 0/1 send or bring food to HH. 

s6a <- s6 %>%
  select(numqest, q62, q62_27, q66, q610)
#6_1: last 12 mos, receive money or goods from non-HH migrants? (it's already conditioned on this) 6_2: where live? (supposed to list area within country but doesn't!), 6_6: times received money (will recode as pres/abs), 6_10: 0/1 receive goods.

s7a <- s7[s7$q71 == "oui", ]
s7b <- s7a %>%
  select(numqest, q01, q05, q72, q73a, q74a, q75, q76, q713, q716_1, q716_5, q717) #72: HH member lived where? 7_3: year arrived. 7_4: year left. 7_5: reason why left HH. 7_6: why chose that place. 713: sent money while there? 716: sent money for drought (_1) and other (_5). 717: goods.

# 57_27 and 57_28 can fill in "other" categories for 57, and 62_27 for 62. 

s5a$q57 <- as.character(s5a$q57)

s5a$q57[s5a$q57=="autres pays africains" & !is.na(s5a$q57)] <- ifelse(s5a$q57_27[s5a$q57=="autres pays africains" & !is.na(s5a$q57)]!="",
      s5a$q57_27[s5a$q57=="autres pays africains" & !is.na(s5a$q57)],
      s5a$q57[s5a$q57=="autres pays africains" & !is.na(s5a$q57)])

s5a$q57[s5a$q57=="autres" & !is.na(s5a$q57)] <- ifelse(s5a$q57_28[s5a$q57=="autres" & !is.na(s5a$q57)]!="",
      s5a$q57_28[s5a$q57=="autres" & !is.na(s5a$q57)], 
      s5a$q57[s5a$q57=="autres" & !is.na(s5a$q57)])

s5a <- s5a %>%
  select(-q57_27, -q57_28)

s6a$q62 <- as.character(s6a$q62)

s6a$q62[s6a$q62=="autres pays africains" & !is.na(s6a$q62)] <- ifelse(s6a$q62_27[s6a$q62=="autres pays africains" & !is.na(s6a$q62)]!="",
      s6a$q62_27[s6a$q62=="autres pays africains" & !is.na(s6a$q62)],
      s6a$q62[s6a$q62=="autres pays africains" & !is.na(s6a$q62)])

s6a <- s6a %>%
  select(-q62_27)

sen1 <- bind_rows(s5a, s6a)
sen1a <- full_join(s1a, sen1, by = "numqest")

sen1a <- sen1a %>%
  group_by(numqest) %>%
  arrange(.by_group = TRUE) %>% #unlike the others, senegal had houses out of order in data; this is needed
  add_column(country = "senegal") %>%
  mutate(id = row_number()) %>%
  rename(house = numqest, loc_meso = q02, loc_micro = q05, curr_why = q56, curr_where = q57, curr_money = q519, curr_goods = q521, nothh_where = q62, nothh_money = q66, nothh_goods = q610)

s7b <- rename(s7b, house = numqest, loc_meso = q01, loc_micro = q05, past_where = q72, past_when_arrive = q73a, past_when_depart = q74a, past_why_left = q75, past_why_there = q76, past_money = q713, past_money_drought = q716_1, past_money_other = q716_5, past_goods = q717)

# Cleaning

sen1a <- droplevels(sen1a)
s7b <- droplevels(s7b)

# Recoding
sen1a$curr_where <- recode(sen1a$curr_where, `AFR SUD` = "south africa", `AFRIQUE DU`= "south africa", `afrique du sud`= "south africa", `Rural area within Nigeria` = "nigeria", `Cote d'Ivoire` = "cote divoire", `Mali`= "mali", `Ghana` = "ghana", `Togo` = "togo", `Benin` = "benin", `Italy` = "italy", `France` = "france", `Germany` = "allemangne", `usa` = "us", `Canada` = "canada", `Holland` = "netherlands", `Spain` = "spain", `Belgium` = "belgium", `Senegal` = "senegal", `Other Africa` = "other africa", `Other` = "other")

sen1a$curr_money <- case_when(
  sen1a$curr_money > 0 ~ TRUE,
  sen1a$curr_money == 0 ~ FALSE)

sen1a$curr_goods <- recode(sen1a$curr_goods, `oui` = TRUE, `non` = FALSE)

sen1a$nothh_where <- recode(sen1a$nothh_where, `Urban area within Nigeria` = "nigeria", `Rural area within Nigeria` = "nigeria", `Mali`= "mali", `Ghana` = "ghana", `Togo` = "togo", `Benin` = "benin", `Germany` = "germany", `usa` = "us", `Canada` = "canada", `Holland` = "netherlands", `Spain` = "spain", `South Africa` = "south africa", `Other Africa` = "other africa", `Other` = "other")

sen1a$nothh_money <- recode(sen1a$nothh_money, `oui` = TRUE, `non` = FALSE)

sen1a$nothh_goods <- recode(sen1a$nothh_goods, `oui` = TRUE, `non` = FALSE)

# Ready for locations!

loc_s <- read_csv("Senegal/senegal locations_meso.csv")
loc_s <- mutate(loc_s, landmark = NULL, notes = NULL)

sen1a <- left_join(sen1a, loc_s, by = c("loc_meso" = "location"))

sen1b <- sen1a %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

sen1b$cts <- rowSums(sen1b[ , 2:5], na.rm = T)
saggs <- aggregate(sen1b$cts, by = list(sen1b$house),function(x){ifelse(sum (x) > 0, 1, 0)})
saggs <- data.frame(house = saggs$Group.1, remit = saggs$x)

sen1c <- sen1a %>%
  select(house, country, lat, long) %>%
  distinct(house, .keep_all = T)

sen2 <- inner_join(sen1c, saggs)


### South Africa
# For here and Uganda, did it after we decided to focus only on ego, so very stripped down comparatively

## Match enumeration area to geo location I can find on Google Maps (as I can't find key with enumeration lat/long)
# Most do not include main place (see details on enumeration areas below), so match to province + municipality/district (first 3 digits) instead (although more fine-grained ones are available here: https://www.statssa.gov.za/?page_id=4503)

ea_codes <- read_csv("South Africa/Enumeration area key.csv", show_col_types = FALSE) %>%
  select(MP_CODE, MN_NAME, DC_NAME) %>%
  rename(ea = MP_CODE, loc_micro = MN_NAME, loc_meso = DC_NAME) %>% #If an error is thrown when running this pipe, run up to this line, then re-run the whole thing. Switched from province to district municipality name on 9-9 for comparability with other locations.
  mutate(ea = as.numeric(str_sub(ea_codes$ea, 1, 3))) %>%
  distinct(ea, .keep_all = T)

z_total <- as_tibble(read.dta("South Africa/wbsasection 1-5 & 9.dta"))
z_curr <- as_tibble(read.dta("South Africa/wbsasection 6.dta"))
z_nothh <- as_tibble(read.dta("South Africa/wbsasection 7.dta"))

# Details on enumeration: position 1 is province (6 = NW, 7 = Gauteng, 9 = Limpopo; 88 is cross-provincial (CBDC8 (Gauteng and NW; West Rand cross-boundary))); position 2-3 is district or municipality code (01-43; again, 88 is CBDC8) OR metro if it starts with 7 (e.g., positions 1-3 are 776 for Pretoria Gauteng; 676 is Pretoria NW); positions 4-5 are unique "main place" in municipality and 6-8 are unique "sub-place" in municipality (see page 8 of https://www.statssa.gov.za/census/census_2001/metadata/Geography.pdf).

## Current HH members
#ID (unique to EA), ea = enumeration area, Q6_45 = money in last 12, Q6_56A-L = goods in last 12 (any = 1, else = 0)

z_total1 <- z_total %>%
  select(id, ea) %>%
  mutate(ea = as.numeric(str_sub(z_total$ea, 1, 3))) %>%
  distinct(id, .keep_all = T)

z_curr1 <- z_curr %>%
  select(id, Q6_45, starts_with("Q6_56"))

# Not HH members
#ID, Q7_8 = times received money in last 12, Q7_10 = goods in last 12

z_nothh1 <- z_nothh %>%
  select(id, Q7_8, Q7_10)

# Get calc out of the way: if anything listed for curr_goods across all these columns, give a TRUE.

z_curr1 <-  mutate(z_curr1, curr_goods = ifelse(if_any(starts_with("Q6_56"), ~!is.na(.x)), TRUE, NA))

# Merge

za <- bind_rows(z_curr1, z_nothh1) %>%
  select(-starts_with("Q6_56")) %>%
  group_by(id)

za1 <- full_join(z_total1, za, by = "id")

za2 <- left_join(za1, ea_codes, by = "ea") %>%
  select(-ea) %>%
  group_by(id) %>%
  rename(house = id, curr_money = Q6_45, nothh_money = Q7_8, nothh_goods = Q7_10) %>%
  mutate_at(vars(loc_micro, loc_meso, nothh_goods), ~ str_to_lower(.)) %>%
  mutate(id = row_number()) %>%
  relocate(loc_meso) %>%
  relocate(loc_micro) %>%
  relocate(house) %>%
  add_column(country = "south africa")

# Cleaning

za2 <- droplevels(za2)

# Recoding

za2$curr_money <- if_else(za2$curr_money > 0, TRUE, NA)

za2$nothh_money <- if_else(za2$nothh_money > 0, TRUE, NA)

za2$nothh_goods <- recode(za2$nothh_goods, `yes` = TRUE, `no` = FALSE)


# Location time!

loc_z <- read_csv("South Africa/south africa locations_meso.csv")
loc_z <- mutate(loc_z, landmark = NULL, notes = NULL)

za2 <- left_join(za2, loc_z, by = c("loc_meso" = "location"))


za2a <- za2 %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

za2a$cts <- rowSums(za2a[ , 2:5], na.rm = T)

zaggs <- aggregate(za2a$cts, by = list(za2a$house),function(x){ifelse(sum (x) > 0, 1, 0)})

zaggs <- data.frame(house = zaggs$Group.1, remit = zaggs$x)

za2b <- za2 %>%
  select(house, country, lat, long) %>%
  distinct(house, .keep_all = T)

za3 <- inner_join(za2b, zaggs)


### Uganda

u_total <- as_tibble(read.dta("Uganda/Uganda Sections 1 and 4 Household_Members_21_03_2011.dta"))
u_curr <- as_tibble(read.dta("Uganda/Uganda Section 5 Household Member Migrants_21_03_2011.dta"))
u_nothh <- as_tibble(read.dta("Uganda/Uganda Section 6 Non Household Member Migrants_21_03_2011.dta"))

# Total interviewed

u_total <- u_total %>%
  select(qnaireno, earea, parish, district) %>% #district is highest level in Uganda. Switched to district on 9-9.
  mutate_at(vars(earea, parish, district), ~ str_to_lower(.)) %>%
  distinct(qnaireno, .keep_all = T) # Each household represented once (already was, but just being sure)

# Current HH members

u_curr <- u_curr %>%
  select(qnaireno, q520_1, q523_1)

# qnaireno = unique ID; q520_1 = curr_money, q523_1 = curr_goods)

# Not HH members

u_nothh <- u_nothh %>%
  select(qnaireno, q68, q6_11)

# qnaireno = unique ID; q68 = nothh_money; q6_11 = nothh_goods

## Merge

u <- bind_rows(u_curr, u_nothh)
u1 <- full_join(u_total, u, by = "qnaireno")

u1 <- u1 %>%
  group_by(qnaireno) %>%
  mutate(id = row_number()) %>%
  add_column(country = "uganda") %>%
  rename(house = qnaireno, loc_micro = earea, loc_meso = district, curr_money = q520_1, curr_goods = q523_1, nothh_money = q68, nothh_goods = q6_11)

## Cleaning

u1 <- droplevels(u1)

## Recoding

u1$curr_money <- if_else(u1$curr_money > 0, TRUE, NA)

u1$curr_goods <- recode(u1$curr_goods, `Yes` = TRUE, `No` = FALSE)

u1$nothh_money <- if_else(u1$nothh_money > 0, TRUE, NA)

u1$nothh_goods <- recode(u1$nothh_goods, `Yes` = TRUE, `No` = FALSE)

#Location time!

loc_u <- read_csv("Uganda/uganda locations_meso.csv")
loc_u <- mutate(loc_u, landmark = NULL, notes = NULL)

u1 <- left_join(u1, loc_u, by = c("loc_meso" = "location"))


u1a <- u1 %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

u1a$cts <- rowSums(u1a[ , 2:5], na.rm = T)

uaggs <- aggregate(u1a$cts, by = list(u1a$house),function(x){ifelse(sum (x) > 0, 1, 0)})

uaggs <- data.frame(house = uaggs$Group.1, remit = uaggs$x)

u1b <- u1 %>%
  select(house, country, lat, long) %>%
  distinct(house, .keep_all = T)

u2 <- inner_join(u1b, uaggs)

######## Row bind ########

dat <- bind_rows(ken2, bur2, nga2, sen2, za3, u2)

write_csv(dat, "remittance data_meso.csv")
