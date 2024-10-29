library(tidyverse)
library(dplyr)

#this code produces the data used in Figures 3 and 4 (with the base figures created in Excel, and then beautified in Affinity)

#read in downloads from iNat
before <- read_csv("data/before_ids_edits.csv")
after <- read_csv("data/after_ids_edits.csv")

#we're interested in the after updated column so I can tell which obs were reviewed, but I don't want the time, only date
after$updated_at <- substr(after$updated_at, 1, 10)

#I need to combine the before and after, as some obs from the start were made casual or bumped out through obscuration and thus get lost from the after
combined<-dplyr::full_join(before, after, by = "id")

#now let's find these lost records so I can individually inspect them
bumped <- combined %>%
  filter(is.na(quality_grade.y))

#there are 70 of them. I will annotate each in a separate excel file, then deal with these later

#similarly, let's check how many records were added to iNat during the event (ie were not there for the before download)

new <- combined %>%
  filter(is.na(quality_grade.x))

#686 of these

#so progression was start as 21316 --> 70 were lost before the end to give us 21,246, but then 686 were added to give us 21,932 at the finish line (noting that we are
#discarding these 686 for the main analysis)
#of the 70 lost, 35 need to be reinstated for a total of 21,967. We'll deal with these later

#aside, just checking if there are any rows in combined df where every cell in the row is NA (there shouldn't be)
na_rows <- combined[rowSums(is.na(combined)) == ncol(combined), ]

#there are none

#now I want to get rid of a whole lot of extraneous columns that I'm not interested in for the basic stats I'm looking at in this script. 
#I'll do this in excel for ease and then read back in. I'm also going to combine the species and hybrid columns, ie treat hybrids as 'species' for these analyses
#note that for these hybrid entities, I had to use x instead of the multiplication symbol, as R doesn't recognise the latter
#and also rename columns for better reading
write.csv(combined, file = "combined_data.csv", row.names = FALSE)

#now load back in edited file
combined_edited <- read_csv("data/combined_data_edited.csv")

#check for all na rows
na_rows_edited <- combined_edited[rowSums(is.na(combined_edited)) == ncol(combined_edited), ]

#all good, there aren't any as expected, so back to the program, we'll discard the 686 batch referred to above. But I want to address the 70 that got bumped
#of these, 35 should stay bumped (various data quality and related reasons), 35 need to be reinstated
#of the 35, 3 are from Kalbarri. 
#First, however, let's filter to Kalbarri only from the combined_edited df without those 3

Kalbarri <- combined_edited[combined_edited$latitude_before > -28.866 & combined_edited$latitude_before < -25.5, ]

#check for all na rows
na_rows_k <- Kalbarri[rowSums(is.na(Kalbarri)) == ncol(Kalbarri), ]
                      
#my code is making the 686 after-only rows move into the Kalbarri file (also weirdly it's making the whole row NA, I can't explain that)
#not a big deal, but need to remove those

Kalbarri2 <- Kalbarri[!is.na(Kalbarri$grade_after), ]

#let's append those 3 that got bumped now
#first I'll write the bumped df as a csv, then edit that with new details from my previous manual checking, then read it back in and append for Kalbarri
#(note that for obscured stuff, the date the new ID was added during the event is generalised to month/masked from my view, so I've just used Feb 12th as the default for everything)
write.csv(bumped, file = "bumped_raw.csv", row.names = FALSE)
bumped_kalbarri <- read_csv("data/bumped_kalbarri_only.csv")

#bind these to the Kalbarri file
pre_KU<-dplyr::bind_rows(Kalbarri2, bumped_kalbarri)

#so this gives us 4506 to play with for Kalbarri. Of these, how many were updated during the event (event started 2024-02-12)
KU <- pre_KU %>% filter(updated_after > as.Date("2024-02-11"))

#2654 observations were reviewed out of 4506, 58.9%
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

KU_species <- KU %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species rank and species name remained the same 
KU_species2 <- KU_species %>% filter(species_before == species_after)

#1482/1620, which means 91.5% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 1620)
#started at RG first

KU_species_RG<- KU_species %>% filter(grade_before == "research")
KU_species_RG2<- KU_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 541/581 were confirmed as correct, 93.1%
#now check Needs ID

KU_species_NEED<- KU_species %>% filter(grade_before == "needs_id")
KU_species_NEED2<- KU_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 941/1039 were correct, 90.6%.
#however, there were actually 3 records that didn't get updated via an added ID by another user, need to account for these in these stats
#all 3 were Needs ID, and not updated, so I want to remove these from the KU file, and then redo the above stats. So updated was actually 2651/4506 (58.8%)
KU <- KU[-c(1091, 2056, 821), ]

#let's redo them all now (same steps as above)
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

KU_species <- KU %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species and species remained the same 
KU_species2 <- KU_species %>% filter(species_before == species_after)

#1479/1617, which means 91.5% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 1617)
#started at RG first

KU_species_RG<- KU_species %>% filter(grade_before == "research")
KU_species_RG2<- KU_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 541/581 were confirmed as correct, 93.1%
#now check Needs ID

KU_species_NEED<- KU_species %>% filter(grade_before == "needs_id")
KU_species_NEED2<- KU_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 938/1036 were correct, 90.5%.

#now we get to some new code again
#of the updated records, how many started at species, and how many finished at species?
species_before <- sum(complete.cases(KU[["species_before"]]))
species_after <- sum(complete.cases(KU[["species_after"]]))

#went from 1617/2651 to 2269/2651 (61 to 85.6%)

#similarly how many started/finished at RG?
RG_before <- sum(KU[["grade_before"]] == "research")
RG_after <- sum(KU[["grade_after"]] == "research")

#went from 581/2651 to 2035/2651 (21.9 to 76.8%)

#now let's look at unique species count

unique_df1 <- KU %>% 
  distinct(species_before, .keep_all = TRUE)

unique_df2 <- KU %>% 
  distinct(species_after, .keep_all = TRUE)

#remove the NA row in each
unq1 <- dplyr::select(unique_df1, species_before)
unq1 <- unq1[-4, ]

unq2 <- dplyr::select(unique_df2, species_after)
unq2 <- unq2[-2, ]

unq2 <- unq2 %>% 
  rename(species_before = species_after)

merged_unq <- merge(unq1, unq2, by = "species_before")

#we started at 480 species, finished at 536. 440 matched between the two, which means 40 were removed via corrections, and then 96 added 

#let's have a look at updated records that started at species, but that ID was wrong (ended up different species or coarser ID)
wrong <- KU_species %>% 
  filter(species_before != species_after | is.na(species_after))

#there were 138 of these. I'm pasting the URLs for each into an excel file so I can individually inspect each one to see what the change was
#having gone through them one by one in excel, 134 were corrected. For these, 83.6% were still in the same genus (112/134) and 
#94% were still in the same family (126/134)
#the remaining four, however, weren't actually corrected; the species name changed due to a taxon swap to amend an orthographic variant
#this affected obs of Guichenotia basiviridis and Goodenia reinwardtii
#2 of these were already RG at species, got confirmed as correct during the event
#2 were needs ID at species, got confirmed and moved to RG

#so some more amendments to stats to be made here
#first though let's remove these from the wrong df
wrong <- wrong[-c(35,83,102,111), ]

#now for the amended stats to account for these 4 errors (which I have done manually below instead of rerunning all the code again)
#1479/1617, which means 91.5% accuracy! --> 1483/1617 = 91.7%
#541/581 RG were confirmed as correct, 93.1% --> 543/581 = 93.5%
#938/1036 needs ID were confirmed as correct, 90.6% --> 940/1036 = 90.7%
#we started at 480 species, finished at 536. 440 matched between the two, which means 40 were removed via corrections, and then 96 added
#--> now 442 actually matched, which means 38 removed and 94 added



#now we're going to repeat all of this, but for Lesueur

#filter to Lesueur obs only:

Lesueur <- combined_edited[combined_edited$latitude_before > -30.883 & combined_edited$latitude_before < -29.979, ]

#check for all na rows
na_rows_L <- Lesueur[rowSums(is.na(Lesueur)) == ncol(Lesueur), ]

#my code is making the 686 after-only rows move into the Lesueur file too (also weirdly it's making the whole row NA, I can't explain that)
#not a big deal, but need to remove those

Lesueur2 <- Lesueur[!is.na(Lesueur$grade_after), ]

#For Lesueur it's 29 records that need to be appended after getting bumped
bumped_lesueur <- read_csv("data/bumped_lesueur_only.csv")

#bind these to the Lesueur file
pre_LS <-dplyr::bind_rows(Lesueur2, bumped_lesueur)

#so this gives us 5867 to play with for Lesueur. Of these, how many were updated during the event
LS <- pre_LS %>% filter(updated_after > as.Date("2024-02-11"))

#3616 observations were reviewed out of 5867, 61.6%
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

LS_species <- LS %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species and species remained the same 
LS_species2 <- LS_species %>% filter(species_before == species_after)

#2203/2394, which means 92% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 2394)
#started at RG first

LS_species_RG<- LS_species %>% filter(grade_before == "research")
LS_species_RG2<- LS_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 1096/1136 were confirmed as correct, 96.5%
#now check Needs ID

LS_species_NEED<- LS_species %>% filter(grade_before == "needs_id")
LS_species_NEED2<- LS_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 1107/1258 were correct, 88%.

#Similar to how I had to discard 3 Kalbarri records due to them not actually being updated, I have to make a few changes here, but with a little more manual curation
#due to some quirks on iNat records. There are 11 of these cases for Lesueur
#not actually updated (or treat as 'not updated') discard from updated pool: 10
#was updated and confirmed correct, RG start and end: 1

#so we need to make some changes here. First, 10 need to be removed from the updated pool, so it's 3606/5867 = 61.5% 
LS <- LS[-c(336,398,1056,2791,1288,1313,1321,3017,1320,2720), ]

#and then amend the cell value for the other one
LS[396, 2] <- "research"
LS[396, 21] <- "research"

#so having removed those, let's redo all the other code now (same steps as above)
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

LS_species <- LS %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species and species remained the same 
LS_species2 <- LS_species %>% filter(species_before == species_after)

#2193/2384, which means 92% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 2384)
#started at RG first

LS_species_RG<- LS_species %>% filter(grade_before == "research")
LS_species_RG2<- LS_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 1097/1137 were confirmed as correct, 96.5%
#now check Needs ID

LS_species_NEED<- LS_species %>% filter(grade_before == "needs_id")
LS_species_NEED2<- LS_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 1096/1247 were correct, 87.9%.


#now we get to some new code again
#of the updated records, how many started at species, and how many finished at species?
species_before_LS <- sum(complete.cases(LS[["species_before"]]))
species_after_LS <- sum(complete.cases(LS[["species_after"]]))

#went from 2384/3606 to 3288/3606 (66.1 to 91.2%)

#similarly how many started/finished at RG?
RG_before_LS <- sum(LS[["grade_before"]] == "research")
RG_after_LS <- sum(LS[["grade_after"]] == "research")

#went from 1137/3606 to 2920/3606 (31.5 to 80.1%)

#now let's look at unique species count

unique_df3 <- LS %>% 
  distinct(species_before, .keep_all = TRUE)

unique_df4 <- LS %>% 
  distinct(species_after, .keep_all = TRUE)

#remove the NA row in each
unq3 <- dplyr::select(unique_df3, species_before)
unq3 <- unq3[-1, ]

unq4 <- dplyr::select(unique_df4, species_after)
unq4 <- unq4[-20, ]

unq4 <- unq4 %>% 
  rename(species_before = species_after)

merged_unq2 <- merge(unq3, unq4, by = "species_before")

#we started at 507 species, finished at 556. 462 matched between the two, which means 45 were removed via corrections, and then 94 added 

#let's have a look at updated records that started at species, but that ID was wrong (ended up different species or coarser ID)
wrong2 <- LS_species %>% 
  filter(species_before != species_after | is.na(species_after))

#there were 191 of these. I'm pasting the URLs for each into an excel file so I can individually inspect each one to see what the change was
#having gone through them one by one in excel, 174 were corrected. For these, 85.1% were still in the same genus (148/174) and 
#97.7% were still in the same family (170/174)
#the remaining 17, however, weren't actually corrected; the species name changed due to a taxon swap to amend an orthographic variant/update a genus
#this affected obs of Goodenia reinwardtii, Melaleuca cuspidata, Babingtonia grandiflora, Ammothryon grandiflorum
#6 obs were already RG at species, got confirmed as correct during the event
#9 were needs ID at species, got confirmed and moved to RG
#and two which were RG start and end weren't actually updated at all unfortunately. So we're gonna have to do all the stats again...


#so some more amendments to stats to be made here!
#first though, let's remove these from the wrong df
wrong2 <- wrong2[-c(50,51,54,58,59,72,76,79,81,88,93,101,106,107,110,154,156), ]

#remove the two non-reviewed ones from the reviewed file
LS <- LS[-c(1481,2525), ]

#so we're back to 3604/5867 reviewed now (61.4%). And let's redo the stats one last time
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

LS_species <- LS %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species and species remained the same 
LS_species2 <- LS_species %>% filter(species_before == species_after)

#2193/2382, which means 92.1% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 2382)
#started at RG first

LS_species_RG<- LS_species %>% filter(grade_before == "research")
LS_species_RG2<- LS_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 1097/1135 were confirmed as correct, 96.7%
#now check Needs ID

LS_species_NEED<- LS_species %>% filter(grade_before == "needs_id")
LS_species_NEED2<- LS_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 1096/1247 were correct, 87.9%.


#of the updated records, how many started at species, and how many finished at species?
species_before_LS <- sum(complete.cases(LS[["species_before"]]))
species_after_LS <- sum(complete.cases(LS[["species_after"]]))

#went from 2382/3604 to 3286/3604 (66.1 to 91.2%)

#similarly how many started/finished at RG?
RG_before_LS <- sum(LS[["grade_before"]] == "research")
RG_after_LS <- sum(LS[["grade_after"]] == "research")

#went from 1135/3604 to 2918/3604 (31.5 to 80.1%)

#and then manually amend the stats one final time to account for those 15 taxon swap cases that aren't actually wrong

#2193/2382, which means 92.1% accuracy! --> 2208/2382 = 92.7%
#1097/1135 were confirmed as correct, 96.7% --> 1103/1135 = 97.2%
#1096/1247 were correct, 87.9% --> 1105/1247 = 88.6%
#we started at 507 species, finished at 556. 462 matched between the two, which means 45 were removed via corrections, and then 94 added
#--> now 466 actually matched, which means 41 removed and 90 added




#now we're going to repeat all of this, but for Fitzgerald 

#filter to Fitzgerald obs only:

Fitzgerald <- combined_edited[combined_edited$latitude_before > -35.042 & combined_edited$latitude_before < -32.461, ]

#check for all na rows
na_rows_L <- Fitzgerald[rowSums(is.na(Fitzgerald)) == ncol(Fitzgerald), ]

#my code is making the 686 after-only rows move into the Fitzgerald file too (also weirdly it's making the whole row NA, I can't explain that)
#not a big deal, but need to remove those

Fitzgerald2 <- Fitzgerald[!is.na(Fitzgerald$grade_after), ]

#For Fitzgerald it's 3 records that need to be appended after getting bumped
bumped_fitzgerald <- read_csv("data/bumped_fitzgerald_only.csv")

#bind these to the Fitzgerald file
pre_FG <-dplyr::bind_rows(Fitzgerald2, bumped_fitzgerald)

#so this gives us 10908 to play with for Fitzgerald. Of these, how many were updated during the event
FG <- pre_FG %>% filter(updated_after > as.Date("2024-02-11"))

#4715 observations were reviewed out of 10908, 43.2%
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

FG_species <- FG %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species and species remained the same 
FG_species2 <- FG_species %>% filter(species_before == species_after)

#3048/3300, which means 92.4% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 3300)
#started at RG first

FG_species_RG<- FG_species %>% filter(grade_before == "research")
FG_species_RG2<- FG_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 1737/1793 were confirmed as correct, 96.9%
#now check Needs ID

FG_species_NEED<- FG_species %>% filter(grade_before == "needs_id")
FG_species_NEED2<- FG_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 1311/1507 were correct, 87%.

#Similar to how I had to discard 3 Kalbarri records due to them not actually being updated, I have to make a few changes here. There are 18 of these cases for Fitzgerald
#not actually updated (or treat as 'not updated') discard from updated pool: 18

#so we need to make some changes here. First, 18 need to be removed from the updated pool, so it's 4697/10908 = 43.1% 
FG <- FG[-c(867,906,927,931,951,1237,1396,1516,1648,2180,2706,2754,3003,3031,3355,4002,4264,4420), ]

#so having removed those 18, let's redo all the other code now (same steps as above)
#let's filter these to ones that started at species pre-event, to see how many were confirmed as correct vs how many corrected

FG_species <- FG %>% filter(!is.na(species_before))

#now how many of these were confirmed as correct at species level, ie started at species and species remained the same 
FG_species2 <- FG_species %>% filter(species_before == species_after)

#3030/3282, which means 92.3% accuracy!
#we can check the differences between those that started as RG vs started as Needs ID (from all 3282)
#started at RG first

FG_species_RG<- FG_species %>% filter(grade_before == "research")
FG_species_RG2<- FG_species_RG %>% filter(species_before == species_after)

#for observations that were RG at species, 1737/1793 were confirmed as correct, 96.9%
#now check Needs ID

FG_species_NEED<- FG_species %>% filter(grade_before == "needs_id")
FG_species_NEED2<- FG_species_NEED %>% filter(species_before == species_after)

#for ones that were needs ID at species, 1293/1489 were correct, 86.8%.

#now we get to some new code again
#of the updated records, how many started at species, and how many finished at species?
species_before_FG <- sum(complete.cases(FG[["species_before"]]))
species_after_FG <- sum(complete.cases(FG[["species_after"]]))

#went from 3282/4697 to 4131/4697 (69.9 to 87.9%)

#similarly how many started/finished at RG?
RG_before_FG <- sum(FG[["grade_before"]] == "research")
RG_after_FG <- sum(FG[["grade_after"]] == "research")

#went from 1793/4697 to 3740/4697 (38.2 to 79.6%)

#now let's look at unique species count

unique_df5 <- FG %>% 
  distinct(species_before, .keep_all = TRUE)

unique_df6 <- FG %>% 
  distinct(species_after, .keep_all = TRUE)

#remove the NA row in each
unq5 <- dplyr::select(unique_df5, species_before)
unq5 <- unq5[-1, ]

unq6 <- dplyr::select(unique_df6, species_after)
unq6 <- unq6[-19, ]

unq6 <- unq6 %>% 
  rename(species_before = species_after)

merged_unq3 <- merge(unq5, unq6, by = "species_before")

#we started at 663 species, finished at 759. 602 matched between the two, which means 61 were removed via corrections, and then 157 added 

#let's have a look at updated records that started at species, but that ID was wrong (ended up different species or coarser ID)
wrong3 <- FG_species %>% 
  filter(species_before != species_after | is.na(species_after))

#there were 252 of these. I'm pasting the URLs for each into an excel file so I can individually inspect each one to see what the change was
#having gone through them one by one in excel, 248 were corrected. For these, 81.5% were still in the same genus (202/248) and 
#95.6% were still in the same family (237/248)
#the remaining 4, however, weren't actually corrected; the species name changed due to a taxon swap to amend an orthographic variant/update a genus
#this affected obs of Ammothryon grandiflorum
#2 obs were already RG at species, got confirmed as correct during the event
#2 were needs ID at species, got confirmed and moved to RG

#so some more amendments to stats to be made here
#first though, let's remove these from the wrong df
wrong3 <- wrong3[-c(169,194,224,230), ]

#now for the manually amended stats
#3030/3282, which means 92.3% accuracy! --> 3034/3282 = 92.4%
#1737/1793 were confirmed as correct, 96.9% --> 1739/1793 = 97%
#1293/1489 were correct, 86.8% --> 1295/1489 = 87%
#we started at 663 species, finished at 759. 602 matched between the two, which means 61 were removed via corrections, and then 157 added 
#--> now 603 actually matched, which means 60 removed and 156 added


#to calculate universal stats across all 3 regions, it's easy enough to just add the various numbers from above, which I have done in Excel. 
#The only exception is unique no. of species given some of these are shared across regions, which we'll do below

#first, combine the 3 'before' unique species df
gsb <- dplyr::bind_rows(unq1, unq3, unq5)
gsb2 <-dplyr::distinct(gsb)

#then after
gsb3 <- dplyr::bind_rows(unq2, unq4, unq6)
gsb4 <-dplyr::distinct(gsb3)

#shared step
merged_unq4 <- merge(gsb2, gsb4, by = "species_before")

#start at 1370, finish at 1553. 1276 matched between the two, which means 94 were removed, and then 277 added
#but we need to account for those taxon swap cases above, which affected 5 different species
#so actually 1281 matched, 89 removed, 272 added
       