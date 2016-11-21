#Working out what I want to do with the housing data for the course
#Note: data not included in this repo. File included for info.
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pryr)
library(maptools)
library(rgeos)
library(zoo)

#library(tidyverse)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Land registry "price paid" data (England and Wales)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads
#Downloaded to my working folder for now.

#Use readr (allows us to see the progress of the file being loaded
#as well as actually succeeding in loading it.)
#Just under three minutes to load into memory
landRegistry <- read_csv('my_working_out/pp-complete.csv',col_names = F)

names(landRegistry)
head(landRegistry)

#"Explanation of column names" is here:
#https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd
#So we can give the data more sensible column names from that

#Last but one column is PPD. Only "A"s are definitely private sales.
#       A        B 
#21483148   233445 
table(landRegistry$X15)

landRegistry <- landRegistry[landRegistry$X15=='A',]

#Keep just the columns we want 
landRegistry <- landRegistry %>% select(2,3,4,5,11:14)

#Currently 1.37gb
object_size(landRegistry)

#Sensible names for these variables
head(landRegistry)
names(landRegistry) <- c('price','date','postcode','type','locality','town/city','district','county')

#Numbers of types of locality?
unique(landRegistry$locality) %>% length#23045
unique(landRegistry$`town/city`) %>% length#1170
unique(landRegistry$district) %>% length#454
unique(landRegistry$county) %>% length#127 ... doesn't quite seem to add up

#Save two smaller subsets, see which one I want to use
write_csv(landRegistry,'my_working_out/price-paids_inc_localities.csv')
#landRegistry <- read_csv('my_working_out/price-paids_inc_localities.csv')

#Just price / date / postcode  /type
write_csv(landRegistry %>% select(price,date,postcode,type),'my_working_out/price-paids_basics.csv')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#"Codepoint open": open access postcode data (Great Britain)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#https://www.ordnancesurvey.co.uk/business-and-government/products/code-point-open.html
#https://www.ordnancesurvey.co.uk/opendatadownload/products.html

#https://stat.ethz.ch/pipermail/r-help/2010-October/255593.html
filenames <- list.files(path = "my_working_out/codepo_gb/Data/CSV",full.names = T)
postcodes <- do.call("rbind", lapply(filenames, read.csv, header = F))

#Can also do this very easily / more intuitively in windows command line 
#http://www.solveyourtech.com/merge-csv-files/

#Again, we have no column names. But the download from Ordnance Survey tells us what's what:
#In codepo_gb/Doc/Code-Point_Open_Column_Headers.csv
#We can actually load this and apply it directly to the postcodes data
postcode_headerNames <- read_csv('my_working_out/codepo_gb/Doc/Code-Point_Open_Column_Headers.csv')

names(postcodes) <- postcode_headerNames
head(postcodes)

#we can drop Scotland: Land Registry data is only for England and Wales
#Actually, let's just use England: 
#for comparing to IMD we can't use multiple countries.
unique(postcodes$Country_code)
postcodes <- postcodes %>% filter(Country_code == 'E92000001')

#And keep only columns with information in... oh, they all have now I've got rid of Scotland
#Save!
write_csv(postcodes,'my_working_out/codepoint_open_combined_EnglandOnly.csv')

postcodes <- read_csv('my_working_out/codepoint_open_combined_EnglandOnly.csv')

#Point in polygon LSOA and MSOA codes into the postcode file
#Get both geographies
LSOA <- readShapeSpatial('C:/Data/MapPolygons/England/2011/England_lsoa_2011_gen_clipped/england_lsoa_2011_gen_clipped.shp')

MSOA <- readShapeSpatial('C:/Data/MapPolygons/England/2011/England_msoa_2011_gen_clipped/england_msoa_2011_gen_clipped.shp')

WARDS <- readShapeSpatial('C:/Data/MapPolygons/England/2011//England_wards_2011_gen_clipped/england_wa_2011_gen_clipped.shp')

TTWAS <- readShapeSpatial('C:/Data/MapPolygons/England/2001/England_ttwa_2001/england_ttwa_2001.shp')

#TTWA <- 

#spatialify the postcodes
postcodes_sp <- postcodes %>% data.frame
coordinates(postcodes_sp) <- ~Eastings+Northings

#Tell R they're currently British National Grid
#(We know the coordinates used in these three *are* BNG, so this is safe.)
proj4string(LSOA) <- CRS("+init=epsg:27700")
proj4string(MSOA) <- CRS("+init=epsg:27700")
proj4string(WARDS) <- CRS("+init=epsg:27700")
proj4string(TTWAS) <- CRS("+init=epsg:27700")
proj4string(postcodes_sp) <- CRS("+init=epsg:27700")

pcode_MSOAs <- postcodes_sp %over% MSOA
pcode_LSOAs <- postcodes_sp %over% LSOA
pcode_WARDS <- postcodes_sp %over% WARDS
pcode_TTWAS <- postcodes_sp %over% TTWAS

#Same order as original postcodes dataframe so can just apply directly
#(I'll check that in QGIS... Yup, all good.)
postcodes$LSOA <- pcode_LSOAs$code
postcodes$MSOA <- pcode_MSOAs$code
postcodes$ward <- pcode_WARDS$NAME
postcodes$ttwa_name <- pcode_TTWAS$NAME

#save
#write_csv(postcodes,'my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs.csv')
write_csv(postcodes,'my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs_WARDS_TTWAs.csv')

#Used below to attach TTWA name to largest-area-this-ward-is-in below
#In "Ward-to-TTWA-intersect"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Attach MSOA/LSOA codes to land registry sales via postcode----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landRegistry <- read_csv('my_working_out/price-paids_inc_localities.csv')
#So currently ~21.7 million sales.

#Get subset of codepoint open with LSOA/MSOAs attached
postcodes <- read_csv('my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs.csv')

#Remove spaces from postcodes in both
landRegistry$pcode_noSpace <- gsub(' ','',landRegistry$postcode)
postcodes$pcode_noSpace <- gsub(' ','',postcodes$Postcode)

landRegistry2 <- merge(landRegistry,postcodes 
                       %>% select(pcode_noSpace,LSOA,MSOA),by = 'pcode_noSpace')

#Newer save below
#saveRDS(landRegistry2,'my_working_out/price-paids_inc_localities_MSOA_LSOAadded.rds')
#landRegistry2 <- readRDS('my_working_out/price-paids_inc_localities_MSOA_LSOAadded.rds')

#With one NA...
unique(landRegistry2$MSOA) %>% length

#Hmm: must have been NA in the original postcodes
nas <- landRegistry2[is.na(landRegistry2$LSOA),]
unique(nas$pcode_noSpace) %>% length

#But not enough.
table(is.na(postcodes$LSOA))

#Keep only sales within a known zone
#20659983 to 20658824
landRegistry2 <- landRegistry2[!is.na(landRegistry2$LSOA),]

#save again
saveRDS(landRegistry2,'my_working_out/price-paids_inc_localities_MSOA_LSOAadded.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Attach WARD/TTWA to land registry sales via postcode----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#landRegistry <- read_csv('my_working_out/price-paids_inc_localities.csv')
landRegistry <- read_csv('my_working_out/price-paids_basics.csv')
#So currently ~21.7 million sales.

#Drop 'other' type of housing. Oh, don't need to - that must have been covered by removing
#the PPD "B"s
#landRegistry <- landRegistry %>% filter(type!='O')

#Get subset of codepoint open with LSOA/MSOAs attached
postcodes <- read_csv('my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs_WARDS_TTWAs.csv')

#Remove spaces from postcodes in both
landRegistry$pcode_noSpace <- gsub(' ','',landRegistry$postcode)
postcodes$pcode_noSpace <- gsub(' ','',postcodes$Postcode)

landRegistry2 <- merge(landRegistry,postcodes 
                       %>% select(pcode_noSpace,ward,ttwa_name),by = 'pcode_noSpace')

unique(postcodes$ttwa_name) %>% length
unique(landRegistry2$ttwa_name) %>% length
unique(landRegistry2$ward) %>% length#one entire ward missing? Wonder where that is.

#Bread street. Huh: yes, very rich mostly commercial bit of London. No sales?
#Or wrong postcode? Or no postcodes?
postcodes$ward[!(postcodes$ward %in% landRegistry2$ward)]

#Check none are NA. Hmm, a few now.
landRegistry2$ward[is.na(landRegistry2$ward)] %>% length
#But they all have TTWAs?
landRegistry2$ward[is.na(landRegistry2$ttwa_name)] %>% length

#What are those NA wards?
nawards <- landRegistry2[is.na(landRegistry2$ward),]
#landRegistry2$ttwa_name[landRegistry2$ttwa_name=='Brighton'] %>% length

#Oops - we lost a few over the edge of the clipped ward shapefiles, into the sea!
#Not many. Drop those, save.
#20659983 to 20658456
landRegistry2 <- landRegistry2[!is.na(landRegistry2$ward),]

#save
#saveRDS(landRegistry2,'my_working_out/price-paids_inc_localities_WARD_TTWA_added.rds')
saveRDS(landRegistry2,'my_working_out/price-paids_WARD_TTWA_added.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checks: Land registry against postcodes----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Update:
#Actually, postcodes themselves were geocoded - LSOA and MSOAs attached.
#Use those to encode land registry sales to MSOAs/LSOAs.

#use subsetted data from above
#landRegistry <- read_csv('my_working_out/price-paids_basics.csv')
landRegistry <- read_csv('my_working_out/price-paids_inc_localities.csv')

#So currently ~21.7 million sales.

#Get subset of codepoint open for postcode geocoding
postcodes <- read_csv('my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs.csv')

#OK - how many matches do we have? I suspect there'll be an historic dropoff
#Oh and are they in the same format? Yes, they seem to be.
#FALSE     TRUE 
#10906902 10809691
table(landRegistry$postcode %in% postcodes$Postcode)

#That's a lot of non-matches. Let's check on dates
#landRegistry$date_formatted <- as.Date(landRegistry$date, format = '%Y-%m-%d')
#landRegistry$year <- year(landRegistry$date_formatted)

landRegistry$hasPcodeMatch <- 0 + (landRegistry$postcode %in% postcodes$Postcode)

#Nope, it's not date. Almost completely perfect split
year_vs_pcode <- table(landRegistry$hasPcodeMatch,landRegistry$year) %>% data.frame()

#Ah silly me: not the same format. Postcode file has fixed length.
#Let's remove spaces from both and check again.
landRegistry$pcode_noSpace <- gsub(' ','',landRegistry$postcode)
postcodes$pcode_noSpace <- gsub(' ','',postcodes$Postcode)

#Much better. Not perfect.
#FALSE     TRUE 
#1056610 20659983
table(landRegistry$pcode_noSpace %in% postcodes$pcode_noSpace)

#So is it year-related this time?
year_vs_pcode <- table(landRegistry$pcode_noSpace %in% postcodes$pcode_noSpace,
                       landRegistry$year)

#Actually, no, the frequency is even across time. Best look again
landRegistry$hasPcode_noSpace_Match <- 0 + (landRegistry$pcode_noSpace %in% postcodes$pcode_noSpace)
table(landRegistry$hasPcode_noSpace_Match)

#Let's look at a small sample of non-matches see
#I suspect some name code changes
#CF36 3EL
#SA1 4HH

#None
inPcodesQ <- postcodes[grepl('CF36',postcodes$Postcode),]
#Loads
inPcodesQ <- postcodes[grepl('BN14',postcodes$Postcode),]

#OK. Let's look *where* the postcodes are that are not in the land registry
#I suspect we'll find what we're looking for
missinz <- postcodes[!(postcodes$pcode_noSpace %in% landRegistry$pcode_noSpace),]

#Let's chuck those into QGIS for easy viewing
write_csv(missinz,'my_working_out/postcodes_notinlandregistry.csv')

#Buggrit - no apparent pattern
#We need to look back at the land registry place names that we dropped
#Re-run the above with the land registry data with name places

#Look at just the non-matches
nonMatches <- landRegistry[landRegistry$hasPcode_noSpace_Match == 0,]

#Oh. I'm very silly aren't I? It's still got Welsh properties in.
#There are a few still missing from England but the vast majority of non-matches
#Are in Wales. Which we don't want anyway since we're only looking at English IMD.
countiez <- table(nonMatches$county) %>% data.frame() %>% 
  arrange(-Freq)

#For the ones that are *not* wales, might be worth checking if these remaining ones
#Don't match because of postcode changes - should show up in date difference.
#Perfect filter is a pain but this should do to tell us.
keepCountiez <- countiez %>% filter(Freq < 10000)

#I'm not sure this was quite what I was looking for...
table(landRegistry$year[landRegistry$county %in% keepCountiez$Var1],
      landRegistry$hasPcode_noSpace_Match[landRegistry$county %in% keepCountiez$Var1])

#Just check it for GREATER LONDON - how do those break down by date?
table(nonMatches$hasPcode_noSpace_Match[nonMatches$county=='GREATER LONDON'],
      nonMatches$year[nonMatches$county=='GREATER LONDON'])
table(landRegistry$hasPcode_noSpace_Match[nonMatches$county=='GREATER LONDON'],
      landRegistry$year[nonMatches$county=='GREATER LONDON'])

#Right so, actually, no I see no date bias there.
#Let's just look at some of those non-matched greater london postcodes
#Check for anything obvious.
GL <- landRegistry %>% filter(county=='GREATER LONDON')

#Err...
table(GL$district,GL$hasPcode_noSpace_Match)

#Oh hah: those with *no* postcode?? About half of the remainder I think.
table(!GL$hasPcode_noSpace_Match,is.na(GL$postcode))

#Can come back to filter Welsh counties specifically
#For now...
#Well, actually. Rather than merge eastings/northings to land registry...
#Let's PiP the geographies into the postcode file.

#~~~~~~~~~~~~
#Some other checks
#Are there any four-char second parts? I think it's always one number and two letters.
#(Wikipedia certainly thinks so!)
postcodeInward <- strsplit(postcodes$Postcode,' ')
postcodeInward <- sapply(postcodeInward,function(x) x[2])

#Hmm...
table(nchar(postcodeInward)!=3)
#Oh. No inward code at all?
postcodeInward[nchar(postcodeInward)!=3]

#No - it's just those with four outward code characters.
#Which, once you think about it, means that proves the outward/second part *must*
#always be three chars. Otherwise the overall postcode length couldn't remain fixed.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#English  Index of Multiple Deprivation----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015
imd <- read_csv('my_working_out/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv')

#Check IMD LSOA codes match shapefile
unique(imd$`LSOA code (2011)`) %>% length

lsoas <- readShapeSpatial('C:/Data/MapPolygons/England/2011/England_lsoa_2011_clipped/england_lsoa_2011_clipped.shp')

#Same number. Good good! 32844
unique(lsoas@data$code) %>% length

#Check LSOA / ward lookup works. Err. Could have sworn they tesselated.
#Yeah, not this!
lookup <- read_csv('my_working_out/OA_lookup/OA11_WD15_LAD15_EW_LU.csv')
#unique(lookup$OA11CD) %>% length

#Let's try OA to ward look up (best fit apparently)
lookup <- read_csv('my_working_out/OA_lookups/output_areas_(2011)_to_wards_(2011)_to_local_authority_districts_(2011)_e+w_lookup/OA11_WD11_LAD11_EW_LU.csv')

#So a few split over more than one ward?
unique(lookup$OA11CD) %>% length
nrow(lookup)-unique(lookup$OA11CD) %>% length#16? Why?

#FALSE   TRUE 
#176981   4443 
table(lookup$PERCENTAGE_BF < 100)

#Right. So *mostly* perfect fit. Wonder where the non-perfect ones are?
#And if the non-perfect ones are the duplicate ones?

#~~~~~~~~~~~~~~~~~
#Looks like I'll have to do this myself! Having done intersect in QGIS.
#Christ, it's 2.9gb
intersectLSOA_ward <- readShapeSpatial('my_working_out/qgis/intersectLSOA_ward.shp')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Finding a suitable geography----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Couple of options:
#If we intersect MSOA/Ward, can steal ward names for "ward it's most in". Mostly those will be different.
#Use wards / LSOA lookup via the QGIS intersect.

#Let's try the latter as an option to start with. Large file, everything else cleared out.
#intersectLSOA_ward <- readShapeSpatial('my_working_out/qgis/intersectLSOA_ward.shp')

#Newp, too big! Let's try the MSOA approach.Because (a) they tesselate properly with LSOA and 
#(b) should be able to get names for them from wards.

#Getting names via wards first, using a ward/msoa intersect from QGIS
intersectMSOA_ward <- readShapeSpatial('my_working_out/qgis/intersectMSOA_ward.shp')

head(intersectMSOA_ward@data)

#E02 is MSOA, E05 is ward. NAME_1 is ward name

intersectMSOA_ward@data$area <- gArea(intersectMSOA_ward, byid = T)

interz <- data.frame(intersectMSOA_ward) %>% select(2:3,5,7)
names(interz) <- c('MSOA','ward','ward_name','area')

#Find out if there's a modal ward name per MSOA
#http://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
singleMSOA <- interz %>% group_by(MSOA) %>% 
  top_n(n=1) %>% 
  arrange(-area)

#Compare numbers of ward names. 
#Probably doesn't matter about duplicates - just gives us a rough idea where they area

#What we could do is select the second-largest area groups
#And for those ones combine the names
#Having just looked at Chadwell Heath, for example:
#One MSOA is totally within in, the other is pretty evenly split.
#We'd need to dismiss slivers too.
#Might come back to this and use the names we currently have.

#Let's just check rate of duplication of ward name there
wardDups <- singleMSOA %>% group_by(ward_name) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

table(wardDups$count)
#Ah: some of those (Abbey at least) are ward names in a number of different places.
#A lot of 'central's for instance.
#So those could do with town names too - which we get via the housing data, actually.

#So let's try getting second-largest too.
#Lots of slivers, right? Right.
boxplot(log(interz$area))

#So what we want to do:
#If an MSOA is entirely within a ward
#Save for some slivers
#We need to discount the sliver that's second-largest.
#We need to only keep those where the top two largest are a reasonable size.

#Well, start by getting the top two largest!
MSOA_topTwoLargestChunks <- interz %>% group_by(MSOA) %>% 
  top_n(n=2) %>% arrange(MSOA,-area)

#Rule out ones with more than three orders of magnitude difference
#(We picked two... they might not all *have* two though?)

#Or did it drop ones that didn't? Newp, all accounted for
unique(MSOA_topTwoLargestChunks$MSOA) %>% length
unique(interz$MSOA) %>% length

MSOA_topTwoLargestChunks <- MSOA_topTwoLargestChunks %>% group_by(MSOA) %>% 
  mutate(diff = max(area)/min(area))

#So need to pick some ratio of these two to mark those I'll give two ward names
#Let's try a relatively high ratio first, see if that gives us unique names overall
#Plus: order the names in each MSOA so we can tell if there are any duplicates
#Update: don't order the ward names. The order would indicate the larger
#So that should be unique
twoWardNames <- MSOA_topTwoLargestChunks %>% filter(diff < 5) %>% 
  group_by(MSOA) 
  #%>% arrange(ward_name)

#Then keep a portmanteau of the two wards and see if they're unique
twoWardNames_p <- twoWardNames %>% group_by(MSOA) %>% 
  summarise(ward_name_p = paste0(min(as.character(ward_name)),'/',max(as.character(ward_name))) )
  
#Duplicates? Yup! And it's not a ratio size thing, that can't get rid of it. Bah!
#Oh well, might just leave those ones as duplicates and not worry - it's a very small number
unique(twoWardNames_p$ward_name_p) %>% length

#So using a ratio of 2 for the area of the largest and smallest ward the MSOA is overlapping
#Do we get unique ward names for the rest? 
#Order by area size this time, use top one.

areTheseUniqueWardNames <- interz %>% group_by(MSOA) %>% 
  top_n(n=1) %>% arrange(MSOA,-area) %>% 
  filter(!(MSOA %in% unique(twoWardNames_p$MSOA)))

#Nope. Nowhere near.
unique(areTheseUniqueWardNames$ward_name) %>% length

#~~~~~~~~~~~~~~~~~~~~~
#Maybe better idea than portmanteau: just for duplicates, number them by size
#So a couple of things we need here:
#For each MSOA, what's the biggest area chunk of ward?
#We already have that - singleMSOA

#Note again: the duplicate names are often in completely different places

#So that just needs a count for each group in order
singleMSOA <- interz %>% group_by(MSOA) %>% 
  top_n(n=1) %>% 
  arrange(-area) %>% 
  group_by(ward_name) %>% 
  mutate(count = seq(1:n())) %>% 
  #mutate(ward_name_c = paste0(ward_name,' ',count))
  mutate(ward_name_c = ifelse(max(count)!=1,
           paste0(ward_name,' ',count),ward_name))


singleMSOA <- interz %>% group_by(MSOA) %>% 
  top_n(n=1) %>% 
  arrange(-area) %>% 
  group_by(ward_name) %>% 
  mutate(count = seq(1:n())) %>% 
  mutate(maxcount = n())
  
  #mutate(ward_name_c = paste0(ward_name,' ',count))
  #mutate(ward_name_c = ifelse(max(count)!=1,
   #        'pang','pong'))
  
#Couldn't get ifelse to work on non-quote variable names. Dunno why, it should I thought.
#So:
singleMSOA$ward_name_c <- as.character(singleMSOA$ward_name)

singleMSOA$ward_name_c[singleMSOA$maxcount > 1] <- 
  paste0(as.character(singleMSOA$ward_name[singleMSOA$maxcount > 1]),' (',
         singleMSOA$count[singleMSOA$maxcount > 1],')')

#FALSE  TRUE 
#3318  3473
table(singleMSOA$maxcount == 1)

#Well that'll have to do! Gotta move on. Might come back to it.
#e.g. check if they're in different places.

#Got all the MSOAs still? Yup. 
unique(interz$MSOA) %>% length

#Save those MSOA ward names
saveRDS(singleMSOA,'my_working_out/MSOAs_w_wardNames.rds')
singleMSOA <- readRDS('my_working_out/MSOAs_w_wardNames.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Linking MSOAs to IMD data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

imd <- read_csv('my_working_out/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv')

#Check IMD LSOA codes match shapefile
unique(imd$`LSOA code (2011)`) %>% length

#Get LSOA/MSOA lookup
lookup <- read.csv('my_working_out/OA_lookups/OAlookup/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU.csv')

#(Actually, note: we can possibly use Local Authorities there to better rename MSOAs to wards)

#Keep only unique LSOAs and MSOA names
#Filter out welsh LSOAs
lookup_L2M <- lookup %>% select(LSOA11CD,MSOA11CD) %>% 
  filter(!grepl('W',LSOA11CD)) %>%
  distinct

#Too many. Is this for Wales too?
#unique(lookup_L2M$LSOA11CD) %>% length
#Yup - welsh filtered out above now
#unique(lookup$LAD11NM)

#Now it's the right number and they match
table(lookup_L2M$LSOA11CD %in% imd$`LSOA code (2011)`)

#And the right number of MSOAs? Yup!
unique(lookup_L2M$MSOA11CD) %>% length()

#Merge MSOA into the IMD LSOAs
imd <- merge(imd,lookup_L2M,by.x = 'LSOA code (2011)',by.y = 'LSOA11CD')

#Rearrange and rename so LS/MSOA are next to each other
imd <- imd %>% select(LSOA = 1,2,MSOA = MSOA11CD,3:57)

#save!
saveRDS(imd,'my_working_out/imd_w_msoas.rds')
imd <- readRDS('my_working_out/imd_w_msoas.rds')

#Hmm: one LSOA in the IMD list that's not in the land registry match. Where?
imd$`LSOA code (2011)`[!(imd$`LSOA code (2011)` %in% landRegistry2$LSOA)]

#DL9 3SW: an example pcode in that LSOA. In the original postcodes? Yes. Found it in the LSOA map.
postcodes$LSOA[postcodes$Postcode == 'DL9 3SW']

#Could be there are no sales. Seems unlikely. Tis but one though!
chk <- landRegistry[grepl('catterick',landRegistry$`town/city`,ignore.case = T),]

#OK, now we can average some IMD values per MSOA.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Linking TTWAs to the land registry via TTWA/MSOA intersect----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get intersect from QGIS
MSOA2TTWA_intersect <- readShapeSpatial('my_working_out/qgis/intersectMSOA_ttwa.shp')

MSOA2TTWA_intersect@data$area <- gArea(MSOA2TTWA_intersect, byid = T)

MSOA2TTWA_intersect_df <- data.frame(MSOA2TTWA_intersect)

#How many MSOAs are entirely within a TTWA?
#(They will only appear once in the intersect)
#5056
MSOAsingletons <- subset(MSOA2TTWA_intersect_df,!duplicated(MSOA2TTWA_intersect_df$code) 
       & !duplicated(MSOA2TTWA_intersect_df$code,fromLast = T))

#the rest...
MSOAdups <- MSOA2TTWA_intersect_df[!(MSOA2TTWA_intersect_df$code %in% MSOAsingletons$code),]

#For the duplicates, pick the TTWA that's got the largest chunk of MSOA
#We didn't actually need both of those separate dfs for that, huh?
MSOA_topArea <- MSOA2TTWA_intersect_df %>% group_by(code) %>% 
  top_n(n=1)

#Let's mark those with overlap and check we agree with the choice by looking in QGIS
MSOA_topArea$partial <- 0
MSOA_topArea$partial[MSOA_topArea$code %in% MSOAdups$code] <- 1

#Hmm - many of them will have been slivers on borders.
#E02004033 should be in Derby. That one's def split
#Yup. OK then!
MSOA_topArea$NAME_1[MSOA_topArea$code=='E02004033']

names(MSOA_topArea)[names(MSOA_topArea)=='NAME_1'] <- 'TTWA_name'

#save that separately
saveRDS(MSOA_topArea,'my_working_out/MSOA_majorityAreaInTTWA.rds')

MSOA_topArea2 <- MSOA_topArea %>% select(code,TTWA_name)

#Just realised I haven't merged the MSOA ward names in. Might do that here to save time.
#Loaded singleMSOA
MSOA_topArea3 <- merge(MSOA_topArea2,singleMSOA[,c('MSOA','ward_name_c')],by.x = 'code', by.y = 'MSOA') %>% 
  rename(ward_name = ward_name_c)

#Merge into land registry, rows to stay at 20658824
landRegistry <- merge(landRegistry,MSOA_topArea3,by.x = 'MSOA',by.y = 'code', all.x = T)

#save
saveRDS(landRegistry,'my_working_out/price-paids_inc_localities_MSOA_LSOA_TTWA_wardName.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Ward-to-TTWA intersect----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get intersect from QGIS
WARD2TTWA_intersect <- readShapeSpatial('my_working_out/qgis/intersect_ward_ttwa.shp')

WARD2TTWA_intersect@data$area <- gArea(WARD2TTWA_intersect, byid = T)

WARD2TTWA_intersect_df <- data.frame(WARD2TTWA_intersect)

#For the duplicates, pick the TTWA that's got the largest chunk of WARD
#A lot of which will just be a single ward entirely within the parent TTWA

#BUT: update.
#There are wards that share names. So we need to do something different
#with the name to make sure this works. Thusly...
#Use both ward/TTWA name so we know we're picking a unique ward

#No, that doesn't work either, somewhat obviously!
#You just pick up all the overlaps. So think again...

#OK, I think the easier approach is just to PiP over TTWA.
#(So not using the code below.)

WARD2TTWA_intersect_df$nameCombo <- paste0(WARD2TTWA_intersect_df$NAME," ",WARD2TTWA_intersect_df$NAME_2)

WARD_topArea <- WARD2TTWA_intersect_df %>% group_by(nameCombo) %>% 
  top_n(n=1,area)

WARD_topArea2 <- WARD_topArea %>% select(ward = NAME,ttwaThisWardIsMostlyIn = NAME_2)

#That the right number of both?
unique(WARD_topArea2$ward) %>% length
unique(WARD_topArea2$ttwaThisWardIsMostlyIn) %>% length

postcodes <- read_csv('codepoint_open_combined_EnglandOnly.csv')

#Missing one ward! Where'd that go? We got NA here? Yup. Should drop those.
unique(postcodes$ward) %>% length
postcodes$ward[is.na(postcodes$ward)] %>% length

#drop out ward NAs
postcodes <- postcodes[!is.na(postcodes$ward),]

#Attach TTWA-ward-mostly-in to postcode data
postcodes2 <- merge(postcodes,WARD_topArea2,by = 'ward')

#save!
write_csv(postcodes2,'my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs_WARDS_TTWAs.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some data checks----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Got all the different bits of data I want. Let's just check what there is.
landRegistry <- readRDS('my_working_out/price-paids_inc_localities_MSOA_LSOAadded.rds')
#saveRDS(landRegistry %>% select,'my_working_out/price-paids_MSOA_LSOAadded.rds')
#landRegistry <- readRDS('my_working_out/price-paids_MSOA_LSOAadded.rds')

#So we have one LSOA with no sales (see above)
imd <- readRDS('my_working_out/imd_w_msoas.rds')

#Number of sales per MSOA per year?
landRegistry$date_formatted <- as.Date(landRegistry$date, format = '%Y-%m-%d')
landRegistry$year <- year(landRegistry$date_formatted)

numSalesByMSOA_n_year <- landRegistry %>% group_by(MSOA,year) %>% 
  summarise(count = n()) 

numSalesByMSOA_n_year <- numSalesByMSOA_n_year[order(numSalesByMSOA_n_year$count),]

output <- ggplot(numSalesByMSOA_n_year,aes(x = factor(year),y = count)) +
  geom_boxplot()

output

#None. Good good!
table(numSalesByMSOA_n_year$count == 0)

#But we do have some very small samples (see order above)
#These will prob show up in volatility of graphing prices across time

#~~~~~~~~~
#Check if counties in the land registry data match those in the 2010
#english county electoral divisions shapefile
countyshpz <- readShapeSpatial('C:/Data/MapPolygons/England/2011/England_ct_2011_gen_clipped/england_ct_2011_gen_clipped.shp')

#Well that's 35 counties vs 104 in the land registry. So no, not the same.
unique(countyshpz@data$name)
unique(landRegistry$county)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Land registry: misc bits of wotsits----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#2.43gb in memory
#landRegistry <- readRDS('my_working_out/price-paids_inc_localities_MSOA_LSOA_TTWA_wardName.rds')
landRegistry <- readRDS('my_working_out/price-paids_inc_localities_MSOA_LSOA_TTWA_wardName.rds')

imd <- readRDS('my_working_out/imd_w_msoas.rds')
imd <- readRDS('my_working_out/imd_w_msoas.rds')

#All good. 6791 of them.
table(unique(imd$MSOA) %in% unique(landRegistry$MSOA))

#GET RANK OF TTWAS/CITIES
#What are the TTWAs with the largest number of sales?
#As a proxy for picking the largest cities and ordering by that.
#Oh, or we could use primary addressable object name - or could if I hadn't dropped it.

#Nah, let's stick to sale number. Too much faff and sale number should be a reasonable proxy.
#From one particular year? Well let's look at all of those. First up:
salesCountby_year_n_TTWA <- landRegistry %>% group_by(ttwa_name,year) %>% 
  summarise(count = n())

#rank by count/year. Does that change much?
rankz <- salesCountby_year_n_TTWA %>% group_by(year) %>% 
  mutate(rank = min_rank(count)) %>% 
  dplyr::select(ttwa_name,year,rank) %>%#dropping redundant column is necessary
  spread(year,rank) %>% 
  arrange(-rowSums(.[2:23]))

rankz$sd <- apply(rankz[,2:23],1,sd)

#OK, so after all that: most don't change too dramatically from their positions.
#I think using the average rank as city/TTWA size is entirely reasonable.
#Just use the rank as is: an index can be added when reloaded.
#Actually, that probably won't load as a dataframe properly.
rankz$rank <- seq(1:nrow(rankz))

write_csv(rankz %>% dplyr::select(ttwa_name,rank),'my_working_out/TTWA_salesCountRank2.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Land registry: playing with the data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.43gb in memory
landRegistry <- read_RDS('my_working_out/price-paids_inc_localities_MSOA_LSOA_TTWA_wardName.rds')

#Let's just see if that Sheffield pattern is repeated in other TTWAs
#Print out to a folder
#my_working_out/TTWA_yearlyPricePatterns

#Look at top few TTWAs by size
TTWAsBySize <- read_csv('my_working_out/TTWA_salesCountRank.csv')


#for(ttwa in unique(landRegistry$TTWA_name)){
for(ttwa in TTWAsBySize$TTWA_name[1:30]){
  
  dt <- landRegistry %>% filter(TTWA_name == ttwa, year!=2016)
  
  #mean price per MSOA/year. How many of those are there?
  #Also, get sample size - too small won't have stable prices
  meanz <- dt %>% group_by(MSOA,year) %>% 
    summarise(count = n(), mean = mean(price))
  
  #Quite a lot to look at there so let's check the mean/min/max count per MSOA
  MSOAmeanMinMax <- meanz %>% group_by(MSOA) %>% 
    summarise(meanCount = mean(count), 
              #medCount = median(count), 
              minCount = min(count), 
              maxCount = max(count)) %>% 
    gather(var,value,meanCount:maxCount)
  
  # ggplot(MSOAmeanMinMax,aes(x = factor(var),y = value)) +
  #   geom_boxplot()
  # 
  #Select only those MSOAs with values above ...
  #Well, let's say the minimum count for each MSOA is above a certain value
  #Min count in a particular year should be a reasonable indication of overall
  #mean count (except in ones where house building occurred...)
  
  #Looking at the boxplot, let's see about minCount > 30
  MSOAmeanMins <- MSOAmeanMinMax %>% filter(var == 'minCount')
  
  MSOAsWithHigherCounts <- MSOAmeanMins %>% filter(value > 70)
  
  #Keep only those MSOAs from this TTWA. That's a lot still! Let's see.
  dt2 <- dt %>% filter(MSOA %in% unique(MSOAsWithHigherCounts$MSOA))
  
  #Get means per year again, ready for graphinz
  meanz2 <- dt2 %>% group_by(MSOA,year) %>% 
    summarise(count = n(), mean = mean(price))
  
  #top and bottom based on final price
  #Won't necessarily be the final year but will do the job
  maxMeanz <- meanz2 %>% group_by(MSOA) %>% 
    summarise(maxval = max(mean)) %>% 
    arrange(maxval) %>% 
    filter(row_number() %in% c(1:3,(n()-2):n()))
  
  meanz3 <- meanz2 %>% 
    filter(MSOA %in% maxMeanz$MSOA)
  
  output <- ggplot() +
    geom_line(data = meanz3, 
              #aes(x = as.Date(year, format = "%Y"), y = mean, colour = MSOA)) +
              aes(x = factor(year), y = mean, colour = MSOA,group = MSOA)) +
    # scale_colour_manual(values=cbPalette)
    scale_y_log10() +
    guides(colour = F) +
    ggtitle(ttwa) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  #output
  
  ggsave(paste0('my_working_out/gglot_outputs/ttwas_topbottomFiveMSOAs/topTTWAsBySize/',ttwa,'.png'),output,
         dpi = 150, height = 6, width = 8)
  
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Land registry: playing with the data 2, ward<>ttwa version----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.43gb in memory
#landRegistry <- readRDS('my_working_out/price-paids_inc_localities_WARD_TTWA_added.rds')
#After update to filter out "B" PPD category (i.e. keep definite residential sales)
landRegistry <- readRDS('my_working_out/price-paids_WARD_TTWA_added.rds')

#Filter out 'other' category. Some very large sales in there, not single properties.
#Experimenting with filtering out flats
#This is now gone, filtered I think along with PPD = "B"
#landRegistry <- landRegistry %>% filter(type!='O',type!='F')
#landRegistry <- landRegistry %>% filter(type!='O')

#table(landRegistry$type)

#Drop flats for now to see. Far fewer sales in centres if we do this... 
#in fact, that's a good point. We probably shouldn't do that.
#landRegistry_w_flats <- landRegistry


#Is missing year!
landRegistry$date_formatted <- as.Date(landRegistry$date, format = '%Y-%m-%d')
landRegistry$year <- year(landRegistry$date_formatted)

#Let's just see if that Sheffield pattern is repeated in other TTWAs
#Print out to a folder
#my_working_out/TTWA_yearlyPricePatterns

#Look at top few TTWAs by size
#(much more sensible now after keeping just residential!)
TTWAsBySize <- read_csv('my_working_out/TTWA_salesCountRank.csv')

selection <- list()

#for(ttwa in unique(landRegistry$TTWA_name)){
for(ttwa in TTWAsBySize$TTWA_name[1:30]){
  
  dt <- landRegistry %>% filter(ttwa_name == ttwa, year!=2016)
  
  #mean price per ward/year. How many of those are there?
  #Also, get sample size - too small won't have stable prices
  meanz <- dt %>% group_by(ward,year) %>% 
    summarise(count = n(), mean = mean(price))
  
  #Quite a lot to look at there so let's check the mean/min/max count per ward
  wardmeanMinMax <- meanz %>% group_by(ward) %>% 
    summarise(meanCount = mean(count), 
              #medCount = median(count), 
              minCount = min(count), 
              maxCount = max(count)) %>% 
    gather(var,value,meanCount:maxCount)
  
  # ggplot(wardmeanMinMax,aes(x = factor(var),y = value)) +
  #   geom_boxplot()
  # 
  #Select only those wards with values above ...
  #Well, let's say the minimum count for each ward is above a certain value
  #Min count in a particular year should be a reasonable indication of overall
  #mean count (except in ones where house building occurred...)
  
  #Looking at the boxplot, let's see about minCount > 30
  wardmeanMins <- wardmeanMinMax %>% filter(var == 'minCount')
  
  wardsWithHigherCounts <- wardmeanMins %>% filter(value > 70)
  
  #Keep only those wards from this TTWA. That's a lot still! Let's see.
  dt2 <- dt %>% filter(ward %in% unique(wardsWithHigherCounts$ward))
  
  #Is the count? Newp.
  #dt2 <- dt
  
  #Get means per year again, ready for graphinz
  meanz2 <- dt2 %>% group_by(ward,year) %>% 
    summarise(count = n(), mean = mean(price))
    #summarise(count = n(), mean = median(price))
  
  #top and bottom based on final price
  #Won't necessarily be the final year but will do the job
  maxMeanz <- meanz2 %>% group_by(ward) %>% 
    summarise(maxval = max(mean)) %>% 
    arrange(maxval) %>% 
    #filter(row_number() %in% c(1:3,(n()-2):n()))#top/bottom 3
    #filter(row_number() %in% c(1:5,(n()-4):n()))#top/bottom 5
    filter(row_number() %in% c(1:4,(n()-3):n())) %>% #top/bottom 4
    arrange(desc(maxval))#Arranged so we can use it to order factors
  
  meanz3 <- meanz2 %>% 
    filter(ward %in% maxMeanz$ward)
  
  meanz3$ward <- factor(meanz3$ward, levels = maxMeanz$ward)
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  output <- ggplot() +
    geom_line(data = meanz3, 
              #aes(x = as.Date(year, format = "%Y"), y = mean, colour = ward)) +
              aes(x = factor(year), y = mean, colour = ward,group = ward)) +
    scale_colour_manual(values=cbPalette) +
    #scale_y_log10() +
    #guides(colour = F) +
    ggtitle(ttwa) + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    expand_limits(y = 0)
  
  output
  
  ggsave(paste0('my_working_out/gglot_outputs/ttwas_topbottomFivewards/topTTWAsBySize/',ttwa,'.png'),output,
         dpi = 150, height = 6, width = 9)
  
  #Add some to a list to combine later for facetting
  #if(ttwa %in% c('Bradford','Sheffield & Rotherham','London','Brighton')){
  if(ttwa %in% c('Bradford','Leeds','London','Brighton')){
    selection[[length(selection)+1]] <- meanz3 %>% mutate(ttwaname = ttwa)
  }

}

#facet those we picked out
bunch <- do.call(rbind,selection)


output <- ggplot() +
  geom_line(data = bunch, 
            #aes(x = as.Date(year, format = "%Y"), y = mean, colour = ward)) +
            aes(x = factor(year), y = mean, colour = ward,group = ward)) +
  #scale_colour_manual(values=cbPalette) +
  #scale_y_log10() +
  guides(colour = F) +
  #ggtitle(ttwa) +
  #theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.2)) +
  # expand_limits(y = 0) +
  facet_wrap(~ttwaname) +
  xlab('year')

output




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Diagnostics. Problem with spikes not in original data. Wassup?
burngreave <- meanz3 %>% filter(ward == 'Burngreave')

#Let's just check the raw sales for the spike year (2013)
burngreave_orig <- landRegistry %>% filter(ward == 'Burngreave', year == 2013)

#Ah: raw data error. Is that raw error in the original download data?
#OK: I think it's "other", which I probably filtered out before and forgot about. Let's check.
#Yup, that's it! So can leave that in as an example of tracking down data problems, possibly.

#Birmingham still has a weird spike. In Soho and Victoria. 2013 again.
brum <- meanz3 %>% filter(ward == 'Soho and Victoria')

#Let's just check the raw sales for the spike year (2013)
brum_orig <- landRegistry %>% filter(ward == 'Soho and Victoria', year == 2013)

#That's looking like the RoS duplicate sale problem. Is it happening here too?

#~~~
#Check another. I think Oxford had a suspiciously high value.
#The fact these are all later years is suspicious.

#Oxford Churchill 2014
#Altrincham Manchester 2014 also.

#After TTWA fixing: Manchester spike still there, as is Birmingham one.

#OXFORD (having set ttwa manually to run above first)
ox <- meanz3 %>% filter(ward == 'Churchill')

#Let's just check the raw sales for the spike year (2013)
ox_orig <- landRegistry %>% filter(ward == 'Churchill', ttwa_name == 'Oxford', year == 2014)

manc_orig <- landRegistry %>% filter(ward == 'Altrincham', year == 2014)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checking duplicate prices for large flat sales----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So do we have this problem here? 
#Matching prices / dates / postcodes should show it.
#Mark duplicates

landRegistry$dups <- 0 + (duplicated(landRegistry[,c('price','date','postcode')])|
                            duplicated(landRegistry[,c('price','date','postcode')],fromLast = T))

#long job, save!
saveRDS(landRegistry,'my_working_out/landRegistry_dupsMarked.rds')
landRegistryDups <- readRDS('my_working_out/landRegistry_dupsMarked.rds')

table(landRegistryDups$dups)#Well, that's a lot!

landRegistryDups <- landRegistryDups %>% arrange(desc(dups))

#Most of those look like they're actually sensible prices. Hum. 
#I should have marked each dup group. Doh.

#There were 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Land registry and IMD linking (after processing both above)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landRegistry <- read_RDS('my_working_out/price-paids_inc_localities_MSOA_LSOA_TTWA_wardName.rds')

imd <- readRDS('my_working_out/imd_w_msoas.rds')






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Processing data ready for use in course----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landRegistry <- readRDS('my_working_out/price-paids_WARD_TTWA_added.rds')

#First-up: a summary of all TTWAs - just to open as an initial CSV for understanding dataframes.

ttwaAverages <- landRegistry %>% group_by(ttwa_name) %>% 
  summarise(avHousePrice = mean(price), numberOfSales = n())

write_csv(ttwaAverages,'my_working_out/averagePropertyPrices.csv')

#~~~~~~~~~~~~~~~~~~~~~
#We want to break data down into TTWAs... 
#but then drop all geog refs so they can link the postcode data back in

#Do we want the land registry data in a different shape? Well, we want to drop geogs.
#And keep only shortened postcode.
landRegistry4class <- landRegistry %>% select(pcode_noSpace,price,date,type,ttwa_name) %>% 
  rename(postcode = pcode_noSpace)

#save class version. Oh wait! I need to keep ttwa for splitting
saveRDS(landRegistry4class,'my_working_out/landRegistry4class_priorToCitySplit.rds')


#Let's just try with a sample first
luton <- landRegistry4class %>% filter(ttwa_name == 'Luton & Watford')
manc <- landRegistry4class %>% filter(ttwa_name == 'Manchester')
london <- landRegistry4class %>% filter(ttwa_name == 'London')

object_size(london)
object_size(brum)
object_size(luton)

#Get postcodes, see what I want to keep / how big file is.
#177mb. (I've got 1.85gb to play with on the memory stick.)
pcodes <- read_csv('my_working_out/codepoint_open_combined_EnglandOnly_MSOA_LSOAs_WARDS_TTWAs.csv')

#Data shouldn't be more than ~410mb. (If London and Manchester.) Loads of head room.
#I should probably try it with the examples with one no-one else will use.
#Something smaller beyond the top 15 or 30. List!
TTWAsBySize <- read_csv('my_working_out/TTWA_salesCountRank.csv')

#Process postcode file to keep just only what we want.
#pcodes4class <- pcodes %>% select(Postcode,LSOA,MSOA,ward,ttwa_name)

#Actually, drop ttwa from pcodes. They can add that in manually.
pcodes4class <- pcodes %>% select(Postcode,LSOA,MSOA,ward)

#Shorten postcode for ease of matching
#Not sure yet about 
pcodes4class$pcode_noSpace <- gsub(' ','',pcodes4class$Postcode)

pcodes4class <- pcodes4class %>% select(-Postcode) %>% 
  rename(postcode = pcode_noSpace)

pcodes4class <- pcodes4class %>% select(postcode,LSOA,MSOA,ward)

write_csv(pcodes4class, 'my_working_out/codePointOpen_postcode_lookup.csv')

#~~~~~~~~~~~~~~~~~~~~~
#Should now have LR and postcode data in the right format.
#Now just want to save a couple of city subsets for making the tutorial
lr <- readRDS('my_working_out/landRegistry4class_priorToCitySplit.rds')

#save the whole thing as a CSV for sticking in downloadable zip
write_csv(lr, 'my_working_out/landRegistry4class_priorToCitySplit.csv')

#Actually... having done that, I think I'd rather get the top x TTWAs.
TTWAsBySize <- read_csv('my_working_out/TTWA_salesCountRank.csv')

#Let's get the top 50. Huh - about 3/4 of sales
lrtop50 <- lr %>% filter(ttwa_name %in% TTWAsBySize$TTWA_name[1:50])
write_csv(lrtop50, 'my_working_out/landRegistry_top50TTWAs_by_salesCount.csv')

lrtop10 <- lr %>% filter(ttwa_name %in% TTWAsBySize$TTWA_name[1:10])

#dammit, wards. I might add them now rather than let them do it. That's not the task at that point.
pcodes <- read_csv('my_working_out/codePointOpen_postcode_lookup.csv')
pcodes <- pcodes %>% select(postcode,ward)

lrtop10 <- inner_join(lrtop10,pcodes,by = 'postcode')

write_csv(lrtop10, 'my_working_out/landRegistry_top10TTWAs_by_salesCount.csv')






#Get two cities
luton <- lr %>% filter(ttwa_name == 'Luton & Watford')
london <- lr %>% filter(ttwa_name == 'London')

#Drop ttwa names from each. Will add back in as part of thing (or will merge back in from postcodes?)
luton <- luton %>% select(-ttwa_name)
london <- london %>% select(-ttwa_name)

#save those both - luton as the example TTWA, London they'll all have to build comparison
write_csv(luton,'my_working_out/LUTON_landRegistryData.csv')
write_csv(london,'my_working_out/LONDON_landRegistryData.csv')

#~~~~~~~~~~~~~~~~





