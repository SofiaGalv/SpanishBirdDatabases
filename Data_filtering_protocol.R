
### DATA FILTERING PROTOCOL ---------------------------------------------------

## 1. Read all databases (using "fread" function for SEO and eBird, and 
## selecting from them just the required columns).

table_MNCN <- read.table("MNCN.csv", header=T, sep=",")
table_EBD <- read.table("EBD.csv", header=T, sep=",")
table_IEET <- read.table("IEET.csv", header=T, sep=",")
table_AVIS <- read.table("AVIS.csv", header=T, sep=",")

library(data.table)
table_SEO <- fread("SEO.txt")
table_SEO <- data.frame(year = tabla_SEO$year, country = tabla_SEO$countryCode, 
                        genus = tabla_SEO$genus, 
                        specificEpithet = tabla_SEO$specificEpithet)
table_ebird <- fread("ebird.txt")
table_ebird <- data.frame(year = tabla_ebird$year, 
                           country = tabla_ebird$countryCode, 
                           genus = tabla_ebird$genus, 
                           specificEpithet = tabla_ebird$specificEpithet)

## 2. Create, for each database, a new "Species" column using "genus" and 
## "specificEpithet" variables. For EBD database, also select just the rows 
## which "Country" is Spain ("ES").

table_MNCN$species <- paste(tabla_MNCN$genus, tabla_MNCN$specificEpithet)

table_EBD$species <- paste(tabla_EBD$genus, tabla_EBD$specificEpithet)
table_EDB <- tabla_EBD[tabla_EBD$country == "ES", ]

table_IEET$species <- paste(tabla_IEET$genus, tabla_IEET$specificEpithet)
table_SEO$species <- paste(tabla_SEO$genus, tabla_SEO$specificEpithet)
table_ebird$species <- paste(tabla_ebird$genus, tabla_ebird$specificEpithet)

   #AVIS is different, so select the 2ºcolumn.

colnames(table_AVIS)[2] <- "species"

## 3. Read D'Amico database and call its "species" column.

table_DAmico <- read.table("DAmico.csv", header = T, sep = ",")
colnames(table_DAmico)[2] <- "species"

## 4. Check which species from each database does not match with D'Amico
## database, and save it in a ".csv" file for checking for taxonomic 
## differences by experts.

no <- match(table_MNCN$species, table_DAmico$species)
no_MNCN <- unique(table_MNCN$species [is.na(no)])
write.table(no_MNCN, "no_MNCN.csv", sep = ",", row.names = F)

no <- match(table_EDB$species, table_DAmico$species)
no_EBD <- unique(table_EDB$species [is.na(no)])
write.table(no_EBD, "no_EBD.csv", sep = ",", row.names = F)

no <- match(table_IEET$species, table_DAmico$species)
no_IEET <- unique(tablE_IEET$species [is.na(no)])
write.table(no_IEET, "no_IEET.csv", sep = ",", row.names = F)

no <- match(table_SEO$species, table_DAmico$species)
no_SEO <- unique(table_SEO$species [is.na(no)])
write.table(no_SEO, "no_SEO.csv", sep = ",", row.names = F)

no <- match(table_ebird$species, table_DAmico$species)
no_ebird <- unique(table_ebird$species [is.na(no)])
write.table(no_ebird, "no_ebird.csv", sep = ",", row.names = F)

no <- match(table_AVIS$species, table_DAmico$species)
no_AVIS <- unique(table_AVIS$species [is.na(no)])
write.table(no_AVIS, "no_AVIS.csv", sep = ",", row.names = F)

## 5. The new databases were returned with the synonyms of some species names
## that does not match at first. They are databases with 2 columns, one for the
## D'Amico species name and one for the original database species name. 
## Now, read them to use them to change the species names in the original
## databases by their D'Amico synonyms.

no_MNCN <- read.table("no_MNCNsyn.csv", header = T, sep = ",")
no_EBD <- read.table("no_EBDsyn.csv", header = T, sep = ",")
no_IEET <- read.table("no_IEETsyn.csv", header = T, sep = ";")
no_SEO <- read.table("no_SEOsyn.csv", header = T, sep = ";")
no_ebird <- read.table("no_ebirdsyn.csv", header = T, sep = ";")
no_AVIS <- read.table("no_avissyn.csv", header = T, sep= ",")

## 6. Create a function to change the species names of the original databases 
## by their D'Amico synonyms.

change_names <- function(synonyms, table) {
  for (i in 1:nrow(synonyms)) {
    veces <- length(table[table$species == synonyms$x[i],  
                            names(table) == "species"])
    table [table$species == synonyms$x[i],  
           names(table) == "species"] <- rep(as.character(synonyms[, 1][i]), veces)
  }
  table
}

## 7. Prepare the synonym database to be used in the function, selecting just 
## the D'Amico columns entries that contain information.  

unique(no_MNCN$DAmico)[-4]
synonyms_MNCN <- no_MNCN[no_MNCN$DAmico %in% unique(no_MNCN$DAmico)[-4], ]
MNCN <- change_names(synonyms_MNCN, table_MNCN)

unique(no_EBD$DAmico)[-1]
synonyms_EBD <- no_EBD[no_EBD$DAmico %in% unique(no_EBD$DAmico)[-1], ]
EBD <- change_names(synonyms_EBD, table_EDB)

unique(no_IEET$DAmico)[-1]
synonyms_IEET <- no_IEET[no_IEET$DAmico %in% unique(no_IEET$DAmico)[-1], ]
IEET <- change_names(synonyms_IEET, table_IEET)

unique(no_SEO$DAmico)[-1]
synonyms_SEO <- no_SEO [no_SEO$DAmico %in% unique(no_SEO$DAmico)[-1], ]
SEO <- change_names(synonyms_SEO, table_SEO)

unique(no_ebird$DAmico)[-1]
synonyms_ebird <- no_ebird[no_ebird$DAmico %in% unique(no_ebird$DAmico)[-1], ]
ebird <- change_names(synonyms_ebird, table_ebird)

unique(no_AVIS$DAmico)[-1]
synonyms_AVIS <- no_AVIS[no_AVIS$DAmico %in% unique(no_AVIS$DAmico)[-4], ]
AVIS <- change_names(synonyms_AVIS, table_AVIS)

## 8. Save the new databases

write.table(MNCN, "MNCN_nuevo.csv", sep = ",", row.names = F)
write.table(EBD, "EBD_nuevo.csv", sep = ",", row.names = F)
write.table(IEET, "IEET_nuevo.csv", sep = ",", row.names = F)
write.table(SEO, "SEO_nuevo.txt", sep = ",", row.names = F)
write.table(ebird, "ebird_nuevo.txt", sep = ",", row.names = F)
write.table(AVIS, "AVIS_nuevo.csv", sep = ",", row.names = F)

## 9. Merge the new databases with D'Amico database by "species" column.

MNCN <- merge(x = MNCN, y = table_DAmico, by = "species", all.x = TRUE)

EBD <- merge(x = EBD, y = table_DAmico, by = "species", all.x = TRUE)

IEET <- merge(x = IEET, y = table_DAmico, by = "species", all.x = TRUE)

SEO <- merge(x = SEO, y = table_DAmico, by = "species", all.x = TRUE)

ebird <- merge(x = ebird, y = table_DAmico, by = "species", all.x = TRUE)

AVIS <- merge(x = AVIS, y = table_DAmico,by = "species", all.x = TRUE)

## 10. Eliminate the "%" symbol from "Occurrences.in.Sp" column, and transform 
## it into a numeric object.

MNCN$Occurrence.in.Sp <- as.character(MNCN$Occurrence.in.Sp)
MNCN$Occurrence.in.Sp = substr(MNCN$Occurrence.in.Sp, 1, 
                               nchar(MNCN$Occurrence.in.Sp)-1)
MNCN$Occurrence.in.Sp <- as.numeric(as.character(MNCN$Occurrence.in.Sp))

EBD$Occurrence.in.Sp <- as.character(EBD$Occurrence.in.Sp)
EBD$Occurrence.in.Sp = substr(EBD$Occurrence.in.Sp, 1, 
                              nchar(EBD$Occurrence.in.Sp)-1)
EBD$Occurrence.in.Sp <- as.numeric(as.character(EBD$Occurrence.in.Sp))

IEET$Occurrence.in.Sp <- as.character(IEET$Occurrence.in.Sp)
IEET$Occurrence.in.Sp = substr(IEET$Occurrence.in.Sp, 1, 
                               nchar(IEET$Occurrence.in.Sp)-1)
IEET$Occurrence.in.Sp <- as.numeric(as.character(IEET$Occurrence.in.Sp))

SEO$Occurrence.in.Sp <- as.character(SEO$Occurrence.in.Sp)
SEO$Occurrence.in.Sp = substr(SEO$Occurrence.in.Sp, 1, 
                              nchar(SEO$Occurrence.in.Sp)-1)
SEO$Occurrence.in.Sp <- as.numeric(as.character(SEO$Occurrence.in.Sp))

ebird$Occurrence.in.Sp <- as.character(ebird$Occurrence.in.Sp)
ebird$Occurrence.in.Sp = substr(ebird$Occurrence.in.Sp, 1, 
                                nchar(ebird$Occurrence.in.Sp)-1)
ebird$Occurrence.in.Sp <- as.numeric(as.character(ebird$Occurrence.in.Sp))

AVIS$Occurrence.in.Sp <- as.character(AVIS$Occurrence.in.Sp)
AVIS$Occurrence.in.Sp = substr(AVIS$Occurrence.in.Sp, 1, 
                               nchar(AVIS$Occurrence.in.Sp)-1)
AVIS$Occurrence.in.Sp <- as.numeric(as.character(AVIS$Occurrence.in.Sp))

## 11. Eliminate NA's rows from databases.

MNCN <- MNCN[!is.na(MNCN$Rank), ]
EBD <- EBD[!is.na(EBD$Rank), ]
IEET <- IEET[!is.na(IEET$Rank), ]
SEO <- SEO[!is.na(SEO$Rank), ]
ebird <- ebird[!is.na(ebird$Rank), ]
AVIS <- AVIS[!is.na(AVIS$Rank), ]

## 12. To compare AVIS database with the others, as its occurrences are grouped
## by species names, it is necessary to create a new dataframe with all 
## occurrences as separate rows. Do it for all the variables under study.
## In the case of the "Threat" variable, it is also necessary to transform it
## into a numeric object.

avis_final <- rep(AVIS$Occurrence.in.Sp, AVIS$Observaciones)
avis_final_Weight <- rep(AVIS$Weight, AVIS$Observaciones)
avis_final_wingspan <- rep(AVIS$Wingspan, AVIS$Observaciones)
avis_final_Wet <- rep(AVIS$Wetland, AVIS$Observaciones)
avis_final_farm <- rep(AVIS$Farmland, AVIS$Observaciones)
avis_final_agro <- rep(AVIS$Agroforest, AVIS$Observaciones)
avis_final_fore <- rep(AVIS$Forest, AVIS$Observaciones)
avis_final_scru <- rep(AVIS$Scrubland, AVIS$Observaciones)
avis_final_clif <- rep(AVIS$Cliff, AVIS$Observaciones)

avis_final_threat <- rep(AVIS$Status.in.Sp, AVIS$Observaciones)
avis_final_threat <- as.numeric(as.character(avis_final_threat))

## 13. Draw plots, calculate some informative statistics (mean, median, 
## quartiles) and calculate percentages for breeding habitats and number of
## endangered individuals. 

  ## Occurrences Boxplot

colcajas<- c("palegreen2", "palegreen2", "palegreen2", "gray82", "lightsalmon",
             "lightsalmon")
boxplot(MNCN$Occurrence.in.Sp, EBD$Occurrence.in.Sp, IEET$Occurrence.in.Sp, 
        SEO$Occurrence.in.Sp, ebird$Occurrence.in.Sp, avis_final, axes = F, 
        main = "Spanish geographical distribution", 
        ylab = "Geographical distribution (%)", boxwex = 0.6, col = colcajas,
        outline = F)
axis(2)
axis(1, labels = c("MNCN", "EBD", "IEET", "SEO", "EBIRD", "AVIS"), at = c(1:6),
     las = 2)

  ## Weight Boxplot

boxplot(MNCN$Weight, EBD$Weight, IEET$Weight, SEO$Weight, ebird$Weight, 
        avis_final_Weight, ylim = c(0, 3000), axes = F, main="Weigth (grams)",
        boxwex = 0.6, outline = F, ylab = "Weight (grams)", col = colcajas)
axis(2)
axis(1, labels = c("MNCN", "EBD", "IEET", "SEO", "EBIRD", "AVIS"), at = c(1:6),
     las = 2)

  ## Wingspan Boxplot

boxplot(MNCN$Wingspan, EBD$Wingspan, IEET$Wingspan, SEO$Wingspan, 
        ebird$Wingspan, avis_final_wingspan, ylim = c(0, 300), axes = F, 
        main = "Wingspan (centimetres)", boxwex = 0.6, outline = F, 
        ylab = "Wingspan (centimetres)", col = colcajas)
axis(2)
axis(1, labels = c("MNCN", "EBD", "IEET", "SEO", "EBIRD", "AVIS"), at = c(1:6), 
     las = 2)

  ##Informative statistics (mean, median, quartiles)

summary(MNCN$Occurrence.in.Sp)
summary(EBD$Occurrence.in.Sp)
summary(IEET$Occurrence.in.Sp)
summary(SEO$Occurrence.in.Sp)
summary(ebird$Occurrence.in.Sp)
summary(avis_final)

summary(MNCN$Weight)
summary(EBD$Weight)
summary(IEET$Weight)
summary(SEO$Weight)
summary(ebird$Weight)
summary(avis_final_Weight)

summary(MNCN$Wingspan)
summary(EBD$Wingspan)
summary(IEET$Wingspan)
summary(SEO$Wingspan)
summary(ebird$Wingspan)
summary(avis_final_wingspan)

  ##Breeding habitat percentages

sum(MNCN$Wetland, na.rm = T)/nrow(MNCN)
sum(MNCN$Farmland, na.rm = T)/nrow(MNCN)
sum(MNCN$Agroforest, na.rm = T)/nrow(MNCN)
sum(MNCN$Forest, na.rm = T)/nrow(MNCN)
sum(MNCN$Scrubland, na.rm = T)/nrow(MNCN)
sum(MNCN$Cliff, na.rm = T)/nrow(MNCN)

sum(EBD$Wetland, na.rm = T)/nrow(EBD)
sum(EBD$Farmland, na.rm = T)/nrow(EBD)
sum(EBD$Agroforest, na.rm = T)/nrow(EBD)
sum(EBD$Forest, na.rm = T)/nrow(EBD)
sum(EBD$Scrubland, na.rm = T)/nrow(EBD)
sum(EBD$Cliff, na.rm = T)/nrow(EBD)

sum(IEET$Wetland, na.rm = T)/nrow(IEET)
sum(IEET$Farmland, na.rm = T)/nrow(IEET)
sum(IEET$Agroforest, na.rm = T)/nrow(IEET)
sum(IEET$Forest, na.rm = T)/nrow(IEET)
sum(IEET$Scrubland, na.rm = T)/nrow(IEET)
sum(IEET$Cliff, na.rm = T)/nrow(IEET)

sum(SEO$Wetland, na.rm = T)/nrow(SEO)
sum(SEO$Farmland, na.rm = T)/nrow(SEO)
sum(SEO$Agroforest, na.rm = T)/nrow(SEO)
sum(SEO$Forest, na.rm = T)/nrow(SEO)
sum(SEO$Scrubland, na.rm = T)/nrow(SEO)
sum(SEO$Cliff, na.rm = T)/nrow(SEO)

sum(ebird$Wetland, na.rm = T)/nrow(ebird)
sum(ebird$Farmland, na.rm = T)/nrow(ebird)
sum(ebird$Agroforest, na.rm = T)/nrow(ebird)
sum(ebird$Forest, na.rm = T)/nrow(ebird)
sum(ebird$Scrubland, na.rm = T)/nrow(ebird)
sum(ebird$Cliff, na.rm = T)/nrow(ebird)

sum(avis_final_Wet, na.rm = T)/length(avis_final_Wet)
sum(avis_final_farm, na.rm = T)/length(avis_final_farm)
sum(avis_final_agro, na.rm = T)/length(avis_final_agro)
sum(avis_final_fore, na.rm = T)/length(avis_final_fore)
sum(avis_final_scru, na.rm = T)/length(avis_final_scru)
sum(avis_final_clif, na.rm = T)/length(avis_final_clif)

  ## Endangered individuals percentage

sum(MNCN$Status.in.Sp %in% c("CR", "EN", "VU"), na.rm = T)/nrow(MNCN)
sum(EBD$Status.in.Sp %in% c("CR", "EN", "VU"), na.rm = T)/nrow(EBD)
sum(IEET$Status.in.Sp %in% c("CR", "EN", "VU"), na.rm = T)/nrow(IEET)
sum(SEO$Status.in.Sp %in% c("CR", "EN", "VU"), na.rm = T)/nrow(SEO)
sum(ebird$Status.in.Sp %in% c("CR", "EN", "VU"), na.rm = T)/nrow(ebird)
sum(avis_final_amenaza, na.rm = T)/length(avis_final_amenaza)

###############################################################################

## 14. On the other hand, for some analysis, we grouped all the occurrences by
## species. AVIS database was already in that format. As you can see, it is 
## necessary to merge again each database with D'Amico one, and remove the "%"
## symbol from "Occurrence.in.Sp" column.

MNCN2 <- as.data.frame(table(MNCN$species))
names(MNCN2) <- c("species", "Observaciones")
MNCN2 <- merge(x = MNCN2, y = table_DAmico, 
              by = "species", all.x = TRUE)
MNCN2$Occurrence.in.Sp <- as.character(MNCN2$Occurrence.in.Sp)
MNCN2$Occurrence.in.Sp = substr(MNCN2$Occurrence.in.Sp, 1, 
                                nchar(MNCN2$Occurrence.in.Sp)-1)
MNCN2$Occurrence.in.Sp <- as.numeric(as.character(MNCN2$Occurrence.in.Sp))

EBD2 <- as.data.frame(table(EBD$species))
names(EBD2) <- c("species", "Observaciones")
EBD2 <- merge(x = EBD2, y = table_DAmico, 
             by = "species", all.x = TRUE)
EBD2$Occurrence.in.Sp <- as.character(EBD2$Occurrence.in.Sp)
EBD2$Occurrence.in.Sp = substr(EBD2$Occurrence.in.Sp, 1,
                               nchar(EBD2$Occurrence.in.Sp)-1)
EBD2$Occurrence.in.Sp <- as.numeric(as.character(EBD2$Occurrence.in.Sp))

IEET2 <- as.data.frame(table(IEET$species))
names(IEET2) <- c("species", "Observaciones")
IEET2 <- merge(x = IEET2, y = table_DAmico, 
                 by = "species", all.x = TRUE)
IEET2$Occurrence.in.Sp <- as.character(IEET2$Occurrence.in.Sp)
IEET2$Occurrence.in.Sp = substr(IEET2$Occurrence.in.Sp, 1,
                                nchar(IEET2$Occurrence.in.Sp)-1)
IEET2$Occurrence.in.Sp <- as.numeric(as.character(IEET2$Occurrence.in.Sp))

SEO2 <- as.data.frame(table(SEO$species))
names(SEO2)<- c("species", "Observaciones")
SEO2 <- merge(x = SEO2, y = table_DAmico, 
             by = "species", all.x = TRUE)
SEO2$Occurrence.in.Sp <- as.character(SEO2$Occurrence.in.Sp)
SEO2$Occurrence.in.Sp = substr(SEO2$Occurrence.in.Sp, 1,
                               nchar(SEO2$Occurrence.in.Sp)-1)
SEO2$Occurrence.in.Sp <- as.numeric(as.character(SEO2$Occurrence.in.Sp))

ebird2 <- as.data.frame(table(ebird$species))
names(ebird2) <- c("species", "Observaciones")
ebird2 <- merge(x = ebird2, y = table_DAmico, 
                by = "species", all.x = TRUE)
ebird2$Occurrence.in.Sp <- as.character(ebird2$Occurrence.in.Sp)
ebird2$Occurrence.in.Sp = substr(ebird2$Occurrence.in.Sp, 1,
                                 nchar(ebird2$Occurrence.in.Sp)-1)
ebird2$Occurrence.in.Sp <- as.numeric(as.character(ebird2$Occurrence.in.Sp))

## 15. Create the new "accessible habitats" variable.

MNCN2$Accesible <- MNCN2$Agroforest + MNCN2$Farmland
MNCN2$Accesible[MNCN2$Accesible>1] <- 1

EBD2$Accesible <- EBD2$Agroforest + EBD2$Farmland
EBD2$Accesible [EBD2$Accesible>1] <- 1

IEET2$Accesible <- IEET2$Agroforest + IEET2$Farmland
IEET2$Accesible[IEET2$Accesible>1] <- 1

SEO2$Accesible <- SEO2$Agroforest + SEO2$Farmland
SEO2$Accesible[SEO2$Accesible>1] <- 1

ebird2$Accesible <- ebird2$Agroforest + ebird2$Farmland
ebird2$Accesible[ebird2$Accesible>1] <- 1

AVIS$Accesible <- AVIS$Agroforest + AVIS$Farmland
AVIS$Accesible[AVIS$Accesible>1] <- 1

## 16. Create a new "Threat" variable that includes "EN", "VU", "CR" categories
## from "Status.in.Sp".

EBD2$Endangered <- EBD2$Status.in.Sp %in% c("EN", "VU", "CR")
MNCN2$Endangered <- MNCN2$Status.in.Sp %in% c("EN", "VU", "CR")
IEET2$Endangered <- IEET2$Status.in.Sp %in% c("EN", "VU", "CR")
SEO2$Endangered <- SEO2$Status.in.Sp %in% c("EN", "VU", "CR")
ebird2$Endangered <- ebird2$Status.in.Sp %in% c("EN", "VU", "CR")
AVIS$Endangered <- AVIS$Status.in.Sp %in% c("EN", "VU", "CR")

## 17. In the case, remove NAs from the databases.

MNCN2 <- MNCN2[!is.na(MNCN2$Rank), ]
EBD2 <- EBD2[!is.na(EBD2$Rank), ]
IEET2 <- IEET2[!is.na(IEET2$Rank), ]
AVIS <- AVIS[!is.na(AVIS$Rank), ]
ebird2 <- ebird2[!is.na(ebird2$Rank), ]
SEO2 <- SEO2[!is.na(SEO2$Rank), ]

## 18. Run statistical analysis.