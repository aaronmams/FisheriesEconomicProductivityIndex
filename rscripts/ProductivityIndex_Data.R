##########*** Landing Data##############
landings.data<-read.csv(file = paste0(dir.data, "fis_foss_coastal_ws-foss_landings_mv.csv"))
write.csv(x = landings.data, file = paste0(dir.rawdata,"fis_foss_coastal_ws-foss_landings_mv.csv"))


landings.data<-landings.data[,c("TSN", "H_NEW_ITIS", "TS_AFS_NAME", "TS_SCIENTIFIC_NAME", "YEAR", 
                                "POUNDS", "DOLLARS", "S_STATE_ID", "COLLECTION")]

names(landings.data)[names(landings.data) %in% "S_STATE_ID"]<-"State.no"
names(landings.data)[names(landings.data) %in% "TSN"]<-"Tsn"
landings.data$TSN[is.na(landings.data$H_NEW_ITIS)]<-landings.data$H_NEW_ITIS[is.na(landings.data$H_NEW_ITIS)]
landings.data$H_NEW_ITIS<-NULL
names(landings.data)[names(landings.data) %in% "TS_AFS_NAME"]<-"AFS.Name"
names(landings.data)[names(landings.data) %in% "YEAR"]<-"Year"
names(landings.data)[names(landings.data) %in% "POUNDS"]<-"Pounds"
names(landings.data)[names(landings.data) %in% "DOLLARS"]<-"Dollars"
names(landings.data)[names(landings.data) %in% "COLLECTION"]<-"Collection"
statereg<-read.csv(file = paste0(dir.data, "statereg.csv"))
landings.data<-merge(x = landings.data, y = statereg, by = "State.no")

# names(landings.data)<-c("X","SS_CODE","TSN","H_OLD_ITIS","H_NEW_ITIS","HM_OLD_ITIS","HM_NEW_ITIS","TS_AFS_NAME",
#                         "TS_SCIENTIFIC_NAME", "CONF_SPECIES","AFS_NAME_CONF","REGION_NAME","REGION_ID","STATE_NAME",        
#                         "S_STATE_NAME","STATE_ID","S_STATE_ID","AFS_NAME","YEAR","POUNDS", "DOLLARS",
#                         "TOT_COUNT","CONF","SOURCE","COLLECTION","SUMMARY_TYPE","MARKER" )

########***How to split species############
########******Taxonomic#####################
categories<-list("Plantae" = 202422, 
                 "Chromista" = 590735,
                 "Fungi" = 555705,
                 "Bacteria" = 50,
                 "Protozoa" = 43780,
                 "Archaea" = 935939,
                 "Porifera" = 46861, 
                 "Cnidaria" = 48738, 
                 "Platyhelminthes" = 53963, 
                 "Nematoda" = 59490, 
                 "Annelida" = 64357, 
                 
                 "Arthropoda" = 82696, 
                 "Echinodermata" = 156857, 
                 "Mollusca" = 69458, 
                 # "Chordata"  = "phylum", 
                 "Urochordata" = 158853,
                 "Agnatha" = 914178,
                 "Chondrichthyes" = 159785,
                 "Sarcopterygii" = 161048, 
                 "Tetrapoda" = 914181, 
                 "Actinopterygii" = 161061)

spp.cat<-itis_reclassify(tsn = unique(landings.data$Tsn), 
                         categories, 
                         missing.name="Uncategorized")

spp.cat$df.out$category.tax<-spp.cat$df.out$category
spp.cat$df.out$category1.tax<-as.numeric(factor(spp.cat$df.out$category.tax))
landings.data<-merge(x = landings.data, y = spp.cat$df.out, by.y = "TSN", by.x = "Tsn")

#regroup after that
landings.data$category.taxsimp<-landings.data$category.tax
landings.data$category.taxsimp[landings.data$category.tax %in% 
                                 c("Plantae", 
                                   "Chromista",
                                   "Fungi",
                                   "Bacteria",
                                   "Protozoa",
                                   "Archaea",
                                   "Porifera", 
                                   "Cnidaria", 
                                   "Platyhelminthes", 
                                   "Nematoda", 
                                   "Annelida", 
                                   "Uncategorized", "Other")]<-"Other"
landings.data$category.taxsimp[as.character(landings.data$category.tax) %in% 
                                 c("Urochordata", "Agnatha", "Sarcopterygii")]<-"Other Fish"
landings.data$category.taxsimp1<-as.numeric(factor(landings.data$category.taxsimp))
########******Origional#####################
landings.data$category.orig<-"Other"
landings.data$category.orig[landings.data$category.tax %in% 
                              c("Arthropoda", 
                                "Echinodermata", 
                                "Mollusca")]<-"Shellfish" 
landings.data$category.orig[landings.data$category.tax %in% 
                              c("Agnatha",
                                "Chondrichthyes",
                                "Sarcopterygii", 
                                "Actinopterygii")]<-"Finfish" 
landings.data$category1.orig<-as.numeric(factor(landings.data$category.orig))
#barplot(table(landings.data$category.orig))

# ########***Edit Species Names##########
landings.data$AFS_NAME1<-landings.data$AFS.Name
landings.data$AFS_NAME1<-gsub(pattern = " ", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = ",", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "/", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "\\(", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "\\)", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "&", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "\\.\\.", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "\\.\\.", replacement = ".", x = landings.data$AFS_NAME1)
landings.data$AFS_NAME1<-gsub(pattern = "\\.", replacement = "_", x = landings.data$AFS_NAME1)

landings.data$Pounds<-as.numeric(paste0(gsub(pattern = ",", replacement = "", x = landings.data$Pounds)))
landings.data$Dollars<-as.numeric(paste0(gsub(pattern = ",", replacement = "", x = landings.data$Dollars)))

landings.data$State<-as.character(landings.data$State)
landings.data$State<-gsub(pattern = "-", " ", landings.data$State)
landings.data$State<-(tolower(landings.data$State))
landings.data$State<-sapply(landings.data$State, simpleCap)
landings.data$State[landings.data$State %in% "Florida East"]<-"East Florida"
landings.data$State[landings.data$State %in% "Florida West"]<-"West Florida"
landings.data$State[landings.data$State %in% "Hawaii"]<-"Hawai`i" 

# for (i in 1:ncol(landings.data)) {
#   landings.data[,1]<-as.character(landings.data[,1])
# }

write.csv(x = landings.data, file = paste0(dir.rawdata,"landings_edited.csv"))
write.csv(x = landings.data, file = paste0(dir.data,"landings_edited.csv"))


#########Documentation#################
# rmarkdown::render(ProdI.Docu,
#                   output_dir = paste0(dir.docu),
#                   output_file = paste0("ProductivityIndex_Documentation_",date0,".docx"))

