##########*** Landing Data##############
landings.data<-read.csv(file = paste0(dir.data, "landings.csv"))
write.csv(x = landings.data, file = paste0(dir.rawdata,"landings.csv"))

########***How to split species############
########******Taxonomic#####################
categories<-list("Plantae" = "kingdom", 
                 "Chromista" = "kingdom",
                 "Fungi" = "kingdom",
                 "Bacteria" = "kingdom",
                 "Protozoa" = "kingdom",
                 "Archaea" = "kingdom",
                 "Porifera" = "phylum", 
                 "Cnidaria" = "phylum", 
                 "Platyhelminthes" = "phylum", 
                 "Nematoda" = "phylum", 
                 "Annelida" = "phylum", 
                 
                 "Arthropoda" = "phylum", 
                 "Echinodermata" = "phylum", 
                 "Mollusca" = "phylum", 
                 # "Chordata"  = "phylum", 
                 "Urochordata" = "subphylum",
                 "Agnatha" = "infraphylum",
                 "Chondrichthyes" = "class",
                 "Sarcopterygii" = "superclass", 
                 "Tetrapoda" = "superclass", 
                 "Actinopterygii" = "superclass")

spp.cat<-itis_reclassify(tsn = unique(landings.data$Tsn), 
                         categories, 
                         missing.name="Uncategorized")

spp.cat$category1.tax<-as.numeric(factor(spp.cat$category.tax))
landings.data<-merge(x = landings.data, y = spp.cat, by.y = "TSN", by.x = "Tsn")

#regroup after that
landings.data$category.taxsimp<-as.character(landings.data$category.tax)
landings.data$category.taxsimp[as.character(landings.data$category.tax) %in% 
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
