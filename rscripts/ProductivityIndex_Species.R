  ####***Intitial edit of commerical.data######

  commercial.data <- read.csv(file = paste0(dir.data, "/fis_foss_coastal_ws-foss_landings_mv.csv"))
  
  commercial.data<-commercial.data[, c("TSN", "STATE_NAME", "YEAR", "TS_AFS_NAME", "POUNDS",
                                       "DOLLARS", "COLLECTION")]#, "REGION_ID")]
  
  commercial.data<-commercial.data[commercial.data$COLLECTION %in% "Commercial",]
  commercial.data$COLLECTION<-NULL
  names(commercial.data)<-c("TSN", "State", "year", "CommonName", "POUNDS", "DOLLARS")
  commercial.data<-commercial.data[commercial.data$year %in% minyr:maxyr,]
  
  #####***State Codes#####
  commercial.data$State<-simpleCap(commercial.data$State)
  commercial.data$State[commercial.data$State %in% "Florida-east"]<-"East Florida"
  commercial.data$State[commercial.data$State %in% "Florida-west"]<-"West Florida"
  commercial.data$State[commercial.data$State %in% "Hawaii"]<-"Hawai`i"
  
  # commercial.data<-merge(x = statereg, y = commercial.data, by = "State")
  
  ######***Apply Updated TSN Codes############
  fisc_itis_history<-read.csv(file = paste0(dir.data, "fis_foss-fisc_itis_history.csv"))
  fisc_itis_history<-fisc_itis_history[,c("NEW_ITIS", "OLD_ITIS")]
  
  commercial.data$TSN.old<-commercial.data$TSN
  
  for (i in 1:length(unique(fisc_itis_history$OLD_ITIS))){
    old<-unique(fisc_itis_history$OLD_ITIS)[i]
    new<-unique(fisc_itis_history$NEW_ITIS[fisc_itis_history$OLD_ITIS %in% old])
    
    #Find the correct common name
    common<-unique(commercial.data$CommonName[commercial.data$TSN %in% c(new)])
    common<-unique(commercial.data$CommonName[commercial.data$TSN %in% c(old, new)])
    common<-common[is.na(as.numeric(common))]
    if (length(common) %in% 0) {
      common<-""
    } else if (length(common)>0) {
      common.new<-unique(commercial.data$CommonName[commercial.data$TSN %in% c(new)])
      
      if (length(common.new)>0){
        common<-common.new[1]
      } else if (length(common.new)>0) {
        common<-common[1]
      }
    }
    commercial.data$CommonName[commercial.data$TSN %in% c(old, new)]<-common
    #Get most up to date TSN number
    commercial.data$TSN[commercial.data$TSN %in% old]<-new
  }
  
  
  
  
  ## Load and minorly edit NMFSToTSNTranslation Reference Data
  
  NMFSToTSNTranslation <- read.csv(paste0(dir.data, '/NMFS to TSN translation.csv'), 
                                   header=TRUE, stringsAsFactors = FALSE)
  names(NMFSToTSNTranslation)<-c("TSN", "CommonName", "SCIENTIFIC_NAME", "NMFS", "Reported.by.Sources")
  NMFSToTSNTranslation<-NMFSToTSNTranslation[, names(NMFSToTSNTranslation) %in% 
                                               c("TSN", "CommonName", "SCIENTIFIC_NAME", "NMFS", "Reported.by.Sources")]
  
  TSNHistory<-read.csv(paste0(dir.data, 'fis_foss-fisc_itis_history.csv'), 
                       header=TRUE, stringsAsFactors = FALSE)
  TSNHistory<-TSNHistory[,c("OLD_ITIS", "NEW_ITIS")]
  
  for (i in 1:nrow(TSNHistory)){
    if (sum(NMFSToTSNTranslation$TSN %in% TSNHistory$OLD_ITIS[i])>1) {
      # print(i)
      NMFSToTSNTranslation$TSN[NMFSToTSNTranslation$TSN %in% TSNHistory$OLD_ITIS[i]]<-TSNHistory$NEW_ITIS[i]
    }
  }
  
  a<-NMFSToTSNTranslation[!(NMFSToTSNTranslation$NMFS %in% 
                              names(table(NMFSToTSNTranslation$NMFS)[table(NMFSToTSNTranslation$NMFS)>1])),]
  
  b<-NMFSToTSNTranslation[NMFSToTSNTranslation$NMFS %in% 
                            names(table(NMFSToTSNTranslation$NMFS)[table(NMFSToTSNTranslation$NMFS)>1]),]
  cc<-c()
  for (i in 1:length(unique(b$NMFS))){
    nmfs<-unique(b$NMFS)[i]
    if ( sum(grepl(pattern = "CLS:88", x = b$Reported.by.Sources[b$NMFS %in% nmfs], ignore.case = T))>0) {
      bb<-b[which(b$NMFS %in% nmfs)[grep(pattern = "CLS:88", x = b$Reported.by.Sources[b$NMFS %in% nmfs], ignore.case = T)],]
    } else if ( sum(grepl(pattern = "SWFSC:9", x = b$Reported.by.Sources[b$NMFS %in% nmfs], ignore.case = T))>0 ) {
      bb<-b[which(b$NMFS %in% nmfs)[!(grepl(pattern = "SWFSC:9", x = b$Reported.by.Sources[b$NMFS %in% nmfs], ignore.case = T))],]
    } else if ( sum(grepl(pattern = "HQHIST:10", x = b$Reported.by.Sources[b$NMFS %in% nmfs], ignore.case = T))>0 ) {
      bb<-b[which(b$NMFS %in% nmfs)[grep(pattern = "HQHIST:10", x = b$Reported.by.Sources[b$NMFS %in% nmfs], ignore.case = T)],]
    }
    cc<-rbind.data.frame(cc, bb)
  }
  NMFSToTSNTranslation<-rbind.data.frame(a, cc)
  
  NMFSToTSNTranslation$Reported.by.Sources<-NULL
  
  ## Load and minorly edit species.codes Reference Data
  
  # species.codes <- read.csv(paste0(dir.data, '/olo_species_codes.csv'), header=TRUE, stringsAsFactors = FALSE)
  # species.codes<-unique(species.codes)
  # species.codes$AFS_NAME<-trimws(species.codes$AFS_NAME)
  # species.codes$FUS_NAME[which(species.codes$AFS_NAME=="POLLOCK, WALLEYE")]<-"Pollock, Alaska"
  # species.codes$NAME[which(species.codes$AFS_NAME=="POLLOCK, WALLEYE")]<-"Pollock,Alaska"
  # species.codes$AFS_NAME[which(species.codes$AFS_NAME=="POLLOCK, WALLEYE")]<-"POLLOCK, ALASKA"
  
  # ####NMFSToITIS###
  # NMFSToITIS<-NMFSToTSNTranslation
  # NMFSToITIS0<-data.frame()
  # for (i in 1:length(unique(NMFSToITIS$NMFS))) {
  #   id<-unique(NMFSToITIS$NMFS)[i]
  #   temp<-NMFSToITIS[NMFSToITIS$NMFS %in% id, ]
  #   if (nrow(temp)>1) {
  #     NMFSToITIS0<-rbind.data.frame(NMFSToITIS0, temp[1,])
  #   } else {
  #     NMFSToITIS0<-rbind.data.frame(NMFSToITIS0, temp)
  #   }
  # }
  # NMFSToITIS<-NMFSToITIS0
  # NMFSToITIS$Sources<-"NMFSToTSNTranslation"
  # 
  # 
  # NMFSToITIS0<-cbind.data.frame("TSN" = NA, 
  #                               "CommonName" = (species.codes$AFS_NAME[!(species.codes$species %in%
  #                                                                          unique(NMFSToITIS$NMFS))]),                                                "SCIENTIFIC_NAME" = NA,
  #                               "NMFS" = (species.codes$species[!(species.codes$species %in% 
  #                                                                   unique(NMFSToITIS$NMFS))]),
  #                               
  #                               "Sources" = "species.codes")
  # 
  # NMFSToITIS<-rbind.data.frame(NMFSToITIS, 
  #                              unique(NMFSToITIS0))
  # 
  # 
  # NMFSToITIS0<-cbind.data.frame("TSN" = NA, 
  #                               "CommonName" = NA, 
  #                               "SCIENTIFIC_NAME" = NA,
  #                               "NMFS" = unique(commercial.data.old$SPECIES[!(commercial.data.old$SPECIES %in% unique(NMFSToITIS$NMFS))]),                                              
  #                               "Sources" = "commercial.data.old")
  # NMFSToITIS<-rbind.data.frame(NMFSToITIS, 
  #                              NMFSToITIS0)
  
  ####ITISToNMFS###
  ITISToNMFS<-NMFSToTSNTranslation
  ITISToNMFS0<-data.frame()
  for (i in 1:length(unique(ITISToNMFS$TSN))) {
    id<-unique(ITISToNMFS$TSN)[i]
    temp<-ITISToNMFS[ITISToNMFS$TSN %in% id, ]
    if (nrow(temp)>1) {
      ITISToNMFS0<-rbind.data.frame(ITISToNMFS0, temp[1,])
    } else {
      ITISToNMFS0<-rbind.data.frame(ITISToNMFS0, temp)
    }
  }
  ITISToNMFS<-ITISToNMFS0
  ITISToNMFS$Sources<-"NMFSToTSNTranslation"
  
  
  
  ITISToNMFS0<-cbind.data.frame("TSN" = unique(commercial.data$TSN[!(commercial.data$TSN %in%
                                                                       unique(ITISToNMFS$TSN))]), 
                                "CommonName" = NA, 
                                "SCIENTIFIC_NAME" = NA,
                                "NMFS" = NA, 
                                "Sources" = "commercial.data.new")
  
  ITISToNMFS<-rbind.data.frame(ITISToNMFS, 
                               ITISToNMFS0)
  
  ###***Merge data####
  
  commercial.data<-commercial.data[!(commercial.data$TSN %in% c(0)),]
  
  commercial.data<-aggregate.data.frame(x = commercial.data[,names(commercial.data) %in%
                                                              c("POUNDS", "DOLLARS")],
                                        by = list("TSN" = commercial.data$TSN, 
                                                  "State" = commercial.data$State,
                                                  "year" = commercial.data$year),
                                        FUN = sum, na.rm = T)
  
  
  commercial.data<-merge(x = commercial.data, 
                         y = ITISToNMFS[,c("TSN", "CommonName")], 
                         by = "TSN")
  
  commercial.data$CommonName<-trimws(commercial.data$CommonName)
  
  commercial.data<-merge(x = commercial.data, 
                         y = statereg, 
                         by = "State")
  
  
  ######***GENERAL########
  SpCodeName.General<-list(
    "All" = c(unique(commercial.data$TSN)), 
    "Finfish" = 914179, # Infraphylum	Gnathostomata
    "Shellfish" = c(82696, # Phylum	Arthropoda  – Artrópode, arthropodes, arthropods
                    69458), # Phylum	Mollusca  – mollusques, molusco, molluscs, mollusks     
    
    ###
    ########********Crustaceans#####
    ###
    'Arthropoda' = 82696, # Phylum	Arthropoda  – Artrópode, arthropodes, arthropods
    'American lobster' = 97314, #Species: Homarus americanus H. Milne Edwards, 1837 – valid
    'Shrimp' = c(96106, # Infraorder	Caridea Dana, 1852
                 83688, # Order	Anostraca G. O. Sars, 1867 – brine shrimp, fairy shrimp
                 621194, # Superfamily	Callianassoidea Dana, 1852
                 97294, #  Infraorder	Stenopodidea Claus, 1872
                 95600), # Suborder	Dendrobranchiata Bate, 1888
    'Mantis Shrimp' = 99140, # Order	Stomatopoda Latreille, 1817 – mantis shrimp, mantis shrimps
    'Penaeidae Shirmp' = 95602, # Family	Penaeidae Rafinesque, 1815 – penaeid shrimps, crevettes pénaéïdes
    'Crab'= c(#95599, # Order	Decapoda Latreille, 1802 – crabs, crayfishes, lobsters, prawns, shrimp, crabes, crevettes, écrevisses, homards # Has previously also included all decapods
      98276, #Infraorder	Brachyura Latreille, 1802 – short-tailed crabs, true crabs
      97698), #  Infraorder	Anomura MacLeay, 1838
    'Sea urchins' = 157821, #Class	Echinoidea Leske, 1778 – heart urchins, sand dollars, sea urchins, châtaignes de mer, clypéastres, oursins, bolacha da praia, equinóide, ouriço do mar, heart urchins, sand dollars, sea urchins, urchins
    'Pac Spiny lobster' = 97650, # Species	Panulirus interruptus (J. W. Randall, 1840) – California spiny lobster
    'Crawfish' = 97306, # Superfamily	Astacoidea Latreille, 1802 – crayfishes
    'Lobsters' = c(97660, # Family	Scyllaridae Latreille, 1825 – slipper lobsters
                   97646), # Family	Palinuridae Latreille, 1802 – spiny lobsters
    'Stone crab' = 98810, # Genus	Menippe De Haan, 1833
    'Blue crab' = 98696, # Species	Callinectes sapidus M. J. Rathbun, 1896 – blue crab, bluepoint
    'Caribbean Spiny lobster' = 97648, # Species	Panulirus argus (Latreille, 1804) – Caribbean spiny lobster
    'Red Crab' = 620992, # Species	Chaceon quinquedens (S. I. Smith, 1879) – red deepsea crab
    'Horseshoe crab' = 82703, # Species	Limulus polyphemus (Linnaeus, 1758) – Atlantic horseshoe crab, American horseshoe crab
    
    'Abalone' = 69492, #  Family	Haliotidae Rafinesque, 1815 – abalone # 'Abalone*',
    'Cockle' = 80865, #  Family	Cardiidae Lamarck, 1809 # 'Cockle',
    'Limpets' = 566854, # Order	Patellogastropoda  # 'LIMPETS',
    'Periwinkles' = 70395, # Genus	Littorina Ferussac, 1822 # 'PERIWINKLES',
    
    
    ###
    ########********Other############
    ###
    'Bloodworms' = 66107, # Species	Glycera dibranchiata Ehlers, 1868
    'Coral' = 51938, # Class	Anthozoa Ehrenberg, 1834 – corals, flower animals, sea anemones, anémones de mer, coraux, água viva, anêmona, antozoário, caravela, corais, gorgônia
    
    
    ###
    ########********General Fish##############
    ###
    
    #######All
    'Flounders' = c(553179, #  Family	Paralichthyidae  – flétans de sable, lenguados areneros, sand flounders, lefteye flounders 
                    172714, # Family	Bothidae  – lefteyed flounders, lefteye flounders, lenguados chuecos, turbots flounders
                    172859), # Family	Pleuronectidae  – halibuts, dabs, righteyed flounders, platijas, plies, righteye flounders    
    'Righteyed flounders' = 172859, #  Family	Pleuronectidae  – halibuts, dabs, righteyed flounders, platijas, plies, righteye flounders
    'Flatfish' = c(172707), #Suborder	Pleuronectoidei
    'Dolphinfish' = 168790, # Genus	Coryphaena Linnaeus, 1758
    'Cusk-eel' = 164839, 
    'Menhaden' = 161731, #Genus	Brevoortia Gill, 1861 – menhadens
    'Herring' = 551153, # Subfamily	Clupeinae  – herrings, sardines, sprats
    'Atlantic Herring' = 161722, # Species	Clupea harengus Linnaeus, 1758 – Baltic herring, herring, hareng atlantique, Atlantic herring
    'Hogchoker' = 172982, # Species	Trinectes maculatus (Bloch & Schneider, 1801) – hogchoker, suela tortilla, Hogchoker
    'Plaice' = 172900, # Genus	Pleuronectes Linnaeus, 1758 – plaices, northern flounders
    'Swordfish' = 172480, #Family	Xiphiidae  – swordfishes, espadas, espadons
    'Goosefish' = c(#164500, #Species	Lophius gastrophysus Miranda Ribeiro, 1915 – blackfin goosefish, rape pescador 
      164498), # Genus	Lophius Linnaeus, 1758
    'Sablefish' = 167123, # Species	Anoplopoma fimbria (Pallas, 1814) – sablefish, bacalao negro
    'Snappers' = 168845, # Family	Lutjanidae  – sea perches, snappers, perches de mer, vivaneaux, fusiliers, pargos y huachinangos    
    'Albacore tuna' = 172419, # Species	Thunnus alalunga (Bonnaterre, 1788) – atún blanco, albacore, longfinned albacore, albacora, germon
    'Bluefin tuna' = 172421, # Species	Thunnus thynnus (Linnaeus, 1758) – bluefin tuna, atún aleta azul, atún, horse mackerel, northern bluefin tuna, Atlantic bluefin tuna, thon rouge
    'Tunas' = c(638252, # Tribe	Thunnini Starks, 1910
                -172454, # Genus	Auxis Cuvier, 1829 – frigate mackerels, frigate tunas
                -172459), # Genus	Allothunnus Serventy, 1948
    'Mullets' = 170333, # Family	Mugilidae  – mullets, grey mullets, lisas, muges
    'Pollock' = 164726, # Genus	Pollachius Nilsson, 1832 – pollocks
    'Salmon' = c(161996, # Species	Salmo salar Linnaeus, 1758 – Atlantic salmon, saumon atlantique
                 161975, # Species	Oncorhynchus gorbuscha (Walbaum, 1792) – pink salmon, humpback, humpbacked salmon, saumon rose
                 161976, # Species	Oncorhynchus keta (Walbaum in Artedi, 1792) – chum salmon
                 161977, # Species	Oncorhynchus kisutch (Walbaum, 1792) – coho salmon, salmón plateado, saumon coho, silver salmon
                 161979, # Species	Oncorhynchus nerka (Walbaum in Artedi, 1792) – blueback salmon, kokanee, red salmon, sockeye salmon, saumon rouge
                 161980 # Species	Oncorhynchus tshawytscha (Walbaum in Artedi, 1792) – Chinook salmon, salmón boquinegra, king salmon, saumon chinook
    ), 
    'Ladyfish' = 161110, # Genus	Elops Linnaeus, 1766 – ladyfishes
    'Oilfish' = 172364, # Species	Ruvettus pretiosus Cocco, 1833 – oilfish, escolar, escolar clavo
    'Anchovies' = 553173, #  Family	Engraulidae  – anchois, anchoas, anchovies
    'Croaker' = c(169258, # Genus	Bairdiella Gill, 1861 – mademoiselle, striped croakers
                  169282, #  Genus	Micropogonias Bonaparte, 1831 – finebarbel croakers
                  169357, # Genus	Cheilotrema Tschudi, 1846
                  169256, # Genus	Genyonemus Gill, 1861
                  169359, # Genus	Roncador Jordan and Gilbert, 1880
                  169297), # Genus	Umbrina Cuvier, 1816 – croakers, roncadores
    'American John Dory' = 166284, # Species	Zenopsis conchifera (Lowe, 1852) – buckler dory, San Pedro plateado, American john dory
    'Grenadiers' = 165332, # Family	Macrouridae Gilbert and Hubbs, 1916 – grenadiers, rattails, grenadiers, marcrouridés, granaderos
    'Lumpfish' = 167611, # Genus	Cyclopterus Linnaeus, 1758 – lumpfishes
    'Octopus' = 82590, #  Family	Octopodidae D'Orbigny, 1839-1842 in Férussac and D'Orbigny, 1834-1848
    'Pomfrets' = 170288, # Genus	Brama Bloch and Schneider, 1801 – pomfrets
    'Sardine' = c(161728, # Genus	Sardinops Hubbs, 1929
                  161812, # Genus	Sardina Antipa, 1904
                  161762), # Genus	Sardinella Valenciennes in Cuvier and Valenciennes, 1847
    'Shad' = c(161701, # Genus	Alosa Linck, 1790 – river herrings
               161736), # Genus	Dorosoma Rafinesque, 1820 – gizzard shads
    'Thornyhead' = 166782, # 'Genus	Sebastolobus Gill, 1881
    
    
    ###
    ########********Atlantic Fish####
    ###
    
    'American eel' = 161127, # Species	Anguilla rostrata (Lesueur, 1817) – American eel, anguila, anguila americana, anguille d'Amérique
    'Atlantic Cod' = 164712, # Species	Gadus morhua Linnaeus, 1758 – morue de l'Atlantique, bacalao del Atlántico, cod, rock cod, morue franche, Atlantic cod 
    'Atlantic haddock' = 164744, # Species	Melanogrammus aeglefinus (Linnaeus, 1758) – haddock, aiglefin
    'Haddock' = 164744, # Species	Melanogrammus aeglefinus (Linnaeus, 1758) – haddock, aiglefin
    'Atlantic mackerel' = 172414, #Species	Scomber scombrus Linnaeus, 1758 – caballa del Atlántico, maquereau commun, maquereau bleu, Atlantic mackerel
    'Atlantic pollock' = 164727, #Species	Pollachius virens (Linnaeus, 1758) – pollock, coalfish, carbonero, lieu noir, saithe, goberge 
    'Atlantic croaker' = 169283, # Species	Micropogonias undulatus (Linnaeus, 1766) – Atlantic croaker, roncadina, gurrubata
    'Atlantic Wolffish' = 171336, # Genus	Anarhichas Linnaeus, 1758 – Atlantic wolffishes
    'Alewife' = 161706, # Species	Alosa pseudoharengus (Wilson, 1811) – alewife, kyak, gaspareau, bigeye herring, branch herring, freshwater herring, gray herring, white herring, sawbelly, grayback
    'Black sea bass' = 167687, # Species	Centropristis striata (Linnaeus, 1758) – black sea bass
    'Porgies' = 169206, # Genus	Pagrus Cuvier, 1816
    'Striped bass' = 167680, # Species	Morone saxatilis (Walbaum, 1792) – rockfish, striped bass, lobina estriada, bar rayé
    'Spot' = 169267, # VSpecies	Leiostomus xanthurus Lacepède, 1802 – spot, croca
    'King mackerel' = 172435, # Species	Scomberomorus cavalla (Cuvier, 1829) – king mackerel, sierra, carito, carite lucio, thazard serra
    'Scups' = 169181, # Genus	Stenotomus Gill, 1865
    'Spanish mackerel' = 172436, # Species	Scomberomorus maculatus (Mitchill, 1815) – serrucho, sierra común, carite Atlántico, thazard Atlantique, Atlantic Spanish mackerel, Spanish mackerel    
    'Tilefish' = 168537, #Family	Malacanthidae  – tilefishes, blanquillos, tiles
    
    'Black drum' = 169288, # Species	Pogonias cromis (Linnaeus, 1766) – black drum, corvina negra, tambor negro
    'Groupers' = 643094, # Tribe	Epinephelini  
    'Gag grouper' = 167759, # 167759 – Mycteroperca microlepis (Goode and Bean, 1879) – valid – abadejo, charcoal belly, gag
    'Red grouper' = 167702, # Species	Epinephelus morio (Valenciennes in Cuvier and Valenciennes, 1828) – red grouper, cherna americana
    'Red snapper' = 168853, # Species	Lutjanus campechanus (Poey, 1860) – red snapper, pargo colorado, huachinango del Golfo, northern red snapper
    'Atlantic Bonito' = 172409, #  Species	Sarda sarda (Bloch, 1793) – bonito, bonito del Atlántico, bonite à dos rayé, bonito Atlántico, bloater, bone jack, Boston mackerel, common bonito, Atlantic bonito
    'Billfishes' = 172486, # Family	Istiophoridae  – billfishes, sailfishes, marlins, spearfishes, picudos, voiliers
    'White perch' = 167678, #Species	Morone americana (Gmelin, 1789) – white perch, baret
    'Vermilion snapper' = 168909, # Rhomboplites aurorubens (Cuvier in Cuvier and Valenciennes, 1829) – vermilion snapper, cotorro, besugo
    'Weakfish' = 169241, #Species	Cynoscion regalis (Bloch and Schneider, 1801) – weakfish, gray trout, sea trout
    'Butterfish' = 172567, # Species	Peprilus triacanthus (Peck, 1804) – palometa estrecha, butterfish, stromatée à fossettes
    'Offshore Hake' = 164793, # Species	Merluccius albidus (Mitchill, 1818) – offshore hake, offshore whiting
    'Blueline Tilefish' = 168543, #  Species	Caulolatilus microps Goode and Bean, 1878 – blueline tilefish, blanquillo lucio
    'Golden Tilefish' = 168546, # Species	Lopholatilus chamaeleonticeps Goode and Bean, 1879 – blue tilefish, tilefish, conejo amarillo
    'Monkfish' = 164499 , #Species	Lophius americanus Valenciennes in Cuvier and Valenciennes, 1837 – goosefish, monkfish, baudroie d'Amérique
    'Acadian Redfish' = 166774, # Species	Sebastes fasciatus Storer, 1854 – Acadian redfish, Labrador redfish, Acadian rockfish, sébaste acadien
    'American Plaice Flounder' = 172877, # Species	Hippoglossoides platessoides (Fabricius, 1780) – American plaice, plie canadienne, American dab, Canadian plaice, dab, American Plaice
    'Bluefish' = 168559, # Species	Pomatomus saltatrix (Linnaeus, 1766) – bluefish, anjova
    
    
    ###
    ########********Pacific Fish############
    ###
    
    'Halibut' = c(172931, # Genus	Hippoglossus Cuvier, 1816 – halibuts
                  172929), #  Genus	Reinhardtius Gill, 1861
    'Pacific halibut' = 172932, #Species: Hippoglossus stenolepis Schmidt, 1904 – valid
    'Atlantic halibut' = 172933, # Species	Hippoglossus hippoglossus (Linnaeus, 1758) – Atlantic halibut, flétan atlantique, Atlantic Halibut
    'Summer flounder' = 172735, #Species	Paralichthys dentatus (Linnaeus, 1766) – summer flounder, fluke, cardeau d'été, Summer Flounder
    'Winter Flounder' = 172905, # Species	Pseudopleuronectes americanus (Walbaum, 1792) – plie rouge, blackback, Georges Bank flounder, lemon sole, rough flounder, winter flounder, Winter Flounder
    'Witch Flounder' = 172873, #Species	Glyptocephalus cynoglossus (Linnaeus, 1758) – witch flounder, gray sole, plie grise, Witch Flounder
    'Yellowtail Flounder' = 172909, #Species	Limanda ferruginea (Storer, 1839) – limande à queue jaune, rusty flounder, yellowtail flounder, Yellowtail Flounder
    'Sand Dab Flounder' = 172746, #Species	Scophthalmus aquosus (Mitchill, 1815) – windowpane, brill, sand dab, spotted flounder, turbot de sable, Windowpane
    'Windowpane Flounder' = 172746, #Species	Scophthalmus aquosus (Mitchill, 1815) – windowpane, brill, sand dab, spotted flounder, turbot de sable, Windowpane
    'Pacific salmon' = c(
      # 161974 # Genus	Oncorhynchus Suckley, 1861 – Pacific salmon
      161975, # Species	Oncorhynchus gorbuscha (Walbaum, 1792) – pink salmon, humpback, humpbacked salmon, saumon rose
      161976, # Species	Oncorhynchus keta (Walbaum in Artedi, 1792) – chum salmon
      161977, # Species	Oncorhynchus kisutch (Walbaum, 1792) – coho salmon, salmón plateado, saumon coho, silver salmon
      161979, # Species	Oncorhynchus nerka (Walbaum in Artedi, 1792) – blueback salmon, kokanee, red salmon, sockeye salmon, saumon rouge
      161980 # Species	Oncorhynchus tshawytscha (Walbaum in Artedi, 1792) – Chinook salmon, salmón boquinegra, king salmon, saumon chinook
      # 161989 # Species	Oncorhynchus mykiss (Walbaum, 1792) – rainbow trout, trucha arcoiris, steelhead, truite arc-en-ciel, redband trout
    ),  
    'Ocean pout' = 630979, # Species	Zoarces americanus (Bloch and Schneider, 1801) – ocean pout, loquette d'Amérique
    'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
    # 'Pacific herring' = 551209, #Species	Clupea pallasii Valenciennes in Cuvier and Valenciennes, 1847 – arenque del Pacífico, Pacific herring
    'Pacific hake (whiting)' = 164792, #Species	Merluccius productus (Ayres, 1855) – North Pacific hake, whiting, Pacific hake, merluza norteña, Pacific whiting
    'Pacific sardine' = 161729,
    'Pacific sanddab' = 172716, # Species	Citharichthys sordidus (Girard, 1854) – Pacific sanddab, lenguado moteado, limande sordide, Pacific Sanddab
    'Arrowtooth flounder' = 172862, # Species	Atheresthes stomias (Jordan & Gilbert, 1880) – arrowtooth flounder, Arrowtooth Flounder, plie à grande bouche
    'Sole' = 172980, # Family	Soleidae  – soles, soles, suelas soles
    'Yellow sole' = 172907, # Species	Limanda aspera (Pallas, 1814) – yellowfin sole, limande à nageoires jaunes, Yellowfin Sole
    'Alaska Pollock' = c(#164722, #invalid - subsequent name/combination 
      934083), # Species	Gadus chalcogrammus Pallas, 1814 – Walleye    
    'Rockfish' = 166705, # Genus	Sebastes Cuvier, 1829 – rockfishes, rockcod, rosefishes
    'Atka mackerel'= 167119, #Genus	Pleurogrammus Gill, 1861 – atka mackerels
    
    'Hake' = c(164729, #Genus	Urophycis Gill, 1863 – codlings 
               164790), # Genus	Merluccius Rafinesque, 1810 – hakes
    'White hake' = 164732, # Species	Urophycis tenuis (Mitchill, 1814) – white hake, mud hake, merluche blanche
    'Red hake' = c(164730 #  Species	Urophycis chuss (Walbaum, 1792) – red hake, squirrel hake, merluche-écureuil
    ),#164729), # Genus	Urophycis Gill, 1863 – codlings # Toledo, includes other hake
    'Silver hake' = c(164791 #Species	Merluccius bilinearis (Mitchill, 1814) – silver hake, merlu argenté
    ), # 164790), # Genus	Merluccius Rafinesque, 1810 – hakes # Toledo, includes other hake
    'Lingcod' = 167116, # Species	Ophiodon elongatus Girard, 1854 – lingcod, molva
    'White seabass' = 169387, # Species	Atractoscion nobilis (Ayres, 1860) – white seabass, corvina cabaicucho
    
    ###
    ########********Cartilagenous Fish###########
    ###
    
    'Skate' = 160845, # Family	Rajidae Blainville, 1816 – rayas, rays, skates, raies
    'Skates' = 160845, # Family	Rajidae Blainville, 1816 – rayas, rays, skates, raies
    'Dogfish Shark' = 160604, # Family	Squalidae Blainville, 1816 – chiens de mer, cazones aguijones, dogfish sharks, spiny dogfishes
    'Spiny dogfish' = 160617, #Species	Squalus acanthias Linnaeus, 1758 – cazón espinoso común, piked dogfish, spiny dogfish, galludo espinoso, aiguillat commun, dogfish, grayfish, spurdog
    'Sharks' = c(563987 # Superorder	Euselachii #TOLEDO - some of these are sawfish, rays, and etc. 
                 # 551500, # Order	Carcharhiniformes  – ground sharks
                 # 159788, # Order	Heterodontiformes  – bullhead sharks
                 # 159810, # Order	Hexanchiformes Compagno, 1973 – cow sharks, frilled sharks
                 # 159851, # Order	Lamniformes  – mackerel sharks
                 # 551499, # Order	Orectolobiformes  – gatas nodrizas, requins-tapis, tiburones tapiceros, carpet sharks
                 # 551498, # Order	Pristiophoriformes  – saw sharks
                 # 160602, #Order	Squaliformes Compagno, 1973 – dogfish sharks
                 # 563990 # Order	Squatiniformes 
    ), 
    'Thresher' = 159915, # Genus	Alopias Rafinesque, 1810
    "Rosette skate" = 564136,# Species	Leucoraja garmani (Whitley, 1939) – rosette skate
    'Little Skate' = 564130,#Species	Leucoraja erinacea (Mitchill, 1825) – raie-hérisson, common skate, little skate, summer skate
    'Winter Skate' = 564145,#Species	Leucoraja ocellata (Mitchill, 1815) – raie tachetée, big skate, eyed skate, winter skate
    'Barndoor Skate' = 564139,#Species	Dipturus laevis (Mitchill, 1818) – grande raie, barndoor skate
    'Smooth Skate' = 564151,# Species	Malacoraja senta (Garman, 1885) – raie à queue de velours, smooth skate
    'Thorny Skate' = 564149,#Species	Amblyraja radiata (Donovan, 1808) – raie épineuse, starry skate, thorny skate
    'Clearnose Skate' = 160855,#Species	Raja eglanteria Bosc in Lacepède, 1800 – raya naricita, clearnose skate
    
    
    ###
    ########********Reef Fish############
    ###
    
    'Bigeye' = 168176, # Family	Priacanthidae  – bigeyes, catalufas, beauclaires, catalufas
    'Butterflyfishes' = 169554, # Family	Chaetodontidae  – butterflyfishes, peces mariposa, poissons-papillons
    'Coney' = 167740, # Species	Cephalopholis fulva (Linnaeus, 1758) – guatívere, cabrilla roja, coney
    'Gag' = 167759, # Species	Mycteroperca microlepis (Goode and Bean, 1879) – gag, abadejo, charcoal belly
    'Glasseye snapper' = 168181, # Species	Heteropriacanthus cruentatus (Lacepède, 1801) – catalufa espinosa, glasseye snapper, catalufa roquera
    'Graysby' = 181220, # Species	Cephalopholis cruentata (Lacepède, 1802) – enjambre, cherna enjambre, graysby
    # 'Groupers' = 551018, #  Subfamily	Epinephelinae 
    'Hogfish' = c(170565, # Genus	Lachnolaimus Cuvier, 1829
                  170482), # Genus	Bodianus Bloch, 1790 – hogfishes
    'Jacks' = c(168605, # Genus	Caranx Lacepède, 1801 – cavallas, crevallies, bigeye jacks,
                168688), #  Genus	Seriola Cuvier, 1816 – amberfishes, amberjacks, yellowtails
    'Green Jobfish' = 168926, # Species	Aprion virescens Valenciennes in Cuvier and Valenciennes, 1830 – blue-gray snapper, green jobfish
    'Mackerel' = 638253, # Tribe	Scombrini Bonaparte, 1831 – mackerels
    'Pompano' = 168707, # Genus	Trachinotus Lacepède, 1801 – pompanos
    'Blackbelly Rosefish' = 166787, # Species	Helicolenus dactylopterus (Delaroche, 1809) – blackbelly rosefish
    'Runners' = 168737, # Genus	Elagatis Bennett, 1840 – runners
    'Scamp' = 167763, # 'Species	Mycteroperca phenax Jordan and Swain, 1884 – scamp, abadejo garropa
    'Scorpionfish' = 166704, # Family	Scorpaenidae  – firefishes, goblinfishes, rockfishes, scorpionfishes, escorpiones y rocotes, rascasses, scorpènes, scorpénidés
    'Sheepshead' = 169189, # Species	Archosargus probatocephalus (Walbaum, 1792) – sheepshead, sargo chopa
    'Snapper' = 168845, #  Family	Lutjanidae  – sea perches, snappers, perches de mer, vivaneaux, fusiliers, pargos y huachinangos
    'Spanish flag' = 167798, # Species	Gonioplectrus hispanus (Cuvier in Cuvier and Valenciennes, 1828) – Spanish flag, biajaiba de lo alto, cherna bandera
    'Squirrelfishes' = 166170, # 'Family	Holocentridae  – soldierfishes, squirrelfishes, candiles, marignans
    'Surgeonfishes' = 172250, # Family	Acanthuridae  – surgeonfishes, tangs, cirujanos, poissons-chirurgiens
    'Tilefish' = 168537, # Family	Malacanthidae  – tilefishes, blanquillos, tiles
    'Wenchman' = c(168913, # 'Species	Pristipomoides aquilonaris (Goode and Bean, 1896) – wenchman, huachinango navaja
                   168914), # Species	Pristipomoides freemani Anderson, 1966 – yelloweye wenchman, slender wenchman
    # 'YELLOWTAIL'
    
    
    ###
    ########********Mollescus#############
    ###
    'Mollusca' = 69458, # Phylum	Mollusca  – mollusques, molusco, molluscs, mollusks    
    'Sea scallop' = 79718, #Species	Placopecten magellanicus (Gmelin, 1791) – sea scallop
    'Squid' = 555706, # Superorder	Decabrachia Boettger, 1952
    'Oysters' = c(79777, # Family	Spondylidae Gray, 1826
                  79857, # Family	Gryphaeidae Vyalov, 1936
                  79866), #  Family	Ostreidae Rafinesque, 1815
    'Clams' = 80384, #Order	Veneroida H. Adams and A. Adams, 1856
    'Mussels' = c(79451, #Family	Mytilidae Rafinesque, 1815
                  79913), #  Family	Unionidae Rafinesque, 1820 # TOLEDO - why include freshwater spp?
    'Blue mussel' = 79454, #Species	Mytilus edulis Linnaeus, 1758 – edible blue mussel, blue mussel
    'Conchs' = 72554, # Family	Strombidae Rafinesque, 1815 (Conches)
    'Whelks' = 74069, # Family	Melongenidae Gill, 1867 (Whelks)
    'Snails' = 72878, # Family	Naticidae Guilding, 1834 (Snails)
    'Loligo squid' = 82370, # Genus	Loligo Lamarck, 1798
    'Illex Squid' = 82520, # Genus	Illex Steenstrup, 1880
    'Atlantic surf clam' = 80944, #Species	Spisula solidissima (Dillwyn, 1817) – Atlantic surfclam
    'Arctic surf (Stimpson) clam' = 80983, #Species	Mactromeris polynyma (Stimpson, 1860) – Arctic surfclam
    'Softshell clam' = 81692, #  Species	Mya arenaria Linnaeus, 1758 – softshell clam, softshell
    "Scallops" = 79611, # Family	Pectinidae Rafinesque, 1815
    'Eastern oyster' = 79872, # Species	Crassostrea virginica (Gmelin, 1791) – eastern oyster
    
    
    'Quahog clams' =  81495, # Genus	Mercenaria Schumacher, 1817     
    'Ocean quahog clam' = 81343, # Species	Arctica islandica (Linnaeus, 1767) – ocean quahog
    'Snails' = 69459, # Class	Gastropoda Cuvier, 1797 – gastropods, slugs, snails, escargots, gastéropodes, limaces, caracol, caramujo, lesma
    
    ###
    ########********Hawai`i##########
    ###
    'Lobsters (*ula*)' = c(206946), # Superfamily	Palinuroidea Latreille, 1802
    'Dolphinfish (*mahimahi*)' = 168790, # Genus	Coryphaena Linnaeus, 1758
    "Marlin (*a'u*)" = c(#172486, # Family	Istiophoridae  – billfishes, sailfishes, marlins, spearfishes, picudos, voiliers TOLEDO
      172490), #Genus	Makaira Lacepède, 1802 – marlins, blue marlin
    'Moonfish (*opah*)' = 166326, # Species	Lampris guttatus (Brünnich, 1788) – opah, opah, pez mariposa
    'Pomfrets (*Monchong*)' = 170287, #Family	Bramidae  – pomfrets, castagnoles, tristones
    'Scad (*opelu*)' = c(168723, # Genus	Decapterus Bleeker, 1851 – mackerel scads, round scads
                         168585, # Genus	Trachurus Rafinesque, 1810 – saurels
                         168676), # Genus	Selar Bleeker, 1851 – bigeyed scads, goggle-eyes, gogglers 
    'Hawaii Snappers' = c(168181, #Heteropriacanthus cruentatus (Lacepède, 1801) – catalufa espinosa, glasseye snapper, catalufa roquera
                          168845), #Family	Lutjanidae  – sea perches, snappers, perches de mer, vivaneaux, fusiliers, pargos y huachinangos
    'Wahoo (*ono*)' = 172451, #Species	Acanthocybium solandri (Cuvier in Cuvier and Valenciennes, 1832) – peto, wahoo, thazard-bâtard
    
    
    ###
    ########********Freshwater################
    ###
    'True perches' = 168356, # Family	Percidae  – true perches, perches, percas, perches et dards, perches
    'Quillback' = 163916, # ' Genus	Carpiodes Rafinesque, 1820 – quillbacks, carpsuckers
    'Silversides' = 630579, #  Family	Atherinopsidae  – charales y pejerreyes, New World silversides, poissons d'argent
    'Sturgeon' = 161064, # Family	Acipenseridae  – sturgeons, esturgeons, esturiones
    'Suckers' = 163892, # Family	Catostomidae  – suckers, matalotes, catostomes
    'Sunfishes' = 168093, # Family	Centrarchidae  – sunfishes, achigans et crapets, lobinas ####TOLEDO this is what data uses, but should be more specific
    'Tilapias' = c(170014, # Genus	Oreochromis Günther, 1889 – blue tilapias
                   169809, # Genus	Tilapia Smith, 1840 – African mouthbrooders, speckled tilapias
                   553244), # Genus	Sarotherodon Rüppell, 1852 – brushtooth tilapias
    'Trout' = c(61989, # Species	Oncorhynchus mykiss (Walbaum, 1792) – rainbow trout, trucha arcoiris, steelhead, truite arc-en-ciel, redband trout
                161987, # Species	Oncorhynchus aguabonita (Jordan, 1892) – golden trout, California golden trout
                161981, # Species	Oncorhynchus apache (Miller, 1972) – Apache trout, Arizona trout
                623485, # Species	Oncorhynchus iwame Kimura and Nakamura, 1961, Iwame trout
                161992, # Species	Oncorhynchus chrysogaster (Needham and Gard, 1964) – trucha dorada mexicana, Mexican golden trout
                161983, # Species	Oncorhynchus clarkii (Richardson, 1836) – cutthroat trout, trucha degollada, truite fardée
                914076, # Species	Oncorhynchus penshinensis (Pallas, 1814), Kamchatkan rainbow trout
                161985), #  Species	Oncorhynchus gilae (Miller, 1950) – Gila trout
    # 'TURTLE*',
    'Walleye' = c(650173, # Species	Sander vitreus (Mitchill, 1818) – walleye, doré jaune
                  168508), # invalid - subsequent name/combination 	Valid Name:	Sander vitreus (Mitchill, 1818
    'Whitefish' = 623284, # Subfamily	Coregoninae 
    'Rock Bass' = 168097, # Species	Ambloplites rupestris (Rafinesque, 1817) – rock bass, crapet de roche
    'Bowfin' = 161100, # Order	Amiiformes  – bowfin
    'Buffalofishes' = 163954, # Genus	Ictiobus Rafinesque, 1820 – buffalo suckers, buffalo fishes
    'Burbot' = 164725,# Species	Lota lota (Linnaeus, 1758) – burbot, eelpout, lotte
    'Carp' = c(163690, #  Genus	Hypophthalmichthys Bleeker, 1860 – bighead carps
               163349, # Genus	Carassius Nilsson, 1832 – Crucian carps
               163678, # Genus	Cirrhinus Oken, 1817 
               163343, # Genus	Cyprinus Linnaeus, 1758 – common carps
               638893, # Genus	Mylopharyngodon Peters, 1881
               163536), # Genus	Ctenopharyngodon Steindachner, 1866 – grass carps herbívora
    'Freshwater Catfish' = c(163995, # Ictaluridae       family 163995
                             164066), # Siluridae       family 164066
    'Chubs' = c(163541, # Genus	Gila Baird and Girard, 1853 – western chubs
                163495, # Genus	Hybopsis Agassiz, 1854 – bigeye chubs
                163374, # Genus	Semotilus Rafinesque, 1820 – creek chubs
                163562, # Genus	Hemitremia Cope, 1870 – flame chub
                163881, # Genus	Platygobio Gill, 1863 – flathead chubs
                163391, # Genus	Nocomis Girard, 1856 – hornyhead chubs
                163534, # Genus	Couesius Jordan, 1878 – lake chubs
                163566, # Genus	Iotichthys Jordan and Evermann, 1896 – least chubs	
                163878, # Genus	Oregonichthys Hubbs in Schultz, 1929 – Oregon chubs
                687586, # Genus	Petroleuciscus Bogutskaya, 2002
                163819, # Genus	Erimystax Jordan, 1882 – slender chubs
                638870, # Genus	Algansea Girard, 1856
                639152, # Genus	Siphateles Cope, 1883 – tui chubs
                913987, # Species	Lepidomeda copei (Jordan and Gilbert, 1881) – leatherside chub, northern leatherside chub
                163863), # Genus	Macrhybopsis Cockerell and Allison, 1909 – blacktail chubs
    'Carppie' = 168165, # Genus	Pomoxis Rafinesque, 1818 – crappies
    'Crayfishes or crawfishes' = 97306,# Superfamily	Astacoidea Latreille, 1802 – crayfishes
    'Freshwater Drum' = 169363, #  Genus	Aplodinotus Rafinesque, 1819 – river drums, thunderpumpers, freshwater drums
    # 'FINFISHES, FW, OTHER',
    'Gars' = 650224, # Order	Lepisosteiformes 
    # 'GOLDFISH', # covered by carps
    'Mooneyes' = 161903, # Family	Hiodontidae  – mooneyes, laquaiches, ojos de luna
    
    
    'Catfish' = c(163992), #, Siluriformes        order 
    
    
    ###
    ########********Inshore and Miscellaneous############
    ###
    'Stingrays' = 649685, # Order	Myliobatiformes  – stingrays
    'Alligator* Land' = c(201897, # Species	Atractosteus spatula (Lacepède, 1803) – alligator gar, catán
                          668671, # Species	Macrochelys temminckii (Troost in Harlan, 1835) – Alligator Snapping Turtle
                          551771), # Species	Alligator mississippiensis (Daudin, 1802) – American Alligator, Aligator americano, alligator, gator, Florida alligator, Mississippi alligator, Louisiana alligator
    # 'Bass',
    'Echinoderm' = 156857, # Phylum	Echinodermata Bruguière, 1791 [ex Klein, 1734] – echinoderms, échinodermes, bolacha da praia, equinoderma, equinodermata, estrela do mar, ouriço do mar
    'Frogs' = 173423, # Order	Anura  – Frogs, perereca, rã, sapo, crapauds, grenouilles, Toads
    # 'HERRING, PACIFIC, ROE ON KELP',
    'Jellyfish' = 51483, # Class	Scyphozoa Götte, 1887 – jellyfish, cup animals, jellyfishes, méduses, água viva, Cifozoa, cifozoário, meduza
    # 'LEATHER-BACK',
    'Permit' = 168709, # Species	Trachinotus falcatus (Linnaeus, 1758) – permit, pámpano, pámpano palometa
    'Stingrays' = 160960, #  Genus	Gymnura van Hasselt, 1823 – stingrays, butterfly rays
    'Sea Cucumber' = 158140, # Class	Holothuroidea  – holotúria, pepino do mar, cocombres de mer, sea cucumbers
    'Seaweed' = 660055, #  Class	Phaeophyceae 
    'Sponges' = 46861, # Phylum	Porifera Grant, 1836 – sponges, éponges, esponja, porifero
    'Starfish' = 156862, #  Class	Asteroidea de Blainville, 1830 – étoiles de mer, stelléridés, estrela do mar, sea stars, starfishes, starfish, estrella de mar
    'Rock bass' = 168097, # Species	Ambloplites rupestris (Rafinesque, 1817) – rock bass, crapet de roche
    
    ###
    ########********Bait############
    ###
    
    'Ballyhoo' = 165459, # Genus	Hemiramphus Cuvier, 1816
    # 'FINFISHES, UNC BAIT AND ANIMAL FOOD',
    'Mummichog' = 165647, # Species	Fundulus heteroclitus (Linnaeus, 1766) – mummichog, choquemort
    'Sandworms' = 65891 # Species	Neanthes virens (Sars, 1835)
    
    
    
    
  )
  
  ######***STATE and UNITED STATES########
  ### Species names and codes for the state tables
  
  #########******United States#############
  SpCodeName<-list(
    "United States" = list('American lobster' = SpCodeName.General$`American lobster`, 
                           'Blue crab' = SpCodeName.General$`Blue crab`,
                           'Menhaden' = SpCodeName.General$Menhaden,
                           'Pacific halibut' = SpCodeName.General$`Pacific halibut`,
                           'Pacific salmon' = SpCodeName.General$`Pacific salmon`,  
                           'Sablefish' = SpCodeName.General$Sablefish,
                           'Sea scallop' = SpCodeName.General$`Sea scallop`,
                           'Shrimp' = SpCodeName.General$Shrimp,
                           'Tunas' = SpCodeName.General$Tunas,
                           'Alaska pollock' = SpCodeName.General$`Alaska Pollock`),
    
    #####******North Pacific Region#####
    #Alaska
    "Alaska" = list('Atka mackerel'=  SpCodeName.General$`Atka mackerel`,
                    'Crab'= SpCodeName.General$Crab,
                    'Flatfish' = SpCodeName.General$Flatfish,
                    'Pacific cod' = SpCodeName.General$`Pacific cod`,
                    'Pacific halibut' = SpCodeName.General$`Pacific halibut`,
                    'Pacific herring' = SpCodeName.General$`Herring`,
                    'Rockfish' = SpCodeName.General$Rockfish,
                    'Sablefish' = SpCodeName.General$Sablefish,
                    'Salmon' = SpCodeName.General$`Pacific salmon`,
                    'Alaska Pollock' = SpCodeName.General$`Alaska Pollock`),
    
    "North Pacific"= list('Atka mackerel'=  SpCodeName.General$`Atka mackerel`,
                          'Crab'= SpCodeName.General$Crab,
                          'Flatfish' = SpCodeName.General$Flatfish,
                          'Pacific cod' = SpCodeName.General$`Pacific cod`,
                          'Pacific halibut' = SpCodeName.General$`Pacific halibut`,
                          'Pacific herring' = SpCodeName.General$`Herring`,
                          'Rockfish' = SpCodeName.General$Rockfish,
                          'Sablefish' = SpCodeName.General$Sablefish,
                          'Salmon' = SpCodeName.General$`Pacific salmon`,
                          'Alaska Pollock' = SpCodeName.General$`Alaska Pollock`
    ),
    
    #####******Pacific Region#####
    "Pacific" = list('Albacore tuna' = SpCodeName.General$`Albacore tuna`, 
                     'Crab' = SpCodeName.General$Crab, 
                     'Flatfish' = SpCodeName.General$Flatfish, 
                     'Pacific hake (whiting)' = SpCodeName.General$`Pacific hake (whiting)`, 
                     'Other shellfish' = c(SpCodeName.General$Arthropoda, 
                                           SpCodeName.General$Mollusca, 
                                           -(SpCodeName.General$Crab),
                                           -(SpCodeName.General$Shrimp), 
                                           -(SpCodeName.General$Squid)),
                     'Rockfish' = SpCodeName.General$Rockfish, 
                     'Sablefish' = SpCodeName.General$Sablefish, 
                     'Salmon' = SpCodeName.General$`Pacific salmon`, 
                     'Shrimp' = SpCodeName.General$Shrimp,
                     'Squid' = SpCodeName.General$Squid),
    
    
    #California
    "California" = list('Crab'= SpCodeName.General$Crab,
                        'Pacific sardine' = SpCodeName.General$`Pacific sardine`, 
                        'Rockfish' = SpCodeName.General$Rockfish,
                        'Sablefish' = SpCodeName.General$Sablefish,
                        'Salmon' = SpCodeName.General$`Pacific salmon`,
                        'Sea urchins' = SpCodeName.General$`Sea urchins`,
                        'Shrimp' = SpCodeName.General$Shrimp,
                        'Spiny lobster' = SpCodeName.General$`Pac Spiny lobster`,
                        'Squid' = SpCodeName.General$Squid,
                        'Swordfish' = SpCodeName.General$Swordfish), 
    
    ## Oregon
    "Oregon" = list('Albacore tuna' = SpCodeName.General$`Albacore tuna`,
                    'Crab' = SpCodeName.General$Crab,
                    'Flatfish' = SpCodeName.General$Flatfish,
                    'Pacific hake (whiting)' = SpCodeName.General$`Pacific hake (whiting)`,
                    'Oysters' = SpCodeName.General$Oysters,
                    'Pacific sardine' = SpCodeName.General$`Pacific sardine`,
                    'Rockfish' = SpCodeName.General$Rockfish,
                    'Sablefish' = SpCodeName.General$Sablefish,
                    'Salmon' = SpCodeName.General$`Pacific salmon`, 
                    'Shrimp' = SpCodeName.General$Shrimp
    ), 
    
    #Washington
    "Washington" = list('Albacore tuna' = SpCodeName.General$`Albacore tuna`,
                        'Clams' = SpCodeName.General$Clams, 
                        'Crab' = SpCodeName.General$Crab,
                        'Pacific hake (whiting)' = SpCodeName.General$`Pacific hake (whiting)`,
                        'Pacific halibut' = SpCodeName.General$`Pacific halibut`,
                        'Mussels' = SpCodeName.General$Mussels, 
                        'Oysters' = SpCodeName.General$Oysters,
                        'Sablefish' = SpCodeName.General$Sablefish,
                        'Salmon' = SpCodeName.General$`Pacific salmon`, 
                        'Shrimp' = SpCodeName.General$Shrimp
    ), 
    
    ########******Western Pacific Region (Hawaii)#######
    ########Hawaii
    "Hawai`i" = list('Lobsters (*ula*)' = SpCodeName.General$`Lobsters (*ula*)`,
                     'Dolphinfish (*mahimahi*)' = SpCodeName.General$`Dolphinfish (*mahimahi*)`,
                     "Marlin (*a'u*)" = SpCodeName.General$`Marlin (*a'u*)`,
                     'Moonfish (*opah*)' = SpCodeName.General$`Moonfish (*opah*)`,
                     'Pomfrets (*Monchong*)' = SpCodeName.General$`Pomfrets (*Monchong*)`,
                     'Scad (*opelu*)' = SpCodeName.General$`Scad (*opelu*)`,
                     'Snappers' = SpCodeName.General$`Hawaii Snappers`,
                     'Swordfish (*mekajiki*)' = SpCodeName.General$Swordfish,
                     'Tunas (*aku*)' = SpCodeName.General$Tunas,
                     'Wahoo (*ono*)' = SpCodeName.General$`Wahoo (*ono*)`
    ),
    
    "Western Pacific (Hawai`i)" = list('Lobsters (*ula*)' = SpCodeName.General$`Lobsters (*ula*)`,
                                       'Dolphinfish (*mahimahi*)' = SpCodeName.General$`Dolphinfish (*mahimahi*)`,
                                       "Marlin (*a'u*)" = SpCodeName.General$`Marlin (*a'u*)`,
                                       'Moonfish (*opah*)' = SpCodeName.General$`Moonfish (*opah*)`,
                                       'Pomfrets (*Monchong*)' = SpCodeName.General$`Pomfrets (*Monchong*)`,
                                       'Scad (*opelu*)' = SpCodeName.General$`Scad (*opelu*)`,
                                       'Snappers' = SpCodeName.General$`Hawaii Snappers`,
                                       'Swordfish (*mekajiki*)' = SpCodeName.General$Swordfish,
                                       'Tunas (*aku*)' = SpCodeName.General$Tunas,
                                       'Wahoo (*ono*)' = SpCodeName.General$`Wahoo (*ono*)`
    ),
    ########******New England Region########
    "New England" = list('American lobster' = SpCodeName.General$`American lobster`, 
                         'Atlantic herring' = SpCodeName.General$`Herring`, 
                         'Atlantic mackerel' = SpCodeName.General$`Atlantic mackerel`, 
                         'Bluefin tuna' = SpCodeName.General$`Bluefin tuna`, 
                         'Cod and haddock' = c(SpCodeName.General$`Atlantic Cod`, 
                                               SpCodeName.General$`Atlantic haddock`), 
                         'Flounders' = SpCodeName.General$Flounders, 
                         'Goosefish' = SpCodeName.General$Goosefish,
                         'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                         'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                         'Squid' = SpCodeName.General$Squid), 
    
    
    ## Connecticut
    "Connecticut"=list('American lobster' = SpCodeName.General$`American lobster`, 
                       'Summer flounder' = SpCodeName.General$`Summer flounder`, 
                       'Other flounders' = c(SpCodeName.General$Flounders, 
                                             -(SpCodeName.General$`Summer flounder`)), #exclude
                       'Goosefish' = SpCodeName.General$Goosefish,
                       'Red hake' = SpCodeName.General$`Silver hake`,
                       'Silver hake' = SpCodeName.General$`Red hake`,
                       'Scups or Porgies' = c(SpCodeName.General$Scups, 
                                              SpCodeName.General$Porgies),
                       'Sea scallop' = SpCodeName.General$`Sea scallop`,
                       'Whelks and Conchs' = c(SpCodeName.General$Conchs,
                                               SpCodeName.General$Whelks,
                                               SpCodeName.General$Snails),
                       'loligo squid' = SpCodeName.General$`Loligo squid`
    ), 
    
    
    ## Maine
    "Maine" = list('American lobster' = SpCodeName.General$`American lobster`, 
                   'Atlantic herring' = SpCodeName.General$`Herring`,
                   'Bloodworms' = SpCodeName.General$Bloodworms, 
                   'Blue mussel' = SpCodeName.General$`Blue mussel`,
                   'Cod and haddock' = c(SpCodeName.General$`Atlantic Cod`, 
                                         SpCodeName.General$`Atlantic haddock`), 
                   'Goosefish' = SpCodeName.General$Goosefish,
                   'Ocean quahog clam' = SpCodeName.General$`Ocean quahog clam`, 
                   'Pollock' = SpCodeName.General$`Atlantic pollock`,
                   'Sea urchins' = SpCodeName.General$`Sea urchins`,
                   'Softshell clam' = SpCodeName.General$`Softshell clam`
    ), 
    
    ## Massachusetts
    "Massachusetts" = list('American lobster' = SpCodeName.General$`American lobster`, 
                           
                           'Atlantic herring' = SpCodeName.General$`Herring`,
                           'Atlantic mackerel' = SpCodeName.General$`Atlantic mackerel`,
                           'Other Clams' = c(SpCodeName.General$Clams, #TOLEDO check
                                             -(SpCodeName.General$`Eastern oyster`), #exclude
                                             -(SpCodeName.General$`Ocean quahog clam`),  #exclude
                                             -(SpCodeName.General$`Sea scallop`)), #exclude
                           'Cod and haddock' = c(SpCodeName.General$`Atlantic Cod`, 
                                                 SpCodeName.General$`Atlantic haddock`), 
                           'Eastern oyster' = SpCodeName.General$`Eastern oyster`, 
                           'Flounders' = SpCodeName.General$Flounders, 
                           'Goosefish' = SpCodeName.General$Goosefish,
                           'Ocean quahog clam' = SpCodeName.General$`Ocean quahog clam`, 
                           'Sea scallop' = SpCodeName.General$`Sea scallop`), 
    
    ## New Hampshire 
    "New Hampshire" = list('American lobster' = SpCodeName.General$`American lobster`, 
                           'Atlantic herring' = SpCodeName.General$`Herring`,
                           'Atlantic cod' = SpCodeName.General$`Atlantic Cod`, 
                           'Goosefish' = SpCodeName.General$Goosefish,                        
                           'Haddock' = SpCodeName.General$`Atlantic haddock`, 
                           'Hake' = SpCodeName.General$Hake, 
                           'Pollock' = SpCodeName.General$`Atlantic pollock`, 
                           'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                           'Shrimp' = SpCodeName.General$Shrimp, 
                           'Spiny dogfish' = SpCodeName.General$`Spiny dogfish`), 
    ## Rhode Island
    "Rhode Island" = list('Other flounders' = c(SpCodeName.General$Flounders, 
                                                -(SpCodeName.General$`Summer flounder`)), #exclude
                          'American lobster' = SpCodeName.General$`American lobster`, 
                          'Atlantic herring' = SpCodeName.General$`Herring`, 
                          'Atlantic mackerel' = SpCodeName.General$`Atlantic mackerel`, 
                          'Goosefish' = SpCodeName.General$Goosefish,
                          'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                          'Scups and porgies' = c(SpCodeName.General$Scups, 
                                                  SpCodeName.General$Porgies), #changed from "or" to "and"
                          'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                          'Squid' = SpCodeName.General$Squid, 
                          'Summer flounder' = SpCodeName.General$`Summer flounder`
    ), 
    
    ########******Mid-Atlantic Region########
    "Mid-Atlantic" = list('American lobster' = SpCodeName.General$`American lobster`, 
                          'Atlantic surf clam' = SpCodeName.General$`Atlantic surf clam`, 
                          'Blue crab' = SpCodeName.General$`Blue crab`, 
                          'Eastern oyster' = SpCodeName.General$`Eastern oyster`,
                          'Menhaden' = SpCodeName.General$Menhaden,
                          'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                          'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                          'Squid' = SpCodeName.General$Squid, 
                          'Striped bass' = SpCodeName.General$`Striped bass`, 
                          'Summer flounder' = SpCodeName.General$`Summer flounder`), 
    
    # Delaware
    "Delaware"=list('American eel' = SpCodeName.General$`American eel`, 
                    'Black sea bass' = SpCodeName.General$`Black sea bass`, 
                    'Blue crab' = SpCodeName.General$`Blue crab`, 
                    'Eastern oyster' = SpCodeName.General$`Eastern oyster`, 
                    'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                    'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                    'Spot' = SpCodeName.General$Spot,  
                    'Striped bass' = SpCodeName.General$`Striped bass`, 
                    'Weakfish' = SpCodeName.General$Weakfish, 
                    'Whelks' = SpCodeName.General$Whelks
    ),
    
    # Maryland
    "Maryland"=list('Atlantic croaker' = SpCodeName.General$`Atlantic croaker`, 
                    'Black sea bass' = SpCodeName.General$`Black sea bass`, 
                    'Blue crab' = SpCodeName.General$`Blue crab`, 
                    'Clams or bivalves' = SpCodeName.General$Clams, #TOLEDO Should just be called "Calms"
                    'Eastern oyster' = SpCodeName.General$`Eastern oyster`, 
                    'Menhaden' = SpCodeName.General$Menhaden,
                    'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                    'Striped bass' = SpCodeName.General$`Striped bass`, 
                    'Summer flounder' = SpCodeName.General$`Summer flounder`, 
                    'White perch' = SpCodeName.General$`White perch`
    ), 
    
    # New Jersey
    "New Jersey"=list('American lobster' = SpCodeName.General$`American lobster`, 
                      'Atlantic herring' = SpCodeName.General$`Herring`, 
                      'Atlantic mackerel' = SpCodeName.General$`Atlantic mackerel`, 
                      'Blue crab' = SpCodeName.General$`Blue crab`,
                      'Eastern oyster' = SpCodeName.General$`Eastern oyster`, 
                      'Goosefish' = SpCodeName.General$Goosefish,                 
                      'Ocean quahog and surf clams' = c(SpCodeName.General$`Ocean quahog clam`#, 
                                                        # SpCodeName.General$`Atlantic surf clam` # Shouldn't this be here too?
                      ),
                      #Shouldnt this be 'Quahog clam' = 81495?
                      'Quahog clam' = c(SpCodeName.General$`Ocean quahog clam`, #Isn't this redundant?
                                        SpCodeName.General$`Arctic surf (Stimpson) clam`, #TOLEDO, check, only time called
                                        SpCodeName.General$`surf clams`),  
                      'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                      'Summer flounder' = SpCodeName.General$`Summer flounder`
    ), 
    
    # New York
    "New York"=list('American lobster' = SpCodeName.General$`American lobster`, 
                    'Atlantic surf clam' = SpCodeName.General$`Atlantic surf clam`, 
                    'Eastern oyster' = SpCodeName.General$`Eastern oyster`, 
                    'Summer flounder' = SpCodeName.General$`Summer flounder`,
                    'Loligo squid' = SpCodeName.General$`Loligo squid`, 
                    'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                    'Scups and porgies' = c(SpCodeName.General$Scups, 
                                            SpCodeName.General$Porgies), 
                    'Sea scallop' = SpCodeName.General$`Sea scallop`,
                    'Softshell clam' = SpCodeName.General$`Softshell clam`, 
                    'Tilefishes' = SpCodeName.General$Tilefish), 
    
    # Virginia 
    "Virginia"=list('Atlantic croaker' = SpCodeName.General$`Atlantic croaker`, 
                    'Black sea bass' = SpCodeName.General$`Black sea bass`, 
                    'Blue crab' = SpCodeName.General$`Blue crab`, 
                    'Goosefish' = SpCodeName.General$Goosefish,            
                    'Menhaden' = SpCodeName.General$Menhaden,
                    'Oysters' = SpCodeName.General$Oysters, 
                    'Sea Scallop' = SpCodeName.General$`Sea scallop`, 
                    'Spot' = SpCodeName.General$Spot, 
                    'Striped bass' = SpCodeName.General$`Striped bass`, 
                    'Summer flounder' = SpCodeName.General$`Summer flounder`
    ), 
    
    ########******South Atlantic Region########
    "South Atlantic" = list('Blue crab' = SpCodeName.General$`Blue crab`, 
                            'Clams' = SpCodeName.General$Clams, 
                            'Flounders' = SpCodeName.General$Flounders, 
                            'Groupers' = SpCodeName.General$Groupers, 
                            'King mackerels' = SpCodeName.General$`King mackerel`,
                            'Oysters' = SpCodeName.General$Oysters, 
                            'Shrimp' = SpCodeName.General$Shrimp, 
                            'Snappers' = SpCodeName.General$Snappers, 
                            'Swordfish' = SpCodeName.General$Swordfish, 
                            'Tunas' = SpCodeName.General$Tunas), 
    
    # East Florida
    "East Florida"=list('Blue crab' = SpCodeName.General$`Blue crab`, 
                        'Clams' = SpCodeName.General$Clams, 
                        'Groupers' = SpCodeName.General$Groupers, 
                        'King mackerel' = SpCodeName.General$`King mackerel`,
                        'Lobsters' = SpCodeName.General$Lobsters, 
                        'Sharks' = SpCodeName.General$Sharks,
                        'Shrimp' = SpCodeName.General$Shrimp, 
                        'Snappers' = SpCodeName.General$Snappers, 
                        'Spanish mackerel' = SpCodeName.General$`Spanish mackerel`, 
                        'Swordfish' = SpCodeName.General$Swordfish
    ), 
    
    # st.sp.codes[["East Florida"]][[4]] <- c(1939, 1940) # East Florida, king mackerel
    
    # Georgia
    # groupers: 1410:1430, 1850, 3800, 4740 groupers
    "Georgia"=list('Blue crab' = SpCodeName.General$`Blue crab`, 
                   'Clams' = SpCodeName.General$Clams, 
                   'Grouper' = SpCodeName.General$Groupers, 
                   'Shrimp' = SpCodeName.General$Shrimp, 
                   'Snails (conchs)' = c(SpCodeName.General$Conchs), #TOLEDO was snails - why?!
                   'Snappers' = SpCodeName.General$Snappers
    ), 
    
    # North Carolina
    "North Carolina"=list('Atlantic croaker' = SpCodeName.General$`Atlantic croaker`, 
                          'Black sea bass' = SpCodeName.General$`Black sea bass`, 
                          'Blue crab' = SpCodeName.General$`Blue crab`, 
                          'Clams' = SpCodeName.General$Clams, 
                          'Flounders' = SpCodeName.General$Flounders,
                          'Groupers' = SpCodeName.General$Groupers, 
                          'King mackerel' = SpCodeName.General$`King mackerel`, 
                          'Shrimp' = SpCodeName.General$Shrimp, 
                          'Snappers' = SpCodeName.General$Snappers, 
                          'Tunas' = SpCodeName.General$Tunas
    ), 
    
    # South Carolina
    "South Carolina"=list('Black sea bass' = SpCodeName.General$`Black sea bass`, 
                          'Blue crab' = SpCodeName.General$`Blue crab`, 
                          'Clams' = SpCodeName.General$Clams, 
                          'Groupers' = SpCodeName.General$Groupers, 
                          'Oysters' = SpCodeName.General$Oysters,
                          'Sharks' = SpCodeName.General$Sharks,
                          'Shrimp' = SpCodeName.General$Shrimp, 
                          'Snappers' = SpCodeName.General$Snappers, 
                          'Swordfish' = SpCodeName.General$Swordfish, 
                          'Tilefish' = SpCodeName.General$Tilefish),
    
    ########******Gulf of Mexico Region########
    "Gulf of Mexico" = list('Blue crab' = SpCodeName.General$`Blue crab`, 
                            'Crawfish' = SpCodeName.General$Crawfish, 
                            'Groupers' = SpCodeName.General$Groupers, 
                            'Menhaden' = SpCodeName.General$Menhaden,
                            'Mullets' = SpCodeName.General$Mullets,
                            'Oysters' = SpCodeName.General$Oysters, 
                            'Red snapper' = SpCodeName.General$`Red snapper`,
                            'Shrimp' = SpCodeName.General$Shrimp, 
                            'Spiny lobster' = SpCodeName.General$`Caribbean Spiny lobster`, 
                            'Tunas' = SpCodeName.General$Tunas), 
    
    ## Alabama
    "Alabama"=list('Blue crab' = SpCodeName.General$`Blue crab`, 
                   'Flounders' = SpCodeName.General$Flatfish, 
                   'Menhaden' = SpCodeName.General$Menhaden,
                   'Mullets' = SpCodeName.General$Mullets, 
                   'Oysters' = SpCodeName.General$Oysters, 
                   'Red snapper' = SpCodeName.General$`Red snapper`, 
                   'Sharks' = SpCodeName.General$Sharks, 
                   'Shrimp' = SpCodeName.General$Shrimp, 
                   'Spanish mackerel' = SpCodeName.General$`Spanish mackerel`,
                   'Vermilion snapper' = SpCodeName.General$`Vermilion snapper`
    ),
    
    ## West Florida
    "West Florida" = list('Blue crab' = SpCodeName.General$`Blue crab`, 
                          'Gag' = SpCodeName.General$Gag, 
                          'Lobsters' = SpCodeName.General$Lobsters, 
                          'Mullet' = SpCodeName.General$Mullets, 
                          'Oyster' = SpCodeName.General$Oysters, 
                          'Quahog clam' = SpCodeName.General$`Quahog clam`,
                          'Red grouper' = SpCodeName.General$`Red grouper`, 
                          'Red snapper' = SpCodeName.General$`Red snapper`, 
                          'Shrimp' = SpCodeName.General$Shrimp, 
                          'Stone crab' = SpCodeName.General$`Stone crab`
    ), 
    ## Louisiana
    "Louisiana"=list('Blue crab' = SpCodeName.General$`Blue crab`, 
                     'Crawfish' = SpCodeName.General$Crawfish, 
                     'King mackerel' = SpCodeName.General$`King mackerel`,
                     'Menhaden' = SpCodeName.General$Menhaden,
                     'Mullets' = SpCodeName.General$Mullets,
                     'Oysters' = SpCodeName.General$Oysters, 
                     'Red snapper' = SpCodeName.General$`Red snapper`, 
                     'Shrimp' = SpCodeName.General$Shrimp, 
                     'Tunas' = SpCodeName.General$Tunas, 
                     'Vermilion snapper' = SpCodeName.General$`Vermilion snapper`
    ), 
    
    ## Mississippi
    "Mississippi"=list('Blue crab' = SpCodeName.General$`Blue crab`, 
                       'Flounders' = SpCodeName.General$Flounders, 
                       'Menhaden' = SpCodeName.General$Menhaden,
                       'Mullets' = SpCodeName.General$Mullets, 
                       'Oysters' = SpCodeName.General$Oysters, 
                       'Red snapper' = SpCodeName.General$`Red snapper`, 
                       'Shrimp' = SpCodeName.General$Shrimp
    ), 
    
    ## Texas 
    "Texas"=list('Atlantic croaker' = SpCodeName.General$`Atlantic croaker`, 
                 'Black drum' = SpCodeName.General$`Black drum`, 
                 'Blue crab' = SpCodeName.General$`Blue crab`, 
                 'Flounders' = SpCodeName.General$Flounders, 
                 'Groupers' = SpCodeName.General$Groupers,
                 'Oysters' = SpCodeName.General$Oysters, 
                 'Red snapper' = SpCodeName.General$`Red snapper`, 
                 'Shrimp' = SpCodeName.General$Shrimp, 
                 'Tunas' = SpCodeName.General$Tunas, 
                 'Vermilion snapper' = SpCodeName.General$`Vermilion snapper`)
  )
  
  
  
  
  #####***spcat.list#####
  spcat.list<-list("General" = SpCodeName.General, 
                   "Areas" = SpCodeName)
  
  
  itis_reclassify<-function(tsn, categories, missing.name){
    
    # Find which codes are in which categories
    
    tsn.indata<-classification(tsn[!(is.na(tsn))], db = 'itis')
    tsn.indata<-tsn.indata[!(names(tsn.indata) %in% 0)]
    valid0<- sciname<-category0<-bottomrank<-sppname<- TSN<-c() 
    
    for (i in 1:length(categories)) {
      
      a<-c()
      for (ii in 1:length(categories[i][[1]])) {
        a<-c(a, list.search(lapply(X = tsn.indata, '[[', 3), categories[i][[1]][[ii]] %in% . ))
      }
      
      if (length(a)!=0) {
        
        sppcode<-names(a)
        
        for (ii in 1:length(sppcode)) {
          TSN<-c(TSN, sppcode[ii])
          
          bottomrank<-c(bottomrank, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$rank[
            nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
          
          category0<-c(category0, names(categories[i]))  
          
          sciname<-c(sciname, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$name[
            nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
          
          valid0<-c(valid0, 
                    ifelse(nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])>1, 
                           "valid", "invalid"))
        }
      }
    }
    
    # for (i in 1:length(tsn.indata)){
    #   TSN[i]<-names(tsn.indata)[i]
    #   if (is.na(tsn.indata[[i]])[1]) {
    #     category0[i]<-"Other"
    #     
    #     #Is any number of the taxonomic classification in the listed categories?
    #   } else if ( sum(as.numeric(tsn.indata[[i]]$id) %in% (as.numeric(unlist(categories))))>0 ) {
    # 
    #     idx<-sort(as.numeric(unlist(categories))[abs((as.numeric(unlist(categories)))) %in%
    #                                                as.numeric(tsn.indata[[i]]$id)],
    #               decreasing = F)
    #     
    #     for (ii in 1:length(idx)) {
    #       
    #       if (idx[ii] > 0) { # If there is 1 positive category code
    #         category0[i]<-gsub(pattern = "[0-9]+", replacement = "", 
    #                                   x = names(unlist(categories)[as.numeric(unlist(categories)) %in% idx[ii] ]) )       
    #         bottomrank[i]<-tsn.indata[[i]]$rank[nrow(tsn.indata[[i]])]
    #         sciname[i]<-tsn.indata[[i]]$name[nrow(tsn.indata[[i]])]
    #       }
    #     }
    #     
    #   }
    #   
    #   
    #   if (is.na(tsn.indata[[i]])[1]) {
    #     valid0[i]<-"NA"
    #   } else if (nrow(tsn.indata[[i]]) %in% 1 &&
    #              !(tsn.indata[[i]]$rank %in% "kingdom")) {
    #     valid0[i]<-"invalid"
    #   }
    #   
    # }
    
    df.out<-data.frame(TSN = TSN, 
                       category = category0, 
                       valid = valid0, 
                       rank = bottomrank, 
                       sciname = sciname )
    
    return(list(df.out, tsn.indata))
  }
  
  
  temp<-itis_reclassify(tsn = as.numeric(paste0(unique(commercial.data$TSN))), 
                        categories = list("Shellfish" = spcat.list$General$Shellfish, 
                                          "Finfish" = spcat.list$General$Finfish), 
                        missing.name="Uncategorized")
  tsn.id<-temp[1][[1]]
  
  if (sum(tsn.id$category %in% c("Other", "Uncategorized"))>0) {
    tsn.id<-tsn.id[!(tsn.id$category %in% c("Other", "Uncategorized")), 
                   c("TSN", "category")]
  }
  

  
  taxspp<-function(spp, spp0, landings.df){
    species<-c()
    notspecies<-c()
    for (ii in 1:length(spp)){
      if (spp[[ii]]$rank[nrow(spp[[ii]])] %in% "species") {
        
        #Find common name
        a<-unique(landings.df$CommonName[landings.df$TSN %in% as.numeric(names(spp[ii]))])
        a<-a[is.na(as.numeric(a))]
        # SPECIES_CROSS_REFERENCE <- read.csv(paste0(
        #   'C:/Users/Emii/Documents/Homework/FEUS/2018/FEUS2018Commercial/data/SPECIES_CROSS_REFERENCE.csv'),
        #   header=TRUE, stringsAsFactors = FALSE)
        # a<-unique(SPECIES_CROSS_REFERENCE$AFS_NAME[SPECIES_CROSS_REFERENCE$ITIS %in% as.numeric(names(spp[ii]))])
        if (!(length(a) %in% 0)) {
          b<-strsplit(x = a, split = ",")[[1]]
          # b<-gsub(pattern = ", Na", replacement = "", x = b)
          c0<-ifelse(length(b) > 1, paste0(b[2], " ", b[1]), b)
          c0<-gsub(pattern = "  ", replacement = " ", x = c0)
          c0<-tolower2(c0, capitalizefirst = T)
          c0<-gsub(pattern = "spp", replacement = "spp.", x = c0)
          c0<-ifelse(grepl(pattern = "spp.", x = c0) &
                       grepl(pattern = spp0, x = c0, ignore.case = T),
                     gsub(pattern = spp0, replacement = "", x = c0, ignore.case = T),
                     c0)
          # c0<-ifelse(bottomrank[i] %in% "genus" & !(grepl(pattern = "spp.", x = c0)),
          #            paste0(c0, " spp."), c0)
          c0<-trimws(c0)
          c0<-tolower2(c0, capitalizefirst = F)
          # commonname[i]<-c0
          species<-c(species,
                     paste0(tolower2(unique(c0))))
        }
        
      }
      
      if (!(spp[[ii]]$rank[nrow(spp[[ii]])] %in% "species")) {
        notspecies<-c(notspecies,
                      paste0(ifelse(spp[[ii]]$rank[nrow(spp[[ii]])] %in% "genus",
                                    paste0("*", (spp[[ii]]$name[nrow(spp[[ii]])]), "*"),
                                    tolower(spp[[ii]]$name[nrow(spp[[ii]])])) ,
                             " ", spp[[ii]]$rank[nrow(spp[[ii]])]))
      }
    }
    return(list("species" = species, 
                "notspecies" = notspecies))
  }
  
  
  spp_reclassify<-function(landings.df, spcat.list, place){
    
    ####
    ####FINFISH AND SHELFISH AND OTHER TOTALS
    ###
    
    temp<-landings.df
    total.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                        by=list(year=temp$year),
                        FUN=sum,na.rm=TRUE)
    
    #Finfish
    temp<-landings.df[landings.df$OFS %in% "F",]
    if (nrow(temp)!=0) {
      finfish.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                            by=list(year=temp$year),
                            FUN=sum, na.rm=TRUE)
      
      if (sum(!(min(landings.df$year):max(landings.df$year) %in% finfish.df$year)) >0){
        
        finfish.df<-rbind.data.frame(finfish.df, 
                                     cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
                                                      "POUNDS" = 0, 
                                                      "DOLLARS" = 0))
        landings.df<-landings.df[order(landings.df$year),]
        
      }
    } else {
      finfish.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
                             POUNDS = 0,
                             DOLLARS = 0)
    }
    
    #Shellfish
    temp<-landings.df[landings.df$OFS %in% "S",]
    if (nrow(temp)!=0) {
      shellfish.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                              by=list(year=temp$year),
                              FUN=sum,na.rm=TRUE)
      
      if (sum(!(min(landings.df$year):max(landings.df$year) %in% shellfish.df$year)) >0){
        
        shellfish.df<-rbind.data.frame(shellfish.df, 
                                       cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
                                                        "POUNDS" = 0, 
                                                        "DOLLARS" = 0))
        shellfish.df<-shellfish.df[order(shellfish.df$year),]
      }
    } else {
      shellfish.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
                               POUNDS = 0,
                               DOLLARS = 0)
    }
    
    
    #Other
    temp<-landings.df[landings.df$OFS %in% "O",]
    if (nrow(temp)!=0) {
      other.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                          by=list(year=temp$year),
                          FUN=sum,na.rm=TRUE)
      
      if (sum(!(min(landings.df$year):max(landings.df$year) %in% other.df$year)) > 0){
        
        other.df<-rbind.data.frame(other.df, 
                                   cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[
                                     !(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
                                     "POUNDS" = 0, 
                                     "DOLLARS" = 0))
        other.df<-other.df[order(other.df$year),]
      }
    } else {
      other.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
                           POUNDS = 0,
                           DOLLARS = 0)
    }
    
    
    land.df<-land.tot<-cbind.data.frame("Total" = total.df$POUNDS,
                                        "Finfish" = finfish.df$POUNDS,
                                        "Shellfish" = shellfish.df$POUNDS,
                                        "Other" = other.df$POUNDS,
                                        "Key Species" = NA)
    rev.df<-rev.tot<-cbind.data.frame("Total" = total.df$DOLLARS,
                                      "Finfish" = finfish.df$DOLLARS,
                                      'Shellfish' = shellfish.df$DOLLARS,
                                      "Other" = other.df$DOLLARS,
                                      "Key Species" = NA)
    price.df<-price.tot<-cbind.data.frame("Key Species" = rep_len(x = NA, length.out = nrow(rev.df)))
    
    
    ####
    ####SPECIES
    ###
    uniquespp<-data.frame()
    
    
    if (!(is.na(spcat.list))) {
      temp<-itis_reclassify(tsn = as.numeric(paste(unique(landings.df$TSN))), 
                            categories = spcat.list$Areas[place][[1]], 
                            missing.name="Uncategorized")
      
      tsn.id<-temp[1][[1]]
      # tsn.id<-tsn.id[!(tsn.id$category %in% c("Other", "Uncategorized")), 
      #                c("TSN", "category")]
      tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
      
      
      land.df<-data.frame(t(land.df))
      rev.df<-data.frame(t(rev.df))
      price.df<-data.frame(t(price.df))
      
      
      #Add species to tables
      land.df$Footnotes<-rev.df$Footnotes<-price.df$Footnotes<-""
      
      for (i in 1:length(names(spcat.list$Areas[place][[1]]))) {
        
        spp0<-sort(names(spcat.list$Areas[place][[1]]))[i]
        
        temp<-landings.df[landings.df$TSN %in% tsn.id$TSN[tsn.id$category %in% spp0],]
        # temp<-temp[!(is.na(temp$POUNDS) & is.na(temp$DOLLARS)), ]
        
        #FOOTNOTES
        Footnotes<-""
        
        #Species that should be included in this sum and footnote
        sppno<-as.numeric(unlist(spcat.list$Areas[place][[1]][spp0]))
        spp<-tsn.indata.foot<-classification(x = sppno[sppno>0], db = 'itis')
        sppnames<-taxspp(spp = tsn.indata.foot, spp0, landings.df)
        species<-sppnames$species
        notspecies<-sppnames$notspecies
        
        spp00<-strsplit(x = spp0, split = ",")
        spp00<-strsplit(x = spp00[[length(spp00)]], split = "and")
        spp00<-gsub(x = unlist(spp00), pattern = "and ", replacement = "", ignore.case = T)
        spp00<-gsub(x = unlist(spp00), pattern = "& ", replacement = "", ignore.case = T)
        spp00<-trimws(spp00)
        
        
        if (!(is.null(species)) && !(tolower(species) %in% tolower(spp00))) {        #If species names the few specific species in the group, then dont footnote
          Footnotes<-""
          #If a specific species (and not a gorup) #No Footnote required
        } else if (length(spp) == 1 & !(spp[[1]]$rank[nrow(spp[[1]])] %in% "species") ) { #If there is only 1 code and its not a specific species
          Footnotes<-paste0("This species group includes species within the ",
                            paste0(ifelse(spp[[1]]$rank[nrow(spp[[1]])] %in% "genus",
                                          paste0("*", (spp[[1]]$name[nrow(spp[[1]])]), "*"),
                                          tolower(spp[[1]]$name[nrow(spp[[1]])])) ,
                                   " ", spp[[1]]$rank[nrow(spp[[1]])]), ".")
        } else if (length(spp) > 1) {
          
          
          if (length(species)>0 & length(notspecies)>0) {
            Footnotes<-paste0("This species group includes species within the ",
                              funct_list(notspecies), " and ", funct_list(species) , ".")
          } else if (length(species)==0 & length(notspecies)>0) {
            Footnotes<-paste0("This species group includes species within the ",
                              funct_list(notspecies), ".")
          } else if (length(species)>0 & length(notspecies)==0) {
            Footnotes<-paste0("This species group includes ",
                              funct_list(species) , ".")
          }
        }
        #Species that should be excluded from this sum and footnote
        if (sum(sppno<0)>0) {
          spp<-tsn.indata.foot<-classification(x = (sppno[sppno<0])*-1, db = 'itis')
          sppnames<-taxspp(spp = tsn.indata.foot, spp0, landings.df)
          species<-sppnames$species
          notspecies<-sppnames$notspecies
          
          if (length(tsn.indata.foot)>1) {
            if (length(species)>0 & length(notspecies)>0) {
              Footnotes<-paste0(Footnotes, " This species group excludes species within the ",
                                funct_list(notspecies), " and, specifically, ", funct_list(species) , ".")
            } else if (length(species)==0 & length(notspecies)>0) {
              Footnotes<-paste0(Footnotes, " This species group excludes species within the ",
                                funct_list(notspecies), ".")
            } else if (length(species)>0 & length(notspecies)==0) {
              Footnotes<-paste0(Footnotes, " This species group excludes ",
                                funct_list(species) , ".")
            }
          }
        }
        
        #COMPILE TABLES
        
        if (nrow(temp) %in% 0) {
          land.df<-rbind.data.frame(land.df, 
                                    c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
          rev.df<-rbind.data.frame(rev.df, 
                                   c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
          price.df<-rbind.data.frame(price.df, 
                                     c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
          
        } else {
          
          #Unique Species
          uniquespp0<-unique(temp[,c("TSN", "CommonName")])
          uniquespp0$Category<-spp0
          uniquespp0$CommonName1<-funct_list(spp00)
          uniquespp<-rbind.data.frame(uniquespp, uniquespp0)  
          
          
          
          temp.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] , 
                             by=list(year=temp$year), 
                             FUN=sum,na.rm=TRUE)
          
          #make sure columns match
          if (sum((!(min(landings.df$year):max(landings.df$year) %in% temp.df$year)) >0)) {
            yr0<-(min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% temp.df$year)]
            temp.df<-rbind.data.frame(temp.df, 
                                      cbind.data.frame('year' = yr0, 
                                                       'POUNDS' = rep_len(x = NA, length.out = length(yr0)), 
                                                       'DOLLARS' = rep_len(x = NA, length.out = length(yr0))))
          }
          temp.df<-temp.df[order(temp.df$year, decreasing = F),]
          
          land.df<-rbind.data.frame(land.df, 
                                    c(temp.df$POUNDS, Footnotes))
          rev.df<-rbind.data.frame(rev.df, 
                                   c(temp.df$DOLLARS, Footnotes))
          price.df<-rbind.data.frame(price.df, 
                                     c(temp.df$POUNDS/temp.df$DOLLARS, Footnotes))
          
        }
        
        rownames(land.df)[length(rownames(land.df))]<-
          rownames(rev.df)[length(rownames(rev.df))]<-
          rownames(price.df)[length(rownames(price.df))]<-tolower2(spp0, capitalizefirst = T)  
      }
      
      
      
      uniquespp$Area<-place
      uniquespp$SciName<-""
      a<-classification(x = uniquespp$TSN, db = 'itis')
      for (i in 1:nrow(uniquespp)){
        if (a[i][[1]]$rank[nrow(a[i][[1]])] %in% "species") {
          uniquespp$SciName[i]<-a[i][[1]]$name[nrow(a[i][[1]])]
        } else {
          uniquespp$SciName[i]<-paste0(a[i][[1]]$name[nrow(a[i][[1]])], " ", a[i][[1]]$rank[nrow(a[i][[1]])])
        }
      } 
      
      
      land.df<-cbind.data.frame(rownames(land.df), land.df)
      rev.df<-cbind.data.frame(rownames(rev.df), rev.df)
      price.df<-cbind.data.frame(rownames(price.df), price.df)
      
    } else {
      
      land.df<-t(land.df)
      land.df<-cbind.data.frame("keyspecies" = rownames(land.df)[-nrow(land.df)], 
                                land.df[-nrow(land.df),], 
                                Footnotes = NA)
      rev.df<-t(rev.df)
      rev.df<-cbind.data.frame("keyspecies" = rownames(rev.df)[-nrow(rev.df)], 
                               rev.df[-nrow(rev.df),], 
                               Footnotes = NA)
      price.df<-t(price.df)
      price.df<-cbind.data.frame("keyspecies" = NA, 
                                 price.df, 
                                 Footnotes = NA)
    }
    
    colnames(land.df)<-colnames(rev.df)<-colnames(price.df)<-c("keyspecies", 
                                                               as.character(min(landings.df$year):
                                                                              max(landings.df$year)), 
                                                               "Footnotes")
    
    #####footnote with species
    return(list("revenue" = rev.df, 
                "landings" = land.df, 
                "price" = price.df, 
                "uniquespp" = uniquespp))
  }
  
  
