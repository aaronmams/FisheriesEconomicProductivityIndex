##########INTRODUCTION####################
### Construct the FEUS Commerical Fisheries state and national tables and output them to csv files
### updated and simplified from MarineEcon_tables.R, *use this version*

#######DIRECTORIES############
dir.in<-getwd()
#Local Directories
dir.output<-paste0(dir.in, "/output/")
dir.data<-paste0(dir.in, "/data/")
dir.out<-paste0(dir.output, "ProductivityIndex_", Sys.Date(), "/")
dir.create(dir.out)
dir.parent<-dirname(dir.in)
dir.presdat<-paste0(dir.in, "/rpresentation/")
dir.pres<-paste0(dir.out, "/Presentation")
dir.create(dir.pres)
dir.scripts<-paste0(dir.in, "/rscripts/")
dir.create(paste0(dir.out, "/rscripts")) 
dir.reports<-paste0(dir.out, "/reports/")
dir.create(paste0(dir.out, "/analyses")) 
dir.analyses<-paste0(dir.out, "/analyses/")
dir.create(paste0(dir.out, "/reports/")) 
dir.create(paste0(dir.out, "/metadata/")) #Save word files
dir.rawdata<-paste0(dir.out, "/rawdata/")
dir.create(paste0(dir.out, "/rawdata/")) 
# dir.figures<-paste0(dir.out, "/figures/")
# dir.create(paste0(dir.out, "/figures")) 
dir.create(paste0(dir.out, "/documentation/")) 
dir.docu<-paste0(dir.out, "/documentation/")
dir.outputtables<-paste0(dir.out, "outputtables/")
dir.create(dir.outputtables)

date0<-""
date00<-paste0(Sys.Date())

ProdI.Run<-paste0(dir.scripts, "ProductivityIndex_Run",date0,".R")
ProdI.Data<-paste0(dir.scripts, "ProductivityIndex_Data",date0,".R")
ProdI.DataDL<-paste0(dir.scripts, "ProductivityIndex_CensusDownloadClean",date0,".R")
ProdI.Funct<-paste0(dir.scripts, "ProductivityIndex_Functions",date0,".R")
ProdI.Report<-paste0(dir.scripts, "ProductivityIndex_Report",date0,".rmd")
ProdI.Docu.Out.P<-paste0(dir.scripts, "ProductivityIndex_Documentation_OutputP",date0,".rmd")
ProdI.Docu.Out.Q<-paste0(dir.scripts, "ProductivityIndex_Documentation_OutputQ",date0,".rmd")
ProdI.Docu.In<-paste0(dir.scripts, "ProductivityIndex_Documentation_Input",date0,".rmd")
ProdI.Pres<-paste0(dir.pres, "ProductivityIndex_Presentation.rmd")

######SAVE WORKING FILES########
#From Specific Files
listfiles<-list.files(path = dir.scripts, pattern = date0) 
listfiles<-listfiles[-(listfiles %in% "old")]

for (i in 1:length(listfiles)){
  file.copy(from = paste0(dir.scripts, listfiles[i]), 
            to = paste0(dir.out, "/rscripts/", listfiles[i]), 
            overwrite = T)
}


#Move most updated  word styles file from reference "common" file to getwd()/rscript
file.copy(from = paste0(dir.scripts, "/word-styles-reference.docx"), 
          to = paste0(dir.out, "/rscripts", "/word-styles-reference.docx"), 
          overwrite = T)

### SOURCE FILES AND DATA #####
#Load data for this section
#Functions specific to this section
source(ProdI.Funct)

# source(ProdI.Data)
landings.data<-read.csv(file = paste0(dir.data, "landings_edited.csv"))
write.csv(x = landings.data, file = paste0(dir.rawdata, "landings_edited.csv"))
maxyr<-max(landings.data$Year)
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico") 

######PreLim Analysis##########
# barplot(table(landings.data$category.tax))
# a<-data.frame(t(table(landings.data[c("category.tax", "Year")])))
# xnames<-as.numeric(paste0(a$Year))
# xnames[!(xnames %in% seq(from = min((as.numeric(xnames))), 
#                          to = max(as.numeric(xnames)), 
#                          by = 10))]<-""
# ggplot(data = a, aes(x = factor(Year), y = Freq, color = category.tax)) +
#   geom_line(aes(group = category.tax)) +
#   geom_point() +
#   theme(
#     panel.grid.major.y = element_line( color=NOAALightBlue, size = .1 ),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     axis.line = element_line( color=NOAALightBlue, size = .1 ),
#     axis.ticks = element_blank(), # remove ticks
#     panel.background = element_blank()
#   )  +
#   scale_x_discrete(labels= xnames) +
#   guides(fill=FALSE)

######MAKE REPORT########
counter<-0


OutputAnalysis<-function(landings.data, category0, baseyr, 
                         state.codes, titleadd,
                         counter, dir.rawdata, dir.reports, pctmiss, dir.figures, dir.outputtables, analysisby = "P", 
                         MinimumNumberOfSpecies = 5) {
  
  dir.analyses1<-paste0(dir.analyses, "/",titleadd, "_", analysisby, "_", 
                        gsub(pattern = "\\.", replacement = "", x = category0), "_pctmiss", 
                        gsub(pattern = "\\.", replacement = "", x = pctmiss*100), "/")
  dir.create(dir.analyses1) 
  dir.reports<-paste0(dir.analyses1, "/reports/")
  dir.create(paste0(dir.analyses1, "/reports/")) 
  dir.figures<-paste0(dir.analyses1, "/figures/")
  dir.create(paste0(dir.analyses1, "/figures/")) 
  dir.outputtables<-paste0(dir.analyses1, "/outputtables/")
  dir.create(paste0(dir.analyses1, "/outputtables/")) 
  
  reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico") 
  reg.order0<-c("US", "NP", "Pac", "WP", "NE", "MA", "SA", "GOM", "NorE")
  
  #Save Stuff
  editeddata.list<-list()
  rawtable.list<-list()
  tottable.list<-list()
  finaltable.list<-list()
  spptable<-data.frame()
  figures.list<-list()
  
  for (r in 1:length(reg.order)) {
    
    if (r != 1) { #only because I am tired of getting the warning messages
      remove(place, title0, temp00, temp0, temp, title000)
    }
    
    if (reg.order[r] == "Northeast") {
      state.codes$Region[state.codes$Region %in% c("Mid-Atlantic", "New England")]<-"Northeast"
      state.codes$abbvreg[state.codes$Region %in% c("Mid-Atlantic", "New England")]<-"NorE"
    }
    
    ### A. Import and Edit data
    #subset data
    place<-reg.order[r]
    print(place)
    counter<-funct_counter(counter)
    
    
    title000<-paste0("_","byr",baseyr, "_", analysisby, 
                     "_",gsub(pattern = "\\.", replacement = "", x = category0), 
                     "_pctmiss", gsub(pattern = "\\.", replacement = "", x = pctmiss))
    title0<-paste0(counter, "_", gsub(pattern = "\\(", replacement = "", x = 
                                        gsub(pattern = ")", replacement = "", x = 
                                               gsub(pattern = "`", replacement = "", x = 
                                                      gsub(reg.order0[r], pattern = " ", replacement = "")))), 
                   title000, "_", titleadd)
    
    
    idx<-c(1:nrow(landings.data))
    if (reg.order[r] != "National") {
      idx<-which(landings.data$State %in% state.codes$State[state.codes$Region %in% place])
    }
    
    temp00<-EditCommData(dat = landings.data[idx,], category0)
    temp.orig<-temp00[[1]] ### Data
    spp.editeddata<-temp00[[2]] ### By the way, which species are included in each category?
    tsn.editeddata<-temp00[[3]]  ### By the way, which species are included in each category by code number?
    
    NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
                                                  strsplit(x = names(temp.orig)[1], 
                                                           split = "_")[[1]][2], 
                                                split = "[a-zA-Z]")[[1]][1]))[1]
    
    ### B. Enter base year
    
    ### C. Run the function
    if (analysisby == "P") {
      temp00<-PriceMethodOutput(temp = temp.orig, baseyr, pctmiss, 
                                       title0 = title0, place = place, 
                                       MinimumNumberOfSpecies = MinimumNumberOfSpecies)
    } else if (analysisby == "Q") {
      temp00<-QuantityMethodOutput(temp = temp.orig, baseyr, pctmiss, 
                                       title0 = title0, place = place, 
                                       MinimumNumberOfSpecies = MinimumNumberOfSpecies)
    }
    
    temp<-temp00[[1]] #Data Output
    warnings.list0<-temp00[[2]] # Warnings
    figures.list0<-temp00[[3]] #Figures
    figures.list<-c(figures.list, figures.list0)
    spptable0<-temp00[[4]] #Species overview info
    spp.output<-temp00[[5]] #List of Species
    
    ### D. Obtain the implicit quantity estimates
    
    #EditedData
    editeddata.list[[r]]<-temp.orig
    names(editeddata.list)[r]<-place
    write.csv(x = editeddata.list[[r]], file = paste0(dir.outputtables, title0,"_EditedData.csv"))
    
    #Raw
    write.csv(x = temp, file = paste0(dir.outputtables, title0,"_AllData.csv"))
    rawtable.list[[r]]<-temp
    names(rawtable.list)[r]<-place
    
    #Review
    temp0<-temp[, grepl(pattern = paste0("_", NumberOfSpecies), x = names(temp))]
    names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
    # temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
    
    tottable.list[[r]]<-temp0
    names(tottable.list)[r]<-place
    write.csv(x = tottable.list[[r]], file = paste0(dir.outputtables, title0,"_Review.csv"))
    
    #Final
    temp0<-temp[, grepl(pattern = paste0("0_", NumberOfSpecies, "Total"), x = names(temp))]
    names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
    # temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
    
    finaltable.list[[r]]<-temp0
    names(finaltable.list)[r]<-place
    write.csv(x = finaltable.list[[r]], file = paste0(dir.outputtables, title0,"_Final.csv"))
    
    #Species Table
    # spptable0<-data.frame(Analysis  = title0,
    #                 Place = place,
    #                 Catagory = rep_len(x = NA, length.out = length(spp.editeddata)), 
    #                 TotCount = rep_len(x = NA, length.out = length(spp.editeddata)), 
    #                 RmCount = rep_len(x = NA, length.out = length(spp.editeddata)), 
    #                 UsedCount = rep_len(x = NA, length.out = length(spp.editeddata)))
    # 
    # for (i in 1:length(spp.editeddata)) {
    #   
    #   #Find the name of the ith species group (in terms of how the data is organized)
    #   cat<-names(spp.editeddata)[i]
    #   XColumns<-grep(pattern = paste0(NumberOfSpecies, cat),
    #                  x = names(temp))
    #       #Test
    #   XColumns<-c(XColumns, 1) #in case there is only one column for the next step
    #   cat0<-as.character(lapply(X = strsplit(x = names(temp[,XColumns])[1], 
    #                                             split = paste0("_", NumberOfSpecies)), 
    #                                function(x) x[2]))
    #   #Find the number of this ith species group (in terms of how the data is organized)
    #   ii<-as.character(lapply(X = strsplit(x = names(temp[,XColumns])[1], 
    #                                        split = paste0("_", NumberOfSpecies)), 
    #                           function(x) x[1]))
    #   ii<-gsub(pattern = "[a-zA-Z]", replacement = "", x = ii)
    # 
    #   #check your work
    #   # VColumns<-grep(pattern = paste0("V", ii,"_"),
    #   #                x = substr(x = names(temp),
    #   #                           start = 1,
    #   #                           stop = (2+nchar(ii))))
    #   
    #   RColumns<-grep(pattern = paste0("R", ii,"_"),
    #                  x = substr(x = names(temp),
    #                             start = 1,
    #                             stop = (2+nchar(ii))))
    #   RColumns<-RColumns[-grep(pattern = paste0(NumberOfSpecies, cat),
    #                            x = names(temp)[RColumns])]
    #   
    #   spptable0$Catagory[i]<- cat
    #   spptable0$TotCount[i]<-length(spp.editeddata[names(spp.editeddata) %in% cat][[1]])
    #   spptable0$UsedCount[i]<-ifelse(is.na(length(RColumns)), 0, length(RColumns))
    #   spptable0$RmCount[i]<-spptable0$TotCount[i] - spptable0$UsedCount[i]
    # }
    
    spptable<-rbind.data.frame(spptable, spptable0)
    write.csv(x = temp0, file = paste0(dir.outputtables, title0,"_Species.csv"))
    
    #Report
    rmarkdown::render(ProdI.Report, 
                      output_dir = paste0(dir.reports), 
                      output_file = paste0(title0,".docx"))
    
  }
  ########SPREADSHEETS########
  
  print("Create spreadsheets")
  
  spptable<-data.frame(spptable)
  spptable$pct.used<-round(x = (spptable$UsedCount/spptable$TotCount)*100,
                           digits = 2)
  
  save(editeddata.list, rawtable.list, finaltable.list, tottable.list, spptable, spp.output,
       file = paste0(dir.outputtables, "AllOutputs.rdata"))
  
  write.csv(x = spptable, file = paste0(dir.outputtables, "000_All", title000,"_Species.csv"))
  
  
  for (r in 1:length(reg.order)){
    
    #Raw
    # write.xlsx2(x = rawtable.list[[r]], 
    #             file = paste0(dir.outputtables, "000_All", title000, "_Raw.xlsx"), 
    #             sheetName = reg.order[r], 
    #             col.names = T, row.names = T, append = T)
    
    #Edited Data
    # write.xlsx2(x = editeddata.list[[r]],
    #             file = paste0(dir.outputtables, "000_All", title000, "_EditedData.xlsx"),
    #             sheetName = reg.order[r],
    #             col.names = T, row.names = T, append = T)
    
    #Print
    write.xlsx2(x = finaltable.list[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_FinalOutput.xlsx"), 
                sheetName = reg.order[r], 
                col.names = T, row.names = T, append = T)
    
    #Review
    write.xlsx2(x = tottable.list[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_Review.xlsx"), 
                sheetName = reg.order[r], 
                col.names = T, row.names = T, append = T)
    
  }
  
  ######PLOTS##########
  
  print("Create plots")
  
  save(figures.list, #gridfigures.list,
       file = paste0(dir.figures, "AllFigures.rdata"))
  
  #Side by Side graphs
  figs<-unique(paste0(lapply(X = strsplit(x = names(figures.list),
                                          split = gsub(pattern = "\\.", replacement = "", x = category0)),
                             function(x) x[2])))
  gridfigures.list<-list()
  
  for (i in 1:length(figs)){
    
    a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
    
    dir.create(paste0(dir.figures, "/", a, "/"))
    
    fig<-figs[i]
    list0<-figures.list[grep(pattern = fig, x = names(figures.list))]
    
    # g<-ggarrange(list0[[1]],
    #                 list0[[2]],
    #                 list0[[3]],
    #                 list0[[4]],
    #                 list0[[5]],
    #                 list0[[6]],
    #                 list0[[7]],
    #                 nrow=3, ncol = 3)
    
    g<-ggarrange(plotlist = list0,
                 nrow=3, ncol = 3)
    
    ggsave(filename = paste0(dir.figures, "/", a, "/", "000_All_byr",baseyr,
                             "_",gsub(pattern = "\\.", replacement = "", x = category0), fig, ".png"),
           plot = g,
           width = 11, height = 8.5)
    
    gridfigures.list<-c(gridfigures.list, list(g))
    names(gridfigures.list)[length(gridfigures.list)]<-paste0("000_All_byr",baseyr,
                                                              "_",gsub(pattern = "\\.", replacement = "", x = category0), fig)
  }
  
  save(gridfigures.list,
       file = paste0(dir.figures, "AllFiguresGrid.rdata"))



  #   #make single plots
  #   for (i in 1:length(figures.list)) {
  #     print(paste0(names(figures.list)[i]))
  #     a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
  #     dir.create(paste0(dir.figures, "/", a, "/"))
  # 
  #     # dir.create(paste0(dir.figures, "/", a, "/"))
  # 
  #     ggsave(filename = paste0(dir.figures, "/", a, "/", names(figures.list)[i], ".png"),
  #            plot = figures.list[[i]],
  #            width = 11, height = 8.5)
  # }
  
}

########RUN############

########*** Price Driven Analysis - category.orig############
analysisby = "P"
category0 = "category.orig"
pctmiss = 0.60
MinimumNumberOfSpecies = 10
baseyr<-2007

# # Data for the whole Time Series
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = paste("1950To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("1997To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# #Data just from the last 10 years
# OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,], 
#                category0, baseyr, 
#                state.codes, titleadd = paste0("2007To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies = 6) 


# analysisby = "P"
# category0 = "category.orig"
# pctmiss = 0.95
# MinimumNumberOfSpecies = 2
# 
# # Data for the whole Time Series
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = paste0("1950To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby)
# 
# #Data just from the last 20 years
# OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
#                category0, baseyr, 
#                state.codes, titleadd = paste0("1997To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby) 


# #Data just since 2008
# OutputAnalysis(landings.data[landings.data$Year>=2007,], category0, baseyr, 
#                state.codes, titleadd = "2007ToPresent",
#                counter, dir.rawdata, pctmiss = pctmiss, 
#                MinimumNumberOfSpecies)

# 
# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr, 
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00,
#                #MinimumNumberOfSpecies = 6) 




category0 = "category.origFSO"
MinimumNumberOfSpecies = 1

a<-landings.data
a$category.origFSO<-a$category.orig

###Without Other
b<-a
a<-a[!(a$category.origFSO %in% "Other"),]


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)


### Data just with key 10 species
#
#For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 

#Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 

#method to get QI you can do both as for the 10 species case. 

a<-b
cc<-data.frame()
tsn.id0<-data.frame()
for (i in 1:length(unique(a$Region))) {
  place<-unique(a$Region)[i]
  tempdat<-a[a$Region %in% place,]
  tempdat$TSN<-tempdat$Tsn
  
  temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
                        categories = spcat.list[place][[1]], 
                        missing.name="Uncategorized")
  
  tsn.id<-data.frame(temp$df.out)
  tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
  tsn.id<-cbind.data.frame(tsn.id, 
                           "area" = place)
  
  tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
  cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
}
a<-cc
category0 = "category.origFSO"
MinimumNumberOfSpecies = 1

OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


########*** Price Driven Analysis - category.tax############
analysisby = "P"
category0 = "category.tax"
pctmiss = 0.60
MinimumNumberOfSpecies = 10
baseyr<-2007

# Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_AllTax"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_AllTax"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

#Data just from the last 10 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_AllTax"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies = 6)

# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00,
#                MinimumNumberOfSpecies)




category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1

a<-landings.data
a$category.taxFSO<-a$category.taxsimp
a$category.taxFSO[a$category.taxFSO %in% c("Other Fish", "Chondrichthyes", "Actinopterygii")]<-"Finfish"
a$category.taxFSO[a$category.taxFSO %in% c("Mollusca","Arthropoda","Echinodermata")]<-"Shellfish"
a$category.taxFSO[a$category.taxFSO %in% c("Other", "Tetrapoda")]<-"Other"


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FSO"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FSO"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FSO"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


###Without Other
b<-a
a<-a[!(a$category.taxFSO %in% "Other"),]


# Data for the whole Time Series
OutputAnalysis(landings.data =a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FS"), 
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


### Data just with key 10 species
#
#For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 

#Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 

#method to get QI you can do both as for the 10 species case. 

a<-b
cc<-data.frame()
tsn.id0<-data.frame()
for (i in 1:length(unique(a$Region))) {
  place<-unique(a$Region)[i]
  tempdat<-a[a$Region %in% place,]
  tempdat$TSN<-tempdat$Tsn
  
  temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
                        categories = spcat.list[place][[1]], 
                        missing.name="Uncategorized")
  
  tsn.id<-data.frame(temp$df.out)
  tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
  tsn.id<-cbind.data.frame(tsn.id, 
                           "area" = place)
  
  tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
  cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
}
a<-cc
category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1


OutputAnalysis(landings.data =a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

########*** Quantity Driven Analysis - category.orig############
analysisby = "Q"
category0 = "category.orig"
pctmiss = 0.60
MinimumNumberOfSpecies = 10
baseyr<-2007

# # Data for the whole Time Series
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = paste0("1950To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("1997To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# #Data just from the last 10 years
# OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,], 
#                category0, baseyr, 
#                state.codes, titleadd = paste0("2007To", maxyr),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies = 6)  
# 
# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr, 
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00) 
category0 = "category.origFSO"
MinimumNumberOfSpecies = 1


a<-landings.data
a$category.origFSO<-a$category.orig

###Without Other
b<-a
a<-a[!(a$category.origFSO %in% "Other"),]


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


### Data just with key 10 species
#
#For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 

#Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 

#method to get QI you can do both as for the 10 species case. 

a<-b
cc<-data.frame()
tsn.id0<-data.frame()
for (i in 1:length(unique(a$Region))) {
  place<-unique(a$Region)[i]
  tempdat<-a[a$Region %in% place,]
  tempdat$TSN<-tempdat$Tsn
  
  temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
                        categories = spcat.list[place][[1]], 
                        missing.name="Uncategorized")
  
  tsn.id<-data.frame(temp$df.out)
  tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
  tsn.id<-cbind.data.frame(tsn.id, 
                           "area" = place)
  
  tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
  cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
}
a<-cc
category0 = "category.origFSO"
MinimumNumberOfSpecies = 1

OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

########*** Quantity Driven Analysis - category.tax############
analysisby = "Q"
category0 = "category.tax"
pctmiss = 0.60
MinimumNumberOfSpecies = 10
baseyr<-2007

# Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_AllTax"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_AllTax"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

# #Data just from the last 10 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_AllTax"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies = 6)

# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00)


category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1


a<-landings.data
a$category.taxFSO<-as.character(a$category.taxsimp)
a$category.taxFSO[a$category.taxFSO %in% c("Other Fish", "Chondrichthyes", "Actinopterygii")]<-"Finfish"
a$category.taxFSO[a$category.taxFSO %in% c("Mollusca","Arthropoda","Echinodermata")]<-"Shellfish"
a$category.taxFSO[a$category.taxFSO %in% c("Other", "Tetrapoda")]<-"Other"


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FSO"),
               counter, dir.rawdata = dir.rawdata, dir.reports = dir.reports, 
               dir.figures = dir.figures, dir.outputtables = dir.outputtables, 
               pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FSO"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FSO"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


###Without Other
b<-a
a<-a[!(a$category.taxFSO %in% "Other"),]


# Data for the whole Time Series
OutputAnalysis(landings.data =a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FS"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


### Data just with key 10 species
#
#For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 

#Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 

#method to get QI you can do both as for the 10 species case. 

a<-b
cc<-data.frame()
tsn.id0<-data.frame()
for (i in 1:length(unique(a$Region))) {
  place<-unique(a$Region)[i]
  tempdat<-a[a$Region %in% place,]
  tempdat$TSN<-tempdat$Tsn
  
  temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
                        categories = spcat.list[place][[1]], 
                        missing.name="Uncategorized")
  
  tsn.id<-data.frame(temp$df.out)
  tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
  tsn.id<-cbind.data.frame(tsn.id, 
                           "area" = place)
  
  tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
  cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
}
a<-cc
category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1


OutputAnalysis(landings.data =a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = data.frame(a[a$Year>=1997,]),
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)

OutputAnalysis(landings.data = data.frame(a[a$Year>=2007,]),
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_FSKey"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies)


##############***Northeast##############

#######KNOWNS
maxyr0 <- 2017
minyr <- 2007
NMFSorITIS<-"ITISTax"

#Functions common to all sections, to help instill standardization
# Common.Funct<-paste0(dirname(getwd()), "/FEUS/2018/FEUS",maxyr,"Common/rscripts/Common_Functions.r")
# source(Common.Funct)
Comm.SppData<-paste0(dir.scripts, "ProductivityIndex_Species.R")
dir.data0<-dir.data
dir.data<-paste0(dirname(getwd()), "/FEUS/2018/FEUS2018Commercial/data/")
dir.data.common<-paste0(dirname(getwd()), "/FEUS/2018/FEUS2018Common/data/")
Comm.Data<-paste0(dirname(getwd()), "/FEUS/2018/FEUS2018Commercial/rscripts/Commercial_Data.R")
dir.nore<-paste0(dir.out,"/analyses/",minyr,"To",maxyr0,"_Fisheries_Northeast/")
create_dir(dir.nore)
#Download new data
# source(Comm.DataDL)

#Data specific to this section
source(Comm.Data)
source(Comm.SppData)
dir.data<-dir.data0

spcat<-c()
spcat0<-read.csv(paste0(dir.data, "/specodes_W_names.csv"))

#Find index of where these species can be found in spcat_list$General
for (i in 1:nrow(spcat0)) {
  spcat<-c(spcat, 
           ifelse(sum(grepl(pattern = spcat0$Name[i], x = names(spcat.list$General) , ignore.case = T)) == 0, 
                  NA, grep(pattern = spcat0$Name[i], x = names(spcat.list$General), ignore.case = T) ))
}

spcat1<-spcat.list$General[spcat]
names(spcat1)<-spcat0$Name

spcat00<-data.frame("TSN" = unlist(spcat.list$General[spcat]), 
                    spcat0)

write.xlsx(spcat00, file = paste0(dir.nore, "NortheastData.xlsx"), sheetName = "AskedFor", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

spcat.list<-list()
spcat.list$Areas<-list()
spcat.list$Areas$Northeast<-spcat1

landings.data1<-data.frame("State1" = landings.data$State, 
                           "State" = landings.data$State, 
                           "TSN" = landings.data$Tsn, 
                           "year" = landings.data$Year, 
                           "POUNDS" = landings.data$Pounds, 
                           "DOLLARS"=landings.data$Dollars, 
                           "CommonName" = landings.data$AFS.Name, 
                           "Region" = landings.data$Region, 
                           "category" = landings.data$category.orig, 
                           "OFS" = substr(x = landings.data$category.orig, 1,1), 
                           "abbvst" = landings.data$abbvst, 
                           "abbvreg" = landings.data$abbvreg, 
                           "xstate" = landings.data$xstate, 
                           "xreg" = landings.data$xreg, 
                           "State.no" = landings.data$State.no, 
                           "Region.no" = landings.data$Region.no, 
                           "fips" = landings.data$fips)


a<-spp_reclassify(landings.df = landings.data1[landings.data1$Region %in% c("New England", "Mid-Atlantic"),], 
                  spcat.list = spcat.list, 
                  place = "Northeast")


for (jjj in 1:length(a)) {
  assign(names(a)[jjj], a[[jjj]])
  write.xlsx(a[[jjj]], file = paste0(dir.nore, "NortheastData.xlsx"), sheetName = names(a)[jjj], 
             col.names = TRUE, row.names = TRUE, append = TRUE)
}

revenue$keyspecies<-tolower(revenue$keyspecies)
spcat0$Name<-tolower(spcat0$Name)
FisherySums<-merge(x = revenue, y = spcat0, by.x = "keyspecies", by.y = "Name")

FisherySums<-aggregate.data.frame(x = sapply(X = FisherySums[,as.character(minyr:maxyr0)], FUN = as.numeric), 
                                      by = list("PLAN" = FisherySums$PLAN),
                                      FUN = sum, na.rm = T)

FisherySums$Total<-rowSums(FisherySums[,as.character(minyr:maxyr0)])

FisherySums <- rbind.data.frame(FisherySums, 
                                    cbind.data.frame("PLAN" = "Fisheries Total", 
                                                     t(colSums(FisherySums[,c(as.character(minyr:maxyr0), "Total")]))), 
                                    cbind.data.frame("PLAN" = a$revenue$keyspecies[1:4], 
                                                     a$revenue[1:4,as.character(minyr:maxyr0)], 
                                                     "Total" = rowSums(sapply(X = a$revenue[1:4,as.character(minyr:maxyr0)], 
                                                                              FUN = as.numeric))))

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = T)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
for (i in 1:length(fold)){
  fold0<-list.files(path = paste0(fold[i], "/outputtables"), full.names = T, pattern = "000_All_")
  if (!(length(fold0) %in% 0)) {
  fold0<-fold0[grep(pattern = "_Review", x = fold0)]
  fold00<-list.files(path = paste0(fold[i], "/outputtables"), pattern = "000_All_")
  fold00<-fold00[grepl(pattern = "_Review", x = fold00)]
  
  # for (ii in 1:length(length( excel_sheets( fold0 ) ))){
    temp <- read.xlsx2(file = fold0, sheetName = "Northeast")

    temp0<- data.frame("PLAN" = paste0(fold00, c(" V_Total")), 
                        t(temp[temp[,1] %in% as.character(minyr:maxyr0), c("V_Total")]), 
                        sum( as.numeric((temp[temp[,1] %in% as.character(minyr:maxyr0), 
                                              c("V_Total")])) ) )  
    
    if (sum(names(temp) %in% "VV_Total")==1) {
      tempQ<- data.frame("PLAN" = paste0(fold00, c(" VV_Total")), 
                         t(temp[temp[,1] %in% as.character(minyr:maxyr0), c("VV_Total")]), 
                         sum( as.numeric((temp[temp[,1] %in% as.character(minyr:maxyr0), 
                                               c("VV_Total")])) ) )   
      temp0<-rbind.data.frame(temp0, tempQ)
    }
    
    names(temp0)<-names(FisherySums)
    FisherySums<-rbind.data.frame(FisherySums, temp0)
  }
}

rownames(FisherySums)<-NULL
FisherySums.rev<-FisherySums

write.xlsx(FisherySums.rev, file = paste0(dir.nore, "NortheastData.xlsx"), sheetName = "FisherySums_Revenue", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

########Landings

landings$keyspecies<-tolower(landings$keyspecies)
spcat0$Name<-tolower(spcat0$Name)
FisherySums<-merge(x = landings, y = spcat0, by.x = "keyspecies", by.y = "Name")

FisherySums<-aggregate.data.frame(x = sapply(X = FisherySums[,as.character(minyr:maxyr0)], FUN = as.numeric), 
                                  by = list("PLAN" = FisherySums$PLAN),
                                  FUN = sum, na.rm = T)

FisherySums$Total<-rowSums(FisherySums[,as.character(minyr:maxyr0)])

FisherySums <- rbind.data.frame(FisherySums, 
                                cbind.data.frame("PLAN" = "Fisheries Total", 
                                                 t(colSums(FisherySums[,c(as.character(minyr:maxyr0), "Total")]))), 
                                cbind.data.frame("PLAN" = a$landings$keyspecies[1:4], 
                                                 a$landings[1:4,as.character(minyr:maxyr0)], 
                                                 "Total" = rowSums(sapply(X = a$landings[1:4,as.character(minyr:maxyr0)], 
                                                                          FUN = as.numeric))))

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = T)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
for (i in 1:length(fold)){
  fold0<-list.files(path = paste0(fold[i], "/outputtables"), full.names = T, pattern = "000_All_")
  if (!(length(fold0) %in% 0)) {
    fold0<-fold0[grep(pattern = "_Review", x = fold0)]
    fold00<-list.files(path = paste0(fold[i], "/outputtables"), pattern = "000_All_")
    fold00<-fold00[grepl(pattern = "_Review", x = fold00)]
    
    # for (ii in 1:length(length( excel_sheets( fold0 ) ))){
    temp <- read.xlsx2(file = fold0, sheetName = "Northeast")
    
    temp0<- data.frame("PLAN" = paste0(fold00, c(" QI_Total")), 
                       t(temp[temp[,1] %in% as.character(minyr:maxyr0), c("QI_Total")]), 
                       sum( as.numeric((temp[temp[,1] %in% as.character(minyr:maxyr), 
                                             c("QI_Total")])) ) )  
    
    if (sum(names(temp) %in% "Q_Total")==1) {
      tempQ<- data.frame("PLAN" = paste0(fold00, c(" Q_Total")), 
                         t(temp[temp[,1] %in% as.character(minyr:maxyr0), c("Q_Total")]), 
                         sum( as.numeric((temp[temp[,1] %in% as.character(minyr:maxyr0), 
                                               c("Q_Total")])) ) )   
      names(tempQ)<-names(temp0)
      temp0<-rbind.data.frame(temp0, tempQ)
    }
    
    names(temp0)<-names(FisherySums)
    FisherySums<-rbind.data.frame(FisherySums, temp0)
  }
}

rownames(FisherySums)<-NULL
FisherySums.land<-FisherySums

write.xlsx(FisherySums.land, file = paste0(dir.nore, "NortheastData.xlsx"), sheetName = "FisherySums_Landings", 
           col.names = TRUE, row.names = TRUE, append = TRUE)


#######***Summary Files##########

dir.sum<-paste0(dir.out,"/analyses/SummaryFiles/")
create_dir(dir.sum)

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = FALSE)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "SummaryFiles", x = fold))]

fold<-strsplit(x = fold, split = "_")
yearrange<-unique(unlist(lapply(fold, `[[`, 1)))
category0<-unique(unlist(lapply(fold, `[[`, 2)))
methods0<-unique(unlist(lapply(fold, `[[`, 3)))

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = T)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "SummaryFiles", x = fold))]

summarydata<-list()

for (yrange0 in 1:length(yearrange)) {
  
  # for (cat0 in 1:length(category0)) {
    
    fold0<-fold[#grepl(pattern = reg.order[reg0], x = fold) & 
      # grepl(pattern = category0[cat0], x = fold) & 
        grepl(pattern = yearrange[yrange0], x = fold)]
    
    
    for (reg0 in 1:length(reg.order)) {
      
      for (i in 1:length(fold0)){
        file00<-list.files(path = paste0(fold0[i], "/outputtables"), full.names = T, pattern = "000_All_")
          if (!(length(file00) %in% 0)) {
        file0<-file00[grep(pattern = "_Review", x = file00)]      
          
          # for (ii in 1:length(length( excel_sheets( fold0 ) ))){
          temp <- read.xlsx2(file = file0, sheetIndex = reg.order[reg0])
          rownames(temp)<-temp[,1]
          
          if (i==1) {
            temp0<-rbind.data.frame(data.frame("Year" = as.numeric(temp$X.)))#, 
                                               # "QE" = temp$QE_Total, 
                                               # "QEI" = temp$QEI_Total))
          }            
          
          if (sum(names(temp) %in% c("QE_Total", "QEI_Total". "QI_Total", "Q_Total", "PI_Total")) != 0) {
          temp00<-data.frame(temp[,names(temp) %in% c("QI_Total", "Q_Total", "PI_Total")])
          names(temp00)<-names(temp)[names(temp) %in% c("QI_Total", "Q_Total", "PI_Total")]
          names(temp00)<-gsub(pattern = "_Total", replacement = "", x = names(temp00))
          names(temp00)<-paste0(names(temp00), " (", 
                                                               ifelse(grepl(pattern = "_P_", x = file0), "Price", "Quantity"), 
                                                               " ", 
                                                               unique(strsplit(x = file0, split = "_")[[1]][grep(pattern = "category", 
                                                                                                          x = strsplit(x = file0, 
                                                                                                                       split = "_")[[1]])]), 
                                                               " ", 
                                                               as.character(category0[sapply(paste0("_", category0, "_"), grepl, file0) %in% TRUE]), 
                                                               ")")
          temp0<-cbind.data.frame(temp0, temp00)
          }
      }  
    }
    
          temp0names<-names(temp0)
      temp0<-data.frame(sapply(temp0, as.numeric))
          names(temp0)<-temp0names
      
    write.xlsx(temp0, file = paste0(dir.sum, "Summary.xlsx"), sheetName = paste0(reg.order[reg0], "_", yearrange[yrange0]), 
               col.names = TRUE, row.names = TRUE, append = TRUE)  
    
    tempM<-temp0
    
      tempM[,grep(pattern = "Q ", x = names(tempM))]<-tempM[,grep(pattern = "Q ", x = names(tempM))]/1e6
      names(tempM)[grep(pattern = "Q ", x = names(tempM))]<-paste0(names(tempM)[grep(pattern = "Q ", x = names(tempM))], " (Millions)")
    
    tempM[,names(tempM) %in% "QE"]<-tempM[,names(tempM) %in% "QE"]/1e6
    names(tempM)[names(tempM) %in% "QE"]<-paste0(names(tempM)[names(tempM) %in% "QE"], " (Millions)")
    
    write.xlsx(tempM, file = paste0(dir.sum, "SummaryMill.xlsx"), sheetName = paste0(reg.order[reg0], "_", yearrange[yrange0]), 
               col.names = TRUE, row.names = TRUE, append = TRUE)  
    
    summarydata[length(summarydata)+1]<-list(temp0)
    names(summarydata)[length(summarydata)]<-paste0(reg.order[reg0], "_", yearrange[yrange0])
    
  }

}

save(summarydata, file = paste0(dir.sum, "Summary.rdata"))


########PRESENTATION######################
library(tidyverse)
library(gapminder)
library(scales)
library(gridExtra)
library(patchwork)

Date0<-"2020-08-26"
datadl<-"2020-08-14"

# Opening Slides

rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_Presentation_Opening.Rmd"),
                  output_dir = dir.pres,
                  output_file = paste0("ProductivityIndex_EmilyMarkowitz_Opening.pptx"))


# Loop through Slide Placement of Key Slides (Q, QE, QI, PI)
fold<-list.files(paste0(dir.out, "/analyses/"), full.names = FALSE)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "SummaryFiles", x = fold))]

fold0<-strsplit(x = fold, split = "_")
aaa<-unique(paste(lapply(fold0, `[[`, 2), 
                lapply(fold0, `[[`, 3), 
                lapply(fold0, `[[`, 4), 
                lapply(fold0, `[[`, 5), 
                sep = "_"))

for (i in 1:length(aaa)){
  folderpattern <- aaa[i]
  rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_Presentation_KeyRegionPlots.Rmd"),
                    output_dir = paste0(dir.out, "/Presentation/"),
                    output_file = paste0("ProductivityIndex_EmilyMarkowitz_KeyRegionPlots_", folderpattern, ".pptx"))
}


# Loop through Slide Placement of Cross Analysis Comparisons by Regions
aaa<-unlist(unique(lapply(fold0, `[[`, 1)))
load(file = paste0(dir.out, "/analyses/SummaryFiles/Summary.rdata")) #summarydata
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico") 
for (i in 1:length(aaa)){
  yr<-aaa[i]
  rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_Presentation_CrossAnalysis.Rmd"),
                    output_dir = paste0(dir.out, "/Presentation/"),
                    output_file = paste0("ProductivityIndex_Presentation_CrossAnalysis_", yr, ".pptx"))
}

########DOCUMENTATION#################

#OUTPUT
#By Price
code<-TRUE
showresults<-TRUE

rmarkdown::render(ProdI.Docu.Out.P,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Price_",date0,".pdf"))

# file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Price_",date0,".pdf"),
#                  to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Price.pdf"))

code<-FALSE
showresults<-FALSE

rmarkdown::render(ProdI.Docu.Out.P,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Price_",date0,"_NoCode.pdf"))

# file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Price_",date0,"_NoCode.pdf"),
#                  to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Price_NoCode.pdf"))

#By Quantity
code<-TRUE
showresults<-TRUE

rmarkdown::render(ProdI.Docu.Out.Q,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Quant_",date0,".pdf"))

# file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Quant_",date0,".pdf"),
#                  to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Quant.pdf"))

code<-FALSE
showresults<-FALSE

rmarkdown::render(ProdI.Docu.Out.Q,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Quant_",date0,"_NoCode.pdf"))

# file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Quant_",date0,"_NoCode.pdf"),
#                  to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Quant_NoCode.pdf"))

# #INPUT
# rmarkdown::render(ProdI.Docu.In,
#                   output_dir = paste0(dir.docu),
#                   output_file = paste0("ProductivityIndex_Documentation_In_",date0,".pdf"))
# 
# file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_In_",date0,".pdf"),
#                  to = paste0(dir.output, "ProductivityIndex_Documentation_Input.pdf"))

###############METADATA##################
CreateMetadata(dir.out = paste0(dir.out, "/metadata"), 
               title = paste0("Fisheries Economic Productivity Index Metadata ", Sys.Date()))

# file.copy.rename(from = paste0(dir.out, "/metadata/Metadata_", Sys.Date(), ".docx"),
#                  to = paste0(dir.output, "/Metadata.docx"))

