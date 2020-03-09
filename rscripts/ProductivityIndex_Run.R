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
dir.scripts<-paste0(dir.in, "/rscripts/")
dir.create(paste0(dir.out, "/rscripts")) 
dir.reports<-paste0(dir.out, "/reports/")
dir.create(paste0(dir.out, "/analyses")) 
dir.analyses<-paste0(dir.out, "/analyses/")
dir.create(paste0(dir.out, "/reports/")) 
dir.create(paste0(dir.out, "/metadata/")) #Save word files
dir.rawdata<-paste0(dir.out, "/rawdata/")
dir.create(paste0(dir.out, "/rawdata/")) 
dir.figures<-paste0(dir.out, "/figures/")
dir.create(paste0(dir.out, "/figures")) 
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
landings.data<-landings.data[landings.data$Year < 2018,] #FUS 2018 hasn't been published yet
landings.data<-landings.data[landings.data$State %in% unique(state.codes$NAME),]
# landings.data<-landings.data[landings.data$Year > 1974,]
write.csv(x = landings.data, file = paste0(dir.rawdata,"landings_edited.csv"))




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

# plotlist<-list()

OutputAnalysis<-function(landings.data, category0, baseyr, 
         state.codes, titleadd,
         counter, dir.rawdata, dir.reports, pctmiss, dir.figures, dir.outputtables, analysisby = "P") {
  
  dir.analyses1<-paste0(dir.analyses, "/",titleadd, "_", analysisby, "_", 
                        gsub(pattern = "\\.", replacement = "", x = category0),"/")
  dir.create(dir.analyses1) 
  dir.reports<-paste0(dir.analyses1, "/reports/")
  dir.create(paste0(dir.analyses1, "/reports/")) 
  dir.figures<-paste0(dir.analyses1, "/figures/")
  dir.create(paste0(dir.analyses1, "/figures/")) 
  dir.outputtables<-paste0(dir.analyses1, "/outputtables/")
  dir.create(paste0(dir.analyses1, "/outputtables/")) 
  
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "South Atlantic", "Gulf of Mexico") 
reg.order0<-c("US", "NP", "Pac", "WP", "NE", "MA", "SA", "GOM")

#Save Stuff
editeddata.list<-list()
rawtable.list<-list()
tottable.list<-list()
finaltable.list<-list()
spptable<-data.frame()
figures.list<-list()
  
for (r in 1:length(reg.order)){
  
  if (r != 1) { #only because I am tired of getting the warning messages
    remove(place, title0, temp00, temp0, temp, title000, title0)
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
    idx<-which(landings.data$State %in% state.codes$NAME[state.codes$Region %in% place])
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
  temp00<-ImplicitQuantityOutput.p(temp = temp.orig, baseyr, pctmiss, 
                                 title0 = title0, place = place)
  } else if (analysisby == "Q") {
  temp00<-ImplicitQuantityOutput.q(temp = temp.orig, baseyr, pctmiss, 
                                     title0 = title0, place = place)
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
  temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]

  tottable.list[[r]]<-temp0
  names(tottable.list)[r]<-place
  write.csv(x = tottable.list[[r]], file = paste0(dir.outputtables, title0,"_Review.csv"))
  
  #Final
  temp0<-temp[, grepl(pattern = paste0("0_", NumberOfSpecies, "Total"), x = names(temp))]
  names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
  temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
  
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
  
  g<-grid.arrange(list0[[1]], 
                  list0[[2]], 
                  list0[[3]], 
                  list0[[4]], 
                  list0[[5]], 
                  list0[[6]], 
                  list0[[7]], 
                  nrow=3, newpage = FALSE)
  
  ggsave(filename = paste0(dir.figures, "/", a, "/", "000_All_baseyr",baseyr, 
                           "_",gsub(pattern = "\\.", replacement = "", x = category0), fig, ".png"), 
         plot = g, 
         width = 11, height = 8.5)
  
  gridfigures.list[length(gridfigures.list)+1]<-g
  names(gridfigures.list)[length(gridfigures.list)]<-paste0("000_All_baseyr",baseyr, 
                                                            "_",gsub(pattern = "\\.", replacement = "", x = category0), fig)
  }
  
  
  save(figures.list, gridfigures.list,
       file = paste0(dir.figures, "AllFigures.rdata"))
  
  #make single plots
  for (i in 1:length(figures.list)) {
    
    a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
    
    # dir.create(paste0(dir.figures, "/", a, "/"))
    
    ggsave(filename = paste0(dir.figures, "/", a, "/", names(figures.list)[i], ".png"), 
           plot = figures.list[[i]], 
           width = 11, height = 8.5)
  }
  
}

########RUN############

########*** Price Driven Analysis - category.orig############
analysisby = "P"
category0 = "category.orig"

#Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr = 2007, 
               state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

#Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
               category0, baseyr = 2007, 
               state.codes, titleadd = "1997ToPresent",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

# #Data just since 2008
# OutputAnalysis(landings.data[landings.data$Year>=2007,], category0, baseyr = 2007, 
#                state.codes, titleadd = "2007ToPresent",
#                counter, dir.rawdata, pctmiss = 0.60) 
# 
# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr = 2007, 
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00) 

########*** Price Driven Analysis - category.tax############
analysisby = "P"
category0 = "category.tax"

#Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr = 2007, 
               state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

#Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
               category0, baseyr = 2007, 
               state.codes, titleadd = "1997ToPresent",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

# #Data just since 2008
# OutputAnalysis(landings.data[landings.data$Year>=2007,], category0, baseyr = 2007, 
#                state.codes, titleadd = "2007ToPresent",
#                counter, dir.rawdata, pctmiss = 0.60) 
# 
# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr = 2007, 
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00) 

########*** Quantity Driven Analysis - category.orig############
analysisby = "Q"
category0 = "category.orig"

#Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr = 2007, 
               state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

#Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
               category0, baseyr = 2007, 
               state.codes, titleadd = "1997ToPresent",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

# #Data just since 2008
# OutputAnalysis(landings.data[landings.data$Year>=2007,], category0, baseyr = 2007, 
#                state.codes, titleadd = "2007ToPresent",
#                counter, dir.rawdata, pctmiss = 0.60) 
# 
# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr = 2007, 
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00) 

########*** Quantity Driven Analysis - category.tax############
analysisby = "Q"
category0 = "category.tax"

#Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr = 2007, 
               state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

#Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
               category0, baseyr = 2007, 
               state.codes, titleadd = "1997ToPresent",
               counter, dir.rawdata, pctmiss = 0.60, analysisby = analysisby) 

# #Data just since 2008
# OutputAnalysis(landings.data[landings.data$Year>=2007,], category0, baseyr = 2007, 
#                state.codes, titleadd = "2007ToPresent",
#                counter, dir.rawdata, pctmiss = 0.60) 
# 
# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr = 2007, 
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00) 

########DOCUMENTATION#################

#OUTPUT
#By Price
code<-TRUE
showresults<-TRUE

rmarkdown::render(ProdI.Docu.Out.P,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Price_",date0,".pdf"))

file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Price_",date0,".pdf"),
               to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Price.pdf"))

code<-FALSE
showresults<-FALSE

rmarkdown::render(ProdI.Docu.Out.P,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Price_",date0,"_NoCode.pdf"))

file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Price_",date0,"_NoCode.pdf"),
                 to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Price_NoCode.pdf"))

#By Quantity
code<-TRUE
showresults<-TRUE

rmarkdown::render(ProdI.Docu.Out.Q,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Quant_",date0,".pdf"))

file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Quant_",date0,".pdf"),
                 to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Quant.pdf"))

code<-FALSE
showresults<-FALSE

rmarkdown::render(ProdI.Docu.Out.Q,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_Quant_",date0,"_NoCode.pdf"))

file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_Quant_",date0,"_NoCode.pdf"),
                 to = paste0(dir.output, "ProductivityIndex_Documentation_Out_Quant_NoCode.pdf"))

#INPUT
rmarkdown::render(ProdI.Docu.In,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_In_",date0,".pdf"))

file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_In_",date0,".pdf"),
               to = paste0(dir.output, "ProductivityIndex_Documentation_Input.pdf"))

###############METADATA##################
CreateMetadata(dir.out = paste0(dir.out, "/metadata"), 
               title = paste0("Fisheries Economic Productivity Index Metadata ", Sys.Date()))

file.copy.rename(from = paste0(dir.out, "/metadata/Metadata_", Sys.Date(), ".docx"),
          to = paste0(dir.output, "/Metadata.docx"))
