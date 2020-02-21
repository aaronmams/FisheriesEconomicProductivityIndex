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
ProdI.Docu.Out<-paste0(dir.scripts, "ProductivityIndex_Documentation_Output",date0,".rmd")
ProdI.Docu.In<-paste0(dir.scripts, "ProductivityIndex_Documentation_Input",date0,".rmd")

######SAVE WORKING FILES########
#From Specific Files
listfiles<-list.files(path = dir.scripts, pattern = date0) 

for (i in 1:length(listfiles)){
  file.copy(from = paste0(dir.scripts, listfiles[i]), 
            to = paste0(dir.out, "/rscripts/", 
                        gsub(pattern = date0, replacement = date00, x = listfiles[i])), 
            overwrite = T)
}


#Move most updated  word styles file from reference "common" file to getwd()/rscript
file.copy(from = paste0(dir.scripts, "/word-styles-reference.docx"), 
          to = paste0(dir.out, "/rscripts", "/word-styles-reference", Sys.Date(),".docx"), 
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
         reg.order, state.codes, titleadd,
         counter, dir.rawdata, dir.reports, PercentMissingThreshold, dir.figures, dir.outputtables) {
  
  dir.analyses1<-paste0(dir.analyses, "/",titleadd, "_", gsub(pattern = "\\.", replacement = "", x = category0),"/")
  dir.create(dir.analyses1) 
  dir.reports<-paste0(dir.analyses1, "/reports/")
  dir.create(paste0(dir.analyses1, "/reports/")) 
  dir.figures<-paste0(dir.analyses1, "/figures/")
  dir.create(paste0(dir.analyses1, "/figures/")) 
  dir.outputtables<-paste0(dir.analyses1, "/outputtables/")
  dir.create(paste0(dir.analyses1, "/outputtables/")) 
  
  
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "South Atlantic", "Gulf of Mexico") 
reg.order0<-c("US", "NP", "Pac", "WP", "NE", "MA", "SA", "GOM")
temp0tot<-list()
  temp0raw<-list()
  figures.list<-list()
  temp0final<-list()
  
for (r in 1:length(reg.order)){
  
  remove(place, title0, temp00, temp0, temp, title000, title0)
  
  ### A. Import and Edit data
  #subset data
  place<-reg.order[r]
  print(place)
  counter<-funct_counter(counter)
  
  
  title000<-paste0("_","byr",baseyr, 
                   "_",gsub(pattern = "\\.", replacement = "", x = category0), 
                   "_pctmiss", gsub(pattern = "\\.", replacement = "", x = PercentMissingThreshold))
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
  spp.temp<-temp00[[2]] ### By the way, which species are included in each category?
  tsn.temp<-temp00[[3]]  ### By the way, which species are included in each category by code number?
  
  NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
                                                strsplit(x = names(temp.orig)[1], 
                                                         split = "_")[[1]][2], 
                                              split = "[a-zA-Z]")[[1]][1]))[1]
  
  ### B. Enter base year

  ### C. Run the function
  temp00<-ImplicitQuantityOutput(temp = temp.orig, baseyr, 
                                 calcQEI = T, PercentMissingThreshold, 
                                 title0 = title0, place = place)
  temp<-temp00[[1]] #Data Output
  warnings.list0<-temp00[[2]] # Warnings
  figures.list0<-temp00[[3]] #Figures
  figures.list<-c(figures.list, figures.list0)
  
  ### D. Obtain the implicit quantity estimates
  
  #Raw
  write.csv(x = temp, file = paste0(dir.outputtables, title0,"_AllData.csv"))
  temp0raw[[r]]<-temp

  #Review
  temp0<-temp[, grepl(pattern = paste0("_", NumberOfSpecies), x = names(temp))]
  names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
  temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]

  temp0tot[[r]]<-temp0
  write.csv(x = temp0tot[[r]], file = paste0(dir.outputtables, title0,"_Review.csv"))
  
  #Final
  temp0<-temp[, grepl(pattern = paste0("0_", NumberOfSpecies, "Total"), x = names(temp))]
  names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
  temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
  
  temp0final[[r]]<-temp0
  write.csv(x = temp0final[[r]], file = paste0(dir.outputtables, title0,"_Final.csv"))
  
  #Report
  rmarkdown::render(ProdI.Report, 
                    output_dir = paste0(dir.reports), 
                    output_file = paste0(title0,".docx"))

}
  ########SPREADSHEETS########
  for (r in 1:length(reg.order)){
    
    #Raw
    # write.xlsx2(x = temp0raw[[r]], 
    #             file = paste0(dir.outputtables, "000_All", title000, "_Raw.xlsx"), 
    #             sheetName = reg.order[r], 
    #             col.names = T, row.names = T, append = T)
    
    #Print
    write.xlsx2(x = temp0final[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_FinalOutput.xlsx"), 
                sheetName = reg.order[r], 
                col.names = T, row.names = T, append = T)
    
    #Review
    write.xlsx2(x = temp0tot[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_Review.xlsx"), 
                sheetName = reg.order[r], 
                col.names = T, row.names = T, append = T)
    
  }
    
    ######PLOTS##########

  #Side by Side graphs
  figs<-unique(paste0(lapply(X = strsplit(x = names(figures.list),
                                         split = gsub(pattern = "\\.", replacement = "", x = category0)),
                            function(x) x[2])))
  
  for (i in 1:length(figs)){
    
    a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
    
    dir.create(paste0(dir.figures, "/", a, "/"))
    
  fig<-figs[i]
  list0<-figures.list[grep(pattern = fig, x = names(figures.list))]
  
  q<-grid.arrange(list0[[1]], 
                  list0[[2]], 
                  list0[[3]], 
                  list0[[4]], 
                  list0[[5]], 
                  list0[[6]], 
                  list0[[7]], 
                  nrow=3, newpage = FALSE)
  
  ggsave(filename = paste0(dir.figures, "/", a, "/", "000_All_baseyr",baseyr, 
                           "_",gsub(pattern = "\\.", replacement = "", x = category0), fig, ".png"), 
         plot = q, 
         width = 11, height = 8.5)
  }
  
  #make single plots
  for (i in 1:length(figures.list)) {
    
    a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
    
    dir.create(paste0(dir.figures, "/", a, "/"))
    
    ggsave(filename = paste0(dir.figures, "/", a, "/", names(figures.list)[i], ".png"), 
           plot = figures.list[[i]], 
           width = 11, height = 8.5)
  }

  
  
}

########*** No. 1############
# OutputAnalysis(landings.data, category0 = "category.taxsimp", baseyr = 2010, 
#                          reg.order, state.codes, 
#                          counter, dir.rawdata, dir.reports, PercentMissingThreshold = 1.00, dir.figures, dir.outputtables) 

########*** No. 2############
# OutputAnalysis(landings.data, category0 = "category.taxsimp", baseyr = 2007, 
#                reg.order, state.codes, 
#                counter, dir.rawdata, dir.reports, PercentMissingThreshold = 0.50, dir.figures, dir.outputtables) 


########*** No. 3############
# OutputAnalysis(landings.data, category0 = "category.orig", baseyr = 2010, 
#                reg.order, state.codes, 
#                counter, dir.rawdata, dir.reports, PercentMissingThreshold = 1.00, dir.figures, dir.outputtables) 

########*** No. 4############
# OutputAnalysis(landings.data, category0 = "category.orig", baseyr = 2007, 
#                reg.order, state.codes, 
#                counter, dir.rawdata, dir.reports, PercentMissingThreshold = 0.50, dir.figures, dir.outputtables) 


########*** No. 5############
category0 = "category.orig"

#Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr = 2007, 
               reg.order, state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, PercentMissingThreshold = 0.60) 

#Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
               category0, baseyr = 2007, 
               reg.order, state.codes, titleadd = "1997ToPresent",
               counter, dir.rawdata, PercentMissingThreshold = 0.60) 

#Data just since 2008
OutputAnalysis(landings.data[landings.data$Year>=2007,], baseyr = 2007, 
               reg.order, state.codes, titleadd = "2007ToPresent",
               counter, dir.rawdata, PercentMissingThreshold = 0.60) 

#Data for the whole timeseries with no PercentMissingThreshold
OutputAnalysis(landings.data, baseyr = 2007, 
               reg.order, state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, PercentMissingThreshold = 1) 

########*** No. 6############
category0 = "category.tax"

#Data for the whole Time Series
OutputAnalysis(landings.data, category0, baseyr = 2007, 
               reg.order, state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, PercentMissingThreshold = 0.60) 

#Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
               category0, baseyr = 2007, 
               reg.order, state.codes, titleadd = "1997ToPresent",
               counter, dir.rawdata, PercentMissingThreshold = 0.60) 

#Data just since 2008
OutputAnalysis(landings.data[landings.data$Year>=2007,], baseyr = 2007, 
               reg.order, state.codes, titleadd = "2007ToPresent",
               counter, dir.rawdata, PercentMissingThreshold = 0.60) 

#Data for the whole timeseries with no PercentMissingThreshold
OutputAnalysis(landings.data, baseyr = 2007, 
               reg.order, state.codes, titleadd = "WholeTimeseries",
               counter, dir.rawdata, PercentMissingThreshold = 1) 


########DOCUMENTATION#################
rmarkdown::render(ProdI.Docu.Out,
                  output_dir = paste0(dir.docu),
                  output_file = paste0("ProductivityIndex_Documentation_Out_",date0,".pdf"))

file.copy.rename(from = paste0(dir.docu, "ProductivityIndex_Documentation_Out_",date0,".pdf"),
               to = paste0(dir.output, "ProductivityIndex_Documentation_Output.pdf"))

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
