
#####LOAD CRAN LIBRARIES#######

#Seperating species by taxonomic group
# install.packages("remotes")
# remotes::install_github("ropensci/taxize")
library(taxize)

# Data Managment
library(tidyr)
library(reshape2)
library(tidyverse)
library(filesstrings)
library(data.table) # := to tag species codes
require(plyr)  #ddply function
library(sas7bdat)

#RMarkdown
library(rmarkdown)
library(knitr)
library(gridExtra)

#Excel File Management
library(officer)
library(xlsx)

#Visuals
library(ggplot2)

options(java.parameters = "-Xmx1000m")
options(scipen=10000)

ln<-log #tricky, tricky, Base R! Didn't fool me this time!!!

#########***########

####COLOR PALLET#####
#mostly for figures
NOAALightBlue<-"#C9E1E6"
NOAADarkBlue<-"#0098A6"
NOAADarkGrey<-"#56575A" #text
NOAABlueScale<-colorRampPalette(colors = c(NOAALightBlue, NOAADarkBlue))

#########***########
##########USER FUNCTIONS##############

echoTF<-function(typical, code = TRUE) {
  return(ifelse(code == TRUE, typical, FALSE))
}

includeTF<-function(typical, showresults = TRUE) {
  return(ifelse(showresults == TRUE, typical, FALSE))
}


file.copy.rename <- function(from, to) {
  todir <- dirname(to)
  fromdir <- dirname(from)
  toname<-strsplit(x = to, split = "/")[[1]][length(strsplit(x = to, split = "/")[[1]])]
  fromname<-strsplit(x = from, split = "/")[[1]][length(strsplit(x = from, split = "/")[[1]])]
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.copy(from = from, to = todir, overwrite = T)
  file.rename(from = paste0(todir, "/", fromname),  to = to)
}


CreateLoadedDataMetadata<-function(dir.out, data, title) {
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    # body_add_par("System Info:", style = "heading 2") %>%
    # body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    # body_add_par("R Version", style = "heading 2") %>%
    # body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
    body_add_par("Input Data used in this Run",
                 style = "heading 2")
  
  for (i in 1:length(loaded.data)){
    
    temp<-loaded.data[[i]]
    
    my_doc <- my_doc %>%
      body_add_par(names(loaded.data)[i], style = "heading 3") %>%
      body_add_table(head(temp)) %>%
      body_add_par(summary(temp), style = "Normal")
  }
  
  # a<-sessionInfo()
  # my_doc <- my_doc %>% 
  #   body_add_par("R Packages Loaded", style = "heading 2")
  # for (i in 1:length(a$basePkgs)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(a$basePkgs[i], style = "Normal") 
  # }
  # for (i in 1:length(a$otherPkgs)){
  #   temp<-a$otherPkgs[[i]]
  #   my_doc <- my_doc %>% 
  #     body_add_par(temp$Package,
  #                  style = "heading 3") %>%
  #     body_add_par(temp$Version, style = "Normal") %>%
  #     body_add_par(temp$Title, style = "Normal") %>%
  #     body_add_par(temp$Description, style = "Normal") %>%
  #     body_add_par(temp$SystemRequirements, style = "Normal") %>%
  #     body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
  #     body_add_par(temp$URL, style = "Normal")
  # }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}


CreateMetadata<-function(dir.out, title){
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 # body_add_par(paste0("Population Narrative of ", commorg, " (", fp_text(sciname, italic = T, color = "black", font.size=10), ")"," in ", region),
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    body_add_par("System Info:", style = "heading 2") %>%
    body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    body_add_par("R Version", style = "heading 2") %>%
    body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
  #   body_add_par("Populations Run in this Iteration", 
  #                style = "heading 2") 
  # for (i in 1:length(org_pop)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(org_pop[i], style = "Normal") 
  # }
  
  a<-sessionInfo()
  my_doc <- my_doc %>% 
    body_add_par("R Packages Loaded", style = "heading 2")
  for (i in 1:length(a$basePkgs)){
    my_doc <- my_doc %>% 
      body_add_par(a$basePkgs[i], style = "Normal") 
  }
  for (i in 1:length(a$otherPkgs)){
    temp<-a$otherPkgs[[i]]
    my_doc <- my_doc %>% 
      body_add_par(temp$Package,
                   style = "heading 3") %>%
      body_add_par(temp$Version, style = "Normal") %>%
      body_add_par(temp$Title, style = "Normal") %>%
      body_add_par(temp$Description, style = "Normal") %>%
      body_add_par(temp$SystemRequirements, style = "Normal") %>%
      body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
      body_add_par(temp$URL, style = "Normal")
  }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}

EditCommData<-function(dat, category0) {
  
  #Make Sure Data is Annual Aggregate
  temp0<-dat
  
  #remove negative values (they're warnings)
  temp0$Dollars[temp0$Dollars<0]<-NA
  temp0$Pounds[temp0$Pounds<0]<-NA
  
  lbs<-aggregate(temp0$Pounds,
                 by=list(year=temp0$Year, species=temp0$AFS_NAME1),
                 FUN=sum, na.rm=TRUE)
  names(lbs)[3]<-"POUNDS"
  
  dol<-aggregate(temp0$Dollars,
                 by=list(year=temp0$Year, species=temp0$AFS_NAME1, category=temp0[,names(temp0) %in% category0]),
                 FUN=sum, na.rm=TRUE)
  names(dol)[4]<-"DOLLARS"
  
  
  temp<-data.frame(merge(y = lbs, x = dol, by = c("year", "species")))
  
  
  cat<-unique(numbers0(as.numeric(factor(temp$category))))
  cat0<-unique(paste0(temp$category))
  
  #If there is only 1 species in a group, add it to "Other"
  for (i in 1:length(cat0)) {
    a<-data.frame(table(temp$category[temp$category %in% cat0[i]], temp$species[temp$category %in% cat0[i]]))
    a<-a[!(a$Freq %in% 0),]
    if (nrow(a) < 2) {
      temp$category[temp$category %in% cat0[i]]<-"Other"
      
    }
  }
  
  cat<-unique(numbers0(as.numeric(factor(temp$category))))
  cat0<-unique(paste0(temp$category))
  
  temp$species<-paste0(numbers0(as.numeric(factor(temp$category))), "_",
                       numbers0(x = as.numeric(factor(temp$species))), temp$species)
  
  
  spp.list<-list()
  tsn.list<-list()
  for (i in 1:length(cat0)) {
    spp.list[[i]]<-as.character(unique(dat$AFS.Name[dat[,names(dat) %in% category0] %in% cat0[i]]))
    tsn.list[[i]]<-as.character(unique(dat$Tsn[dat[,names(dat) %in% category0] %in% cat0[i]]))
    names(spp.list)[[i]]<-names(tsn.list)[[i]]<-cat0[i]
  }
  
  temp$category<-NULL
  
  #Quantity
  temp.q<-temp
  temp.q$POUNDS<-NULL
  temp.q<-spread(data = temp.q, key = year, value = DOLLARS)
  rownames(temp.q)<-paste0("Q", temp.q$species)
  temp.q$species<-NULL
  temp.q<-data.frame(t(temp.q))
  temp.q$temp<-rowSums(temp.q, na.rm = T)
  names(temp.q)[names(temp.q) %in% "temp"]<-paste0("QE",numbers0(x = c(0, length(cat0)))[1],"_", 
                                                   paste(rep_len(x = 0, 
                                                                 length.out = nchar(numbers0(x = as.numeric(factor(temp$species)))[1])), collapse = ""), 
                                                   "Total")
  for (i in 1:length(cat)) {
    if (sum(grepl(x = names(temp.q), pattern = paste0("Q", cat[i], "_"))) == 1) {
      temp.q$temp<-temp.q[,grepl(x = names(temp.q), pattern = paste0("Q", cat[i], "_")) ]
    } else {
      temp.q$temp<-rowSums(temp.q[,grepl(x = names(temp.q), pattern = paste0("Q", cat[i], "_")) ], na.rm = T)
    }
    names(temp.q)[names(temp.q) %in% "temp"]<-paste0("QE", cat[i], "_", 
                                                     paste(rep_len(x = 0, 
                                                                   length.out = nchar(numbers0(x = as.numeric(factor(temp$species)))[1])), collapse = ""), 
                                                     gsub(pattern = " ", replacement = "", x = cat0[i]))
  }
  
  #Value
  temp.v<-temp
  temp.v$DOLLARS<-NULL
  temp.v<-spread(data = temp.v, key = year, value = POUNDS)
  rownames(temp.v)<-paste0("V", temp.v$species)
  temp.v$species<-NULL
  temp.v<-data.frame(t(temp.v))
  
  temp.v$temp<-rowSums(temp.v, na.rm = T)
  names(temp.v)[names(temp.v) %in% "temp"]<-paste0("V",numbers0(x = c(0, length(cat0)))[1],"_", 
                                                   paste(rep_len(x = 0, 
                                                                 length.out = nchar(numbers0(x = as.numeric(factor(temp$species)))[1])), collapse = ""), 
                                                   "Total")
  for (i in 1:length(cat)) {
    if (sum(grepl(x = names(temp.v), pattern = paste0("V", cat[i], "_"))) == 1) {
      temp.v$temp<-temp.v[,grepl(x = names(temp.v), pattern = paste0("V", cat[i], "_")) ]
    } else {
      temp.v$temp<-rowSums(temp.v[,grepl(x = names(temp.v), pattern = paste0("V", cat[i], "_")) ], na.rm = T)
    }
    names(temp.v)[names(temp.v) %in% "temp"]<-paste0("V", cat[i], "_", 
                                                     paste(rep_len(x = 0, 
                                                                   length.out = nchar(numbers0(x = as.numeric(factor(temp$species)))[1])), collapse = ""), 
                                                     gsub(pattern = " ", replacement = "", x = cat0[i]))
  }
  
  
  
  temp<-cbind.data.frame(temp.q, temp.v)
  
  return(list(temp, spp.list, tsn.list))
}


#A function to caluclate the price change
PriceChange = function(R0, P0) {
  PCW<-rep_len(x = 0, length.out = length(P0))
  for (t in 2:length(P0)) {
    AverageR<-((R0[t]+R0[t-1])/2) #Average Revenue Share
    PC<-ln(P0[t]/P0[t-1]) #Price Change
    PCW[t]<-AverageR*PC #Revenue Share-Weighted Price Chage
  }
  return(PCW)
}

PriceIndex <- function(temp, BaseColName, baseyr) {
  ###Price Index for the entire commercial fishery ($PI_t$)
  
  # We calculate the price index first by comparing by multiplying the previous years $PI_{t-1}$ by that year's price change $PC_{t}$, where the PI of the first year $PI_{t=firstyear} = 1$
  # $$PI_t = PI_{t-1}*exp(ln(\frac{P_{i,t}}{P_{i,t-1}})) = PI_{t-1}*exp(PC_{t})$$
  # Where
  # $$PI_{i, t_{first year}} = 1$$
  
  #Note that the first row of this column is = 1
  tempPI_yr1<-c(1, rep_len(x = NA, length.out = nrow(temp)-1))
  
  PC0<-temp[,names(temp) %in% paste0("PC", BaseColName)] #this is equal to ln(P_it/P_it-1)
  
  # Calculate
  for (t in 2:length(tempPI_yr1)){  #Since the first row is defined, we need to start at the second row
    tempPI_yr1[t]<-tempPI_yr1[t-1]*exp(PC0[t])
  }
  
  tempPI_yr1<-data.frame(tempPI_yr1)
  rownames(tempPI_yr1)<-rownames(temp)
  
  # Then, to change the price (calulated later) into base year dollars, we use the following equation: 
  # $$PI_{t} = PI_{t}/PI_{t = baseyear}$$
  # In this example, we'll decide that the base year is `r baseyr`, for whatever reason. Notice that the $PI_{i,t=baseyr} = 1$ 
  
  tempPI_yrb<-tempPI_yr1/tempPI_yr1[rownames(tempPI_yr1) %in% baseyr,]
  
  tempPI<-data.frame(tempPI_yrb)
  names(tempPI)<-paste0("PI", BaseColName)
  
  return(tempPI)
}

ReplaceFirst<-function(colnames, temp) {
  for (c in 1:length(colnames)) {
    
    #If the first value of the timeseries of this column (c) is 0/NaN/NA
    #Change the first value (and subsequent 0/NaN/NA values) to the first available non-0/NaN/NA value
    if (temp[1,colnames[c]] %in% c(0, NA, NaN)) {
      findfirstvalue<-temp[which(!(temp[,colnames[c]]  %in% c(0, NA, NaN))), 
                           colnames[c]][1]
      temp[1,colnames[c]]<-findfirstvalue
    }
  }
  return(temp)
}

ReplaceMid<-function(colnames, temp) {
  for (c in 1:length(colnames)) {
    #If a middle value of the timeseries of this column (c) is 0/NaN/NA
    #Change the currently 0/NaN/NA value to the previous available non-0/NaN/NA value
    if (sum(temp[,colnames[c]] %in% c(0, NA, NaN))>0) {
      troublenumber<-which(temp[,colnames[c]] %in% c(0, NA, NaN))
      for (r in 1:length(troublenumber)){
        findlastvalue<-temp[troublenumber[r]-1, colnames[c]][1]
        temp[troublenumber[r],colnames[c]]<-findlastvalue
      }
    }
  }
  return(temp)
}

### Function to calculate the Implicit Quanity Output at Species and category Level
ImplicitQuantityOutput.species.cat<-function(temp, ii, baseyr, maxyr, minyr, pctmiss, warnings.list) {
  
  ########Housekeeping
  # Here I am just going to collect some housekeeping items
  temp<-data.frame(temp)
  
  NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
                                                strsplit(x = names(temp)[1], 
                                                         split = "_")[[1]][2], 
                                              split = "[a-zA-Z]")[[1]][1]))[1]
  
  NameBaseTotal<-substr(x = names(temp)[grep(x = names(temp), pattern = paste0(NumberOfSpecies, "Total"))][2],
                        start = 2, 
                        stop = nchar(names(temp)[grep(x = names(temp),
                                                      pattern = paste0(NumberOfSpecies, "Total"))][2]))
  
  NameBasecategory<-names(temp)[grepl(pattern = NumberOfSpecies, 
                                      x = names(temp)) &
                                  grepl(pattern = paste0("V", ii), x = names(temp))]
  
  NameBasecategory<-substr(start = 2,
                           stop = nchar(NameBasecategory),
                           x = NameBasecategory)
  
  
  VColumns<-grep(pattern = paste0("V", ii,"_"),
                 x = substr(x = names(temp),
                            start = 1,
                            stop = (2+nchar(ii))))
  
  VColumns<-VColumns[!(grepl(pattern = NameBasecategory,
                             x = names(temp)[VColumns]))]
  
  
  ###Remove any related V and Q data where V column has less data than the $pctmiss$
  VColumns0<-VColumns
  QColumns0<-QColumns<-which(names(temp) %in% 
                               paste0("Q", substr(x = names(temp)[VColumns], 
                                                  start = 2, 
                                                  stop = nchar(names(temp)[VColumns]))))
  
  for (i in 1:length(VColumns)) {
    
    #if the percent missing is less in V or Q columns for a species than the percentmissingtrheshold, we remove the data before the analysis
    if (sum(is.na(temp[VColumns[i]]))/nrow(temp) > pctmiss) {
      
      names(temp)[VColumns[i]]<-paste0("REMOVED_", names(temp)[VColumns[i]])
      VColumns0<-VColumns0[!(VColumns0 %in% VColumns[i])]
      names(temp)[QColumns[i]]<-paste0("REMOVED_", names(temp)[QColumns[i]])
      QColumns0<-QColumns0[!(QColumns0 %in% QColumns[i])]
    }
  }
  
  VColumns<-names(temp)[VColumns0]
  QColumns<-names(temp)[QColumns0]

  
  ###Caluclate Catagory Sums of $V$ and $Q$
  
  # Because we removed some columns for not meeting a perecent missing threshold and those columns will not be used at all in any part of the analysis, we need to calculate the totals of $V$ and $Q$ for the catagories and the fishery as a whole. 
  names(temp)[grep(pattern = NameBasecategory, x = names(temp))]<-
    paste0("REMOVED_", 
           names(temp)[grep(pattern = NameBasecategory, x = names(temp))])
  
  ####
  #if there are still columns to assess that haven't been "removed"
  PColumns<-c()
  if (length(VColumns) == 0) {
    warnings.list[length(warnings.list)+1]<-paste0("FYI: ", NameBasecategory, " is no longer being calculated because there were no more available columns of V after data was removed for not meeting the pctmiss")
    
  } else {
    
    # Q
  temp.q<-temp[,grepl(pattern = paste0("Q", ii), x = substr(names(temp), start = 1, stop = 2+nchar(ii))) ]
  temp.q<-data.frame(temp.q)
  if (ncol(temp.q)>1) {
    temp.q<-rowSums(temp.q, na.rm = T)
  }
  temp[ncol(temp)+1]<-temp.q
  names(temp)[ncol(temp)]<-paste0("QE",NameBasecategory)
  
  # V
  temp.v<-temp[,grepl(pattern = paste0("V", ii), x = substr(names(temp), start = 1, stop = 2+nchar(ii))) ]
  temp.v<-data.frame(temp.v)
  if (ncol(temp.v)>1) {
    temp.v<-rowSums(temp.v, na.rm = T)
  }
  temp[ncol(temp)+1]<-temp.v
  names(temp)[ncol(temp)]<-paste0("V",NameBasecategory)
  
  

  # #Caulculate the summed quantity
  # temp[,(ncol(temp)+1)]<-rowSums(temp[, grep(pattern = paste0("Q", strsplit(x = NameBasecategory, split = "_")[[1]][1], "_"), 
  #                                  x = names(temp))], na.rm = T)
  # names(temp)[ncol(temp)]<-paste0("QE", NameBasecategory)
  
  
  # Find which columns in this table are price Columns - we will need this for later
  PColumns<-paste0("P", substr(x = VColumns, 
                               start = 2, 
                               stop = nchar(VColumns)))
  
  #####Price for each species
  tempP<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(VColumns)) {
    
    NameBase<-substr(start = 2, 
                     stop = nchar(VColumns[c]), 
                     x = VColumns[c]) 
    
    Q0<-temp[,names(temp) %in% paste0("Q", NameBase)]
    V0<-temp[,names(temp) %in% paste0("V", NameBase)] #to make sure its the same column
    tempP[,c]<-V0/Q0
    names(tempP)[c]<-paste0("P", NameBase ) #name the column
  }
  
  tempP<-as.matrix(tempP)
  tempP[tempP %in% Inf]<-NA
  tempP<-data.frame(tempP)
  temp<-cbind.data.frame(temp, data.frame(tempP))
  
  # There may be instances where price cannot be calculated because there is no Q or V data for that species in that year. The next goal will be to calculate the price change, so we need to have a value in there that won't show change. If we left a 0 in the spot, then the price change from 0 to the next year would be huge and misrepresented on the index. To avoid this, we have to deal with two senarios:
  
  #### 2.3. V and/or Q are completely missing from the timeseries. In this case, we will remove the offending price columns entierly, so they don't influence the downstream price change or price index calculations.  
  
  #Find which columns in this table are price Columns
  cc<-c() #Empty
  for (c in 1:length(VColumns)) {
    
    #If price could never be caluclated at any point in the timeseries (is 0/NaN/NA) for a column (c) 
    #Remove the column from the analysis. 
    #We will not be removing the column from the data, but simply remove it from the varaible "PColumns"
    if (sum(temp[,PColumns[c]] %in% c(0, NA, NaN))/nrow(temp) > pctmiss |
        # sum(temp[,QColumns[c]] %in% c(0, NA, NaN))/nrow(temp) > pctmiss|
        sum(temp[,VColumns[c]] %in% c(0, NA, NaN))/nrow(temp) > pctmiss) {
      cc<-c(cc, c)#Collect offending columns
    }
  }
  
  if (length(cc)>0){
    PColumns<-PColumns[-cc]
    # VColumns<-VColumns[-cc]
  }
  
  }
  
  if (length(PColumns) == 0) {
    
    warnings.list[length(warnings.list)+1]<-paste0("FYI: ", NameBasecategory, " is no longer being calculated because there were no more available columns o P after data was removed for not meeting the pctmiss")
    
  } else {
    
  
  # 2.1. If the first value of P is 0 in a timeseries, we let the next available non-zero value of P in the timeseries inform the past.
  
  for (c in 1:length(PColumns)) {
    
    #If the first value of the timeseries of this column (c) is 0/NaN/NA
    #Change the first value (and subsequent 0/NaN/NA values) to the first available non-0/NaN/NA value
    if (temp[1,PColumns[c]] %in% c(0, NA, NaN)) {
      findfirstvalue<-temp[which(!(temp[,PColumns[c]]  %in% c(0, NA, NaN))), PColumns[c]][1]
      temp[1,PColumns[c]]<-findfirstvalue
    }
  }
  
  # 2.2. If there is a value in the middle of P's timeseries that is 0, we let the most recent past available non-zero of P in the timeseries inform the future.
  for (c in 1:length(PColumns)) {
    
    #If a middle value of the timeseries of this column (c) is 0/NaN/NA
    #Change the currently 0/NaN/NA value to the previous available non-0/NaN/NA value
    if (sum(temp[,PColumns[c]] %in% c(0, NA, NaN))>0) {
      troublenumber<-which(temp[,PColumns[c]] %in% c(0, NA, NaN))
      for (r in 1:length(troublenumber)){
        findlastvalue<-temp[troublenumber[r]-1, PColumns[c]][1]
        temp[troublenumber[r],PColumns[c]]<-findlastvalue
      }
    }
  }
  
  ###Fill in values of $V_{i,t,s}$ where P was able to be calculated
  # To ensure that the price index does not rise or fall to quickly with changes (that are really because of NA values) we fill in the missing instances of $V_{i,t,s}$. 
  # $$where \begin{cases} if: V_{i,t=1} = 0, then: V_{i,t=1} = V_{i,t=1+1...} \\ if: V_{i,t\neq1} = 0, then: V_{i,t} = V_{i,t-1} \end{cases}$$ 
    
  #### 1. If the first value of $V_{i,t,s}$ is 0/NA in a timeseries, we let the next available non-zero value of $V_{i,t,s}$ in the timeseries inform the past. 
  VVColumns<-paste0("V", substr(x = PColumns, start = 2, stop = nchar(PColumns)))
  temp<-ReplaceFirst(colnames = VVColumns, temp) 

  #### 2. If there is a value in the middle of $V_{i,t,s}$'s timeseries that is 0/NA, we let the most recent past available non-zero of $V_{i,t,s}$ in the timeseries inform the future. 
  temp<-ReplaceMid(colnames = VVColumns, temp) 

  ###Value of species where P was able to be calculated
  # $R_{i,t}$, defined and discussed in the subsequent step, will need to sum to 1 across all species in a category. Therefore, you will need to sum a new total of $V_{i,t}$ (called $VV_{i,t}$) for the category using only values for species that were used to calculate $P_{i,t}$ (called  $V_{s,i,t, available}$). 
  # $$VV_{i,t} = \sum_{s=1}^{n}(V_{s,i,t, available})$$
  #   where: 
  #   - $VV_{i,t}$ is the new total of $V_{i,t}$ (called $VV_{i,t}$) for the category using only values for species that were used to calculate $P_{i,t}$
  #   - $V_{s,i,t, available}$ are the $V_{s,i,t}$ where P were able to be calculated

  VVColumns<-paste0("V", substr(x = PColumns, start = 2, stop = nchar(PColumns)))
  
  temp[ncol(temp)+1]<-rowSums(data.frame(temp[,names(temp) %in% VVColumns]), na.rm = T)
  names(temp)[ncol(temp)]<-paste0("VV",NameBasecategory)
  
  ###Revenue Share for each species ($R_{s,i,t}$; e.g., Salmon and Flounder) 
  
  # $$R_{s,i,t} = V_{s,i,t}/VV_{i,t}$$
  #   where: 
  #   - $R_{s,i,t}$ is the revenue share per individual species (s), category (i), for each year (t)
  #   - $V_{s,i,t}$ is the value ($) per individual species (s), category (i), for each year (t)
  
  tempR<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(PColumns)) {
    
    #for renaming the columns
    NameBase<-substr(start = 2, 
                     stop = nchar(PColumns[c]), 
                     x = PColumns[c]) 
    
    VV<-temp[,names(temp) %in% paste0("VV", NameBasecategory)]  # sum of V where P was calculated
    V0<-temp[,names(temp) %in% paste0("V", NameBase)] #V of species; to make sure its the same column
    tempR[,c]<-V0/VV
    names(tempR)[c]<-paste0("R", NameBase) #name the column
  }
  
  tempR<-data.frame(tempR)
  temp<-cbind.data.frame(temp, tempR)
  
  #Note if there is an error
  if (sum(rowSums(tempR, na.rm = T)) != nrow(temp)) {
    warnings.list[length(warnings.list)+1]<-paste0("FYI: Rows of R_{s,i,t} for ",NameBasecategory," did not sum to 1")
  }
  
  #remove duplicates
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  
  
  ###Price Changes for each species ($PC_{s,i,t}$ aka $\Delta ln(P_{s,i,t})$; e.g., Salmon and Flounder)
  
  # $$PCW_{i,t,s} = \frac{R_{s,i,t} + R_{s,i,t-1}}{2} * ln(\frac{P_{s,i,t}}{P_{s,i,t-1}}) = \frac{R_{s,i,t} + R_{s,i,t-1}}{2} * [ln(P_{s,i,t}) - ln(P_{s,i,t-1})] $$
  # Where: 
    # - $PCW_{i,t,s}$ = Revenue share-weighted price change for a species (s)
  # Such that: 
    # - category's (i) Price Change for each species (s) = $\frac{R_{s,i,t} + R_{s,i,t-1}}{2}$
    # - category's (i) Revenue Share for each species (s) = $ln(\frac{P_{s,i,t}}{P_{s,i,t-1}} = [ln(P_{s,i,t}) - ln(P_{s,i,t-1})]$
                                                               
  #Find which columns in this table are price and revenue share columns
  tempPC<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(PColumns)){
    #For nameing columns
    NameBase<-substr(start = 2,
                     stop = nchar(PColumns[c]),
                     x = PColumns[c])
    
    # Calculate
    P0<-temp[, names(temp) %in% paste0("P", NameBase)]
    R0<-temp[, names(temp) %in% paste0("R", NameBase)] #to make sure its the same column
    tempPC[,c]<-PriceChange(R0, P0)
    names(tempPC)[c]<-paste0("PCW", NameBase ) #name the column
  }
  
  temp<-cbind.data.frame(temp, tempPC)
  
  ###Price Changes for the catagory ($PC_{i,t}$; e.g., Finfish)
  # $$PC_{i,t} = ln(\frac{P_{i,t}}{P_{i,t-1}}) = \sum_{s=1}^n(PCW_{i,t,s}) $$ 
  #   Where: 
  #   - $PC_{i,t}$ = Revenue share-weighted price change for a catagory (i)
  
  temp[ncol(temp)+1]<-rowSums(tempPC, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("PC", NameBasecategory)
  
  ###4. Price Changes for each species ($PC_{s,i,t}$ aka $\Delta ln(P_{s,i,t})$; e.g., Salmon and Flounder)
  
  #Find which columns in this table are price and revenue share columns
  tempPC<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(PColumns)){
    #For nameing columns
    NameBase<-substr(start = 2,
                     stop = nchar(PColumns[c]),
                     x = PColumns[c])
    
    # Calculate
    P0<-temp[, names(temp) %in% paste0("P", NameBase)]
    R0<-temp[, names(temp) %in% paste0("R", NameBase)] #to make sure its the same column
    tempPC[,c]<-PriceChange(R0, P0)
    names(tempPC)[c]<-paste0("PC", NameBase ) #name the column
  }
  
  temp[ncol(temp)+1]<-rowSums(tempPC, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("PC", NameBasecategory)
  
  
  
  #remove duplicates
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  
###6. Price Indexes for each category ($PI_{i,t}$; Finfish & others and Shellfish)
# We calculate the price index first by comparing by multiplying the previous years $PI_{t-1}$ by that year's price change $PC_{t}$, where the PI of the first year $PI_{t=firstyear} = 1$
  ###Price Index for the each category ($PI_t$)
  # 
  # We calculate the price index first by comparing by multiplying the previous years $PI_{t-1}$ by that year's price change $PC_{t}$, where the PI of the first year $PI_{t=firstyear} = 1$
  # $$PI_t = PI_{t-1}*exp(ln(\frac{P_{i,t}}{P_{i,t-1}})) = PI_{t-1}*exp(PC_{t})$$
  # Where
  # $$PI_{i, t_{first year}} = 1$$
  #Note that the first row of this column is = 1
  # 
  # Then, to change the price (calulated later) into base year dollars, we use the following equation: 
    # $$PI_{t} = PI_{t}/PI_{t = baseyear}$$
    # In this example, we'll decide that the base year is `r baseyr`, for whatever reason. Notice that the $PI_{i,t=baseyr} = 1$ 
  
  tempPI<-PriceIndex(temp, BaseColName = NameBasecategory, baseyr)
  temp[ncol(temp)+1]<-(tempPI)
  names(temp)[ncol(temp)]<-paste0("PI", NameBasecategory)
  
  
  #remove duplicates
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  
  ###7. Implicit Quantity/Output for each category ($Q_{i,t}$; Finfish & others and Shellfish)
  temp[,ncol(temp)+1]<-temp[,names(temp) %in% paste0("V", NameBasecategory)]/
    temp[,names(temp) %in% paste0("PI", NameBasecategory)]
  
  names(temp)[ncol(temp)]<-paste0("Q", NameBasecategory)
  
  
  #############WARNINGS
  ####1. When back calculated, $V_t$ did not equal $P_t * Q_{t}$
  # $$V_i = P_t * Q_i$$
  
  temp0<-temp[names(temp) %in% c(paste0("Q",NameBasecategory), 
                                 paste0("PI",NameBasecategory), 
                                 paste0("V",NameBasecategory))]
  
  temp0[,(ncol(temp0)+1)]<-temp0[,paste0("Q",NameBasecategory)]*temp0[,paste0("PI",NameBasecategory)]
  names(temp0)[ncol(temp0)]<-paste0("V", NameBasecategory, "_Check")
  
  if ((length(setdiff(as.character(temp0[,paste0("V", NameBasecategory, "_Check")]), 
                      as.character(temp0[,paste0("V", NameBasecategory)]))) != 0)) {
    warnings.list[length(warnings.list)+1]<-"Warning: When back calculated, V_{i,t} did not equal PI_{i,t} * Q_{i,t}"
  }
  
  
  ####2. When back calculated, $Q_{t}$ did not equal $V_t / P_{t}$
  # $$Q_{i,t} = V_t / P_{i,t}$$
  
  temp0[,(ncol(temp0)+1)]<-temp0[, paste0("V", NameBasecategory)]/temp0[, paste0("PI", NameBasecategory)]
  names(temp0)[ncol(temp0)]<-paste0("Q", NameBasecategory, "_Check")
    
 if (length(setdiff(as.character(temp0[,paste0("Q", NameBasecategory, "_Check")]), 
                                 as.character(temp0[,paste0("Q", NameBasecategory)]))) != 0) {
    warnings.list[length(warnings.list)+1]<-"Warning: When back calculated, Q_{i,t} did not equal V_{i,t}/PI_{i,t}"
  }
  
}
  return(list(temp, warnings.list))
}

### Function to calculate the Implicit Quanity Output at Fishery Level
ImplicitQuantityOutput<-function(temp, baseyr, pctmiss = 1.00, 
                                 title0 = "", place = ""){
  
  temp.orig<-temp
  
  warnings.list<-list()
  figures.list<-list()
  
  NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
                                              strsplit(x = names(temp)[3], 
                                                       split = "_")[[1]][2], 
                                            split = "[a-zA-Z]")[[1]][1]))[1]
  
  #####HOUSEKEEPING
  NameBaseTotal<-substr(x = names(temp)[grep(x = names(temp), pattern = paste0(NumberOfSpecies, "Total"))][2],
                        start = 2, stop = nchar(names(temp)[grep(x = names(temp),
                                                                 pattern = paste0(NumberOfSpecies, "Total"))][2]))
  
  ######START ANALYSIS
  category<-unique(as.character(lapply(X = strsplit(x = as.character(names(temp)), 
                                             split = paste0("_")), 
                                function(x) x[1])))
  category<-unique(substr(x = category, start = 2, stop = nchar(category)))
  category<-category[!grepl(pattern = "[a-zA-Z]", x = category)]
  category<-category[!(category %in% numbers0(c(0, (category)[1]))[1])]
  
  temp0<-data.frame(rep_len(x = NA, length.out = nrow(temp)))
  tempPC<-data.frame(rep_len(x = NA, length.out = nrow(temp0)))
  tempQC<-data.frame(rep_len(x = NA, length.out = nrow(temp0)))
  
  maxyr<-max(rownames(temp))
  minyr<-min(rownames(temp))
  
  category<-category00<-sort(category)
  category.rm<-c()
  
  for (ii in 1:length(category)){

    VColumns<-grep(pattern = paste0("V", category[ii],"_"),
                   x = substr(x = names(temp),
                              start = 1,
                              stop = (2+nchar(category[ii]))))
    
    NameBasecategory<-names(temp)[grepl(pattern = NumberOfSpecies, 
                                        x = names(temp)) &
                                    grepl(pattern = paste0("V", category[ii]), x = names(temp))]
    
    NameBasecategory<-substr(start = 2,
                             stop = nchar(NameBasecategory),
                             x = NameBasecategory)
    
    VColumns<-VColumns[!(grepl(pattern = NameBasecategory,
                               x = names(temp)[VColumns]))]
    
    #if there are still columns to assess that haven't been "removed"
    # if (length(VColumns) != 0) {
    ###Append species and category level calculations
    temp00<-ImplicitQuantityOutput.species.cat(temp, ii=category[ii],
                                               baseyr, maxyr, minyr, 
                                               pctmiss, warnings.list)

    
    #If data for a catagory is no longer available after precentmissingthreshold etc, remove it from the category lineup
    if (sum(names(temp00[1][[1]]) %in% paste0("PI", NameBasecategory)) == 0) {
      category.rm<-c(category.rm, ii)
    } else {
     temp0<-cbind.data.frame(temp0, temp00[[1]])
    }
    
    ###Remove duplicate columns
    temp0<-temp0[, !(grepl(pattern = "\\.[0-9]+", x = names(temp0)))]    
    
    warnings.list<-ImplicitQuantityOutput.species.cat(temp, ii=category[ii],
                                     baseyr, maxyr, minyr, 
                                     pctmiss, warnings.list)[[2]]
    


  }
  
  if(!(is.null(category.rm))) {
    category<-category[-category.rm]
  }
  
  temp<-temp0#[,2:ncol(temp0)]
  
    
    ###8. Value for all fisheries for species where P was able to be calculated
    # $R_{i,t}$, defined and discussed in the subsequent step, will need to sum to 1 across all species in a category. Therefore, you will need to sum a new total of $V_{i,t}$ (called $VV_{t}$) for the category using only values for species that were used to calculate $P_{i,t}$. 
    # $$VV_{t} = \sum_{s=1}^{n}(VV_{i,t})$$ 
    # where: 
    # - $VV_{t}$ is the new total of $V_{i,t}$ for the entire fishery using only values for species that were used to calculate $P_{i,t}$
    
    temp[,ncol(temp)+1]<-rowSums(temp[,grep(pattern = "VV", x = names(temp))], na.rm = T)
    names(temp)[ncol(temp)]<-paste0("VV",NameBaseTotal)
    
    ###Revenue Share for the entire commercial fishery ($R_t$)
    # $$R_{i,t} = V_{i,t}/V_{t}$$
    # where: 
    # - $R_{i,t}$ is the revenue share per individual species (s), category (i), for each year (t)
    # - $V_{i,t}$ is the value ($) per individual species (s), category (i), for each year (t)
    #Here, we don't use $VV_{t}$ beacause we want to expand the proportion to include all of the species caught, regardless if they were used in the price calculations. 
    
    names(temp)[names(temp) %in% paste0("V", NameBaseTotal)]<-paste0("REMOVED_V", NameBaseTotal)
    
    temp0<-temp[grep(x = names(temp), 
                     pattern = paste0("V[1-9]+_", NumberOfSpecies))]
    temp0<-temp0[,-(grep(x = names(temp0), pattern = c("VV")))]
    temp0<-temp0[,-(grep(x = names(temp0), pattern = c("REMOVED_")))]
    
    temp[ncol(temp)+1]<-rowSums(temp0, na.rm = T)
    names(temp)[ncol(temp)]<-paste0("V", NameBaseTotal)
    
    #remove duplicates
    temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
    temp <- temp[, !duplicated(colnames(temp))]
    temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
    temp <- temp[, !duplicated(colnames(temp))]
    
    
    tempR<-data.frame(rep_len(x = NA, length.out = nrow(temp)))
    
    for (ii in 1:length(category)){
      
      NameBasecategory<-paste0(category[ii], "_", NumberOfSpecies)
      NameBasecategory<-paste0(NameBasecategory, 
                               strsplit(x = names(temp)[grep(pattern = NameBasecategory, 
                                                             x = names(temp))][1], split = NumberOfSpecies)[[1]][2])
      
      # names(temp)[names(temp) %in% paste0("V", NameBaseTotal)]<-paste0("REMOVED_V", NameBaseTotal)
      # 
      # temp0<-temp[grep(x = names(temp),
      #                  pattern = paste0("V[1-9]+_", NumberOfSpecies))]
      # temp0<-temp0[,-(grep(x = names(temp0), pattern = c("VV")))]
      # temp0<-temp0[,-(grep(x = names(temp0), pattern = c("REMOVED_")))]
      # 
      # temp[ncol(temp)+1]<-rowSums(temp0, na.rm = T)
      # names(temp)[ncol(temp)]<-paste0("V", NameBaseTotal)
      
      #remove duplicates
      temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
      temp <- temp[, !duplicated(colnames(temp))]
      temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
      temp <- temp[, !duplicated(colnames(temp))]
      
    
    tempR[,ii]<-data.frame(temp[,names(temp) %in% paste0("V", NameBasecategory)]/
                             temp[,names(temp) %in% paste0("V", NameBaseTotal)])
    names(tempR)[ii]<-paste0("R", NameBasecategory)

    temp[,ncol(temp)+1]<-tempR[,ii]
    names(temp)[ncol(temp)]<-paste0("R", NameBasecategory)
    
    #remove duplicates
    temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
    temp <- temp[, !duplicated(colnames(temp))]
    temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
    temp <- temp[, !duplicated(colnames(temp))]
    
    
    ###8. Price Changes for the entire commercial fishery ($PC_t$)
    # Measure output price changes ($PC_t$) for total output ($Q_t$) using $R_{i,t}$ and $P_{i,t}$ estimates. 
    
    # $$PC_{t} = ln(\frac{P_{t}}{P_{t-1}}) =  \sum_{i=1}^n([\frac{R_{i,t} + R_{i,t-1}}{2}] * [ln(P_{i,t}) - ln(P_{i,t-1})]) $$
    tempPC[,ii]<-PriceChange(R0 = temp[, names(temp) %in% 
                                          paste0("R", NameBasecategory) & 
                                         !grepl(pattern = "REMOVED_", x = names(temp))], 
                             P0 = temp[, names(temp) %in% 
                                          paste0("PI", NameBasecategory) & 
                                         !grepl(pattern = "REMOVED_", x = names(temp))])
    
    names(tempPC)[ii]<-paste0("PC", NameBasecategory)
    
    #Calculate Quantity Change
    tempQC[,ii]<-PriceChange(R0 = temp[, names(temp) %in% 
                                          paste0("R", NameBasecategory) & 
                                         !grepl(pattern = "REMOVED_", x = names(temp))], 
                             P0 = temp[, names(temp) %in% 
                                          paste0("Q", NameBasecategory) & 
                                         !grepl(pattern = "REMOVED_", x = names(temp))])
    
    names(tempQC)[ii]<-paste0("QC", NameBasecategory)
    
  }
  
  temp<-cbind.data.frame(temp, tempPC)
  temp[,ncol(temp)+1]<-rowSums(tempPC, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("PC", NameBaseTotal)

  #Note if there is an Error
  if (sum(rowSums(tempR, na.rm = T)) != nrow(temp)) {
    warnings.list[length(warnings.list)+1]<-paste0("Warning: Rows of R_{i,t} for ",NameBaseTotal," did not sum to 1")
  }
  
  ###14.  Solve Output portion of the equation for the Output Changes: 
  # $$QC = \sum_{i=1}^n((\frac{R_{it} + R_{it-1}}{2}) * ln(\frac{Q_{it}}{Q_{it-1}}))$$
  temp<-cbind.data.frame(temp, tempQC)
  temp[,ncol(temp)+1]<-rowSums(tempQC, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("QC", NameBaseTotal)
  
  ###Price Index for the entire commercial fishery ($PI_t$)
  # We calculate the price index first by comparing by multiplying the previous years $PI_{t-1}$ by that year's price change $PC_{t}$, where the PI of the first year $PI_{t=firstyear} = 1$
  # $$PI_t = PI_{t-1}*exp(ln(\frac{P_{i,t}}{P_{i,t-1}})) = PI_{t-1}*exp(PC_{t})$$
  # Where
  # $$PI_{i, t_{first year}} = 1$$
  #Note that the first row of this column is = 1
  # 
  # Then, to change the price (calulated later) into base year dollars, we use the following equation: 
  # $$PI_{t} = PI_{t}/PI_{t = baseyear}$$
  # In this example, we'll decide that the base year is `r baseyr`, for whatever reason. Notice that the $PI_{i,t=baseyr} = 1$ 
  tempPI<-PriceIndex(temp, BaseColName = NameBaseTotal, baseyr)
  temp[ncol(temp)+1]<-(tempPI)
  names(temp)[ncol(temp)]<-paste0("PI", NameBaseTotal)
  
  ### 11. Total Implicit Quantity/Output for the entire commercial fishery ($Q_t = Y_t$)
  # To get quantity estimates for total output using total value of landings divided by price index as follow: $Y=V/I$
  
  temp[,ncol(temp)+1]<-temp[,names(temp) %in% paste0("V", NameBaseTotal)]/
    temp[, names(temp) %in% paste0("PI", NameBaseTotal)]
  names(temp)[ncol(temp)]<-paste0("Q", NameBaseTotal)
  
  ### 12. Total Implicit Quantity/Output Index
    temp[,ncol(temp)+1]<-temp[,names(temp) %in% paste0("Q", NameBaseTotal)]/
      temp[rownames(temp) %in% baseyr, names(temp) %in% paste0("Q", NameBaseTotal)]
    names(temp)[ncol(temp)]<-paste0("QI", NameBaseTotal)
    
    
    ### 13. Sum Total Implicit Quantity/Output Index
    temp[,ncol(temp)+1]<-temp[,names(temp) %in% paste0("QE", NameBaseTotal)]/
      temp[rownames(temp) %in% baseyr, names(temp) %in% paste0("QE", NameBaseTotal)]
    names(temp)[ncol(temp)]<-paste0("QEI", NameBaseTotal)
  
  
  #Remove Duplicate Columns
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  
  
  ## Analysis Warnings Checks
  
  # To make sure our analyses worked as inteded, let's see if we can back calculate our numbers.
  # We want the calcuated V to equal this check:
  
  ####1. When back calculated, $V_t$ did not equal $PI_t * Q_{t}$
  # $$V_i = P_t * Q_i$$
  
  temp0<-temp[names(temp) %in% c(paste0("Q",NameBaseTotal), 
                                 paste0("PI",NameBaseTotal), 
                                        paste0("V",NameBaseTotal))]
  
  temp0[,(ncol(temp0)+1)]<-temp0[,paste0("Q",NameBaseTotal)]*temp0[,paste0("PI",NameBaseTotal)]
  names(temp0)[ncol(temp0)]<-paste0("V", NameBaseTotal, "_Check")
  
  if (length(setdiff(as.character(temp0[,paste0("V", NameBaseTotal, "_Check")]), 
                     as.character(temp0[,paste0("V", NameBaseTotal)]))) != 0) {
  warnings.list[length(warnings.list)+1]<-"Warning: When back calculated, V_t did not equal PI_t * Q_t"
  }

  
  ####2. When back calculated, $Q_{t}$ did not equal $V_t / PI_{t}$
  # $$Q_{i,t} = V_t / P_{i,t}$$
  
  temp0[,(ncol(temp0)+1)]<-temp0[,paste0("V",NameBaseTotal)]/temp0[,paste0("PI",NameBaseTotal)]
  names(temp0)[ncol(temp0)]<-paste0("Q", NameBaseTotal, "_Check")
  
  if (length(setdiff(as.character(temp0[,paste0("Q", NameBaseTotal, "_Check")]), 
                     as.character(temp0[,paste0("Q", NameBaseTotal)]))) != 0) {
  warnings.list[length(warnings.list)+1]<-"Warning: When back calculated, Q_t did not equal V_t/PI_t"
  }
  
  
  ####3. When back calculated, growth rate ?
  
  # $$ln(Q_t/Q_{t-1}) = \sum( ( \frac{R_{i, t} + R_{i, t-1}}{2})  * ln(\frac{Q_{i,t}}{Q_{i,t-1}}))$$  
  
  #Remove Duplicate Columns
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]

  names0<-c(paste0("Q",NameBaseTotal))
  for (ii in 1:length(category)) {
    names0<-c(names0, 
              names(temp)[grep(pattern = paste0("Q", category[ii], "_", NumberOfSpecies), names(temp))],
              names(temp)[grep(pattern = paste0("R", category[ii], "_", NumberOfSpecies), names(temp))])
  }
  
  temp0<-temp[,unique(names0)]
  
  temp0[,(ncol(temp0)+1)]<-c(NA, ln(temp0[-nrow(temp0),paste0("Q",NameBaseTotal)]/
                                      temp0[-1,paste0("Q",NameBaseTotal)]))
  names(temp0)[ncol(temp0)]<-"part1"
  
  temp00<-data.frame(rep_len(x = NA, length.out = nrow(temp)))
  for (ii in 1:length(category)) { 
    R0<-temp0[,grep(pattern = paste0("R", category[ii]), x = names(temp0)) ]
    Q0<-temp0[,grep(pattern = paste0("Q", category[ii]), x = names(temp0)) ]
    
    for (r in 2:(nrow(temp))){
      temp00[r,ii]<-(((R0[r] + R0[r-1])/2) * ln(Q0[r] / Q0[r-1]) )
    }
  }
  
  temp0[,(ncol(temp0)+1)]<-c(NA, rowSums(temp00, na.rm = T)[2:length(rowSums(temp00, na.rm = T))])
  names(temp0)[ncol(temp0)]<-"part2"
  
  if (length(setdiff(as.character(temp0[,"part1"]), 
                     as.character(temp0[,"part2"]))) != 0) {
    warnings.list[length(warnings.list)+1]<-"Warning: When back calculated, ln(Q_t/Q_{t-1}) = did not equal sum( ( R_{i, t} - R_{i, t-1} ) / 2 )  x ln( (Q_{i,t}) / (Q_{i,t-1} ) )"
  }
  
  
  
  ####4. Missing Data
  
  #value
  a<-temp
  a<-a[,grep(pattern = "V[1-9]+_[1-9]+", x = names(a))]
  if(length(grep(pattern = "REMOVED_", x = names(a)) & 
            grep(pattern = "Total", x = names(a), ignore.case = T)) != 0 ){ 
    a<-a[,-c(grep(pattern = "REMOVED_", x = names(a)), grep(pattern = "Total", x = names(a), ignore.case = T))]
  }
  ncol0<-ncol(a)
  aa<-0
  a<-data.frame(a)
  if(ncol(a) != 0){
    for (iii in 1:ncol(a)) {
    aa<-c(aa, ifelse(sum(a[,iii] %in% c(NA, NaN, 0)) == nrow(a), iii, NA))
  }
  vv<-(aa[!(is.na(aa))])
} else {
  pp<-0
}
  #quantity
  a<-temp
  a<-a[,grep(pattern = "Q[1-9]+_[1-9]+", x = names(a))]
  if(length(grep(pattern = "REMOVED_", x = names(a)) & 
            grep(pattern = "Total", x = names(a), ignore.case = T)) != 0 ){ 
    a<-a[,-c(grep(pattern = "REMOVED_", x = names(a)), grep(pattern = "Total", x = names(a), ignore.case = T))]
  }
  ncol0<-ncol(a)
  aa<-0
  a<-data.frame(a)
  if(ncol(a) != 0){
    for (iii in 1:ncol(a)) {
    aa<-c(aa, ifelse(sum(a[,iii] %in% c(NA, NaN, 0)) == nrow(a), iii, NA))
  }
  qq<-(aa[!(is.na(aa))])
  } else {
    pp<-0
  }
  #Price
  a<-temp
  a<-a[,grep(pattern = "P[1-9]+_[1-9]+", x = names(a))]
  if(length(grep(pattern = "REMOVED_", x = names(a)) & 
            grep(pattern = "Total", x = names(a), ignore.case = T)) != 0 ){ 
    a<-a[,-c(grep(pattern = "REMOVED_", x = names(a)), grep(pattern = "Total", x = names(a), ignore.case = T))]
  }
  ncol0<-ncol(a)
  aa<-0
  
  a<-data.frame(a)
  if (ncol(a) != 0) {
   for (iii in 1:ncol(a)) {
    aa<-c(aa, ifelse(sum(a[,iii] %in% c(NA, NaN, 0)) == nrow(a), iii, NA))
   }
    pp<-(aa[!(is.na(aa))])
  } else {
    pp<-0
  }
  
  
  warnings.list[length(warnings.list)+1]<-paste0("FYI: ", ifelse(length(vv)==1, 0, length(vv)-1) ,
                                                 " of species V columns are completely empty, ", 
                                                 ifelse(length(qq)==1, 0, length(qq)-1) ,
                                                 " of species Q columns are completely empty, and ", 
                                                 ifelse(length(pp)==1, 0, length(pp)-1) ," of ", ncol0,
                                                 " species P columns are completely empty. ")
  
  
  # ####5. Negative Numbers
  # a0<-landings.data[idx,]
  # a0[(a0$Dollars<0 & !(is.na(a0$Dollars))),] %>%
  #   knitr::kable(row.names = F, booktabs = T)
  # 
  # a0<-landings.data[idx,]
  # a0[(a0$Pounds<0 & !(is.na(a0$Pounds))),] %>%
  #   knitr::kable(row.names = F, booktabs = T)
  # 
  # if (sum(temp0[,"part1"] %in% temp0[,"part2"]) != nrow(temp0))  {
  #   warnings.list[length(warnings.list)+1]<-"When back calculated, ln(Q_t/Q_{t-1}) = did not equal sum( ( frac{R_{i, t} - R_{i, t-1}}{2})  * ln(\frac{Q_{i,t}}{Q_{i,t-1}}))"
  # }
  
  ###########################################GRAPHS
  
  
  
  
  #####Calculated Q by Species
  title00<- "_QBySpecies"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("Q[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  
  a0$Year<-rownames(a0)
  
  a <- gather(a0, Category, val, names(a0)[grepl(pattern = NumberOfSpecies, x = names(a0))], factor_key=TRUE)
  
  a$cat<-as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[2]))
  
  temp0<-a
  
  g<-plotnlines(dat = temp0, title00, place)
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  #####Summed Q By Species
  title00<- "_QE-Species"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("QE[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  
  a0$Year<-rownames(a0)
  
  a <- gather(a0, Category, val, names(a0)[grepl(pattern = NumberOfSpecies, x = names(a0))], factor_key=TRUE)
  
  a$cat<-as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[2]))
  
  temp0<-a

  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  #####Price Index
  title00<- "_PI-Line"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("PI[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  
  a0$Year<-rownames(a0)
  
  a <- gather(a0, Category, val, names(a0)[grepl(pattern = NumberOfSpecies, x = names(a0))], factor_key=TRUE)
  
  a$cat<-as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[2]))
  
  temp0<-a
  
  g<-plotnlines(dat = temp0, title00, place) 

  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  ########VV
  title00<- "_VV-Line"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("VV[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  
  a0$Year<-rownames(a0)
  
  a <- gather(a0, Category, val, names(a0)[grepl(pattern = NumberOfSpecies, x = names(a0))], factor_key=TRUE)
  
  a$cat<-as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[2]))
  
  temp0<-a
  
  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  ########V
  title00<- "_V-Line"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("V[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  a0<-a0[,-grep(pattern = "REMOVED_", x = names(a0))]
  a0<-a0[,-grep(pattern = "VV[0-9]+_", x = names(a0))]
  
  a0$Year<-rownames(a0)
  
  a <- gather(a0, Category, val, names(a0)[grepl(pattern = NumberOfSpecies, x = names(a0))], factor_key=TRUE)
  
  a$cat<-as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[2]))
  
  temp0<-a
  
  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  
  ########V and VV
  title00<- "_VvVV-Line"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("V[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  a0<-a0[,-grep(pattern = "REMOVED_", x = names(a0))]
  # a0<-a0[,-grep(pattern = "VV[0-9]+_", x = names(a0))]
  
  a0$Year<-rownames(a0)
  
  a <- gather(a0, Category, val, names(a0)[grepl(pattern = NumberOfSpecies, x = names(a0))], factor_key=TRUE)
  
  a$cat<-paste0(as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[1])), 
                     "_",
                     as.character(lapply(X = strsplit(x = as.character(a$Category), split = paste0("_", NumberOfSpecies)), function(x) x[2])))
  
  temp0<-a
  
  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  ########VE
  title00<- "_VE-Line"
  
  a0<-data.frame(temp[,grepl(
    pattern = paste0("V[0-9]+_", NumberOfSpecies), 
    x = names(temp))])
  
  a0<-a0[,grep(pattern = "REMOVED_", x = names(a0))]
  names(a0)<-gsub(pattern = "REMOVED_", replacement = "", x = names(a0))
  names(a0)<-paste0("VE", substr(x = names(a0), start = 3, stop = nchar(names(a0))))
  
  a0$Year<-rownames(a0)
  
  temp0<-a0
  
  temp0<-gather(temp0, cat, val, 
                names(temp0)[1]:names(temp0)[length(names(temp0))-1], 
                factor_key = T)  
  
  temp0$cat<-paste0(as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[1])), 
                "_",
                as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                 split = paste0("_", NumberOfSpecies)), function(x) x[2])))
  
  
  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  #### Quantity Index Compare
  # For comparison, let's recreate those graphs to make sure we are getting the same output:
  title00<-"_QIvQEI-Line"
  
  temp0<-temp
  temp0$Year<-rownames(temp0)
  
  temp0<-data.frame(temp0[,names(temp0) %in% c("Year", 
                                               paste0("QI", NameBaseTotal), 
                                               paste0("QEI", NameBaseTotal))])
  temp0$Year<-rownames(temp)
  
  temp0<-gather(temp0, cat, val, 
                names(temp0)[1]:names(temp0)[length(names(temp0))-1], 
                factor_key = T)  
  
  temp0$cat<-paste0(as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[1])), 
                    "_",
                    as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[2])))
  
  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  #### Quantity Compare
  title00<-"_QvQE-Line"
  temp0<-temp
  temp0$Year<-rownames(temp0)
  
  temp0<-data.frame(temp0[,names(temp0) %in% c("Year", 
                                               paste0("Q", NameBaseTotal), 
                                               paste0("QE", NameBaseTotal))])
  temp0$Year<-rownames(temp)
  
  temp0<-gather(temp0, cat, val, 
                names(temp0)[1]:names(temp0)[length(names(temp0))-1], 
                factor_key = T)  
  
  temp0$cat<-paste0(as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[1])), 
                    "_",
                    as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[2])))
  
  g<-plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  #### Revenue Share
  title00<-"_R-Line"
  temp0<-temp
  
  temp0<-temp0[grep(pattern = paste0("R[0-9]+_", NumberOfSpecies), x = names(temp0))]
  temp0$Year<-rownames(temp)
  
  temp0<-gather(temp0, cat, val, 
                names(temp0)[1]:names(temp0)[length(names(temp0))-1], 
                factor_key = T)  

  plotnlines(dat = temp0, title00, place) 
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  #### Price Change
  title00<-"_PC-Line"
  temp0<-temp
  
  temp0<-temp0[grep(pattern = paste0("PC[0-9]+_", NumberOfSpecies), x = names(temp0))]
  temp0$Year<-rownames(temp)
  
  temp0<-gather(temp0, cat, val, 
                names(temp0)[1]:names(temp0)[length(names(temp0))-1], 
                factor_key = T)  
  
  temp0$cat<-paste0(as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[1])), 
                    "_",
                    as.character(lapply(X = strsplit(x = as.character(temp0$cat), 
                                                     split = paste0("_", NumberOfSpecies)), function(x) x[2])))

  g<-plotnlines(dat = temp0, title00, place)
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  
  #################Number Missing V Per Year
  title00<- "_VNumberMissing-Line"
  
  a0<-data.frame(temp.orig[,grepl(
    pattern = paste0("V[0-9]+_"), 
    x = names(temp.orig)) & 
      !(grepl(
        pattern = paste0("V[0-9]+_", NumberOfSpecies), 
        x = names(temp.orig)))])
  
  total.no.v<-ncol(a0)*nrow(a0)
  
  cat0<-data.frame(names0 = (names(temp.orig)[grepl(pattern = paste0("V[0-9]+_", NumberOfSpecies), 
                                                    x = names(temp.orig))]))
  
  cat0$no<-as.character(lapply(X = strsplit(x = as.character(cat0$names0), split = "_"), 
                                       function(x) x[1]))
  
  cat0$numberofspp<-NA
  for (i in 1:nrow(cat0)) {
    cat0$numberofspp[i]<-length(grep(pattern = cat0$no[i], 
                                    x = substr(x = names(a0), 
                                               start = 1, 
                                               stop = max(nchar(cat0$no))) ))
  }
  
  
  cat0$no<-as.character(gsub(pattern = "[a-zA-Z]", replacement = "", x = cat0$no))
  
  cat0$catname<-as.character(lapply(X = strsplit(x = as.character(cat0$names0), split = NumberOfSpecies),
                                    function(x) x[2]))
  cat0$label<-paste0(cat0$catname, " (n=",  cat0$numberofspp,")")
  
  # cat0$catname<-(names(temp00[[2]]))[merge(x = merge(names(temp00[[2]])]
  # cat0$names0<-NULL
  a<-temp.orig
  a$Year<-rownames(a)
  a<-a[,!grepl(pattern = NumberOfSpecies, x = names(a))]
  a00<-gather(data = a, spp, val, names(a)[1]:names(a)[length(names(a))-1], factor_key = T)
  a00<-a00[grep(pattern = "V", x = substr(x = a00$spp, start = 1, stop = 1)),]
  a00$na<-0
  a00$na[(is.na(a00$val)) | a00$val %in% 0]<-1
  a00$no<-gsub(pattern = "[a-zA-Z]", replacement = "", x = as.character(lapply(X = strsplit(x = as.character(a00$spp), split = "_"), 
                               function(x) x[1])) ) #catagory number
  a00$x<-1
  
  aa<-a00

  a00<-merge(x = aa, y = cat0, by = "no")
  
  #SUM
  a<-aggregate(x = a00[,c("na", "x")], 
               by = list("Year" = a00$Year, "Category" = a00$catname), 
               FUN = sum, na.rm = T)
  
  a<-rbind.data.frame(a, 
                      data.frame(Year = aggregate(x = a00$na, by = list("Year" = a00$Year), FUN = sum, na.rm = T)[,1], 
                                 Category = "Total",
                                 na = aggregate(x = a00$na, by = list("Year" = a00$Year), FUN = sum, na.rm = T)[,2], 
                                 x = nrow(temp.orig)*ncol(temp.orig)))
  
  a$x.perc<-a$na/a$x*100
  a$bins<-round_any(a$x.perc, 10)
  
  xnames<-as.numeric(paste0(a$Year))
  xnames[!(xnames %in% seq(from = min((as.numeric(xnames))),
                           to = max(as.numeric(xnames)),
                           by = 10))]<-""
  
  g<-ggplot(data = a, aes(x = factor(Year), y = na, color = Category)) +
    geom_line(aes(group = Category)) +
    geom_point() +
    theme(
      panel.grid.major.y = element_line( color=NOAALightBlue, size = .1 ),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line = element_line( color=NOAALightBlue, size = .1 ),
      axis.ticks = element_blank(), # remove ticks
      panel.background = element_blank()
    )  +
    scale_x_discrete(labels= xnames) +
    guides(fill=FALSE) + 
    ggtitle(paste0(place, " ", title00))
  
  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  #################Percent Missing V
  # title00<- "_PctMissingV_Line"
  # 
  # g<-ggplot(data = a, aes(x = factor(Year), y = x.perc, color = Category)) +
  #   geom_line(aes(group = Category)) +
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
  #   guides(fill=FALSE) +
  #   ggtitle(paste0(place, " ", title00))
  # 
  # figures.list[[length(figures.list)+1]]<-g
  # names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  
  # How many V columns have X percentage data missing 
  title00<- "_VPctMissing-Bar"
  
  a00<-data.frame(table(a[,names(a) %in% c("bins", "Category")]))

  cat00<-merge(y = cat0[,c("catname", "numberofspp")],
                x = a,
                by.y = "catname", by.x = "Category")

  cat00$label<-paste0(cat00$Category, " (n=",  cat00$numberofspp,")")

  cat000<-data.frame(table(cat00[,names(cat00) %in% c("label", "bins")]))

  xnames<-paste0(sort(as.numeric(paste(unique(a$bins)))), "%")

  g<-ggplot(data = cat000, aes(x = factor(bins), y = Freq, fill = label)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme(
      panel.grid.major.y = element_line( color=NOAALightBlue, size = .1 ),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line = element_line( color=NOAALightBlue, size = .1 ),
      axis.ticks = element_blank(), # remove ticks
      panel.background = element_blank()
    )  +
    scale_x_discrete(labels = xnames) +
    ggtitle(paste0(place, " ", title00))

  figures.list[[length(figures.list)+1]]<-g
  names(figures.list)[length(figures.list)]<-paste0(title0, title00)
  
  return(list(temp, warnings.list, figures.list))
}


#### The nameing conventions of the column names. 
ImplicitQuantityInput<-function(temp, baseyr){
  
  ####HOUSEKEEPING
  NameBaseTotal<-substr(x = names(temp)[grep(x = names(temp), pattern = "0Total")][1], 
                        start = 3, stop = nchar(names(temp)[grep(x = names(temp), 
                                                                 pattern = "0Total")][1]))
  
  # Find which columns in this table are quantity Columns - we will need this for later
  QColumns<-grep(pattern = paste0("Q[0-9]+"), 
                 x = names(temp))
  
  # Find which columns in this table are price Columns - we will need this for later
  PColumns<-grep(pattern = paste0("P[0-9]+"), 
                 x = names(temp))
  
  # Find which columns in this table are value Columns - we will need this for later
  VColumns<-rep_len(x = "", length.out = length(QColumns))
  for (j in 1:length(VColumns)){
    VColumns[j]<-paste0("V", substr(x = names(temp)[QColumns[j]], 
                                    start = 2, 
                                    stop = nchar(names(temp)[QColumns[j]])))
  }
  
  ###Value for each category ($V_{i,t}$; e.g., K, L, ...)
  # We first calculate the value for each input category. 
  
  # Price for a species (s) of category (i) in year (t) = 
  #   $$V_{i,t} = VP_{i,t}*/*Q_{i,t}$$
  
  # where: 
  # - $P_{i,t}$ is the price per category (i), for each year (t)
  # - $Q_{i,t}$ is the quantity per category (i), for each year (t)
  # - $V_{i,t}$ is the value ($) per category (i), for each year (t)
  
  # Here we calculate the value for each category
  tempV<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(PColumns)) {
    
    NameBase<-substr(start = 2, 
                     stop = nchar(names(temp)[PColumns[c]]), 
                     x = names(temp)[PColumns[c]]) 
    
    Q0<-temp[,names(temp) %in% paste0("Q", NameBase)]
    P0<-temp[,names(temp) %in% paste0("P", NameBase)] #to make sure its the same column
    tempV[,c]<-P0*Q0
    names(tempV)[c]<-paste0("V", NameBase ) #name the column
  }
  
  tempV<-data.frame(tempV)
  temp<-cbind.data.frame(temp, tempV)
  
  ###Sum value of the the fishery
  # $$V_t = \sum_{i=1}^{m} V_{i,t}$$
  
  # where: 
  # - $V_{t}$ is the value of the fishery by year
  
  temp[,ncol(temp)+1]<-rowSums(tempV, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("V", NameBaseTotal) #name the column
  
  ##Weight of each category in the fishery ($W_{i,t}$; e.g., K, L, ...)
  # $$W_{i,t} = (P_{i,t}*Q_{i,t})/V_{t}$$
  
  # where: 
  # - $W_{i,t}$ is the weight of the category (i) for each year (t) in the fishery
  
  tempW<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(VColumns)) {
    
    #for renaming the columns
    NameBase<-substr(start = 2, 
                     stop = nchar(names(temp)[QColumns[c]]), 
                     x = names(temp)[QColumns[c]]) 
    
    V<-temp[,grep(pattern = paste0("V", NameBaseTotal), x = names(temp))]
    V0<-temp[,names(temp) %in% paste0("V", NameBase)] #to make sure its the same column
    tempW[,c]<-V0/V
    names(tempW)[c]<-paste0("W", NameBase ) #name the column
  }
  
  tempW<-data.frame(tempW)
  temp<-cbind.data.frame(temp, tempW)
  
  ###Price Changes for each species ($PC_{s,i,t}$ aka $\Delta ln(P_{s,i,t})$; e.g., Salmon and Flounder)
  # $$PC_{i,t} = ln(\frac{P_{i,t}}{P_{i,t-1}}) = \sum_{s=1}^n([\frac{W_{i,t} + W_{i,t-1}}{2}] * [ln(\frac{P_{i,t}}{P_{i,t-1}}]) = \sum_{s=1}^n([\frac{W_{i,t} + W_{i,t-1}}{2}] * [ln(P_{i,t}) - ln(P_{i,t-1})]) $$
  
  # Where:
  # - category's (i) Price for each category (i), for each year (t) = $P_{i,t}$
  # - category's (i) Weight for each category (i), for each year (t) = $W_{i,t}$
  
  #Find which columns in this table are price and revenue share columns
  tempPC<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(PColumns)){
    #For nameing columns
    NameBase<-substr(start = 2, 
                     stop = nchar(names(temp)[QColumns[c]]), 
                     x = names(temp)[QColumns[c]]) 
    
    # Calculate
    P0<-temp[, names(temp) %in% paste0("P", NameBase)]
    R0<-temp[, names(temp) %in% paste0("W", NameBase)] #to make sure its the same column
    tempPC[,c]<-PriceChange(R0, P0)
    names(tempPC)[c]<-paste0("PC", NameBase ) #name the column
  }
  
  temp<-cbind.data.frame(temp, tempPC)
  
  ###Summed Price Change for the fishery ($PC_{t}$)
  # $$PC_{t} = \sum_{i=1}^{m} PC_{i,t}$$
  
  # Where:
  # - category's (i) Price Change of the fishery, for each year (t) = $PC_{t}$
  
  temp[ncol(temp)+1]<-rowSums(tempPC, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("PC", NameBaseTotal)
  
  ###Price for the entire commercial fishery ($P_t$)
  # $$P_t = P_{t-1}*[1+ln(\frac{P_{i,t}}{P_{i,t-1}})] = P_{t-1}*[1-PC_{t}]$$
  
  #Note that the first row of this column is = 1
  tempP<-c(1, rep_len(x = NA, length.out = nrow(temp)-1))
  
  PC0<-temp[,paste0("PC", NameBaseTotal)] #this is equal to ln(P_it/P_it-1)
  
  #Since the first row is defined, we need to start at the second row
  for (t in 2:length(tempP)){
    tempP[t]<-tempP[t-1]*(1+PC0[t])
  }
  
  temp[,ncol(temp)+1]<-tempP
  names(temp)[ncol(temp)]<-paste0("P", NameBaseTotal)
  
  ###Price Index for the entire commercial fishery ($PI_t$)
  # $$PI_{t} = \frac{P_{t}}{P_{t=baseyr}}$$
  
  # In this example, we'll decide that the base year is 2010, for whatever reason. Notice that the $PI_{i,t=2010} = 1$ 
  
  temp[,ncol(temp)+1]<-temp[,paste0("P", NameBaseTotal)]/temp[rownames(temp) %in% baseyr,paste0("P", NameBaseTotal)]
  names(temp)[ncol(temp)]<-paste0("PI", NameBaseTotal)
  
  ### Total Implicit Quantity Input for the entire commercial fishery ($Q_t = Y_t$)
  # To get quantity estimates for total output using total value of landings divided by price index as follow: $Y=Q=V/I$ 
  # Note here that all columns of $V$ are being used, despite having been removed earlier in the analysis when $P$ could not be calculated and $P$ columns have functionally been removed from the analysis. 
  # $$Q_{t}=V_{t}/PI_{t}$$
  
  temp[,ncol(temp)+1]<-temp[,paste0("V", NameBaseTotal)]/temp[,paste0("PI", NameBaseTotal)]
  names(temp)[ncol(temp)]<-paste0("Q", NameBaseTotal)
  
  ### Total Implicit Quantity/Output Index
  # $$QI_t = Q_t/Q_{t=baseyr}$$
  
  # Where:
  # - $QI$ is the sum of Q after these equations
  
  temp[,ncol(temp)+1]<-temp[,paste0("Q", NameBaseTotal)]/temp[rownames(temp) %in% baseyr, paste0("Q", NameBaseTotal)]
  names(temp)[ncol(temp)]<-paste0("QI", NameBaseTotal)
  
  ### Sum Total Implicit Quantity/Output Index (Optional)
  # $$QEI_t = QE_t/QE_{t=baseyr}$$
  
  # Where:
  # - $QE$ is the sum of Q before these equations
  # - $QEI$ is the index of the sum of Q before these equations
  
  temp[,ncol(temp)+1]<-temp[,paste0("QE", NameBaseTotal)]/temp[rownames(temp) %in% baseyr, paste0("QE", NameBaseTotal)]
  names(temp)[ncol(temp)]<-paste0("QEI", NameBaseTotal)
  ### Solve Output portion of the equation for the Input Changes: 
  # $$QC_t = \sum_{i=1}^n((\frac{R_{it} + R_{it-1}}{2}) * ln(\frac{Q_{it}}{Q_{it-1}}))$$
  
  #Find which columns in this table are price and revenue share columns
  tempQC<-data.frame(data = rep_len(x = NA, length.out = nrow(temp)))
  for (c in 1:length(PColumns)){
    #For nameing columns
    NameBase<-substr(start = 2, 
                     stop = nchar(names(temp)[QColumns[c]]), 
                     x = names(temp)[QColumns[c]]) 
    
    # Calculate
    P0<-temp[, names(temp) %in% paste0("Q", NameBase)]
    R0<-temp[, names(temp) %in% paste0("W", NameBase)] #to make sure its the same column
    tempQC[,c]<-PriceChange(R0, P0)
    names(tempQC)[c]<-paste0("QC", NameBase ) #name the column
  }
  
  temp<-cbind.data.frame(temp, tempQC)
  
  ###Summed Price Change for the fishery ($PC_{t}$)
  # $$QC_{t} = \sum_{i=1}^{m} QC_{i,t}$$
  
  # Where:
  # - category's (i) Price Change of the fishery, for each year (t) = $PC_{t}$
  
  temp[ncol(temp)+1]<-rowSums(tempQC, na.rm = T)
  names(temp)[ncol(temp)]<-paste0("QC", NameBaseTotal)
  
  #Remove duplicate columns
  temp<-temp[, !(grepl(pattern = "\\.[0-9]+", x = names(temp)))]
  temp <- temp[, !duplicated(colnames(temp))]
  
  return(temp)
  
}

TFP_ChangeRate_Method1<-function(temp.output, temp.input, baseyr, pctmiss){
  
  NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
                                                strsplit(x = names(temp)[1], 
                                                         split = "_")[[1]][2], 
                                              split = "[a-zA-Z]")[[1]][1]))[1]

  #OUTPUT
  temp00<-ImplicitQuantityOutput(temp.output, baseyr, pctmiss)
  temp<-temp00[[1]]
  warnings.list0<-temp00[[2]]
  figures.list0<-temp00[[3]]
  
  names(temp)<-paste0(gsub(pattern = "Q", replacement = "Y", 
                           x = substr(x = names(temp), start = 1, stop = 1)), 
                      substr(x = names(temp), start = 2, stop = nchar(names(temp))))
  temp.output1<-temp
  
  
  #input
  temp<-ImplicitQuantityInput(temp.input, baseyr)
  names(temp)<-paste0(gsub(pattern = "Q", replacement = "X", 
                           x = substr(x = names(temp), start = 1, stop = 1)), 
                      substr(x = names(temp), start = 2, stop = nchar(names(temp))))
  temp.input1<-temp

  #Calculate
  TFP1<-temp.output1[,grep(pattern = paste0("Y",NumberOfSpecies,".*Total"), x = names(temp.output1))] / 
    temp.input1[,grep(pattern = paste0("X",NumberOfSpecies,".*Total"), x = names(temp.input1))] 
  
  
  TFP1_CR<-rep_len(x = 0, length.out = length(TFP1))
  for (i in 2:length(TFP1)) {
    TFP1_CR[i] <- ln(TFP1[i]/TFP1[i-1])
  }
  
  temp<-cbind.data.frame(TFP1, TFP1_CR, 
                         temp.output1,#[,grep(pattern = paste0("Y",NumberOfSpecies,".*Total"), x = names(temp.output1))], 
                           temp.input1)#[,grep(pattern = paste0("X",NumberOfSpecies,".*Total"), x = names(temp.input1))] )
  
  return(temp)
}


TFP_ChangeRate_Method2<-function(temp.output, temp.input, baseyr, pctmiss){
  
  #OUTPUT
  temp00<-ImplicitQuantityOutput(temp.output, baseyr, pctmiss)
  temp<-temp00[[1]]
  warnings.list0<-temp00[[2]]
  figures.list0<-temp00[[3]]
  
  names(temp)<-paste0(gsub(pattern = "Q", replacement = "Y", 
                           x = substr(x = names(temp), start = 1, stop = 1)), 
                      substr(x = names(temp), start = 2, stop = nchar(names(temp))))
  temp.output1<-temp
  
  
  #input
  temp<-ImplicitQuantityInput(temp.input, baseyr)
  names(temp)<-paste0(gsub(pattern = "Q", replacement = "X", 
                           x = substr(x = names(temp), start = 1, stop = 1)), 
                      substr(x = names(temp), start = 2, stop = nchar(names(temp))))
  temp.input1<-temp
  
  TFP <- temp.output1[,grep(pattern = "YC.*Total", x = names(temp.output1))] - 
    temp.input1[,grep(pattern = "XC.*Total", x = names(temp.input1))]
  
  TFP2_CR<-data.frame(TFP)
  names(TFP2_CR)<-"TFP2_CR"
  rownames(TFP2_CR)<-rownames(temp.output1)
  
  TFP2_CR<-cbind.data.frame(TFP2_CR, 
                            temp.output1,#[,grep(pattern = "YC.*Total", x = names(temp.output1))], 
                            temp.input1)#[,grep(pattern = "XC.*Total", x = names(temp.input1))])
  
  return(TFP2_CR)
}


itis_reclassify<-function(tsn, categories, missing.name){
  temp<-classification(tsn, db = 'itis')
  TSN<-NA
  category0<-NA
  for (i in 1:length(temp)){
    # print(i)
    TSN[i]<-tsn[i]
    if (is.na(temp[[i]])[1]) {
      category0[i]<-"Other"
    } else if (sum(temp[[i]]$name %in% names(categories) & #is the name of the group right?
                   temp[[i]]$rank %in% as.character(categories))==0 #is the taxonomic level right? Just in case
    ) {
      category0[i]<-missing.name
    } else {
      category0[i]<-temp[[i]]$name[temp[[i]]$name %in% names(categories) & #is the name of the group right?
                                     temp[[i]]$rank %in% as.character(categories)] #is the taxonomic level right? Just in case
    }
  }
  tempcategories<-data.frame(TSN = TSN, 
                             category.tax = category0)
  return(tempcategories)
}


# This funciton standardizes the length of the category or species numbers e.g.,(numbers of 33, 440, and 1 are converted to 033, 440, and 001)
numbers0<-function(x) {
  xx<-rep_len(x = NA, length.out = length(x))
  for (i in 1:length(x)){
    xx[i]<-paste0(paste(rep_len(x = 0, 
                                length.out = nchar(max(x))-nchar(x[i])), 
                        collapse = ""), 
                  as.character(x[i]))
  }
  return(xx)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

funct_counter<-function(counter0) {
  counter0<-as.numeric(counter0)+1
  counter<-formatC(x = counter0, width = 3)
  counter<-gsub(counter, pattern = " ", replacement = "0")
  return(counter)
}


xunits<-function(temp00, combine=T) {
  
  temp00<-sum(as.numeric(temp00))
  sigfig<-format(temp00, digits = 3, scientific = TRUE)
  sigfig0<-as.numeric(substr(x = sigfig, start = (nchar(sigfig)-1), stop = nchar(sigfig)))
  
  if (sigfig0<=5) {
    # if (sigfig0<4) {
    unit<-""
    x<-format(x = temp00, big.mark = ",", digits = 0, scientific = F)
    # } else if (sigfig0>=4 & sigfig0<6) {
    #   unit<-" thousand"
    # x<-round(temp00/1e3, digits = 1)
    # } else if (sigfig0==5) {
    #   unit<-" thousand"
    #   x<-round(temp00/1e3, digits = 0)
  } else if (sigfig0>=6 & sigfig0<9) {
    unit<-" million"
    x<-round(temp00/1e6, digits = 1)
  } else if (sigfig0>=9 & sigfig0<12) {
    unit<-" billion"
    x<-round(temp00/1e9, digits = 1)
  } else if (sigfig0>=12) {
    unit<-" trillion"
    x<-round(temp00/1e12, digits = 1)
  }
  
  out<-ifelse(combine==T, paste0(x, unit), list(x, unit))
  
  return(out)
}



plotnlines<-function(dat, title00, place){
  
  xnames<-as.numeric(paste0(dat$Year))
  xnames[!(xnames %in% seq(from = min((as.numeric(xnames))),
                           to = max(as.numeric(xnames)),
                           by = 10))]<-""
  
  dat$val<-as.numeric(as.character(dat$val))
  dat$val[(is.infinite(dat$val))]<-NA
  divideby<-paste0("(", strsplit(x = xunits(mean(dat$val, na.rm = T)), split = " ")[[1]][2], "s)")
  if (divideby %in% "(trillions)") {
    divideby0<-1e12
  } else if (divideby %in% "(billions)") {
    divideby0<-1e9
  } else if (divideby %in% "(millions)") {
    divideby0<-1e6
  } else if (divideby %in% "(thousands)") {
    divideby0<-1e3
  } else if (divideby %in% "(NAs)") {
    divideby0<-1
    divideby<-""
  }
  
  dat$val<-dat$val/divideby0
  # ynames<-as.numeric(paste0(val))
  
  g<-ggplot(data = dat, aes(x = factor(Year), y = val, color = cat)) +
    geom_line(aes(group = cat)) +
    geom_point() +
    theme(
      # legend.position = c(0.9, 0.2), 
      panel.grid.major.y = element_line( color=NOAALightBlue, size = .1 ),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line = element_line( color=NOAALightBlue, size = .1 ),
      axis.ticks = element_blank(), # remove ticks
      panel.background = element_blank()
    )  + 
    ylab(paste0(gsub(pattern = "_", replacement = "", 
                    x = strsplit(x = title00, split = "-")[[1]][1]), 
               " ", divideby)) +
    scale_x_discrete(labels= xnames) +
    # scale_y_discrete(labels= ynames) +
    guides(fill=FALSE) + 
    ggtitle(paste0(place))
  
  return(g)
}

#########***########
##########LOAD DATA##############

##########*** State Codes##############
state.codes <- read.csv(paste0(dir.data, '/statecodes.csv'), stringsAsFactors = FALSE)
write.csv(x = state.codes, file = paste0(dir.rawdata, "/statecodes.csv"))

state.codes<-state.codes[state.codes$STATE %in% c(1,2,5,7,8,10,11,13,14,21,22,23,24,27,32,33,35,36,40,42,43,46,49,50,98),]#Only,keep,the,data,for,the,states,that,we,are,making,state,tables,for
state.codes$NAME[state.codes$NAME %in% "Florida East Coast"]<-"East Florida"
state.codes$NAME[state.codes$NAME %in% "Florida West Coast"]<-"West Florida"
state.codes$Region<-c("Gulf of Mexico","North Pacific","Pacific","New England","Mid-Atlantic",
                      "South Atlantic","Gulf of Mexico","South Atlantic","Western Pacific (Hawai`i)", 
                      "Gulf of Mexico","New England","Mid-Atlantic",
                      "New England","Gulf of Mexico","New England","Mid-Atlantic","Mid-Atlantic",
                      "South Atlantic","Pacific","New England","South Atlantic","Gulf of Mexico",
                      "Mid-Atlantic","Pacific","Pacific")
state.codes$Region.no<-c(5,7,6,1,2,
                         4,5,4,8, 
                         5,1,2,
                         1,5,1,2,2,
                         4,6,1,4,5,
                         2,6,6)

state.codes$NAME[state.codes$NAME %in% "Hawaii"]<-"Hawai`i"
state.codes<-state.codes[!(state.codes$STATE %in% 98),]



