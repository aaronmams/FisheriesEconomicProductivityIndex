##########INTRODUCTION####################
# replacing "me downloads.R", "me cbp data.R", "me econ.R", and "me nonemp data.R" which just compartimentalized the downloading and parsing process. 
# NOTE: Toledo is a warning word. If you see it in the code, it means that something was wrong there
# NOTE: for nonemp data, they started to change the way they saved files from txt (until 2015) to zip (2016 and beyond). You will need to change that in the for loop of which files are run by which funcitons. 

######DIRECTORIES#########
dir<-getwd()
setwd(dir)
# setwd("U:/ST5/FEUS 2017/marecon")
# setwd(paste0(getwd(), "/data"))
dir.create(paste0(getwd(), "/data/"))

######KNOWNS#########
minyr<-2006
maxyr<-2017-1 #report year-1

#####LIBRARY FUNCTIONS#####
library(reshape2)

#####*##########
#####ME CENSUS DATA AND CLEAN########

download_unzip<-function(yr, urlsample, yrsample, folder, fnct, st.us) {
  print(yr)
  
  url0<-gsub(pattern = yrsample, 
             x = urlsample, 
             replacement = yr)
  url0<-gsub(pattern = substr(x = as.character(yrsample), start = 3, stop = 4), 
             x = url0, 
             replacement = substr(x = as.character(yr), start = 3, stop = 4))
  
  filetype<-substr(x = url0, start = nchar(url0)-3, stop = nchar(url0)) # e.g., txt or zip, if the former, no reason to unzip (hense "if(zip))
  download.file(url0, 
                destfile=paste0("./data/", folder, "/", 
                                folder, substr(x = as.character(yr), start = 3, stop = 4), st.us, filetype), 
                mode="wb")
  if (filetype==".zip") {
    unzip(paste0("./data/", folder, "/", folder, substr(x = as.character(yr), start = 3, stop = 4), st.us, filetype), 
          exdir=paste0("./data/", folder))
  }
  
  
  if (folder != "lq") {
    #   dat<-read.table(paste0("./data/", folder, "/", folder, substr(x = yr, start = 3, stop = 4), st.us, ".txt"),
    #                    sep = ",",quote="",header=TRUE, as.is=TRUE, stringsAsFactors=FALSE)
    # } else {
    dat<-fnct(file = paste0("./data/", folder, "/", folder, substr(x = yr, start = 3, stop = 4), st.us, ".txt"))
    
    dat$year <- yr
    
    assign(paste0(folder, substr(x = i, start = 3, stop = 4), st.us), dat)
    
    save(dat, #list = get(paste0(folder, substr(x = i, start = 3, stop = 4), st.us)), 
         file = paste0("./data/", folder, "/",folder, substr(x = yr, start = 3, stop = 4), 
                       st.us,".rdata"))
    
    write.csv(x = dat, #list = get(paste0(folder, substr(x = i, start = 3, stop = 4), st.us)), 
              paste0("./data/", folder, "/",folder, substr(x = yr, start = 3, stop = 4), 
                     st.us,".csv"))
    
    return(get(paste0(folder, substr(x = yr, start = 3, stop = 4), st.us)))
  }  
}


###########*County Buisness Patterns##################

### ******Info#####
##READ IN and PROCESS CBP DATA for employer firms

#NAICS code
#"3117//" SEAFOOD PRODUCT PREP & PACKAGING
#"42446/" SEAFOOD SALES, WHOLESALE
#"44522/" SEAFOOD SALES, RETAIL

#483113 Coastal and Great Lakes Freight Transportation
#483111 Deep Sea Freight Transportation
#483112 Deep Sea Passenger Transportation
#71393/ Marinas
#48832/ Margine Cargo Handling
#48833/ Navigational Services to Shipping
#48831/ Port and Harbor Operations
#3366// Ship and Boat Building

#Abbreviations and Symbols
#D  Withheld to avoid disclosing data for individual businesses; data are included in broader industry totals.
#S  Withheld to avoid releasing data that do not meet publication standards; data are included in broader industry totals.

# Marine industries to get data for
marinenaics <- list("------",
                    "1141//", # Fishing
                    "114111", # Finfish Fishing
                    "114112", # Shellfish Fishing
                    "114119") # Other Marine Fishing

### US CBP - #files are in different formats depending on the year

folder<-"cbp"
dir.create(paste0(getwd(), paste0("/data/", folder, "/")))

### ******State#####

## STATE CBP - #The format of thes files changes from year to year.  There are 2 idnetifiable formats from 2007-2015.  
######FUNCTION to read in and format data for STATES, pre-LFO format (2007-2009)
read.cbp_STpreLFO <- function(file) {
  st_pre <- data.frame(read.table(file, sep = ",",header=TRUE, as.is=TRUE,na.strings=".",strip.white=TRUE)) #read in the raw data files
  names(st_pre)<-tolower(names(st_pre))                                              #this is necessary b/c the 2006 file is all caps
  st_pre <- data.frame(cbind(st_pre$fipstate, st_pre$naics, st_pre$est, st_pre$emp,st_pre$empflag, st_pre$ap))
  names(st_pre) <- c("fipstate", "naics","est","emp","empflag","ap")
  st_pre$fipstate<-as.character(st_pre$fipstate)      #R formatting bullshit
  st_pre$naics<-as.character(st_pre$naics)        
  st_pre$est<-as.numeric(as.character(st_pre$est))
  st_pre$emp<-as.numeric(as.character(st_pre$emp))
  st_pre$empflag<-as.character(st_pre$empflag)
  st_pre$ap<-as.numeric(as.character(st_pre$ap)  )
  st_pre<-st_pre[st_pre$naics %in% marinenaics,]      #keep only the NAICS we want
  
  return(st_pre)
}

## STATE CBP - #The format of thes files changes from year to year.  There are 2 idnetifiable formats from 2007-2015.  
######FUNCTION to read in and format data from STATE file that includes LFO field (2010-...), All Establishments = '-'
read.cbp_st <- function(file) {
  cbp_st <- data.frame(read.table(file, sep = ",",header=TRUE, as.is=TRUE,na.strings=".",strip.white=TRUE))  #read in the raw data files
  names(cbp_st) <- tolower(names(cbp_st))
  cbp_st<-cbp_st[cbp_st$lfo=="-",] #keep only data for "All Establishments"
  cbp_st <- data.frame(cbind(cbp_st$fipstate, cbp_st$naics, cbp_st$est, cbp_st$emp,cbp_st$empflag, cbp_st$ap))
  names(cbp_st) <- c("fipstate", "naics","est","emp","empflag","ap") #establishments= est, employees= emp,annual payroll= ap    
  cbp_st$fipstate<-as.character(cbp_st$fipstate)      #R formatting bullshit
  cbp_st$naics<-as.character(cbp_st$naics)          
  cbp_st$est<-as.numeric(as.character(cbp_st$est))
  cbp_st$emp<-as.numeric(as.character(cbp_st$emp))
  cbp_st$empflag<-as.character(cbp_st$empflag)
  cbp_st$ap<-as.numeric(as.character(cbp_st$ap)  )
  cbp_st<-cbp_st[cbp_st$naics %in% marinenaics,]  #keep only the NAICS we want
  
  return(cbp_st)
}

urlsample = "https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16st.zip"
yrsample = "2016"
st.us<-substr(x = urlsample, start = nchar(urlsample)-5, stop = nchar(urlsample)-4) #is this state (st) or US (us) data?
for (i in (minyr:maxyr)){
  fnct = ifelse(test = i>2009, yes = read.cbp_st, no = read.cbp_STpreLFO)
  assign(paste0(folder, substr(x = i, start = 3, stop = 4), st.us),
         download_unzip(yr = i, 
                        urlsample = urlsample, 
                        yrsample = yrsample, 
                        folder=folder, 
                        fnct = fnct, 
                        st.us = st.us))
}


### ******US#####
read.cbp_us <- function(file) {
  cbp_us <- data.frame(read.table(file, sep = ",",header=TRUE, 
                                  as.is=TRUE,na.strings=".",strip.white=TRUE))#read in the raw data files
  names(cbp_us)<-tolower(names(cbp_us))                                              #this is necessary b/c the 2015 file is all caps
  cbp_us<-cbp_us[cbp_us$lfo=="-",]                                                  #keep only data for "All Establishments"
  cbp_us <- data.frame(cbind(cbp_us$naics, cbp_us$est, cbp_us$emp, cbp_us$ap))      #keep only the columns we need
  names(cbp_us) <- c("naics","est","emp","ap")       #establishments= est, employees= emp,annual payroll= ap    
  cbp_us$naics<-as.character(cbp_us$naics)           #R formatting bullshit
  cbp_us$est<-as.numeric(as.character(cbp_us$est))
  cbp_us$emp<-as.numeric(as.character(cbp_us$emp))
  cbp_us$ap<-as.numeric(as.character(cbp_us$ap)  )
  cbp_us<-cbp_us[cbp_us$naics %in% marinenaics,]     #keep only the NAICS we want
  
  return(cbp_us)
}

urlsample = "https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16us.zip"
yrsample = "2016"
fnct = read.cbp_us
st.us<-substr(x = urlsample, start = nchar(urlsample)-5, stop = nchar(urlsample)-4) #is this state (st) or US (us) data?
for (i in (ifelse(minyr>2008, minyr, 2008):maxyr)){ #earlier than 2008 doesn't work
  urlsample<-ifelse(test = i>=2008, yes = urlsample, no = gsub(pattern = ".zip", x = urlsample, replacement = ".txt"))
  assign(paste0(folder, substr(x = i, start = 3, stop = 4), st.us),
         download_unzip(yr = i, 
                        urlsample = urlsample, 
                        yrsample = yrsample, 
                        folder=folder, 
                        fnct = fnct, 
                        st.us = st.us))
}
