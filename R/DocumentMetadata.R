
library(officer)


#' @title Document Metadata
#' 
#' @source 
#' @description A function to document your metadata for people who hate code things but insist they want to know what you are doing
#' @param dir.out The directory that you want this file to be saved to. 
#' @param title Name of the project, to appear at the top of the page. 
#' @author Emily Markowitz Emily.Markowitz@noaa.gov
#' @keywords packages documentation
#' @export
#' @examples 
#' DocumentMetadata(dir.out = getwd(), title = "Test")
#' 
DocumentMetadata<-function(dir.out, title) {
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
}
  
  