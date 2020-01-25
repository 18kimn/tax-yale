#scraping assessment values
library(tidyverse)
library(rvest)
library(magrittr) 

#This is not a very "nice" way to scrape the website as in it goes to every property to obtain information for every single building in New Haven. I couldn't get R to send a POST request for just Yale-owned buildings.

#Still, I guess this is a useful thing anyhow to get non-Yale data -- i.e. on UNH, quinnipiac, real estate immediately surrounding yale, etc... 
base_url <- "http://gis.vgsi.com/newhavenct/"
urls <- paste0(base_url, "Streets.aspx?Letter=", LETTERS)


#get_url(url) goes to url and gets all of the links on there 
get_url <- function(url){
  read_html(url) %>% 
    html_nodes("#list a") %>% 
    html_attrs %>% 
    unlist
}

street_urls <- paste0(base_url, unlist(map(urls, get_url))) %>% 
  str_replace_all(" ", "%20")
property_urls <- paste0(base_url, unlist(map(street_urls, get_url)))

url <- "http://gis.vgsi.com/newhavenct/Parcel.aspx?pid=23162"

scrape_property_data <- function(url){
  pg <- read_html(url) 
  
  #basic identifiers
  address <- html_text(html_nodes(pg, "#MainContent_lblLocation"))
  owner <- html_text(html_nodes(pg,"#MainContent_lblGenOwner"))
  pid <- html_text(html_nodes(pg, "#MainContent_lblPid")) 
  mblu <- html_text(html_nodes(pg, "#MainContent_lblMblu"))
  acctnum <- html_text(html_nodes(pg, "#MainContent_lblAcctNum"))
  building_count <- html_text(html_nodes(pg, "#MainContent_lblBldCount"))
  
  #more detailed info 
  yearbuilt <- html_text(html_nodes(pg, "#MainContent_ctl01_lblYearBuilt"))
  livingarea <- html_text(html_nodes(pg, "#MainContent_ctl01_lblBldArea")) 
  replacement_cost <- html_text(html_nodes(pg, "#MainContent_ctl01_lblRcn"))
  pctgood <- html_text(html_nodes(pg, "#MainContent_ctl01_lblPctGood"))
  rcmdepreciation <- html_text(html_nodes(pg, "#MainContent_ctl01_lblRcnld"))
  
  #detailed information contained in tables
  overview <- html_table(html_nodes(pg, "#MainContent_ctl01_tblBldg"))[[1]]  %>% tbl_df %>% pivot_wider(names_from = X1, values_from = X2)
  assessment_value <- html_table(html_nodes(pg, "#MainContent_grdCurrentValueAsmt"))[[1]] 
  attributes <- html_table(html_nodes(pg, "#MainContent_ctl01_grdCns"))[[1]] %>% pivot_wider(names_from= Field, values_from = Description)
  
  #the following tables don't merge nicely into our table, so they'll be saved for now and will be referenced later when we need them 
  html_table(html_nodes(pg, "#MainContent_grdSales"))[[1]] %>% saveRDS(paste0("sales/", pid, ".RDS"))
  areas <- html_table(html_nodes(pg, "#MainContent_ctl01_grdSub"))[[1]] %>% saveRDS(paste0("areas/", pid, ".RDS"))
  outbuildings <- html_table(html_nodes(pg, "#MainContent_grdOb"))[[1]] %>% saveRDS(paste0("outbuildings/", pid, ".RDS"))
  valuation_history <- html_table(html_nodes(pg, "#MainContent_grdHistoryValuesAsmt"))  %>% saveRDS(paste0("valuations/", pid, ".RDS"))
  
  #land -- wasn't contained in strict html tables 
  landsize <- html_text(html_nodes(pg, "#MainContent_lblLndAcres"))
  frontage <- html_text(html_nodes(pg, "#MainContent_lblLndFront"))
  depth <- html_text(html_nodes(pg, "#MainContent_lblDepth"))
  landvalue <- html_text(html_nodes(pg, "#MainContent_lblLndAsmt"))
  
  usecode <- html_text(html_nodes(pg, "#MainContent_lblUseCode"))
  landdesc <- html_text(html_nodes(pg, "#MainContent_lblUseCodeDescription"))
  zone <- html_text(html_nodes(pg, "#MainContent_lblZone"))
  neighborhood <- html_text(html_nodes(pg, "#MainContent_lblNbhd"))
  category <- html_text(html_nodes(pg, "#MainContent_rowLndCategory"))
  
  #assembling
  property_info <- data.frame(address, owner, pid, mblu, acctnum, building_count, yearbuilt, livingarea,
                              replacement_cost, pctgood, rcmdepreciation, landsize, frontage, depth, landvalue, 
                              usecode, landdesc, zone, neighborhood,category,
                              stringsAsFactors = FALSE) %>% 
    bind_cols(overview, assessment_value, attributes)
  
  #fin
  saveRDS(property_info, paste0("property_info/", pid, ".RDS")) #if the code stops halfway (which, knowing how inconsistencies work, it will) we can start over no biggie, our work was saved
  print(paste0("Scraped data for ", address, "!")) # a little message to let us know where we're at
  return(property_info)
}

pids <- list.files("property_info/") %>% str_remove_all(".RDS")
full_pids <- str_remove_all(property_urls, "http://gis.vgsi.com/newhavenct/Parcel.aspx\\?pid=")
remaining_urls <- property_urls[!unlist(map(full_pids, function(id) id %in% pids))]
map(remaining_urls, scrape_property_data)



full_df <- map(list.files("property_info/", full.names=T), function(x){
  print(x) 
  df <- readRDS(x) %>% tbl_df %>% 
    mutate_all(as.character)
  return(df)
  }
) %>% reduce(vec_rbind)

full_df <- full_df %>% mutate(City = "New Haven", State = "Connecticut", Zip = "") %>% rename(Address = address)
write_csv(full_df, "full_df.csv")
