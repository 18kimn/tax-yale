library(rvest)
library(dplyr)
library(tidyr)
library(tidyverse)


df <- data.frame(
  names = rep(NA, 99),
  address = rep(NA, 99)
)
letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "V", "W", "Y")
corpus <- c()

for (i in letters) {
  web <- paste0("https://www.yale.edu/directories?title=", i)
  corpus0 <- web %>% read_html() %>% html_nodes("p") %>% html_text()
  corpus1 <- corpus0[-(1:2)]
  corpus <- c(corpus, corpus1)
}

names <- corpus %>% 
  as_tibble() %>% 
  dplyr::filter(!grepl("\n", value)) %>% 
  as.data.frame()
addresses <- corpus %>% 
  as_tibble() %>% 
  dplyr::filter(grepl(", CT", value)) %>% 
  as.data.frame()
for (i in 1:nrow(addresses)) {
  df[i,1] <- names[i,1]
  df[i,2] <- addresses[i,1]
}

df$address <- str_replace_all(df$address, "\n", "")
df$address <- str_replace_all(df$address, "   ", "")
df$address <- str_replace_all(df$address, "  ", "")

df <- df %>% 
  dplyr::filter(grepl("New Haven", address))

write.csv(df, "property_directory.csv", row.names=FALSE)