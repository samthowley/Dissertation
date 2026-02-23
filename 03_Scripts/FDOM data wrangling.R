#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(weathermetrics)
library(measurements)
library(cowplot)


eem <- read_csv("01_Raw_data/Aqualog_processing/projects/Howley_Bradford/output/indices_eem_05012025.csv")%>%
  rename(sample= "site_date")%>%
  select(sample, bix, hix, fi)%>%
  separate(sample, into = c("Site", "Date","Rep"), sep = "_")%>%
  mutate(Date=mdy(Date))%>%
  filter(!is.na(Site))


carbon<-eem %>% mutate(Site= if_else(Site=='5,5','5.5', Site))%>%
  mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                      Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                      Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',

                      Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                      Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                      Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                      Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                      Site=='9GW4'~'9',Site=='9GW5'~'9',Site=='5GW8'~'5',

                      Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
                      Site=='5.4'~'5',Site=='5.5'~'5',Site=='6.1'~'6',Site=='6.2'~'6',
                      Site=='6.3'~'6',Site=='6.4'~'6',Site=='6.5'~'6',Site=='6.6'~'6',
                      Site=='9.1'~'9',Site=='9.2'~'9',Site=='9.3'~'9',Site=='9.4'~'9',
                      Site=='9.5'~'9',Site=='9.6'~'9',Site=='9.Sam'~'9'))


carbon<-carbon %>% mutate(chapter=case_when(Site=='3'~'long',Site=='5'~'long',Site=='5a'~'stream',
                                            Site=='6'~'long',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'long',Site=='13'~'stream',Site=='15'~'stream',

                                            Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                            Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='5GW8'~'RC',

                                            Site=='6GW1'~'RC',
                                            Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                            Site=='6GW6'~'RC',

                                            Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                            Site=='9GW4'~'RC',Site=='9GW5'~'RC',

                                            Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                            Site=='5.4'~'long',Site=='5.5'~'long',Site=='5.6'~'long',

                                            Site=='6.1'~'long',Site=='6.2'~'long',
                                            Site=='6.3'~'long',Site=='3.1'~'long',Site=='3.2'~'long',Site=='3.3'~'long',
                                            Site=='3.4'~'long',

                                            Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                            Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

carbon$chapter[is.na(carbon$chapter)]<-'wetland'

forstream <- carbon %>%
  mutate(Site = if_else(Site == "6.2", "6", Site),
         Site = if_else(Site == "5.5", "5", Site),
         Site = if_else(Site == "9.5", "9", Site),
         Site = if_else(Site == "3.1", "3", Site))
stream<-forstream %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                               Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                               Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream'))%>%
  filter(chapter=='stream')
write_csv(stream, "04_Output/eem_stream.csv")



