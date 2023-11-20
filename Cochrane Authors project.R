library(tidyverse)
library(openxlsx)
library(stringr)
library(rvest)
library(pbapply)
library(ggthemes)
library(maps)


setwd("C:/Users/ahmad/Dropbox/Cochrane Review Groups Authors by Country/New")

main_file <- read.xlsx("Cochrane 1 April.xlsx")
pubmed_file <- read.xlsx("Cochrane 1 April PubMed DIO and Address.xlsx")

merging <- left_join(main_file, pubmed_file)

merging <- merging[!duplicated(merging$DOI),]

all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")

merging$country <- 
  sapply(str_extract_all(merging$Address, all_countries), 
         toString)

merging$rest_location <- 
  str_remove_all(merging$Address, all_countries)

merging$country <- sapply(merging$country,
                          function(x) paste(
                            unique(unlist(
                              str_split(x,", ")
                            )),
                            collapse = ", "
                          ))

write.xlsx(merging, "merged.xlsx")
write.xlsx(table(merging$country), "countries.xlsx")


# web crawling

URLlist <- data.frame(DOI=main_file$DOI, 
                      URL=paste0("https://www.cochranelibrary.com/cdsr/doi/",main_file$DOI,"/information"))

a2 <- for (URL in URLlist) {
  countries <- read_html(URL) %>% html_nodes(".authors") %>% 
    html_text()
}

countries <- mutate(URLlist, countries = sapply(URLlist$URL, function(url) {
  read_html(url) %>% html_nodes(".authors") %>% 
    html_text()
}))

countries <- tryCatch(mutate(URLlist, countries = sapply(URLlist$URL, function(url) {
  read_html(url) %>% html_nodes(".authors") %>% 
    html_text()
})), error = function(e) {NA})

#pbsapply
countries8000 <- slice(URLlist, 7001:8000)

countries <- mutate(countries10, countries = sapply(countries10$URL, function(url) {
  read_html(url) %>% html_nodes(".authors") %>% 
    html_text()
}))

countries <- tryCatch(mutate(countries8552, affiliations = pbsapply(countries8552$URL, function(url) {
  read_html(url) %>% html_nodes(".authors") %>% 
    html_text()
})), error = function(e) {NA})

countries <- tryCatch(mutate(URLlist, countries = sapply(URLlist$URL, function(url) {
  read_html(url) %>% html_nodes(".authors") %>% 
    html_text()
}, function(i){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
})), error = function(e) {NA})

write.xlsx(countries, "countries_main.xlsx")


cochrane <- read_html(
  "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD005620.pub2/information") %>% 
  html_nodes(".authors") %>% 
  html_text()

countries <- sapply(str_extract_all(cochrane, all_countries), toString)

# remove duplicate countries
setwd("C:/Users/ahmad/Dropbox/Cochrane Review Groups Authors by Country/")

final_file <- read.xlsx("Cochrane Review Groups Authors - Final.xlsx")

rem_dup.one <- function(x){
  paste(unique(
    trimws(
      unlist(
        strsplit(
          x,
          split="(?!')[ [:punct:]]",
          fixed=F,
          perl=T)))),
    collapse = " ")
}

final_file <- final_file %>% 
  rowwise() %>% 
  mutate(nonduplicate_countries = 
           rem_dup.one(country)
         )

final_file$nonduplicate_countries <- trimws(final_file$nonduplicate_countries)

list(final_file$nonduplicate_countries[3])

out <- strsplit(as.character(final_file$country), ", ")
out.noduplicate <- lapply(out, unique)
out.new <- plyr::ldply(out, rbind)
# convert list to columns
out.new.noduplicate <- plyr::ldply(out.noduplicate, rbind)


## count number of authors
authors_list <- strsplit(as.character(final_file$`Author(s)`), "; ")
authors_number_in_each_review <- sapply(authors_list, length)
write.csv(authors_number_in_each_review, "authors number in each review.csv")


# regions of countries
# un regions
unregions <- lapply(out.noduplicate, 
                    countrycode, 
                    origin = "country.name", 
                    destination = "un.region.name")

unsubregions <- lapply(out.noduplicate, 
                    countrycode, 
                    origin = "country.name", 
                    destination = "un.regionsub.name")

unintregions <- lapply(out.noduplicate, 
                    countrycode, 
                    origin = "country.name", 
                    destination = "un.regionintermediate.name")

wbname <- lapply(out.noduplicate, 
                       countrycode, 
                       origin = "country.name", 
                       destination = "wb")

continents <- lapply(out.noduplicate, 
                    countrycode, 
                    origin = "country.name", 
                    destination = "continent")

wbregions <- lapply(out.noduplicate, 
                     countrycode, 
                     origin = "country.name", 
                     destination = "region")


# delete duplicates
unregions <- lapply(unregions, unique)
unsubregions <- lapply(unsubregions, unique)
unintregions <- lapply(unintregions, unique)
continents <- lapply(continents, unique)
wbregions <- lapply(wbregions, unique)



# convert lists to columns
col_unregions <- plyr::ldply(unregions, rbind)
col_unsubregions <- plyr::ldply(unsubregions, rbind)
col_unintregions <- plyr::ldply(unintregions, rbind)
col_wbname <- plyr::ldply(wbname, rbind)
col_continents <- plyr::ldply(continents, rbind)
col_wbregions <- plyr::ldply(wbregions, rbind)


# write to csv
write.csv(col_unregions, "unregions.csv")
write.csv(col_unsubregions, "unsubregions.csv")
write.csv(col_unintregions, "unintregions.csv")
write.csv(col_wbname, "wbname.csv")
write.csv(col_continents, "continents.csv")
write.csv(col_wbregions, "wbregions.csv")


# paste list
pastelistun <- lapply(unsubregions, paste, collapse = ", ")
pastelistun <- plyr::ldply(pastelistun, rbind)
write.csv(pastelistun, "unsubregions together.csv")

# countries together
out.noduplicate.paste <- lapply(out.noduplicate, paste, collapse = ", ")
out.noduplicate.paste <- plyr::ldply(out.noduplicate.paste, rbind)
write.csv(out.noduplicate.paste, "countries together.csv")
write.csv(table(out.noduplicate.paste$`1`), "countriestatistics.csv")


finalcsv <- read.csv("unsubregions - Copy.csv")
write.csv(table(finalcsv$regionstogether), "regionsstatistics.csv")

#csv by column
write.csv(table(col_unsubregions$`8`), "8 - subregion.csv")
write.csv(table(out.new.noduplicate$`14`), "14 - countries.csv")

#csv by column and review group
csv1 <- read.csv("countries list without duplicates.csv")
csv2 <- read.csv("unsubregions - Copy.csv")

write.csv(table(csv1$X14, csv1$year), "14 - countries - years.csv")
write.csv(table(csv2$X8, csv2$year), "8 - subregions - years.csv")


# merge all together
# Countries
# All
mergedcountries<- bind_rows(read.csv("Descriptive/1 - countries.csv"), 
          read.csv("Descriptive/2 - countries.csv"), 
          read.csv("Descriptive/3 - countries.csv"), 
          read.csv("Descriptive/4 - countries.csv"), 
          read.csv("Descriptive/5 - countries.csv"), 
          read.csv("Descriptive/6 - countries.csv"), 
          read.csv("Descriptive/7 - countries.csv"), 
          read.csv("Descriptive/8 - countries.csv"), 
          read.csv("Descriptive/9 - countries.csv"), 
          read.csv("Descriptive/10 - countries.csv"), 
          read.csv("Descriptive/11 - countries.csv"), 
          read.csv("Descriptive/12 - countries.csv"), 
          read.csv("Descriptive/13 - countries.csv"), 
          read.csv("Descriptive/14 - countries.csv")) %>% 
  group_by(Var1) %>% 
  summarise_all(funs(sum(., na.rm = T)))

write.csv(mergedcountries, "mergedcountries.csv")

# By review group
mergedcountriesreviewgroups <- bind_rows(read.csv("Descriptive/1 - countries - review groups.csv"), 
                            read.csv("Descriptive/2 - countries - review groups.csv"), 
                            read.csv("Descriptive/3 - countries - review groups.csv"), 
                            read.csv("Descriptive/4 - countries - review groups.csv"), 
                            read.csv("Descriptive/5 - countries - review groups.csv"), 
                            read.csv("Descriptive/6 - countries - review groups.csv"), 
                            read.csv("Descriptive/7 - countries - review groups.csv"), 
                            read.csv("Descriptive/8 - countries - review groups.csv"), 
                            read.csv("Descriptive/9 - countries - review groups.csv"), 
                            read.csv("Descriptive/10 - countries - review groups.csv"), 
                            read.csv("Descriptive/11 - countries - review groups.csv"), 
                            read.csv("Descriptive/12 - countries - review groups.csv"), 
                            read.csv("Descriptive/13 - countries - review groups.csv"), 
                            read.csv("Descriptive/14 - countries - review groups.csv")) %>% 
  group_by(country) %>% 
  summarise_all(funs(sum(., na.rm = T)))

write.csv(mergedcountriesreviewgroups, "mergedcountriesreviewgroups.csv")

# By year
mergedcountriesyears <- bind_rows(read.csv("Descriptive/1 - countries - years.csv"), 
                            read.csv("Descriptive/2 - countries - years.csv"), 
                            read.csv("Descriptive/3 - countries - years.csv"), 
                            read.csv("Descriptive/4 - countries - years.csv"), 
                            read.csv("Descriptive/5 - countries - years.csv"), 
                            read.csv("Descriptive/6 - countries - years.csv"), 
                            read.csv("Descriptive/7 - countries - years.csv"), 
                            read.csv("Descriptive/8 - countries - years.csv"), 
                            read.csv("Descriptive/9 - countries - years.csv"), 
                            read.csv("Descriptive/10 - countries - years.csv"), 
                            read.csv("Descriptive/11 - countries - years.csv"), 
                            read.csv("Descriptive/12 - countries - years.csv"), 
                            read.csv("Descriptive/13 - countries - years.csv"), 
                            read.csv("Descriptive/14 - countries - years.csv")) %>% 
  group_by(country) %>% 
  summarise_all(funs(sum(., na.rm = T)))

write.csv(mergedcountriesyears, "mergedcountriesyears.csv")

# Regions
# All
mergedsubregions <- bind_rows(read.csv("Descriptive/1 - subregion.csv"), 
                              read.csv("Descriptive/2 - subregion.csv"), 
                              read.csv("Descriptive/3 - subregion.csv"), 
                              read.csv("Descriptive/4 - subregion.csv"), 
                              read.csv("Descriptive/5 - subregion.csv"), 
                              read.csv("Descriptive/6 - subregion.csv"), 
                              read.csv("Descriptive/7 - subregion.csv"), 
                              read.csv("Descriptive/8 - subregion.csv")) %>% 
  group_by(Var1) %>% 
  summarise_all(funs(sum(., na.rm = T)))

write.csv(mergedsubregions, "mergedsubregions.csv")

# By review group
mergedsubregionsbyreviewgroup <- bind_rows(read.csv("Descriptive/1 - subregions - review groups.csv"), 
                              read.csv("Descriptive/2 - subregions - review groups.csv"), 
                              read.csv("Descriptive/3 - subregions - review groups.csv"), 
                              read.csv("Descriptive/4 - subregions - review groups.csv"), 
                              read.csv("Descriptive/5 - subregions - review groups.csv"), 
                              read.csv("Descriptive/6 - subregions - review groups.csv"), 
                              read.csv("Descriptive/7 - subregions - review groups.csv"), 
                              read.csv("Descriptive/8 - subregions - review groups.csv")) %>% 
  group_by(region) %>% 
  summarise_all(funs(sum(., na.rm = T)))

write.csv(mergedsubregionsbyreviewgroup, "mergedsubregionsbyreviewgroup.csv")


# Year
mergedsubregionsbyyear <- bind_rows(read.csv("Descriptive/1 - subregions - years.csv"), 
                              read.csv("Descriptive/2 - subregions - years.csv"), 
                              read.csv("Descriptive/3 - subregions - years.csv"), 
                              read.csv("Descriptive/4 - subregions - years.csv"), 
                              read.csv("Descriptive/5 - subregions - years.csv"), 
                              read.csv("Descriptive/6 - subregions - years.csv"), 
                              read.csv("Descriptive/7 - subregions - years.csv"), 
                              read.csv("Descriptive/8 - subregions - years.csv")) %>% 
  group_by(region) %>% 
  summarise_all(funs(sum(., na.rm = T)))

write.csv(mergedsubregionsbyyear, "mergedsubregionsbyyear.csv")


