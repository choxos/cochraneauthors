# Loading the required packages
library(dplyr)
library(here)
library(purrr)
library(rvest)

# Loading the list of reviews retrieved through Cochrane Library
reviews <- read.csv(here("data", "cochraneauthors_db_may8.csv"))

# Create a dataframe that has the URL to the information page for all reviews:
URLlist <- paste0("https://www.cochranelibrary.com/cdsr/doi/",
                  reviews$DOI,
                  "/information")

# Retrieving all authors names and their affiliations via Cochrane Library

affiliationList <- list() # an empty list to put the affiliations in

for(i in URLlist){
  tryCatch({
  output <- read_html(i) %>% html_nodes(".authors") %>% 
    map(~html_nodes(.x, 'li') %>% 
          html_text() %>% 
          gsub(pattern = '\\t|\\r|\\n', replacement = ''))
  affiliationList [[i]] <- output},
  error = function(e){})
}


# Saving the list to an RDS file
saveRDS(affiliationList, "affList.rds")


# Removing unnecessary parts from the names in two steps:
## Step 1:
names(affList) <- gsub("https://www.cochranelibrary.com/cdsr/doi/", 
                       "", 
                       as.character(names(affList)))
## Step 2:
names(affList) <- gsub("/information", 
                       "", 
                       as.character(names(affList)))

# Getting the length of each nested list
len <- as.data.frame(matrix(NA, ncol = 1, nrow = length(affList)))

for (i in 1:length(affList)){
  if (length(affList[[i]]) == 0) {
    len[i,] <- 0
  } else {
    len[i,] <- length(affList[[i]][[1]])
  }
}


# Creating affiliations dataframe:
aff_df <- as.data.frame(matrix(NA, 
                           ncol = max(len$V1, na.rm = T), 
                           nrow = length(affList)))

for (i in 1:length(affList)) {
  if (length(affList[[i]]) == 0) {
    aff_df[i,] <- NA
  } else {
    output <- data.frame(plyr::ldply(affList[[i]], rbind))
    aff_df[i, 1:length(output)] <- output
    aff_df$doi[i] <- names(affList[i])
  }
}

write.csv(aff_df, "data/cochraneauthors_affiliations_db.csv")


# Creating final db
db <- reviews %>% select(DOI, 
                         Author.s., 
                         Title, 
                         Year, 
                         Cochrane.Review.Group.Code)

db <- merge(db, aff_df, by.x = "DOI", by.y = "doi")

## Changing column names
colnames(db) <- c("doi",
                  "authors",
                  "title",
                  "year",
                  "revgroup",
                  "aff1",
                  "aff2",
                  "aff3",
                  "aff4",
                  "aff5",
                  "aff6",
                  "aff7",
                  "aff8",
                  "aff9",
                  "aff10",
                  "aff11",
                  "aff12",
                  "aff13",
                  "aff14",
                  "aff15",
                  "aff16",
                  "aff17",
                  "aff18",
                  "aff19",
                  "aff20",
                  "aff21",
                  "aff22",
                  "aff23",
                  "aff24",
                  "aff25",
                  "aff26",
                  "aff27",
                  "aff28",
                  "aff29",
                  "aff30",
                  "aff31",
                  "aff32",
                  "aff33",
                  "aff34",
                  "aff35",
                  "aff36",
                  "aff37",
                  "aff38",
                  "aff39",
                  "aff40",
                  "aff41")

## Saving the dataframe
write.csv(db, "data/cochraneauthors_db.csv")





# Extracting first authors countries, regions, and genders
pacman::p_load(dplyr,
               here,
               ggplot2,
               maps,
               stringr,
               rnaturalearth,
               sf)

# Countries of first authors

## Getting a list of all countries and then create a new column for the 
## affiliation of first authors:

all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")

db$first_author_country <- 
  sapply(str_extract_all(db$aff1, all_countries), 
         toString)


## As some first authors may have more than one affiliation and sometimes the 
## country differs, we extract only the country of their first affiliation as 
## it is their main affiliation:


db$first_author_country <- gsub("[,;(].*$", "", db$first_author_country)

## World Bank and United Nations regions:
world <- rnaturalearth::ne_countries(returnclass = "sf")
world <- world %>% st_drop_geometry()
world <- world %>% select(name, region_un, region_wb, income_grp)

### Chnaging UK to United Kingdom and USA to United States
db$first_author_country <- ifelse(db$first_author_country == "UK",
                                  "United Kingdom",
                                  db$first_author_country)

db$first_author_country <- ifelse(db$first_author_country == "USA",
                                  "United States",
                                  db$first_author_country)


### World Bank regions
for (i in 1:nrow(db)) {
  a <- db$first_author_country[i]
  b <- world[grep(a, world$name),3]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$first_author_region_wb[i] <- NA, 
         db$first_author_region_wb[i] <- b)
}

### UN regions
for (i in 1:nrow(db)) {
  a <- db$first_author_country[i]
  b <- world[grep(a, world$name), 2]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$first_author_region_un[i] <- NA, 
         db$first_author_region_un[i] <- b)
}


### Income level regions
for (i in 1:nrow(db)) {
  a <- db$first_author_country[i]
  b <- world[grep(a, world$name),4]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$first_author_region_income[i] <- NA, 
         db$first_author_region_income[i] <- b)
}


### Developed English-speaking countries are:
### The United States
### The United Kingdom
### Canada
### Australia
### Ireland
### New Zealand

## We want to measure how many of these reviews had a first author from the 
## abovementioned countries and provide a trend for that:
## First, define these countries in a string:
english_speaking <- "United States|US|USA|United Kingdom|UK|England|Canada|Australia|Ireland|New Zealand"

## Extract whether the first author has an affiliation from English-speaking 
## countries
db$first_author_english <- sapply(
  db$first_author_country,
  function(x) if (str_extract_all(x, 
                                  english_speaking) == "character(0)"){
                                    "No"
                                  } else {
                                    "Yes"
                                  })

## Gender analysis

## Extract the first word from affiliations:
db$first_author_given_name <- ifelse(word(db$aff1, 1) == "G", 
                                     word(db$aff1, 2), 
                                     word(db$aff1, 1))

genderdb <- read.csv("data/cochraneauthors_gender_db.csv")
genderdb <- genderdb[!duplicated(genderdb$name), ]

### This takes a while

for (i in 1:nrow(db)) {
  a <- db$first_author_given_name[i]
  a <- tolower(a)
  b <- genderdb[which(genderdb$name == a), 2]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$first_author_gender[i] <- NA, 
         db$first_author_gender[i] <- b)
}


# Extracting corresponding authors countries, regions, and genders

corres_db <- as.data.frame(matrix(NA, ncol = 1, nrow = nrow(db)))


for (i in 1:nrow(db)) {
  a <- grep("Correspondence", db[i, 6:46], value = T)
  if (length(a) == 0){
    a <- NA
  } else {
    a = a
  }
  corres_db[i, ] <- a
}

### Some of the reviews are withdrawn hence they do not have corresponding
### author. That is why some columns include NA.

### Also, we get an error because one of the papers seem to have two 
### corresponding authors. As it is just one papers, we can ignore it.

### Next, we make a column for corresponding authors countries:

db$corresponding_author_country <- 
  sapply(str_extract_all(corres_db$V1, all_countries), 
         toString)

### As some corresponding authors may have more than one affiliation and 
### sometimes the country differs, we extract only the country of their first 
### affiliation as it is their main affiliation:

db$corresponding_author_country <- gsub("[,;(].*$",
                                        "", 
                                        db$corresponding_author_country)


### Chnaging UK to United Kingdom and USA to United States
db$corresponding_author_country <- ifelse(db$corresponding_author_country == "UK",
                                  "United Kingdom",
                                  db$corresponding_author_country)

db$corresponding_author_country <- ifelse(db$corresponding_author_country == "USA",
                                  "United States",
                                  db$corresponding_author_country)


### World Bank regions
for (i in 1:nrow(db)) {
  a <- db$corresponding_author_country[i]
  b <- world[grep(a, world$name),3]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$corresponding_author_region_wb[i] <- NA, 
         db$corresponding_author_region_wb[i] <- b)
}

### UN regions
for (i in 1:nrow(db)) {
  a <- db$corresponding_author_country[i]
  b <- world[grep(a, world$name), 2]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$corresponding_author_region_un[i] <- NA, 
         db$corresponding_author_region_un[i] <- b)
}


### Income level regions
for (i in 1:nrow(db)) {
  a <- db$corresponding_author_country[i]
  b <- world[grep(a, world$name),4]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$corresponding_author_region_income[i] <- NA, 
         db$corresponding_author_region_income[i] <- b)
}



### We want to measure how many of these reviews had a corresponding author 
### from the English-speaking countries and provide a trend for that:

db$corresponding_author_english <- sapply(
  db$corresponding_author_country,
  function(x) if (str_extract_all(x, 
                                  english_speaking) == "character(0)"){
                                            "No"
                                          } else {
                                            "Yes"
                                          })

## Gender analysis

## Extract the first word from affiliations:
db$corresponding_author_given_name <- ifelse(word(db$aff1, 1) == "G", 
                                             word(db$aff1, 2), 
                                             word(db$aff1, 1))

### This takes a while
for (i in 1:nrow(db)) {
  a <- db$corresponding_author_given_name[i]
  a <- tolower(a)
  b <- genderdb[which(genderdb$name == a), 2]
  ifelse(rlang::is_empty(b) == TRUE,
         db$corresponding_author_gender[i] <- NA,
         db$corresponding_author_gender[i] <- b)
}



# Extracting last authors countries, regions, and genders
### First, extracting affiliation of the last authors:

last_db <- as.data.frame(matrix(NA, ncol = 1, nrow = nrow(db)))

for (i in 1:nrow(db)) {
  a <- db[i,] %>% select(6:46)
  a <- as.data.frame(a[, colSums(is.na(a)) == 0])
  b <- a[, ncol(a)]
  last_db[i,] <- b
}


### Next, we make a column for corresponding authors countries:

db$last_author_country <- 
  sapply(str_extract_all(last_db$V1, all_countries), 
         toString)

### As some corresponding authors may have more than one affiliation and 
### sometimes the country differs, we extract only the country of their 
### first affiliation as it is their main affiliation:

db$last_author_country <- gsub("[,;(].*$", "", db$last_author_country)


### Chnaging UK to United Kingdom and USA to United States
db$last_author_country <- ifelse(db$last_author_country == "UK",
                                          "United Kingdom",
                                          db$last_author_country)

db$last_author_country <- ifelse(db$last_author_country == "USA",
                                          "United States",
                                          db$last_author_country)


### World Bank regions
for (i in 1:nrow(db)) {
  a <- db$last_author_country[i]
  b <- world[grep(a, world$name),3]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$last_author_region_wb[i] <- NA, 
         db$last_author_region_wb[i] <- b)
}

### UN regions
for (i in 1:nrow(db)) {
  a <- db$last_author_country[i]
  b <- world[grep(a, world$name), 2]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$last_author_region_un[i] <- NA, 
         db$last_author_region_un[i] <- b)
}


### Income level regions
for (i in 1:nrow(db)) {
  a <- db$last_author_country[i]
  b <- world[grep(a, world$name),4]
  ifelse(rlang::is_empty(b) == TRUE, 
         db$last_author_region_income[i] <- NA, 
         db$last_author_region_income[i] <- b)
}



### We want to measure how many of these reviews had a corresponding author 
### from the English-speaking countries and provide a trend for that:

db$last_author_english <- sapply(
  db$last_author_country,
  function(x) if (str_extract_all(x, 
                                  english_speaking) == "character(0)"){
    "No"
  } else {
    "Yes"
  })

## Gender analysis

## Extract the first word from affiliations:
db$last_author_given_name <- ifelse(word(db$aff1, 1) == "G", 
                                             word(db$aff1, 2), 
                                             word(db$aff1, 1))

### This takes a while

for (i in 1:nrow(db)) {
  a <- db$last_author_given_name[i]
  a <- tolower(a)
  b <- genderdb[which(genderdb$name == a), 2]
  ifelse(rlang::is_empty(b) == TRUE,
         db$last_author_gender[i] <- NA,
         db$last_author_gender[i] <- b)
}

write.csv(db, "data/cochraneauthors_db_final.csv")


