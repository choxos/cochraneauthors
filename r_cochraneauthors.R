# Loading the required packages
library(dplyr)
library(here)
library(purrr)
library(rvest)
library(tidyr)


# Loading the list of reviews retrieved through Cochrane Library:
reviews = read.csv(here("data", "cochraneauthors_db_reviews.csv"))

# Next, we want to extract the URL of all versions of a Cochrane review. Cochrane
# uses .pubx at the end of URL/DOI to indicate the version. URLs/DOIs without
# .pubx at the end are the ones which their first version is published (usually
# the protocol). x at .pubx starts with 0 and goes up with the release of each 
# new version of the review. So, we should check whether the DOI has .pubx at
# its end and if yes, extract all other previous versions.
all_dois = data.frame()

for (i in 1:nrow(reviews)){
    number = i
    all_dois[i, 1] = db$doi[i]
    if (grepl("pub", db$doi[i])) {
      last_number = as.numeric(substr(db$doi[i], nchar(db$doi[i]), nchar(db$doi[i])))
      for (i in 1:(last_number-1)) {
        if (i == (last_number-1)) {
          # Omit .pub1
          prev_doi = sub("\\.pub\\d+$", "", db$doi[number])
        } else {
          # Remove ".pubX" and append the current iteration number
          prev_doi = sub("\\.pub\\d+$", paste0(".pub", (last_number-1) - i + 1), db$doi[number])
        }
        all_dois[number, (i+1)] = prev_doi
      }
    }
}


# Now, we want all these DOIs to be in a single column (long format):
long_dois = gather(all_dois, key = "VType", value = "doi")
long_dois = na.omit(long_dois)

# Creating a dataframe that has the URL to the information page for all reviews:
URLlist = paste0("https://www.cochranelibrary.com/cdsr/doi/",
                 long_dois$doi,
                 "/information")

# Retrieving all authors names and their affiliations via Cochrane Library
# + article information

revinfoList = list() # an empty list to put the affiliations in

for(i in URLlist) {
    # Just showing the progress
    print(paste0("URL ", which(URLlist == i), " of ", length(URLlist), ". ", "Progress: ", round(which(URLlist == i)/length(URLlist)*100, 2), "%"))
    print(paste0("Catching URL: ", i))
    tryCatch({
      authors = read_html(i) %>% html_nodes(".authors") %>% 
        map(~html_nodes(.x, 'li') %>% 
              html_text() %>% 
              gsub(pattern = '\\t|\\r|\\n', replacement = ''))
      
      publish_info_untidy = read_html(i) %>% html_nodes(".publish-information") %>% 
        map(~html_nodes(.x, 'li') %>% 
              html_text() %>% 
              gsub(pattern = '\\t|\\r|\\n', replacement = ''))
      
      publish_info = data.frame()
      publish_info[1,1] = gsub("see what's new ", "", publish_info_untidy[[1]][2])
      publish_info[1,2] = publish_info_untidy[[1]][3]
      publish_info[1,3] = publish_info_untidy[[1]][4]
      publish_info[1,4] = publish_info_untidy[[1]][5]
      names(publish_info) = c("date", "type", "stage", "group")
      
      
      output = list(authors = authors, publish_info = publish_info)
      revinfoList[[i]] = output
    },
    error = function(e){})
}


# Saving the list to an RDS file
saveRDS(revinfoList, "data/revinfoList.rds")


# Removing unnecessary parts from the names in two steps:
## Step 1:
names(revinfoList) <- gsub("https://www.cochranelibrary.com/cdsr/doi/", 
                       "", 
                       as.character(names(revinfoList)))
## Step 2:
names(revinfoList) <- gsub("/information", 
                       "", 
                       as.character(names(revinfoList)))

# Getting the length of each nested list
len = as.data.frame(matrix(NA, ncol = 1, nrow = length(revinfoList)))

for (i in 1:length(revinfoList)){
  if (length(revinfoList[[i]]) == 0) {
    len[i,] = 0
  } else {
    len[i,] = length(revinfoList[[i]][[1]][[1]])
  }
}


# Creating review characteristics dataframe:
review_df = as.data.frame(matrix(NA, 
                                 ncol = max(len$V1, na.rm = T), 
                                 nrow = length(revinfoList)))

for (i in 1:length(revinfoList)) {
  if (length(revinfoList[[i]]) == 0) {
    review_df[i,] = NA
  } else {
    output = data.frame(plyr::ldply(revinfoList[[i]][[1]], rbind))
    review_df[i, 1:length(output)] = output
    review_df$doi[i] <- names(revinfoList[i])
    review_df$date[i] = revinfoList[[i]][[2]][1]
    review_df$type[i] = revinfoList[[i]][[2]][2]
    review_df$stage[i] = revinfoList[[i]][[2]][3]
    review_df$group[i] = revinfoList[[i]][[2]][4]
  }
}

review_df = data.frame(lapply(review_df, as.character), stringsAsFactors=FALSE)

write.csv(review_df, "data/cochraneauthors_reviewdf_db.csv")


# Creating final db
db = review_df[, c(42:46, 1:41)]

colnames(db) = c("doi",
                         "date",
                         "type",
                         "stage",
                         "group",
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

# Convert date to date format:
db$date = as.Date(db$date, format = "%d %b %Y")


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
  a = grep("Correspondence", db[i, 6:46], value = T)[1]
  if (length(a) == 0){
    a = NA
  } else {
    a = a
  }
  corres_db[i, ] = a
}

### Some of the reviews are withdrawn hence they do not have corresponding
### author. That is why some columns include NA.

### Also, we get an error because one of the papers seem to have two 
### corresponding authors. As it is just one papers, we can ignore it.

### Next, we make a column for corresponding authors countries:

db$corresponding_author_country = 
  sapply(str_extract_all(corres_db$V1, all_countries), 
         toString)

### As some corresponding authors may have more than one affiliation and 
### sometimes the country differs, we extract only the country of their first 
### affiliation as it is their main affiliation:

db$corresponding_author_country = gsub("[,;(].*$",
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
db$corresponding_author_given_name <- ifelse(word(corres_db$V1, 1) == "G", 
                                             word(corres_db$V1, 2), 
                                             word(corres_db$V1, 1))

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

last_db = as.data.frame(matrix(NA, ncol = 1, nrow = nrow(db)))

for (i in 1:nrow(db)) {
  a = db[i,] %>% select(6:46)
  a = as.data.frame(a[, colSums(is.na(a)) == 0])
  b = a[, ncol(a)]
  last_db[i,] = b
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
db$last_author_given_name <- ifelse(word(last_db$V1, 1) == "G", 
                                             word(last_db$V1, 2), 
                                             word(last_db$V1, 1))

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


