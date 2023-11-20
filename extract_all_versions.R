reviews = read.csv("data/20231106/cochraneauthors_reviewdf_db.csv")

all_dois = data.frame()

for (i in 1:nrow(reviews)){
  number = i
  all_dois[i, 1] = reviews$DOI[i]
  if (grepl("pub", reviews$DOI[i])) {
    last_number = as.numeric(substr(reviews$DOI[i], nchar(reviews$DOI[i]), nchar(reviews$DOI[i])))
    for (i in 1:(last_number-1)) {
      if (i == (last_number-1)) {
        # Omit .pub1
        prev_doi = sub("\\.pub\\d+$", "", reviews$DOI[number])
      } else {
        # Remove ".pubX" and append the current iteration number
        prev_doi = sub("\\.pub\\d+$", paste0(".pub", (last_number-1) - i + 1), reviews$DOI[number])
      }
      all_dois[number, (i+1)] = prev_doi
    }
  }
}


# Loading the required packages
library(dplyr)
library(here)
library(purrr)
library(rvest)
library(tidyr)


long_dois = gather(all_dois, key = "VType", value = "doi")
long_dois = na.omit(long_dois)

# Create a dataframe that has the URL to the information page for all reviews:
URLlist = paste0("https://www.cochranelibrary.com/cdsr/doi/",
                  long_dois$doi,
                  "/information")

# Retrieving all authors names and their affiliations via Cochrane Library

affiliationList = list() # an empty list to put the affiliations in

URLlist5 = URLlist[20001:length(URLlist)]

for(i in URLlist6) {
    # Just showing the progress
    print(paste0("URL ", which(URLlist6 == i), " of ", length(URLlist6), ". ", "Progress: ", round(which(URLlist6 == i)/length(URLlist6)*100, 2), "%"))
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
      affiliationList6[[i]] = output
      },
      error = function(e){})
}


affiliationList = c(affiliationList1, affiliationList2, affiliationList3, affiliationList4, affiliationList5, affiliationList6)

saveRDS(affiliationList, "data/cochraneauthors_revinfo_list.rds")

URLlist6 = setdiff(URLlist, names(affiliationList))

unique_names <- unique(names(a))

# Create a new list with unique names
unique_nested_list <- list()

# Iterate through the unique names and add corresponding elements to the new list
for (name in unique_names) {
  unique_nested_list[[name]] <- a[[name]]
}


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
len = as.data.frame(matrix(NA, ncol = 1, nrow = length(affList)))

for (i in 1:length(affList)){
  if (length(affList[[i]]) == 0) {
    len[i,] <- 0
  } else {
    len[i,] <- length(affList[[i]][[1]][[1]])
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
    output <- data.frame(plyr::ldply(affList[[i]][[1]], rbind))
    aff_df[i, 1:length(output)] <- output
    aff_df$doi[i] <- names(affList[i])
  }
}

write.csv(aff_df, "data/new/cochraneauthors_affiliations_db.csv")

# Creating review charc dataframe:
review_df = as.data.frame(matrix(NA, 
                               ncol = max(len$V1, na.rm = T)+4, 
                               nrow = length(affList)))

for (i in 1:length(affList)) {
  if (length(affList[[i]]) == 0) {
    review_df[i,] <- NA
  } else {
    output <- data.frame(plyr::ldply(affList[[i]][[1]], rbind))
    review_df[i, 1:length(output)] <- output
    review_df$doi[i] <- names(affList[i])
    review_df$date[i] = affList[[i]][[2]][1]
    review_df$type[i] = affList[[i]][[2]][2]
    review_df$stage[i] = affList[[i]][[2]][3]
    review_df$group[i] = affList[[i]][[2]][4]
  }
}

review_df = review_df[, c(46:50, 1:45)]

## Changing column names
colnames(review_df) <- c("doi",
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
                  "aff41",
                  "aff42",
                  "aff43",
                  "aff44",
                  "aff45")

# Convert date to date format:
review_df$date = as.Date(review_df$date[[1]], format = "%d %b %Y")

review_df = apply(review_df,2,as.character)

write.csv(review_df, "data/new/cochraneauthors_db.csv")
