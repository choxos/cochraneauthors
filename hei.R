hei = read.csv("~/Downloads/scopus (2).csv")

library(dplyr)
hei = hei %>% mutate(startswith = ifelse(startsWith(Affiliations, "Department of Health Research Methods, Evidence, and Impact") == T, T, F))

hei_first = hei %>% filter(startswith == T)

library(stringr)

first_authors = strsplit(hei_first$Author.full.names, ' [(]') %>% sapply("[[", 1) %>% str_extract('\\b[^,]+$') %>% word(1)
first_authors2 = as.data.frame(first_authors)


genderdb = read.csv("~/Documents/GitHub/cochraneauthors/data/cochraneauthors_gender_db.csv")

first_authors_gender = 0

for (i in 1:length(first_authors)) {
        a = first_authors[i]
        a = tolower(a)
        b = genderdb[which(genderdb$name == a), 2]
        ifelse(rlang::is_empty(b) == TRUE,
               first_authors_gender[i] <- NA,
               first_authors_gender[i] <- b)
}


gender_percent = as.numeric(prop.table(table(first_authors_gender))*100)
gender_percent_df = data.frame(value = gender_percent,
                               name = c("Female", "Male"))


library(plotly)

fig <- plot_ly(gender_percent_df, labels = ~name, values = ~value, type = 'pie')
fig <- fig %>% layout(title = 'Gender Inequality in Publishing as First Author in Papers from HEI Department',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

