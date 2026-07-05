library(dplyr)
library(stringr)
library(tidyr)

# install.packages(c("stringr", "dplyr", "countrycode"))

library(countrycode)
library(xml2)

# install.packages(c("httr2", "dplyr"))
library(httr2)

# install.packages("reticulate")
library(reticulate)

# install_miniconda()

conda_create("r-gender", python_version = "3.11")
conda_install("r-gender", "pip")
conda_install("r-gender", "gender-guesser", pip = TRUE)

use_condaenv("r-gender", required = TRUE)

gender <- import("gender_guesser.detector")
d <- gender$Detector(case_sensitive = FALSE)



# import xml.gz file
# read xml.gz file
xml_file_array <- ""



start_num <- 1
end_num <- 1462



for (i in start_num:end_num) { # 1-1334
  xml_file_array[i] <- paste0("pubmed26n", str_pad(i, 4, pad = "0"), ".xml.gz")
}


for (i in start_num:end_num) { # i <- 1333
  
  # xml_data <- read_xml("pubmed26n1334.xml.gz")
  xml_data <- read_xml(xml_file_array[i])
  
  
  # How many PubmedArticle ?
  length(xml_find_all(xml_data, ".//PubmedArticle"))
  
  #articles
  articles <- xml_find_all(xml_data, ".//PubmedArticle")
  length(articles)
  
  #extract variables
  #PMID
  # pmid <- xml_data %>% xml_find_all(".//MedlineCitation/PMID") %>% xml_text()
  pmid_nodes_list <- xml_find_all(
    articles,
    ".//MedlineCitation/PMID",
    flatten = FALSE
  )
  pmid <- vapply(pmid_nodes_list, function(pmid_nodes) {
    if (length(pmid_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(pmid_nodes), collapse = " ")
    }
  }, character(1))
  
  #AbstractText
  abs_nodes_list <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/Abstract/AbstractText",
    flatten = FALSE
  )
  
  abstract <- vapply(abs_nodes_list, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  #Title
  title_nodes_list <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/ArticleTitle",
    flatten = FALSE
  )
  title <- vapply(title_nodes_list, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  # Last name of fist author
  first_author_last_name_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author[1]/LastName",
    flatten = FALSE
  )
  first_author_last_name <- vapply(first_author_last_name_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  # First name of fist author
  first_author_first_name_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author[1]/ForeName",
    flatten = FALSE
  )
  first_author_first_name <- vapply(first_author_first_name_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  
  # Last name of last author
  last_author_last_name_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author[last()]/LastName",
    flatten = FALSE
  )
  last_author_last_name <- vapply(last_author_last_name_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  # First name of last author
  last_author_first_name_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author[last()]/ForeName",
    flatten = FALSE
  )
  last_author_first_name <- vapply(last_author_first_name_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  
  
  
  
  # Publication year
  publication_year_nodes <- xml_find_all(
    articles,
    ".//JournalIssue/PubDate/Year",
    flatten = FALSE
  )
  publication_year <- vapply(publication_year_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  
  
  
  # Author number
  author_number_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author",
    flatten = FALSE
  )
  author_number <- vapply(author_number_nodes, function(abs_nodes) {
    length(abs_nodes)
  }, numeric(1))
  
  
  
  
  
  
  
  
  
  # Publication type
  publication_type_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/PublicationTypeList/PublicationType[1]",
    flatten = FALSE
  )
  publication_type <- vapply(publication_type_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  
  
  # Jounral ID
  journal_id_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/Journal/ISSN",
    flatten = FALSE
  )
  journal_id <- vapply(journal_id_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))

  
  

  
  # First affiliation of first author
  first_author_affiliation_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author[1]/AffiliationInfo[1]/Affiliation",
    flatten = FALSE
  )
  first_author_affiliation <- vapply(first_author_affiliation_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  # First affiliation of last author
  last_author_affiliation_nodes <- xml_find_all(
    articles,
    ".//MedlineCitation/Article/AuthorList/Author[last()]/AffiliationInfo[1]/Affiliation",
    flatten = FALSE
  )
  last_author_affiliation <- vapply(last_author_affiliation_nodes, function(abs_nodes) {
    if (length(abs_nodes) == 0) {
      NA_character_
    } else {
      paste(xml_text(abs_nodes), collapse = " ")
    }
  }, character(1))
  
  
  
  
  # variation
  
  custom_match <- c(
    "USA" = "US",
    "U.S.A." = "US",
    "U.S." = "US",
    "United States" = "US",
    "United States of America" = "US",
    "UK" = "GB",
    "U.K." = "GB",
    "England" = "GB",
    "Scotland" = "GB",
    "Wales" = "GB",
    "South Korea" = "KR",
    "Korea" = "KR",
    "Republic of Korea" = "KR",
    "Russia" = "RU",
    "Vietnam" = "VN",
    "Taiwan" = "TW"
  )
  
  # last 1000 chr
  first_author_affiliation <- str_sub(first_author_affiliation, -1000)
  last_author_affiliation <- str_sub(last_author_affiliation, -1000)
  
  
  
  df_country_first <- tibble(affiliation = first_author_affiliation) %>%
    mutate(
      # raw
      country_raw = affiliation,  #raw
      
      lastcomma = str_trim(str_extract(affiliation, "[^,]+$")), # last comma
      
      # ISO 2-letter code
      iso2c = countrycode(
        country_raw,
        origin = "country.name",
        destination = "iso2c",
        custom_match = custom_match
      ),
      
      iso2c_lastcomma = countrycode(
        lastcomma,
        origin = "country.name",
        destination = "iso2c",
        custom_match = custom_match
      )
    )
  
  df_country_last <- tibble(affiliation = last_author_affiliation) %>%
    mutate(
      # raw
      country_raw = affiliation,  #raw
      
      lastcomma = str_trim(str_extract(affiliation, "[^,]+$")), # last comma
      
      # ISO 2-letter code
      iso2c = countrycode(
        country_raw,
        origin = "country.name",
        destination = "iso2c",
        custom_match = custom_match
      ),
      
      iso2c_lastcomma = countrycode(
        lastcomma,
        origin = "country.name",
        destination = "iso2c",
        custom_match = custom_match
      )
    )
  
  

  
  
  state_list <- c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York",
    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
    "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming",
    "District of Columbia",
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
    "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
    "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
    "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
    "WI", "WY", "DC"
  )
  
  # if NA and contains state name, then "US"
  df_country_first <- df_country_first %>% mutate(
    iso2c = ifelse(
      is.na(iso2c) & str_detect(country_raw, paste(state_list, collapse = "|")),
      "US",
      iso2c
    )
  )
  df_country_first <- df_country_first %>% mutate(
    iso2c_lastcomma = ifelse(
      is.na(iso2c_lastcomma) & str_detect(lastcomma, paste(state_list, collapse = "|")),
      "US",
      iso2c_lastcomma
    )
  )
  
  
  
  
  df_country_last <- df_country_last %>% mutate(
    iso2c = ifelse(
      is.na(iso2c) & str_detect(country_raw, paste(state_list, collapse = "|")),
      "US",
      iso2c
    )
  )
  df_country_last <- df_country_last %>% mutate(
    iso2c_lastcomma = ifelse(
      is.na(iso2c_lastcomma) & str_detect(lastcomma, paste(state_list, collapse = "|")),
      "US",
      iso2c_lastcomma
    )
  )
  
    
  # UK
  df_country_first <- df_country_first %>% mutate(
    iso2c = ifelse(
      is.na(iso2c) & str_detect(country_raw, "UK"),
      "GB",
      iso2c
    )
  )
  
  df_country_last <- df_country_last %>% mutate(
    iso2c = ifelse(
      is.na(iso2c) & str_detect(country_raw, "UK"),
      "GB",
      iso2c
    )
  )
  
  
  
  
  
  
  # feed from iso2c_lastcomma to iso2c if iso2c is NA
  df_country_first <- df_country_first %>% mutate(
    iso2c = ifelse(is.na(iso2c), iso2c_lastcomma, iso2c)
  )
  df_country_last <- df_country_last %>% mutate(
    iso2c = ifelse(is.na(iso2c), iso2c_lastcomma, iso2c)
  )
  
  # feed from iso2c_lastcomma to iso2c when different
  df_country_first <- df_country_first %>% mutate(
    iso2c = ifelse(
      !is.na(iso2c) & !is.na(iso2c_lastcomma) & iso2c != iso2c_lastcomma,
      iso2c_lastcomma,
      iso2c
    )
  )
  df_country_last <- df_country_last %>% mutate(
    iso2c = ifelse(
      !is.na(iso2c) & !is.na(iso2c_lastcomma) & iso2c != iso2c_lastcomma,
      iso2c_lastcomma,
      iso2c
    )       
  )
  
  # table(df_country$country_standard)
  
  # Korea
  # if df_country$iso2c is KP and df_country$country_raw contains "North" OR "People's" OR "DPRK", then keep df_country$iso2c in "KP"
  # otherwise, KP -> KR
  df_country_first <- df_country_first %>% mutate(
    iso2c = ifelse(
      iso2c == "KP" & !str_detect(country_raw, "North|People's|DPRK"),
      "KR",
      iso2c
    )
  )
  
  df_country_last <- df_country_last %>% mutate(
    iso2c = ifelse(
      iso2c == "KP" & !str_detect(country_raw, "North|People's|DPRK"),
      "KR",
      iso2c
    )
  )
  
  if (i == start_num) {
    outputdata <- tibble(
      pmid = pmid,
      title = title,
      abstract = abstract,
      year = publication_year,
      journal_id = journal_id,
      publication_type = publication_type,
      author_num = author_number,
      first_name1 = first_author_first_name,
      last_name1 = first_author_last_name,
      country1 = df_country_first$iso2c,
      first_name2 = last_author_first_name,
      last_name2 = last_author_last_name,
      country2 = df_country_last$iso2c)
    
  } else {
    temp <- tibble(
      pmid = pmid,
      title = title,
      abstract = abstract,
      year = publication_year,
      journal_id = journal_id,
      publication_type = publication_type,
      author_num = author_number,
      first_name1 = first_author_first_name,
      last_name1 = first_author_last_name,
      country1 = df_country_first$iso2c,
      first_name2 = last_author_first_name,
      last_name2 = last_author_last_name,
      country2 = df_country_last$iso2c)
    outputdata <- bind_rows(outputdata, temp)
  }
  
} # end of loop






names1 <- outputdata$first_name1
# keep before first " "
names1 <- str_extract(names1, "^[^ ]+")
gender1 <- sapply(names1, function(x) d$get_gender(x))

names2 <- outputdata$first_name2
names2 <- str_extract(names2, "^[^ ]+")
gender2 <- sapply(names2, function(x) d$get_gender(x))

outputdata$gender1 <- gender1
outputdata$gender2 <- gender2


nrow(outputdata)
length(unique(outputdata$pmid))







# when the same pmid appears, keep the last one
outputdata2 <- outputdata %>%
  group_by(pmid) %>%
  slice_tail(n = 1) %>%
  ungroup()





# write tsv
# change new_line to space
outputdata2 <- outputdata2 %>%
  mutate(
    title = str_replace_all(title, "\n", " "),
    abstract = str_replace_all(abstract, "\n", " ")
  )

write.table(outputdata2, "data.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
