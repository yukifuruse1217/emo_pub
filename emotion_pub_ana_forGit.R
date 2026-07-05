library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(data.table)

library(cols4all)
library(RColorBrewer)
#install.packages("Polychrome")
library(Polychrome)
#install.packages("patchwork")
library(patchwork)

library(dplyr)
library(ggrepel)
library(forcats)

# install.packages("Kendall")
library(Kendall)



df <- fread(
  "data.tsv",
  sep = "\t",
  quote = "",
  na.strings = c("", "NA")
)
nrow(df)
table(df$year)
table(df$gender1)






# when the same pmid appears, keep the last one
nrow(df)
length(unique(df$pmid))

# save everything in env
# save.image("before_slice.RData")

df <- df %>%
  group_by(pmid) %>%
  slice_tail(n = 1) %>%
  ungroup()

nrow(df)






# check_array

check_array <- c("!", "!!", "?", "??", "!?", "?!", "<", ">", "-", "–", "—", "*", "**")
check_col <- c("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc", "sym_lt", "sym_gt", "sym_minus", "sym_endash", "sym_emdash", "sym_ast", "sym_astasta")

col_title <- paste0("title_", check_col)
col_abst <- paste0("abst_", check_col)
col_titleabst <- paste0("titleabst_", check_col)

col_title <- paste0("title_", check_col)
col_abst <- paste0("abst_", check_col)
col_titleabst <- paste0("titleabst_", check_col)






check_array_txt <- c(" surprised", " surprising", " shocked", " shocking", " astonishing", " astonished", " exciting", " excited", " amazing", " amazed",
                     " impressive", " impressed", " thrilling", " thrilled", " fascinating", " fascinated", " stunning", " stunned",
                     " interested", " interesting", "startled", "startling",
                     " spectacular", " breathtaking", " delighted", " wonderful", " lovely", " adorable", " awesome", " handsome", " manly", " feminine", " womanly", " masculine", " beautiful",
                     " mind-blowing", " jaw-dropping", " game-chang", " game chang", " paradigm shift",
                     " neutral")
check_col_txt <- c("surprised", "surprising", "shocked", "shocking", "astonishing", "astonished", "exciting", "excited", "amazing", "amazed",
                   "impressive", "impressed", "thrilling", "thrilled", "fascinating", "fascinated", "stunning", "stunned",
                   "interested", "interesting", "startled", "startling",
                   "spectacular", "breathtaking", "delighted", "wonderful", "lovely", "adorable", "awesome", "handsome", "manly", "feminine", "womanly", "masculine", "beautiful",
                   "mind-blowing", "jaw-dropping", "game-chang", "game chang", "paradigm shift",
                   "neutral")

col_title_txt <- paste0("title_", check_col_txt)
col_abst_txt <- paste0("abst_", check_col_txt)
col_titleabst_txt <- paste0("titleabst_", check_col_txt)






check_col_comb <- c(check_col, check_col_txt)
col_title_comb <- paste0("title_", check_col_comb)
col_abst_comb <- paste0("abst_", check_col_comb)
col_titleabst_comb <- paste0("titleabst_", check_col_comb)






df <- data.frame(df)
setDT(df)






n <- nrow(df)

res_list <- list()

for (i in seq_along(check_array)) {
  pat <- fixed(check_array[i])
  
  hit_abst  <- str_detect(df[["abstract"]], pat)
  hit_title <- str_detect(df[["title"]], pat)
  
  # hit_titleabst <- rep(NA, n)
  # hit_titleabst[hit_abst %in% FALSE | hit_title %in% FALSE] <- FALSE
  # hit_titleabst[hit_abst %in% TRUE  | hit_title %in% TRUE]  <- TRUE
  
  res_list[[col_abst[i]]]      <- as.integer(hit_abst)
  res_list[[col_title[i]]]     <- as.integer(hit_title)
  # res_list[[col_titleabst[i]]] <- as.integer(hit_titleabst)
  
  rm(hit_abst, hit_title)
  gc()
}





for (i in seq_along(check_array_txt)) {
  pat <- check_array_txt[i]
  
  hit_abst  <- str_detect(df[["abstract"]], pat)
  hit_title <- str_detect(df[["title"]], pat)
  
  # hit_titleabst <- rep(NA, n)
  # hit_titleabst[hit_abst %in% FALSE | hit_title %in% FALSE] <- FALSE
  # hit_titleabst[hit_abst %in% TRUE  | hit_title %in% TRUE]  <- TRUE
  
  res_list[[col_abst_txt[i]]]      <- as.integer(hit_abst)
  res_list[[col_title_txt[i]]]     <- as.integer(hit_title)
  # res_list[[col_titleabst_txt[i]]] <- as.integer(hit_titleabst)
  
  rm(hit_abst, hit_title)
  gc()
}






res_dt <- as.data.table(res_list)






# combine with dt
setDT(df)
setDT(res_dt)

stopifnot(nrow(df) == nrow(res_dt))

cols_add <- setdiff(names(res_dt), names(df))

for (nm in cols_add) {
  set(df, j = nm, value = res_dt[[nm]])
}

df <- data.frame(df)

rm(res_dt)
rm(res_list)






# read csv
jif <- read_csv("if2026_over10.csv")
jif <- c(jif$issn, jif$eissn)

df$highIF <- ifelse(df$journal_id %in% jif, "1", "0")





 
# Author num group
df <- df %>%
  mutate(author_num_group = case_when(
    author_num == 1 ~ "01",
    author_num == 2 ~ "02",
    author_num == 3 ~ "03",
    author_num == 4 ~ "04",
    author_num == 5 ~ "05",
    author_num <= 10 ~ "06",
    author_num >= 11 ~ "11",
    TRUE ~ NA_character_
  ))

df$author_num_group <- relevel(factor(df$author_num_group), ref = "05")






# filter article
table(df$publication_type)
df <- df %>%
  filter(publication_type %in% c("Journal Article"))

# nrow(df_original00)
nrow(df)



# list of country >=10000
table(df$country1)
country_counts <- table(df$country1)

countries_to_keep <- names(country_counts[country_counts >= 10000])
df <- df %>%
  mutate(country1 = ifelse(country1 %in% countries_to_keep, country1, NA),
         country2 = ifelse(country2 %in% countries_to_keep, country2, NA))
table(df$country1)

df$country1 <- relevel(factor(df$country1), ref = "US")
df$country2 <- relevel(factor(df$country2), ref = "US")






table(df$gender1)
df <- df %>%
  mutate(gender1 = case_when(
    gender1 == "male" ~  "male",
    gender1 == "mostly_male" ~ "male",
    gender1 == "female" ~ "female",
    gender1 == "mostly_female" ~ "female",
    NA ~ NA_character_)
  )
table(df$gender1)

table(df$gender2)
df <- df %>%
  mutate(gender2 = case_when(
    gender2 == "male" ~  "male",
    gender2 == "mostly_male" ~ "male",
    gender2 == "female" ~ "female",
    gender2 == "mostly_female" ~ "female",
    NA ~ NA_character_)
  )
table(df$gender2)






# nrow of abstract not empty
nrow(df %>% filter(!is.na(abstract) & abstract != ""))

# nrow of abstract not empty & gender1 == "male"
nrow(df %>% filter(!is.na(abstract) & abstract != "" & gender1 == "male"))
nrow(df %>% filter(!is.na(abstract) & abstract != "" & gender2 == "male"))
nrow(df %>% filter(!is.na(abstract) & abstract != "" & gender1 == "female"))
nrow(df %>% filter(!is.na(abstract) & abstract != "" & gender2 == "female"))

# nrow of countries
nrow(df %>% filter(!is.na(abstract) & abstract != "" & country1 == "US"))
nrow(df %>% filter(!is.na(abstract) & abstract != "" & country2 == "US"))
nrow(df %>% filter(!is.na(abstract) & abstract != "" & country1 == "CN"))
nrow(df %>% filter(!is.na(abstract) & abstract != "" & country2 == "CN"))

# top 20 countries in descending order
df %>%
  filter(!is.na(abstract) & abstract != "") %>%
  group_by(country1) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

df %>%
  filter(!is.na(abstract) & abstract != "") %>%
  group_by(country2) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)



# year
min(df$year, na.rm = TRUE)
max(df$year, na.rm = TRUE)

# year with abstract
min(df$year[!is.na(df$abstract) & df$abstract != ""], na.rm = TRUE)
max(df$year[!is.na(df$abstract) & df$abstract != ""], na.rm = TRUE)




# Analysis
library(data.table)

or_factor_fast <- function(df, x, y = "outcome", ref = NULL, add = 0.5) {
  setDT(df)
  
  cnt <- df[
    !is.na(get(x)) & !is.na(get(y)),
    .N,
    by = .(level = get(x), y01 = as.integer(get(y)))
  ]
  
  wide <- dcast(cnt, level ~ y01, value.var = "N", fill = 0)
  
  if (!"0" %in% names(wide)) wide[, `0` := 0L]
  if (!"1" %in% names(wide)) wide[, `1` := 0L]
  
  setnames(wide, c("0", "1"), c("noncase", "case"))
  
  if (is.null(ref)) {
    if (is.factor(df[[x]])) {
      ref <- levels(df[[x]])[1]
    } else {
      ref <- wide$level[1]
    }
  }
  
  ref_row <- wide[level == ref]
  
  if (nrow(ref_row) != 1) {
    stop("Reference level not found or not unique.")
  }
  
  c_ref <- ref_row$case
  d_ref <- ref_row$noncase
  
  wide[, zero_cell := case == 0 | noncase == 0 | c_ref == 0 | d_ref == 0]
  
  wide[, a := case    + ifelse(zero_cell, add, 0)]
  wide[, b := noncase + ifelse(zero_cell, add, 0)]
  wide[, c := c_ref   + ifelse(zero_cell, add, 0)]
  wide[, d := d_ref   + ifelse(zero_cell, add, 0)]
  
  wide[, logOR := log((a / b) / (c / d))]
  wide[, SE := sqrt(1/a + 1/b + 1/c + 1/d)]
  
  wide[, `:=`(
    z = logOR / SE,
    p_value = 2 * pnorm(-abs(logOR / SE)),
    OR = exp(logOR),
    lower95 = exp(logOR - 1.96 * SE),
    upper95 = exp(logOR + 1.96 * SE)
  )]
  
  wide[level == ref, `:=`(
    OR = 1,
    lower95 = NA_real_,
    upper95 = NA_real_,
    z = NA_real_,
    p_value = NA_real_
  )]
  
  wide[, .(
    variable = x,
    level,
    case,
    noncase,
    OR,
    lower95,
    upper95,
    p_value
  )]
}






################ Abstract ################
# filter abstract is not NA

df <- df %>%
  filter(!is.na(abstract))

nrow(df)



# sum column for data[[col_abst_comb[i]]
sum_array <- c()
for (i in seq_along(check_col_comb)) {
  sum_array[i] <- sum(df[[col_abst_comb[i]]], na.rm = TRUE)
}

sum_result <- data.frame(check_col_comb, sum_array)

#save csv
write.table(sum_result, "abstract_sum_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")






#year analysis
# count TRUE, FALSE in df[[col_abst[i]] by year
year_range <- 1781:2028
df_year <- data.frame(year = year_range)


setDT(df)
setDT(df_year)

df_year[, year := as.integer(year)]


missing_cols <- setdiff(col_abst_comb, names(df))
missing_cols
col_abst_comb_exist <- intersect(col_abst_comb, names(df))

prop_true <- function(x) {
  out <- mean(x, na.rm = TRUE)
  if (is.nan(out)) NA_real_ else out
}

df_prop <- df[
  ,
  lapply(.SD, prop_true),
  by = .(year = as.integer(year)),
  .SDcols = col_abst_comb_exist
]


df_year <- merge(
  df_year,
  df_prop,
  by = "year",
  all.x = TRUE
)

write.table(df_year, "abstract_df_year.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")






# Statistical analysis abst_sym_que by gender1

# gender
# number of TRUE, FALSE, proportion by gender 1
res <- or_factor_fast(df, x = "gender1", y = "abst_sym_que", ref = "female")
res

symbol <- c()
gender1 <- c()
count1 <- c()
count0 <- c()
or <- c()
lo <- c()
hi <- c()
p <- c()

for (i in seq_along(col_abst_comb_exist)) {
  temp_res <- or_factor_fast(df, x = "gender1", y = col_abst_comb_exist[i], ref = "female")
  gender1 <- c(gender1, temp_res[[2]])
  count1 <- c(count1, temp_res[[3]])
  count0 <- c(count0, temp_res[[4]])
  or <- c(or, temp_res[[5]])
  lo <- c(lo, temp_res[[6]])
  hi <- c(hi, temp_res[[7]])
  p <- c(p, temp_res[[8]])
  symbol <- c(symbol, rep(col_abst_comb[i], nrow(temp_res)))
}

gender_result <- data.frame(symbol, gender1, count1, count0, or, lo, hi, p)
gender_result$prop <- gender_result$count1 / (gender_result$count0 + gender_result$count1)

write.table(gender_result, "abstract_gender1_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")



# number of TRUE, FALSE, proportion by gender 2
res <- or_factor_fast(df, x = "gender2", y = "abst_sym_que", ref = "female")
res

symbol <- c()
gender2 <- c()
count1 <- c()
count0 <- c()
or <- c()
lo <- c()
hi <- c()
p <- c()

for (i in seq_along(col_abst_comb_exist)) {
  temp_res <- or_factor_fast(df, x = "gender2", y = col_abst_comb_exist[i], ref = "female")
  gender2 <- c(gender2, temp_res[[2]])
  count1 <- c(count1, temp_res[[3]])
  count0 <- c(count0, temp_res[[4]])
  or <- c(or, temp_res[[5]])
  lo <- c(lo, temp_res[[6]])
  hi <- c(hi, temp_res[[7]])
  p <- c(p, temp_res[[8]])
  symbol <- c(symbol, rep(col_abst_comb[i], nrow(temp_res)))
}

gender_result <- data.frame(symbol, gender2, count1, count0, or, lo, hi, p)
gender_result$prop <- gender_result$count1 / (gender_result$count0 + gender_result$count1)

write.table(gender_result, "abstract_gender2_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")






# Author num
temp <- df %>%
  group_by(author_num_group) %>%
  summarise(count_true = sum(abst_sym_que, na.rm = TRUE),
            count_false = sum(!abst_sym_que, na.rm = TRUE),
            proportion = count_true / (count_true + count_false)) %>%
  arrange(desc(proportion))
temp

table(df$author_num_group, df$abst_sym_que)

# odds ratio, p-value
symbol <- c()
auth_num <- c()
count1 <- c()
count0 <- c()
or <- c()
lo <- c()
hi <- c()
p <- c()

res <- or_factor_fast(df, x = "author_num_group", y = "abst_sym_que", ref = "05")
res

for (i in seq_along(col_abst_comb_exist)) {
  temp_res <- or_factor_fast(df, x = "author_num_group", y = col_abst_comb_exist[i], ref = "05")
  auth_num <- c(auth_num, as.character(temp_res[[2]]))
  count1 <- c(count1, temp_res[[3]])
  count0 <- c(count0, temp_res[[4]])
  or <- c(or, temp_res[[5]])
  lo <- c(lo, temp_res[[6]])
  hi <- c(hi, temp_res[[7]])
  p <- c(p, temp_res[[8]])
  symbol <- c(symbol, rep(col_abst_comb[i], nrow(temp_res)))
}

authNum_result <- data.frame(symbol, auth_num, count1, count0, or, lo, hi, p)
authNum_result$prop <- authNum_result$count1 / (authNum_result$count0 + authNum_result$count1)

write.table(authNum_result, "abstract_author_num_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")






# Country1
temp <- df %>%
  group_by(country1) %>%
  summarise(count_true = sum(abst_sym_que, na.rm = TRUE),
            count_false = sum(!abst_sym_que, na.rm = TRUE),
            proportion = count_true / (count_true + count_false)) %>%
  arrange(desc(proportion))
temp

table(df$country1, df$abst_sym_que)

# odds ratio, p-value
symbol <- c()
country1 <- c()
count1 <- c()
count0 <- c()
or <- c()
lo <- c()
hi <- c()
p <- c()

res <- or_factor_fast(df, x = "country1", y = "abst_sym_que", ref = "US")
res

for (i in seq_along(col_abst_comb_exist)) {
  temp_res <- or_factor_fast(df, x = "country1", y = col_abst_comb_exist[i], ref = "US")
  country1 <- c(country1, as.character(temp_res[[2]]))
  count1 <- c(count1, temp_res[[3]])
  count0 <- c(count0, temp_res[[4]])
  or <- c(or, temp_res[[5]])
  lo <- c(lo, temp_res[[6]])
  hi <- c(hi, temp_res[[7]])
  p <- c(p, temp_res[[8]])
  symbol <- c(symbol, rep(col_abst_comb[i], nrow(temp_res)))
}

country_result <- data.frame(symbol, country1, count1, count0, or, lo, hi, p)
country_result$prop <- country_result$count1 / (country_result$count0 + country_result$count1)

write.table(country_result, "abstract_country1_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")




# Country2
temp <- df %>%
  group_by(country2) %>%
  summarise(count_true = sum(abst_sym_que, na.rm = TRUE),
            count_false = sum(!abst_sym_que, na.rm = TRUE),
            proportion = count_true / (count_true + count_false)) %>%
  arrange(desc(proportion))
temp

table(df$country2, df$abst_sym_que)

# odds ratio, p-value
symbol <- c()
country2 <- c()
count1 <- c()
count0 <- c()
or <- c()
lo <- c()
hi <- c()
p <- c()

res <- or_factor_fast(df, x = "country2", y = "abst_sym_que", ref = "US")
res

for (i in seq_along(col_abst_comb_exist)) {
  temp_res <- or_factor_fast(df, x = "country2", y = col_abst_comb_exist[i], ref = "US")
  country2 <- c(country2, as.character(temp_res[[2]]))
  count1 <- c(count1, temp_res[[3]])
  count0 <- c(count0, temp_res[[4]])
  or <- c(or, temp_res[[5]])
  lo <- c(lo, temp_res[[6]])
  hi <- c(hi, temp_res[[7]])
  p <- c(p, temp_res[[8]])
  symbol <- c(symbol, rep(col_abst_comb[i], nrow(temp_res)))
}

country_result <- data.frame(symbol, country2, count1, count0, or, lo, hi, p)
country_result$prop <- country_result$count1 / (country_result$count0 + country_result$count1)

write.table(country_result, "abstract_country2_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")






### Impact factor
# number of TRUE, FALSE, proportion by gender 1
res <- or_factor_fast(df, x = "highIF", y = "abst_sym_que", ref = "0")
res

symbol <- c()
highIF <- c()
count1 <- c()
count0 <- c()
or <- c()
lo <- c()
hi <- c()
p <- c()

for (i in seq_along(col_abst_comb_exist)) {
  temp_res <- or_factor_fast(df, x = "highIF", y = col_abst_comb_exist[i], ref = "0")
  highIF <- c(highIF, temp_res[[2]])
  count1 <- c(count1, temp_res[[3]])
  count0 <- c(count0, temp_res[[4]])
  or <- c(or, temp_res[[5]])
  lo <- c(lo, temp_res[[6]])
  hi <- c(hi, temp_res[[7]])
  p <- c(p, temp_res[[8]])
  symbol <- c(symbol, rep(col_abst_comb[i], nrow(temp_res)))
}

highIF_result <- data.frame(symbol, highIF, count1, count0, or, lo, hi, p)
highIF_result$prop <- highIF_result$count1 / (highIF_result$count0 + highIF_result$count1)

write.table(highIF_result, "abstract_highIF_result.csv", sep = ",", quote = FALSE, row.names = FALSE, na = "")






###### Analysis ######

###### basic
base_abst <- read_csv("abstract_sum_result.csv")






###### year

cat1 <- c("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc")
cat2 <- c("surprised", "surprising", "shocked", "shocking", "exciting", "excited", "interested", "interesting")
cat3 <- c("spectacular", "wonderful", "awesome", "beautiful")
cat4 <- c("feminine", "masculine", "neutral")

# year_title <- read_csv("title_df_year.csv")
year_abst <- read_csv("abstract_df_year.csv")




df_year_abst <- year_abst
df_year_long_abst <- df_year_abst %>%
  pivot_longer(cols = starts_with("abst_"), names_to = "check", values_to = "proportion") %>%
  mutate(check = str_replace(check, "abst_", ""))



# df_year_long_title$proportion <- log10(df_year_long_title$proportion)
df_year_long_abst$proportion <- log10(df_year_long_abst$proportion)


# filter >1970, <2026
# df_year_long_title <- df_year_long_title %>%
#   filter(year >= 1970 & year <= 2026)
df_year_long_abst <- df_year_long_abst %>%
  filter(year >= 1980 & year <= 2026)



### stat
var <- cat2[8]
var

temp <- df_year_long_abst %>%
  filter(check %in% var)
# after 1990
temp <- temp %>%
  filter(year >= 1990)

# test
cor.test(temp$year, temp$proportion, method = "kendall")






# split
# df_year_long_sub <- df_year_long_title %>%
#   filter(check %in% cat2)
df_year_long_sub <- df_year_long_abst %>%
  filter(check %in% cat2)




# re-write
df_year_long_sub$check <- str_replace(df_year_long_sub$check, "sym_excexc", "!!")
df_year_long_sub$check <- str_replace(df_year_long_sub$check, "sym_queque", "??")
df_year_long_sub$check <- str_replace(df_year_long_sub$check, "sym_excque", "!?")
df_year_long_sub$check <- str_replace(df_year_long_sub$check, "sym_queexc", "?!")
df_year_long_sub$check <- str_replace(df_year_long_sub$check, "sym_exc", "!")
df_year_long_sub$check <- str_replace(df_year_long_sub$check, "sym_que", "?")




cols <- light.colors(10)
cols <- as.character(cols)




df_year_long_sub <- df_year_long_sub %>%
  mutate(label_x = case_when(
    year == 2026 ~ check,
    TRUE ~ NA_character_
  ))




ggplot(df_year_long_sub, aes(x = year, y = proportion, group = check, color = check)) +
  geom_line(size = 2, alpha = 0.7) +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "", x = "Year", y = "Proportion (log10)", color = "") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1980, 2035),
    breaks = seq(1980, 2026, by = 5)
  ) +
  scale_y_continuous(
    limits = c(-7, 0),
    breaks = seq(-7, 0, by = 1)
  ) +
  theme(
    axis.title.x = element_text(size = 28),
    axis.text.x  = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.y  = element_text(size = 28),
    plot.title   = element_text(size = 32),
    legend.position = "none",
    plot.margin = margin(5.5, 50, 5.5, 5.5) 
  ) +
  geom_text_repel(
    aes(color = check, label = label_x),
    family = "Lato",
    fontface = "bold",
    size = 10,
    direction = "y",
    xlim = c(2026.0, NA),
    hjust = 0,
    nudge_x = 1.5,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  ) +
  coord_cartesian(clip = "off")

#1200 x 800, cat1 (14), cat2 (10)






###### gender
cat1 <- c("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc")
cat2 <- c("surprised", "surprising", "shocked", "shocking", "exciting", "excited", "interested", "interesting")
cat3 <- c("spectacular", "wonderful", "awesome", "beautiful")
cat4 <- c("feminine", "masculine", "neutral")

gender_result <- read_csv("abstract_gender1_result.csv")
gender_result2 <- read_csv("abstract_gender2_result.csv")

colnames(gender_result)[2] <- "gender"
colnames(gender_result2)[2] <- "gender"

gender_result$auth <- rep("First author", nrow(gender_result))
gender_result2$auth <- rep("Last author", nrow(gender_result))

gender_result <- rbind(gender_result, gender_result2)



# remove "title/abst_" from symbol
gender_result$symbol <- str_replace(gender_result$symbol, "title_", "")
gender_result$symbol <- str_replace(gender_result$symbol, "abst_", "")




# split
gender_result_sub <- gender_result %>%
  filter(symbol %in% cat2)

# change symbol ("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc") to ("!", "!!", "?", "??", "!?", "?!")
gender_result_sub$symbol <- str_replace(gender_result_sub$symbol, "sym_excexc", "!!")
gender_result_sub$symbol <- str_replace(gender_result_sub$symbol, "sym_queque", "??")
gender_result_sub$symbol <- str_replace(gender_result_sub$symbol, "sym_excque", "!?")
gender_result_sub$symbol <- str_replace(gender_result_sub$symbol, "sym_queexc", "?!")
gender_result_sub$symbol <- str_replace(gender_result_sub$symbol, "sym_exc", "!")
gender_result_sub$symbol <- str_replace(gender_result_sub$symbol, "sym_que", "?")


### visualization
gender_result_sub <- gender_result_sub %>%
  filter(gender == "male")

# order of symbol by or
gender_result_sub$symbol <- as.factor(gender_result_sub$symbol)
unique(gender_result_sub$symbol)
gender_result_sub <- gender_result_sub %>%
  mutate(category = fct_reorder(symbol, or))
unique(gender_result_sub$category)

# graph. or on x-axis (95%CI lo, hi), symbol on y-axis

dodge_width <- -0.75

ggplot(gender_result_sub, aes(x = or, y = category, xmin = lo, xmax = hi)) +
  geom_point(aes(color = auth, group = auth), size = 5, position = position_dodge(width = dodge_width)) +  # Apply dodge here
  geom_errorbarh(aes(xmin = lo, xmax = hi, color = auth, group = auth), height = 0.2, position = position_dodge(width = dodge_width), size = 1) +  # And here
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = "", x = "Female  <-  Odds ratio  ->  Male                                      ", y = "") +
  scale_color_manual(values = c("gray20", "gray50")) +
  # scale_color_manual(values = c("Vaccine development possible" = "lightblue", "100 days mission achievable" = "pink"),
  #                    breaks = c("Vaccine development possible", "100 days mission achievable")) +
  
  # coord_flip() +
  scale_y_discrete(limits = rev) +
  
  scale_x_continuous(
    limits = c(0, 3.2),
    breaks = seq(0, 3.2, by = 0.5)
  ) +
  theme(legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
  theme(legend.position = "top", legend.direction = "vertical") +
  guides(color = guide_legend(title = ""))

# 700 x 700






###### Num Author
cat1 <- c("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc")
cat2 <- c("surprised", "surprising", "shocked", "shocking", "exciting", "excited", "interested", "interesting")
cat3 <- c("spectacular", "wonderful", "awesome", "beautiful")
cat4 <- c("feminine", "masculine", "neutral")

authNum_result <- read_csv("abstract_author_num_result.csv")

# visualization
authNum_result$or <- log2(as.numeric(authNum_result$or))
authNum_result$lo <- log2(as.numeric(authNum_result$lo))
authNum_result$hi <- log2(as.numeric(authNum_result$hi))

authNum_result$auth_num <- as.factor(authNum_result$auth_num)

# remove "title/abst_" from symbol
authNum_result$symbol <- str_replace(authNum_result$symbol, "title_", "")
authNum_result$symbol <- str_replace(authNum_result$symbol, "abst_", "")




# split
authNum_result_sub <- authNum_result %>%
  filter(symbol %in% cat2)



# change symbol ("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc") to ("!", "!!", "?", "??", "!?", "?!")
authNum_result_sub$symbol <- str_replace(authNum_result_sub$symbol, "sym_excexc", "!!")
authNum_result_sub$symbol <- str_replace(authNum_result_sub$symbol, "sym_queque", "??")
authNum_result_sub$symbol <- str_replace(authNum_result_sub$symbol, "sym_excque", "!?")
authNum_result_sub$symbol <- str_replace(authNum_result_sub$symbol, "sym_queexc", "?!")
authNum_result_sub$symbol <- str_replace(authNum_result_sub$symbol, "sym_exc", "!")
authNum_result_sub$symbol <- str_replace(authNum_result_sub$symbol, "sym_que", "?")

# visualization

# authNum_result_sub$symbol <- factor(authNum_result_sub$symbol, levels = c("!", "?", "!!", "??", "!?", "?!"))
authNum_result_sub$symbol <- factor(authNum_result_sub$symbol, levels = sort(unique(authNum_result_sub$symbol)))



authNum_result_sub <- authNum_result_sub %>%
  mutate(auth_num = case_when(
    auth_num == "01" ~ "1",
    auth_num == "02" ~ "2",
    auth_num == "03" ~ "3",
    auth_num == "04" ~ "4",
    auth_num == "05" ~ "5",
    auth_num == "06" ~ "6-10",
    auth_num == "11" ~ ">=10",
    TRUE ~ NA_character_
    )
  )

authNum_result_sub$auth_num2 <- factor(authNum_result_sub$auth_num, levels = c("1", "2", "3", "4", "5", "6-10", ">=10"))

ggplot(authNum_result_sub, aes(x = symbol, y = as.numeric(or), color = auth_num2)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8, color = "black") +
  geom_point(size = 3.0, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = as.numeric(lo), ymax = as.numeric(hi)),
    width = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  theme_minimal() +
  labs(title = "", x = "", y = "log2(Odds ratio)", color = "Number of authors") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 28),
    # axis.text.x = element_text(size = 28),
    axis.text.x = element_text(size = 24, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 28),
    plot.title = element_text(size = 28),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.position = "right",
    legend.direction = "vertical"
  )

# 1000 x 800
# cat1 (28), cat2 (24, amgle)






###### Country
cat1 <- c("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc")
cat2 <- c("surprised", "surprising", "shocked", "shocking", "exciting", "excited", "interested", "interesting")
cat3 <- c("spectacular", "wonderful", "awesome", "beautiful")
cat4 <- c("feminine", "masculine", "neutral")


country_result <- read.csv("title_country1_result.csv")
country_result2 <- read.csv("title_country2_result.csv")

colnames(country_result)[2] <- "country"
colnames(country_result2)[2] <- "country"

country_result$auth <- rep("first", nrow(country_result))
country_result2$auth <- rep("last", nrow(country_result))

country_result <- rbind(country_result, country_result2)

country_result <- country_result %>%
  filter(country != "JE")


# remove "title/abst_" from symbol
country_result$symbol <- str_replace(country_result$symbol, "title_", "")
country_result$symbol <- str_replace(country_result$symbol, "abst_", "")



# split

country_result$symbol <- str_replace(country_result$symbol, "sym_exc", "!")
country_result$symbol <- str_replace(country_result$symbol, "sym_que", "?")



### log
country_result$or <- log2(as.numeric(country_result$or))
country_result$lo <- log2(as.numeric(country_result$lo))
country_result$hi <- log2(as.numeric(country_result$hi))

country_result$country <- as.factor(country_result$country)




# top 20 countries by count0 + count1 for symbol == "!"
top20_countries <- country_result %>%
  filter(symbol == "!" & auth == "first") %>%
  arrange(desc(count0 + count1)) %>%
  slice_head(n = 20) %>%
  pull(country)

top20_countries <- as.character(top20_countries)

sort(top20_countries)

country_result_sub <- country_result %>%
  filter(country %in% top20_countries)




# ranking list

symbol <- c()
top1 <- c()
top2 <- c()
top3 <- c()
top4 <- c()
top5 <- c()
bottom1 <- c()
bottom2 <- c()
bottom3 <- c()
bottom4 <- c()
bottom5 <- c()

df_mean <- country_result_sub %>%
  group_by(symbol, country) %>%
  summarise(
    mean_or = mean(or, na.rm = TRUE),
    .groups = "drop"
  )

df_mean <- country_result_sub %>%
  group_by(symbol, country) %>%
  summarise(
    mean_prop = mean(prop, na.rm = TRUE),
    .groups = "drop"
  )


# for (i in cat5) {
for (i in unique(df_mean$symbol)) {
  # i=cat5[1]
  temp <- df_mean %>%
    filter(symbol == i)

  top1 <- c(top1, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = TRUE)[1]]), temp$mean_prop[order(temp$mean_prop, decreasing = TRUE)[1]]))
  top2 <- c(top2, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = TRUE)[2]]), temp$mean_prop[order(temp$mean_prop, decreasing = TRUE)[2]]))
  top3 <- c(top3, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = TRUE)[3]]), temp$mean_prop[order(temp$mean_prop, decreasing = TRUE)[3]]))
  top4 <- c(top4, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = TRUE)[4]]), temp$mean_prop[order(temp$mean_prop, decreasing = TRUE)[4]]))
  top5 <- c(top5, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = TRUE)[5]]), temp$mean_prop[order(temp$mean_prop, decreasing = TRUE)[5]]))
  bottom1 <- c(bottom1, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = FALSE)[1]]), temp$mean_prop[order(temp$mean_prop, decreasing = FALSE)[1]]))
  bottom2 <- c(bottom2, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = FALSE)[2]]), temp$mean_prop[order(temp$mean_prop, decreasing = FALSE)[2]]))
  bottom3 <- c(bottom3, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = FALSE)[3]]), temp$mean_prop[order(temp$mean_prop, decreasing = FALSE)[3]]))
  bottom4 <- c(bottom4, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = FALSE)[4]]), temp$mean_prop[order(temp$mean_prop, decreasing = FALSE)[4]]))
  bottom5 <- c(bottom5, paste0(as.character(temp$country[order(temp$mean_prop, decreasing = FALSE)[5]]), temp$mean_prop[order(temp$mean_prop, decreasing = FALSE)[5]]))
  symbol <- c(symbol, i)
}

country_rank <- data.frame(symbol, top1, top2, top3, top4, top5, bottom5, bottom4, bottom3, bottom2, bottom1)

write.csv(country_rank, "abstract_country_rank_all.csv", row.names = FALSE)


df_mean %>%
  filter(symbol == "surprised" & mean_prop == 0) %>%
  select(country)





# pickup one
cat5 <- c("!", "?", "!!", "??", "!?", "?!", cat2)
length(cat5) #1-14
selection <- cat5[1]

country_result_subsub <- country_result_sub %>%
  filter(symbol == selection)



ggplot(country_result_subsub, aes(x = country, y = as.numeric(or), group = auth, color = auth)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = as.numeric(lo), ymax = as.numeric(hi)), width = 0.2, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  scale_color_manual(values = c("gray20", "gray50")) +
  labs(title = selection, x = "", y = "log2(Odds ratio)") +
  theme(legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
  theme(legend.position = "right", legend.direction = "vertical") +
  guides(color = guide_legend(title = ""))






###### IF
cat1 <- c("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc")
cat2 <- c("surprised", "surprising", "shocked", "shocking", "exciting", "excited", "interested", "interesting")
cat3 <- c("spectacular", "wonderful", "awesome", "beautiful")
cat4 <- c("feminine", "masculine", "neutral")

highIF_result <- read.csv("title_highIF_result.csv")



# remove "title/abst_" from symbol
highIF_result$symbol <- str_replace(highIF_result$symbol, "title_", "")
highIF_result$symbol <- str_replace(highIF_result$symbol, "abst_", "")



# split
highIF_result_sub <- highIF_result %>%
  filter(symbol %in% cat1)

# change symbol ("sym_exc", "sym_excexc", "sym_que", "sym_queque", "sym_excque", "sym_queexc") to ("!", "!!", "?", "??", "!?", "?!")
highIF_result_sub$symbol <- str_replace(highIF_result_sub$symbol, "sym_excexc", "!!")
highIF_result_sub$symbol <- str_replace(highIF_result_sub$symbol, "sym_queque", "??")
highIF_result_sub$symbol <- str_replace(highIF_result_sub$symbol, "sym_excque", "!?")
highIF_result_sub$symbol <- str_replace(highIF_result_sub$symbol, "sym_queexc", "?!")
highIF_result_sub$symbol <- str_replace(highIF_result_sub$symbol, "sym_exc", "!")
highIF_result_sub$symbol <- str_replace(highIF_result_sub$symbol, "sym_que", "?")


### visualization
highIF_result_sub <- highIF_result_sub %>%
  filter(highIF == 1)

# order of symbol by or
highIF_result_sub$symbol <- as.factor(highIF_result_sub$symbol)
unique(highIF_result_sub$symbol)
highIF_result_sub <- highIF_result_sub %>%
  mutate(category = fct_reorder(symbol, or))
unique(highIF_result_sub$category)

# graph. or on x-axis (95%CI lo, hi), symbol on y-axis

dodge_width <- -0.75

ggplot(highIF_result_sub, aes(x = or, y = category, xmin = lo, xmax = hi)) +
  geom_point(size = 5, position = position_dodge(width = dodge_width)) +  # Apply dodge here
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, position = position_dodge(width = dodge_width), size = 1) +  # And here
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_minimal() +
  labs(titlif_exce = "", x = "                       Low impact  <-  Odds ratio  ->  High impact", y = "") +
  # labs(if_exctitle = "", x = "Low impact  <-  Odds ratio  ->  High impact               ", y = "") +
  # scale_color_manual(values = c("Vaccine development possible" = "lightblue", "100 days mission achievable" = "pink"),
  #                    breaks = c("Vaccine development possible", "100 days mission achievable")) +
  
  # coord_flip() +
  scale_y_discrete(limits = rev) +
  
  scale_x_continuous(
    limits = c(0, NA),
    breaks = seq(0, 3.4, by = 0.5)
  ) +
  theme(legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
  theme(legend.position = "top", legend.direction = "vertical") +
  guides(color = guide_legend(title = ""))



# 700 x 700


