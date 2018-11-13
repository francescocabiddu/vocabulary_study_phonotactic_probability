# load libraries
lib <- c("magrittr", "tidyverse", "janitor")
lapply(lib, require, character.only = TRUE)
rm(lib)

#### log pp - main ####
# save words to load on iphod calculator
c("chi_uni_on", "mot_uni_on", "mod_on") %>%
  sapply(function(x) {
    get(x) %>%
      .$phon %>%
      unlist()
  }) %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  str_extract(".*_.*") %>%
  .[!is.na(.)] %>%
  str_replace_all("_", ".") %>%
  str_replace_all("UU", "AH") %>%
  .[-which(. == "D.")] %>%
  write_lines(path = "phoneme_words.txt")

# load words given to iphod
subtlex_us_pp <- "phoneme_words_logpp.txt" %>%
  read_tsv() %>%
  .[!duplicated(.$Input), ]

# assign subtlex_us pp df_on (v2)
assign_subtlex_us_pp <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(ph_pr = sapply(!!var, function(x) {
      x %>%
        str_replace_all("_", ".") %>%
        str_replace_all("UU", "AH") %>%
        (function(y) {
          tibble(Input = y) %>%
            left_join(., subtlex_us_pp, "Input") %>%
            .$unsLBPAV
        })
    }))
}

chi_uni_on_subtlex_us <- assign_subtlex_us_pp(chi_uni_on)
mod_on_subtlex_us <- assign_subtlex_us_pp(mod_on)
mot_uni_on_subtlex_us <- assign_subtlex_us_pp(mot_uni_on)

# recalculate parcentages in quartiles
pp_qu_subtlex_us <- function(df1_var, df2_var) {
  subtlex_us_pp %>%
    filter(Input %in% (df1_var %>% 
                         unlist() %>%
                         str_replace_all("_", ".") %>%
                         str_replace_all("UU", "AH") %>%
                         c(df2_var %>% 
                             unlist() %>%
                             str_replace_all("_", ".") %>%
                             str_replace_all("UU", "AH"))
    )) %>%
    .$unsLBPAV %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

ph_pr_qu_subtlex_us <- pp_qu_subtlex_us(mot_uni_on_subtlex_us$phon, chi_uni_on_subtlex_us$phon)

quartiles_sets <- function(df, pp_qu = ph_pr_qu_subtlex_us) {
  df %<>%
    mutate(ph_pr_qu1 = sapply(ph_pr, function(x) {
      length(x[which(x < pp_qu[1])])
    }),
    ph_pr_qu2 = sapply(ph_pr, function(x) {
      length(x[which(x >= pp_qu[1] & x < pp_qu[2])])
    }),
    ph_pr_qu3 = sapply(ph_pr, function(x) {
      length(x[which(x >= pp_qu[2] & x < pp_qu[3])])
    }),
    ph_pr_qu4 = sapply(ph_pr, function(x) {
      length(x[which(x >= pp_qu[3])])
    })
    ) %>%
    group_by(baby) %>%
    mutate(ph_pr_qu1_cum = c(cumsum(ph_pr_qu1)),
           ph_pr_qu2_cum = c(cumsum(ph_pr_qu2)),
           ph_pr_qu3_cum = c(cumsum(ph_pr_qu3)),
           ph_pr_qu4_cum = c(cumsum(ph_pr_qu4))) %>%
    select(baby:ph_pr_qu4_cum) %>%
    (function(x) {
      percentages <- x %>%
        select(ph_pr_qu1_cum:ph_pr_qu4_cum) %>%
        adorn_percentages() %>%
        .[,-1] %>%
        (function(y) {
          colnames(y) <- gsub("_cum$", "_perc", colnames(y))
          y
        }) %>%
        mutate(ph_pr_qu1_perc = ph_pr_qu1_perc*100,
               ph_pr_qu2_perc = ph_pr_qu2_perc*100,
               ph_pr_qu3_perc = ph_pr_qu3_perc*100,
               ph_pr_qu4_perc = ph_pr_qu4_perc*100)
      
      x %>%
        ungroup() %>%
        cbind(percentages)
    })
  
}

chi_uni_on_subtlex_us <- quartiles_sets(chi_uni_on_subtlex_us)
mot_uni_on_subtlex_us <- quartiles_sets(mot_uni_on_subtlex_us)
mod_on_subtlex_us <- quartiles_sets(mod_on_subtlex_us)

#### log pp - phonemic lengths ####
log_pp_lengths <- list()

# quartiles on 2:6 phonemes
for (df in c("chi_uni_len2", "chi_uni_len3", "chi_uni_len4", "chi_uni_len5", "chi_uni_len6",
             "mod_len2", "mod_len3", "mod_len4", "mod_len5", "mod_len6")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len23456$phon_plu, 
                                            chi_uni_len23456$phon_plu))
} ; rm(df)

# quartiles on each phonemic length
for (df in c("chi_uni_len2b",
             "mod_len2b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len2$phon_plu, 
                                            chi_uni_len2$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len3b",
             "mod_len3b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len3$phon_plu, 
                                            chi_uni_len3$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len4b",
             "mod_len4b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len4$phon_plu, 
                                            chi_uni_len4$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len5b",
             "mod_len5b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len5$phon_plu, 
                                            chi_uni_len5$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len6b",
             "mod_len6b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len6$phon_plu, 
                                            chi_uni_len6$phon_plu))
} ; rm(df)
