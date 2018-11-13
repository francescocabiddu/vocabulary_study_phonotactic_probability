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
assign_subtlex_us_pp <- function(df) {
  df %>%
    mutate(ph_pr = sapply(phon, function(x) {
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
ph_pr_qu_subtlex_us <- subtlex_us_pp %>%
  filter(Input %in% (mot_uni_on_subtlex_us$phon %>% 
                       unlist() %>%
                       str_replace_all("_", ".") %>%
                       str_replace_all("UU", "AH") %>%
                       c(chi_uni_on_subtlex_us$phon %>% 
                           unlist() %>%
                           str_replace_all("_", ".") %>%
                           str_replace_all("UU", "AH"))
                     )) %>%
  .$unsLBPAV %>%
  quantile(na.rm = TRUE) %>%
  .[c(2,3,4)]

quartiles_sets <- function(df) {
  df %<>%
    mutate(ph_pr_qu1 = sapply(ph_pr, function(x) {
      length(x[which(x < ph_pr_qu_subtlex_us[1])])
    }),
    ph_pr_qu2 = sapply(ph_pr, function(x) {
      length(x[which(x >= ph_pr_qu_subtlex_us[1] & x < ph_pr_qu_subtlex_us[2])])
    }),
    ph_pr_qu3 = sapply(ph_pr, function(x) {
      length(x[which(x >= ph_pr_qu_subtlex_us[2] & x < ph_pr_qu_subtlex_us[3])])
    }),
    ph_pr_qu4 = sapply(ph_pr, function(x) {
      length(x[which(x >= ph_pr_qu_subtlex_us[3])])
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
