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

quartiles_pp_sets <- function(df, pp_qu = ph_pr_qu_subtlex_us) {
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

chi_uni_on_subtlex_us <- quartiles_pp_sets(chi_uni_on_subtlex_us)
mot_uni_on_subtlex_us <- quartiles_pp_sets(mot_uni_on_subtlex_us)
mod_on_subtlex_us <- quartiles_pp_sets(mod_on_subtlex_us)

#### log pp - phonemic lengths ####
log_pp_lengths <- list()

# quartiles on 2:6 phonemes
for (df in c("chi_uni_len2", "chi_uni_len3", "chi_uni_len4", "chi_uni_len5", "chi_uni_len6",
             "mod_len2", "mod_len3", "mod_len4", "mod_len5", "mod_len6")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_pp_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len23456$phon_plu, 
                                            chi_uni_len23456$phon_plu))
} ; rm(df)

# quartiles on each phonemic length
for (df in c("chi_uni_len2b",
             "mod_len2b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_pp_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len2$phon_plu, 
                                            chi_uni_len2$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len3b",
             "mod_len3b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_pp_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len3$phon_plu, 
                                            chi_uni_len3$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len4b",
             "mod_len4b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_pp_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len4$phon_plu, 
                                            chi_uni_len4$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len5b",
             "mod_len5b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_pp_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len5$phon_plu, 
                                            chi_uni_len5$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len6b",
             "mod_len6b")) {
  log_pp_lengths[[df]] <- df %>%
    get() %>%
    assign_subtlex_us_pp(var = phon_plu) %>%
    quartiles_pp_sets(pp_qu = pp_qu_subtlex_us(mot_uni_len6$phon_plu, 
                                            chi_uni_len6$phon_plu))
} ; rm(df)

#### General frequency - main ####
#### assign iphod frequency to datasets
iphod_freq <- iphod %>%
  group_by(UnTrn) %>%
  summarise(SFreq = sum(SFreq)) %>% 
  ungroup()

assign_freq <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(freq = sapply(!!var, function(x) {
      x %>%
        str_replace_all("_", ".") %>%
        str_replace_all("UU", "AH") %>%
        (function(y) {
          tibble(UnTrn = y) %>%
            left_join(., iphod_freq, "UnTrn") %>%
            .$SFreq
        })
    }))
}

chi_uni_on_freq <- assign_freq(chi_uni_on) %>%
  select(baby:phon, freq)
mot_uni_on_freq <- assign_freq(mot_uni_on) %>%
  select(baby:phon, freq)
mod_on_freq <- assign_freq(mod_on) %>%
  select(baby:phon, freq)

# calculate parcentages in quartiles
freq_qu_fun <- function(df1_var, df2_var) {
  iphod_freq %>%
    filter(UnTrn %in% (df1_var %>% 
                         unlist() %>%
                         str_replace_all("_", ".") %>%
                         str_replace_all("UU", "AH") %>%
                         c(df2_var %>% 
                             unlist() %>%
                             str_replace_all("_", ".") %>%
                             str_replace_all("UU", "AH"))
    )) %>%
    .$SFreq %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

freq_qus <- freq_qu_fun(mot_uni_on_freq$phon, chi_uni_on_freq$phon)

quartiles_freq_sets <- function(df, freq_qu = freq_qus) {
  df %<>%
    mutate(freq_qu1 = sapply(freq, function(x) {
      length(x[which(x < freq_qu[1])])
    }),
    freq_qu2 = sapply(freq, function(x) {
      length(x[which(x >= freq_qu[1] & x < freq_qu[2])])
    }),
    freq_qu3 = sapply(freq, function(x) {
      length(x[which(x >= freq_qu[2] & x < freq_qu[3])])
    }),
    freq_qu4 = sapply(freq, function(x) {
      length(x[which(x >= freq_qu[3])])
    })
    ) %>%
    group_by(baby) %>%
    mutate(freq_qu1_cum = c(cumsum(freq_qu1)),
           freq_qu2_cum = c(cumsum(freq_qu2)),
           freq_qu3_cum = c(cumsum(freq_qu3)),
           freq_qu4_cum = c(cumsum(freq_qu4))) %>%
    select(baby:freq_qu4_cum) %>%
    (function(x) {
      percentages <- x %>%
        select(freq_qu1_cum:freq_qu4_cum) %>%
        adorn_percentages() %>%
        .[,-1] %>%
        (function(y) {
          colnames(y) <- gsub("_cum$", "_perc", colnames(y))
          y
        }) %>%
        mutate(freq_qu1_perc = freq_qu1_perc*100,
               freq_qu2_perc = freq_qu2_perc*100,
               freq_qu3_perc = freq_qu3_perc*100,
               freq_qu4_perc = freq_qu4_perc*100)
      
      x %>%
        ungroup() %>%
        cbind(percentages)
    })
  
}

chi_uni_on_freq <- quartiles_freq_sets(chi_uni_on_freq)
mot_uni_on_freq <- quartiles_freq_sets(mot_uni_on_freq)
mod_on_freq <- quartiles_freq_sets(mod_on_freq)

#### General frequency - phonemic lengths ####
freq_lengths <- list()

# quartiles on 2:6 phonemes
for (df in c("chi_uni_len2", "chi_uni_len3", "chi_uni_len4", "chi_uni_len5", "chi_uni_len6",
             "mod_len2", "mod_len3", "mod_len4", "mod_len5", "mod_len6")) {
  freq_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len23456$phon_plu, 
                                               chi_uni_len23456$phon_plu))
} ; rm(df)

# quartiles on each phonemic length
for (df in c("chi_uni_len2b",
             "mod_len2b")) {
  freq_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len2$phon_plu, 
                                               chi_uni_len2$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len3b",
             "mod_len3b")) {
  freq_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len3$phon_plu, 
                                               chi_uni_len3$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len4b",
             "mod_len4b")) {
  freq_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len4$phon_plu, 
                                               chi_uni_len4$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len5b",
             "mod_len5b")) {
  freq_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len5$phon_plu, 
                                               chi_uni_len5$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len6b",
             "mod_len6b")) {
  freq_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len6$phon_plu, 
                                               chi_uni_len6$phon_plu))
} ; rm(df)

#### Maternal frequency - main ####
mot_freq <- "mot_tokens.txt" %>%
  read_tsv() %>%
  rename(UnTrn = phon) %>%
  group_by(UnTrn) %>%
  summarise(SFreq = n()) %>%
  ungroup()

assign_freq <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(freq = sapply(!!var, function(x) {
      x %>%
        (function(y) {
          tibble(UnTrn = y) %>%
            left_join(., mot_freq, "UnTrn") %>%
            .$SFreq
        })
    }))
}

chi_uni_on_mot_freq <- assign_freq(chi_uni_on) %>%
  select(baby:phon, freq)
mot_uni_on_mot_freq <- assign_freq(mot_uni_on) %>%
  select(baby:phon, freq)
mod_on_mot_freq <- assign_freq(mod_on) %>%
  select(baby:phon, freq)

# calculate parcentages in quartiles
freq_qu_fun <- function(df1_var, df2_var) {
  mot_freq %>%
    filter(UnTrn %in% (df1_var %>% 
                         unlist() %>%
                         c(df2_var %>% 
                             unlist())
    )) %>%
    .$SFreq %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

freq_qus <- freq_qu_fun(mot_uni_on_mot_freq$phon, chi_uni_on_mot_freq$phon)

quartiles_freq_sets <- function(df, freq_qu = freq_qus) {
  df %<>%
    mutate(freq_qu1 = sapply(freq, function(x) {
      length(x[which(x < freq_qu[1])])
    }),
    freq_qu2 = sapply(freq, function(x) {
      length(x[which(x >= freq_qu[1] & x < freq_qu[2])])
    }),
    freq_qu3 = sapply(freq, function(x) {
      length(x[which(x >= freq_qu[2] & x < freq_qu[3])])
    }),
    freq_qu4 = sapply(freq, function(x) {
      length(x[which(x >= freq_qu[3])])
    })
    ) %>%
    group_by(baby) %>%
    mutate(freq_qu1_cum = c(cumsum(freq_qu1)),
           freq_qu2_cum = c(cumsum(freq_qu2)),
           freq_qu3_cum = c(cumsum(freq_qu3)),
           freq_qu4_cum = c(cumsum(freq_qu4))) %>%
    select(baby:freq_qu4_cum) %>%
    (function(x) {
      percentages <- x %>%
        select(freq_qu1_cum:freq_qu4_cum) %>%
        adorn_percentages() %>%
        .[,-1] %>%
        (function(y) {
          colnames(y) <- gsub("_cum$", "_perc", colnames(y))
          y
        }) %>%
        mutate(freq_qu1_perc = freq_qu1_perc*100,
               freq_qu2_perc = freq_qu2_perc*100,
               freq_qu3_perc = freq_qu3_perc*100,
               freq_qu4_perc = freq_qu4_perc*100)
      
      x %>%
        ungroup() %>%
        cbind(percentages)
    })
  
}

chi_uni_on_mot_freq <- quartiles_freq_sets(chi_uni_on_mot_freq)
mot_uni_on_mot_freq <- quartiles_freq_sets(mot_uni_on_mot_freq)
mod_on_mot_freq <- quartiles_freq_sets(mod_on_mot_freq)

#### Maternal frequency - phonemic lengths ####
freq_mot_lengths <- list()

# quartiles on 2:6 phonemes
for (df in c("chi_uni_len2", "chi_uni_len3", "chi_uni_len4", "chi_uni_len5", "chi_uni_len6",
             "mod_len2", "mod_len3", "mod_len4", "mod_len5", "mod_len6")) {
  freq_mot_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len23456$phon_plu, 
                                              chi_uni_len23456$phon_plu))
} ; rm(df)

# quartiles on each phonemic length
for (df in c("chi_uni_len2b",
             "mod_len2b")) {
  freq_mot_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len2$phon_plu, 
                                              chi_uni_len2$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len3b",
             "mod_len3b")) {
  freq_mot_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len3$phon_plu, 
                                              chi_uni_len3$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len4b",
             "mod_len4b")) {
  freq_mot_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len4$phon_plu, 
                                              chi_uni_len4$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len5b",
             "mod_len5b")) {
  freq_mot_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len5$phon_plu, 
                                              chi_uni_len5$phon_plu))
} ; rm(df)

for (df in c("chi_uni_len6b",
             "mod_len6b")) {
  freq_mot_lengths[[df]] <- df %>%
    get() %>%
    assign_freq(var = phon_plu) %>%
    quartiles_freq_sets(freq_qu = freq_qu_fun(mot_uni_len6$phon_plu, 
                                              chi_uni_len6$phon_plu))
} ; rm(df)


#### ND by general frequency ####
assign_freq <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(freq = sapply(!!var, function(x) {
      x %>%
        str_replace_all("_", ".") %>%
        str_replace_all("UU", "AH") %>%
        (function(y) {
          tibble(UnTrn = y) %>%
            left_join(., iphod_freq, "UnTrn") %>%
            .$SFreq
        })
    }))
}

freq_qu_fun <- function(df1_var, df2_var) {
  iphod_freq %>%
    filter(UnTrn %in% (df1_var %>% 
                         unlist() %>%
                         str_replace_all("_", ".") %>%
                         str_replace_all("UU", "AH") %>%
                         c(df2_var %>% 
                             unlist() %>%
                             str_replace_all("_", ".") %>%
                             str_replace_all("UU", "AH"))
    )) %>%
    .$SFreq %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

freq_qus <- freq_qu_fun(mot_nei_on$phon, chi_nei_on$phon)

select_freq <- function(df, quartile) {
  prova <- df %>%
    select(baby:nei) %>%
    assign_freq() %>%
    mutate(freq_q1 = sapply(freq, function(x) {
      y <- x < freq_qus[1]
      y[is.na(y)] <- FALSE
      y
    }),
    freq_q2 = sapply(freq, function(x) {
      y <- x >= freq_qus[1] & x < freq_qus[2]
      y[is.na(y)] <- FALSE
      y
    }),
    freq_q3 = sapply(freq, function(x) {
      y <- x >= freq_qus[2] & x < freq_qus[3]
      y[is.na(y)] <- FALSE
      y
    }),
    freq_q4 = sapply(freq, function(x) {
      y <- x >= freq_qus[3]
      y[is.na(y)] <- FALSE
      y
    }))
  
  for (i in seq_along(prova$baby)) {
    prova$phon[[i]] <- prova$phon[[i]][prova[[quartile]][[i]]]
    prova$nei[[i]] <- prova$nei[[i]][prova[[quartile]][[i]]]
    prova$freq[[i]] <- prova$freq[[i]][prova[[quartile]][[i]]]
  }
  
  prova %>%
    select(baby:nei)
} 

quartiles_nei_sets <- function(df, nei_qu = quantile(online$nei)[c(2,3,4)]) {
  df %<>%
    mutate(nei_qu1 = sapply(nei, function(x) {
      length(x[which(x < nei_qu[1])])
    }),
    nei_qu2 = sapply(nei, function(x) {
      length(x[which(x >= nei_qu[1] & x < nei_qu[2])])
    }),
    nei_qu3 = sapply(nei, function(x) {
      length(x[which(x >= nei_qu[2] & x < nei_qu[3])])
    }),
    nei_qu4 = sapply(nei, function(x) {
      length(x[which(x >= nei_qu[3])])
    })
    ) %>%
    group_by(baby) %>%
    mutate(nei_qu1_cum = c(cumsum(nei_qu1)),
           nei_qu2_cum = c(cumsum(nei_qu2)),
           nei_qu3_cum = c(cumsum(nei_qu3)),
           nei_qu4_cum = c(cumsum(nei_qu4))) %>%
    select(baby:nei_qu4_cum) %>%
    (function(x) {
      percentages <- x %>%
        select(nei_qu1_cum:nei_qu4_cum) %>%
        adorn_percentages() %>%
        .[,-1] %>%
        (function(y) {
          colnames(y) <- gsub("_cum$", "_perc", colnames(y))
          y
        }) %>%
        mutate(nei_qu1_perc = nei_qu1_perc*100,
               nei_qu2_perc = nei_qu2_perc*100,
               nei_qu3_perc = nei_qu3_perc*100,
               nei_qu4_perc = nei_qu4_perc*100)
      
      x %>%
        ungroup() %>%
        cbind(percentages)
    })
  
}

nei_freqs <- list()
nei_freqs[["chi_nei_freq1"]] <- select_freq(chi_nei_on, "freq_q1") %>%
  quartiles_nei_sets()
nei_freqs[["chi_nei_freq2"]] <- select_freq(chi_nei_on, "freq_q2") %>%
  quartiles_nei_sets()
nei_freqs[["chi_nei_freq3"]] <- select_freq(chi_nei_on, "freq_q3") %>%
  quartiles_nei_sets()
nei_freqs[["chi_nei_freq4"]] <- select_freq(chi_nei_on, "freq_q4") %>%
  quartiles_nei_sets()
nei_freqs[["mot_nei_freq1"]] <- select_freq(mot_nei_on, "freq_q1") %>%
  quartiles_nei_sets()
nei_freqs[["mot_nei_freq2"]] <- select_freq(mot_nei_on, "freq_q2") %>%
  quartiles_nei_sets()
nei_freqs[["mot_nei_freq3"]] <- select_freq(mot_nei_on, "freq_q3") %>%
  quartiles_nei_sets()
nei_freqs[["mot_nei_freq4"]] <- select_freq(mot_nei_on, "freq_q4") %>%
  quartiles_nei_sets()
nei_freqs[["mod_nei_freq1"]] <- select_freq(mod_nei_on, "freq_q1") %>%
  quartiles_nei_sets()
nei_freqs[["mod_nei_freq2"]] <- select_freq(mod_nei_on, "freq_q2") %>%
  quartiles_nei_sets()
nei_freqs[["mod_nei_freq3"]] <- select_freq(mod_nei_on, "freq_q3") %>%
  quartiles_nei_sets()
nei_freqs[["mod_nei_freq4"]] <- select_freq(mod_nei_on, "freq_q4") %>%
  quartiles_nei_sets()

#### ND by maternal frequency ####
assign_freq <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(freq = sapply(!!var, function(x) {
      x %>%
        (function(y) {
          tibble(UnTrn = y) %>%
            left_join(., mot_freq, "UnTrn") %>%
            .$SFreq
        })
    }))
}

# calculate parcentages in quartiles
freq_qu_fun <- function(df1_var, df2_var) {
  mot_freq %>%
    filter(UnTrn %in% (df1_var %>% 
                         unlist() %>%
                         c(df2_var %>% 
                             unlist())
    )) %>%
    .$SFreq %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

freq_qus <- freq_qu_fun(mot_nei_on$phon, chi_nei_on$phon)

nei_mot_freqs <- list()
nei_mot_freqs[["chi_nei_freq1"]] <- select_freq(chi_nei_on, "freq_q1") %>%
  quartiles_nei_sets()
nei_mot_freqs[["chi_nei_freq2"]] <- select_freq(chi_nei_on, "freq_q2") %>%
  quartiles_nei_sets()
nei_mot_freqs[["chi_nei_freq3"]] <- select_freq(chi_nei_on, "freq_q3") %>%
  quartiles_nei_sets()
nei_mot_freqs[["chi_nei_freq4"]] <- select_freq(chi_nei_on, "freq_q4") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mot_nei_freq1"]] <- select_freq(mot_nei_on, "freq_q1") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mot_nei_freq2"]] <- select_freq(mot_nei_on, "freq_q2") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mot_nei_freq3"]] <- select_freq(mot_nei_on, "freq_q3") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mot_nei_freq4"]] <- select_freq(mot_nei_on, "freq_q4") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mod_nei_freq1"]] <- select_freq(mod_nei_on, "freq_q1") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mod_nei_freq2"]] <- select_freq(mod_nei_on, "freq_q2") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mod_nei_freq3"]] <- select_freq(mod_nei_on, "freq_q3") %>%
  quartiles_nei_sets()
nei_mot_freqs[["mod_nei_freq4"]] <- select_freq(mod_nei_on, "freq_q4") %>%
  quartiles_nei_sets()

#### LOG PP by general frequency ####
assign_freq <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(freq = sapply(!!var, function(x) {
      x %>%
        str_replace_all("_", ".") %>%
        str_replace_all("UU", "AH") %>%
        (function(y) {
          tibble(UnTrn = y) %>%
            left_join(., iphod_freq, "UnTrn") %>%
            .$SFreq
        })
    }))
}

freq_qu_fun <- function(df1_var, df2_var) {
  iphod_freq %>%
    filter(UnTrn %in% (df1_var %>% 
                         unlist() %>%
                         str_replace_all("_", ".") %>%
                         str_replace_all("UU", "AH") %>%
                         c(df2_var %>% 
                             unlist() %>%
                             str_replace_all("_", ".") %>%
                             str_replace_all("UU", "AH"))
    )) %>%
    .$SFreq %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

freq_qus <- freq_qu_fun(mot_uni_on_subtlex_us$phon, chi_uni_on_subtlex_us$phon)

select_freq <- function(df, quartile) {
  prova <- df %>%
    select(baby:ph_pr) %>%
    assign_freq() %>%
    mutate(freq_q1 = sapply(freq, function(x) {
      y <- x < freq_qus[1]
      y[is.na(y)] <- FALSE
      y
    }),
    freq_q2 = sapply(freq, function(x) {
      y <- x >= freq_qus[1] & x < freq_qus[2]
      y[is.na(y)] <- FALSE
      y
    }),
    freq_q3 = sapply(freq, function(x) {
      y <- x >= freq_qus[2] & x < freq_qus[3]
      y[is.na(y)] <- FALSE
      y
    }),
    freq_q4 = sapply(freq, function(x) {
      y <- x >= freq_qus[3]
      y[is.na(y)] <- FALSE
      y
    }))
  
  for (i in seq_along(prova$baby)) {
    prova$phon[[i]] <- prova$phon[[i]][prova[[quartile]][[i]]]
    prova$ph_pr[[i]] <- prova$ph_pr[[i]][prova[[quartile]][[i]]]
    prova$freq[[i]] <- prova$freq[[i]][prova[[quartile]][[i]]]
  }
  
  prova %>%
    select(baby:ph_pr)
} 

quartiles_ph_pr_sets <- function(df, ph_pr_qu = quantile(online$phon_prob, na.rm = TRUE)[c(2,3,4)]) {
  df %<>%
    mutate(ph_pr_qu1 = sapply(ph_pr, function(x) {
      length(x[which(x < ph_pr_qu[1])])
    }),
    ph_pr_qu2 = sapply(ph_pr, function(x) {
      length(x[which(x >= ph_pr_qu[1] & x < ph_pr_qu[2])])
    }),
    ph_pr_qu3 = sapply(ph_pr, function(x) {
      length(x[which(x >= ph_pr_qu[2] & x < ph_pr_qu[3])])
    }),
    ph_pr_qu4 = sapply(ph_pr, function(x) {
      length(x[which(x >= ph_pr_qu[3])])
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

pp_freqs <- list()
pp_freqs[["chi_pp_freq1"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q1") %>%
  quartiles_ph_pr_sets()
pp_freqs[["chi_pp_freq2"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q2") %>%
  quartiles_ph_pr_sets()
pp_freqs[["chi_pp_freq3"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q3") %>%
  quartiles_ph_pr_sets()
pp_freqs[["chi_pp_freq4"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q4") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mot_pp_freq1"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q1") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mot_pp_freq2"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q2") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mot_pp_freq3"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q3") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mot_pp_freq4"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q4") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mod_pp_freq1"]] <- select_freq(mod_on_subtlex_us, "freq_q1") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mod_pp_freq2"]] <- select_freq(mod_on_subtlex_us, "freq_q2") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mod_pp_freq3"]] <- select_freq(mod_on_subtlex_us, "freq_q3") %>%
  quartiles_ph_pr_sets()
pp_freqs[["mod_pp_freq4"]] <- select_freq(mod_on_subtlex_us, "freq_q4") %>%
  quartiles_ph_pr_sets()

#### LOG PP by maternal frequency ####
assign_freq <- function(df, var = phon) {
  var <- enquo(var)
  df %>%
    mutate(freq = sapply(!!var, function(x) {
      x %>%
        (function(y) {
          tibble(UnTrn = y) %>%
            left_join(., mot_freq, "UnTrn") %>%
            .$SFreq
        })
    }))
}

# calculate parcentages in quartiles
freq_qu_fun <- function(df1_var, df2_var) {
  mot_freq %>%
    filter(UnTrn %in% (df1_var %>% 
                         unlist() %>%
                         c(df2_var %>% 
                             unlist())
    )) %>%
    .$SFreq %>%
    quantile(na.rm = TRUE) %>%
    .[c(2,3,4)]
}

freq_qus <- freq_qu_fun(mot_uni_on_subtlex_us$phon, chi_uni_on_subtlex_us$phon)

pp_mot_freqs <- list()
pp_mot_freqs[["chi_pp_freq1"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q1") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["chi_pp_freq2"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q2") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["chi_pp_freq3"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q3") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["chi_pp_freq4"]] <- select_freq(chi_uni_on_subtlex_us, "freq_q4") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mot_pp_freq1"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q1") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mot_pp_freq2"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q2") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mot_pp_freq3"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q3") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mot_pp_freq4"]] <- select_freq(mot_uni_on_subtlex_us, "freq_q4") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mod_pp_freq1"]] <- select_freq(mod_on_subtlex_us, "freq_q1") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mod_pp_freq2"]] <- select_freq(mod_on_subtlex_us, "freq_q2") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mod_pp_freq3"]] <- select_freq(mod_on_subtlex_us, "freq_q3") %>%
  quartiles_ph_pr_sets()
pp_mot_freqs[["mod_pp_freq4"]] <- select_freq(mod_on_subtlex_us, "freq_q4") %>%
  quartiles_ph_pr_sets()
