# Load the mating type tables to be tested

mating_type_tables <- set_up_mating_type_tables(
  parameters,
  cores = 1,
  overwrite = FALSE)[[2]] %>%
  lapply(function(x) mutate(x, mating_type = as.integer(as.factor(parents))))


nice_melt <- function(table_output){
  data.frame(allele = names(table_output),
             n = as.numeric(table_output), stringsAsFactors = FALSE) %>%
    arrange(allele)
}

make_test_info <- function(mating_type_table){
  sex_chr1_mother <- substr(mating_type_table$mother, 1, 2)
  sex_chr2_mother <- substr(mating_type_table$mother, 3, 4)
  sex_chr1_father <- substr(mating_type_table$father, 1, 2)
  sex_chr2_father <- substr(mating_type_table$father, 3, 4)
  sex_chr1_zygote <- substr(mating_type_table$zygote, 1, 2)
  sex_chr2_zygote <- substr(mating_type_table$zygote, 3, 4)
  autosomes_mother <- strsplit(substr(mating_type_table$mother, 5, 8), split = "")
  autosomes_father <- strsplit(substr(mating_type_table$father, 5, 8), split = "")
  autosomes_zygote <- strsplit(substr(mating_type_table$zygote, 5, 8), split = "")

  alleles_parents <- pmap(list(sex_chr1_mother,
                               sex_chr2_mother,
                               sex_chr1_father,
                               sex_chr2_father,
                               autosomes_mother,
                               autosomes_father),
                          function(a,b,c,d,e,f) c(a,b,c,d,e,f))


  alleles_zygote <- pmap(list(sex_chr1_zygote,
                              sex_chr2_zygote,
                              autosomes_zygote),
                         function(a,b,c) c(a,b,c))

  n_mating_types <- max(mating_type_table$mating_type)
  allele_freqs_in_parents <- vector(length = n_mating_types, mode = "list")
  allele_freqs_in_zygotes <- vector(length = n_mating_types, mode = "list")

  for(i in 1:n_mating_types){
    focal <- mating_type_table$mating_type == i
    parents <- alleles_parents[focal][[1]] %>% table() %>% nice_melt()
    zygotes <- alleles_zygote[focal] %>% unlist() %>% table() %>% nice_melt()

    allele_freqs_in_parents[[i]] <- parents %>% mutate(n = n / 4)

    # Change this line to use freqs instead of counting all zygotes classes equal
    allele_freqs_in_zygotes[[i]] <- zygotes %>% mutate(n = n / (sum(zygotes$n) / 3))
  }

  tibble(
    mating_type = 1:n_mating_types,
    allele_freqs_in_parents,
    allele_freqs_in_zygotes
  )
}


test_for_novel_or_missing_alleles_in_progeny <- function(test_info, mating_type_table){
  ids <- mating_type_table %>% select(mating_type, parents) %>% distinct()
  n <- nrow(test_info)
  output <- tibble(mating_type = 1:n,
                   novel_alleles = NA,
                   missing_alleles = NA) %>%
    left_join(ids, by = "mating_type")
  for(i in 1:n){
    parent_alleles <- test_info[[i,2]][,1]
    zygote_alleles <- test_info[[i,3]][,1]
    novel <- zygote_alleles[!(zygote_alleles %in% parent_alleles)]
    missing <- parent_alleles[!(parent_alleles %in% zygote_alleles)]

    if(length(novel) != 0) output$novel_alleles[i] <- paste0(novel, collapse = "_")
    if(length(missing) != 0) output$missing_alleles[i] <- paste0(missing, collapse = "_")
  }
  output <- output %>%
    filter((!is.na(novel_alleles)) | !is.na(missing_alleles)) %>%
    filter(!(novel_alleles == "Zr" & str_detect(parents, "Z*Z[+]")))
  if(nrow(output) == 0) return("No missing or unexpected alleles in progeny")
  return(output)
}


test_for_changes_in_allele_freqs <- function(mating_type_table, test_info){

  ids <- mating_type_table %>% select(mating_type, parents) %>% distinct()

  is_identical <- sapply(1:nrow(test_info),
                         function(i) identical(test_info$allele_freqs_in_parents[i],
                                               test_info$allele_freqs_in_zygotes[i]))

  # Remove those where freqs are identical in offspring, or where
  # The number of genotypes is different (i.e. Zr creation)
  # Check this later, Zr creation does preclude other problems
  test_info <- test_info[which(!is_identical), ]

  test_info %>%
    filter(sapply(test_info$allele_freqs_in_parents, nrow) ==
             sapply(test_info$allele_freqs_in_zygotes, nrow))  %>%
    left_join(ids, by = "mating_type") %>%
    filter(!str_detect(parents, "Z*Z[+]"))

}
xx <- make_test_info(mating_type_tables[[1]])
yy <- test_for_changes_in_allele_freqs(mating_type_tables[[1]], xx)
yy[[1,2]]; yy[[1,3]]

run_tests <- function(mating_type_table){
  test_info <- make_test_info(mating_type_table)
  output <- list(test_for_novel_alleles_in_progeny(mating_type_table, test_info),
       test_for_missing_alleles_in_progeny(mating_type_table, test_info),
       test_for_changes_in_allele_freqs(mating_type_table, test_info))
  output
}



run_tests(mating_type_tables[[1]])

#lapply(mating_type_tables, run_tests)
