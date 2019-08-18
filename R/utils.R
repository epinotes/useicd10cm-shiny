
usethis::use_package("shiny")
usethis::use_package("readr")
usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("magrittr")

s_new_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {
  
  requireNamespace("dplyr", quietly = T)
  requireNamespace("purrr", quietly = T)
  
  
  colvec <- enquo(colvec)
  # assign '1' if the regular expression matched
  f1 <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)
  # any 1 in the diagnosis field suffices
  f2 <- function(x){
    sign(rowSums(x, na.rm = TRUE))
  }
  
  data %>% as_tibble() %>%
    select(!!colvec) %>%
    mutate_all(as.character) %>%
    purrr::map_dfr(f1) %>%
    transmute(new_diag = f2(.)) %>%
    flatten_dbl()
}


##
s_drug_opioid <- function(data, diag_ecode_col) {
  
  requireNamespace("dplyr", quietly = T)
  
  
  drugs_icd10cm_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"
  
  opioid_icd10cm_ <- "(T40[0-4].|T406[09])[1-4](A|$)"
  
  non_heroin_opioid_icd10cm_ <- "(T40[0234].|T406[09])[1-4](A|$)"
  
  heroin_icd10cm_ <- "T401.[1-4](A|$)"
  
  stimulant_icd10cm_ <- "((T405.|T436[0-49])[1-4])(A|$)"
  
  cocaine_icd10cm_ <- "T405.[1-4](A|$)"
  
  non_cocaine_stimulant_icd10cm_ <- "436[0-49][1-4](A|$)"
  
  
  data %>%
    mutate(
      any_drug = s_new_diag(.,
                            expr = drugs_icd10cm_,
                            colvec = diag_ecode_col
      ),
      
      any_opioid = s_new_diag(.,
                              expr = opioid_icd10cm_,
                              colvec = diag_ecode_col
      ),
      
      non_heroin_opioid = s_new_diag(.,
                                     expr = non_heroin_opioid_icd10cm_,
                                     colvec = diag_ecode_col
      ),
      
      heroin = s_new_diag(.,
                          expr = heroin_icd10cm_,
                          colvec = diag_ecode_col
      ),
      
      stimulant = s_new_diag(.,
                             expr = stimulant_icd10cm_,
                             colvec = diag_ecode_col
      ),
      cocaine = s_new_diag(.,
                           expr = cocaine_icd10cm_,
                           colvec = diag_ecode_col
      ),
      non_cocaine_stimulant = s_new_diag(.,
                                         expr = non_cocaine_stimulant_icd10cm_,
                                         colvec = diag_ecode_col
      )
    ) %>%
    mutate(
      non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid),
      non_cocaine_stimulant = ifelse(cocaine == 1, 0, non_cocaine_stimulant)
    )
}