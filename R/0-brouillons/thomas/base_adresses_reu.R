

length(unique(adresses_2022_reu %>% 
  collect() %>% 
  filter(id_brut_bv_reu %in% bv_sept2022_reu_7$ID_REU) %>% 
  pull(id_brut_bv_reu)))

unique(setdiff(bv_sept2022_reu_7$ID_REU, adresses_2022_reu$id_brut_bv_reu))

filosofi_2019_200m <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "projet-ensai-methodo-3a/sources/filosofi_2019_200m.csv",
    bucket = "thomasguinhut",
    opts = list("region" = ""),
    show_col_types = FALSE
  )



legi <-
  aws.s3::s3read_using(
    FUN = read_csv2,
    object = "/resultats-definitifs-par-bureau-de-vote.csv",
    bucket = "projet-ensai-methodo-3a",
    opts = list("region" = "")
  )
