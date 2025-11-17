adresses_2022_reu <- arrow::open_dataset(
  sources = "s3://projet-ensai-methodo-3a/sources/adresses_2022_reu.parquet",
  format = "parquet",
  filesystem = "s3"
)

adresses_2022_reu$schema

names(adresses_2022_reu)

adresses_2022_reu %>% 
  filter(code_commune_ref == "07230") %>% 
  collect()

filosofi_2019_200m <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ",",
    object = "projet-ensai-methodo-3a/sources/filosofi_2019_200m.csv",
    bucket = "thomasguinhut",
    opts = list("region" = ""),
    show_col_types = FALSE
  )
