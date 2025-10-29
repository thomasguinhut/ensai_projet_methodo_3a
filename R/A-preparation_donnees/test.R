table_bv_reu <-
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    delim = ";",
    object = "projet_methodo_3a/table-bv-reu.csv",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )