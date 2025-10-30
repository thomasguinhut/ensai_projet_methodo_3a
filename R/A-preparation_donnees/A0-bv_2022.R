bv_2022 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_methodo_3a/bv_2022_4.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

aws.s3::s3write_using(
  bv_2022,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_methodo_3a/bv_2022.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

rm(list = setdiff(ls(), "bv_2022"))
