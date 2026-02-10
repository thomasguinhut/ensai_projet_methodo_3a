bv_2022_final %>% dplyr::select(TIRABLE, DENS3_LIB,
                                starts_with(c("IND", "LOG", "MEN")),
                                CLUSTER_AFM_DENSITE_FILOSOFI) %>%
  filter(TIRABLE) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI)

bv_2022_final %>% dplyr::select(TIRABLE, DENS3_LIB,
                                starts_with(c("IND", "LOG", "MEN", "PROP")),
                                ends_with("2017_T1"),
                                CLUSTER_AFM_DENSITE_FILOSOFI_2017) %>%
  filter(TIRABLE) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI_2017)
