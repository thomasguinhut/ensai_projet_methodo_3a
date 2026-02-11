bv_2022_final %>% dplyr::select(TIRABLE, DENS3_LIB,
                                starts_with(c("PROP_IND", "PROP_LOG",
                                              "PROP_MEN", "MOY")),
                                CLUSTER_AFM_DENSITE_FILOSOFI) %>%
  filter(TIRABLE) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI)


bv_2022_final %>% dplyr::select(TIRABLE, DENS3_LIB,
                                starts_with(c("PROP", "MOY")),
                                CLUSTER_AFM_DENSITE_FILOSOFI_2017) %>%
  filter(TIRABLE) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI_2017)
