bv_2022_final %>% dplyr::select(TIRABLE, DENS3_LIB, FERMETURE,
                                starts_with(c("PROP_IND", "PROP_LOG",
                                              "PROP_MEN", "MOY")),
                                CLUSTER_AFM_DENSITE_FILOSOFI) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI)


bv_2022_final %>% dplyr::select(TIRABLE, DENS3_LIB, FERMETURE,
                                starts_with(c("PROP", "MOY")),
                                CLUSTER_AFM_DENSITE_FILOSOFI_2017) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI_2017)
