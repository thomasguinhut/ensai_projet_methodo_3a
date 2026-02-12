base_sondage %>% dplyr::select(TIRABLE, DENS3_LIB, FERMETURE,
                                starts_with(c("PROP_IND", "PROP_LOG",
                                              "PROP_MEN", "MOY")),
                                CLUSTER_AFM_DENSITE_FILOSOFI) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI)


base_sondage %>% dplyr::select(TIRABLE, DENS3_LIB, FERMETURE,
                                starts_with(c("PROP", "MOY")),
                                CLUSTER_AFM_DENSITE_FILOSOFI_2017_5) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_FILOSOFI_2017_5)
