base_sondage %>% dplyr::select(TIRABLE, DENS3_LIB, FERMETURE,
                                starts_with(c("PROP_IND", "PROP_LOG",
                                              "PROP_MEN", "MOY")),
                                CLUSTER_AFM_DENSITE_2017_6) %>%
  tbl_summary(by = CLUSTER_AFM_DENSITE_2017_6)


base_sondage %>% dplyr::select(TIRABLE, DENS3_LIB, FERMETURE,
                                starts_with(c("PROP", "MOY")),
                               CLUSTER_AFM_REG_DENSITE_FILOSOFI_2017_3) %>%
  tbl_summary(by = CLUSTER_AFM_REG_DENSITE_FILOSOFI_2017_3)


