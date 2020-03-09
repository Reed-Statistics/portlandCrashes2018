library(tidyverse)
library(readxl)
library(naniar)
library(janitor)

# join excel crash and partic data sheets together
portland_crashes_2018 <- left_join(read_excel("data-raw/pdx_crash_2018.xls", 4), 
                       read_excel("data-raw/pdx_crash_2018.xls"), 
                       by = c("CRASH_ID" = "CRASH_ID"))


#### Selecting what variables we want from crash_dat
portland_crashes_2018 <- portland_crashes_2018 %>%
  select(CRASH_ID,
         PARTIC_ID,
         VHCL_ID,
         PARTIC_DSPLY_SEQ_NO,
         VHCL_CODED_SEQ_NO,
         PARTIC_TYP_SHORT_DESC,
         SEX_CD,
         AGE_VAL,
         DRVR_LIC_STAT_SHORT_DESC,
         INJ_SVRTY_SHORT_DESC,
         SFTY_EQUIP_USE_SHORT_DESC,
         AIRBAG_DEPLOY_IND,
         CRASH_MO_NO,
         CRASH_DAY_NO,
         CRASH_YR_NO,
         CRASH_WK_DAY_CD,
         LONGTD_DD, 
         LAT_DD, 
         PARTIC_HIT_RUN_FLG,
         RD_CHAR_SHORT_DESC,
         RD_CNTL_MED_DESC,
         CRASH_TYP_CD,
         COLLIS_TYP_SHORT_DESC,
         FROM_ISECT_DSTNC_QTY, 
         POST_SPEED_LMT_VAL, 
         CRASH_SVRTY_CD,
         CRASH_SVRTY_SHORT_DESC,
         WTHR_COND_SHORT_DESC,
         RD_SURF_SHORT_DESC,
         LGT_COND_SHORT_DESC,
         TRAF_CNTL_DEVICE_SHORT_DESC,
         
         BAC_VAL,
         ALCHL_USE_RPT_IND,
         DRUG_USE_RPT_IND,
         MJ_USE_RPT_IND,
         
         CRASH_SPEED_INVLV_FLG,
         TOT_VHCL_CNT,
         TOT_FATAL_CNT,
         TOT_INJ_CNT,
         TOT_PED_CNT,
         TOT_PEDCYCL_CNT,
         TOT_PER_INVLV_CNT
         ) %>%
  mutate(PARTIC_ERR_1_CD = case_when(
    PARTIC_ERR_1_CD == 000 ~ "no error",
    PARTIC_ERR_1_CD != 000 ~ "error"
  )) %>%
  mutate(SEX_CD = case_when(
    SEX_CD == 1 ~ "male",
    SEX_CD == 2 ~ "female",
    SEX_CD == 3 ~ "non-binary",
    SEX_CD == 9 ~ "unknown"
  )) %>% 
  arrange(CRASH_ID) %>%
  clean_names() %>%
  rename(partic_no = partic_dsply_seq_no,
          vhcl_no = vhcl_coded_seq_no,
          partic_type = partic_typ_short_desc,
          sex = sex_cd,
          age = age_val,
          drvr_lic_stat = drvr_lic_stat_short_desc,
          inj_svrty = inj_svrty_short_desc,
          sfty_equip_use = sfty_equip_use_short_desc,
          airbag_deploy = airbag_deploy_ind,
          longitude = longtd_dd,
          latitude = lat_dd,
          partic_hit_run = partic_hit_run_flg,
          rd_characteristic = rd_char_short_desc,
          rd_type = rd_cntl_med_desc,
          collision_type = collis_typ_short_desc,
          speed_limit = post_speed_lmt_val,
          crash_svrty = crash_svrty_short_desc,
          weather_cond = wthr_cond_short_desc,
          rd_surface_cond = rd_surf_short_desc,
          light_cond = lgt_cond_short_desc,
          traf_device = traf_cntl_device_short_desc,
          alcohol_use = alchl_use_rpt_ind,
          drug_use = drug_use_rpt_ind,
          mj_use = mj_use_rpt_ind,
          speed_invlv = crash_speed_invlv_flg,
          tot_bicycle_cnt = tot_pedcycl_cnt,
          crash_month = crash_mo_no,
          day_of_month = crash_day_no,
          day_of_week = crash_wk_day_cd,
          crash_year = crash_yr_no
         ) %>%
  select(-c(crash_svrty_cd, from_isect_dstnc_qty, crash_typ_cd)) %>%
  mutate(partic_type = tolower(partic_type),
         inj_svrty = tolower(inj_svrty),
         sfty_equip_use = tolower(sfty_equip_use),
         rd_characteristic = tolower(rd_characteristic),
         rd_type = tolower(rd_type),
         collision_type = tolower(collision_type),
         crash_svrty = tolower(crash_svrty),
         weather_cond = tolower(weather_cond),
         rd_surface_cond = tolower(rd_surface_cond),
         light_cond = tolower(light_cond),
         traf_device = tolower(traf_device)
         ) %>% 
  mutate(rd_characteristic = replace(rd_characteristic, 
                                     rd_characteristic == "inter",
                                     "intersection")) %>%
  mutate(rd_characteristic = replace(rd_characteristic, 
                                    rd_characteristic == "strght",
                                    "straight")) %>%
  mutate(rd_type = case_when(
    rd_type == "portland hwy system" ~ "highway system",
    rd_type == "portland city street" ~ "city street"
  )) %>%
  mutate(drvr_lic_stat = case_when(
    drvr_lic_stat == "N-VAL" ~ "other non-valid",
    drvr_lic_stat == "NONE" ~ "none",
    drvr_lic_stat == "OR-Y" ~ "OR",
    drvr_lic_stat == "OTH-Y" ~ "other state",
    drvr_lic_stat == "SUSP" ~ "suspended",
    drvr_lic_stat == "UNK" ~ "unknown"
  ))
 
usethis::use_data(portland_crashes_2018, overwrite = TRUE)
