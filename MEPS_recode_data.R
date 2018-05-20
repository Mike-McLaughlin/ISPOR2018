# This script cleans the MEPS data prior to fitting any models.
# Note that this program will rely on output from the `lowdown` package by Anthony Damico
# (See: http://asdfree.com/). It also requires fucntions from the dplyr package
# The result is an .RDS file called 'consol_fin' for 'consolidated final'
# Note that codebooks for the MEPS data are available online at the MEPS website: https://meps.ahrq.gov/mepsweb/

library(dplyr)

##Dataset -- MEPS
consol <- readRDS("~/Desktop/ISPOR2018/full year consolidated.rds")
consol <- consol %>% dplyr::select(dupersid, 
  #Demographics
  region15, age15x, sex, racev2x,racethx, marry15x, povlev15, 
  #Chronic conditions
  hibpdx, chddx, angidx, midx, ohrtdx,
  strkdx, emphdx, choldx, cancerdx, diabdx, arthdx, asthdx, adhdaddx,
  #Diabetes Care Survey Variables: 2015
  dsa1c53, dsft1553, dsey1553, dsch1553, dsfl1553, 
  dsdiet53, dsmed53, dsinsu53,
  #Health System Engagement: Do they like/care about their doctor?
  haveus42, treatm42, respct42,
  #Employment status
  empst31, empst42, empst53,
  #Insurance status: Uninsured and insurance type
  unins15, inscov15,
  #Outcome: Expenditures
  totexp15
  ) %>%
  filter(diabdx==1, region15 != -1) %>%
  mutate(
    region15 = factor(region15, levels = c("1","2","3","4"), labels = c("NE","MW","S","W")),
    sex = factor(sex, levels = c("1", "2"), labels=c("M","F")),
    racev2x = factor(racev2x, levels = c("1","2","3","4","5","6","10","12"), 
      labels = c("White","Black","NativeAmer","Indian","Chinese","Filipino","OthAsian","MultRace")),
    hispanic = factor(ifelse(racethx == 1, 1, 0), levels = c("0","1"), labels = c("NotHisp","Hisp")),
    #Careful below: Dropping the -1's from region 15 drops -1's elsewhere too!
    marry_final = ifelse(marry15x %in% c(2, 3, 4), 2, marry15x),
    marry_final = factor(marry_final, levels = c("1","2","5"),
      labels = c("Married","Divorced/Sep","NeverMarried")),
    hibp_y = ifelse(hibpdx == 1, 1, 0), 
    chd_y = ifelse(chddx == 1, 1, 0), 
    angi_y = ifelse(angidx == 1, 1, 0),
    strk_y = ifelse(strkdx == 1, 1, 0),
    emph_y = ifelse(emphdx == 1, 1, 0),
    chol_y = ifelse(choldx == 1, 1, 0),
    cancer_y = ifelse(cancerdx == 1, 1, 0),
    arth_y = ifelse(arthdx == 1, 1, 0),
    asth_y = ifelse(asthdx == 1, 1, 0),
    adhdadd_y = ifelse(adhdaddx == 1, 1, 0),
    had_a1c = ifelse(dsa1c53 >= 1 & dsa1c53 < 96, 1, 0), 
    had_ft = ifelse(dsft1553 == 1, 1, 0), 
    had_eye = ifelse(dsey1553 == 1, 1, 0), 
    had_chol = ifelse(dsch1553 == 1, 1, 0),
    on_diet = ifelse(dsdiet53 == 1, 1, 0),
    on_insulin = ifelse(dsmed53 == 1, 1, 0),
    on_dm_med = ifelse(dsinsu53 == 1, 1, 0),
    dm_composite = ifelse(had_a1c == 1 & had_ft == 1 & had_eye == 1 & had_chol == 1, 1, 0),
    has_prim_prov = ifelse(haveus42 == 1, 1, 0),
    prov_asks_oth_treat = ifelse(treatm42 == 1, 1, 0),
    prov_rspct_oth_treat = ifelse(respct42 == 1, 1, 0),
    emp_dur_53 = ifelse(empst53 %in% c(1, 2, 3), 1, 0),
    inscov15 = factor(inscov15, levels = c("1", "2", "3"), labels = c("AnyPrivate", "PublicOnly", "Uninsured"))) %>%
  dplyr::select(-starts_with("ds"),-racethx, -marry15x, -ends_with("dx"), - starts_with("had_"),
         -haveus42, -treatm42, -respct42, -empst31, -empst42, -empst53, -unins15)

saveRDS(consol, file = "~/Desktop/ISPOR2018/consol_fin.rds")