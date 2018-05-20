###This file allows for replication of McLaughlin poster @ ISPOR Baltimore 2018
###All files used in the poster are created as a result of this script. Each section is delimited by {}
###to aid users who want to know where each piece is implemented.

# Necessary libraries
{
  library(dplyr) #For data manipulation
  library(tidyr) #For data manipulation
  library(caret) #For dummyVars()...ok this is lazy, so sue me..
  library(cobalt) #For balance plot()
  library(ggplot2) #For other plots
  library(ggthemes) #To standardize plots and make them pretty
  library(stargazer) #For producing output from lm
  library(BART) #For Bayesian Additive Regression Trees
  library(grf) #For CausalForests
  library(sparsereg) #For BayesianLASSO Plus
  library(glmnet) #For elastic net glm
}

#Read in clean data and set up 'common basis' idea--i.e., all models estimate from same set of inputs
{
  consol <- readRDS(file = "~/Desktop/ISPOR--CATE/consol_fin.rds")
  set.seed(4228)
  ##Set up 'common basis' idea
  basis_form <- formula(~ region15 + age15x + sex + racev2x + povlev15 + emp_dur_53 + 
                          hispanic + marry_final + inscov15 + hibp_y + chd_y + angi_y + strk_y + 
                          emph_y + chol_y + cancer_y + arth_y + asth_y + adhdadd_y + on_diet + 
                          on_insulin + on_dm_med + has_prim_prov + prov_asks_oth_treat + prov_rspct_oth_treat)
  
  X <- predict(dummyVars(basis_form, data = consol), newdata = consol)
  Y <- consol$totexp15
  D <- consol$dm_composite
}

#Balance plot -- For descriptive statistics (Need to remove legend)
{
  bal <- bal.tab(formula = update.formula(basis_form, dm_composite ~ .), data = consol)
  vn <- data.frame(old = rownames(bal$Balance),
                   new = c("Region-NE", "Region-MW", "Region-S", "Region-W", "Age (Years)", "Male", 
                           "Race-White", "Race-Black", "Race-NativeAmerican", "Race-Indian", "Race-Chinese",
                           "Race-Filipino", "Race-OtherAsian", "Race-MultiRacial", "Poverty Level (% of 2015 FPL)",
                           "Insurance-ANY Private", "Insurance-ONLY Public", "Insurance-Uninsured",
                           "Not Hispanic", "Marital Status-Married","Marital Status-Divorced/Separated", "Marital Status-Never Married",
                           "ChronCond-HBP", "ChronCond-CHD","ChronCond-Angina", "ChronCond-Stroke", "ChronCond-Emphysema",
                           "ChronCond-Cholesterolemia","ChronCond-Cancer","ChronCond-Arthritis","ChronCond-Asthma","ChronCond-ADHD/ADD",
                           "DM-OnDiet", "DM-OnInsulin","DM-OnDMMeds", "Has PCP", "Prov Ask Alt. Ther.", "Prov. Resp. Alt. Ther.",
                           "Employed All 2015"))
  
  jpeg(file = "~/Desktop/ISPOR--CATE/figures/balance_plot.jpeg", bg = "transparent")
  love.plot(bal, stat = c("mean.diffs"), var.names = vn, var.order = "unadjusted",
            title = "Covariate Balance Between Individuals Receiving vs Not Receiving Recommended Diabetes Care") +
    theme_tufte() + 
    labs(x = "Standardized Mean Difference (Positive Implies Higher Mean Among 'Treated')")
  dev.off()
}

#Initialize data frames for tables later
{
ite_plot_dat <- data.frame(method = c("BART", "Causal Forest", "LASSOplus", "Elastic Net"),
  estimate = c(0,0,0,0),
  se = c(0,0,0,0))
ate_plot_dat <- data.frame(method = c("BART", "Causal Forest", "LASSOplus", "Elastic Net"),
  estimate = c(0,0,0,0),
  se = c(0,0,0,0))
}

#BART approach -- Draws heavily on code in Hill (2011)
{
  
  X_treat0 <- cbind(0, X)
  X_treat1 <- cbind(1, X)
  X_test_fin <- rbind(X_treat0, X_treat1)
  
  fit_bart<- wbart(x.train=cbind(D,X), y.train=Y, 
                  x.test = X_test_fin, ndpost=1000, nskip=500)
  
  #BART ITE
  y_0 <- fit_bart$yhat.test[,1:nrow(X)]
  y_1 <- fit_bart$yhat.test[,(nrow(X)+1):(2*nrow(X))]
  bart_pp_ite <- t(y_1 - y_0)
  
  #BART ATE
  ate_ests = colMeans(bart_pp_ite)
  
  #Store data for plots
  ite_plot_dat[ite_plot_dat$method == "BART",]$estimate <- mean(bart_pp_ite[1,])
  ite_plot_dat[ite_plot_dat$method == "BART",]$se <- sd(bart_pp_ite[1,])
  
  ate_plot_dat[ate_plot_dat$method == "BART",]$estimate <- mean(ate_ests)
  ate_plot_dat[ate_plot_dat$method == "BART",]$se <- sd(ate_ests)
}

#Causal Forest 
{
  
  cf <- causal_forest(X = X, Y = Y, W = D, num.trees = 4000)
  
  #CF ITE
  cf_ite <- predict(cf, X, estimate.variance = TRUE)
  
  #CF ATE
  cf_ate <- estimate_average_effect(cf)
  
  ite_plot_dat[ite_plot_dat$method == "Causal Forest",]$estimate <- cf_ite$predictions[1,]
  ite_plot_dat[ite_plot_dat$method == "Causal Forest",]$se <- sqrt(cf_ite$variance.estimates[1,])
  
  ate_plot_dat[ate_plot_dat$method == "Causal Forest",]$estimate <- cf_ate[1]
  ate_plot_dat[ate_plot_dat$method == "Causal Forest",]$se <- cf_ate[2]
}

#'Sparse regression'/BLASSOplus
{
  sp <- sparsereg(y = Y, X = X, treat = D, scale.type = "TX", EM = F)
  
  ite_plot_dat[ite_plot_dat$method == "LASSOplus",]$estimate <- 0
  ite_plot_dat[ite_plot_dat$method == "LASSOplus",]$se <- 0
  
  ate_plot_dat[ate_plot_dat$method == "LASSOplus",]$estimate <- 0
  ate_plot_dat[ate_plot_dat$method == "LASSOplus",]$se <- 0
}
difference(sp, var1 = 1, var2 = 2)

#glmnet -- Draws heavily on code in Grimmer et al. (2017)
{
  Xfull <- model.matrix(~X*D)
  
  net_fit <- cv.glmnet(x = Xfull, y = Y, family = "poisson")
  
  X_glm_t0 <- model.matrix(~ X*rep(0, nrow(X)))
  X_glm_t1 <- model.matrix(~ X*rep(1, nrow(X)))
  
  y_0 <- predict(net_fit, newx = X_glm_t0, s = net_fit$lambda.min)
  y_1 <- predict(net_fit, newx = X_glm_t1, s = net_fit$lambda.min)
  net_ite <- exp(y_1) - exp(y_0)
  
  ite_plot_dat[ite_plot_dat$method == "Elastic Net",]$estimate <- net_ite[1,]
  ite_plot_dat[ite_plot_dat$method == "Elastic Net",]$se <- 0
  
  ate_plot_dat[ate_plot_dat$method == "Elastic Net",]$estimate <- mean(net_ite)
  ate_plot_dat[ate_plot_dat$method == "Elastic Net",]$se <- 0
}

#Table for ITE and ATE
{
d <- ite_plot_dat %>%  mutate(type = "ite") %>%
    union(ate_plot_dat %>% mutate(type = "ate"))
write.csv(d, file = "~/Desktop/ISPOR--CATE/figures/estimate_table.csv")
}

#Density for ITE
{
gg_ite <- data.frame(bart = rowMeans(bart_pp_ite),
                     cf = cf_ite$predictions,
                     net = net_ite)
cuts <- lapply(gg_ite, quantile, probs = c(.05, .95))

gg_ite <- gg_ite %>%
    gather(method, ite) %>%
    mutate(
           #For 95th and 5th percentile cutoffs
           drop_rows = case_when(
              method == "bart" & (ite < cuts$bart[1] | ite > cuts$bart[2]) ~ 1,
              method == "cf" & (ite < cuts$cf[1] | ite > cuts$cf[2]) ~ 1,                                        
              method == "X1" & (ite < cuts$X1[1] | ite > cuts$X1[2]) ~ 1,
              TRUE ~ 0),
           #For ggplot formatting
           method = factor(method, levels = c("bart", "cf", "X1"),
              labels = c("BART", "Causal Forest", "Elastic Net GLM"))) %>%
  filter(drop_rows == 0)


jpeg(file = "~/Desktop/ISPOR--CATE/figures/density_plot.jpeg", bg = "transparent")
ggplot(gg_ite, aes(ite, fill = method)) +
  geom_density(alpha = 0.1) +
  theme_tufte() +
  scale_fill_discrete(name = "Estimation Method") + 
  labs(x = "Estimated Individual Treatment Effect", 
       y = "Density",
       title = "Smoothed Density Plots of Individual Treatment Effects From Different Methods",
       subtitle = "Note: Data below the 5th and above 95th percentile suppressed for plotting purposes")
dev.off()
}

#Lm the ITE for Bart, CausalForest, ElasticNet
{
  library(stargazer)
  bart_ite_lm <- lm(bart_ite ~ region15 + age15x + sex + racev2x + povlev15 + emp_dur_53 + 
                     hispanic + marry_final + inscov15 + hibp_y + chd_y + angi_y + strk_y + 
                     emph_y + chol_y + cancer_y + arth_y + asth_y + adhdadd_y + on_diet + 
                     on_insulin + on_dm_med + has_prim_prov + prov_asks_oth_treat + prov_rspct_oth_treat, 
                   data = data.frame(bart_ite = rowMeans(bart_pp_ite), consol))
  
  cf_ite_lm <- lm(cf_ite ~ region15 + age15x + sex + racev2x + povlev15 + emp_dur_53 + 
                     hispanic + marry_final + inscov15 + hibp_y + chd_y + angi_y + strk_y + 
                     emph_y + chol_y + cancer_y + arth_y + asth_y + adhdadd_y + on_diet + 
                     on_insulin + on_dm_med + has_prim_prov + prov_asks_oth_treat + prov_rspct_oth_treat, 
                   data = data.frame(cf_ite = cf_ite$predictions, consol))
  
  net_ite_lm <- lm(net_ite ~ region15 + age15x + sex + racev2x + povlev15 + emp_dur_53 + 
                     hispanic + marry_final + inscov15 + hibp_y + chd_y + angi_y + strk_y + 
                     emph_y + chol_y + cancer_y + arth_y + asth_y + adhdadd_y + on_diet + 
                     on_insulin + on_dm_med + has_prim_prov + prov_asks_oth_treat + prov_rspct_oth_treat, 
                   data = data.frame(net_ite = net_ite, consol))
  
  writeLines(stargazer(bart_ite_lm, cf_ite_lm, net_ite_lm, type = "html"), 
            con = "~/Desktop/ISPOR--CATE/figures/lm_table.html")
}