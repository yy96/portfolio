################
# BT4211 Model #
################

setwd("/Users/yuanyuanchen/Desktop/BT4211/Project")

# load the required package
library(plm)
library(stargazer)
library(lmtest)

df <- read.csv('./Data/merged_df_final.csv')
# getting the relevant columns for modelling later
df_model <- df[c( "year","listing_id","price","occupancy_rate","neighbourhood_group_cleansed", "houses_count",
                  "dist_to_stn", "dist_to_ctrl_park", "number_of_amenities", "basic_amenity", "bathroom_amenity", 
                  "kitchen_amenity", "children_amenity", "convenience_amenity", "safety_amenity", "leisure_amenity",
                  "Polarity.std", "Polarity.mean", "Subjectivity.std", "Subjectivity.mean", "review_when_booking","no_months_host", "review_diff", 
                  "host_is_superhost", "no_words_description", "ImgBrightness","ImgWidth", "ImgHeight", "noun_sim", "adj_sim", "verb_sim")]

# continuous variables with considerable big range are logged
df_model['log_price'] <- log(df_model['price'])
df_model$log_price[is.infinite(df_model$log_price)] <- 0 
df_model['log_houses_count'] <- log(df_model['houses_count'])
df_model$log_houses_count[is.infinite(df_model$log_houses_count)] <- 0 
df_model['log_dist_to_stn'] <- log(df_model['dist_to_stn'])
df_model$log_dist_to_stn[is.infinite(df_model$log_dist_to_stn)] <- 0 
df_model['log_dist_to_ctrl_park'] <- log(df_model['dist_to_ctrl_park'])
df_model$log_dist_to_ctrl_park[is.infinite(df_model$log_dist_to_ctrl_park)] <- 0 
df_model['log_number_of_amenities'] <- log(df_model['number_of_amenities'])
df_model$log_number_of_amenities[is.infinite(df_model$log_number_of_amenities)] <- 0 
df_model['log_review_when_booking'] <- log(df_model['review_when_booking'])
df_model$log_review_when_booking[is.infinite(df_model$log_review_when_booking)] <- 0 
df_model['log_no_months_host'] <- log(df_model['no_months_host'])
df_model$log_no_months_host[is.infinite(df_model$log_no_months_host)] <- 0 
df_model['log_review_diff'] <- log(df_model['review_diff'])
df_model$log_review_diff[is.infinite(df_model$log_review_diff)] <- 0 
df_model['log_no_words_description'] <- log(df_model['no_words_description'])
df_model$log_no_words_description[is.infinite(df_model$log_no_words_description)] <- 0 
df_model['log_ImgBrightness'] <- log(df_model['ImgBrightness'])
df_model$log_ImgBrightness[is.infinite(df_model$log_ImgBrightness)] <- 0 
df_model['log_ImgWidth'] <- log(df_model['ImgWidth'])
df_model$log_ImgWidth[is.infinite(df_model$log_ImgWidth)] <- 0 
df_model['log_ImgHeight'] <- log(df_model['ImgHeight'])
df_model$log_ImgHeight[is.infinite(df_model$log_ImgHeight)] <- 0 

df_model['year2017'] <- df_model$year == 2017
df_model['year2018'] <- df_model$year == 2018

# getting the summary for the input variables
stargazer(df_model, type = "html", title="Descriptive statistics", digits=1, out="data_describe.htm")

##################
# Occupancy Rate #
##################
# ols for robustness check
lm_ols <- lm(formula = occupancy_rate~year2017+year2018+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
               convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
               host_is_superhost+noun_sim+adj_sim+verb_sim+log_price+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
               log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight+log_price*log_houses_count+log_price*log_dist_to_ctrl_park, 
             data = df_model)
summary(lm_ols)

# fixed effects:Hausman taylor model
ht_plm <- plm(formula = occupancy_rate~year2017+year2018+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
                convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
                host_is_superhost+noun_sim+adj_sim+verb_sim+log_price+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
                log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight+log_price*log_houses_count+log_price*log_dist_to_ctrl_park, 
              index = c('listing_id','year'), data = df_model,model ="random", random.method = "ht",inst.method = "baltagi")
summary(ht_plm)

fix_model_plm <- plm(formula = occupancy_rate~year2017+year2018+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
                       convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
                       host_is_superhost+noun_sim+adj_sim+verb_sim+log_price+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
                       log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight+log_price*log_houses_count+log_price*log_dist_to_ctrl_park, 
                     data = df_model, index = c('listing_id','year'), model = 'within', effect = 'individual')
summary(fix_model_plm)

random_model_plm <- plm(formula = occupancy_rate~year2017+year2018+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
                       convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
                         host_is_superhost+noun_sim+adj_sim+verb_sim+log_price+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
                       log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight+log_price*log_houses_count+log_price*log_dist_to_ctrl_park, 
                       data = df_model, index = c('listing_id','year'), model = 'pooling', effect = 'individual')
summary(random_model_plm)

#  the null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects
#  If this number is < 0.05 then use fixed effects
ht_result <- phtest(ht_plm,random_model_plm)

# model_output
stargazer(ht_plm, random_model_plm, type = 'html', no.space=TRUE, out="occupancy_model.htm", add.lines = list(c('hausmen test', paste0('p value: ', '< 2.2e-16'), paste0('chi sqr: ', as.character(ht_result$statistic)))))
# robustness check
stargazer(lm_ols, ht_plm, random_model_plm, type = 'html', no.space=TRUE, out="occupancy_robust.htm")

#########
# Price #
#########
# normal OLS 
lm_ols_price <- lm(formula = log_price~year+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
               convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
               host_is_superhost+noun_sim+adj_sim+verb_sim+occupancy_rate+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
               log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight+log_price*log_houses_count+log_price*log_dist_to_ctrl_park, 
             data = df_model)
summary(lm_ols_price)

# linear regression but with cluster standard error
# http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
# https://stats.stackexchange.com/questions/68354/adjusted-r2-f-test-are-not-shown-in-regression-with-robust-standard-errors
# compute Stata like df-adjustment
plm_model <- plm(formula = log_price~year+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
                   convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
                   host_is_superhost+noun_sim+adj_sim+verb_sim+occupancy_rate+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
                   log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight, 
                 data = df_model, model='pooling')
summary(plm_model)

G <- length(unique(df_model$listing_id))
N <- length(df_model$listing_id)
dfa <- (G/(G - 1)) * (N - 1)/plm_model$df.residual

# display with cluster VCE and df-adjustment
time_c_vcov <- dfa * vcovHC(plm_model, type = "HC0", cluster = "time", adjust = T)
cluster_plm <- coeftest(plm_model, vcov = time_c_vcov)

# RE specification
random_model_plm_price <- plm(formula = log_price~year+neighbourhood_group_cleansed+basic_amenity+bathroom_amenity+kitchen_amenity+children_amenity+
                          convenience_amenity+safety_amenity+leisure_amenity+Polarity.std+Polarity.mean+Subjectivity.std+Subjectivity.mean+
                          host_is_superhost+noun_sim+adj_sim+verb_sim+occupancy_rate+log_houses_count+log_dist_to_stn+log_dist_to_ctrl_park+log_number_of_amenities+log_review_when_booking+
                          log_review_diff+log_no_words_description+log_ImgBrightness+log_ImgWidth+log_ImgHeight, 
                        data = df_model, index = c('listing_id','year'), model = 'pooling', effect = 'individual')
summary(random_model_plm_price)

#  the null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects
#  If this number is < 0.05 then use fixed effects
# phtest(fix_model_plm_price,random_model_plm_price)

# output
stargazer(cluster_plm, random_model_plm_price, type = 'html', no.space=TRUE, out="models_price.htm")
