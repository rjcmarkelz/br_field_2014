#laptop
library(lme4)
library(lsmeans)
library(ggplot2)
setwd("~/git.repos/brassica_field_2014/raw_data/")

field_2014 <- read.table("Brapa_2014_Field_data_timpoint2.csv", 
	                    header=TRUE, sep = ",")
head(field_2014)
str(field_2014)
?as.Date
field_test <- field_2014
head(field_test)

as.Date(field_test[,c("plant_date","germ_date")], format = "%m/%d/%Y")
field_2014$plant_date <- as.Date(field_2014$plant_date, format = "%m/%d/%Y")
field_2014$germ_date <- as.Date(field_2014$germ_date, format = "%m/%d/%Y")
field_2014$flr_date <- as.Date(field_2014$flr_date, format = "%m/%d/%Y")
field_2014$bolt_date <- as.Date(field_2014$bolt_date, format = "%m/%d/%Y")
field_2014$plant <- as.factor(field_2014$plant)
field_2014$blk <- as.factor(field_2014$blk)
field_2014$neigh_num <- as.factor(field_2014$neigh_num)

# relevel 
field_2014$density_trt <- relevel(field_2014$density_trt, ref = "UN")
field_2014$nutrient_trt <- relevel(field_2014$nutrient_trt, ref = "High")
field_2014$RIL <- relevel(field_2014$RIL, ref = "IMB211")
field_2014$RIL

dfplot <- function(data.frame)
{
  df <- data.frame
  ln <- length(names(data.frame))
  for(i in 1:ln){
    mname <- substitute(df[,i])
      if(is.factor(df[,i])){
        plot(df[,i],main=names(df)[i])}
        else{hist(df[,i],main=names(df)[i])}
  }
}

?substitute

dfplot(field_2014)
# cannot deal with dates
# do not have time to debug

plot(field_2014$ht_wk3)
plot(field_2014$ht_wk3)
hist(field_2014$ht_wk3)
hist(field_2014$ht_wk2)
hist(field_2014$ht_wk1)
hist(field_2014$wd_wk1)
hist(field_2014$wd_wk2)
hist(field_2014$wd_wk3)
hist(field_2014$ln_wk1)
hist(field_2014$ln_wk2)
hist(field_201
     4$ln_wk3)


#lms 
?lm
head(field_2014)
heightlm_1 <- lm(ht_wk3 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_1)
heightlm_2 <- lm(ht_wk3 ~ 0 + density_trt*nutrient_trt, data = field_2014)
anova(heightlm_1, heightlm_2)

heightlm_3 <- lm(ht_wk2 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_3)
heightlm_4 <- lm(ht_wk2 ~ 0 + density_trt*nutrient_trt, data = field_2014)
anova(heightlm_3, heightlm_4)

heightlm_5 <- lm(ht_wk1 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_5)
heightlm_6 <- lm(ht_wk1 ~ 0 + density_trt*nutrient_trt, data = field_2014)
anova(heightlm_3, heightlm_4)

lengthlm_3 <- lm(ln_wk2 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(lengthlm_3)
lengthlm_4 <- lm(ln_wk2 ~ 0 + density_trt*nutrient_trt, data = field_2014)
anova(lengthlm_3, lengthlm_4)

head(field_2014)





