#laptop
library(lme4)
library(lsmeans)
library(ggplot2)
setwd("~/git.repos/brassica_field_2014_gh/raw_data/")

field_2014 <- read.table("Brapa_2014_Field_data_timpoint2.csv", 
	                    header=TRUE, sep = ",")
fruit_set <- read.table("BRASSICA_pod_and_branch_number.csv", header = TRUE, sep = ",")

dim(field_2014)
dim(fruit_set)

head(field_2014)
str(field_2014)

head(fruit_set)
str(fruit_set)

?merge
field_2014 <- merge(field_2014, fruit_set, by.x = "plant", by.y = "plant.id", all.x = TRUE )
dim(field_2014)

head(field_2014)
field_2014[, c("plant", "RIL.x", "RIL.y")]
# need to figure this out for christina
# 29     79    R500     93 #add this to below
# 30     79    R500   R500

# 38    105 RIL_303    307 #switch to 303
# 41    108 RIL_248    298
# 47    118  IMB211    128
# 135   335 RIL_284    289
# 170   415 RIL_248    298
# 201   512 RIL_363    303
# 208   528 RIL_268    368 # change to 268
# 233   588 RIL_143    193 # change to 143
# 234   595 RIL_284    289
# 299   765 RIL_268   R500

?as.Date
field_test <- field_2014
head(field_2014)
field_2014$plant

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
field_2014$RIL.x <- as.factor(field_2014$RIL.x)
field_2014$RIL.x <- relevel(field_2014$RIL.x, ref = "R500")
field_2014$RIL.x

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
hist(field_2014$ln_wk3)


# lms 
?lm
heightlm_1 <- lm(ht_wk3 ~ density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_1)


heightlm_2 <- lm(ht_wk2 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_2)
heightlm_3 <- lm(ht_wk1 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_1)

names(field_2014)
heightlm_1 <- lm(wd_wk3 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_1)
heightlm_2 <- lm(wd_wk2 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_2)
heightlm_3 <- lm(wd_wk1 ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(heightlm_3)

names(field_2014)
field_2014$plant2bolt <- as.numeric(field_2014$bolt_date - field_2014$germ_date)
field_2014$plant2bolt
field_2014$plant2flr <- as.numeric(field_2014$flr_date - field_2014$germ_date)
field_2014$plant2flr

boltlm_1 <- lm(plant2bolt ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(boltlm_1)
flrlm_1 <- lm(plant2flr ~ 0 + density_trt*nutrient_trt*RIL, data = field_2014)
summary(flrlm_1)


library(lme4)

?lmer

height_1 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
height_1 
height_2 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	             density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
height_2 
height_3 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
height_3

height_4 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + 
	              (1|blk), data = field_2014, REML = FALSE)
height_4

height_5 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	             density_trt:RIL + 
	              (1|blk), data = field_2014, REML = FALSE)
height_5

anova(height_1, height_2) # no crowding by nutrient interaction
anova(height_1, height_3) # crowding by RIL interaction, yeah!
anova(height_1, height_4) # no nutrient by genotype interaction

height_5_lsmeans <- .old.lsmeans(height_5, pairwise ~ 
	                              density_trt|nutrient_trt|RIL)
height_5_lsmeans
height_5_lsmeans_1 <- height_5_lsmeans[[1]]
height_5_lsmeans_1

limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge <- position_dodge(width=0.9)
plotheight5 <- ggplot(data = height_5_lsmeans_1, 
                  aes(fill=density_trt, y=lsmean, x=nutrient_trt))
plotheight5 <- plotheight5 + geom_bar(position=dodge, stat="identity") 
plotheight5 <- plotheight5 + geom_errorbar(limits, position = dodge, width = 0.25) 
plotheight5 <- plotheight5 + xlab("Nitrogen") + ylab("Plant Height (cm)")
plotheight5 <- plotheight5 + ggtitle("Plant Height") + facet_grid(. ~ RIL)
plotheight5 <- plotheight5 + theme(axis.title=element_text(face="bold",
                               size="14"), axis.text=element_text(face="bold",
                               size="10"))  
plotheight5
setwd("~/git.repos/brassica_field_2014/output/")
ggsave(plotheight5, file="brassica_height_2014.png", width=15, height=8)








width_1 <- lmer(wd_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
width_1 
width_2 <- lmer(wd_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
width_2 
width_3 <- lmer(wd_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
width_3

width_4 <- lmer(wd_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + 
	              (1|blk), data = field_2014, REML = FALSE)
width_4

anova(width_1, width_2) # no crowding by nutrient interaction
anova(width_1, width_3) # crowding by RIL interaction, yeah!
anova(width_1, width_4) # marginal nutrient by genotype interaction

# use model 2
width_2_lsmeans <- .old.lsmeans(width_2, pairwise ~ 
	                              density_trt|nutrient_trt|RIL)
width_2_lsmeans
width_2_lsmeans_1 <- width_2_lsmeans[[1]]
width_2_lsmeans_1

limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge <- position_dodge(width=0.9)
plotwidth5 <- ggplot(data = width_2_lsmeans_1, 
                  aes(fill=density_trt, y=lsmean, x=nutrient_trt))
plotwidth5 <- plotwidth5 + geom_bar(position=dodge, stat="identity") 
plotwidth5 <- plotwidth5 + geom_errorbar(limits, position = dodge, width = 0.25) 
plotwidth5 <- plotwidth5 + xlab("Nitrogen") + ylab("Leaf width (cm)")
plotwidth5 <- plotwidth5 + ggtitle("Leaf width") + facet_grid(. ~ RIL)
plotwidth5 <- plotWidth5 + theme(axis.title=element_text(face="bold",
                               size="14"), axis.text=element_text(face="bold",
                               size="10"))  
plotwidth5
setwd("~/git.repos/brassica_field_2014/output/")
ggsave(plotwidth5, file="brassica_width_2014.png", width=15, height=8)



#leaf length


head(field_2014)
length_1 <- lmer(ln_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
length_1 
length_2 <- lmer(ln_wk2 ~ density_trt + nutrient_trt + RIL.x +
	             density_trt:RIL.x + nutrient_trt:RIL.x +
	              (1|blk), data = field_2014, REML = FALSE)
length_2 
length_3 <- lmer(ln_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
length_3

length_4 <- lmer(ln_wk2 ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + 
	              (1|blk), data = field_2014, REML = FALSE)
length_4

anova(length_1, length_2) # no crowding by nutrient interaction
anova(length_1, length_3) # crowding by RIL interaction, yeah!
anova(length_1, length_4) # marginal nutrient by genotype interaction


?lsmeans
# use model 2
length_2_lsmeans <- .old.lsmeans(length_2, pairwise ~ 
	                              density_trt|nutrient_trt|RIL)
length_2_lsmeans
length_2_lsmeans_1 <- length_2_lsmeans[[1]]
length_2_lsmeans_1

limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge <- position_dodge(width=0.9)
plotlength5 <- ggplot(data = length_2_lsmeans_1, 
                  aes(fill=density_trt, y=lsmean, x=nutrient_trt))
plotlength5 <- plotlength5 + geom_bar(position=dodge, stat="identity") 
plotlength5 <- plotlength5 + geom_errorbar(limits, position = dodge, width = 0.25) 
plotlength5 <- plotlength5 + xlab("Nitrogen") + ylab("Leaf length (cm)")
plotlength5 <- plotlength5 + ggtitle("Leaf Length") + facet_grid(. ~ RIL)
plotlength5 <- plotlength5 + theme(axis.title=element_text(face="bold",
                               size="14"), axis.text=element_text(face="bold",
                               size="10"))  
plotlength5
setwd("~/git.repos/brassica_field_2014/output/")
ggsave(plotlength5, file="brassica_length_2014.png", width=15, height=8)


head(field_2014)
plant2flr_1 <- lmer(plant2flr ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
plant2flr_1 
plant2flr_2 <- lmer(plant2flr ~ density_trt + nutrient_trt + RIL +
	             density_trt:RIL + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
plant2flr_2 
plant2flr_3 <- lmer(plant2flr ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + nutrient_trt:RIL +
	              (1|blk), data = field_2014, REML = FALSE)
plant2flr_3

plant2flr_4 <- lmer(plant2flr ~ density_trt + nutrient_trt + RIL +
	             density_trt:nutrient_trt + density_trt:RIL + 
	              (1|blk), data = field_2014, REML = FALSE)
plant2flr_4

plant2flr_5 <- lmer(plant2flr ~ density_trt + nutrient_trt + RIL +
	             density_trt:RIL + 
	              (1|blk), data = field_2014, REML = FALSE)
plant2flr_5

anova(plant2flr_1, plant2flr_2) # no crowding by nutrient interaction
anova(plant2flr_1, plant2flr_3) # crowding by RIL interaction, yeah!
anova(plant2flr_1, plant2flr_4) # no nutrient by genotype interaction

# use model 5
plant2flr_2_lsmeans <- .old.lsmeans(plant2flr_5, pairwise ~ 
	                              density_trt|nutrient_trt|RIL)
plant2flr_2_lsmeans
plant2flr_2_lsmeans_1 <- plant2flr_2_lsmeans[[1]]
plant2flr_2_lsmeans_1

limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge <- position_dodge(width=0.9)
plotplant2flr5 <- ggplot(data = plant2flr_2_lsmeans_1, 
                  aes(fill=density_trt, y=lsmean, x=nutrient_trt))
plotplant2flr5 <- plotplant2flr5 + geom_bar(position=dodge, stat="identity") 
plotplant2flr5 <- plotplant2flr5 + geom_errorbar(limits, position = dodge, width = 0.25) 
plotplant2flr5 <- plotplant2flr5 + xlab("Nitrogen") + ylab("Days Germ to Flower")
plotplant2flr5 <- plotplant2flr5 + ggtitle("Flowering Time") + facet_grid(. ~ RIL)
plotplant2flr5 <- plotplant2flr5 + theme(axis.title=element_text(face="bold",
                               size="14"), axis.text=element_text(face="bold",
                               size="10"))  
plotplant2flr5
setwd("~/git.repos/brassica_field_2014/output/")
ggsave(plotplant2flr5, file="brassica_plant2flr_2014.png", width=15, height=8)







height_2 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	              (1|density_trt:RIL) + (1|nutrient_trt:RIL), 
	              data = field_2014, REML = FALSE)
height_2

height_3 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	              (1+nutrient_trt|RIL), 
	              data = field_2014, REML = FALSE)
height_3

height_4 <- lmer(ht_wk3 ~ density_trt + nutrient_trt + RIL +
	              (1|density_trt:RIL), 
	              data = field_2014, REML = FALSE)
	             
coef(height_2)




#######
# 2015_16_04
#######
head(field_2014)
plot(field_2014$pod.number)
fruitslm_1 <- lm(pod.number ~ 0 + density_trt*nutrient_trt*RIL.x, data = field_2014)
summary(fruitslm_1)


fruitnumber_lmer <- lmer(pod.number ~ density_trt + nutrient_trt + RIL.x +
	             density_trt:RIL.x + nutrient_trt:RIL.x + density_trt:RIL.x:nutrient_trt +
	              (1|blk), data = field_2014, REML = FALSE)
fruitnumber_lmer

fruitnumber_lmer2 <- lmer(pod.number ~ density_trt + nutrient_trt + RIL.x +
	             density_trt:RIL.x +
	              (1|blk), data = field_2014, REML = FALSE)
fruitnumber_lmer2
anova(fruitnumber_lmer, fruitnumber_lmer2)
fruitnumber_lmer3 <- lmer(pod.number ~ density_trt + nutrient_trt + RIL.x +
	             density_trt:RIL.x + nutrient_trt:RIL.x + density_trt:RIL.x:nutrient_trt +
	              + (1|blk), data = field_2014, REML = FALSE)
fruitnumber_lmer3




fruitnumber_lmer3 <- lmer(pod.number ~ density_trt*nutrient_trt*RIL.x 
	              + (1|blk), data = field_2014, REML = FALSE)
fruitnumber_lmer3

library(lmerTest)
fruits_lsmeans <- lsmeans(fruitnumber_lmer3)
fruits_lsmeans
head(fruits_lsmeans)dd
fruits_lsmeans_1 <- as.data.frame(fruits_lsmeans[[1]])
head(fruits_lsmeans_1)
dim(fruits_lsmeans_1)
fruits_lsmeans_1$rows <- 1:nrow(fruits_lsmeans_1)
fruits_lsmeans_2 <- subset(fruits_lsmeans_1, rows > 88)
str(fruits_lsmeans_2)
fruits_lsmeans_2$density_trt <- relevel(fruits_lsmeans_2$density_trt, ref = "UN")
colnames(fruits_lsmeans_2)[5] <- paste("SE")

limits <- aes(ymax= Estimate + SE, ymin = Estimate - SE)
dodge <- position_dodge(width=0.9)
plotfruits5 <- ggplot(data = fruits_lsmeans_2, 
                  aes(fill = density_trt, y = Estimate, x = nutrient_trt))
plotfruits5 <- plotfruits5 + geom_bar(position=dodge, stat="identity") 
plotfruits5 <- plotfruits5 + geom_errorbar(limits, position = dodge, width = 0.25) 
plotfruits5 <- plotfruits5 + xlab("Nitrogen") + ylab("Fruit Number")
plotfruits5 <- plotfruits5 + ggtitle("") + facet_grid(. ~ RIL.x)
plotfruits5 <- plotfruits5 + theme(axis.title=element_text(face="bold",
                               size="14"), axis.text=element_text(face="bold",
                               size="10"))  
plotfruits5
setwd("~/git.repos/brassica_field_2014/output/")
ggsave(plotfruits5, file="brassica_fruits_2014.png", width=15, height=8)

#######
#fruit set
#######
library(lme4)
head(field_2014)
plot()

fruitnumber_lmer3 <- lmer(pod.number ~ density_trt*RIL.x*nutrient_trt 
	              + (1|blk), data = field_2014, REML = FALSE)
fruitnumber_lmer3

library(lmerTest)
fruits_lsmeans <- lsmeans(fruitnumber_lmer3)
fruits_lsmeans

head(fruits_lsmeans)dd
fruits_lsmeans_1 <- as.data.frame(fruits_lsmeans[[1]])
head(fruits_lsmeans_1)

fruits_lsmeans_1$rows <- 1:nrow(fruits_lsmeans_1)
rownames(fruits_lsmeans_1)
fruits_lsmeans_2 <- subset(fruits_lsmeans_1, rows > 88)
str(fruits_lsmeans_2)
fruits_lsmeans_2


fruits_lsmeans_2$density_trt <- relevel(fruits_lsmeans_2$density_trt, ref = "UN")
colnames(fruits_lsmeans_2)[5] <- paste("SE")
head(fruits_lsmeans_2)

fruits_un <- subset(fruits_lsmeans_2, density_trt == "UN")

fruits_cr <- subset(fruits_lsmeans_2, density_trt == "CR")
head(fruits_un)
head(fruits_cr)
fruits_un$area <- fruits_un$Estimate*126
fruits_cr$area <- fruits_cr$Estimate*640


fruits <- merge(fruits_un, fruits_cr, by = "RIL.x")
head(fruits)
colnames(fruits)
colnames(fruits)[c(3,13)] <- paste(c("UN_fruits", "CR_fruits"))
colnames(fruits)[c(11,21)] <- paste(c("UN_fruits_area", "CR_fruits_area"))
str(fruits)

library(ggplot2)
library(tidyr)
plot(fruits$UN_fruits)
ggplot(fruits, aes(x=UN_fruits, y=CR_fruits)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)  +  
    geom_abline(intercept = 0, colour = "red", size = 1) +
    scale_y_continuous(limits=c(0, 400)) +
    scale_x_continuous(limits=c(0, 400)) +
    xlab("Uncrowded Fruits Per Plant") +
    ylab("Crowded Fruits Per Plant") +
    theme_bw() +
    theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=16,face="bold"))


head(fruits_lsmeans_2)
fruits_lsmeans_2 <- unite(fruits_lsmeans_2, trt, c(density_trt, nutrient_trt), sep = "_", remove = FALSE)
fruits_lsmeans_2$trt <- as.factor(fruits_lsmeans_2$trt)
fruits_lsmeans_2$trt <- relevel(fruits_lsmeans_2$trt, ref = "UN_High")

levels(fruits_lsmeans_2$trt)

fruits_lsmeans_2$trt <- paste(c(fruits_lsmeans_2$density_trt,fruits_lsmeans_2$density_trt))

ggplot(fruits, aes(x = UN_fruits_area, y = CR_fruits_area)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)  +  
    geom_abline(intercept = 0, colour = "red", size = 1) +
    scale_y_continuous(limits=c(0, 60000)) +
    scale_x_continuous(limits=c(0, 60000)) +
    xlab(expression(bold(paste(Uncrowded~Fruits~per,~m^-2)))) +
    ylab(expression(bold(paste(Crowded~Fruits~per,~m^-2)))) +
    theme_bw() +
    theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=16,face="bold"))


ggplot(fruits, aes(x = UN_fruits_area, y = CR_fruits_area)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)  +  
    geom_abline(intercept = 0, colour = "red", size = 1) +
    scale_y_continuous(limits=c(0, 60000)) +
    scale_x_continuous(limits=c(0, 60000)) +
    xlab(expression(bold(paste(Uncrowded~Fruits~per,~m^-2)))) +
    ylab(expression(bold(paste(Crowded~Fruits~per,~m^-2)))) +
    theme_bw() +
    theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=16,face="bold"))


# figure for research proposal
limits <- aes(ymax= Estimate + SE, ymin = Estimate - SE)
head(limits)
dodge <- position_dodge(width=0.9)


ggplot(fruits_lsmeans_2) +
    geom_pointrange(mapping=aes(x = RIL.x, y = Estimate, ymax= Estimate + SE, ymin = Estimate - SE, color = trt), size = 1.5) + 
    #coord_flip() +
    xlab("Genotype") +
    ylab("Pod Number") +
    theme_bw() +
    theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=16,face="bold"))

#subset for only a few genotypes
head(fruits_lsmeans_2)
names(fruits_lsmeans_2)
rils <- c("IMB211", "R500", "RIL_182", "RIL_207", "RIL_93")
rils
fruits_2 <- fruits_lsmeans_2[fruits_lsmeans_2$RIL.x %in% rils,]
fruits_2
levels(fruits_2$trt)
fruits_2$trt <- factor(fruits_2$trt, levels = c("UN_High", "UN_Low", "CR_High", "CR_Low"))

fruits_2_plot <- ggplot(fruits_2) +
    geom_pointrange(mapping=aes(x = RIL.x, y = Estimate, ymax= Estimate + SE, ymin = Estimate - SE, color = trt), size = 2) + 
    # coord_flip() +
    xlab("Genotype") +
    ylab("Measured Pod Number") +
    theme_bw() +
    theme(axis.title.x = element_text(face="bold", size=26),
        axis.text.x  = element_text(face="bold", size=22),
        axis.title.y = element_text(face="bold", size=26),
        axis.text.y  = element_text(face="bold", size=22),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.9), 
        legend.text = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
fruits_2_plot
class(fruits_2_plot)
setwd("~/git.repos/brassica_field_2014_gh/output/")
?ggsave
ggsave("small_dataset.pdf", width = 8, height = 8)

