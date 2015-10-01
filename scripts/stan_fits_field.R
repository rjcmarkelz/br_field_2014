setwd("~/git.repos/brassica_field_2014/raw_data/")

field_2014 <- read.table("Brapa_2014_Field_data_timpoint2.csv", 
	                    header=TRUE, sep = ",")
fruit_set <- read.table("BRASSICA_pod_and_branch_number.csv", header = TRUE, sep = ",")

field_2014 <- merge(field_2014, fruit_set, by.x = "plant", by.y = "plant.id", all.x = TRUE )
dim(field_2014)
head(field_2014)

head(field_2014)
field_2014_h3 <- field_2014[, c("ln_wk2", "density_trt", "nutrient_trt", "RIL.y")]
head(field_2014_h3)
str(field_2014_h3)
field_2014_h3$density_trt <- as.character(field_2014_h3$density_trt)
field_2014_h3$density_trt <- as.character(field_2014_h3$density_trt)

field_2014_h3$trt[field_2014_h3$density_trt == "UN" & field_2014_h3$nutrient_trt == "High"] <- 1
field_2014_h3$trt[field_2014_h3$density_trt == "UN" & field_2014_h3$nutrient_trt == "Low"] <- 2
field_2014_h3$trt[field_2014_h3$density_trt == "CR" & field_2014_h3$nutrient_trt == "High"] <- 3
field_2014_h3$trt[field_2014_h3$density_trt == "CR" & field_2014_h3$nutrient_trt == "Low"] <- 4
field_2014_h3$RIL.y <- sub("(IMB211)","511", field_2014_h3$RIL.y)
field_2014_h3$RIL.y <- sub("(R500)","500", field_2014_h3$RIL.y)
field_2014_h3 <- na.omit(field_2014_h3)
field_2014_h3 <- field_2014_h3[c(1,4:5)]


colnames(field_2014_h3) <- paste(c("length", "geno", "trt"))
hist(field_2014_h3$height)
str(field_2014_h3)
dim(field_2014_h3)
?ifelse
field_2014_h3$geno <- as.numeric(field_2014_h3$geno)


field_2014_h3 <- field_2014_h3[with(field_2014_h3, order(geno, trt)),]

field_2014_l2_stan3 <- map2stan(
	alist(
		length ~ dnorm(mu , sigma),
		mu <- a_geno[geno] + bg_geno[geno]*trt, 
		c(a_geno, bg_geno)[geno] ~ dmvnorm2( c(a, bg), sigma_geno, Rho),
		a ~ dnorm(0, 100), 
		bg ~ dnorm(0, 100),
		sigma_geno ~ dcauchy(0, 2),
		sigma ~ dcauchy(0 , 2),
		Rho ~ dlkjcorr(2)
	),
	data = field_2014_h3, warmup = 1000, iter = 5000, chains = 2
)

precis(field_2014_l2_stan3, depth = 2)
plot(field_2014_l2_stan3)
plot(precis(field_2014_l2_stan3, depth = 2))
stancode(field_2014_l2_stan3)

