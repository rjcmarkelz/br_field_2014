setwd("~/git.repos/brassica_field_2014/raw_data/")

field_2014 <- read.table("Brapa_2014_Field_data_timpoint2.csv", 
	                    header=TRUE, sep = ",")
fruit_set <- read.table("BRASSICA_pod_and_branch_number.csv", header = TRUE, sep = ",")

field_2014 <- merge(field_2014, fruit_set, by.x = "plant", by.y = "plant.id", all.x = TRUE )
dim(field_2014)
head(field_2014)

head(field_2014)
field_2014_h3 <- field_2014[, c("pod.number.x", "density_trt", "nutrient_trt", "RIL.y")]
head(field_2014_h3)

field_2014_h3 <- subset(field_2014_h3, nutrient_trt == "High")
field_2014_h3



field_2014_h3$density_trt <- as.character(field_2014_h3$density_trt)



# field_2014_h3$trt[field_2014_h3$density_trt == "UN" & field_2014_h3$nutrient_trt == "High"] <- 1
# field_2014_h3$trt[field_2014_h3$density_trt == "UN" & field_2014_h3$nutrient_trt == "Low"] <- 2
# field_2014_h3$trt[field_2014_h3$density_trt == "CR" & field_2014_h3$nutrient_trt == "High"] <- 3
# field_2014_h3$trt[field_2014_h3$density_trt == "CR" & field_2014_h3$nutrient_trt == "Low"] <- 4

field_2014_h3$RIL.y <- sub("(IMB211)","511", field_2014_h3$RIL.y)
field_2014_h3$RIL.y <- sub("(R500)","500", field_2014_h3$RIL.y)
field_2014_h3 <- na.omit(field_2014_h3)
field_2014_h3 <- field_2014_h3[c(1,2,4)]
field_2014_h3

colnames(field_2014_h3) <- paste(c("yield", "trt", "geno"))
hist(field_2014_h3$yield)
str(field_2014_h3)
dim(field_2014_h3)
?ifelse
field_2014_h3$geno <- as.numeric(field_2014_h3$geno)

field_2014_h3$trt <- ifelse(field_2014_h3$trt == "UN", 0, 1)
field_2014_h3 <- field_2014_h3[with(field_2014_h3, order(geno, trt)),]
field_2014_h3

field_2014_l2_stan3 <- map2stan(
	alist(
		yield ~ dnorm(mu , sigma),
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

yield_out <- precis(field_2014_l2_stan3, depth = 2)
yield_out
yield_out <- yield_out@output
head(yield_out)
str(yield_out)
yield_out$name <- rownames(yield_out)
# to do is figure out ggplot
colnames(yield_out)[3:4] <- paste(c("low95", "high95"))
head(yield_out) 
str(yield_out)
yield_out$name <- sub("(*)+(\\])", "\\1", yield_out$name) 
yield_out$name <- sub("(*)+(\\[)", "\\1", yield_out$name) 
yield_out$name <- sub("(*)+(,)(*)+", "\\1\\3", yield_out$name) 
head(yield_out)
tail(yield_out, 20)

library(ggplot2)
setwd("~/git.repos/brassica_field_2014_gh/output/")
yield_plot <- ggplot(data = yield_out)
yield_plot <- yield_plot + geom_pointrange(mapping=aes(x = name, y = Mean, ymin=low95, ymax=high95)) + coord_flip()
yield_plot
ggsave("yield_CR_UN.pdf", height_plot, width = 10, height = 30)


field_2014_h4 <- field_2014[, c("pod.number.x", "density_trt", "nutrient_trt", "RIL.y")]
head(field_2014_h4)

field_2014_h4 <- subset(field_2014_h4, density_trt == "UN")
field_2014_h4

field_2014_h4$density_trt <- as.character(field_2014_h4$density_trt)



# field_2014_h4$trt[field_2014_h4$density_trt == "UN" & field_2014_h4$nutrient_trt == "High"] <- 1
# field_2014_h4$trt[field_2014_h4$density_trt == "UN" & field_2014_h4$nutrient_trt == "Low"] <- 2
# field_2014_h4$trt[field_2014_h4$density_trt == "CR" & field_2014_h4$nutrient_trt == "High"] <- 3
# field_2014_h4$trt[field_2014_h4$density_trt == "CR" & field_2014_h4$nutrient_trt == "Low"] <- 4

field_2014_h4$RIL.y <- sub("(IMB211)","511", field_2014_h4$RIL.y)
field_2014_h4$RIL.y <- sub("(R500)","500", field_2014_h4$RIL.y)
field_2014_h4 <- na.omit(field_2014_h4)
field_2014_h4 <- field_2014_h4[c(1,3,4)]
field_2014_h4

colnames(field_2014_h4) <- paste(c("yield", "trt", "geno"))
hist(field_2014_h4$yield)
str(field_2014_h4)
dim(field_2014_h4)
?ifelse
field_2014_h4$geno <- as.numeric(field_2014_h4$geno)

field_2014_h4$trt <- ifelse(field_2014_h4$trt == "High", 0, 1)
field_2014_h4 <- field_2014_h4[with(field_2014_h4, order(geno, trt)),]
field_2014_h4

field_2014_l2_stan4 <- map2stan(
	alist(
		yield ~ dnorm(mu , sigma),
		mu <- a_geno[geno] + bg_geno[geno]*trt, 
		c(a_geno, bg_geno)[geno] ~ dmvnorm2( c(a, bg), sigma_geno, Rho),
		a ~ dnorm(0, 100), 
		bg ~ dnorm(0, 100),
		sigma_geno ~ dcauchy(0, 2),
		sigma ~ dcauchy(0 , 2),
		Rho ~ dlkjcorr(2)
	),
	data = field_2014_h4, warmup = 1000, iter = 5000, chains = 2
)

precis(field_2014_l2_stan4, depth = 2)
plot(field_2014_l2_stan4)
plot(precis(field_2014_l2_stan4, depth = 2))
stancode(field_2014_l2_stan4)

yield_out <- precis(field_2014_l2_stan4, depth = 2)
yield_out
yield_out <- yield_out@output
head(yield_out)
str(yield_out)
yield_out$name <- rownames(yield_out)
# to do is figure out ggplot
colnames(yield_out)[3:4] <- paste(c("low95", "high95"))
head(yield_out) 
str(yield_out)
yield_out$name <- sub("(*)+(\\])", "\\1", yield_out$name) 
yield_out$name <- sub("(*)+(\\[)", "\\1", yield_out$name) 
yield_out$name <- sub("(*)+(,)(*)+", "\\1\\3", yield_out$name) 
head(yield_out)
tail(yield_out, 20)

library(ggplot2)
setwd("~/git.repos/brassica_field_2014_gh/output/")
yield_plot_N <- ggplot(data = yield_out)
yield_plot_N <- yield_plot_N + geom_pointrange(mapping=aes(x = name, y = Mean, ymin=low95, ymax=high95)) + coord_flip()
yield_plot_N
ggsave("yield_UN_HN_LN.pdf", yield_plot_N, width = 10, height = 30)

########
########
# both treatments
########
########

field_2014_h5 <- field_2014[, c("pod.number.x", "density_trt", "nutrient_trt", "RIL.y")]
head(field_2014_h5)

field_2014_h5$nutrient_trt <- as.character(field_2014_h5$nutrient_trt)
field_2014_h5$density_trt <- as.character(field_2014_h5$density_trt)

field_2014_h5$RIL.y <- sub("(IMB211)","511", field_2014_h5$RIL.y)
field_2014_h5$RIL.y <- sub("(R500)","500", field_2014_h5$RIL.y)
field_2014_h5 <- na.omit(field_2014_h5)
field_2014_h5 <- field_2014_h5[c(1,3,4)]
field_2014_h5

colnames(field_2014_h5) <- paste(c("yield", "trt", "nrt", "geno"))
hist(field_2014_h5$yield)
str(field_2014_h5)
dim(field_2014_h5)
?ifelse
field_2014_h5$geno <- as.numeric(field_2014_h5$geno)

field_2014_h5$trt <- ifelse(field_2014_h5$trt == "UN", 0, 1)
field_2014_h5$nrt <- ifelse(field_2014_h5$nrt == "High", 0, 1)
field_2014_h5 <- field_2014_h5[with(field_2014_h5, order(geno, trt)),]
field_2014_h5

field_2014_l2_stan5 <- map2stan(
	alist(
		yield ~ dnorm(mu , sigma),
		mu <- a_geno[geno] + bg_geno[geno]*trt + bg_geno[geno]*nrt, 
		c(a_geno, bg_geno)[geno] ~ dmvnorm2( c(a, bg), sigma_geno, Rho),
		a ~ dnorm(0, 100), 
		bg ~ dnorm(0, 100),
		sigma_geno ~ dcauchy(0, 2),
		sigma ~ dcauchy(0 , 2),
		Rho ~ dlkjcorr(2)
	),
	data = field_2014_h5, warmup = 1000, iter = 5000, chains = 2
)


precis(field_2014_l2_stan5, depth = 2)
plot(field_2014_l2_stan5)
plot(precis(field_2014_l2_stan5, depth = 2))
stancode(field_2014_l2_stan5)

yield_out <- precis(field_2014_l2_stan5, depth = 2)
yield_out
yield_out <- yield_out@output
head(yield_out)
str(yield_out)
yield_out$name <- rownames(yield_out)
# to do is figure out ggplot
colnames(yield_out)[3:4] <- paste(c("low95", "high95"))
head(yield_out) 
str(yield_out)
yield_out$name <- sub("(*)+(\\])", "\\1", yield_out$name) 
yield_out$name <- sub("(*)+(\\[)", "\\1", yield_out$name) 
yield_out$name <- sub("(*)+(,)(*)+", "\\1\\3", yield_out$name) 
head(yield_out)
tail(yield_out, 20)

library(ggplot2)
setwd("~/git.repos/brassica_field_2014_gh/output/")
yield_plot_N <- ggplot(data = yield_out)
yield_plot_N <- yield_plot_N + geom_pointrange(mapping=aes(x = name, y = Mean, ymin=low95, ymax=high95)) + coord_flip()
yield_plot_N
ggsave("yield_UN_HN_LN.pdf", yield_plot_N, width = 10, height = 30)

