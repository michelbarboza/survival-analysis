rm(list=ls())

# Required packages
library(readxl)
library(survival)
library(survminer)
library(gridExtra)

data <- read_excel("BaseDadosCovid.xls")
data <- data[-which(data$pcr=="NA"),]

covid <- data[,c("id","time","death","cid1","sex","age","chemo","radio","surgery","hypertension","diabetes","pcr")]
colnames(covid)[4] <- "tumor"

covid$diabetes <- as.numeric(covid$diabetes)
covid$hypertension <- as.numeric(covid$hypertension)
covid$time <- as.numeric(covid$time)
covid$pcr <- as.numeric(covid$pcr)

# Regular Kaplan-Meier plot
# http://www.sthda.com/english/wiki/survminer-0-2-4
km_sex <- survfit(Surv(time, death) ~ sex, data=covid)
km_tumor <- survfit(Surv(time, death) ~ tumor, data=covid)
km_chemo <- survfit(Surv(time, death) ~ chemo, data=covid)
km_radio <- survfit(Surv(time, death) ~ radio, data=covid)
km_surg <- survfit(Surv(time, death) ~ surgery, data=covid)
km_hyper <- survfit(Surv(time, death) ~ hypertension, data=covid)
km_diab <- survfit(Surv(time, death) ~ diabetes, data=covid)
km_pcr <- survfit(Surv(time, death) ~ pcr, data=covid)

mplots <- list()
mplots[[1]] <- ggsurvplot(km_sex, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[2]] <- ggsurvplot(km_tumor, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[3]] <- ggsurvplot(km_chemo, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[4]] <- ggsurvplot(km_radio, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[5]] <- ggsurvplot(km_surg, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[6]] <- ggsurvplot(km_hyper, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[7]] <- ggsurvplot(km_diab, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())
mplots[[8]] <- ggsurvplot(km_pcr, xlim=c(0,55), xlab="Day", legend.title="Variable", pval=T, conf.int=T, pval.coord=c(40,0.95), ggtheme=theme_bw())

arrange_ggsurvplots(mplots, ncol=2, nrow=4)

# if(FALSE){
#   res <- arrange_ggsurvplots(mplots, print=FALSE)
#   ggsave("KMplots.pdf", res)
# }
