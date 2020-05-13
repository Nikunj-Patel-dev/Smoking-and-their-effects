#Libraries
library(foreign)
require(gtools)
install.packages('fastDummies')
require(fastDummies)
require(data.table)
require(tibble)
library(corrplot)
require(class)

################################ Data loading and cleaning ########################################

#fetching data
year_seq <- seq(2012,2017,1)
dir_url <- 'C:/Users/ABC/Desktop/Rach Github/Github/Cigarette Smoking/Original SAV files/'
load_url <- paste(dir_url,year_seq,'.sav',sep="") #urls to fetch data from

df <- data.frame()

for (i in 1:length(load_url)) {
  temp = read.spss(load_url[i],to.data.frame = TRUE)
  #temp$year = year_df[i,1]
  cat("Processing",load_url[i],"...")
  
  if (nrow(df)==0 && ncol(df)==0){
    df = temp
    rm(temp)
  } else {
    df = smartbind(df,temp, fill = NA)
    rm(temp)
  }
}


col_df <- data.frame(colnames(df))
column_label <- read.csv("C:/Users/ABC/Desktop/Rach Github/Github/Cigarette Smoking/UpdatedColumnNames.csv")

colnames(df) <- column_label$updated_names

smk_df <- df[,cbind(1,2,3,4,5,6,7,8,9,10,68,93,94,112,113,114,115,130,139,142,156,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,179,180,181,182,183,184,185,186,187,188,189,201,202,219,223,244,277,297,318,329,330,340,341,343,387,423,464,473,501,502,520,553,605)]
colnames(smk_df)
dim(smk_df)
summary(smk_df)

#removed columns with missing > 70%
table(is.na(smk_df))

#missing values pattern
as.data.frame(colMeans(is.na(smk_df)))
plot(colMeans(is.na(smk_df)))

smk_df <- smk_df[, -which(colMeans(is.na(smk_df)) > 0.7)]

#imputing median 
smk_df3 <- smk_df
for(i in which(colnames(smk_df3)=="EVR_SMK_CIG_REGL"):ncol(smk_df3)){
  x <- round(median(as.numeric(factor(smk_df3[,i])),na.rm=T))
  y <- as.list(levels(smk_df3[,i]))
  smk_df3[which(is.na(smk_df3[i])),i] <- y[x]
  print(summary(smk_df3[i]))
}

smk_df3$GRADE <- as.factor(smk_df3$GRADE)
levels(smk_df3$GRADE)<- list('EIGHTH:(1)'=c('8', 'EIGHTH:(8)'), 'TENTH:(2)'=c('10','TENTH:(10)'))

levels(smk_df3$EVR_SMK_CIG_REGL)<- list(YES=c('1-2X:(2)', 'OCCASNLY:(3)', 'REG PAST:(4)', 'REG NOW:(5)', 'OCCASIONALLY:(3)', 'REGULAR PAST:(4)', 'REGULAR NOW:(5)'), NO='NEVER:(1)')
names(smk_df3)[names(smk_df3)=='EVR_SMK_CIG_REGL'] <- 'SMK_CIG'

smk_df3$GRADE<- as.numeric(factor(smk_df3$GRADE))
smk_df3$SMK_CIG<- as.numeric(factor(smk_df3$SMK_CIG))
head(smk_df3$SMK_CIG) 


smk_df3 <- dummy_cols(smk_df3, select_columns = c('SMK_CIG'), remove_first_dummy = FALSE)
head(smk_df3$SMK_CIG)
colnames(smk_df3)
smk_df3 <- subset(smk_df3, select = -c(10,41))
head(smk_df3$SMK_CIG_1)

smk_df3$SCHOOL_REGION<- as.numeric(factor(smk_df3$SCHOOL_REGION))
smk_df3$CIGS_SMKD_30DAY<- as.numeric(factor(smk_df3$CIGS_SMKD_30DAY))
smk_df3$GR_1ST_SMOK_DLY<- as.numeric(factor(smk_df3$GR_1ST_SMOK_DLY))
smk_df3$GR_1ST_SMOK_EVR<- as.numeric(factor(smk_df3$GR_1ST_SMOK_EVR))
smk_df3$DIF_TRY_QUIT_CIG<- as.numeric(factor(smk_df3$DIF_TRY_QUIT_CIG))
smk_df3$ALL_FRD_SMK_CIGS<- as.numeric(factor(smk_df3$ALL_FRD_SMK_CIGS))
smk_df3$STDNTS_SMK_1CIG<- as.numeric(factor(smk_df3$STDNTS_SMK_1CIG))
smk_df3$EASY_GT_CIGS<- as.numeric(factor(smk_df3$EASY_GT_CIGS))
smk_df3$NO_SMK_IN_5_YR<- as.numeric(factor(smk_df3$NO_SMK_IN_5_YR))
smk_df3$NEVER_CIG_ADDICT<- as.numeric(factor(smk_df3$NEVER_CIG_ADDICT))
smk_df3$QUIT_CIG_WN_WANT<- as.numeric(factor(smk_df3$QUIT_CIG_WN_WANT))
smk_df3$SMK_DANGER_QUIT<- as.numeric(factor(smk_df3$SMK_DANGER_QUIT))
smk_df3$START_SMK_THISYR<- as.numeric(factor(smk_df3$START_SMK_THISYR))
smk_df3$USE_DRUG_HOME<- as.numeric(factor(smk_df3$USE_DRUG_HOME))
smk_df3$USE_DRG_FRNDS<- as.numeric(factor(smk_df3$USE_DRG_FRNDS))
smk_df3$US_DRG_SCHEVE<- as.numeric(factor(smk_df3$US_DRG_SCHEVE))
smk_df3$USE_DRG_ATSCH<- as.numeric(factor(smk_df3$USE_DRG_ATSCH))
smk_df3$DISAP_1_PK_CIGS<- as.numeric(factor(smk_df3$DISAP_1_PK_CIGS))
smk_df3$RSK_OF_CIG1_PK_D<- as.numeric(factor(smk_df3$RSK_OF_CIG1_PK_D))
smk_df3$RSK_1_5_CIGS_DAY<- as.numeric(factor(smk_df3$RSK_1_5_CIGS_DAY))
smk_df3$CIG_SMKRS_ATHLTS<- as.numeric(factor(smk_df3$CIG_SMKRS_ATHLTS))
smk_df3$SAVED_CIG_COUPON<- as.numeric(factor(smk_df3$SAVED_CIG_COUPON))
smk_df3$LSTYR_ENJOY_SCHL<- as.numeric(factor(smk_df3$LSTYR_ENJOY_SCHL))
smk_df3$LSTYR_HATE_SCHL<- as.numeric(factor(smk_df3$LSTYR_HATE_SCHL))
smk_df3$LSTYR_SCH_2_HARD<- as.numeric(factor(smk_df3$LSTYR_SCH_2_HARD))
smk_df3$SCHL_RULES_FAIR<- as.numeric(factor(smk_df3$SCHL_RULES_FAIR))
smk_df3$DALY_ACTV_SPORTS<- as.numeric(factor(smk_df3$DALY_ACTV_SPORTS))
smk_df3$PRNT_ALW_OUT<- as.numeric(factor(smk_df3$PRNT_ALW_OUT))
smk_df3$SATISFD_W_MYSELF<- as.numeric(factor(smk_df3$SATISFD_W_MYSELF))
smk_df3$RS_SEX<- as.numeric(factor(smk_df3$RS_SEX))
smk_df3$RACE_B_W_H<- as.numeric(factor(smk_df3$RACE_B_W_H))

names(smk_df3)[names(smk_df3)=='SMK_CIG_1'] <- 'SMK_CIG'
head(smk_df3)

# correlation matrix

colnames(smk_df3)
smk_df3 <-smk_df3[,c(40,6,7,10:(ncol(smk_df3)-1))]
colnames(smk_df3)
corrplot(cor(smk_df3), method = "color",xlim = c(0, 1), ylim = c(0, 1), type= "upper",number.digits = 1, tl.cex = 0.5, tl.col = "gray50")

cor(smk_df3[,16], smk_df3[,17])
cor(smk_df3[,16], smk_df3[,18]) 
cor(smk_df3[,17], smk_df3[,18])

smk_df3 <- subset(smk_df3, select = -17)
colnames(smk_df3)

cor(smk_df3[,16], smk_df3[,17])

cor(smk_df3[,1], smk_df3[,17])# delete 17 variable
cor(smk_df3[,1], smk_df3[,16]) 

smk_df3 <- subset(smk_df3, select = -17)
colnames(smk_df3)

#check corrplot again
corrplot(cor(smk_df3), method = "color",xlim = c(0, 1), ylim = c(0, 1), type= "upper",number.digits = 1, tl.cex = 0.5, tl.col = "gray50")

clean.dat <- smk_df3
write.csv(clean.dat, "C:/Users/ABC/Desktop/Ciggerate/CleanedFile_3May.csv")


################################### KNN  ####################################

rm(list=ls())

dat <- read.csv("C:/Users/ABC/Desktop/Ciggerate/CleanedFile_3May.csv", stringsAsFactors = F, header = T)
dim(dat)
str(dat)
dat4 <- dat[,-1]
n = nrow(dat4)

# PARTITIONING
set.seed(1) 
id.train = sample(1:n, nrow(dat4)*.6) 
id.test = setdiff(1:n, id.train) 
dat.train = dat4[id.train,]
dat.test = dat4[id.test,]
nrow(dat.test)
nrow(dat.train)

knn.bestK = function(train, test, y.train, y.test, k.max = 20) {
  k.grid = seq(1, k.max, 2)
  
  fun.tmp = function(x) {
    y.hat = knn(train, test, y.train, k = x, prob=F)
    return(sum(y.hat != y.test))
  }
  error = unlist(lapply(k.grid, fun.tmp))/length(y.test)
  out = list(k.optimal = k.grid[which.min(error)], error.min = min(error), error)
  return(out)
}


id1 = which(colnames(dat4) == 'SMK_CIG')
pr <- knn.bestK(dat.train[,-id1], dat.test[,-id1], dat.train$SMK_CIG, dat.test$SMK_CIG)
##create confusion matrix
tab <- table(pr,dat.test$SMK_CIG)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

################################### Logistic  ####################################

lr.train <- dat.train
lr.test <- dat.test

logistic <- glm(SMK_CIG~. , data=lr.train, family = "binomial")
summary(logistic)

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null  # Pseudo R-squared = 0.78
1-pchisq(2*(ll.proposed-ll.null), df=(length(logistic$coefficients)-1))  #p-value = 0
# we can thus interpret that the pseudo R-sq value is not just due to luck

predicted.data<- data.frame(probability.of.smk= logistic$fitted.values, smk = lr.train$SMK_CIG)
#sort dataframe from low probab to high probab
predicted.data <- predicted.data[order(predicted.data$probability.of.smk, decreasing = FALSE),]
#add a new column that has the rank of each sample
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
usePackage('cowplot')
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.smk))+
  geom_point(aes(color=smk), alpha=1, shape=4, stroke=2)+
  xlab("Index")+
  ylab("Predicted Probability of Smoking a Cigarette")
ggsave("CigSmoking.png")
