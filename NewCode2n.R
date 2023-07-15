library(GGally)
library(ggplot2)
library(Hmisc)
library(coin)
#install.packages("mdatools")
library(mdatools)
#install.packages("mice")
library(mice) #ordinal imp


set.seed(123)
#main
getwd()
dir()
setwd("E:/Lakni UOC/3rd Year/2nd sem/ST 3082 Stat learning I/data analysis project 1/project 1/NewCode2")
df.main = read.csv("dementia_dataset.csv")
summary(df.main)


#Remove duplicates
data_without_duplicates = unique(df.main)
df.main = data_without_duplicates

#Drop hand
table(df.main$Hand)
df.main$Hand = NULL

#Drop Ids
subject_id = df.main$Subject.ID
MRI_id = df.main$MRI.ID

df.main$Subject.ID = NULL
df.main$MRI.ID = NULL

#Checking for null values
sort(apply(df.main, 2, function(x){sum(is.na(x))}), decreasing = TRUE)
sum(is.na(df.main))
##df.main = na.omit(df.main)

#imputation mmse
df.main$MMSE = as.numeric(impute(df.main$MMSE,"random"))
sum(is.na(df.main))

#imputation ses
#df.main$SES = as.numeric(impute(df.main$SES,"random"))
imp = mice(df.main)
# get the completed dataset
df.main <- complete(imp)
df.main

summary(df.main)
table(df.main$SES)

#fig 1
table(df.main$Group,df.main$CDR)
YD0 = sum(ifelse(df.main$CDR == "0" & df.main$Group == "Demented",1,0))
YD5 = sum(ifelse(df.main$CDR == "0.5" & df.main$Group == "Demented",1,0))
YD1 = sum(ifelse(df.main$CDR == "1" & df.main$Group == "Demented",1,0))
YD2 = sum(ifelse(df.main$CDR == "2" & df.main$Group == "Demented",1,0))
YN0 = sum(ifelse(df.main$CDR == "0" & df.main$Group == "Nondemented",1,0))
YN5 = sum(ifelse(df.main$CDR == "0.5" & df.main$Group == "Nondemented",1,0))
YN1 = sum(ifelse(df.main$CDR == "1" & df.main$Group == "Nondemented",1,0))
YN2 = sum(ifelse(df.main$CDR == "2" & df.main$Group == "Nondemented",1,0))
YC0 = sum(ifelse(df.main$CDR == "0" & df.main$Group == "Converted",1,0))
YC5 = sum(ifelse(df.main$CDR == "0.5" & df.main$Group == "Converted",1,0))
YC1 = sum(ifelse(df.main$CDR == "1" & df.main$Group == "Converted",1,0))
YC2 = sum(ifelse(df.main$CDR == "2" & df.main$Group == "Converted",1,0))

CDRBar = data.frame(Group=c(rep("Demented",4),rep("Nondemented",4),rep("Converted",4)),
                    CDR=rep(c("0","0.5","1","2"),3),
                    Count=c(YD0,YD5,YD1,YD2,YN0,YN5,YN1,YN2,YC0,YC5,YC1,YC2))
ggplot(CDRBar, aes(fill=CDR, y=Count, x=Group)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Group with CDRs")

#convert group into nondem & demented
df.main$GroupN = ifelse(df.main$Group == "Demented","Demented","Nondemented")
describe(df.main)

#Convert the group variable into a numerical representation
#cat_var = factor(df.main$Group)
#df.main$GroupNum = as.numeric(cat_var) -1

#Convert the MMSE variable to 5 groups
#for(i in 1:nrow(df.main)){
 # if(df.main$MMSE[i]<10){
  #  df.main$MMSEgrp[i]="0-9"
   # } else if(df.main$MMSE[i]>=10 & df.main$MMSE[i]<=20){
    #df.main$MMSEgrp[i]="10-20"
    #} else if(df.main$MMSE[i]>=21 & df.main$MMSE[i]<25){
    #df.main$MMSEgrp[i]="21-24"
    #} else{
    #df.main$MMSEgrp[i]="25 or more"
    #}
  #}
#describe(df.main)

##splitting data into training and test
n_train = round(0.8*nrow(df.main))
train_indices = sample(1:nrow(df.main),n_train)
trainData = df.main[train_indices,]
testData = df.main[-train_indices,]


barchart(trainData$GroupN)

table(trainData$GroupN)







#####fig 2 Grp vs Educ box plot
ggplot(trainData, aes(x=GroupN, y=EDUC)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs Years of education")
#
ggplot(trainData, aes(x=GroupN, y=EDUC))+
  geom_point(col = "blue")+
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  # geom_point(stat = "summary", fun.y = "median", col = "green", size = 5)+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs EDUC")

# Run the Shapiro test
table(trainData$GroupN,trainData$CDR)
group_A = trainData$EDUC[trainData$GroupN == "Demented"]
group_B = trainData$EDUC[trainData$GroupN == "Nondemented"]
shapiro.test(group_A)
shapiro.test(group_B)
#null the data is likely to have come from a normal distribution
#we rejected null here : not normal
#Run the Kruskal-Wallis test
kruskal.test(EDUC ~ GroupN, data = trainData)
#null hypothesis the medians are equal
#rejected null : not equal medians







#####fig 2 Grp vs Age box plot
ggplot(trainData, aes(x=GroupN, y=Age)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs Age")
#
ggplot(trainData, aes(x=GroupN, y=Age))+
  geom_point(col = "blue")+
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  # geom_point(stat = "summary", fun.y = "median", col = "green", size = 5)+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs Age")

# Run the Shapiro test
group_A = trainData$Age[trainData$GroupN == "Demented"]
group_B = trainData$Age[trainData$GroupN == "Nondemented"]
shapiro.test(group_A)
shapiro.test(group_B)
#null the data is likely to have come from a normal distribution
#do not reject : normal
#Run the t test
t.test(group_A,group_B)
#null hypothesis the means are equal
#do not reject null : equal means

#fig .1
ggplot(trainData, aes(x=GroupN, y=Age, fill=M.F)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs Age with M/F")







####fig Gender vs Grp
table(trainData$GroupN,trainData$M.F)
Y1M = sum(ifelse(trainData$GroupN == "Demented" & trainData$M.F == "M",1,0))
Y1F = sum(ifelse(trainData$GroupN == "Demented" & trainData$M.F == "F",1,0))
Y2M = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$M.F == "M",1,0))
Y2F = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$M.F == "F",1,0))
GroupBar = data.frame(Group=c(rep("Demented",2),rep("Nondemented",2)),
                      M.F=rep(c("M","F"),2),
                      Count=c(Y1M,Y1F,Y2M,Y2F))
#ggplot(GroupBar, aes(fill=M.F, y=Count, x=Group)) + 
 # geom_bar(position="dodge", stat="identity") +
  #ggtitle("Group with Male/Female grouped")
#fig .1
ggplot(GroupBar, aes(fill=M.F, y=Count, x=Group)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Group with Male/Female grouped")
table(trainData$M.F,trainData$Group)
#fig 1 new
GroupBar = data.frame(M.F=c(rep("Male",2),rep("Female",2)),
                      Group=rep(c("Demented","Nondemented"),2),
                      Count=c(Y1M,Y2M,Y1F,Y2F))
#fig .1
ggplot(GroupBar, aes(fill=Group, y=Count, x=M.F)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Group with Male/Female grouped")

table(trainData$M.F)



#fig Age vs Gender
ggplot(trainData, aes(x=M.F, y=Age)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Gender vs Age")
#testing
group_A = trainData$Age[trainData$M.F == "F"]
group_B = trainData$Age[trainData$M.F == "M"]
shapiro.test(group_A)
shapiro.test(group_B)
# Run the two-sample t-test
t.test(group_A, group_B)

#fig Educ vs Gender
ggplot(trainData, aes(x=M.F, y=EDUC)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Gender vs EDUC")
#testing
group_A = trainData$EDUC[trainData$M.F == "F"]
group_B = trainData$EDUC[trainData$M.F == "M"]
shapiro.test(group_A)
shapiro.test(group_B)
#Run the Kruskal-Wallis test
kruskal.test(EDUC ~ M.F, data = trainData)
#null hypothesis i.e. the medians are equal
#no association


#fig SES vs Gender
table(trainData$M.F,trainData$SES)
YM1 = sum(ifelse(trainData$M.F == "M" & trainData$SES == 1,1,0))
YM2 = sum(ifelse(trainData$M.F == "M" & trainData$SES == 2,1,0))
YM3 = sum(ifelse(trainData$M.F == "M" & trainData$SES == 3,1,0))
YM4 = sum(ifelse(trainData$M.F == "M" & trainData$SES == 4,1,0))
YM5 = sum(ifelse(trainData$M.F == "M" & trainData$SES == 5,1,0))
YF1 = sum(ifelse(trainData$M.F == "F" & trainData$SES == 1,1,0))
YF2 = sum(ifelse(trainData$M.F == "F" & trainData$SES == 2,1,0))
YF3 = sum(ifelse(trainData$M.F == "F" & trainData$SES == 3,1,0))
YF4 = sum(ifelse(trainData$M.F == "F" & trainData$SES == 4,1,0))
YF5 = sum(ifelse(trainData$M.F == "F" & trainData$SES == 5,1,0))
GroupBar = data.frame(Gender=c(rep("M",5),rep("F",5)),
                      SES=rep(c("1","2","3","4","5"),2),
                      Count=c(YM1,YM2,YM3,YM4,YM5,YF1,YF2,YF3,YF4,YF5))
ggplot(GroupBar, aes(fill=SES, y=Count, x=Gender)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("SES with Male/Female grouped")
#fig .1
ggplot(GroupBar, aes(fill=SES, y=Count, x=Gender)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("SES with Male/Female grouped")



#fig grp vs ses
YM1 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 1,1,0))
YM2 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 2,1,0))
YM3 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 3,1,0))
YM4 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 4,1,0))
YM5 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 5,1,0))
YF1 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 1,1,0))
YF2 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 2,1,0))
YF3 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 3,1,0))
YF4 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 4,1,0))
YF5 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 5,1,0))
GroupBar = data.frame(GroupN=c(rep("Nondemented",5),rep("Demented",5)),
                      SES=rep(c("1","2","3","4","5"),2),
                      Count=c(YM1,YM2,YM3,YM4,YM5,YF1,YF2,YF3,YF4,YF5))
ggplot(GroupBar, aes(fill=SES, y=Count, x=GroupN)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("SES with Group grouped")
table(trainData$EDUC,trainData$SES)

#educ and ses association
ggplot(trainData, aes(x=as.factor(SES), y=EDUC)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("EDUC vs SES")

table(trainData$EDUC,trainData$SES)
#test
group_A = trainData$EDUC[trainData$SES == "1"]
group_B = trainData$EDUC[trainData$SES == "2"]
group_C = trainData$EDUC[trainData$SES == "3"]
group_D = trainData$EDUC[trainData$SES == "4"]
group_E = trainData$EDUC[trainData$SES == "5"]
shapiro.test(group_A)
shapiro.test(group_B)
shapiro.test(group_C)
shapiro.test(group_D)
shapiro.test(group_E)
#null the data is likely to have come from a normal distribution
#not normal
#kw test
kruskal.test(EDUC ~ SES, data = trainData)
#have association







#fig MMSE vs Group
ggplot(trainData, aes(x=GroupN, y=MMSE))+
  geom_point(col = "blue")+
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
 # geom_point(stat = "summary", fun.y = "median", col = "green", size = 5)+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs MMSE")
#
ggplot(trainData, aes(x=GroupN, y=MMSE)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs MMSE")
#fig .1
ggplot(trainData, aes(x=GroupN, y=MMSE, fill=M.F)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs MMSE with M/F")






#fig MMSEn vs Group
#table(trainData$GroupN,trainData$MMSEgrp)
#YM1 = sum(ifelse(trainData$GroupN == "Demented" & trainData$MMSEgrp == "0-9",1,0))
#YM2 = sum(ifelse(trainData$GroupN == "Demented" & trainData$MMSEgrp == "10-20",1,0))
#YM3 = sum(ifelse(trainData$GroupN == "Demented" & trainData$MMSEgrp == "21-24",1,0))
#YM4 = sum(ifelse(trainData$GroupN == "Demented" & trainData$MMSEgrp == "25 or more",1,0))
#YF1 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$MMSEgrp == "0-9",1,0))
#YF2 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$MMSEgrp == "10-20",1,0))
#YF3 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$MMSEgrp == "21-24",1,0))
#YF4 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$MMSEgrp == "4",1,0))
#GroupBar = data.frame(Group=c(rep("Demented",4),rep("Nondemented",4)),
 #                     MMSEgrp=rep(c("0-9","10-20","21-24","25 or more"),2),
  #                    Count=c(YM1,YM2,YM3,YM4,YF1,YF2,YF3,YF4))
#ggplot(GroupBar, aes(fill=MMSEgrp, y=Count, x=Group)) + 
 # geom_bar(position="dodge", stat="identity") +
  #ggtitle("MMSEgrp with Group")
#fig .1
#ggplot(GroupBar, aes(fill=MMSEgrp, y=Count, x=Group)) + 
 # geom_bar(position="fill", stat="identity") +
  #ggtitle("MMSEgrp with Group")







#fig SES vs Group
table(trainData$GroupN,trainData$SES)
YM1 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 1,1,0))
YM2 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 2,1,0))
YM3 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 3,1,0))
YM4 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 4,1,0))
YM5 = sum(ifelse(trainData$GroupN == "Demented" & trainData$SES == 5,1,0))
YF1 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 1,1,0))
YF2 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 2,1,0))
YF3 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 3,1,0))
YF4 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 4,1,0))
YF5 = sum(ifelse(trainData$GroupN == "Nondemented" & trainData$SES == 5,1,0))
GroupBar = data.frame(Group=rep(c("Demented","Nondemented"),5),
                      SES=c(rep("1",2),rep("2",2),rep("3",2),rep("4",2),rep("5",2)),
                      Count=c(YM1,YF1,YM2,YF2,YM3,YF3,YM4,YF4,YM5,YF5))

ggplot(GroupBar, aes(fill=Group, y=Count, x=SES)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("SES with Group")















#fig nWBV vs Group
ggplot(trainData, aes(x=GroupN, y=nWBV))+
  geom_point(col = "blue")+
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  # geom_point(stat = "summary", fun.y = "median", col = "green", size = 5)+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs nWBV")
#
ggplot(trainData, aes(x=GroupN, y=nWBV)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs nWBV")
#
ggplot(trainData, aes(x=GroupN, y=nWBV, fill=M.F)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("nWBV by Group and M/F")



#nWBV and gender test
group_A = trainData$nWBV[trainData$M.F == "F"]
group_B = trainData$nWBV[trainData$M.F == "M"]
shapiro.test(group_A)
shapiro.test(group_B)
#Run the Kruskal-Wallis test
kruskal.test(nWBV ~ M.F, data = trainData)
#null hypothesis i.e. the medians are equal
#association

#nwbv and grp association
group_A = trainData$SES[trainData$GroupN == "Nondemented"]
group_B = trainData$SES[trainData$GroupN == "Demented"]
shapiro.test(group_A)
shapiro.test(group_B)
#not normal
#kw test
kruskal.test(nWBV~GroupN, data = trainData)
#association





#fig eTIV vs Group
ggplot(trainData, aes(x=GroupN, y=eTIV))+
  geom_point(col = "blue")+
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  # geom_point(stat = "summary", fun.y = "median", col = "green", size = 5)+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs eTIV")
#
ggplot(trainData, aes(x=GroupN, y=eTIV)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs eTIV")







#fig ASF vs Group
ggplot(trainData, aes(x=GroupN, y=ASF))+
  geom_point(col = "blue")+
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  # geom_point(stat = "summary", fun.y = "median", col = "green", size = 5)+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs ASF")
#
ggplot(trainData, aes(x=GroupN, y=ASF)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", col = "red", size = 5) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Group vs ASF")







#fig hist of age
ggplot(trainData, aes(fill=M.F, x=Age)) + 
  geom_histogram()








#fig corr plots
df.quant = subset(trainData, select = c(Visit, MR.Delay, Age, EDUC, eTIV, nWBV, ASF,MMSE))
ggcorr(df.quant, method = c("everything", "pearson"))
cor(df.quant)
#write.csv(cor(df.quant),'table.csv')
ggpairs(df.quant, title="correlogram with ggpairs()")






trainData$GroupNum = ifelse(trainData$GroupN=="Demented",1,0)
trainData$M.FNum = ifelse(trainData$M.F=="M",1,0)

trainDataP = subset(trainData, select = -c(Group, GroupN, M.F, CDR))#,MMSEgrp))

xc = trainDataP[-10]
yc = trainDataP[10]
ModelPLS = pls(xc, yc, scale = TRUE, cv=5, info = "Dementia prediction")
summary(ModelPLS)
plot(ModelPLS)
plotXScores(ModelPLS,show.label = F)
plotXYLoadings(ModelPLS,show.label = TRUE)
plotVIPScores(ModelPLS,ncomp = 3, type = "h",show.label = TRUE)
summary(ModelPLS$coeffs)
plot(ModelPLS$coeffs,show.label = TRUE)

