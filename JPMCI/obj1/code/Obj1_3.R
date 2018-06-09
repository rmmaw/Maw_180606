rm(list=ls())
data<-read.csv("jpmc_rse_assignment_data.csv",sep=",",header=T)
dim(data)



############# Test option iii) #################
# ER34104 is the age of the individual as of the 2011 interview, ER34204 is the age of individual as of the 2013 
# interview 

age_11 <- data[7]
age_13 <- data[67]
summary(age_11)
summary(age_13)

# One interesting find here is that in 2011 an individual was entered as 1,004 years old, which is obviously
# a mistake. This data point should probably be removed. It does prove that the document is not absent of data 
# entry issues. 

# In the survey, 999 (refusal, NA, DK) and 0 (Inap.) represents an unknown age. So we will remove these entries. 
clean_11 <- as.data.frame(age_11[! age_11$ER34104 %in% c(0,999,1004), ])
clean_13 <- as.data.frame(age_13[! age_13$ER34204 %in% c(0,999), ])

# 1stQ = 14, Median = 29, Mean = 32.15, 3rdQ = 48
summary(clean_11)
hist(clean_11[,1],col="red",freq=T,xlim=c(1,100),main ='2011 Survey', xlab = "Age")

# 1stQ = 14, Median = 29, Mean = 32.17, 3rdQ = 49
summary(clean_13)
hist(clean_13[,1],col="red",freq=T,xlim=c(1,100),main ='2013 Survey', xlab = "Age")

# As we can see from the histograms above, the bucket "1" represents a good portion of surveys in both years. 
# This "1" maps to an individual from the age of 0-2 years old. After doing some research, it seems that this is 
# an overstatement of the US population in this age band.  This raises some concern as to why this occurred, and
# may require more investigation. If we remove the 0-2 year old’s from the data, the data better represents the 
# population, as seen below. 

clean_11 <- as.data.frame(age_11[! age_11$ER34104 %in% c(0,999,1004,1), ])
clean_13 <- as.data.frame(age_13[! age_13$ER34204 %in% c(0,999,1), ])
hist(clean_11[,1],col="red",freq=T,main ='2011 Survey', xlab = "Age")
hist(clean_13[,1],col="red",freq=T,main ='2013 Survey', xlab = "Age")

# It seems that after some cleansing the two years of interviews seemed to be mostly representative of our 
# population. 

############# Test option iii) #################
#ER34144 represents the accuracy of the social security income, i.e. if it 
# was imputed manually, and how. ER34144B represents the same information for total labor income. There seems 
# to be four other variables (ER34144A,ER34144C,ER34144D, and ER34144E) that may, or may not be manually altered. 
# Unlike the first two, there is no explanation variable to say exactly how. I will try to work these three 
# variables into my analysis. 

total <- nrow(data)
accSocialInc <- data[47]
totLaborInc<- data[48]
accLaborInc<- data[49]
totAssetInc<- data[50]
totTaxIn<- data[51]
totTranferIn<- data[52]

# Remove the values that weren’t imputed manually. 
manualPerSocial<- nrow(as.data.frame(accSocialInc[! accSocialInc$ER34144 %in% 0, ]))/total
manualPerLabor<- nrow(as.data.frame(accLaborInc[! accLaborInc$ER34144B %in% 0, ]))/total

# As we can see the percentage of manually altered data in these two variables are pretty low around 
# .07% for Social Income and 1.5% for Labor Income. 

manualPerSocial
manualPerLabor

# It seems that these variables have a high amount of "0"s or unknown information. Let's find the exact 
# percentage. This could be an issue if we try to draw conclusions from this information. 

unknownTotLaborInc <- nrow(as.data.frame(totLaborInc[ totLaborInc$ER34144A %in% 0, ]))/total
unknownTotAssetInc <- nrow(as.data.frame(totAssetInc[ totAssetInc$ER34144C %in% 0, ]))/total
unknownTaxInc <- nrow(as.data.frame(totTaxIn[ totTaxIn$ER34144D %in% 0, ]))/total
unknownTotTranferInc <-nrow(as.data.frame(totTranferIn[ totTranferIn$ER34144E %in% 0, ]))/total

# These numbers below are cause for concern as far as strength of conclusions drawn from the information
# the survey provided. Conclusions including total Labor Income, total Asset Income, total Taxable Income,
# or, total Transfer Income should be scrutinized, since about 95% of values were represented as "0".
# There are many circumstances that play into this "0" such as, moving out, death, or divorce. 

unknownTotLaborInc
unknownTotAssetInc
unknownTaxInc
unknownTotTranferInc

# For example, it could be misleading to project conclusions found from the numbers below because 93.27% of 
# the observations were labeled "0". 

LabIncome <- as.data.frame(totLaborInc[! totLaborInc$ER34144A %in% 0, ])
summary(LabIncome)

# A recommendation I would make would be to have 0 represent an income of 0, and find another 
# number to represent missing information. This way, I would feel better about the strength of conclusions made.

############# Test option i) #################
#ER34104, ER34105, and ER34106 map to age, month, and year of birth from 2011.
# Similarly, ER34204, ER34205, and ER34206 map to the dame information from the 2013 interview. 
# I will also pull in ER34103 and ER34203, which is the relationship to Head at time of 2011 and 2013 interview.

ageCheck <- data[c(6:9,66:69)]

# The first step is to remove any row of data where either relationship to head is 0 because we won't be able to 
# compare birthdates with empty values. 

ageCheck <- ageCheck[ageCheck$ER34103!=0 & ageCheck$ER34203!=0, ]

# Now I will add a conditional flag column to the end indicating if the relationship to head is the same 
# in each year. 

ageCheck$RelHeadFlag <- 1
for (i in 1:nrow(ageCheck)){
  if (ageCheck$ER34103[i] == ageCheck$ER34203[i] ){
    ageCheck$RelHeadFlag[i] <- 0
  }
}
sum(ageCheck$RelHeadFlag)/nrow(ageCheck)

# 8.8% of the data remaining theoretically had a different person present for the second interview. Checking
# the data, this doesn't seem to be correct. Some data points tagged 0 have similar birthdates. I'm 
# assuming some individual's relationship to head were either incorrectly labeled, or the relationship changed. 
# So, I will perform the same process to determine if the birth month is the same for those individuals that 
# were present for both interviews. There isn't a hard and fast rule as far as age goes, because depending on
# what date the interviews were conducted we could see differences of 1, 2, or 3 years of age. It also seems 
# that year of birth is shifted 5 years, so I will create a flag marking any difference other than 5. 

# 0 means they are labeled as different people, 1 means they were labeled as the same people, but had different
# birth months. This would suggest an issue with the data. 

ageCheck$MonthFlag <- 0
for (i in 1:nrow(ageCheck)){
  # if the people at each interview are labeled with same relationship to head
  if(ageCheck$RelHeadFlag[i] == 0){
    # but they have different birth months flag them
    if(ageCheck$ER34105[i] != ageCheck$ER34205[i]){
      ageCheck$MonthFlag[i] <- 1
    }
  }
}

# Only 2 cases occurred where the same person (based on the relationship to head) was 
# present for both interviews and was recorded having different birth months. This is a good sign for having 
# matched surveys together correctly. These two data points should be considered for deletion. 

sum(ageCheck$MonthFlag)/nrow(ageCheck)

# Now I will perform the same process on Birth Year. These rows seem to have a consistent 5 year difference 
# which might be explained by the difference between the 2009 and 2013 survey, so I will assume that this means 
# they have entered the correct birth year. I will flag any entries that are labeled as the same person, but 
# have a difference other than 5 years in birthdate. 

ageCheck$BirthYearFlag <- 0
for (i in 1:nrow(ageCheck)){
  # if the people at each interview are labeled with same relationship to head
  if (ageCheck$RelHeadFlag[i] == 0){
    # but they have different birth years, label them 1
    if(abs(ageCheck$ER34106[i] - ageCheck$ER34206[i]) !=5){
      ageCheck$BirthYearFlag[i] <- 1
    }
  }
}

# It seems that 39 rows of data have the birthdate entered incorrectly. From an initial look, it may be due to 
# the birth year being labeled "9999" or NA;DK. These data points may be candidates for deletion to strengthen 
# findings.

sum(ageCheck$BirthYearFlag)/nrow(ageCheck)

# Overall, I would say that the data has been transformed/matched up pretty well. There are some rows 
# that may require further investigation, or simply just deletion. 