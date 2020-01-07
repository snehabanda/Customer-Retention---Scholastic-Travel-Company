library(mice)
library(VIM)


# First reading the data into R

data1 <- read.csv("exhibit1.csv", stringsAsFactors = FALSE)[1:2389,]
data2 <- read.csv("exhibit2.csv", stringsAsFactors = FALSE)[1:2389,]

df_merged <- merge(data1, data2, by=c("ID"))
table(df_merged$Cancelled.Pax)

df_merged$Retained <- as.factor(df_merged$Retained.in.2012.)
df_merged$cancellation <- df_merged$FRP.Cancelled/df_merged$FRP.Active

# Now changing the date columns into Posix elements

date_cols <- c("Departure.Date","Return.Date","Deposit.Date","Early.RPL","Latest.RPL","Initial.System.Date","FirstMeeting","LastMeeting")
df_merged[,date_cols] <- apply(df_merged[,date_cols], 2, function(x) as.Date(x, format="%m/%d/%Y"))

# Now let's start creating the features we need

unchanged_cols <- c("Is.Non.Annual.","Days","Tuition","School.Type","Parent.Meeting.Flag","MDR.High.Grade","Income.Level","School.Sponsor","SPR.New.Existing","NumberOfMeetingswithParents","DifferenceTraveltoLastMeeting","SchoolGradeTypeHigh","FPP.to.School.enrollment","FPP.to.PAX","SchoolSizeIndicator","Retained.in.2012.")

df_final1 <- df_merged[,unchanged_cols]
df_final1$frp.active <- df_merged$FRP.Active/df_merged$FPP
df_final1$cancellation <- df_merged$FRP.Cancelled/df_merged$FRP.Active
df_final1$cancelled.PAX <- df_merged$Cancelled.Pax/df_merged$FPP
df_final1$fpp <- cut(df_merged$FPP, breaks = c(-Inf,10,20,35,Inf), labels= c("small","critical","safe","large"), right = TRUE)
df_final1$trend <- as.factor(as.numeric(rowMeans(df_merged[,57:60], na.rm = TRUE) > rowMeans(df_merged[,57:59], na.rm = TRUE)))

impute <- mice(df_final1, seed=12345)

df_final_imp <- complete(impute, 2)
df_scaled <- df_final_imp
df_scaled[unlist(lapply(df_scaled, is.numeric))] <- scale(df_scaled[unlist(lapply(df_scaled, is.numeric))])



# Counting the number of times columns have the string value "NA"
apply(data1, 2, function(x) sum(x == "NA", na.rm = TRUE))
apply(data2, 2, function(x) sum(x == "NA", na.rm = TRUE))
# Found some in data1 and none in data2
# Went back and replaced all "NA" values with NA

# Now checking for NA values in columns
apply(data1, 2, function(x) sum(is.na(x), na.rm = TRUE))
apply(data2, 2, function(x) sum(is.na(x), na.rm = TRUE))


# Checking for data-types
str(data1)
str(data2)

complete.cases(df_scaled)

write.csv(df_scaled, "df_unscaled.csv", row.names = FALSE)

df_scaled <- setDT(df_scaled)
df_scaled <- df_scaled[,-df_scaled$frp.cancelled]

df_scaled$Retained <- df_scaled$Retained.in.2012.
model2 <- regsubsets(Retained ~ ., data= df_scaled, nbest=1, nvmax=19, method = "forward")
best <- summary(model2)
best$adjr2
bestm <- glm(Retained~ Is.Non.Annual.+SPR.New.Existing+Income.Level+fpp+Tuition+DifferenceTraveltoLastMeeting+SchoolGradeTypeHigh+frp.active+cancelled.PAX+FPP.to.School.enrollment+SchoolSizeIndicator+FPP.to.PAX, data=df_scaled, family=binomial)
summary(bestm)
table(df_scaled$Retained.in.2012.)
table(df_scaled$SchoolGradeTypeHigh)
