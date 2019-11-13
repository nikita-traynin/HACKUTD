install.packages("adabag");
install.packages("dplyr");
install.packages("caret");
install.packages("ggplot2");
library(adabag);
library(dplyr);
library(caret);
library(ggplot2);



#dataf = read.table("C:/Users/nikit/Documents/R/HackUTD/Performance_2008Q3.txt", sep = "|");

###Acquisitions table
#Acq = read.table("C:/Users/nikit/Documents/R/HackUTD/Acquisition_2008Q3.txt", sep = "|");

###Performance table
#Perf = read.table("C:/Users/nikit/Documents/R/HackUTD/Performance_2008Q3.txt", sep = "|");

###Isolate columns we want (7 out of the 31)
Perf_col = Perf[,c(1,2,4,5,6,12,13)];

###Convert dates to numerics
Periods = as.numeric(as.POSIXct(Perf_col[,2], format = "%m/%d/%Y"))
Perf_col$V2 = Periods;

###Extract only the latest entry fro each loan
groupQuant = Perf_col %>% group_by(V1) %>% summarize(n());
Perf_col_latest = Perf_col[cumsum(groupQuant$`n()`),];

###Replace NA's in zero balance code column with 0s
Perf_col_latest$V13[is.na(Perf_col_latest$V13)==TRUE] = 0;

###Drop rows with UPB NA's (only removes like 8 rows)
Perf_col_latest_UPB = Perf_col_latest[which(!is.na(Perf_col_latest$V5)),];

###Merge acquistion and performance datasets (the loans removed from Perf will not appear here either)
loandf = merge(Acq, Perf_col_latest_UPB, by="V1");

###count NA's in each column
colSums(is.na(loandf))

###default the NA's and delete others (DTI, borrower FICO) (as in sample notebook)
###In addition, Co-Borrower FICO score variable *removed* (ok?)
loandf_def = loandf;
loandf_def$V10[which(is.na(loandf_def$V10))] = loandf[which(is.na(loandf$V10)), "V9"];    #default CLTV value is set to the corresponding LTV value
loandf_def$V11[which(is.na(loandf_def$V11))] = 1;                                         #default num of borrowers is 1 (duh)
loandf_def$V21[which(is.na(loandf_def$V21))] = 0;                                         #default Insurance Percent is 0
loandf_def$V24[which(is.na(loandf_def$V24))] = 0;                                         #default Insurance Type is 0
loandf_def = loandf_def[, !(names(loandf_def) == "V23")];                                 #removed 23rd column aka Co-Borrow credit score
loandf_def = loandf_def[which(!is.na(loandf_def$V12.x)), ]                                #remove rows that have NA in the D.T.I. column
loandf_def = loandf_def[which(!is.na(loandf_def$V13.x)), ]                                #remove rows that have NA in the Borrower credit score column

###change format of certain columns to make computations faster
#loandf_def$V7
loandf_def$V7 = as.numeric(as.POSIXct(paste(loandf_def$V7, rep("/01", length(loandf_def$V7)), sep = ""), format = "%m/%Y/%d"));
loandf_def$V8 = as.numeric(as.POSIXct(paste(loandf_def$V8, rep("/01", length(loandf_def$V7)), sep = ""), format = "%m/%Y/%d"));

###temporarily remove tough rows (states, banks)
dftempA = loandf_def[,!(names(loandf_def) == "V19" | names(loandf_def) == "V3")]
###change numerics to factors as necessary
dftempA$V24 = as.factor(dftempA$V24);
dftempA$V13.y = as.factor(dftempA$V13.y);
###re-bin the zero balance codes into three different categories
levels(dftempA$V13.y) = list("Current"=c(0), "Prepaid"=c(1), "Underpermorming" = c("2","3","6","9","15","16"));
###remove column 22- only a single value (??)
dftempA = dftempA[,which(!(names(dftempA) == "V22"))];

###randomForest
set.seed(150)
rows1 = sample(1:nrow(dftempA), 5000);
rows2 = sample(1:nrow(dftempA), 10000);
dfSlice1 = dftempA[rows1,];
dfSlice2 = dftempA[rows2,];

modelA = randomForest(V13.y~., data = dfSlice1, proximity = TRUE, ntree = 500, mtry = 3);      ##As expected, worst model 
modelB = randomForest(V13.y~., data = dfSlice1, proximity = TRUE, ntree = 1000, mtry = 3);
modelC = randomForest(V13.y~., data = dfSlice2, proximity = TRUE, ntree = 500, mtry = 3);
modelD = randomForest(V13.y~., data = dfSlice2, proximity = TRUE, ntree = 1000, mtry = 3);     ##As expected, best model (most samples, most trees)

###Adaboosting
NumRows = sample(1:nrow(dftempA), 20000);
train = dftempA[NumRows[1:15000],];
test = dftempA[NumRows[15001:20000],];
adaModel = boosting(V13.y~., data = train, boos = TRUE, mfinal = 250);
adaPred = predict(
  , test);
importanceplot(adaPred2)