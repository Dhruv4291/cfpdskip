library(caret)
pddata = read.csv('data_skip_PD_DPD_v2_09012018.csv')  #reading the file
pddata$X = NULL #deleting csv index
library(mlr)
summarytable = summarizeColumns(pddata) #summarytable of all the variables 
#changing the class of variables with levels
pddata$renewal_flag = as.factor(pddata$renewal_flag)
pddata$coh_flag = as.factor(pddata$coh_flag)
pddata$office_landline_flag = as.factor(pddata$office_landline_flag)
pddata$landline_flag = as.factor(pddata$landline_flag)
#checking the near zero variance variables which have only a single value among all the columns
#e.g term_loan_emi have all values 0
nzv <- nearZeroVar(pddata, saveMetrics= TRUE)
nzv = nzv[nzv$nzv == TRUE,]
nzv = nzv[nzv$zeroVar == TRUE,]
nzvvar = rownames(nzv)
#removing the nzv columns
pddata = pddata[,-which(colnames(pddata) %in% nzvvar)]

#variable analysis
''' 
x = pddata$business_to
x[which(x != 0)] = 1 
x = as.factor(x)
CrossTable(x,pddata$target_variable_30DPD) '''

'''
pl = ggplot(pddata, aes(x=non_oper_income_trend  )) + geom_density(aes(fill=target_variable_30DPD), alpha=0.7) + xlim(-2, 20)
pl
pddata$non_oper_income_trend = NULL
pl = ggplot(pddata, aes(x=direct_expenses_trend)) + geom_density(aes(fill=target_variable_30DPD), alpha=0.7) + xlim(-2, 10)
pl
'''

#deleting the useless variables
pddata$business_to = NULL
pddata$direct_expenses_trend = NULL
pddata$working_capital_limit_trend = NULL

#imputing the missing values in specific variables like specific loan amounts to zero
pddata$auto_loan_amt = as.numeric(pddata$auto_loan_amt)
pddata$home_loan_amt = as.numeric(pddata$home_loan_amt)
pddata$personal_loan_amt = as.numeric(pddata$personal_loan_amt)
pddata$loan_against_prop_amt = as.numeric(pddata$loan_against_prop_amt)
pddata$auto_loan_amt[which(pddata$auto_loan_amt %in% NA)] = 0 
pddata$home_loan_amt[which(pddata$home_loan_amt %in% NA)] = 0 
pddata$loan_against_prop_amt[which(pddata$loan_against_prop_amt %in% NA)] = 0 
pddata$personal_loan_amt[which(pddata$personal_loan_amt %in% NA)] = 0 
pddata$outstanding_loans = as.numeric(pddata$outstanding_loans)
pddata$outstanding_loans[which(pddata$outstanding_loans %in% NA)] = 0
pddata$current_maturity_ltd_trend[which(pddata$current_maturity_ltd_trend %in% NA)] = 0
pddata$loan_adv_afl_borrowings_ratio[which(pddata$loan_adv_afl_borrowings_ratio %in% NA)] = 0
pddata$debtors_gt_6_trend[which(pddata$debtors_gt_6_trend %in% NA)] = 0
pddata$borroiwings_other_trend[which(pddata$borroiwings_other_trend %in% NA)] = 0
pddata$other_income_trend[which(pddata$other_income_trend %in% NA)] = 0
pddata$non_oper_income_trend[which(pddata$non_oper_income_trend %in% NA)] = 0
pddata$avg_inventory_days_trend[which(pddata$avg_inventory_days_trend %in% NA)] = 0
pddata$receivables_trend[which(pddata$receivables_trend %in% NA)] = 0
pddata$liquidity_ratio_trend[which(pddata$liquidity_ratio_trend %in% NA)] = 0
pddata$interest_coverage_ratio_trend[which(pddata$interest_coverage_ratio_trend %in% NA)] = 0
pddata$earnings_interset_ratio_trend[which(pddata$earnings_interset_ratio_trend %in% NA)] = 0
pddata$tangible_networth_bank_borrowings_ratio[which(pddata$tangible_networth_bank_borrowings_ratio %in% NA)] = 0
pddata$working_cap_bank_borrowings_ratio[which(pddata$working_cap_bank_borrowings_ratio %in% NA)] = 0
pddata$creditors_turnover_trend[which(pddata$creditors_turnover_trend %in% NA)] = 0
pddata$avg_collection_period_trend[which(pddata$avg_collection_period_trend %in% NA)] = 0

summarytable = summarizeColumns(pddata)
#generating the variables which have less than 5% Nas and are to be imputed
names = summarytable[which(summarytable$na/1898 >0  ),c(1,3)]
names = names[which(names$na/1898 < 0.05   ),]
names$na = names$na/1898*100
write.csv(names,'nanamesimputed.csv')

library(MASS)
#deleting the constant variables and the other variables which should not be included
pddata$id = NULL
pddata$app_id = NULL
pddata$target_variable = NULL
pddata$DOB = NULL
pddata$FL_everDPD30 = NULL
pddata$pd_type = NULL
pddata$app_submitted_lo = NULL
pddata$rfl_start_lo = NULL
pddata$sign_up_lo = NULL
pddata$company_pan = NULL
pddata$company_profile = NULL
pddata$main_category = NULL
pddata$OWNER = NULL
pddata$loan_amt = NULL
pddata$industry_type =NULL
pddata$MONTH = NULL
pddata$pre_funding_dscr = NULL
pddata$dscr_years = NULL
#changing the NA factor level in factors
pddata$ASM_Name = as.character(pddata$ASM_Name)
pddata$ASM_Name[which(pddata$ASM_Name %in% NA)] = 'No ASM'
pddata$ASM_Name = as.factor(pddata$ASM_Name)
pddata$RSM_Name = as.character(pddata$RSM_Name)
pddata$RSM_Name[which(pddata$RSM_Name %in% NA)] = 'No RSM'
pddata$RSM_Name = as.factor(pddata$RSM_Name)

list = c("app_state","app_region" ,"region_SM", "location_SM")
for (i in 1:4)
  pddata[,which(colnames(pddata) == list[i])] = as.character(pddata[,which(colnames(pddata) == list[i])])
  
pddata$app_state[which(pddata$app_state %in% NA)] = 'Not Known'
pddata$app_region[which(pddata$app_region %in% NA)] = 'Not Known'
pddata$region_SM[which(pddata$region_SM %in% NA)] = 'Not Known'
pddata$location_SM[which(pddata$location_SM %in% NA)] = 'Not Known'

for (i in 1:4)
  pddata[,which(colnames(pddata) == list[i])] = as.factor(pddata[,which(colnames(pddata) == list[i])])


summarytable = summarizeColumns(pddata)

#Lumping the factor levels into more concentrated levels

library(forcats)
pddata$ASM_Name =  fct_lump(pddata$ASM_Name, n  = 6)
pddata$RSM_Name = fct_lump(pddata$RSM_Name, n =3)
pddata$app_state =  fct_lump(pddata$app_state, n =8)
pddata$location_SM =  fct_lump(pddata$location_SM, n =6)
pddata$state_CIBIL_all_promoters =  fct_lump(pddata$state_CIBIL_all_promoters, n =5)
pddata$operate_as = fct_lump(pddata$operate_as,n = 3)
pddata$renewal = as.factor(pddata$renewal)


list = c("max_shareholder_experience", "max_experience"    ,         "min_experience"            
         ,"max_shareholder_education",  "min_education"     ,         "max_education" ,"NumberOfApplicants" )

for (i in 1:7)
  pddata[,which(colnames(pddata) == list[i])] = as.factor(pddata[,which(colnames(pddata) == list[i])])

for (i in 1:7)
  pddata[,which(colnames(pddata) == list[i])] = fct_lump(pddata[,which(colnames(pddata) == list[i])], n = 3)



pddata$emi_bounces_l6m[which(pddata$emi_bounces_l6m %in% NA)] = 0

pddata$emi_bounces = as.factor(pddata$emi_bounces)
pddata$emi_bounces_l3m = as.factor(pddata$emi_bounces_l3m)
pddata$emi_bounces_l6m = as.factor(pddata$emi_bounces_l6m)
pddata$emi_bounces = fct_lump(pddata$emi_bounces,n=2)
pddata$emi_bounces_l3m = fct_lump(pddata$emi_bounces_l3m,n=1)
pddata$emi_bounces_l6m = fct_lump(pddata$emi_bounces_l6m,n=1)

summarytable = summarizeColumns(pddata)
#deleting the variables with more than 5% Nas after consulting with credit, which can be removed
names = summarytable$name[which(summarytable$na/1898 >0.05  )]
pddata = pddata[,-which(colnames(pddata) %in% names)]

#deleting the factor variables with mostly only one factor level
nzv <- nearZeroVar(pddata, saveMetrics= TRUE)
nzv = nzv[nzv$nzv == TRUE,]
nzvvar = rownames(nzv)
pddata = pddata[,-which(colnames(pddata) %in% nzvvar)]

summarytable = summarizeColumns(pddata)
#converting to factor and lumping
pddata$turnover_this_year = as.numeric(pddata$turnover_this_year)
pddata$profit_this_year = as.numeric(pddata$profit_this_year)
pddata$profit_prev_year = as.numeric(pddata$profit_prev_year)
for (i in 233:240)
  pddata[,i] = as.factor(pddata[,i])
for (i in 251:253)
  pddata[,i] = as.factor(pddata[,i])

pddata$office_ownership = fct_lump(pddata$office_ownership,n = 4)

#the variables which will be imputed in the next step
summarytable = summarizeColumns(pddata)
names = summarytable[which(summarytable$na/1898 >0  ),c(1,3)]
names = names[which(names$na/1898 < 0.05   ),]
names$na = names$na/1898*100
write.csv(names,'nanamesimputed.csv')

#imputation of all numeric variables with 5 iterations
library(mice)
tempData <- mice(pddata[,sapply(pddata,is.numeric)],m=1,maxit=5,meth='cart',seed=500)
a = complete(tempData)
pddata = pddata[,-which(colnames(pddata) %in% colnames(a))]
pddata = cbind(pddata,a)
pddata[is.na(pddata)] <- 0
write.csv(pddata,'finaldata.csv')
library(caTools)
set.seed(500)
split = sample.split(pddata$target_variable_30DPD,SplitRatio =0.85)
train = subset(pddata,split==T)
test = subset(pddata,split==F)


# setting  the parallel backend with two cores
library(doParallel)
cls = makeCluster(2)
registerDoParallel(cls)


#feature selection, no need to run again
'''control <- rfeControl(functions=rfFuncs, method="cv", number=5,allowParallel = TRUE ) #5 fold cross validation
sizes = c(1:255)
results <- rfe(target_variable_30DPD~., data = train ,sizes = sizes, rfeControl=control, metric = "ROC",maximize = TRUE)
library(Boruta)
set.seed(500)
boruta.train <- Boruta(target_variable_30DPD~., data = train,doTrace = 2,maxruns = 250)
final.boruta <- TentativeRoughFix(boruta.train)
borutavars = getSelectedAttributes(final.boruta)
'''
load(file = 'borutavars') #loading the vars selected by boruta
load(file ='rfepredictors') #loading the vars selected by rfe
rfemostimportant = rfepredictors[1:13] #most important features by selectsize function

#control : 5 fold cross validation 
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,allowParallel = TRUE)

#creating a data frame with one hot encoding of factors
new_tr = model.matrix(~.+0,data = train[,-34])
new_tr = as.data.frame(new_tr)
new_tr = new_tr[,which(colnames(new_tr) %in% rfemostimportant)]

#best found tuning grid
xgbgrid =expand.grid(nrounds = 92,eta = 0.05,max_depth = 2,gamma = 0,min_child_weight = 7,colsample_bytree =0.4)

#detach the mlr package  before running this so train function dont overlap
detach("package:mlr", unload=TRUE)
xgbmodel = train(y = train[,34],x = new_tr,method = "xgbTree",trControl = fitControl,metric = "ROC",tuneGrid = xgbgrid)

#creating one hot encoded test data
new_ts = model.matrix(~.+0,data = test[,-34])
new_ts = as.data.frame(new_ts)
new_ts = new_ts[,which(colnames(new_ts) %in% rfemostimportant)]

#predicting test data
pred = predict(xgbmodel,newdata = new_ts,type = 'prob')

#calculating AUC
library(pROC)
roc(test$target_variable_30DPD,pred$bad)

'''glmgrid = expand.grid(alpha = seq(0,1,length = 10), lambda = 10^seq(2,-2,length = 100))
glmmodel = train(y = train[,34],x = new_tr,method = "glmnet",trControl = fitControl,tuneGrid = glmgrid,metric = "ROC")

rfgrid = expand.grid(mtry = 2)
rfmodel = train(y = train[,34],x = new_tr,method = "ranger",trControl = fitControl,metric = "ROC",tuneGrid = rfgrid)


model_list <- caretList(
  y = train[,34],x = new_tr,
  trControl=fitControl,
  metric="ROC",
  tuneList=list(
    xgb=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 92,eta = 0.05,max_depth = 2,gamma = 0,min_child_weight = 7,colsample_bytree =0.4)),
    xgb=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 92,eta = 0.05,max_depth = 2,gamma = 0,min_child_weight = 7,colsample_bytree =0.4)),
    xgb=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 92,eta = 0.05,max_depth = 2,gamma = 0,min_child_weight = 7,colsample_bytree =0.4))))

modelCor(resamples(model_list))

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=fitControl)

new_ts = test[,which(colnames(test) %in% borutavars)]
new_ts = model.matrix(~.+0,data = new_ts)
new_ts = new_ts[,which(colnames(new_ts) %in% rfemostimportant)]
labels = as.numeric(test$target_variable_30DPD) - 1
dtest= xgb.DMatrix(data = new_ts,label = labels)

new_tr = train[,which(colnames(train) %in% borutavars)]
new_tr = model.matrix(~.+0,data = new_tr)
labels = as.numeric(train$target_variable_30DPD) - 1
dtrain= xgb.DMatrix(data = new_tr,label = labels)
params <- list(objective = "binary:logistic", eta=0.01, gamma=0, max_depth=2, min_child_weight=1, subsample=1, colsample_bytree=1,eval_metric = 'auc')
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 300, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20)


xgbgrid =expand.grid(nrounds = 92,eta = 0.05,max_depth = 2,gamma = 0,min_child_weight = 7,colsample_bytree =0.4)
xgbmodel = train(y = train[,34],x = new_tr,method = "xgbTree",trControl = fitControl,metric = "ROC",tuneGrid = xgbgrid)

[1] "pre_pbdit"                      "bankthroughput_vat"             "pre_dscr"                      
[4] "avg_cibil_score"                "dev_bt_vat1"                    "tot_l3m_online_sales"          
[7] "banking_tot_for_vat"            "debt_equity_ratio"              "bank_borrowings_networth_ratio"
  [10] "sales_growth"                   "lt_debt_networth_ratio"         "sales_assets_ratio"            
[13] "gross_profit_margin_ratio" '''     
