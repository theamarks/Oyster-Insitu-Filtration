# Random Forest Regression
# Predictors of Habitat Clearance Rate (Lhr^-1m^-2)


###### Estimate missing values (Imputation) 
#In our data we are missing 4 out of 400ish values.  (cells)
#Because we have limited observations but a high number of variables simply
#removing incomplete observations is too costly

#For imputation we are doing a non parametric random forest imputation.  See missForest documentation for details/references.  
#Stekhoven, D.J. and Buehlmann, P. (2012), "missForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597

# Packages with version control

library(groundhog)
pkgs <- c("missForest",
         "rpart",
         "randomForest",
         "adabag",
         "magrittr",
         "xtable")
groundhog.library(pkgs, "2020-03-01")

# Import data
Analysis_data <- Master_analysis_table %>% 
  mutate(Date = mdy(Date),
         L_hr_m2 = L_Hr_m2,
         pcnt_Chl_rmvd = pcnt_Chl_rmvd,
         Temp_C_Up = Temp_C_Up,
         Sal_ppt_Up = Sal_ppt_Up,
         Turbidity_NTU_Up = Turbidity_NTU_Up,
         Avg_TPM_mg_L = Avg_TPM_mg_L,
         Avg_OC_Ratio = Avg_OC_Ratio,
         Chl_ug_L_Up = Chl_ug_L_up,
         Chl_ug_L_Down = Chl_ug_L_down,
         avg_depth_m = avg_depth_m,
         d_bw_sondes_m = d_bw_sondes_m,
         avg_m_sec = avg_m_hr/3600) %>%
  dplyr::select(Site, Date, Experiment, Temp_C_Up, Sal_ppt_Up, Turbidity_NTU_Up, Avg_TPM_mg_L, 
                Avg_OC_Ratio, Chl_ug_L_Up, Chl_ug_L_Down, avg_depth_m, d_bw_sondes_m, avg_m_sec, 
                pcnt_Chl_rmvd, L_hr_m2) %>% 
  dplyr::arrange(Site, Date)


# Imputation for 4 missing values (TPM & OC)
temp1 = Analysis_data[,c(1,2,3)] # temporarily remove data labels for imputation
temp2 = missForest(Analysis_data[,-c(1,2,3)])$ximp # Imputation of missing values
test = cbind(temp1,temp2) # bring back data labels 
# dimensions of data with labels removed (data only) 28 x 12 = 336 data cells
dim(Analysis_data[,-c(1,2,3)]) 
num_NA_values <- sum(is.na(Analysis_data)) # 4 missing values

colnames(test) <- c( "Site", "Date", "Experiment", "Temp","Salinity", "Turbidity",
                     "TPM", "OC", "Chl.up", "Chl.dn", 
                     "Depth","Distance", "Water velocity", "Chl.removal", 
                     "Clearance")

#Shellmaker 06-09 Neg Control, Shellmaker 05-22 Filtration and Deanza 4-17 Filtration just have astronomical outlier values for clearance.
#Lets ignore them for now as they complete dominate any model useful for comparison.

##### Habitat Clearance Rate Model - Random Forest


###  Use Domain knowledge to remove one single trial (NPD_2020_4_17) avg Chlup at sensor detection limit (+- 0.1 ug/L). Leave other extreme values.
test1 = test %>% 
  dplyr::filter(round(Chl.up,2) > 0.1 | round(Chl.up,2) < -0.1)

####KN edit 9-21-2021, 5-fold cross validation (repeated 1000 times) to assess tuning parameters based on SSR objective measure of fit)

#Note, I'm manually playing around with these to find a sweet spot, not going to document progression.
#Goal isn't to find best model, goal is to find competitive model.

trees=200
try = 1
split=5
cp.par = .08

SSR.sim = rep(0,200)
seed.vec = 2001:2200 # giving same numbers
for(k in 1:200){
  
  SSR = 0
  set.seed = seed.vec[k]
  test.ind.matrix = matrix(sample(1:25),5,5)
  for(j in 1:5){
    test.subset = test1[test.ind.matrix[j,],]
    control.subset = test1[-test.ind.matrix[j,],]
    
    control.model.rf = 			randomForest(Clearance~Temp+Salinity+Turbidity+TPM+OC+Site	,
                                       data=control.subset, # use data	with extreme values removed 
                                       ntree=trees,
                                       mtry=try,
                                       control=rpart.control(minsplit=		split,cp=cp.par))
    residuals = test1$Clearance[test.ind.matrix[j,]] - 		predict(control.model.rf,newdata=test.subset)
    SSR = SSR + sum(residuals^2)
  }
  SSR.sim[k] = SSR
}
mean(SSR.sim)

#mean SSR Value (cursory search only, not systematic).
#(trees,try,minsplit,cp) , mean(SSR)

#(100,3,2,.05) , 101.5m
#(200,3,2,.05) , 100.4m
#(50,3,2,.05) ,  108.5m
#(400,3,2,.05) , 106.1m

#Feel comfortable with 200 trees.

#(200,3,2,.05) , 100.4m
#(200,2,2,.05) , 97.8m
#(200,1,2,.05) , 92.3m
#(200,4,2,.05) , 104.4m

#Feel comfortable with mtry = 1

#(200,1,2,.05) , 92.3m
#(200,1,3,.05) , 84.5m
#(200,1,4,.05) , 91.9m
#(200,1,5,.05) , 97.9m

#(200,1,3,.05) , 84.5m
#(200,1,3,.02) , 90.0m
#(200,1,3,.08) , 89.3m

#Feel comfortable with cp=.05

#THEA, I only went through this once, if you want to spin your wheels you could go back through with 200,1,3,.05 as your starting point and redo ntrees, then mtry, then splits, then cp a second time (or even a third).  I think we've got most the low hanging fruit to be honest.  In general our original model was overfitting, most optimized parameters have been tuned towards less aggressive settings.

####KN edit end


############# Random Forest - WQ values affect on Filtration trails only #############

test2 <- test1[test1$Experiment=="Filtration",] # Filtration trials only

r.sq.vec = rep(0,1000)
rank.mat = data.frame(matrix(0,6,1001))
imp.mat = data.frame(matrix(0,6,1001))
rank.mat[,1] = c("OC","Salinity","Site","Temp","TPM","Turbidity")
imp.mat[,1] = c("OC","Salinity","Site","Temp","TPM","Turbidity")
for(i in 1:1000){
  
  model.rf.filter = randomForest(Clearance~Temp+Salinity+Turbidity+TPM+OC+Site,
                                 data=test2, # use data with extreme values removed 
                                 ntree=100,
                                 mtry=3,
                                 control=rpart.control(minsplit=2,cp=.05))
  model.rf.filter
  # plot(model.rf.filter)
  y.hat.filter = predict(model.rf.filter,newdata=test2) # model predicted values
  res.filter = test2$Clearance - y.hat.filter # residuals - difference between measured values and model predicted values 
  r.sq.filter = (var(test2$Clearance) - var(res.filter))/var(test2$Clearance)
  r.sq.filter # r squared value for random forest model
  ### R^2: 0.646
  
  r.sq.vec[i]=r.sq.filter
  
  ### Random Forest variable importance
  Var_import <- importance(model.rf.filter)
  Var_import <- Var_import[order(Var_import[,1], decreasing=TRUE),] # sort matrix highest to lowest - output names numeric
  Var_import_df <- data.frame(as.list(Var_import)) # convert named numeric to data.frame
  
  # dataframe with sorted importance values - highest to lowest
  Var_import_df %<>% 
    pivot_longer(cols = c(Turbidity, Temp, TPM, Salinity, OC, Site), 
                 names_to = "Variable", 
                 values_to = "Importance") %>% 
    arrange(desc(Importance)) 
  
  # convert importance values to percents
  sum_import <- sum(Var_import_df$Importance)
  Var_import_df %<>% 
    mutate(Importance_pct = (Importance / sum_import)*100) %>% 
    dplyr::select(-c(Importance))
  Var_import_df$rank = 1:6
  Var_import_df = Var_import_df[order(Var_import_df$Variable),]
  imp.mat[,i+1] = Var_import_df$Importance_pct
  rank.mat[,i+1] = as.numeric(Var_import_df$rank)
  
}
# alph order for variables
apply(rank.mat[,2:1001],1, mean)   

output = data.frame(variable = c("OC","Salinity","Site","Temp","TPM","Turbidity"),
                    rank_avg = apply(rank.mat[,2:1001],1,mean),
                    rank_sd = apply(rank.mat[,2:1001],1,sd),
                    imp_avg = apply(imp.mat[,2:1001],1,mean),
                    imp_sd = apply(imp.mat[,2:1001],1,sd))

output <- output %>% 
  arrange(rank_avg)

output_table <- xtable(output, label = NULL)
