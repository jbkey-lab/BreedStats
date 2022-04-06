

xgboostTraitLoop = function(max_depth, min_child_weight, refresh_leaf, grow_policy, max_bin, max_leaves,
                            eta, nrounds,r2, subsample){


  max_depth = 15 #15
  min_child_weight = 0 #5
  refresh_leaf = 0
  grow_policy="lossguide"
  max_bin = 20000
  max_leaves = 60# 50
  eta = .2 #.2
  nrounds = 3000
  r2 = 0.6
  subsample = 1
  #4.39459

  gc()
  R2=r2

  # Prior hyperparameter values
  # sigmaE2 (residual variance)
  mode.sigE=R2*var(trainx2  %>% dplyr::select(feature) %>% as.matrix())

  # lambda
  mode.sigL=(1-R2)*var(trainx2 %>% dplyr::select(feature) %>% as.matrix())
  lambda.hat=sqrt(2*mode.sigE/mode.sigL/sum(colMeans(trainx2[,1:(ncol(trainx2))] )))

  alpha=(mean(trainx2  %>%
                dplyr::select(feature) %>%
                as.matrix())^2)/var(trainx2  %>% dplyr::select(feature) %>% as.matrix())

  gamma = stats::dgamma(trainx2  %>% dplyr::select(feature) %>% as.matrix(), shape=1)
  gamma=colMeans(gamma)

  dtrain <- xgboost::xgb.DMatrix(data = trainx2 %>% dplyr::select(-feature) %>% as.matrix(),
                                 label = trainx2[,"feature"])

  dtest <- xgboost::xgb.DMatrix(data = validatex2 %>% dplyr::select(-feature) %>% as.matrix(),
                                label=validatex2[, "feature"])

  parms = list(lambda =lambda.hat,
               alpha= alpha,
               gamma = gamma,
               eval_metric = "rmse",
              #  colsample_bytree = 0.7,
               min_child_weight = min_child_weight,
               max_depth = max_depth,
               refresh_leaf =refresh_leaf,
                grow_policy =grow_policy,
                max_bin = max_bin,
                max_leaves =max_leaves
               # #sampling_method = "gradient_based"
               # #scale_pos_weight = 1
               # subsample =subsample

               #updater = "grow_colmaker" #grow_gpu_hist
               #predictor = "cpu_predictor",  #gpu_predictor
               #num_parallel_tree = 5
               #single_precision_histogram = T

  )

  NCAA.stacked <- xgboost::xgb.train(data = dtrain,
                                     nthread = 8,
                                     objective = "reg:squarederror",
                                     booster = "gbtree",  eta = eta, #eta=0.8 for tree_method = auto
                                     tree_method = "hist", #"gpu_hist"
                                     print_every_n = 50,
                                     nrounds = nrounds,
                                     params = parms,
                                     #watchlist = list(val=dtest,train=dtrain)
                                     watchlist <- list(train=dtrain, test=dtest),
                                     early_stopping_rounds=1000

  )   #5.79

  #importance_matrix <- xgboost::xgb.importance(model = NCAA.stacked)
  # preds = predict(NCAA.stacked, validatex2 %>% dplyr::select(-feature) %>% as.matrix())
  # corr=round(cor(validatex2[, "feature"], preds)^2, 4)
  # xgboost::xgb.save(NCAA.stacked, paste0(fdp,season,"/",name,"_Valr2-",corr,"_xgboost.model"))

   preds = predict(NCAA.stacked, validatex2 %>% dplyr::select(-feature) %>% as.matrix())
   cat("r2 for Validate ALL is: ",cor(validatex2[, "feature"], preds)^2, "\n")
   cat("rmse for Validate ALL is: ",sqrt(mean((validatex2[, "feature"] -  preds)^2)), "\n")




  return(NCAA.stacked)
}







