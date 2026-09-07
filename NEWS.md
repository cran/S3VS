# S3VS 1.1

* Corrected selected coef extraction for GLM using lasso/enet and lambda.1se issue in GLM with ncvreg.
* Added all changes similar to method <- match.arg(method) for all function arguments whose default values is a vector.
* Forwarded parallel argument from S3VS() to S3VS_GLM() or S3VS_SURV().
* Passed alpha into the instances of using VS_method_LM(), VS_method_GLM(), or VS_method_SURV().
* Corrected bridge_aft call as bridge_fit <- bridge_aft(y, X, gamma = gamma).
* Finished unfinshed parts of AFTREG branch in VS_method_SURV() and pred_S3VS_SURV().
* Added missing “AFTREG” option in vsel_method() and missing “COXGLMNET” option in pred_S3VS_SURV().
* Simplified some functions.
* Removed "parallel" options for GLM.
* Corrected "percthresh" inputs in examples.
* Dependency on package 'mombf' was replaced by dependency on package 'modelSelection'.  
