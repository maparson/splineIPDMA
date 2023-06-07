#--------------------------------#
# Mvmeta Script
#--------------------------------#  

mvmeta <- 
  function(
    dat = NULL,
    studyID.var = "studyID",
    time.var = "timeScale"
  ) 
{
    
    studyIDs <- dat %>% pull(get(studyID.var)) %>% unique()
    nStudy <- studyIDs %>% length()

    # Get the empty final matrix to plot   
    finalMat <- tibble(
      !!studyID.var := numeric(), # need this to pass character string as new variable
      !!time.var := numeric(),
      pred = numeric(),
      outcome = numeric(),
      se = numeric(),
      lower = numeric(),
      upper = numeric()
    )
    
    #- Get the FP model over all studies
    formAll <- paste("outcome ~ fp(", time.var, ", df = 4)")
    fpAll <- mfp(data = dat, formula = as.formula(formAll), family = "binomial")
    powersAll <- fpAll$powers[is.na(fpAll$powers) == FALSE] 
    nPowers <- length(powersAll)
      
    # Get the lists for the fitted coefficients and the variance matrices
    coefList <- list()
    vcovList <- list()
    icovList <- list()
    for(i in studyIDs){
      #- Get data for one study
      testDati <- dat %>% filter(studyID == i)
      

      
      #- Fit the model
      #--- get the formula for the model
      if(nPowers == 2) { # If two powers, have to make sure we deal with the case when they are equal
        if(powersAll[1] != powersAll[2]) {
      form1 <- paste("outcome ~ ", 
                     paste("FPone(FP =", 
                           paste0(powersAll, ", x = ", time.var, ")"), 
                           collapse = " + "))
        } else {
          form1 <- paste("outcome ~ FPone(FP = ",
                         powersAll[1],
                         ", x = ",
                         time.var,
                         ") + I(log(",
                         time.var,
                         ") * FPone(FP = ", powersAll[1], ", x = ", time.var, "))")
          
        }
      } else {
        form1 <- paste("outcome ~ ", 
                       paste("FPone(FP =", 
                             paste0(powersAll, ", x = ", time.var, ")"), 
                             collapse = " + "))
      }
      #--- fit the model with the basis variables
      fit1 <- glm(data = testDati,
                  formula = as.formula(form1),
                  family = "binomial")
      #--- get the within-study variance-covariance matrix
      # X1 <- model.matrix(fit1) 
      # V1 <- solve(t(X1) %*% X1)
      
      #--- get the one-person basis matrix for a single participant
      Timepoints <- testDati %>% pull(!!time.var) %>% unique() 
      nTimepoints <- Timepoints %>% length()
      TimepointsTibble <- tibble(
        !!time.var := Timepoints
      )
      if(nPowers == 2) {
        if(powersAll[1] != powersAll[2]) { # If the powers are different, just add em
      BasisTibble <- tibble( # Get the basis matrix including the intercept for matrix algebra
          intercept = 1,
          fp1 = FPone(FP = powersAll[1], beta = 1, x = Timepoints),
          fp2 = FPone(FP = powersAll[2], beta = 1, x = Timepoints),         
        )
        } else { # If the powers are the same then we need to multiply the second one by log
          BasisTibble <- tibble( # Get the basis matrix including the intercept for matrix algebra
            intercept = 1,
            fp1 = FPone(FP = powersAll[1], beta = 1, x = Timepoints),
            fp2 = log(Timepoints)*FPone(FP = powersAll[2], beta = 1, x = Timepoints),         
          )          
        }
      } else {
        BasisTibble <- tibble( # Get the basis matrix including the intercept for matrix algebra
          intercept = 1,
          fp1 = FPone(FP = powersAll[1], beta = 1, x = Timepoints)
        )        
      }
      BasisMatrix <- as.matrix(BasisTibble)
      #--- get the predicted values from the first fit using the one-person basis
      pred1 <- predict(fit1, newdata = TimepointsTibble, se.fit = TRUE, type = "link")
      #--- get the within-study variance-covariance matrix
      vcov1 <- vcov(fit1)
      icov1 <- solve(vcov1)
      #--- get the pointwise variance/se at each timepoint
      vars1 <- apply(BasisMatrix, MARGIN = 1, FUN = function(x) {t(as.matrix(x)) %*% vcov1 %*% as.matrix(x)})
      se1 <- sqrt(vars1)
      #-- save the stuff for the overall MA
      coef1 <- coef(fit1)
      coefList[[length(coefList)+1]] <- coef1
      vcovList[[length(vcovList)+1]] <- vcov1 
      icovList[[length(icovList)+1]] <- icov1       
      #-- save all the results
      graphTesti <- tibble(
        !!studyID.var := i,
        !!time.var := Timepoints,
        se = se1,
        pred = pred1$fit,
        outcome = fit1$family$linkinv(pred),
        lower = fit1$family$linkinv(pred - 1.96*se),
        upper = fit1$family$linkinv(pred + 1.96*se)
      )
      finalMat <- finalMat %>% add_row(graphTesti)
    }
    
    # Now meta-analyse the study-level data
    icovsum <- Reduce("+", icovList)
    icovsumInv <- solve(icovsum)
    icovsumInv2 <- solve(icovsum %*% icovsum)
    WList <- list()
    betaHatList <- list()
    for(i in 1:nStudy) {
      # For beta-hat
      Wi <- icovsumInv %*% icovList[[i]]
      betaHati <- Wi %*% coefList[[i]]
      WList[[length(WList)+1]] <- Wi
      betaHatList[[length(betaHatList)+1]] <- betaHati
    }
    betaHat <- Reduce("+", betaHatList)
    preds <- as.matrix(BasisMatrix) %*% betaHat
    outcome <- fit1$family$linkinv(preds)
    
    x1 <- as.matrix(BasisMatrix)
    si <- apply(x1, 1, function(x) {t(x) %*% icovsumInv %*% x})
    sei <- sqrt(si)
    loweri <- fit1$family$linkinv(as.matrix(BasisMatrix) %*% betaHat - 1.96*sei)
    upperi <- fit1$family$linkinv(as.matrix(BasisMatrix) %*% betaHat + 1.96*sei)
    
    #-- get the MA results
    MAID <- max(studyIDs)+1
    graphTesti <- tibble(
      studyID = MAID,
      !!time.var := Timepoints,
      pred = as.numeric(preds),
      outcome = as.numeric(outcome),
      se = as.numeric(sei),
      lower = as.numeric(loweri),
      upper = as.numeric(upperi)
    )
    
    # Return the final matrix
    finalMat <- finalMat %>% add_row(graphTesti)
    return(finalMat)
    
    
  }


