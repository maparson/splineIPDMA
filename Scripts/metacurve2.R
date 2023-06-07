#--------------------------------#
# Metacurve Script
#--------------------------------#  

metacurve <- 
  function(
    dat = NULL,
    studyID.var = "studyID",
    time.var = "timeScale",
    ranInt = FALSE, # Use a random study-level intercept?
    ranSlo = FALSE # Use random study-level slopes?
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
    # Check if we're using random slopes (random slopes/int?)
    ranFlag = as.logical((ranInt == TRUE) | (ranSlo == TRUE))
    
    
    # Get the vector of possible time points to use for the MA
    TimepointsGraphOverall <- 
      seq(from = dat %>% pull(!!time.var) %>% min,
          to = dat %>% pull(!!time.var) %>% max,
          length.out = 1000)
    
    
    # Get the lists for the fitted coefficients and the variance matrices
    coefList <- list()
    vcovList <- list()
    icovList <- list()
    for(i in studyIDs){
      #- Get data for one study
      testDati <- dat %>% filter(studyID == i)
      
      
      
      #- Fit the model
      #- Get the FP model for this study
      formi <- paste("outcome ~ fp(", time.var, ", df = 4)")
      fpi <- mfp(data = testDati, formula = as.formula(formi), family = "binomial")
      powersi <- fpi$powers[is.na(fpi$powers) == FALSE] 
      nPowersi <- length(powersi)
      
      #--- get the formula for the model
      if(nPowersi == 2) { # If two powers, have to make sure we deal with the case when they are equal
        if(powersi[1] != powersi[2]) {
          form1 <- paste("outcome ~ ", 
                         paste("FPone(FP =", 
                               paste0(powersi, ", x = ", time.var, ")"), 
                               collapse = " + "))
        } else {
          form1 <- paste("outcome ~ FPone(FP = ",
                         powersi[1],
                         ", x = ",
                         time.var,
                         ") + I(log(",
                         time.var,
                         ") * FPone(FP = ", powersi[1], ", x = ", time.var, "))")
          
        }
      } else {
        form1 <- paste("outcome ~ ", 
                       paste("FPone(FP =", 
                             paste0(powersi, ", x = ", time.var, ")"), 
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
      
      #--- get the timepoints for the graph by study
      #TimepointsGraph <- seq(from = Timepoints[1], to = Timepoints[nTimepoints], length.out = 100)
      TimepointsGraphLogical <-
        dplyr::between(TimepointsGraphOverall,
                       left = testDati %>% pull(!!time.var) %>% min,
                       right = testDati %>% pull(!!time.var) %>% max)
      TimepointsGraph <- TimepointsGraphOverall[TimepointsGraphLogical]

      if(nPowersi == 2) {
        if(powersi[1] != powersi[2]) { # If the powers are different, just add em
          BasisTibble <- tibble( # Get the basis matrix including the intercept for matrix algebra
            intercept = 1,
            fp1 = FPone(FP = powersi[1], beta = 1, x = TimepointsGraph),
            fp2 = FPone(FP = powersi[2], beta = 1, x = TimepointsGraph),         
          )
        } else { # If the powers are the same then we need to multiply the second one by log
          BasisTibble <- tibble( # Get the basis matrix including the intercept for matrix algebra
            intercept = 1,
            fp1 = FPone(FP = powersi[1], beta = 1, x = TimepointsGraph),
            fp2 = log(TimepointsGraph)*FPone(FP = powersi[2], beta = 1, x = TimepointsGraph),         
          )          
        }
      } else {
        BasisTibble <- tibble( # Get the basis matrix including the intercept for matrix algebra
          intercept = 1,
          fp1 = FPone(FP = powersi[1], beta = 1, x = TimepointsGraph)
        )        
      }
      BasisMatrix <- as.matrix(BasisTibble)
      TimepointsTibble <- tibble(
        !!time.var := TimepointsGraph
      )
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
        !!time.var := TimepointsGraph,
        se = se1,
        pred = pred1$fit,
        outcome = fit1$family$linkinv(pred),
        lower = fit1$family$linkinv(pred - 1.96*se),
        upper = fit1$family$linkinv(pred + 1.96*se)
      )
      finalMat <- finalMat %>% add_row(graphTesti)
    }
    # Now meta-analyse the study-level data
    #-- save the matrix with the study-level stuff
    sigma2 <- finalMat %>% group_by(timeScale) %>% summarise(sigma2x = var(pred))
    
    if(ranFlag == TRUE) {
      studyMat <- finalMat %>%
        left_join(sigma2, by = time.var) %>%
        mutate(weight = 1/(se+sigma2x))
    } else {
      studyMat <- finalMat %>%
        mutate(weight = 1/se)    
    }
    
    #-- do the point-wise meta-analysis
    timeMat <- studyMat %>%
      group_by_at(time.var) %>%
      summarise(predWeight = sum(pred*weight),
                sumWeight = sum(weight),
                se2Weight2 = sum((se^2)*(weight^2))) %>%
      mutate(sumWeight2 = sumWeight^2,
             se = sqrt(se2Weight2/sumWeight2),
             pred = predWeight/sumWeight,
             outcome = fit1$family$linkinv(pred),
             lower = fit1$family$linkinv(pred - 1.96*se), # Transform back for the CIs
             upper = fit1$family$linkinv(pred + 1.96*se))
    
    #-- save the results as studyID = max(studyID)+1
    MAID <- max(studyIDs)+1
    MAMat <- tibble(
      studyID = MAID,
      !!time.var := timeMat %>% pull(!!time.var),
      pred = timeMat$pred,
      outcome = timeMat$outcome,
      se = timeMat$se,
      lower = timeMat$lower,
      upper = timeMat$upper
    ) 
    #-- merge the matrices together
    finalMat <- finalMat %>%
      add_row(MAMat)
    
    # Return the final matrix
    return(finalMat)
  }


