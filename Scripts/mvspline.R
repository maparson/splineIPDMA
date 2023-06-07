#--------------------------------#
# Mvspline Script
#--------------------------------#  

mvspline <- 
  function(
    dat = NULL,
    knotLoc = NULL,
    knotN = 1,
    studyID.var = "studyID",
    time.var = "timeScale"
  ) 
{
    
    studyIDs <- dat %>% pull(get(studyID.var)) %>% unique()
    nStudy <- studyIDs %>% length()
    
    # If no knotLoc argument passed, then use quantiles based on desired num of knots (knotN)
    if(is.null(knotLoc)) {
      quantsFull = seq(from = 0, to = 1, length.out = knotN + 2)
      quants = quantsFull[2:(knotN+1)]
      knotLoc = quantile(dat %>% pull(!!time.var), probs = quants)
    }
    nKnots <- length(knotLoc)
    
    # Get the knot variable names
    splineVars <- paste("V", 1:(nKnots+1), sep = "")
    splineVars0 <- c("V0", splineVars)
    # Get the matrix for the spline basis
    VMat <- as_tibble(matrix(nrow = 0, 
                             ncol = length(splineVars0), 
                             dimnames = list(NULL, splineVars0)))
    
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
    # Get the lists for the fitted coefficients and the variance matrices
    coefList <- list()
    vcovList <- list()
    icovList <- list()
    for(i in studyIDs){
      #- Get data for one study
      testDati <- testDat %>% filter(studyID == i)
      
      #- Get the spline basis
      #--- knot locations 
      #knotLoc <- c(3,5,7)
      #--- pull the basis matrix
      spline1 <- testDati %>% pull(!!time.var) %>% ns(knots = knotLoc)
      #--- remove matrix attributes
      spline2 <- lapply(spline1, function(x) { attributes(x) <- NULL; x })
      spline3 <- matrix(unlist(spline2), ncol = nKnots+1)
      #--- paste basis matrix into dataset as V1, V2, ...
      testDatiSpline <- testDati %>%
        bind_cols(as.data.frame(spline3)) 
      
      #- Fit the model
      #--- get the formula for the model
      splineVars <- paste("V", 1:(length(knotLoc)+1), sep = "")
      form1 <- paste("outcome ~ ", paste(splineVars, collapse = " + "))
      #--- fit the model with the basis variables
      fit1 <- glm(data = testDatiSpline,
                  formula = as.formula(form1),
                  family = "binomial")
      #--- get the within-study variance-covariance matrix
      vcov1 <- vcov(fit1)
      icov1 <- inv(vcov1)
      #--- get the fitted coefficients
      coef1 <- coef(fit1)
      #--- get the fitted values over time
      betaHat <- coef1
      predsi <- as.matrix(basis1) %*% betaHat
      outcome <- fit1$family$linkinv(predsi)
      
      x1 <- as.matrix(basis1)
      X1 <- as.matrix(cbind(rep(1,nrow(fit1$model)), fit1$model[,2:3]))
      middle <- solve(t(X1) %*% (X1))
      si <- apply(x1, 1, function(x) {t(x) %*% (middle) %*% x})
      Si =  sum(fit1$residuals^2) / fit1$df.residual
      leveri <- apply(x1, 1, function(x) {t(x) %*% inv(t(X1) %*% X1) %*% x})
      sei <- poop*sqrt(si)
      loweri <- fit1$family$linkinv(as.matrix(basis1) %*% betaHat - 1.96*sei)
      upperi <- fit1$family$linkinv(as.matrix(basis1) %*% betaHat + 1.96*sei)
      
      #-- get the MA results
      graphTesti <- tibble(
        !!studyID.var := i,
        !!time.var := Timepoints,
        pred = as.numeric(predsi),
        outcome = as.numeric(outcome),
        se = as.numeric(sei),
        lower = as.numeric(loweri),
        upper = as.numeric(upperi)
      )

      coefList[[length(coefList)+1]] <- coef1
      vcovList[[length(vcovList)+1]] <- vcov1 
      icovList[[length(icovList)+1]] <- icov1 
      
      finalMat <- finalMat %>% add_row(graphTesti)
    }
    
    # Now meta-analyse the study-level data
    icovsum <- Reduce("+", icovList)
    icovsumInv <- inv(icovsum)
    WList <- list()
    betaHatList <- list()
    for(i in 1:nStudy) {
      Wi <- icovsumInv %*% icovList[[i]]
      betaHati <- Wi %*% coefList[[i]]
      
      WList[[length(WList)+1]] <- Wi
      betaHatList[[length(betaHatList)+1]] <- betaHati
    }
    betaHat <- Reduce("+", betaHatList)
    predsi <- as.matrix(basis1) %*% betaHat
    outcome <- fit1$family$linkinv(predsi)
    
    x1 <- as.matrix(basis1)
    X1 <- as.matrix(basis1)
    si <- apply(x1, 1, function(x) {t(x) %*% vcov1 %*% x})
    leveri <- apply(x1, 1, function(x) {t(x) %*% inv(t(X1) %*% X1) %*% x})
    sei <- sqrt(si)*sqrt(leveri)
    loweri <- fit1$family$linkinv(as.matrix(basis1) %*% betaHat - 1.96*sei)
    upperi <- fit1$family$linkinv(as.matrix(basis1) %*% betaHat + 1.96*sei)
    
    #-- get the MA results
    graphTesti <- tibble(
      studyID = rep(MAID, nrow(timeMat)),
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


