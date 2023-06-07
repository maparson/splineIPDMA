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
    # Get the vector of possible time points to use for the MA
    TimepointsGraphOverall <- 
      seq(from = dat %>% pull(!!time.var) %>% min,
          to = dat %>% pull(!!time.var) %>% max,
          length.out = 1000)
    #--- pull the basis matrix
    spline1 <- dat %>% pull(!!time.var) %>% ns(knots = knotLoc)
    #--- remove matrix attributes
    spline2 <- lapply(spline1, function(x) { attributes(x) <- NULL; x })
    spline3 <- matrix(unlist(spline2), ncol = nKnots+1)
    dat2 <- dat %>%
      bind_cols(as.data.frame(spline3)) 
    for(i in studyIDs){
      #- Get data for one study
      testDati <- dat %>% filter(studyID == i)
      
      #- Get the spline basis
      #--- knot locations 
      #knotLoc <- c(3,5,7)

      #--- paste basis matrix into dataset as V1, V2, ...
      testDatiSpline <- testDati %>% left_join(dat2, by = c("studyID", "patID", "timePoint", "outcome"))
      #- Fit the model
      #--- get the formula for the model
      splineVars <- paste("V", 1:(length(knotLoc)+1), sep = "")
      form1 <- paste("outcome ~ ", paste(splineVars, collapse = " + "))
      #--- fit the model with the basis variables
      fit1 <- glm(data = testDatiSpline,
                  formula = as.formula(form1),
                  family = "binomial")
      #--- get the within-study variance-covariance matrix
      # X1 <- model.matrix(fit1) 
      # V1 <- solve(t(X1) %*% X1)
      
      
      #--- get the timepoints for the graph by study
      TimepointsGraphLogical <-
        between(TimepointsGraphOverall,
                lower = testDati %>% pull(!!time.var) %>% min,
                upper = testDati %>% pull(!!time.var) %>% max)
      TimepointsGraph <- TimepointsGraphOverall[TimepointsGraphLogical]
      basis0 <- predict(spline1, TimepointsGraph)
      basis1 <- data.frame(cbind(rep(1,nrow(basis0)), basis0))
      colnames(basis1) <- paste("V", 0:(length(knotLoc)+1), sep = "")
      #--- get the predicted values from the first fit using the one-person basis
      pred1 <- predict(fit1, newdata = basis1, se.fit = TRUE, type = "link")
      #--- find which spline coefficients are not missing (due to not being in the range)
      naNames <- attributes(coef(fit1)[is.na(coef(fit1))])$names
      coefNames <- attributes(coef(fit1)[is.na(coef(fit1))==FALSE])$names
      coefNames[1] <- "V0"
      naNum <- length(naNames)
      coefNum <- length(coefNames)
      #--- get the within-study variance-covariance matrix
      vcov0 <- vcov(fit1)
      vcov1 <- vcov0[is.na(vcov0) == FALSE] %>% matrix(nrow = coefNum) # need to drop NAs
      icov1 <- solve(vcov1)
      #--- get the pointwise variance/se at each timepoint
      basis1i <- subset(basis1, select = coefNames) # select the basis columns with full data
      vars1 <- apply(basis1i, MARGIN = 1, FUN = function(x) {t(as.matrix(x)) %*% vcov1 %*% as.matrix(x)})
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
    preds <- as.matrix(basis1) %*% betaHat
    outcome <- fit1$family$linkinv(preds)
    
    x1 <- as.matrix(basis1)
    si <- apply(x1, 1, function(x) {t(x) %*% icovsumInv %*% x})
    sei <- sqrt(si)
    loweri <- fit1$family$linkinv(as.matrix(basis1) %*% betaHat - 1.96*sei)
    upperi <- fit1$family$linkinv(as.matrix(basis1) %*% betaHat + 1.96*sei)
    
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


