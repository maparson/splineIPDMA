#--------------------------------#
# Metaspline Script
#--------------------------------#  

metaspline <- 
  function(
    dat = NULL,
    knotLoc = NULL,
    knotN = 1,
    studyID.var = "studyID",
    time.var = "timeScale",
    ranInt = FALSE, # Use a random study-level intercept?
    ranSlo = FALSE # Use random study-level slopes?
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
  ) %>%
    add_column(VMat)
  
  # Check if we're using random slopes (random slopes/int?)
  ranFlag = as.logical((ranInt == TRUE) | (ranSlo == TRUE))
  
  
  # Go over all the studies
  for(i in studyIDs){
    #- Get data for one study
    testDati <- dat %>% filter(studyID == i)
    
    #- Get the spline basis
    #--- count the timepoints
    Timepoints <- testDati %>% pull(!!time.var) %>% unique() %>% sort()
    nTimepoints <- Timepoints %>% length()
    #--- if two timepoints, do linear model
    linearCheck = nTimepoints == 2
    #--- make sure that max time points is more than knots + 1 (DO LATER)
    # if(nTimepoints <= length(knotLoc) + 1) {
    #   
    # }
    #--- knot locations 
    #knotLoc <- c(3,5,7)
    #--- pull the basis matrix
    if(linearCheck == FALSE) {
      spline1 <- testDati %>% pull(!!time.var) %>% ns(knots = knotLoc)
      #--- remove matrix attributes
      spline2 <- lapply(spline1, function(x) { attributes(x) <- NULL; x })
      spline3 <- matrix(unlist(spline2), ncol = nKnots+1)
    } else {
      spline1 <- testDati %>% pull(!!time.var) %>% ns(df=1)
      #--- remove matrix attributes
      spline2 <- lapply(spline1, function(x) { attributes(x) <- NULL; x })
      spline3 <- matrix(unlist(spline2), ncol = 1)
    }

    #--- paste basis matrix into dataset as V1, V2, ...
    testDatiSpline <- testDati %>%
      bind_cols(as.data.frame(spline3)) 
    
    #- Fit the model
    #--- get the formula for the model
    if(linearCheck == FALSE) {
      splineVars <- paste("V", 1:(length(knotLoc)+1), sep = "")
    } else {
      splineVars <- c("V1")
    }
    form1 <- paste("outcome ~ ", paste(splineVars, collapse = " + "))
    #--- fit the model with the basis variables
    fit1 <- glm(data = testDatiSpline,
                formula = as.formula(form1),
                family = "binomial")
    #--- fit using ns within the formula to check it worked
    # fit2 <- glm(data = testDat1Spline,
    #             formula = outcome ~ ns(timePoint, knotLoc),
    #             family = "binomial")
    
    #- Check
    #--- get the one-person basis matrix for a single participant
    basis1 <- data.frame(cbind(rep(1,nTimepoints), spline3[1:nTimepoints,]))
    if(linearCheck == FALSE) {
      colnames(basis1) <- paste("V", 0:(length(knotLoc)+1), sep = "")
    } else {
      colnames(basis1) <- c("V0", "V1")
    }
    #--- get the predicted values from the first fit using the one-person basis
    pred1 <- predict(fit1, newdata = basis1, se.fit = TRUE, type = "link")
    #--- get the within-study variance-covariance matrix
    vcov1 <- vcov(fit1)
    #--- get the pointwise variance/se at each timepoint
    vars1 <- apply(basis1, MARGIN = 1, FUN = function(x) {t(as.matrix(x)) %*% vcov1 %*% as.matrix(x)})
    se1 <- sqrt(vars1)
    
    #- Graph
    #--- get the data to graph out
    graphTesti <- basis1 %>%
      as_tibble() %>%
      mutate(
        !!studyID.var := i,
        se = se1,
        !!time.var := Timepoints,
        pred = pred1$fit,
        outcome = fit1$family$linkinv(pred),
        lower = fit1$family$linkinv(pred - 1.96*se),
        upper = fit1$family$linkinv(pred + 1.96*se)
        ) 

    #--- check it's the same as plot_model
    # plot_model(fit2, type = "pred", terms = c("timePoint [all]"))
    #--- check se is the same as from ggpredict
    # graphTest2 <- ggpredict(fit2, terms = "timePoint [all]") 
    # se2 <- graphTest2$std.error
    
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
  ) #%>% removed this for now as long as some studies can have different knots
  #   add_column(basis1)
  #-- merge the matrices together
  finalMat <- finalMat %>%
    add_row(MAMat)
  
  # Return the final matrix
  return(finalMat)
}
