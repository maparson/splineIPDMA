
#- Get data for one study
testDat1 <- testDat %>% filter(studyID == 1)

#- Get the spline basis
#--- knot locations 
knotLocations <- c(3,5,7)
#--- pull the basis matrix
spline1 <- testDat1 %>% pull(timePoint) %>% ns(knots = knotLocations)
#--- remove matrix attributes
spline2 <- lapply(spline1, function(x) { attributes(x) <- NULL; x })
spline3 <- matrix(unlist(spline2), ncol = 4)
#--- paste basis matrix into dataset as V1, V2, ...
testDat1Spline <- testDat1 %>%
  bind_cols(as.data.frame(spline3)) 

#- Fit the model
#--- get the formula for the model
splineVars <- paste("V", 1:(length(knotLocations)+1), sep = "")
form1 <- paste("outcome ~ ", paste(splineVars, collapse = " + "))
#--- fit the model with the basis variables
fit1 <- glm(data = testDat1Spline,
            formula = as.formula(form1),
            family = "binomial")
#--- fit using ns within the formula to check it worked
fit2 <- glm(data = testDat1Spline,
            formula = outcome ~ ns(timePoint, knotLocations),
            family = "binomial")

#- Check
#--- get the one-person basis matrix for a single participant
nTimepoints <- testDat1$timePoint %>% unique() %>% length()
basis1 <- data.frame(cbind(rep(1,nTimepoints), spline3[1:nTimepoints,]))
colnames(basis1) <- paste("V", 0:(length(knotLocations)+1), sep = "")
#--- get the predicted values from the first fit using the one-person basis
pred1 <- predict(fit1, newdata = basis1, se.fit = TRUE, type = "link")
#--- get the within-study variance-covariance matrix
vcov1 <- vcov(fit1)[1:5, 1:5]
#varAtSomePoint <- as.matrix(basis1[6,]) %*% vcov1 %*% t(as.matrix(basis1[6,])) #?
#--- get the pointwise variance/se at each timepoint
vars1 <- apply(basis1, MARGIN = 1, FUN = function(x) {t(as.matrix(x)) %*% vcov1 %*% as.matrix(x)})
se1 <- sqrt(vars1)

#- Graph
#--- get the data to graph out
graphTest1 <- basis1 %>%
  as_tibble() %>%
  mutate(se = se1,
         timePoint = 1:100,
         outcome = fit1$family$linkinv(pred1$fit),
         lower = fit1$family$linkinv(pred1$fit -1.96*se),
         upper = fit1$family$linkinv(pred1$fit +1.96*se)) 
#--- graph the trajectory and CI
graphTest1 %>% 
  ggplot(aes(x = timePoint,
             y = outcome)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.1)
#--- check it's the same as plot_model
plot_model(fit2, type = "pred", terms = c("timePoint [all]"))
#--- check se is the same as from ggpredict
graphTest2 <- ggpredict(fit2, terms = "timePoint [all]") 
se2 <- graphTest2$std.error


