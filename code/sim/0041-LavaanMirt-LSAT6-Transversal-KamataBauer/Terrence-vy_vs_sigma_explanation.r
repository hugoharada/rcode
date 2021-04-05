HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
                                 "x6","x7","x8","x9")]
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )

## fit 3-indicator CFA
HS.model <- ' visual  =~ x1 + x2 + x3 '
fit <- cfa(HS.model, data=HSbinary, ordered=names(HSbinary),
           parameterization = "theta")

## these don't match
#OK, turns out for categorical data, 
#"vy" is the model-implied variances of the latent item responses (not necessarily 1), 
# whereas "sigma" will return the model-implied polychoric correlations (i.e., the diagonal will be 1s).  See below when using theta parameterization.

lavInspect(fit, "sigma")
lavInspect(fit, "vy")

## but the variances match the standard expectation for covariance structure
EST <- lavInspect(fit, "est")
LAMBDA <- EST$lambda
PSI <- EST$psi
THETA <- EST$theta
LAMBDA %*% PSI %*% t(LAMBDA) + THETA

