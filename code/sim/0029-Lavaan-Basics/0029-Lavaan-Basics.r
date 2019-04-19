
rm(list=ls())
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)

data(HolzingerSwineford1939)

# basic model
HS.model <- ' visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

# running cfa
fit <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)

###########################################################
# Sample Matrix - from data
# All commands below return the same results
###########################################################
lavInspect(fit,what = "sampstat")
lavInspect(fit,what = "obs")
lavInspect(fit,what = "observed")
lavInspect(fit,what = "samp")
lavInspect(fit,what = "sample")
lavInspect(fit,what = "samplestatistics")

###########################################################
# Model Implied Matrix - from data
# All commands below return the same results
###########################################################
lavInspect(fit,what = "implied")
lavInspect(fit,what = "fitted")
lavInspect(fit,what = "expected")
lavInspect(fit,what = "exp")
lavInspect(fit,what = "cov.ov")
fitted(fit)
#this should give a null matrix.
lavInspect(fit,what = "cov.ov") - fitted(fit)$cov


###########################################################
# Residual Matrix - Sample Matrix - Implied Matrix
# All commands below return the same results
###########################################################
lavInspect(fit,what = "resid")
lavInspect(fit,what = "residuals")
lavInspect(fit,what = "residual")
lavInspect(fit,what = "res")
resid(fit)

#this should give a null matrix.
lavInspect(fit,what = "sampstat")$cov - lavInspect(fit,what = "implied")$cov - lavInspect(fit,what = "resid")$cov

###########################################################
# List of fit measures.
# Much more than available in summary(fit, fit.measures=TRUE)
# All commands below return the same results
###########################################################

lavInspect(fit,what = 'fit')
lavInspect(fit,what = 'fitmeasures')
lavInspect(fit,what = 'fit.measures')
lavInspect(fit,what = 'fit.indices')
fitMeasures(fit)

###########################################################
# List of optins passed to the model
###########################################################
lavInspect(fit,what = 'options')

###########################################################
# Input data
###########################################################
lavInspect(fit,what = 'data')

###########################################################
# List of parameters in the model
###########################################################
lavInspect(fit,what = 'free') # parameter list with their 
# indexes in the parameter list
lavInspect(fit,what = 'list') #parameter table 
parTable(fit) #parameter table


###########################################################
# List of estimated parameters
###########################################################
lavInspect(fit, what = 'est')
lavInspect(fit, what = 'std') #variances of both the observed and the latent variables are set to unity
lavInspect(fit, what = 'std.lv') #variances of the latent variables are set to unity

###########################################################
# List of Model-implied loadings
###########################################################
lavInspect(fit,what = 'est')$lambda #parameter list with their indexes in the parameter list

###########################################################
# List of Model-implied thresholds
###########################################################
lavInspect(fit,what = 'est')$tau 
lavInspect(fit,what = "th")

###########################################################
# Model-implied mean vector of the latent variables
###########################################################
lavInspect(fit,what = 'est')$alpha 
lavInspect(fit,what = "mean.lv")
lavInspect(fit,what = "eeta")

###########################################################
# Model-implied latent variable covariances matrix
###########################################################
lavInspect(lavaan.model.fit,what = 'est')$psi
lavInspect(lavaan.model.fit,what = "cov.lv")
lavInspect(lavaan.model.fit,what = "veta")
diag(lavInspect(lavaan.model.fit,what = 'est')$psi) #getting the diagonal

###########################################################
# Model-implied latent variable correlation matrix
###########################################################
lavInspect(lavaan.model.fit,what = "cor.lv")

###########################################################
# Model-implied mean vector of the observed variables
###########################################################
lavInspect(fit,what = "mean.ov")
lavInspect(fit,what = "mu")
lavInspect(fit,what = "mu.hat")

###########################################################
# Model-implied variance-covariance matrix of the observed variables
###########################################################
lavInspect(fit,what = "cov.ov")
lavInspect(fit,what = "sigma")
lavInspect(fit,what = "sigma.hat")

###########################################################
# correlation matrix of the observed variables
###########################################################
lavInspect(fit,what = "cor.ov")


###########################################################
# residual covariance matrix of observed variables
#
# x* = intercept + lambda x factor + residual
# var(residual) = $theta
#
###########################################################
lavInspect(fit,what = 'est')$theta

# Note that for cathegorical variables, one can use the
# parameterization option to choose what to fix to make the
# sistem identifiable
# Parameterization = "theta" would set var(residual) =1
# Parameterization = "delta" would set var(x*) =1
  

HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
                                 "x6","x7","x8","x9")]
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HSbinary, ordered=names(HSbinary)) #with Parameterization = "delta" 
lavInspect(fit, 'est')$theta
lavInspect(update(fit, parameterization = "theta"), 'est')$theta





