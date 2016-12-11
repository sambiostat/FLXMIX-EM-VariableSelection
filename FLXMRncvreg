#This is an interface of ncvreg/glmnet in Flexmix
#It fits generalized linear model in 
#ncvreg defined as 
#   function (X, y, family = c("gaussian", "binomial", "poisson"), 
#             penalty = c("MCP", "SCAD", "lasso"), 
#             gamma = switch(penalty, SCAD = 3.7, 3), alpha = 1, 
#             lambda.min = ifelse(n > p,0.001, 0.05), nlambda = 100, 
#             lambda, eps = 0.001, max.iter = 1000, 
#             convex = TRUE, dfmax = p + 1, penalty.factor = rep(1, ncol(X)), 
#             warn = TRUE, returnX = FALSE, ...) 

#Note:
#   "w" passing in z@fit might be problematic since the ncvreg dosen't allow weight parameter
#   Need figure out how "W" involved in the caculation of flexmix/glmnet

#ProblmeOne
#     for binomial distribution, FLXMRglm requires Y be a 2 columns matrix,one indicates failure another indicates sucess
#     However, for ncvreg, the Y has to be a two level vector numerical vector 

FLXMRncvreg <- function(formula=.~.,     family = c("gaussian", "binomial", "poisson"), 
                penalty = c("MCP", "SCAD", "lasso"), select = TRUE, adaptive = FALSE, offset = NULL, ...) {   
      family <- match.arg(family)
      penalty_choice <-match.arg(penalty)
      
      z <- FLXMRglm(formula = formula, family=family)
      
      z@preproc.x <- function(x) {
        if (!isTRUE(all.equal(x[, 1], rep(1, nrow(x)), check.attributes = FALSE)))
          stop("The model needs to include an intercept in the first column.")
        x
      }
      
      z@fit <- function(x, y, w){
        if (all(!select)) {
          coef <- if (family == "gaussian")
                     lm.wfit(x, y, w = w)$coef
                  else if (family == "binomial")
                     glm.fit(x, y, family = binomial(), weights = w)$coef
                  else if (family == "poisson")
                      glm.fit(x, y, family=poisson(), weights = w)$coef
        } else {
          if (adaptive) {
            coef <- if (family == "gaussian")
                      lm.wfit(x, y, w = w)$coef[-1]
                    else if(family == "binomial")
                      glm.fit(x, y, family = binomial(), weights = w)$coef[-1]
                    else if (family == "poisson")
                      glm.fit(x, y, family = poisson(), weights = w)$coef[-1]
            penalty <- mean(w) / abs(coef)
            m <-  glmnet::cv.glmnet(x[, -1, drop = FALSE], y, family = family, weights = w, penalty.factor = penalty, ...)
            coef <- as.vector(coef(m, s = "lambda.min"))
          } else {
            penalty <- rep(1, ncol(x) - 1)
            if (any(!select)){
                select <- which(!select)        #set the penalty.factor as 0 if this covariate is not selected
                penalty[select] <- 0
              }  
            if (family = "binomial"){
              m <- ncvreg::cv.ncvreg(x[, -1, drop = FALSE], y, family = family,penalty=penalty_choice, penalty.factor = penalty, ...)  
                }            
            else{
                 m<- ncvreg::cv.ncvreg(x[, -1, drop = FALSE], y, family = family,penalty=penalty_choice, penalty.factor = penalty, ...)
            }
            coef <- as.vector(coef(m, s = "lambda.min"))
          }
        }
        df <- sum(coef != 0)
        sigma <- if (family == "gaussian") sqrt(sum(w * (y - x %*% coef)^2/mean(w))/(nrow(x) - df)) else NULL
        with(list(coef = coef, sigma = sigma, df = df + ifelse(family == "gaussian", 1, 0)),
             eval(z@defineComponent))
      }
      z
}

    

