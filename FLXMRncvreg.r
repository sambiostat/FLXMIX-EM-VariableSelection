FLXMRncvreg <- function(formula=.~.,     family = c("gaussian", "binomial", "poisson"), 
                       penalty = c("MCP", "SCAD", "lasso"), select = TRUE, adaptive = FALSE, 
                       offset = NULL, ...) {   
  
  family <- match.arg(family)
  if (!is.element(family, c("gaussian", "binomial", "poisson"))) {
    stop("Family can only be gaussian, binomial or poisson")
  }

  penalty_choice <-match.arg(penalty)
  if (!is.element(penalty_choice, c("MCP", "SCAD", "lasso"))) 
    stop("Family can only be gaussian, binomial or poisson")
  
  z <- FLXMRglm(formula = formula, family=family)
  
  z@preproc.x <- function(x) {
    if (!isTRUE(all.equal(x[, 1], rep(1, nrow(x)), check.attributes = FALSE)))
      stop("The model needs to include an intercept in the first column.")
    x
  }
  
  z@fit <- function(x, y, w) {
    #firts of all, when covariate is not included in the selection process
    if (all(!select)) {    
      coef <- if (family == "gaussian")
        lm.wfit(x, y, w = w)$coef
      else if (family == "binomial")
        glm.fit(x, y, family = binomial(), weights = w)$coef
      else if (family == "poisson")
        glm.fit(x, y, family=poisson(), weights = w)$coef
      } 
    #if some of the covariate is specifed to be inside the selection process
    #And when variable selection method is Adaptive Lasso
    else {
      if (adaptive) {
          if (family == "gaussian") { coef <- lm.wfit(x, y, w = w)$coef }
          else if (family == "binomial"){ coef <- glm.fit(x, y, family = binomial(), weights = w)$coef}
          else if (family == "poisson"){ coef <- glm.fit(x, y, family=poisson(), weights = w)$coef}
          penalty <- mean(w) / abs(coef)
          # set the penalty.factor as 0 if this covariate is not selected
          if (any(!select)){
          select <- which(!select)        
          penalty[select] <- 0
          }           
          m <-  glmnet::cv.glmnet(x[, -1, drop = FALSE], y, family = family, weights = w, penalty.factor = penalty, ...)
          coef <- as.vector(coef(m, s = "lambda.min"))
        } else {
              penalty <- rep(1, ncol(x) - 1)
              # set the penalty.factor as 0 if this covariate is not selected
              if (any(!select)){
              select <- which(!select)        
              penalty[select] <- 0
                }  
              if (family == "binomial"){
                m <- ncvreg::cv.ncvreg(x[, -1, drop = FALSE], y[1, drop=FALSE], family = family, penalty=penalty_choice, penalty.factor = penalty, ...)  
              } else {
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
