#' Function for a multiple regression model
#' 
#' 
#'
#' @param formula an object of class 'fromula': a symbolic decription of the model to be fitted.
#' 
#' @param data an 'data.frame' containing the variables in the model. 
#'
#' @return Returns an object of class 'linreg'. 
#' 
#' @examples
#' data("iris")
#' linear_model <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#' 
#' @export


linreg <- function(formula, data){
  stopifnot(class(formula)=="formula")
  
  X <- as.matrix(model.matrix(formula, data))
  y <- as.matrix(data[all.vars(formula, max.names=1)])
  
  beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y # regression coefficient
  y_hat <- X%*%beta_hat # fitted_values
  e_hat <- y-y_hat# residuals
  
  n <- nrow(data) # number of observations
  p <- ncol(X) # number of parameters in the model
  df <- n-p
  
  sigma2_hat <- as.numeric((t(e_hat)%*%e_hat)/df) # residual variance
  
  est_var_sigma2_hat <- sigma2_hat*(solve(t(X)%*%X)) # variance of the regression coefficients
  t_beta <- beta_hat/sqrt(diag(est_var_sigma2_hat)) # t-values for each coefficient
  
  p_value <- 2*pt(-abs(t_beta),df=n-1) # p-value for each regression coefficient
  
  # Create Class
  linreg <- setRefClass ("linreg",
                         fields = list ( coefficients = "matrix", residuals = "matrix",
                                         fitted.values = "matrix", df.residuals = "numeric",
                                         residual.variance = "numeric", coefficient.variance = "matrix",
                                         t.values = "numeric", p.values = "numeric")
  )
  
  object <- linreg$new(coefficients=beta_hat, residuals=e_hat, 
                       fitted.values=y_hat,df.residuals=df, 
                       residual.variance=sigma2_hat, coefficient.variance=est_var_sigma2_hat,
                       t.values=as.numeric(t_beta), p.values=as.numeric(p_value))
  
  return(object)
}

# data("iris")
# linear_model <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
