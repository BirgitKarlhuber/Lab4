#' Function for a multiple regression model
#' 
#' test test test test test test test test test test test test test test test test test test test test test test 
#'
#' @param formula an object of class 'formula': a symbolic description of the model to be fitted.
#' 
#' @param data an 'data.frame' containing the variables in the model. 
#'
#' @return Returns an object of class 'linreg'. 
#' 
#' @import ggplot2
#'
#' @export
#' 
#' @examples
#' data("iris")
#' library(Lab4)
#' linear_model <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)


linreg <- function(formula, data){
  stopifnot(class(formula)=="formula", is.data.frame(data))
  
  X <- as.matrix(model.matrix(formula, data))
  y <- as.matrix(data[all.vars(formula, max.names=1)])
  
  beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y # regression coefficient
 
  y_hat <- round(X%*%beta_hat,3) # fitted_values
  e_hat <- y-y_hat # residuals
  
  n <- nrow(data) # number of observations
  p <- ncol(X) # number of parameters in the model
  df <- n-p
  
  sigma2_hat <- as.numeric((t(e_hat)%*%e_hat)/df) # residual variance
  
  est_var_sigma2_hat <- sigma2_hat*(solve(t(X)%*%X)) # variance of the regression coefficients
  t_beta <- beta_hat/sqrt(diag(est_var_sigma2_hat)) # t-values for each coefficient
  
  p_value <- 2*pt(-abs(t_beta),df=n-1) # p-value for each regression coefficient
  
  
  # RC class object
  linreg <- setRefClass("linreg",
                        fields = list (coefficients = "matrix", residuals = "matrix",
                                       fitted.values = "matrix", df.residuals = "numeric",
                                       residual.variance = "numeric", coefficient.variance = "matrix",
                                       t.values = "numeric", p.values = "numeric")
  )
  
  # methods for the RC class object
  linreg$methods(
    print = function(){
      cat(paste0(dimnames(as.vector(coefficients))[[1]]))
      cat(paste0(coefficients))
    },
    
    plot = function(x){
      med_group <- aggregate(x$residuals,         # Median by group
                             list(x$fitted.values),
                             median)
      
      df <- data.frame(fitted.values=med_group[,1], residuals=med_group[,2])
      
      ggplot2::ggplot(x, aes(fitted.values(x), residuals(x))) + 
        theme_bw() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_point(shape=1, size=2) +
        geom_hline(yintercept = 0, linetype = "dotted", color="grey") +
        geom_line(df, color="red") +
        labs(title='Residuals vs Fitted', 
             x=expression(paste("Fitted values \n lm(", formula, ")")), y='Residuals')
    }, # second plot is missing
    
    resid = function(){
      # retunr e_hat
    }, 
    
    pred = function(){
      # return y_hat
    }, 
    
    coef = function(){
      # coefficients as a named vector
    }, 
    
    summary = function(){
      # return a similar printout as printed for lm objects ...
    }
  )
  
  # create new object of class 'linreg' - with values (return this)
  object <- linreg$new(coefficients=beta_hat, residuals=e_hat, 
                       fitted.values=y_hat, df.residuals=df, 
                       residual.variance=sigma2_hat, coefficient.variance=est_var_sigma2_hat,
                       t.values=as.numeric(t_beta), p.values=as.numeric(p_value))
  return(object)
}

# data("iris")
# mod_object <- linreg(Petal.Length~Species, data = iris)
# print(mod_object)
# plot(mod_object)
