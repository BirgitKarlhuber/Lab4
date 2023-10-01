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

# RC class object
linreg <- setRefClass("linreg",
                      fields = list (formula = "formula",
                                     data = "data.frame",
                                     data_name = "character",
                                     coefficients = "matrix",
                                     residuals = "matrix",
                                     fitted.values = "matrix",
                                     n.obs = "numeric",
                                     n.par = "numeric",
                                     df.residuals = "numeric",
                                     residual.variance = "numeric",
                                     coefficient.variance = "matrix",
                                     t.values = "matrix",
                                     p.values = "matrix"))

# methods for the RC class object
linreg$methods(
  initialize = function(formula, data){
    stopifnot(class(formula)=="formula", is.data.frame(data))
    
    .self$formula <<- formula
    .self$data <<- data
    .self$data_name <<-deparse(substitute(data))
    
    X <- as.matrix(model.matrix(formula, data))
    y <- as.matrix(data[all.vars(formula, max.names=1)])
    
    .self$coefficients <<- solve(t(X)%*%X)%*%t(X)%*%y # regression coefficient
    .self$fitted.values <<- round(X%*%coefficients,3) # fitted_values
    .self$residuals <<- y-fitted.values # residuals
    .self$n.obs <<- nrow(data) # number of observations
    .self$n.par <<- ncol(X) # number of parameters in the model
    .self$df.residuals <<- n.obs - n.par # degrees of freedom
    .self$residual.variance <<- as.numeric((t(residuals)%*%residuals)/df.residuals) # residual variance
    .self$coefficient.variance <<- residual.variance*(solve(t(X)%*%X)) # variance of the regression coefficients
    .self$t.values <<- coefficients/sqrt(diag(coefficient.variance)) # t-values for each coefficient
    .self$p.values <<- 2*pt(-abs(t.values),n.obs-1) # p-value for each regression coefficient
  },
  
  print = function(){
    char_formula <- as.character(formula)
    coef <- as.vector(coefficients)
    names(coef) <- rownames(coefficients)
    
    cat(paste("linreg(formula = ", char_formula[2]," ", char_formula[1], " ",
        char_formula[3], ", data = ", data_name,")\n\nCoefficients:\n",sep=""))
    base::print(coef)
    },
  
  plot = function(){
    med_group <- aggregate(residuals,         # Median by group
                           list(fitted.values),
                           median)
    df <- data.frame(fitted.values=med_group[,1], residuals=med_group[,2])
    char_formula <- as.character(formula)
    
    plotA <- ggplot2::ggplot(data = data, mapping = aes(fitted.values, residuals)) + 
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      geom_point(shape=1, size=2) +
      geom_hline(yintercept = 0, linetype = "dotted", color="grey") +
      geom_line(mapping = aes(x = fitted.values, y = residuals), data = df,
                colour = 'red') +
      labs(title='Residuals vs Fitted', 
           x=paste("Fitted values \n lm(", char_formula[2], char_formula[1], 
                   char_formula[3], ")"), 
           y='Residuals')
    
    # second graph
    med_group2 <- aggregate(sqrt(abs(residuals/sd(residuals))),         # Median by group
                            list(fitted.values),
                            median)
    df2 <- data.frame(fitted.values=med_group2[,1], std.residuals=med_group2[,2])

    plotB <- ggplot2::ggplot(data, aes(fitted.values, sqrt(abs(residuals/sd(residuals))))) + 
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      geom_point(shape=1, size=2) +
      geom_line(mapping = aes(x = fitted.values, y = std.residuals), data = df2,
                colour = 'red') +
      labs(title='Scale-Location', 
           x=paste("Fitted values \n lm(", char_formula[2], char_formula[1], 
                   char_formula[3], ")"), 
           y=expression(sqrt(abs('Standardized residuals')))
           )
    return(list(plotA,plotB))
  },
  
  resid = function(){
    return(residuals)
  }, 
  
  pred = function(){
    return(fitted.values)
  }, 
  
  coef = function(){
    coef <- as.vector(coefficients)
    names(coef) <- rownames(coefficients)
    return(coef)
  }, 
  
  summary = function(){
    coef_names <- rownames(coefficients)
    coef_sd <- sqrt(diag(coefficient.variance))
    
    for(i in 1:length(coef_names)){
      cat(coef_names[i], round(coefficients[i],4), round(coef_sd[i],4), round(t.values[i],4),
          p.values[i],sep=" ")
    }
    cat("\n\n", "Residual standard error:", round(sqrt(residual.variance),4), "on", 
        df.residuals,"degrees of freedom")
  }
) 

# data("iris")
# library(ggplot2)
# 
# mod_object <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data = iris)
# mod_object$print()
# mod_object$resid()
# mod_object$plot()
# mod_object$coef()
# mod_object$summary()
