#' Model Selection Using Ridge, LASSO, Elastic Net
#'
#' Takes in the generated dataset and fits a penalized
#' regression models using L2, L1 penalties.
#'
#' @param df The dataset with the predictors and outcome.
#' @param family.value Specifies the family for running the lasso model.
#' The default is family.value = "binomial".
#' @param model.type Specifies which model we're trying to run.
#' Possible values: "lasso", "ridge", "elastic net"
#' @param max_lambda Specifies the maximum lambda considered for LASSO.
#' Default is 10
#' @param method Specifies which method to use for LASSO. Default is CV.
#' @param opt_lambda Specifies which lambda from CV you want to use for final
#' model. Default is min.
#'
#' @return
#' Returns the coefficient dataframe for the dataset.
#'
#' @examples penalized_regression(df, family.value = "binomial",
#' alpha.value = 1)
#'
#'
#'
#'
#' @export
penalized_regression <- function(df,
                           family.value = "binomial",
                           model.type = "lasso",
                           max_lambda = 10,
                           method = "CV",
                           opt_lambda = "min"){

  #####################
  #extract the covariates and outcomes
  #####################
  x <- subset(df, select = -AE)
  y <- subset(df, select = AE)

  #####################
  #create a data matrix from the dataframe of covariates
  #####################
  input = data.matrix(x)

  #####################
  #create a data matrix from vector of outcomes
  #####################
  output = data.matrix(y)

  #####################
  #alpha
  #####################
  if(model.type == "lasso"){
    alpha.value = 1
  } else if(model.type == "ridge"){
    alpha.value = 0
  } else if(model.type == "elastic net"){
    alpha.value = seq(0,1,length=20)
  }

  #maximum lambda
 # mysd <- function(y) sqrt(sum((y-mean(y))^2)/length(y))
 # sx <- scale(input, scale=apply(input, 2, mysd))
 # max_lambda <- norm(t(sx) %*% output, 'i') / nrow(input)

  #####################
  #use CV to find optimal lambda value
  #####################
  if(method == "CV"){

    #####################
    #perform k-fold cross-validation for LASSO
    #####################
    cv_model <- cv.glmnet(x = input, y = output, alpha = 1)

    #####################
    #find optimal lambda that minimizes test MSE
    #####################
    if(opt_lambda == "min"){
      optimal_lambda = cv_model$lambda.min
    }else{
      optimal_lambda = cv_model$lambda.1se
    }

    #####################
    #performs alpha selection for ELASTIC NET
    #####################
    if(model.type == "elastic net"){

      #####################
      #Initialize the deviance value
      #####################
      best_deviance <- Inf

      #####################
      #Go through all the alphas and select the one which gives best (training) deviance
      #####################
      s <- sample(nrow(input), 50)
      s_input <- input[s, ]
      s_output <- output[s]

      #####################
      #Iterate through all values of alpha
      #####################
      for(i in seq(alpha.value)){

        # Start with the first one
        best_alpha <- alpha.value[1]

        # Fit the model
        model <- glmnet(x = input, y = output, alpha = alpha.value[i],
                        lambda = optimal_lambda, family=family.value)

        # Calculate the deviance
        current_deviance <- deviance(model)

        #####################
        #If the model deviance is better than the previous, store it.
        #####################
        if(current_deviance < best_deviance){
          best_deviance <- current_deviance
          best_alpha <- alpha.value[j]
        }
      }
      #####################
      #Optimal alpha value
      #####################
      alpha.value <- best_alpha
    }


  }
  #####################
  #use BIC to find optimal lambda value
  #####################
  else if(method == "BIC"){

    #####################
    #set sequence of lambda
    #####################
    lambdas_to_try <- seq(0.0001, max_lambda, length.out = 100)

    #####################
    #initialized bic and alpha vectors
    #####################
    bic <- numeric(length(lambdas_to_try))
    alpha <- numeric(length(lambdas_to_try))

    #####################
    #start loop to run through all prospective lambdas
    #####################
    for (i in seq(lambdas_to_try)) {

      #set the lambda_value to the lambda we're trying out
      lambda_value <- lambdas_to_try[i]

      # Print the current index in the lambda sequence
      #  print(paste("Testing lambda index:", i, "with lambda value:", lambda_value))

      #####################
      # If we're doing elastic net, add a code chunk which iterates through a sequence of alphas
      # for each potential lambda value.
      #####################
      if(model.type == "elastic net"){

        # Initialize a variable to keep track of the best BIC for the current lambda
        best_bic <- Inf  # Start with a very large number
        best_alpha <- alpha.value[1] # Start with the first one

        #####################
        #Compute the BIC for the model for each alpha
        #####################
        for(j in seq(alpha.value)){
          model <- glmnet(x = input, y = output, alpha = alpha.value[j],
                          lambda = lambda_value, family=family.value)


          #####################
          # Compute the Total Log-Likelihood adjustment
          #####################
          tLL <- model$nulldev - deviance(model)
          k <- model$df
          n <- model$nobs
          current_bic <- log(n) * k - tLL

          # Print alpha iteration
          #      print(paste("Testing alpha index:", j, "with alpha value:", alpha.value[j]))

          #####################
          # Update best BIC if the current BIC is lower
          #####################
          if (current_bic < best_bic) {
            best_bic <- current_bic
            best_alpha <- alpha.value[j]
          }

        }

        #####################
        # Store the best BIC found for this lambda
        #####################
        bic[i] <- best_bic
        alpha[i] <- best_alpha


      }
      #####################
      #For LASSO
      #####################
      else{
        model <- glmnet(x = input, y = output, alpha = alpha.value,
                        lambda = lambda_value, family=family.value)

        #####################
        # Compute the Total Log-Likelihood adjustment
        #####################
        tLL <- model$nulldev - deviance(model)
        k <- model$df
        n <- model$nobs

        #####################
        # Compute information criteria
        #####################
        bic[i] <- log(n)*k - tLL
      }

    }

    #####################
    # Find the index of the minimum BIC
    #####################
    min_bic_index <- which.min(bic)

    #####################
    # (Elastic Net) Find the index of the minimum alpha
    #####################
    if(model.type == "elastic net"){
      alpha.value <- alpha[min_bic_index]
    }

    #####################
    # Extract the lambda corresponding to the minimum BIC
    #####################
    optimal_lambda <- lambdas_to_try[min_bic_index]

  }

  #####################
  #find the best model
  #####################
  best_model <- glmnet(x = input, y = output,
                       alpha = alpha.value,
                       lambda = optimal_lambda,
                       family = family.value)

  #####################
  #extract the coefficients of the best model
  #####################
  coef_best_model <- coef(best_model)

  #create a dataframe with the coefficients of the best model
  #along with their names
 # coef_data_frame <- data.frame(DRUG = coef_best_model@Dimnames[[1]][coef_best_model@i + 1],
 #                               COEF = coef_best_model@x)

  #output
 # coef_data_frame

  #####################
  #results
  #####################
  coef_best_model
}
