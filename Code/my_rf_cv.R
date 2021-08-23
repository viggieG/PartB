#' Random forest cross-validation function
#' 
#' This function will predict body_mass_g by building and using a random forest 
#'   cross-validation model, which uses covariates bill_length_mm, 
#'   bill_depth_mm, and flipper_length_mm.
#' 
#' @param k Integer representing the number of folds.
#' @keywords prediction
#'
#' @return A numeric with the cross-validation error.
#'   
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#' 
#' @export
my_rf_cv <- function(k) {
  clean <- drop_na(my_penguins)
  # create variable to store
  diff <- 0
  # Split data in k parts, randomly
  inds <- sample(rep(1:k, length = nrow(clean)))
  # combine the inds into data
  clean$inds <- inds
  # go through all the data
  for (i in 1:k) {
    # filter the data used to train
    data_train <- clean %>% 
      dplyr::filter(inds != i)
    # filter the data used to test
    data_test <-  clean %>% 
      dplyr::filter(inds == i)
    # build the predicting model
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + 
                             bill_depth_mm  + flipper_length_mm, 
                             data = clean, ntree = 100)
    # predict the object
    pred <- predict(model, data_test[, -1])
    # store the MSE
    diff <- append(diff, mean((pred - clean$body_mass_g) ^ 2))
  }
  # return the MSE
  return(diff[-1])
}