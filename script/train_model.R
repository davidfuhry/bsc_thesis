library(Matrix)
library(caret)
library(lightgbm)

set.seed(42)

inTraining <- as.vector(createDataPartition(features$is_conspiracy, p = .66, list = FALSE))
training <- features[inTraining,]
validate  <- features[-inTraining,]

# LightGBM

target <- training$is_conspiracy
training <- Matrix(as.matrix(training[, -3:-1]), sparse = TRUE)

validate_target <- validate$is_conspiracy
validate <- Matrix(as.matrix(validate[, -3:-1]), sparse = TRUE)

training <- lgb.Dataset(data = training, label = target)


pars = list(objective = "binary",
            learning_rate = 0.05,
            num_iterations = 10000L,
            max_depth = -1L,
            num_leaves = 50L,
            early_stopping_round = 10L)

lgb_test <- lgb.cv(params = pars,
                   data = training,
                   nfold = 10L)

pars$num_iterations <- round(lgb_test$best_iter + (lgb_test$best_iter / 10))

# model <- lightgbm(data = training,
#                   params = pars)
#
# predicted <- predict(model, data = validate)



# Cross validate

set.seed(42)

folds <- caret::createFolds(features$is_conspiracy, k = 10, list = TRUE)

results <- numeric()

for (i in 1:10) {
    inValidate <- folds[[i]]

    training <- features[-inValidate,]
    validate  <- features[inValidate,]

    # LightGBM

    target <- training$is_conspiracy
    training <- Matrix(as.matrix(training[, -3:-1]), sparse = TRUE)

    validate_target <- validate$is_conspiracy
    validate <- Matrix(as.matrix(validate[, -3:-1]), sparse = TRUE)

    training <- lgb.Dataset(data = training, label = target)

    model <- lightgbm(data = training,
                      params = pars)

    predicted <- predict(model, data = validate)

    cp <- cutpointr::cutpointr(predicted, validate_target)

    predicted <- ifelse(predicted >= cp$optimal_cutpoint, 1, 0)

    result <- caret::confusionMatrix(as.factor(predicted), as.factor(validate_target), positive = "1")$overall

    results <- rbind(results, result)
}


rm(inTraining, training, target, validate, pars, lgb_test, model, folds, cp, result, i, inValidate, predicted, validate_target)

gc()
