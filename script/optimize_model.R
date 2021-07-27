library(ParBayesianOptimization)
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




bounds <- list(
    n_leaves = c(10L, 200L),
    b_fraction = c(0.25, 1),
    f_fraction = c(0.25, 1),
    learning_rate = c(0.01, 0.1)
)

scoringFunction <- function(n_leaves, b_fraction, f_fraction, learning_rate) {

    pars = list(objective = "binary",
                boosting = "gbdt",
                learning_rate = learning_rate,
                num_iterations = 10000,
                num_leaves = n_leaves,
                max_depth = -1,
                feature_fraction = f_fraction,
                bagging_fraction = b_fraction,
                early_stopping_round = 10)


    lgbmcv <- lgb.cv(
        params = pars,
        data = training,
        nfold = 10
    )

    return(list(Score = -lgbmcv$best_score,
                nrounds = lgbmcv$best_iteration
    )
    )
}

optimization_object <- bayesOpt(
    FUN = scoringFunction,
    bounds = bounds,
    initPoints = 5,
    iters.n = 5,
    iters.k = 1,
    plotProgress = TRUE,
    verbose = 2
)


optimization_object <- addIterations(optimization_object, 1, verbose = TRUE)
