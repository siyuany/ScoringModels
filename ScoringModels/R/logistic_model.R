#' Stepwise Logistic Regression
#'
#' @importFrom pROC roc
#' @importFrom pROC auc
#' @import doParallel
#' @import parallel
#' @import foreach
#' @export

logistic_model <- function(df,
                           y,
                           summary_woe,
                           max_nvars = NULL,
                           seed = NULL,
                           parallel = FALSE,
                           cv_nfold = 5,
                           auc_revise = 0,
                           ...) {
    predictors <- colnames(df)
    if (!any(y != predictors)) {
        stop("'y' does not exist in df")
    }
    predictors <- predictors[predictors != y]
    predictors_iv <- names(summary_woe)
    predictors <-
        intersect(predictors, paste0(predictors_iv, '_woe'))

    start <- sapply(predictors, function(predictor) {
        x <- substr(predictor, 1, stringr::str_length(predictor) - 4)
        summary_woe[[x]][['iv']]
    })
    iter_form <- as.formula(paste0(y, ' ~ 1'))

    step_record <- list()
    i <- 1

    if (is.null(seed))
        seed <- sample(1:10000, 1)

    registerDoParallel(detectCores(logical = FALSE))
    while (TRUE) {
        round_name <- paste0('Round_', i)
        i <- i + 1
        existed_terms <- all.vars(formula)[-1]
        model.accuracy <- data.frame(Model = NULL,
                                     Accuracy = NULL,
                                     SD = NULL)
        model.accuracy <-
            foreach(new_var = predictors[!predictors %in% existed_terms],
                    .combine = rbind) %dopar% {
                        new_formula <- update(iter_form,
                                              paste0('. ~ . + ', new_var))
                        acc <-
                            cv.glm(new_formula,
                                   df,
                                   seed = seed,
                                   family = binomial('logit'))
                        data.frame(Model = paste('+ ', new_var),
                                   Accuracy = acc[1] - auc_revise * acc[2],
                                   SD = acc[2])
                    }
        if (length(existed_terms) >= 2) {
            model.accuracy <- rbind(
            model.accuracy,
            foreach(old_var = existed_terms,
                    .combine = rbind) %dopar% {
                        new_formula <- update(iter_form, paste0('. ~ . - ', old_var))
                        acc <- cv.glm(new_formula,
                                      df,
                                      seed = seed,
                                      family = binomial('logit'))
                        data.frame(Model = paste('- ', old_var),
                                   Accuracy = acc[1] - auc_revise * acc[2],
                                   SD = acc[2])
                    })
        }
        acc <-
            cv.glm(iter_form, df, seed = seed, family = binomial('logit'))

        model.accuracy <-
            rbind(data.frame(Model = '.',
                             Accuracy = acc[1] - auc_revise * acc[2],
                             SD = acc[2]),
                  model.accuracy)
        model.accuracy <-
            model.accuracy[order(model.accuracy$Accuracy, decreasing = TRUE),]
        new.var <- as.character(model.accuracy$Model[1])

        if (new.var == '.')
            break
        # if ((model.accuracy[1, 'Accuracy'] - acc[1]) / model.accuracy[1, 'SD'] < 1)
        #     break
        if ((model.accuracy[1, 'Accuracy'] - acc[1]) / acc[1] < 0.01)
            break

        iter_form <- update(iter_form, paste0('. ~ . + ', new.var))
        step_record[[round_name]] <-
            list(formula = iter_form,
                 revised_auc = model.accuracy[1, 'Accuracy'])
    }
    stopImplicitCluster()
    step_record
}

cv.glm <- function(formula, data, seed, nfold = 10, ...) {
    set.seed(seed)
    n_obs <- nrow(data)
    index <- sample(rep(1:nfold, length = n_obs))
    formula = formula(formula)
    response = all.vars(formula)[1]

    accuracy <- vector(mode = 'numeric', length = nfold)
    for (i in 1:10) {
        rf.fit <- glm(formula, data[index != i, ], ...)
        pred <- predict(rf.fit, newdata = data[index == i, ], type = 'response')
        accuracy[i] <- auc(roc(data[index == i, response], pred))
    }

    c(mean(accuracy), sd(accuracy))
}
