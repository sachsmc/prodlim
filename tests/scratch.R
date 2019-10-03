if(FALSE) {




    test <- SimSurv(40000)
    head(test)
    test$event[test$event == 1] <- rbinom(sum(test$event), 1, p = .2) + 1
    table(test$event)
    test$event <- as.factor(test$event)
    object <- prodlim(Hist(time, event) ~ 1, data = test)
    times <- 4
    cause <- "1"
    Fee <- predict(object,times=times,newdata=object$model.response,cause=cause)
    loo.main <- leaveOneOut.competing.risks(object, times = 4, cause = "1")
    N <- length(loo.main)
    Jk <- N*Fee-(N-1)*loo.main

    Jk <- Jk[object$originalDataOrder]
    Jk2 <- jackknife(object, times = 4, cause = "1")
    Jk3 <- pseudo::pseudoci(test$time, as.integer(test$event) - 1, tmax = 4)$pseudo$cause1[, 1]

    loo2b <- leaveOneOut.competing.risks2(object, times = 4, cause = "1")
    Sk <- (N*Fee-(N-1)*loo2b)[object$originalDataOrder]

}
