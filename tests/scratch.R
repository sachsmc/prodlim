if(FALSE) {
    test <- SimSurv(10000)
head(test)
test$event[test$event == 1] <- rbinom(sum(test$event), 1, p = .2) + 1
table(test$event)
test$event <- as.factor(test$event)
object <- prodlim(Hist(time, event) ~ 1, data = test)
times <- 4
cause <- "1"


mr <- object$model.response
states <- attr(mr,"states")
C <- match(cause,states,nomatch=0)
D <- object$n.event[[C]]
D0 <- rowSums(do.call(cbind, object$n.event))
#  it is sufficient to consider time points where events occur
time <- object$time[D0>0]
Y <- object$n.risk[D0>0]

D <- D[D>0]
D0 <- D0[D0>0]

NU <- length(time)
obstimes <- mr[,"time"]
status <- mr[,"status"]
E <- getEvent(mr)
N <- length(obstimes)
tdex <- max(which(time <= times))
## idea: see leaveOneOut.survival
## browser()
## if (useC==TRUE){
## print(cbind(time=time,Y=Y,D=D))
loo2 <- .C("loo_comprisk2",
           Y = as.double(Y),
           D=as.double(D),
           D0 = as.double(D0),
           time=as.double(time),
           obsT=as.double(obstimes),
           status=as.double(status*(E==cause)),
           status0=as.double(status),
           F=double(N),
           N=as.integer(N),
           NT=as.integer(NU),
           Tdex=as.integer(tdex),
           PACKAGE="prodlim")$F


sFit <- prodlim(Hist(time,status)~1,data=data.frame(unclass(mr)))
S <- sFit$surv[D>0]
lagSk <- leaveOneOut.survival(sFit,times=time,lag=1)
## idea: see leaveOneOut.survival
## browser()
## if (useC==TRUE){
## print(cbind(time=time,Y=Y,D=D))
loo <- .C("loo_comprisk",
          Y = as.double(Y),
          D=as.double(D),
          time=as.double(time),  ## sorted and unique observation times
          obsT=as.double(obstimes), ### observed times
          status=as.double(status*(E==cause)),
          lagSurv=as.double(lagSk),
          F=double(NU*N),
          N=as.integer(N),
          NT=as.integer(NU),
          PACKAGE="prodlim")$F
out <- matrix(loo,nrow=N,ncol=NU,byrow=FALSE)

loo <- out[, tdex]

plot(loo, loo2)

}
