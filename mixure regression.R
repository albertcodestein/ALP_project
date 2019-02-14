library(flexmix)


out1 <- stepFlexmix(LogTrnsRev ~ hits + I(hits^2) + log(visitNumber) + as.factor(day) + as.factor(newVisits) + week +
                            as.factor(isMobile) + as.factor(isTrueDirect), k = 1:3, nrep = 5, control = list(verbose = 0), data = temp2)

out1

out2 <- getModel(out1,"BIC")
summary(out2)

out3 <- refit(out2)
summary(out3)

pr<-posterior(out2)

pred <- predict(out2, temp2)

class1 <- unlist(pred.mixure[[1]])
class2 <- unlist(pred.mixure[[2]])

total.pred <- class1 * pr[,1] + class2 * pr[,2]
sum(total.pred)

clust <- clusters(out2, temp2)

result = cbind(temp2,data.frame(pred),data.frame(clust))

final.pred <- vector()

for (i in 1:nrow(result)) {
        final.pred[i] = ifelse(result$clust[i] == 1, result$Comp.1[i], result$Comp.2[i])       
}

RMSE <- sqrt(mean((total.pred - temp2$transactionRevenue)^2))


ggplot(temp2$transactionRevenue, pred.mixure,xlab="Actual",ylab="Predicted")

hist(log(class1))




