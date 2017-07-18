###Linear regression model with the same pre processing techniques
lm_model = lm(X1 ~ X4 + X9 +X13 + X21 + X22 + X24+ X27 + X28 + X29 + X30 +
                X31 + credit_limit, data = train)
summary(lm_model)
pred = predict(lm_model, test[,c(4,9,12,13,21,22,24,27:29,30,31,33)])

#calculate MAPE
mape = function(y, yhat)
  mean(abs((y - yhat)/y))

1-mape(test[,1], pred)

#extract coefficients from lm
coeff = data.frame(Coeff = lm_model$coefficients, StdError = summary(lm_model)$coefficients[, 2],
                   tValue = summary(lm_model)$coefficients[, 3],
                   Pval = summary(lm_model)$coefficients[, 4])
coeff$variables = row.names(coeff)
coeff = coeff[,c(5,1:4)]
rownames(coeff) = NULL
write.csv(coeff, "Coefficients_stats.csv", row.names = F)
