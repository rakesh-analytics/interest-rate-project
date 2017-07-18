#Random Forest Model
library(randomForest)
library('dplyr')
library("caret")

#divide the data into train and test
# to get same data in each time
set.seed(123) 

train = data[sample(nrow(data), 20000, replace = F), ]

test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

rf_model <- randomForest(X1 ~ X4 + X8 + X9 + X13 + X21 + X22 + X24+ X27 + X28 + X29 + X30 +
                              X31 + credit_limit , data = train, 
                              importance = TRUE ,  ntree = 100)


## Predict using the test set
prediction = predict(rf_model, test)
importance = importance(rf_model, type = 1)

#calculate MAPE
mape = function(y, yhat)
  mean(abs((y - yhat)/y))

1-mape(test[,1], prediction)

# Get importance
importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance = round(importance[ , '%IncMSE'], 2))

# Create a rank variable based on importance
rankImportance = varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))




# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 6, colour = 'yellow') +
  labs(x = 'Variables') + coord_flip() + theme_few()

