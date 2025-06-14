
### Train data
train.data=read.csv("train.csv")
train.data[train.data == ""]=NA
train.clean=na.omit(train.data)
train.clean$group.pass=substr(train.clean$PassengerId, 1, 4)
write.csv(train.clean, "train_clean.csv", row.names=FALSE)
train.clean$Deck=substr(train.clean$Cabin, 1, 1)
train.clean$Side=substr(train.clean$Cabin, nchar(train.clean$Cabin),nchar(train.clean$Cabin))
train.clean$Side=ifelse(train.clean$Side == "P", "Port", "Starboard")
split.cabin=strsplit(train.clean$Cabin, "/")

# Extract the second element from each split result
middle_number <- sapply(split.cabin, function(x) x[2])
train.clean$Number=middle_number

### Test data
test.data=read.csv("test.csv")
test.data[test.data == ""]=NA
test.data=na.omit(test.data)
test.data$Deck=substr(test.data$Cabin, 1, 1)
test.data$Side=substr(test.data$Cabin, nchar(test.data$Cabin),nchar(test.data$Cabin))
test.data$Side=ifelse(test.data$Side == "P", "Port", "Starboard")
test.split_cabin <- strsplit(test.data$Cabin, "/")
# Extract the second element from each split result
test.middle_number=sapply(test.split_cabin, function(x) x[2])
test.data$Number=test.middle_number
write.csv(test.data, "test_data.csv", row.names=FALSE)



### cabin and transported status
train.clean$Transported <- as.factor(train.clean$Transported)
train.clean$Deck= as.factor(train.clean$Deck)
train.clean$Number=as.numeric(train.clean$Number)
train.clean$Side= as.factor(train.clean$Side)

test.data$Deck= as.factor(test.data$Deck)
test.data$Number=as.numeric(test.data$Number)
test.data$Side= as.factor(test.data$Side)

# chi-square test
deck.chq=chisq.test(table(train.clean$Deck, train.clean$Transported))
side.chq=chisq.test(table(train.clean$Side, train.clean$Transported))
vip.chq=chisq.test(table(train.clean$VIP, train.clean$Transported))
chisq.test(table(train.clean$Age, train.clean$Transported), simulate.p.value = TRUE)


# Build logistic regression model
library(caTools)
model=glm(Transported~ Deck + Number + Side,
           data=train.clean,
           family="binomial")
summary(model)
predicted_prob=predict(model, type = "response")
predicted_class=ifelse(predicted_prob > 0.5, TRUE, FALSE)
table(Predicted = predicted_class, Actual = train.clean$Transported)


test.data$PredictionProb=predict(model, newdata = test.data, type = "response")
test.data$PredictionTransported=ifelse(pred_prob > 0.5, TRUE, FALSE)
head(test.data[, c("PassengerId", "Transported")])
write.csv(test.data[, c("PassengerId", "Transported")], 
          "predicted_transport_status.csv", 
          row.names = FALSE)

### lm
lm=lm(Age~Transported, data=train.clean)



### ROC
library(pROC)
modelroc=glm(Transported ~ CryoSleep + Deck, data = train.clean, family = binomial)
summary(modelroc)
train.clean$predicted_prob=predict(modelroc, type = "response")
train.clean$Transported_numeric=as.numeric(train.clean$Transported) - 1
roc.curv=roc(train.clean$Transported_numeric, train.clean$predicted_prob)
plot(roc.curv, main = "ROC Curve for Transported Prediction", col = "blue")
abline(a = 0, b = 1, lty = 2, col = "gray")
auc(roc.curv)


### table
Deck.Cyrosleep=xtabs(~ Deck + CryoSleep + Transported, data = train.clean)


### plot
library(ggplot2)

side_table <- prop.table(table(train.clean$Side, train.clean$Transported), 1)
deck.table=prop.table(table(train.clean$Deck, train.clean$Transported), 1)

ggplot(train.clean, aes(x = Deck, fill = Transported)) +
  geom_bar(position = "fill") +  # Shows proportion (stacked to 100%)
  facet_wrap(~ Side) +          # Creates separate plots for Port & Starboard
  labs(title = "Transported Status by Deck and Side",
       x = "Deck",
       y = "Proportion of Passengers",
       fill = "Transported") +
  theme_minimal()

ggplot(train.clean, aes(x = CryoSleep, fill = Transported)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Deck) +
  labs(title = "Transported by CryoSleep and Deck", y = "Proportion") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  theme_minimal()

table(train.clean$Destination)
