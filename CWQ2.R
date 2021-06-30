rm(list = ls())

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)
library(dplyr)

dollar.exchange <- read_excel("D:/EDUCATION/IIT/SECOND YEAR/SECOND SEMESTER/MACHINE LEARNING/Coursework 01/ExchangeUSD.xlsx") %>%
  janitor::clean_names() %>%
  mutate(YMD_date = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(YMD_date,everything())


#all the input is in only one dataframe to be able to preserve the testing and training
x.new <-as.data.frame(exchangeGBP[,3])

#dataset for the two sets of input variables
dollar.exchange.full = dollar.exchange %>%
  mutate(prior_day_one_set_a = lag(dollar.exchange$usd_eur,1),
         prior_day_one_set_b = lag(dollar.exchange$usd_eur,1),
         prior_day_two_set_b = lag(dollar.exchange$usd_eur,2),
         prior_day_one_set_c = lag(dollar.exchange$usd_eur,1),
         prior_day_two_set_c = lag(dollar.exchange$usd_eur,2),
         prior_day_three_set_c = lag(dollar.exchange$usd_eur,3),
         prior_day_one_set_d = lag(dollar.exchange$usd_eur,1),
         prior_day_two_set_d = lag(dollar.exchange$usd_eur,2),
         day_five_rolling = rollmean(usd_eur,5, fill = NA),
         day_ten_rolling = rollmean(usd_eur,10, fill = NA)) %>%
  
  
  drop_na()

dollar.exchange.full %>%
  pivot_longer(cols = 4,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(YMD_date,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Input Variables- First Set") +
  theme(legend.position = "none")


dollar.exchange.full %>%
  pivot_longer(cols = c(5,6),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(YMD_date,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Input Variables- Set Two") +
  theme(legend.position = "none")

dollar.exchange.full %>%
  pivot_longer(cols = 7:9,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(YMD_date,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Input Variables- Set Three") +
  theme(legend.position = "none")

dollar.exchange.full %>%
  pivot_longer(cols = 10:13,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(YMD_date,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Input Variables- Set Four") +
  theme(legend.position = "none")

# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }


# All the variables are normalized
normalized_data = dollar.exchange.full %>%
  mutate(across(3:12, ~normalize(.x)))


# Look at the data that has been normalized
summary(normalized_data)

set.seed(123)
dol_train <- normalized_data[1:400,]
dol_test <- normalized_data[401:491,]


# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }



# Get the min and max of the original training values
min_dol_train <- min(dollar.exchange.full[1:400,3])
max_dol_train <- max(dollar.exchange.full[1:400,3])



# Get the min and max of the original testing values
min_dol_test <- min(dollar.exchange.full[401:491,3])
max_dol_test <- max(dollar.exchange.full[401:491,3])


# Check the range of the min and max of the training dataset
min_dol_test

min_dol_train

max_dol_test

max_dol_train

relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}





relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



############################################################################################################

# function setup that creates 1 layer model for Input Variable One

set.seed(12345)

model1_hidden_layers_A = function(hidden) {
  nn.model = neuralnet(usd_eur ~ prior_day_one_set_a, data=dol_train, hidden=c(
    hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[3:4])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "One Hidden Layer") %>%
    mutate(hidden_layers = paste0(hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
One_hidden_layers_results_A = bind_rows(
  lapply(1:10, function(n) {
    model1_hidden_layers_A(n)
  })) %>%
  janitor::clean_names()



# save the stat indices to a dataframe

one_layer_model_set.a = One_hidden_layers_results_A %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(one_layer_model_set.a[1:10,])


#################################PLOTTING THE GRAGH FOR THE SINGLE LAYER MODEL WITH HIGHEST ACCURACY##############
### NEURONS=10 

# Train:
set.seed(0)
final.nn.model1 <- neuralnet(usd_eur ~ prior_day_one_set_a, data=dol_train, hidden=c(
  10), linear.output=TRUE)

# Predict:
nn_predictions_A1 <- as.numeric(neuralnet::compute(final.nn.model1,dol_test[3:4])$net.result
)
# Re-scale:
predictions_A1 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_A1,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_A1,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "green",
            data = predictions_A1) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate-SET A",
       subtitle = "Green is for predictions made with a neural network with 1 layers containing 10 hidden neurons, respectively")

predictions_A1 %>% select(YMD_date,actual_value,value_predicted)






# function setup that creates 2 layer model for Input Variable One
model2_hidden_layers_A = function(hidden,sec_hidden) {
  nn.model = neuralnet(usd_eur ~ prior_day_one_set_a, data=dol_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[3:4])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hidden_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
Two_hidden_layers_results_A = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model2_hidden_layers_A(n,m)
      })
    )
  })) %>%
  janitor::clean_names()



# save the stat indices to a dataframe

two_layer_model_set.a = Two_hidden_layers_results_A %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(two_layer_model_set.a[1:10,])




##we can say that the model with 2 hidden layers with 1 and 2 nodes 
##have the highest accuracy as it has the lowest mae and mape value

# Train:
set.seed(0)
final.nn.model2 <- neuralnet(usd_eur ~ prior_day_one_set_a, data=dol_train, hidden=c(
  1,2), linear.output=TRUE)

# Predict:
nn_predictions_A2 <- as.numeric(neuralnet::compute(final.nn.model2,dol_test[3:4])$net.result
)
# Re-scale:
predictions_A2 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_A2,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_A2,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "red",
            data = predictions_A2) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET A",
       subtitle = "Red is for predictions made with a neural network with 2 layers containing 1 and 2 hidden neurons, respectively")

###view predicted value and actual value
predictions_A2 %>% select(YMD_date,actual_value,value_predicted)


################################################################################################################3
##################Input Variable Set 2- ONE HIDDEN LAYER########################
set.seed(12345)

model1_hidden_layers_B = function(hidden) {
  nn.model = neuralnet(usd_eur ~ (prior_day_one_set_b+prior_day_two_set_b), data=dol_train, hidden=c(
    hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[3,5:6])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "One Hidden Layers") %>%
    mutate(hidden_layers = paste0(hidden),
           input_set = "B") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
One_hidden_layers_results_B = bind_rows(
  lapply(1:10, function(n) {
    model1_hidden_layers_B(n)
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe

one_layer_model_set.b = One_hidden_layers_results_B %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(one_layer_model_set.b[1:10,])


#################################PLOTTING FOR THE SINGLE LAYER MODEL WITH HIGHEST ACCURACY-SET 2##############
### NEURONS=3 

# Train:
set.seed(0)
final.nn.model3 <- neuralnet(usd_eur ~ (prior_day_one_set_b+prior_day_two_set_b), data=dol_train, hidden=c(
  3), linear.output=TRUE)

# Predict:
nn_predictions_B1 <- as.numeric(neuralnet::compute(final.nn.model3,dol_test[3,5:6])$net.result
)
# Re-scale:
predictions_B1 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_B1,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_B1,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "green",
            data = predictions_B1) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET B",
       subtitle = "Green is for predictions made with a neural network with 1 layers containing 3 hidden neurons, respectively")



predictions_B1 %>% select(YMD_date,actual_value,value_predicted)



############# function setup that creates 2 layer model##########

set.seed(12345)

model2_hidden_layers_B = function(hidden,sec_hidden) {
  nn.model = neuralnet(usd_eur ~ (prior_day_one_set_b+prior_day_two_set_b), data=dol_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[3,5:6])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "B") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
Two_hidden_layers_results_B = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model2_hidden_layers_B(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe

two_layer_model_set.b = Two_hidden_layers_results_B %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(two_layer_model_set.b[1:10,])




#################################PLOTTING FOR THE DOUBLE LAYER MODEL WITH HIGHEST ACCURACY- SET B##############
### NEURONS=9,1

# Train:
set.seed(0)
final.nn.model4 <- neuralnet(usd_eur ~ (prior_day_one_set_b+prior_day_two_set_b), data=dol_train, hidden=c(
  9,1), linear.output=TRUE)

# Predict:
nn_predictions_B2 <- as.numeric(neuralnet::compute(final.nn.model4,dol_test[5:6])$net.result
)
# Re-scale:
predictions_B2 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_B2,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_B2,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "red",
            data = predictions_B2) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET B",
       subtitle = "Red is for predictions made with a neural network with 2 layers containing 9 and 1 hidden neurons, respectively")

predictions_B2 %>% select(1,3,4)





















###########################Input Variable Set 3########################



################################################################################################################3
##################Input Variable Set 3- ONE HIDDEN LAYER########################
set.seed(12345)

model1_hidden_layers_C = function(hidden) {
  nn.model = neuralnet(usd_eur ~ (prior_day_one_set_c+prior_day_two_set_c+prior_day_three_set_c), data=dol_train, hidden=c(
    hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[7:9])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "One Hidden Layers") %>%
    mutate(hidden_layers = paste0(hidden),
           input_set = "C") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
One_hidden_layers_results_C = bind_rows(
  lapply(1:10, function(n) {
    model1_hidden_layers_C(n)
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe

one_layer_model_set.c = One_hidden_layers_results_C %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(one_layer_model_set.c[1:10,])


#################################PLOTTING FOR THE SINGLE LAYER MODEL WITH HIGHEST ACCURACY##############
### NEURONS=10

# Train:
set.seed(0)
final.nn.model5 <- neuralnet(usd_eur ~ (prior_day_one_set_c+prior_day_two_set_c+prior_day_three_set_c), data=dol_train, hidden=c(
  10), linear.output=TRUE)

# Predict:
nn_predictions_C1 <- as.numeric(neuralnet::compute(final.nn.model5,dol_test[7:9])$net.result
)
# Re-scale:
predictions_C1 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_C1,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_C1,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "green",
            data = predictions_C1) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET C",
       subtitle = "Green is for predictions made with a neural network with 1 layers containing 3 hidden neurons, respectively")

predictions_C1 %>% select(1,3,4)


############# function setup that creates 2 layer model##########

set.seed(12345)

model2_hidden_layers_C = function(hidden,sec_hidden) {
  nn.model = neuralnet(usd_eur ~ (prior_day_one_set_c+prior_day_two_set_c+prior_day_three_set_c), data=dol_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[7:9])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "C") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
Two_hidden_layers_results_C = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model2_hidden_layers_C(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe

two_layer_model_set.c = Two_hidden_layers_results_C %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(two_layer_model_set.c[1:10,])



#################################PLOTTING FOR THE DOUBLE LAYER MODEL WITH HIGHEST ACCURACY##############
### NEURONS=10,1

# Train:
set.seed(0)
final.nn.model6 <- neuralnet(usd_eur ~ (prior_day_one_set_c+prior_day_two_set_c+prior_day_three_set_c), data=dol_train, hidden=c(
  2,3), linear.output=TRUE)

# Predict:
nn_predictions_C2 <- as.numeric(neuralnet::compute(final.nn.model5,dol_test[7:9])$net.result
)
# Re-scale:
predictions_C2 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_C2,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_C2,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "red",
            data = predictions_C2) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET C",
       subtitle = "Red is for predictions made with a neural network with 1 layers containing 3 hidden neurons, respectively")




predictions_C2 %>% select(1,3,4)













##################Input Variable Set 4########################

####################SINGLE LAYER MODEL############################
set.seed(12345)

model1_hidden_layers_D = function(hidden) {
  nn.model = neuralnet(usd_eur ~ (prior_day_one_set_d+prior_day_two_set_d+day_five_rolling+day_ten_rolling), data=dol_train, hidden=c(
    hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[3,10:13])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hidden_layers = paste0(hidden),
           input_set = "D") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
One_hidden_layers_results_D = bind_rows(
  lapply(1:10, function(n) {
    model1_hidden_layers_D(n)
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe

one_layer_model_set.d = One_hidden_layers_results_D %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(one_layer_model_set.d[1:10,])




#################################PLOTTING FOR THE SINGLE LAYER MODEL WITH HIGHEST ACCURACY##############
### NEURONS=2

# Train:
set.seed(0)
final.nn.model7 <- neuralnet(usd_eur ~ (prior_day_one_set_d+prior_day_two_set_d+day_five_rolling+day_ten_rolling), data=dol_train, hidden=c(
  2), linear.output=TRUE)

# Predict:
nn_predictions_D1 <- as.numeric(neuralnet::compute(final.nn.model7,dol_test[10:13])$net.result
)
# Re-scale:
predictions_D1 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_D1,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_D1,min_dol_train, max_dol_train)
)



ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "green",
            data = predictions_D1) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET D",
       subtitle = "Green is for predictions made with a neural network with 1 layers containing 3 hidden neurons, respectively")




predictions_D1 %>% select(1,3,4)





############# function setup that creates 2 layer model##########
set.seed(12345)

model2_hidden_layers_D = function(hidden,sec_hidden) {
  nn.model = neuralnet(usd_eur ~ (prior_day_one_set_d+prior_day_two_set_d+day_five_rolling+day_ten_rolling), data=dol_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn.model,dol_test[3,10:13])
  truthcol = dollar.exchange.full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,min_dol_train, max_dol_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hidden_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "D") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
Two_hidden_layers_results_D = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model2_hidden_layers_D(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe

two_layer_model_set.d = Two_hidden_layers_results_D %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(two_layer_model_set.d[1:10,])


#################################PLOTTING FOR THE DOUBLE LAYER MODEL WITH HIGHEST ACCURACY##############
### NEURONS=1,2


set.seed(0)
final.nn.model8 <- neuralnet(usd_eur ~ (prior_day_one_set_d+prior_day_two_set_d+day_five_rolling+day_ten_rolling), data=dol_train, hidden=c(
  1,2), linear.output=TRUE)

# Predict:
nn_predictions_D2 <- as.numeric(neuralnet::compute(final.nn.model8,dol_test[10:13])$net.result
)
# Re-scale:
predictions_D2 <- data.frame(
  YMD_date = dol_test$YMD_date,
  normalized = nn_predictions_D2,
  actual_value= unnormalize(dol_test$usd_eur,min_dol_test, max_dol_test),
  value_predicted = unnormalize(nn_predictions_D2,min_dol_train, max_dol_train)
)


ggplot(dplyr::filter(dollar.exchange.full),
       aes(x = YMD_date, y = usd_eur)) +
  geom_line() +
  geom_line(aes(y = value_predicted), color = "red",
            data = predictions_D2) +
  theme_minimal() +
  labs(x = "Date", y = "usd_eur",
       title = "Forecast of the dollar Exchange rate- SET D",
       subtitle = "Red is for predictions made with a neural network with 1 layers containing 3 hidden neurons, respectively")


predictions_D2 %>% select(1,3,4)









