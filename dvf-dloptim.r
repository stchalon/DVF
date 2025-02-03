# dvf-dlo : dl optimization
#
if (setdloptim==1) {

# >>> x=xt, y=yt from training
x=xt
y=yt

# Next, we will tune the model
#
library(kerastuneR)

# split (x,y) into (x_data,y_data) and (x_data2,y_data2)
#
ntr=floor(nrow(x)/2)
nte=nrow(x)-ntr
x_data <- matrix(data=x[1:ntr,],nrow=ntr,ncol=ncol(x))
y_data <- matrix(data=y[1:ntr,],nrow=ntr,ncol=ncol(y))

x_data2<- matrix(data=x[(ntr+1):(nrow(x)),],nrow=nte,ncol=ncol(x))
y_data2<- matrix(data=y[(ntr+1):(nrow(y)),],nrow=nte,ncol=ncol(y))

# set hyper parameters ranges (min, max, step) used for optimization
#
hp = HyperParameters()
hp$Int('num_layers', 1L, 4L, 1L)
hp$Int('units', 32L, 128L, 32L)

# build_model: variable model building (number of layers, number of units per layer)
build_model <- function(hp) { 
  
  model <- keras_model_sequential() %>%
  layer_dense(units = hp$get('units'), activation = 'relu', input_shape = ncol(x))
  
  for (i in 1:hp$get('num_layers')) { 
    model %>% layer_dense(units = hp$get('units'), activation = 'relu')
  }
  
  model %>% layer_dense(units = 1, activation="linear")
  
  model %>% compile(
	loss = 'mse',	
	optimizer = 'adam',
        metrics = list("mean_absolute_error")
  )
  return(model)
}

# https://keras.io/keras_tuner/api/tuners/random/
# RandomSearch class
#
tuner = RandomSearch(
  hypermodel =  build_model,
  objective = 'loss',
  max_trials = 10,
  hyperparameters = hp,
  directory = 'C:\\ML\\',
  project_name = 'sampleRegression', 
  overwrite = TRUE
)

tuner %>% fit_tuner(x = x_data, y = y_data, 
                    epochs = 5, 
                    validation_data = list(x_data2, y_data2))

print("Completed")
tuner$search_space_summary()
# gives (min,max)=(1,4) & (32,128)

# https://keras.io/keras_tuner/getting_started/
tuner$results_summary()

#This method returns a list of models sorted by their validation accuracy, with the best model at index 0.
best_hps<-tuner$get_best_hyperparameters()[[1]]
modelt = build_model(best_hps)
summary(modelt)

########################################################################
# Finally, rebuild using best model (128, 128, 128, 1)
# Next, we'll create a keras sequential model.
#
modelb = keras_model_sequential() %>% 
   layer_dense(units=128, activation="relu", input_shape=ncol(x)) %>% 
   layer_dense(units=128, activation = "relu") %>% 
   layer_dense(units=128, activation = "relu") %>% 
   layer_dense(units=1, activation="linear")
 
modelb %>% compile(
   loss = "mse",
   optimizer =  "adam", 
   metrics = list("mean_absolute_error")
 )
 
modelb %>% summary()
modelb %>% fit(x, y, epochs = 100,verbose = 1)
 
scores = modelb %>% evaluate(x, y, verbose = 0)
print(scores)

# Next, we will compute predicted values and plot on top of actual data
y_predb = modelb %>% predict(x)

training_dlo = modelb %>% predict(xt)
prediction_dlo = modelb %>% predict(xp)

training_dlo = pmax(training_dlo,0)
prediction_dlo = pmax(prediction_dlo,1)

stat_training_dlo <- statdf(dt$valeur_fonciere,training_dlo,dt)
stat_prediction_dlo <- statdf(dp$valeur_fonciere,prediction_dlo,dp)

# plot original & predictions
plot(dt$valeur_fonciere, col="black", type = "l",lwd=1)
points(training_dlo, col="blue", type = "l",lwd=1)

plot_histogram_and_gaussian(dt$valeur_fonciere, training_dlo)
#plot_histogram_and_gaussian(dp$valeur_fonciere, prediction_dlo)

#plot(dp$valeur_fonciere, col="black", type = "l",lwd=1)
#points(prediction_dlo, col="blue", type = "l",lwd=1)

}