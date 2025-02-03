############################################################################
# Deep Learning regression
#	read data
#	add scaling
# https://saturncloud.io/blog/how-to-improve-accuracy-in-neural-networks-with-keras/#:~:text=In%20this%20article%2C%20we%20explored,perform%20better%20on%20our%20data.
#
if (setdl==1) {
	setdldropout=0
	setdloptim=1

	x1<-cbind(dt$surface_carrez,dt$surface_reelle_bati,dt$nombre_pieces_principales,dt$surface_terrain,
		dt$longitude,dt$latitude,
#		dt$id_occurrence,
		dt$id_prefixe,dt$id_type_local
	)
	mx<-apply(x1,2,mean)
	ms<-apply(x1,2,sd)
	xt<-scale(x1,center=mx,scale=ms)
	xt<-x1
	yt<-cbind(dt$valeur_fonciere)
#	colnames(xt)<-c("surface_carrez","surface_reelle_bati","nombre_pieces_principales","surface_terrain",
#		"longitude","latitude","id_occurrence","id_prefixe","id_type_local")
#	colnames(yt)[1]<-"valeur_fonciere"

	x2<-cbind(dp$surface_carrez,dp$surface_reelle_bati,dp$nombre_pieces_principales,dp$surface_terrain,
		dp$longitude,dp$latitude,
#		dp$id_occurrence,
		dp$id_prefixe,dp$id_type_local
	)
	mx<-apply(x2,2,mean)
	ms<-apply(x2,2,sd)
	xp<-scale(x2,center=mx,scale=ms)
	xp<-x2
	yp<-cbind(dp$valeur_fonciere)
#	colnames(xp)<-c("valeur_fonciere","surface_carrez","surface_reelle_bati","nombre_pieces_principales","surface_terrain",
#	"longitude","latitude","id_occurrence","id_prefixe","id_type_local")
#	colnames(yp)[1]<-"id_mutation"

	library(keras)
	Sys.setenv(TF_ENABLE_ONEDNN_OPTS=0)

#	increase number of layers 	(64,32,1) > (64,64,1)
#	increase number of neurons	units=64 > 128
#	1/ (64,32,1) -> (64,64,1)	prediction accuracy2 = 74.9 > 76.7
#	2/ (64,64,1) -> (128,128,1)	prediction accuracy2 = 76.7 > 80.3
#	3/ dropout?

	# input_shape : ncol(x) columns
	if (setdldropout==0){
	    model_dl = keras_model_sequential() %>% 
		layer_dense(units=128, activation="relu", input_shape=ncol(xt)) %>% 
		layer_dense(units=128, activation = "relu") %>%
		layer_dense(units=1, activation="linear")
	} else {
	    model_dl = keras_model_sequential() %>% 
   		layer_dense(units=128, activation="relu", input_shape=ncol(xt)) %>% 
		layer_dropout(rate=0.5) %>%
   		layer_dense(units=128, activation = "relu") %>%
   		layer_dense(units=1, activation="linear")
	}

	model_dl %>% compile(
   		loss = "mse",
   		optimizer =  "adam", 
   		metrics = list("mean_absolute_error")
 	)
 
	model_dl %>% summary()

	# training the model and checking the accuracy
	# error "https://stackoverflow.com/questions/51853185/tf-keras-giving-nan-loss-and-non-validation-error"
	#       - change activation function from 'softmax' ('linear') to 'sigmoid'
	#       - & change loss to 'binary_crossentropy'
	#       - also check for null values
	#
	model_dl %>% fit(xt, yt, epochs = 100,verbose = 1)

	scores = model_dl %>% evaluate(xt, yt, verbose = 0)
	print(scores)

	# predict x data and compare with original y value in a plot
	training_dl = model_dl %>% predict(xt)
	prediction_dl = model_dl %>% predict(xp)

	training_dl = pmax(training_dl,0)
	prediction_dl = pmax(prediction_dl,1)

	stat_training_dl <- statdf(dt$valeur_fonciere,training_dl,dt)
	stat_prediction_dl <- statdf(dp$valeur_fonciere,prediction_dl,dp)

	plot(dt$valeur_fonciere,typ='l')
	lines(training_dl,typ='l',col='red')

	plot(dp$valeur_fonciere,typ='l',col='blue')
	lines(prediction_dl,typ='l',col='red')

	filter(dt,valeur_fonciere>2000000)
}

#######################################################################################
# input_shape : ncol(x) columns
#	model_dl = keras_model_sequential() %>% 
#   		layer_dense(units=64, activation="relu", input_shape=ncol(xt)) %>% 
#   		layer_dense(units=32, activation = "relu") %>% 
#   		layer_dense(units=1, activation="linear")
#> stat_training_dl
#             Item    Value
#1  mean(original) 419279.8
#2    sd(original) 253763.5
#3 mean(predicted) 423071.5
#4   sd(predicted) 222206.7
#5            rmse   3791.8
#6             mae  77231.1
#7        accuracy      0.2
#8       accuracy2     89.4
#9            rows     1319
#
#             Item    Value
#1  mean(original) 240440.0
#2    sd(original)  34003.4
#3 mean(predicted) 179954.4
#4   sd(predicted)  23226.7
#5            rmse  60485.6
#6             mae  60485.6
#7        accuracy        0
#8       accuracy2     74.9
#9            rows        2
#