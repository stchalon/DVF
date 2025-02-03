####################################################################################################
# MLR :
# model_mlr    : dt/tt -> training_mlr,         dp/tp -> prediction_mlr
#
#
if (setmlr==1) {

	model_mlr<-lm(
		valeur_fonciere~surface_carrez+surface_reelle_bati+nombre_pieces_principales+surface_terrain+longitude+latitude
		+id_prefixe+id_type_local+code_postal,
		data=tt) 

	training_mlr<-predict(model_mlr,tt)
	prediction_mlr<-predict(model_mlr,tp)

	summary(model_mlr)
	anova(model_mlr)
	stat_training_mlr <- statdf(dt$valeur_fonciere,training_mlr,dt)
	stat_prediction_mlr <- statdf(dp$valeur_fonciere,prediction_mlr,dp)

#	plot_original_predicted(dt$valeur_fonciere, training_mlr)
	plot_original_predicted(dp$valeur_fonciere, prediction_mlr)
    
#    	plot_histogram_and_gaussian(dt$valeur_fonciere, training_mlr)
#    	plot_histogram_and_gaussian(dp$valeur_fonciere, prediction_mlr)
    
	stat_training_mlr
	stat_prediction_mlr

#    	plot_barchart(dt$id_mutation,dt$valeur_fonciere, training_mlr)
    	plot_barchart(dp$id_mutation,dp$valeur_fonciere, prediction_mlr)
}
