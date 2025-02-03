################################################################################
# SVR
#
if (setsvr==1) {
	library(e1071)
	model_svr<-svm(valeur_fonciere~surface_carrez+surface_reelle_bati+nombre_pieces_principales+surface_terrain
		+longitude+latitude
#		+id_occurrence
		+id_prefixe
		+id_type_local,
		data=tt)

	ttune <- tune(svm,
		valeur_fonciere~surface_carrez+surface_reelle_bati+nombre_pieces_principales+surface_terrain
		+longitude+latitude
#		+id_occurrence
		+id_prefixe
		+id_type_local,
    		data = tt, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

	model_svrt <- ttune$best.model
	training_svr=predict(model_svrt,tt)
	prediction_svr=predict(model_svrt,tp)

	summary(model_svrt)
	statdf_svr <- statdf(dp$valeur_fonciere,prediction_svr,dp)
	stat_training_svr <- statdf(dt$valeur_fonciere,training_svr,dt)
	stat_prediction_svr <- statdf(dp$valeur_fonciere,prediction_svr,dp)
}
