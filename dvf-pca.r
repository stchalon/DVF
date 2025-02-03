##########################################################################################################################
# PCA on training
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/
# valeur_fonciere~surface_carrez+surface_reelle_bati+nombre_pieces_principales+surface_terrain
#
if (setpca==1) {

    library('FactoMineR')

    dfp<-data.frame(
        dt$id_mutation,
        dt$surface_carrez,
	dt$surface_reelle_bati,
	dt$nombre_pieces_principales,
	dt$surface_terrain,
	dt$longitude,dt$latitude,
	dt$id_prefixe,dt$id_type_local
    )
    colnames(dfp)<-c("id_mutation",
	"surface_carrez","surface_reelle_bati","nombre_pieces_principales","surface_terrain",
	"longitude","latitude",
	"id_prefixe","id_type_local")
    write.csv(dfp,file='c:/ML/print_dfp.csv')

    # removing not quantitative columns: id_mutation
    res.pca<-PCA(dfp[,-c(1:1)])

    heatmap(cor(dfp[,-(1:1)]))

    library("factoextra")
    fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# http://factominer.free.fr/factomethods/analyse-en-composantes-principales.html
    plot.PCA(res.pca, axes=c(1, 2), choix="var")
    plot.PCA(res.pca, axes=c(1, 2), choix="ind")

    dimdesc(res.pca, axes=c(1,2))
}
