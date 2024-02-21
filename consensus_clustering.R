library("ConsensusClusterPlus")

#standard
perform_consensus_clustering = function(data, dataname,k=15,clusterAlg="hc",distance="pearson",
                                        innerLinkage="ward.D2",finalLinkage="ward.D2")
{d<-as.dist(1-cor(data,method=distance))
pdf_path = paste(path.expand("~"),paste("/", paste("consensusClusterPlus2",k=k,dataname,Sys.Date(),".pdf",sep="_"),sep=""),sep="")
print(pdf_path)
pdf(file=pdf_path)
results = ConsensusClusterPlus(d,maxK=k,reps=1000,pItem=0.8,pFeature=1, clusterAlg=clusterAlg,
                               distance=distance,innerLinkage=innerLinkage, 
                               finalLinkage=finalLinkage) 
icl = calcICL(results)
dev.off()
Clust<-results[[2]][["consensusClass"]]
for (i in 3:k){Clust<-cbind(Clust,results[[i]][["consensusClass"]])}
colnames(Clust)<-paste("k", 2:k,sep="")
write.table(Clust,file=paste("Clusters_",dataname, "_k2-k",k,Sys.Date(),".xls",sep=""),sep="\t",row.names=T)

return(results=results)
}

#png
perform_consensus_clustering_png = function(data, dataname,k=15,clusterAlg="hc",distance="pearson",
                                        innerLinkage="ward.D2",finalLinkage="ward.D2")
{d<-as.dist(1-cor(data,method=distance))
# png_path = paste(path.expand("~"),paste("/", paste("_consensusClusterPlus2",".png"),sep=""),sep="")
#png(filename = png_path)
wd=getwd()
# title=tempdir()
# title = gsub("*\\\\..*","",stri_reverse(title))
setwd(path.expand("~"))
rnum = sample.int(2, 1, TRUE) * 1e+4 + sample.int(2, 1, TRUE) * 1e+3 + sample.int(999, 1, TRUE)
plot_outdir = paste("consensus_plots",Sys.Date(),clusterAlg,distance,rnum, sep="_")
results = ConsensusClusterPlus(d,maxK=k,reps=1000,pItem=0.8,pFeature=1, clusterAlg=clusterAlg,
                               distance=distance,innerLinkage=innerLinkage,title=plot_outdir,
                               finalLinkage=finalLinkage, plot = "png") 
icl = calcICL(results)
dev.off()
folder_path = paste(getwd(),plot_outdir, sep="/")
print(folder_path)
setwd(folder_path)

Clust<-results[[2]][["consensusClass"]]
for (i in 3:k){Clust<-cbind(Clust,results[[i]][["consensusClass"]])}
colnames(Clust)<-paste("k", 2:k,sep="")
# write.table(Clust,file=paste("Clusters_",dataname, "_k2-k",k,Sys.Date(),".xls",sep=""),sep="\t",row.names=T)
write.csv(Clust,file=paste("Clusters_",dataname, "_k2-k",k,Sys.Date(),".csv",sep=""),row.names=T)
setwd(wd)
return(folder_path)
}

#pdf
perform_consensus_clustering_pdf = function(data, dataname,k=15,clusterAlg="hc",distance="pearson",
                                        innerLinkage="ward.D2",finalLinkage="ward.D2",
                                        pdf_path)
{d<-as.dist(1-cor(data,method=distance))

pdf_path = paste(pdf_path,paste("/", paste("consensusClusterPlus2",k=k,dataname,Sys.Date(),".pdf",sep="_"),sep=""),sep="")
pdf(file=pdf_path)
results = ConsensusClusterPlus(d,maxK=k,reps=100,pItem=0.8,pFeature=1, clusterAlg=clusterAlg,
                               distance=distance,innerLinkage=innerLinkage, 
                               finalLinkage=finalLinkage) 
icl = calcICL(results)
dev.off()

return(pdf_path)
}