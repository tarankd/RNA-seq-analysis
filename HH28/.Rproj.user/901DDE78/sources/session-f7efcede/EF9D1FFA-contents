


umap_trial <- function(sObj, resolutions = c(0.6, 0.7, 0.8, 0.9, 1),
                           dims, label.size = 10, repel = TRUE){
  
  plots <- list()
  objects <- list()
  
  for(res in resolutions){
    
    
    objres <- FindClusters(sObj, resolution = res)
    objres <- RunUMAP(objres, dims = dims)
    
    
    
    plots[[as.character(res)]] <- DimPlot(
      objres, reduction = "umap", label = TRUE, label.size = 10, 
      repel = TRUE) + ggtitle(paste0("Resolution = ", res))
    
    objects[[as.character(res)]] <- objres
  }
  return(list(plots = plots, objects = objects))
}




output <- umap_trial(HH28A, dims = 1:30)

wrap_plots(plotlist = output$plots, ncol = 2)





trial_2 <- function(sObj, resolutions = c(0.6, 0.7, 0.8, 0.9, 1),
                       dims, label.size = 10, repel = TRUE){
  
  plots <- list()
  objects <- list()
  
  for(res in resolutions){
    
    
    objres <- FindClusters(sObj, resolution = res)
    objres <- RunUMAP(objres, dims = dims)
    
    
    
    plots[[as.character(res)]] <- DimPlot(
      objres, reduction = "umap", label = TRUE, label.size = 10, 
      repel = TRUE) + ggtitle(paste0("Resolution = ", res))
    
    objects[[as.character(res)]] <- objres
  }
  return(list(plots = plots, objects = objects))
}

