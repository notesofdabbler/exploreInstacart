
# Do a PCA analysis 
getpca = function(pcdata){
  
  # Perform a PCA on dataset pcadata
  pc_mdl = prcomp(x = pcdata[,-1], center = TRUE, scale. = FALSE)
  
  # Get the plot of cumulative variance explained by each principal component
  pc_mdl_var = cumsum(pc_mdl$sdev^2)/sum(pc_mdl$sdev^2)
  pc_mdl_vardf = data.frame(pc = 1:length(pc_mdl_var), cumvar = pc_mdl_var)
  p_cumvar = ggplot() + 
    geom_line(data = pc_mdl_vardf, aes(x = pc, y = cumvar)) + 
    geom_point(data = pc_mdl_vardf, aes(x = pc, y = cumvar)) + 
    xlab("PC") + ylab("Cumulative Variance") + 
    scale_x_continuous(breaks = 1:length(pc_mdl_var)) + 
    scale_y_continuous(labels = percent, breaks = seq(0,1,by=0.1)) + expand_limits(y = 0) + 
    theme_bw(20)
  
  return(list(pc_mdl = pc_mdl, p_cumvar = p_cumvar))
  
}

# Get PC loading plots and plots of items that have top/bottom 20 scores based on a principal component
get_pcaplots = function(pc_mdl, numcomp, origdata, pcdata, prods, xrange, xlabstr){

  # Get PC loadings
  pc_loadings = data.frame(pc_mdl$pc_mdl$rotation)
  pc_loadings$xrange = xrange
  pc_loadings_g = pc_loadings %>% gather(pc, pcload, -xrange)
  
  pcfilt = paste0("PC",seq(1,numcomp))
  pc_loadings_g = pc_loadings_g[pc_loadings_g$pc %in% pcfilt,]
  
  # plot PC loadings
  p_pcload = ggplot() + geom_line(data = pc_loadings_g, aes(x = xrange, y = pcload, color = pc)) +
    theme_bw(20)

  # get PC scores
  pc_scores = data.frame(pc_mdl$pc_mdl$x)
  pc_scores$product_id = pcdata$product_id
  pc_scores = inner_join(pc_scores, prods[,c("product_id", "product_name")], by = "product_id")
  
  # get plot of orders for items with top and bottom scores for each of the principal components that is chosen
  p_items = list()
  prodlist_topbot20 = list()
  
  for(i in 1:numcomp){
    
    # Sort by score for principal component i
    pc_scores_pick = pc_scores[order(-pc_scores[[paste0("PC", i)]]),]
    
    # Get items that are top20 and bottom20 in terms of the PC score
    prod_top20 = pc_scores_pick$product_name[1:20]
    prod_bot20 = pc_scores_pick$product_name[(nrow(pc_scores_pick) - 19):nrow(pc_scores_pick)]
    
    # plot orders in original data for the top and bottom 20 items in terms of PC score
    
    prodlist_topbot20[[i]] = data.frame(top_scores = prod_top20, bottom_scores = rev(prod_bot20))
    prodlist = c(prod_top20, rev(prod_bot20))
    prodlist_label = c(rep("top 20 scores", 20), rep("bottom 20 scores", 20))
    scoreid = c(seq(1,20), seq(1,20))
    prodlistdf = data.frame(product_name = prodlist, prodlist_label, scoreid, stringsAsFactors = FALSE)
    prodlistdf = inner_join(prodlistdf, prods[, c("product_id", "product_name")], by = "product_name")
    
    prodlist_profile = inner_join(origdata, prodlistdf[, c("product_id", "product_name", "prodlist_label")], by = "product_id")
    
    prodlist_profile_g = prodlist_profile %>% gather(xrange, pctorders, -product_name, -product_id, -prodlist_label)
    prodlist_text = bind_rows(data.frame(x = 3, y = 0.1, label = paste(prod_top20, collapse = "\n"), 
                                         prodlist_label = "top 20 scores", stringsAsFactors = FALSE),
                              data.frame(x = 3, y = 0.1, label = paste(prod_bot20, collapse = "\n"), 
                                         prodlist_label = "bottom 20 scores", stringsAsFactors = FALSE)
    )
    
    p_items[[i]] = ggplot() + 
      geom_line(data = prodlist_profile_g, aes(x = as.numeric(xrange), y = pctorders, 
                                               group = product_name, color = prodlist_label)) +
                       xlab(xlabstr) + ylab("% orders") + 
                       scale_x_continuous(breaks = xrange) + scale_y_continuous(labels = percent) + 
                       theme_bw(20) + theme(legend.title = element_blank())

    
  }
  
  return(list(p_pcload = p_pcload, p_items = p_items, prodlist_topbot20 = prodlist_topbot20))
}

# plot scores plot of PC1 and PC2 and color points that are top/bottom scores in terms of a principal component
get_scorepairplts = function(pc_mdl, pcrnk20){
  # get PC scores
  pc_scores = data.frame(pc_mdl$pc_mdl$x)
  
  # Sort by score for principal component i
  pc_scores_pick = pc_scores[order(-pc_scores[[pcrnk20]]), ]
  
  pc_scores_pick$label = c(rep(paste0("top 20", pcrnk20), 20), rep("black", nrow(pc_scores_pick) - 40), rep(paste0("bottom 20", pcrnk20), 20))
  p_scores = ggplot() + geom_point(data = pc_scores_pick, aes(x = PC1, y = PC2, color = label)) + theme_bw(20)
  
  return(list(p_scores = p_scores))
  
}

