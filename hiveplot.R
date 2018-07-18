mod.edge2HPD <- function(edge_df = NULL, unique.rows = TRUE, axis.cols = NULL, type = "2D", desc = NULL, edge.weight = NULL, edge.color = NULL, node.color = NULL, node.size = NULL, node.radius = NULL, node.axis = NULL) 
{
  #edge.weight - a list corresponding to edge weights (same order as in edge_df)
  #edge.color - a lis corresponding to edge colors (same order as in edge_df)
  #node.color - a data frame consisting of two columns: column 1 - node labels, column 2 - node color
  #node.size - a data frame consisting of two columns: column 1 - node labels, column 2 - node size
  #node.radius - a data frame consisting of two columns: column 1 - node labels, column 2 - node radius
  #node.axis - a data frame consisting of two columns: column 1 - node labels, column 2 - node axis
  
  if (is.null(edge_df)){
    stop("No edge data provided")
  }
  if (!is.data.frame(edge_df)){
    stop("edge_df is not a data frame")
  }
  if (unique.rows)
  {
    nr.old <- nrow(edge_df)
    edge_df <- unique(edge_df)
    
    if (nr.old > nrow(edge_df))
      cat("\n\t", nr.old - nrow(edge_df), "non-unique data-frame rows removed!\n\n")
  }
  
  # Get node labels
  lab1 <- as.character(unlist(edge_df[, 1]))
  lab2 <- as.character(unlist(edge_df[, 2]))
  
  
  # Get number of unique nodes
  nn <- length(unique(c(lab1, lab2)))
  
  # Define node ID
  id <- 1:nn
  # Define node label
  label <- unique(c(lab1, lab2))
  # Create a data frame for node attributes
  node.attributes <- data.frame(id, label)
  
####################################################
# Node size definition
  if (!is.null(node.size))
  {
    if (is.numeric(node.size[, 2]) | is.integer(node.size[, 2]))
    {
      nSize <- c()
      
      for (i in 1:length(label))
      {
        indx <- which(as.character(node.size[,1]) == label[i])
        
        if (length(indx[1]) != 0)
          nSize = c(nSize, node.size[indx[1],2])
        else
        {
          msg <- paste("No size data provided for the node ", nodes$id[n], ". Value 1 will be assigned to this node!", sep = "")
          warning(msg)
          nSize = c(nSize, 1)
        }
      }
          
      node.attributes <- cbind(node.attributes, size = nSize)
      rm(i, nSize, indx)
    }#is.numeric
    else{
      stop("Node size is not numeric or integer.")  
      }
  }#is.null
    
  if (is.null(node.size))
  {
    warning("No data provided for the node size. All nodes will be assigned size 1!")
    node.attributes <- cbind(node.attributes, size = rep(1, nn))
  }
    
####################################################
# Node color definition
  
  if (!is.null(node.color))
  {
    nCol <- c()
      
    for (i in 1:length(label))
    {
      indx <- which(as.character(node.color[,1]) == label[i])
      
      if (length(indx[1]) != 0)
        nCol = c(nCol, as.character(node.color[indx[1],2]))
      else
      {
        msg <- paste("No color data provided for the node ", nodes$id[n], ". Black color will be assigned to this node!", sep = "")
        warning(msg)
        nCol = c(nCol, "black")
      }
    }
    
    node.attributes <- cbind(node.attributes, color = nCol)
    rm(i, nCol, indx)
  }#is.null
  
  if (is.null(node.color))
  {
    warning("No data provided for the node color. All nodes will be colored black!")
    node.attributes <- cbind(node.attributes, color = as.character(rep("black", nn)))
  }
  
####################################################
# Node radius definition

  if (!is.null(node.radius))
  {
    if (is.numeric(node.radius[, 2]) | is.integer(node.radius[, 2]))
    {
      nSize <- c()
      
      for (i in 1:length(label))
      {
        indx <- which(as.character(node.radius[,1]) == label[i])
        
        if (length(indx[1]) != 0)
          nSize = c(nSize, node.radius[indx[1],2])
        else
        {
          msg <- paste("No raidus data provided for the node ", nodes$id[n], ". Random values will be assigned!", sep = "")
          warning(msg)
          nSize = c(nSize,  sample(nn, 1))
        }
      }
      
      node.attributes <- cbind(node.attributes, radius = nSize)
      rm(i, nSize, indx)
    }#is.numeric
    else{
      stop("Node raidus is not integer.")  
    }
  }#is.null
  
  if (is.null(node.radius))
  {
    warning("No data provided for the node radius. All nodes will be assigned random radius values")
    node.attributes <- cbind(node.attributes, radius = sample(nn, nn))
  }
  
####################################################
# Node axis definition
  
  if (!is.null(node.axis))
  {
    if (is.integer(node.axis[, 2]))
    {
      nSize <- c()
      
      for (i in 1:length(label))
      {
        indx <- which(as.character(node.axis[,1]) == label[i])
        
        if (length(indx[1]) != 0)
          nSize = c(nSize, node.axis[indx[1],2])
        else
        {
          msg <- paste("No axis data provided for the node ", nodes$id[n], ". This node will be assigned to axis 1!", sep = "")
          warning(msg)
          nSize = c(nSize,  1)
        }
      }
      
      node.attributes <- cbind(node.attributes, axis = nSize)
      rm(i, nSize, indx)
    }#is.integer
    else{
      stop("Node axis is not integer.")  
    }
  }#is.null
  
  if (is.null(node.axis))
  {
    warning("No data provided for the node axis. All nodes will be assigned to axis 1")
    node.attributes <- cbind(node.attributes, axis = rep(1, nn))
  }

  ######################################################
  
  # Create HPD object
  HPD <- list()
  
  # Define node attributes
  HPD$nodes$id <- as.integer(node.attributes$id)
  HPD$nodes$lab <- as.character(node.attributes$label)
  HPD$nodes$axis <- as.integer(node.attributes$axis)
  HPD$nodes$radius <- as.numeric(node.attributes$radius)
  HPD$nodes$size <- as.numeric(node.attributes$size)
  HPD$nodes$color <- as.character(node.attributes$color)
  
  ####################################################
  
  # Get number of edges
  ne <- nrow(edge_df)
    
  ####################################################
  # Edge weight definition
  
  if (!(is.null(edge.weight))) 
  {
    if (length(edge.weight) != nrow(edge_df))
      stop("Edge weights are not provided for all edges!") 
      
    if (is.numeric(edge.weight) | is.integer(edge.weight))
      edge_df <- cbind(edge_df, weight = edge.weight)
    else
      stop("Edge weight column is not numeric or integer.")  
  } 

  if (is.null(edge.weight))
  {
    warning("No edge weight provided Setting default edge weight to 1")
    edge_df <- cbind(edge_df, weight = rep(1, ne))
  }
  
  ####################################################
  # Edge color definition
  
  if (!(is.null(edge.color))) 
  {
    if (length(edge.color) != nrow(edge_df))
      stop("Edge colors are not provided for all edges!") 
    else 
      edge_df <- cbind(edge_df, color = as.character(edge.color))
  } 
  
  if (is.null(edge.color))
  {
    warning("No edge color provided. Setting default edge color to gray")
    edge_df <- cbind(edge_df, color = rep("gray", ne))
  }
  
  ####################################################
  # Set up edge list
  # Merge by default sorts things and changes the order of edges, so edge list has to stay paired
  edge.hlp <- merge(edge_df, node.attributes[, 1:2], by.x = 1, by.y = "label")
  edge <- merge(edge.hlp, node.attributes[1:2], by.x = 2, by.y = "label")
  
  HPD$edges$id1 <- as.integer(edge$id.x)
  HPD$edges$id2 <- as.integer(edge$id.y)
  
  HPD$edges$weight <- as.numeric(edge$weight)
  HPD$edges$color <- as.character(edge$color)
  
  HPD$nodes <- as.data.frame(HPD$nodes)
  HPD$edges <- as.data.frame(HPD$edges)
  
  # Add description
  if (is.null(desc)) {
    desc <- "No description provided"
  }
  HPD$desc <- desc
  
  # Define axis columns
  if (is.null(axis.cols)){
    axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
  }

  
  HPD$axis.cols <- axis.cols
  HPD$nodes$axis <- as.integer(HPD$nodes$axis)
  HPD$nodes$size <- as.numeric(HPD$nodes$size)
  HPD$nodes$color <- as.character(HPD$nodes$color)
  HPD$nodes$lab <- as.character(HPD$nodes$lab)
  HPD$nodes$radius <- as.numeric(HPD$nodes$radius)
  HPD$nodes$id <- as.integer(HPD$nodes$id)
  HPD$edges$id1 <- as.integer(HPD$edges$id1)
  HPD$edges$id2 <- as.integer(HPD$edges$id2)
  HPD$edges$weight <- as.numeric(HPD$edges$weight)
  HPD$edges$color <- as.character(HPD$edges$color)
  HPD$type <- type
  
  class(HPD) <- "HivePlotData"
  
  # Check HPD object
  chkHPD(HPD)
  return (HPD)
}

mod.mineHPD <- function(HPD, option = "", radData = NULL) 
{
  edges <- HPD$edges
  nodes <- HPD$nodes
  nn <- length(nodes$id)   
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- source.man.sink") {
    
    # A change that allows this function to be used for undirected graphs
    # Now all nodes will be assigned to an axis
        
    done <- FALSE # a check to make sure all nodes get an axis
    
    for (n in 1:nn) {    
      id1 <- which(n ==edges$id1)
      id2 <- which(n ==edges$id2)
      
      if ((length(id1) == 0) & (length(id2) > 0 )) {
        nodes$axis[n] <- 2
        done <- TRUE
        next
      } # these are sinks, as they only receive an edge
      
      # note that set operations below drop duplicate values
      
      #Change 1 starts here
      if (length(id1) > 0)
      {
        if (length(id2) == 0)
        {
          nodes$axis[n] <- 1
          done <- TRUE
          next
        }        
        else
        {
          #Change 1 ends here
          common <- union(id1, id2)          
          source <- setdiff(id1, common)
          if (length(source) == 1) {
            nodes$axis[n] <- 1
            done <- TRUE
            next		
          } # these are sources
          
          if (length(common) >= 1) {
            nodes$axis[n] <- 3
            done <- TRUE
            next		
          } # these are managers
        }
      } 
      
      if (!done) {
        msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
        warning(msg)
      }  # alert the user there was a problem
      
    } # end of loop inspecting nodes
    
    nodes$axis <- as.integer(nodes$axis)
    
  }  ##### end of option == "axis <- source.man.sink
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
    
  if (option == "rad <- random") {
    
    # This option assigns a random radius value to a node
   
    for (n in 1:nn)           
      nodes$radius[n] <- sample(1:nn, 1)
    
  }  ##### end of option == "rad <- random"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "rad <- userDefined") {
    
    # This option assigns a radius value to a node
    # based upon user specified values.
    
    if (is.null(radData)){
      stop("No edge data provided")
    }
    
    if (length(intersect(as.character(radData[,1]), as.character(nodes$lab))) == 0){
      stop("Provided data does not contain correct node labels")
    }          
      
    for (n in 1:nn)           
    {
      indexHlp <- which(as.character(radData[,1]) == nodes$lab[n])
      
      if (length(indexHlp) != 0)        
        nodes$radius[n] <- radData[indexHlp[1], 2]
      else
      {
        msg <- paste("No data provided for the node ", nodes$id[n], ". Value 1 will be assigned to this node!", sep = "")
        warning(msg)
        nodes$radius[n] <- 1
      }
    }
  }  ##### end of option == "rad <- userDefined"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- deg_one_two_more") 
  {
    
    # This option assigns a node to an axis
    # based upon whether its degree is 1, 2, or greater than two
    #     
    # degree 1 = axis 1, degree 2 = axis 2, degree >2 = axis3
        
    done <- FALSE # a check to make sure all nodes get an axis
    
    for (n in 1:nn) 
    {    
      id1 <- which(n ==edges$id1)
      id2 <- which(n ==edges$id2)         
      
      if ((length(id1) + length(id2)) == 1)
      {
        nodes$axis[n] <- 1
        done <- TRUE
        next
      } 
        
      if ((length(id1) + length(id2)) == 2)
      {
        nodes$axis[n] <- 2
        done <- TRUE
        next
      } 
      
      if ((length(id1) + length(id2)) > 2)
      {
        nodes$axis[n] <- 3
        done <- TRUE
        next
      }                 
      
      if (!done) {
        msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
        warning(msg)
      }  # alert the user there was a problem
      
    } # end of loop inspecting nodes
    
    nodes$axis <- as.integer(nodes$axis)
    
  }  ##### end of option == "axis <- deg_1_2_more
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- deg_five_ten_more") 
  {
    
    # This option assigns a node to an axis
    # based upon whether its degree is <=5, 6-10, or greater than 10
    #     
    # degree <=5 = axis 1, degree between 6 and 10 = axis 2, degree >10 = axis32
    
    done <- FALSE # a check to make sure all nodes get an axis
    
    for (n in 1:nn) 
    {    
      id1 <- which(n ==edges$id1)
      id2 <- which(n ==edges$id2)         
      
      if ((length(id1) + length(id2)) <= 5)
      {
        nodes$axis[n] <- 1
        done <- TRUE
        next
      } 
      
      if (((length(id1) + length(id2)) > 5) & ((length(id1) + length(id2)) <= 10))
      {
        nodes$axis[n] <- 2
        done <- TRUE
        next
      } 
      
      if ((length(id1) + length(id2)) > 10)
      {
        nodes$axis[n] <- 3
        done <- TRUE
        next
      }                 
      
      if (!done) {
        msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
        warning(msg)
      }  # alert the user there was a problem
      
    } # end of loop inspecting nodes
    
    nodes$axis <- as.integer(nodes$axis)
    
  }  ##### end of option == "axis <- deg_five_ten_more"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
    
  if (option == "remove axis edge") {
    
    # This option removes edges which start and end on the same axis
    # It re-uses code from sumHPD
    
    # Create a list of edges to be drawn
    
    n1.lab <- n1.rad <- n2.lab <- n2.rad <- n1.ax <- n2.ax <- c()
    
    for (n in 1:(length(HPD$edges$id1))) {
      i1 <- which(HPD$edges$id1[n] == HPD$nodes$id)
      i2 <- which(HPD$edges$id2[n] == HPD$nodes$id)
      n1.lab <- c(n1.lab, HPD$nodes$lab[i1])
      n2.lab <- c(n2.lab, HPD$nodes$lab[i2])
      n1.rad <- c(n1.rad, HPD$nodes$radius[i1])
      n2.rad <- c(n2.rad, HPD$nodes$radius[i2])
      n1.ax <- c(n1.ax, HPD$nodes$axis[i1])
      n2.ax <- c(n2.ax, HPD$nodes$axis[i2])
    }
    
    fd <- data.frame(
      n1.id = HPD$edges$id1,
      n1.ax,
      n1.lab,
      n1.rad,
      n2.id = HPD$edges$id2,
      n2.ax,
      n2.lab,
      n2.rad,
      e.wt = HPD$edges$weight,
      e.col = HPD$edges$color)  	
    
    prob <- which(fd$n1.ax == fd$n2.ax)
    if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same axis\n")
    if (length(prob) > 0) {
      edges <- edges[-prob,]
      cat("\n\t", length(prob), "edges that start and end on the same axis were removed\n")
    }
    
  }  ##### end of option == "remove axis edge"

  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- split") {
    
    # This option splits all axes into 2 new axes 
    # It can be used to address the "edge on the same axis" issue
    # This option may increase the number of nodes - a single node from the parent axis may appear on 2 "children" axes
      
    nodesNew <- nodes
    nodesOld <- nodes
    
    nAxes <- unique(nodes$axis)
    numAxes <- length(nAxes)

    #Renumerate axes
    for (i in numAxes:1)
      nodesOld[which(nodesOld$axis == nAxes[i]), "axis"] <- as.integer(2*nAxes[i] - 1)
    
    
    #Duplicate nodes 
    #Renumerate axes
    for (i in numAxes:1)
      nodesNew[which(nodesNew$axis == nAxes[i]), "axis"] <- as.integer(2*nAxes[i])
   
    #Re-numerate node ids
    nodesNew$id <- nodesNew$id + nn
    
    #Duplicated set of nodes with correct axis and node ids
    nodes <- rbind(nodesOld, nodesNew)
    rm(nodesOld, nodesNew)
    
    #Now create duplicated set of edges and re-numerate node ids for interactions
    edgesNew1 <- edges
    edgesNew1$id1 <- edgesNew1$id1 + nn
    edgesNew1$id2 <- edgesNew1$id2 + nn
    
    edgesNew2 <- edges
    edgesNew2$id1 <- edgesNew2$id1 + nn
    
    edgesNew3 <- edges
    edgesNew3$id2 <- edgesNew3$id2 + nn
    
    edges <- rbind(edges, edgesNew1, edgesNew2, edgesNew3)
    
    nodesAxis <- nodes[, c("id", "axis")]
    
    edgesHlp <- merge(edges, nodesAxis, by.x = "id1", by.y = "id")
    edges <- merge(edgesHlp, nodesAxis, by.x = "id2", by.y = "id")
    
    edgesOK <- edges[((edges$axis.x == 1) & (edges$axis.y == 2*numAxes)) | ((edges$axis.x == 2*numAxes) & (edges$axis.y == 1)), ]
    edgesHlp <- edgesOK

    if (numAxes > 1)
      for (i in 1:(numAxes - 1))
      {
        edgesOK <- edges[((edges$axis.x == 2*i) & (edges$axis.y == (2*i + 1))) | ((edges$axis.x == (2*i + 1)) & (edges$axis.y == 2*i)), ]
        edgesHlp <- rbind(edgesHlp, edgesOK)
      }

    for (i in 1:numAxes)
    {
       edgesOK <- edges[((edges$axis.x == (2*i - 1)) & (edges$axis.y == 2*i)) | ((edges$axis.x == 2*i) & (edges$axis.y == (2*i - 1))), ]
       edgesHlp <- rbind(edgesHlp, edgesOK)
    }

    edges <- edgesHlp[, 1:4]
    
    unique.ids <- unique(c(edges$id1, edges$id2))
    
    nodes <- nodes[nodes$id %in% unique.ids, ]  

    # Check if the new number of axes is 2 times larger than old one
    # if not, we need to adjust axis numbers
    nodesAxis.new <- sort(unique(nodes$axis))
    
    if(length(nodesAxis.new) != 2*numAxes)
      for (i in 1:length(nodesAxis.new))
        if (i != nodesAxis.new[i]){
          nodes[which(nodes$axis == nodesAxis.new[i]), "axis"] <- i
        }     
    
  }  ##### end of option == "axis <- split"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  # Final assembly and checking...
  
  HPD$edges <- edges
  HPD$nodes <- nodes
  chkHPD(HPD)
  HPD
}


## WRAPPER FUNCTIONS ##
## --------------------------------------------------------- ##
graph2nodes <- function(graph, gD, dsAll) {
	degAll <- degree(gD, v = V(gD), mode = "all");
	betAll <- betweenness(gD, v = V(gD), directed = FALSE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2);
	betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll));
	node.list <- data.frame(name = V(gD)$name, degree = degAll, betw = betAll.norm);
	F1 <- function(x) {
		data.frame(
			V4 = dsAll[which(V(gD)$name == as.character(x$V1)), 
			which(V(gD)$name == as.character(x$V2))]
		)
	}
	approxVals <- approx(c(0.1, 2), n = length(unique(node.list$bet)));
	nodes_size <- sapply(node.list$bet, function(x) approxVals$y[which(sort(unique(node.list$bet)) == x)])
	node.list <- cbind(node.list, size = nodes_size);
	node.list$name <- as.character(node.list$name);
	rownames(node.list) <- node.list$name;
	return(node.list);
}

graph2edges <- function(graph, gD, dsAll) {
	F1 <- function(x) {
		data.frame(
			V4 = dsAll[which(V(gD)$name == as.character(x$V1)), 
			which(V(gD)$name == as.character(x$V2))]
		)
	}
	edge.list <- ddply(graph, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)));
	return(edge.list);
}

colorNodes  <- function(node.list, cols=c("#F5DEB3", "#FF0000")) {
	F2 <- colorRampPalette(
		cols, 
		bias = length(unique(node.list$degree)), 
		space = "rgb", 
		interpolate = "linear"
	);
	colCodes <- F2(length(unique(node.list$degree)))
	nodes_col <- sapply(node.list$degree, function(x) colCodes[which(sort(unique(node.list$degree)) == x)])
	node.list <- cbind(node.list, color = nodes_col)
	return(node.list);
}

colorEdges <- function(edge.list, cols=c("#FFFF00", "#006400")) {
	F2 <- colorRampPalette(
		cols, 
		bias = length(unique(edge.list$V4)), 
		space = "rgb", 
		interpolate = "linear"
	);
	colCodes <- F2(length(unique(edge.list$V4)))
	edges_col <- sapply(edge.list$V4, function(x) colCodes[which(sort(unique(edge.list$V4)) == x)])
	edge.list <- cbind(edge.list, color = edges_col);
	edge.list$V3 <- 1; # Edge weight
	return(edge.list);
}

clusterNodes <- function(gD, dsAll, node.list, csize) {
	d <- dist(dsAll);
	hc <- hclust(d);
	nodeAxis <- as.integer(cutreeDynamic(hc, distM=dsAll, minClusterSize=csize));
	nodeAxis <- as.integer(nodeAxis);
	node.list <- cbind(node.list, axis = nodeAxis);
	print("# Clustering:\n");
	print(summary(as.factor(nodeAxis)));
	return(node.list);
}

loadPackages <- function() {
	library(igraph);
	library(HiveR);
	library("RColorBrewer");
	library(plyr);
	library(dplyr);
	library("grDevices");
	library(dynamicTreeCut);
}

diceDistance <- function(gD) {
	dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")
	return(dsAll);
}

doHivePlot <- function(filename, node.list, edge.list, mode="all") {
	edges2Plot <- NA;
	edgeWeight <- 1
	if (mode == "intra") {
		filename <- paste0(filename, "_intra.png");
		edges2Plot <- edge.list[edge.list$cluster_edge == "intra",]
	} else if (mode == "inter") {
		filename <- paste0(filename, "_inter.png");
		edges2Plot <- edge.list[edge.list$cluster_edge == "inter",]
	} else if (mode == "drivers") {
		filename <- paste0(filename, "_drivers.png");
		edgeWeight <- 0; # Hide all edges
		edges2Plot <- edge.list;
	} else {
		filename <- paste0(filename, "_all.png");
		edges2Plot <- edge.list;
	}
	print(paste0("# Plotting ", length(rownames(edges2Plot)), " edges."))
	png(filename, width=1000, height=1000);
	hive1 <- mod.edge2HPD(
		edge_df = edges2Plot[, 1:2], 
		edge.weight = edges2Plot[, 3], 
		edge.color = edges2Plot[, 5], 
		node.color = node.list[,c("name", "color")], 
		node.size = node.list[,c("name", "size")], 
		node.radius = node.list[,c("name", "degree")], 
		node.axis = node.list[,c("name", "axis")]
	);
	hive1$edges$weight <- edgeWeight;
	theHive <- NA;
	theHive <- mineHPD(hive1, option = "remove virtual edge");
	if (length(rownames(theHive$edges)) == 0) {
		theHive <- hive1;
	}
	if (min(theHive$nodes$axis) == 2) {
		theHive$nodes$axis <- as.integer(theHive$nodes$axis - 1);
	} 
	plotHive(theHive, method = "abs", bkgnd = "white", axLab.pos = 1);
	dev.off();
	print(paste0("# Saved in ", filename));
}

graph2Hive <- function(graph, filename="hivePlot", clusterSize=250, drivers) {
	loadPackages();

	# Prepare data
	print("# Computing graph distance metrics...");
	gD <- simplify(graph.data.frame(graph, directed=T))
	dsAll <- diceDistance(gD);

	print("# Preparing nodes and edges...");
	node.list <- graph2nodes(graph, gD, dsAll);
	edge.list <- graph2edges(graph, gD, dsAll);

	print("# Coloring nodes and edges...");
	node.list <- colorNodes(node.list);
	edge.list <- colorEdges(edge.list);

	# Cluster the nodes
	print("# Clustering nodes...");
	node.list <- clusterNodes(gD, dsAll, node.list, clusterSize);

	# Intra or Inter edges?
	edge.list$V1 <- as.character(edge.list$V1);
	edge.list$V2 <- as.character(edge.list$V2);
	edge.list$source_axis <- node.list[edge.list$V1, "axis"];
	edge.list$target_axis <- node.list[edge.list$V2, "axis"];
	edge.list$cluster_edge <- ifelse(edge.list$source_axis == edge.list$target_axis, "intra", "inter");

	# Print drivers
	dcolors <- c("#9070c7", "#4dad98", "#c5793e");
	print(node.list %>% filter(name %in% drivers$identifier) %>% group_by(axis) %>% top_n(5, degree) %>% arrange(axis));

	# Print Summaries
	print("# Node Summary:");
	print(summary(node.list));
	print("# Edge Summary:");
	print(summary(edge.list));

	# Plot the thing
	doHivePlot(filename, node.list, edge.list, mode="all");
	doHivePlot(filename, node.list, edge.list, mode="intra");
	doHivePlot(filename, node.list, edge.list, mode="inter");
	node.list$color <- ifelse(
		node.list$name %in% drivers$identifier, 
		dcolors[drivers[node.list$name,]$gene_disease], 
		"transparent"
	);
	doHivePlot(filename, node.list, edge.list, mode="drivers");
}