#'Computes the OPF accuracy of a given set
#'
#'@param dataSet Subgraph object, normaly is the training object
#'@param classification The classification array produced by the classification of the test subgraph
#'
#'@return
#'Returns the calculated test subgraph classification accuracy
#'
#'@export
opf_accuracy <- function(dataSet, classification){
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  opf_WriteClassification(classification,paste(file,".out", sep = ""))
	argv <- c("", file)
	aux <- .C("opf_accuracy",length(argv),as.character(argv))
  return(opf_ReadClassification(paste(file,".acc", sep = "")))
}

#'Computes the OPF accuracy for each class of a given set
#'
#'@param dataSet Subgraph object, normaly is the training object
#'@param classification The classification array produced by the classification of the test subgraph
#'
#'@return
#'Returns the calculated test subgraph classification accuracy for each class
#'
#'@export
opf_accuracy4label <- function(dataSet, classification){
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  opf_WriteClassification(classification,paste(file,".out", sep = ""))
  argv <- c("", file)
  aux <- .C("opf_accuracy4label",length(argv),as.character(argv))
  return(opf_ReadClassification(paste(file,".acc", sep = "")))
}

#'Executes the test phase of the OPF classifier
#'
#'@param dataSet Testing subgraph object
#'@param classifier The classifier model object
#'@param precomputedDistance The precomputed distance matrix (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns the given graph classification array
#'
#'@export
opf_classify <- function(dataSet, classifier, precomputedDistance = NA){ #T #MCrash
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  opf_WriteModelFile(classifier,paste(file,".classifier.opf", sep = ""))
  argv <- c("", file)
	if(!is.na(precomputedDistance)){
	  opf_WriteDistances(precomputedDistance,paste(file,".distances", sep = ""))
	  argv <- append(argv,paste(file,".distances", sep = ""))
	}
	aux <- .C("opf_classify",length(argv),as.character(argv))
	return(opf_ReadClassification(paste(file,".classifier.opf.out", sep = "")))
}

#'Computes clusters by OPF
#'
#'@param dataSet Unlabeled subgraph object
#'@param kmax kmax(maximum degree for the knn graph)
#'@param calculateOp how its calculate. 0 for (height), 1 for (area) and 2 for (volume)
#'@param value value of parameter "op" in (0-1)
#'@param precomputedDistance The precomputed distance subgraph object (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns a list which contains the classifier object and the classificantion array object
#'
#'@export
opf_cluster <- function(dataSet, kmax, calculateOp, value,precomputedDistance = NA){ #T #MCrash
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  argv <- c("", file,kmax,calculateOp,value)
	if(!is.na(precomputedDistance)){
	  opf_WriteDistances(precomputedDistance,paste(file,".distances", sep = ""))
	  argv <- append(argv,paste(file,".distances", sep = ""))
	}
	aux <- .C("opf_cluster",length(argv),as.character(argv))
	setClass(Class="Return",
	         representation(
	           classifier="Subgraph",
	           classification="vector"
	         )
	)
	return(new("Return",classifier=opf_ReadModelFile(paste(file,".classifier.opf", sep = "")),classification=opf_ReadClassification(paste(file,".classifier.opf.out", sep = ""))))
}

#'Generates the precomputed distance file for the OPF classifier
#'
#'@param dataSet Subgraph object, normaly is the full data
#'@param distanceOp distance calculation option
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@details
#'Options for distance calculation:\cr
#'1 - Euclidean\cr
#'2 - Chi-Square\cr
#'3 - Manhattan (L1)\cr
#'4 - Canberra\cr
#'5 - Squared Chord\cr
#'6 - Squared Chi-Squared\cr
#'7 - BrayCurtis
#'
#'@return
#'Returns a distance matrix
#'
#'@export
opf_distance <- function(dataSet, distanceOp, normalize = 0){
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  argv <- c("", file,distanceOp,normalize)
	aux <- .C("opf_distance",length(argv),as.character(argv))
	return(opf_ReadDistances(paste(file,".distances.dat", sep = "")))
}

#'Generates k folds (objects) for the OPF classifier
#'
#'@param dataSet The graph object
#'@param k number of folds
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@return
#'Returns a array of subgraph objects
#'
#'@export
opf_fold <- function(dataSet, k, normalize = 0){
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
	argv <- c("", file,k,normalize)
	aux <- .C("opf_fold",length(argv),as.character(argv))
	result <- c()
	for(i in 1:k){
    file <- paste(file,"1", sep = "")
    result <- append(result,opf_ReadSubGraph(file))
	}
	return(result)
}

#'Gives information about the OPF file
#'
#'@param
#'dataSet The OPF object
#'
#'@export
opf_info <- function(dataSet){
  file <- tempfile()
  fp <- file(file, "wb")
  writeBin(as.integer(dataSet$nnodes), size = 4, fp)
  writeBin(as.integer(dataSet$nlabels), size = 4, fp)
  writeBin(as.integer(dataSet$nfeats), size = 4, fp)
  close(fp)
	argv <- c("", file)
	aux <- .C("opf_info",length(argv),as.character(argv))
}

#'Executes the learning phase for the OPF classifier
#'
#'@param trainFile The training set object
#'@param evaluatFile The evaluation set object
#'@param precomputedDistance The precomputed distance subgraph object (leave it in blank if you are not using this resource)
#'
#'@details
#'Execute the training phase and generates an output file named classifier.opf
#'
#'@return
#'Returns the classifier model object
#'
#'@export
opf_learn <- function(trainFile, evaluatFile, precomputedDistance = NA){ #T
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  opf_WriteSubGraph(evaluatFile,paste(file,".evaluet", sep = ""))
  argv <- c("", file, paste(file,".evaluet", sep = ""))
  if(!is.na(precomputedDistance)){
    opf_WriteDistances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
	aux <- .C("opf_learn",length(argv),as.character(argv))
	return(opf_ReadModelFile(paste(file,".classifier.opf", sep = "")))
}

#'Merge subgraphs
#'
#'@param dataSets vector of dataSets objects
#'
#'@return
#'Returns the merged subgraph object
#'
#'@export
opf_merge <- function(dataSets){
  file <- tempfile()
  argv <- c("")
  for(i in 1:length(dataSets)){
    opf_WriteSubGraph(dataSets[[i]],paste(file,as.character(i), sep = ""))
    argv <- c(argv, paste(file,as.character(i), sep = ""))
  }
	aux <- .C("opf_merge",length(argv),as.character(argv))
  return(opf_ReadSubGraph(paste(file,"1.merged.dat", sep = "")))
}

#'Normalizes data for the OPF classifier
#'
#'@param dataSet The subgraph object
#'
#'@return
#'Returns the normalized subgraph
#'
#'@export
opf_normalize <- function(dataSet){ #T
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  argv <- c("", file, paste(file,".out", sep = ""))
	aux <- .C("opf_normalize",length(argv),as.character(argv))
	return(opf_ReadSubGraph(paste(file,".out", sep = "")))
}

#'Executes the pruning algorithm of the OPF classifier
#'
#'@param dataTraining Training subgraph object
#'@param dataEvaluating Evaluating subgraph object
#'@param percentageAccuracy percentage of accuracy [0,1]
#'@param precomputedDistance The precomputed distance matrix (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns the classifier model object
#'
#'@export
opf_pruning <- function(dataTraining, dataEvaluating, percentageAccuracy, precomputedDistance){ #T
  file <- tempfile()
  opf_WriteSubGraph(dataTraining,file)
  opf_WriteSubGraph(dataEvaluating,paste(file,".evaluate", sep = ""))
	argv <- c("", file,  paste(file,".evaluate", sep = ""),  percentageAccuracy)
	if(!is.na(precomputedDistance)){
	  opf_WriteDistances(precomputedDistance,paste(file,".distances", sep = ""))
	  argv <- append(argv,paste(file,".distances", sep = ""))
	}
	aux <- .C("opf_pruning",length(argv),as.character(argv))
	return(opf_ReadModelFile(paste(file,".classifier.opf", sep = "")))
}

#'Executes the semi supervised training phase of the OPF classifier
#'
#'@param labeledTrainFile Labeled training subgraph object
#'@param unLabeledTrainFile Unlabeled training subgraph object
#'@param evaluatFile Evaluation subgraph object
#'@param precomputedDistance The precomputed distance matrix (leave it in blank if you are not using this resource)
#'
#'@details
#'Returns the classifier model object
#'
#'@export
opf_semi <- function(labeledTrainFile,  unLabeledTrainFile,  evaluatFile = NA,  precomputedDistance = NA){ #T?????? #Arrumar C
  argv <- c("", labeledTrainFile,  unLabeledTrainFile,  evaluatFile)
  if(!is.na(precomputedDistance)){
    opf_WriteDistances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
	aux <- .C("opf_semi",length(argv),as.character(argv))
}

#'Generates training, evaluation and test sets for the OPF classifier
#'
#'@param dataSet The data subgraph object
#'@param training_p percentage for the training set size [0,1]
#'@param evaluating_p percentage for the evaluation set size [0,1] (leave 0 in the case of no learning)
#'@param testing_p percentage for the test set size [0,1]
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@return
#'Returns the Training, Evaluating and the Testing objects
#'
#'@export
opf_split <- function(dataSet, training_p, evaluating_p, testing_p, normalize){ #T
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  argv <- c("", file, training_p, evaluating_p, testing_p, normalize=0)
	aux <- .C("opf_split",length(argv),as.character(argv))
	setClass(Class="Return",
	         representation(
	           Training="Subgraph",
	           Evaluating="Subgraph",
	           Testing="Subgraph"
	         )
	)
	return <- new("Return")
	if(training_p > 0.0) return$Training = opf_ReadSubGraph(paste(file,".training.dat", sep = ""))
	if(evaluating_p > 0.0) return$Training = opf_ReadSubGraph(paste(file,".evaluating.dat", sep = ""))
	if(testing_p > 0.0) return$Training = opf_ReadSubGraph(paste(file,".testing.dat", sep = ""))
	return(return)
}

#-------------------------------------------------------------------------------------------------------- parei aqui

#'Executes the training phase of the OPF classifier
#'
#'@param dataSet The training subgraph object
#'@param precomputedDistance The precomputed distance matrix (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns the classifier model object
#'
#'@export
opf_train <- function(dataSet, precomputedDistance = NA){
  file <- tempfile()
  opf_WriteSubGraph(dataSet,file)
  argv <- c("", file)
  if(!is.na(precomputedDistance)){
    opf_WriteDistances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opf_train",length(argv),as.character(argv))
  return(opf_ReadModelFile(paste(file,".classifier.opf", sep = ""))) #TEM COISA PARA ARRUMAR AQUI usar SPRINTF?
}

#'Executes the test phase of the OPF classifier with knn adjacency
#'
#'@param dataSet path to test set in the OPF file format
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource)
#'
#'@details
#'Creates a file .out, which is used in opf_accuracy, for example.
#'
#'@export
opf_knn_classify <- function(dataSet, precomputedDistance = NA){
	argv <- c("", dataSet)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	aux <- .C("opfknn_classify",length(argv),as.character(argv))
}

#'Executes the training phase of the OPF classifier with knn adjacency
#'
#'@param trainFile path to training set in the OPF file format
#'@param evaluatFile path to evaluating set in the OPF file format (used to learn k)
#'@param kmax kmax(maximum degree for the knn graph)
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource)
#'
#'@details
#'Creates a file .out, which is used in opf_knn_classify, for example. Also creates a .time that contais the processing time.
#'
#'@export
opf_knn_train <- function(trainFile, evaluatFile, kmax, precomputedDistance = NA){
	argv <- c("", trainFile, evaluatFile, kmax)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	aux <- .C("opfknn_train",length(argv),as.character(argv))
}

#tools -------------------------------------------------------------------------------------

#'Calculate cluster centroids using K-means
#'
#'@param inputFile path to input file in the OPF binary format
#'@param numberClusters number of clusters (k)
#'@param outputFile path to output file containing cluster centroids
#'
#'@details
#'Creates a file with the cluster centroids.
#'
#'@export
kmeans <- function(inputFile, numberClusters, outputFile){
  argv <- c("", inputFile, numberClusters, outputFile)
  aux <- .C("kmeans",length(argv),as.character(argv))
}

#'Check the OPF file
#'
#'@param inputFile path to input ASCII file in the LibOPF format
#'
#'@details
#'usage opf_check <input ASCII file in the LibOPF format>:
#'Note that the input file for opf_check must be a text file.
#'Use opf2txt to convert your OPF binary file into a text file.
#'
#'@export
opf_check <- function(inputFile){
  argv <- c("", inputFile)
  aux <- .C("opf_check",length(argv),as.character(argv))
}

#'Converts OPF to SVM
#'
#'@param inputFile path to OPF input file
#'@param outputFile path to SVM output file
#'
#'@export
opf2svm <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  aux <- .C("opf2svm",length(argv),as.character(argv))
}

#'Converts OPF to text
#'
#'@param inputFile path to OPF input file
#'@param outputFile path to text output file
#'
#'@export
opf2txt <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  aux <- .C("opf2txt",length(argv),as.character(argv))
}

#'Calculates mean of accuracy
#'
#'@param inputFile path to accuracy file
#'@param numberAccuracy how much accuracy will take
#'@param message text that will be show
#'
#'@export
statistics <- function(inputFile, numberAccuracy, message){
  argv <- c("", inputFile, numberAccuracy, message)
  aux <- .C("statistics",length(argv),as.character(argv))
}

#'Converts SVM to OPF
#'
#'@param inputFile path to SVM input file
#'@param outputFile path to OPF output file
#'
#'@export
svm2opf <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  aux <- .C("svm2opf",length(argv),as.character(argv))
}

#'Converts text to OPF
#'
#'@param inputFile path to text input file
#'@param outputFile path to OPF output file
#'
#'@export
txt2opf <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  aux <- .C("txt2opf",length(argv),as.character(argv))
}

SNode <- setRefClass (
  "SNode",
  fields = list(
    pathval = "numeric",
    dens = "numeric",
    radius = "numeric",
    label = "numeric",
    root = "numeric",
    pred = "numeric",
    truelabel = "numeric",
    position = "numeric",
    feat = "vector",
    status = "numeric",
    relevant = "numeric",
    nplatadj = "numeric",
    adj = "vector"
  ),
  method = list(
    initialize =
      function()
      {
        pred <<- 1
        pathval <<- 0.0
        dens <<- 0.0
        radius <<- 0.0
        label <<- 0
        root <<- 0
        truelabel <<- 1
        position <<- 0
        nplatadj <<- 0
        status <<- -1
      })
)

Subgraph <- setRefClass (
  "Subgraph",
  fields = list(
    node = "vector",
    nnodes = "numeric",
    nfeats = "numeric",
    bestk = "numeric",
    nlabels = "numeric",
    df = "numeric",
    mindens = "numeric",
    maxdens = "numeric",
    K = "numeric",
    ordered_list_of_nodes = "vector"
  ),
  method = list(
    initialize =
      function()
      {
        nnodes <<- 0
        nfeats <<- 0
        bestk <<- 0
        nlabels <<- 0
        df <<- 0.0
        mindens <<- 0.0
        maxdens <<- 0.0
        K <<- 0.0
      })
)

#'Creates a empty opf graph structure
#'
#'@param
#'nnodes number of nodes
#'
#'@return
#'Returns the empty opf graph
#'
#'@export
opf_CreateSubgraph <- function(nnodes){ #CreateSubgraph
  sg <- Subgraph()
  sg$nnodes <- nnodes
  sg$ordered_list_of_nodes <- vector(mode = "integer", length = nnodes)
  for(i in 1:nnodes){
    sg$node <- c(sg$node, SNode())
    sg$node[[i]]$feat <- NA
    sg$node[[i]]$relevant <- 0
  }
  return(sg)
}

#'Reads a file which has the opf graph
#'
#'@param
#'file file where the graph is
#'
#'@return
#'Returns the graph object
#'
#'@export
opf_ReadSubGraph <- function(file){ #ReadSubgraph
  binFile <- file(file, "rb")
  nnodes <- readBin(binFile, "int", endian = "little")
  g <- opf_CreateSubgraph(nnodes);
  g$nlabels <- readBin(binFile, "int", endian = "little")
  g$nfeats <- readBin(binFile, "int", endian = "little")
  for (i in 1:g$nnodes){
    g$node[[i]]$position <- readBin(binFile, "int", endian = "little")# + 1
    g$node[[i]]$truelabel <- readBin(binFile, "int", endian = "little")
    for (j in 1:g$nfeats){
      g$node[[i]]$feat[[j]] <- readBin(binFile, "double", size=4, endian = "little")
    }
  }
  close(binFile)
  return(g)
}

#'Writes into a opf file a graph
#'
#'@param
#'g the graph object
#'file file where you want to save the graph
#'
#'@export
opf_WriteSubGraph <- function(g, file){ #WriteSubgraph
  if(!is.null(g)){
    fileBin <- file(file, "wb")
    writeBin(as.integer(g$nnodes),  size = 4, fileBin)
    writeBin(as.integer(g$nlabels),  size = 4,fileBin)
    writeBin(as.integer(g$nfeats),  size = 4,fileBin)
    for (i in 1:g$nnodes){
      writeBin(as.integer(g$node[[i]]$position),  size = 4,fileBin)# - 1 in position
      writeBin(as.integer(g$node[[i]]$truelabel),  size = 4,fileBin)
      for (j in 1:g$nfeats){
        writeBin(as.double(g$node[[i]]$feat[[j]]),  size = 4,fileBin)
      }
    }
    close(fileBin)
  }
}

#'Reads a file which has the classificator
#'
#'@param
#'file file which has the classificator
#'
#'@return
#'Returns the classificator object
#'
#'@export
opf_ReadModelFile <- function(file){ #opf_ReadModelFile
  #if((fp = fopen(file, "rb")) == NULL){ #arrumar
  #  sprintf(msg, "%s%s", "Unable to open file ", file)
  #  Error(msg,"ReadSubGraph")
  #}
  binFile <- file(file, "rb")
  nnodes <- readBin(binFile, "int", size=4, endian = "little")
  g <- opf_CreateSubgraph(nnodes)
  g$nlabels <- readBin(binFile, "int", size=4, endian = "little")
  g$nfeats <- readBin(binFile, "int", size=4, endian = "little")
  g$df <- readBin(binFile, "double", size=4, endian = "little")
  g$bestk <- readBin(binFile, "int", size=4, endian = "little")
  g$K <- readBin(binFile, "double", size=4, endian = "little")
  g$mindens <- readBin(binFile, "double", size=4, endian = "little")
  g$maxdens <- readBin(binFile, "double", size=4, endian = "little")
  for (i in c(1:g$nnodes)){
    g$node[[i]]$feat <- vector(mode = "numeric", length = g$nfeats)
    g$node[[i]]$position <- readBin(binFile, "int", size=4, endian = "little")# + 1
    g$node[[i]]$truelabel <- readBin(binFile, "int", size=4, endian = "little")# + 1
    g$node[[i]]$pred <- readBin(binFile, "int", size=4, endian = "little")#  + 1
    g$node[[i]]$label <- readBin(binFile, "int", size=4, endian = "little")# + 1
    g$node[[i]]$pathval <- readBin(binFile, "double", size=4, endian = "little")
    g$node[[i]]$radius <- readBin(binFile, "double", size=4, endian = "little")
    g$node[[i]]$dens <- readBin(binFile, "double", size=4, endian = "little")
    for (j in c(1:g$nfeats)){
      g$node[[i]]$feat[[j]] <- readBin(binFile, "double", size=4, endian = "little")
    }
  }
  for (i in c(1:g$nnodes)){
    g$ordered_list_of_nodes[[i]] <- readBin(binFile, "int", size=4, endian = "little")# + 1
  }
  close(binFile)
  return(g)
}

#'Writes into a opf file the trained opf classifier
#'
#'@param
#'g the classifier object
#'file file where you want to save the classificator
#'
#'@export
opf_WriteModelFile <- function(g, file){
  fp <- file(file, "wb")
  writeBin(as.integer(g$nnodes), size = 4, fp)
  writeBin(as.integer(g$nlabels), size = 4, fp)
  writeBin(as.integer(g$nfeats), size = 4, fp)
  writeBin(as.double(g$df), size = 4, fp)
  writeBin(as.integer(g$bestk), size = 4, fp)
  writeBin(as.double(g$K), size = 4, fp)
  writeBin(as.double(g$mindens), size = 4,fp)
  writeBin(as.double(g$maxdens), size = 4,fp)
  for(i in c(1:g$nnodes)){
    writeBin(as.integer(g$node[[i]]$position), size = 4, fp) #-1 in position
    writeBin(as.integer(g$node[[i]]$truelabel), size = 4, fp)# - 1
    #if(g$node[[i]]$pred != -1)
    #  g$node[[i]]$pred <- g$node[[i]]$pred# - 1
    writeBin(as.integer(g$node[[i]]$pred), size = 4, fp)# - 1
    writeBin(as.integer(g$node[[i]]$label), size = 4, fp)# - 1
    writeBin(as.double(g$node[[i]]$pathval), size = 4, fp)
    writeBin(as.double(g$node[[i]]$radius), size = 4, fp)
    writeBin(as.double(g$node[[i]]$dens), size = 4, fp)
    for(j in c(1:g$nfeats)){
      writeBin(as.double(g$node[[i]]$feat[[j]]), size = 4, fp)
    }
  }
  for(i in c(1:g$nnodes)){
    writeBin(as.integer(g$ordered_list_of_nodes[[i]]), size = 4, fp)# - 1 in nodes
  }
  close(fp)
}

#'Reads a file which has the nodes classification
#'
#'@param
#'file file which has the nodes classification
#'
#'@return
#'Returns the nodes classification array
#'
#'@export
opf_ReadClassification <- function(file){ #opf_WriteOutputFile
  fp <- file(file, "r")
  return <- as.double(readLines(fp))
  close(fp)
  return(return)
}

#'Writes into a file the node classification vector produced by the opf classificator
#'
#'@param
#'classes classification vector produced by the classificator
#'file file where you want to save the classification vector
#'
#'@export
opf_WriteClassification <- function(classes, file){ #opf_WriteOutputFile
  fp <- file(file, "w")
  writeLines(as.character(classes),fp)
  close(fp)
}

#'Reads a file which has the precalculated distances
#'
#'@param
#'file file which has the nodes classification
#'
#'@return
#'Returns the precalculated distances matrix
#'
#'@export
opf_ReadDistances <- function(fileName)
{
  fp <- file(fileName,"rb");
  nsamples <- readBin(fp, "int", size=4, endian = "little")
  #if (fread(&nsamples, sizeof(int), 1, fp) != 1)
  #  Error("Could not read number of samples","opf_ReadDistances"); #arrumar
  M <- matrix(data = 0, nrow = nsamples, ncol = nsamples)
  for (i in 1:nsamples)
  {
    for (j in 1:nsamples)
    {
      M[[i,j]] <- readBin(fp, "double", size=4, endian = "little")
    }
  }
  close(fp)
  return(M)
}

#'Writes into a file the precalculated distances of the gives set produced by the opf distances
#'
#'@param
#'distances the matrix produced by the opf distances
#'file file where you want to save the distances
#'
#'@export
opf_WriteDistances <- function(distances, fileName)
{
  fp <- file(fileName,"wb");
  writeBin(as.integer(nrow(distances)), size = 4, fp)
  for (i in 1:as.integer(nrow(distances)))
  {
    for (j in 1:as.integer(ncol(distances)))
    {
      writeBin(as.double(distances[[i,j]]), size = 4, fp)
    }
  }
  close(fp)
}
