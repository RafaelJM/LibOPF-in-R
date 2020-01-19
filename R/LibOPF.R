#'Computes the OPF accuracy
#'
#'@param dataSet Data object used in the opf_classify function (subGraph object), normaly is the testing object
#'@param classification The output list produced by opf_classify function
#'
#'@return
#'Returns the classification accuracy
#'
#'@useDynLib libOPF
#'@export
opf_accuracy <- function(dataSet, classification){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  opf_write_classification(classification,paste(file,".out", sep = ""))
  argv <- c("", file)
  aux <- .C("opf_accuracy",length(argv),as.character(argv))
  return(opf_read_classification(paste(file,".acc", sep = "")))
}

#'Computes the OPF accuracy for each class of a given set
#'
#'@param dataSet Data object used in in the opf_classify function (subGraph object), normaly is the testing object
#'@param classification The output list produced by opf_classify function
#'
#'@return
#'Returns the classification accuracy for each class
#'
#'@useDynLib libOPF
#'@export
opf_accuracy4label <- function(dataSet, classification){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  opf_write_classification(classification,paste(file,".out", sep = ""))
  argv <- c("", file)
  aux <- .C("opf_accuracy4label",length(argv),as.character(argv))
  return(opf_read_classification(paste(file,".acc", sep = "")))
}

#'Executes the test phase of the OPF classifier
#'
#'@param dataSet The testing data object produced by the opf_split function (subGraph object)
#'@param classifier The classifier object produced by one of the classification functions (model object)
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns the given subGraph classification list (predicted labels)
#'
#'@useDynLib libOPF
#'@export
opf_classify <- function(dataSet, classifier, precomputedDistance = NA){ #MCrash
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  opf_write_modelfile(classifier,paste(file,".classifier.opf", sep = ""))
  argv <- c("", file)
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opf_classify",length(argv),as.character(argv))
  return(opf_read_classification(paste(file,".classifier.opf.out", sep = "")))
}

#'Computes clusters by unsupervised OPF
#'
#'@param dataSet The training object produced by the opf_split function (subGraph object)
#'@param kmax The kmax (maximum degree for the knn graph)
#'@param calculateOp Clusters by: 0 for height, 1 for area and 2 for volume
#'@param value Value of parameter "calculateOp" in [0-1]
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns a list which contains the classifier object and the classification list object (i.e., clusters' id)
#'
#'@useDynLib libOPF
#'@export
opf_cluster <- function(dataSet, kmax, calculateOp, value,precomputedDistance = NA){ #MCrash
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  argv <- c("", file,kmax,calculateOp,value)
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opf_cluster",length(argv),as.character(argv))
  return(c("classifier"=opf_read_modelfile(paste(file,".classifier.opf", sep = "")),"classification"=opf_read_classification(paste(file,".classifier.opf.out", sep = ""))))
}

#'Generates the precomputed distance file for the OPF classifier
#'
#'@param dataSet The subGraph object, normaly is the whole data
#'@param distanceOp Distance calculation option
#'@param normalize Distance normalization? 1- yes 0 - no
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
#'Returns the distance matrix
#'
#'@useDynLib libOPF
#'@export
opf_distance <- function(dataSet, distanceOp, normalize = 0){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  argv <- c("", file,distanceOp,normalize)
  aux <- .C("opf_distance",length(argv),as.character(argv))
  return(opf_read_distances(paste(file,".distances.dat", sep = "")))
}

#'Generates k folds (objects) for the OPF classifier
#'
#'@param dataSet The subGraph object
#'@param k Number of folds
#'@param normalize Distance normalization? 1- yes 0 - no
#'
#'@return
#'Returns a list of subGraph objects
#'
#'@useDynLib libOPF
#'@export
opf_fold <- function(dataSet, k, normalize = 0){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  argv <- c("", file,k,normalize)
  aux <- .C("opf_fold",length(argv),as.character(argv))
  result <- c()
  for(i in 1:k){
    file <- paste(file,"1", sep = "")
    result <- append(result,opf_read_subGraph(file))
  }
  return(result)
}

#'Gives information about the OPF file
#'
#'@param dataSet The OPF file
#'
#'@useDynLib libOPF
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

#'Executes the learning phase of the OPF classifier
#'
#'@param trainFile The training object produced by the opf_split (subGraph object)
#'@param evaluatFile The evaluation produced object by the opf_split (subGraph object)
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@details
#'Executes the training phase
#'
#'@return
#'Returns the classifier model object
#'
#'@useDynLib libOPF
#'@export
opf_learn <- function(trainFile, evaluatFile, precomputedDistance = NA){
  file <- tempfile()
  opf_write_subGraph(trainFile,file)
  opf_write_subGraph(evaluatFile,paste(file,".evaluet", sep = ""))
  argv <- c("", file, paste(file,".evaluet", sep = ""))
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opf_learn",length(argv),as.character(argv))
  return(opf_read_modelfile(paste(file,".classifier.opf", sep = "")))
}

#'Merge subGraphs
#'
#'@param dataSets array of subGraph objects
#'
#'@return
#'Returns the merged subGraph object
#'
#'@useDynLib libOPF
#'@export
opf_merge <- function(dataSets){
  file <- tempfile()
  argv <- c("")
  for(i in 1:length(dataSets)){
    opf_write_subGraph(dataSets[[i]],paste(file,as.character(i), sep = ""))
    argv <- c(argv, paste(file,as.character(i), sep = ""))
  }
  aux <- .C("opf_merge",length(argv),as.character(argv))
  return(opf_read_subGraph(paste(file,"1.merged.dat", sep = "")))
}

#'Normalizes data for the OPF classifier
#'
#'@param dataSet The subGraph object
#'
#'@return
#'Returns the normalized subGraph
#'
#'@useDynLib libOPF
#'@export
opf_normalize <- function(dataSet){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  argv <- c("", file, paste(file,".out", sep = ""))
  aux <- .C("opf_normalize",length(argv),as.character(argv))
  return(opf_read_subGraph(paste(file,".out", sep = "")))
}

#'Executes the pruning algorithm
#'
#'@param dataTraining The training object produced by the opf_split (subGraph object)
#'@param dataEvaluating The evaluating object produced by the opf_split (subGraph object)
#'@param percentageAccuracy Percentage of accuracy [0,1]
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns the classifier model object
#'
#'@useDynLib libOPF
#'@export
opf_pruning <- function(dataTraining, dataEvaluating, percentageAccuracy, precomputedDistance){
  file <- tempfile()
  opf_write_subGraph(dataTraining,file)
  opf_write_subGraph(dataEvaluating,paste(file,".evaluate", sep = ""))
  argv <- c("", file,  paste(file,".evaluate", sep = ""),  percentageAccuracy)
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opf_pruning",length(argv),as.character(argv))
  return(opf_read_modelfile(paste(file,".classifier.opf", sep = "")))
}

#'Executes the semi supervised training phase
#'
#'@param labeledTrainFile The labeled training object (subGraph object)
#'@param unLabeledTrainFile The unlabeled training object (subGraph object)
#'@param evaluatFile The evaluation object produced by the opf_split (subGraph object)
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@details
#'Returns the learned model object
#'
#'@useDynLib libOPF
#'@export
opf_semi <- function(labeledTrainFile,  unLabeledTrainFile,  evaluatFile = NA,  precomputedDistance = NA){#?????? Arrumar C
    argv <- c("", labeledTrainFile,  unLabeledTrainFile,  evaluatFile)
    if(!is.na(precomputedDistance)){
      opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
      argv <- append(argv,paste(file,".distances", sep = ""))
    }
    aux <- .C("opf_semi",length(argv),as.character(argv))
}

#'Generates training, evaluation and test sets for the OPF classifier
#'
#'@param dataSet The data (subGraph object)
#'@param training_p percentage for the training set size [0,1]
#'@param evaluating_p percentage for the evaluation set size [0,1] (leave 0 in the case of no learning)
#'@param testing_p percentage for the test set size [0,1]
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@return
#'Returns the Training, Evaluating and the Testing objects
#'
#'@useDynLib libOPF
#'@export
opf_split <- function(dataSet, training_p, evaluating_p, testing_p, normalize){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  argv <- c("", file, training_p, evaluating_p, testing_p, normalize=0)
  aux <- .C("opf_split",length(argv),as.character(argv))
  return <- c("Training" = (if(training_p > 0.0) opf_read_subGraph(paste(file,".training.dat", sep = "")) else NA),
              "Evaluating" = (if(evaluating_p > 0.0) opf_read_subGraph(paste(file,".evaluating.dat", sep = "")) else NA),
              "Testing" = (if(testing_p > 0.0) opf_read_subGraph(paste(file,".testing.dat", sep = "")) else NA))
  return(return)
}

#'Executes the training phase of the OPF classifier
#'
#'@param dataSet The training object produced by the opf_split (subGraph object)
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns a list which contains the classifier object and the classification list object
#'
#'@useDynLib libOPF
#'@export
opf_train <- function(dataSet, precomputedDistance = NA){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  argv <- c("", file)
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opf_train",length(argv),as.character(argv))
  return(c("classifier"=opf_read_modelfile(paste(file,".classifier.opf", sep = "")),"classification"=opf_read_classification(paste(file,".classifier.opf.out", sep = ""))))
}

#'Executes the test phase of the OPF classifier with knn adjacency
#'
#'@param dataSet The testing object produced by the opf_split (subGraph object)
#'@param classifier The classifier object produced by one of the classification functions (model object)
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@return
#'Returns the given subGraph classification list
#'
#'@useDynLib libOPF
#'@export
opf_knn_classify <- function(dataSet, classifier, precomputedDistance = NA){
  file <- tempfile()
  opf_write_subGraph(dataSet,file)
  opf_write_modelfile(classifier,paste(file,".classifier.opf", sep = ""))
  argv <- c("", file)
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opfknn_classify",length(argv),as.character(argv))
  return(opf_read_classification(paste(file,".classifier.opf.out", sep = "")))
}

#'Executes the training phase of the OPF classifier with knn adjacency
#'
#'@param trainFile The training object produced by the opf_split (subGraph object)
#'@param evaluatFile The evaluation object produced by the opf_split (subGraph object)
#'@param kmax kmax (maximum degree for the knn graph)
#'@param precomputedDistance The precomputed distance matrix produced by the opf_distance (leave it in blank if you are not using this resource)
#'
#'@details
#'Returns a list which contains the classifier object and the classification list object
#'
#'@useDynLib libOPF
#'@export
opf_knn_train <- function(trainFile, evaluatFile, kmax, precomputedDistance = NA){
  file <- tempfile()
  opf_write_subGraph(trainFile,file)
  opf_write_subGraph(evaluatFile,paste(file,".evaluat", sep = ""))
  argv <- c("", file, paste(file,".evaluat", sep = ""), kmax)
  if(!is.na(precomputedDistance)){
    opf_write_distances(precomputedDistance,paste(file,".distances", sep = ""))
    argv <- append(argv,paste(file,".distances", sep = ""))
  }
  aux <- .C("opfknn_train",length(argv),as.character(argv))
  return(c("classifier"=opf_read_modelfile(paste(file,".classifier.opf", sep = "")),"classification"=opf_read_classification(paste(file,".classifier.opf.out", sep = ""))))
}

#tools -------------------------------------------------------------------------------------

#'Checks the OPF file for proper formatting purposes
#'
#'@param file The text OPF file name
#'
#'@details
#'usage opf_check <input ASCII file in the LibOPF format>:
#'Note that the input file for opf_check must be a text file.
#'Use opf2txt to convert your OPF binary file into a text file.
#'
#'@useDynLib libOPF
#'@export
opf_check <- function(file){
  argv <- c("", file)
  aux <- .C("opf_check",length(argv),as.character(argv))
}

#'Converts an OPF subGraph object to a LIBSVM file
#'
#'@param data The subGraph object
#'@param outputFile LIBSVM output file name
#'
#'@useDynLib libOPF
#'@export
opf2svm <- function(data, outputFile){
  file <- tempfile()
  opf_write_subGraph(data,file)
  argv <- c("", file, outputFile)
  aux <- .C("opf2svm",length(argv),as.character(argv))
}

#'Converts an OPF subGraph object to a text file
#'
#'@param data OPF subGraph object
#'@param outputFile Text output file name
#'
#'@useDynLib libOPF
#'@export
opf2txt <- function(data, outputFile){
  file <- tempfile()
  opf_write_subGraph(data,file)
  argv <- c("", file, outputFile)
  aux <- .C("opf2txt",length(argv),as.character(argv))
}

#'Converts a LIBSVM file to an OPF subGraph object
#'
#'@param inputFile LIBSVM input file
#'
#'@return
#'Returns the OPF object
#'
#'@useDynLib libOPF
#'@export
svm2opf <- function(inputFile){
  file <- tempfile()
  argv <- c("", inputFile, file)
  aux <- .C("svm2opf",length(argv),as.character(argv))
  return(opf_read_subGraph(file))
}

#'Converts a text file to an OPF subGraph object
#'
#'@param inputFile Text input file
#'
#'@return
#'Returns the OPF object
#'
#'@useDynLib libOPF
#'@export
txt2opf <- function(inputFile){
  file <- tempfile()
  argv <- c("", inputFile, file)
  aux <- .C("txt2opf",length(argv),as.character(argv))
  return(opf_read_subGraph(file))
}

#'Subgraphs' node class
#'
#'@import methods
#'@export SNode
#'@exportClass SNode
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

#'Subgraph class
#'
#'@import methods
#'@export subGraph
#'@exportClass subGraph
subGraph <- setRefClass (
  "subGraph",
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

#'Creates an empty subGraph structure
#'
#'@param nnodes number of nodes
#'
#'@return
#'Returns an empty subGraph
#'
#'@useDynLib libOPF
#'@export
opf_create_subGraph <- function(nnodes){ #CreatesubGraph
  sg <- subGraph()
  sg$nnodes <- nnodes
  sg$ordered_list_of_nodes <- vector(mode = "integer", length = nnodes)
  for(i in 1:nnodes){
    sg$node <- c(sg$node, SNode())
    sg$node[[i]]$feat <- NA
    sg$node[[i]]$relevant <- 0
  }
  return(sg)
}

#'Reads a file which contains the subGraph
#'
#'@param file file name
#'
#'@return
#'Returns the subGraph object
#'
#'@useDynLib libOPF
#'@export
opf_read_subGraph <- function(file){ #ReadsubGraph
  binFile <- file(file, "rb")
  nnodes <- readBin(binFile, "int", endian = "little")
  g <- opf_create_subGraph(nnodes);
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

#'Writes into a file a subGraph
#'
#'@param g the subGraph object
#'@param file file name to save the subGraph
#'
#'@useDynLib libOPF
#'@export
opf_write_subGraph <- function(g, file){ #WritesubGraph
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

#'Reads a file which contains the learned model
#'
#'@param file file which contains the learned model
#'
#'@return
#'Returns the learned model object
#'
#'@useDynLib libOPF
#'@export
opf_read_modelfile <- function(file){ #opf_read_modelfile
  #if((fp = fopen(file, "rb")) == NULL){ #arrumar
  #  sprintf(msg, "%s%s", "Unable to open file ", file)
  #  Error(msg,"ReadsubGraph")
  #}
  binFile <- file(file, "rb")
  nnodes <- readBin(binFile, "int", size=4, endian = "little")
  g <- opf_create_subGraph(nnodes)
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

#'Writes into a file the trained OPF classifier
#'
#'@param g the classifier object
#'@param file file name to save the classifier
#'
#'@useDynLib libOPF
#'@export
opf_write_modelfile <- function(g, file){
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

#'Reads a file which contains the nodes' predicted labels
#'
#'@param file file which contains the nodes' predicted labels
#'
#'@return
#'Returns the predicted labels list
#'
#'@useDynLib libOPF
#'@export
opf_read_classification <- function(file){ #opf_WriteOutputFile
  fp <- file(file, "r")
  return <- as.double(readLines(fp))
  close(fp)
  return(return)
}

#'Writes into a file the predicted labels produced by the opf classificator
#'
#'@param classes classification list (i.e.,predicted labels) produced by the classifier
#'@param file where you want to save the classification vector
#'
#'@useDynLib libOPF
#'@export
opf_write_classification <- function(classes, file){ #opf_WriteOutputFile
  fp <- file(file, "w")
  writeLines(as.character(classes),fp)
  close(fp)
}

#'Reads a file which contains the precalculated distances
#'
#'@param file file which contains the distances matrix
#'
#'@return
#'Returns the precalculated distances matrix
#'
#'@useDynLib libOPF
#'@export
opf_read_distances <- function(file)
{
  fp <- file(file,"rb");
  nsamples <- readBin(fp, "int", size=4, endian = "little")
  #if (fread(&nsamples, sizeof(int), 1, fp) != 1)
  #  Error("Could not read number of samples","opf_read_distances"); #arrumar
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

#'Writes into a file the precalculated distances computed by opf_distances function
#'
#'@param distances the matrix produced by the opf distances function
#'@param file file name where you want to save the distances
#'
#'@useDynLib libOPF
#'@export
opf_write_distances <- function(distances, file)
{
  fp <- file(file,"wb");
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
