#'Computes the OPF accuracy of a given set
#'
#'@param
#'dataSet path to data set in the OPF file format (.dat and .dat.out)
#'
#'@details
#'Reads the .dat and .dat.out file with the name that was past in the variable dataSet
#'Computes and creates a .acc file, with the same name as the input, that contains the accuracy. Also creates a .time that contais the processing time.
#'
#'@export
#'@useDynLib opf_accuracy
opf_accuracy <- function(dataSet){
	argv <- c("", dataSet)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_accuracy")
}

#'Computes the OPF accuracy for each class of a given set
#'
#'@param
#'dataSet path to data set in the OPF file format (.dat and .dat.out)
#'
#'@details
#'Reads the .dat and .dat.out file with the name that was past in the variable dataSet
#'Computes and creates a .acc file, with the same name as the input, that contains the accuracy. Also creates a .time that contais the processing time.
#'
#'@export
#'@useDynLib opf_accuracy4label
opf_accuracy4label <- function(dataSet){
	argv <- c("", dataSet)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_accuracy4label")
}

#'Executes the test phase of the OPF classifier
#'
#'@param dataSet path to test set in the OPF file format
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource)
#'
#'@details
#'Creates a file .out, which is used in opf_accuracy, for example.
#'
#'@export
#'@useDynLib opf_classify
opf_classify <- function(dataSet,precomputedDistance = NA){
  	argv <- c("", dataSet)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_classify")
}

#'Computes clusters by OPF
#'
#'@param dataSet path to unlabeled data set in the OPF file format
#'@param kmax kmax(maximum degree for the knn graph)
#'@param calculateOp how its calculate. 0 for (height), 1 for (area) and 2 for (volume)
#'@param value value of parameter "op" in (0-1)
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource
#'
#'@details
#'Creates a file .out, which is used in opf_knn_classify, for example.
#'
#'@export
#'@useDynLib opf_cluster
opf_cluster <- function(dataSet, kmax, calculateOp, value,precomputedDistance = NA){
  	argv <- c("", dataSet,kmax,calculateOp,value)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_cluster")
}

#'Generates the precomputed distance file for the OPF classifier
#'
#'@param dataSet path to dataset in the OPF file format
#'@param distanceOp distance calculation option
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@details
#'Options for distance calculation:
#'1 - Euclidean
#'2 - Chi-Square
#'3 - Manhattan (L1)
#'4 - Canberra
#'5 - Squared Chord
#'6 - Squared Chi-Squared
#'7 - BrayCurtis
#'
#'@details
#'Reads the dataset and create a distances.dat with the distances.
#'
#'@export
#'@useDynLib opf_distance
opf_distance <- function(dataSet, distanceOp, normalize){
  	argv <- c("", dataSet,distanceOp,normalize)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_cluster")
}

#'Generates k folds (files) for the OPF classifier
#'
#'@param dataSet path to input dataSet in the OPF file format
#'@param k number of folds
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@details
#'Generate k files with the name fold_(number).dat
#'
#'@export
#'@useDynLib opf_fold
opf_fold <- function(dataSet, k, normalize){
	argv <- c("", dataSet,k,normalize)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_fold")
}

#'Gives information about the OPF file
#'
#'@param
#'dataSet path to OPF file
#'
#'@export
#'@useDynLib opf_info
opf_info <- function(dataSet){
	argv <- c("", dataSet)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_info")
}

#'Executes the learning phase for the OPF classifier
#'
#'@param trainFile path to training set in the OPF file format
#'@param evaluatFile path to evaluation set in the OPF file format
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource
#'
#'@details
#'Execute the training phase and generates an output file named classifier.opf
#'
#'@export
#'@useDynLib opf_learn
opf_learn <- function(trainFile, evaluatFile, precomputedDistance = NA){
  	argv <- c("", trainFile, evaluatFile)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_learn")
}

#'Merge subgraphs
#'
#'@param dataSets vector of paths to dataSets in the OPF file format
#'
#'@export
#'@useDynLib opf_merge
opf_merge <- function(dataSets){
  	argv <- c("", dataSets)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_merge")
}

#'Normalizes data for the OPF classifier
#'
#'@param dataSet path to dataSet in the OPF file format
#'@param normalizedOutput path to normalized output dataSet in the OPF file format
#'
#'@export
#'@useDynLib opf_normalize
opf_normalize <- function(dataSet, normalizedOutput){
  	argv <- c("", dataSet, normalizedOutput)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_normalize")
}

#'Executes the pruning algorithm of the OPF classifier
#'
#'@param dataTraining path to training set in the OPF file format
#'@param dataEvaluating path to evaluating set in the OPF file format
#'@param dataEvaluating path to evaluating set in the OPF file format
#'@param percentageAccuracy percentage of accuracy [0,1]
#'
#'@export
#'@useDynLib opf_pruning
opf_pruning <- function(dataTraining, dataEvaluating, percentageAccuracy, precomputedDistance){
  	argv <- c("", dataTraining,  dataEvaluating,  percentageAccuracy)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_pruning")
}

#'Executes the semi supervised training phase of the OPF classifier
#'
#'@param labeledTrainFile path to labeled training set in the OPF file format
#'@param unLabeledTrainFile path to unlabeled training set in the OPF file format
#'@param evaluatFile path to evaluation set in the OPF file format
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource)
#'
#'@details
#'Creates a file .out, which is used in opf_classify, for example.
#'
#'@export
#'@useDynLib opf_semi
opf_semi <- function(labeledTrainFile,  unLabeledTrainFile,  evaluatFile = "",  precomputedDistance = NA){
  	argv <- c("", labeledTrainFile,  unLabeledTrainFile,  evaluatFile)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_semi")
}

#'Generates training, evaluation and test sets for the OPF classifier
#'
#'@param dataSet path to input dataSet in the OPF file format
#'@param training_p percentage for the training set size [0,1]
#'@param evaluating_p percentage for the evaluation set size [0,1] (leave 0 in the case of no learning)
#'@param testing_p percentage for the test set size [0,1]
#'@param normalize distance normalization? 1- yes 0 - no
#'
#'@details
#'produces two or three files, training.dat, testing.dat and evaluating.dat
#'
#'@export
#'@useDynLib opf_split
opf_split <- function(dataSet, training_p, evaluating_p, testing_p, normalize){
  	argv <- c("", dataSet, training_p, evaluating_p, testing_p, normalize)
	.C("main",length(argv),as.character(argv), PACKAGE = "opf_split")
}

#'Executes the training phase of the OPF classifier
#'
#'@param dataSet path to training set in the OPF file format
#'@param precomputedDistance path to precomputed distance file (leave it in blank if you are not using this resource)
#'
#'@details
#'Execute the training phase and generates an output file named classifier.opf
#'
#'@export
#'@useDynLib opf_train
opf_train <- function(dataSet, precomputedDistance = NA){
  argv <- c("", dataSet)
  if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
  .C("main",length(argv),as.character(argv), PACKAGE = "opf_train")
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
#'@useDynLib opfknn_classify
opfknn_classify <- function(dataSet, precomputedDistance = NA){
	argv <- c("", dataSet)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opfknn_classify")
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
#'@useDynLib opfknn_train
opfknn_train <- function(trainFile, evaluatFile, kmax, precomputedDistance = NA){
	argv <- c("", trainFile, evaluatFile, kmax)
	if(!is.na(precomputedDistance)) argv <- append(argv,precomputedDistance)
	.C("main",length(argv),as.character(argv), PACKAGE = "opfknn_train")
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
#'@useDynLib kmeans
kmeans <- function(inputFile, numberClusters, outputFile){
  argv <- c("", inputFile, numberClusters, outputFile)
  .C("main",length(argv),as.character(argv), PACKAGE = "kmeans")
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
#'@useDynLib opf_check
opf_check <- function(inputFile){
  argv <- c("", inputFile)
  .C("main",length(argv),as.character(argv), PACKAGE = "opf_check")
}

#'Converts OPF to SVM
#'
#'@param inputFile path to OPF input file
#'@param outputFile path to SVM output file
#'
#'@export
#'@useDynLib opf2svm
opf2svm <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  .C("main",length(argv),as.character(argv), PACKAGE = "opf2svm")
}

#'Converts OPF to text
#'
#'@param inputFile path to OPF input file
#'@param outputFile path to text output file
#'
#'@export
#'@useDynLib opf2txt
opf2txt <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  .C("main",length(argv),as.character(argv), PACKAGE = "opf2txt")
}

#'Calculates mean of accuracy
#'
#'@param inputFile path to accuracy file
#'@param numberAccuracy how much accuracy will take
#'@param message text that will be show
#'
#'@export
#'@useDynLib statistcs
statistcs <- function(inputFile, numberAccuracy, message){
  argv <- c("", inputFile, numberAccuracy, message)
  .C("main",length(argv),as.character(argv), PACKAGE = "statistcs")
}

#'Converts SVM to OPF
#'
#'@param inputFile path to SVM input file
#'@param outputFile path to OPF output file
#'
#'@export
#'@useDynLib svm2opf
svm2opf <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  .C("main",length(argv),as.character(argv), PACKAGE = "svm2opf")
}

#'Converts text to OPF
#'
#'@param inputFile path to text input file
#'@param outputFile path to OPF output file
#'
#'@export
#'@useDynLib txt2opf
txt2opf <- function(inputFile, outputFile){
  argv <- c("", inputFile, outputFile)
  .C("main",length(argv),as.character(argv), PACKAGE = "txt2opf")
}


