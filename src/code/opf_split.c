#include "OPF.h"
#include <stdio.h>

void CheckInputData(float TrPercentage, float EvalPercentage, float TestPercentage)
{
	REprintf("\nSummation of set percentages = %.1f ...", TrPercentage + EvalPercentage + TestPercentage);
	if ((float)(TrPercentage + EvalPercentage + TestPercentage) != (float)1.0){
		Error("Percentage summation is not equal to 1", "CheckInputData"); return;
	}
	REprintf(" OK");

	REprintf("\nChecking set percentages ...");
	if (TrPercentage == 0.0f || TestPercentage == 0.0f){
		Error("Percentage of either training set or test set is equal to 0", "CheckInputData"); return;
	}
	Rprintf(" OK");
}

static int main(int argc, char **argv)
{
	
	Rprintf("\nProgram that generates training, evaluation and test sets for the OPF classifier\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if (argc != 6)
	{
		REprintf("\nusage opf_split <P1> <P2> <P3> <P4> <P5>");
		REprintf("\nP1: input dataset in the OPF file format");
		REprintf("\nP2: percentage for the training set size [0,1]");
		REprintf("\nP3: percentage for the evaluation set size [0,1] (leave 0 in the case of no learning)");
		REprintf("\nP4: percentage for the test set size [0,1]");
		REprintf("\nP5: normalize features? 1 - Yes  0 - No\n\n");
		return 0;
	}
	Subgraph *g = NULL, *gAux = NULL, *gTraining = NULL, *gEvaluating = NULL, *gTesting = NULL;
	float training_p = atof(argv[2]), evaluating_p = atof(argv[3]), testing_p = atof(argv[4]);
	int normalize = atoi(argv[5]);

	CheckInputData(training_p, evaluating_p, testing_p); if(errorOccurred) return 0;

	Rprintf("\nReading data set ...");
	
	g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	if (normalize){
		opf_NormalizeFeatures(g); if(errorOccurred) return 0;
	}

	Rprintf("\nSplitting data set ...");
	
	opf_SplitSubgraph(g, &gAux, &gTesting, training_p + evaluating_p); if(errorOccurred) return 0;

	if (evaluating_p > 0){
		opf_SplitSubgraph(gAux, &gTraining, &gEvaluating, training_p / (training_p + evaluating_p)); if(errorOccurred) return 0;
	}
	else{
		gTraining = CopySubgraph(gAux); if(errorOccurred) return 0;
	}

	Rprintf(" OK");
	

	Rprintf("\nWriting data sets to disk ...");
	
	WriteSubgraph(gTraining, "training.dat"); if(errorOccurred) return 0;
	if (evaluating_p > 0){
		WriteSubgraph(gEvaluating, "evaluating.dat"); if(errorOccurred) return 0;
	}
	WriteSubgraph(gTesting, "testing.dat"); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&g);
	DestroySubgraph(&gAux);
	DestroySubgraph(&gTraining);
	DestroySubgraph(&gEvaluating);
	DestroySubgraph(&gTesting);
	Rprintf(" OK\n");

	return 0;
}

SEXP opf_split(int argc, char **argv){
	main(argc,argv);
	return NULL;
}