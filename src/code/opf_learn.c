#include "OPF.h"

static int main(int argc, char **argv)
{
	
	Rprintf("\nProgram that executes the learning phase for the OPF classifier\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if ((argc != 3) && (argc != 4))
	{
		REprintf("\nusage opf_learn <P1> <P2> <P3>");
		REprintf("\nP1: training set in the OPF file format");
		REprintf("\nP2: evaluation set in the OPF file format");
		REprintf("\nP3: precomputed distance file (leave it in blank if you are not using this resource\n");
		return 0;
	}

	float Acc, time;
	char fileName[512];
	int i, n;
	timer tic, toc;
	FILE *f = NULL;

	if (argc == 4)
		opf_PrecomputedDistance = 1;
	Rprintf("\nReading data file ...");
	
	Subgraph *gTrain = ReadSubgraph(argv[1]), *gEval = ReadSubgraph(argv[2]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[3], &n); if(errorOccurred) return 0;
	}

	Rprintf("\nLearning from errors in the evaluation set...");
	
	gettimeofday(&tic, NULL);
	opf_OPFLearning(&gTrain, &gEval); if(errorOccurred) return 0;
	gettimeofday(&toc, NULL);
	time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
	Acc = opf_Accuracy(gTrain); if(errorOccurred) return 0;
	Rprintf("\nFinal opf_Accuracy in the training set: %.2f%%", Acc * 100);
	
	Acc = opf_Accuracy(gEval); if(errorOccurred) return 0;
	Rprintf("\nFinal opf_Accuracy in the evaluation set: %.2f%%", Acc * 100);
	

	Rprintf("\n\nWriting classifier's model file ...");
	
	opf_WriteModelFile(gTrain, "classifier.opf");
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&gTrain);
	DestroySubgraph(&gEval);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}
	Rprintf(" OK\n");
	

	sprintf(fileName, "%s.time", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", time);
	fclose(f);

	return 0;
}

void opf_learn(int *argc, char **argv){
	main(*argc,argv);
	
}