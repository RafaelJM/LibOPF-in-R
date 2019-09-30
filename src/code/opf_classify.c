#include "OPF.h"

static int main(int argc, char **argv)
{
	
	Rprintf("\nProgram that executes the test phase of the OPF classifier\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 3.0 (2013)\n");
	Rprintf("\n");
	

	if ((argc != 3) && (argc != 2))
	{
		REprintf("\nusage opf_classify <P1> <P2>");
		REprintf("\nP1: test set in the OPF file format");
		REprintf("\nP2: precomputed distance file (leave it in blank if you are not using this resource\n");
		return 0;
	}

	int n, i;
	float time;
	char fileName[256];
	FILE *f = NULL;
	timer tic, toc;

	if (argc == 3)
		opf_PrecomputedDistance = 1;
	Rprintf("\nReading data files ...");
	
	Subgraph *gTest = ReadSubgraph(argv[1]), *gTrain = opf_ReadModelFile("classifier.opf"); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[2], &n); if(errorOccurred) return 0;
	}

	Rprintf("\nClassifying test set ...");
	
	gettimeofday(&tic, NULL);
	opf_OPFClassifying(gTrain, gTest);
	gettimeofday(&toc, NULL);
	Rprintf(" OK");
	

	Rprintf("\nWriting output file ...");
	
	sprintf(fileName, "%s.out", argv[1]);
	opf_WriteOutputFile(gTest, fileName);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&gTrain);
	DestroySubgraph(&gTest);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}
	Rprintf(" OK\n");

	time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
	Rprintf("\nTesting time: %f seconds\n", time);
	

	sprintf(fileName, "%s.time", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", time);
	fclose(f);

	return 0;
}

SEXP opf_classify(int argc, char **argv){
	main(argc,argv);
	return NULL;
}