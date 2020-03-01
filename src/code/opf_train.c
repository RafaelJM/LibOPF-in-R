#include "OPF.h"

static int main(int argc, char **argv)
{
	errorOccurred = 0;


	if ((argc != 3) && (argc != 2))
	{
		REprintf("\nusage opf_train <P1> <P2>");
		REprintf("\nP1: training set in the OPF file format");
		REprintf("\nP2: precomputed distance file (leave it in blank if you are not using this resource)\n");
		return 0;
	}

	int n, i;
	char fileName[256];
	FILE *f = NULL;
	timer tic, toc;
	float time;

	if (argc == 3)
		opf_PrecomputedDistance = 1;

	Rprintf("\nReading data file ...");
	
	Subgraph *g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[2], &n); if(errorOccurred) return 0;
	}

	Rprintf("\nTraining OPF classifier ...");
	
	gettimeofday(&tic, NULL);
	opf_OPFTraining(g); if(errorOccurred) return 0;
	gettimeofday(&toc, NULL);
	Rprintf(" OK");
	

	Rprintf("\nWriting classifier's model file ...");
	
	sprintf(fileName, "%s.classifier.opf", argv[1]);
	opf_WriteModelFile(g, fileName);
	Rprintf(" OK");
	

	Rprintf("\nWriting output file ...");
	
	sprintf(fileName, "%s.out", argv[1]);
	opf_WriteOutputFile(g, fileName);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	
	DestroySubgraph(&g);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}
	Rprintf(" OK\n");

	time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
	Rprintf("\nTraining time: %f seconds\n", time);
	

	sprintf(fileName, "%s.time", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", time);
	fclose(f);

	return 0;
}

void opf_train(int *argc, char **argv){
	main(*argc,argv);
	
}