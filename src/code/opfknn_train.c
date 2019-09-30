#include "OPF.h"

static int main(int argc, char **argv)
{
	
	Rprintf("\nProgram that executes the training phase of the OPF classifier with knn adjacency\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if ((argc != 5) && (argc != 4))
	{
		REprintf("\nusage opf_train <P1> <P2>");
		REprintf("\nP1: training set in the OPF file format");
		REprintf("\nP2: evaluating set in the OPF file format (used to learn k)");
		REprintf("\nP3: kmax");
		REprintf("\nP4: precomputed distance file (leave it in blank if you are not using this resource)\n");
		return 0;
	}

	int n, i, kmax = atoi(argv[3]);
	char fileName[256];
	FILE *f = NULL;
	timer tic, toc;
	double time;

	if (argc == 5)
		opf_PrecomputedDistance = 1;

	Rprintf("\nReading data file ...");
	
	Subgraph *Train = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	Subgraph *Eval = ReadSubgraph(argv[2]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[4], &n); if(errorOccurred) return 0;
	}

	Rprintf("\nTraining OPF classifier ...");
	
	gettimeofday(&tic, NULL);
	opf_OPFknnTraining(Train, Eval, kmax); if(errorOccurred) return 0;
	gettimeofday(&toc, NULL);
	Rprintf(" OK");
	

	Rprintf("\nWriting classifier's model file ...");
	
	opf_WriteModelFile(Train, "classifier.opf");
	Rprintf(" OK");
	

	Rprintf("\nWriting output file ...");
	
	sprintf(fileName, "%s.out", argv[1]);
	opf_WriteOutputFile(Train, fileName);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	
	DestroySubgraph(&Train);
	DestroySubgraph(&Eval);
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

SEXP opfknn_train(int argc, char **argv){
	main(argc,argv);
	return NULL;
}