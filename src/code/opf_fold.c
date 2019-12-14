#include "OPF.h"
#include <stdio.h>

static int main(int argc, char **argv)
{
	errorOccurred = 0;
	Rprintf("\nProgram that generates k folds (files) for the OPF classifier\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if (argc != 4)
	{
		REprintf("\nusage opf_fold <P1> <P2> <P3>");
		REprintf("\nP1: input dataset in the OPF file format");
		REprintf("\nP2: k");
		REprintf("\nP3: normalize features? 1 - Yes  0 - No\n\n");
		return 0;
	}
	Subgraph *g = NULL, **fold = NULL;
	int k = atoi(argv[2]), i, op = atoi(argv[3]);
	char fileName[16];

	Rprintf("\nReading data set ...");
	
	g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nCreating %d folds ...", k);
	
	fold = opf_kFoldSubgraph(g, k); if(errorOccurred) return 0;
	Rprintf(" OK\n");

	for (i = 0; i < k; i++)
	{
		Rprintf("\nWriting fold %d ...", i + 1);
		
		sprintf(fileName, strcat(argv[1],"1"));
		if (op){
			opf_NormalizeFeatures(fold[i]); if(errorOccurred) return 0;
		}
		WriteSubgraph(fold[i], fileName); if(errorOccurred) return 0;
	}
	Rprintf(" OK\n");

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&g);
	for (i = 0; i < k; i++)
		DestroySubgraph(&fold[i]);
	free(fold);
	Rprintf(" OK\n");

	return 0;
}

void opf_fold(int *argc, char **argv){
	main(*argc,argv);
	
}