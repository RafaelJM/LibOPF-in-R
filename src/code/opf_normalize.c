#include "OPF.h"
#include <stdio.h>

static int main(int argc, char **argv)
{
	errorOccurred = 0;
	Rprintf("\nProgram that normalizes data for the OPF classifier\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 3.0 (2013)\n");
	Rprintf("\n");
	

	if (argc != 3)
	{
		REprintf("\nusage opf_normalize <P1> <P2>");
		REprintf("\nP1: input dataset in the OPF file format");
		REprintf("\nP2: normalized output dataset in the OPF file format\n");
		return 0;
	}
	Subgraph *g = NULL;

	Rprintf("\nReading data set ...");
	
	g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nNormalizing data set ...");
	
	opf_NormalizeFeatures(g); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nWriting normalized data set to disk ...");
	
	WriteSubgraph(g, argv[2]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&g);
	Rprintf(" OK\n");

	return 0;
}

void opf_normalize(int *argc, char **argv){
	main(*argc,argv);
	
}