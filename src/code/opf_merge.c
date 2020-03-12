#include "OPF.h"
#include <stdio.h>

static int main(int argc, char **argv)
{
	errorOccurred = 0;	

	if ((argc == 2) || (argc <= 1))
	{
		REprintf("\nusage opf_merge <P1> <P2> ... <Pn>");
		REprintf("\nP1: input dataset 1 in the OPF file format");
		REprintf("\nP2: input dataset 2 in the OPF file format");
		REprintf("\nPn: input dataset n in the OPF file format\n");
		return 0;
	}
	Subgraph **g = (Subgraph **)malloc(sizeof(Subgraph **) * (argc - 1)), *merged = NULL, *aux = NULL;
	int i;
	char fileName[512];
	
	Rprintf("\nReading data sets ...");
	
	for (i = 0; i < argc - 1; i++){
		g[i] = ReadSubgraph(argv[i + 1]); if(errorOccurred) return 0;
	}
	Rprintf(" OK");
	

	aux = CopySubgraph(g[0]); if(errorOccurred) return 0;
	for (i = 1; i < argc - 1; i++)
	{
		merged = opf_MergeSubgraph(aux, g[i]); if(errorOccurred) return 0;
		DestroySubgraph(&aux);
		aux = CopySubgraph(merged); if(errorOccurred) return 0;
		DestroySubgraph(&merged);
	}

	Rprintf("\nWriting data set to disk ...");
	
	sprintf(fileName, "%s.merged.dat", argv[1]);
	WriteSubgraph(aux, fileName); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	for (i = 0; i < argc - 1; i++)
		DestroySubgraph(&g[i]);
	DestroySubgraph(&aux);
	free(g);
	Rprintf(" OK\n");

	return 0;
}

void c_opf_merge(int *argc, char **argv){
	main(*argc,argv);
	
}