#include <stdio.h>
#include <stdlib.h>
#include "OPF.h"

static int main(int argc, char **argv)
{

	if (argc != 4)
	{
		REprintf("\nusage kmeans <P1> <P2> <P3>\n");
		REprintf("\nP1: input file name in the OPF binary format");
		REprintf("\nP2: number of clusters (k)");
		REprintf("\nP3: output file name containing cluster centroids\n");
		return 0;
	}

	REprintf("\nProgram to calculate cluster centroids using K-means.\n");

	int i, j, k = atoi(argv[2]);
	double **mean = NULL;
	Subgraph *g = NULL;
	FILE *fp = NULL;

	fflush(stderr);
	REprintf("\nReading dataset ... ");
	g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	REprintf("OK");

	mean = (double **)calloc(k, sizeof(double *));
	for (i = 0; i < k; i++)
		mean[i] = (double *)calloc(g->nfeats, sizeof(double *));

	fflush(stderr);
	REprintf("\nRunning k-means ... ");
	kMeans(g, mean, k);
	REprintf("OK");

	REprintf("\nPurity of clustering: %lf", Purity(g));

	Rprintf("\nWriting output file ...");
	
	fp = fopen(argv[3], "w");
	for (i = 0; i < k; i++)
	{
		for (j = 0; j < g->nfeats; j++)
			fprintf(fp, "%lf ", mean[i][j]);
		fprintf(fp, "\n");
	}
	fclose(fp);
	Rprintf(" OK");
	

	fflush(stderr);
	REprintf("\nDeallocating memory ... ");
	for (i = 0; i < k; i++)
		free(mean[i]);
	free(mean);
	DestroySubgraph(&g);
	REprintf("OK\n");

	return 0;
}

void kmeans(int *argc, char **argv){
	main(*argc,argv);
	
}