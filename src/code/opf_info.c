#include "OPF.h"
#include <stdio.h>
#include <time.h>

static int main(int argc, char **argv)
{
	
	Rprintf("\nProgram that gives information about the OPF file\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if (argc != 2)
	{
		REprintf("\nusage opf_info <P1>");
		REprintf("\nP1: OPF file\n");
		return 0;
	}

	Subgraph *g = NULL;
	FILE *fp = NULL;
	int ndata, nfeats, nlabels;
	char msg[128];

	if ((fp = fopen(argv[1], "rb")) == NULL)
	{
		sprintf(msg, "%s%s", "Unable to open file ", argv[1]);
		Error(msg, "opf_info"); return 0;
	}

	if (fread(&ndata, sizeof(int), 1, fp) != 1)
	{
		REprintf("\n Could not read number of samples");
		return 0;
	}
	if (fread(&nlabels, sizeof(int), 1, fp) != 1)
	{
		REprintf("\n Could not read number of labels");
		return 0;
	}

	if (fread(&nfeats, sizeof(int), 1, fp) != 1)
	{
		REprintf("\n Could not read number of features");
		return 0;
	}

	Rprintf("\nInformations about %s file\n --------------------------------", argv[1]);
	Rprintf("\nData size: %d", ndata);
	Rprintf("\nFeatures size: %d", nfeats);
	Rprintf("\nLabels number: %d", nlabels);
	Rprintf("\n--------------------------------\n");

	DestroySubgraph(&g);
	fclose(fp);

	return 0;
}

SEXP opf_info(int argc, char **argv){
	main(argc,argv);
	return NULL;
}