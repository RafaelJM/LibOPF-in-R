#include "OPF.h"

static int main(int argc, char **argv)
{
	
	Rprintf("\nProgram that computes the OPF accuracy for each class of a given set\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if (argc != 2)
	{
		REprintf("\nusage opf_accuracyforlabel <P1>");
		REprintf("\nP1: data set in the OPF file format\n");
		return 0;
	}

	int i;
	float *Acc = NULL;
	FILE *f = NULL;
	char fileName[256];

	Rprintf("\nReading data file ...");
	
	Subgraph *g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
	Rprintf(" OK");
	

	Rprintf("\nReading output file ...");
	
	sprintf(fileName, "%s.out", argv[1]);
	f = fopen(fileName, "r");
	if (!f)
	{
		REprintf("\nunable to open file %s", argv[2]);
		return 0;
	}

	for (i = 0; i < g->nnodes; i++)
	{
		if (fscanf(f, "%d", &g->node[i].label) != 1)
		{
			Error("Error reading node label", "opf_Accuracy"); return 0;
		}
	}
	fclose(f);
	Rprintf(" OK");
	

	Rprintf("\nComputing accuracy ...");
	
	Acc = opf_Accuracy4Label(g); if(errorOccurred) return 0;
	for (i = 1; i <= g->nlabels; i++)
		Rprintf("\nClass %d: %.2f%%", i, Acc[i] * 100);
	

	Rprintf("\nWriting accuracy in output file ...");
	
	sprintf(fileName, "%s.acc", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f", Acc[1]);
	for (i = 2; i <= g->nlabels; i++)
	{
		fprintf(f, " %f", Acc[i]);
		fprintf(f, "\n");
	}
	fclose(f);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	
	DestroySubgraph(&g);
	Rprintf(" OK\n");

	free(Acc);

	return 0;
}

SEXP opf_accuracy4label(int argc, char **argv){
	main(argc,argv);
	return NULL;
}