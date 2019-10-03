#include "OPF.h"

static int main(int argc, char **argv)
{
	errorOccurred = 0;
	Rprintf("\nProgram that computes OPF accuracy of a given set\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");
	

	if (argc != 2)
	{
		REprintf("\nusage opf_accuracy <P1>");
		REprintf("\nP1: data set in the OPF file format");
		return 0;
	}

	int i, j, **CM = NULL;
	;
	float Acc, tmp;
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
		if (fscanf(f, "%d", &g->node[i].label) != 1)
		{
			Error("Error reading node label", "opf_Accuracy"); return 0;
		}
	fclose(f);
	Rprintf(" OK");
	

	CM = opf_ConfusionMatrix(g);
	for (i = 1; i <= g->nlabels; i++)
	{
		Rprintf("\n");
		tmp = 0;
		for (j = 1; j <= g->nlabels; j++)
		{
			tmp += CM[i][j];
		}
	}

	for (i = 0; i < g->nlabels + 1; i++)
		free(CM[i]);
	free(CM);

	Rprintf("\nComputing accuracy ...");
	
	Acc = opf_Accuracy(g); if(errorOccurred) return 0;
	Rprintf("\nAccuracy: %.2f%%", Acc * 100);
	

	Rprintf("\nWriting accuracy in output file ...");
	
	sprintf(fileName, "%s.acc", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", Acc * 100);
	fclose(f);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	
	DestroySubgraph(&g);
	Rprintf(" OK\n");

	return 0;
}

void opf_accuracy(int *argc, char **argv){
	main(*argc,argv);
	
}