#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>

static int main(int argc, char **argv)
{
	if (argc != 4)
	{
		REprintf("\nusage statistics <file name> <running times> <message>\n");
		return 0;
	}

	FILE *fpIn = NULL;
	int i, it = atoi(argv[2]);
	float Std = 0.0f, MeanAcc = 0.0f, aux, *acc = NULL;

	/*Computing mean accuracy and standard deviation***/
	fpIn = fopen(argv[1], "r");
	if (!fpIn)
	{
		REprintf("\nunable to open file %s\n", argv[1]);
		return 0;
	}

	acc = (float *)malloc(it * sizeof(float));
	for (i = 1; i <= it; i++)
	{
		if (fscanf(fpIn, "%f", &aux) != 1)
		{
			REprintf("\n Could not read accuracy");
			return 0;
		}
		acc[i - 1] = aux;
		MeanAcc += aux;
	}
	MeanAcc /= it;
	for (i = 0; i < it; i++)
		Std += pow(acc[i] - MeanAcc, 2);
	Std = sqrt(Std / it);

	fclose(fpIn);
	free(acc);

	Rprintf("\n%s %f with standard deviation: %f\n", argv[3], MeanAcc, Std);

	return 0;
}

void statistics(int *argc, char **argv){
	main(*argc,argv);
	
}