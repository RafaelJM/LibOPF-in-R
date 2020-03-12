#include "OPF.h"

static int main(int argc, char **argv)
{
  errorOccurred = 0;  

  if ((argc != 5) && (argc != 4) && (argc != 3) && (argc != 2))
  {
    REprintf("\nusage opf_semi_train <P1> <P2>");
    REprintf("\nP1: Labeled training set in the OPF file format");
    REprintf("\nP2: Unlabeled training set in the OPF file format");
    REprintf("\nP3: Evaluation set in the OPF file format");
    REprintf("\nP4: Precomputed distance file (leave it in blank if you are not using this resource)\n");
    return 0;
  }

  int n, i;
  int opf_ComputeEvaluation = 0;
  char fileName[256];
  FILE *f = NULL;
  timer tic, toc;
  float time;
  Subgraph *geval = NULL;

  if (argc == 4)
    opf_ComputeEvaluation = 1;

  if (argc == 5)
    opf_PrecomputedDistance = 1;

  Rprintf("\nReading labeled data file...");
  
  Subgraph *g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;
  Rprintf(" OK");
  

  Rprintf("\nReading unlabeled data file...");
  
  Subgraph *gunl = ReadSubgraph(argv[2]); if(errorOccurred) return 0;
  Rprintf(" OK");
  

  if (opf_ComputeEvaluation)
  {
    Rprintf("\nReading evaluation data file...");
    
    geval = ReadSubgraph(argv[3]); if(errorOccurred) return 0;
    Rprintf(" OK");
    
  }

  if (opf_PrecomputedDistance){
    opf_DistanceValue = opf_ReadDistances(argv[4], &n); if(errorOccurred) return 0;
  }

  Rprintf("\nTraining Semi OPF classifier ...");
  
  gettimeofday(&tic, NULL);
  Subgraph *s = opf_OPFSemiLearning(g, gunl, geval); if(errorOccurred) return 0;
  opf_OPFTraining(s); if(errorOccurred) return 0;
  gettimeofday(&toc, NULL);
  Rprintf(" OK");
  

  Rprintf("\nWriting classifier's model file ...");
  
  opf_WriteModelFile(s, "classifier.opf");
  Rprintf(" OK");
  

  Rprintf("\nWriting output file ...");
  
  sprintf(fileName, "%s.out", argv[1]);
  opf_WriteOutputFile(s, fileName);
  Rprintf(" OK");
  

  Rprintf("\nDeallocating memory ...");
  
  DestroySubgraph(&s);
  if (opf_PrecomputedDistance)
  {
    for (i = 0; i < n; i++)
      free(opf_DistanceValue[i]);
    free(opf_DistanceValue);
  }
  Rprintf(" OK\n");

  time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
  Rprintf("\nExecution time: %f seconds\n", time);
  

  sprintf(fileName, "%s.time", argv[1]);
  f = fopen(fileName, "a");
  fprintf(f, "%f\n", time);
  fclose(f);

  return 0;
}

void opf_semi(int *argc, char **argv){
	main(*argc,argv);
	
}