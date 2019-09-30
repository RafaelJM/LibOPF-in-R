#include "OPF.h"

static int main(int argc, char **argv)
{
	int i, n, op;
	float value;
	char fileName[256];

	Rprintf("\nProgram that computes clusters by OPF\n");
	Rprintf("\nIf you have any problem, please contact: ");
	Rprintf("\n- alexandre.falcao@gmail.com");
	Rprintf("\n- papa.joaopaulo@gmail.com\n");
	Rprintf("\nLibOPF version 2.0 (2009)\n");
	Rprintf("\n");

	if ((argc != 6) && (argc != 5))
	{
		REprintf("\nusage opf_cluster <P1> <P2> <P3> <P4> <P5> <P6>");
		REprintf("\nP1: unlabeled data set in the OPF file format");
		REprintf("\nP2: kmax(maximum degree for the knn graph)");
		REprintf("\nP3: P3 0 (height), 1(area) and 2(volume)");
		REprintf("\nP4: value of parameter P3 in (0-1)");
		REprintf("\nP5: precomputed distance file (leave it in blank if you are not using this resource");
		return 0;
	}

	if (argc == 6)
		opf_PrecomputedDistance = 1;
	Rprintf("\nReading data file ...");
	Subgraph *g = ReadSubgraph(argv[1]); if(errorOccurred) return 0;

	if (opf_PrecomputedDistance)
	{
		opf_DistanceValue = opf_ReadDistances(argv[5], &n); if(errorOccurred) return 0;
	}

	op = atoi(argv[3]);

	opf_BestkMinCut(g, 1, atoi(argv[2])); if(errorOccurred) return 0; //default kmin = 1

	value = atof(argv[4]);
	if ((value < 1) && (value > 0))
	{
		Rprintf("\n\n Filtering clusters ... ");
		switch (op)
		{
		case 0:
			Rprintf("\n by dome height ... ");
			float Hmax = 0;
			for (i = 0; i < g->nnodes; i++)
				if (g->node[i].dens > Hmax)
					Hmax = g->node[i].dens;
			opf_ElimMaxBelowH(g, value * Hmax);
			break;
		case 1:
			Rprintf("\n by area ... ");
			opf_ElimMaxBelowArea(g, (int)(value * g->nnodes)); if(errorOccurred) return 0;
			break;
		case 2:
			Rprintf("\n by volume ... ");
			double Vmax = 0;
			for (i = 0; i < g->nnodes; i++)
				Vmax += g->node[i].dens;
			opf_ElimMaxBelowVolume(g, (int)(value * Vmax / g->nnodes)); if(errorOccurred) return 0;
			break;
		default:
			REprintf("\nInvalid option for parameter P3 ... ");
			return 0;
			break;
		}
	}

	Rprintf("\n\nClustering by OPF ");
	opf_OPFClustering(g); if(errorOccurred) return 0;
	Rprintf("num of clusters %d\n", g->nlabels);

	/* If the training set has true labels, then create a
	   classifier by propagating the true label of each root to
	   the nodes of its tree (cluster). This classifier can be
	   evaluated by running opf_knn_classify on the training set
	   or on unseen testing set. Otherwise, copy the cluster
	   labels to the true label of the training set and write a
	   classifier, which essentially can propagate the cluster
	   labels to new nodes in a testing set. */

	/*if (g->node[0].truelabel!=0){ // labeled training set
	  g->nlabels = 0;
	  for (i = 0; i < g->nnodes; i++){//propagating root labels
	    if (g->node[i].root==i)
	      g->node[i].label = g->node[i].truelabel;
	    else
	      g->node[i].label = g->node[g->node[i].root].truelabel;
	  }

	  for (i = 0; i < g->nnodes; i++){
		  // retrieve the original number of true labels
		  if (g->node[i].label > g->nlabels)
			  g->nlabels = g->node[i].label;
	  }
	}else{ // unlabeled training set
	  for (i = 0; i < g->nnodes; i++)
	    g->node[i].truelabel = g->node[i].label+1;
	}*/

	Rprintf("\nWriting classifier's model file ...");
	
	opf_WriteModelFile(g, "classifier.opf");
	Rprintf(" OK");
	

	Rprintf("\nWriting output file ...");
	
	sprintf(fileName, "%s.out", argv[1]);
	opf_WriteOutputFile(g, fileName);
	Rprintf(" OK");
	

	Rprintf("\n\nDeallocating memory ...\n");
	DestroySubgraph(&g);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}

	return 0;
}

void opf_cluster(int *argc, char **argv){
	main(*argc,argv);
	
}