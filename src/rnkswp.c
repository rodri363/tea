#include <apop.h>

static void swap(int *restrict L, int *restrict R){
	int tmp = *L;
	*L = *R;
	*R = tmp;
}

static void markswap(gsl_vector *swp, int j, int *swpsum){
	gsl_vector_set(swp, j, 1);
	(*swpsum) ++;
}

/**
\param xvals the array of values. Cutoffs matches this.
\param rnk pointer to an integer vector of ranks
\param N pointer to a single integer giving the length of *rnk
\param p pointer to a single double giving the percent of ranks allowed for swapping
\param cutoffs pointer to an interger vector giving the cutoffs
    of rank values separating distinct values of the generating vector
\param lcut pointer to a single integer giving the length of cutoffs
\param up pointer to a single integer giving the allowable swapping range based on cutoffs
*/
void rnkswpConstr(double *restrict xvals, int *restrict rnk, int *restrict N, double *restrict p,
					int *restrict cutoffs, int *restrict lcut, int *restrict up, int *restrict seed){
	int j; 				// current rank available for swapping
	int M; 				// last available rank to swap to
	int Q = *p * *N; 	// maximal swapping distance, based on N and p
	int kj; 			// rank to with which rank j will swap
	int swpsum = 0; 	// # of ranks that have been swapped (stopping criterion)
	int jcut,jval;		// last available rank for swapping based on cutoffs
	if ((Q <= 0) || (*up <= 0))
		return;
	gsl_vector *dons = gsl_vector_alloc(*N);// possible donors for rank under consideration
	gsl_vector *is_swapped = gsl_vector_calloc(*N); // 1 if rnk[i] has been swapped, 0 if not
	//gsl_rng *r = apop_rng_alloc(apop_opts.rng_seed++);
	gsl_rng *r = apop_rng_alloc(*seed);
	while (swpsum < *N){ // While any ranks remain unswapped
		j = gsl_vector_min_index(is_swapped);
		//find cutoff value index for j returns cutoff INDEX, not value
		for (jcut = 0; jcut < *lcut && j >= cutoffs[jcut]; jcut++)
			;
        assert(jcut ==0 || xvals[cutoffs[jcut-1]]==xvals[j]);
		int max_x = xvals[j]+*up;
		while (jcut < *lcut && xvals[cutoffs[jcut]] <= max_x)
			jcut++;
		/*add up value to cutoff to set donor index limit
		  then modify jcut to be that indexed value from cutoffs*/
		jval = (jcut == *lcut ? *N : cutoffs[jcut]); // add -1 to get uniform draws later...

		// M is the min of [the percent Q, the jcut cutoff that gives the max jump, the max]
		M = gsl_min(j+Q,*N);
		M = gsl_min(M,jval);

		int subdx  = 0;
		for(int jdx=j+1; jdx<M; jdx++)
			if (!gsl_vector_get(is_swapped,jdx))
				gsl_vector_set(dons, subdx++, jdx);
		if (!subdx) //j has nobody to play with
			kj = j;
		else
			kj = gsl_vector_get(dons, (int)(subdx* gsl_rng_uniform(r)));
			//kj = dons->data[(int) gsl_ran_flat(r,0,subdx)];
		markswap(is_swapped, j, &swpsum);
		if (kj != j){
			swap(rnk+j, rnk+kj);
			markswap(is_swapped, kj, &swpsum);
		}
	}
	gsl_vector_free(dons);
	gsl_vector_free(is_swapped);
    gsl_rng_free(r);
}

#ifdef TESTING
int main(){
	int N = 101;
	int rnk[N];
	double pct = .25;
	for (int i=0; i< N; i++)
		rnk[i] = i+1;
	int cutoffs[] = {30, 45, 50, 60, 70, 101};
	int lcut = sizeof(cutoffs)/sizeof(cutoffs[0]);
	int up = 3;
	rnkswpConstr(rnk, &N, &pct, cutoffs, &lcut, &up);

	for (int i=0; i< N; i++)
		printf("%i\t", rnk[i]);

	//assert that all elements of output ranks appear exactly once.
	gsl_vector *v = gsl_vector_calloc(N);
	for (int i=0; i< N; i++)
		v->data[rnk[i]-1]++;
	for (int i=0; i< N; i++)
		assert(v->data[i] == 1);
}
#endif 
