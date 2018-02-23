
#define STRICT_R_HEADERS

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Print.h>

#include "gensvm_debug.h"
#include "gensvm_print.h"
#include "gensvm_train.h"
#include "gensvm_predict.h"

// forward declarations

SEXP R_gensvm_train( SEXP R_X, SEXP R_y, SEXP R_p, SEXP R_lambda, 
		SEXP R_kappa, SEXP R_epsilon, SEXP R_weight_idx, 
		SEXP R_kernel_idx, SEXP R_gamma, SEXP R_coef, SEXP R_degree, 
		SEXP R_kernel_eigen_cutoff, SEXP R_verbose, SEXP R_max_iter, 
		SEXP R_random_seed, SEXP R_seed_V, SEXP R_n, SEXP R_m, 
		SEXP R_K);
SEXP R_gensvm_predict(SEXP R_Xtest, SEXP R_V, SEXP R_n, SEXP R_m, SEXP R_K);


void _set_verbosity(int verbosity_flag);
void _set_seed_model(struct GenModel *model, double *V, long n, long m, 
		long K);

// Start R package stuff

R_CallMethodDef callMethods[] = {
	{"R_gensvm_train", (DL_FUNC) &R_gensvm_train, 19},
	{"R_gensvm_predict", (DL_FUNC) &R_gensvm_predict, 5},
	{NULL, NULL, 0}
};
R_CMethodDef cMethods[] = {
	{NULL, NULL, 0}
};

void R_init_gensvm_wrapper(DllInfo *info) {
	R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
	R_useDynamicSymbols(info, TRUE);
}

// End R package stuff

void _set_verbosity(int verbosity_flag)
{
	extern FILE *GENSVM_OUTPUT_FILE;
	extern FILE *GENSVM_ERROR_FILE;

	if (verbosity_flag) {
		gensvm_print_out = Rprintf;
		gensvm_print_err = REprintf;
	}
	else {
		GENSVM_OUTPUT_FILE = NULL;
		GENSVM_ERROR_FILE = NULL;
	}
}

void _set_seed_model(struct GenModel *model, double *V, long n, long m, long K)
{
	long i, j;
	double value;

	model->n = 0;
	model->m = m;
	model->K = K;

	gensvm_allocate_model(model);

	for (i=0; i<m+1; i++) {
		for (j=0; j<K-1; j++) {
			value = matrix_get(V, m+1, K-1, i, j);
			matrix_set(model->V, m+1, K-1, i, j, value);
		}
	}
}


// NOTE: Let's supply X here as it is represented in R: a matrix in 
// Column-Major order. Since we have to augment the matrix X with the column 
// of ones to form Z, we might as well do that *and* convert to RowMajor in a 
// single step. Otherwise we have the RowMajor version of X as well as Z in 
// memory, which is unnecessary.
SEXP R_gensvm_train(
		SEXP R_X,
		SEXP R_y,
		SEXP R_p,
		SEXP R_lambda,
		SEXP R_kappa,
		SEXP R_epsilon,
		SEXP R_weight_idx,
		SEXP R_kernel_idx,
		SEXP R_gamma,
		SEXP R_coef,
		SEXP R_degree,
		SEXP R_kernel_eigen_cutoff,
		SEXP R_verbose,
		SEXP R_max_iter,
		SEXP R_random_seed,
		SEXP R_seed_V,
		SEXP R_n,
		SEXP R_m,
		SEXP R_K
		)
{
	double *X = REAL(R_X);
	int *y = INTEGER(R_y); // R doesn't know long?
	double p = *REAL(R_p);
	double lambda = *REAL(R_lambda);
	double kappa = *REAL(R_kappa);
	double epsilon = *REAL(R_epsilon);
	int weight_idx = *INTEGER(R_weight_idx);
	int kernel_idx = *INTEGER(R_kernel_idx);
	double gamma = *REAL(R_gamma);
	double coef = *REAL(R_coef);
	double degree = *REAL(R_degree);
	double kernel_eigen_cutoff = *REAL(R_kernel_eigen_cutoff);
	int verbose = *INTEGER(R_verbose);
	int max_iter = *INTEGER(R_max_iter);
	int random_seed = *INTEGER(R_random_seed);
	double *seed_V = isNull(R_seed_V) ? NULL : REAL(R_seed_V);
	int n = *INTEGER(R_n);
	int m = *INTEGER(R_m);
	int K = *INTEGER(R_K);

	_set_verbosity(verbose);

	struct GenModel *model = gensvm_init_model();
	struct GenModel *seed_model = NULL;
	struct GenData *data = NULL;
	long i, j;
	double value;

	// Set model parameters from function input arguments
	model->p = p;
	model->lambda = lambda;
	model->kappa = kappa;
	model->epsilon = epsilon;
	model->weight_idx = weight_idx;
	model->kerneltype = kernel_idx;
	model->gamma = gamma;
	model->coef = coef;
	model->degree = degree;
	model->kernel_eigen_cutoff = kernel_eigen_cutoff;
	model->max_iter = max_iter;
	model->seed = random_seed;

	if (seed_V != NULL) {
		seed_model = gensvm_init_model();
		_set_seed_model(seed_model, seed_V, n, m, K);
	}

	data = gensvm_init_data();

	data->y = Malloc(long, n);
	for (i=0; i<n; i++)
		data->y[i] = (long) y[i];

	data->RAW = Malloc(double, n*(m+1));
	for (i=0; i<n; i++) {
		for (j=0; j<m; j++) {
			value = matrix_get(X, n, m, i, j);
			matrix_set(data->RAW, n, m+1, i, j+1, value);
		}
		// column of 1's
		matrix_set(data->RAW, n, m+1, i, 0, 1.0);
	}

	data->n = n;
	data->m = m;
	data->r = m;
	data->K = K;
	data->Z = data->RAW;

	// convert to sparse matrix if possible
	if (gensvm_could_sparse(data->Z, n, m+1)) {
		note("Converting to sparse ... ");
		data->spZ = gensvm_dense_to_sparse(data->Z, n, m+1);
		note("done.\n");
		free(data->RAW);
		data->RAW = NULL;
		data->Z = NULL;
	}

	// actually do the training
	gensvm_train(model, data, seed_model);

	// create the output list
	SEXP output = PROTECT(allocVector(VECSXP, 3));

	// create and fill output matrix
	SEXP R_V = PROTECT(allocMatrix(REALSXP, m+1, K-1));
	double *rR_V = REAL(R_V);
	for (i=0; i<m+1; i++) {
		for (j=0; j<K-1; j++) {
			value = matrix_get(model->V, m+1, K-1, i, j);
			matrix_set(rR_V, m+1, K-1, i, j, value);
		}
	}

	SEXP R_iter = PROTECT(allocVector(INTSXP, 1));
	int *r_iter = INTEGER(R_iter);
	r_iter[0] = model->elapsed_iter;

	SEXP R_sv = PROTECT(allocVector(INTSXP, 1));
	int *r_sv = INTEGER(R_sv);
	r_sv[0] = gensvm_num_sv(model);

	// set output list elements
	SET_VECTOR_ELT(output, 0, R_V);
	SET_VECTOR_ELT(output, 1, R_iter);
	SET_VECTOR_ELT(output, 2, R_sv);

	// create names
	SEXP names = PROTECT(allocVector(STRSXP, 3));
	SET_STRING_ELT(names, 0, mkChar("V"));
	SET_STRING_ELT(names, 1, mkChar("n.iter"));
	SET_STRING_ELT(names, 2, mkChar("n.support"));

	// assign names to list
	setAttrib(output, R_NamesSymbol, names);

	// cleanup
	UNPROTECT(5);

	gensvm_free_model(model);
	gensvm_free_model(seed_model);
	gensvm_free_data(data);

	return output;
}

SEXP R_gensvm_predict(
		SEXP R_Xtest,
		SEXP R_V,
		SEXP R_n,
		SEXP R_m,
		SEXP R_K
		)
{
	double *X = REAL(R_Xtest);
	double *V = REAL(R_V);
	int n_test = *INTEGER(R_n);
	int m = *INTEGER(R_m);
	int K = *INTEGER(R_K);

	int i, j;
	double value;

	struct GenModel *model = gensvm_init_model();
	model->m = m;
	model->K = K;
	model->U = Calloc(double, K*(K-1));
	model->V = Calloc(double, (m+1) * (K-1));
	for (i=0; i<m+1; i++) {
		for (j=0; j<K-1; j++) {
			value = matrix_get(V, m+1, K-1, i, j);
			matrix_set(model->V, m+1, K-1, i, j, value);
		}
	}

	struct GenData *data = gensvm_init_data();
	data->n = n_test;
	data->m = m;
	data->r = m;
	data->K = K;

	data->RAW = Calloc(double, n_test*(m+1));

	for (i=0; i<n_test; i++) {
		for (j=0; j<m; j++) {
			value = matrix_get(X, n_test, m, i, j);
			matrix_set(data->RAW, n_test, m+1, i, j+1, value);
		}
		matrix_set(data->RAW, n_test, m+1, i, 0, 1.0);
	}
	data->Z = data->RAW;

	// convert to sparse matrix if possible
	if (gensvm_could_sparse(data->Z, n_test, m+1)) {
		note("Converting to sparse ... ");
		data->spZ = gensvm_dense_to_sparse(data->Z, n_test, m+1);
		note("done.\n");
		free(data->RAW);
		data->RAW = NULL;
		data->Z = NULL;
	}

	long *pred_temp = Calloc(long, n_test);

	gensvm_predict_labels(data, model, pred_temp);

	SEXP R_y = PROTECT(allocMatrix(INTSXP, n_test, 1));
	int *rR_y = INTEGER(R_y);
	for (i=0; i<n_test; i++)
		rR_y[i] = pred_temp[i];

	gensvm_free_data(data);
	gensvm_free_model(model);
	free(pred_temp);

	UNPROTECT(1);

	return(R_y);
}
