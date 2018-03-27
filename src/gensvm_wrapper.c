/**
 * @file gensvm_wrapper.c
 * @author G.J.J. van den Burg
 * @date 2018-03-26
 * @brief Wrapper code for the GenSVM R package

 * Copyright (C) G.J.J. van den Burg

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

 */

#define STRICT_R_HEADERS

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Print.h>

#include "gensvm_print.h"
#include "gensvm_train.h"
#include "gensvm_predict.h"
#include "gensvm_gridsearch.h"

// forward declarations

SEXP R_gensvm_train( SEXP R_X, SEXP R_y, SEXP R_p, SEXP R_lambda, 
		SEXP R_kappa, SEXP R_epsilon, SEXP R_weight_idx, 
		SEXP R_kernel_idx, SEXP R_gamma, SEXP R_coef, SEXP R_degree, 
		SEXP R_kernel_eigen_cutoff, SEXP R_verbose, SEXP R_max_iter, 
		SEXP R_random_seed, SEXP R_seed_V, SEXP R_seed_rows, 
		SEXP R_seed_cols, SEXP R_n, SEXP R_m, SEXP R_K);
SEXP R_gensvm_predict(SEXP R_Xtest, SEXP R_V, SEXP R_n, SEXP R_m, SEXP R_K);
SEXP R_gensvm_predict_kernels(
		SEXP R_Xtest, SEXP R_Xtrain, SEXP R_V, SEXP R_V_row,
		SEXP R_V_col, SEXP R_n_train, SEXP R_n_test, SEXP R_m,
		SEXP R_K, SEXP R_kernel_idx, SEXP R_gamma, SEXP R_coef,
		SEXP R_degree, SEXP R_kernel_eigen_cutoff);
SEXP R_gensvm_plotdata_kernels(
		SEXP R_Xtest, SEXP R_Xtrain, SEXP R_V, SEXP R_V_row,
		SEXP R_V_col, SEXP R_n_train, SEXP R_n_test, SEXP R_m,
		SEXP R_K, SEXP R_kernel_idx, SEXP R_gamma, SEXP R_coef,
		SEXP R_degree, SEXP R_kernel_eigen_cutoff);

SEXP R_gensvm_grid(SEXP R_X, SEXP R_y, SEXP R_df, SEXP R_df_rows, 
		SEXP R_df_cols, SEXP R_cv_idx, SEXP R_cv_folds, SEXP R_verbosity, 
		SEXP R_n, SEXP R_m, SEXP R_K);

void _set_verbosity(int verbosity);
struct GenData *_build_gensvm_data(double *X, int *y, int n, int m, int K);

// Start R package stuff

R_CallMethodDef callMethods[] = {
	{"R_gensvm_train", (DL_FUNC) &R_gensvm_train, 21},
	{"R_gensvm_predict", (DL_FUNC) &R_gensvm_predict, 5},
	{"R_gensvm_predict_kernels", (DL_FUNC) &R_gensvm_predict_kernels, 14},
	{"R_gensvm_plotdata_kernels", (DL_FUNC) &R_gensvm_plotdata_kernels, 14},
	{"R_gensvm_grid", (DL_FUNC) &R_gensvm_grid, 11},
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

/**
 * @brief Set the verbosity of the GenSVM library
 *
 * @description
 * This sets the printing functions of the GenSVM library to print to the R 
 * console if desired.
 *
 * @param[in] 	verbosity 	if 0 all output is surpressed
 *
 */
void _set_verbosity(int verbosity)
{
	extern FILE *GENSVM_OUTPUT_FILE;
	extern FILE *GENSVM_ERROR_FILE;

	if (verbosity) {
		gensvm_print_out = Rprintf;
		gensvm_print_err = REprintf;
	}
	else {
		gensvm_print_out = gensvm_print_output_fpt;
		gensvm_print_err = gensvm_print_error_fpt;
		GENSVM_OUTPUT_FILE = NULL;
		GENSVM_ERROR_FILE = NULL;
	}
}


/**
 * @brief Construct a GenData struct from the given dataset
 *
 * @param[in] 	X
 * @param[in] 	y can be NULL
 * @param[in] 	n
 * @param[in] 	m
 * @param[in] 	K
 *
 * @return GenData structure
 */
struct GenData *_build_gensvm_data(double *X, int *y, int n, int m, int K)
{
	int i, j;
	double value;

	struct GenData *data = gensvm_init_data();
	data->n = n;
	data->m = m;
	data->r = m;
	data->K = K;

	data->RAW = Calloc(double, n*(m+1));

	for (i=0; i<n; i++) {
		for (j=0; j<m; j++) {
			value = matrix_get(X, n, m, i, j);
			matrix_set(data->RAW, n, m+1, i, j+1, value);
		}
		matrix_set(data->RAW, n, m+1, i, 0, 1.0);
	}
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

	if (y == NULL) {
		data->y = NULL;
	} else {
		data->y = Malloc(long, n);
		for (i=0; i<n; i++)
			data->y[i] = y[i];
	}

	return data;
}

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
		SEXP R_seed_rows,
		SEXP R_seed_cols,
		SEXP R_n,
		SEXP R_m,
		SEXP R_K
		)
{
	double *X = REAL(R_X);
	int *y = INTEGER(R_y);
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
	int seed_rows = *INTEGER(R_seed_rows);
	int seed_cols = *INTEGER(R_seed_cols);
	int n = *INTEGER(R_n);
	int m = *INTEGER(R_m);
	int K = *INTEGER(R_K);

	_set_verbosity(verbose);

	struct GenModel *model = gensvm_init_model();
	struct GenModel *seed_model = NULL;
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

		seed_model->n = 0;
		seed_model->m = seed_rows - 1;
		seed_model->K = seed_cols + 1;
		gensvm_allocate_model(seed_model);

		for (i=0; i<seed_model->m+1; i++) {
			for (j=0; j<seed_model->K-1; j++) {
				matrix_set(seed_model->V, seed_model->m+1,
						seed_model->K-1, i ,j,
						matrix_get(seed_V, seed_rows,
							seed_cols, i, j));
			}
		}
	}

	struct GenData *data = _build_gensvm_data(X, y, n, m, K);

	// actually do the training
	gensvm_train(model, data, seed_model);

	// create the output list
	SEXP output = PROTECT(allocVector(VECSXP, 4));

	// create and fill output matrix
	SEXP R_V = PROTECT(allocMatrix(REALSXP, model->m+1, model->K-1));
	double *rR_V = REAL(R_V);
	for (i=0; i<model->m+1; i++) {
		for (j=0; j<model->K-1; j++) {
			value = matrix_get(model->V, model->m+1, model->K-1, 
					i, j);
			matrix_set(rR_V, model->m+1, model->K-1, i, j, value);
		}
	}

	SEXP R_iter = PROTECT(allocVector(INTSXP, 1));
	int *r_iter = INTEGER(R_iter);
	r_iter[0] = model->elapsed_iter;

	SEXP R_sv = PROTECT(allocVector(INTSXP, 1));
	int *r_sv = INTEGER(R_sv);
	r_sv[0] = gensvm_num_sv(model);

	SEXP R_time = PROTECT(allocVector(REALSXP, 1));
	double *r_time = REAL(R_time);
	r_time[0] = model->elapsed_time;

	// set output list elements
	SET_VECTOR_ELT(output, 0, R_V);
	SET_VECTOR_ELT(output, 1, R_iter);
	SET_VECTOR_ELT(output, 2, R_sv);
	SET_VECTOR_ELT(output, 3, R_time);

	// create names
	SEXP names = PROTECT(allocVector(STRSXP, 4));
	SET_STRING_ELT(names, 0, mkChar("V"));
	SET_STRING_ELT(names, 1, mkChar("n.iter"));
	SET_STRING_ELT(names, 2, mkChar("n.support"));
	SET_STRING_ELT(names, 3, mkChar("training.time"));

	// assign names to list
	setAttrib(output, R_NamesSymbol, names);

	// cleanup
	UNPROTECT(6);

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

	struct GenData *data = _build_gensvm_data(X, NULL, n_test, m, K);

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

SEXP R_gensvm_predict_kernels(
		SEXP R_Xtest,
		SEXP R_Xtrain,
		SEXP R_V,
		SEXP R_V_row,
		SEXP R_V_col,
		SEXP R_n_train,
		SEXP R_n_test,
		SEXP R_m,
		SEXP R_K,
		SEXP R_kernel_idx,
		SEXP R_gamma,
		SEXP R_coef,
		SEXP R_degree,
		SEXP R_kernel_eigen_cutoff
		)
{
	double *X_test = REAL(R_Xtest);
	double *X_train = REAL(R_Xtrain);
	double *V = REAL(R_V);
	int V_row = *INTEGER(R_V_row);
	int V_col = *INTEGER(R_V_col);
	int n_train = *INTEGER(R_n_train);
	int n_test = *INTEGER(R_n_test);
	int m = *INTEGER(R_m);
	int K = *INTEGER(R_K);

	int kernel_idx = *INTEGER(R_kernel_idx);
	double gamma = *REAL(R_gamma);
	double coef = *REAL(R_coef);
	double degree = *REAL(R_degree);
	double kernel_eigen_cutoff = *REAL(R_kernel_eigen_cutoff);

	int i, j;
	double value;

	struct GenModel *model = gensvm_init_model();
	model->n = n_train;
	model->m = V_row - 1;
	model->K = V_col + 1;
	model->kerneltype = kernel_idx;
	model->gamma = gamma;
	model->coef = coef;
	model->degree = degree;
	model->kernel_eigen_cutoff = kernel_eigen_cutoff;
	gensvm_allocate_model(model);

	struct GenData *traindata = _build_gensvm_data(X_train, NULL, n_train, 
			m, K);
	struct GenData *testdata = _build_gensvm_data(X_test, NULL, n_test, 
			m, K);

	gensvm_kernel_preprocess(model, traindata);
	gensvm_reallocate_model(model, traindata->n, traindata->r);

	for (i=0; i<model->m+1; i++) {
		for (j=0; j<model->K-1; j++) {
			value = matrix_get(V, V_row, V_col, i, j);
			matrix_set(model->V, model->m+1, model->K-1, i, j, 
					value);
		}
	}

	gensvm_kernel_postprocess(model, traindata, testdata);

	long *pred_temp = Calloc(long, n_test);
	gensvm_predict_labels(testdata, model, pred_temp);

	SEXP R_y = PROTECT(allocMatrix(INTSXP, n_test, 1));
	int *rR_y = INTEGER(R_y);
	for (i=0; i<n_test; i++)
		rR_y[i] = pred_temp[i];

	gensvm_free_data(traindata);
	gensvm_free_data(testdata);
	gensvm_free_model(model);
	free(pred_temp);

	UNPROTECT(1);

	return(R_y);
}

SEXP R_gensvm_plotdata_kernels(
		SEXP R_Xtest,
		SEXP R_Xtrain,
		SEXP R_V,
		SEXP R_V_row,
		SEXP R_V_col,
		SEXP R_n_train,
		SEXP R_n_test,
		SEXP R_m,
		SEXP R_K,
		SEXP R_kernel_idx,
		SEXP R_gamma,
		SEXP R_coef,
		SEXP R_degree,
		SEXP R_kernel_eigen_cutoff
		)
{
	double *X_test = REAL(R_Xtest);
	double *X_train = REAL(R_Xtrain);
	double *V = REAL(R_V);
	int V_row = *INTEGER(R_V_row);
	int V_col = *INTEGER(R_V_col);
	int n_train = *INTEGER(R_n_train);
	int n_test = *INTEGER(R_n_test);
	int m = *INTEGER(R_m);
	int K = *INTEGER(R_K);

	int kernel_idx = *INTEGER(R_kernel_idx);
	double gamma = *REAL(R_gamma);
	double coef = *REAL(R_coef);
	double degree = *REAL(R_degree);
	double kernel_eigen_cutoff = *REAL(R_kernel_eigen_cutoff);

	int i, j;
	double value;

	struct GenModel *model = gensvm_init_model();
	model->n = n_train;
	model->m = V_row - 1;
	model->K = V_col + 1;
	model->kerneltype = kernel_idx;
	model->gamma = gamma;
	model->coef = coef;
	model->degree = degree;
	model->kernel_eigen_cutoff = kernel_eigen_cutoff;
	gensvm_allocate_model(model);

	struct GenData *traindata = _build_gensvm_data(X_train, NULL, n_train, 
			m, K);
	struct GenData *testdata = _build_gensvm_data(X_test, NULL, n_test, 
			m, K);

	gensvm_kernel_preprocess(model, traindata);
	gensvm_reallocate_model(model, traindata->n, traindata->r);

	for (i=0; i<model->m+1; i++) {
		for (j=0; j<model->K-1; j++) {
			value = matrix_get(V, V_row, V_col, i, j);
			matrix_set(model->V, model->m+1, model->K-1, i, j, 
					value);
		}
	}

	gensvm_kernel_postprocess(model, traindata, testdata);

	double *ZV = Calloc(long, n_test * (K-1));
	gensvm_calculate_ZV(model, testdata, ZV);

	long *pred_temp = Calloc(long, n_test);
	gensvm_predict_labels(testdata, model, pred_temp);

	// create the output list
	SEXP output = PROTECT(allocVector(VECSXP, 2));

	// Copy predictions
	SEXP R_y = PROTECT(allocMatrix(INTSXP, n_test, 1));
	int *rR_y = INTEGER(R_y);
	for (i=0; i<n_test; i++)
		rR_y[i] = pred_temp[i];

	// Copy ZV
	SEXP R_ZV = PROTECT(allocMatrix(REALSXP, n_test, K-1));
	double *rR_ZV = REAL(R_ZV);
	for (i=0; i<n_test*(K-1); i++)
		rR_ZV[i] = ZV[i];

	SET_VECTOR_ELT(output, 0, R_y);
	SET_VECTOR_ELT(output, 1, R_ZV);

	SEXP names = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(names, 0, mkChar("y.pred"));
	SET_STRING_ELT(names, 1, mkChar("ZV"));

	setAttrib(output, R_NamesSymbol, names);

	UNPROTECT(4);

	gensvm_free_data(traindata);
	gensvm_free_data(testdata);
	gensvm_free_model(model);
	free(pred_temp);
	free(ZV);

	return output;
}

SEXP R_gensvm_grid(
		SEXP R_X,
		SEXP R_y,
		SEXP R_df,
		SEXP R_df_rows,
		SEXP R_df_cols,
		SEXP R_cv_idx,
		SEXP R_cv_folds,
		SEXP R_verbosity,
		SEXP R_n,
		SEXP R_m,
		SEXP R_K
		)
{
	double *X = REAL(R_X);
	int *y = INTEGER(R_y);
	double *df = REAL(R_df);
	int df_rows = *INTEGER(R_df_rows);
	int df_cols = *INTEGER(R_df_cols);
	int *icv_idx = INTEGER(R_cv_idx);
	int folds = *INTEGER(R_cv_folds);
	int verbosity = *INTEGER(R_verbosity);
	int n = *INTEGER(R_n);
	int m = *INTEGER(R_m);
	int K = *INTEGER(R_K);

	int i, j, pred;
	long *cv_idx = NULL;
	double val, total_time;

	// Check input
	if (df_cols < 9) {
		// TODO: Raise error to R
	}

	// set verbosity
	_set_verbosity(verbosity);

	// copy the cv_idx array
	cv_idx = Malloc(long, n);
	for (i=0; i<n; i++)
		cv_idx[i] = icv_idx[i];

	// Read the data into a GenData struct
	struct GenData *data = _build_gensvm_data(X, y, n, m, K);

	// Initialize and populate the queue
	struct GenQueue *q = gensvm_init_queue();
	q->tasks = Malloc(struct GenTask *, df_rows);
	q->N = df_rows;

	struct GenTask *t = NULL;

	for (i=0; i<df_rows; i++) {
		t = gensvm_init_task();
		t->ID = i;

		t->kerneltype = matrix_get(df, df_rows, df_cols, i, 0);
		t->coef = matrix_get(df, df_rows, df_cols, i, 1);
		t->degree = matrix_get(df, df_rows, df_cols, i, 2);
		t->gamma = matrix_get(df, df_rows, df_cols, i, 3);
		t->weight_idx = matrix_get(df, df_rows, df_cols, i, 4);
		t->kappa = matrix_get(df, df_rows, df_cols, i, 5);
		t->lambda = matrix_get(df, df_rows, df_cols, i, 6);
		t->p = matrix_get(df, df_rows, df_cols, i, 7);
		t->epsilon = matrix_get(df, df_rows, df_cols, i, 8);
		t->max_iter = matrix_get(df, df_rows, df_cols, i, 9);
		t->folds = folds;

		t->train_data = data;

		q->tasks[i] = t;
	}

	// start training
	total_time = gensvm_train_queue(q, cv_idx, true, verbosity);

	// create the output list
	SEXP output = PROTECT(allocVector(VECSXP, 3));

	// copy predictions
	SEXP R_predictions = PROTECT(allocMatrix(INTSXP, df_rows, n));
	int *rR_predictions = INTEGER(R_predictions);
	for (i=0; i<df_rows; i++) {
		t = q->tasks[i];
		if (t->predictions == NULL) { // if interrupt occurred
			for (j=0; j<n; j++)
				matrix_set(rR_predictions, df_rows, n, i, j,
						NA_INTEGER);
		} else {
			for (j=0; j<n; j++) {
				pred = t->predictions[j];
				pred = (pred == -1) ? NA_INTEGER : pred;
				matrix_set(rR_predictions, df_rows, n, i, j, 
						pred);
			}
		}
	}

	// copy durations
	SEXP R_durations = PROTECT(allocMatrix(REALSXP, df_rows, folds));
	double *rR_durations = REAL(R_durations);
	for (i=0; i<df_rows; i++) {
		t = q->tasks[i];
		if (t->durations == NULL) { // if interrupt occurred
			for (j=0; j<folds; j++) {
				matrix_set(rR_durations, df_rows, folds, i, j,
						NA_REAL);
			}
		} else {
			for (j=0; j<folds; j++) {
				val = t->durations[j];
				val = (val == -1) ? NA_REAL : val;
				matrix_set(rR_durations, df_rows, folds, i, j,
						val);
			}
		}
	}

	SEXP R_time = PROTECT(allocVector(REALSXP, 1));
	double *r_time = REAL(R_time);
	r_time[0] = total_time;

	// set output list elements
	SET_VECTOR_ELT(output, 0, R_predictions);
	SET_VECTOR_ELT(output, 1, R_durations);
	SET_VECTOR_ELT(output, 2, R_time);

	// create names
	SEXP names = PROTECT(allocVector(STRSXP, 3));
	SET_STRING_ELT(names, 0, mkChar("predictions"));
	SET_STRING_ELT(names, 1, mkChar("durations"));
	SET_STRING_ELT(names, 2, mkChar("total.time"));

	// assign names to list
	setAttrib(output, R_NamesSymbol, names);

	UNPROTECT(5);

	gensvm_free_data(data);
	gensvm_free_queue(q);

	free(cv_idx);

	return output;
}
