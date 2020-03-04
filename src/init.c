#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP opf_accuracy(int, char **);
extern SEXP opf_accuracy4label(int, char **);
extern SEXP opf_check(int, char **);
extern SEXP opf_classify(int, char **);
extern SEXP opf_cluster(int, char **);
extern SEXP opf_distance(int, char **);
extern SEXP opf_fold(int, char **);
extern SEXP opf_info(int, char **);
extern SEXP opf_learn(int, char **);
extern SEXP opf_merge(int, char **);
extern SEXP opf_normalize(int, char **);
extern SEXP opf_pruning(int, char **);
extern SEXP opf_semi(int, char **);
extern SEXP opf_split(int, char **);
extern SEXP opf2svm(int, char **);
extern SEXP opf2txt(int, char **);
extern SEXP opfknn_classify(int, char **);
extern SEXP opfknn_train(int, char **);
extern SEXP statistics(int, char **);
extern SEXP svm2opf(int, char **);
extern SEXP txt2opf(int, char **);

static const R_CallMethodDef CallEntries[] = {
    {"opf_accuracy",       (DL_FUNC) &opf_accuracy,       2},
    {"opf_accuracy4label", (DL_FUNC) &opf_accuracy4label, 2},
    {"opf_check",          (DL_FUNC) &opf_check,          2},
    {"opf_classify",       (DL_FUNC) &opf_classify,       2},
    {"opf_cluster",        (DL_FUNC) &opf_cluster,        2},
    {"opf_distance",       (DL_FUNC) &opf_distance,       2},
    {"opf_fold",           (DL_FUNC) &opf_fold,           2},
    {"opf_info",           (DL_FUNC) &opf_info,           2},
    {"opf_learn",          (DL_FUNC) &opf_learn,          2},
    {"opf_merge",          (DL_FUNC) &opf_merge,          2},
    {"opf_normalize",      (DL_FUNC) &opf_normalize,      2},
    {"opf_pruning",        (DL_FUNC) &opf_pruning,        2},
    {"opf_semi",           (DL_FUNC) &opf_semi,           2},
    {"opf_split",          (DL_FUNC) &opf_split,          2},
    {"opf2svm",            (DL_FUNC) &opf2svm,            2},
    {"opf2txt",            (DL_FUNC) &opf2txt,            2},
    {"opfknn_classify",    (DL_FUNC) &opfknn_classify,    2},
    {"opfknn_train",       (DL_FUNC) &opfknn_train,       2},
    {"statistics",         (DL_FUNC) &statistics,         2},
    {"svm2opf",            (DL_FUNC) &svm2opf,            2},
    {"txt2opf",            (DL_FUNC) &txt2opf,            2},
    {NULL, NULL, 0}
};

void R_init_LibOPF(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}