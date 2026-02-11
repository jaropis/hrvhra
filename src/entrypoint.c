#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP wrap__hello_world(void);
SEXP wrap__analyze_rr_runs(SEXP rr, SEXP annotations, SEXP write_last_run);
SEXP wrap__get_runs_summary(SEXP rr, SEXP annotations, SEXP write_last_run);
SEXP wrap__samp_en(SEXP signal, SEXP m, SEXP r);

static const R_CallMethodDef CallEntries[] = {
    {"wrap__hello_world", (DL_FUNC)&wrap__hello_world, 0},
    {"wrap__analyze_rr_runs", (DL_FUNC)&wrap__analyze_rr_runs, 3},
    {"wrap__get_runs_summary", (DL_FUNC)&wrap__get_runs_summary, 3},
    {"wrap__samp_en", (DL_FUNC)&wrap__samp_en, 3},
    {NULL, NULL, 0}
};

void R_init_hrvhra(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
