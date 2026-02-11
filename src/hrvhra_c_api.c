#include <errno.h>
#include <stddef.h>
#include <string.h>

#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include "runs.h"
#include "samp_en.h"

static size_t checked_length(R_xlen_t n, const char *arg_name) {
    if (n < 0) {
        Rf_error("%s has invalid length", arg_name);
    }
    return (size_t)n;
}

static int checked_logical(SEXP x, const char *arg_name) {
    int value = Rf_asLogical(x);
    if (value == NA_LOGICAL) {
        Rf_error("%s must be TRUE or FALSE", arg_name);
    }
    return value;
}

static size_t checked_nonnegative_int(SEXP x, const char *arg_name) {
    int value = Rf_asInteger(x);
    if (value == NA_INTEGER || value < 0) {
        Rf_error("%s must be a non-negative integer", arg_name);
    }
    return (size_t)value;
}

SEXP wrap__hello_world(void) {
    return Rf_mkString("Hello world!");
}

static SEXP runs_summary_to_r(const RunsSummary *summary) {
    SEXP out = R_NilValue;
    SEXP names = R_NilValue;
    SEXP data = R_NilValue;
    SEXP rows = R_NilValue;
    SEXP cols = R_NilValue;
    R_xlen_t i;
    PROTECT(data = Rf_allocVector(INTSXP, (R_xlen_t)(summary->len * 3U)));
    for (i = 0; i < (R_xlen_t)summary->len; i++) {
        INTEGER(data)[i * 3 + 0] = summary->rows[i][0];
        INTEGER(data)[i * 3 + 1] = summary->rows[i][1];
        INTEGER(data)[i * 3 + 2] = summary->rows[i][2];
    }

    PROTECT(rows = Rf_ScalarInteger((int)summary->len));
    PROTECT(cols = Rf_ScalarInteger(3));

    PROTECT(out = Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(out, 0, data);
    SET_VECTOR_ELT(out, 1, rows);
    SET_VECTOR_ELT(out, 2, cols);

    PROTECT(names = Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, Rf_mkChar("data"));
    SET_STRING_ELT(names, 1, Rf_mkChar("rows"));
    SET_STRING_ELT(names, 2, Rf_mkChar("cols"));
    Rf_setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(5);
    return out;
}

static SEXP wrap_runs_common(SEXP rr, SEXP annotations, SEXP write_last_run) {
    SEXP rr_real = R_NilValue;
    SEXP annotations_int = R_NilValue;
    size_t n = 0;
    RRRuns runs;
    RunsSummary summary;
    int err;

    memset(&runs, 0, sizeof(runs));
    memset(&summary, 0, sizeof(summary));

    PROTECT(rr_real = Rf_coerceVector(rr, REALSXP));
    PROTECT(annotations_int = Rf_coerceVector(annotations, INTSXP));

    n = checked_length(XLENGTH(rr_real), "rr");
    if (n != checked_length(XLENGTH(annotations_int), "annotations")) {
        UNPROTECT(2);
        Rf_error("rr and annotations must have the same length");
    }

    err = rr_runs_init(
        &runs,
        REAL(rr_real),
        INTEGER(annotations_int),
        n,
        checked_logical(write_last_run, "write_last_run"));
    if (err != 0) {
        rr_runs_free(&runs);
        UNPROTECT(2);
        Rf_error("rr_runs_init failed: %s", strerror(err));
    }

    err = rr_runs_get_runs_summary(&runs, &summary);
    if (err != 0) {
        rr_runs_free(&runs);
        UNPROTECT(2);
        Rf_error("rr_runs_get_runs_summary failed: %s", strerror(err));
    }

    rr_runs_free(&runs);
    UNPROTECT(2);

    {
        SEXP out = runs_summary_to_r(&summary);
        runs_summary_free(&summary);
        return out;
    }
}

SEXP wrap__analyze_rr_runs(SEXP rr, SEXP annotations, SEXP write_last_run) {
    return wrap_runs_common(rr, annotations, write_last_run);
}

SEXP wrap__get_runs_summary(SEXP rr, SEXP annotations, SEXP write_last_run) {
    return wrap_runs_common(rr, annotations, write_last_run);
}

SEXP wrap__samp_en(SEXP signal, SEXP m, SEXP r) {
    SEXP signal_real = R_NilValue;
    size_t n = 0;
    size_t m_value = 0;
    double r_value = 0.0;
    double result;

    PROTECT(signal_real = Rf_coerceVector(signal, REALSXP));
    n = checked_length(XLENGTH(signal_real), "signal");
    m_value = checked_nonnegative_int(m, "m");
    r_value = Rf_asReal(r);

    result = calc_samp_en(REAL(signal_real), n, m_value, r_value);

    UNPROTECT(1);
    return Rf_ScalarReal(result);
}
