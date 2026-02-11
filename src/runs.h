#ifndef RUNS_H
#define RUNS_H

#include <stddef.h>

typedef enum {
    RUN_TYPE_ACC = -1,
    RUN_TYPE_NEU = 0,
    RUN_TYPE_DEC = 1
} RunType;

typedef struct {
    int end_idx;
    int length;
    int type;
} RunAddress;

typedef struct {
    int *dec_counts;
    int *acc_counts;
    int *neu_counts;
    size_t counts_capacity;
    RunAddress *runs_addresses;
    size_t runs_addresses_len;
    size_t runs_addresses_cap;
} RunsAccumulator;

typedef struct {
    int (*rows)[3];
    size_t len;
} RunsSummary;

typedef struct {
    double *rr_intervals;
    double mean_rr;
    size_t rr_length;
    int *annotations;
    int write_last_run;
    RunsAccumulator accumulator;
    double *runs_variances_dec;
    size_t runs_variances_dec_len;
    double *runs_variances_acc;
    size_t runs_variances_acc_len;
    double *runs_variances_neu;
    size_t runs_variances_neu_len;
    int analyzed;
    size_t max_dec;
    size_t max_acc;
    size_t max_neu;
} RRRuns;

int rr_runs_init(RRRuns *runs, const double *rr, const int *annot, size_t len, int write_last_run);
void rr_runs_free(RRRuns *runs);

size_t rr_runs_get_nonzero_length(const int *counts, size_t counts_capacity);
void rr_runs_set_max(RRRuns *runs);
const RunsAccumulator *rr_runs_get_full_runs(RRRuns *runs);
int rr_runs_get_runs_summary(RRRuns *runs, RunsSummary *out_summary);
void runs_summary_free(RunsSummary *summary);

void rr_runs_print_runs(RRRuns *runs);
void rr_runs_print_addresses(RRRuns *runs, RunType run_type, int run_length, int reference_beat);
void rr_runs_print_runs_addresses(const RRRuns *runs);
void rr_runs_print_runs_accumulator(const RRRuns *runs);
void rr_runs_calculate_runs_variances(RRRuns *runs);
void rr_runs_print_runs_variances(const RRRuns *runs);

#endif
