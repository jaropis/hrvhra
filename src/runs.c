#include "runs.h"

#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void update_runs_addresses(RRRuns *runs, int end_idx, int length, int type) {
    RunAddress *next = NULL;
    size_t next_cap;
    if (!runs) {
        return;
    }
    if (runs->accumulator.runs_addresses_len == runs->accumulator.runs_addresses_cap) {
        next_cap = runs->accumulator.runs_addresses_cap == 0 ? 16 : runs->accumulator.runs_addresses_cap * 2;
        next = (RunAddress *)realloc(
            runs->accumulator.runs_addresses,
            next_cap * sizeof(*runs->accumulator.runs_addresses));
        if (!next) {
            return;
        }
        runs->accumulator.runs_addresses = next;
        runs->accumulator.runs_addresses_cap = next_cap;
    }
    runs->accumulator.runs_addresses[runs->accumulator.runs_addresses_len].end_idx = end_idx;
    runs->accumulator.runs_addresses[runs->accumulator.runs_addresses_len].length = length;
    runs->accumulator.runs_addresses[runs->accumulator.runs_addresses_len].type = type;
    runs->accumulator.runs_addresses_len++;
}

static int *run_type_counts(RRRuns *runs, RunType run_type) {
    if (run_type == RUN_TYPE_DEC) {
        return runs->accumulator.dec_counts;
    }
    if (run_type == RUN_TYPE_ACC) {
        return runs->accumulator.acc_counts;
    }
    return runs->accumulator.neu_counts;
}

static void register_run(RRRuns *runs, RunType run_type, int length, int end_idx) {
    int *counts = NULL;
    if (!runs || length <= 0) {
        return;
    }
    counts = run_type_counts(runs, run_type);
    if ((size_t)length < runs->accumulator.counts_capacity) {
        counts[length] += 1;
    }
    update_runs_addresses(runs, end_idx, length, (int)run_type);
}

static void analyze_runs(RRRuns *runs) {
    bool flag_dec = false;
    bool flag_acc = false;
    bool flag_neu = false;
    int index_dec = 0;
    int index_acc = 0;
    int index_neu = 0;
    size_t running_rr_number = 0;

    if (!runs || runs->rr_length < 2) {
        rr_runs_set_max(runs);
        if (runs) {
            runs->analyzed = 1;
        }
        return;
    }

    while (running_rr_number + 1 < runs->rr_length &&
           (runs->annotations[running_rr_number] != 0 ||
            runs->annotations[running_rr_number + 1] != 0)) {
        if (running_rr_number == runs->rr_length - 1) {
            rr_runs_set_max(runs);
            runs->analyzed = 1;
            return;
        }
        running_rr_number++;
    }

    if (running_rr_number + 1 >= runs->rr_length) {
        rr_runs_set_max(runs);
        runs->analyzed = 1;
        return;
    }

    if (runs->rr_intervals[running_rr_number] < runs->rr_intervals[running_rr_number + 1]) {
        flag_dec = true;
        index_dec += 1;
    }
    if (runs->rr_intervals[running_rr_number] > runs->rr_intervals[running_rr_number + 1]) {
        flag_acc = true;
        index_acc += 1;
    }
    if (runs->rr_intervals[running_rr_number] == runs->rr_intervals[running_rr_number + 1]) {
        flag_neu = true;
        index_neu += 1;
    }
    running_rr_number += 1;

    while (running_rr_number < (runs->rr_length - 1)) {
        if (runs->annotations[running_rr_number + 1] != 0) {
            if (flag_dec) {
                register_run(runs, RUN_TYPE_DEC, index_dec, (int)running_rr_number);
            }
            if (flag_acc) {
                register_run(runs, RUN_TYPE_ACC, index_acc, (int)running_rr_number);
            }
            if (flag_neu) {
                register_run(runs, RUN_TYPE_NEU, index_neu, (int)running_rr_number);
            }

            index_dec = 0;
            index_acc = 0;
            index_neu = 0;
            flag_acc = false;
            flag_dec = false;
            flag_neu = false;

            while (running_rr_number + 1 < runs->rr_length &&
                   (runs->annotations[running_rr_number] != 0 ||
                    runs->annotations[running_rr_number + 1] != 0)) {
                running_rr_number += 1;
                if (running_rr_number >= runs->rr_length - 1) {
                    rr_runs_set_max(runs);
                    runs->analyzed = 1;
                    return;
                }
            }

            if (running_rr_number < runs->rr_length - 1) {
                if (runs->rr_intervals[running_rr_number] < runs->rr_intervals[running_rr_number + 1] &&
                    runs->annotations[running_rr_number + 1] == 0) {
                    flag_dec = true;
                    index_dec += 1;
                }
                if (runs->rr_intervals[running_rr_number] > runs->rr_intervals[running_rr_number + 1] &&
                    runs->annotations[running_rr_number + 1] == 0) {
                    flag_acc = true;
                    index_acc += 1;
                }
                if (runs->rr_intervals[running_rr_number] == runs->rr_intervals[running_rr_number + 1] &&
                    runs->annotations[running_rr_number + 1] == 0) {
                    flag_neu = true;
                    index_neu += 1;
                }
            }
            running_rr_number += 1;
            continue;
        }

        if (running_rr_number >= runs->rr_length - 1) {
            break;
        }

        if (runs->annotations[running_rr_number] == 0 &&
            runs->annotations[running_rr_number + 1] == 0) {
            if (runs->rr_intervals[running_rr_number + 1] >
                runs->rr_intervals[running_rr_number]) {
                index_dec += 1;
                if (!flag_dec) {
                    if (flag_acc) {
                        register_run(runs, RUN_TYPE_ACC, index_acc, (int)running_rr_number);
                        index_acc = 0;
                        flag_acc = false;
                    } else if (flag_neu) {
                        register_run(runs, RUN_TYPE_NEU, index_neu, (int)running_rr_number);
                        index_neu = 0;
                        flag_neu = false;
                    }
                    flag_dec = true;
                }
            } else if (runs->rr_intervals[running_rr_number + 1] <
                       runs->rr_intervals[running_rr_number]) {
                index_acc += 1;
                if (!flag_acc) {
                    if (flag_dec) {
                        register_run(runs, RUN_TYPE_DEC, index_dec, (int)running_rr_number);
                        index_dec = 0;
                        flag_dec = false;
                    } else if (flag_neu) {
                        register_run(runs, RUN_TYPE_NEU, index_neu, (int)running_rr_number);
                        index_neu = 0;
                        flag_neu = false;
                    }
                    flag_acc = true;
                }
            } else {
                index_neu += 1;
                if (!flag_neu) {
                    if (flag_dec) {
                        register_run(runs, RUN_TYPE_DEC, index_dec, (int)running_rr_number);
                        index_dec = 0;
                        flag_dec = false;
                    } else if (flag_acc) {
                        register_run(runs, RUN_TYPE_ACC, index_acc, (int)running_rr_number);
                        index_acc = 0;
                        flag_acc = false;
                    }
                    flag_neu = true;
                }
            }
        }

        running_rr_number += 1;
    }

    if (runs->write_last_run) {
        if (index_acc > 0) {
            register_run(runs, RUN_TYPE_ACC, index_acc, (int)running_rr_number);
        }
        if (index_dec > 0) {
            register_run(runs, RUN_TYPE_DEC, index_dec, (int)running_rr_number);
        }
        if (index_neu > 0) {
            register_run(runs, RUN_TYPE_NEU, index_neu, (int)running_rr_number);
        }
    } else {
        printf("the last run not needed\n");
    }

    rr_runs_set_max(runs);
    runs->analyzed = 1;
}

int rr_runs_init(RRRuns *runs, const double *rr, const int *annot, size_t len, int write_last_run) {
    size_t i;
    double mean_rr = 0.0;

    if (!runs || !rr || !annot) {
        return EINVAL;
    }

    memset(runs, 0, sizeof(*runs));

    runs->rr_intervals = (double *)malloc(len * sizeof(*runs->rr_intervals));
    runs->annotations = (int *)malloc(len * sizeof(*runs->annotations));
    runs->accumulator.dec_counts = (int *)calloc(len + 1, sizeof(*runs->accumulator.dec_counts));
    runs->accumulator.acc_counts = (int *)calloc(len + 1, sizeof(*runs->accumulator.acc_counts));
    runs->accumulator.neu_counts = (int *)calloc(len + 1, sizeof(*runs->accumulator.neu_counts));
    if (!runs->rr_intervals || !runs->annotations || !runs->accumulator.dec_counts ||
        !runs->accumulator.acc_counts || !runs->accumulator.neu_counts) {
        rr_runs_free(runs);
        return ENOMEM;
    }

    memcpy(runs->rr_intervals, rr, len * sizeof(*runs->rr_intervals));
    memcpy(runs->annotations, annot, len * sizeof(*runs->annotations));
    for (i = 0; i < len; i++) {
        mean_rr += rr[i];
    }

    runs->mean_rr = len > 0 ? mean_rr / (double)len : NAN;
    runs->rr_length = len;
    runs->write_last_run = write_last_run;
    runs->accumulator.counts_capacity = len + 1;
    runs->analyzed = 0;
    runs->max_acc = 0;
    runs->max_dec = 0;
    runs->max_neu = 0;
    return 0;
}

void rr_runs_free(RRRuns *runs) {
    if (!runs) {
        return;
    }
    free(runs->rr_intervals);
    free(runs->annotations);
    free(runs->accumulator.dec_counts);
    free(runs->accumulator.acc_counts);
    free(runs->accumulator.neu_counts);
    free(runs->accumulator.runs_addresses);
    free(runs->runs_variances_dec);
    free(runs->runs_variances_acc);
    free(runs->runs_variances_neu);
    memset(runs, 0, sizeof(*runs));
}

size_t rr_runs_get_nonzero_length(const int *counts, size_t counts_capacity) {
    size_t i;
    size_t max = 0;
    if (!counts) {
        return 0;
    }
    for (i = 1; i < counts_capacity; i++) {
        if (counts[i] != 0) {
            max = i;
        }
    }
    return max;
}

void rr_runs_set_max(RRRuns *runs) {
    if (!runs) {
        return;
    }
    runs->max_dec = rr_runs_get_nonzero_length(
        runs->accumulator.dec_counts, runs->accumulator.counts_capacity);
    runs->max_acc = rr_runs_get_nonzero_length(
        runs->accumulator.acc_counts, runs->accumulator.counts_capacity);
    runs->max_neu = rr_runs_get_nonzero_length(
        runs->accumulator.neu_counts, runs->accumulator.counts_capacity);
}

const RunsAccumulator *rr_runs_get_full_runs(RRRuns *runs) {
    if (!runs) {
        return NULL;
    }
    if (!runs->analyzed) {
        analyze_runs(runs);
    }
    return &runs->accumulator;
}

int rr_runs_get_runs_summary(RRRuns *runs, RunsSummary *out_summary) {
    size_t i;
    size_t max_length;
    if (!runs || !out_summary) {
        return EINVAL;
    }

    out_summary->rows = NULL;
    out_summary->len = 0;

    if (!runs->analyzed) {
        analyze_runs(runs);
    }

    max_length = runs->max_acc;
    if (runs->max_dec > max_length) {
        max_length = runs->max_dec;
    }
    if (runs->max_neu > max_length) {
        max_length = runs->max_neu;
    }

    if (max_length == 0) {
        out_summary->rows = malloc(sizeof(*out_summary->rows));
        if (!out_summary->rows) {
            return ENOMEM;
        }
        out_summary->rows[0][0] = 0;
        out_summary->rows[0][1] = 0;
        out_summary->rows[0][2] = 0;
        out_summary->len = 1;
        return 0;
    }

    out_summary->rows = (int(*)[3])calloc(max_length, sizeof(*out_summary->rows));
    if (!out_summary->rows) {
        return ENOMEM;
    }
    out_summary->len = max_length;

    for (i = 1; i <= max_length; i++) {
        out_summary->rows[i - 1][0] = i <= runs->max_acc ? runs->accumulator.acc_counts[i] : 0;
        out_summary->rows[i - 1][1] = i <= runs->max_dec ? runs->accumulator.dec_counts[i] : 0;
        out_summary->rows[i - 1][2] = i <= runs->max_neu ? runs->accumulator.neu_counts[i] : 0;
    }

    return 0;
}

void runs_summary_free(RunsSummary *summary) {
    if (!summary) {
        return;
    }
    free(summary->rows);
    summary->rows = NULL;
    summary->len = 0;
}

void rr_runs_print_runs(RRRuns *runs) {
    size_t max_length;
    size_t i;
    if (!runs) {
        return;
    }
    if (!runs->analyzed) {
        analyze_runs(runs);
    }
    max_length = runs->max_acc;
    if (runs->max_dec > max_length) {
        max_length = runs->max_dec;
    }
    if (runs->max_neu > max_length) {
        max_length = runs->max_neu;
    }
    printf("i  Ar - DR - N\n");
    for (i = 1; i < max_length; i++) {
        printf(
            "%zu %d - %d - %d\n",
            i,
            i < runs->max_acc ? runs->accumulator.acc_counts[i] : 0,
            i < runs->max_dec ? runs->accumulator.dec_counts[i] : 0,
            i < runs->max_neu ? runs->accumulator.neu_counts[i] : 0);
    }
}

void rr_runs_print_addresses(RRRuns *runs, RunType run_type, int run_length, int reference_beat) {
    size_t i;
    int reference_offset = reference_beat ? 1 : 0;

    if (!runs) {
        return;
    }

    printf(
        "run type: %s run length: %d\n",
        run_type == RUN_TYPE_DEC ? "DEC" : run_type == RUN_TYPE_ACC ? "ACC" : "NEU",
        run_length);

    if (!runs->analyzed) {
        analyze_runs(runs);
    }

    for (i = 0; i < runs->accumulator.runs_addresses_len; i++) {
        const RunAddress run = runs->accumulator.runs_addresses[i];
        if (run.type == (int)run_type && run.length == run_length) {
            size_t end_idx = (size_t)(run.end_idx + (reference_beat ? 1 : 0));
            size_t length = (size_t)(run.length + reference_offset);
            if (length <= end_idx + 1 && end_idx < runs->rr_length) {
                size_t start_idx = end_idx - length;
                size_t idx;
                printf("start_idx: %zu, end_idx: %zu\n", start_idx, end_idx);
                for (idx = start_idx; idx <= end_idx && idx < runs->rr_length; idx++) {
                    printf("%g ", runs->rr_intervals[idx]);
                }
                printf("\n");
            }
        }
    }
}

void rr_runs_print_runs_addresses(const RRRuns *runs) {
    size_t i;
    if (!runs) {
        return;
    }
    for (i = 0; i < runs->accumulator.runs_addresses_len; i++) {
        const RunAddress *run = &runs->accumulator.runs_addresses[i];
        printf("[%d, %d, %d]\n", run->end_idx, run->length, run->type);
    }
}

static void print_counts(const char *name, const int *counts, size_t cap) {
    size_t i;
    printf("%s: {", name);
    for (i = 1; i < cap; i++) {
        if (counts[i] != 0) {
            printf("%zu:%d ", i, counts[i]);
        }
    }
    printf("}\n");
}

void rr_runs_print_runs_accumulator(const RRRuns *runs) {
    if (!runs) {
        return;
    }
    print_counts("dec", runs->accumulator.dec_counts, runs->accumulator.counts_capacity);
    print_counts("acc", runs->accumulator.acc_counts, runs->accumulator.counts_capacity);
    print_counts("neu", runs->accumulator.neu_counts, runs->accumulator.counts_capacity);
}

void rr_runs_calculate_runs_variances(RRRuns *runs) {
    size_t i;
    if (!runs) {
        return;
    }
    if (!runs->analyzed) {
        analyze_runs(runs);
    }

    for (i = 0; i < runs->accumulator.runs_addresses_len; i++) {
        const RunAddress run = runs->accumulator.runs_addresses[i];
        int rr_index = run.end_idx;
        int length = run.length;
        int run_type = run.type;
        size_t max_len;
        double **run_var = NULL;
        size_t *run_var_len = NULL;
        double local_run_variance = 0.0;
        int j;

        if (run_type == (int)RUN_TYPE_DEC) {
            max_len = runs->max_dec;
            run_var = &runs->runs_variances_dec;
            run_var_len = &runs->runs_variances_dec_len;
        } else if (run_type == (int)RUN_TYPE_ACC) {
            max_len = runs->max_acc;
            run_var = &runs->runs_variances_acc;
            run_var_len = &runs->runs_variances_acc_len;
        } else {
            max_len = runs->max_neu;
            run_var = &runs->runs_variances_neu;
            run_var_len = &runs->runs_variances_neu_len;
        }

        if (!*run_var && max_len > 0) {
            *run_var = (double *)calloc(max_len, sizeof(**run_var));
            if (!*run_var) {
                return;
            }
            *run_var_len = max_len;
        }

        for (j = rr_index - length; j < rr_index; j++) {
            if (j + 1 >= 0 && (size_t)(j + 1) < runs->rr_length) {
                local_run_variance += runs->rr_intervals[j + 1];
            }
        }

        if (*run_var && length > 0 && (size_t)(length - 1) < *run_var_len) {
            (*run_var)[length - 1] = (*run_var)[length - 1] + local_run_variance;
        }
    }
}

static void print_variance_array(const char *name, const double *arr, size_t len) {
    size_t i;
    printf("%s: [", name);
    for (i = 0; i < len; i++) {
        printf("%g", arr ? arr[i] : 0.0);
        if (i + 1 < len) {
            printf(", ");
        }
    }
    printf("]\n");
}

void rr_runs_print_runs_variances(const RRRuns *runs) {
    if (!runs) {
        return;
    }
    print_variance_array("dec", runs->runs_variances_dec, runs->runs_variances_dec_len);
    print_variance_array("acc", runs->runs_variances_acc, runs->runs_variances_acc_len);
    print_variance_array("neu", runs->runs_variances_neu, runs->runs_variances_neu_len);
}
