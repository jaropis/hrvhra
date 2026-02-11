#include "samp_en.h"

#include <math.h>
#include <stddef.h>
#include <stdlib.h>

static double *ncm_correlation_sums(const double *signal, size_t signal_len, size_t m, double r) {
    size_t m_val;
    const size_t tau = 1;
    double *corsum_matrix = NULL;

    if (!signal || m == 0) {
        return NULL;
    }

    corsum_matrix = (double *)calloc(m, sizeof(*corsum_matrix));
    if (!corsum_matrix) {
        return NULL;
    }

    for (m_val = 0; m_val < m; m_val++) {
        double count = 0.0;
        size_t i;
        size_t factor_a;
        size_t factor_b;

        if (signal_len <= (m_val * tau + 1U)) {
            corsum_matrix[m_val] = 0.0;
            continue;
        }

        for (i = 0; i < (signal_len - m_val * tau - 1U); i++) {
            size_t j;
            for (j = i + 1U; j < (signal_len - m_val * tau); j++) {
                double max_diff = 0.0;
                size_t k;
                for (k = 0; k <= m_val; k++) {
                    double diff = fabs(signal[i + k * tau] - signal[j + k * tau]);
                    if (diff > max_diff) {
                        max_diff = diff;
                    }
                }
                if (max_diff <= r) {
                    count += 1.0;
                }
            }
        }

        factor_a = signal_len - m_val * tau;
        factor_b = signal_len - 1U - m_val * tau;
        corsum_matrix[m_val] = count * 2.0 / ((double)factor_a * (double)factor_b);
    }

    return corsum_matrix;
}

double calc_samp_en(const double *signal, size_t signal_len, size_t m, double r) {
    double *cm = ncm_correlation_sums(signal, signal_len, m, r);
    double sampen = NAN;
    if (!cm || m < 2) {
        free(cm);
        return NAN;
    }
    sampen = log(cm[0]) - log(cm[1]);
    free(cm);
    return sampen;
}
