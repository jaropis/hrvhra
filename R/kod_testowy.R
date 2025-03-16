# rr_intervals <- c(800, 810, 790, 785, 795, 805, 815, 810, 800, 795)
# annotations <- rep(0L, length(rr_intervals))  # Using integer zeros
# 
# # Get runs summary
# result <- get_runs_summary(rr_intervals, annotations, TRUE)
# 
# # Convert to matrix
# matrix_data <- matrix(result$data,
#                       nrow = result$rows,
#                       ncol = result$cols,
#                       byrow = TRUE)
# colnames(matrix_data) <- c("Acceleration", "Deceleration", "Neutral")
# print(matrix_data)
# 
# # Get full analysis
# analysis <- analyze_rr_runs(rr_intervals, annotations, TRUE)