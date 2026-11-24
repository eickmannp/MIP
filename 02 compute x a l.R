# ============================================================
# 02_compute_x_A_L.R
# Output (x), technical coefficients (A), adjustment of the
# problematic columns of A, and Leontief inverse (L).
# Run after 01. Peak memory: ~16 GB.
# ============================================================


# ---- 1. Output -------------------------------------------------
x <- x_row                                        # use side (row sums)
stopifnot(all(labels$IO[x <= 0, 1] == "DYE"))     # x = 0 only for DYE (extinct region)
x_inv <- ifelse(x > 0, 1 / x, 0)


# ---- 2. Technical coefficients: a_ij = z_ij / x_j ----------------
A <- sweep(IO, 2, x_inv, "*")
rm(IO); gc()
stopifnot(min(A) >= 0)


# ---- 3. Adjustment of columns summing to >= 1 (provisional) ------
# Same logic as Luca's cell.value.adjustment: the whole column is rescaled
# to the mean of the n.ref largest columns of the same sector (in other
# countries) that are below colsum.max.
colsum.max <- 1
n.ref      <- 10

colsum   <- colSums(A)
to_adj   <- which(colsum >= colsum.max)
adj_log  <- data.frame(ctr    = labels$IO[to_adj, 1],
                       sector = labels$IO[to_adj, 3],
                       group  = group_IO[to_adj],
                       before = colsum[to_adj],
                       after  = NA)

for (k in seq_along(to_adj)) {
  j      <- to_adj[k]
  ref    <- colsum[labels$IO[, 3] == labels$IO[j, 3] & colsum < colsum.max]
  target <- mean(head(sort(ref, decreasing = TRUE), n.ref))
  A[, j] <- A[, j] * target / colsum[j]
  adj_log$after[k] <- target
}

stopifnot(max(colSums(A)) < 1)
table(adj_log$group)                                    # 18/11: 238 columns
adj_log[adj_log$group %in% c("Mercosur", "EU"), ]


# ---- 4. Leontief inverse: L = (I - A)^-1 -------------------------
L <- -A
diag(L) <- diag(L) + 1
L <- solve(L)
gc()

stopifnot(min(L) > -1e-8, min(diag(L)) > 1 - 1e-8)

# Does L reproduce output from final demand? Exact except for sectors
# linked to the adjusted columns; this shows how large the deviation is.
x_L <- as.vector(L %*% rowSums(FD))
quantile(abs(x_L - x) / pmax(x, 1), c(0.5, 0.9, 0.99))


# ---- 5. Save results (optional) --------------------------------
# Saved in the persistent folder of the RStudio service (~/work), OUTSIDE the
# Git repository (~/work/MIP) and not in the shared bucket. A and L take
# ~1–3 GB each on disk; check free space with system("df -h ~/work") first.
save_results <- FALSE
out.path     <- "~/work/results/"

if (save_results) {
  dir.create(out.path, recursive = TRUE, showWarnings = FALSE)
  saveRDS(x,       paste0(out.path, "x_2019.rds"))
  saveRDS(adj_log, paste0(out.path, "adj_log_2019.rds"))
  saveRDS(A,       paste0(out.path, "A_adj_2019.rds"))
  saveRDS(L,       paste0(out.path, "L_2019.rds"))
  saveRDS(exp_dest, paste0(out.path, "exp_dest_2019.rds"))   # from script 01
  saveRDS(x_col,    paste0(out.path, "x_col_2019.rds"))      # input side, for robustness
}