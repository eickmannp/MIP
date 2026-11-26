# ============================================================
# 03_footprints_TiVA.R
# Part A, steps 1-7b of the plan:
#   1. resource indicators and intensities (q), VA coefficients (v)
#      - official VA and residual VA, side by side
#      - Brazilian pasture: original and corrected (plan 5.1)
#   2. footprints and TiVA by origin x destination: F = q^ L Y
#   3. balances Mercosur <-> EU and VA/resource ratios (rule A)
#   4. resource and VA content of bilateral gross exports, by
#      exporting sector (q^ L e; input for the decomposition, step 8)
#   5. by Mercosur country (appendix)
#   6. save (small files, ~/work/results/)
# Run after 01 and 02 (needs labels, sectors, group_IO, group_FD, FD,
# VA, QT, x, A, L, e_ME, e_EM in memory).
# Money: thousand US$, 2019, basic prices. Physical units: as printed
# in block 1 (from labels$QT).
# Sign convention (Peters et al. 2011): balance > 0 = Mercosur is a net
# exporter of the resource to the EU.
# ============================================================


# ---- 0. Basics -------------------------------------------------
grp   <- c("EU", "Mercosur", "China", "USA", "Rest")
x_inv <- ifelse(x > 0, 1 / x, 0)
merc  <- which(group_IO == "Mercosur")
eu    <- which(group_IO == "EU")
pos   <- function(ctr, sec) which(labels$IO[, 1] == ctr & labels$IO[, 3] == sec)


# ---- 1. Indicators ---------------------------------------------
# QT rows used (plan 5.2). CHECK the printed names and units before
# reading any result: if a block is wrong, fix the row numbers here.
idx <- list(material   = 1:367,      # all material extraction (4 IRP groups: next step)
            land_agri  = 373:375,    # temporary crops, permanent crops, pasture
            ghg        = 3169,       # GHG total EDGAR, without land-use change
            employment = 368:369)    # employment by sex (sum), exploratory

labels$QT[c(1, 367, 368:369, 373:375, 3169), ]   # names and units: check
sapply(idx, function(i) paste(unique(labels$QT[i, ncol(labels$QT)]), collapse = " / "))  # one unit per block?

R <- rbind(material   = colSums(QT[idx$material, ]),
           land_agri  = colSums(QT[idx$land_agri, ]),
           ghg        = QT[idx$ghg, ],
           employment = colSums(QT[idx$employment, ]))

# -- 1b. Brazilian pasture: corrected version (plan 5.1) ---------
# In GLORIA 2019 Brazilian cattle sells almost nothing to the meat
# industry, so pasture never reaches exported beef. Correction in the
# satellite only (A and L untouched): pasture of BRA cattle is moved to
# BRA beef meat and dairy products, in proportion to their output (x).
sec_cattle <- grep("cattle", sectors, ignore.case = TRUE, value = TRUE)
sec_beef   <- grep("beef",   sectors, ignore.case = TRUE, value = TRUE)
sec_dairy  <- grep("dairy",  sectors, ignore.case = TRUE, value = TRUE)
print(c(cattle = sec_cattle, beef = sec_beef, dairy = sec_dairy))
stopifnot(length(sec_cattle) == 1, length(sec_beef) == 1, length(sec_dairy) == 1)

j_cat   <- pos("BRA", sec_cattle)
j_beef  <- pos("BRA", sec_beef)
j_dairy <- pos("BRA", sec_dairy)
row_pasture <- 375
moved       <- QT[row_pasture, j_cat]
share_beef  <- x[j_beef] / (x[j_beef] + x[j_dairy])

land_corr          <- R["land_agri", ]
land_corr[j_cat]   <- land_corr[j_cat]   - moved
land_corr[j_beef]  <- land_corr[j_beef]  + moved * share_beef
land_corr[j_dairy] <- land_corr[j_dairy] + moved * (1 - share_beef)
stopifnot(abs(sum(land_corr) - sum(R["land_agri", ])) <= 1e-9 * sum(R["land_agri", ]))

R <- rbind(R[c("material", "land_agri"), ],
           land_agri_corr = land_corr,
           R[c("ghg", "employment"), ])

c(pasture_moved = moved,
  share_of_BRA_pasture = moved / sum(QT[row_pasture, labels$IO[, 1] == "BRA"]),
  share_to_beef = share_beef)

# -- 1c. Intensities and VA coefficients ---------------------------
q <- sweep(R, 2, x_inv, "*")                    # resource per thousand US$ of output

V <- rbind(va_off = colSums(VA) * x_inv,        # official VA (matrix VA)
           va_res = ifelse(x > 0, 1 - colSums(A), 0))   # residual VA (x minus inputs)
stopifnot(min(V["va_res", ]) >= 0)

# How far apart are the two VAs? Mercosur sectors with the largest gap
gap <- (V["va_res", ] - V["va_off", ]) * x
o   <- order(abs(gap * (group_IO == "Mercosur")), decreasing = TRUE)[1:10]
data.frame(sector = paste(labels$IO[o, 1], "|", labels$IO[o, 3]),
           va_off = round(V["va_off", o] * x[o] / 1e3),       # million US$
           va_res = round(V["va_res", o] * x[o] / 1e3))


# ---- 2. Footprints and TiVA by origin x destination --------------
# Y: final demand of each destination group (6 categories summed)
S_FD <- sapply(grp, function(g) as.numeric(group_FD == g))   # 984 x 5
Y    <- FD %*% S_FD                                           # 19,680 x 5
LY   <- L %*% Y            # output of each country-sector pulled by each group's final demand

# F_r[i, g] = q_r[i] * LY[i, g]; then sum the rows by origin group
by_origin <- function(M) rowsum(M, group_IO)[grp, ]           # 5 x 5, origin x destination
F  <- lapply(rownames(q), function(r) by_origin(q[r, ] * LY)); names(F)  <- rownames(q)
TV <- lapply(rownames(V), function(r) by_origin(V[r, ] * LY)); names(TV) <- rownames(V)

round(F$land_agri)        # example: origin (rows) x destination (columns)
round(TV$va_off / 1e6)    # TiVA, billion US$

# -- Checks (plan 5.3) ------------------------------------------
# (a) footprints over all destinations = extraction of each origin
#     (approximate only because of the A adjustment)
chk_a <- sapply(rownames(R), function(r)
  max(abs(rowSums(F[[r]]) - rowsum(R[r, ], group_IO)[grp, ]) / rowsum(R[r, ], group_IO)[grp, ]))
round(chk_a, 4)
# (b) TiVA rows = VA of each origin (official VA)
round(rowSums(TV$va_off) / rowsum(colSums(VA), group_IO)[grp, ] - 1, 4)
# (c) TiVA columns = final demand of each group: exact with residual VA,
#     approximate with official VA
round(colSums(TV$va_res) / colSums(Y) - 1, 6)
round(colSums(TV$va_off) / colSums(Y) - 1, 4)


# ---- 3. Balances and VA/resource ratios (rule A) -----------------
# M->E: extracted in the Mercosur, pulled by EU final demand; E->M: inverse.
res_names <- rownames(q)
balances <- data.frame(
  indicator = res_names,
  unit      = c(sapply(idx[c("material", "land_agri")], function(i) labels$QT[i[1], ncol(labels$QT)]),
                labels$QT[idx$land_agri[1], ncol(labels$QT)],
                labels$QT[idx$ghg, ncol(labels$QT)],
                labels$QT[idx$employment[1], ncol(labels$QT)]),
  M_to_EU   = sapply(res_names, function(r) F[[r]]["Mercosur", "EU"]),
  EU_to_M   = sapply(res_names, function(r) F[[r]]["EU", "Mercosur"]))
balances$balance <- balances$M_to_EU - balances$EU_to_M      # > 0: Mercosur net exporter
balances$ratio   <- balances$M_to_EU / balances$EU_to_M      # like the ETT of Muradian et al.
balances

va_flows <- sapply(rownames(V), function(v)
  c(M_to_EU = TV[[v]]["Mercosur", "EU"], EU_to_M = TV[[v]]["EU", "Mercosur"])) / 1e6
va_flows                                                     # billion US$

# Ratios: thousand US$ of VA per unit of resource, each direction
ratios <- do.call(rbind, lapply(rownames(V), function(v)
  data.frame(va = v, indicator = res_names,
             M_to_EU = TV[[v]]["Mercosur", "EU"] / balances$M_to_EU,
             EU_to_M = TV[[v]]["EU", "Mercosur"] / balances$EU_to_M)))
ratios$EU_over_M <- ratios$EU_to_M / ratios$M_to_EU          # H2: expected well above 1
ratios


# ---- 4. Content of bilateral gross exports, by exporting sector ----
# q^ L e with exports as exogenous demand and the global L (Magacho,
# Espagne & Godin 2024). Rule A: resource and VA of the exporting bloc
# ("own"); what comes from other countries is reported apart ("third").
QV <- rbind(q, V)                                            # 7 x 19,680

content <- function(own, e) {
  m_own   <- QV[, own,  drop = FALSE] %*% L[own,  own]       # own-bloc multipliers
  m_third <- QV[, -own, drop = FALSE] %*% L[-own, own]       # third-country multipliers
  out <- data.frame(ctr = labels$IO[own, 1], sector = labels$IO[own, 3],
                    exports = e[own])
  for (r in rownames(QV)) {
    out[[paste0(r, "_own")]]   <- m_own[r, ]   * e[own]
    out[[paste0(r, "_third")]] <- m_third[r, ] * e[own]
  }
  out
}
exp_M <- content(merc, e_ME)          # Mercosur sectors exporting to the EU
exp_E <- content(eu,   e_EM)          # EU sectors exporting to the Mercosur
gc()

# Totals by direction: own and third
tot <- function(d) colSums(d[, -(1:3)])
round(rbind(M_to_EU = tot(exp_M), EU_to_M = tot(exp_E)))

# Share of third-country content in what each bloc sells to the other
third_share <- sapply(rownames(QV), function(r)
  c(M_to_EU = sum(exp_M[[paste0(r, "_third")]]) / sum(exp_M[[paste0(r, "_own")]] + exp_M[[paste0(r, "_third")]]),
    EU_to_M = sum(exp_E[[paste0(r, "_third")]]) / sum(exp_E[[paste0(r, "_own")]] + exp_E[[paste0(r, "_third")]])))
round(third_share, 3)

# Top 10 Mercosur exporting sectors by agricultural land (corrected), own content
head(exp_M[order(-exp_M$land_agri_corr_own),
           c("ctr", "sector", "exports", "land_agri_own", "land_agri_corr_own", "va_off_own")], 10)


# ---- 5. By Mercosur country (appendix) ---------------------------
by_ctr_M <- sapply(c(rownames(q), rownames(V)), function(r)
  rowsum((QV[r, ] * LY[, "EU"])[merc], labels$IO[merc, 1])[, 1])
round(by_ctr_M)                       # extracted in each country, pulled by EU final demand


# ---- 6. Save (small files) ---------------------------------------
# Outside the Git repository and never in the bucket.
save_results03 <- TRUE
out.path       <- "~/work/results/"
if (save_results03) {
  dir.create(out.path, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(idx = idx, F = F, TV = TV, balances = balances, va_flows = va_flows,
               ratios = ratios, exp_M = exp_M, exp_E = exp_E, third_share = third_share,
               by_ctr_M = by_ctr_M,
               pasture = c(moved = moved, share_beef = share_beef)),
          paste0(out.path, "03_results_2019.rds"))
  write.csv(balances, paste0(out.path, "03_balances_2019.csv"), row.names = FALSE)
  write.csv(ratios,   paste0(out.path, "03_ratios_2019.csv"),   row.names = FALSE)
  write.csv(exp_M,    paste0(out.path, "03_exports_content_M_to_EU_2019.csv"), row.names = FALSE)
  write.csv(exp_E,    paste0(out.path, "03_exports_content_EU_to_M_2019.csv"), row.names = FALSE)
}