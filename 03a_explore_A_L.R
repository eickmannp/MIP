# ============================================================
# 03a_explore_A_L.R
# Reading A and L with examples. Exploration only: nothing here
# feeds the results, and nothing is saved.
# Run after 01 and 02 (needs labels, sectors, group_IO, x, VA,
# FD, A, L in memory), or after reloading the saved results.
# Cheap: only columns of A and L and matrix-vector products.
# ============================================================


# ---- 0. Helpers ------------------------------------------------
# Position of a country-sector in the 19,680 rows/columns
pos <- function(ctr, sec) which(labels$IO[, 1] == ctr & labels$IO[, 3] == sec)

# Find sector names by a piece of text (prints all matches)
find_sec <- function(pattern) {
  hit <- sectors[grepl(pattern, sectors, ignore.case = TRUE)]
  print(hit)
  invisible(hit)
}

# Readable label of a position: "BRA | Iron ores"
lab <- function(i) paste(labels$IO[i, 1], "|", labels$IO[i, 3])

# Top n entries of a column vector, with labels and group
top_k <- function(v, n = 10, drop = NULL) {
  if (!is.null(drop)) v[drop] <- 0
  o <- order(v, decreasing = TRUE)[1:n]
  data.frame(origin = sapply(o, lab), group = group_IO[o],
             value = round(v[o], 4))
}

# Sum of a column vector by country group
by_group <- function(v) round(tapply(v, group_IO, sum), 4)


# ---- 1. Choose the example sectors -----------------------------
# Check the printed names and fix the patterns if needed.
sectors[1:10]                 # first sectors (agriculture)
find_sec("iron")              # iron ore
find_sec("cattle")            # cattle / livestock
find_sec("motor vehicle")     # cars
find_sec("chemical")          # chemicals

# Pick one name from each list above (edit the index [1] if needed)
sec_soy     <- sectors[4]                     # plan: soy is sector 4; check the name
sec_iron    <- find_sec("iron")[1]
sec_cattle  <- find_sec("cattle")[1]
sec_cars    <- find_sec("motor vehicle")[1]

j_soy   <- pos("BRA", sec_soy)
j_iron  <- pos("BRA", sec_iron)
j_cat   <- pos("BRA", sec_cattle)
j_car   <- pos("DEU", sec_cars)
ex      <- c(soy_BRA = j_soy, iron_BRA = j_iron, cattle_BRA = j_cat, cars_DEU = j_car)
sapply(ex, lab)               # confirm the four examples


# ============================================================
# PART A: the matrix A (direct input coefficients)
# ============================================================

# ---- A1. The "recipe" of a sector: its largest inputs -----------
# A[i, j] = dollars bought from i per dollar produced by j.
top_k(A[, j_car])             # German cars
top_k(A[, j_soy])             # Brazilian soy
top_k(A[, j_iron])            # Brazilian iron ore

# ---- A2. Column sum = input share; 1 - sum ~ value-added share ---
va_off <- ifelse(x > 0, colSums(VA) / x, 0)   # official VA per dollar of output
data.frame(sector    = sapply(ex, lab),
           inputs    = round(colSums(A[, ex]), 3),
           va_resid  = round(1 - colSums(A[, ex]), 3),
           va_offic  = round(va_off[ex], 3))

# ---- A3. Where the inputs come from (domestic vs imported) ------
sapply(ex, function(j) by_group(A[, j]))
# Domestic share of inputs: rows of the sector's own country
dom_share <- function(j) {
  own <- labels$IO[, 1] == labels$IO[j, 1]
  round(sum(A[own, j]) / sum(A[, j]), 3)
}
sapply(ex, dom_share)

# ---- A4. A overall: how sparse, how large ----------------------
mean(A[, j_car] == 0)                         # share of zero cells in one column
summary(colSums(A))                           # distribution of input shares
tapply(colSums(A), group_IO, median)          # median input share by group
# Brazil vs Germany: input share by sector, side by side
bra <- which(labels$IO[, 1] == "BRA")
deu <- which(labels$IO[, 1] == "DEU")
cmp <- data.frame(sector = sectors,
                  BRA = round(colSums(A[, bra]), 3),
                  DEU = round(colSums(A[, deu]), 3))
head(cmp[order(-cmp$BRA), ], 10)              # BRA sectors that buy most inputs


# ============================================================
# PART B: the matrix L (total requirements, direct + indirect)
# ============================================================

# ---- L1. Diagonal and output multiplier ------------------------
# L[i, j] = output of i needed for 1 dollar of final demand for j.
# Column sum = output multiplier (world); own-country rows = domestic.
dom_mult <- function(j) {
  own <- labels$IO[, 1] == labels$IO[j, 1]
  sum(L[own, j])
}
data.frame(sector     = sapply(ex, lab),
           diag       = round(diag(L)[ex], 3),
           mult_world = round(colSums(L[, ex]), 3),
           mult_dom   = round(sapply(ex, dom_mult), 3))

# ---- L2. Where the chain goes: production pulled by group -------
sapply(ex, function(j) by_group(L[, j]))
# Read: for 1 dollar of final demand for German cars, how many
# cents of output are pulled in the Mercosur, China, USA, Rest.

# ---- L3. Who is pulled by German cars (outside Germany) ---------
top_k(L[, j_car], n = 15, drop = deu)
L[j_iron, j_car]                              # Brazilian iron ore per $ of German cars
A[j_iron, j_car]                              # ...and the direct part (probably ~0)

# ---- L4. L = I + A + A^2 + ...: the rounds of the chain ---------
# Build the column of L for German cars round by round.
j <- j_car
v <- numeric(nrow(A)); v[j] <- 1               # round 0: the dollar delivered
acc <- v; rounds <- numeric(0)
for (r in 1:15) {
  v   <- as.vector(A %*% v)                   # next round of inputs
  acc <- acc + v
  rounds[r] <- sum(acc) / sum(L[, j])         # share of the full multiplier reached
}
round(rounds, 3)
# Same for the Mercosur part only: when does the iron ore show up?
# (repeat with acc restricted to group_IO == "Mercosur" if useful)

# ---- L5. Backward linkages (Rasmussen-Hirschman), Brazil --------
# Domestic block of L is not the domestic inverse; for a first look,
# use the Brazilian rows of the global L.
bl_bra <- colSums(L[bra, bra])
bl_bra <- bl_bra / mean(bl_bra)
bl <- data.frame(sector = sectors, BL = round(bl_bra, 2))
head(bl[order(-bl$BL), ], 10)                 # sectors that pull most of the Brazilian economy
tail(bl[order(-bl$BL), ], 10)


# ============================================================
# PART C: bridge to the work (preview of script 03)
# ============================================================

# ---- C1. Production pulled in the Mercosur by EU final demand ---
y_EU  <- rowSums(FD[, group_FD == "EU"])      # EU final demand, by producing country-sector
y_MER <- rowSums(FD[, group_FD == "Mercosur"])
x_EU  <- as.vector(L %*% y_EU)                 # world output needed for EU final demand
x_MER <- as.vector(L %*% y_MER)

by_group(x_EU)  / 1e6                          # bn US$ of output, by group, pulled by the EU
by_group(x_MER) / 1e6                          # same, pulled by the Mercosur

# ---- C2. Value added: a first TiVA ------------------------------
va_EU  <- va_off * x_EU                         # VA generated in each country-sector by EU demand
va_MER <- va_off * x_MER
c(Mercosur_VA_in_EU_demand = sum(va_EU[group_IO == "Mercosur"]),
  EU_VA_in_Mercosur_demand = sum(va_MER[group_IO == "EU"])) / 1e6   # bn US$

# Which Mercosur sectors earn most from EU final demand
top_k(va_EU * (group_IO == "Mercosur"), n = 15) # thousand US$

# ---- C3. One resource, to see the logic of the footprint --------
# Agricultural land (QT rows 373-375; check with labels$QT[373:375, ])
labels$QT[373:375, ]                          # check names and the unit (last column)
land  <- colSums(QT[373:375, ])
q_land <- ifelse(x > 0, land / x, 0)           # land per thousand US$ of output
f_EU  <- q_land * x_EU                          # land used in each country-sector for EU demand
f_MER <- q_land * x_MER
c(Mercosur_land_for_EU = sum(f_EU[group_IO == "Mercosur"]),
  EU_land_for_Mercosur = sum(f_MER[group_IO == "EU"]))              # in the QT unit
# Ratio of the work (H2), first version: VA per hectare, each direction
c(Mercosur_to_EU = sum(va_EU[group_IO == "Mercosur"]) / sum(f_EU[group_IO == "Mercosur"]),
  EU_to_Mercosur = sum(va_MER[group_IO == "EU"])     / sum(f_MER[group_IO == "EU"]))
# thousand US$ of VA per unit of land (QT unit)