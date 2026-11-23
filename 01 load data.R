# ============================================================
# 01_load_data.R
# EEMRIO EU–Mercosur — GLORIA 059, year 2019
# Loads matrices and labels, checks dimensions and balance,
# and defines the country groups.
# Monetary unit: thousand current US$, basic prices.
# ============================================================


# ---- 0. Packages and paths -------------------------------------
# install.packages(c("aws.s3", "data.table", "readxl"))   # once
library(aws.s3)
library(data.table)
library(readxl)

# Credentials: already set inside the SSP Cloud RStudio service.
# Outside of it: Sys.setenv(AWS_S3_ENDPOINT = , AWS_ACCESS_KEY_ID = ,
#                           AWS_SECRET_ACCESS_KEY = , AWS_SESSION_TOKEN = )

bucket      <- "projet-esteem"
io.path     <- "Gloria/matrices/059/global/2019/"
labels.path <- "Gloria/matrices/059/labels/"


# ---- 1. Reading ------------------------------------------------
# All files in the 059 folder are CSV despite the ".rds" extension (checked 23/11).
read_s3 <- function(path) {
  s3read_using(FUN = data.table::fread, encoding = "UTF-8",
               object = path, bucket = bucket, opts = list("region" = ""))
}

IO <- as.matrix(read_s3(paste0(io.path, "IO_2019.rds")))   # intermediate use
FD <- as.matrix(read_s3(paste0(io.path, "FD_2019.rds")))   # final demand
VA <- as.matrix(read_s3(paste0(io.path, "VA_2019.rds")))   # value added
QT <- as.matrix(read_s3(paste0(io.path, "QT_2019.rds")))   # satellite accounts
X  <- as.vector(as.matrix(read_s3(paste0(io.path, "x_2019.rds"))))  # official output (check only)

labels <- list()
for (l in c("IO", "FD", "VA", "QT")) {
  labels[[l]] <- as.data.frame(read_s3(paste0(labels.path, "label_", l, ".rds")))
}
regions <- s3read_using(FUN = read_excel, sheet = "Regions",
                        object = paste0(labels.path, "GLORIA_ReadMe_059a.xlsx"),
                        bucket = bucket, opts = list("region" = ""))

sectors <- unique(labels$IO[, 3])   # 120 sectors
all.ctr <- unique(labels$IO[, 1])   # 164 regions, in matrix order
n <- length(all.ctr) * length(sectors)


# ---- 2. Checks -------------------------------------------------
stopifnot(
  identical(all.ctr, regions$Region_acronyms),   # same order as the official ReadMe
  length(sectors) == 120,
  length(all.ctr) == 164,
  dim(IO) == c(n, n),
  dim(FD) == c(n, 164 * 6),
  dim(VA) == c(6, n),
  dim(QT) == c(nrow(labels$QT), n),
  length(X) == n
)

# Balance: total output from the use side (row sums, x_row) and from the
# input side (column sums, x_col)
x_row <- rowSums(IO) + rowSums(FD)
x_col <- colSums(IO) + colSums(VA)

# x_row must match the official output (rounding differences only)
stopifnot(quantile(abs(x_row - X) / pmax(X, 1), 0.99) < 1e-3)

# Input side: known imbalance (18/11: 0.03% / 2.9% / 50%)
quantile(abs(x_row - x_col) / pmax(x_row, 1), c(0.5, 0.9, 0.99))


# ---- 3. Country groups -----------------------------------------
mercosur <- c("ARG", "BRA", "PRY", "URY")
eu27 <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST",
          "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX",
          "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")

groups <- list(EU = eu27, Mercosur = mercosur, China = "CHN", USA = "USA")
stopifnot(all(unlist(groups) %in% all.ctr), !anyDuplicated(unlist(groups)))

group_of <- function(ctr) {
  g <- rep("Rest", length(ctr))
  for (k in names(groups)) g[ctr %in% groups[[k]]] <- k
  g
}
group_IO <- group_of(labels$IO[, 1])   # rows and columns of IO, A, L; columns of QT and VA
group_FD <- group_of(labels$FD[, 1])   # columns of FD

table(group_IO) / 120   # China 1, EU 27, Mercosur 4, Rest 131, USA 1