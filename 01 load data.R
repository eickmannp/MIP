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

Sys.setenv(
  AWS_S3_ENDPOINT = "minio.lab.sspcloud.fr",
  AWS_ACCESS_KEY_ID = "YEEKA0EO0I9DJODHWBTD",
  AWS_SECRET_ACCESS_KEY = "LRT3KSCtbqlG0hetsfDutfLta2jHxeB9ZwZ1tBm5",
  AWS_SESSION_TOKEN = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJZRUVLQTBFTzBJOURKT0RIV0JURCIsImF1ZCI6Im1pbmlvLWRhdGFub2RlIiwiYXV0aF90aW1lIjoxNzk0ODY0OTExLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6InRhdXNjaGxAYWZkLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTc5NjE0NTY3MSwiZmFtaWx5X25hbWUiOiJUYXVzY2giLCJnaXZlbl9uYW1lIjoiTHVjYSIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSIsImVzdGVlbSJdLCJpYXQiOjE3OTU1NDA4NzAsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiJvbnJ0cnQ6Zjk3MTdjYWItZmM3Zi0xZmQxLTk1ZWYtYmZiNTdhNzhjNmNkIiwibG9jYWxlIjoiZnIiLCJuYW1lIjoiTHVjYSBUYXVzY2giLCJvbnl4aWEtdXNlcm5hbWUiOiJ0YXVzY2giLCJwb2xpY3kiOiJzdHNvbmx5LHN0c29ubHkyIiwicHJlZmVycmVkX3VzZXJuYW1lIjoidGF1c2NoIiwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBlbWFpbCByZXNvdXJjZTptaW5pbyIsInNpZCI6ImE2NEVaU2JvVEFmdjdEX1Rrc2d1c0JoeiIsInN1YiI6IjJhMDI0NGQwLWJlYTktNDBkOC1hNzczLWQ0ZDZlMjAwM2EwZSIsInR5cCI6IkJlYXJlciJ9.AiLK3UjguZ2ozW5fLb-MgSrNISCog31F3Ark30BBpzWU6MTjEZA5Px_TQl926ZDlizuNlCN9Iqd2yi9kNN4XsA"
)

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


# ---- 4. Gross exports by destination group ------------------------
# Needed for the composition by exporting sector and for Part B
# (q L e, exports as exogenous demand; Magacho, Espagne & Godin 2024).
# Computed here, from the original flows, because script 02 removes IO
# (and the adjusted A would change the flows of the adjusted columns).
# exp_dest[i, g]: sales of region-sector i to group g, intermediate + final.
grp  <- c("EU", "Mercosur", "China", "USA", "Rest")
S_IO <- sapply(grp, function(g) as.numeric(group_IO == g))   # 19,680 x 5
S_FD <- sapply(grp, function(g) as.numeric(group_FD == g))   #    984 x 5
exp_dest <- IO %*% S_IO + FD %*% S_FD                        # 19,680 x 5
stopifnot(max(abs(rowSums(exp_dest) - x_row) / pmax(x_row, 1)) < 1e-8)

# Bilateral gross exports by sector (the e of q L e)
e_ME <- exp_dest[, "EU"]       * (group_IO == "Mercosur")   # Mercosur -> EU
e_EM <- exp_dest[, "Mercosur"] * (group_IO == "EU")         # EU -> Mercosur
c(Mercosur_to_EU = sum(e_ME), EU_to_Mercosur = sum(e_EM)) / 1e6   # billion US$