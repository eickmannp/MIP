# ============================================================
# 0. Pacotes
# ============================================================
library(aws.s3)
library(data.table)

# ============================================================
# 1. Função de leitura (tenta .rds; se não der, lê como CSV)
# ============================================================
ler_matriz <- function(caminho) {
  m <- tryCatch(
    s3read_using(FUN = readRDS,
                 object = caminho, bucket = "projet-esteem",
                 opts = list("region" = "")),
    error = function(e) {
      message(caminho, ": não é .rds de verdade, lendo como CSV")
      s3read_using(FUN = data.table::fread,
                   object = caminho, bucket = "projet-esteem",
                   opts = list("region" = ""))
    }
  )
  as.matrix(m)
}

# ============================================================
# 2. Carregar as matrizes
# ============================================================
IO <- ler_matriz("Gloria/matrices/IO_2019.rds")
FD <- ler_matriz("Gloria/matrices/FD_2019.rds")
X  <- ler_matriz("Gloria/matrices/x_2019.rds")
VA <- ler_matriz("Gloria/matrices/VA_2019.rds")
QT <- ler_matriz("Gloria/matrices/QT_2019.rds")
QY <- ler_matriz("Gloria/matrices/QY_2019.rds")

# ============================================================
# 3. Dimensões — o que esperar:
#    IO 19680 x 19680 | FD 19680 x 984 | X 19680 x 1
#    VA 6 x 19680     | QT 159 x 19680 | QY ? x 984
# ============================================================
dim(IO); dim(FD); dim(X); dim(VA); dim(QT); dim(QY)

# ============================================================
# 4. As matrizes fecham entre si? (diferenças devem ser ~0)
# ============================================================
x <- as.vector(X)

# Vendas: produção = vendas intermediárias + demanda final
dif_linha <- x - (rowSums(IO) + rowSums(FD))
summary(dif_linha)

# Custos: produção = insumos intermediários + valor adicionado
dif_coluna <- x - (colSums(IO) + colSums(VA))
summary(dif_coluna)



###########


ler_label <- function(caminho) {
  l <- tryCatch(
    s3read_using(FUN = readRDS,
                 object = caminho, bucket = "projet-esteem",
                 opts = list("region" = "")),
    error = function(e) {
      s3read_using(FUN = data.table::fread,
                   object = caminho, bucket = "projet-esteem",
                   opts = list("region" = ""))
    }
  )
  as.data.frame(l)
}

lab_IO <- ler_label("Gloria/labels/label_IO.rds")
lab_FD <- ler_label("Gloria/labels/label_FD.rds")
lab_VA <- ler_label("Gloria/labels/label_VA.rds")
lab_QT <- ler_label("Gloria/labels/label_QT.rds")

# Conferência: os números devem bater com as matrizes
nrow(lab_IO)   # 19680
nrow(lab_FD)   # 984
nrow(lab_VA)   # 6
nrow(lab_QT)   # 159
head(lab_IO)

