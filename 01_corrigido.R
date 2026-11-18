# ============================================================
# 01_carregar_dados.R
# EEMRIO UE–Mercosul — GLORIA 2019
# Carrega matrizes e labels, faz as checagens básicas
# e define as regiões (UE-27, Mercosul, resto do mundo).
#
# Unidade: a conferir no bloco 5 (com os dados antigos era mil US$ correntes)
# ============================================================


# ------------------------------------------------------------
# 0. Pacotes e acesso ao SSP Cloud
# ------------------------------------------------------------
# Instalar só uma vez:
# install.packages(c("aws.s3", "data.table"), repos = "https://cloud.r-project.org")

library(aws.s3)
library(data.table)

# Credenciais do S3 (SSP Cloud). NÃO salvar as chaves reais neste arquivo.
# Os tokens expiram: copiar valores atualizados da página "Mon compte"
# do SSP Cloud a cada sessão. Num serviço RStudio do próprio SSP Cloud,
# essas variáveis normalmente já vêm preenchidas — aí este bloco é dispensável.
Sys.setenv(
  AWS_S3_ENDPOINT = "minio.lab.sspcloud.fr",
  AWS_ACCESS_KEY_ID = "VTCNKYXT4XZBHSWF390L",
  AWS_SECRET_ACCESS_KEY = "Ki+e+O5pY5d19N+KkW9627DtlpoboAoFyF5yAwHV",
  AWS_SESSION_TOKEN = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJWVENOS1lYVDRYWkJIU1dGMzkwTCIsImF1ZCI6Im1pbmlvLWRhdGFub2RlIiwiYXV0aF90aW1lIjoxNzk0ODY0OTExLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6InRhdXNjaGxAYWZkLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTc5NTQ3NDgxNywiZmFtaWx5X25hbWUiOiJUYXVzY2giLCJnaXZlbl9uYW1lIjoiTHVjYSIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSIsImVzdGVlbSJdLCJpYXQiOjE3OTQ4NzAwMTYsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiJvbnJ0cnQ6NzcwNDkzNjMtODlkOC04ODZlLThlYjYtOWJjYjg2NDA4ZjI2IiwibG9jYWxlIjoiZnIiLCJuYW1lIjoiTHVjYSBUYXVzY2giLCJvbnl4aWEtdXNlcm5hbWUiOiJ0YXVzY2giLCJwb2xpY3kiOiJzdHNvbmx5LHN0c29ubHkyIiwicHJlZmVycmVkX3VzZXJuYW1lIjoidGF1c2NoIiwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBlbWFpbCByZXNvdXJjZTptaW5pbyIsInNpZCI6ImE2NEVaU2JvVEFmdjdEX1Rrc2d1c0JoeiIsInN1YiI6IjJhMDI0NGQwLWJlYTktNDBkOC1hNzczLWQ0ZDZlMjAwM2EwZSIsInR5cCI6IkJlYXJlciJ9.luq_eYWXvO10qJjMkmkvtGmV6C7J15kftkVxotzYbZzyvj4blbiq9CahW8vgZDjQBgQaKlDSSBxRd_LM5Fkmww"
)

bucket <- "projet-esteem"


# ------------------------------------------------------------
# 1. Funções de leitura
#    Alguns arquivos ".rds" são .rds de verdade, outros são CSV
#    com a extensão errada. As funções tentam os dois formatos.
# ------------------------------------------------------------
ler_s3 <- function(caminho) {
  tryCatch(
    s3read_using(FUN = readRDS, object = caminho, bucket = bucket,
                 opts = list("region" = "")),
    error = function(e) {
      s3read_using(FUN = data.table::fread, encoding = "UTF-8",
                   object = caminho, bucket = bucket, opts = list("region" = ""))
    }
  )
}

ler_matriz <- function(caminho) as.matrix(ler_s3(caminho))
ler_label  <- function(caminho) as.data.frame(ler_s3(caminho))


# ------------------------------------------------------------
# 2. Matrizes
#    Pasta certa: Gloria/matrices/059/global/2019/
#    (até 18/11/2026 estava sendo usada Gloria/matrices/, que era a errada)
#    A, L, B e G também estão na pasta, mas cada uma ocupa ~3 GB na memória.
#    Carregar só quando for usar.
# ------------------------------------------------------------
pasta <- "Gloria/matrices/059/global/2019/"

IO <- ler_matriz(paste0(pasta, "IO_2019.rds"))   # consumo intermediário
FD <- ler_matriz(paste0(pasta, "FD_2019.rds"))   # demanda final
VA <- ler_matriz(paste0(pasta, "VA_2019.rds"))   # valor adicionado
QT <- ler_matriz(paste0(pasta, "QT_2019.rds"))   # satélite dos setores
QY <- ler_matriz(paste0(pasta, "QY_2019.rds"))   # satélite da demanda final
X  <- ler_matriz(paste0(pasta, "x_2019.rds"))    # produção


# ------------------------------------------------------------
# 3. Labels (versão 059 — as da pasta antiga eram de outra versão do GLORIA)
# ------------------------------------------------------------
pasta_labels <- "Gloria/matrices/059/labels/"

lab_IO <- ler_label(paste0(pasta_labels, "label_IO.rds"))  # V1 = país, V2 = nome, V3 = setor
lab_FD <- ler_label(paste0(pasta_labels, "label_FD.rds"))  # V1 = país, V2 = nome, V3 = categoria
lab_VA <- ler_label(paste0(pasta_labels, "label_VA.rds"))  # Lfd_Nr, Value_added_names
lab_QT <- ler_label(paste0(pasta_labels, "label_QT.rds"))  # 6130 indicadores satélite:
# Lfd_Nr, Sat_head_indicator,
# Sat_indicator, Sat_unit

# Lista das 164 regiões, da aba "Regions" do ReadMe oficial (Excel de verdade)
lab_regioes <- s3read_using(FUN = readxl::read_excel, sheet = "Regions",
                            object = paste0(pasta_labels, "GLORIA_ReadMe_059a.xlsx"),
                            bucket = bucket, opts = list("region" = ""))

paises <- unique(lab_IO$V1)                        # as 164 regiões, na ordem da matriz
stopifnot(identical(paises, lab_regioes$Region_acronyms))  # mesma ordem nos dois


# ------------------------------------------------------------
# 4. Checagem de dimensões
# ------------------------------------------------------------
n <- 164 * 120

stopifnot(
  dim(IO) == c(n, n),
  dim(FD) == c(n, 164 * 6),
  dim(VA) == c(6, n),
  dim(QT) == c(nrow(lab_QT), n),
  nrow(QY) == nrow(lab_QT),
  ncol(QY) %in% c(164, 164 * 6),   # por país ou por categoria de demanda final
  length(X) == n,
  nrow(lab_IO) == n,
  nrow(lab_FD) == 164 * 6,
  nrow(lab_QT) == 6130,
  length(paises) == 164
)


# ------------------------------------------------------------
# 5. Checagem de equilíbrio da tabela
#    Numa MRIO equilibrada, produção pelo lado das vendas (x_lin)
#    = produção pelo lado dos custos (x_col) = x do arquivo, setor por setor.
#    Os três quantis devem dar ~0.
#
#    RESULTADO 18/11/2026 (versão 059):
#    - vendas x arquivo x: ~0 (diferença de arredondamento) -> x do arquivo = lado das vendas
#    - custos x vendas: mediana 0,03%, mas 99º percentil ~65% -> desequilíbrio
#      no lado dos custos concentrado em poucos setores
# ------------------------------------------------------------
x     <- as.vector(X)
x_lin <- rowSums(IO) + rowSums(FD)   # vendas: intermediárias + demanda final
x_col <- colSums(IO) + colSums(VA)   # custos: insumos + valor adicionado

relativo <- function(a, b) quantile(abs((a - b) / ifelse(b != 0, b, NA)),
                                    c(0.5, 0.9, 0.99), na.rm = TRUE)
relativo(x_lin, x_col)   # vendas x custos
relativo(x_lin, x)       # vendas x arquivo x
relativo(x_col, x)       # custos x arquivo x

# Unidade: soma do VA do Brasil (PIB 2019 ~ 1,87 tri US$; a preços básicos, um pouco menos)
sum(VA[, lab_IO$V1 == "BRA"])   # ~1,6e9 -> mil US$


# ------------------------------------------------------------
# 6. Regiões
# ------------------------------------------------------------
mercosul <- c("ARG", "BRA", "PRY", "URY")

ue27 <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST",
          "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX",
          "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")

# Grupos de análise — para acrescentar ou tirar um grupo, é só mexer aqui.
# Países que não estão em nenhum grupo viram "Resto".
grupos <- list(
  UE       = ue27,
  Mercosul = mercosul,
  China    = "CHN",
  EUA      = "USA"
  # , AmSul = c("BOL", "CHL", "COL", "ECU", "PER", "VEN")  # resto da Am. do Sul (opcional)
)

stopifnot(all(unlist(grupos) %in% paises))   # todos os códigos existem
stopifnot(!any(duplicated(unlist(grupos))))  # nenhum país em dois grupos

# Grupo de cada país
bloco <- function(pais) {
  b <- rep("Resto", length(pais))
  for (g in names(grupos)) b[pais %in% grupos[[g]]] <- g
  b
}

# Grupo de cada linha/coluna, por dimensão da matriz
bloco_IO <- bloco(lab_IO$V1)   # 19.680 — linhas e colunas de IO, A, L;
#          linhas de FD; colunas de QT e VA; x
bloco_FD <- bloco(lab_FD$V1)   # 984    — colunas de FD
bloco_QY <- bloco(paises)      # 164    — colunas de QY

# Posições prontas para recortar as matrizes
pos_mer <- which(bloco_IO == "Mercosul")   # 480  (4 x 120)
pos_ue  <- which(bloco_IO == "UE")         # 3240 (27 x 120)

col_FD_mer <- which(bloco_FD == "Mercosul")   # 24  (4 x 6)
col_FD_ue  <- which(bloco_FD == "UE")         # 162 (27 x 6)

col_QY_mer <- which(bloco_QY == "Mercosul")   # 4
col_QY_ue  <- which(bloco_QY == "UE")         # 27

# China e EUA: mesma lógica, quando precisar
# pos_chn <- which(bloco_IO == "China");  col_FD_chn <- which(bloco_FD == "China")
# pos_usa <- which(bloco_IO == "EUA");    col_FD_usa <- which(bloco_FD == "EUA")

# Conferência: países por grupo
table(bloco_IO) / 120   # China 1, EUA 1, Mercosul 4, Resto 131, UE 27