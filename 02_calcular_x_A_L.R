# ============================================================
# 02_calcular_x_A_L.R
# Calcula produção (x), coeficientes técnicos (A) e inversa de Leontief (L)
# Rodar depois de 01_carregar_dados.R (usa IO, FD, x_lin, lab_IO, bloco_IO)
#
# Memória: IO, A e L têm ~3 GB cada; a inversão precisa de espaço extra.
# Contar com ~12–15 GB livres. IO é apagada depois de calcular A.
# ============================================================


# ------------------------------------------------------------
# 0. Velocidade da álgebra linear
#    Procurar a linha "BLAS" na saída:
#    - OpenBLAS ou MKL -> a inversão leva minutos
#    - "libRblas" (BLAS padrão do R) -> pode levar mais de uma hora
# ------------------------------------------------------------
sessionInfo()


# ------------------------------------------------------------
# 1. Produção (x)
#    Lado das vendas: x = vendas intermediárias + demanda final
#    (já calculado no script 01 como x_lin).
#    É o que garante a identidade do modelo de Leontief: L %*% y = x.
# ------------------------------------------------------------
x <- x_lin
rm(x_lin, x_col)

sum(x <= 0)                        # setores com produção zero ou negativa
x_inv <- ifelse(x > 0, 1 / x, 0)   # evita divisão por zero


# ------------------------------------------------------------
# 2. Coeficientes técnicos: a_ij = z_ij / x_j
#    (cada coluna de IO dividida pela produção do setor comprador)
# ------------------------------------------------------------
A <- sweep(IO, 2, x_inv, "*")

rm(IO); gc()   # IO não é mais necessária

# Checagens padrão
sum(A < 0)                # coeficientes negativos (deveria ser 0 ou quase)

soma_col <- colSums(A)
problema <- which(soma_col >= 1)   # setor "compra mais insumo do que produz"
length(problema)

# Se houver, mostra onde estão (por grupo e os 20 piores)
# -> com a tabela desequilibrada isso pode acontecer;
#    se forem muitos, a L pode sair com valores negativos
if (length(problema) > 0) {
  print(table(bloco_IO[problema]))
  piores <- problema[order(-soma_col[problema])][1:min(20, length(problema))]
  print(data.frame(pais     = lab_IO$V1[piores],
                   setor    = lab_IO$V3[piores],
                   soma_col = round(soma_col[piores], 2)))
}


# ------------------------------------------------------------
# 3. Inversa de Leontief: L = (I - A)^-1
#    Monta (I - A) sem criar uma matriz identidade separada (economiza 3 GB)
# ------------------------------------------------------------
I_A <- -A
diag(I_A) <- diag(I_A) + 1

tempo <- system.time(L <- solve(I_A))
tempo["elapsed"] / 60    # minutos que a inversão levou

rm(I_A); gc()


# ------------------------------------------------------------
# 4. Checagens padrão da L
# ------------------------------------------------------------
sum(L < -1e-10)     # valores negativos (fora erro numérico): deveria ser 0
min(diag(L))        # diagonal >= 1

# Teste principal: L reproduz a produção a partir da demanda final?
y <- rowSums(FD)
x_teste <- as.vector(L %*% y)
summary(abs(x_teste - x) / ifelse(x > 0, x, NA))   # deve ser ~0
rm(x_teste)


# ------------------------------------------------------------
# NOTA — se um dia quiser salvar a L (para não recalcular)
#
# Nunca salvar no bucket "projet-esteem" (é compartilhado).
# Usar o bucket PESSOAL do SSP Cloud: tem o nome do seu usuário
# e só você vê, exceto o que estiver na pasta "diffusion/".
#
#   meu_bucket <- "SEU_USUARIO_SSPCLOUD"
#
#   s3write_using(L, FUN = saveRDS, object = "eemrio/L_2019.rds",
#                 bucket = meu_bucket, opts = list("region" = ""))
#
#   # para ler numa próxima sessão:
#   L <- s3read_using(FUN = readRDS, object = "eemrio/L_2019.rds",
#                     bucket = meu_bucket, opts = list("region" = ""))
#
# O arquivo tem alguns GB; salvar e ler leva uns minutos.
# ------------------------------------------------------------