# ============================================================
# 02_calcular_x_A_L.R
# Calcula produção (x), coeficientes técnicos (A) e inversa de Leontief (L)
# Rodar depois de 01_carregar_dados.R
#
# Memória: IO, A e L têm ~3 GB cada; a inversão precisa de espaço extra.
# Contar com ~12–15 GB livres. IO é apagada depois de calcular A.
# ============================================================


# ------------------------------------------------------------
# 1. Produção (x)
#    Usamos o lado das vendas: x = vendas intermediárias + demanda final.
#    É o que garante a identidade do modelo de Leontief: L %*% y = x.
#    (O arquivo x do bucket não é usado — não bate com a tabela.)
# ------------------------------------------------------------
x <- rowSums(IO) + rowSums(FD)

sum(x <= 0)    # setores com produção zero ou negativa
x_inv <- ifelse(x > 0, 1 / x, 0)   # evita divisão por zero


# ------------------------------------------------------------
# 2. Coeficientes técnicos: a_ij = z_ij / x_j
#    (cada coluna de IO dividida pela produção do setor comprador)
# ------------------------------------------------------------
A <- sweep(IO, 2, x_inv, "*")

rm(IO); gc()   # IO não é mais necessária

# Checagens padrão
sum(A < 0)            # coeficientes negativos (deveria ser 0 ou quase)
sum(colSums(A) >= 1)  # colunas com soma >= 1: setor usa mais insumo do que produz
                      # -> com a tabela desequilibrada isso pode acontecer;
                      #    se for > 0, a L pode sair com valores estranhos


# ------------------------------------------------------------
# 3. Inversa de Leontief: L = (I - A)^-1
#    Monta (I - A) sem criar uma matriz identidade separada (economiza 3 GB)
# ------------------------------------------------------------
I_A <- -A
diag(I_A) <- diag(I_A) + 1

L <- solve(I_A)
rm(I_A); gc()


# ------------------------------------------------------------
# 4. Checagens padrão da L
# ------------------------------------------------------------
sum(L < 0)          # L não deveria ter valores negativos
min(diag(L))        # diagonal >= 1

# Teste principal: L reproduz a produção a partir da demanda final?
y <- rowSums(FD)
x_teste <- as.vector(L %*% y)
summary(abs(x_teste - x) / ifelse(x > 0, x, NA))   # deve ser ~0


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
