IO <- as.matrix(s3read_using(FUN = data.table::fread,
                             object = "Gloria/matrices/IO_2019.rds",
                             bucket = "projet-esteem", opts = list("region" = "")))

FD <- as.matrix(s3read_using(FUN = data.table::fread,
                             object = "Gloria/matrices/FD_2019.rds",
                             bucket = "projet-esteem", opts = list("region" = "")))


X <- as.matrix(s3read_using(FUN = data.table::fread,
                                 object = "Gloria/matrices/x_2019.rds",
                                 bucket = "projet-esteem", opts = list("region" = "")))

VA <- as.matrix(s3read_using(FUN = data.table::fread,
                                    object = "Gloria/matrices/VA_2019.rds",
                                    bucket = "projet-esteem", opts = list("region" = "")))

QT <- as.matrix(s3read_using(FUN = data.table::fread,
                             object = "Gloria/matrices/QT_2019.rds",
                             bucket = "projet-esteem", opts = list("region" = "")))

QY <- as.matrix(s3read_using(FUN = data.table::fread,
                             object = "Gloria/matrices/QY_2019.rds",
                             bucket = "projet-esteem", opts = list("region" = "")))

