#install package to access SSP Cloud

install.packages("aws.s3", repos = "https://cloud.r-project.org")
library(aws.s3)

Sys.setenv(
  AWS_S3_ENDPOINT = "minio.lab.sspcloud.fr",
  AWS_ACCESS_KEY_ID = "VTCNKYXT4XZBHSWF390L",
  AWS_SECRET_ACCESS_KEY = "Ki+e+O5pY5d19N+KkW9627DtlpoboAoFyF5yAwHV",
  AWS_SESSION_TOKEN = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJWVENOS1lYVDRYWkJIU1dGMzkwTCIsImF1ZCI6Im1pbmlvLWRhdGFub2RlIiwiYXV0aF90aW1lIjoxNzk0ODY0OTExLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6InRhdXNjaGxAYWZkLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTc5NTQ3NDgxNywiZmFtaWx5X25hbWUiOiJUYXVzY2giLCJnaXZlbl9uYW1lIjoiTHVjYSIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSIsImVzdGVlbSJdLCJpYXQiOjE3OTQ4NzAwMTYsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiJvbnJ0cnQ6NzcwNDkzNjMtODlkOC04ODZlLThlYjYtOWJjYjg2NDA4ZjI2IiwibG9jYWxlIjoiZnIiLCJuYW1lIjoiTHVjYSBUYXVzY2giLCJvbnl4aWEtdXNlcm5hbWUiOiJ0YXVzY2giLCJwb2xpY3kiOiJzdHNvbmx5LHN0c29ubHkyIiwicHJlZmVycmVkX3VzZXJuYW1lIjoidGF1c2NoIiwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBlbWFpbCByZXNvdXJjZTptaW5pbyIsInNpZCI6ImE2NEVaU2JvVEFmdjdEX1Rrc2d1c0JoeiIsInN1YiI6IjJhMDI0NGQwLWJlYTktNDBkOC1hNzczLWQ0ZDZlMjAwM2EwZSIsInR5cCI6IkJlYXJlciJ9.luq_eYWXvO10qJjMkmkvtGmV6C7J15kftkVxotzYbZzyvj4blbiq9CahW8vgZDjQBgQaKlDSSBxRd_LM5Fkmww"
)

#accessing data
ReadMe <- as.matrix(s3read_using(FUN = data.table::fread,
                                 object = "Gloria/GLORIA_ReadMe_059a.xlsx",
                                 bucket = "projet-esteem", opts = list("region" = "")))


#saving data
s3write_using(x = as.data.frame(ReadMe), FUN = data.table::fwrite,
              object = "Gloria/GLORIA_ReadMe_059a.xlsx",
              bucket = "projet-esteem", opts = list("region" = ""))


