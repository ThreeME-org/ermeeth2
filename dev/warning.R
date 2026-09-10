plop <- readLines("warning.txt")

treated <- plop[grepl("[a-z0-9A-Z]: no visible binding for global variable ",plop)]

data <- data.frame(
  fun = str_extract(treated,"^(\\w+):" ,group = 1),
  variable =  str_extract(treated,"‘(.+)’" , group = 1 )
) |>
  mutate(
    line = str_c(" ",variable, " <- NULL"  )
  )

function_list = set_names(unique(data$fun) , unique(data$fun)) |> map(
  ~{data |> filter(fun == .x) |> select(line) |> as.vector() |> unlist()}
)

for (funi in unique(data$fun) ){
  cat(str_c("\n\n",funi,"\n\n"),file = "plop.txt", append = TRUE)
  lines <- function_list[[funi]]
  cat(lines,sep = "\n",file = "plop.txt", append = TRUE)
}
