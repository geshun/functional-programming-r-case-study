# Functional Programming for Data Science with R
# A real world example to facilitate data pre-processing with Tidyverse
# fiiinspires@gmail.com
# -----------------------------------------------------------------------------#

if (!require(tidyverse)) install.packages("tidyverse")

# Loading package ----
library(tidyverse)


# Reading dataset into R's environment ----

## Reading file into memory ----
csv_files <- fs::dir_ls(path = "data")

### Base R ----
csv_files <- list.files(path = "data", full.names = TRUE)


## Unifying the datasets ----
num_label <- c(k = "e3", M = "e6", B = "e9")

csv_files |>
  map(\(x) read_csv(x, col_types = cols(.default = col_character()))) |>
  list_rbind(names_to = "file") |>
  mutate(across(file, ~ str_remove_all(.x, "data/|.csv"))) |>
  pivot_longer(
    cols = -c(file, country),
    names_to = "year",
    names_transform = as.integer
  ) |>
  mutate(across(value, ~ as.numeric(str_replace_all(.x, num_label)))) |>
  pivot_wider(names_from = file, values_from = value) |>
  mutate(country = as.factor(country), population = as.integer(population))


## Reading a single file ----
read_csv("data/gdp.csv")

### Base R ----
read.csv("data/gdp.csv", check.names = FALSE)


## Reading multiple files ----
read_csv(csv_files[2:3], id = "origin")


## Reading all files the right way ----
dfs <- csv_files %>% map(.f = read_csv)

### Base R ----
dfs <- csv_files |> lapply(\(df) read.csv(df, check.names = FALSE))

### Challenge 1 ----
files <- c("gdp", "life_expectancy", "population")
url <- "https://github.com/geshun/analytics-group/raw/main/weeks/7/"
paths <- str_c(url, files, ".csv")
map(paths, read_csv)

### Challenge 2 ----
fs::dir_map(path = "data", fun = read_csv)


# Mutating, reshaping and cleaning data ----

## Setting names attribute ----
dfs <-
  csv_files %>%
  map(read_csv) %>%
  set_names(str_remove_all(csv_files, "data/|.csv"))

### Base R ----
dfs <-
  csv_files |>
  lapply(read.csv, check.names = FALSE) |>
  setNames(gsub("data/|.csv", "", csv_files))


## Checking column data type ----
dfs |>
  pluck("gdp") |>
  select(-country) |>
  map_chr(typeof) |>
  unique()

### Base R ----
dfs$gdp |>
  subset(select = -country) |>
  vapply(typeof, character(1)) |>
  unique()

### Using type_sum() function from pillar package ----
dfs |>
  pluck("gdp") |>
  select(-country) |>
  map_chr(type_sum) |>
  unique()

### Using every() function from purrr package ----
dfs |>
  pluck("gdp") |>
  select(-country) |>
  every(is.numeric)

#### Base R ----
dfs$gdp |>
  subset(select = -country) |>
  vapply(is.numeric, logical(1)) |>
  all()


## Writing a function to find column types ----
get_col_type <- function(df) {
  df |>
    map_chr(type_sum) |>
    unique()
}

### Base R ----
get_col_type <- function(df) {
  df |>
    vapply(typeof, character(1)) |>
    unique()
}

### Applying the get_col_type() function ----
dfs |>
  map(\(df) select(df, -country)) |>
  map(get_col_type)

#### Base R ----
dfs |>
  lapply(\(df) subset(df, select = -country)) |>
  lapply(get_col_type)


## Mutating column as character ----
mutate_as_chr <- function(df) {
  df |>
    mutate(across(.cols = !where(is.character), .fns = as.character))
}

### Base R
mutate_as_chr <- function(df) {
  df[] <- df |> apply(2, as.character)
  df
}

### Applying the mutate_as_chr() function ----
dfs |>
  map(mutate_as_chr) |>
  map(get_col_type)

#### Base R ----
dfs |>
  lapply(mutate_as_chr) |>
  lapply(get_col_type)


## Modifying column as character ----
modify_as_chr <- function(df) {
  df |>
    modify_if(negate(is.character), as.character)
}

### Base R ----
modify_as_chr <- function(df) {
  df[vapply(df, Negate(is.character), logical(1))] <-
    Filter(Negate(is.character), df) |>
    apply(2, as.character)
  df
}

### Applying modify_as_chr() function ----
dfs |>
  map(modify_as_chr) |>
  map(\(df) every(df, is.character))

#### Base R ----
dfs |>
  lapply(modify_as_chr) |>
  lapply(\(df) all(vapply(df, is.character, logical(1))))

### Challenge ----
csv_files |>
  map(
    \(df) read_csv(df, col_types = cols(.default = col_character()))
  )

#### Base R ----
csv_files |>
  lapply(
    \(df) read.csv(df, check.names = FALSE, colClasses = "character")
  )


## Reshaping from wide to long data frame ----
dfs <- dfs |>
  map(~ pivot_longer(.x, cols = -country, names_to = "year"))

### Base R ----
cast_to_long <- function(df) {
  # correct names except country - numeric colnames
  colnames(df)[-1] <- paste0("x", colnames(df)[-1])

  df <- reshape(
    df,
    direction = "long",
    varying = 2:ncol(df),
    idvar = "country",
    sep = ""
  )

  row.names(df) <- NULL
  colnames(df) <- c("country", "year", "value")
  df
}

dfs <- dfs |>
  lapply(cast_to_long)


## Adding extra columns to data frame ----
adorn_col <- function(df, val) {
  df |> mutate(origin = val)
}

### Base R ----
adorn_col <- function(df, val) {
  df |> transform(origin = val)
}

### Applying adorn_col() function ----
dfs |> map2(.y = names(dfs), .f = adorn_col)

#### Base R ----
Map(adorn_col, dfs, names(dfs))

### Using reduce and union ----
df <- dfs |>
  imap(adorn_col) |>
  reduce(union)

### Base R ----
df <- Reduce(rbind, Map(adorn_col, dfs, names(dfs)))

### Challenge ----
pmap(list(dfs, names(dfs)), adorn_col)

### Modifying adorn_col() function ----
adorn_col <- function(df, col_name, val) {
  df |> mutate("{{ col_name }}" := val)
}


## Counting missing records ----
count_na <- function(x) {
  sum(is.na(x))
}

df |>
  map_dfr(count_na) |>
  pivot_longer(cols = everything())

### Base R ----
df |>
  lapply(count_na) |>
  cbind()


## Dealing with records with missing entries ----
df |>
  filter(if_any(.cols = everything(), .fns = is.na))

### Base R ----
df |>
  subset(subset = rowSums(is.na(df)) != 0)

### Distinct years missing ----
df |>
  filter(if_any(.cols = everything(), .fns = is.na)) |>
  select(country, year) |>
  nest(.by = country) |>
  distinct(data)

#### Base R ----
df |>
  subset(subset = !complete.cases(df), select = c(country, year)) |>
  (\(x) split(x, x$country))() |>
  lapply(\(x) subset(x, select = year)) |>
  unique() |>
  length()

### Dropping missing records ----
df <- drop_na(df)

#### Base R ----
df <- df |> subset(subset = complete.cases(df))


## Cleaning the columns ----
label_to_num <- function(x) {
  value <- as.numeric(str_remove(x, "[kMB]"))
  case_when(
    str_ends(x, "k") ~ value * 1e3,
    str_ends(x, "M") ~ value * 1e6,
    str_ends(x, "B") ~ value * 1e9,
    .default = value
  )
}

label_to_num2 <- function(x) {
  labels <- c(k = 1e3, M = 1e6, B = 1e9)
  label <- str_extract(x, "[kMB]")

  label_values <- labels[label]
  label_values[is.na(label)] <- 1

  as.numeric(str_remove(x, "[kMB]")) * label_values
}

### Base R ----
label_to_num <- function(x) {
  value <- as.numeric(gsub("[kMB]", "", x))
  ifelse(
    grepl("k", x), value * 1e3,
    ifelse(
      grepl("M", x), value * 1e6,
      ifelse(
        grepl("B", x), value * 1e9, value
      )
    )
  )
}

### Using label_to_num() function ----
df <- df |>
  mutate(
    value = label_to_num(value),
    year = as.integer(year),
    country = as.factor(country)
  )

#### Base R ----
df <- df |>
  transform(value = label_to_num(value)) |>
  transform(year = as.integer(year)) |>
  transform(country, as.factor(country))

### Challenge ----
label <- c(k = "e3", M = "e6", B = "e9")

df |>
  mutate(value = as.numeric(str_replace_all(value, label)))


## Reshaping from long to wide data frame ----
df_wide <- df |>
  pivot_wider(names_from = origin, values_from = value) |>
  mutate(across(population, as.integer))

### Base R ----
df_wide <-
  df |>
  reshape(
    direction = "wide",
    idvar = c("country", "year"), timevar = "origin"
  ) |>
  (\(x) setNames(x, gsub("value.", "", colnames(x))))() |>
  transform(population = as.integer(population))

### Challenge ----
count_na(df_wide)
df_wide |> filter(if_any(.col = everything(), .fns = is.na))

# With the data in this format, we cannot drop records with missing values since
# that will affect variables like gdp and population we intend analyzing.

## Composing data pre-processing steps ----
read_and_combine <- function(files) {
  files |>
    map(read_csv) |>
    map(\(df) modify_if(df, negate(is.character), as.character)) |>
    list_rbind(names_to = "origin") |>
    mutate(origin = str_remove_all(origin, "data/|.csv"))
}

read_and_combine(csv_files)

reshape_to_long <- function(df) {
  df |>
    pivot_longer(
      cols = -c(origin, country),
      names_to = "year",
      names_transform = as.integer
    ) |>
    mutate(value = as.numeric(str_replace_all(
      value, c(k = "e3", M = "e6", B = "e9")
    )))
}

csv_files |>
  read_and_combine() |>
  reshape_to_long()

reshape_to_wide <- function(df) {
  df |>
    pivot_wider(names_from = origin, values_from = value) |>
    mutate(
      country = as.factor(country),
      population = as.integer(population)
    )
}

csv_files |>
  read_and_combine() |>
  reshape_to_long() |>
  reshape_to_wide()

preprocess_data <- compose(
  reshape_to_wide, reshape_to_long, read_and_combine
)

preprocess_data(csv_files)


## Saving data object ----
write_rds(df_wide, file = "gdp_population_life")

### Base R ----
saveRDS(df_wide, "gdp_population_life.rds")


# Performing basic EDA ----

## Computing summary statistics ----
df_wide |>
  filter(country == "China", year %in% 2000:2009) |>
  select(gdp, life_expectancy) |>
  summary()

### Base R ----
df_wide |>
  subset(country == "China" & year %in% 2000:2009,
    select = -c(country, year)
  ) |>
  summary()

### Using keep() function ----
df_wide |>
  filter(country == "China", year %in% 2000:2009) |>
  keep(is.double) |>
  summary()

#### Base R ----
df_wide |>
  subset(country == "China" & year %in% 2000:2009) |>
  (\(x) Filter(is.double, x))() |>
  summary()

### Challenge ----
df_wide |>
  filter(country == "China", year %in% 2000:2009) |>
  discard(negate(is.double)) |>
  summary()


## Finding group correlation coefficient ----
df_wide |>
  filter(country %in% c("India", "China"), year %in% 2000:2009) |>
  (\(df) split(df, df$country, drop = TRUE))() |>
  map(\(df) keep(df, is.double)) |>
  map(cor)

### Base R ----
df_wide |>
  subset(country %in% c("India", "China") & year %in% 2000:2009) |>
  (\(df) split(df, df$country, drop = TRUE))() |>
  lapply(\(df) subset(df, select = vapply(df, is.double, logical(1)))) |>
  lapply(cor)

### Challenge ----
df_wide |>
  filter(country %in% c("India", "China"), year %in% 2000:2009) |>
  nest(data = -country) |>
  mutate(correlation = map(data, \(df) cor(df$gdp, df$life_expectancy))) |>
  unnest(correlation)


## Generating multiple scatter plots ----
df_wide |>
  filter(country %in% c("India", "China"), year %in% 2000:2009) |>
  ggplot(aes(x = gdp, y = life_expectancy, color = country)) +
  geom_point()

### Base R ----
df_wide |>
  subset(country %in% c("India", "China") & year %in% 2000:2009) |>
  (\(df) plot(df$gdp, df$life_expectancy, col = df$country))()

### Individual plots ----
plots <- df_wide |>
  filter(country %in% c("India", "China"), year %in% 2000:2009) |>
  (\(df) split(df, df$country, drop = TRUE))() |>
  map(
    ~ ggplot(.x, aes(x = gdp, y = life_expectancy, color = country)) +
      geom_point() +
      theme_bw()
  )

walk(plots, print)

#### Base R ----
df_wide |>
  subset(country %in% c("India", "China") & year %in% 2000:2009) |>
  (\(df) split(df, df$country, drop = TRUE))() |>
  lapply(\(df) subset(df, select = sapply(df, is.double))) |>
  lapply(\(df) plot(df, type = "p"))
