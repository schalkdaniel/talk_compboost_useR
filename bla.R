library(compboost)
library(mboost)

n_sim = 20000L
n_noise_features = 2000L

set.seed(314159)

country = sample(x = c("Austria", "Seychelles", "Germany", "Czechia", "Australia", "USA"),
  size = n_sim, replace = TRUE)

country_biases = c(Austria = 106, Seychelles = 111, Germany = 104, Czechia = 140,
  Australia = 71, USA = 74)

age = sample(x = 16:70, size = n_sim, replace = TRUE)
ageFun = function (age) { return ((10 - 0.1 * (age - 16)^2 + 0.002 * (age - 16)^3) / 1) }
contr_age = ageFun(age)

# plot(age, contr_age)

gender = sample(x = c("m", "f"), size = n_sim, replace = TRUE)
gender_biases = c(m = 5, f = -2)

beer_consumption = country_biases[country] + gender_biases[gender] + contr_age + rnorm(n = n_sim, mean = 0, sd = 2)
beer_consumption = ifelse(beer_consumption < 0, 0, beer_consumption)

beer_data = data.frame(
  beer_consumption = round(beer_consumption / 0.5) * 0.5,
  # beer_consumption_cups = round(beer_consumption / 0.5),
  gender = gender,
  country = country,
  age = age,
  weight = runif(n = n_sim, min = 60, max = 120),
  height = runif(n = n_sim, min = 156, max = 204)
)

for (i in seq_len(n_noise_features)) {
  set.seed(i * 3)
  beer_data[[paste0("app_usage", i)]] = runif(n = n_sim)
}

# 200 feats

6 - 5.72       # Used Memory 1 core
628            # Runtime using 1 core
6.24 - 5.92    # Used Memory 16 cores
57             # Runtime using 16 cores

# 2000 feats

    # Used Memory 1 core
5379        # Runtime using 1 core
9 - 6.02    # Used Memory 16 cores
489         # Runtime using 16 cores

time_cboost = proc.time()
cboost = boostSplines(data = beer_data, target = "beer_consumption",
  loss = LossAbsolute$new(), learning_rate = 0.1, iterations = 5000L,
  trace = 100L, optimizer = OptimizerCoordinateDescent$new(16L))
time_cboost = proc.time() - time_cboost

# 200 feats

6.8 - 5.76  # Used Memory
1266        # Runtime using 1 core

# 2000 feats

15 - 6.3  # Used Memory
13000     # Runtime using 1 core

time_mboost = proc.time()
mod_mboost = mboost(beer_consumption ~ ., data = beer_data, family = Laplace(),
  control = boost_control(mstop = 5000L, nu = 0.1, trace = TRUE))
time_mboost = proc.time() - time_mboost
