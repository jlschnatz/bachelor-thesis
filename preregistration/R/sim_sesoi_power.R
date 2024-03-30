# Load libraries ———————————————————————————————————————————————————————————————————————————————————————————————————————

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, furrr, extraDistr, betareg, TOSTER, here)
set.seed(42)

# Hypothesis I —————————————————————————————————————————————————————————————————————————————————————————————————————————

simulate_power_h1 <- function(n, b0, b1, b2, phi, tol = 1e-10, iter) {
  simulate_power_h1_ <- function(n, b0, b1, b2, phi, tol) {
    # standard normal fisher z-transformed correlation values
    z <- rnorm(n, mean = -.1, sd = 0.5) 
    # linear term
    lp <- b0 + b1*z + b2*z^2
    # weights of publication bias
    w_pbs <- rprop(n, phi, plogis(lp))
    # handle edges cases
    w_pbs[w_pbs == 1] <- 1 - tol 
    w_pbs[w_pbs == 0] <- tol
    # fit beta-regression model with logit link
    mod <- betareg(formula = w_pbs ~ z, link = "logit")
    # calculate p-value
    p <- pnorm(coef(mod) / sqrt(diag(vcov(mod))), lower.tail = FALSE)["z"]
    return(data.frame(n, b0, b1, phi, p))
  }
  results <- vector("list", length = iter)
  for (i in seq_len(iter)) {
    results[[i]] <- simulate_power_h1_(n, b0, b1, b2, phi, tol)
    rownames(results[[i]]) <- NULL
  }
  return(bind_rows(results, .id = "id_iter"))
}

## Run simulation
set.seed(42)
sim_grid <- expand_grid(
  n = 150,                              # n = 150 is fixed (150 meta-analyses)
  b0 = c(0),                            # vary intercept
  b1 = seq(0.02, 0.3, 0.01),            # vary slope (parameter of interest)
  b2 = 0,                               # no quadratic term
  phi = c(10, 20, 30),                  # vary dispersion parameter
  tol = 1e-10,                          # set tol
  iter = 5e3                            # number of iterations
  )

plan(multisession(workers = availableCores() -2))

t1 <- Sys.time()
df_h1 <- future_pmap(
  .l = sim_grid,
  .f = simulate_power_h1,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
  )
t2 <- Sys.time()

print(t2 - t1)

## Save results
bind_rows(df_h1) |> 
  group_by(n, b0, b1, phi) |> 
  summarise(power_mean = mean(p < 0.05),
            power_se = sd(p < 0.05)/sqrt(n()),
            ub = power_mean + qnorm(0.975) * power_se,
            lb = power_mean - qnorm(0.975) * power_se,
            .groups = "drop") |> 
  write_rds(here("preregistration/data/h1_sim.rds"))

# Hypothesis II ———————————————————————————————————————————————————————————————————————————————————————————————————————

simulate_power_h2 <- function(n, b0, b1, b2, phi, tol = 1e-10, iter) {
  simulate_power_h2_ <- function(n, b0, b1, b2, phi, tol) {
    # standard normal fisher z-transformed correlation values
    delta <- rnorm(n, mean = 0, sd = sqrt(0.3^2 + 0.3^2)) 
    # linear term
    lp <- b0 + b1*delta + b2*delta^2
    # weights of publication bias
    w_pbs <- rprop(n, phi, plogis(lp))
    # handle edges cases
    w_pbs[w_pbs == 1] <- 1 - tol 
    w_pbs[w_pbs == 0] <- tol
    tryCatch({
      mod <- betareg(formula = w_pbs ~ delta + I(delta^2), link = "logit")
      # calculate p-value
      p <- pnorm(coef(mod) / sqrt(diag(vcov(mod))), lower.tail = TRUE)["I(delta^2)"]
      data.frame(n, b0, b1, b2, phi, p)
    }, error = function(e) {
      NULL
    })
  }
  results <- vector("list", length = iter)
  for (i in seq_len(iter)) {
    results[[i]] <- simulate_power_h2_(n, b0, b1, b2, phi, tol)
    rownames(results[[i]]) <- NULL
  }
  return(results)
}

set.seed(42)
sim_grid_h2 <- expand_grid(
  n = 150,
  b0 = c(0),
  b1 = c(0),
  b2 = seq(-0.025, -0.6, -0.02),
  phi = c(10, 20, 30),
  tol = 1e-10,
  iter = 5e3
)

plan(multisession(workers = availableCores() -2))

t1 <- Sys.time()
df_h2 <- future_pmap(
  .l = sim_grid_h2,
  .f = simulate_power_h2,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)
t2 <- Sys.time()

## Save results
bind_rows(df_h2) |> 
  group_by(n, b0, b1, b2, phi) |> 
  summarise(power_mean = mean(p < 0.05),
            power_se = sd(p < 0.05)/sqrt(n()),
            ub = power_mean + qnorm(0.975) * power_se,
            lb = power_mean - qnorm(0.975) * power_se,
            .groups = "drop") |> 
  write_rds("preregistration/data/h2_sim.rds")

# Hypothesis III ————————————————————————————————————————————————————————————————————————————————————————————————————————

s_diff <- sqrt(0.3^2 + 0.3^2) # both distributions assumed sds = 0.3
power_function <- function(eqb) {
  power <- power_t_TOST(
    alpha = 0.05,    # significance level
    n = 57,         # predetermined sample size
    delta = 0,       # assumed true effect size
    eqb = eqb,       # equivalence bounds
    type = "paired", # paired test
    sd = s_diff     # standard deviation of the difference
    )$power
  return(power - 0.8)  # difference from the target power
}

eqb <- uniroot(power_function, c(0, 1))$root 
write_rds(eqb, "preregistration/data/h3_sim.rds")

# Hypothesis IV ————————————————————————————————————————————————————————————————————————————————————————————————————————

simulate_power_h4 <- function(n_mr, n_ma, b0, b1, phi, tol = 1e-10, iter) {
  simulate_power_h4_ <- function(n_mr, n_ma, b0, b1, phi, tol = 1e-10) {
    n <- n_mr + n_ma
    D <- c(rep(0, n_ma), rep(1, n_mr))
    lp <- b0 + b1*D
    w_pbs <- extraDistr::rprop(n, phi, plogis(lp))
    w_pbs[w_pbs == 1] <- 1 - tol
    w_pbs[w_pbs == 0] <- tol
    df <- data.frame(D = factor(D), w_pbs)
    contrasts(df$D) <- contr.treatment(2, base = 1)
    fit <- betareg(
      formula = w_pbs ~ D, 
      link = "logit",
      data = df
    )
    z <- coef(fit) / sqrt(diag(vcov(fit)))
    p <- pnorm(z[2], lower.tail = FALSE)
    out <- data.frame(n, b0, b1, phi, p)
    rownames(out) <- NULL
    return(out)
  }
  results <- vector("list", length = iter)
  for (i in seq_len(iter)) results[[i]] <- simulate_power_h4_(n_mr, n_ma, b0, b1, phi, tol)
  return(bind_rows(results, .id = "id_iter"))
}

set.seed(42)
sim_grid_h4 <- expand_grid(
  n_mr = 57,
  n_ma = 150,
  b0 = 0,
  b1 = seq(.05, 0.35, 0.025),
  phi = c(10, 20, 30),
  tol = 1e-10,
  iter = 5e3
)

plan(multisession(workers = availableCores() -2))

t1 <- Sys.time()
df_h4 <- future_pmap(
  .l = sim_grid_h4,
  .f = simulate_power_h4,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)
t2 <- Sys.time()

print(t2 - t1)

## Save results
bind_rows(df_h4) |> 
  group_by(n, phi, b0, b1) |> 
  summarise(power_mean = mean(p < 0.05),
            power_se = sd(p < 0.05)/sqrt(n()),
            ub = power_mean + qnorm(0.975) * power_se,
            lb = power_mean - qnorm(0.975) * power_se,
            .groups = "drop") |> 
  write_rds("preregistration/data/h4_sim.rds")

