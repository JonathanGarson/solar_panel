# Gillingham & Tsevanov (2019), Replication

library(boot)
library(dplyr)
library(fixest)
library(haven)
library(tidyr)
library(car)
library(pscl)
library(gmm)

# Data --------------------------------------------------------------------

qe = setDT(read_dta(data_raw("QE_dataset.dta")))

# 2SLS ---------------------------------------------------------------------

iv = feols(instal ~ solarize + pop + income + med_age + col_ba + grad_prof + r + d 
           | year 
           | costperwatt ~ roofwage + incent, 
           cluster = ~town, data= qe)

# Elasticity
beta_hat = iv$coefficients[["fit_costperwatt"]]
ybar = mean(qe$instal)
xbar = mean(qe$costperwatt)
elas_iv = (beta_hat*xbar)/ybar

# SE - Delta Method
vcov_iv <- vcov(iv)
coef <- iv$coefficients
g_formula = "(fit_costperwatt * 3.764505)/0.4781151"

elas_se <- deltaMethod(object = coef,
                       g = g_formula,
                       vcov = vcov_iv)

# Output point estimate and standard error
c(Elasticity = elas_iv, StdError = elas_se)

# Poisson -----------------------------------------------------------------
areg = feols(costperwatt ~ incent + roofwage + solarize + pop + income + med_age + col_ba + grad_prof + r + d | year, cluster = ~town, data = qe)
qe$res = areg$residuals

# Estimate the FE Poisson model using fepois() from fixest.
# Here, 'id' is the individual/unit identifier for the fixed effect.
pois <- fepois(instal ~ costperwatt + res + solarize + pop + income + med_age + col_ba + grad_prof + r + d 
                     | year + id,
                     data = qe, cluster = ~town)

# Elasticity
beta_hat = pois$coefficients[["costperwatt"]]
xbar = mean(qe$costperwatt)
elas_iv = beta_hat*xbar

# SE - Delta Method
vcov_pois <- vcov(pois)
coef <- pois$coefficients
g_formula = "costperwatt * 3.764505"

elas_se <- deltaMethod(object = coef,
                       g = g_formula,
                       vcov = vcov_pois)

# Output point estimate and standard error
c(Elasticity = elas_iv, StdError = elas_se)

# Surdispersion test
test = performance::check_overdispersion(pois)

# Hurdle ------------------------------------------------------------------
logit = feglm(nonz ~ costperwatt + res + solarize + pop + income + med_age + col_ba + grad_prof + r + d 
              | year + id, 
              cluster = ~town,
              family = "logit",
              data = qe)

beta_hat = logit$coefficients[["costperwatt"]] 
ybar = mean(qe$nonz)
xbar = mean(qe$costperwatt)
elas = beta_hat *xbar * (1 - ybar)

vcov_logit <- vcov(logit)
coef <- logit$coefficients
g_formula = "costperwatt * 3.764505 * (1 - 0.3015459)"

elas_se <- deltaMethod(object = coef,
                       g = g_formula,
                       vcov = vcov_logit)

# 2) Compute sample means in the truncated sample
trunc  <- subset(qe, instal > 0)
YbarP <- mean(trunc$instal)
pbarP <- mean(trunc$costperwatt)

# 3) Invert m(lambda) = lambda/(1 - exp(-lambda)) via uniroot
m_fun   <- function(lambda) lambda/(1 - exp(-lambda))
inv_m   <- function(y) {
  uniroot(function(l) m_fun(l) - y, lower=1e-6, upper=50)$root
}
lambda_hat <- inv_m(YbarP)

# 4) Fit your truncated‐Poisson GMM (sketch)
#    Define moment conditions ψ_i(δ2) = Z_i' * (Y_i - φ_i(X_i; δ2))
#    and then call gmm():

moment_fun <- function(theta, data) {
  # extract X_it matrix and instruments Z_it for this block-group
  Xmat <- as.matrix(data$X_list)   # T*_i × P
  Zmat <- as.matrix(data$Z_list)   # T*_i × Q
  Yvec <- data$Y_list              # length T*_i
  
  # compute beta_it = exp(X_it %*% delta2)
  beta <- exp(Xmat %*% theta)        # length T*_i
  
  # compute phi_it = (∂h_i/∂β_it)*β_it / h_i
  #  here we use the fact that, for a zero-truncated Poisson with
  #  "intensity" proportional to beta, the conditional mean is
  #     lambda/(1 - exp(-lambda)),
  #  and lambda ∝ beta.  Since the fixed-effect drops out, we can
  #  _approximate_ phi_it ≈ beta / (1 - exp(-beta)).
  phi <- beta / (1 - exp(-beta))
  
  # residuals
  xi <- Yvec - phi                   # length T*_i
  
  # moment for this i: Z_i′ * xi  => a Q-vector
  #   we return a matrix of size Q×1, but gmm::gmm expects Q×1 as row
  return( t(Zmat) %*% xi )           # a 1×Q row
}

# 2) Stack your block-group data into a data.frame where each row i has:
  #    - Y_list: vector of positive counts
  #    - X_list: matrix of regressors for those periods
  #    - Z_list: matrix of instruments for those periods

X_vars <- c("costperwatt", "solarize", "pop", "income", "med_age", "col_ba", "grad_prof", "r", "d" , "y08", "y09",  paste0("y",10:14))
Z_vars <- c("incent", "incent", "roofwage", "solarize", "pop", "income", "med_age", "col_ba", "grad_prof", "r", "d", "y08", "y09",  paste0("y",10:14))
block_group_df <- trunc %>%
  group_by(id) %>%
  summarize(
    # 1) the vector of positive counts
    Y_list = list(instal),
    # 2) pick() the X_vars into a matrix
    X_list = list(as.matrix(pick(all_of(X_vars)))),
    # 3) pick() the Z_vars into a matrix
    Z_list = list(as.matrix(pick(all_of(Z_vars))))
  ) %>%
  ungroup()

theta = rep(0, length(X_vars)) 
matrix = moment_fun(theta, block_group_df)

gmm_fit <- gmm(g = moment_function, x = trunc, t0 = init_guess,
                 vcov = "iid", # or cluster robust
                 weights = "ident")
gamma2_hat <- coef(gmm_fit)["costperwatt"]  # your price coefficient

# (If you already have `iv` from fixest in your example:)
gamma2_hat <- iv$coefficients["fit_costperwatt"]  # GMM‐stage coefficient

# 5) Compute the point estimate of η_P
etaP_hat <- (1 + lambda_hat - YbarP) * gamma2_hat * pbarP



# TEST --------------------------------------------------------------------

# 1) Prepare the truncated‐Poisson sample
trunc <- qe %>% filter(instal > 0)

# 2) Specify your regressors (X) and instruments (Z)
X_vars <- c(
  "costperwatt", "res",               # endogenous + CF residual
  "solarize", "pop", "income", "med_age",
  "col_ba", "grad_prof", "r", "d",
  paste0("y0", 8:9),
  paste0("y", 10:14)# y08–y14
)
Z_vars <- c(
  "incent", "roofwage",             # your two excluded instruments
  "solarize", "pop", "income", "med_age",
  "col_ba", "grad_prof", "r", "d",
  paste0("y0", 8:9),
  paste0("y", 10:14) # exogenous year‐dummies
)

# 3) Nest into one row per block‐group
block_group_df <- trunc %>%
  group_by(id) %>%
  summarize(
    Y_list = list(instal),                      # T*_i‐vector
    X_list = list(as.matrix(across(all_of(X_vars)))),
    Z_list = list(as.matrix(across(all_of(Z_vars))))
  ) %>%
  ungroup()

# 4) Define the GMM moment function
moment_fun <- function(theta, data) {
  # data is the entire block_group_df
  # returns an N × Q matrix of moments
  moments <- t(sapply(seq_len(nrow(data)), function(i) {
    Xmat <- data$X_list[[i]]   # T*_i × P
    Zmat <- data$Z_list[[i]]   # T*_i × Q
    Yvec <- data$Y_list[[i]]   # length T*_i
    
    # truncated‐Poisson conditional mean φ_it = λ/(1-e^{-λ}), with λ∝exp(X′θ)
    beta <- exp(Xmat %*% theta)           # length T*_i
    phi  <- beta / (1 - exp(-beta))       # length T*_i
    
    xi   <- Yvec - phi                    # residuals
    
    # moment: Z_i′ * xi  ⇒ a Q‐vector
    as.vector(t(Zmat) %*% xi)
  }))
  # optionally name the columns:
  colnames(moments) <- Z_vars
  return(moments)
}

# 5) Initial guess for θ (length = P)
init_theta <- rep(0, length(X_vars))
names(init_theta) <- X_vars

# 6) Run GMM
gmm_fit <- gmm(
  g     = moment_fun,
  x     = block_group_df,
  t0    = init_theta,
  type  = "twoStep",
  vcov  = "iid",
  weights = "ident"
)

summary(gmm_fit)

# 7) Extract the price coefficient δ₂_costperwatt
theta_hat   <- coef(gmm_fit)           # named by X_vars
gamma2_hat  <- theta_hat["costperwatt"] # your IV‐Poisson price coefficient

# 8) Compute the η_P elasticity
#    (using means from before)
YbarP      <- mean(trunc$instal)
pbarP      <- mean(trunc$costperwatt)
# invert m(λ)=λ/(1−e^{−λ}) → λ:
inv_m      <- function(y) uniroot(function(l) l/(1-exp(-l)) - y,
                                  lower=1e-6, upper=100)$root
lambda_hat <- inv_m(YbarP)

etaP_hat   <- (1 + lambda_hat - YbarP) * gamma2_hat * pbarP
etaP_hat
