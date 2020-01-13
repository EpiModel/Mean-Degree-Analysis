
##
## Diagnostics for Combination Prevention Model
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))

setwd("~/EpiModel Lab/ARTnet RADAR Mean Degree Comparison")
est <- readRDS("netest.rda")
netstats <- readRDS("netstats.rda")


# Main --------------------------------------------------------------------

fit_main <- est[[1]]

model_main_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = -1) +
  nodematch("race", diff = FALSE) + #race = TRUE; omit if FALSE
  nodefactor("race", levels = -1) +
  nodefactor("deg.casl", levels = -1) +
  concurrent +
  degrange(from = 3) +
  nodematch("role.class", diff = TRUE, levels = 1:2
  )
dx_main <- netdx(fit_main, nsims = 10, ncores = 6, nsteps = 500,
                 nwstats.formula = model_main_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_main)
plot(dx_main)

netstats$netstats_main


# Casual ------------------------------------------------------------------

fit_casl <- est[[2]]

model_casl_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = -c(1,5)) +
  nodefactor("deg.main", levels = -3) +
  concurrent +
  degrange(from = 4) +
  nodematch("role.class", diff = TRUE, levels = 1:2) +
  #If race = TRUE:
  nodematch("race", diff = FALSE) +
  nodefactor("race", levels = -1)
dx_casl <- netdx(fit_casl, nsims = 10, ncores = 6, nsteps = 500,
                 nwstats.formula = model_casl_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_casl, digits = 1)
plot(dx_casl)

netstats$casl


# One-Off -----------------------------------------------------------------

fit_inst <- est[[3]]

model_inst_dx <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", levels = -1) +
  nodefactor("risk.grp", levels = -5) +
  nodefactor("deg.tot", levels = -1) +
  nodematch("role.class", diff = TRUE, levels = 1:2) +
  #If race = TRUE
  nodematch("race", diff = FALSE) +
  nodefactor("race", levels = -1)
dx_inst <- netdx(fit_inst, nsims = 10000, dynamic = FALSE,
                 nwstats.formula = model_inst_dx,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))

print(dx_inst, digits = 1)

plot(dx_inst, sim.lines = TRUE, sim.lwd = 0.05)

netstats$inst
