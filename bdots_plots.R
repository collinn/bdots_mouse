
library(bdots)
library(ggplot2)
library(gridExtra)

###################################
## Data Introduction and Fitting ##
###################################

mouse <- fread("GJZ16-091.csv")

head(mouse, n = 10)

mousey <- copy(mouse)
mousey$colm <- rnorm(nrow(mousey))

mouse_fit <- bfit(data = mousey,
        subject = "ID",
        time = "Day",
        y = "Volume",
        group = "Treatment",
        curveFun = expCurve())

summary(mouse_fit)

class(mouse_fit)

head(mouse_fit)

pdf("mouse_fit.pdf", width = 6, height = 5)
rr <- plot(mouse_fit[1:4, ])
dev.off()

######################
## Permutation Test ##
######################

mouse_boot <- bboot(Volume ~ Treatment(A, E),
                    mouse_fit, permutation = TRUE)

summary(mouse_boot)

## Modify time scale so that we are only testing in limited range
# This helps with scale of plots when t > 60 (which get very large)
mft <- copy(mouse_fit)
tt <- attr(mft, "time")
tt <- tt[tt < 60]
attributes(mft)$time <- tt

mouse_boot <- bboot(Volume ~ Treatment(A, E),
                        mft, permutation = TRUE)

summary(mouse_boot)
plot(mouse_boot)


pdf("mouse_boot_plot.pdf", width = 6, height = 3.5)
plot(mouse_boot)
dev.off()


#####################
## Refit Procedure ##
#####################

## Jittering parameters should give good illustration of this
mouse_refit <- brefit(mouse_fit, subset = ID == 11)


#####################
## p-value adjust  ##
#####################

i <- 92
set.seed(i)
ts <- diffinv(rnorm(5))
rho <- bdots::ar1Solver(ts)
unadjp <- pt(ts, df = 10)
adjp <- p_adjust(unadjp, method = "oleson", df = 10, rho = rho, alpha = 0.05)

unadjp
adjp