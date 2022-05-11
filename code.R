############
## Code 1 ##
############

# Install necessary packages if not yet done so
install.packages(
    c("mice", "VIM", "MASS"),
    dependencies = T
)

# Load the mice package (suppress both warnings and messages)
suppressWarnings(suppressMessages(
    library(mice)
))





############
## Code 2 ##
############

# Generate MCAR, MAR and MNAR
logistic <- function(x) exp(x) / (1 + exp(x))
set.seed(80122)
n <- 300
y <- MASS::mvrnorm(
    n = n,
    mu = c(0, 0),
    Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)
)
r2.mcar <- 1 - rbinom(n, 1, 0.5)
r2.mar <- 1 - rbinom(n, 1, logistic(y[, 1]))
r2.mnar <- 1 - rbinom(n, 1, logistic(y[, 2]))

# Plot MCAR, MAR and MNAR
library(lattice)
y1 <- y[, 1]
y2 <- y[, 2]
y3 <- rbind(y, y, y)
r2 <- c(r2.mcar, r2.mar, r2.mnar)
r2 <- factor(r2, labels = c("Ymis", "Yobs"))
typ <- factor(rep(3:1, each = n), labels = c("MNAR", "MAR", "MCAR"))
d <- data.frame(y1 = y3[, 1], y2 = y3[, 2], r2 = r2, typ = typ)
trellis.par.set(box.rectangle = list(col = c(mdc(2), mdc(1)), lwd = 1.2))
trellis.par.set(box.umbrella = list(col = c(mdc(2), mdc(1)), lwd = 1.2))
trellis.par.set(plot.symbol = list(col = mdc(3), lwd = 1))
tp <- bwplot(r2 ~ y2 | typ,
    data = d,
    horizontal = TRUE, layout = c(1, 3),
    xlab = expression(Y[2]),
    col = c(mdc(2), mdc(1)), strip = FALSE, xlim = c(-3, 3),
    strip.left = strip.custom(bg = "grey95")
)
print(tp)





############
## Code 3 ##
############

# Use the example dataset nhanes (came with mice package)
nhanes
# observations = 25, variables = 4

# Variable names
#    age     age group               ordered categorical
#    bmi     body mass index         numerical
#    hyp     hypertension status     binary
#    chl     cholesterol level       numerical


# Inspect missing pattern (Method 1: Visual)
md.pattern(nhanes)

# Colour convention
#   blue    observed
#   red     missing

# Interpretation
#   13 rows are complete
#   3 row that chl is missing
#   1 row that bmi is missing
#   1 row that both hyp and bmi are missing
#   7 rows that only age is observed
#
#   Total number of missing values = 3x1 + 1x1 + 1x2 + 7x3 = 27
#   Most missing values (10) occur in chl


# Inspection by variable pairs (Method 2: Tabulation)
md.pairs(nhanes)

# Symbol convention (left, top)
#   r   observed (remain?)
#   m   missing

# Interpretation (focus on (bmi, chl) pair)
#   ($rr) 13 completely observed paris
#   ($rm) 3 pairs: bmi is observed but chl is missing
#   ($mr) 2 pairs: bmi is missing but chl is observed
#   ($mm) 7 pairs: both bmi and chl are missing





############
## Code 4 ##
############

# Margin plot
par(mar = c(7, 7, 3, 3)) # In order to show the axes labels

# Inspect data range
c(min(nhanes$chl, na.rm = T), max(nhanes$chl, na.rm = T)) # (113, 284)
c(min(nhanes$bmi, na.rm = T), max(nhanes$bmi, na.rm = T)) # (20.4, 35.3)

# Generate margin plot
VIM::marginplot(nhanes[, c("chl", "bmi")],
    xlim = c(110, 290), ylim = c(20, 36),
    col = mdc(1:2), pch = 19,
    cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3
)

# Interpretation
#   red 9   variable bmi contains 9 missings
#   red 10  variable chl contains 10 missings
#   red 7   The variable pair (bmi, chl) contains 7 missings
#
#   three red dots on the left: bmi values are known but chl missing
#   two red dots on the bottom: chl values are known but bmi missing
#   red dot cross between bmi and chl actually represents 7 dots
#
#   Total # dots = 13 (blue) + 3 (red left) + 2 (red bottom) + 7 (red cross) = 25
#   Box plots: marginal distributions (blue = obs, red = mis)
#
#   If MCAR => red and blue box plots are expected to be identical





############
## Code 5 ##
############

# Impute missing data
imp <- mice(nhanes, printFlag = F, seed = 23109)

# The multiply imputed dataset, imp, is of class mids (MI data set)
print(imp)





############
## Code 6 ##
############

# Diagnostic check
# Recall that bmi contains 9 missings
# The MI procedure produced five guesses for each missing:
imp$imp$bmi

# The 1st complete dataset combines the observed and imputed values:
complete(imp)

# We can print out the 2nd set of the complete dataset
complete(imp, 2)

# If complete to start with => identical in all five sets
# If missing to start with => differ in each set
# Degree of difference reflects degree of uncertainty





############
## Code 7 ##
############

# Visual inspection (big picture)
stripplot(imp, pch = 20, cex = 1.2)

# Colour convention
#   blue    observed
#   red     imputed

# Each x-axis marker is one version of MI
# 0 = original set

# Red points follow the blue points reasonably well, including the gaps in the distribution.





############
## Code 8 ##
############

# Visual inspection (fine details)
xyplot(imp, bmi ~ chl | .imp, pch = 20, cex = 1.4)

# Red points have more or less the same shape as blue data => imputed data could have been plausible measurements if they had not been missing

# Differences between the red points represents uncertainty about the true, but unknown, values





############
## Code 9 ##
############

# Analysing Imputed Datasets

# Original regression:
#   lm(chl ~ age + bmi)

# Repeat this analysis to each version of MI
fit <- with(
    data = imp,
    exp = lm(chl ~ age + bmi)
)

# Pool the multiple versions of the analyses together
summary(pool(fit))

# Both age and bmi are significant at .05 level


# If we increase m, the number of imputations, significant levels may change
summary(pool(with(
    mice(nhanes, m = 10, printFlag = F, seed = 23109), # order 10 sets of MI
    lm(chl ~ age + bmi)
))) # More significant





#############
## Code 10 ##
#############

# Specifying Imputation Methods

summary(pool(with(
    mice(nhanes,
        method = c("", "norm", "pmm", "mean"), # Specify MI method for each var
        m = 10, print = F, seed = 23109 # printFlag = print = pri
    ),
    lm(chl ~ age + bmi)
)))

summary(pool(with(
    mice(nhanes,
        method = "norm", # Use norm MI method for all variables
        m = 10, pri = F, seed = 23109
    ),
    lm(chl ~ age + bmi)
)))

# In order to show case different imputation methods, use nhanes2 dataset
str(nhanes2)

# Data type
#   age     factor, 3 levels
#   bmi     numeric
#   hyp     factor, 2 levels
#   chl     numeric

summary(pool(with(
    mice(nhanes2,
        me = c("polyreg", "pmm", "logreg", "norm"), # me = method
        m = 10, pri = F, seed = 23109
    ),
    lm(chl ~ age + bmi)
)))





#############
## Code 11 ##
#############

# Inspect original data
head(popmis)

# Extract predictor matrix
suppressWarnings(suppressMessages(
    ini <- mice(popmis, maxit = 0)
))
pred <- ini$pred

# Turn every entry in the predictor matrix to zero
pred <- matrix(0,
    nrow = nrow(pred), ncol = ncol(pred),
    dimnames = list(rownames(pred), colnames(pred))
)

# Assign info to the "popular" row
pred["popular", ] <- c(0, -2, 0, 2, 1, 2, 0)

# MI
suppressWarnings(suppressMessages(
    imp <- mice(popmis,
        meth = c("", "", "2l.norm", "", "", "", ""),
        pred = pred, pri = F, maxit = 1, seed = 71152
    )
))

# Inspect imputed data
head(complete(imp))
