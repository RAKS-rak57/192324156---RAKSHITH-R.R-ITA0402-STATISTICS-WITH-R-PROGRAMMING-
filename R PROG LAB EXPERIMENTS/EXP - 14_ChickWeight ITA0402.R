# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 14
# CHICKWEIGHT DATASET - SORT, MELT, CAST
# ======================================================================
#
# AIM:
#   To explore and manipulate the ChickWeight dataset (sorting, melting, casting by Diet).
#
# PSEUDOCODE:
#   1. Start
#   2. Load ChickWeight and convert it to a data frame
#   3. Display structure and summary
#   4. Sort by weight in descending order
#   5. Melt the data with id = (Chick, Diet, Time)
#   6. Cast by Diet to find average weight
#   7. Cast by Diet and Time (selected times)
#   8. Stop
#
# SAMPLE INPUT:
#   Dataset: ChickWeight (built-in)
#
# SAMPLE OUTPUT:
#     weight Time Chick Diet
#   1     42    0     1    1
#   2     51    2     1    1
#   3     59    4     1    1
#   4     64    6     1    1
#   5     76    8     1    1
#   6     93   10     1    1
#   'data.frame':	578 obs. of  4 variables:
#    $ weight: num  42 51 59 64 76 93 106 125 149 171 ...
#    $ Time  : num  0 2 4 6 8 10 12 14 16 18 ...
#    $ Chick : Ord.factor w/ 50 levels "18"<"16"<"15"<..: 15 15 15 15 15 15 15 15 15 15 ...
#    $ Diet  : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
#    - attr(*, "formula")=Class 'formula'  language weight ~ Time | Chick
#     .. ..- attr(*, ".Environment")=<environment: R_EmptyEnv> 
#    - attr(*, "outer")=Class 'formula'  language ~Diet
#     .. ..- attr(*, ".Environment")=<environment: R_EmptyEnv> 
#    - attr(*, "labels")=List of 2
#     ..$ x: chr "Time"
#     ..$ y: chr "Body weight"
#    - attr(*, "units")=List of 2
#     ..$ x: chr "(days)"
#     ..$ y: chr "(gm)"
#
#   Top 5 heaviest records:
#       weight Time Chick Diet
#   400    373   21    35    3
#   399    361   20    35    3
#   388    341   21    34    3
#   398    332   18    35    3
#   232    331   21    21    2
#
#   Molten data:
#     Chick Diet Time variable value
#   1     1    1    0   weight    42
#   2     1    1    2   weight    51
#   3     1    1    4   weight    59
#   4     1    1    6   weight    64
#   5     1    1    8   weight    76
#   6     1    1   10   weight    93
#
#   ... (output truncated here, full output appears in R console)
#
# ======================================================================
# PROGRAM
# ======================================================================

library(reshape2)

cw <- as.data.frame(ChickWeight)
print(head(cw))
str(cw)

# Sorting (heaviest first)
cat("\nTop 5 heaviest records:\n")
print(head(cw[order(-cw$weight), ], 5))

# Melting
cw_melt <- melt(cw, id.vars = c("Chick", "Diet", "Time"), measure.vars = "weight")
cat("\nMolten data:\n")
print(head(cw_melt))

# Casting by Diet
cat("\nAverage weight by Diet:\n")
print(dcast(cw_melt, Diet ~ variable, mean))

# Casting by Diet and Time
cat("\nAverage weight by Diet at Time 0, 10, 20:\n")
sub <- cw_melt[cw_melt$Time %in% c(0, 10, 20), ]
tbl <- dcast(sub, Diet ~ Time, mean)
tbl[, -1] <- round(tbl[, -1], 2)
print(tbl)

# ======================================================================
# RESULT:
#   ChickWeight data was sorted, melted and cast by Diet successfully.
# ======================================================================
