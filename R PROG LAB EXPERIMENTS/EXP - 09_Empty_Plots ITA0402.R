# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 9
# EMPTY PLOTS WITH AXIS LIMITS
# ======================================================================
#
# AIM:
#   To create empty plots with specified axis limits.
#
# PSEUDOCODE:
#   1. Start
#   2. Use plot() with type = "n" so nothing is drawn
#   3. Set xlim and ylim to the required limits
#   4. Add labels and title
#   5. Create a second empty plot with different limits
#   6. Stop
#
# SAMPLE INPUT:
#   Plot 1: x = 0 to 10, y = 0 to 50 | Plot 2: x = -5 to 5, y = -10 to 10
#
# SAMPLE OUTPUT:
#   Two empty plots created.
#   [Graph(s) appear in the R Graphics window]
#
# ======================================================================
# PROGRAM
# ======================================================================

par(mfrow = c(1, 2))   # two plots side by side

plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 50),
     xlab = "X axis", ylab = "Y axis", main = "Empty plot 1")

plot(1, type = "n", xlim = c(-5, 5), ylim = c(-10, 10),
     xlab = "X axis", ylab = "Y axis", main = "Empty plot 2")

par(mfrow = c(1, 1))
cat("Two empty plots created.\n")

# ======================================================================
# RESULT:
#   Two empty plots with the specified axis limits were created successfully (see the Graphics window).
# ======================================================================
