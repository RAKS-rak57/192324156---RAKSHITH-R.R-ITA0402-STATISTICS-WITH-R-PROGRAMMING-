# ===================== QUESTION 1 =====================
# Q1: Multi-factor wildfire readiness alert
options(width = 85)
set.seed(2026)

# ---- Rule parameters (weights are powers of 1/2 so boundary arithmetic is exact) ----
w   <- c(wind = 0.25, humidity = 0.25, rain = 0.125, dryness = 0.375)
SEV <- 80                     # indicator score at or above this is "severe"
CUT <- c(MODERATE = 35, HIGH = 55, EXTREME = 75)   # average-score cut points
OVR_WIND <- 80; OVR_HUM <- 15 # changed condition: very high wind + very low humidity

cnt <- new.env(); cnt$evals <- 0
chk <- function(cond) { cnt$evals <- cnt$evals + 1; cond }   # counts each condition tested

indicator_scores <- function(wind, hum, rain, dry)           # each on a 0-100 danger scale
  c(wind = min(wind, 100), humidity = 100 - hum,
    rain = max(0, 100 - 10 * rain), dryness = dry)

classify_zone <- function(wind, hum, rain, dry, version = "v2") {
  cnt$evals <- 0
  if (version == "v2") {                       # priority 1: override rule (needs only wind + humidity)
    if (chk(!is.na(wind)) && chk(!is.na(hum)) && chk(wind >= OVR_WIND) && chk(hum <= OVR_HUM))
      return(list(class = "EXTREME", avg = NA_real_, n_sev = NA_integer_, evals = cnt$evals, rule = "OVERRIDE"))
  }
  if (chk(any(is.na(c(wind, hum, rain, dry)))))                          # explicit missing-data handling
    return(list(class = "INCOMPLETE", avg = NA_real_, n_sev = NA_integer_, evals = cnt$evals, rule = "MISSING"))
  if (chk(wind < 0 || hum < 0 || hum > 100 || rain < 0 || dry < 0 || dry > 100))
    return(list(class = "INVALID", avg = NA_real_, n_sev = NA_integer_, evals = cnt$evals, rule = "RANGE"))
  s <- indicator_scores(wind, hum, rain, dry)
  avg <- sum(w * s); n_sev <- sum(s >= SEV)
  cl <- if (chk(avg >= CUT["EXTREME"]) || chk(n_sev >= 3)) "EXTREME"
        else if (chk(avg >= CUT["HIGH"]) || chk(n_sev >= 2)) "HIGH"
        else if (chk(avg >= CUT["MODERATE"]) || chk(n_sev >= 1)) "MODERATE"
        else "LOW"
  list(class = cl, avg = avg, n_sev = n_sev, evals = cnt$evals, rule = "HIERARCHY")
}

avg_only <- function(wind, hum, rain, dry) {     # baseline: weighted average alone
  if (any(is.na(c(wind, hum, rain, dry)))) return("INCOMPLETE")
  if (wind < 0 || hum < 0 || hum > 100 || rain < 0 || dry < 0 || dry > 100) return("INVALID")
  a <- sum(w * indicator_scores(wind, hum, rain, dry))
  if (a >= 75) "EXTREME" else if (a >= 55) "HIGH" else if (a >= 35) "MODERATE" else "LOW"
}

process_zones <- function(df, version) {          # for loop over records
  n <- nrow(df); cls <- character(n); ev <- integer(n); avg <- numeric(n); ns <- integer(n); rl <- character(n)
  iterations <- 0
  for (i in seq_len(n)) {
    iterations <- iterations + 1
    r <- classify_zone(df$wind[i], df$hum[i], df$rain[i], df$dry[i], version)
    cls[i] <- r$class; ev[i] <- r$evals; avg[i] <- r$avg; ns[i] <- r$n_sev; rl[i] <- r$rule
  }
  list(data = data.frame(zone = df$zone, class = cls, avg = avg, n_sev = ns, rule = rl, evals = ev,
                         stringsAsFactors = FALSE), iterations = iterations)
}

# ---- Four adversarial datasets (zone, wind km/h, humidity %, rain mm/72h, dryness 0-100) ----
zones <- data.frame(
  zone = c(paste0("A", 1:7), paste0("B", 1:7), paste0("C", 1:8), paste0("D", 1:6)),
  wind = c(10, 30, 50, 70, 5, 25, NA,      95, 5, 90, 60, 100, 0, 95,
           40, 40, 60, 60, 70, 80, 79.9, 40,      100, 120, 85, 75, 100, 90),
  hum  = c(70, 50, 35, 20, 90, 60, 70,     10, 90, 12, 80, 5, 100, 8,
           60, 60, 40, 40, 30, 15, 15.1, 100.5,   0, 0, 10, 12, 0, 14),
  rain = c(5, 2, 0.5, 0, 12, 3, 5,         10, 0, 8, 0, 0, 0, NA,
           7, 7, 5, 5, 2, 5, 5, 7,                0, 0, 0, 1, 15, 9.9),
  dry  = c(20, 50, 70, 90, 5, 40, 20,      5, 100, 10, 90, 0, 100, NA,
           30, 29, 50, 49, 80, 10, 10, 30,        100, 100, 95, 85, 0, 100))
zones$group <- c(rep("A ordinary", 7), rep("B conflicting", 7), rep("C boundary", 8), rep("D combined extreme", 6))

# ---- PREDICTIONS written by hand BEFORE running (see report for the arithmetic) ----
pred_v1 <- c("LOW","MODERATE","HIGH","EXTREME","LOW","MODERATE","INCOMPLETE",
             "HIGH","HIGH","HIGH","HIGH","EXTREME","HIGH","INCOMPLETE",
             "MODERATE","LOW","HIGH","MODERATE","EXTREME","HIGH","MODERATE","INVALID",
             "EXTREME","EXTREME","EXTREME","EXTREME","HIGH","EXTREME")
pred_v2 <- c("LOW","MODERATE","HIGH","EXTREME","LOW","MODERATE","INCOMPLETE",
             "EXTREME","HIGH","EXTREME","HIGH","EXTREME","HIGH","EXTREME",
             "MODERATE","LOW","HIGH","MODERATE","EXTREME","EXTREME","MODERATE","INVALID",
             "EXTREME","EXTREME","EXTREME","EXTREME","EXTREME","EXTREME")
pred_avg <- c("LOW","MODERATE","HIGH","EXTREME","LOW","MODERATE","INCOMPLETE",
              "MODERATE","MODERATE","MODERATE","HIGH","HIGH","MODERATE","INCOMPLETE",
              "MODERATE","LOW","HIGH","MODERATE","EXTREME","MODERATE","MODERATE","INVALID",
              "EXTREME","EXTREME","EXTREME","EXTREME","MODERATE","EXTREME")

r1 <- process_zones(zones, "v1"); r2 <- process_zones(zones, "v2")
obs_avg <- mapply(avg_only, zones$wind, zones$hum, zones$rain, zones$dry)

cat("=== Prediction vs observation: 28 adversarial zones ===\n")
cmp <- data.frame(zone = zones$zone, v1_pred = pred_v1, v1_obs = r1$data$class,
                  v2_pred = pred_v2, v2_obs = r2$data$class, avg_pred = pred_avg, avg_obs = obs_avg)
cmp$ok <- ifelse(cmp$v1_pred == cmp$v1_obs & cmp$v2_pred == cmp$v2_obs & cmp$avg_pred == cmp$avg_obs, "yes", "NO")
print(cmp, row.names = FALSE)
cat("\nMatches  v1:", sum(cmp$v1_pred == cmp$v1_obs), "/ 28 | v2:", sum(cmp$v2_pred == cmp$v2_obs),
    "/ 28 | average-only:", sum(cmp$avg_pred == cmp$avg_obs), "/ 28\n")

cat("\n=== Details: average score, severe-indicator count and rule that fired (v2) ===\n")
det <- data.frame(zone = zones$zone, avg = round(r1$data$avg, 3), n_severe = r1$data$n_sev, rule_v2 = r2$data$rule)
print(det, row.names = FALSE)

cat("\n=== Iterations and condition evaluations (28 zones) ===\n")
cat("v1: iterations =", r1$iterations, "| condition evaluations =", sum(r1$data$evals), "\n")
cat("v2: iterations =", r2$iterations, "| condition evaluations =", sum(r2$data$evals), "\n")
by_grp <- aggregate(cbind(v1 = r1$data$evals, v2 = r2$data$evals) ~ group, data = zones, FUN = sum)
print(by_grp, row.names = FALSE)

cat("\n=== Where the weighted average alone misleads (average-only vs hierarchy) ===\n")
rank <- c(INCOMPLETE = 0, INVALID = 0, LOW = 1, MODERATE = 2, HIGH = 3, EXTREME = 4)
mis <- cmp$avg_obs != cmp$v2_obs & rank[cmp$avg_obs] < rank[cmp$v2_obs]
print(data.frame(zone = cmp$zone[mis], average_only = cmp$avg_obs[mis], v1 = cmp$v1_obs[mis], v2 = cmp$v2_obs[mis]), row.names = FALSE)
cat("Zones under-classified by the average alone:", sum(mis), "of 28\n")

# ---- Changed condition: 25,000 zones ----
cat("\n=== CHANGED CONDITION: 25,000 zones ===\n")
n <- 25000
big <- data.frame(zone = sprintf("Z%05d", 1:n),
                  wind = pmax(0, rnorm(n, 35, 22)), hum = pmin(100, pmax(0, rnorm(n, 50, 22))),
                  rain = rexp(n, 1 / 4), dry = pmin(100, pmax(0, rnorm(n, 50, 25))))
inj <- sample(n, 1000)                                         # force 1,000 dangerous wind + humidity cases
big$wind[inj] <- runif(1000, 80, 110); big$hum[inj] <- runif(1000, 0, 15)
big$rain[sample(n, 500)] <- NA; big$dry[sample(n, 500)] <- NA       # missing measurements
big$wind[sample(n, 250)] <- NA; big$hum[sample(n, 250)] <- NA
t1 <- system.time(b1 <- process_zones(big, "v1"))[["elapsed"]]
t2 <- system.time(b2 <- process_zones(big, "v2"))[["elapsed"]]
cat("Loop iterations: v1 =", b1$iterations, "| v2 =", b2$iterations, "\n")
cat("Elapsed (s): v1 =", round(t1, 2), "| v2 =", round(t2, 2), "(supporting evidence only)\n")
cat("Total condition evaluations: v1 =", sum(b1$data$evals), "| v2 =", sum(b2$data$evals),
    "| mean per zone: v1 =", round(mean(b1$data$evals), 2), "v2 =", round(mean(b2$data$evals), 2), "\n")
lv <- c("LOW", "MODERATE", "HIGH", "EXTREME", "INCOMPLETE", "INVALID")
dist <- rbind(v1 = table(factor(b1$data$class, lv)), v2 = table(factor(b2$data$class, lv)))
cat("\nClass counts:\n"); print(dist)
trig <- which(!is.na(big$wind) & !is.na(big$hum) & big$wind >= OVR_WIND & big$hum <= OVR_HUM)
cat("\nZones meeting the override condition (wind >= 80 and humidity <= 15):", length(trig), "\n")
cat("  classified EXTREME by v1:", sum(b1$data$class[trig] == "EXTREME"), "\n")
cat("  classified EXTREME by v2:", sum(b2$data$class[trig] == "EXTREME"), "(guarantee requires all)\n")
cat("  of those, with a missing rain or dryness value:", sum(is.na(big$rain[trig]) | is.na(big$dry[trig])), "\n")
upgraded <- which(b1$data$class != b2$data$class)
cat("Zones whose class changed from v1 to v2:", length(upgraded), "\n")
print(table(from = b1$data$class[upgraded], to = b2$data$class[upgraded]))
cat("Zones downgraded by v2 (rank lower than v1):", sum(rank[b2$data$class] < rank[b1$data$class]), "\n")
nonov <- setdiff(seq_len(n), trig)
cat("Zones outside the override region: identical class in v1 and v2:", all(b1$data$class[nonov] == b2$data$class[nonov]), "\n")

png("fig/fig_q1_alert.png", width = 1800, height = 900, res = 170)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
pal <- c(LOW = "#a1d99b", MODERATE = "#fdd0a2", HIGH = "#fc9272", EXTREME = "#cb181d", INCOMPLETE = "grey70", INVALID = "grey40")
s <- sample(n, 4000)
plot(big$wind[s], big$hum[s], col = pal[b2$data$class[s]], pch = 16, cex = 0.6, xlab = "Wind speed (km/h)", ylab = "Relative humidity (%)",
     main = "v2 classes; override region boxed", xlim = c(0, 115), ylim = c(0, 100))
rect(OVR_WIND, 0, 115, OVR_HUM, border = "black", lwd = 2, lty = 2)
legend("topright", legend = names(pal)[1:4], col = pal[1:4], pch = 16, cex = 0.7, bty = "n")
barplot(dist[, 1:4], beside = TRUE, col = c("#9ecae1", "#3182bd"), xlab = "Readiness level", ylab = "Number of zones",
        main = "25,000 zones: v1 vs v2")
legend("topright", legend = c("v1 (no override)", "v2 (override)"), fill = c("#9ecae1", "#3182bd"), bty = "n", cex = 0.8)
dev.off()

# ===================== QUESTION 2 =====================
# Q2: Scholarship eligibility and exception engine
options(width = 85)

# ---- Policy as DATA (the engine below never changes when the policy changes) ----
policy_v1 <- list(
  min_avg = 60, min_att = 75, max_band = 3,
  waivers = list(SPORTS = "average", SINGLE_PARENT = "income"),   # provision -> the ONLY condition it waives
  never_waivable = "documents",
  order = c("documents", "income", "average", "attendance"))      # priority order of the checks

check_defs <- list(                                               # relational operators on the application record
  documents  = function(a, p) a$docs == "VERIFIED",
  income     = function(a, p) a$income_band <= p$max_band,
  average    = function(a, p) a$avg >= p$min_avg,
  attendance = function(a, p) a$att >= p$min_att)
fail_code <- c(documents = "DOC_MISSING", income = "INCOME_ABOVE_LIMIT",
               average = "LOW_AVERAGE", attendance = "LOW_ATTENDANCE")

evaluate_application <- function(app, policy) {
  trace <- character()
  bad <- is.na(app$avg) || is.na(app$att) || is.na(app$income_band) || is.na(app$docs) ||
         app$avg < 0 || app$avg > 100 || app$att < 0 || app$att > 100
  if (bad) return(list(status = "REVIEW", reason = "DATA_INVALID", trace = "data:INVALID"))
  provs <- if (app$prov_status == "APPROVED" && nzchar(app$prov)) strsplit(app$prov, ";")[[1]] else character()
  waived <- unique(unlist(policy$waivers[intersect(provs, names(policy$waivers))]))
  waived <- setdiff(waived, policy$never_waivable)                # NOT/AND logic: documents can never be waived
  used <- character()
  for (ck in policy$order) {
    passed <- check_defs[[ck]](app, policy)
    if (!passed && (ck %in% waived)) { trace <- c(trace, paste0(ck, ":WAIVED")); used <- c(used, ck); next }
    if (!passed) {
      trace <- c(trace, paste0(ck, ":FAIL"))
      return(list(status = "REJECTED", reason = unname(fail_code[ck]), trace = paste(trace, collapse = " | ")))
    }
    trace <- c(trace, paste0(ck, ":PASS"))
  }
  list(status = "APPROVED",
       reason = if (length(used) == 0) "ALL_CRITERIA_MET" else paste0("APPROVED_WAIVED:", paste(used, collapse = "+")),
       trace = paste(trace, collapse = " | "))
}

evaluate_all <- function(apps, policy) {                          # loop over application records
  n <- nrow(apps); st <- re <- tr <- character(n)
  for (i in seq_len(n)) {
    r <- evaluate_application(as.list(apps[i, ]), policy)
    st[i] <- r$status; re[i] <- r$reason; tr[i] <- r$trace
  }
  data.frame(id = apps$id, status = st, reason = re, trace = tr, stringsAsFactors = FALSE)
}

# ---- Twelve adversarial profiles ----
apps <- data.frame(stringsAsFactors = FALSE,
  id = paste0("P", 1:12),
  income_band = c(3, 2, 1, 2, 4, 5, 2, 2, 2, NA, 2, 4),
  avg    = c(60.0, 59.99, 98, 80, 55, 35, 55, 80, 85, 80, 55, 70),
  att    = c(75.0, 90, 95, 74.9, 80, 40, 85, 60, 50, 80, 60, 80),
  docs   = c("VERIFIED", "VERIFIED", "MISSING", "VERIFIED", "VERIFIED", "MISSING", "VERIFIED", "VERIFIED", "MISSING", "VERIFIED", "VERIFIED", "VERIFIED"),
  prov   = c("", "", "SPORTS", "", "SPORTS;SINGLE_PARENT", "", "SPORTS", "DISABILITY", "DISABILITY", "", "SPORTS;DISABILITY", ""),
  prov_status = c("NONE", "NONE", "APPROVED", "NONE", "APPROVED", "NONE", "PENDING", "APPROVED", "APPROVED", "NONE", "APPROVED", "NONE"))
apps$income_band[10] <- 3; apps$avg[10] <- NA              # P10: missing academic average
cat("--- Applications ---\n"); print(apps, row.names = FALSE)

# ---- PREDICTIONS (hand-traced before execution) ----
pred_v1 <- data.frame(id = apps$id, stringsAsFactors = FALSE,
  status = c("APPROVED","REJECTED","REJECTED","REJECTED","APPROVED","REJECTED","REJECTED","REJECTED","REJECTED","REVIEW","REJECTED","REJECTED"),
  reason = c("ALL_CRITERIA_MET","LOW_AVERAGE","DOC_MISSING","LOW_ATTENDANCE","APPROVED_WAIVED:income+average","DOC_MISSING",
             "LOW_AVERAGE","LOW_ATTENDANCE","DOC_MISSING","DATA_INVALID","LOW_ATTENDANCE","INCOME_ABOVE_LIMIT"))
pred_v2 <- pred_v1
pred_v2[8, c("status", "reason")]  <- c("APPROVED", "APPROVED_WAIVED:attendance")
pred_v2[11, c("status", "reason")] <- c("APPROVED", "APPROVED_WAIVED:average+attendance")

v1 <- evaluate_all(apps, policy_v1)
cat("\n=== Baseline policy v1: prediction vs observation ===\n")
cmp1 <- data.frame(id = v1$id, predicted = paste(pred_v1$status, pred_v1$reason, sep = "/"),
                   observed = paste(v1$status, v1$reason, sep = "/"),
                   ok = ifelse(pred_v1$status == v1$status & pred_v1$reason == v1$reason, "yes", "NO"))
cmp1$predicted <- sub("APPROVED_WAIVED:", "WAIVED:", cmp1$predicted); cmp1$observed <- sub("APPROVED_WAIVED:", "WAIVED:", cmp1$observed)
print(cmp1, row.names = FALSE)
cat("Matches:", sum(cmp1$ok == "yes"), "/ 12\n")

cat("\n=== Condition traces (v1) ===\n")
for (i in seq_len(nrow(v1))) cat(sprintf("%-4s %-9s %-30s %s\n", v1$id[i], v1$status[i], v1$reason[i], v1$trace[i]))
cat("Conditions evaluated per profile:", paste(sapply(strsplit(v1$trace, " \\| "), length), collapse = " "), "\n")

# ---- Effect of CONDITION ORDER ----
cat("\n=== Condition-order experiment ===\n")
pol_rev <- policy_v1; pol_rev$order <- c("attendance", "average", "income", "documents")
ord_tab <- data.frame(id = c("P3", "P6", "P9"),
  docs_first = evaluate_all(apps[c(3, 6, 9), ], policy_v1)$reason,
  docs_last  = evaluate_all(apps[c(3, 6, 9), ], pol_rev)$reason,
  steps_docs_first = sapply(strsplit(evaluate_all(apps[c(3, 6, 9), ], policy_v1)$trace, " \\| "), length),
  steps_docs_last  = sapply(strsplit(evaluate_all(apps[c(3, 6, 9), ], pol_rev)$trace, " \\| "), length))
ord_tab$predicted_docs_last <- c("DOC_MISSING", "LOW_ATTENDANCE", "DOC_MISSING")   # written before running
ord_tab$prediction_ok <- ifelse(ord_tab$predicted_docs_last == ord_tab$docs_last, "yes", "NO")
print(ord_tab[, c("id", "docs_first", "docs_last", "predicted_docs_last", "prediction_ok")], row.names = FALSE)
cat("Steps (conditions evaluated): docs first =", paste(ord_tab$steps_docs_first, collapse = " "), "| docs last =", paste(ord_tab$steps_docs_last, collapse = " "), "\n")

naive_provision_first <- function(app, policy) {      # FLAWED ordering: an approved provision short-circuits everything
  if (app$prov_status == "APPROVED" && nzchar(app$prov)) return(list(status = "APPROVED", reason = "PROVISION_APPROVED"))
  evaluate_application(app, policy)
}
nv <- sapply(seq_len(nrow(apps)), function(i) naive_provision_first(as.list(apps[i, ]), policy_v1)$status)
chg <- which(nv != v1$status)
cat("\nFlawed 'provision first' ordering changes the STATUS of", length(chg), "profiles:\n")
print(data.frame(id = apps$id[chg], correct = v1$status[chg], provision_first = nv[chg]), row.names = FALSE)

# ---- POLICY CHANGE: waive attendance for DISABILITY only; documents never waived ----
policy_v2 <- policy_v1
policy_v2$waivers$DISABILITY <- "attendance"             # the ONLY edit: one line of policy data
v2 <- evaluate_all(apps, policy_v2)
cat("\n=== Policy v2 (DISABILITY waives attendance): prediction vs observation ===\n")
cmp2 <- data.frame(id = v2$id, predicted = paste(pred_v2$status, pred_v2$reason, sep = "/"),
                   observed = paste(v2$status, v2$reason, sep = "/"),
                   ok = ifelse(pred_v2$status == v2$status & pred_v2$reason == v2$reason, "yes", "NO"))
cmp2$predicted <- sub("APPROVED_WAIVED:", "WAIVED:", cmp2$predicted); cmp2$observed <- sub("APPROVED_WAIVED:", "WAIVED:", cmp2$observed)
print(cmp2, row.names = FALSE)
cat("Matches:", sum(cmp2$ok == "yes"), "/ 12\n")
cat("\n=== Before / after the policy change ===\n")
ba <- data.frame(id = apps$id, before = sub("APPROVED_WAIVED:", "WAIVED:", v1$reason), after = sub("APPROVED_WAIVED:", "WAIVED:", v2$reason))
ba$changed <- ifelse(v1$status != v2$status | v1$reason != v2$reason, "CHANGED", "")
print(ba, row.names = FALSE)
cat("Profiles changed:", sum(ba$changed == "CHANGED"), "of 12 | approvals before:", sum(v1$status == "APPROVED"),
    "after:", sum(v2$status == "APPROVED"), "\n")
cat("Edits needed: engine functions changed = 0; policy lines added = 1\n")

cat("\n=== Safeguard: even a mistaken policy entry cannot waive documents ===\n")
policy_bad <- policy_v2; policy_bad$waivers$DISABILITY <- c("attendance", "documents")
sg <- evaluate_all(apps[c(9), ], policy_bad)
cat("P9 (DISABILITY, documents MISSING) under the mistaken policy ->", sg$status, sg$reason, "\n")

png("fig/fig_q2_policy.png", width = 1700, height = 850, res = 170)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
tb <- rbind(v1 = table(factor(v1$status, c("APPROVED", "REJECTED", "REVIEW"))), v2 = table(factor(v2$status, c("APPROVED", "REJECTED", "REVIEW"))))
barplot(tb, beside = TRUE, col = c("#9ecae1", "#3182bd"), ylab = "Number of profiles", xlab = "Final status", main = "Outcomes before and after the change")
legend("topleft", legend = c("policy v1", "policy v2"), fill = c("#9ecae1", "#3182bd"), bty = "n", cex = 0.8)
len1 <- sapply(strsplit(v1$trace, " \\| "), length)
barplot(len1, names.arg = apps$id, col = "#a1d99b", ylab = "Conditions evaluated (same in v1 and v2)", xlab = "Application profile", main = "Condition trace length", las = 2, cex.names = 0.8)
dev.off()

# ===================== QUESTION 3 =====================
# Q3: Menu-driven online order analytics utility
options(width = 85)

log_env <- new.env()
reset_log <- function() { log_env$calls <- character(); log_env$helper <- 0 }
reset_log()
log_call <- function(name) log_env$calls <- c(log_env$calls, name)

# ---- Shared helper: ALL missing-value / type handling lives here (no duplication) ----
clean_amounts <- function(x, na.rm = TRUE) {
  log_env$helper <- log_env$helper + 1
  if (!is.numeric(x)) stop("amounts must be numeric")
  if (na.rm) x <- x[!is.na(x)]
  x
}

# ---- Modular operations (net revenue: refunds are negative amounts and reduce the total) ----
total_revenue <- function(x, na.rm = TRUE) { log_call("total_revenue"); sum(clean_amounts(x, na.rm)) }
mean_order    <- function(x, na.rm = TRUE) { log_call("mean_order"); v <- clean_amounts(x, na.rm); if (length(v) == 0) NA_real_ else mean(v) }
median_order  <- function(x, na.rm = TRUE) { log_call("median_order"); v <- clean_amounts(x, na.rm); if (length(v) == 0) NA_real_ else median(v) }
order_range   <- function(x, na.rm = TRUE) { log_call("order_range"); v <- clean_amounts(x, na.rm); if (length(v) == 0) c(NA_real_, NA_real_) else range(v) }
sd_order      <- function(x, na.rm = TRUE) { log_call("sd_order"); v <- clean_amounts(x, na.rm); if (length(v) < 2) NA_real_ else sd(v) }
count_refunds <- function(x, na.rm = TRUE) { log_call("count_refunds"); sum(clean_amounts(x, na.rm) < 0) }
count_above   <- function(x, thr, na.rm = TRUE) { log_call("count_above"); sum(clean_amounts(x, na.rm) > thr) }
count_missing <- function(x) { log_call("count_missing"); sum(is.na(x)) }

# ---- Menu, version 1: switch() selection ----
menu_labels_v1 <- c("1" = "Total revenue (net)", "2" = "Mean order value", "3" = "Median order value",
                    "4" = "Range", "5" = "Standard deviation", "6" = "Count of refunds",
                    "7" = "Count above threshold", "8" = "Number of missing values", "0" = "Exit")
dispatch_v1 <- function(choice, x, thr)
  switch(choice, "1" = total_revenue(x), "2" = mean_order(x), "3" = median_order(x), "4" = order_range(x),
         "5" = sd_order(x), "6" = count_refunds(x), "7" = count_above(x, thr), "8" = count_missing(x), NULL)

run_menu <- function(x, choices = NULL, thr = 40, dispatch = dispatch_v1, labels = menu_labels_v1, verbose = TRUE) {
  reset_log(); iter <- 0; invalid <- 0; exit_reason <- "INPUT_END"; outputs <- list()
  repeat {                                                       # loop for repeated menu requests
    if (is.null(choices)) { ch <- readline("Choose an option: ") }
    else { if (iter >= length(choices)) break; ch <- as.character(choices[iter + 1]) }
    iter <- iter + 1
    if (is.na(ch) || !nzchar(ch)) { invalid <- invalid + 1; if (verbose) cat("[", iter, "] invalid (empty)\n"); next }
    if (ch == "0") { exit_reason <- "USER_EXIT"; if (verbose) cat("[", iter, "] exit\n"); break }
    res <- dispatch(ch, x, thr)
    if (is.null(res)) { invalid <- invalid + 1; if (verbose) cat("[", iter, "] invalid option:", ch, "\n"); next }
    outputs[[length(outputs) + 1]] <- res
    if (verbose) cat("[", iter, "]", labels[ch], "->", paste(round(res, 4), collapse = " "), "\n")
  }
  list(iterations = iter, invalid = invalid, calls = log_env$calls, helper_calls = log_env$helper,
       exit = exit_reason, outputs = outputs)
}

# ---- Edge-case datasets with PREDICTED results (hand-calculated before running; threshold = 40) ----
ds <- list(
  repeated  = c(50, 50, 50, 50),
  refunds   = c(100, -20, 50, -5, 30),
  outliers  = c(20, 25, 22, 24, 5000),
  missing   = c(10, NA, 30, NA, 50),
  one_obs   = c(75),
  empty     = numeric(0),
  all_na    = c(NA_real_, NA_real_))
pred <- list(                      # total, mean, median, min, max, sd, refunds, above 40, missing
  repeated = c(200, 50, 50, 50, 50, 0, 0, 4, 0),
  refunds  = c(155, 31, 30, -20, 100, 47.4868, 2, 2, 0),
  outliers = c(5091, 1018.2, 24, 20, 5000, 2225.9, 0, 1, 0),
  missing  = c(90, 30, 30, 10, 50, 20, 0, 1, 2),
  one_obs  = c(75, 75, 75, 75, 75, NA, 0, 1, 0),
  empty    = c(0, NA, NA, NA, NA, NA, 0, 0, 0),
  all_na   = c(0, NA, NA, NA, NA, NA, 0, 0, 2))
obs_row <- function(x) { r <- order_range(x); c(total_revenue(x), mean_order(x), median_order(x), r[1], r[2],
                         sd_order(x), count_refunds(x), count_above(x, 40), count_missing(x)) }
op_names <- c("total", "mean", "median", "min", "max", "sd", "refunds", ">40", "missing")
cat("=== Predicted vs observed results for 7 edge-case datasets (9 values each) ===\n")
tab <- do.call(rbind, lapply(names(ds), function(nm) {
  o <- obs_row(ds[[nm]]); p <- pred[[nm]]
  same <- (is.na(o) & is.na(p)) | (!is.na(o) & !is.na(p) & abs(o - p) <= 0.05)
  data.frame(dataset = nm, matched = sum(same), of = 9, mismatches = if (all(same)) "-" else paste(op_names[!same], collapse = ","))
}))
print(tab, row.names = FALSE)
cat("Total matched:", sum(tab$matched), "of", sum(tab$of), "\n")
cat("\nDetail for the refunds dataset (observed): "); print(round(setNames(obs_row(ds$refunds), op_names), 4))
cat("Detail for the outliers dataset (observed): "); print(round(setNames(obs_row(ds$outliers), op_names), 2))
cat("Observed vs predicted for empty / one-observation / all-NA inputs follow the documented rules:\n")
print(rbind(empty = setNames(obs_row(ds$empty), op_names), one_obs = setNames(obs_row(ds$one_obs), op_names),
            all_na = setNames(obs_row(ds$all_na), op_names)))
bad <- tryCatch(total_revenue(c("a", "b")), error = function(e) conditionMessage(e))
cat("\nNon-numeric input -> error:", bad, "\n")

# ---- Menu sessions: predicted path vs observed ----
cat("\n=== Menu sessions (predicted before running) ===\n")
sessA <- c("1", "3", "x", "5", "", "0")
cat("Session A:", paste(shQuote(sessA), collapse = " "), "\n")
rA <- run_menu(ds$refunds, sessA)
cat("Predicted: iterations 6, calls 3 (total_revenue, median_order, sd_order), invalid 2, exit USER_EXIT\n")
cat("Observed : iterations", rA$iterations, ", calls", length(rA$calls), "(", paste(rA$calls, collapse = ", "), "), invalid", rA$invalid, ", exit", rA$exit, "\n")
sessB <- c("9", "10", "0")
cat("\nSession B before extension:", paste(shQuote(sessB), collapse = " "), "\n")
rB <- run_menu(ds$refunds, sessB)
cat("Predicted: iterations 3, calls 0, invalid 2\n")
cat("Observed : iterations", rB$iterations, ", calls", length(rB$calls), ", invalid", rB$invalid, "\n")
sessC <- c("2", "6")
cat("\nSession C (no exit option typed):\n")
rC <- run_menu(ds$missing, sessC)
cat("Predicted: iterations 2, calls 2, exit INPUT_END\n")
cat("Observed : iterations", rC$iterations, ", calls", length(rC$calls), ", exit", rC$exit, "\n")
sessD <- c("-1", "2.5", "abc", "08", " ")
rD <- run_menu(ds$repeated, sessD, verbose = FALSE)
cat("\nSession D invalid-only", paste(shQuote(sessD), collapse = " "), ": predicted invalid 5, calls 0; observed invalid", rD$invalid, ", calls", length(rD$calls), "\n")

# ---- Extension: add two operations and evaluate duplication ----
cat("\n=== Extension: two new operations (9 refund value, 10 trimmed mean) ===\n")
refund_value <- function(x, na.rm = TRUE) { log_call("refund_value"); v <- clean_amounts(x, na.rm); sum(v[v < 0]) }
trimmed_mean <- function(x, na.rm = TRUE, trim = 0.1) { log_call("trimmed_mean"); v <- clean_amounts(x, na.rm); if (length(v) == 0) NA_real_ else mean(v, trim = trim) }
before_lines <- length(deparse(dispatch_v1)) + length(menu_labels_v1)
dispatch_v1b <- function(choice, x, thr)
  switch(choice, "1" = total_revenue(x), "2" = mean_order(x), "3" = median_order(x), "4" = order_range(x),
         "5" = sd_order(x), "6" = count_refunds(x), "7" = count_above(x, thr), "8" = count_missing(x),
         "9" = refund_value(x), "10" = trimmed_mean(x), NULL)
labels_v1b <- c(menu_labels_v1, "9" = "Total refund value", "10" = "Trimmed mean (10%)")
cat("Places edited per new operation in the switch design: 3 (function, switch case, menu label)\n")
rB2 <- run_menu(ds$refunds, sessB, dispatch = dispatch_v1b, labels = labels_v1b)
cat("Session B after extension -> iterations", rB2$iterations, ", calls", length(rB2$calls), ", invalid", rB2$invalid, "\n")

# ---- Maintainability v2: registry-driven menu (the loop and dispatcher never change) ----
registry <- list(
  "1" = list(label = "Total revenue (net)", fun = function(x, thr) total_revenue(x)),
  "2" = list(label = "Mean order value",    fun = function(x, thr) mean_order(x)),
  "3" = list(label = "Median order value",  fun = function(x, thr) median_order(x)),
  "4" = list(label = "Range",               fun = function(x, thr) order_range(x)),
  "5" = list(label = "Standard deviation",  fun = function(x, thr) sd_order(x)),
  "6" = list(label = "Count of refunds",    fun = function(x, thr) count_refunds(x)),
  "7" = list(label = "Count above threshold", fun = function(x, thr) count_above(x, thr)),
  "8" = list(label = "Number of missing values", fun = function(x, thr) count_missing(x)),
  "9" = list(label = "Total refund value",  fun = function(x, thr) refund_value(x)),
  "10" = list(label = "Trimmed mean (10%)", fun = function(x, thr) trimmed_mean(x)))
dispatch_reg <- function(choice, x, thr) { e <- registry[[choice]]; if (is.null(e)) NULL else e$fun(x, thr) }
labels_reg <- c(setNames(sapply(registry, function(e) e$label), names(registry)), "0" = "Exit")
same <- sapply(names(ds), function(nm) {
  a <- run_menu(ds[[nm]], as.character(1:10), dispatch = dispatch_v1b, labels = labels_v1b, verbose = FALSE)$outputs
  b <- run_menu(ds[[nm]], as.character(1:10), dispatch = dispatch_reg, labels = labels_reg, verbose = FALSE)$outputs
  isTRUE(all.equal(a, b)) })
cat("Registry menu gives identical outputs for all 10 operations on all 7 datasets:", all(same), "\n")
registry[["11"]] <- list(label = "Largest order", fun = function(x, thr) { log_call("max_order"); max(clean_amounts(x)) })
r11 <- run_menu(ds$refunds, c("11", "0"), dispatch = dispatch_reg, labels = c(labels_reg, "11" = "Largest order"))
cat("Adding operation 11 to the registry: 1 registry entry, 0 changes to run_menu() or dispatch_reg(); result:", r11$outputs[[1]], "\n")

ops <- list(total_revenue, mean_order, median_order, order_range, sd_order, count_refunds, count_above, count_missing, refund_value, trimmed_mean)
nm <- c("total_revenue", "mean_order", "median_order", "order_range", "sd_order", "count_refunds", "count_above", "count_missing", "refund_value", "trimmed_mean")
src <- sapply(ops, function(f) paste(deparse(f), collapse = " "))
dup <- data.frame(operation = nm, uses_clean_amounts = grepl("clean_amounts\\(", src), own_is_na_handling = grepl("is\\.na", src),
                  own_empty_check = grepl("length\\(v\\)", src))
print(dup, row.names = FALSE)
cat("Operations with their own NA-handling code:", sum(dup$own_is_na_handling), "(count_missing needs is.na by definition)\n")

# ---- Session with call and iteration log ----
set.seed(5)
long <- c(sample(as.character(1:8), 25, TRUE), "x", "99", "0")
rL <- run_menu(ds$refunds, long, verbose = FALSE)
cat("\nLong scripted session: iterations =", rL$iterations, "| operation calls =", length(rL$calls),
    "| invalid =", rL$invalid, "| helper (clean_amounts) calls =", rL$helper_calls, "| exit =", rL$exit, "\n")
cc <- table(factor(rL$calls, levels = c("total_revenue", "mean_order", "median_order", "order_range", "sd_order", "count_refunds", "count_above", "count_missing")))
print(cc)
png("fig/fig_q3_calls.png", width = 1700, height = 850, res = 170)
par(mar = c(8, 5, 4, 1))
barplot(cc, las = 2, col = "#6baed6", ylab = "Function calls", main = "Function calls in a 28-iteration scripted session", cex.names = 0.8)
dev.off()

# ===================== QUESTION 4 =====================
# Q4: Iterative package-length validation routine
options(width = 85)
set.seed(44)
LO <- 1.0; HI <- 120.0; MAX_ATTEMPTS <- 3      # declared limits and retry policy
# Final dispositions: ACCEPTED, ACCEPTED_AFTER_CORRECTION, REJECTED_RETRY_LIMIT, REJECTED_NO_DATA

ev <- new.env(); ev$n <- 0
ck <- function(cond) { ev$n <- ev$n + 1; cond }           # counts each condition evaluated

# Version 1: missing -> blank -> numeric -> lower limit -> upper limit
check_v1 <- function(raw, lo = LO, hi = HI) {
  if (ck(is.na(raw))) return("MISSING")
  if (ck(!nzchar(trimws(as.character(raw))))) return("MISSING")
  num <- suppressWarnings(as.numeric(raw))
  if (ck(is.na(num))) return("NON_NUMERIC")
  if (ck(num < lo)) return("BELOW_MIN")
  if (ck(num > hi)) return("ABOVE_MAX")
  "OK"
}
# Version 2: parse once, test the common (valid) case first, classify the reason only on failure
check_v2 <- function(raw, lo = LO, hi = HI) {
  num <- suppressWarnings(as.numeric(raw))
  if (ck(!is.na(num)) && ck(num >= lo) && ck(num <= hi)) return("OK")
  if (ck(!is.na(num))) return(if (ck(num < lo)) "BELOW_MIN" else "ABOVE_MAX")
  if (ck(is.na(raw)) || ck(!nzchar(trimws(as.character(raw))))) "MISSING" else "NON_NUMERIC"
}

validate_sequence <- function(readings, checker, max_attempts = MAX_ATTEMPTS) {
  stopifnot(max_attempts >= 1, LO < HI)
  ev$n <- 0
  attempts <- 0; failures <- 0; i <- 1; passes <- 0; status <- NA_character_; value <- NA_real_
  reasons <- character()
  while (is.na(status)) {                                  # terminates: i and attempts increase every pass
    passes <- passes + 1
    if (attempts >= max_attempts) { status <- "REJECTED_RETRY_LIMIT"; next }
    if (i > length(readings))     { status <- "REJECTED_NO_DATA"; next }
    raw <- readings[[i]]; i <- i + 1; attempts <- attempts + 1
    r <- checker(raw)
    if (r == "OK") {
      value <- as.numeric(raw)
      status <- if (failures == 0) "ACCEPTED" else "ACCEPTED_AFTER_CORRECTION"
    } else { failures <- failures + 1; reasons <- c(reasons, r) }
  }
  list(status = status, attempts = attempts, failures = failures, passes = passes,
       unread = length(readings) - (i - 1), evals = ev$n, value = value, reasons = reasons)
}

# ---- Eleven test sequences with PREDICTIONS (status, attempts, failures) written before running ----
seqs <- list(
  S1_valid_first   = list("45.5"),
  S2_invalid_valid = list(150, 80),
  S3a_low_endpoint = list(1.0),
  S3b_high_endpoint = list(120.0),
  S3c_edges_then_ok = list(0.99, 120.01, 120.0),
  S4_alternating   = list("x", 60, "y", 70),
  S5a_repeat_bad   = list(NA, "", "abc", "5"),
  S5b_missing_only = list(NA, NA),
  S6_empty         = list(),
  S7_duplicates    = list(42, 42, 42),
  S8_limit_numeric = list(0, 130, "abc", 50))
pred <- data.frame(seq = names(seqs),
  status = c("ACCEPTED", "ACCEPTED_AFTER_CORRECTION", "ACCEPTED", "ACCEPTED", "ACCEPTED_AFTER_CORRECTION",
             "ACCEPTED_AFTER_CORRECTION", "REJECTED_RETRY_LIMIT", "REJECTED_NO_DATA", "REJECTED_NO_DATA",
             "ACCEPTED", "REJECTED_RETRY_LIMIT"),
  attempts = c(1, 2, 1, 1, 3, 2, 3, 2, 0, 1, 3),
  failures = c(0, 1, 0, 0, 2, 1, 3, 2, 0, 0, 3), stringsAsFactors = FALSE)

obs1 <- lapply(seqs, validate_sequence, checker = check_v1)
obs2 <- lapply(seqs, validate_sequence, checker = check_v2)
cmp <- data.frame(seq = names(seqs),
  pred = paste(sub("ACCEPTED_AFTER_CORRECTION", "ACC_CORR", sub("REJECTED_", "REJ_", pred$status)), pred$attempts, pred$failures, sep = " "),
  obs = sapply(obs1, function(o) paste(sub("ACCEPTED_AFTER_CORRECTION", "ACC_CORR", sub("REJECTED_", "REJ_", o$status)), o$attempts, o$failures)),
  stringsAsFactors = FALSE, row.names = NULL)
cmp$ok <- ifelse(cmp$pred == cmp$obs, "yes", "NO")
cat("=== Prediction vs observation (status attempts failures; ACC_CORR = accepted after correction) ===\n")
print(cmp, row.names = FALSE)
cat("Matches:", sum(cmp$ok == "yes"), "of", nrow(cmp), "\n")

cat("\n=== Measured iterations and condition evaluations (v1 = original order, v2 = redesigned) ===\n")
m <- data.frame(seq = names(seqs), attempts = sapply(obs1, `[[`, "attempts"), loop_passes = sapply(obs1, `[[`, "passes"),
                unread = sapply(obs1, `[[`, "unread"), evals_v1 = sapply(obs1, `[[`, "evals"), evals_v2 = sapply(obs2, `[[`, "evals"), row.names = NULL)
print(m, row.names = FALSE)
cat("Totals: attempts =", sum(m$attempts), "| loop passes =", sum(m$loop_passes), "| evaluations v1 =", sum(m$evals_v1), "| v2 =", sum(m$evals_v2), "\n")
same_out <- all(mapply(function(a, b) identical(a$status, b$status) && a$attempts == b$attempts && a$failures == b$failures &&
                                   identical(a$reasons, b$reasons), obs1, obs2))
cat("v1 and v2 give identical status, attempts, failures and failure reasons on all 11 sequences:", same_out, "\n")
cat("Failure reasons recorded (v1):\n")
for (nm in names(seqs)) if (length(obs1[[nm]]$reasons)) cat(sprintf("  %-18s %s\n", nm, paste(obs1[[nm]]$reasons, collapse = ", ")))

cat("\n=== Evaluations per single reading, by reading type ===\n")
types <- list(valid = "50", NA_value = NA, blank = "", text = "abc", below = 0.5, above = 200)
per <- data.frame(reading = names(types), v1 = NA_integer_, v2 = NA_integer_, same_result = NA)
for (k in seq_along(types)) {
  ev$n <- 0; r1 <- check_v1(types[[k]]); per$v1[k] <- ev$n
  ev$n <- 0; r2 <- check_v2(types[[k]]); per$v2[k] <- ev$n
  per$same_result[k] <- r1 == r2
}
per$saving <- per$v1 - per$v2
print(per, row.names = FALSE)

cat("\n=== Simulation: 5,000 sequences per invalid-rate (up to 4 readings each) ===\n")
make_reading <- function(p) {
  if (runif(1) >= p) return(round(runif(1, LO, HI), 1))
  switch(sample(5, 1, prob = c(.25, .15, .20, .20, .20)), NA, "", "abc", round(runif(1, -20, LO - 0.1), 1), round(runif(1, HI + 0.1, 300), 1))
}
rates <- c(0, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95)
sim <- do.call(rbind, lapply(rates, function(p) {
  e1 <- e2 <- att <- 0; agree <- TRUE
  for (s in 1:5000) {
    rd <- replicate(4, make_reading(p), simplify = FALSE)
    a <- validate_sequence(rd, check_v1); b <- validate_sequence(rd, check_v2)
    e1 <- e1 + a$evals; e2 <- e2 + b$evals; att <- att + a$attempts
    if (a$status != b$status || a$attempts != b$attempts || a$failures != b$failures) agree <- FALSE
  }
  data.frame(invalid_rate = p, evals_per_attempt_v1 = round(e1 / att, 3), evals_per_attempt_v2 = round(e2 / att, 3), outcomes_identical = agree)
}))
print(sim, row.names = FALSE)
cat("Rate at which v2 stops being cheaper (interpolated):",
    round(approx(sim$evals_per_attempt_v2 - sim$evals_per_attempt_v1, sim$invalid_rate, xout = 0)$y, 2), "\n")

png("fig/fig_q4_evals.png", width = 1700, height = 900, res = 170)
par(mar = c(5, 5, 4, 1))
plot(sim$invalid_rate, sim$evals_per_attempt_v1, type = "b", pch = 16, col = "#3182bd", ylim = c(2.5, 5), lwd = 2,
     xlab = "Probability that a reading is invalid", ylab = "Condition evaluations per reading",
     main = "Validation order: v1 (original) vs v2 (redesigned)")
lines(sim$invalid_rate, sim$evals_per_attempt_v2, type = "b", pch = 17, col = "#e6550d", lwd = 2)
legend("topright", legend = c("v1 missing > blank > numeric > range", "v2 parse once, valid case first"), col = c("#3182bd", "#e6550d"), pch = c(16, 17), lwd = 2, bty = "n", cex = 0.85)
dev.off()

# ===================== QUESTION 5 =====================
# Q5: Recursive analysis of a nested file-and-folder archive
options(width = 85)
# MODEL: a folder is an R list(); a file is any NON-list element (here a character string).
# Folders may be empty, nested to any depth, and may contain NULL entries (broken links) that count as nothing.

calls <- new.env(); calls$n <- 0; calls$max_depth <- 0
count_files <- function(node, depth = 1) {
  calls$n <- calls$n + 1                                  # every call (file, folder, NULL) is counted
  if (depth > calls$max_depth) calls$max_depth <- depth
  if (is.null(node)) return(0)                            # base case 1: broken link, nothing to count
  if (!is.list(node)) return(1)                           # base case 2: a file leaf counts as one
  if (length(node) == 0) return(0)                        # base case 3: empty folder contributes zero
  total <- 0
  for (child in node) total <- total + count_files(child, depth + 1)   # recursive case: sum over children
  total
}
count_files_naive <- function(node) {                     # first draft WITHOUT the NULL base case
  if (!is.list(node)) return(1)
  if (length(node) == 0) return(0)
  total <- 0; for (child in node) total <- total + count_files_naive(child); total
}
run_rec <- function(tree) { calls$n <- 0; calls$max_depth <- 0; f <- count_files(tree); list(files = f, calls = calls$n, depth = calls$max_depth) }

# ---- Iterative alternative: explicit stack (pop one node at a time) ----
count_files_stack <- function(root) {
  stack <- list(root); files <- 0; pops <- 0
  while (length(stack) > 0) {
    node <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL; pops <- pops + 1
    if (is.null(node)) next
    if (!is.list(node)) files <- files + 1
    else if (length(node) > 0) stack <- c(stack, node)
  }
  list(files = files, pops = pops)
}

# ---- Test structures ----
chain <- list("deep.txt"); for (i in 1:4) chain <- list(chain)       # 5 nested folders, 1 file
tests <- list(
  T1_shallow    = list("a.txt", "b.txt", "c.txt"),
  T2_deep_chain = chain,
  T3_unequal    = list(docs = list("a", "b", list("c", list("d"))), img = list("e"), "readme"),
  T4_empty      = list(empty1 = list(), a = list("x", list()), "y"),
  T5_single     = list("only.txt"),
  T6_null_link  = list("a", NULL),
  T7_all_empty  = list())
# PREDICTIONS (counted by hand before running): files, calls = number of nodes visited, depth
pred <- data.frame(test = names(tests), files = c(3, 1, 6, 2, 1, 1, 0), calls = c(4, 6, 11, 6, 2, 3, 1), depth = c(2, 6, 6, 4, 2, 2, 1))
obs <- do.call(rbind, lapply(tests, function(t) as.data.frame(run_rec(t))))
cmp <- data.frame(test = names(tests), pred_files = pred$files, obs_files = obs$files, pred_calls = pred$calls, obs_calls = obs$calls,
                  pred_depth = pred$depth, obs_depth = obs$depth, row.names = NULL)
cmp$ok <- ifelse(cmp$pred_files == cmp$obs_files & cmp$pred_calls == cmp$obs_calls & cmp$pred_depth == cmp$obs_depth, "yes", "NO")
cat("=== Recursive count: predicted vs observed (files, recursive calls, maximum depth) ===\n")
print(cmp, row.names = FALSE)
cat("Matches:", sum(cmp$ok == "yes"), "of", nrow(cmp), "\n")

cat("\n=== Base-case diagnosis: draft without the NULL base case ===\n")
naive <- sapply(tests, function(t) tryCatch(count_files_naive(t), error = function(e) NA))
print(data.frame(test = names(tests), correct = obs$files, naive_draft = naive, row.names = NULL), row.names = FALSE)

cat("\n=== Recursive vs iterative stack: same answers and same number of visits ===\n")
st <- do.call(rbind, lapply(tests, function(t) as.data.frame(count_files_stack(t))))
print(data.frame(test = names(tests), rec_files = obs$files, stack_files = st$files, rec_calls = obs$calls, stack_pops = st$pops,
                 equal = obs$files == st$files & obs$calls == st$pops, row.names = NULL), row.names = FALSE)

# ---- Effect of depth and branching: complete trees ----
make_tree <- function(d, b) if (d == 0) "f" else lapply(seq_len(b), function(i) make_tree(d - 1, b))
grid <- data.frame(depth = c(2, 3, 3, 4, 5, 2, 6), branching = c(2, 2, 3, 3, 2, 10, 2))
grid$pred_files <- grid$branching^grid$depth
grid$pred_calls <- (grid$branching^(grid$depth + 1) - 1) / (grid$branching - 1)
res <- lapply(seq_len(nrow(grid)), function(i) run_rec(make_tree(grid$depth[i], grid$branching[i])))
grid$obs_files <- sapply(res, `[[`, "files"); grid$obs_calls <- sapply(res, `[[`, "calls"); grid$max_depth <- sapply(res, `[[`, "depth")
grid$ok <- ifelse(grid$pred_files == grid$obs_files & grid$pred_calls == grid$obs_calls, "yes", "NO")
cat("\n=== Complete trees: formulas files = b^d and calls = (b^(d+1)-1)/(b-1), predicted before running ===\n")
print(grid, row.names = FALSE)
cat("Calls grow with the NUMBER OF NODES (branching); maximum stack depth grows with DEPTH only.\n")

# ---- Depth limit of recursion vs iteration ----
build_chain <- function(d) { x <- "leaf.txt"; for (i in seq_len(d)) x <- list(x); x }
depths <- c(100, 250, 500, 750, 1000, 1500, 5000, 20000)
dl <- do.call(rbind, lapply(depths, function(d) {
  tr <- build_chain(d)
  r <- tryCatch({ calls$n <- 0; calls$max_depth <- 0; count_files(tr); "ok" }, error = function(e) substr(conditionMessage(e), 1, 40))
  s <- tryCatch({ count_files_stack(tr)$files; "ok" }, error = function(e) substr(conditionMessage(e), 1, 40))
  data.frame(chain_depth = d, recursive = r, stack_iterative = s)
}))
cat("\n=== Chain of nested folders: recursion versus the explicit stack ===\n")
print(dl, row.names = FALSE)
ok_depths <- depths[dl$recursive == "ok"]
cat("Deepest chain the recursive version handled in this test:", max(ok_depths), "\n")

# ---- Timing on a larger complete tree (supporting evidence) ----
big <- make_tree(8, 3)                                   # 3^8 = 6,561 files, 9,841 nodes
t_rec <- median(replicate(3, system.time(count_files(big))[["elapsed"]]))
t_stk <- median(replicate(3, system.time(count_files_stack(big))[["elapsed"]]))
cat("\nComplete tree depth 8, branching 3:", run_rec(big)$files, "files,", run_rec(big)$calls, "calls\n")
cat("Elapsed (median of 3): recursive =", round(t_rec, 3), "s | stack =", round(t_stk, 3), "s\n")

png("fig/fig_q5_growth.png", width = 1800, height = 850, res = 170)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
plot(grid$pred_calls, grid$obs_calls, pch = 16, col = "#3182bd", cex = 1.4, log = "xy", xlab = "Predicted calls (formula)", ylab = "Observed calls",
     main = "Calls follow the node-count formula"); abline(0, 1, lty = 2)
plot(grid$depth, grid$max_depth, pch = 17, col = "#e6550d", cex = 1.4, xlab = "Tree depth d", ylab = "Maximum recursion depth observed",
     main = "Recursion depth tracks tree depth"); abline(1, 1, lty = 2)
dev.off()

# ===================== QUESTION 6 =====================
# Q6: Iterative vs recursive evaluation of a geometric series
options(width = 85)
# DEFINITION. First term a, common ratio r (any real number), number of terms n (integer, n >= 0).
#   term recurrence : t_1 = a,  t_k = r * t_{k-1}              (so t_k = a * r^(k-1))
#   partial sum     : S_0 = 0 (empty sum),  S_n = a + r * S_{n-1}   for n >= 1
#   closed form     : S_n = a * (1 - r^n) / (1 - r)  for r != 1 ;  S_n = n * a  for r = 1
# Base case: n = 0 gives 0. Each recursive call reduces n by one, so it reaches the base case for every valid n.

geo_iter <- function(n, a, r) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || n != floor(n)) stop("n must be a whole number >= 0")
  total <- 0; term <- a; iters <- 0
  for (k in seq_len(n)) { total <- total + term; term <- term * r; iters <- iters + 1 }
  list(sum = total, count = iters)
}
rc <- new.env(); rc$calls <- 0
geo_rec_inner <- function(n, a, r) {
  rc$calls <- rc$calls + 1
  if (n == 0) return(0)                         # base case
  a + r * geo_rec_inner(n - 1, a, r)            # recursive case
}
geo_rec <- function(n, a, r) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || n != floor(n)) stop("n must be a whole number >= 0")
  rc$calls <- 0; s <- geo_rec_inner(n, a, r); list(sum = s, count = rc$calls)
}
geo_closed <- function(n, a, r) if (r == 1) n * a else a * (1 - r^n) / (1 - r)

# ---- Tests: PREDICTIONS (sum, iterations, recursive calls) written before running ----
tests <- data.frame(case = c("n=0", "n=1", "n=5", "n=10", "n=100", "n=1000 (r=0.5)", "r=1, n=5", "r=-1, n=5", "r=-1, n=6", "r=0, n=5", "r=2, n=10", "r=2, n=20"),
                    n = c(0, 1, 5, 10, 100, 1000, 5, 5, 6, 5, 10, 20),
                    a = c(1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1),
                    r = c(.5, .5, .5, .5, .5, .5, 1, -1, -1, 0, 2, 2), stringsAsFactors = FALSE)
tests$pred_sum   <- c(0, 1, 1.9375, 1.998046875, 2, 2, 15, 1, 0, 1, 1023, 1048575)
tests$pred_iter  <- tests$n
tests$pred_calls <- tests$n + 1
obs <- lapply(seq_len(nrow(tests)), function(i) list(it = geo_iter(tests$n[i], tests$a[i], tests$r[i]), re = tryCatch(geo_rec(tests$n[i], tests$a[i], tests$r[i]), error = function(e) NULL)))
tests$iter_sum <- sapply(obs, function(o) o$it$sum); tests$iter_count <- sapply(obs, function(o) o$it$count)
tests$rec_sum <- sapply(obs, function(o) if (is.null(o$re)) NA else o$re$sum); tests$rec_calls <- sapply(obs, function(o) if (is.null(o$re)) NA else o$re$count)
tests$closed <- mapply(geo_closed, tests$n, tests$a, tests$r)
cat("=== Predicted vs observed sums ===\n")
print(data.frame(case = tests$case, predicted = tests$pred_sum, iterative = tests$iter_sum, recursive = tests$rec_sum, closed_form = tests$closed,
                 sums_ok = ifelse(is.na(tests$rec_sum), "RECURSION FAILED", ifelse(abs(tests$pred_sum - tests$iter_sum) < 1e-9 & abs(tests$pred_sum - tests$rec_sum) < 1e-9, "yes", "NO"))), row.names = FALSE)
cat("\n=== Predicted vs observed iterations and recursive calls ===\n")
print(data.frame(case = tests$case, pred_iter = tests$pred_iter, obs_iter = tests$iter_count, pred_calls = tests$pred_calls, obs_calls = tests$rec_calls,
                 counts_ok = ifelse(is.na(tests$rec_calls), "RECURSION FAILED", ifelse(tests$pred_iter == tests$iter_count & tests$pred_calls == tests$rec_calls, "yes", "NO"))), row.names = FALSE)
cat("\nLargest |iterative - recursive| over these tests:", format(max(abs(tests$iter_sum - tests$rec_sum), na.rm = TRUE), digits = 3), "\n")
cat("Largest |iterative - closed form|:", format(max(abs(tests$iter_sum - tests$closed)), digits = 3), "\n")

cat("\n=== Increasing term counts (a=1, r=0.5): counts grow linearly ===\n")
ns <- c(0, 1, 10, 100, 500, 750)
inc <- data.frame(n = ns, iterations = sapply(ns, function(n) geo_iter(n, 1, .5)$count), rec_calls = sapply(ns, function(n) geo_rec(n, 1, .5)$count),
                  sum_iter = sapply(ns, function(n) geo_iter(n, 1, .5)$sum), sum_rec = sapply(ns, function(n) geo_rec(n, 1, .5)$sum))
inc$abs_diff <- abs(inc$sum_iter - inc$sum_rec)
print(inc, digits = 10, row.names = FALSE)

cat("\n=== Ratio boundary cases ===\n")
cat("r = 1 : closed form (1-r^n)/(1-r) would be 0/0, so the n*a branch gives", geo_closed(5, 1, 1), "(handled by the n*a branch); iterative n=5, a=3 gives", geo_iter(5, 3, 1)$sum, "\n")
cat("r = -1: alternating partial sums 1,0,1,0 -> n=5:", geo_iter(5, 1, -1)$sum, " n=6:", geo_iter(6, 1, -1)$sum, " (no convergence, no error)\n")
cat("|r| < 1 converges to a/(1-r) = 2 for r = 0.5; S_100 =", geo_iter(100, 1, .5)$sum, "\n")
ov <- sapply(c(1023, 1024, 1025), function(n) geo_iter(n, 1, 2)$sum)
cat("r = 2 overflow boundary, S_n for n = 1023, 1024, 1025:", format(ov, digits = 4), "(n = 1024 overflows double precision to Inf)\n")
cat("Recursive version at the same boundary (n = 1024) -> ", tryCatch(format(geo_rec(1024, 1, 2)$sum), error = function(e) "ERROR: recursion limit reached before the numbers overflow"), "\n")

cat("\n=== Termination and depth ===\n")
for (bad in list(-1, 2.5, NA)) cat("geo_rec(", format(bad), ") ->", tryCatch({ geo_rec(bad, 1, .5); "accepted" }, error = function(e) conditionMessage(e)), "\n")
unguarded <- function(n) if (n == 0) 0 else 1 + unguarded(n - 1)
cat("Unguarded version with n = -1 ->", tryCatch(unguarded(-1), error = function(e) paste("never reaches the base case:", substr(conditionMessage(e), 1, 45))), "\n")
ds <- c(500, 750, 800, 900, 1000, 2000)
dd <- data.frame(n = ds, recursive = sapply(ds, function(n) tryCatch({ geo_rec(n, 1, .5); "ok" }, error = function(e) "FAILED (stack limit)")),
                 iterative = sapply(ds, function(n) tryCatch({ geo_iter(n, 1, .5); "ok" }, error = function(e) "FAILED")))
print(dd, row.names = FALSE)
cat("Iterative version at n = 1e6:", format(geo_iter(1e6, 1, .5)$sum), "with", geo_iter(1e6, 1, .5)$count, "iterations\n")

cat("\n=== Elapsed time (supporting evidence): R 4.3.3, single core, median of 5 repeats, a=1 r=0.5 ===\n")
tm <- function(f, reps) median(replicate(5, system.time(for (j in seq_len(reps)) f())[["elapsed"]])) / reps * 1e6
tt <- data.frame(n = c(10, 100, 500, 750))
tt$iterative_us <- sapply(tt$n, function(n) tm(function() geo_iter(n, 1, .5), 2000))
tt$recursive_us <- sapply(tt$n, function(n) tm(function() geo_rec(n, 1, .5), 2000))
tt$ratio <- round(tt$recursive_us / tt$iterative_us, 1)
tt$iterative_us <- round(tt$iterative_us, 1); tt$recursive_us <- round(tt$recursive_us, 1)
print(tt, row.names = FALSE)

png("fig/fig_q6_compare.png", width = 1800, height = 850, res = 170)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
plot(inc$n, inc$rec_calls, type = "b", pch = 17, col = "#e6550d", lwd = 2, xlab = "Number of terms n", ylab = "Count",
     main = "Work grows linearly in n")
lines(inc$n, inc$iterations, type = "b", pch = 16, col = "#3182bd", lwd = 2)
legend("topleft", legend = c("recursive calls (n+1)", "loop iterations (n)"), col = c("#e6550d", "#3182bd"), pch = c(17, 16), lwd = 2, bty = "n", cex = 0.8)
plot(tt$n, tt$recursive_us, type = "b", pch = 17, col = "#e6550d", lwd = 2, xlab = "Number of terms n", ylab = "Microseconds per evaluation",
     main = "Elapsed time (supporting evidence)", ylim = c(0, max(tt$recursive_us) * 1.1))
lines(tt$n, tt$iterative_us, type = "b", pch = 16, col = "#3182bd", lwd = 2)
legend("topleft", legend = c("recursive", "iterative"), col = c("#e6550d", "#3182bd"), pch = c(17, 16), lwd = 2, bty = "n", cex = 0.8)
dev.off()

# ===================== QUESTION 7 =====================
# Q7: Diagnose - Redesign - Validate review of the Q5 file counter
options(width = 85)
# ================= A. DIAGNOSE (reuses count_files and count_files_stack from Q5) =================
cat("=== A1. Defect 1: recursion fails on deep folders (10 trials per depth, same calling context) ===\n")
chain_of <- function(d) { x <- "leaf.txt"; for (i in seq_len(d)) x <- list(x); x }
rec_ok <- function(d) tryCatch({ calls$n <- 0; calls$max_depth <- 0; count_files(chain_of(d)); TRUE }, error = function(e) FALSE)
depths_a <- c(500, 700, 800, 900, 1000, 1500, 3000)
trial <- data.frame(chain_depth = depths_a, recursive_ok = sapply(depths_a, function(d) sum(replicate(10, rec_ok(d)))))
trial$result <- paste0(trial$recursive_ok, "/10 succeed")
print(trial[, c("chain_depth", "result")], row.names = FALSE)
always_fail <- min(depths_a[trial$recursive_ok == 0]); always_ok <- max(depths_a[trial$recursive_ok == 10 & depths_a < always_fail])
cat("Always succeeds up to depth", always_ok, "| always fails from depth", always_fail, "| unstable in between (depends on calling context and garbage collection)\n")
msg <- tryCatch(count_files(chain_of(always_fail)), error = function(e) conditionMessage(e))
cat("Error text at depth", always_fail, ":", substr(msg, 1, 60), "\n")
cat("Root cause: each nesting level adds R function-call frames on the C stack; the 8 MB limit is reached long before memory runs out.\n")

cat("\n=== A2. Defect 2: the stack version slows down quadratically on wide folders ===\n")
wide_of <- function(n) as.list(sprintf("f%d.txt", seq_len(n)))
sizes <- c(5000, 10000, 20000, 40000)
wd <- do.call(rbind, lapply(sizes, function(n) {
  tr <- wide_of(n)
  data.frame(children = n, stack_s = system.time(count_files_stack(tr))[["elapsed"]], recursive_s = system.time({ calls$n <- 0; count_files(tr) })[["elapsed"]])
}))
wd$stack_growth_vs_previous <- c(NA, round(wd$stack_s[-1] / pmax(wd$stack_s[-nrow(wd)], 0.001), 1))
print(wd, row.names = FALSE)
cat("Doubling the folder size multiplies the stack time by about 4 (quadratic); the recursive loop stays linear.\n")
cat("Root cause: stack[[length(stack)]] <- NULL builds a new, shorter list on every pop, copying the whole stack.\n")

# ================= B. EVALUATION CRITERIA =================
cat("\n=== B. Criteria (measurable) ===\n")
crit <- data.frame(id = paste0("K", 1:5), criterion = c("Correctness", "Depth safety", "Operation count", "Scalability on wide folders", "Maintainability"),
  target = c("Same file count on 100% of test and random trees", "Handles chain depth >= 100,000", "Nodes visited equal recursive calls",
             "Time grows less than x3 when size doubles", "Code at most 20 lines, base cases explicit"))
print(crit, right = FALSE, row.names = FALSE)

# ================= C. REDESIGN =================
cat("\n=== C. Redesign: process the tree LEVEL BY LEVEL (iterative, vectorised) ===\n")
cat("Pseudocode:\n  level <- [root]; files <- 0; visited <- 0\n  while level is not empty:\n    visited += size(level)\n    files   += count of entries that are neither NULL nor a folder\n    level    = concatenation of the children of every folder in level\n  return files, visited\n")
count_files_levels <- function(root) {
  level <- list(root); files <- 0; visited <- 0; passes <- 0
  while (length(level) > 0) {
    passes <- passes + 1; visited <- visited + length(level)
    is_null <- vapply(level, is.null, logical(1)); is_folder <- vapply(level, is.list, logical(1))
    files <- files + sum(!is_null & !is_folder)
    level <- unlist(level[is_folder], recursive = FALSE, use.names = FALSE)
  }
  list(files = files, visited = visited, passes = passes)
}
cat("Lines of code: recursive =", length(deparse(count_files)), "| stack =", length(deparse(count_files_stack)), "| level-wise =", length(deparse(count_files_levels)), "\n")
cat("Trade-off: the level-wise version loses the one-to-one match with the recursive definition and keeps a whole level in memory at once.\n")

# ================= D. VALIDATE with equivalent inputs =================
cat("\n=== D1. Correctness on the 7 Q5 structures and 300 random trees ===\n")
q5 <- sapply(tests, function(t) count_files_levels(t)$files); q5r <- sapply(tests, function(t) run_rec(t)$files)
cat("Q5 structures: level-wise equals recursive on", sum(q5 == q5r), "of", length(tests), "\n")
gen_tree <- function(depth) {
  if (depth == 0 || runif(1) < 0.25) return(if (runif(1) < 0.1) NULL else sprintf("f%d", sample(1e6, 1)))
  k <- sample(0:4, 1); lapply(seq_len(k), function(i) gen_tree(depth - 1))
}
set.seed(77); agree <- 0; same_visits <- 0; total_nodes <- 0
for (i in 1:300) {
  tr <- list(gen_tree(10), gen_tree(10), gen_tree(10))
  a <- run_rec(tr); b <- count_files_stack(tr); cc <- count_files_levels(tr)
  agree <- agree + (a$files == b$files && a$files == cc$files)
  same_visits <- same_visits + (a$calls == b$pops && a$calls == cc$visited)
  total_nodes <- total_nodes + a$calls
}
cat("Random trees (300, with empty folders and NULL links, ", total_nodes, " nodes in total):\n", sep = "")
cat("  all three methods give the same file count on", agree, "of 300\n")
cat("  recursive calls = stack pops = level-wise visits on", same_visits, "of 300\n")

cat("\n=== D2. Depth safety ===\n")
dd <- c(750, 1000, 20000, 100000)
dtab <- do.call(rbind, lapply(dd, function(d) { tr <- chain_of(d)
  data.frame(chain_depth = d, recursive = paste0(sum(replicate(5, rec_ok(d))), "/5 ok"), stack = tryCatch({ count_files_stack(tr)$files; "ok" }, error = function(e) "FAILED"),
             level_wise = tryCatch({ r <- count_files_levels(tr); paste0("ok (", r$files, " file, ", r$passes, " passes)") }, error = function(e) "FAILED")) }))
print(dtab, row.names = FALSE)

cat("\n=== D3. Wide folders (equivalent inputs; seconds) ===\n")
sizes2 <- c(5000, 10000, 20000, 40000, 160000)
wt <- do.call(rbind, lapply(sizes2, function(n) { tr <- wide_of(n)
  data.frame(children = n, recursive = system.time({ calls$n <- 0; count_files(tr) })[["elapsed"]],
             stack = if (n <= 40000) system.time(count_files_stack(tr))[["elapsed"]] else NA,
             level_wise = system.time(count_files_levels(tr))[["elapsed"]]) }))
print(wt, row.names = FALSE)
r160 <- count_files_levels(wide_of(160000))
cat("Level-wise result for 160,000 files:", r160$files, "files,", r160$visited, "nodes visited,", r160$passes, "passes\n")

cat("\n=== D4. Complete trees (balanced, equivalent inputs; seconds, median of 3) ===\n")
tt <- do.call(rbind, lapply(c(6, 7, 8, 9), function(d) { tr <- make_tree(d, 4)
  data.frame(depth = d, nodes = (4^(d + 1) - 1) / 3,
    recursive = median(replicate(3, system.time({ calls$n <- 0; count_files(tr) })[["elapsed"]])),
    stack = median(replicate(3, system.time(count_files_stack(tr))[["elapsed"]])),
    level_wise = median(replicate(3, system.time(count_files_levels(tr))[["elapsed"]]))) }))
print(tt, row.names = FALSE)

cat("\n=== Scorecard against the criteria (computed from the measurements above) ===\n")
growth <- function(col) wt[[col]][4] / wt[[col]][3]            # time at 40,000 / time at 20,000 children
lines_of <- c(rec = length(deparse(count_files)), stack = length(deparse(count_files_stack)), level = length(deparse(count_files_levels)))
verdict <- function(ok) ifelse(ok, "PASS", "FAIL")
sc <- data.frame(id = crit$id, criterion = crit$criterion,
  recursive = c(verdict(agree == 300), verdict(grepl("^5/5", dtab$recursive[4])), verdict(same_visits == 300), verdict(growth("recursive") < 3), verdict(lines_of["rec"] <= 20)),
  naive_stack = c(verdict(agree == 300), verdict(dtab$stack[4] == "ok"), verdict(same_visits == 300), verdict(growth("stack") < 3), verdict(lines_of["stack"] <= 20)),
  level_wise = c(verdict(agree == 300 && all(q5 == q5r)), verdict(grepl("ok", dtab$level_wise[4])), verdict(same_visits == 300), verdict(growth("level_wise") < 3), verdict(lines_of["level"] <= 20)))
print(sc, right = FALSE, row.names = FALSE)
cat("Growth factor when the folder doubles from 20,000 to 40,000 children: recursive", round(growth("recursive"), 2), "| stack", round(growth("stack"), 2), "| level-wise", round(growth("level_wise"), 2), "\n")
cat("Lines of code: recursive", lines_of["rec"], "| stack", lines_of["stack"], "| level-wise", lines_of["level"], "\n")

png("fig/fig_q7_redesign.png", width = 1800, height = 850, res = 170)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
plot(wt$children, wt$recursive, type = "b", pch = 16, col = "#3182bd", lwd = 2, log = "xy", xlab = "Children in one folder", ylab = "Seconds (log scale)",
     main = "Wide folders: time vs size", ylim = c(0.0005, 10))
lines(wt$children, wt$stack, type = "b", pch = 15, col = "#e6550d", lwd = 2)
lines(wt$children, wt$level_wise, type = "b", pch = 17, col = "#31a354", lwd = 2)
legend("topleft", legend = c("naive stack", "recursive", "level-wise"), col = c("#e6550d", "#3182bd", "#31a354"), pch = c(15, 16, 17), lwd = 2, bty = "n", cex = 0.8)
barplot(c(recursive = always_ok, stack = 100000, level_wise = 100000), log = "y", ylim = c(100, 300000), col = c("#3182bd", "#e6550d", "#31a354"),
        ylab = "Deepest chain handled (log scale)", main = "Depth safety (tested up to 100,000)", cex.names = 0.8)
dev.off()

