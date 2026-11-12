# ===================== TASK 1 =====================
# TASK 1: Enquiry experiments behind the architecture blueprint
options(width = 85)

cat("=== EQ1: How does representation affect coercion? ===\n")
mixed <- list(num_chr = c(85.5, "A"), num_lgl = c(85.5, TRUE),
              int_num = c(5L, 2.5),   lgl_chr = c(TRUE, "Pass"),
              int_lgl = c(3L, FALSE))
coercion_tab <- data.frame(
  expression   = names(mixed),
  result_class = sapply(mixed, class),
  result_value = sapply(mixed, function(v) paste(v, collapse = " | ")),
  row.names = NULL)
print(coercion_tab)
rec <- list(reg_no = "192324001", attendance = 85.5, eligible = TRUE)
cat("\nThe same values inside a list keep their own types:\n")
print(sapply(rec, class))

cat("\n=== EQ2: How does representation affect indexing? ===\n")
marks_vec <- c(S1 = 78, S2 = 65, S3 = 90)
marks_mat <- matrix(c(78, 65, 90, 55, 70, 82), nrow = 3,
                    dimnames = list(c("S1", "S2", "S3"), c("MA", "CS")))
cat("Vector by name   marks_vec['S2']       :", marks_vec["S2"], "\n")
cat("Matrix by name   marks_mat['S2','CS']  :", marks_mat["S2", "CS"], "\n")
cat("marks_mat[1, ] class                   :", class(marks_mat[1, ]), "\n")
cat("marks_mat[1, , drop=FALSE] class       :", class(marks_mat[1, , drop = FALSE]), "\n")

cat("\n=== EQ3: How does representation affect validation? ===\n")
att <- c(92, 88, NA, 105, -4, 76)
cat("mean(att)               :", mean(att), "\n")
cat("mean(att, na.rm = TRUE) :", mean(att, na.rm = TRUE),
    "(silently includes 105 and -4)\n")
ok <- !is.na(att) & att >= 0 & att <= 100
cat("validated mean          :", mean(att[ok]), "| rejected positions:",
    paste(which(!ok), collapse = ", "), "\n")

cat("\n=== EQ4: How does representation affect updating? ===\n")
prog <- factor(c("AI&DS", "CSE", "AI&DS"), levels = c("AI&DS", "CSE"))
res <- withCallingHandlers({ prog[2] <- "ECE"; prog },
  warning = function(w) { cat("Warning:", conditionMessage(w), "\n")
                          invokeRestart("muffleWarning") })
cat("Factor after assigning an unknown level:\n"); print(res)
f <- factor(c("10", "20", "30", "20"))
cat("as.numeric(factor)               :", as.numeric(f), "\n")
cat("as.numeric(as.character(factor)) :", as.numeric(as.character(f)), "\n")

cat("\n=== Design criteria used as acceptance criteria ===\n")
design_criteria <- data.frame(
  id = paste0("C", 1:6),
  criterion = c("Type safety", "Validity", "Interpretability",
                "Round-trip integrity", "Scalability", "Maintainability"),
  acceptance_test = c(
    "No silent coercion; all type defects detected",
    "All invalid / out-of-range records flagged",
    "All dimensions named; lookup by ID works",
    "matrix/data frame/array conversions lossless",
    "100,000 students: < 10 s per op, < 1 GB",
    "Update = 1 validated call; add component, no rewrite"))
print(design_criteria, right = FALSE)

# ===================== TASK 2 =====================
# TASK 2: Student core-data representation
options(width = 85)
valid_programmes <- c("AI&DS", "CSE", "IT", "ECE")

reg_no     <- sprintf("1923240%02d", 1:12)
name       <- c("Aarav Kumar", "Bhavya Rao", "Charan Teja", "Divya Nair",
                "Eshwar Pillai", "Farhan Ali", "Gayathri S", "Harini M",
                "Ishaan Verma", "Jaya Lakshmi", "Karthik R", "Lavanya P")
programme  <- c("AI&DS", "AI&DS", "CSE", "IT", "ECE", "AI&DS",
                "CSE", "IT", "AI&DS", "ECE", "CSE", "AI&DS")
attendance <- c(92, 88, 74.5, 96, 81, 69, 90, 75, 85.5, 78, 100, 83)
eligible   <- attendance >= 75

cat("--- Naming and type-consistency rules (schema) ---\n")
schema <- data.frame(
  attribute = c("reg_no", "name", "programme", "attendance", "eligible"),
  r_type    = c("character", "character", "character", "numeric", "logical"),
  rule      = c("9 digits, kept as text (IDs are labels, never arithmetic)",
                "non-empty text", "one of AI&DS/CSE/IT/ECE",
                "percentage 0-100", "TRUE if attendance >= 75"))
print(schema, right = FALSE)

cat("\n--- Alternative representations of ATTENDANCE ---\n")
att_chr <- c("92%", "100%", "75%", "68.5%", "88%")
att_num <- c(92, 100, 75, 68.5, 88)
attended <- c(46L, 50L, 38L, 34L, 44L); total <- 50L
cat("A) character  sort():", sort(att_chr), "\n")
cat("B) numeric    sort():", sort(att_num), "\n")
cat("A) '100%' >= '75%' ?", "100%" >= "75%", " (wrong: text comparison)\n")
cat("B) 100 >= 75 ?      ", 100 >= 75, "\n")
mA <- tryCatch(mean(att_chr),
               warning = function(w) paste("WARNING ->", conditionMessage(w)))
cat("A) mean(character)  :", mA, "\n")
cat("B) mean(numeric)    :", mean(att_num), "\n")
cat("C) attended/total*100 (two integer vectors):",
    attended / total * 100, "\n")
set.seed(1); big <- round(runif(1e5, 50, 100), 1)
cat("Memory for 100,000 values -> character:",
    format(object.size(as.character(big)), units = "Kb"),
    "| numeric:", format(object.size(big), units = "Kb"), "\n")

alt <- data.frame(
  option = c("A character '92%'", "B numeric 92", "C integer pair"),
  arith_ok = c("No", "Yes", "Yes (derived)"), compare_ok = c("No", "Yes", "Yes (derived)"),
  n_vectors = c(1, 1, 2), effort = c("High", "Low", "Medium"))
print(alt, right = FALSE)
cat("DECISION: B (numeric percentage). A fails arithmetic and comparison;",
    "C needs two synchronised vectors.\n")

validate_core <- function(reg_no, name, attendance, programme,
                          valid_programmes = c("AI&DS", "CSE", "IT", "ECE")) {
  issues <- data.frame(row = integer(), field = character(),
                       problem = character(), stringsAsFactors = FALSE)
  add <- function(rows, field, problem)
    if (length(rows) > 0)
      issues <<- rbind(issues, data.frame(row = rows, field = field,
                                          problem = problem))
  if (!is.character(reg_no))
    add(NA_integer_, "reg_no", paste("type is", class(reg_no)[1]))
  reg_chr <- as.character(reg_no)
  add(which(!grepl("^[0-9]{9}$", reg_chr)), "reg_no", "not a 9-digit ID")
  add(which(duplicated(reg_chr)), "reg_no", "duplicate ID")
  add(which(is.na(name) | trimws(name) == ""), "name", "missing or empty")
  att_num  <- suppressWarnings(as.numeric(attendance))
  not_num  <- which(is.na(att_num) & !is.na(attendance))
  add(not_num, "attendance", "non-numeric text (unintended conversion)")
  add(which(is.na(attendance)), "attendance", "missing value")
  add(which(!is.na(att_num) & (att_num < 0 | att_num > 100)),
      "attendance", "outside 0-100")
  add(which(is.na(programme) | !programme %in% valid_programmes),
      "programme", "invalid category")
  issues
}

cat("\n--- Test case 1: clean data ---\n")
iss <- validate_core(reg_no, name, attendance, programme)
cat("Issues found:", nrow(iss), "\n")

cat("\n--- Test case 2: dirty data ---\n")
bad_reg  <- c(reg_no[1:3], "19232", reg_no[2], reg_no[6:8])
bad_name <- c("A", "B", NA, "D", " ", "F", "G", "H")
bad_att  <- c("92", "85%", NA, "120", "-3", "abc", "75", "88")
bad_prog <- c("AI&DS", "CSE", "MECH", "IT", NA, "ECE", "CSE", "AI&DS")
print(validate_core(bad_reg, bad_name, bad_att, bad_prog))

core <- data.frame(reg_no = reg_no, name = name, programme = programme,
                   attendance = attendance, eligible = eligible,
                   stringsAsFactors = FALSE)
cat("\n--- Final student core data ---\n")
str(core)
print(head(core, 4))

# ===================== TASK 3 =====================
# TASK 3: Coercion-safe data preparation module
options(width = 85)

detect_coercion <- function(...) {
  items    <- list(...)
  in_types <- vapply(items, function(i) class(i)[1], character(1))
  out_type <- class(do.call(c, items))[1]
  risk <- if (all(in_types == out_type)) "none"
          else if (out_type == "character") "HARMFUL: text"
          else if ("logical" %in% in_types) "semantic: TRUE->1"
          else "benign widening"
  data.frame(inputs = paste(in_types, collapse = " + "), output = out_type,
             risk = risk, stringsAsFactors = FALSE)
}

cat("--- Recorded result types of mixed inputs ---\n")
print(rbind(detect_coercion(1L, 2L),
            detect_coercion(1L, 2.5),
            detect_coercion(85, TRUE),
            detect_coercion(3L, FALSE),
            detect_coercion(85, "A"),
            detect_coercion(TRUE, "Pass"),
            detect_coercion(1L, 2.5, TRUE, "x")), right = FALSE)

safe_convert <- function(x, to = c("numeric", "integer", "logical", "character")) {
  to <- match.arg(to)
  was_na <- is.na(x)
  out <- suppressWarnings(switch(to, numeric = as.numeric(x),
                                 integer = as.integer(x),
                                 logical = as.logical(x),
                                 character = as.character(x)))
  failed <- which(is.na(out) & !was_na)
  lossy <- integer(0)
  if (to == "integer") {
    as_num <- suppressWarnings(as.numeric(x))
    lossy  <- which(!is.na(out) & !is.na(as_num) & as_num != out)
  }
  list(value = out, ok = length(failed) == 0 && length(lossy) == 0,
       failed_index = failed, lossy_index = lossy, n_missing = sum(is.na(out)))
}

strict_combine <- function(...) {
  items <- list(...)
  types <- unique(vapply(items, function(i) class(i)[1], character(1)))
  if (all(c("integer", "numeric") %in% types)) types <- setdiff(types, "integer")
  if (length(types) > 1)
    stop("mixed types refused: ", paste(types, collapse = ", "), call. = FALSE)
  do.call(c, items)
}

check_frame_types <- function(df, schema) {
  actual <- vapply(df[names(schema)], function(v) class(v)[1], character(1))
  bad <- actual != unlist(schema)
  data.frame(column = names(schema), expected = unlist(schema),
             actual = actual, mismatch = bad, row.names = NULL)
}

cat("\n--- Test cases for safe_convert ---\n")
cases <- list(valid_numeric = c("85", "90.5"), invalid_text = c("85", "abc", "70"),
              missing_value = c("85", NA, "70"), percent_sign = c("85%", "90"),
              truncation_risk = c("3.7", "4"), logical_words = c("TRUE", "yes", "T", "0"))
targets <- c("numeric", "numeric", "numeric", "numeric", "integer", "logical")
res <- do.call(rbind, Map(function(nm, x, to) {
  r <- safe_convert(x, to)
  data.frame(case = nm, target = to, accepted = r$ok,
             failed_at = paste(r$failed_index, collapse = ","),
             lossy_at = paste(r$lossy_index, collapse = ","),
             n_NA = r$n_missing)
}, names(cases), cases, targets))
print(res, row.names = FALSE)

cat("\n--- strict_combine versus c() ---\n")
cat("c(85, 'A')         ->", class(c(85, "A")), "(silent)\n")
r1 <- tryCatch(strict_combine(85, "A"), error = function(e) conditionMessage(e))
cat("strict_combine(85, 'A')   ->", r1, "\n")
cat("strict_combine(1L, 2.5)   ->", strict_combine(1L, 2.5), "(widening allowed)\n")
r3 <- tryCatch(strict_combine(85, TRUE), error = function(e) conditionMessage(e))
cat("strict_combine(85, TRUE)  ->", r3, "\n")

cat("\n--- check_frame_types on a file-style import ---\n")
raw <- read.csv(text = "reg_no,attendance,eligible
192324001,92,TRUE
192324002,85%,TRUE
192324003,74,FALSE", stringsAsFactors = FALSE)
schema_t <- list(reg_no = "character", attendance = "numeric", eligible = "logical")
print(check_frame_types(raw, schema_t))
cat("reg_no was read as", class(raw$reg_no), "-> leading zeros would be lost;",
    "attendance read as", class(raw$attendance), "because of '%'.\n")

# ===================== TASK 4 =====================
# TASK 4: Student-by-subject matrix processing engine
options(width = 85)
set.seed(2026)

subjects <- data.frame(code = c("CS101", "MA102", "ST103", "DS104", "EN105"),
                       title = c("Programming", "Mathematics", "Statistics",
                                 "Data Science", "Communication"),
                       credits = c(4, 4, 3, 4, 2), stringsAsFactors = FALSE)
ability <- rnorm(12, 68, 9)
marks <- sapply(seq_len(5), function(j) round(pmin(100, pmax(0, ability + rnorm(12, 0, 8)))))
dimnames(marks) <- list(core$reg_no, subjects$code)
marks["192324006", "EN105"] <- 38   # deliberate failing marks so pass/fail logic is visible
marks["192324007", "DS104"] <- 36
cat("Marks matrix (students x subjects), dim =", dim(marks), "\n")
print(marks)

cat("\n--- Indexing ---\n")
cat("By position  marks[3, 2]                 :", marks[3, 2], "\n")
cat("By names     marks['192324003','MA102']  :", marks["192324003", "MA102"], "\n")
cat("One student  marks['192324005', ]:\n"); print(marks["192324005", ])
cat("One subject, first 4 students  marks[1:4, 'ST103']:\n"); print(marks[1:4, "ST103"])
cat("Sub-matrix kept as matrix (drop = FALSE):\n")
print(marks[1:3, c("CS101", "DS104"), drop = FALSE])

cat("\n--- Row and column summaries ---\n")
cat("Student averages (rowMeans):\n"); print(round(rowMeans(marks), 2))
cat("Subject averages (colMeans):\n"); print(round(colMeans(marks), 2))
cat("Subject standard deviations:\n"); print(round(apply(marks, 2, sd), 2))
cat("Subjects passed per student (>= 40):\n"); print(rowSums(marks >= 40))

cat("\n--- Transposition ---\n")
tm <- t(marks)
cat("dim(t(marks)) =", dim(tm), " | dimnames swapped:",
    identical(dimnames(tm), rev(dimnames(marks))), "\n")
print(tm[, 1:4])

cat("\n--- Matrix operation 1: credit-weighted average (matrix %*% vector) ---\n")
w <- subjects$credits / sum(subjects$credits)
weighted_avg <- drop(marks %*% w)
names(weighted_avg) <- rownames(marks)
print(round(weighted_avg, 2))
cat("Top 3 students:", paste(names(sort(weighted_avg, decreasing = TRUE))[1:3],
                            collapse = ", "), "\n")

cat("\n--- Matrix operation 2: subject correlation matrix ---\n")
print(round(cor(marks), 2))

cat("\n--- Linking matrix positions to identifiers ---\n")
hit <- which(marks == max(marks), arr.ind = TRUE)[1, ]
cat("Highest mark", max(marks), "-> row", hit[1], "col", hit[2], "=>",
    rownames(marks)[hit[1]], "in", colnames(marks)[hit[2]],
    "(", subjects$title[match(colnames(marks)[hit[2]], subjects$code)], ")\n")
locate_mark <- function(m, id, subject) {
  r <- match(id, rownames(m)); c <- match(subject, colnames(m))
  if (is.na(r) || is.na(c)) stop("unknown ID or subject: ", id, " / ", subject)
  list(row = r, col = c, mark = m[r, c])
}
str(locate_mark(marks, "192324007", "EN105"))
cat("Top scorer per subject:\n")
print(apply(marks, 2, function(col) rownames(marks)[which.max(col)]))

cat("\n--- Matrix -> data frame (long, metadata-rich) ---\n")
long <- as.data.frame(as.table(marks), stringsAsFactors = FALSE)
names(long) <- c("reg_no", "subject", "mark")
long <- merge(long, core[, c("reg_no", "programme", "attendance")], by = "reg_no")
long <- merge(long, subjects[, c("code", "title", "credits")],
              by.x = "subject", by.y = "code")
long <- long[order(long$reg_no, match(long$subject, subjects$code)), ]
rownames(long) <- NULL
cat("rows:", nrow(long), "(12 students x 5 subjects)\n"); print(head(long, 6))

cat("\n--- Data frame -> matrix (round trip) ---\n")
back <- tapply(long$mark, list(long$reg_no, long$subject), sum)
back <- back[rownames(marks), colnames(marks)]
cat("After tapply(), columns come back alphabetical:",
    paste(colnames(tapply(long$mark, list(long$reg_no, long$subject), sum)), collapse = " "), "\n")
cat("After re-indexing by name, identical to original:",
    isTRUE(all.equal(back, marks, check.attributes = FALSE)) &&
      identical(dimnames(back), dimnames(marks)), "\n")

png("fig/fig_t4_heatmap.png", width = 1500, height = 1000, res = 170)
par(mar = c(5, 7, 4, 2))
image(1:5, 1:12, t(marks)[, 12:1], col = hcl.colors(25, "YlGnBu", rev = TRUE),
      axes = FALSE, xlab = "Subject", ylab = "")
axis(1, 1:5, colnames(marks)); axis(2, 1:12, rev(rownames(marks)), las = 1, cex.axis = 0.8)
for (i in 1:12) for (j in 1:5)
  text(j, 13 - i, marks[i, j], cex = 0.8, col = if (marks[i, j] >= 68) "white" else "black")
title("Student-by-subject marks matrix (interpretable via dimnames)")
dev.off()

# ===================== TASK 5 =====================
# TASK 5: Multi-dimensional assessment array (students x subjects x components)
options(width = 85)
set.seed(77)
components <- c("internal", "model", "practical")
arr <- array(NA_real_, dim = c(12, 5, 3),
             dimnames = list(student = rownames(marks), subject = colnames(marks),
                             component = components))
arr[, , "model"]     <- marks                         # reuse the Task 4 matrix
arr[, , "internal"]  <- round(pmin(100, pmax(0, marks + rnorm(60, 4, 6))))
arr[, , "practical"] <- round(pmin(100, pmax(0, marks + rnorm(60, 8, 7))))
cat("dim(arr) =", dim(arr), "| named dimensions:", paste(names(dimnames(arr)), collapse = ", "), "\n")
cat("Model slice equals the Task 4 matrix (values and IDs):",
    isTRUE(all.equal(arr[, , "model"], marks, check.attributes = FALSE)) &&
      identical(rownames(arr), rownames(marks)) && identical(colnames(arr), colnames(marks)), "\n")
cat("\nFirst two students (all subjects, all components):\n")
print(arr[1:2, , ])

cat("\n--- Retrieval ---\n")
cat("One cell     arr['192324003','ST103','internal'] :", arr["192324003", "ST103", "internal"], "\n")
cat("One student/subject across components:\n"); print(arr["192324003", "ST103", ])
cat("All students, CS101 practical (first 5):", arr[1:5, "CS101", "practical"], "\n")
sub <- arr[1:3, c("CS101", "MA102"), c("internal", "practical")]
cat("Sub-array dims:", dim(sub), "| dimnames kept:",
    paste(names(dimnames(sub)), collapse = ", "), "\n")

cat("\n--- Calculations over selected dimensions ---\n")
cat("Mean mark per component (all students and subjects):\n"); print(round(apply(arr, 3, mean), 2))
cat("Mean per subject x component:\n"); print(round(apply(arr, c(2, 3), mean), 1))
cat("Overall mean per student (first 5):\n"); print(round(apply(arr, 1, mean)[1:5], 2))
w <- c(internal = 0.3, model = 0.3, practical = 0.4)
total <- Reduce(`+`, lapply(components, function(k) arr[, , k] * w[k]))
total_apply <- apply(arr, c(1, 2), function(x) sum(x * w))
cat("Weighted total (30/30/40) via vectorised sum equals apply():",
    isTRUE(all.equal(total, total_apply)), "\n")
print(round(total[1:4, ], 1))

cat("\n--- Why dimensions stay interpretable ---\n")
p <- aperm(arr, c(2, 1, 3))
cat("aperm(arr, c(2,1,3)) dim:", dim(p), "| names:", paste(names(dimnames(p)), collapse = ", "), "\n")
cat("Same cell by names after permutation:", p["ST103", "192324003", "internal"] ==
      arr["192324003", "ST103", "internal"], "\n")
e1 <- tryCatch(arr["XXXX", "CS101", "internal"], error = function(e) conditionMessage(e))
e2 <- tryCatch(arr[1, 1, 4], error = function(e) conditionMessage(e))
cat("Unknown ID    ->", e1, "\n"); cat("Component 4   ->", e2, "\n")

cat("\n--- Transformations ---\n")
long5 <- as.data.frame.table(arr, responseName = "mark", stringsAsFactors = FALSE)
cat("1) array -> long data frame:", nrow(long5), "rows x", ncol(long5), "cols\n")
print(head(long5, 4))
by_comp <- lapply(setNames(components, components), function(k) arr[, , k])
cat("2) array -> list of matrices:", paste(names(by_comp), sapply(by_comp, function(m) paste(dim(m), collapse = "x"))), "\n")
report <- data.frame(reg_no = rownames(total), round(total, 1), check.names = FALSE, row.names = NULL)
cat("3) array -> wide report of weighted totals:\n"); print(head(report, 4))
back <- xtabs(mark ~ student + subject + component, data = long5)
cat("4) long -> array round trip identical:",
    isTRUE(all.equal(unclass(back)[rownames(arr), colnames(arr), components],
                     arr, check.attributes = FALSE)), "\n")

png("fig/fig_t5_components.png", width = 1500, height = 950, res = 170)
par(mar = c(5, 5, 4, 2))
boxplot(mark ~ component, data = long5, col = c("#9ecae1", "#a1d99b", "#fdae6b"),
        ylab = "Marks", xlab = "Assessment component",
        main = "Marks by assessment component (array dimension 3)")
points(1:3, apply(arr, 3, mean), pch = 18, cex = 1.6, col = "red")
legend("bottomright", legend = "Mean", pch = 18, col = "red", bty = "n")
dev.off()

# ===================== TASK 6 =====================
# TASK 6: Integrated list, factor and data-frame layer
options(width = 85)
rec <- list(reg_no = "192324003", name = "Charan Teja", programme = "CSE",
            attendance = 74.5, marks = c(CS101 = 71, MA102 = 66, ST103 = 59),
            eligible = FALSE, contact = list(email = "charan@example.edu", phone = "9876500003"))
cat("--- Heterogeneous student record (list) ---\n"); str(rec)
cat("Nested access rec$contact$email:", rec$contact$email, "| rec[['marks']]['ST103']:", rec[["marks"]]["ST103"], "\n")
cat("Element classes:", paste(names(rec), sapply(rec, class), sep = "=", collapse = "; "), "\n")

grade_breaks <- c(-Inf, 40, 55, 70, 85, Inf); grade_labels <- c("F", "C", "B", "A", "O")
avg_total <- rowMeans(total)
analysis <- data.frame(
  reg_no     = core$reg_no, name = core$name,
  programme  = factor(core$programme, levels = valid_programmes),
  attendance = core$attendance, eligible = core$eligible,
  avg_total  = round(unname(avg_total[core$reg_no]), 2),
  grade      = cut(avg_total[core$reg_no], breaks = grade_breaks, labels = grade_labels,
                   right = FALSE, ordered_result = TRUE),
  stringsAsFactors = FALSE)
cat("\n--- Analysis-ready data frame ---\n"); str(analysis); print(analysis[1:5, ])
cat("\nlevels(programme):", levels(analysis$programme), "\n")
cat("levels(grade) [ordered]:", paste(levels(analysis$grade), collapse = " < "), "\n")

cat("\n--- Rule: character or factor? ---\n")
recommend_type <- function(x, max_levels = 20, max_unique_ratio = 0.5) {
  n <- length(x); u <- length(unique(x[!is.na(x)]))
  if (!is.character(x)) return("keep as is")
  if (u / n > max_unique_ratio || u > max_levels) "character" else "factor"
}
decision <- data.frame(variable = c("reg_no", "name", "programme", "gender", "email_domain"),
  unique_values = c(12, 12, 4, 2, 3),
  decision = c(recommend_type(core$reg_no), recommend_type(core$name),
               recommend_type(core$programme),
               recommend_type(rep(c("F", "M"), 6)),
               recommend_type(rep(c("a.edu", "b.edu", "c.edu"), 4))))
print(decision, row.names = FALSE)

cat("\n--- Operations: add, update, select, summarise ---\n")
add_student <- function(df, reg_no, name, programme, attendance, avg_total) {
  if (!programme %in% levels(df$programme)) stop("unknown programme: ", programme)
  new <- data.frame(reg_no = reg_no, name = name,
                    programme = factor(programme, levels = levels(df$programme)),
                    attendance = attendance, eligible = attendance >= 75,
                    avg_total = avg_total,
                    grade = cut(avg_total, grade_breaks, grade_labels, right = FALSE, ordered_result = TRUE),
                    stringsAsFactors = FALSE)
  rbind(df, new)
}
analysis <- add_student(analysis, "192324013", "Meena K", "IT", 91, 73.4)
cat("ADD    rows now:", nrow(analysis), "| last row:", analysis$name[13], as.character(analysis$grade[13]), "\n")
bad <- tryCatch(add_student(analysis, "192324014", "X", "MECH", 80, 60), error = function(e) conditionMessage(e))
cat("ADD (invalid programme) ->", bad, "\n")
analysis$attendance[analysis$reg_no == "192324003"] <- 76
analysis$eligible <- analysis$attendance >= 75
cat("UPDATE 192324003 attendance -> 76, eligible =", analysis$eligible[analysis$reg_no == "192324003"], "\n")
cat("SELECT grade >= 'A' (ordered factor comparison):\n")
print(analysis[analysis$grade >= "A", c("reg_no", "programme", "avg_total", "grade")])
cat("SUMMARISE mean avg_total by programme:\n")
print(aggregate(avg_total ~ programme, data = analysis, FUN = function(x) round(mean(x), 2)))
cat("SUMMARISE programme x grade table:\n"); print(table(analysis$programme, analysis$grade))

cat("\n--- Exchange with matrix and array components ---\n")
tot_df <- data.frame(reg_no = rownames(total), total, row.names = NULL)
back_m <- as.matrix(tot_df[, colnames(total)]); rownames(back_m) <- tot_df$reg_no
cat("matrix -> data frame -> matrix identical:",
    isTRUE(all.equal(back_m, total, check.attributes = FALSE)) &&
      identical(rownames(back_m), rownames(total)), "\n")
joined <- merge(analysis[, c("reg_no", "programme")], tot_df, by = "reg_no")
cat("Subject means by programme (data frame groups applied to matrix columns):\n")
sm <- aggregate(. ~ programme, data = joined[, -1], FUN = mean)
sm[-1] <- round(sm[-1], 1); print(sm)
pg <- with(analysis, table(programme, grade))
cat("data-frame summary -> 2-D table class:", class(pg), "dim:", dim(pg), "\n")
arr_back <- array(unlist(by_comp), dim = dim(arr), dimnames = dimnames(arr))
cat("list of matrices -> array identical:", isTRUE(all.equal(arr_back, arr)), "\n")

png("fig/fig_t6_grades.png", width = 1500, height = 950, res = 170)
par(mar = c(5, 5, 4, 2))
barplot(t(table(analysis$programme, analysis$grade)), beside = FALSE,
        col = hcl.colors(5, "Blues 3"), legend.text = TRUE, args.legend = list(title = "Grade", x = "topright"),
        xlab = "Programme (factor)", ylab = "Number of students",
        main = "Grade distribution by programme (ordered factor)")
dev.off()

# ===================== TASK 7 =====================
# TASK 7: Object-oriented Student model (S4 final design, S3 comparative prototype)
options(width = 85)

# ---------- S4 : final candidate ----------
setClass("Student",
  representation(reg_no = "character", name = "character", programme = "character",
                 attendance = "numeric", marks = "numeric"),
  validity = function(object) {
    errs <- character()
    if (length(object@reg_no) != 1 || !grepl("^[0-9]{9}$", object@reg_no))
      errs <- c(errs, "reg_no must be one 9-digit string")
    if (length(object@attendance) != 1 || is.na(object@attendance) ||
        object@attendance < 0 || object@attendance > 100)
      errs <- c(errs, "attendance must be one number in 0-100")
    if (!object@programme %in% c("AI&DS", "CSE", "IT", "ECE"))
      errs <- c(errs, "programme is not a valid category")
    if (length(object@marks) > 0 &&
        (is.null(names(object@marks)) || any(is.na(object@marks)) ||
         any(object@marks < 0 | object@marks > 100)))
      errs <- c(errs, "marks must be named values in 0-100")
    if (length(errs)) errs else TRUE
  })

setGeneric("update_marks", function(obj, subject, value) standardGeneric("update_marks"))
setGeneric("eligibility",  function(obj, min_attendance = 75, pass_mark = 40) standardGeneric("eligibility"))
setGeneric("average_mark", function(obj) standardGeneric("average_mark"))

setMethod("update_marks", "Student", function(obj, subject, value) {
  obj@marks[subject] <- value
  validObject(obj)
  obj
})
setMethod("average_mark", "Student", function(obj) mean(obj@marks))
setMethod("eligibility", "Student", function(obj, min_attendance = 75, pass_mark = 40) {
  reasons <- character()
  if (obj@attendance < min_attendance) reasons <- c(reasons, "attendance below minimum")
  failed <- names(obj@marks)[obj@marks < pass_mark]
  if (length(failed)) reasons <- c(reasons, paste("failed:", paste(failed, collapse = ", ")))
  list(eligible = length(reasons) == 0, reasons = reasons)
})
setMethod("show", "Student", function(object) {
  cat("<Student>", object@reg_no, "-", object@name, "(", object@programme, ")\n")
  cat("  attendance:", object@attendance, "% | average mark:", round(average_mark(object), 2), "\n")
  cat("  marks:", paste(names(object@marks), object@marks, sep = "=", collapse = ", "), "\n")
})

cat("--- S4 object built from the earlier components ---\n")
i  <- which(core$reg_no == "192324006")
s4 <- new("Student", reg_no = core$reg_no[i], name = core$name[i], programme = core$programme[i],
          attendance = core$attendance[i], marks = marks[core$reg_no[i], ])
show(s4)
cat("eligibility():\n"); str(eligibility(s4))
s4b <- update_marks(s4, "EN105", 52)
cat("After update_marks(EN105 = 52): average =", round(average_mark(s4b), 2),
    "| original untouched (copy semantics):", s4@marks["EN105"], "\n")
str(eligibility(s4b))

cat("\n--- S4 test cases (expected to be rejected) ---\n")
try_new <- function(label, expr) {
  r <- tryCatch({ force(expr); "ACCEPTED" }, error = function(e) conditionMessage(e))
  cat(sprintf("%-26s -> %s\n", label, r))
}
try_new("attendance = 120", new("Student", reg_no = "192324001", name = "A", programme = "CSE", attendance = 120, marks = c(CS101 = 50)))
try_new("attendance as text", new("Student", reg_no = "192324001", name = "A", programme = "CSE", attendance = "85%", marks = c(CS101 = 50)))
try_new("bad programme", new("Student", reg_no = "192324001", name = "A", programme = "MECH", attendance = 80, marks = c(CS101 = 50)))
try_new("mark = 140 via update", update_marks(s4, "CS101", 140))
try_new("unnamed marks", new("Student", reg_no = "192324001", name = "A", programme = "CSE", attendance = 80, marks = c(50, 60)))

# ---------- S3 : comparative prototype ----------
new_student_s3 <- function(reg_no, name, programme, attendance, marks) {
  obj <- structure(list(reg_no = reg_no, name = name, programme = programme,
                        attendance = attendance, marks = marks), class = "student_s3")
  validate_student_s3(obj)
  obj
}
validate_student_s3 <- function(obj) {
  if (!is.numeric(obj$attendance) || obj$attendance < 0 || obj$attendance > 100)
    stop("attendance must be a number in 0-100", call. = FALSE)
  invisible(obj)
}
update_marks_s3 <- function(obj, subject, value) UseMethod("update_marks_s3")
update_marks_s3.student_s3 <- function(obj, subject, value) { obj$marks[subject] <- value; obj }
print.student_s3 <- function(x, ...) cat("<student_s3>", x$reg_no, x$name, "attendance", x$attendance, "\n")

cat("\n--- S3 prototype ---\n")
s3 <- new_student_s3(core$reg_no[i], core$name[i], core$programme[i], core$attendance[i], marks[core$reg_no[i], ])
print(s3)
r <- tryCatch(new_student_s3("192324001", "A", "CSE", 120, c(CS101 = 50)), error = function(e) conditionMessage(e))
cat("Constructor with attendance = 120 ->", r, "\n")
rogue <- structure(list(reg_no = "x", attendance = 999, marks = c(CS101 = 500)), class = "student_s3")
cat("Bypassing the constructor with structure(): accepted, attendance =", rogue$attendance, "\n")
s3u <- update_marks_s3(s3, "CS101", 140)
cat("S3 update_marks_s3(CS101 = 140) accepted silently: mark =", s3u$marks["CS101"], "\n")

cat("\n--- Measured comparison ---\n")
n <- 2000
t_s4 <- system.time(for (k in seq_len(n)) new("Student", reg_no = "192324001", name = "A", programme = "CSE", attendance = 80, marks = c(CS101 = 50, MA102 = 60)))[["elapsed"]]
t_s3 <- system.time(for (k in seq_len(n)) new_student_s3("192324001", "A", "CSE", 80, c(CS101 = 50, MA102 = 60)))[["elapsed"]]
comparison <- data.frame(
  criterion = c("Validation enforced on every new()", "Update re-validated automatically",
                "Can be bypassed by structure()", paste("Time to build", n, "objects (s)")),
  S4 = c("Yes", "Yes (validObject)", "No", sprintf("%.2f", t_s4)),
  S3 = c("Only if constructor used", "No", "Yes", sprintf("%.2f", t_s3)))
print(comparison, right = FALSE)
cat("DECISION: S4 for the single-student object (guaranteed validity); see Task 11 for bulk storage.\n")

# ===================== TASK 8 =====================
# TASK 8: Reusable function library
options(width = 85)

#' validate_marks(x, min = 0, max = 100)
#'   x: numeric/character vector of marks.  Returns a list: valid (logical vector),
#'   n_invalid (integer), issues (data frame: index, value, reason).
validate_marks <- function(x, min = 0, max = 100) {
  num <- suppressWarnings(as.numeric(x))
  reason <- ifelse(is.na(x), "missing",
            ifelse(is.na(num), "not numeric",
            ifelse(num < min | num > max, "out of range", NA_character_)))
  bad <- which(!is.na(reason))
  list(valid = is.na(reason), n_invalid = length(bad),
       issues = data.frame(index = bad, value = as.character(x[bad]), reason = reason[bad],
                           stringsAsFactors = FALSE))
}

#' eligibility_status(attendance, min_attendance = 75)
#'   Returns a logical vector; NA stays NA (unknown is not "not eligible").
eligibility_status <- function(attendance, min_attendance = 75) {
  if (!is.numeric(attendance)) stop("attendance must be numeric")
  attendance >= min_attendance
}

#' assign_grade(marks, breaks, labels)
#'   Returns an ORDERED FACTOR; NA for NA; error for values outside [0, 100].
assign_grade <- function(marks, breaks = c(-Inf, 40, 55, 70, 85, Inf),
                         labels = c("F", "C", "B", "A", "O")) {
  if (any(marks < 0 | marks > 100, na.rm = TRUE)) stop("marks outside 0-100")
  cut(marks, breaks = breaks, labels = labels, right = FALSE, ordered_result = TRUE)
}

#' summarise_marks(m, pass_mark = 40)
#'   m: students x subjects numeric matrix with dimnames.
#'   Returns a data frame: reg_no, mean, min, max, n_pass, all_pass, grade.
summarise_marks <- function(m, pass_mark = 40) {
  if (is.null(rownames(m)) || is.null(colnames(m))) stop("matrix needs row and column names")
  avg <- rowMeans(m, na.rm = TRUE)
  data.frame(reg_no = rownames(m), mean = round(avg, 2),
             min = apply(m, 1, min, na.rm = TRUE), max = apply(m, 1, max, na.rm = TRUE),
             n_pass = rowSums(m >= pass_mark, na.rm = TRUE),
             all_pass = apply(m >= pass_mark, 1, all), grade = assign_grade(avg),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' weighted_total(arr, weights)
#'   arr: students x subjects x components array with named 3rd dimension.
#'   weights: named numeric vector summing to 1.  Returns a students x subjects matrix.
weighted_total <- function(arr, weights) {
  if (abs(sum(weights) - 1) > 1e-8) stop("weights must sum to 1")
  if (!setequal(names(weights), dimnames(arr)[[3]])) stop("weights do not match components")
  Reduce(`+`, lapply(names(weights), function(k) arr[, , k] * weights[[k]]))
}

cat("--- Function signatures ---\n")
for (fn in c("validate_marks", "eligibility_status", "assign_grade", "summarise_marks", "weighted_total"))
  cat(fn, "(", paste(names(formals(get(fn))), collapse = ", "), ")\n", sep = "")

check <- function(id, fn, case, ok) data.frame(id = id, fn = fn, case = case,
                                               result = if (isTRUE(ok)) "PASS" else "FAIL")
err <- function(expr) tryCatch({ expr; FALSE }, error = function(e) TRUE)
tests <- rbind(
  check(1,  "validate_marks", "valid vector",       validate_marks(c(0, 55, 100))$n_invalid == 0),
  check(2,  "validate_marks", "boundary 100.01",    validate_marks(100.01)$issues$reason == "out of range"),
  check(3,  "validate_marks", "invalid text",       validate_marks("AB")$issues$reason == "not numeric"),
  check(4,  "validate_marks", "missing NA",         validate_marks(NA_real_)$issues$reason == "missing"),
  check(5,  "validate_marks", "custom range 0-50",  validate_marks(60, max = 50)$n_invalid == 1),
  check(6,  "eligibility_status", "boundary 75",    isTRUE(eligibility_status(75))),
  check(7,  "eligibility_status", "boundary 74.99", isFALSE(eligibility_status(74.99))),
  check(8,  "eligibility_status", "NA stays NA",    is.na(eligibility_status(NA_real_))),
  check(9,  "eligibility_status", "text rejected",  err(eligibility_status("85%"))),
  check(10, "assign_grade", "40 -> C (boundary)",   as.character(assign_grade(40)) == "C"),
  check(11, "assign_grade", "39.9 -> F",            as.character(assign_grade(39.9)) == "F"),
  check(12, "assign_grade", "100 -> O",             as.character(assign_grade(100)) == "O"),
  check(13, "assign_grade", "105 rejected",         err(assign_grade(105))),
  check(14, "assign_grade", "ordered factor",       is.ordered(assign_grade(c(10, 90)))),
  check(15, "summarise_marks", "matrix with names", nrow(summarise_marks(marks)) == 12),
  check(16, "summarise_marks", "no dimnames",       err(summarise_marks(unname(marks)))),
  check(17, "weighted_total", "weights sum to 1",   all(dim(weighted_total(arr, w)) == c(12, 5))),
  check(18, "weighted_total", "weights sum 1.2",    err(weighted_total(arr, c(internal = .4, model = .4, practical = .4)))),
  check(19, "weighted_total", "wrong names",        err(weighted_total(arr, c(a = .3, b = .3, c = .4))))
)
cat("\n--- Test results (valid, boundary, invalid inputs) ---\n")
print(tests, row.names = FALSE)
cat("Passed:", sum(tests$result == "PASS"), "of", nrow(tests), "\n")

cat("\n--- Integration: summarise_marks() on the Task 4 matrix ---\n")
print(head(summarise_marks(marks), 6))
cat("\n--- Integration: weighted_total() on the Task 5 array ---\n")
cat("Equal to Task 5 'total':", isTRUE(all.equal(weighted_total(arr, w), total, check.attributes = FALSE)), "\n")

# ===================== TASK 9 =====================
# TASK 9: Integrated prototype - raw feed -> validated -> structured -> analysis-ready
options(width = 85)
set.seed(909)
N <- 200
subj <- subjects$code; comp <- c("internal", "model", "practical")
ids  <- sprintf("1923%05d", 1:N)

# ---- 1. Build a messy RAW feed (as it would arrive in files) ----
raw_core <- data.frame(
  reg_no = ids, name = paste("Student", 1:N),
  programme = sample(c("AI&DS", "CSE", "IT", "ECE"), N, TRUE, prob = c(.35, .3, .2, .15)),
  attendance = as.character(round(pmin(100, rnorm(N, 84, 9)), 1)), stringsAsFactors = FALSE)
raw_core$programme[c(5, 17, 40)] <- c("cse", " IT", "MECH")      # case / space / invalid category
raw_core$attendance[c(3, 9)] <- c("85%", "n/a")                  # text in numeric field
raw_core$attendance[12] <- "132"                                 # impossible value
raw_core$reg_no[60] <- raw_core$reg_no[59]                       # duplicate ID
ability <- rnorm(N, 67, 9)
grid <- expand.grid(reg_no = ids, subject = subj, component = comp, stringsAsFactors = FALSE)
grid$mark <- round(pmin(100, pmax(0, ability[match(grid$reg_no, ids)] + rnorm(nrow(grid), 2, 9))))
grid <- grid[sample(nrow(grid)), ]                               # feed arrives unsorted
grid$mark <- as.character(grid$mark)
grid$mark[sample(nrow(grid), 14)] <- "AB"                        # absent marker
grid$mark[sample(nrow(grid), 6)]  <- "105"                       # out of range
grid$mark[sample(nrow(grid), 10)] <- NA                          # missing
grid <- grid[-sample(nrow(grid), 8), ]                           # 8 records never arrived
tf <- tempfile(fileext = ".csv"); write.csv(grid, tf, row.names = FALSE)
tc <- tempfile(fileext = ".csv"); write.csv(raw_core, tc, row.names = FALSE)
in_core  <- read.csv(tc, stringsAsFactors = FALSE, colClasses = c(reg_no = "character"))
in_marks <- read.csv(tf, stringsAsFactors = FALSE, colClasses = c(reg_no = "character"))
cat("Raw core :", nrow(in_core), "rows | attendance read as", class(in_core$attendance), "\n")
cat("Raw marks:", nrow(in_marks), "rows (expected", N * 5 * 3, ") | mark read as", class(in_marks$mark), "\n")

# ---- 2. VERSION 1 (naive pipeline): record the failures ----
cat("\n=========== VERSION 1 : naive pipeline ===========\n")
v1_att  <- suppressWarnings(as.numeric(in_core$attendance))
cat("F1  attendance as.numeric(): NA introduced silently =", sum(is.na(v1_att)), "(expected 1 'n/a' + 1 '85%')\n")
cat("F2  attendance 132 accepted, max =", max(v1_att, na.rm = TRUE), "\n")
v1_prog <- factor(in_core$programme)
cat("F3  factor(programme) created", nlevels(v1_prog), "levels instead of 4:", paste(levels(v1_prog), collapse = ", "), "\n")
v1_arr <- array(suppressWarnings(as.numeric(in_marks$mark)), dim = c(N, 5, 3))
true_cell <- in_marks$mark[in_marks$reg_no == ids[1] & in_marks$subject == "CS101" & in_marks$component == "internal"]
cat("F4  positional fill: cell [1,1,1] holds", v1_arr[1, 1, 1], "but student 1 CS101 internal is", true_cell, "\n")
cat("F5  duplicated IDs undetected:", sum(duplicated(in_core$reg_no)), "duplicate\n")

# ---- 3. VERSION 2 (refined pipeline using Tasks 2, 3 and 8) ----
cat("\n=========== VERSION 2 : refined pipeline ===========\n")
issues <- list()
log_issue <- function(src, id, field, problem)
  issues[[length(issues) + 1]] <<- data.frame(source = src, id = id, field = field, problem = problem)

clean_core <- in_core
prog_raw <- clean_core$programme
clean_core$programme <- toupper(trimws(clean_core$programme))
for (k in which(prog_raw != clean_core$programme & clean_core$programme %in% valid_programmes))
  log_issue("core", clean_core$reg_no[k], "programme", "case/space fixed")
pct <- grepl("%$", clean_core$attendance)
for (k in which(pct)) log_issue("core", clean_core$reg_no[k], "attendance", "% removed")
att <- safe_convert(sub("%$", "", clean_core$attendance), "numeric")
for (k in att$failed_index) log_issue("core", clean_core$reg_no[k], "attendance", "non-numeric")
va <- validate_marks(att$value)
for (k in va$issues$index[va$issues$reason == "out of range"]) log_issue("core", clean_core$reg_no[k], "attendance", "out of range")
clean_core$attendance <- ifelse(va$valid, att$value, NA)
bad_prog <- which(!clean_core$programme %in% valid_programmes)
for (k in bad_prog) log_issue("core", clean_core$reg_no[k], "programme", "bad category")
clean_core$programme[bad_prog] <- NA
for (k in which(duplicated(clean_core$reg_no))) log_issue("core", clean_core$reg_no[k], "reg_no", "duplicate")
clean_core <- clean_core[!duplicated(clean_core$reg_no), ]
clean_core$eligible <- eligibility_status(clean_core$attendance)

mk <- safe_convert(in_marks$mark, "numeric")
vm <- validate_marks(mk$value)
for (k in mk$failed_index) log_issue("marks", in_marks$reg_no[k], paste(in_marks$subject[k], in_marks$component[k]), "non-numeric")
for (k in vm$issues$index[vm$issues$reason == "out of range"]) log_issue("marks", in_marks$reg_no[k], paste(in_marks$subject[k], in_marks$component[k]), "out of range")
in_marks$mark <- ifelse(vm$valid, mk$value, NA)

arr2 <- array(NA_real_, dim = c(length(ids), 5, 3),
              dimnames = list(student = ids, subject = subj, component = comp))
arr2[cbind(match(in_marks$reg_no, ids), match(in_marks$subject, subj), match(in_marks$component, comp))] <- in_marks$mark
n_cells <- length(arr2); n_na <- sum(is.na(arr2))
cat("Array", paste(dim(arr2), collapse = "x"), "built by NAME matching; NA cells =", n_na,
    "(", sum(!vm$valid), "flagged +", n_cells - nrow(in_marks), "never arrived )\n")

w9 <- c(internal = 0.3, model = 0.3, practical = 0.4)
tot2 <- weighted_total(arr2, w9)
avg2 <- rowMeans(tot2, na.rm = TRUE)
analysis2 <- data.frame(reg_no = clean_core$reg_no, programme = factor(clean_core$programme, levels = valid_programmes),
                        attendance = clean_core$attendance, eligible = clean_core$eligible,
                        avg_total = round(avg2[clean_core$reg_no], 2),
                        grade = assign_grade(avg2[clean_core$reg_no]), stringsAsFactors = FALSE)
cat("Analysis-ready frame:", nrow(analysis2), "rows\n"); str(analysis2)
cat("\nIssue log (", length(issues), "entries ); counts by problem:\n")
issue_df <- do.call(rbind, issues); print(table(issue_df$source, issue_df$problem))
cat("\nSample Student S4 object from the pipeline:\n")
k <- analysis2$reg_no[1]
show(new("Student", reg_no = k, name = "Student 1", programme = as.character(analysis2$programme[1]),
         attendance = analysis2$attendance[1], marks = round(tot2[k, ], 1)))

# ---- 4. Systematic tests ----
cat("\n=========== Systematic test cases ===========\n")
T <- function(id, area, desc, ok) data.frame(id = id, area = area, test = desc, result = if (isTRUE(ok)) "PASS" else "FAIL")
res <- rbind(
  T("T1", "correctness", "student 1 CS101 total recomputed by hand",
    isTRUE(all.equal(tot2[1, "CS101"], sum(arr2[1, "CS101", ] * w9)))),
  T("T2", "correctness", "array cell equals raw record (by name)",
    { r <- in_marks[10, ]; isTRUE(all.equal(arr2[r$reg_no, r$subject, r$component], r$mark)) }),
  T("T3", "missing values", "AB/missing/dropped become NA, not 0",
    sum(is.na(arr2)) == (sum(!vm$valid) + n_cells - nrow(in_marks)) && !any(arr2 == 0, na.rm = TRUE)),
  T("T4", "invalid category", "invalid programmes flagged -> NA", sum(is.na(analysis2$programme)) == length(bad_prog)),
  T("T5", "dimension error", "3x3 matrix into 200x5 slice fails",
    tryCatch({ arr2[, , 1] <- matrix(0, 3, 3); FALSE }, error = function(e) TRUE)),
  T("T6", "dimension error", "unknown student ID gives subscript error",
    tryCatch({ arr2["NOPE", 1, 1]; FALSE }, error = function(e) TRUE)),
  T("T7", "type / coercion", "text attendance: 1 unusable + 1 normalised",
    length(att$failed_index) == 1 && sum(pct) == 1),
  T("T8", "type / coercion", "no mark above 100 remains", max(arr2, na.rm = TRUE) <= 100),
  T("T9", "integrity", "duplicate ID detected and removed", !any(duplicated(analysis2$reg_no)) && sum(duplicated(in_core$reg_no)) == 1),
  T("T10", "integrity", "array -> long -> array round trip",
    { lg <- as.data.frame.table(arr2, responseName = "mark", stringsAsFactors = FALSE)
      bk <- xtabs(mark ~ student + subject + component, data = lg, addNA = FALSE)
      isTRUE(all.equal(unclass(bk)[ids, subj, comp][!is.na(arr2)], arr2[!is.na(arr2)], check.attributes = FALSE)) })
)
print(res, row.names = FALSE); cat("Passed:", sum(res$result == "PASS"), "of", nrow(res), "\n")

# ---- 5. Failure / refinement record ----
cat("\n=========== Failure and refinement record ===========\n")
fr <- data.frame(
  id = paste0("F", 1:8),
  failure_observed = c("'n/a' became NA with no record", "attendance 132 accepted",
    "typos created 7 factor levels (not 4)", "positional fill misplaced records",
    "duplicate reg_no passed through", "Task 8 test 17 failed (test bug)",
    "identical() FALSE on equal matrices", "round() failed on factor column"),
  refinement = c("safe_convert() logs index + value", "range check on attendance",
    "toupper/trimws + match valid list", "fill by match() on IDs/names",
    "duplicated() check + log", "all(dim(..) == c(12, 5))",
    "all.equal() + explicit ID check", "round numeric columns only"))
print(fr, right = FALSE)

# ===================== TASK 10 =====================
# TASK 10: Evidence-based scalability evaluation
options(width = 85)
set.seed(10)
w10 <- c(internal = 0.3, model = 0.3, practical = 0.4)
build_data <- function(n, n_subj = 25) {
  ids <- sprintf("S%06d", seq_len(n)); sj <- sprintf("SUB%02d", seq_len(n_subj))
  a <- array(round(runif(n * n_subj * 3, 30, 100)), dim = c(n, n_subj, 3),
             dimnames = list(student = ids, subject = sj, component = names(w10)))
  d <- data.frame(reg_no = ids, programme = factor(sample(valid_programmes, n, TRUE), levels = valid_programmes),
                  stringsAsFactors = FALSE)
  list(arr = a, df = d)
}
timeit <- function(expr, reps = 1) {
  e <- substitute(expr); env <- parent.frame()
  median(sapply(seq_len(reps), function(i) system.time(eval(e, env))[["elapsed"]]))
}
rows <- list()
for (n in c(5000, 100000)) {
  bd <- build_data(n); a <- bd$arr; d <- bd$df
  reps <- if (n == 5000) 3 else 1
  # Operation A: weighted total (v1 apply, v2 vectorised)
  tA1 <- timeit(apply(a, c(1, 2), function(x) sum(x * w10)), reps)
  tA2 <- timeit(weighted_total(a, w10), reps)
  tot <- weighted_total(a, w10); d$avg <- rowMeans(tot)
  # Operation B: validation of every mark (v1 per-student loop, v2 vectorised)
  tB1 <- timeit(vapply(seq_len(n), function(i) validate_marks(a[i, , ])$n_invalid, numeric(1)), reps)
  tB2 <- timeit(sum(a < 0 | a > 100 | is.na(a)), reps)
  # Operation C: group summary by programme (v1 aggregate, v2 tapply)
  tC1 <- timeit(aggregate(avg ~ programme, data = d, FUN = mean), reps)
  tC2 <- timeit(tapply(d$avg, d$programme, mean), reps)
  # Operation D: array -> long data frame
  tD <- timeit(as.data.frame.table(a, responseName = "mark", stringsAsFactors = FALSE), 1)
  rows[[length(rows) + 1]] <- data.frame(
    students = n, subjects = 25, components = 3, cells = prod(dim(a)),
    array_MB = round(as.numeric(object.size(a)) / 1024^2, 1),
    A_apply = tA1, A_vector = tA2, B_loop = tB1, B_vector = tB2,
    C_aggregate = tC1, C_tapply = tC2, D_to_long = tD)
  rm(bd, d, a, tot); invisible(gc())
}
res <- do.call(rbind, rows)
cat("--- Experimental setup ---\n")
cat("Students: 5,000 and 100,000 | subjects: 25 | components: 3 | R", R.version.string, "\n")
cat("Timing = elapsed seconds (median of 3 runs at 5,000; single run at 100,000)\n\n")
show_res <- res[, 1:5]; show_res$students <- format(show_res$students, big.mark = ",", scientific = FALSE)
show_res$cells <- format(show_res$cells, big.mark = ",", scientific = FALSE)
print(show_res, row.names = FALSE)
cmp <- data.frame(
  operation = c("A weighted total", "A weighted total", "B validate every mark", "B validate every mark",
                "C group mean by programme", "C group mean by programme", "D array -> long frame"),
  version = c("v1 apply()", "v2 vectorised", "v1 per-student loop", "v2 vectorised", "v1 aggregate()", "v2 tapply()", "as.data.frame.table"),
  s_5000 = c(res$A_apply[1], res$A_vector[1], res$B_loop[1], res$B_vector[1], res$C_aggregate[1], res$C_tapply[1], res$D_to_long[1]),
  s_100000 = c(res$A_apply[2], res$A_vector[2], res$B_loop[2], res$B_vector[2], res$C_aggregate[2], res$C_tapply[2], res$D_to_long[2]))
cmp$s_5000 <- round(cmp$s_5000, 3); cmp$s_100000 <- round(cmp$s_100000, 3)
cmp$growth_x <- round(cmp$s_100000 / pmax(cmp$s_5000, 0.001), 1)
cat("\n--- Comparison table (seconds) ---\n"); print(cmp, row.names = FALSE)
sp <- data.frame(operation = c("A", "B", "C"),
                 speedup_5000 = round(c(res$A_apply[1] / res$A_vector[1], res$B_loop[1] / max(res$B_vector[1], 0.001), res$C_aggregate[1] / max(res$C_tapply[1], 0.001)), 1),
                 speedup_100000 = round(c(res$A_apply[2] / res$A_vector[2], res$B_loop[2] / max(res$B_vector[2], 0.001), res$C_aggregate[2] / max(res$C_tapply[2], 0.001)), 1))
cat("\n--- Speed-up of v2 over v1 (v1 time / v2 time) ---\n"); print(sp, row.names = FALSE)

png("fig/fig_t10_scalability.png", width = 1700, height = 1000, res = 170)
par(mar = c(8, 7, 4, 1))
vals <- rbind(pmax(cmp$s_5000, 0.001), pmax(cmp$s_100000, 0.001))
bp <- barplot(vals, beside = TRUE, log = "y", col = c("#9ecae1", "#3182bd"), ylim = c(0.0005, max(vals) * 3),
              names.arg = paste(cmp$operation, cmp$version, sep = "\n"), las = 2, cex.names = 0.6,
              ylab = "", main = "Operation time: 5,000 vs 100,000 students")
mtext("Elapsed seconds (log scale)", side = 2, line = 5)
legend("topleft", legend = c("5,000 students", "100,000 students"), fill = c("#9ecae1", "#3182bd"), bty = "n")
dev.off()
scal <- res

# ===================== TASK 11 =====================
# TASK 11: Changed requirement - 100,000 students, 40 subjects, continuous assessment
options(width = 85)
set.seed(11)
N11 <- 100000; M11 <- 40
ids11  <- sprintf("S%06d", seq_len(N11)); sub11 <- sprintf("SUB%02d", seq_len(M11))
comp11 <- c("CA1", "CA2", "CA3", "model", "practical")
w11    <- c(CA1 = 0.10, CA2 = 0.10, CA3 = 0.10, model = 0.40, practical = 0.30)

cat("--- Decision table: retain / modify / replace / redesign ---\n")
decision <- data.frame(
  component = c("Typed core vectors + validation", "safe_convert / strict_combine", "S4 Student class",
                "3-D assessment array", "Immutable update by copying", "Function library (Task 8)"),
  action = c("RETAIN", "RETAIN", "MODIFY (boundary use only)", "REDESIGN (matrices in env)",
             "REPLACE (Reference Class)", "MODIFY (per-component weights)"))
print(decision, right = FALSE)

# ---- New component: Reference Class AssessmentStore ----
# First attempt (v1): matrices kept in a list field and edited with <<-
StoreV1 <- setRefClass("StoreV1", fields = list(marks = "list"),
  methods = list(update = function(component, i, j, value) { marks[[component]][i, j] <<- value }))

# Final design (v2): matrices live in an environment; an update detaches the matrix,
# edits it while only one reference exists (no copy), and re-attaches it.
AssessmentStore <- setRefClass("AssessmentStore",
  fields = list(mats = "environment", component_names = "character", weights = "numeric",
                student_ids = "character", subject_ids = "character",
                row_index = "environment", audit = "list"),
  methods = list(
    initialize = function(student_ids = character(), subject_ids = character(),
                          components = character(), weights = numeric(), ...) {
      callSuper(student_ids = student_ids, subject_ids = subject_ids,
                component_names = components, weights = weights, ...)
      if (length(components) > 0) {
        if (abs(sum(weights) - 1) > 1e-8) stop("weights must sum to 1")
        mats <<- new.env()
        for (k in components)
          assign(k, matrix(NA_real_, length(student_ids), length(subject_ids),
                           dimnames = list(student_ids, subject_ids)), envir = mats)
        row_index <<- list2env(as.list(setNames(seq_along(student_ids), student_ids)), hash = TRUE)
      }
    },
    get_matrix = function(component) get(component, envir = mats),
    update_mark = function(reg_no, subject, component, value) {
      if (!is.numeric(value) || length(value) != 1 || is.na(value) || value < 0 || value > 100)
        stop("value must be one number in 0-100")
      i <- row_index[[reg_no]]; j <- match(subject, subject_ids)
      if (is.null(i)) stop("unknown student: ", reg_no)
      if (is.na(j)) stop("unknown subject: ", subject)
      if (!component %in% component_names) stop("unknown component: ", component)
      m <- get(component, envir = mats); rm(list = component, envir = mats)
      old <- m[i, j]; m[i, j] <- value
      assign(component, m, envir = mats)
      audit[[length(audit) + 1]] <<- list(id = reg_no, subject = subject, component = component,
                                          old = old, new = value)
      invisible(.self)
    },
    get_mark = function(reg_no, subject, component) get(component, envir = mats)[row_index[[reg_no]], subject],
    add_component = function(name, mat, new_weights) {
      if (!identical(dimnames(mat), list(student_ids, subject_ids))) stop("matrix dimnames do not match the store")
      if (abs(sum(new_weights) - 1) > 1e-8 || !setequal(names(new_weights), c(component_names, name)))
        stop("new_weights must cover all components and sum to 1")
      assign(name, mat, envir = mats)
      component_names <<- c(component_names, name); weights <<- new_weights; invisible(.self)
    },
    weighted_total = function() {
      Reduce(`+`, lapply(names(weights), function(k) get(k, envir = mats) * weights[[k]]))
    },
    to_long = function(ids = student_ids) {
      do.call(rbind, lapply(component_names, function(k)
        data.frame(student = rep(ids, times = length(subject_ids)), subject = rep(subject_ids, each = length(ids)),
                   component = k, mark = as.vector(get(k, envir = mats)[ids, ]), stringsAsFactors = FALSE)))
    }
  ))

cat("\n--- Build the store at the changed scale ---\n")
t_build <- system.time({
  store <- AssessmentStore$new(ids11, sub11, comp11, w11)
  for (k in comp11) { m <- store$get_matrix(k); m[] <- round(runif(N11 * M11, 30, 100)); assign(k, m, envir = store$mats) }
})[["elapsed"]]
store_mb <- function(st) round(sum(sapply(st$component_names, function(k) as.numeric(object.size(st$get_matrix(k))))) / 1024^2, 1)
cat("Build time (s):", round(t_build, 2), "| matrices:", length(store$component_names), "each", paste(dim(store$get_matrix("CA1")), collapse = " x "), "\n")
cat("Store size (MB):", store_mb(store), "\n")

cat("\n--- Experiment 1: frequent individual-record updates ---\n")
arr11 <- array(unlist(mget(comp11, envir = store$mats), use.names = FALSE), dim = c(N11, M11, 5), dimnames = list(ids11, sub11, comp11))
upd_arr <- function(a, i, j, k, v) { a[i, j, k] <- v; a }
n_slow <- 10; n_fast <- 1000
pos <- cbind(sample(N11, n_fast), sample(M11, n_fast, TRUE), sample(5, n_fast, TRUE)); val <- sample(30:100, n_fast, TRUE)
t_copy <- system.time({ a <- arr11; for (u in seq_len(n_slow)) a <- upd_arr(a, pos[u, 1], pos[u, 2], pos[u, 3], val[u]) })[["elapsed"]] / n_slow
a2 <- arr11
t_inplace <- system.time(for (u in seq_len(n_fast)) a2[pos[u, 1], pos[u, 2], pos[u, 3]] <- val[u])[["elapsed"]] / n_fast
v1 <- StoreV1$new(marks = mget(comp11, envir = store$mats))
t_v1 <- system.time(for (u in 1:20) v1$update(comp11[pos[u, 3]], pos[u, 1], pos[u, 2], val[u]))[["elapsed"]] / 20
rm(v1); invisible(gc())
t_store <- system.time(for (u in seq_len(n_fast))
  store$update_mark(ids11[pos[u, 1]], sub11[pos[u, 2]], comp11[pos[u, 3]], val[u]))[["elapsed"]] / n_fast
upd <- data.frame(method = c("A copy 3-D array in a function", "B unchecked in-place edit",
                             "C RC v1 (list field, <<-)", "D RC v2 AssessmentStore (detach-edit)"),
                  ms_per_update = round(c(t_copy, t_inplace, t_v1, t_store) * 1000, 3),
                  validated = c("no", "no", "no", "yes"), audit_log = c("no", "no", "no", "yes"))
print(upd, right = FALSE)
cat("v1 -> v2 refinement: ", round(t_v1 / t_store), "x faster per update, with validation and audit log added.\n", sep = "")
cat("v2 costs ", round(t_store / t_inplace, 1), "x an unchecked in-place edit; that overhead buys validation + logging.\n", sep = "")
cat("Audit entries recorded:", length(store$audit), "| last entry:\n"); str(store$audit[[n_fast]])

cat("\n--- Experiment 2: adding a continuous-assessment component (CA4) ---\n")
new_mat <- matrix(round(runif(N11 * M11, 30, 100)), N11, M11, dimnames = list(ids11, sub11))
t_add_arr  <- system.time(arr_new <- array(c(arr11, new_mat), dim = c(N11, M11, 6), dimnames = list(ids11, sub11, c(comp11, "CA4"))))[["elapsed"]]
w_new <- c(CA1 = .08, CA2 = .08, CA3 = .08, CA4 = .06, model = .40, practical = .30)
t_add_store <- system.time(store$add_component("CA4", new_mat, w_new))[["elapsed"]]
cat("Add component -> rebuild array:", round(t_add_arr, 3), "s | store$add_component:", round(t_add_store, 3), "s\n")
rm(arr_new, a, a2); invisible(gc())

cat("\n--- Experiment 3: weighted totals for 100,000 x 40 ---\n")
t_tot <- system.time(tot11 <- store$weighted_total())[["elapsed"]]
cat("weighted_total():", round(t_tot, 3), "s | result dim:", paste(dim(tot11), collapse = " x "), "\n")
avg11 <- rowMeans(tot11); grade11 <- assign_grade(avg11)
cat("Grade distribution for 100,000 students:\n"); print(table(grade11))

cat("\n--- Integration with the existing framework ---\n")
one <- ids11[2500]
s4_one <- new("Student", reg_no = "192324006", name = "From store", programme = "IT", attendance = 82,
              marks = setNames(as.numeric(tot11[one, 1:5]), sub11[1:5]))
show(s4_one); str(eligibility(s4_one))
sample_ids <- sample(ids11, 50)
lg <- store$to_long(sample_ids)
bk <- xtabs(mark ~ student + subject + component, data = lg)
rt <- all(sapply(store$component_names, function(k) isTRUE(all.equal(unclass(bk)[sample_ids, sub11, k], store$get_matrix(k)[sample_ids, ], check.attributes = FALSE))))
cat("to_long() -> xtabs() round trip for 50 sampled students identical:", rt, "\n")

cat("\n--- Rejected updates (validation still enforced at scale) ---\n")
try_up <- function(label, ...) cat(sprintf("%-24s -> %s\n", label, tryCatch({ store$update_mark(...); "ACCEPTED" }, error = function(e) conditionMessage(e))))
try_up("mark = 150", "S000001", "SUB01", "CA1", 150)
try_up("unknown student", "S999999", "SUB01", "CA1", 50)
try_up("unknown component", "S000001", "SUB01", "CA9", 50)
try_up("text value", "S000001", "SUB01", "CA1", "AB")

cat("\n--- Acceptance criteria re-tested on the revised design ---\n")
c1 <- !safe_convert(c("85", "AB"), "numeric")$ok
c2 <- all(!sapply(list(function() store$update_mark("S000001", "SUB01", "CA1", 150), function() store$update_mark("NOPE", "SUB01", "CA1", 50)),
                  function(f) tryCatch({ f(); TRUE }, error = function(e) FALSE)))
c3 <- all(sapply(store$component_names, function(k) { m <- store$get_matrix(k); !is.null(rownames(m)) && !is.null(colnames(m)) }))
c5 <- t_tot < 10 && store_mb(store) < 1024
c6 <- identical(store$get_matrix("CA1")[1:5, 1:3], arr11[1:5, 1:3, "CA1"]) && "CA4" %in% store$component_names && abs(sum(store$weights) - 1) < 1e-8
acc <- data.frame(id = paste0("C", 1:6),
  criterion = c("Type safety", "Validity", "Interpretability", "Round-trip integrity", "Scalability", "Maintainability"),
  evidence = c("safe_convert() flags 'AB'", "bad value / unknown ID rejected", "every matrix has dimnames",
               "to_long() -> xtabs() identical", sprintf("weighted_total %.2f s, store %.0f MB", t_tot, store_mb(store)),
               "one-call update; CA4 added safely"),
  result = ifelse(c(c1, c2, c3, rt, c5, c6), "PASS", "FAIL"))
print(acc, right = FALSE)

cat("\n--- Cost of holding 100,000 S4 Student objects (extrapolated from Task 7) ---\n")
cat("Measured 2,000 S4 objects in", round(t_s4, 2), "s ->", round(t_s4 / 2000 * N11, 1), "s for 100,000 (linear extrapolation)\n")

png("fig/fig_t11_updates.png", width = 1600, height = 950, res = 170)
par(mar = c(5, 6, 4, 1))
barplot(upd$ms_per_update, log = "y", names.arg = c("A copy\n3-D array", "B unchecked\nin-place", "C RC v1\n(list field)", "D RC v2\n(detach-edit)"),
        col = c("#fc9272", "#9ecae1", "#fdd0a2", "#74c476"), ylab = "ms per update (log scale)", ylim = c(min(upd$ms_per_update) / 3, max(upd$ms_per_update) * 3),
        main = "Cost of one record update at 100,000 students x 40 subjects")
text(c(0.7, 1.9, 3.1, 4.3), upd$ms_per_update * 1.5, upd$ms_per_update)
dev.off()

