# TraceExplorer.DSNeuralRNAS
# CLEAN FINAL RELEASE SCRIPT
#
# This script:
#   1. validates the clean source tree;
#   2. regenerates Rd documentation only;
#   3. runs the full testthat suite;
#   4. builds and checks 0.10.2 with R CMD check --as-cran --no-manual;
#   5. installs into a clean temporary library and runs a smoke test;
#   6. promotes DESCRIPTION metadata to 1.0.0;
#   7. rebuilds, rechecks, reinstalls, and re-runs the smoke test;
#   8. creates the final tarball and freeze manifest under _release/final/.
#
# It never relies on relative log paths after changing directories.

.te_this_file <- tryCatch(
  normalizePath(
    sys.frame(1)$ofile,
    winslash = "/",
    mustWork = TRUE
  ),
  error = function(e) NA_character_
)

if (!is.na(.te_this_file)) {
  root <- dirname(dirname(.te_this_file))
} else {
  root <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}

description_path <- file.path(root, "DESCRIPTION")

if (!file.exists(description_path)) {
  stop(
    "DESCRIPTION was not found. Source tools/FINAL_RELEASE.R from the clean project.",
    call. = FALSE
  )
}

dcf <- read.dcf(description_path)

if (!identical(
  unname(dcf[1, "Package"]),
  "TraceExplorer.DSNeuralRNAS"
)) {
  stop(
    "This is not the TraceExplorer.DSNeuralRNAS project.",
    call. = FALSE
  )
}

release_dir <- file.path(root, "_release")
final_dir <- file.path(release_dir, "final")
logs_dir <- file.path(release_dir, "logs")

if (dir.exists(release_dir)) {
  unlink(
    release_dir,
    recursive = TRUE,
    force = TRUE
  )
}

dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n")
cat("============================================================\n")
cat(" TraceExplorer.DSNeuralRNAS — CLEAN FINAL RELEASE\n")
cat("============================================================\n")
cat("Project:", root, "\n")
cat("Check mode: R CMD check --as-cran --no-manual\n\n")

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

.te_fail <- function(...) {
  stop(
    paste0(...),
    call. = FALSE
  )
}

.te_with_dir_cmd <- function(
    command,
    args,
    wd,
    log,
    env = character()) {

  wd <- normalizePath(
    wd,
    winslash = "/",
    mustWork = TRUE
  )

  log <- normalizePath(
    dirname(log),
    winslash = "/",
    mustWork = TRUE
  ) |>
    file.path(basename(log))

  old <- getwd()

  on.exit(
    setwd(old),
    add = TRUE
  )

  setwd(wd)

  status <- tryCatch(
    system2(
      command,
      args = args,
      stdout = log,
      stderr = log,
      env = env
    ),
    error = function(e) {
      cat(
        "\nSYSTEM2 ERROR:\n",
        conditionMessage(e),
        "\n",
        file = log,
        append = TRUE
      )
      999L
    }
  )

  if (is.null(status)) {
    status <- 0L
  }

  as.integer(status)
}

.te_flatten <- function(x) {
  if (is.null(x) || !length(x)) {
    return("")
  }

  if (inherits(x, "condition")) {
    return(conditionMessage(x))
  }

  if (is.atomic(x)) {
    return(
      paste(
        as.character(x),
        collapse = " | "
      )
    )
  }

  paste(
    capture.output(
      str(
        x,
        give.attr = FALSE,
        vec.len = 6L,
        list.len = 6L
      )
    ),
    collapse = " "
  )
}

.te_test_counts <- function(res) {
  df <- as.data.frame(res)

  num <- function(name) {
    if (!name %in% names(df)) {
      return(0L)
    }

    x <- df[[name]]

    if (is.logical(x)) {
      return(sum(x, na.rm = TRUE))
    }

    suppressWarnings(
      sum(
        as.integer(x),
        na.rm = TRUE
      )
    )
  }

  list(
    df = df,
    failed = num("failed"),
    error = num("error"),
    warning = num("warning"),
    skipped = num("skipped"),
    passed = if ("passed" %in% names(df)) {
      sum(df$passed, na.rm = TRUE)
    } else {
      NA_integer_
    }
  )
}

.te_save_test_results <- function(df, path) {
  export <- df

  for (nm in names(export)) {
    if (is.list(export[[nm]])) {
      export[[nm]] <- vapply(
        export[[nm]],
        .te_flatten,
        character(1)
      )
    }
  }

  utils::write.csv(
    export,
    path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

.te_assert_no_non_ascii_R <- function() {
  files <- list.files(
    file.path(root, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )

  bad <- character()

  for (f in files) {
    raw <- readBin(
      f,
      what = "raw",
      n = file.info(f)$size
    )

    if (any(as.integer(raw) > 127L)) {
      bad <- c(
        bad,
        basename(f)
      )
    }
  }

  if (length(bad)) {
    .te_fail(
      "Non-ASCII bytes remain in R source: ",
      paste(bad, collapse = ", ")
    )
  }

  invisible(TRUE)
}

.te_build <- function(tag) {
  work <- file.path(
    release_dir,
    paste0("work_", tag)
  )

  if (dir.exists(work)) {
    unlink(
      work,
      recursive = TRUE,
      force = TRUE
    )
  }

  dir.create(
    work,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Build from a physically curated source snapshot, not from the working
  # project tree. This guarantees that release logs, README, tools, CLEAN_*
  # helpers and _release cannot enter the source tarball.
  stage_parent <- file.path(
    work,
    "stage"
  )

  stage_pkg <- file.path(
    stage_parent,
    "TraceExplorer.DSNeuralRNAS"
  )

  dir.create(
    stage_pkg,
    recursive = TRUE,
    showWarnings = FALSE
  )

  source_files <- c(
    "DESCRIPTION",
    "NAMESPACE",
    "LICENSE"
  )

  source_dirs <- c(
    "R",
    "man",
    "tests",
    "inst"
  )

  missing_files <- source_files[
    !file.exists(
      file.path(
        root,
        source_files
      )
    )
  ]

  missing_dirs <- source_dirs[
    !dir.exists(
      file.path(
        root,
        source_dirs
      )
    )
  ]

  if (length(missing_files) || length(missing_dirs)) {
    .te_fail(
      "Cannot create clean source stage [",
      tag,
      "]. Missing: ",
      paste(
        c(
          missing_files,
          missing_dirs
        ),
        collapse = ", "
      )
    )
  }

  copied_files <- file.copy(
    file.path(
      root,
      source_files
    ),
    stage_pkg,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = FALSE
  )

  if (!all(copied_files)) {
    .te_fail(
      "Could not copy package metadata into clean stage [",
      tag,
      "]."
    )
  }

  for (d in source_dirs) {
    ok <- file.copy(
      file.path(root, d),
      stage_pkg,
      recursive = TRUE,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = FALSE
    )

    if (!isTRUE(ok)) {
      .te_fail(
        "Could not copy directory ",
        d,
        " into clean stage [",
        tag,
        "]."
      )
    }
  }

  # Normalize staged file timestamps to one hour in the past so that a local
  # clock/ZIP timestamp discrepancy cannot trigger "future file timestamps".
  staged_files <- list.files(
    stage_pkg,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  info <- file.info(
    staged_files
  )

  staged_files <- staged_files[
    !is.na(info$isdir) &
    !info$isdir
  ]

  if (length(staged_files)) {
    Sys.setFileTime(
      staged_files,
      Sys.time() - 3600
    )
  }

  log <- file.path(
    logs_dir,
    paste0(tag, "_build.log")
  )

  rbin <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "R.exe" else "R"
  )

  status <- .te_with_dir_cmd(
    rbin,
    c(
      "CMD",
      "build",
      shQuote(stage_pkg)
    ),
    work,
    log
  )

  if (status != 0L) {
    .te_fail(
      "R CMD build failed [",
      tag,
      "]. See ",
      log
    )
  }

  tarballs <- list.files(
    work,
    pattern = "^TraceExplorer\\.DSNeuralRNAS_.*\\.tar\\.gz$",
    full.names = TRUE
  )

  if (!length(tarballs)) {
    .te_fail(
      "R CMD build returned success but no tarball was created [",
      tag,
      "]."
    )
  }

  normalizePath(
    tarballs[
      which.max(
        file.info(tarballs)$mtime
      )
    ],
    winslash = "/",
    mustWork = TRUE
  )
}

.te_check <- function(tarball, tag) {
  work <- file.path(
    release_dir,
    paste0("check_", tag)
  )

  dir.create(
    work,
    recursive = TRUE,
    showWarnings = FALSE
  )

  local_tar <- file.path(
    work,
    basename(tarball)
  )

  if (!file.copy(
    tarball,
    local_tar,
    overwrite = TRUE
  )) {
    .te_fail(
      "Could not copy tarball into clean check workspace [",
      tag,
      "]."
    )
  }

  log <- file.path(
    logs_dir,
    paste0(tag, "_check_console.log")
  )

  rbin <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "R.exe" else "R"
  )

  status <- .te_with_dir_cmd(
    rbin,
    c(
      "CMD",
      "check",
      "--as-cran",
      "--no-manual",
      shQuote(basename(local_tar))
    ),
    work,
    log,
    env = c(
      "_R_CHECK_CRAN_INCOMING_REMOTE_=false"
    )
  )

  checks <- list.dirs(
    work,
    recursive = FALSE,
    full.names = TRUE
  )

  checks <- checks[
    grepl(
      "\\.Rcheck$",
      checks
    )
  ]

  if (!length(checks)) {
    excerpt <- if (file.exists(log)) {
      tail(
        readLines(
          log,
          warn = FALSE
        ),
        80L
      )
    } else {
      "No console log was produced."
    }

    cat(
      paste(
        excerpt,
        collapse = "\n"
      ),
      "\n"
    )

    .te_fail(
      "R CMD check did not create an .Rcheck directory [",
      tag,
      "]. See ",
      log
    )
  }

  selected <- checks[
    which.max(
      file.info(checks)$mtime
    )
  ]

  check_log <- file.path(
    selected,
    "00check.log"
  )

  if (!file.exists(check_log)) {
    .te_fail(
      "00check.log is missing [",
      tag,
      "]."
    )
  }

  lines <- readLines(
    check_log,
    warn = FALSE
  )

  file.copy(
    check_log,
    file.path(
      logs_dir,
      paste0(tag, "_00check.log")
    ),
    overwrite = TRUE
  )

  status_lines <- grep(
    "^Status:",
    lines,
    value = TRUE
  )

  clean_status <- !length(status_lines) ||
    all(
      trimws(status_lines) == "Status: OK"
    )

  if (
    status != 0L ||
    !clean_status
  ) {
    cat(
      "\nLast 100 lines of ",
      tag,
      " 00check.log:\n",
      sep = ""
    )

    cat(
      paste(
        tail(lines, 100L),
        collapse = "\n"
      ),
      "\n"
    )

    .te_fail(
      "R CMD check gate failed [",
      tag,
      "]. See ",
      file.path(
        logs_dir,
        paste0(tag, "_00check.log")
      )
    )
  }

  cat(
    "CHECK PASS [",
    tag,
    "]: 0 errors | 0 warnings | 0 notes\n",
    sep = ""
  )

  invisible(check_log)
}

.te_install_smoke <- function(tarball, tag, expected_version) {
  lib <- file.path(
    release_dir,
    paste0("lib_", tag)
  )

  if (dir.exists(lib)) {
    unlink(
      lib,
      recursive = TRUE,
      force = TRUE
    )
  }

  dir.create(
    lib,
    recursive = TRUE,
    showWarnings = FALSE
  )

  install_log <- file.path(
    logs_dir,
    paste0(tag, "_install.log")
  )

  smoke_log <- file.path(
    logs_dir,
    paste0(tag, "_smoke.log")
  )

  rbin <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "R.exe" else "R"
  )

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  status <- .te_with_dir_cmd(
    rbin,
    c(
      "CMD",
      "INSTALL",
      paste0("--library=", shQuote(lib)),
      shQuote(tarball)
    ),
    release_dir,
    install_log
  )

  if (status != 0L) {
    .te_fail(
      "Clean install failed [",
      tag,
      "]. See ",
      install_log
    )
  }

  smoke_file <- file.path(
    release_dir,
    paste0("smoke_", tag, ".R")
  )

  q <- function(x) {
    encodeString(
      x,
      quote = "\""
    )
  }

  smoke <- c(
    paste0(
      ".libPaths(c(",
      q(lib),
      ", .libPaths()))"
    ),
    paste0("library(TraceExplorer.DSNeuralRNAS, lib.loc = ", q(lib), ")"),
    paste0(
      "stopifnot(as.character(packageVersion('TraceExplorer.DSNeuralRNAS', lib.loc = ",
      q(lib),
      ")) == ",
      q(expected_version),
      ")"
    ),
    "tr <- te_demo_mlp_trace(12)",
    "stopifnot(inherits(tr, 'te_trace'))",
    "stopifnot(nrow(te_trace_data(tr)) >= 2L)",
    "stopifnot(all(te_validate_trace(te_trace_data(tr))$status == 'PASS'))",
    "sp <- te_learning_space(tr)",
    "stopifnot(is.data.frame(sp), nrow(sp) >= 2L)",
    "ev <- te_evidence_stack(tr)",
    "stopifnot(inherits(ev, 'te_evidence_stack'))",
    "stopifnot(all(te_validate_evidence_stack(ev)$status == 'PASS'))",
    "app <- system.file('shiny', 'app', 'app.R', package = 'TraceExplorer.DSNeuralRNAS')",
    "stopifnot(nzchar(app), file.exists(app))",
    "parse(file = app)",
    "if (requireNamespace('DSNeuralRNAS.FormalSpec', quietly = TRUE)) {",
    "  fs <- te_run_formalspec(tr)",
    "  stopifnot(inherits(fs, 'te_formalspec_analysis'))",
    "  stopifnot(all(te_validate_formalspec_analysis(fs)$status == 'PASS'))",
    "}",
    "cat('SMOKE PASS\\n')"
  )

  writeLines(
    smoke,
    smoke_file,
    useBytes = TRUE
  )

  status <- .te_with_dir_cmd(
    rscript,
    c(
      "--vanilla",
      shQuote(smoke_file)
    ),
    release_dir,
    smoke_log
  )

  if (status != 0L) {
    .te_fail(
      "Installed-package smoke test failed [",
      tag,
      "]. See ",
      smoke_log
    )
  }

  cat(
    "INSTALL + SMOKE PASS [",
    tag,
    "]\n",
    sep = ""
  )

  invisible(lib)
}

.te_set_version <- function(version) {
  lines <- readLines(
    description_path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  lines <- sub(
    "^Version:.*$",
    paste0(
      "Version: ",
      version
    ),
    lines
  )

  writeLines(
    lines,
    description_path,
    useBytes = TRUE
  )
}

.te_manifest <- function() {
  files <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  files <- files[
    file.info(files)$isdir %in% FALSE
  ]

  rel <- substring(
    files,
    nchar(root) + 2L
  )

  keep <- !grepl(
    "^_release/",
    rel
  )

  files <- files[keep]
  rel <- rel[keep]

  md5 <- tools::md5sum(files)

  data.frame(
    file = rel,
    md5 = unname(md5),
    stringsAsFactors = FALSE
  )
}

.te_final_release_main <- function() {
  # ------------------------------------------------------------------
  # Preflight
  # ------------------------------------------------------------------

  required_packages <- c(
    "roxygen2",
    "pkgload",
    "testthat",
    "shiny",
    "plotly",
    "DT",
    "DSNeuralRNAS.FormalSpec",
    "DSNeuralRNAS",
    "ML.DSNeuralRNAS"
  )

  available <- vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )

  if (!all(available)) {
    .te_fail(
      "Missing required release packages: ",
      paste(
        required_packages[!available],
        collapse = ", "
      )
    )
  }

  current_version <- unname(
    read.dcf(description_path)[1, "Version"]
  )

  if (!identical(
    current_version,
    "0.10.2"
  )) {
    .te_fail(
      "Clean RC must begin at Version 0.10.2; found ",
      current_version,
      "."
    )
  }

  auth <- unname(
    read.dcf(description_path)[1, "Authors@R"]
  )

  if (!grepl(
    "rmorev@unp.edu.pe",
    auth,
    fixed = TRUE
  )) {
    .te_fail(
      "Expected maintainer email is missing from Authors@R."
    )
  }

  .te_assert_no_non_ascii_R()

  cat("PREFLIGHT PASS\n")

  # Keep a backup so any failure after promotion restores the RC version.
  description_backup <- readLines(
    description_path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  release_success <- FALSE

  on.exit({
    if (!release_success) {
      writeLines(
        description_backup,
        description_path,
        useBytes = TRUE
      )

      cat(
        "\nRelease did not complete. DESCRIPTION restored to 0.10.2.\n"
      )
    }
  }, add = TRUE)

  # ------------------------------------------------------------------
  # Documentation + testthat
  # ------------------------------------------------------------------

  cat("\nRegenerating Rd documentation...\n")

  roxygen2::roxygenise(
    root,
    roclets = "rd"
  )

  man_dir <- file.path(
    root,
    "man"
  )

  rd_files <- list.files(
    man_dir,
    pattern = "\\.Rd$",
    full.names = TRUE
  )

  if (!length(rd_files)) {
    .te_fail(
      "Roxygen generated no man/*.Rd files."
    )
  }

  cat(
    "RD PASS: ",
    length(rd_files),
    " files\n",
    sep = ""
  )

  cat("\nLoading clean source and running full testthat suite...\n")

  pkgload::load_all(
    root,
    quiet = TRUE
  )

  test_res <- testthat::test_dir(
    file.path(
      root,
      "tests",
      "testthat"
    ),
    reporter = "summary",
    stop_on_failure = FALSE,
    stop_on_warning = FALSE
  )

  tc <- .te_test_counts(
    test_res
  )

  .te_save_test_results(
    tc$df,
    file.path(
      logs_dir,
      "testthat_results.csv"
    )
  )

  cat(
    "\nTESTTHAT: FAIL ",
    tc$failed,
    " | ERROR ",
    tc$error,
    " | WARN ",
    tc$warning,
    " | SKIP ",
    tc$skipped,
    " | PASS ",
    tc$passed,
    "\n",
    sep = ""
  )

  if (
    tc$failed != 0L ||
    tc$error != 0L ||
    tc$warning != 0L ||
    tc$skipped != 0L
  ) {
    .te_fail(
      "Full testthat release gate failed."
    )
  }

  cat("TESTTHAT PASS\n")

  # ------------------------------------------------------------------
  # RC 0.10.2 check + install
  # ------------------------------------------------------------------

  cat("\n--- RC 0.10.2 BUILD / CHECK / INSTALL ---\n")

  rc_tar <- .te_build(
    "rc_0.10.2"
  )

  .te_check(
    rc_tar,
    "rc_0.10.2"
  )

  .te_install_smoke(
    rc_tar,
    "rc_0.10.2",
    "0.10.2"
  )

  # ------------------------------------------------------------------
  # Promotion to 1.0.0
  # ------------------------------------------------------------------

  cat("\nPromoting metadata to Version 1.0.0...\n")

  .te_set_version(
    "1.0.0"
  )

  if (!identical(
    unname(
      read.dcf(description_path)[1, "Version"]
    ),
    "1.0.0"
  )) {
    .te_fail(
      "Version promotion to 1.0.0 failed."
    )
  }

  cat("\n--- FINAL 1.0.0 BUILD / CHECK / INSTALL ---\n")

  final_tar <- .te_build(
    "final_1.0.0"
  )

  .te_check(
    final_tar,
    "final_1.0.0"
  )

  .te_install_smoke(
    final_tar,
    "final_1.0.0",
    "1.0.0"
  )

  # ------------------------------------------------------------------
  # Freeze
  # ------------------------------------------------------------------

  final_tar_copy <- file.path(
    final_dir,
    basename(final_tar)
  )

  file.copy(
    final_tar,
    final_tar_copy,
    overwrite = TRUE
  )

  manifest <- .te_manifest()

  utils::write.csv(
    manifest,
    file.path(
      final_dir,
      "FREEZE_MANIFEST_1.0.0.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  freeze_id <- "TE-1.0.0-DSNEURAL-FORMALSPEC"

  fs_version <- tryCatch(
    as.character(
      utils::packageVersion(
        "DSNeuralRNAS.FormalSpec"
      )
    ),
    error = function(e) "unknown"
  )

  freeze_txt <- c(
    paste0(
      "freeze_id=",
      freeze_id
    ),
    "package=TraceExplorer.DSNeuralRNAS",
    "version=1.0.0",
    paste0(
      "frozen_at=",
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %z"
      )
    ),
    paste0(
      "formalspec_package_version=",
      fs_version
    ),
    "formalspec_article_freeze=DSFS-1.0.0-FIRST-ARTICLE",
    paste0(
      "testthat_pass=",
      tc$passed
    ),
    "testthat_fail=0",
    "testthat_error=0",
    "testthat_warn=0",
    "testthat_skip=0",
    "check_mode=R CMD check --as-cran --no-manual",
    "rc_check=PASS",
    "rc_clean_install=PASS",
    "rc_smoke=PASS",
    "final_check=PASS",
    "final_clean_install=PASS",
    "final_smoke=PASS"
  )

  writeLines(
    freeze_txt,
    file.path(
      final_dir,
      "FREEZE_1.0.0.txt"
    ),
    useBytes = TRUE
  )

  release_success <- TRUE

  cat("\n")
  cat("============================================================\n")
  cat(" TE-1.0.0-DSNEURAL-FORMALSPEC READY\n")
  cat("============================================================\n")
  cat("Final tarball:\n")
  cat(final_tar_copy, "\n")
  cat("Freeze manifest:\n")
  cat(
    file.path(
      final_dir,
      "FREEZE_MANIFEST_1.0.0.csv"
    ),
    "\n"
  )
  cat("Freeze record:\n")
  cat(
    file.path(
      final_dir,
      "FREEZE_1.0.0.txt"
    ),
    "\n"
  )
  cat("\nTraceExplorer.DSNeuralRNAS 1.0.0 is frozen.\n")

}

.te_final_release_main()
rm(.te_final_release_main)
