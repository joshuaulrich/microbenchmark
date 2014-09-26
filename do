#!/usr/bin/env Rscript
# vim: syntax=r

library("methods")
library("devtools", warn.conflicts=FALSE)
library("roxygen2", warn.conflicts=FALSE)

catf <- function(fmt, ...) cat(sprintf(fmt, ...))
messagef <- function(fmt, ...) message(sprintf(fmt, ...))

package_name <- function() {
  read.dcf("./DESCRIPTION", "Package")[1]
}

get_version_from_git <- function() {
  tag <- system2("git", c("describe", "--tags", "--match", "v*"),
                 stdout=TRUE, stderr=TRUE)

  ## Ignoring changes in whitespace is critical. Roxygen may have changed the
  ## spacing in the regenerated manual pages and esp. in the DESCRIPTION file
  ## because the y pretty print it compared to what write.dcf does.
  clean_args <- c("diff-index", "--ignore-space-change", "--quiet", tag)
  is_clean <- system2("git", clean_args) == 0
  version <- sub("v", "", tag, fixed=TRUE)

  ## Reformat version number by chopping of the hash at the end and
  ## appending an appropriate suffix if the tree is dirty.
  version_parts <- strsplit(version, "-")[[1]]
  version <- if (length(version_parts) == 2) {
    if (is_clean) {
      paste(version_parts, collapse="-")
    } else {
      paste0(paste(version_parts, collapse="-"), ".1")
    }
  } else if (length(version_parts) == 4) {
    revision <- if (is_clean) {
      version_parts[3]
    } else {
      revision <- as.integer(version_parts[3]) + 1
    }
    paste(paste(version_parts[1:2], collapse="-"), revision, sep=".")
  }
  version
}

do_build <- function(args) {
  do_update("all")
  message("INFO: Building package.")
  if (!file.exists("dist"))
    dir.create("dist")
  fn <- build(".", path="dist", quiet=TRUE)
  messagef("INFO: Package source tarball '%s' created.", fn)
}

do_check <- function(args) {
  do_update("all")
  check_dir <- format(Sys.time(), "check-%Y%m%d_%H%M%S")
  check_log <- file.path(check_dir, paste0(package_name(), ".Rcheck"),
                         "00check.log")
  dir.create(check_dir)
  message("INFO: Checking package.")
  ok <- tryCatch(check(".", document=FALSE, quiet=TRUE, cleanup=FALSE,
                       check_dir=check_dir),
                 error = function(e) FALSE)

  if (ok) {
    ## Read check log lines
    lines <- readLines(check_log)
    ## Find all lines containing stuff we know is OK or irrelevant
    irrelevant_indexes <- c(grep("^\\* using", lines),
                            grep("OK$", lines),
                            grep("^\\* this is package .* version .*$", lines)
                            )
    relevant_lines <- lines[-irrelevant_indexes]
    ## Output all relevant lines
    if (length(relevant_lines) > 0) {
      message("INFO: Found the following anomalies in the log:")
      message(paste("  ", relevant_lines, collapse="\n"))
      ## Because of Issue #507 we may get warnings # related to checking an UTF-8
      ## package in an ASCII locale.
      if (any(grepl(".*with.*encoding.*in.*an.*locale.*",
                    lines[relevant_lines]))) {
        message("INFO: Please ignore the encoding related WARNINGs. They are caused by devtools issue #507.")
      }
    }
    ## Remove cruft
    unlink(check_dir, recursive=TRUE)
    message("INFO: Package passed R CMD check.")
  } else {
    messagef("ERROR: Check failed. See '%s' for details.", check_log)
    ## If someone is sitting at the console, display the check logfile.
    if (isatty(stdout()))
      file.show(check_log)
  }
}

do_clean <- function(args=NULL) {
  ## Compiled code
  ofiles <- c(list.files("src", pattern=".*\\.o$", full.names=TRUE),
              list.files("src", pattern=".*\\.so$", full.names=TRUE),
              list.files("src", pattern=".*\\.dll$", full.names=TRUE))
  if (length(ofiles) > 0) {
    unlink(ofiles)
    messagef("INFO: Removed object files (%s).",
             paste0("'", ofiles, "'", collapse=", "))
  }
}

do_help <- function(args=NULL) {
  cmd_help <- function(cmd, msg) {
    messagef("  ./do %s\n      %s", cmd, msg)
  }
  message("Usage:")
  topic <- if (length(args) > 0) {
    args[1]
  } else {
    "*"
  }

  if (topic == "*") {
    cmd_help("help", "This help message.")
    cmd_help("help <command>", "Help message for <command>.")
  }
  if (topic == "*" || topic == "build")
    cmd_help("build", "Build the package.")
  if (topic == "*" || topic == "check")
    cmd_help("check", "Check the package.")
  if (topic == "*" || topic == "clean")
    cmd_help("clean", "Remove object and other temporary files.")
  if (topic == "*" || topic == "update") {
    cmd_help("update (man|documentation)",
             "Update the manual pages in './man' using roxygen.")
    cmd_help("update namespace",
             "Update the NAMESPACE file using roxygen.")
    cmd_help("update all",
             "Perform all of the above update commands.")
  }
}

do_update <- function(args=NULL) {
  roclets <- c()

  if (length(args) == 0) {
    message("ERROR: Not enough arguments for update command.")
    do_help("update")
    quit(save="no", status=1)
  }
  if (args[1] == "all" || args[1] == "documentation" || args[1] == "man") {
    message("INFO: Updating manual pages.")
    roclets <- c(roclets, "rd")
  }
  if (args[1] == "all" || args[1] == "namespace") {
    message("INFO: Updating NAMESPACE file.")
    roclets <- c(roclets, "namespace")
  }
  if (length(roclets) > 0)
    o <- capture.output(roxygenize(".", roclets=roclets, clean=TRUE))
  version <- get_version_from_git()
  message("INFO: Setting version to ", version)
  desc <- read.dcf("DESCRIPTION")
  desc[,"Version"] <- version
  write.dcf(desc, file="DESCRIPTION")
}

do_checkspelling <- function(args) {
  dictionaries <- "en_stats.rds"
  if (file.exists("./.dict.rds"))
    dictionaries <- c(dictionaries, "./.dict.rds")

  do_update("man")
  aspell(Sys.glob("man/*.Rd"), filter="Rd",
         dictionaries=dictionaries)
}

do <- function(args) {
  if (length(args) < 1) {
    do_help()
  } else if (args[1] == "help") {
    do_help(args[-1])
  } else if (args[1] == "build") {
    do_build(args[-1])
  } else if (args[1] == "check") {
    do_check(args[-1])
  } else if (args[1] == "clean") {
    do_clean(args[-1])
  } else if (args[1] == "update") {
    do_update(args[-1])
  } else if (args[1] == "check-spelling") {
    do_checkspelling()
  } else {
    messagef("ERROR: Unknown command '%s'.", args[1])
    do_help()
  }
}

if (interactive()) {
  message("WARN: 'do' is not ment to be used interactively!")
} else {
  do(commandArgs(TRUE))
}
