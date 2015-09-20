#!/usr/bin/env Rscript

## NOTE: We do not load all required packages upfront because most
##   commands only require a subset or no extra packages at all and loading
##   packages is expensive. So in the interest of fast startup, they are
##   loaded on demand by the respective functions.

SELFUPGRADE_URL <- "https://raw.githubusercontent.com/olafmersmann/do-r/master/do"

catf <- function(fmt, ...) cat(sprintf(fmt, ...))
messagef <- function(fmt, ...) message(sprintf(fmt, ...))

die <- function(status, fmt, ...) {
  if (!missing(fmt))
    messagef(paste0("ERROR: ", fmt), ...)
  quit(save="no", status=status)
}

parse_arguments <- function(arguments) {
  res <- list()
  equal_pos <- regexpr("=", arguments, fixed=TRUE)
  keys <- substr(arguments, 1, equal_pos-1)
  keys <- gsub('-', '_', keys, fixed=TRUE)

  values <- substr(arguments, equal_pos + 1, nchar(arguments))
  res <- as.list(values)
  names(res) <- keys
  res
}

help_line <- function(commandline, helptext) {
  messagef("  ./do %s", commandline)
  messagef(paste(strwrap(helptext, indent=6, exdent=6), collapse="\n"))
}

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

do_build <- function(...) {
  library("devtools", warn.conflicts=FALSE)
  args <- list(...)
  if (length(args) > 0) {
    if (args[[1]] != "help") {
      messagef("ERROR: Invalid subcommand '%s' given.", args[[1]])
    }
    help_line("build", "Build a source package.")
    return(invisible())
  }
  do_update("all")
  message("INFO: Building package.")
  if (!file.exists("dist"))
    dir.create("dist")
  fn <- build(".", path="dist", quiet=TRUE)
  messagef("INFO: Package source tarball '%s' created.", fn)
  invisible(fn)
}

do_check <- function(subcommand, ...) {
  if (missing(subcommand))
    subcommand <- "package"
  if (subcommand == "help" || ! subcommand %in% c("package", "spelling")) {
    help_line("check (package)", "Run R CMD check on package.")
    help_line("check spelling", "Check spelling of man pages.")
    return(invisible())
  }

  if (subcommand == "package") {
    library("devtools", warn.conflicts=FALSE)
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
  } else if (subcommand == "spelling") {
    dictionaries <- "en_stats.rds"
    if (file.exists("./.dict.rds"))
      dictionaries <- c(dictionaries, "./.dict.rds")

    do_update("man")
    aspell(Sys.glob("man/*.Rd"), filter="Rd",
           dictionaries=dictionaries)
  }
}

do_clean <- function(...) {
  l <- list(...)
  if (length(l) > 0 && l[[1]] == "help") {
    help_line("clean", "Remove cruft from 'src/' directory.")
    return(invisible())
  }

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

do_help <- function(cmd, ...) {
  message("Usage:")
  if (missing(cmd) || cmd == "all") {
    for (thing in sort(ls(.GlobalEnv))) {
      if (thing != "do_help" && regexpr("^do_.*", thing) > 0) {
        if (exists(thing, mode="function")) {
          command <- get(thing, mode="function")
          command("help")
        }
      }
    }
  } else {
    command_name <- paste0("do_", cmd)
    if (exists(command_name, mode="function")) {
      command <- get(command_name, mode="function")
      message("Usage:")
      command("help")
    } else {
      messagef("ERROR: Unknown subcommand '%s'. No help available.'",
               command_name)
    }
  }
}

do_update <- function(subcommand, ...) {
  if (missing(subcommand))
    subcommand <- "help"

  if (subcommand == "help") {
    help_line("update collate", "Update collation order of files.")
    help_line("update man", "Update manual pages using Roxygen2.")
    help_line("update namespace",
              "Update NAMESPACE file using Roxygen2.")
    help_line("update version",
              "Update version field in DESCRIPTION based on SCM.")
    help_line("update all", "All of the above.")
  } else if (subcommand == "man") {
    library("roxygen2", warn.conflicts=FALSE)
    message("INFO: Updating manual pages")
    capture.output(roxygenize(".", roclets="rd", clean=TRUE))
  } else if (subcommand == "version") {
    version <- get_version_from_git()
    message("INFO: Setting version to ", version)
    desc <- read.dcf("DESCRIPTION")
    desc[,"Version"] <- version
    write.dcf(desc, file="DESCRIPTION")
  } else if (subcommand == "namespace") {
    library("roxygen2", warn.conflicts=FALSE)
    message("INFO: Updating NAMESPACE file")
    capture.output(roxygenize(".", roclets="namespace", clean=TRUE))
  } else if (subcommand == "all") {
    do_update("namespace")
    do_update("man")
    do_update("version")
  }
 }

do_selfupgrade <- function(...) {
  args <- list(...)
  if (length(args) > 0 && args[[1]] == "help") {
    help_line("selfupgrade", "Replace do script with latest version from public repository. Note: This will unconditionally overwrite any changes you may have made to the do script.")
    return(invisible())
  }
  if (file.exists("do.tmp"))
    die(100, "File 'do.tmp' from previous selfupgrade attempt exists. Please remove it and retry.")

  download.file(SELFUPGRADE_URL, "do.tmp", method="curl")
  if (file.exists("do.tmp")) {
    ## Check that we can parse the script.
    ok <- tryCatch({
      parse("do.tmp")
      TRUE
    }, error=function(e) FALSE)
    if (ok) {
      file.rename("do.tmp", "do")
      ## Avoid user modifications by only setting read and execute bits on do
      Sys.chmod("do", mode="0555", use_umask=FALSE)
    } else {
      die(100, "The new do script (file 'do.tmp') appears to be corrupt. Please investigate!")
    }
  } else {
    die(100, "Failed to download new do script to file 'do.tmp'.")
  }
}

do_drat <- function(subcommand, ...) {
  if (missing(subcommand))
    subcommand <- "help"

  if (subcommand == "help") {
    help_line("drat publish", "Publish package to drat repository.")
  } else if (subcommand == "publish") {
    library("drat", warn.conflicts=FALSE)
    fn <- do_build()
    insertPackage(fn, "../drat", commit=TRUE)
    message("INFO: Source package published to drat repository '../drat'.")
  }
}

main <- function(cmd, ...) {
  if (missing(cmd))
    cmd = "help"
  command_name <- paste0("do_", cmd)
  if (exists(command_name, mode="function")) {
    command <- get(command_name, mode="function")
    command(...)
  } else {
    messagef("ERROR: Unknown command '%s' given.", cmd)
    do_help()
  }
}

if (interactive()) {
  message("WARN: 'do' is not ment to be used interactively! Consider using devtools instead.")
} else {
  args <- parse_arguments(commandArgs(TRUE))
  do.call(main, args)
}

# vim: filetype=r
