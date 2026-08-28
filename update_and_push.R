# update_and_push.R
#
# Run this (source it, or click "Source" in RStudio) any time you've
# edited the package and want to document, check, and push it to GitHub
# in one go. It stops before pushing if devtools::check() finds any
# errors or warnings, so you never push a broken build.
#
# Usage: just source the file. It will ask you for a commit message
# interactively, and lets you cancel cleanly if you change your mind.

update_and_push <- function() {
  
  # --- 0. Make sure we're in the package root -------------------------------
  if (!file.exists("DESCRIPTION")) {
    stop(
      "No DESCRIPTION file found in the current working directory.\n",
      "Set your working directory to the package root first ",
      "(e.g. via Session > Set Working Directory in RStudio, or open the ",
      ".Rproj file), then re-run this script."
    )
  }
  
  # --- 1. Regenerate documentation / NAMESPACE -------------------------------
  message("== Running devtools::document() ==")
  devtools::document()
  
  # --- 2. Run a full check ---------------------------------------------------
  message("\n== Running devtools::check() ==")
  check_result <- devtools::check(quiet = TRUE)
  
  n_errors   <- length(check_result$errors)
  n_warnings <- length(check_result$warnings)
  n_notes    <- length(check_result$notes)
  
  message(sprintf(
    "\ncheck() result: %d error(s), %d warning(s), %d note(s)",
    n_errors, n_warnings, n_notes
  ))
  
  if (n_errors > 0 || n_warnings > 0) {
    stop(
      "devtools::check() found errors or warnings above \u2014 fix these before ",
      "pushing. (NOTEs alone won't block the push.)"
    )
  }
  
  if (n_notes > 0) {
    message("There are NOTE(s) above. Review them; proceeding anyway.")
  }
  
  # --- 3. Check git status ----------------------------------------------------
  git_status <- system("git status --porcelain", intern = TRUE)
  
  if (length(git_status) == 0) {
    message("\nNothing to commit \u2014 working tree is already clean. Nothing pushed.")
    return(invisible(NULL))
  }
  
  message("\n== Changed files ==")
  message(paste(git_status, collapse = "\n"))
  
  # --- 4. Stage everything -----------------------------------------------------
  system("git add .")
  
  # --- 5. Ask what to do, then commit ---------------------------------------
  choice <- readline(paste0(
    "What do you want to do?\n",
    "  1) Commit without message\n",
    "  2) Commit with message\n",
    "  3) Cancel\n",
    "Choice [1/2/3]: "
  ))
  choice <- trimws(choice)
  
  if (choice == "3") {
    system("git restore --staged .")
    message("Cancelled. Changes unstaged; nothing committed or pushed.")
    return(invisible(NULL))
  } else if (choice == "2") {
    commit_msg <- readline("Commit message: ")
    if (nchar(trimws(commit_msg)) == 0) {
      commit_msg <- paste0("Update ", format(Sys.time(), "%Y-%m-%d %H:%M"))
      message(sprintf("Empty message \u2014 using default: \"%s\"", commit_msg))
    }
  } else if (choice == "1") {
    commit_msg <- paste0("Update ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    message(sprintf("Using default message: \"%s\"", commit_msg))
  } else {
    system("git restore --staged .")
    message("Unrecognised choice \u2014 cancelling. Changes unstaged; nothing committed or pushed.")
    return(invisible(NULL))
  }
  
  # Use a temp file for the message to safely handle quotes/special characters
  msg_file <- tempfile()
  writeLines(commit_msg, msg_file)
  commit_out <- system(paste("git commit -F", shQuote(msg_file)), intern = TRUE)
  message(paste(commit_out, collapse = "\n"))
  unlink(msg_file)
  
  # --- 6. Push -----------------------------------------------------------------
  message("\n== Pushing to GitHub ==")
  push_out <- system("git push 2>&1", intern = TRUE)
  message(paste(push_out, collapse = "\n"))
  
  message("\nDone. Pushed to GitHub.")
  invisible(NULL)
}

update_and_push()
