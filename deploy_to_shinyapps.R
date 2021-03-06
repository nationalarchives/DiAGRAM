if (!requireNamespace("rsconnect")) install.packages("rsconnect")
if (!requireNamespace("stringr")) install.packages("stringr")
if (!requireNamespace("cli")) install.packages("cli")
# Add Biconductor repos
options(repos = BiocManager::repositories())

# Required for packrat to get a repo
install_pkg = function() {
  cli::cli_h1("Install pkg")
  path = paste(Sys.getenv('TRAVIS_REPO_SLUG'), Sys.getenv("TRAVIS_BRANCH"), sep = "@")
  cli::cli_alert_info("Installing {path}")
  remotes::install_github(path, upgrade = "never")
  cli::cli_alert_success("{path} installed!")
}

deploy = function(account = "nationalarchives", server = "shinyapps.io") {
  cli::cli_h1("Deploying app")
  rsconnect::setAccountInfo(name = account,
                            token = Sys.getenv("shinyapps_io_token"),
                            secret = Sys.getenv("shinyapps_io_secret"))
  slug = stringr::str_match(Sys.getenv('TRAVIS_REPO_SLUG'), "/(.*)")[1, 2]
  if (Sys.getenv("TRAVIS_BRANCH") == "master") {
    appName = paste(slug)
  }  else {
    appName = paste(slug, Sys.getenv("TRAVIS_BRANCH"), sep = "-")
  }
  rsconnect::deployApp(
    account = account, server = server,
    appDir = system.file("example", package = "diagramNAT"), appName = appName)
  cli::cli_alert_success("{appName} successfully deployed")
}

terminate = function(account = "nationalarchives", server = "shinyapps.io") {
  msg = Sys.getenv("TRAVIS_COMMIT_MESSAGE")
  if (stringr::str_detect(msg, "^Merge pull", negate = TRUE)) return(NULL)

  cli::cli_h1("Terminating app")
  branch = stringr::str_match(msg, "/([^-\\s]*)")[1, 2]
  slug = stringr::str_match(Sys.getenv('TRAVIS_REPO_SLUG'), "/(.*)")[1, 2]

  appName = paste(slug, branch, sep = '-')
  rsconnect::terminateApp(appName = appName, account = account, server = server)
  cli::cli_alert_success("{appName} successfully terminated")
}


install_pkg()
deploy()
terminate()


