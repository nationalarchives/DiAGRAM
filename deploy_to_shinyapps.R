if (!requireNamespace("rsconnect")) install.packages("rsconnect")
if (!requireNamespace("stringr")) install.packages("stringr")
if (!requireNamespace("cli")) install.packages("cli")

deploy = function(account = "jumpingrivers", server = "shinyapps.io") {
  cli::cli_h1("Deploying app")
  rsconnect::setAccountInfo(name = account,
                            token = Sys.getenv("shinyapps_io_token"),
                            secret = Sys.getenv("shinyapps_io_secret"))
  slug = stringr::str_match(Sys.getenv('TRAVIS_REPO_SLUG'), "/(.*)")[1, 2]
  appName = paste(slug, Sys.getenv("TRAVIS_BRANCH"), sep = "-")
  rsconnect::deployApp(account = account, server = server, appName = appName)
  cli::cli_alert_success("{appName} successfully deployed")

}

terminate = function(account = "jumpingrivers", server = "shinyapps.io") {
  msg = Sys.getenv("TRAVIS_COMMIT_MESSAGE")
  if (stringr::str_detect(msg, "^Merge pull", negate = TRUE)) return(NULL)

  cli::cli_h1("Terminating app")
  branch = stringr::str_match(msg, "/([^-\\s]*)")[1, 2]
  slug = stringr::str_match(Sys.getenv('TRAVIS_REPO_SLUG'), "/(.*)")[1, 2]

  appName = paste(slug, branch, sep = '-')
  rsconnect::terminateApp(appName = appName, account = account, server = server)
  cli::cli_alert_success("{appName} successfully terminated")
}



deploy()
terminate()


