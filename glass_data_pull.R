
required_pkgs <- c("RSelenium", "stringr", "dplyr", "readr", "optparse")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_pkgs, collapse = ", "),
    ". Install with install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
    "))."
  )
}

library(RSelenium)
library(stringr)
library(dplyr)
library(readr)
library(optparse)

# ---- CLI options ----
option_list <- list(
  optparse::make_option(c("--url"), type="character",
              default = "https://worldhealthorg.shinyapps.io/glass-dashboard/_w_617981d8e302447388b84497ad0cefd9/#!/cta-profiles",
              help = "Dashboard URL (adjust if your CTA profiles URL differs)"),
  optparse::make_option(c("--headless"), action="store_true", default=FALSE, help="Run chrome headless"),
  optparse::make_option(c("--output"), type="character", default = "glass_last_submission.csv", help="Output CSV")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

# ---- Helper: start RSelenium ----
start_driver <- function(headless = FALSE) {
  # try rsDriver (local), fallback to connecting to remote Selenium if needed
  chrome_args <- list()
  if (headless) chrome_args <- c(chrome_args, "--headless=new", "--disable-gpu", "--window-size=1600,1200")
  chrome_args <- c(chrome_args, "--no-sandbox", "--disable-dev-shm-usage")
  # Launch; rsDriver downloads and starts a chromedriver in many setups
  rD <- tryCatch({
    rsDriver(browser = "chrome",
             chromever = "latest",
             phantomver = NULL,
             check = FALSE,
             extraCapabilities = list(
               chromeOptions = list(args = chrome_args)
             ),
             verbose = FALSE)
  }, error = function(e) {
    message("rsDriver failed to start locally: ", e$message)
    message("Please ensure chromedriver matches Chrome or run a Selenium container (see README notes).")
    stop(e)
  })
  remDr <- rD[["client"]]
  attr(remDr, "server") <- rD[["server"]]
  remDr
}

# ---- Helper: safe stop ----
stop_driver <- function(remDr) {
  tryCatch({
    server <- attr(remDr, "server")
    remDr$close()
    # stop server if present
    if (!is.null(server)) server$stop()
  }, error = function(e) NULL)
}

# ---- Helper: wait function ----
wait_for <- function(remDr, condition_fun, timeout = 15, poll = 0.5) {
  t0 <- Sys.time()
  repeat {
    if (condition_fun()) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) return(FALSE)
    Sys.sleep(poll)
  }
}

# ---- Selector helpers (you will likely need to adjust these) ----
# These are plausible selectors; inspect the dashboard page and adjust if necessary.
country_select_css <- "select#country"        # example: a <select> element with id 'country'
# if the dashboard uses shiny 'selectize', the control may be a hidden select plus an input; you might need a different selector
profile_container_candidates <- c("#cta-profiles", ".cta-profile", "#page-content", ".shiny-bound-output", "body")

# ---- Extract visible text for profile area ----
extract_profile_text <- function(remDr) {
  for (sel in profile_container_candidates) {
    res <- tryCatch({
      elems <- remDr$findElements(using = "css selector", value = sel)
      if (length(elems) == 0) return(NULL)
      txts <- vapply(elems, function(e) e$getElementText()[[1]], FUN.VALUE = "")
      txt_combined <- paste(txts, collapse = "\n")
      if (nchar(str_trim(txt_combined)) > 10) return(txt_combined)
      NULL
    }, error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  # fallback to body text
  tryCatch(remDr$findElement(using = "css selector", "body")$getElementText()[[1]], error = function(e) "")
}

# ---- Extract last year from text ----
extract_last_year_from_text <- function(text, earliest=2000, latest=as.integer(format(Sys.Date(), "%Y")) + 1) {
  if (is.null(text) || nchar(text) == 0) return(NA_integer_)
  years <- str_extract_all(text, "\\b(19\\d{2}|20\\d{2})\\b")[[1]]
  if (length(years) == 0) return(NA_integer_)
  years_num <- as.integer(years)
  years_in_range <- years_num[years_num >= earliest & years_num <= latest]
  if (length(years_in_range) > 0) return(max(years_in_range))
  # fallback: take max of any detected year
  return(max(years_num, na.rm = TRUE))
}

# ---- Collect options from a <select> or selectize control ----
collect_country_options <- function(remDr) {
  # Try standard <select> first
  sel <- tryCatch(remDr$findElement("css selector", country_select_css), error = function(e) NULL)
  if (!is.null(sel)) {
    # retrieve options
    opts <- sel$findChildElements("css selector", "option")
    out <- lapply(opts, function(o) {
      list(value = o$getElementAttribute("value")[[1]],
           label = o$getElementText()[[1]])
    })
    # filter empty
    out <- Filter(function(x) nzchar(x$value) && nzchar(x$label), out)
    return(out)
  }
  # Fallback: attempt to find visible country links/buttons (depends on dashboard)
  # implement other heuristics if needed
  return(list())
}

# ---- Select a country option ----
select_country_value <- function(remDr, value) {
  # Try to set SELECT's value via JS (works even for selectize if select present)
  js_set <- sprintf("
    s = document.querySelector('%s');
    if (s) { s.value = '%s'; s.dispatchEvent(new Event('change')); true; } else { false; }
  ", country_select_css, value)
  res <- tryCatch(remDr$executeScript(js_set, args = list()), error = function(e) FALSE)
  if (isTRUE(res)) return(TRUE)
  # fallback: find option and click
  sel <- tryCatch(remDr$findElement("css selector", country_select_css), error = function(e) NULL)
  if (is.null(sel)) stop("Country select element not found; adjust country_select_css.")
  opts <- sel$findChildElements("css selector", "option")
  for (o in opts) {
    if (o$getElementAttribute("value")[[1]] == value) {
      o$clickElement()
      return(TRUE)
    }
  }
  stop("Could not select country value: ", value)
}

# ---- main loop ----
remDr <- start_driver(headless = opt$headless)
on.exit(stop_driver(remDr), add = TRUE)

# navigate to the dashboard
remDr$navigate(opt$url)
Sys.sleep(1.5)  # let page boot

# wait for country select to appear
ok <- wait_for(remDr, function() {
  tryCatch({
    length(remDr$findElements("css selector", country_select_css)) > 0
  }, error = function(e) FALSE)
}, timeout = 20)
if (!ok) {
  stop("Country selector not found. Inspect the page and set 'country_select_css' to the correct selector.")
}

# collect countries
opts <- collect_country_options(remDr)
if (length(opts) == 0) stop("No country options found. You may need to change 'country_select_css' to match the dashboard control.")

message("Found ", length(opts), " country options. Beginning loop...")

results <- list()
i <- 0
for (opt_country in opts) {
  i <- i + 1
  label <- opt_country$label
  value <- opt_country$value
  message(sprintf("[%d/%d] %s (%s)", i, length(opts), label, value))
  try({
    select_country_value(remDr, value)
    # Wait for Shiny to be idle: a pragmatic check is to wait for profile container text to change or stable
    Sys.sleep(0.6)
    # Wait up to 12s for non-empty profile text / for dynamic rendering
    stable <- wait_for(remDr, function() {
      txt <- extract_profile_text(remDr)
      !is.null(txt) && nchar(str_trim(txt)) > 10
    }, timeout = 12, poll = 0.5)
    profile_text <- extract_profile_text(remDr)
    # quick no-data detection
    if (grepl("no data|no submissions|not available|no reported data", tolower(profile_text))) {
      results[[length(results) + 1]] <- list(label=label, value=value, last_submitted_year=NA_integer_, note="no data reported")
      message(" -> no data reported")
      next
    }
    last_year <- extract_last_year_from_text(profile_text)
    note <- if (is.na(last_year)) "no-year-found" else "inferred_from_text"
    results[[length(results) + 1]] <- list(label=label, value=value, last_submitted_year=last_year, note=note)
    message(" -> ", ifelse(is.na(last_year), "no year", last_year), " (", note, ")", sep = "")
    Sys.sleep(0.2)
  }, silent = TRUE)
}

# write CSV
out_df <- bind_rows(lapply(results, as_tibble))
write_csv(out_df, opt$output, na = "")
message("Wrote ", opt$output, " (", nrow(out_df), " rows).")

# stop driver


















  
 
