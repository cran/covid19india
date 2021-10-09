#' Create metrics tables
#' @param seed set seed
#' @param top20 Vector of state abbreviations for top 20 table
#' @param corr_check Check for data corrections of X-times magnitude. Default is `TRUE`
#' @return Creates metrics tables for use in covind19.org
#' @import gt
#' @import data.table
#' @import cli
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom scales col_bin
#' @export
#' @examples
#' \dontrun{
#' tabs   <- get_metrics_tables()
#'
#' tabs$full
#' }

get_metrics_tables <- function(seed = 46342, top20 = NULL, corr_check = TRUE) {

  cli::cli_alert_info("getting data...")

  set.seed(set_seed <- seed)
  today           <- Sys.Date() - 1
  dat             <- get_all_data(corr_check = corr_check)[date <= today]
  cfr1            <- unique(get_cfr(dat))
  r_est           <- get_r_est(dat)
  tp              <- dat
  india_state_pop <- covid19india::pop

  cli::cli_alert_success("data load success!!")

  # pull abbrevs -----------
  use_abbrevs <- tolower(unique(tp[abbrev != "la", abbrev]))

  # state data ----------
  vax_dat <- get_state_vax()
  setnames(vax_dat, c("total_doses", "pct_one_dose", "pct_two_doses", "daily_doses"), c("total_vacc", "pct_at_least_one", "pct_second", "daily_vax_dose"))

  vax_dat <- na.omit(vax_dat)[,place := data.table::fifelse(place == "India", "National estimate", place)][date == max(date)]

  # TEMPORARY: pull ICMR for the national tests data (expires October 30, 2021)
  icmr <- fread("https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv", showProgress = FALSE)

  setnames(icmr, names(icmr), janitor::make_clean_names(names(icmr)))

  icmr <- icmr[, date := as.Date(tested_as_of, "%d/%m/%Y") - 1][, .(date, total_samples_tested, sample_reported_today)]

  icmr_nat_date_limit <- range(icmr$date, na.rm = TRUE)

  tp_nat <- tp[tp$place == "India" & tp$date <= icmr_nat_date_limit[2] & tp$date >= icmr_nat_date_limit[1]]

  icmr    <- utils::tail(icmr, 30)
  tp_nat  <- utils::tail(tp_nat, 30)
  tp_rest <- tp[tp$place != "India"]

  tp_nat$daily_tests <- as.numeric(icmr$sample_reported_today)
  tp_nat[, tpr := daily_cases / daily_tests]
  # tp_nat$tpr         <- tp_nat$daily_cases / tp_nat$daily_tests

  tp <- rbindlist(list(tp_nat, tp_rest))

  setkeyv(tp, cols = "place")

  sf <- unique(tp[, .SD[date > max(as.Date(date) - 7)], by = "place"][
    , `:=` (dailyTPR7 = daily_cases / daily_tests, dailyCFR7 = daily_deaths / daily_cases)
  ][
    , `:=` (dailyTPR7d = mean(dailyTPR7, na.rm = T), dailyCFR7d = mean(dailyCFR7, na.rm = T)), by = "place"
  ][, .SD[date == max(date)], by = "place"])

  sf <- sf[, .(place, total_tests, ppt, dailyTPR7d, dailyCFR7d,
               daily_cases, daily_deaths, daily_tests, total_cases, total_deaths)]

  sf <- data.table::merge.data.table(sf, covid19india::pop, by = "place", all.x = TRUE)[
    , `:=` (place = data.table::fifelse(place == "India", "National estimate", place),
            total_tested = trimws(format(total_tests, big.mark = ",")),
            ppt = round(ppt * 100, digits = 2))
  ]

  sf <- data.table::merge.data.table(sf, vax_dat, by = "place", all.x = TRUE)

  # table ----------
  tib <- cfr1[, .(place, cfr)]

  tib <- data.table::merge.data.table(tib, r_est[, place := data.table::fifelse(place == "India", "National estimate", place)][, .(place, r)], by = "place", all.x = TRUE)

  tib <- data.table::merge.data.table(tib, extract_latest(tp, clmns = c("tpr")), by = "place", all.x = TRUE)

  tib <- data.table::merge.data.table(tib, sf, by = "place", all.x = TRUE)[
    , `:=` (
      perc_vaccine   = pct_at_least_one,
      total_vacc     = format(total_vacc, big.mark = ","),
      daily_cases    = format(daily_cases, big.mark = ","),
      daily_deaths   = format(daily_deaths, big.mark = ","),
      daily_tests    = format(daily_tests, big.mark = ","),
      daily_vax_dose = format(daily_vax_dose, big.mark = ","),
      cases          = format(total_cases, big.mark = ","),
      deaths         = format(total_deaths, big.mark = ",")
    )
  ]

  setnames(tib,
           old = c( "daily_cases", "daily_deaths", "dailyTPR7d", "dailyCFR7d", "r", "daily_tests", "daily_vax_dose", "place", "cfr", "cases", "deaths", "tpr", "total_tested", "perc_vaccine", "total_vacc", "pct_second", "pct_at_least_one"),
           new = c("# daily new cases", "# daily new deaths", "7-day average daily TPR", "7-day average daily CFR", "R", "daily tests", "daily vaccine doses", "Location", "CFR", "total cases","total deaths", "TPR", "Total tested", "Percent with at least one dose", "Total doses", "% pop. with two shots", "% pop. with at least one shot"))

  tib <- tib[order(-`total_cases`)][
    , `:=` (
      `7-day average daily CFR`       = round(`7-day average daily CFR`, digits = 3),
      `% pop. with two shots`         = round(`% pop. with two shots`, digits = 2),
      `% pop. with at least one shot` = round(`% pop. with at least one shot`, digits = 2)
    )
  ][
    , .(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
        `7-day average daily CFR`, Location, R, `daily tests`,
        `daily vaccine doses`, CFR, `Total tested`, `total cases`,
        `total deaths`, `Total doses`, `TPR`,`% pop. with two shots`,
        `% pop. with at least one shot`)
  ]

  tib <- unique(tib[, !c("Total tested")][, Location := data.table::fifelse(Location == "National estimate", "India", Location)])

  source_note_text <- glue::glue(
    "**\uA9 COV-IND-19 Study Group**<br>**Source data:** covid19india.org<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown.
      States are omitted if they have missing case count data.
      <br>
      **Abbrev:** CFR, Case-fatality rate."
  )

  tabl <- tib %>%
    gt() %>%
    # format table body text
    tab_style(
      style     = cell_text(size = px(14), font = "helvetica"),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body((Location))
    ) %>%
    # format column names
    tab_style(
      style = cell_text(
        size      = px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    # format numbers
    fmt_number(
      columns  = c(CFR, `7-day average daily TPR`, TPR),
      decimals = 3
    ) %>%
    fmt_number(
      columns  = c(R),
      decimals = 2
    ) %>%
    # random formatting
    tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = px(4)
    ) %>%
    # column widths
    cols_width(
      Location ~ px(150),
      c(R, CFR) ~ px(75),
      everything() ~ px(100)
    ) %>%
    cols_align(
      align   = "center",
      columns = everything()
    ) %>%
    # title
    tab_header(
      title    = md("**Assessing COVID-19 in India**"),
      subtitle = glue("data through {format(today, '%B %e')}")
    ) %>%
    # caption
    tab_source_note(
      source_note = md(source_note_text)
    ) %>%
    # add and format column spanners
    tab_spanner(
      label   = "Point in time metrics",
      columns = c(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                  `7-day average daily CFR`, R, `daily tests`, `daily vaccine doses`)
    ) %>%
    tab_spanner(
      label   = "Cumulative metrics",
      columns = c(`total cases`, `total deaths`, `TPR`, CFR,
                  `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
    ) %>%
    cols_move_to_start((Location)) %>%
    tab_style(
      style = cell_text(
        size      = px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = c("Point in time metrics", "Cumulative metrics"))
    ) %>%
    # adjust title font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(24))),
      locations = list(cells_title(groups = "title"))
    ) %>%
    # adjust subtitle font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(18))),
      locations = list(cells_title(groups = "subtitle"))
    ) %>%
    # color cells based on values
    data_color(
      columns = c(R),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
    ) %>%
    data_color(
      columns = c(`7-day average daily TPR`),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
    ) %>%
    # highlight national estimate
    tab_style(
      style = cell_fill(color = "#fcf8d4"),
      locations = cells_body(
        rows = Location == "India")
    ) %>%
    tab_style(
      style = cell_borders(sides = "left"),
      locations = cells_body(columns = (`total cases`))
    ) %>%
    tab_style(
      style = cell_borders(sides = "left"),
      locations = cells_column_labels(columns = (`total cases`))
    ) %>%
    tab_style(
      style = cell_borders(sides = "left"),
      locations = cells_column_spanners(("Cumulative metrics"))
    )

  cli::cli_alert_success("full table made")

  # new table
  point_in_time <- tib[, !c("total cases", "total deaths", "TPR", "CFR",
                            "Total doses", "% pop. with two shots",
                            "% pop. with at least one shot")] %>%
    gt() %>%
    # format table body text
    tab_style(
      style     = cell_text(size = px(14), font = "helvetica"),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(c(Location))
    ) %>%
    # format column names
    tab_style(
      style = cell_text(
        size      = px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    # format numbers
    fmt_number(
      columns  = c(`7-day average daily TPR`),
      decimals = 3
    ) %>%
    fmt_number(
      columns  = c(R),
      decimals = 2
    ) %>%
    # random formatting
    tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = px(4)
    ) %>%
    # column widths
    cols_width(
      Location ~ px(150),
      R ~ px(75),
      everything() ~ px(100)
    ) %>%
    cols_align(
      align   = "center",
      columns = everything()
    ) %>%
    # title
    tab_header(
      title    = md("**Assessing COVID-19 in India**"),
      subtitle = glue("data through {format(today, '%B %e')}")
    ) %>%
    # caption
    tab_source_note(
      source_note = md(source_note_text)
    ) %>%
    tab_spanner(
      label   = "Point in time metrics",
      columns = c(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                  `7-day average daily CFR`, R, `daily tests`, `daily vaccine doses`)
    ) %>%
    cols_move_to_start((Location)) %>%
    tab_style(
      style = cell_text(
        size      = px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = c("Point in time metrics")) #, glue("Predictions on ({format(today + 21, '%m/%d')}) (No intervention)")
    ) %>%
    # adjust title font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(24))),
      locations = list(cells_title(groups = "title"))
    ) %>%
    # adjust subtitle font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(18))),
      locations = list(cells_title(groups = "subtitle"))
    ) %>%
    # color cells based on values
    data_color(
      columns = c(R),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
    ) %>%
    data_color(
      columns = c(`7-day average daily TPR`),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
    ) %>%
    # highlight national estimate
    tab_style(
      style = cell_fill(color = "#fcf8d4"),
      locations = cells_body(
        rows = Location == "India")
    )

  cli::cli_alert_success("point-in-time table made")

  cumulative <- tib[, !c("# daily new cases", "# daily new deaths", "7-day average daily TPR",
                         "7-day average daily CFR", "R", "daily tests", "daily vaccine doses")] %>%
    gt() %>%
    # format table body text
    tab_style(
      style     = cell_text(size = px(14), font = "helvetica"),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(c(Location))
    ) %>%
    # format column names
    tab_style(
      style = cell_text(
        size      = px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    # format numbers
    fmt_number(
      columns  = c(CFR, TPR),
      decimals = 3
    ) %>%
    # random formatting
    tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = px(4)
    ) %>%
    # column widths
    cols_width(
      (Location) ~ px(150),
      (CFR) ~ px(75),
      everything() ~ px(100)
    ) %>%
    cols_align(
      align   = "center",
      columns = everything()
    ) %>%
    # title
    tab_header(
      title    = md("**Assessing COVID-19 in India**"),
      subtitle = glue("data through {format(today, '%B %e')}")
    ) %>%
    # caption
    tab_source_note(
      source_note = md(source_note_text)
    ) %>%
    tab_spanner(
      label   = "Cumulative metrics",
      columns = c(`total cases`, `total deaths`, `TPR`, CFR,
                  `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
    ) %>%
    cols_move_to_start((Location)) %>%
    tab_style(
      style = cell_text(
        size      = px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = c("Cumulative metrics"))
    ) %>%
    # adjust title font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(24))),
      locations = list(cells_title(groups = "title"))
    ) %>%
    # adjust subtitle font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(18))),
      locations = list(cells_title(groups = "subtitle"))
    ) %>%
    # highlight national estimate
    tab_style(
      style = cell_fill(color = "#fcf8d4"),
      locations = cells_body(rows = Location == "India")
    )

  cli::cli_alert_success("cumulative table made")

  if (!is.null(top20)) {

    t20_tib <- data.table::merge.data.table(tib, covid19india::pop[, !c("population")], by.x = "Location", by.y = "place")[abbrev %in% unique(c(top20, "tt"))][, !c("abbrev")][]

    source_note_text <- glue(
      "**\uA9 COV-IND-19 Study Group**<br>**Source data:** covid19india.org<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown.
      States are omitted if they have missing case count data.
      <br>
      **Abbrev:** CFR, Case-fatality rate."
    )

    t20_tabl <- t20_tib %>%
      gt() %>%
      # format table body text
      tab_style(
        style     = cell_text(size = px(14), font = "helvetica"),
        locations = cells_body()
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body((Location))
      ) %>%
      # format column names
      tab_style(
        style = cell_text(
          size      = px(12),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # format numbers
      fmt_number(
        columns  = c(CFR, `7-day average daily TPR`, TPR),
        decimals = 3
      ) %>%
      fmt_number(
        columns  = c(R),
        decimals = 2
      ) %>%
      # random formatting
      tab_options(
        column_labels.border.top.style    = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        table_body.border.bottom.color    = "#0000001A",
        data_row.padding                  = px(4)
      ) %>%
      # column widths
      cols_width(
        Location ~ px(150),
        c(R, CFR) ~ px(75),
        everything() ~ px(100)
      ) %>%
      cols_align(
        align   = "center",
        columns = everything()
      ) %>%
      # title
      tab_header(
        title    = md("**Assessing COVID-19 in India**"),
        subtitle = glue("data through {format(today, '%B %e')}")
      ) %>%
      # caption
      tab_source_note(
        source_note = md(source_note_text)
      ) %>%
      # add and format column spanners
      tab_spanner(
        label   = "Point in time metrics",
        columns = c(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                    `7-day average daily CFR`, R, `daily tests`, `daily vaccine doses`)
      ) %>%
      tab_spanner(
        label   = "Cumulative metrics",
        columns = c(`total cases`, `total deaths`, `TPR`, CFR,
                    `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
      ) %>%
      cols_move_to_start((Location)) %>%
      tab_style(
        style = cell_text(
          size      = px(14),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = c("Point in time metrics", "Cumulative metrics"))
      ) %>%
      # adjust title font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(24))),
        locations = list(cells_title(groups = "title"))
      ) %>%
      # adjust subtitle font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(18))),
        locations = list(cells_title(groups = "subtitle"))
      ) %>%
      # color cells based on values
      data_color(
        columns = c(R),
        colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
      ) %>%
      data_color(
        columns = c(`7-day average daily TPR`),
        colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
      ) %>%
      # highlight national estimate
      tab_style(
        style = cell_fill(color = "#fcf8d4"),
        locations = cells_body(
          rows = Location == "India")
      ) %>%
      tab_style(
        style = cell_borders(sides = "left"),
        locations = cells_body(columns = (`total cases`))
      ) %>%
      tab_style(
        style = cell_borders(sides = "left"),
        locations = cells_column_labels(columns = (`total cases`))
      ) %>%
      tab_style(
        style = cell_borders(sides = "left"),
        locations = cells_column_spanners(("Cumulative metrics"))
      )

    cli::cli_alert_success("full top 20 table made")

    # new table
    t20_point_in_time <- t20_tib[, !c("total cases", "total deaths", "TPR", "CFR",
                                      "Total doses", "% pop. with two shots",
                                      "% pop. with at least one shot")] %>%
      gt() %>%
      # format table body text
      tab_style(
        style     = cell_text(size = px(14), font = "helvetica"),
        locations = cells_body()
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(c(Location))
      ) %>%
      # format column names
      tab_style(
        style = cell_text(
          size      = px(12),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # format numbers
      fmt_number(
        columns  = c(`7-day average daily TPR`),
        decimals = 3
      ) %>%
      fmt_number(
        columns  = c(R),
        decimals = 2
      ) %>%
      # random formatting
      tab_options(
        column_labels.border.top.style    = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        table_body.border.bottom.color    = "#0000001A",
        data_row.padding                  = px(4)
      ) %>%
      # column widths
      cols_width(
        Location ~ px(150),
        R ~ px(75),
        everything() ~ px(100)
      ) %>%
      cols_align(
        align   = "center",
        columns = everything()
      ) %>%
      # title
      tab_header(
        title    = md("**Assessing COVID-19 in India**"),
        subtitle = glue("data through {format(today, '%B %e')}")
      ) %>%
      # caption
      tab_source_note(
        source_note = md(source_note_text)
      ) %>%
      tab_spanner(
        label   = "Point in time metrics",
        columns = c(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                    `7-day average daily CFR`, R, `daily tests`, `daily vaccine doses`)
      ) %>%
      cols_move_to_start((Location)) %>%
      tab_style(
        style = cell_text(
          size      = px(14),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = c("Point in time metrics")) #, glue("Predictions on ({format(today + 21, '%m/%d')}) (No intervention)")
      ) %>%
      # adjust title font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(24))),
        locations = list(cells_title(groups = "title"))
      ) %>%
      # adjust subtitle font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(18))),
        locations = list(cells_title(groups = "subtitle"))
      ) %>%
      # color cells based on values
      data_color(
        columns = c(R),
        colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
      ) %>%
      data_color(
        columns = c(`7-day average daily TPR`),
        colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
      ) %>%
      # highlight national estimate
      tab_style(
        style = cell_fill(color = "#fcf8d4"),
        locations = cells_body(
          rows = Location == "India")
      )

    cli::cli_alert_success("top 20 point-in-time table made")

    t20_cumulative <- t20_tib[, !c("# daily new cases", "# daily new deaths", "7-day average daily TPR",
                                   "7-day average daily CFR", "R", "daily tests", "daily vaccine doses")] %>%
      gt() %>%
      # format table body text
      tab_style(
        style     = cell_text(size = px(14), font = "helvetica"),
        locations = cells_body()
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(c(Location))
      ) %>%
      # format column names
      tab_style(
        style = cell_text(
          size      = px(12),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # format numbers
      fmt_number(
        columns  = c(CFR, TPR),
        decimals = 3
      ) %>%
      # random formatting
      tab_options(
        column_labels.border.top.style    = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        table_body.border.bottom.color    = "#0000001A",
        data_row.padding                  = px(4)
      ) %>%
      # column widths
      cols_width(
        (Location) ~ px(150),
        (CFR) ~ px(75),
        everything() ~ px(100)
      ) %>%
      cols_align(
        align   = "center",
        columns = everything()
      ) %>%
      # title
      tab_header(
        title    = md("**Assessing COVID-19 in India**"),
        subtitle = glue("data through {format(today, '%B %e')}")
      ) %>%
      # caption
      tab_source_note(
        source_note = md(source_note_text)
      ) %>%
      tab_spanner(
        label   = "Cumulative metrics",
        columns = c(`total cases`, `total deaths`, `TPR`, CFR,
                    `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
      ) %>%
      cols_move_to_start((Location)) %>%
      tab_style(
        style = cell_text(
          size      = px(14),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = c("Cumulative metrics"))
      ) %>%
      # adjust title font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(24))),
        locations = list(cells_title(groups = "title"))
      ) %>%
      # adjust subtitle font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(18))),
        locations = list(cells_title(groups = "subtitle"))
      ) %>%
      # highlight national estimate
      tab_style(
        style = cell_fill(color = "#fcf8d4"),
        locations = cells_body(rows = Location == "India")
      )

    cli::cli_alert_success("top 20 cumulative table made")

  }

  if (is.null(top20)) {
    return(list(full       = tabl,
         point_in_time     = point_in_time,
         cumulative        = cumulative
    ))
  } else {
    return(list(full       = tabl,
         point_in_time     = point_in_time,
         cumulative        = cumulative,
         full_t20          = t20_tabl,
         point_in_time_t20 = t20_point_in_time,
         cumulative_t20    = t20_cumulative
    ))
  }

}
