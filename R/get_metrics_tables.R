#' Create metrics tables
#' @param seed set seed
#' @param top20 Vector of state abbreviations for top 20 table
#' @param corr_check Check for data corrections of X-times magnitude. Default is `TRUE`
#' @return Creates metrics tables for use in covind19.org
#' @import gt
#' @import dplyr
#' @import cli
#' @importFrom tidyr drop_na
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom scales col_bin
#' @export
#' @examples
#' \dontrun{
#' dat <- get_all_data()
#' cfr    <- get_cfr(dat) %>% distinct()
#' r0_est <- get_r_est(dat)
#' tabs   <- get_metrics_tables()
#'
#' tabs$full
#' }

get_metrics_tables <- function(seed = 46342, top20 = NULL, corr_check = TRUE) {

  cli::cli_alert_info("getting data...")

  set.seed(set_seed <- seed)
  today           <- Sys.Date() - 1
  dat             <- get_all_data(corr_check = corr_check) %>% dplyr::filter(date <= today)
  cfr1            <- get_cfr(dat) %>% distinct()
  r_est           <- get_r_est(dat)
  tp              <- dat
  india_state_pop <- covid19india::pop

  cli::cli_alert_success("data load success!!")

  # pull abbrevs -----------
  use_abbrevs <- tp %>% dplyr::filter(abbrev != "la") %>% dplyr::pull(abbrev) %>% unique() %>% tolower()

  # state data ----------
  vax_dat <- get_state_vax() %>%
    dplyr::rename(total_vacc = total_doses, pct_at_least_one = pct_one_doses, pct_second = pct_two_doses, daily_vax_dose = daily_doses) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(place = ifelse(place == "India", "National estimate", place)) %>%
    dplyr::filter(date == max(date))

  sf <- tp %>%
    dplyr::group_by(place) %>%
    dplyr::filter(date > max(as.Date(date)) - 7) %>%
    dplyr::mutate(
      dailyTPR7 = daily_cases/daily_tests,
      dailyCFR7 = daily_deaths/daily_cases
    ) %>%
    dplyr::filter(is.finite(dailyTPR7)) %>%
    dplyr::mutate(dailyTPR7d = mean(dailyTPR7, na.rm = T),
                  dailyCFR7d = mean(dailyCFR7, na.rm = T)) %>%
    dplyr::filter(date == max(as.Date(date))) %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(place, total_tests, ppt, dailyTPR7d, dailyCFR7d,
                  daily_cases, daily_deaths, daily_tests, total_cases, total_deaths) %>%
    dplyr::left_join(india_state_pop, by = c("place")) %>%
    dplyr::mutate(
      place = dplyr::case_when(
        place == "India" ~ "National estimate",
        TRUE ~ place
      ),
      total_tested = trimws(format(total_tests, big.mark = ",")),
      ppt          = round(ppt * 100, digits = 2)
    ) %>%
    dplyr::left_join(vax_dat, by = c("place"))

  # table ----------
  tib <- cfr1 %>%
    dplyr::distinct(place, .keep_all = TRUE) %>%
    dplyr::left_join(r_est %>%
                       dplyr::mutate(place = recode(place, "India" = "National estimate")),
                     by = c("place")) %>%
    dplyr::left_join(tp %>%
                       extract_latest(cols = c("tpr")),
                     by = c("place")) %>%
    dplyr::left_join(sf, by = c("place")) %>%
    dplyr::mutate(perc_vaccine   = pct_at_least_one,
                  total_vacc     = format(total_vacc, big.mark = ","),
                  daily_cases    = format(daily_cases, big.mark = ","),
                  daily_deaths   = format(daily_deaths, big.mark = ","),
                  daily_tests    = format(daily_tests, big.mark = ","),
                  daily_vax_dose = format(daily_vax_dose, big.mark = ","),
                  cases          = format(total_cases, big.mark = ","),
                  deaths         = format(total_deaths, big.mark = ",")) %>%
    dplyr::rename(
      `# daily new cases`              = daily_cases,
      `# daily new deaths`             = daily_deaths,
      `7-day average daily TPR`        = dailyTPR7d,
      `7-day average daily CFR`        = dailyCFR7d,
      R                                = r,
      `daily tests`                    = daily_tests,
      `daily vaccine doses`            = daily_vax_dose,
      Location                         = place,
      CFR                              = cfr,
      `total cases`                    = cases,
      `total deaths`                   = deaths,
      `TPR`                            = tpr,
      `Total tested`                   = total_tested,
      `Percent with at least one dose` = perc_vaccine,
      `Total doses`                    = total_vacc,
      `% pop. with two shots`          = pct_second,
      `% pop. with at least one shot`  = pct_at_least_one
    ) %>%
    dplyr::arrange(dplyr::desc(`total cases`)) %>%
    dplyr::mutate(
      `7-day average daily CFR`       = round(`7-day average daily CFR`, digits = 3),
      `% pop. with two shots`         = round(`% pop. with two shots`, digits = 2),
      `% pop. with at least one shot` = round(`% pop. with at least one shot`, digits = 2)
    ) %>%
    dplyr::select(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                  `7-day average daily CFR`, Location, R, `daily tests`,
                  `daily vaccine doses`, CFR, `Total tested`, `total cases`,
                  `total deaths`, `Total doses`, `TPR`,`% pop. with two shots`,
                  `% pop. with at least one shot`)

  tib <- tib %>%
    dplyr::select(-`Total tested`) %>%
    dplyr::mutate(Location = case_when(
      Location == "National estimate" ~ "India",
      TRUE ~ Location)
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate_if(is.character, trimws) %>%
    tidyr::drop_na(`# daily new cases`) %>%
    dplyr::filter(`# daily new cases` != "NA")

  source_note_text <- glue(
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
        color     = "#999",
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
        color     = "#999",
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
  point_in_time <- tib %>%
    select(-`total cases`, -`total deaths`, -`TPR`, -CFR,
           -`Total doses`, -`% pop. with two shots`, -`% pop. with at least one shot`) %>%
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
        color     = "#999",
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
        color     = "#999",
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

  cumulative <- tib %>%
    select(-`# daily new cases`, -`# daily new deaths`, -`7-day average daily TPR`,
           -`7-day average daily CFR`, -R, -`daily tests`, -`daily vaccine doses`) %>%
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
        color     = "#999",
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
        color     = "#999",
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

    t20_tib <- tib %>%
      dplyr::left_join(
        pop %>% dplyr::select(-population), by = c("Location" = "place")
      ) %>%
      dplyr::filter(abbrev %in% unique(c(top20, "tt"))) %>%
      dplyr::select(-abbrev)

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
          color     = "#999",
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
          color     = "#999",
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
    t20_point_in_time <- t20_tib %>%
      select(-`total cases`, -`total deaths`, -`TPR`, -CFR,
             -`Total doses`, -`% pop. with two shots`, -`% pop. with at least one shot`) %>%
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
          color     = "#999",
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
          color     = "#999",
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

    t20_cumulative <- t20_tib %>%
      select(-`# daily new cases`, -`# daily new deaths`, -`7-day average daily TPR`,
             -`7-day average daily CFR`, -R, -`daily tests`, -`daily vaccine doses`) %>%
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
          color     = "#999",
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
          color     = "#999",
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
