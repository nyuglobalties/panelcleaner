context("Homogenization")

test_that("Variable name homogenization works", {
  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(id = ids_1, time = 1, q1 = sample(1:5, 100, replace = TRUE), q2 = sample(0:1, 100, replace = TRUE), q3 = sample(letters[1:10], 100, replace = TRUE), stringsAsFactors = FALSE)
  wave_2 <- data.frame(id = ids_2, time = 2, question1 = sample(1:5, 100, replace = TRUE), Q2 = sample(0:1, 100, replace = TRUE), q3 = sample(letters[1:10], 100, replace = TRUE), stringsAsFactors = FALSE)

  mapping <- tibble::tribble(
    ~name_t1, ~coding_t1, ~name_t2, ~coding_t2, ~panel_name, ~homogenized_name, ~homogenized_coding,
    "id", NA_character_, "id", NA_character_, "test_panel", "id", NA_character_,
    "time", NA_character_, "time", NA_character_, "test_panel", "time", NA_character_,
    "q1", NA_character_, "question1", NA_character_, "test_panel", "question_1", NA_character_,
    "q2", NA_character_, "Q2", NA_character_, "test_panel", "question_2", NA_character_,
    "q3", NA_character_, "q3", NA_character_, "test_panel", "question_3", NA_character_
  )

  panel_map <- panel_mapping(
    mapping,
    c("t1", "t2"),
    .schema = list(
      wave_name = "name",
      wave_coding = "coding",
      panel = "panel_name",
      homogenized_name = "homogenized_name",
      homogenized_coding = "homogenized_coding"
    )
  )

  panel <-
    enpanel("test_panel", t1 = wave_1, t2 = wave_2) %>%
    add_mapping(panel_map)

  homogenized_panel <- homogenize_panel(panel)

  for (w in homogenized_panel$waves) {
    expect_true(setequal(
      names(wave(homogenized_panel, w)),
      panel_map$homogenized_name
    ))
  }
})

test_that("Missing variables don't stop homogenization, if desired", {
  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(id = ids_1, time = 1, q1 = sample(1:5, 100, replace = TRUE), q2 = sample(0:1, 100, replace = TRUE), stringsAsFactors = FALSE)
  wave_2 <- data.frame(id = ids_2, time = 2, question1 = sample(1:5, 100, replace = TRUE), Q2 = sample(0:1, 100, replace = TRUE), stringsAsFactors = FALSE)

  mapping <- tibble::tribble(
    ~name_t1, ~coding_t1, ~name_t2, ~coding_t2, ~panel_name, ~homogenized_name, ~homogenized_coding,
    "id", NA_character_, "id", NA_character_, "test_panel", "id", NA_character_,
    "time", NA_character_, "time", NA_character_, "test_panel", "time", NA_character_,
    "q1", NA_character_, "question1", NA_character_, "test_panel", "question_1", NA_character_,
    "q2", NA_character_, "Q2", NA_character_, "test_panel", "question_2", NA_character_,
    "q3", NA_character_, "q3", NA_character_, "test_panel", "question_3", NA_character_
  )

  panel_map <- panel_mapping(
    mapping,
    c("t1", "t2"),
    .schema = list(
      wave_name = "name",
      wave_coding = "coding",
      panel = "panel_name",
      homogenized_name = "homogenized_name",
      homogenized_coding = "homogenized_coding"
    )
  )

  panel <-
    enpanel("test_panel", t1 = wave_1, t2 = wave_2) %>%
    add_mapping(panel_map)

  homogenized_panel <- homogenize_panel(panel, error_missing_raw_variables = FALSE)

  expect_true(has_issues(homogenized_panel))
  expect_identical(
    names(issues(homogenized_panel)),
    c("missing_raw_variables_t1", "missing_raw_variables_t2")
  )
})

test_that("Coding homogenization works", {
  set.seed(1182)

  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(
    id = ids_1,
    time = 1,
    q1 = sample(1:5, 100, replace = TRUE),
    q2 = sample(1:5, 100, replace = TRUE)
  )

  wave_2 <- data.frame(
    id = ids_2,
    time = 2,
    question1 = sample(1:5, 100, replace = TRUE),
    question2 = sample(1:5, 100, replace = TRUE)
  )

  coding_1 <- bquote(
    coding(
      code("Never", 1),
      code("Rarely", 2),
      code("Sometimes", 3),
      code("Frequently", 4),
      code("Always", 5)
    )
  )

  coding_2 <- bquote(
    coding(
      code("Never", 5),
      code("Rarely", 4),
      code("Sometimes", 3),
      code("Frequently", 2),
      code("Always", 1)
    )
  )

  coding_h <- bquote(
    coding(
      code("Never", 1),
      code("Rarely", 2),
      code("Sometimes", 3),
      code("Frequently", 4),
      code("Always", 5)
    )
  )

  single_deparse <- function(expr) {
    paste0(deparse(expr), collapse = "")
  }

  mapping <- tibble::tribble(
    ~name_t1, ~coding_t1, ~name_t2, ~coding_t2, ~panel_name, ~homogenized_name, ~homogenized_coding,
    "id", NA_character_, "id", NA_character_, "test_panel", "id", NA_character_,
    "q1", single_deparse(coding_1), "question1", single_deparse(coding_2), "test_panel", "question_1", single_deparse(coding_h),
    "q2", single_deparse(coding_1), "question2", single_deparse(coding_2), "test_panel", "question_2", single_deparse(coding_h)
  )

  panel_map <- panel_mapping(
    mapping,
    c("t1", "t2"),
    .schema = list(
      wave_name = "name",
      wave_coding = "coding",
      panel = "panel_name",
      homogenized_name = "homogenized_name",
      homogenized_coding = "homogenized_coding"
    )
  )

  panel <-
    enpanel("test_panel", t1 = wave_1, t2 = wave_2) %>%
    add_mapping(panel_map) %>%
    homogenize_panel()

  for (w in panel$waves) {
    expect_true(
      rcoder::matches_coding(
        wave(panel, w)$question_1,
        rcoder::eval_coding(coding_h)
      )
    )
  }

  homogenized_panel <- bind_waves(panel)
  expect_true(inherits(homogenized_panel, "homogenized_panel"))

  panel_df <- as.data.frame(homogenized_panel)

  expect_true(inherits(panel_df, "mapped_df"))
  expect_true(identical(attr(panel_df, "panel_name"), homogenized_panel$name))
  expect_true(identical(attr(panel_df, "mapping"), panel_map))
})

test_that("Partial homogenization works", {
  set.seed(1182)

  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(
    id = ids_1,
    time = 1,
    q1 = sample(1:5, 100, replace = TRUE),
    q2 = sample(1:5, 100, replace = TRUE)
  )

  wave_2 <- data.frame(
    id = ids_2,
    time = 2,
    question1 = sample(1:5, 100, replace = TRUE),
    question2 = sample(1:5, 100, replace = TRUE)
  )

  coding_1 <- bquote(
    coding(
      code("Never", 1),
      code("Rarely", 2),
      code("Sometimes", 3),
      code("Frequently", 4),
      code("Always", 5)
    )
  )

  coding_2 <- bquote(
    coding(
      code("Never", 5),
      code("Rarely", 4),
      code("Sometimes", 3),
      code("Frequently", 2),
      code("Always", 1)
    )
  )

  coding_h <- bquote(
    coding(
      code("Never", 1),
      code("Rarely", 2),
      code("Sometimes", 3),
      code("Frequently", 4),
      code("Always", 5)
    )
  )

  single_deparse <- function(expr) {
    paste0(deparse(expr), collapse = "")
  }

  mapping <- tibble::tribble(
    ~name_1, ~coding_1, ~name_2, ~coding_2, ~panel_name, ~homogenized_name, ~homogenized_coding,
    NA_character_, NA_character_, "id", NA_character_, "test_panel", "id", NA_character_,
    NA_character_, NA_character_, "question1", single_deparse(coding_2), "test_panel", "question_1", single_deparse(coding_h),
    NA_character_, NA_character_, "question2", single_deparse(coding_2), "test_panel", "question_2", single_deparse(coding_h)
  )

  panel_map <- panel_mapping(
    mapping,
    1:2,
    .schema = list(
      wave_name = "name",
      wave_coding = "coding",
      panel = "panel_name",
      homogenized_name = "homogenized_name",
      homogenized_coding = "homogenized_coding"
    )
  )

  panel <-
    enpanel("test_panel", `2` = wave_2) %>%
    add_mapping(panel_map) %>%
    homogenize_panel() %>%
    bind_waves()

  panel_dat <- as.data.frame(panel)
  expect_true(setequal(unique(panel_dat$wave), "2"))
  expect_true(setequal(unique(panel_dat$question_2), 1:5))
})

test_that("description mapping works", {
  set.seed(1182)

  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(
    id = ids_1
  )

  wave_2 <- data.frame(
    id = ids_2
  )

  # Fails because wave descriptions are needed
  mapping_bad <- tibble::tribble(
    ~name_1, ~coding_1, ~name_2, ~coding_2, ~panel, ~homogenized_name, ~homogenized_coding, ~homogenized_description, # nolint: line_length_linter
    "id", NA_character_, "id", NA_character_, "test_panel", "id", NA_character_, "Test description"
  )
  err <- expect_error(panel_mapping(mapping_bad, 1:2), class = "tk_error")
  expect_true(grepl("description", err$message))

  mapping <- tibble::tribble(
    ~name_1, ~coding_1, ~description_1, ~name_2, ~coding_2, ~description_2, ~panel, ~homogenized_name, ~homogenized_coding, ~homogenized_description, # nolint: line_length_linter
    "id", NA_character_, "Test description", "id", NA_character_, "Test description", "test_panel", "ID", NA_character_, "An example description" # nolint: line_length_linter
  )
  panel_map <- panel_mapping(mapping, 1:2)

  panel <-
    enpanel("test_panel", wave_1, wave_2) %>%
    add_mapping(panel_map) %>%
    homogenize_panel()

  expect_identical(
    get_attr(wave(panel, 1)$ID, "bpr.description"),
    "An example description"
  )

  expect_identical(
    get_attr(wave(panel, 2)$ID, "bpr.description"),
    "An example description"
  )

  # If homogenizing description, homogenized description must be filled in if wave descs exist
  bad_mapping_no_hom_desc <- tibble::tribble(
    ~name_1, ~coding_1, ~description_1, ~name_2, ~coding_2, ~description_2, ~panel, ~homogenized_name, ~homogenized_coding, ~homogenized_description, # nolint: line_length_linter
    "id", NA_character_, "Test description", "id", NA_character_, "Test description", "test_panel", "id", NA_character_, NA_character_ # nolint: line_length_linter
  )

  err <- expect_error(
    enpanel("test_panel", wave_1, wave_2) %>%
      add_mapping(panel_mapping(bad_mapping_no_hom_desc, 1:2)) %>%
      homogenize_panel(),
    class = "tk_error"
  )
  expect_true(grepl("missing homogenized", err$message))

  # Conversely, wave descs must be filled in if homogenized description exists
  bad_mapping_no_wave_desc <- tibble::tribble(
    ~name_1, ~coding_1, ~description_1, ~name_2, ~coding_2, ~description_2, ~panel, ~homogenized_name, ~homogenized_coding, ~homogenized_description, # nolint: line_length_linter
    "id", NA_character_, "Test description", "id", NA_character_, NA_character_, "test_panel", "id", NA_character_, "An example description" # nolint: line_length_linter
  )

  err <- expect_error(
    enpanel("test_panel", wave_1, wave_2) %>%
      add_mapping(panel_mapping(bad_mapping_no_wave_desc, 1:2)) %>%
      homogenize_panel(),
    class = "tk_error"
  )
  expect_true(grepl("missing descriptions", err$message))
})
