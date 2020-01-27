context("Panel functionality")

test_that("Basic panel structure holds", {
  set.seed(1182)

  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(id = ids_1, time = 1, q1 = sample(1:5, 100, replace = TRUE), q2 = sample(0:1, 100, replace = TRUE), q3 = sample(letters[1:10], 100, replace = TRUE), stringsAsFactors = FALSE)
  wave_2 <- data.frame(id = ids_2, time = 2, question1 = sample(1:5, 100, replace = TRUE), Q2 = sample(0:1, 100, replace = TRUE), q3 = sample(letters[1:10], 100, replace = TRUE), stringsAsFactors = FALSE)

  mapping <- tibble::tribble(
    ~ name_t1, ~ coding_t1, ~ name_t2, ~ coding_t2, ~ panel_name, ~ homogenized_name, ~ homogenized_coding,
    "id", NA_character_, "id", NA_character_, "test_panel", "id", NA_character_,
    "time", NA_character_, "time", NA_character_, "test_panel", "time", NA_character_,
    "q1", NA_character_, "question1", NA_character_, "test_panel", "question_1", NA_character_,
    "q2", NA_character_, "Q2", NA_character_, "test_panel", "question_2", NA_character_,
    "q3", NA_character_, "q3", NA_character_, "test_panel", "question_3", NA_character_
  )

  panel <- enpanel(list(t1 = wave_1), "test_panel")
  expect_identical(enpanel(wave_1, "test_panel", waves = "t1"), panel)

  expect_true(is_unhomogenized_panel(panel))
  expect_identical(wave(panel, "t1"), wave_1)

  panel <- add_wave(panel, wave_2, "t2")
  expect_identical(wave(panel, "t2"), wave_2)

  expect_error(panel_mapping(mapping, c("t1", "t2")))
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

  expect_true(is.panel_mapping(panel_map))
})

test_that("Coding homogenization works", {
  set.seed(1182)

  ids_1 <- sample(1:500, 100)
  ids_2 <- ids_1
  ids_2[sample(1:100, 10)] <- sample(500:1000, 10)

  wave_1 <- data.frame(id = ids_1, time = 1, q1 = sample(1:5, 100, replace = TRUE))
  wave_2 <- data.frame(id = ids_2, time = 2, question1 = sample(1:5, 100, replace = TRUE))

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
    ~ name_t1, ~ coding_t1, ~ name_t2, ~ coding_t2, ~ panel_name, ~ homogenized_name, ~ homogenized_coding,
    "id", NA_character_, "id", NA_character_, "test_panel", "id", NA_character_,
    "q1", single_deparse(coding_1), "question1", single_deparse(coding_2), "test_panel", "question_1", single_deparse(coding_h)
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

  coding_map <- coding_mapping(panel_map)

  expect_true(inherits(coding_map, "coding_mapping"))

  print(recoding_tree(coding_map)$homogenized_coding)
})
