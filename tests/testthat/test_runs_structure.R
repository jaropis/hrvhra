library(hrvhra)
context("Runs counting and interpretation")
test_that("Various runs are counted and interpreted correctly", {
  expect_equal(get_runs(c(1, 2, 2, 1)),
               list(
                 all_runs = list(c(2), c(2), c(1)),
                 directions = c("Up", "no_Change", "Down")
               ))
  expect_equal(get_runs(c(1, 2)), list(all_runs = list(c(2)), directions = c("Up")))
  expect_equal(get_runs(c(1, 1, 1, 1, 1, 1)), list(
    all_runs = list(c(1, 1, 1, 1, 1)),
    directions = c("no_Change")
  ))

  expect_equal(split_all_into_runs(c(1, 1, 1, 1, 1), c(0, 0, 1, 0, 0)),
               list(
                 all_runs = list(c(1), c(1)),
                 directions = c("no_Change", "no_Change")
               ))
  expect_equal(split_all_into_runs(c(1, 2, 3, 1, 2, 3), c(0, 0, 0, 0, 0, 0)),
               list(
                 all_runs = list(c(2, 3), c(1), c(2, 3)),
                 directions = c("Up", "Down", "Up")
               ))
  expect_equal(split_all_into_runs(c(1, 2, 3, 3, 2, 1), c(0, 0, 0, 0, 0, 0)),
               list(
                 all_runs = list(c(2, 3), c(3), c(2, 1)),
                 directions = c("Up", "no_Change", "Down")
               ))
  expect_equal(split_all_into_runs(c(1, 2, 3, 8, 3, 2, 1), c(0, 0, 0, 1, 0, 0, 0)),
               list(
                 all_runs = list(c(2, 3), c(2, 1)),
                 directions = c("Up", "Down")
               ))

  expect_equal(countruns(c(1, 2, 4, 3, 2, 1, 2, 3, 4, 4), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
               list(
                 direction_up = c(up1 = 0, up2 = 1, up3 = 1),
                 direction_down = c(
                   down1 = 0,
                   down2 = 0,
                   down3 = 1
                 ),
                 no_change = c(no_change1 = 1)
               ))
  expect_equal(countruns(c(1, 2, 4, 3, 2, 1, 2, 3, 4, 5), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
               list(
                 direction_up = c(
                   up1 = 0,
                   up2 = 1,
                   up3 = 0,
                   up4 = 1
                 ),
                 direction_down = c(
                   down1 = 0,
                   down2 = 0,
                   down3 = 1
                 ),
                 no_change = c(NULL)
               ))
  expect_equal(countruns(c(1, 2, 4, 3, 2, 1, 2, 3, 4, 5), c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)),
               list(
                 direction_up = c(
                   up1 = 0,
                   up2 = 1,
                   up3 = 0,
                   up4 = 1
                 ),
                 direction_down = c(down1 = 1),
                 no_change = c(NULL)
               ))
  expect_equal(countruns(c(4, 3, 2, 1, 2, 3, 4, 5), c(0, 0, 0, 0, 0, 0, 0, 0)),
               list(
                 direction_up = c(
                   up1 = 0,
                   up2 = 0,
                   up3 = 0,
                   up4 = 1
                 ),
                 direction_down = c(
                   down1 = 0,
                   down2 = 0,
                   down3 = 1
                 ),
                 no_change = c(NULL)
               ))
  # checking if flags may be left out
  expect_equal(countruns(RR$RR, RR$flags), countruns(RR$RR))
})
