
test_that(
  "Test that vlrr provides the same output as lm for simple cases.",
  {
    expect_equal(
      as.numeric(coef(lm(mpg ~ disp, data = mtcars))),
      coef(vlrr(mpg ~ disp, data = mtcars)),
      tolerance = 1.0e-06
    )
    expect_equal(
      as.numeric(coef(lm(mpg ~ disp * cyl,data = mtcars))),
      coef(vlrr(mpg ~ disp * cyl, data = mtcars)),
      tolerance = 1.0e-06
    )
  }
)

test_that(
  "Test that regularisation works",

  {

    model <- vlrr(mpg ~ disp,lambda = 0.1, data = mtcars)

    expect_is(model,"vlrr")
    expect_is(summary(model),"summary.vlrr")
    expect_is(coef(model),"numeric")
    expect_equal(length(coef(model)),2)
  }
)


test_that(
  "Test that regularisation works",

  {

    model <- vlrr(mpg ~ disp,lambda = 0.1, data = mtcars)

    expect_is(summary(model)$MSE,"numeric")
  }
)
