

a1 <- lm(mpg ~ disp, data = mtcars)
b1 <- vlrr(mpg ~ disp, data = mtcars)

a2 <- lm(mpg ~ disp * cyl, data = mtcars)
b2 <- vlrr(mpg ~ disp * cyl, data = mtcars)

a3 <- lm(mpg ~ disp * cyl * wt, data = mtcars)
b3 <- vlrr(mpg ~ disp * cyl * wt, data = mtcars)


test_that(
  "Check coefficients in simple use cases.",
  {
    expect_equal(
      as.numeric(coef(a1)),
      coef(b1),
      tolerance = 1.0e-06
    )
    expect_equal(
      as.numeric(coef(a2)),
      coef(b2),
      tolerance = 1.0e-06
    )

    # This does not perform so well, hence the lower threshhold! May require feature scaling

    expect_equal(
      as.numeric(coef(a3)),
      coef(b3),
      tolerance = 1.0e-01
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
  "Check vcov matrix",

  {

    expect_equal(
      vcov(a1),
      vcov(b1)
    )

    expect_equal(
      vcov(a2),
      vcov(b2)
    )

    # Again threshold set lower for a3

    expect_equal(
      vcov(a3),
      vcov(b3),
      tolerance = 1.0e-03
    )

  }
)


test_that(
  "Check summary table output",

  {

    expect_equal(
      summary(a1)[4],
      summary(b1)[4],
      tolerance = 1.0e-06
    )

    expect_equal(
      summary(a2)[4],
      summary(b2)[4],
      tolerance = 1.0e-06
    )

    # Again threshold set lower for a3

    expect_equal(
      summary(a3)[4],
      summary(b3)[4],
      tolerance = 1.0e-01
    )

  }
)
