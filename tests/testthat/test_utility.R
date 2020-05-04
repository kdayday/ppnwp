context("Test utility")
library(ppnwp)
library(lubridate)
library(abind)

test_that('get_maxar_data_by_horizon indexes along the diagonal', {
  # This tests using a matrix where the values indicate hour,lead time

  # [1 days x 4 hours x 5 lead time x 2 member]
  data_rectangle <- array(c(seq(11,41,by=10), seq(12,42,by=10),
                            seq(13,43,by=10), seq(14,44,by=10),
                            seq(15,45,by=10), rep(NaN, 20)), dim=c(1, 4, 5, 2))
  metadata <- list(ts_per_day=4, lead_time=1)
  # Hours are 0 indexed from lubridate
  with_mock(hour = function(...) {1}, get_ndays = function(...) {1},
            {expect_equal(get_maxar_data_by_horizon(h=1, issue=NaN, member=1, data_rectangle=data_rectangle,
                         date_start=NaN, metadata=metadata), 21)
              expect_equal(get_maxar_data_by_horizon(h=2, issue=NaN, member=1, data_rectangle=data_rectangle,
                                                     date_start=NaN, metadata=metadata), 32)
              expect_equal(get_maxar_data_by_horizon(h=3, issue=NaN, member=1, data_rectangle=data_rectangle,
                                                     date_start=NaN, metadata=metadata), 43)})
})

test_that('get_maxar_data_by_horizon wraps along days', {
  # This tests using a matrix where the values indicate day,hour,lead time

  col <- seq(111,131,by=10)
  day <- array(c(col, col+1, col+2, col+3,
                 rep(NaN, 12)), dim=c(1, 3, 4, 2))
  # [2 days x 3 hours x 4 lead time x 2 member]
  data_rectangle <- abind::abind(day, day+100, along=1)

  metadata <- list(ts_per_day=3, lead_time=1)
  # Hours are 0 indexed from lubridate
  with_mock(hour = function(...) {1}, get_ndays = function(...) {1},
            {expect_equal(get_maxar_data_by_horizon(h=1, issue=NaN, member=1, data_rectangle=data_rectangle,
                                                    date_start=NaN, metadata=metadata), 121)
              expect_equal(get_maxar_data_by_horizon(h=2, issue=NaN, member=1, data_rectangle=data_rectangle,
                                                     date_start=NaN, metadata=metadata), 132)
              expect_equal(get_maxar_data_by_horizon(h=3, issue=NaN, member=1, data_rectangle=data_rectangle,
                                                     date_start=NaN, metadata=metadata), 213)
              expect_equal(get_maxar_data_by_horizon(h=4, issue=NaN, member=1, data_rectangle=data_rectangle,
                                                     date_start=NaN, metadata=metadata), 224)})

})


test_that('check_maxar_parameters throws errors', {
  expect_error(check_maxar_parameters(nc=NaN, metadata=list(update_rate=0),
                                      site=NaN), "Update rate*")
  expect_error(check_maxar_parameters(nc=NaN, metadata=list(update_rate=1.5),
                                      site=NaN), "Update rate*")
  expect_error(check_maxar_parameters(nc=NaN, metadata=list(update_rate=1,
                                                            resolution=0.5),
                                      site=NaN), "Maxar lookup*")
  expect_error(check_maxar_parameters(nc=NaN, metadata=list(update_rate=10,
                                                            resolution=1,
                                                            lead_time=0,
                                                            horizon=5,
                                                            is_rolling=T),
                                      site=NaN), "Use equal*")
  expect_error(check_maxar_parameters(nc=NaN,
                                      metadata=list(update_rate=10, resolution=1,
                                                    lead_time=0, horizon=10,
                                                    is_rolling=T,
                                                    ts_per_day=24,
                                                    date_benchmark_start=as.POSIXlt(lubridate::ymd_h("20100101_00")),
                                                    date_benchmark_end=as.POSIXlt(lubridate::ymd(20100101))),
                                      site=NaN), "Horizon must be consistent*")
  expect_error(check_maxar_parameters(nc=NaN, metadata=list(update_rate=1,
                                                            resolution=1,
                                                            horizon=2.5,
                                                            is_rolling=F),
                                      site=NaN), "Horizon must be a multiple*")
  expect_error(check_maxar_parameters(nc=list(dim=list(NaN, NaN, list(vals=1:3), list(len=4))),
                                      metadata=list(update_rate=1,
                                                    resolution=1,
                                                    horizon=5,
                                                    is_rolling=F),
                                      site=NaN), "Horizon cannot*")
  expect_error(check_maxar_parameters(nc=list(dim=list(NaN, NaN, list(vals=c(1,2,4)), list(len=4))), metadata=list(update_rate=1,
                                                                                           resolution=1,
                                                                                           horizon=2,
                                                                                           is_rolling=F),
                                      site=3), "Site*")

})

test_that('issue_2_valid_index throws error', {
  issue <- lubridate::ymd("20010101")
  metadata <- list(resolution=1, lead_time=2, is_rolling=F)
  telemetry <- list(validtime=seq(from=as.POSIXlt(issue), by="hours", length.out=20))
  expect_error(issue_2_valid_index(issue=issue, step=0, metadata=metadata, telemetry=telemetry), "Step*")
})

test_that('issue_2_valid_index is correct for non-rolling forecast', {
  issue <- lubridate::ymd("20010101")
  metadata <- list(resolution=2, lead_time=2, is_rolling=F)
  telemetry <- list(validtime=seq(from=as.POSIXlt(issue), by="hours", length.out=20))
  expect_equal(issue_2_valid_index(issue=issue, step=1, metadata=metadata, telemetry=telemetry), 3)
  expect_equal(issue_2_valid_index(issue=issue, step=2, metadata=metadata, telemetry=telemetry), 5)
})

test_that('issue_2_valid_index is correct for rolling forecast', {
  issue <- lubridate::ymd("20010101")
  metadata <- list(resolution=2, lead_time=4, is_rolling=T)
  telemetry <- list(validtime=seq(from=as.POSIXlt(issue), by="hours", length.out=20))
  expect_equal(issue_2_valid_index(issue=issue, step=1, metadata=metadata, telemetry=telemetry), 1)
})

test_that('valid_2_issue_index with default issue time is correct', {
  valid <- lubridate::ymd_h("20010101_18")
  metadata <- list(resolution=1, lead_time=1, update_rate=6, horizon=18, is_rolling=F)
  ensemble <- list(issuetime=seq(from=as.POSIXlt(lubridate::ymd("20010101")), by="6 hours", length.out=4))
  expect_equal(valid_2_issue_index(valid, metadata, ensemble), c(3, 6))
})

test_that('valid_2_issue_index with given issue time is correct', {
  issue <- as.POSIXlt(lubridate::ymd("20010101"))
  valid <- lubridate::ymd_h("20010101_18")
  metadata <- list(resolution=1, lead_time=1, update_rate=6, horizon=18, is_rolling=F)
  ensemble <- list(issuetime=seq(from=as.POSIXlt(issue), by="6 hours", length.out=4))
  expect_equal(valid_2_issue_index(valid, metadata, ensemble, issue=issue), c(1, 18))
})
