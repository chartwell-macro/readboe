mpr_archive_cache <- function() {
  cache<-Sys.getenv('READBOE_MPR_ARCHIVE_CACHE')
  skip_if(!nzchar(cache),'Set READBOE_MPR_ARCHIVE_CACHE for verified official-cache replay')
  cache
}
mpr_source <- function(key,cache,...) read_boe_mpr_forecasts(source='raw',key,deadline=180,cache_only=TRUE,cache_dir=cache,...)

test_that('official 2026 sources preserve all previously verified scalar observations', {
  cache<-mpr_archive_cache();outputs<-lapply(c('2026-02','2026-04','2026-07'),mpr_source,cache=cache)
  expect_equal(vapply(outputs,nrow,integer(1)),c(165L,455L,438L))
  expect_equal(vapply(outputs,function(x)sum(is.na(x$value)),integer(1)),c(0L,7L,0L))
  expect_equal(vapply(outputs,function(x)length(unique(x$source_sheet)),integer(1)),c(38L,14L,45L))
  for(x in outputs){
    expect_equal(anyDuplicated(x[c('series_id','target_period')]),0L);expect_true(boe_provenance(x)$from_cache)
    expect_match(boe_provenance(x)$raw_sha256,'^[0-9a-f]{64}$');expect_match(boe_provenance(x)$member_sha256,'^[0-9a-f]{64}$')
  }
  cell<-function(x,sheet,row,column)x$value[x$source_sheet==sheet&x$source_row==row&x$source_column%in%column]
  feb<-outputs[[1]];apr<-outputs[[2]];jul<-outputs[[3]]
  expect_equal(cell(feb,'1. CPI inflation',94,91),2.1);expect_equal(cell(feb,'1. CPI inflation',94,94),1.7)
  expect_equal(cell(feb,'11. UK GDP',46,31),1.5)
  expect_equal(cell(apr,'2. CPI inflation',122,2:5),c(1.7,3.4,3.6,6.2))
  expect_equal(cell(apr,'10. GDP growth',35,2:5),c(1.5,1,1,.8))
  expect_true(all(apr$scenario[is.na(apr$value)]=='february_comparator'));expect_false(any(apr$scenario=='central'))
  expect_equal(cell(jul,'44. CPI inflation',122,2),2.9);expect_equal(cell(jul,'44. CPI inflation',122,3),4.3)
  expect_equal(cell(jul,'44. CPI inflation',122,4),3.2);expect_equal(cell(jul,'1. CPI inflation',96,94),3.2)
  expect_equal(unique(jul$source_vintage[jul$source_sheet=='1. CPI inflation']),'2026-08')
  expect_equal(unique(jul$source_vintage_label[jul$source_sheet=='1. CPI inflation']),'46235')
  replay<-mpr_source('2026-07',cache,expected_sha256=boe_provenance(jul)$raw_sha256)
  expect_equal(as.data.frame(jul),as.data.frame(replay),ignore_attr=TRUE)
  expect_error(mpr_source('2026-07',cache,expected_sha256=strrep('0',64)),class='readboe_identity_mismatch')
})

test_that('held-out historical probability, conditioning, binary XLS and multipage formats replay', {
  cache<-mpr_archive_cache()
  p<-mpr_source('1999-08',cache,include_distributions=TRUE,historical_source=FALSE)
  expect_equal(nrow(p),36L);expect_equal(sum(p$value_status=='censored'),5L)
  expect_setequal(p$measure,c('rpix_inflation','gdp_growth'));expect_true(all(p$scenario=='constant_rate'))
  r<-mpr_source('2002-08',cache,historical_source=FALSE)
  expect_equal(r$target_period,c('2002-Q3','2002-Q4','2003-Q1','2003-Q2','2003-Q3','2003-Q4','2004-Q1','2004-Q2','2004-Q3'))
  expect_equal(r$value,c(4,4,4.1,4.3,4.6,4.8,4.9,5,5.1));expect_true(all(r$kind=='conditioning_assumption'&r$scenario=='market'))
  xls<-mpr_source('2015-02',cache,include_distributions=TRUE,historical_source=FALSE)
  expect_true(any(grepl('[.]xls$',xls$source_url)));expect_true(any(xls$value_status=='censored'))
  expect_true(all(xls$value_upper_bound[xls$raw_value=='<5%']==.05,na.rm=TRUE))
  modern<-mpr_source('2024-05',cache,historical_source=FALSE)
  annual<-modern[modern$layout=='pdf_annual_profiles',]
  expect_equal(length(unique(annual$measure)),29L)
  world<-annual[annual$measure=='world_gdp_ppp',];expect_equal(world$value,c(3.25,3,3))
  expect_true(all(grepl('189 countries',world$definition,fixed=TRUE)))
  expect_true(all(annual$statistic[annual$measure=='participation_rate']=='q4_level'))
  expect_true(all(annual$units[annual$measure=='average_weekly_hours_worked']=='Hours'))
})

test_that('illustrative semantics and optional distribution selection are stable under historical fallback', {
  cache<-mpr_archive_cache();x<-mpr_source('2020-05',cache)
  annual<-x[x$layout=='pdf_annual_profiles',]
  expect_equal(nrow(annual),42L);expect_false(any(x$kind=='central_forecast'))
  expect_equal(annual$value[annual$measure=='gdp_growth'],c(-14,15,3))
  expect_equal(annual$value[annual$measure=='cpi_inflation'],c(.6,.5,2));expect_true(all(annual$statistic[annual$measure=='cpi_inflation']=='annual_average'))
  for(key in c('2004-02','2014-02','2020-05')){
    a<-mpr_source(key,cache);b<-mpr_source(key,cache,include_distributions=TRUE)
    ak<-paste(a$series_id,a$target_period);bk<-paste(b$series_id,b$target_period)
    expect_true(all(ak%in%bk));expect_equal(a$value,b$value[match(ak,bk)])
  }
  e<-tryCatch(mpr_source('1993-02',cache),error=identity)
  expect_s3_class(e,'readboe_no_numeric_forecast');expect_gt(e$source_evidence$pages_inspected,0)
  expect_equal(e$source_evidence$historical_source_search$matched_observations,0)
  expect_error(mpr_source('1998-02',cache),class='readboe_no_scalar_forecast')
})

test_that('live discovery and a complete original-source download replay use the public API', {
  skip_if_not(identical(tolower(Sys.getenv('READBOE_RUN_MPR_LIVE_TESTS')),'true'))
  cache<-tempfile('readboe-mpr-live-');on.exit(unlink(cache,recursive=TRUE),add=TRUE)
  rel<-boe_mpr_releases(source='raw',include_upcoming=TRUE,deadline=180,cache_dir=cache)
  expect_true(any(rel$family=='inflation_report'));expect_true(any(rel$family=='monetary_policy_report'))
  x<-read_boe_mpr_forecasts(source='raw','latest',measure='cpi_inflation',frequency='Q',deadline=180,cache_dir=cache)
  p<-boe_provenance(x);expect_false(p$from_cache)
  y<-read_boe_mpr_forecasts(source='raw',unique(x$release),measure='cpi_inflation',frequency='Q',expected_sha256=p$raw_sha256,deadline=60,cache_only=TRUE,cache_dir=cache)
  expect_equal(as.data.frame(x),as.data.frame(y),ignore_attr=TRUE);expect_true(boe_provenance(y)$from_cache)
  cat('\nMPR LIVE',unique(x$release),p$raw_sha256,'rows',nrow(x),'\n')
})

test_that('held-out judgement tables retain every future column, row units and scoped definitions', {
  cache<-mpr_archive_cache();x<-mpr_source('2014-05',cache,historical_source=FALSE)
  expected<-list(world_gdp=c(2.5,2.75,2.75),euro_area_gdp=c(1.25,1.5,1.5),us_gdp=c(2.5,3,3),
    credit_spreads=c(2.25,2,2),household_saving_ratio=c(5,3.75,3.5),business_investment_to_gdp_ratio=c(8.75,9.5,10.5),
    productivity=c(1,1.25,2),participation_rate=rep(63.75,3),average_weekly_hours_worked=rep(32.25,3),
    uk_import_prices=c(-2.25,0,.25),unit_wage_costs=c(1,1.75,1.75))
  stats<-c('annual_change','annual_change','annual_change','q4_level','annual_average','annual_average',
    'annual_change','q4_level','q4_level','q4_four_quarter_change','q4_four_quarter_change')
  for(i in seq_along(expected)){
    rows<-x[x$source_sheet=='Table 5.A'&x$source_page==41L&x$measure==names(expected)[[i]],]
    expect_equal(rows$value,expected[[i]]);expect_equal(nrow(rows),3L)
    expect_true(all(rows$statistic==stats[[i]]));expect_false(any(grepl('Ukraine',rows$definition,fixed=TRUE)))
  }
  expect_true(all(x$units[x$measure=='average_weekly_hours_worked']=='Hours'))
  ratio<-x[x$measure=='business_investment_to_gdp_ratio',];expect_true(all(grepl('percentage of GDP',ratio$definition,fixed=TRUE)))
})

test_that('printed percentile cells and prior conditioning remain distinct without reconstructed quantiles', {
  cache<-mpr_archive_cache();x<-mpr_source('2013-08',cache,include_distributions=TRUE,historical_source=FALSE)
  y<-x[x$kind=='distribution_quantile',];expect_equal(nrow(y),30L)
  expect_true(all(y$scenario=='constant_rate'&y$units=='Per cent'))
  expect_true(all(y$comparison_forecast_vintage=='2013-05'&y$comparison_scenario=='market_conditioned'))
  expected<-list(gdp_growth=c(.7,1.6,2.5,3.5,4.3,.3,1.3,2.4,3.3,4.2,.4,1.5,2.6,3.7,4.7),
    cpi_inflation=c(.9,1.7,2.6,3.5,4.4,.2,1.1,2.1,3.1,4,.1,1,2,3.1,4))
  previous<-list(gdp_growth=c(0,.9,1.8,2.8,3.6,-.1,.9,2,3,3.9),
    cpi_inflation=c(.6,1.4,2.4,3.3,4.1,.1,1,2,3,3.9))
  for(measure in names(expected)){
    rows<-y[y$measure==measure,];rows<-rows[order(rows$target_date,rows$quantile_probability),]
    expect_equal(rows$value,expected[[measure]])
    expect_equal(rows$quantile_probability,rep(c(.1,.25,.5,.75,.9),3))
    expect_equal(as.numeric(rows$comparison_raw_value[1:10]),previous[[measure]])
    expect_true(all(is.na(rows$comparison_raw_value[11:15])))
  }
  may<-mpr_source('2013-05',cache,include_distributions=TRUE,historical_source=FALSE)
  rows<-may[may$kind=='distribution_quantile'&may$measure=='cpi_inflation',]
  rows<-rows[order(rows$target_date,rows$quantile_probability),]
  expect_equal(rows$value,c(.9,1.7,2.6,3.5,4.3,.1,1,2,3,3.9,0,.9,1.9,2.9,3.9))
  expect_true(all(rows$scenario=='market_conditioned'))
})
