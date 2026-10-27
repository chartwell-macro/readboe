# All default tests are synthetic and offline. Dates deliberately include new,
# irregular edition months so source layout, rather than a release list, admits data.
mpr_rel <- function(key = '2099-03', date = '2099-03-12') data.frame(release = key,
  release_date = as.Date(date), release_date_precision = 'day', source_published_date = as.Date(date),
  landing_url = paste0('https://www.bankofengland.co.uk/monetary-policy-report/', substr(key, 1, 4), '/march-', substr(key, 1, 4)),
  publication_status = 'published')
mpr_meta <- function(title = 'CPI inflation', rel = mpr_rel(), layout = 'vintage_matrix') {
  readboe:::boe_mpr_source_meta(rel, rel, 'https://www.bankofengland.co.uk/source.xlsx', '', title,
    layout, title, 'Per cent', 'Four-quarter percentage change')
}
mpr_cells <- function() {
  x <- matrix(NA_character_, 8, 5)
  x[1:3,1] <- c('CPI inflation', 'Per cent', 'Four-quarter percentage change')
  x[5,] <- c('Date of publication','2098 Q4','2099 Q1','2099 Q2','2100 Q1')
  x[6,] <- c('January 2099','2.0','2.1','2.2',NA)
  x[7,] <- c('March 2099','2.0','2.3','2.4','n.a.')
  x
}
mpr_options <- function(...) readboe:::boe_mpr_options(10, 1e6, 20L, 1e7, 20L, 100L, 30L, 1e4, 1000L, FALSE, FALSE, FALSE, tempdir())

test_that('future and irregular vintage matrices preserve cells, dates and explicit missingness', {
  x <- readboe:::boe_mpr_matrix(mpr_cells(), mpr_meta())
  expect_equal(x$value, c(2,2.3,2.4,NA))
  expect_equal(x$raw_value, c('2.0','2.3','2.4','n.a.'))
  expect_equal(x$source_row, rep(7L,4)); expect_equal(x$source_column, 2:5)
  expect_equal(x$source_vintage, rep('2099-03',4)); expect_true(all(x$kind=='central_forecast'))
  y <- readboe:::boe_mpr_finalize(x,mpr_rel(),mpr_options(),FALSE)
  expect_equal(y$target_period,c('2099-Q1','2099-Q2','2100-Q1'))
  opt<-mpr_options();opt$max_output_rows<-2L
  expect_error(readboe:::boe_mpr_finalize(x,mpr_rel(),opt,TRUE), class='readboe_bounds')
  expect_equal(readboe:::boe_mpr_vintages(c(as.character(as.integer(as.Date('2099-03-01')-as.Date('1899-12-30'))),'2099-03-12','March 2099 (e)')),rep('2099-03',3))
})

test_that('scenario identities derive from labels and retain comparators without invented central paths', {
  x<-matrix(NA_character_,8,5);x[1:3,1]<-mpr_cells()[1:3,1]
  x[5,]<-c('Date','March 2098 central projection','Scenario X','Milder scenario','Memo: central projection')
  x[6:8,1]<-c('2099 Q1','2099 Q2','2100 Q1');x[6:8,-1]<-as.character(1:12)
  y<-readboe:::boe_mpr_scenario_columns(x,mpr_meta(layout='scenario_columns'))
  expect_setequal(y$scenario,c('march_2098_comparator','scenario_x','milder','central_memo'))
  expect_true(all(y$forecast_vintage[y$kind=='comparator']=='2098-03'))
  expect_false(any(y$scenario=='central'))
  expect_equal(anyDuplicated(y[c('series_id','target_period')]),0L)
  expect_equal(readboe:::boe_mpr_select(y,list(scenario='milder'))$value,7:9)
  expect_error(readboe:::boe_mpr_select(y,list(scenario='central')),class='readboe_no_data')
  expect_error(readboe:::boe_mpr_select(y,list(scenario=c('milder','milder'))),class='readboe_config')
})

test_that('annual averages and Q4 endpoints retain separate period identities', {
  a<-readboe:::boe_mpr_periods(c('2099','2100'),'A','annual_average')
  q<-readboe:::boe_mpr_periods(c('2099','2100 Q4'),'A','mode_q4_four_quarter_change')
  expect_equal(a$target_period,c('2099','2100'));expect_equal(q$target_period,c('2099-Q4','2100-Q4'))
  expect_equal(q$target_date,as.Date(c('2099-10-01','2100-10-01')))
  expect_error(readboe:::boe_mpr_periods(c('2099 Q2','2099 Q1'),'Q','level'),class='readboe_schema_drift')
})

test_that('censor bounds, probability units and comparison directions survive parsing', {
  x<-readboe:::boe_mpr_values(c('<5%','>95%','<=1%','8%','0.25','n.a.'),TRUE,'fraction')
  expect_equal(x$value,c(NA,NA,NA,.08,.25,NA));expect_equal(x$value_upper_bound[1:3],c(.05,NA,.01))
  expect_equal(x$value_lower_bound[2],.95);expect_equal(x$value_bound_inclusive[1:3],c(FALSE,FALSE,TRUE))
  expect_equal(x$value_status[1:3],rep('censored',3))
  expect_false(identical(readboe:::boe_mpr_probability_slug('Pr. {>Mode}'),readboe:::boe_mpr_probability_slug('Pr. {<Mode}')))
  expect_error(readboe:::boe_mpr_values('<5%'),class='readboe_schema_drift')
  expect_error(readboe:::boe_mpr_values('unknown'),class='readboe_schema_drift')
  expect_equal(readboe:::boe_mpr_pdf_number(c('¾','1/2','21/2','−¼')),c(.75,.5,2.5,-.25))
})

test_that('source headers fail closed on duplicate periods, vintages and invalid scalar tokens', {
  bad<-mpr_cells();bad[5,4]<-bad[5,3]
  expect_error(readboe:::boe_mpr_matrix(bad,mpr_meta()),class='readboe_schema_drift')
  bad<-mpr_cells();bad[8,]<-bad[7,]
  expect_error(readboe:::boe_mpr_matrix(bad,mpr_meta()),class='readboe_schema_drift')
  bad<-mpr_cells();bad[7,4]<-'revised?'
  expect_error(readboe:::boe_mpr_matrix(bad,mpr_meta()),class='readboe_schema_drift')
})

test_that('fan blocks retain conditioning and backcast variants and censored probabilities', {
  x<-matrix(NA_character_,13,3)
  x[1,1]<-'CPI inflation projections based on market interest rate expectations'
  x[2,2:3]<-c('2099Q1','2099Q2');x[3,]<-c('Mode','2','3')
  x[4,]<-c('Pr. {<Mode}','<5%','8%');x[5,]<-c('Pr. {>Mode}','>95%','92%')
  x[8,1]<-'CPI inflation projections based on interest rates constant at 3%'
  x[9,2:3]<-c('2099Q1','2099Q2');x[10,]<-c('Mode','4','5')
  y<-readboe:::boe_mpr_fan_rows(x,mpr_meta(),TRUE)
  expect_equal(y$value[y$scenario=='constant_rate'],c(4,5));expect_true(all(y$kind[y$scenario=='constant_rate']=='conditional_forecast'))
  expect_equal(sum(y$value_status=='censored'),2L);expect_equal(anyDuplicated(y[c('series_id','target_period')]),0L)
  expect_equal(y$value[y$raw_value=='8%'],.08)
})

test_that('historical paths inherit an unambiguous original illustrative classification', {
  own<-do.call(rbind,lapply(c('CPI inflation','GDP growth','Unemployment'),function(title){m<-mpr_meta(title);readboe:::boe_mpr_path(m,'2099','1',1,1,readboe:::boe_mpr_measure(title),'illustrative','annual_change','A','scenario_projection',scenario_label='Illustrative scenario')}))
  history<-readboe:::boe_mpr_matrix(mpr_cells(),mpr_meta())
  x<-readboe:::boe_mpr_contextualize_historical(history,own)
  expect_true(all(x$scenario=='illustrative'&x$kind=='scenario_projection'))
  expect_true(all(grepl('Original report scenario:',x$classification_basis,fixed=TRUE)))
  own$kind[1]<-'central_forecast'
  expect_identical(readboe:::boe_mpr_contextualize_historical(history,own),history)
})

test_that('multipage annual profiles retain wrapped labels, printed fractions and footnote statistics', {
  pages<-c(paste(c('Table 1.D: Indicative projections consistent with the MPC projections',
    ' Average    2098    2099    2100', '1998–    2098    2099    2100', '2007(c)    18(c)',
    'World GDP       3    2    1¾ (2)    2¼ (3)',
    '(PPP-', 'weighted) (c)'),collapse='\n'),paste(c('Bank of England      Page 2',
    'Contribution of -¼    0    ½ (1)    -½ (0)', 'net trade to GDP (d)',
    'Participation    63    64    65 (66)    66 (67)', 'rate (e)',
    'Sources: Synthetic data', '(c) Annual average growth.', '(d) Contribution to annual growth.',
    '(e) Participation rate in Q4 as a percentage of the population.'),collapse='\n'))
  y<-readboe:::boe_mpr_pdf_annual_text(pages,mpr_rel(),'https://www.bankofengland.co.uk/report.pdf')$data
  expect_equal(nrow(y),6L);expect_setequal(y$measure,c('world_gdp_ppp','net_trade_contribution','participation_rate'))
  expect_equal(y$value[y$measure=='world_gdp_ppp'],c(1.75,2.25))
  expect_true(all(y$statistic[y$measure=='participation_rate']=='q4_level'))
  expect_true(all(y$source_page[y$measure=='net_trade_contribution']==2))
})

test_that('expired deadlines reject cached entry and workbook extraction', {
  expect_error(read_boe_mpr_forecasts(source='raw',deadline=boe_deadline(at=Sys.time()-1)),class='readboe_timeout')
  expect_error(readboe:::boe_read_workbook_raw(as.raw(1),'.xlsx',deadline=boe_deadline(at=Sys.time()-1)),class='readboe_timeout')
  expect_error(read_boe_mpr_forecasts(source='raw',expected_sha256='bad'),class='readboe_config')
  expect_error(read_boe_mpr_forecasts(source='raw',max_cells=0),class='readboe_config')
  expect_error(read_boe_mpr_forecasts(source='raw',measure='cpi_inflation',series_id='x'),class='readboe_config')
})
test_that("selective workbook helpers retain coordinates and respect sheet bounds", {
  path <- test_path("fixtures", "workbook.xlsx")
  raw <- readBin(path, "raw", n = file.info(path)$size)
  sheets <- readxl::excel_sheets(path)
  expect_length(readboe:::boe_read_workbook_raw(raw, ".xlsx", sheets = sheets[1]), 1L)
  expect_error(readboe:::boe_read_workbook_raw(raw, ".xlsx", sheets = "absent"), class = "readboe_identity_mismatch")
  expect_error(readboe:::boe_read_workbook_raw(raw, ".xlsx", sheets = rep(sheets[1], 2)), class = "readboe_identity_mismatch")
})

# Structural XLSX preflight tests use small generated files without source data.


test_that("XLSX preflight checks dimensions, schema and absolute deadlines before readxl", {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx"); on.exit(unlink(path), add = TRUE)
  openxlsx::write.xlsx(list(Scalar = data.frame(a = 1, b = 2)), path)
  inventory <- data.frame(source_sheet = "Scalar")
  preflight <- function(...) readboe:::boe_mpr_preflight(path, inventory, "Scalar", boe_deadline(10),
    max_uncompressed_bytes = 1e6, ...)
  dims <- preflight(max_sheets = 1L, max_rows = 2L, max_columns = 2L, max_cells = 4L)
  expect_equal(dims$rows, 2)
  expect_equal(dims$columns, 2)
  expect_error(preflight(max_sheets = 1L, max_rows = 1L, max_columns = 2L, max_cells = 4L), class = "readboe_bounds")
  expect_error(preflight(max_sheets = 1L, max_rows = 2L, max_columns = 1L, max_cells = 4L), class = "readboe_bounds")
  expect_error(preflight(max_sheets = 1L, max_rows = 2L, max_columns = 2L, max_cells = 3L), class = "readboe_bounds")
  expect_error(readboe:::boe_mpr_preflight(path, data.frame(source_sheet = "Other"), "Other", boe_deadline(10),
    1e6, 1L, 2L, 2L, 4L), class = "readboe_schema_drift")
})



test_that("ZIP safety rejects aliases, traversal, duplicates and budget overruns", {
  for (name in c("../escape.xlsx", "a/../escape.xlsx", "a\\..\\escape.xlsx", "/escape.xlsx",
                 "C:/escape.xlsx", "a/./escape.xlsx", "a//escape.xlsx")) {
    path <- mpr_test_zip(name)
    expect_error(readboe:::boe_safe_zip_members(path), class = "readboe_bounds")
    unlink(path)
  }
  path <- mpr_test_zip(c("same.xlsx", "same.xlsx")); on.exit(unlink(path), add = TRUE)
  expect_error(readboe:::boe_safe_zip_members(path), class = "readboe_bounds")
  case_alias <- mpr_test_zip(c("Same.xlsx", "same.xlsx")); on.exit(unlink(case_alias), add = TRUE)
  expect_error(readboe:::boe_safe_zip_members(case_alias), class = "readboe_bounds")
  path2 <- mpr_test_zip(c("a.xlsx", "b.xlsx")); on.exit(unlink(path2), add = TRUE)
  expect_error(readboe:::boe_safe_zip_members(path2, max_members = 1L), class = "readboe_bounds")
  expect_error(readboe:::boe_safe_zip_members(path2, max_uncompressed_bytes = 10L), class = "readboe_bounds")
  expect_equal(nrow(readboe:::boe_safe_zip_members(path2)), 2L)
  expect_error(readboe:::boe_extract_members(path2, c("a.xlsx", "a.xlsx")), class = "readboe_identity_mismatch")
})



test_that("ZIP extraction never applies symlink attributes and checks directory sizes", {
  path <- mpr_test_zip("entry.xlsx", bodies = list(charToRaw("/outside/target")), attributes = 0xa1ff0000)
  on.exit(unlink(path), add = TRUE)
  extracted <- readboe:::boe_extract_members(path, "entry.xlsx")
  on.exit(unlink(attr(extracted, "extraction_dir"), recursive = TRUE), add = TRUE)
  expect_equal(Sys.readlink(extracted[[1]]), "")
  expect_equal(readChar(extracted[[1]], file.info(extracted[[1]])$size), "/outside/target")
  bad <- mpr_test_zip("entry.xlsx", declared_sizes = 10L); on.exit(unlink(bad), add = TRUE)
  expect_error(readboe:::boe_extract_members(bad, "entry.xlsx"), class = "readboe_bounds")
})



test_that("XLSX date-system and duplicate-cell drift is rejected before scalar parsing", {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx"); on.exit(unlink(path), add = TRUE)
  openxlsx::write.xlsx(list(Scalar = data.frame(a = 1)), path)
  preflight <- function(p) readboe:::boe_mpr_preflight(p, data.frame(source_sheet = "Scalar"),
    "Scalar", boe_deadline(10), 1e6, 1L, 10L, 10L, 100L)
  date <- mpr_mutate_xlsx(path, "xl/workbook.xml", function(x) {
    doc <- xml2::read_xml(x)
    node <- xml2::xml_find_first(doc, ".//*[local-name()='workbookPr']")
    xml2::xml_set_attr(node, "date1904", "1")
    as.character(doc)
  })
  on.exit(unlink(date), add = TRUE)
  expect_error(preflight(date), class = "readboe_schema_drift")
  dup <- mpr_mutate_xlsx(path, "xl/worksheets/sheet1.xml", function(x)
    sub("</row>", '<c r="A1"><v>99</v></c></row>', x, fixed = TRUE))
  on.exit(unlink(dup), add = TRUE)
  expect_error(preflight(dup), class = "readboe_schema_drift")
})

test_that('conditioning table years come from every printed header including year gaps', {
  words<-data.frame(page=1L,text=c('2099','2101','Q3','Q4','Q1','Q2','4.0','4.1','4.2','4.3'),
    x=c(90,190,80,120,180,220,80,120,180,220),y=c(80,80,100,100,100,100,120,120,120,120),
    xmax=c(110,210,90,130,190,230,90,130,190,230),ymax=c(90,90,110,110,110,110,130,130,130,130),width=700)
  lines<-data.frame(page=1L,text=c('Table 5.A','Market expectations of the Bank official interest rate'),x=40,y=c(20,32),xmax=350,ymax=c(30,42))
  x<-readboe:::boe_mpr_pdf_conditioning(words,lines,1L,mpr_rel(),'https://www.bankofengland.co.uk/report.pdf')
  expect_equal(x$target_period,c('2099-Q3','2099-Q4','2101-Q1','2101-Q2'))
  expect_equal(x$value,c(4,4.1,4.2,4.3));expect_true(all(x$kind=='conditioning_assumption'))
  expect_error(readboe:::boe_mpr_pdf_conditioning(words[-2,],lines,1L,mpr_rel(),'https://www.bankofengland.co.uk/report.pdf'),class='readboe_schema_drift')
})

test_that('future editions use structural memo reconciliation without a dated manifest', {
  quarters<-paste(rep(2099:2101,each=4),paste0('Q',1:4));workbook<-list()
  for(title in c('CPI inflation','GDP growth')){
    matrix<-matrix(NA_character_,6,13);matrix[1,1]<-title;matrix[5,]<-c('Date of publication',quarters)
    matrix[6,]<-c('April 2099',as.character(seq_len(12)/10))
    memo<-matrix(NA_character_,17,3);memo[1,1]<-title;memo[5,]<-c('Date','Milder scenario','Memo: central projection')
    memo[6:17,1]<-quarters;memo[6:17,2]<-as.character(seq_len(12));memo[6:17,3]<-as.character(seq_len(12)/10)
    workbook[[paste(title,'matrix')]]<-as.data.frame(matrix);workbook[[paste(title,'memo')]]<-as.data.frame(memo)
  }
  alias<-readboe:::boe_mpr_reconcile_vintage(workbook,mpr_rel());expect_equal(alias,'2099-04')
  meta<-mpr_meta();x<-readboe:::boe_mpr_matrix(as.matrix(workbook[[1]]),meta,alias)
  expect_equal(unique(x$forecast_vintage),'2099-03');expect_equal(unique(x$source_vintage),'2099-04')
  meta$source_edition<-'2100-01';expect_equal(nrow(readboe:::boe_mpr_matrix(as.matrix(workbook[[1]]),meta,alias)),0L)
})

test_that('public forecast acquisition replays an unlisted synthetic edition with revision and byte checks', {
  skip_if_not_installed('openxlsx')
  file<-tempfile(fileext='.xlsx');on.exit(unlink(file),add=TRUE)
  openxlsx::write.xlsx(list(Forecast=as.data.frame(mpr_cells())),file,colNames=FALSE)
  member<-readBin(file,'raw',n=file.info(file)$size)
  zip<-mpr_test_zip('Future Projections Databank.xlsx',list(member));on.exit(unlink(zip),add=TRUE)
  bytes<-readBin(zip,'raw',n=file.info(zip)$size);hash<-digest::digest(bytes,algo='sha256',serialize=FALSE)
  asset<-data.frame(url='https://www.bankofengland.co.uk/future.zip',file_type='zip',asset_role='data_bundle')
  attr(asset,'boe_provenance')<-list(receipt=list(url='https://www.bankofengland.co.uk/future-page',raw_sha256=strrep('1',64)))
  calls<-0L
  local_mocked_bindings(boe_mpr_discover=function(...)mpr_rel(),boe_mpr_assets_for_release=function(...)asset,
    boe_perform=function(req){calls<<-calls+1L;httr2::response(200,url=req$url,body=bytes)},.package='readboe')
  cache<-tempfile();on.exit(unlink(cache,recursive=TRUE),add=TRUE)
  x<-read_boe_mpr_forecasts(source='raw','latest',deadline=10,cache_dir=cache,expected_sha256=hash)
  expect_equal(x$value,c(2.3,2.4,NA));expect_equal(calls,1L)
  expect_equal(boe_provenance(x)$member_sha256,digest::digest(member,algo='sha256',serialize=FALSE))
  y<-read_boe_mpr_forecasts(source='raw','2099-03',deadline=10,cache_dir=cache,cache_only=TRUE)
  expect_equal(x,y,ignore_attr=TRUE);expect_true(boe_provenance(y)$from_cache);expect_equal(calls,1L)
  expect_error(read_boe_mpr_forecasts(source='raw','2099-03',deadline=10,cache_dir=cache,cache_only=TRUE,expected_sha256=strrep('0',64)),class='readboe_identity_mismatch')
  expect_error(read_boe_mpr_forecasts(source='raw','2099-03',deadline=10,cache_dir=cache,cache_only=TRUE,max_bytes=length(bytes)-1L),class='readboe_bounds')
  local_mocked_bindings(boe_mpr_load_workbook=function(...)list(Changed=as.data.frame(matrix('Unknown layout',2,2))),.package='readboe')
  expect_error(read_boe_mpr_forecasts(source='raw','2099-03',deadline=10,cache_dir=cache,cache_only=TRUE,historical_source=FALSE),class='readboe_unsupported_format')
})

test_that('percentile conditioning belongs to the named vintage rather than a comparator phrase', {
  text<-'Conditioned on constant interest rates (June) and market interest rates (March).'
  expect_equal(readboe:::boe_mpr_quantile_condition(text,'2077-06'),'constant_rate')
  expect_equal(readboe:::boe_mpr_quantile_condition(text,'2077-03'),'market_conditioned')
  expect_equal(readboe:::boe_mpr_quantile_condition(text,'2077-09'),'published_conditioning')
  expect_equal(readboe:::boe_mpr_quantile_condition('Conditioned on market interest rates','2077-06'),'market_conditioned')
})
