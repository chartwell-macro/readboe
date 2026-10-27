mpr_discovery_html <- function(records) paste(vapply(seq_len(nrow(records)), function(i)
  paste0('<a class="release" href="',records$url[[i]],'"><time datetime="',records$date[[i]],'">date</time></a>'),character(1)),collapse='')
mpr_discovery_records <- function() list(
  ir=data.frame(url=paste0('https://www.bankofengland.co.uk/inflation-report/2031/',c('march','july','october'),'-2031'),date=c('2031-03-01','2031-07-01','2031-10-01')),
  mpr=data.frame(url=c('https://www.bankofengland.co.uk/report/2034/monetary-policy-report-financial-stability-report-may-2034',
    'https://www.bankofengland.co.uk/monetary-policy-report/2034/september-2034',
    'https://www.bankofengland.co.uk/monetary-policy-report/2035/january-2035'),date=c('2034-05-07','2034-09-14','2035-01-10')))

test_that('official discovery unions search/sitemaps, joint reports, irregular months and scheduled dates', {
  records<-mpr_discovery_records();calls<-0L
  local_mocked_bindings(boe_perform=function(req){
    calls<<-calls+1L;url<-req$url;news<-grepl('monetary-policy|News/',url);x<-records[[if(news)'mpr' else 'ir']]
    body<-if(grepl('/sitemap/',url))paste0(mpr_discovery_html(x),'<a href="/inflation-report/2031/october-2031/visual-summary">summary</a>') else if(grepl('/_api/',url))paste0('{"Results":',encodeString(mpr_discovery_html(x),quote='"'),',"NumResults":3}') else if(news)
      'CP.BOE.NewsPageDataSourceID="news";CP.BOE.NewsTypes=["report"];CP.BOE.NewsTaxonomies=["mpr"];' else
      'CP.BOE.ContentPageSearchDataSourceID="search";CP.BOE.ContentPageTypes=["report"];CP.BOE.ContentPageTaxonomies=["ir"];'
    httr2::response(200,url=url,body=charToRaw(body))
  },.package='readboe')
  cache<-tempfile();on.exit(unlink(cache,recursive=TRUE),add=TRUE)
  x<-boe_mpr_releases(source='raw',TRUE,as_of=as.Date('2034-12-31'),deadline=10,cache_dir=cache)
  expect_equal(nrow(x),6L);expect_equal(sum(x$publication_status=='published'),5L)
  expect_true(all(is.na(x$release_date[x$release_date_precision=='month'])))
  expect_equal(sum(x$release_date_precision=='month'),3L)
  expect_true('2034-05'%in%x$release);expect_false(any(grepl('visual-summary',x$landing_url)))
  before<-calls;y<-boe_mpr_releases(source='raw',TRUE,as_of=as.Date('2034-12-31'),deadline=10,cache_only=TRUE,cache_dir=cache)
  expect_equal(x,y,ignore_attr=TRUE);expect_equal(calls,before)
  expect_equal(nrow(boe_mpr_releases(source='raw',as_of=as.Date('2034-12-31'),deadline=10,cache_only=TRUE,cache_dir=cache)),5L)
  expect_error(boe_mpr_releases(source='raw',max_releases=1L,cache_only=TRUE,cache_dir=cache,deadline=10),class='readboe_bounds')
})

test_that('archive pagination and changed schema are bounded failures', {
  expect_error(readboe:::boe_mpr_json(charToRaw('{"Unexpected":1}')),class='readboe_schema_drift')
  expect_false(readboe:::boe_mpr_is_release_url('https://www.bankofengland.co.uk/inflation-report/2031/october-2031/visual-summary'))
  expect_true(readboe:::boe_mpr_is_release_url('https://www.bankofengland.co.uk/report/2034/monetary-policy-report-financial-stability-report-may-2034'))
  expect_error(boe_mpr_releases(source='raw',as_of='2034-01-01'),class='readboe_config')
  expect_error(boe_mpr_releases(source='raw',max_pages=0),class='readboe_config')
})

test_that('search POST cache identities include all form values and preserve GET identities', {
  calls<-0L
  local_mocked_bindings(boe_perform=function(req){calls<<-calls+1L;httr2::response(200,url=req$url,body=charToRaw('okay'))},.package='readboe')
  cache<-tempfile();on.exit(unlink(cache,recursive=TRUE),add=TRUE)
  url<-'https://www.bankofengland.co.uk/_api/News/RefreshPagedNewsList'
  request<-function(form,...) readboe:::boe_request_raw(url,method='POST',form=form,cache_dir=cache,deadline=10,...)
  a<-request(list(Page='1',Id='news'));b<-request(list(Id='news',Page='1'),cache_only=TRUE)
  expect_equal(calls,1L);expect_true(b$from_cache);expect_equal(a$request_identity,b$request_identity)
  expect_error(request(list(Page='2',Id='news'),cache_only=TRUE),class='readboe_cache_miss')
  request(list(Page='2',Id='news'));expect_equal(calls,2L)
  expect_error(readboe:::boe_request_raw(url,cache_only=TRUE,cache_dir=cache,deadline=10),class='readboe_cache_miss')
  expect_error(request(list(Page=c('1','2'))),class='readboe_config')
  expect_error(readboe:::boe_request_raw(url,form=list(Page='1')),class='readboe_config')
})
