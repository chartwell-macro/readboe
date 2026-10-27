test_that('saved reads use structured data with no network, raw sources or Poppler',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=function(...)stop('HTTP forbidden'),boe_mpr_download=function(...)stop('Raw download forbidden'),boe_mpr_pdf_words=function(...)stop('PDF extraction forbidden'),.package='readboe')
 old<-Sys.getenv('PATH');on.exit(Sys.setenv(PATH=old),add=TRUE);Sys.setenv(PATH='')
 x<-read_boe_mpr_forecasts('latest',data_dir=c$data_dir)
 expect_equal(x$value,c(2,3));expect_equal(x,read_boe_mpr_forecasts('all',data_dir=c$data_dir),ignore_attr=TRUE)
 expect_equal(x,read_boe_mpr_forecasts('2034-03',data_dir=c$data_dir),ignore_attr=TRUE)
 expect_equal(nrow(boe_mpr_catalogue('all',data_dir=c$data_dir)),1L)
 expect_equal(nrow(boe_mpr_releases(as_of=as.Date('2035-01-01'),data_dir=c$data_dir)),1L)
 expect_equal(boe_mpr_assets(data_dir=c$data_dir)$url,c$assets[[1]]$url)
 a<-readboe:::.boe_mpr_memory$snapshot;read_boe_mpr_forecasts(data_dir=c$data_dir)
 expect_identical(a,readboe:::.boe_mpr_memory$snapshot)
 expect_error(read_boe_mpr_forecasts('unknown',data_dir=c$data_dir),class='readboe_identity_mismatch')
 expect_error(read_boe_mpr_forecasts(expected_sha256=strrep('0',64),data_dir=c$data_dir),class='readboe_identity_mismatch')
})

test_that('daily checks preserve files on no-op and throttle without HTTP',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 a<-boe_mpr_update(data_dir=c$data_dir);expect_equal(a$status,'unchanged');expect_length(a$affected_editions,0L)
 expect_false(file.exists(file.path(c$data_dir,'CURRENT.rds')))
 before<-c$requests;b<-boe_mpr_update(data_dir=c$data_dir);expect_equal(b$status,'throttled');expect_equal(c$requests,before)
 d<-boe_mpr_update(data_dir=c$data_dir,force=TRUE);expect_equal(d$status,'unchanged');expect_gt(c$requests,before)
 expect_equal(read_boe_mpr_forecasts(data_dir=c$data_dir)$value,c(2,3))
})

test_that('revised bytes at the same URL feed ordinary reads through atomic refreshed snapshots',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);a<-boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 expect_equal(a$status,'updated');expect_equal(a$affected_editions,'2034-03')
 expect_equal(read_boe_mpr_forecasts(data_dir=c$data_dir)$value,c(4,5))
 expect_equal(boe_mpr_snapshot_status(c$data_dir)$origin,'refreshed')
 file<-readRDS(file.path(c$data_dir,'CURRENT.rds'))$current$file;stamp<-file.info(file.path(c$data_dir,file))$mtime
 b<-boe_mpr_update(data_dir=c$data_dir,force=TRUE);expect_equal(b$status,'unchanged');expect_equal(file.info(file.path(c$data_dir,file))$mtime,stamp)
 c$set_edition('2034-06',7,'2034-06-15');d<-boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 expect_true('2034-06'%in%d$affected_editions)
 expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$value,8)
 expect_equal(nrow(read_boe_mpr_forecasts('all',data_dir=c$data_dir)),3L)
 expect_equal(nrow(read_boe_mpr_forecasts('all',include_history=TRUE,data_dir=c$data_dir)),4L)
 expect_equal(read_boe_mpr_forecasts('2034-03',data_dir=c$data_dir)$value,c(4,5))
})

test_that('failed and interrupted updates retain the prior validated data and explicit failed freshness',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 pointer<-readRDS(file.path(c$data_dir,'CURRENT.rds'));last<-boe_mpr_snapshot_status(c$data_dir)$last_successful_check
 c$set_edition(value=9)
 atomic<-readboe:::boe_mpr_atomic_rds
 local_mocked_bindings(boe_mpr_atomic_rds=function(x,path,...){if(basename(path)=='CURRENT.rds')stop('Interrupted before commit');atomic(x,path,...)},.package='readboe')
 a<-boe_mpr_update(data_dir=c$data_dir,force=TRUE,stop_on_error=FALSE)
 expect_equal(a$status,'failed');expect_match(a$error,'Interrupted')
 expect_identical(readRDS(file.path(c$data_dir,'CURRENT.rds')),pointer)
 expect_warning(x<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(x$value,c(4,5));expect_equal(boe_provenance(x)$snapshot$update_status$status,'failed')
 expect_equal(boe_provenance(x)$snapshot$last_successful_check,last)
 before<-c$requests;expect_equal(boe_mpr_update(data_dir=c$data_dir,stop_on_error=FALSE)$status,'failed_throttled');expect_equal(c$requests,before)
})

test_that('corrupt generations fall back to a previous good snapshot without raw-source recovery',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 c$set_edition(value=6);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 pointer<-readRDS(file.path(c$data_dir,'CURRENT.rds'));writeBin(charToRaw('corrupt'),file.path(c$data_dir,pointer$current$file))
 local_mocked_bindings(boe_perform=function(...)stop('Offline'),.package='readboe')
 expect_warning(x<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(x$value,c(4,5));expect_match(boe_provenance(x)$snapshot$fallback_reason,'hash|decode')
})

test_that('dependencies, parser revisions and coverage losses are validated',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 fp<-readboe:::boe_mpr_parser_fingerprint();local_mocked_bindings(boe_mpr_parser_fingerprint=function()paste(rep('f',64),collapse=''),.package='readboe')
 a<-boe_mpr_update(data_dir=c$data_dir,force=TRUE);expect_equal(a$affected_editions,'2034-03')
 snapshot<-readboe:::boe_mpr_load_saved(c$data_dir)$snapshot
 changed<-snapshot;changed$observations$target_date[1]<-as.Date('1900-01-01')
 expect_error(readboe:::boe_mpr_validate_snapshot(changed),class='readboe_snapshot_corruption')
 changed<-snapshot;changed$observations<-changed$observations[-1,]
 expect_error(readboe:::boe_mpr_check_preservation(snapshot,changed),class='readboe_coverage')
 snapshot$editions[[1]]$historical_search<-TRUE
 snapshot$editions[[1]]$receipts[[1]]$canonical_url<-'https://www.bankofengland.co.uk/revised.zip'
 plan<-readboe:::boe_mpr_update_plan(snapshot,c$releases,c$assets,list(),'https://www.bankofengland.co.uk/revised.zip',FALSE)
 expect_equal(plan,'2034-03')
})

test_that('an active writer blocks another writer and abandoned locks can recover',{
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=function(...)stop('No request while busy'),.package='readboe')
 lock<-readboe:::boe_mpr_lock(c$data_dir)
 raw<-file.path(c$data_dir,'raw');dir.create(raw);body<-file.path(raw,paste0(strrep('b',64),'.rds'));saveRDS(list(body=as.raw(1:10)),body)
 hash<-tools::md5sum(body)
 expect_equal(boe_mpr_update(data_dir=c$data_dir)$status,'busy')
 expect_identical(tools::md5sum(body),hash)
 readboe:::boe_mpr_unlock(lock)
 dir.create(file.path(c$data_dir,'.update-lock'));saveRDS(list(pid=2147483646L,host=Sys.info()[['nodename']],token='old'),file.path(c$data_dir,'.update-lock','owner.rds'))
 Sys.setFileTime(file.path(c$data_dir,'.update-lock'),Sys.time()-120)
 next_lock<-readboe:::boe_mpr_lock(c$data_dir);expect_true(is.list(next_lock));readboe:::boe_mpr_unlock(next_lock)
})

test_that('raw historical search merges partial providers before the complete databank',{
 releases<-data.frame(release=c('2030-03','2033-03','2034-03'),source_published_date=as.Date(c('2030-03-01','2033-03-01','2034-03-01')))
 local_mocked_bindings(boe_mpr_assets_for_release=function(...)data.frame(asset_role='data_bundle'),
 boe_mpr_parse_assets=function(rel,numeric,options,include_distributions,source_rel,historical_only){
  measure<-if(source_rel$release=='2034-03')'bank_rate'else'cpi_inflation'
  list(data=data.frame(series_id=measure,target_period='2031-Q1',measure=measure,value=1),receipts=list(),inventory=data.frame(),primary=list(raw_sha256=strrep('1',64)),databank=source_rel$release=='2033-03')
 },.package='readboe')
 x<-readboe:::boe_mpr_historical_fallback(releases[1,],releases,list(deadline=boe_deadline(10)),TRUE)
 expect_setequal(x$data$measure,c('bank_rate','cpi_inflation'))
 expect_equal(x$inspected_editions,c('2034-03','2033-03'))
})

test_that('conditional probes retain known hashes on 304 and never write empty raw cache data',{
 local_mocked_bindings(boe_perform=function(req)httr2::response(304,url=req$url,headers=list(etag='same')),.package='readboe')
 raw<-tempfile();on.exit(unlink(raw,recursive=TRUE),add=TRUE)
 previous<-list(url='https://www.bankofengland.co.uk/report.pdf',sha256=strrep('1',64),etag='same',last_modified=NA_character_)
 x<-readboe:::boe_mpr_probe(previous$url,previous,boe_deadline(10),raw)
 expect_false(x$changed);expect_equal(x$source$sha256,previous$sha256);expect_null(x$entry)
 expect_length(list.files(raw),0L)
})

test_that('concurrent readers see complete old or new generations and a second writer stays busy',{
 skip_on_os('windows');c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 lock<-readboe:::boe_mpr_lock(c$data_dir)
 child<-parallel::mcparallel(boe_mpr_update(data_dir=c$data_dir,force=TRUE))
 b<-parallel::mccollect(child)[[1L]];expect_equal(b$status,'busy');readboe:::boe_mpr_unlock(lock)
 child<-parallel::mcparallel({v<-list();for(i in 1:40){v[[i]]<-read_boe_mpr_forecasts(data_dir=c$data_dir)$value;Sys.sleep(.005)};v})
 c$set_edition(value=8);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 values<-parallel::mccollect(child)[[1L]]
 expect_false(inherits(values,'try-error'));expect_true(all(vapply(values,function(x)identical(x,c(4,5))||identical(x,c(8,9)),logical(1))))
})

test_that('parser changes bypass a successful same-day throttle', {
 c <- saved_fixture(); on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 expect_equal(boe_mpr_update(data_dir=c$data_dir)$status,'unchanged')
 local_mocked_bindings(boe_mpr_parser_fingerprint=function()strrep('e',64),.package='readboe')
 x <- boe_mpr_update(data_dir=c$data_dir)
 expect_equal(x$status,'updated'); expect_equal(x$affected_editions,'2034-03')
})

test_that('updated scheduled dates persist without reparsing numeric editions', {
 c <- saved_fixture(); on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 c$set_edition('2034-06',7,'2034-06-15'); c$releases$publication_status[2]<-'scheduled'
 c$releases$source_published_date[2]<-c$releases$release_date[2]<-as.Date('2034-06-16')
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,
  boe_mpr_build_record=function(...)stop('No numeric edition changed'),.package='readboe')
 x <- boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 expect_equal(x$status,'updated'); expect_length(x$affected_editions,0L)
 r <- boe_mpr_releases(include_upcoming=TRUE,as_of=as.Date('2035-01-01'),data_dir=c$data_dir)
 expect_equal(r$release_date[2],as.Date('2034-06-16')); expect_equal(r$publication_status[2],'scheduled')
 expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$release[1],'2034-03')
 # Reuse a checked upcoming page's structured link inventory on 304.
 state_path<-file.path(c$data_dir,'STATUS.rds');state<-readboe:::boe_mpr_load_saved(c$data_dir)$status
 state$attempted_at<-as.POSIXct('2020-01-01',tz='UTC');saveRDS(state,state_path)
 current_path<-file.path(c$data_dir,'CURRENT.rds');current<-readRDS(current_path)
 current$current$status$attempted_at<-state$attempted_at;saveRDS(current,current_path)
 before<-c$requests;second<-boe_mpr_update(data_dir=c$data_dir)
 # Two report pages and the published edition's numeric asset; the scheduled
 # edition's asset remains unrequested.
 expect_equal(second$status,'unchanged');expect_equal(c$requests-before,3L)
})

test_that('changed links and failed acquisition cannot report fresh success or lose coverage', {
 c <- saved_fixture(); on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 old <- boe_mpr_snapshot_status(c$data_dir)$last_successful_check
 url <- c$assets[[1]]$url; next_url <- sub('forecast.xlsx','revised.xlsx',url,fixed=TRUE)
 c$bodies[[next_url]] <- c$bodies[[url]]
 landing <- c$releases$landing_url[1]; c$bodies[[landing]]<-charToRaw(paste0('<a href="',next_url,'">Forecast data</a>'))
 a <- boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 expect_equal(a$status,'updated'); expect_true(all(read_boe_mpr_forecasts(data_dir=c$data_dir)$source_url==next_url))
 previous <- a$snapshot_sha256
 c$bodies[[next_url]] <- NULL
 b <- boe_mpr_update(data_dir=c$data_dir,force=TRUE,stop_on_error=FALSE)
 expect_equal(b$status,'failed'); expect_equal(b$snapshot_sha256,previous)
 expect_equal(boe_mpr_snapshot_status(c$data_dir)$last_successful_check,a$last_successful_check)
 expect_warning(x<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(x$value,c(2,3))
 c$releases <- c$releases[FALSE,]
 d <- boe_mpr_update(data_dir=c$data_dir,force=TRUE,stop_on_error=FALSE)
 expect_equal(d$status,'failed');expect_equal(d$error_class,'readboe_coverage')
})

test_that('later databank additions and same-URL revisions invalidate prior absence searches', {
 c <- saved_fixture(); on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 old <- readRDS(c$paths()[['data']]); old$editions[[1]]$historical_search<-TRUE
 # A proved absence is still a dependency user, even with no observations.
 old$editions[[1]]$status <- 'no_numeric_forecast'
 c$set_edition('2034-06',7,'2034-06-15')
 c$assets[[2]]$asset_role <- 'data_bundle'
 plan <- readboe:::boe_mpr_update_plan(old,c$releases,c$assets,list(),character(),FALSE)
 expect_setequal(plan,c('2034-03','2034-06'))
 old$releases<-c$releases;old$editions[['2034-06']]<-old$editions[[1]]
 old$editions[['2034-06']]$assets<-c$assets[[2]]
 plan <- readboe:::boe_mpr_update_plan(old,c$releases,c$assets,list(),c$assets[[2]]$url,FALSE)
 expect_true('2034-03'%in%plan)
})

test_that('full bundled data are available offline with explicit absence inventory', {
 local_mocked_bindings(boe_perform=function(...)stop('HTTP forbidden'),
  boe_mpr_download=function(...)stop('Raw bytes forbidden'),boe_mpr_pdf_words=function(...)stop('PDF forbidden'),.package='readboe')
 directory <- tempfile(); old<-Sys.getenv('PATH');on.exit(Sys.setenv(PATH=old),add=TRUE);Sys.setenv(PATH='')
 releases <- boe_mpr_releases(include_upcoming=TRUE,as_of=as.Date('2100-01-01'),data_dir=directory)
 expect_gte(sum(releases$publication_status=='published'),135L)
 expect_equal(sum(releases$coverage_status=='no_numeric_forecast'),20L)
 x <- read_boe_mpr_forecasts('all',include_history=TRUE,include_distributions=TRUE,data_dir=directory)
 expect_gte(nrow(x),149526L); expect_false(anyDuplicated(readboe:::boe_mpr_keys(x))>0L)
 expect_true(all(c('raw_value','value_status','source_row','source_column','source_page','source_url')%in%names(x)))
 expect_true(all(c('central_forecast','conditioning_assumption','scenario_projection')%in%x$kind))
 expect_error(read_boe_mpr_forecasts(releases$release[releases$coverage_status=='no_numeric_forecast'][1],data_dir=directory),class='readboe_no_numeric_forecast')
})

test_that('parser fingerprints follow XML/PDF helper changes and ignore saved selectors', {
 before <- readboe:::boe_mpr_parser_fingerprint()
 helper <- readboe:::boe_mpr_col
 local_mocked_bindings(boe_mpr_col=function(x)helper(x)+0L,.package='readboe')
 expect_false(identical(before,readboe:::boe_mpr_parser_fingerprint()))
})

test_that('raw retention and numeric generations stay bounded', {
 directory <- tempfile();dir.create(directory);on.exit(unlink(directory,recursive=TRUE),add=TRUE)
 hashes <- vapply(1:7,function(i)digest::digest(i,algo='sha256'),character(1L))
 files <- paste0('mpr-',hashes,'.rds')
 for(i in seq_along(files))saveRDS(i,file.path(directory,files[i]))
 saveRDS(list(schema=1L,current=list(file=files[7],sha256=hashes[7]),previous=list(file=files[6],sha256=hashes[6])),file.path(directory,'CURRENT.rds'))
 readboe:::boe_mpr_prune_generations(directory)
 expect_length(list.files(directory,pattern='^mpr-'),4L)
 expect_true(all(file.exists(file.path(directory,files[6:7]))))
 raw <- file.path(directory,'raw');dir.create(raw)
 paths <- file.path(raw,paste0(hashes,'.rds'))
 for(path in paths)saveRDS(rep(1,100),path,compress=FALSE)
 Sys.setFileTime(paths[1],Sys.time()-10*86400)
 readboe:::boe_mpr_prune_sources(raw,max_bytes=3000,max_age_days=7)
 expect_false(file.exists(paths[1]));expect_lte(sum(file.info(list.files(raw,full.names=TRUE))$size),3000)
})

test_that('snapshot validation rejects conflicting dimensions, catalogues, identities and revisions', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 x<-readRDS(c$paths()[['data']])
 bad<-x;bad$observations$measure<-as.character(bad$observations$measure);bad$observations$measure[2]<-'gdp_growth'
 expect_error(readboe:::boe_mpr_validate_snapshot(bad),class='readboe_snapshot_corruption')
 bad<-x;bad$observations$target_period[2]<-bad$observations$target_period[1]
 expect_error(readboe:::boe_mpr_validate_snapshot(bad),class='readboe_snapshot_corruption')
 bad<-x;bad$catalogue<-bad$catalogue[FALSE,]
 expect_error(readboe:::boe_mpr_validate_snapshot(bad),class='readboe_snapshot_corruption')
 bad<-x;url<-c$assets[[1]]$url;bad$sources[[url]]$sha256<-strrep('0',64)
 expect_error(readboe:::boe_mpr_validate_snapshot(bad),class='readboe_source_revision')
 bad<-x;bad$observations$.forecast[1]<-!bad$observations$.forecast[1]
 expect_error(readboe:::boe_mpr_validate_snapshot(bad),class='readboe_snapshot_corruption')
 bad<-x;bad$payload<-as.raw(1)
 expect_error(readboe:::boe_mpr_validate_snapshot(bad),class='readboe_snapshot_corruption')
})

test_that('a revision changing again before required raw replay rejects the candidate', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 original_build<-readboe:::boe_mpr_build_record
 changing_build<-function(key,releases,inventory,fetch,raw_dir,deadline) {
  c$set_edition(value=9)
  url<-inventory[[key]]$url[1];cache_key<-readboe:::boe_cache_key(list(method='GET',url=url,accept='*/*',contract=1L))
  unlink(file.path(raw_dir,paste0(cache_key,'.rds')))
  original_build(key,releases,inventory,fetch,raw_dir,deadline)
 }
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,boe_mpr_build_record=changing_build,.package='readboe')
 c$set_edition(value=4)
 x<-boe_mpr_update(data_dir=c$data_dir,force=TRUE,stop_on_error=FALSE)
 expect_equal(x$status,'failed');expect_equal(x$error_class,'readboe_source_revision')
 expect_false(file.exists(file.path(c$data_dir,'CURRENT.rds')))
 expect_warning(saved<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(saved$value,c(2,3))
})

test_that('a real later synthetic databank fills an absence and revises earlier saved vintages', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 initial<-readRDS(c$paths()[['data']]);existing<-readboe:::boe_mpr_unpack_record(initial,'2034-03')
 oldrel<-c$releases[1,,drop=FALSE];oldrel$release<-'2033-03';oldrel$release_date<-oldrel$source_published_date<-as.Date('2033-03-12')
 oldrel$release_period_start<-as.Date('2033-03-01');oldrel$landing_url<-sub('2034','2033',oldrel$landing_url)
 c$releases<-rbind(oldrel,c$releases);page<-charToRaw('<html>Synthetic chart/prose-only fixture</html>')
 c$bodies[[oldrel$landing_url]]<-page
 entry<-list(url=oldrel$landing_url,body=page,raw_sha256=readboe:::boe_sha256_raw(page),retrieved_at=Sys.time(),status=200L,etag=readboe:::boe_sha256_raw(page),last_modified=NA_character_)
 assets<-readboe:::boe_mpr_assets_from_entry(oldrel,entry);receipt<-readboe:::boe_receipt_from_entry(entry)
 absent<-tryCatch(readboe:::boe_abort('Synthetic evidenced absence','no_numeric_forecast',release='2033-03',assets=assets,
   inventory=data.frame(),receipts=list(receipt),source_evidence=list(pages_inspected=1L,evidence_scope='synthetic fixture')),error=identity)
 records<-list(`2033-03`=readboe:::boe_mpr_record(absent,absent,assets),`2034-03`=existing)
 snapshot<-readboe:::boe_mpr_pack_snapshot(c$releases,records,Sys.time(),readboe:::boe_mpr_sources_from_records(records))
 readboe:::boe_mpr_write_bundle(snapshot,c$bundle_dir)
 c$set_edition('2034-06',7,'2034-06-15')
 landing<-c$releases$landing_url[c$releases$release=='2034-06']
 url<-'https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/2034/june/data.zip'
 c$bodies[[landing]]<-charToRaw(paste0('<a href="',url,'">Forecast data</a>'))
 set_bank<-function(old_value) {
   x<-matrix(NA_character_,8,3);x[1:3,1]<-c('CPI inflation','Per cent','Four-quarter percentage change')
   x[5,]<-c('Date of publication','2034 Q3','2034 Q4');x[6,]<-c('March 2033',old_value,old_value+1)
   x[7,]<-c('March 2034',2,3);x[8,]<-c('June 2034',7,8)
   path<-tempfile(fileext='.xlsx');openxlsx::write.xlsx(list(Forecast=as.data.frame(x)),path,colNames=FALSE)
   member<-readBin(path,'raw',n=file.info(path)$size);unlink(path)
   zip<-mpr_test_zip('Projections Databank.xlsx',list(member));c$bodies[[url]]<-readBin(zip,'raw',n=file.info(zip)$size);unlink(zip)
 }
 set_bank(4)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 first<-boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 expect_setequal(first$affected_editions,c('2033-03','2034-03','2034-06'))
 expect_equal(read_boe_mpr_forecasts('2033-03',data_dir=c$data_dir)$value,c(4,5))
 expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$value,c(7,8))
 set_bank(9);second<-boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 expect_true('2033-03'%in%second$affected_editions)
 expect_equal(read_boe_mpr_forecasts('2033-03',data_dir=c$data_dir)$value,c(9,10))
 expect_equal(boe_mpr_snapshot_status(c$data_dir)$coverage[['parsed']],3L)
})

test_that('an actual interrupt before CURRENT cannot publish new generation freshness', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 before<-boe_mpr_snapshot_status(c$data_dir);pointer<-readRDS(file.path(c$data_dir,'CURRENT.rds'))
 c$set_edition(value=9);atomic<-readboe:::boe_mpr_atomic_rds
 local_mocked_bindings(boe_mpr_atomic_rds=function(x,path,...){
  if(basename(path)=='CURRENT.rds')stop(structure(list(message='Actual pre-commit interrupt',call=NULL),class=c('interrupt','condition')))
  atomic(x,path,...)
 },.package='readboe')
 interrupted<-tryCatch(boe_mpr_update(data_dir=c$data_dir,force=TRUE),interrupt=function(e)TRUE)
 expect_true(interrupted);expect_identical(pointer,readRDS(file.path(c$data_dir,'CURRENT.rds')))
 expect_length(list.files(file.path(c$data_dir,'raw')),0L)
 after<-boe_mpr_snapshot_status(c$data_dir);loaded<-readboe:::boe_mpr_load_saved(c$data_dir)
 expect_equal(loaded$status$status,'interrupted');expect_identical(loaded$sha256,loaded$status$snapshot_sha256)
 expect_identical(after$last_successful_check,before$last_successful_check)
 expect_warning(x<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(x$value,c(4,5))
 expect_equal(boe_mpr_update(data_dir=c$data_dir)$status,'failed_throttled')
})

test_that('a process exit in the pre-commit crash window leaves old data and successful time intact', {
 skip_on_os('windows');c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 before<-boe_mpr_snapshot_status(c$data_dir);pointer<-readRDS(file.path(c$data_dir,'CURRENT.rds'))
 c$set_edition(value=9);atomic<-readboe:::boe_mpr_atomic_rds
 child<-parallel::mcparallel({
  testthat::local_mocked_bindings(boe_mpr_atomic_rds=function(x,path,...){
   if(basename(path)=='CURRENT.rds'){tools::pskill(Sys.getpid(),signal=9L);Sys.sleep(2);stop('Child process kill unavailable')}
   atomic(x,path,...)
  },.package='readboe')
  boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 })
 suppressWarnings(parallel::mccollect(child))
 raw<-file.path(c$data_dir,'raw')
 expect_gt(length(list.files(raw,pattern='^[a-f0-9]{64}[.]rds$')),0L)
 expect_lte(sum(file.info(list.files(raw,full.names=TRUE))$size),2*1024^3)
 expect_identical(pointer,readRDS(file.path(c$data_dir,'CURRENT.rds')))
 staged<-readRDS(file.path(c$data_dir,'SOURCES.rds'))
 expect_false(identical(staged$snapshot_sha256,before$snapshot_sha256))
 after<-boe_mpr_snapshot_status(c$data_dir);loaded<-readboe:::boe_mpr_load_saved(c$data_dir)
 expect_identical(after$last_successful_check,before$last_successful_check)
 expect_identical(loaded$sha256,loaded$status$snapshot_sha256)
 expect_equal(read_boe_mpr_forecasts(data_dir=c$data_dir)$value,c(4,5))
 Sys.setFileTime(file.path(c$data_dir,'.update-lock'),Sys.time()-120)
 # Even a same-day throttled recovered writer clears its predecessor's bodies.
 expect_equal(boe_mpr_update(data_dir=c$data_dir)$status,'throttled')
 expect_length(list.files(raw),0L)
 expect_equal(boe_mpr_update(data_dir=c$data_dir,force=TRUE)$status,'updated')
 expect_equal(read_boe_mpr_forecasts(data_dir=c$data_dir)$value,c(9,10))
})

test_that('advisory status from a different generation never supplies successful freshness', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 before<-boe_mpr_snapshot_status(c$data_dir)
 fake<-list(status='updated',snapshot_sha256=strrep('f',64),attempted_at=Sys.time()+10,last_successful_check=Sys.time()+10)
 saveRDS(fake,file.path(c$data_dir,'STATUS.rds'))
 after<-boe_mpr_snapshot_status(c$data_dir)
 expect_identical(after$last_successful_check,before$last_successful_check)
 expect_identical(after$update_status$snapshot_sha256,after$snapshot_sha256)
 pointer<-readRDS(file.path(c$data_dir,'CURRENT.rds'));pointer$current$status<-NULL
 saveRDS(pointer,file.path(c$data_dir,'CURRENT.rds'))
 legacy<-boe_mpr_snapshot_status(c$data_dir)
 expect_equal(legacy$update_status$status,'interrupted')
 expect_identical(legacy$update_status$snapshot_sha256,legacy$snapshot_sha256)
 expect_lte(legacy$last_successful_check,before$last_successful_check)
})

test_that('a post-commit interrupt still reports the actual committed generation', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 c$set_edition(value=4);boe_mpr_update(data_dir=c$data_dir,force=TRUE)
 before<-boe_mpr_snapshot_status(c$data_dir);c$set_edition(value=9);atomic<-readboe:::boe_mpr_atomic_rds
 local_mocked_bindings(boe_mpr_atomic_rds=function(x,path,...){
  atomic(x,path,...)
  if(basename(path)=='CURRENT.rds')stop(structure(list(message='Actual post-commit interrupt',call=NULL),class=c('interrupt','condition')))
 },.package='readboe')
 expect_true(tryCatch(boe_mpr_update(data_dir=c$data_dir,force=TRUE),interrupt=function(e)TRUE))
 after<-boe_mpr_snapshot_status(c$data_dir)
 expect_false(identical(after$snapshot_sha256,before$snapshot_sha256))
 expect_identical(after$snapshot_sha256,after$update_status$snapshot_sha256)
 expect_gte(after$last_successful_check,before$last_successful_check)
 expect_warning(x<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(x$value,c(9,10))
})

test_that('the installed scheduler shares reader directory defaults and respects explicit overrides', {
 script<-system.file('scripts','update-mpr-forecasts.R',package='readboe')
 expect_true(nzchar(script))
 previous<-Sys.getenv(c('READBOE_MPR_DATA_DIR','GITHUB_STEP_SUMMARY'),unset=NA_character_)
 on.exit({for(name in names(previous)){if(is.na(previous[[name]]))Sys.unsetenv(name)else do.call(Sys.setenv,stats::setNames(list(previous[[name]]),name))}},add=TRUE)
 Sys.setenv(GITHUB_STEP_SUMMARY='')
 default<-file.path(tools::R_user_dir('readboe','data'),'mpr')
 selected<-tempfile('scheduled-selected-');override<-tempfile('scheduled-explicit-')
 cases<-list(list(value=NULL,args=character(),expected=default),
  list(value='',args=character(),expected=default),
  list(value=selected,args=character(),expected=selected),
  list(value=selected,args=c('--data-dir',override),expected=override))
 for(case in cases){
  if(is.null(case$value))Sys.unsetenv('READBOE_MPR_DATA_DIR')else Sys.setenv(READBOE_MPR_DATA_DIR=case$value)
  environment<-new.env(parent=globalenv());captured<-NULL
  environment$commandArgs<-function(...)case$args
  environment$boe_mpr_update<-function(data_dir,...){captured<<-data_dir;list(status='throttled',snapshot_sha256=strrep('0',64),affected_editions=character())}
  capture.output(sys.source(script,envir=environment))
  expect_identical(captured,case$expected)
 }
 expect_false(dir.exists(selected));expect_false(dir.exists(override))
})

test_that('the writer retains its lock through all raw and generation cleanup', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 raw_clear<-readboe:::boe_mpr_clear_sources;generation_prune<-readboe:::boe_mpr_prune_generations
 observed<-logical();locked<-function(){other<-readboe:::boe_mpr_lock(c$data_dir);observed<<-c(observed,is.null(other));if(!is.null(other))readboe:::boe_mpr_unlock(other)}
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,
  boe_mpr_clear_sources=function(...){locked();raw_clear(...)},
  boe_mpr_prune_generations=function(...){locked();generation_prune(...)},.package='readboe')
 expect_equal(boe_mpr_update(data_dir=c$data_dir)$status,'unchanged')
 expect_identical(observed,c(TRUE,TRUE));expect_false(dir.exists(file.path(c$data_dir,'.update-lock')))
})

test_that('default source staging is disposable and conditional no-ops need no bodies or parsing', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 requests<-list();perform<-function(req){requests[[length(requests)+1L]]<<-req;c$perform(req)}
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 x<-boe_mpr_update(c$data_dir,force=TRUE)
 expect_equal(x$status,'unchanged');raw<-file.path(c$data_dir,'raw')
 expect_length(list.files(raw),0L)
 sources<-readRDS(file.path(c$data_dir,'SOURCES.rds'))
 expect_true(all(vapply(sources$sources,function(x)nzchar(x$sha256)&&nzchar(x$etag),logical(1L))))
 requests<-list()
 local_mocked_bindings(boe_mpr_build_record=function(...)stop('No reparse allowed'),.package='readboe')
 x<-readboe:::boe_mpr_update_run(c$data_dir,bypass_daily=TRUE)
 expect_equal(x$status,'unchanged');expect_length(requests,2L)
 expect_true(all(vapply(requests,function(x)!is.na(readboe:::boe_header(x$headers,'If-None-Match')),logical(1L))))
 expect_length(list.files(raw),0L)
 expect_equal(read_boe_mpr_forecasts(data_dir=c$data_dir)$value,c(2,3))
})

test_that('new and revised XLSX data ingest through disposable staging with retained metadata', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 build<-readboe:::boe_mpr_build_record;working<-integer()
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,
  boe_mpr_build_record=function(key,releases,inventory,fetch,raw_dir,deadline){
   working<<-c(working,length(list.files(raw_dir)));build(key,releases,inventory,fetch,raw_dir,deadline)
  },.package='readboe')
 c$set_edition('2034-06',7,'2034-06-15')
 first<-boe_mpr_update(c$data_dir,force=TRUE)
 expect_equal(first$status,'updated');expect_true('2034-06'%in%first$affected_editions)
 expect_true(all(working>0L));expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$value,8)
 raw<-file.path(c$data_dir,'raw');expect_length(list.files(raw),0L)
 c$set_edition('2034-06',9,'2034-06-15')
 second<-readboe:::boe_mpr_update_run(c$data_dir,bypass_daily=TRUE)
 expect_equal(second$status,'updated');expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$value,10)
 expect_length(list.files(raw),0L)
 expect_identical(readRDS(file.path(c$data_dir,'SOURCES.rds'))$snapshot_sha256,second$snapshot_sha256)
})

test_that('retention is explicit and default cleanup preserves unrelated files and last-good data', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,
  boe_perform=c$perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 boe_mpr_update(c$data_dir,force=TRUE,retain_raw=TRUE)
 raw<-file.path(c$data_dir,'raw');expect_gt(length(list.files(raw)),0L)
 before<-boe_mpr_snapshot_status(c$data_dir)$last_successful_check
 source_hash<-tools::md5sum(file.path(c$data_dir,'SOURCES.rds'))
 note<-file.path(raw,'my-note.txt');writeLines('User file',note)
 stale<-file.path(raw,'readboe-cache-abc123');writeBin(as.raw(1:20),stale)
 c$set_edition(value=9)
 local_mocked_bindings(boe_mpr_build_record=function(...)stop('Injected parse failure after acquisition'),.package='readboe')
 x<-boe_mpr_update(c$data_dir,force=TRUE,stop_on_error=FALSE)
 expect_equal(x$status,'failed');expect_equal(list.files(raw),'my-note.txt');expect_true(file.exists(note));expect_false(file.exists(stale))
 expect_identical(tools::md5sum(file.path(c$data_dir,'SOURCES.rds')),source_hash)
 expect_warning(y<-read_boe_mpr_forecasts(data_dir=c$data_dir),'last good');expect_equal(y$value,c(2,3))
 expect_identical(boe_mpr_snapshot_status(c$data_dir)$last_successful_check,before)
})
