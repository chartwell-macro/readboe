test_that('calendar rows are scoped, duplicate equivalent links collapse and minutes are excluded', {
 f<-mpr_calendar_fixture(c('2034-03-16','2035-02-01'),duplicate=TRUE)
 x<-readboe:::boe_mpr_parse_calendar(f$calendar,f$times,as.POSIXct('2034-01-01',tz='UTC'))
 expect_equal(x$edition,c('2034-03','2035-02'));expect_equal(nrow(x),2L)
 f$calendar$body<-charToRaw(sub('</td></tr>','<a href="https://www.bankofengland.co.uk/monetary-policy-report/2034/june-2034">Different MPR</a></td></tr>',rawToChar(f$calendar$body),fixed=TRUE))
 expect_error(readboe:::boe_mpr_parse_calendar(f$calendar,f$times,as.POSIXct('2034-01-01',tz='UTC')),class='readboe_schema_drift')
})

test_that('explicit London clocks convert winter/summer and unknown distant clocks stay unknown', {
 f<-mpr_calendar_fixture(c('2034-02-02','2034-04-27','2034-07-27','2034-11-02'),c('12pm','12pm','12pm',NA))
 x<-readboe:::boe_mpr_parse_calendar(f$calendar,f$times,as.POSIXct('2034-01-01',tz='UTC'))
 expect_equal(format(x$publication_at[1:3],tz='UTC',format='%H:%M'),c('12:00','11:00','11:00'))
 expect_true(is.na(x$publication_at[4]));expect_equal(x$due_basis[4],'date_start_metadata_poll')
 expect_equal(format(x$due_at[4],tz='Europe/London',format='%H:%M'),'00:00')
 expect_equal(readboe:::boe_mpr_clock('at 10.30am'),'10:30')
 expect_error(readboe:::boe_mpr_london_time(as.Date('2034-10-29'),'01:30'),class='readboe_schema_drift')
})

test_that('retained actual official calendar DOM parses with partial publication-time coverage', {
 path<-Sys.getenv('READBOE_MPR_CALENDAR_FIXTURE');times<-Sys.getenv('READBOE_MPR_TIMES_FIXTURE')
 skip_if(!nzchar(path)||!nzchar(times),'Set both official calendar replay fixture paths')
 a<-readRDS(path);b<-readRDS(times)
 x<-readboe:::boe_mpr_parse_calendar(a,b,as.POSIXct('2026-10-25',tz='UTC'))
 expect_equal(as.character(x$release_date),c('2026-02-05','2026-04-30','2026-07-30','2026-11-05','2027-02-04','2027-04-29','2027-07-29','2027-11-04'))
 expect_equal(format(x$publication_at[x$edition=='2026-11'],tz='UTC',format='%H:%M'),'12:00')
})

test_that('not-due invocations read only metadata and never load data, fetch or write', {
 c<-mpr_maintenance_fixture();on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 first<-boe_mpr_maintain(c$dir,now=c$now);expect_equal(first$next_due_at,'2034-03-16T12:00:00Z')
 expect_equal(first$next_release_edition,'2034-03');expect_equal(c$updates,0L)
 files<-list.files(c$dir,recursive=TRUE,full.names=TRUE);before<-tools::md5sum(files)
 local_mocked_bindings(boe_mpr_maintenance_data=function(...)stop('data load forbidden'),boe_mpr_calendar=function(...)stop('network forbidden'),boe_mpr_maintenance_write=function(...)stop('write forbidden'),.package='readboe')
 idle<-boe_mpr_maintain(c$dir,now=c$now+60);expect_equal(idle$status,'idle')
 expect_identical(tools::md5sum(files),before)
})

test_that('unknown calendar availability is distinct from a known exhausted horizon', {
 c<-mpr_maintenance_fixture();on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 initial<-boe_mpr_maintenance_status(c$dir,now=c$now)
 expect_null(initial$calendar_horizon_exhausted);expect_null(initial$next_release_edition)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 c$calendar_fail<-TRUE
 unavailable<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(unavailable$status,'failed');expect_null(unavailable$calendar_horizon_exhausted)
 expect_equal(unavailable$revision_next_check_at,readboe:::boe_mpr_utc(c$checked+86400))
 c$calendar_fail<-FALSE
 available<-boe_mpr_maintain(c$dir,now=c$now,force=TRUE)
 expect_false(available$calendar_horizon_exhausted)
 c$now<-as.POSIXct('2034-06-20 12:00:00',tz='UTC');c$checked<-c$now;c$editions<-union(c$editions,c$rows$edition)
 exhausted<-boe_mpr_maintain(c$dir,now=c$now,force=TRUE)
 expect_true(exhausted$calendar_horizon_exhausted);expect_null(exhausted$next_release_edition)
 expect_equal(exhausted$revision_next_check_at,readboe:::boe_mpr_utc(c$now+86400))
})

test_that('release metadata triggers one conditional throttle bypass and advances the next official event', {
 c<-mpr_maintenance_fixture();on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 boe_mpr_maintain(c$dir,now=c$now)
 c$now<-c$now+15*60;waiting<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(waiting$status,'waiting');expect_equal(c$updates,0L)
 c$now<-c$now+15*60;c$ready<-TRUE
 done<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,1L);expect_true(c$bypass[1]);expect_equal(done$next_release_edition,'2034-06')
 expect_false(c$retain_raw)
 expect_gte(c$calendar_calls,2L)
 c$now<-c$now+3600;again<-boe_mpr_maintain(c$dir,now=c$now,force=TRUE)
 expect_equal(c$updates,1L);expect_equal(again$status,'waiting')
 c$fp<-'new-asset-link';c$now<-c$now+3600;boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,2L)
})

test_that('maintenance release asset probes never cache bodies outside the updater lock', {
 c<-mpr_maintenance_fixture();directory<-tempfile();dir.create(directory)
 on.exit(unlink(directory,recursive=TRUE),add=TRUE)
 row<-c$rows[1,,drop=FALSE]
 url<-'https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/2034/march/data.zip'
 body<-charToRaw('Checked asset bytes');hash<-readboe:::boe_sha256_raw(body)
 source<-list(sha256=hash,etag='v1',last_modified=NA_character_)
 saveRDS(list(snapshot_sha256=c$sha,sources=stats::setNames(list(source),url)),file.path(directory,'SOURCES.rds'))
 # A concurrent updater's working body is deliberately preserved.
 raw<-file.path(directory,'raw');dir.create(raw);owned<-file.path(raw,paste0(strrep('b',64),'.rds'));saveRDS(list(body=as.raw(1:10)),owned)
 before<-tools::md5sum(owned);status<-200L
 perform<-function(req){if(req$url==row$landing_url)return(httr2::response(200,url=req$url,body=charToRaw(paste0('<main><h1>Monetary Policy Report</h1><a href="',url,'">Data</a></main>'))));httr2::response(status,url=req$url,body=if(status==304L)raw()else body,headers=list(etag='v1'))}
 local_mocked_bindings(boe_perform=perform,boe_cache_write=function(...)stop('Probe must not write cache'),.package='readboe')
 unchanged<-readboe:::boe_mpr_maintenance_probe(row,directory,boe_deadline(10),c$sha)
 expect_true(unchanged$ready);expect_null(unchanged$revision)
 body<-charToRaw('Corrected asset bytes');changed<-readboe:::boe_mpr_maintenance_probe(row,directory,boe_deadline(10),c$sha)
 expect_identical(changed$revision,readboe:::boe_sha256_raw(body))
 status<-304L;expect_null(readboe:::boe_mpr_maintenance_probe(row,directory,boe_deadline(10),c$sha)$revision)
 expect_identical(tools::md5sum(owned),before);expect_equal(list.files(raw),basename(owned))
})

test_that('calendar failure keeps last-good dates while an independent due daily check succeeds', {
 c<-mpr_maintenance_fixture();on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 boe_mpr_maintain(c$dir,now=c$now);before<-readRDS(file.path(c$dir,'MAINTENANCE.rds'))$calendar
 c$now<-c$now+25*3600;c$calendar_fail<-TRUE
 failed<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,1L);expect_equal(failed$updater_outcome,'unchanged')
 expect_equal(failed$status,'failed');expect_match(failed$calendar_error$message,'calendar unavailable')
 expect_identical(readRDS(file.path(c$dir,'MAINTENANCE.rds'))$calendar,before)
 expect_false(failed$revision_overdue)
})

test_that('a report more than a day late cannot suppress historical daily revision checks', {
 c<-mpr_maintenance_fixture('2034-03-17 18:00:00');c$checked<-c$now-26*3600
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 x<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,1L);expect_equal(x$updater_outcome,'unchanged');expect_false(x$revision_overdue)
 expect_gt(x$release_overdue_seconds,24*3600)
 c$now<-c$now+25*3600;boe_mpr_maintain(c$dir,now=c$now);expect_equal(c$updates,2L)
})

test_that('failed full updates are retried at bounded intervals rather than each hourly wake', {
 c<-mpr_maintenance_fixture('2034-03-16 12:05:00');c$ready<-TRUE;c$update_fail<-TRUE
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 first<-boe_mpr_maintain(c$dir,now=c$now);expect_equal(first$status,'failed');expect_equal(c$updates,1L)
 for(i in 1:5){c$now<-c$now+3600;boe_mpr_maintain(c$dir,now=c$now)}
 expect_equal(c$updates,1L)
 c$now<-c$now+3600;boe_mpr_maintain(c$dir,now=c$now);expect_equal(c$updates,2L)
})

test_that('expired release errors become bounded history after a successful daily ingestion', {
 c<-mpr_maintenance_fixture('2034-03-16 12:05:00');c$probe_fail<-TRUE
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 expect_equal(boe_mpr_maintain(c$dir,now=c$now)$status,'failed')
 c$now<-c$now+3*86400;c$ready<-TRUE
 x<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(x$updater_outcome,'updated');expect_length(x$errors,0L);expect_equal(x$status,'success')
 state<-readRDS(file.path(c$dir,'MAINTENANCE.rds'))
 expect_null(state$releases[['2034-03']]);expect_length(state$history,1L)
 expect_match(state$history[[1]]$record$error$message,'probe unavailable')
})

test_that('calendar year rollover carries missed old editions but recomputes future schedule', {
 c<-mpr_maintenance_fixture('2034-12-31 12:00:00');f<-mpr_calendar_fixture(c('2034-11-02','2035-02-01'))
 c$rows<-readboe:::boe_mpr_parse_calendar(f$calendar,f$times,c$now);c$checked<-c$now;c$ready<-FALSE
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 boe_mpr_maintain(c$dir,now=c$now)
 f<-mpr_calendar_fixture(c('2035-02-01','2035-04-26'));c$rows<-readboe:::boe_mpr_parse_calendar(f$calendar,f$times,c$now)
 c$now<-c$now+86400;x<-boe_mpr_maintain(c$dir,now=c$now)
 state<-readRDS(file.path(c$dir,'MAINTENANCE.rds'))
 expect_true('2034-11'%in%state$calendar$edition);expect_equal(x$next_release_edition,'2034-11')
 c$editions<-union(c$editions,'2034-11');c$now<-c$now+86400
 x<-boe_mpr_maintain(c$dir,now=c$now);expect_equal(x$next_release_edition,'2035-02')
})

test_that('metadata fingerprints ignore nonces and calendar receipt changes', {
 c<-mpr_maintenance_fixture();directory<-tempfile();on.exit(unlink(directory,recursive=TRUE),add=TRUE)
 row<-c$rows[1,,drop=FALSE]
 make<-function(nonce){body<-charToRaw(paste0('<html><main><h1>Monetary Policy Report</h1><input value="',nonce,'"><a href="https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/2034/march/data.zip">Data</a></main></html>'));list(url=row$landing_url,body=body,raw_sha256=readboe:::boe_sha256_raw(body),retrieved_at=Sys.time(),status=200L)}
 entry<-make('one');local_mocked_bindings(boe_request_raw=function(...)entry,.package='readboe')
 a<-readboe:::boe_mpr_maintenance_probe(row,directory,boe_deadline(10))
 entry<-make('two');b<-readboe:::boe_mpr_maintenance_probe(row,directory,boe_deadline(10))
 expect_identical(a$fingerprint,b$fingerprint)
 before<-readboe:::boe_mpr_maintenance_identity(row);attr(row,'boe_provenance')<-list(checked_at=Sys.time(),nonce='different')
 expect_identical(before,readboe:::boe_mpr_maintenance_identity(row))
})

test_that('a removed date within the covered year closes its stale release record', {
 c<-mpr_maintenance_fixture('2034-03-16 12:05:00');c$checked<-c$now;c$probe_fail<-TRUE
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 expect_equal(boe_mpr_maintain(c$dir,now=c$now)$status,'failed')
 c$rows<-c$rows[c$rows$edition=='2034-06',,drop=FALSE];c$now<-c$now+3600
 x<-boe_mpr_maintain(c$dir,now=c$now,force=TRUE)
 state<-readRDS(file.path(c$dir,'MAINTENANCE.rds'))
 expect_equal(state$calendar$edition,'2034-06');expect_equal(x$next_release_edition,'2034-06')
 expect_length(x$errors,0L);expect_length(state$releases,0L);expect_length(state$history,1L)
})

test_that('release full-check budgets persist across midnight and changing metadata', {
 c<-mpr_maintenance_fixture('2034-03-16 20:05:00');c$checked<-c$now;c$ready<-TRUE;c$ingest<-FALSE
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 start<-c$now
 for(i in 0:3){c$now<-start+i*3600;c$fp<-paste0('asset-',i);boe_mpr_maintain(c$dir,now=c$now)}
 expect_equal(c$updates,4L)
 c$now<-start+4*3600;c$fp<-'asset-after-midnight';boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,4L)
 state<-readRDS(file.path(c$dir,'MAINTENANCE.rds'));expect_length(state$releases[['2034-03']]$attempts,4L)
 expect_gte(as.numeric(state$releases[['2034-03']]$next_probe_at),as.numeric(start+86400))
 c$now<-start+86400;boe_mpr_maintain(c$dir,now=c$now);expect_equal(c$updates,5L)
})

test_that('a live maintenance lock returns waiting without loading data or changing state', {
 c<-mpr_maintenance_fixture()
 lock<-readboe:::boe_mpr_lock(c$dir,name='.maintenance-lock')
 on.exit({readboe:::boe_mpr_unlock(lock);unlink(c$dir,recursive=TRUE)},add=TRUE)
 files<-list.files(c$dir,recursive=TRUE,full.names=TRUE,all.files=TRUE);before<-tools::md5sum(files)
 local_mocked_bindings(boe_mpr_calendar=function(...)stop('network forbidden'),boe_mpr_maintenance_data=function(...)stop('data load forbidden'),boe_mpr_maintenance_write=function(...)stop('write forbidden'),.package='readboe')
 x<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(x$status,'waiting');expect_equal(x$outcome,'busy');expect_identical(tools::md5sum(files),before)
})

test_that('the updater keeps dated unpublished placeholders scheduled while revising history', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 c$set_edition(value=4);c$set_edition('2034-06',7,'2034-06-15')
 landing<-c$releases$landing_url[c$releases$release=='2034-06']
 c$bodies[[landing]]<-charToRaw('<html><main><h1>Monetary Policy Report - June 2034 (to be published)</h1><a href="https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/unreleased.zip">Data</a></main></html>')
 perform<-function(req){if(grepl('unreleased.zip',req$url,fixed=TRUE))stop('Unpublished asset must not be downloaded');c$perform(req)}
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 x<-boe_mpr_update(c$data_dir,force=TRUE)
 expect_equal(x$status,'updated');expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$value,c(4,5))
 r<-boe_mpr_releases(include_upcoming=TRUE,as_of=as.Date('2040-01-01'),data_dir=c$data_dir)
 expect_equal(r$publication_status[r$release=='2034-06'],'scheduled')
 # Upgrade/304 case: old state did not record the pending flag.
 path<-file.path(c$data_dir,'SOURCES.rds');state<-readRDS(path);state$sources[[landing]]$publication_pending<-NULL;saveRDS(state,path)
 again<-readboe:::boe_mpr_update_run(c$data_dir,bypass_daily=TRUE)
 expect_equal(again$status,'unchanged')
 expect_equal(boe_mpr_releases(include_upcoming=TRUE,as_of=as.Date('2040-01-01'),data_dir=c$data_dir)$publication_status[2],'scheduled')
})

test_that('a bypassed same-day update retains conditional probes instead of forcing downloads', {
 c<-saved_fixture();on.exit(unlink(c(c$bundle_dir,c$data_dir),recursive=TRUE),add=TRUE)
 headers<-list();perform<-function(req){headers[[length(headers)+1L]]<<-req$headers;c$perform(req)}
 local_mocked_bindings(boe_mpr_snapshot_paths=c$paths,boe_mpr_parser_fingerprint=c$fingerprint,boe_perform=perform,boe_mpr_discover_live=function(...)c$releases,.package='readboe')
 boe_mpr_update(c$data_dir);c$set_edition('2034-06',7,'2034-06-15')
 expect_equal(boe_mpr_update(c$data_dir)$status,'throttled')
 headers<-list();x<-readboe:::boe_mpr_update_run(c$data_dir,bypass_daily=TRUE)
 expect_equal(x$status,'updated');expect_true(any(vapply(headers,function(h)!is.na(readboe:::boe_header(h,'If-None-Match')),logical(1L))))
 expect_equal(read_boe_mpr_forecasts('latest',data_dir=c$data_dir)$value,8)
})

test_that('a killed maintenance process keeps its retry journal and does not scan again next hour', {
 skip_on_os('windows');c<-mpr_maintenance_fixture('2034-03-16 12:05:00');c$ready<-TRUE
 on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 child<-parallel::mcparallel({
  testthat::local_mocked_bindings(boe_mpr_maintenance_update=function(...){tools::pskill(Sys.getpid(),signal=9L);Sys.sleep(2);stop('kill unavailable')},.package='readboe')
  boe_mpr_maintain(c$dir,now=c$now)
 })
 suppressWarnings(parallel::mccollect(child))
 journal<-readRDS(file.path(c$dir,'MAINTENANCE.rds'));expect_equal(journal$outcome,'updater_in_progress');expect_true(journal$data_initialized)
 Sys.setFileTime(file.path(c$dir,'.maintenance-lock'),Sys.time()-120)
 c$now<-c$now+3600;x<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,0L);expect_equal(x$status,'failed');expect_match(x$error,'did not complete')
 c$now<-c$now+5*3600;x<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(c$updates,1L);expect_equal(x$status,'success')
})

test_that('corrupt maintenance metadata falls back and repairs without losing calendar or data', {
 c<-mpr_maintenance_fixture();on.exit(unlink(c$dir,recursive=TRUE),add=TRUE)
 local_mocked_bindings(boe_mpr_calendar=c$calendar,boe_mpr_maintenance_data=c$data,boe_mpr_maintenance_probe=c$probe,boe_mpr_maintenance_update=c$update,.package='readboe')
 boe_mpr_maintain(c$dir,now=c$now);boe_mpr_maintain(c$dir,now=c$now,force=TRUE)
 writeBin(charToRaw('corrupt'),file.path(c$dir,'MAINTENANCE.rds'))
 expect_equal(boe_mpr_maintenance_status(c$dir,now=c$now)$status,'failed')
 x<-boe_mpr_maintain(c$dir,now=c$now)
 expect_equal(x$status,'success');expect_equal(x$next_release_edition,'2034-03');expect_equal(c$updates,0L)
})

test_that('maintenance CLI prints machine-readable status without activating a schedule', {
 script<-system.file('scripts','update-mpr-forecasts.R',package='readboe');path<-tempfile()
 environment<-new.env(parent=globalenv());environment$commandArgs<-function(...)c('--maintain','--json','--retain-raw','--data-dir',path)
 captured<-NULL;retained<-NULL;environment$boe_mpr_maintain<-function(data_dir,...,retain_raw){captured<<-data_dir;retained<<-retain_raw;list(status='idle',next_due_at='2034-03-16T12:00:00Z',next_release_edition='2034-03',errors=list())}
 old<-Sys.getenv('GITHUB_STEP_SUMMARY',unset=NA_character_);on.exit(if(is.na(old))Sys.unsetenv('GITHUB_STEP_SUMMARY')else Sys.setenv(GITHUB_STEP_SUMMARY=old),add=TRUE);Sys.setenv(GITHUB_STEP_SUMMARY='')
 output<-capture.output(sys.source(script,envir=environment));x<-jsonlite::fromJSON(paste(output,collapse='\n'))
 expect_identical(captured,path);expect_equal(x$next_due_at,'2034-03-16T12:00:00Z');expect_false(dir.exists(path));expect_true(retained)
})
