# Synthetic calendar and a controllable maintenance backend; no external writes.
mpr_calendar_fixture <- function(dates = c('2034-03-16','2034-06-15'), clocks = rep('12pm',length(dates)), duplicate = FALSE) {
 dates<-as.Date(dates);keys<-format(dates,'%Y-%m')
 urls<-paste0('https://www.bankofengland.co.uk/monetary-policy-report/',format(dates,'%Y'),'/',tolower(month.name[as.integer(format(dates,'%m'))]),'-',format(dates,'%Y'))
 html<-'<html><main><a href="/monetary-policy-report/2099/january-2099">Outside calendar</a>'
 for(year in unique(format(dates,'%Y'))){
  html<-paste0(html,'<section><h2>',year,' confirmed dates</h2><table><tbody>')
  for(i in which(format(dates,'%Y')==year)){
   label<-paste(format(dates[i],'%d'),month.name[as.integer(format(dates[i],'%m'))])
   link<-paste0('<a href="',urls[i],'">MPR</a>')
   html<-paste0(html,'<tr><td>Thursday ',label,'</td><td>',link,if(duplicate)link else'','</td></tr>')
  }
  html<-paste0(html,'<tr><td>Thursday 1 December</td><td><a href="/minutes">Minutes only</a></td></tr></tbody></table></section>')
 }
 html<-paste0(html,'</main></html>');listing<-'<html>'
 for(i in seq_along(dates)){
  label<-paste0('Monetary Policy Report - ',keys[i],if(!is.na(clocks[i]))paste0(' (to be published at ',clocks[i],')')else'')
  listing<-paste0(listing,'<a href="',urls[i],'"><time datetime="',dates[i],'"></time><h3>Short title</h3><h3>',label,'</h3></a>')
 }
 entry<-function(body,url){body<-charToRaw(body);list(url=url,final_url=url,body=body,status=200L,raw_sha256=readboe:::boe_sha256_raw(body),retrieved_at=Sys.time(),etag=NA_character_,last_modified=NA_character_,from_cache=FALSE)}
 list(calendar=entry(html,readboe:::.boe_mpr_calendar_url),times=entry(paste0(listing,'</html>'),readboe:::.boe_mpr_times_url),keys=keys,urls=urls)
}
mpr_maintenance_fixture <- function(now = '2034-03-16 11:50:00') {
 c<-new.env(parent=emptyenv());c$now<-as.POSIXct(now,tz='UTC');c$dir<-tempfile('mpr-maintain-')
 f<-mpr_calendar_fixture();c$rows<-readboe:::boe_mpr_parse_calendar(f$calendar,f$times,c$now)
 c$editions<-'2033-11';c$checked<-c$now-3600;c$sha<-strrep('a',64)
 c$calendar_calls<-c$updates<-c$probes<-0L;c$bypass<-logical();c$ready<-FALSE;c$fp<-'metadata-one';c$revision<-NULL
 c$calendar_fail<-c$probe_fail<-c$update_fail<-FALSE;c$ingest<-TRUE
 c$calendar<-function(...){c$calendar_calls<-c$calendar_calls+1L;if(c$calendar_fail)stop('calendar unavailable');c$rows}
 c$data<-function(...)list(editions=c$editions,snapshot_sha256=c$sha,checked_at=c$checked)
 c$probe<-function(...){c$probes<-c$probes+1L;if(c$probe_fail)stop('probe unavailable');list(ready=c$ready,fingerprint=c$fp,revision=c$revision,receipt=list(url='https://www.bankofengland.co.uk/probe',raw_sha256=strrep('b',64)),assets=as.integer(c$ready))}
 c$update<-function(data_dir,deadline,bypass_daily,retain_raw=FALSE){
  c$retain_raw<-retain_raw
  c$updates<-c$updates+1L;c$bypass<-c(c$bypass,bypass_daily)
  if(c$update_fail)return(list(status='failed',error='update unavailable',error_class='readboe_transport'))
  before<-c$editions
  if(c$ingest&&c$ready)c$editions<-union(c$editions,c$rows$edition[c$rows$due_at<=c$now])
  c$checked<-c$now
  list(status=if(identical(before,c$editions))'unchanged'else'updated',last_successful_check=c$checked)
 }
 c
}
