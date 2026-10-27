saved_fixture <- function() {
  skip_if_not_installed('openxlsx')
  context <- new.env(parent=emptyenv());context$releases<-data.frame(release='2034-03',release_date=as.Date('2034-03-12'),
    release_date_precision='day',release_period_start=as.Date('2034-03-01'),source_published_date=as.Date('2034-03-12'),
    family='monetary_policy_report',publication_status='published',landing_url='https://www.bankofengland.co.uk/monetary-policy-report/2034/march-2034',date_source='fixture',label='Report')
  context$bodies<-list();context$assets<-list();context$requests<-0L
  context$set_edition<-function(key='2034-03',value=2,date='2034-03-12',provider=FALSE){
    if(!key%in%context$releases$release){r<-context$releases[1,];r$release<-key;r$release_date<-r$source_published_date<-as.Date(date);r$release_period_start<-as.Date(paste0(key,'-01'));r$landing_url<-paste0('https://www.bankofengland.co.uk/monetary-policy-report/',substr(key,1,4),'/',tolower(month.name[as.integer(substr(key,6,7))]),'-',substr(key,1,4));context$releases<-rbind(context$releases,r)}
    rel<-context$releases[context$releases$release==key,,drop=FALSE]
    url<-paste0('https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy-report/',key,'-forecast.xlsx')
    x<-matrix(NA_character_,6,3);x[1:3,1]<-c('CPI inflation','Per cent','Four-quarter percentage change')
    year<-substr(key,1,4);x[5,]<-c('Date of publication',paste(year,'Q1'),paste(year,'Q2'));x[6,]<-c(paste(month.name[as.integer(substr(key,6,7))],year),as.character(value),as.character(value+1))
    path<-tempfile(fileext='.xlsx');openxlsx::write.xlsx(list(Forecast=as.data.frame(x)),path,colNames=FALSE)
    body<-readBin(path,'raw',n=file.info(path)$size);unlink(path)
    context$bodies[[url]]<-body
    page<-charToRaw(paste0('<html><a href="',url,'">Forecast data</a></html>'))
    context$bodies[[rel$landing_url]]<-page
    entry<-list(url=rel$landing_url,body=page,raw_sha256=readboe:::boe_sha256_raw(page),retrieved_at=Sys.time(),status=200L,etag=readboe:::boe_sha256_raw(page),last_modified=NA_character_)
    context$assets[[key]]<-readboe:::boe_mpr_assets_from_entry(rel,entry)
    invisible(url)
  }
  context$set_edition()
  fingerprint <- readboe:::boe_mpr_parser_fingerprint();context$fingerprint<-function()fingerprint
  context$perform<-function(req){context$requests<-context$requests+1L;body<-context$bodies[[req$url]];if(is.null(body))return(httr2::response(404,url=req$url));etag<-readboe:::boe_sha256_raw(body);header<-readboe:::boe_header(req$headers,'If-None-Match');if(!is.na(header)&&identical(header,etag))httr2::response(304,url=req$url,headers=list(etag=etag)) else httr2::response(200,url=req$url,body=body,headers=list(etag=etag))}
  context$fetch<-function(url){body<-context$bodies[[url]];receipt<-list(url=url,raw_bytes=length(body),raw_sha256=readboe:::boe_sha256_raw(body),retrieved_at=Sys.time(),etag=readboe:::boe_sha256_raw(body),last_modified=NA_character_);readboe:::boe_attach_provenance(body,receipt,list(surface='fixture',url=url))}
  record<-readboe:::boe_mpr_build_record('2034-03',context$releases,context$assets,context$fetch,tempdir(),boe_deadline(30))
  sources<-readboe:::boe_mpr_sources_from_records(list(record));snapshot<-readboe:::boe_mpr_pack_snapshot(context$releases,list(`2034-03`=record),Sys.time(),sources)
  base<-tempfile('mpr-bundle-');dir.create(base);context$bundle_dir<-base
  saveRDS(snapshot,file.path(base,'mpr-forecasts.rds'),compress='xz')
  saveRDS(list(schema=1L,sha256=readboe:::boe_sha256_file(file.path(base,'mpr-forecasts.rds'))),file.path(base,'mpr-forecasts-manifest.rds'))
  context$data_dir<-tempfile('mpr-data-');context$paths<-function()c(data=file.path(base,'mpr-forecasts.rds'),manifest=file.path(base,'mpr-forecasts-manifest.rds'))
  context
}
