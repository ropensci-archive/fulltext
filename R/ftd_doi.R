#' DOI
#' 
#' @export
#' @param doi (character) one or more DOIs. required
#' @param ... curl options passed on to [crul::verb-GET]
#' @return data.frame of rows equal to number of DOIs supplied, with columns:
#' 
#' - doi: the doi
#' - url: url for the article
#' - content_type: content type of the article format
#' - issn: ISSN for the journal containing the DOI
#' - member_name: Crossref member name
#' - member_url: Crossref member url
#' 
#' @family ftdoi
#' @examples
#' \dontrun{
#' # pensoft
#' ftd_doi(doi = '10.3897/zookeys.594.8768')
#' ftd_doi(doi = '10.3897/mycokeys.54.34571')
#' ftd_doi(doi = '10.3897/phytokeys.99.26489')
#' ftd_doi(doi = '10.3897/subtbiol.13.6719')
#' # plos
#' ftd_doi(doi = '10.1371/journal.pgen.1006546')
#' ftd_doi(c('10.1371/journal.pgen.1006546', '10.1371/journal.pbio.1001809'))
#' # mdpi
#' ftd_doi('10.3390/ani4010082')
#' ftd_doi(doi = c('10.3390/ani4010082', "10.3390/ijms19040965",
#'   "10.3390/rs9010083"))
#' # frontiers
#' ftd_doi(doi = '10.3389/fmed.2015.00081')
#' # informa
#' ftd_doi(doi = '10.4324/9780203538333')
#' # thieme
#' ftd_doi(doi = '10.1055/s-0042-103414')
#' # peerj
#' ftd_doi(doi = '10.7717/peerj.991')
#' ftd_doi(doi = '10.7717/peerj-cs.39')
#' # American Phyiscal Society
#' ftd_doi(doi = '10.1103/physreve.68.067402')
#' # Royal Society of Chemistry
#' ftd_doi(doi = '10.1039/c4ra04415k')
#' # Karger
#' ftd_doi(doi = '10.1159/000360225')
#' ftd_doi(doi = c("10.1159/000094345","10.1159/000086754"))
#' # Trans Tech Publications
#' ftd_doi(doi = '10.4028/www.scientific.net/msf.702-703.774')
#' # Emerald
#' ftd_doi(doi = '10.1108/00251740210413370')
#' # mixed publishers
#' ftd_doi(doi = c("10.1371/journal.pgen.1006546","10.1159/000086754"))
#' # Pleiades
#' ftd_doi(doi = '10.1134/s1063784215120075')
#' # Instituto de Investigaciones Filologicas
#' ftd_doi(doi = '10.1016/s0185-3082(14)70398-0')
#' ftd_doi(c('10.1016/s0185-2574(13)71376-5', '10.19130/iifl.nt.1997.15.0.650'))
#' # Sage
#' ftd_doi(doi = '10.1177/0267659117690248')
#' ftd_doi('10.1177/002193470003000403')
#' # SPIE
#' ftd_doi(c("10.1117/12.59493", "10.1117/12.460027",
#'   "10.1117/1.jei.27.3.033002"))
#' # PNAS
#' ftd_doi(c("10.1073/pnas.93.19.10405", "10.1073/pnas.88.4.1182",
#'   "10.1073/pnas.87.24.9794"))
#' # Springer
#' ftd_doi("10.1007/s10107-017-1136-5")
#' ftd_doi(c("10.1007/s10107-017-1136-5", "10.1007/978-94-017-8625-6",
#'   "10.1016/s0952-8733(00)00008-8"))
#' # American Society of Clinical Oncology
#' ftd_doi(c("10.1200/JCO.20.01121", "10.1200/JCO.19.02959",
#'   "10.1200/JCO.20.01002"))
#' # AIP: American Institute of Physics
#' ftd_doi(c("10.1063/1.5046187", "10.1063/1.4973652", "10.1063/1.5080806"))
#' # ACS
#' ftd_doi(c("10.1021/am508843z", "10.1021/acs.analchem.8b05115",
#'   "10.1021/acs.jchemed.5b00997"))
#' # The Royal Society
#' ftd_doi(c("10.1098/rspa.2007.1849", "10.1098/rstb.1970.0037",
#'   "10.1098/rsif.2006.0142"))
#' # Company of Biologists
#' ftd_doi("10.1242/jeb.00137")
#' ftd_doi(c("10.1242/dev.00905", "10.1242/dev.00915"))
#' ftd_doi("10.1242/bio.042192")
#' # Hindawi
#' ftd_doi("10.1155/2017/4724852")
#' ftd_doi("10.1155/2020/6914878")
#' # IOP
#' ftd_doi("10.1088/2043-6262/7/2/025018")
#' # AAAS
#' # z <- rcrossref::cr_members(221, works=TRUE)
#' # dois <- z$data$doi
#' # ftd_doi(dois[12:20])
#' # ftd_doi(dois[2])
#' ftd_doi("10.1126/science.276.5312.548")
#' # Oxford
#' # z <- rcrossref::cr_members(286, works=TRUE)
#' # dois <- z$data$doi
#' # ftd_doi(dois[1:5])
#' ftd_doi("10.1016/s0895-7061(01)02279-8")
#' # CDC
#' # z <- rcrossref::cr_members(1822, works=TRUE)
#' # dois <- z$data$doi
#' # ftd_doi(dois[1:5])
#' ftd_doi("10.3201/eid1209.051606")
#' # Elsevier
#' ## a cc-by3 paper
#' ftd_doi(doi="10.1016/j.jsamd.2019.02.002")
#' ftd_doi(c("10.1016/j.nuclphysbps.2015.09.127", "10.1016/j.nuclphysb.2011.09.011", 
#'  "10.1016/j.eurpolymj.2018.07.009", "10.1016/j.jsamd.2019.02.002",
#'  "10.1016/j.physletb.2015.11.072"))
#' # American Society for Microbiology
#' ftd_doi(doi="10.1128/jcm.39.12.4344-4348.2001")
#' ftd_doi(c("10.1128/jcm.42.6.2623-2628.2004",
#'   "10.1128/jcm.42.9.4147-4153.2004",
#'   "10.1128/jcm.40.10.3826-3830.2002", 
#'   "10.1128/jcm.41.3.943-947.2003"))
#' ## some DOIs we just can't easily make URLs for, returns NA
#' ftd_doi(c("10.1128/mcb.11.10.4966", "10.1128/cmr.7.1.14"))
#' # Walter de Gruyter
#' ftd_doi(doi="10.1515/geo-2020-0173")
#' ftd_doi(doi="10.1515/ci.2013.35.2.19b")
#' ftd_doi(c("10.1515/geo-2020-0173", "10.1515/ci.2013.35.2.bm", 
#'   "10.2478/jvetres-2020-0058", "10.2478/acmy-2020-0008"))
#' # Biorxiv
#' ftd_doi(doi='10.1101/012476')
#' }
ftd_doi <- function(doi, ...) {
  assert(doi, "character")
  check_dois(doi)
  if (!has_patterns()) suppressMessages(ftd_fetch_patterns())
  rbl(lapply(doi, function(d) {
    df <- data.frame(foo=NA)
    w <- prefix_get(d)
    if (inherits(w, "error")) w <- prefix_local(d)
    have_pattern(w$member)
    if (
      (!key_exists(w) && no_http_needed(w)) ||
      (
        (if ("member" %in% names(w)) w$member %in% members_need_crossref) %||% FALSE &&
        !url_exists(d)
      ) ||
      (
        (if ("member" %in% names(w)) w$member %in% members_need_issn) %||% FALSE &&
        !doi_issn_exists(d)
      )
    ) {
      res <- rcrossref::cr_works(d)
      res$data <- data.frame(res$data)
      prefix_update(d, res$data)
      df <- res$data[tolower(res$data$doi) == tolower(d), ]
      w <- prefix_get(d)
      # member=w$member; prefix=w$prefix; issn=df$issn; res=df
    }
    if (url_exists(d)) {
      zz <- url_get(d)
      if (w$member %in% members_sim_check)
        df <- list(link = list(data.frame(intended.application="similarity-checking",
          URL=zz[[1]]$url, `content-type`=zz[[1]]$content_type,
          stringsAsFactors = FALSE)))
      else
        df <- list(link = list(
          data.frame(
            URL=pluck(zz, "url"), `content-type`=pluck(zz, "content_type"),
            stringsAsFactors = FALSE
          )
        ))
    }
    # z <- tryCatch(pattern_member(d, w$member, df$issn, df), error = function(e) e)
    z <- pattern_member(d, w$member, df$issn, df)
    if (inherits(z, "error")) empty_df(d) else z
  }))
}

empty_df <- function(d) {
  na_cr <- NA_character_
  data.frame(doi = d, url = na_cr, content_type = na_cr, issn = na_cr,
    member_name = na_cr, member_url = na_cr,
    stringsAsFactors = FALSE)
}
