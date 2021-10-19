##' @title Proxy settings
##'
##' @param proxy_url a character vector of proxy URI.
##'
##' @description This function provides the proxy settings.
##'
##' @author Satoshi Kume
##' @export ProxySet
##'
##' @examples \dontrun{
##'
##' proxy_url = "http://wwwproxy.osakac.ac.jp:8080"
##' ProxySet(proxy_url)
##'
##' }
##'

ProxySet <- function(proxy_url){
Sys.setenv("http_proxy" = proxy_url)
Sys.setenv("https_proxy" = proxy_url)
Sys.setenv("ftp_proxy" = proxy_url)
}

