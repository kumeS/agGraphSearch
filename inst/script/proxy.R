if(T){
proxy_url = "http://wwwproxy.osakac.ac.jp:8080"
Sys.setenv("http_proxy" = proxy_url)
Sys.setenv("https_proxy" = proxy_url)
Sys.setenv("ftp_proxy" = proxy_url)
}

devtools::install_github( "kumeS/agGraphSearch" )
library(agGraphSearch)

