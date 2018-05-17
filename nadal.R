library(mcpstats)

url <- "http://www.tennisabstract.com/charting/meta.html"

player_urls <- read_html(url) %>%
	html_nodes("a") %>%
	html_attr("href") 
	
	
player_urls <- player_urls[grep("Nadal", ignore = T, player_urls)]

# limit to 2009+, clay events
player_urls <- player_urls[grep("Madrid|Monte|Barcelona", ignore = T, player_urls)]
player_urls <- grep("2009|201", player_urls, val = T)
player_urls <- unique(player_urls)

urls <- sapply(player_urls, function(x) paste("http://www.tennisabstract.com/charting/", x, sep = ""))

return <- do.call("rbind", lapply(urls, return_stats, player = "RN"))

return_detail <- do.call("rbind", lapply(urls, return_detail_stats, player = "RN"))

save(return, return_detail, file = "~/Desktop/nadal_return_mcp.RData")


url <- "http://www.tennisabstract.com/charting/20180511-M-Madrid_Masters-QF-Rafael_Nadal-Dominic_Thiem.html"

serve <- serve_stats(url, "RN", "Total")
serve1 <- serve_stats(url, "RN", "1st")
serve2 <- serve_stats(url, "RN", "2nd")


serve_detail <- serve_detail_stats(url, "RN", "Total")
serve1_detail <- serve_detail_stats(url, "RN", "1st")
serve2_detail <- serve_detail_stats(url, "RN", "2nd")

return <- return_stats(url, "RN")
return_detail <- return_detail_stats(url, "RN")

save(list= objects(), file = "~/Desktop/nadal_mcp_thiem_madrid.RData")