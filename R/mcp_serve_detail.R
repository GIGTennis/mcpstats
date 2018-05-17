#' Scrape Detailed Serve Stats
#'
#'
#' @param url Match url 
#' @param player Two-letter initial of player
#' @param level One of 'Total', '1st', or '2nd'
#'
#' @export
serve_detail_stats <- function(url, player, level){
	
	print(url)
	
	overall_serve <- function(x, player = "RN 1st"){
	
	if(length(x[[1]]) == 0)
		NA
	else{
		clean <- function(x) gsub("(^[0-9]+)( .*)", "\\1", x)
        result <- x[[1]][[2]] # Direction stats
		result <- result[result$`DIRECTION` == player,]
		result[1,] <- clean(result[1,])
		names(result)[names(result) == ""] <- "<=3W---%"
	 result[,-1]
	}
	
	}	

	overall_pre <- function(x, player = "RN", level = "1st"){
		result <- do.call("rbind", x[[1]][9:14])
		result <- as.numeric(result[result[,1] == player & result[,2] == level, -(1:2), drop = F])
		names(result) <- x[[1]][[8]][-1]
	result
	}
	
	lines <- readLines(url)

	serve <- lines[grep("var.serve", lines)]


	if(grepl("<pre>", serve[1])){ # Old format
		serve <- gsub("(.*pre>)(.*)(pre>.*)", "\\2", serve[1:3])
	
		tables <- lapply(serve, function(x){
			table <- strsplit(x, "\\n", fixed = T)[[1]]
			table <- table[grep("^[A-Z][A-Z]", table)]
			result <- strsplit(table, " ")
			result <- sapply(result, function(x) x[!grepl("\\(", x) & x != ""])
		result
		})
		
		result <- overall_pre(tables, player, level)
	}
	else{
		serve <- gsub("(.*\\')(.*)(\\'.*)", "\\2", serve[1:3])
	
		tables <- lapply(serve, function(x){
			read_html(x) %>%
			html_nodes("table") %>%
			html_table()
		})
		
		result <- overall_serve(tables, paste(player, level))
	}
	
	if(class(result) == "numeric")
		result <- as.data.frame(t(result), row.names = NULL)
	
	names(result) <- gsub("---%", "", names(result))

	matchid <- sub("(.*charting.)([0-9].*)", "\\2", url)
	
	result$matchid <- matchid
	
	matchid_split <- strsplit(matchid, "-")[[1]]
	
	result$year <- as.numeric(substr(matchid, 1, 4))

	result$round <- matchid_split[4]
	result$event <- matchid_split[3]
	result$opponent <- matchid_split[5]
	

result
}