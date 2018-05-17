#' Scrape Detailed Return Stats
#'
#'
#' @param url Match url 
#' @param player Two-letter initial of player
#'
#' @export
return_detail_stats <- function(url, player){
	
	print(url)
	
	overall_return <- function(x, player = "RN"){
	
	if(length(x[[1]]) == 0)
		NA
	else{
		clean <- function(x) gsub("(^[0-9]+)( .*)", "\\1", x)
		
		if(names(x[[1]][[1]])[1] == paste(player, "OUTCOMES"))
			result <- x[[1]][[2]]
		else
			result <- x[[2]][[2]]
		result <- result %>% filter(!is.na(Returnable))
		
		result[,-1] <- result[,-1] %>%
			mutate_all(funs(clean))
			
	 result
	 }
	
	}	

	overall_pre <- function(x, player = "RN"){
		if(x[[1]][[1]][1] == player)
			result <- x[[1]]
		else
			result <- x[[2]]

		
		first <- sapply(result, function(x) x[1])
		
		result <- result[max(which(first == player)):length(result)]
		
		result[[1]][1] <- paste(result[[1]][1:2], collapse = " ")
		result[[1]][7] <- paste(result[[1]][6:7], collapse = " ")
		result[[1]] <- result[[1]][-c(2, 6)]
		
		result[-1] <- lapply(result[-1], function(x){
			
			if(grepl("[A-Za-z]", x[2])){
				x[1] <- paste(x[1:2], collapse = " ")
				x <- x[-2]
			}
			
			if(grepl("[A-Za-z]", x[2])){
				x[1] <- paste(x[1:2], collapse = " ")
				x <- x[-2]
			}			
			
			if(grepl("[A-Za-z]", x[2]) & !grepl("DEPTH", x[1])){
				x[1] <- paste(x[1:2], collapse = " ")
				x <- x[-2]
			}
			
						
			x
		})
						
		result <- do.call("rbind", result)
		
		cnames <- result[1,]
		
		result <- as.data.frame(result[-1,], stringsAsFactors = F) %>%
			mutate_at(vars(V2:V10), as.numeric)	
			
		colnames(result) <- cnames

	result
	}
	
	lines <- readLines(url)

	return <- lines[grep("var.return", lines)]


	if(!grepl("<table", return[1])){ # Old format
		return <- gsub("(.*pre>)(.*)(pre>.*)", "\\2", return[1:2])
	
		tables <- lapply(return, function(x){
			table <- strsplit(x, "\\n", fixed = T)[[1]]
			table <- table[grep("^[a-zA-Z][a-zA-Z]", table)]
			result <- strsplit(table, " ")
			result <- sapply(result, function(x) x[!grepl("\\(", x) & x != ""])
		result
		})
		
		result <- overall_pre(tables, player)
	}
	else{
		return <- gsub("(.*\\'<pre>)(.*)(</pre>\\'.*)", "\\2", return[1:2])
	
		tables <- lapply(return, function(x){
			read_html(x) %>%
			html_nodes("table") %>%
			html_table()
		})
		
		result <- overall_return(tables, player)
	}
	
	
	names(result) <- gsub("-+%", "", names(result))

	matchid <- sub("(.*charting.)([0-9].*)", "\\2", url)
	
	result$matchid <- matchid
	
	matchid_split <- strsplit(matchid, "-")[[1]]
	
	result$year <- as.numeric(substr(matchid, 1, 4))

	result$round <- matchid_split[4]
	result$event <- matchid_split[3]
	result$opponent <- matchid_split[5]
	

result
}