library(stringr)
library(XML)
library(curl)

split_block <- function (list_of_strings_solid) {
  current_name = ""
  current_block = ""
  my_list = list()
  list_of_strings <- unlist(strsplit(list_of_strings_solid,"\n"))
  # XML example class notes -- different HTML features
  for (i in 1:length(list_of_strings)) {
    if (((toupper(list_of_strings[[i]]) == list_of_strings[[i]]) &&
         !grepl("^\\([A-Z]+\\)$",list_of_strings[[i]]))) {
      # This is a "caps line" which doesn't contain useful information about
      # the text of the debate.  Mostly filler
      next
    }
    if (grepl("END",list_of_strings[[i]])) { break }
    name <- str_match(list_of_strings[[i]],regex("([A-Z]+):"))[,2]
    if ((!is.na(name)) && name != current_name) {
      if (current_name != "") {
        my_list[[current_name]] <- c(my_list[[current_name]],
        str_replace_all(current_block,paste(current_name,": ",sep=""),
        ""))
      }
      current_name <- name
      current_block <- ""
      # ... and maybe add a new block to the list?
    }
    if (length(current_name) != 0) {
      if (current_block != "") {
        current_block <- paste(current_block,
                               list_of_strings[[i]], sep=" ")
      }
      else {
        current_block <- list_of_strings[[i]]
      }
    }
  }
  my_list[[current_name]] <- c(my_list[[current_name]], 
  str_replace_all(current_block,
                  paste(current_name,": ",sep=""),""))
  return(my_list)
}

create_debate_text <- function(file_url)
{
  xml_handle <- htmlParse(file_url)
  v <- xpathSApply(xml_handle,
                   "//div[@id = 'content-sm']",xmlValue)
  text_data <- lapply(v,str_replace_all,
                      "([A-Z]+:)","\n\n\\1")
  return(text_data[[1]])
}

debate_summary <- function(file_url)
{
text_data <- create_debate_text(file_url)

events = list()
debate_blocks <- split_block(text_data)
for (n in names(debate_blocks))
{
   events[[n]] <- table(
     str_extract_all(
       paste(debate_blocks[[n]],collapse=" ")
       ,"\\([A-Za-z]+\\)"))
   debate_blocks[[n]] <- lapply(debate_blocks[[n]],
                                str_replace_all,
                                "\\([A-Za-z]+\\)","")
}

words = list()
sentence = list()
word_counts = list()
patterns = c("I[^a-zA-Z]", "we[^a-zA-Z]", "America(n)?[^a-zA-Z]", 
             "democra(cy|tic)[^a-zA-Z]", "republic[^a-zA-Z]",
             "Democrat(ic)?[^a-zA-Z]","Republican[^a-zA-Z]",
             "free(dom)?[^a-zA-Z]", "war[^a-zA-Z]",
             "God(?! bless)[^a-zA-Z]",
             "(Jesus|Christ|Christian)[^a-zA-Z]",
             "God bless[^a-zA-Z]")
for (n in names(debate_blocks))
{
  to_analyze <- paste(debate_blocks[[n]],collapse=" ")
  for (pattern in patterns)
  {
    word_counts[[n]][[pattern]] <- str_count(to_analyze,pattern)
  }
  names(word_counts[[n]]) <- c("I","we","America{n}",
                               "democra{cy,tic}","republic",
                               "Democrat{,ic}","Republican",
                               "free{,dom}","war",
                               "God (only)","God Bless",
                               "{Jesus, Christ, Christian}")
  # print(word_counts)
  words[[n]] <- str_extract_all(to_analyze,
'(([:alpha:]+(\'([:alpha:]+)?)?)|([:digit:]+(,([:digit:]+)?)?))')
  sentence[[n]] <- str_extract_all(to_analyze,
"([:alpha:])(([:alpha:]|[:space:]|[:digit:]|\'|,|-)*)(\\.|\\?|\\!)")
  print(paste("The average word length of ", n,"'s speach is:",sep = ""))
  print(mean(rapply(words[[n]],nchar)))
  print(paste("The number of characters (in the words) in ",
              n,"'s speach is:",sep = ""))
  print(sum(rapply(words[[n]],nchar)))
  print(paste("The number of words in ", n,"'s speach is:",
              sep = ""))
  print(length(unlist(words[[n]])))
  print(paste("The buzzwords in ", n,"'s speach is:",
              sep = ""))
  print(word_counts[[n]])
  print(paste("Event occurences in ", n,"'s speach is:",sep = ""))
  print(events[[n]])
}
return(debate_blocks)
}

menu_url="http://www.debates.org/index.php?page=debate-transcripts"
menu_xml_handle <- htmlParse(menu_url)
menu_nodes <- getNodeSet(menu_xml_handle,"//a[@href]")
all_debate_links <- xpathSApply(menu_xml_handle, "//a[@href]", xmlGetAttr, 'href')
years <- c("2012","2008","2004","2000","1996")

year_reg <- paste("(",paste(paste(years,collapse="|"),").+(First)",sep=""),sep="")

my_debate_links <- all_debate_links[grepl(
  year_reg,
  sapply(menu_nodes,xmlValue))]
debate_blocks_list = list()
i <- 1
for (year in years)
{
  print(paste("The statistics for the first debate in",year,"..."))
  debate_blocks_list[[year]] <- debate_summary(my_debate_links[i])
  i <- i + 1
  cat("\n\n")
}

# debate_blocks <- lapply(my_debate_links,debate_summary)
