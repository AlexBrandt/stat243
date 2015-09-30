library(stringr)
library(XML)
library(curl)
library(RCurl)

# Create debate takes a url and extracts all of the 
# text from between the dev[id = 'content-sm'] tags
# and then splits the block of text on the speaker
# information (here given as "SPEAKER: ").  Note it 
# preserves the speaker's name using the \\1 value.
create_debate_text <- function(file_url)
{
  # Get rid of br tags for all non-2012 debates
  temp <- gsub("<br/>","\n",paste(readLines(curl(file_url),warn = FALSE),collapse=""))
  # Get ride of p tags for the 2012 debate
  temp <- gsub("<p>","\n",temp)
  temp <- gsub("<*p>","\n",temp)
  
  xml_handle <- htmlParse(temp, asText=TRUE)
  v <- xpathSApply(xml_handle,
                   "//div[@id = 'content-sm']",xmlValue)
  text_data <- lapply(v,str_replace_all,
                      "([A-Z]+:)","\n\n\\1")
  return(unlist(text_data))
}

# Split block takes a large block of text as a string
# (with "\n" still preserved).  It then skips any 
# pre-speaking information, like the location, etc.
# and it truncates the debate at the END (or until the
# text runs out).
split_block <- function (list_of_strings_solid) {
  current_name = ""
  current_block = ""
  my_list = list()
  # Breaks on newlines, preserves general format of the
  # webpage
  list_of_strings <- unlist(strsplit(list_of_strings_solid,"\n"))
  # XML example class notes -- different HTML features
  for (i in 1:length(list_of_strings)) {
    if (((toupper(list_of_strings[[i]]) == list_of_strings[[i]]) &&
         !grepl("^\\([A-Z]+\\)$",list_of_strings[[i]]))) {
      # This is a "caps line" which doesn't contain useful 
      # information about the text of the debate.  
      # Mostly filler.  Move to next UNLESS it is
      # of the form "(EVENT)" (i.e. "(APPLAUSE)")
      next
    }
    # Helps prevent bad formatting on the part of the website
    if (grepl("END",list_of_strings[[i]]) || grepl("Transcription",list_of_strings[[i]])) { break }
    # Uses the name of the speaker as the index
    name <- str_match(list_of_strings[[i]],regex("([A-Z]+):"))[,2]
    if ((!is.na(name)) && name != current_name) {
      if (current_name != "") {
        my_list[[current_name]] <- c(my_list[[current_name]],
        str_replace_all(current_block,paste(current_name,": ",sep=""),
        ""))
      }
      # Resets the name, resets the block
      current_name <- name
      current_block <- ""
      # ... and maybe add a new block to the list?
    }
    # If there is a name, append the speaking block to the list
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
  # Gets any "hanging" information within the block buffer
  my_list[[current_name]] <- c(my_list[[current_name]], 
  str_replace_all(current_block,
  paste(current_name,": ",sep=""),""))
  return(my_list)
}

# Constructs summary information about the debate based
# on a debate block
summary_stats <- function(debate_block)
{
   return_vals = list()
   # QUESTION 2C) gets "events" like (APPLAUSE) as they
   # occur, and saves the freqency of the event before
   # sanitizing the text
   return_vals$events <- table(
    str_extract_all(
      paste(debate_block,collapse=" "),
      "\\([A-Za-z]+\\)"))
   debate_block <- lapply(debate_block,
                               str_replace_all,
                               "\\([A-Za-z]+\\)","")

   # The various REGEX patterns we are interested in.
   # Note that I take capital letters to generally be an
   # unambiguous start to the word (vs. a word like "we"
   # which could show up as a suffix).  I assuemd that
   # "we" and "war" and "freedom" should be case insensitive
     patterns = c("I[^a-zA-Z]",
                  "[^a-zA-Z](W|w)e[^a-zA-Z]",
                  "America(n)?[^a-zA-Z]", 
                  "[^a-zA-Z]democra(cy|tic)[^a-zA-Z]",
                  "[^a-zA-Z]republic[^a-zA-Z]",
                  "Democrat(ic)?[^a-zA-Z]",
                  "Republican[^a-zA-Z]",
                  "[^a-zA-Z](F|f)ree(dom)?[^a-zA-Z]",
                  "[^a-zA-Z](W|w)ar[^a-zA-Z]",
                  "God(?! bless)[^a-zA-Z]",
                  "(Jesus|Christ|Christian)[^a-zA-Z]",
                  "God bless[^a-zA-Z]")
  
  to_analyze <- paste(debate_block,collapse=" ")
  # Count the number of each buzzword
  word_counts <- lapply(patterns, function(me) str_count(to_analyze,me))
  # Present more cleanly as a dataframe with nice names
  word_counts <- as.data.frame(word_counts)
  names(word_counts) <- c("I","we","America{n}",
                               "democra{cy,tic}","republic",
                               "Democrat{,ic}","Republican",
                               "free{,dom}","war",
                               "God (only)","God Bless",
                               "{Jesus, Christ, Christian}")
  # QUESTION 2D)
  # A regular expression to get words and sentences.  I treated
  # written numbers ("400,000") as one word, when possible.
  words <- str_extract_all(to_analyze,
  '(([:alpha:]+(\'([:alpha:]+)?)?)|([:digit:]+(,([:digit:]+)?)?))')
  sentence <- str_extract_all(to_analyze,
  "([:alpha:])(|[:alpha:]|[:space:]|[:digit:]|'|,|-|\\$|/|\\\"|(\\.[A-Za-z]))*((\\.\\.\\.)|\\.|\\?|\\!)")
  #return_vals$my_words <- words
  #return_vals$my_sentences <- sentence
  return_vals$mean_word_length <- mean(rapply(words,nchar))
  return_vals$num_character_from_words <- sum(rapply(words,nchar))
  return_vals$number_of_words <- length(unlist(words))
  return_vals$buzzwords <- word_counts
  #return_vals$db <- debate_block
  # Return all of the pertinent information for later printing
  return(return_vals)
}

debate_summary <- function(file_url)
{
  # QUESTION 2B) SOLUTION
  text_data <- create_debate_text(file_url)
  debate_blocks <- split_block(text_data)
  debate_statistics <- lapply(debate_blocks,summary_stats)
  
  return(debate_statistics)
}
# QUESTION 2A) solution
# Grabs the html links for the debates using the a href= tag,
# and then subsets based on the descriptor for the correct year
# and the first debate.
menu_url="http://www.debates.org/index.php?page=debate-transcripts"
menu_xml_handle <- htmlParse(menu_url)
menu_nodes <- getNodeSet(menu_xml_handle,"//a[@href]")
all_debate_links <- xpathSApply(menu_xml_handle,
                                "//a[@href]", xmlGetAttr, 'href')
years <- c("2012","2008","2004","2000","1996")
year_reg <- paste("(",paste(paste(years,collapse="|"),").+(First)"
                            ,sep="")
                  ,sep="")

# Uses a logical grep to subset all of the debate links into just
# the ones we want
my_debate_links <- all_debate_links[grepl(
  year_reg,
  sapply(menu_nodes,xmlValue))]
debate_blocks_list = list()

# Using a simple for loop to print with a nice index for the years.
# We do it with an lapply but it is nice to have the debate year
# printed out with the summary stats.  I thought that would be fine 
# for output.

debate_blocks_statistics <- lapply(my_debate_links,debate_summary)
names(debate_blocks_statistics) <- years
print(debate_blocks_statistics)
