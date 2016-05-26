DT_News_cleansing <- iconv(NewsTrain, "latin1", "ASCII","")	
 	DT_News_cleansing <- gsub("[[:punct:]]", " ", DT_News_cleansing) 	
 	DT_News_cleansing <- gsub('[0-9]+', '', DT_News_cleansing) ## remove number	
 	DT_News_cleansing <- gsub("[^[:alnum:]]", " ", DT_News_cleansing) 	
 	DT_News_cleansing <- gsub(" [b-hj-z] "," ", DT_News_cleansing) # removes all single letters except "a" and "i"	
 	

