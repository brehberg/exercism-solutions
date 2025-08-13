# Generate an acronym from a string. "This is a string" => "TIAS"
acronym<-function(s){toupper(paste(substr(strsplit(gsub("_","",s),"[- ]")[[1]],1,1),collapse=""))}