## the very first function to install or load namespaces
load_or_install <- function(func = "", ...){
  if(require(package =  func, character.only = T) == FALSE){
    install.packages(func, clean = T, ...)
    require(package =  func, character.only = T)
  } else {
    require(package =  func, character.only = T)
  }
}
##

getWatsonClass <- function(classifier, query){
  ans <- watson.nlc.processtextreturnclass(classifier, query)
  return(list("query" = query, ans[1,1, with=F], ans[1,2, with=F]))
}

getWatsonProfile <- function(classifier,user.txt){
  in.sentences <- unlist(strsplit(user.txt, "\\."))
  in.sentences <- in.sentences[!(in.sentences %in% " ")]
  in.sentences <- in.sentences[!(in.sentences %in% "")]
  ans.df <- lapply(in.sentences, function(xx){
      return(getWatsonClass(classifier, xx))
    })
  ans.df <- ldply(ans.df, data.frame)
  return(ans.df)
}


getUsername <- function(txt){
  remove.txt <- c("my", "surname", "name", "is", "i ", "am", "called", "full", "\\.", " ")
  txt.ans <- tolower(txt)
  for(xx in remove.txt){
    txt.ans <- gsub(xx, "", txt.ans)
  }
  return(txt.ans)
}


getDigits <- function(txt){
  return(as.numeric(unlist(str_extract_all(txt, "[:digit:]+"))))
}


getProfile <- function(classifier, in.txt){
  in.txt <- gsub("\\n", "", in.txt)
  ans.df <- getWatsonProfile(classifier, user.txt = in.txt)
  ans.name <- tail(as.character(ans.df[which(ans.df$class == "Username"),1]), 1)
  ans.name <- getUsername(ans.name)
  ans.age <- tail(as.character(ans.df[which(ans.df$class == "Age"),1]), 1)
  ans.age <- getDigits(ans.age)
  ans.df2 <- data.frame(matrix(nrow = 6, ncol = 2))
  names(ans.df2) <- c("class", "value")
  ans.df2$value <- rep("", nrow(ans.df2))
  ans.df2$class <- c("Username", "Age", "Player_level", "Player_type", "Stroke_type", "Common_error")
  # if(length(ans.df$confidence[grep("Username", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Username", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Username"] <- ans.name
  #   }
  # }
  # if(length(ans.df$confidence[grep("Age", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Age", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Age"] <- ans.age
  #   }
  # }
  # if(length(ans.df$confidence[grep("Ptype", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Ptype", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Player_type"] <- ans.df$class[grep("Ptype", ans.df$class)]
  #   }
  # }
  # if(length(ans.df$confidence[grep("Player_type", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Player_type", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Player_level"] <- ans.df$class[grep("Player_type", ans.df$class)]
  #   }
  # }
  # if(length(ans.df$confidence[grep("Stype", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Stype", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Stroke_type"] <- ans.df$class[grep("Stype", ans.df$class)]
  #   }
  # }
  # if(length(ans.df$confidence[grep("Err_type", ans.df$class)]) != 0){
  #   if(ans.df$confidence[grep("Err_type", ans.df$class)] > 0.5){
  #     ans.df2$value[ans.df2$class == "Common_error"] <- ans.df$class[grep("Err_type", ans.df$class)]
  #   }
  # }
  if(length(tail(ans.df$confidence[grep("Username", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Username", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Username"] <- ans.name
    }
  }
  if(length(tail(ans.df$confidence[grep("Age", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Age", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Age"] <- ans.age
    }
  }
  if(length(tail(ans.df$confidence[grep("Ptype", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Ptype", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Player_type"] <- tail(ans.df$class[grep("Ptype", ans.df$class)], 1)
    }
  }
  if(length(tail(ans.df$confidence[grep("Player_type", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Player_type", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Player_level"] <- tail(ans.df$class[grep("Player_type", ans.df$class)], 1)
    }
  }
  if(length(tail(ans.df$confidence[grep("Stype", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Stype", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Stroke_type"] <- tail(ans.df$class[grep("Stype", ans.df$class)], 1)
    }
  }
  if(length(tail(ans.df$confidence[grep("Err_type", ans.df$class)], 1)) != 0){
    if(tail(ans.df$confidence[grep("Err_type", ans.df$class)], 1) > 0.5){
      ans.df2$value[ans.df2$class == "Common_error"] <- tail(ans.df$class[grep("Err_type", ans.df$class)], 1)
    }
  }
  return(ans.df2)
}