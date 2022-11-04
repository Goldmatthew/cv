format_edu <- function(data){
    title <- ifelse(!is.na(data$duration), 
                    sprintf("%s (%s)", data$title, data$duration), 
                    data$title)
    cat("### ", title, "\n\n")
    cat(data$place, "\n\n")
    cat("N/A", "\n\n")
    cat(data$date, "\n\n")
    if(!is.na(data$details)){
        cat(data$details, "\n\n")
    }
}

format_social <- function(data, size){
    pattern <- "<a href='%s'> <i class='%s-%sx'></i></a>"
    sprintf(pattern, data$link, data$icon, size)
}

format_pub <- function(data){
    cat(data$title, "\n\n")
    cat(data$ref, "\n\n")
    cat("N/A", "\n\n")
    cat(data$date, "\n\n")
    cat(data$authors, "\n\n")
}

format_teaching <- function(data){
    title <- ifelse(!is.na(data$hours), 
                    sprintf("%s (%s hrs.)", data$title, data$hours), 
                    data$title)
    cat("### ", title, "\n\n")
    cat(data$place, "\n\n")
    cat("N/A", "\n\n")
    cat(data$date, "\n\n")
    if(!is.na(data$materials)){
        materials <- sprintf("[Materials](%s)", data$materials)
        data$details <- paste(data$details, " - ", materials)
    }
    if(!is.na(data$details)){
        cat(data$details, "\n\n")
    }else{
        cat("\n\n")
    }
}

format_conf <- function(data){
    authors <- str_replace_all(data$authors, "Gambarota", "**Gambarota**")
    authors <- ifelse(startsWith(authors, "**Gambarota**"),
           str_replace_all(authors[startsWith(authors, "**Gambarota**")], 
                           pattern = "\\*\\*Gambarota\\*\\*", 
                           "\\*\\*Gambarota\\*\\* [presenter]"),
           authors)
    
    video <- ifelse(!is.na(data$link_talk), 
                    sprintf("[Video](%s)", data$link_talk),
                    NA)
    materials <- ifelse(!is.na(data$link_materials), 
                        sprintf("[Materials](%s)", data$link_materials),
                        NA)
    video_materials <- c(video, materials)
    video_materials <- video_materials[!is.na(video_materials)]
    if(length(video_materials) > 0){
        video_materials <- paste(video_materials, collapse = ", ")
        data$conference <- sprintf("%s [*%s*] - %s", 
                                   data$conference, 
                                   data$type, 
                                   video_materials)
    }else{
        data$conference <- sprintf("%s [*%s*]", 
                                   data$conference, 
                                   data$type)
    }
    
    cat("### ", data$title, "\n\n")
    cat(data$conference, "\n\n")
    cat(data$place, "\n\n")
    cat(data$date, "\n\n")
    cat(authors, "\n\n")
    
}

format_all <- function(data, format_fun){
    data_by_row <- split(data, 1:nrow(data))
    purrr::walk(data_by_row, format_fun)
}

get_author_surname <- function(author){
    if(grepl("\\}", author)){
        temp <- unlist(str_split(author, " \\{"))
        temp <- str_remove(temp, "\\}")
    }else{
        temp <- unlist(str_split(author, " "))
    }
    temp[length(temp)]
}


bold <- function(x){
    sprintf("**%s**", x)
}