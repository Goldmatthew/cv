# download the online cv using the googledrive package

get_online_cv <- function(){
    file <- "data/data.xlsx"
    cv <- "https://docs.google.com/spreadsheets/d/1hVyK8LqC0c03antzkewx9K5sidw6Sfx5/edit#gid=1080341057"
    googledrive::drive_download(cv, path = "data/data.xlsx", overwrite = TRUE)
    invisible(file)
}

clean_cv_data <- function(data){
    data %>% 
        mutate(date = str_remove_all(date, "\\.0"))
}

download_sign <- function(data){
    link <- "https://drive.google.com/file/d/1SAxSjMblAUdM3zp6hflW_EI6rzD_XJIo/view?usp=share_link"
    googledrive::drive_download(link, path = "img/signature.png", overwrite = TRUE)
}

# reading the cv into a list

import_cv <- function(cv){
    cv_sec_names <- readxl::excel_sheets(cv)
    cv_secs <- lapply(cv_sec_names, function(sec) readxl::read_xlsx(cv, sec))
    cv_secs <- setNames(cv_secs, cv_sec_names)
    return(cv_secs)
}

# download the bib file with my publications

get_publications <- function(){
    my_pubs <- "https://raw.githubusercontent.com/filippogambarota/fg-publications/main/fg-references.bib"
    outfile <- "data/ref.bib"
    download.file(my_pubs, destfile = outfile, quiet = TRUE)
    invisible(outfile)
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

bib_to_df <- function(bib){
    bib <- RefManageR::ReadBib(bib, check = "warn", .Encoding = "UTF-8")
    as.data.frame(bib)
}

prepare_bib <- function(bib){
    bib <- bib_to_df(bib)
    title <- paste("###", bib$title)
    title <- str_remove_all(title, "\n")
    title <- str_replace_all(title, "`", "'")
    title <- str_replace_all(title, "''", "'")
    date <- bib$year
    authors <- stringr::str_split(bib$author, " and ")
    authors <- map_depth(authors, 2, get_author_surname)
    authors <- sapply(authors, function(x) paste(unlist(x), collapse = ", "))
    authors <- str_replace_all(authors, "Doro", "**Doro**")
    ref <- ifelse(!is.na(bib$doi), 
                  sprintf("%s [DOI: %s](%s)", bib$journal, bib$doi, bib$doi),
                  bib$journal)
    tibble(
        type = bib$bibtype, title, authors, date, ref
    )
}

update_cv <- function(upload = FALSE){ 
    
    if(!file.exists("img/signature.png")){
        download_sign()
    }
    
    out_html <- rmarkdown::render("cv.Rmd", 
                                  output_dir = "docs",
                                  output_file = "index.html",
                                  quiet = TRUE,
                                  params = list(pdf_mode = FALSE,
                                                html_mode = TRUE))
    
    out_html_pdf <- rmarkdown::render("cv.Rmd", 
                                  output_dir = "docs", 
                                  output_file = "temp",
                                  quiet = TRUE,
                                  params = list(pdf_mode = TRUE,
                                                html_mode = FALSE))
    
    pagedown::chrome_print(out_html_pdf, output = "docs/cv.pdf")
    fs::file_delete(out_html_pdf)
    cli::cli_alert_success("CV updated! :)")
    
    if(upload){
        gert::git_add(c("cv.Rmd", "docs/", "data/")) # adding
        gert::git_commit("updating cv")
        gert::git_push()
        cli::cli_alert_success("CV uploaded on Github! :)")
    }
}