### CREATE HTML FOR IMAGES ########################

loi <- list.files(here::here("content/photography/img/"))

html_template <- '\t\t\t\t\t<img src="./img/%s">'

html_text <- sprintf(html_template, loi)                  

readr::write_lines(html_text, file = sprintf("%s/img_files.txt", here::here("content/photography/")))
