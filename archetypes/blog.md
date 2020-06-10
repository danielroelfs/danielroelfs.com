---
title: "{{ replace .TranslationBaseName "-" " " | title }}"
author: Daniel Roelfs
date: {{ .Date }}

draft: true
output:
  html_document:
    keep_md: yes
always_allow_html: true

tags: [R, ]
image: index_files/
---