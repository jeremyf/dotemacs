path_to_pdf = "~/Documents/RPGs/Worlds-without-Number-WWN/WorldsWithoutNumber_DeluxePDF_031521.pdf"
title = "Worlds without Number Deluxe Edition"
results = %x( pdftocio -H #{path_to_pdf})

transformations = results.gsub(/^/, "*").gsub((" " * 4), "*").gsub(/\*(\w)/, '* \1').gsub(/ ··· (\d+)/, "\n:PROPERTIES:\n:PAGE: \\1\n:END:\n")

file = $stdout
file.puts ":PROPERTIES:"
file.puts ":PATH_TO_PDF: #{path_to_pdf}"
file.puts ":END:"
file.puts "#title: #{title}"
file.puts "#FILETAGS:"
file.puts "#+STARTUP: overview"
file.puts ""
file.puts transformations
