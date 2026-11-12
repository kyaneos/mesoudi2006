# Makes the blind hand-coding workbook from the whispers export.
#   Rscript coding_sheet.R          (run from new_code/, after npm run export in whispers)
# Writes two files next to the export (_study instead of _pilot for the real study), which is gitignored because it holds participant text:
#   coding_sheet_pilot.xlsx  a rubric tab, then one tab per coder: shuffled recalls, no chain or generation
#   coding_key_pilot.csv     what links each recall code back to its chain and generation. Coders never see it
# Each coder tab has one row per recall and one column per point of the original, with the point
# itself as the header. Coders put 1 when the recall contains that point, in any wording, and
# leave it blank when it doesn't. Every generation is checked against the original.

library(tidyverse)

# TRUE: the pilot (test link). FALSE: the real study
pilot <- TRUE
coders <- c("Justin", "temp")
export_path <- "whispers/exports/whispers_responses.json"
# the pilot and the study each get their own sheet and key, so their codes never mix
sheet_path <- paste0("whispers/exports/coding_sheet_", if (pilot) "pilot" else "study", ".xlsx")
key_path <- paste0("whispers/exports/coding_key_", if (pilot) "pilot" else "study", ".csv")
set.seed(1974) # the same shuffle every time, after Kintsch (1974)

# Mesoudi's four paragraphs, from study2_material.pdf
paragraphs <- c(
  gossip = "Nancy is having an affair with her married college professor. Nancy recently became pregnant with the professor’s child. The professor promised Nancy that he would leave his wife, but since Nancy told him she was pregnant, the professor refused to see her. So Nancy told the professor’s wife about the affair. The professor’s wife was so upset that she left the professor.",
  social = "Nancy enjoys swimming. Nancy was going to the swimming pool but got lost, so she asked an old man waiting at a bus stop for directions. The old man could not give her directions. A bus arrived at the bus stop and the old man asked the driver for directions. The driver gave Nancy directions to the swimming pool, so Nancy was able to go swimming.",
  individual = "One morning Nancy’s alarm clock broke and she overslept. When she woke up she realised that she was late for an important lecture. She got dressed as quickly as she could, left the house and ran to the lecture theatre. When she got there the lecture theatre was empty. Nancy had missed the lecture.",
  physical = "The weather in Colorado gets hot and dry in the summer. This removes moisture from the soil and dries out the plants that grow there. The dry vegetation catches fire easily, leading to frequent forest fires. These fires release smoke containing carbon monoxide into the atmosphere. This smoke contributes to global warming, increasing temperatures further."
)

# Mesoudi's 56 propositions, 14 per material, from study2_material.pdf, each with
# the plain-language point coders see
propositions <- tribble(
  ~material, ~number, ~proposition, ~point,
  "gossip", 1, "HAVE, NANCY, AFFAIR", "Nancy is having an affair",
  "gossip", 2, "WITH, AFFAIR, COLLEGE-PROFESSOR", "The affair is with her college professor",
  "gossip", 3, "IS, PROFESSOR, MARRIED", "The professor is married",
  "gossip", 4, "IS, NANCY, PREGNANT", "Nancy is pregnant",
  "gossip", 5, "PREGNANT-BY, NANCY, PROFESSOR", "Nancy is pregnant by the professor",
  "gossip", 6, "PROMISE, PROFESSOR, NANCY, 7", "The professor promised Nancy something",
  "gossip", 7, "LEAVE, PROFESSOR, WIFE", "The professor would leave his wife",
  "gossip", 8, "SEE, PROFESSOR, NANCY, REFUSE", "The professor refused to see Nancy",
  "gossip", 9, "TELL, NANCY, PROFESSOR, 4", "Nancy told the professor she was pregnant",
  "gossip", 10, "SINCE, 9, 8", "He refused to see her because she told him she was pregnant",
  "gossip", 11, "TELL, NANCY, WIFE", "Nancy told the professor's wife",
  "gossip", 12, "ABOUT, AFFAIR", "What she told the wife was about the affair",
  "gossip", 13, "UPSET, WIFE", "The wife was upset",
  "gossip", 14, "LEAVE, WIFE, PROFESSOR", "The wife left the professor",
  "social", 1, "ENJOY, NANCY, SWIMMING", "Nancy enjoys swimming",
  "social", 2, "GO, NANCY, SWIMMING-POOL", "Nancy was going to the swimming pool",
  "social", 3, "LOST, NANCY", "Nancy got lost",
  "social", 4, "ASK, NANCY, MAN", "Nancy asked a man",
  "social", 5, "FOR, DIRECTIONS", "Nancy asked the man for directions",
  "social", 6, "IS, MAN, OLD", "The man was old",
  "social", 7, "WAIT, MAN, BUS-STOP", "The man was waiting at a bus stop",
  "social", 8, "GIVE, MAN, DIRECTIONS, CANNOT", "The man couldn't give her directions",
  "social", 9, "ARRIVE, BUS, BUS-STOP", "A bus arrived at the bus stop",
  "social", 10, "ASK, MAN, DRIVER", "The man asked the driver",
  "social", 11, "FOR, DIRECTIONS", "The man asked the driver for directions",
  "social", 12, "GIVE, DRIVER, NANCY, DIRECTIONS", "The driver gave Nancy directions",
  "social", 13, "TO, DIRECTIONS, SWIMMING-POOL", "The directions were to the swimming pool",
  "social", 14, "GO, NANCY, SWIMMING", "Nancy went swimming",
  "individual", 1, "BROKE, ALARM-CLOCK", "The alarm clock broke",
  "individual", 2, "IN, MORNING", "It happened in the morning",
  "individual", 3, "BELONGS-TO, ALARM-CLOCK, NANCY", "The alarm clock was Nancy's",
  "individual", 4, "OVERSLEEP, NANCY", "Nancy overslept",
  "individual", 5, "WAKE-UP, NANCY", "Nancy woke up",
  "individual", 6, "LATE, NANCY, LECTURE", "Nancy was late for a lecture",
  "individual", 7, "IMPORTANT, LECTURE", "The lecture was important",
  "individual", 8, "DRESS, NANCY", "Nancy got dressed",
  "individual", 9, "QUICKLY, DRESS", "She got dressed quickly",
  "individual", 10, "LEAVE, NANCY, HOUSE", "Nancy left the house",
  "individual", 11, "RUN, NANCY, LECTURE-THEATRE", "Nancy ran to the lecture theatre",
  "individual", 12, "ARRIVE, NANCY, LECTURE-THEATRE", "Nancy got to the lecture theatre",
  "individual", 13, "EMPTY, LECTURE-THEATRE", "The lecture theatre was empty",
  "individual", 14, "MISS, NANCY, LECTURE", "Nancy missed the lecture",
  "physical", 1, "HOT, WEATHER", "The weather is hot",
  "physical", 2, "DRY, WEATHER", "The weather is dry",
  "physical", 3, "IN, SUMMER", "It happens in summer",
  "physical", 4, "REMOVE, MOISTURE, SOIL", "Moisture is removed from the soil",
  "physical", 5, "DRY, PLANTS", "The plants dry out",
  "physical", 6, "CATCH-FIRE, VEGETATION", "The vegetation catches fire",
  "physical", 7, "EASILY, CATCH-FIRE", "It catches fire easily",
  "physical", 8, "CAUSE, FOREST-FIRES", "This leads to forest fires",
  "physical", 9, "FREQUENT, FOREST-FIRES", "The forest fires are frequent",
  "physical", 10, "RELEASE, SMOKE", "The fires release smoke",
  "physical", 11, "CONTAIN, SMOKE, CARBON-MONOXIDE", "The smoke contains carbon monoxide",
  "physical", 12, "INTO, SMOKE, ATMOSPHERE", "The smoke goes into the atmosphere",
  "physical", 13, "CONTRIBUTE, GLOBAL-WARMING", "The smoke contributes to global warming",
  "physical", 14, "INCREASE, TEMPERATURE", "Temperatures increase"
)

# accepted recalls. Ones already in the key keep their code, so a rerun never
# changes what R01 means once someone is coding. New ones are shuffled and
# numbered after the last code
accepted <- jsonlite::read_json(export_path, simplifyVector = TRUE) |>
  filter(test == pilot, status == "passed")
key <- if (file.exists(key_path)) read_csv(key_path, show_col_types = FALSE) else tibble(recall = character(), id = character())
new <- accepted |>
  filter(!id %in% key$id) |>
  slice_sample(prop = 1) |>
  mutate(recall = sprintf("R%02d", nrow(key) + row_number()))
key <- bind_rows(key, select(new, recall, id, chain, generation))
write_csv(key, key_path)

recalls <- accepted |>
  inner_join(select(key, recall, id), by = "id") |>
  arrange(recall)

# the rubric tab: each material's name and paragraph on its first row only, then its 14 points
rubric <- propositions |>
  mutate(paragraph = if_else(number == 1, paragraphs[material], ""),
         material = if_else(number == 1, material, "")) |>
  select(material, paragraph, number, point, proposition)

# a coder tab: the recall, then all 56 points as empty columns. Notes go in cell comments
coder_tab <- select(recalls, recall, text = recall_raw)
for (p in propositions$point) coder_tab[[p]] <- ""

# write the workbook, laid out the way it reads in Google Sheets: wrapped text, top
# aligned, frozen headers. Widths are in Excel units, roughly (pixels - 5) / 7, so
# 54 is about 380 px for the recall text and 12 about 90 px for each point
wrapped <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
header <- openxlsx::createStyle(wrapText = TRUE, valign = "top", textDecoration = "bold")
wb <- openxlsx::createWorkbook()
openxlsx::modifyBaseFont(wb, fontName = "Average") # the font the coding sheet uses in Sheets

openxlsx::addWorksheet(wb, "rubric")
openxlsx::writeData(wb, "rubric", rubric, headerStyle = header)
openxlsx::setColWidths(wb, "rubric", 1:5, widths = c(8, 59, 8, 36, 36))
openxlsx::addStyle(wb, "rubric", wrapped, rows = 2:(nrow(rubric) + 1), cols = 1:5, gridExpand = TRUE)
openxlsx::freezePane(wb, "rubric", firstRow = TRUE)

# coder tabs: recall code and text frozen on the left, then narrow point columns.
# No fixed row heights, so Sheets sizes each row to its wrapped text
for (coder in coders) {
  openxlsx::addWorksheet(wb, coder)
  openxlsx::writeData(wb, coder, coder_tab, headerStyle = header)
  openxlsx::setColWidths(wb, coder, 1:ncol(coder_tab),
                         widths = c(8, 54, rep(12, ncol(coder_tab) - 2)))
  openxlsx::addStyle(wb, coder, wrapped, rows = 2:(nrow(coder_tab) + 1), cols = 1:ncol(coder_tab), gridExpand = TRUE)
  openxlsx::freezePane(wb, coder, firstActiveRow = 2, firstActiveCol = 3)
}

openxlsx::saveWorkbook(wb, sheet_path, overwrite = TRUE)

cat("wrote", nrow(recalls), "recalls for", length(coders), "coders to", sheet_path, "\n")
