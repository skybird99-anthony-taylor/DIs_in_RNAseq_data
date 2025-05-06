#UpSet plot script
# Libraries
library(tidyverse)
library(readxl)
library(UpSetR)
library(ComplexUpset)

# Load & Edit all the files to an UpSet readable format ####
bmv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_bmv.xlsx")
bmv <- bmv %>%
  drop_na(program,junction) %>%
    distinct(program, junction) %>%
    mutate(value = TRUE) %>%
    pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
bmv <- bmv %>%
  rename(
    VODKA2 = vodka2,
    ViReMa = virema,
    `DI-tector` = ditector,
    `DG-Seq` = dgseq,
    DVGfinder = dvgfinder)

cmv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_cmv.xlsx")
cmv <- cmv %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
cmv <- cmv %>%
  rename(
    VODKA2 = vodka2,
    ViReMa = virema,
    `DG-Seq` = dgseq)

covid <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_covid.xlsx")
covid <- covid %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
covid <- covid %>%
  rename(
    ViReMa = virema,
    `DI-tector` = ditector)

cymrsv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_cymrsv.xlsx")
cymrsv <- cymrsv %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
cymrsv <- cymrsv %>%
  rename(
    ViReMa = virema,
    `DI-tector` = ditector,
    DVGfinder = dvgfinder,
    VODKA2 = vodka2)

cymv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_cymv.xlsx")
cymv <- cymv %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
cymv <- cymv %>%
  rename(
    VODKA2 = vodka2,
    ViReMa = virema,
    `DG-Seq` = dgseq)

tcv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_tcv.xlsx")
tcv <- tcv %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
tcv <- tcv %>%
  rename(
    VODKA2 = vodka2,
    ViReMa = virema,
    DVGfinder = dvgfinder)

vodka2_junctions <- read_lines("E:\\DI_bioinformatics_data\\Results\\TSWV\\VODKA2\\junctions.txt")
vodka2_junctions <- tibble(junction = vodka2_junctions)
vodka2_junctions <- vodka2_junctions %>%
  mutate(
    virus = "TSWV",
    program = "vodka2"
  ) %>%
  select(virus, program, junction)  # Ensure correct column order
tswv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_tswv.xlsx")
tswv <- bind_rows(tswv, vodka2_junctions)
tswv <- tswv %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
tswv <- tswv %>%
  rename(
    ViReMa = virema,
    `DG-Seq` = dgseq,
    VODKA2 = vodka2)


tumv <- read_excel("E:\\DI_bioinformatics_data\\Results\\all_junctions_for_upset\\combined_tumv.xlsx")
tumv <- tumv %>%
  drop_na(program,junction) %>%
  distinct(program, junction) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = program, values_from = value, values_fill = FALSE)
tumv <- tumv %>%
  select(-`NA`)
tumv <- tumv %>%
  rename(
    VODKA2 = vodka2,
    ViReMa = virema,
    `DI-tector` = ditector,
    `DG-Seq` = dgseq,
    DVGfinder = dvgfinder)

# Make UpSet plots ####
annotations <- list(
  'Intersection size' = intersection_size(text = list(vjust = -0.1, size = 3))
)
# Create the UpSet plots (save as 1046x564)
upset(bmv, 
  intersect = setdiff(names(bmv), "junction"),
  name = "Programs",
  base_annotations = annotations,
  width_ratio = 0.2,
  queries = list(
    upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
    upset_query(set = "DI-tector", color = "deepskyblue", fill = "deepskyblue"),
    upset_query(set = "DVGfinder", color = "darkolivegreen4", fill = "darkolivegreen"),
    upset_query(set = "DG-Seq", color = "darkorange1", fill = "darkorange1"),
    upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))

upset(cmv, 
      intersect = setdiff(names(cmv), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DG-Seq", color = "darkorange1", fill = "darkorange1"),
        upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))

upset(covid, 
      intersect = setdiff(names(covid), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DI-tector", color = "deepskyblue", fill = "deepskyblue")))

upset(cymrsv, 
      intersect = setdiff(names(cymrsv), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DI-tector", color = "deepskyblue", fill = "deepskyblue"),
        upset_query(set = "DVGfinder", color = "darkolivegreen4", fill = "darkolivegreen4"),
        upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))

upset(cymv, 
      intersect = setdiff(names(cymv), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DG-Seq", color = "darkorange1", fill = "darkorange1"),
        upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))

upset(tcv, 
      intersect = setdiff(names(tcv), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DVGfinder", color = "darkolivegreen4", fill = "darkolivegreen4"),
        upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))

upset(tswv, 
      intersect = setdiff(names(tswv), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DG-Seq", color = "darkorange1", fill = "darkorange1"),
        upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))

upset(tumv, 
      intersect = setdiff(names(tumv), "junction"),
      name = "Programs",
      base_annotations = annotations,
      width_ratio = 0.2,
      queries = list(
        upset_query(set = "ViReMa", color = "darkred", fill = "darkred"),
        upset_query(set = "DI-tector", color = "deepskyblue", fill = "deepskyblue"),
        upset_query(set = "DVGfinder", color = "darkolivegreen4", fill = "darkolivegreen4"),
        upset_query(set = "DG-Seq", color = "darkorange1", fill = "darkorange1"),
        upset_query(set = "VODKA2", color = "cadetblue", fill = "cadetblue")))
