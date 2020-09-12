library("tidyverse")

set_up_questions = read_csv("inst/text_content/setup_questions.csv")
node_states = read_csv("inst/text_content/node_states.csv")
node_info = read_csv("inst/text_content/node_information.csv") %>%
  select(-type)


nodes = left_join(set_up_questions, node_info, by = "node_name") %>%
  left_join(node_states, by = "node_name")

nodes = nodes %>%
  rename(node = node_name,
         definition = node_definition,
         text = node_question,
         options = node_state,
         detail = state_definition) %>%
  select(node, type, definition, text, options, detail)

nodes = nodes %>%
  mutate(type = case_when(node == "Technical_Skills" ~ "slider",
                          node == "System_Security" ~ "slider",
                          node == "Checksum" ~ "grouper slider",
                          node == "Info_Management" ~ "slider",
                          node == "Digital_Object" ~ "grouper slider",
                          node == "Storage_Medium" ~ "grouped slider",
                          node == "Rep_and_Refresh" ~ "slider",
                          node == "Op_Environment" ~ "slider",
                          node == "Physical_Disaster" ~ "non-numeric slider"))

nodes = nodes %>%
  nest(cols = -node) %>%
  mutate(question = paste0("question", row_number())) %>%
  unnest(cols = cols) %>%
  pivot_longer(names_to = "type", values_to = "value", -question) %>%
  nest(cols = c(-question, -type)) %>%
  mutate(cols = map(cols, ~unique(unlist(.x))))


nodes_list = list()

for(n in unique(nodes$question)){
  filtered_node = filter(nodes, question == n) %>%
    select(-question)

  filtered_node_list = setNames(as.list(filtered_node$cols),
                                filtered_node$type)

  nodes_list[[n]] = filtered_node_list
}

nodes_list %>%
  yaml::write_yaml("inst/text_content/questions_auto_gen.yml")
