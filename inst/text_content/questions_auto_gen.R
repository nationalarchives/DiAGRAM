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
                          node == "Checksum" ~ "grouped slider",
                          node == "Info_Management" ~ "slider",
                          node == "Digital_Object" ~ "grouped slider",
                          node == "Storage_Medium" ~ "grouped slider",
                          node == "Rep_and_Refresh" ~ "slider",
                          node == "Op_Environment" ~ "slider",
                          node == "Physical_Disaster" ~ "non-numeric slider"),
         extra = case_when(
           node == "Technical_Skills" ~ "The default is based on responses to the JISC digital skills
            survey and how many said that there was full capability within their organisation
            to do file format migration, software emulation or data recovery. (15%)",
           node == "System_Security" ~ "The default is based on responses to the JISC digital skills
            survey and how many agreed that their IT provider supports the requirements of
            the archival activities of your organisation toa large or very great extent and
            that their digital collections are protected with access restrictions/
            permissions. (17%)",
           node == "Checksum" ~ NA_character_,
           node == "Info_Management" ~ "The default is based on responses to the JISC digital skills
            survey. 70% of respondents agreed that their catalogue management
            system meets the needs of the organisation and 40% that their
            digital asset management system meets the needs of the organisation.
            We have estimated that 55% would therefore have sufficient
            information management systems, as you don't need a bespoke
            digital asset management system to have support for coherent
            information management and documentation of preservation actions,
            but you may need more than just a catalogue system.",
           node == "Digital_Object" ~ NA_character_,
           node == "Storage_Medium" ~ NA_character_,
           node == "Rep_and_Refresh" ~ NA_character_,
           node == "Op_Environment" ~ NA_character_,
           node == "Physical_Disaster" ~ NA_character_
         ))

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
