
# The following script builds a dummy model which can be used for development.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com


library(bnlearn)

names.NA = c("Service_continuity","Trust","Permited_access",
             "System_security",  "Provenance","Findability","Digital_data_type",
             "Search_facilities", "Cataloguing","Renderability","Technical_metadata",
             "Content_metadata",  "Tools","Obsolecence","Physical_disaster",
             "Economic_political_upheaval","Technological_upheaval",
             "Operating_environment", "Storage_life","Storage_media","Institution_type",
             "Copy_protocol","User_type","Replacement_protocol", "Preservation",
             "Processing", "Target_community")
dag.NA = empty.graph(nodes = names.NA)

dag.NA = set.arc(dag.NA, from = "Service_continuity", to = "Preservation")
dag.NA = set.arc(dag.NA, from = "Institution_type", to = "Service_continuity")
dag.NA = set.arc(dag.NA, from = "Processing", to = "Cataloguing")
dag.NA = set.arc(dag.NA, from = "Digital_data_type", to = "Processing")
dag.NA = set.arc(dag.NA, from = "Target_community", to = "User_type")
dag.NA = set.arc(dag.NA, from = "Institution_type", to = "Target_community")
dag.NA = set.arc(dag.NA, from = "Permited_access", to = "Findability")
dag.NA = set.arc(dag.NA, from = "Cataloguing", to = "Findability")
dag.NA = set.arc(dag.NA, from = "Search_facilities", to = "Findability")
dag.NA = set.arc(dag.NA, from = "Tools", to = "Renderability")
dag.NA = set.arc(dag.NA, from = "Obsolecence", to = "Tools")
dag.NA = set.arc(dag.NA, from = "Physical_disaster", to = "Operating_environment")
dag.NA = set.arc(dag.NA, from = "Economic_political_upheaval", to = "Service_continuity")
dag.NA = set.arc(dag.NA, from = "Economic_political_upheaval", to = "Copy_protocol")
dag.NA = set.arc(dag.NA, from = "Technological_upheaval", to = "Storage_media")
dag.NA = set.arc(dag.NA, from = "Content_metadata", to = "Provenance")
dag.NA = set.arc(dag.NA, from = "Content_metadata", to = "Cataloguing")
dag.NA = set.arc(dag.NA, from = "Operating_environment", to = "Storage_life")
dag.NA = set.arc(dag.NA, from = "Storage_life", to = "Preservation")
dag.NA = set.arc(dag.NA, from = "Findability", to = "Trust")
dag.NA = set.arc(dag.NA, from = "System_security", to = "Preservation")
dag.NA = set.arc(dag.NA, from = "Storage_media", to = "Obsolecence")
dag.NA = set.arc(dag.NA, from = "Storage_media", to = "Operating_environment")
dag.NA = set.arc(dag.NA, from = "Preservation", to = "Renderability")
dag.NA = set.arc(dag.NA, from = "Technical_metadata", to = "Tools")
dag.NA = set.arc(dag.NA, from = "Technical_metadata", to = "Obsolecence")
dag.NA = set.arc(dag.NA, from = "Provenance", to = "Preservation")
dag.NA = set.arc(dag.NA, from = "Digital_data_type", to = "Technical_metadata")
dag.NA = set.arc(dag.NA, from = "Digital_data_type", to = "Content_metadata")
dag.NA = set.arc(dag.NA, from = "Storage_media", to = "Storage_life")
dag.NA = set.arc(dag.NA, from = "Replacement_protocol", to = "Storage_life")
dag.NA = set.arc(dag.NA, from = "Copy_protocol", to = "Processing")
dag.NA = set.arc(dag.NA, from = "Processing", to = "Operating_environment")
dag.NA = set.arc(dag.NA, from = "Technical_metadata", to = "Processing")
dag.NA = set.arc(dag.NA, from = "User_type", to = "Search_facilities")
dag.NA = set.arc(dag.NA, from = "Content_metadata", to = "Permited_access")
dag.NA = set.arc(dag.NA, from = "Renderability", to = "Trust")


graphviz.plot(dag.NA, layout = "dot",
              highlight = list(nodes=c("Renderability","Findability",
                                        "Trust"), fill="lightgrey"),
              shape = "ellipse",
              render = TRUE,
              main="Proposed network")


# Create prob tables for each node
# Institution_type
prob.institution_type <- matrix(c(0.2, 0.2, 0.2, 0.2, 0.2), ncol = 5, dimnames = list(NULL, c("Government_Central", 
                                                                                              "Government_Local",
                                                                                              "Charity",
                                                                                              "Private_Corporate",
                                                                                              "Higher_Education")))

# Economic_political_upheaval
prob.economic <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))

# Digital_data_type
prob.digital_data_type <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("Digitised", "Surrogate")))

# Technical Upheaval
prob.technical_upheaval <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))

# Physical_disaster
prob.physical_disaster <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("High", "Low")))

# Replacement_protocol
prob.replacement_protocol <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))

# System_security
prob.system_security <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))

# Target_community
prob.target_community <- c(0.5, 0.5, 0.5, 0.5, 0.5,
                           0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.target_community) <- c(2, 5)
dimnames(prob.target_community) <- list("Target_community"=c("True", "False"), 
                                        "Institution_type"=c("Government_Central", 
                                                             "Government_Local",
                                                             "Charity",
                                                             "Private_Corporate",
                                                             "Higher_Education"))

# Copy_protocol
prob.copy_protocol <- c(0.4, 0.3, 0.3, 0.4, 0.3, 0.3)
dim(prob.copy_protocol) <- c(3, 2)
dimnames(prob.copy_protocol) <- list("Copy_protocol"=c("All", "Partial", "None"), 
                                     "Economic_political_upheaval"=c("True", "False"))

# Technical_metadata
prob.technical_metadata <- c(0.5, 0.5, 0.5, 0.5)
dim(prob.technical_metadata) <- c(2, 2)
dimnames(prob.technical_metadata) <- list("Technical_metadata"=c("True", "False"), 
                                          "Digital_data_type"=c("Digitised", "Surrogate"))

# User_type
prob.user_type <- c(0.5, 0.5, 0.5, 0.5)
dim(prob.user_type) <- c(2, 2)
dimnames(prob.user_type) <- list("User_type"=c("True", "False"), 
                                 "Target_community"=c("True", "False"))

# Storage_media
prob.storage_media <- c(0.2, 0.2, 0.2, 0.2, 0.2,
                        0.2
                        , 0.2, 0.2, 0.2, 0.2)
dim(prob.storage_media) <- c(5, 2)
dimnames(prob.storage_media) <- list("Storage_media"=c("Tape", "Optical", "SSD", "HDD", "Cloud"), 
                                     "Technological_upheaval"=c("True", "False"))

# Search_facilities
prob.search_facilities <- c(0.5, 0.5, 0.5, 0.5)
dim(prob.search_facilities) <- c(2, 2)
dimnames(prob.search_facilities) <- list("Search_facilities"=c("True", "False"), 
                                         "User_type"=c("True", "False"))

# Content_metadata
prob.content_metadata<- c(0.5, 0.5, 0.5, 0.5)
dim(prob.content_metadata) <- c(2, 2)
dimnames(prob.content_metadata) <- list("Content_metadata"=c("True", "False"), 
                                        "Digital_data_type"=c("Digitised", "Surrogate"))

# Permitted Access
prob.permited_access<- c(0.5, 0.5, 0.5, 0.5)
dim(prob.permited_access) <- c(2, 2)
dimnames(prob.permited_access) <- list("Permited_access"=c("True", "False"), 
                                        "Content_metadata"=c("True", "False"))
# Provenance
prob.provenance<- c(0.5, 0.5, 0.5, 0.5)
dim(prob.provenance) <- c(2, 2)
dimnames(prob.provenance) <- list("Provenance"=c("True", "False"), 
                                  "Content_metadata"=c("True", "False"))

# Service_continuity
prob.service_continuity <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                             0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.service_continuity) <- c(2, 5, 2)
dimnames(prob.service_continuity) <- list("Service_continuity"=c("True", "False"), 
                                          "Institution_type"=c("Government_Central", 
                                                               "Government_Local",
                                                               "Charity",
                                                               "Private_Corporate",
                                                               "Higher_Education"),
                                          "Economic_political_upheaval"=c("True", "False"))

# Cataloguing
prob.cataloguing <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.cataloguing) <- c(2, 2, 2)
dimnames(prob.cataloguing) <- list("Cataloguing"=c("True", "False"), 
                                   "Content_metadata"=c("True", "False"),
                                   "Processing"=c("True", "False"))

# Obsolecence
prob.obsolecence <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.obsolecence) <- c(2, 5, 2)
dimnames(prob.obsolecence) <- list("Obsolecence"=c("True", "False"), 
                                   "Storage_media"=c("Tape", "Optical", "SSD", "HDD", "Cloud"),
                                   "Technical_metadata"=c("True", "False"))

# Tools
prob.tools <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.tools) <- c(2, 2, 2)
dimnames(prob.tools) <- list("Tools"=c("True", "False"), 
                             "Obsolecence"=c("True", "False"),
                             "Technical_metadata"=c("True", "False"))

# Renderability
prob.renderability <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.renderability) <- c(2, 2, 2)
dimnames(prob.renderability) <- list("Renderability"=c("True", "False"), 
                                     "Preservation"=c("True", "False"),
                                     "Tools"=c("True", "False"))

# Trust
prob.trust <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.trust) <- c(2, 2, 2)
dimnames(prob.trust) <- list("Trust"=c("True", "False"), 
                             "Findability"=c("True", "False"),
                             "Renderability"=c("True", "False"))

# Processing
prob.processing <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.processing) <- c(2, 3, 2, 2)
dimnames(prob.processing) <- list("Processing"=c("True", "False"), 
                             "Copy_protocol"=c("All", "Partial", "None"),
                             "Digital_data_type"=c("Digitised", "Surrogate"),
                             "Technical_metadata"=c("True", "False"))

# Operating_environment
prob.operating_environment <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                                0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                                0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                                0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.operating_environment) <- c(2, 2, 2, 5)
dimnames(prob.operating_environment) <- list("Operating_environment"=c("True", "False"), 
                                             "Processing"=c("True", "False"),
                                             "Physical_disaster"=c("High", "Low"),
                                             "Storage_media"=c("Tape", "Optical", "SSD", "HDD", "Cloud"))

# Findability
prob.findability <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                                0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.findability) <- c(2, 2, 2, 2)
dimnames(prob.findability) <- list("Findability"=c("True", "False"), 
                                   "Search_facilities"=c("True", "False"),
                                   "Permited_access"=c("True", "False"),
                                   "Cataloguing"=c("True", "False"))

# Storage_life
prob.storage_life<- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.storage_life) <- c(2, 2, 2, 5)
dimnames(prob.storage_life) <- list("Storage_life"=c("True", "False"), 
                                    "Operating_environment"=c("True", "False"),
                                    "Replacement_protocol"=c("True", "False"),
                                    "Storage_media"=c("Tape", "Optical", "SSD", "HDD", "Cloud"))

# Preservation
prob.preservation <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
dim(prob.preservation) <- c(2, 2, 2, 2, 2)
dimnames(prob.preservation) <- list("Preservation"=c("True", "False"), 
                                    "Provenance"=c("True", "False"),
                                    "Service_continuity"=c("True", "False"),
                                    "System_security"=c("True", "False"),
                                    "Storage_life"=c("True", "False"))

model.fit <- custom.fit(dag.NA,
                        dist=list("Institution_type"=prob.institution_type,
                                  "Economic_political_upheaval"=prob.economic,
                                  "Digital_data_type"=prob.digital_data_type,
                                  "Technological_upheaval"=prob.technical_upheaval,
                                  "Physical_disaster"=prob.physical_disaster,
                                  "Replacement_protocol"=prob.replacement_protocol,
                                  "System_security"=prob.system_security,
                                  "Target_community"=prob.target_community,
                                  "Copy_protocol"=prob.copy_protocol,
                                  "Technical_metadata"=prob.technical_metadata,
                                  "User_type"=prob.user_type,
                                  "Storage_media"=prob.storage_media,
                                  "Search_facilities"=prob.search_facilities,
                                  "Content_metadata"=prob.content_metadata,
                                  "Permited_access"=prob.permited_access,
                                  "Provenance"=prob.provenance,
                                  "Service_continuity"=prob.service_continuity,
                                  "Cataloguing"=prob.cataloguing,
                                  "Obsolecence"=prob.obsolecence,
                                  "Tools"=prob.tools,
                                  "Processing"=prob.processing,
                                  "Operating_environment"=prob.operating_environment,
                                  "Renderability"=prob.renderability,
                                  "Trust"=prob.trust,
                                  "Findability"=prob.findability,
                                  "Storage_life"=prob.storage_life,
                                  "Preservation"=prob.preservation))

write.bif("Model.bif", model.fit)

# temp.names <- c("A", "B", "C", "D")# , "E", "F", "G")
# dag.temp <- empty.graph(nodes=temp.names)

# dag.temp = set.arc(dag.temp, from = "A", to = "D")
# dag.temp = set.arc(dag.temp, from = "B", to = "D")
# dag.temp = set.arc(dag.temp, from = "A", to = "C")
# dag.temp = set.arc(dag.temp, from="A", to="B")

# graphviz.plot(dag.temp, layout = "dot",
#               shape = "ellipse",
#               render = TRUE,
#               main="Proposed network")


# prob.A <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))
# prob.C <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))
# prob.B <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))
# prob.E <- matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("True", "False")))

# prob.B <- c(0.5, 0.5, 0.5, 0.5)
# dim(prob.B) <- c(2, 2)
# dimnames(prob.B) <- list("B"=c("True", "False"), 
#                          "A"=c("True", "False"))

# prob.C <- c(0.5, 0.5, 0.5, 0.5)
# dim(prob.C) <- c(2, 2)
# dimnames(prob.C) <- list("C"=c("True", "False"), 
#                          "A"=c("True", "False"))

# prob.D <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
# dim(prob.D) <- c(2, 2, 2)
# dimnames(prob.D) <- list("D"=c("True", "False"), 
#                          "B"=c("True", "False"),
#                          "A"=c("True", "False"))

# model.fit.temp <- custom.fit(dag.temp,
#                         dist=list(
#                           `D`=prob.D,
#                           `A`=prob.A,
#                           `B`=prob.B,
#                           C=prob.C
#                         ))
# model.fit.temp


