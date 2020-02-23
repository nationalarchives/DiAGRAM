
# The following script builds a dummy model which can be used for development.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com

# Updated 21/02/2020 to relfect lastest model


library(bnlearn)
node.names <- c("Op_Environment","Integrity","System_Security","Info_Management","Storage_Medium", "ReplicationAndRefreshment", 
                "Digital_Object","Content_Metadata","Tech_Metadata","File_Format", "Checksum", "Obsolescence",
                "Tools_to_Render", "Intellectual_Control","Conditions_of_Use","Renderability","Bit_Preservation","Identity",
                "Physical_Disaster","Storage_Life","Technical_Skills")

#empty graph
TNA.dag <- empty.graph(nodes = node.names)

#set edges

#data management - from Digital_Object
TNA.dag <- set.arc(TNA.dag, from="Digital_Object", to="Content_Metadata")
TNA.dag <- set.arc(TNA.dag, from="Digital_Object", to="Tech_Metadata")
TNA.dag <- set.arc(TNA.dag, from="Digital_Object", to="Conditions_of_Use")
TNA.dag <- set.arc(TNA.dag, from="Digital_Object", to="File_Format")

#data management/access - to Identity and Intellectual_Control
TNA.dag <- set.arc(TNA.dag, from="Content_Metadata", to="Identity")
TNA.dag <- set.arc(TNA.dag, from="Info_Management", to="Identity")

TNA.dag <- set.arc(TNA.dag, from="Identity", to="Intellectual_Control")
TNA.dag <- set.arc(TNA.dag, from="Conditions_of_Use", to="Intellectual_Control")

#preservation planning - obsolescence 
TNA.dag <- set.arc(TNA.dag, from="Technical_Skills", to="Obsolescence")
TNA.dag <- set.arc(TNA.dag, from="Technical_Skills", to="Tools_to_Render")
TNA.dag <- set.arc(TNA.dag, from="Technical_Skills", to="Tech_Metadata")

TNA.dag <- set.arc(TNA.dag, from="File_Format", to="Tools_to_Render")
TNA.dag <- set.arc(TNA.dag, from="Storage_Medium", to="Obsolescence")
TNA.dag <- set.arc(TNA.dag, from="Obsolescence", to="Bit_Preservation")

#preservation planning - Integrity
TNA.dag <- set.arc(TNA.dag, from="Info_Management", to="Integrity") 
TNA.dag <- set.arc(TNA.dag, from="Checksum", to="Integrity") 
TNA.dag <- set.arc(TNA.dag, from="System_Security", to="Integrity")
TNA.dag <- set.arc(TNA.dag, from="Integrity", to="Bit_Preservation")

#storage - to Storage_Life
TNA.dag <- set.arc(TNA.dag, from="Storage_Medium", to="Storage_Life")
TNA.dag <- set.arc(TNA.dag, from="Physical_Disaster", to="Storage_Life")
TNA.dag <- set.arc(TNA.dag, from="Op_Environment", to="Storage_Life")
TNA.dag <- set.arc(TNA.dag, from="ReplicationAndRefreshment", to="Storage_Life")

TNA.dag <- set.arc(TNA.dag, from="Storage_Life", to="Bit_Preservation")

#preservation planning - to renderability
TNA.dag <- set.arc(TNA.dag, from="Tools_to_Render", to="Renderability")
TNA.dag <- set.arc(TNA.dag, from="Bit_Preservation", to="Renderability")
TNA.dag <- set.arc(TNA.dag, from="Tech_Metadata", to="Renderability")


graphviz.plot(TNA.dag, layout = "dot",
              highlight = list(nodes=c("Renderability","Intellectual_Control"), fill="lightgrey"),
              shape = "ellipse",
              render = TRUE,
              main="DiAGRAM Network")



# Create prob tables for each node

#Digital_Object
prob.digital_object = matrix(c(0.05, 0.20, 0.75), ncol = 3, 
                             dimnames = list(NULL,c("Born_digital","Digitised","Surrogate")))

#Technical_Skills
prob.technical_skills = matrix(c(0.5,0.5), ncol = 2, 
                               dimnames = list(NULL,c("Good","Poor")))

#Op_Environment
prob.op_environment = matrix(c(0.5,0.5), ncol = 2, 
                             dimnames = list(NULL,c("Good","Poor")))

#System_Security
prob.system_security = matrix(c(0.5,0.5), ncol = 2, 
                              dimnames = list(NULL,c("Good","Poor")))

#ReplicationAndRefreshment
prob.replication_refreshment = matrix(c(0.5,0.5), ncol = 2, 
                                      dimnames = list(NULL,c("Yes","No")))

#Storage_Medium
prob.storage_medium = matrix(c(0.05, 0.90, 0.05), ncol = 3, 
                             dimnames = list(NULL,c("A","B","C")))

#Checksum
prob.checksum = matrix(c(0.05, 0.90, 0.05), ncol = 3, 
                       dimnames = list(NULL,c("Yes","No_but_generated","No")))

#Physical_Distaster
prob.physical_disaster = matrix(c(0.01,0.99), ncol = 2, 
                                dimnames = list(NULL,c("High","Low")))

#Info_Management
prob.info_management = matrix(c(0.6,0.4), ncol = 2, 
                              dimnames = list(NULL,c("Yes","No")))

#Conditions_of_Use
prob.conditions_of_use = array(c(0.5, 0.5, 0.95, 0.05, 0.95, 0.05), dim = c(2, 3),
                               dimnames = list("Conditions_of_Use" = c("Yes","No"), "Digital_Object" = c("Born_digital","Digitised","Surrogate")))

#Content_Metadata
prob.content_metadata = array(c(0.2, 0.3, 0.5, 0.5, 0.4, 0.1, 0.7, 0.25, 0.05), dim = c(3, 3),
                              dimnames = list("Content_Metadata" = c("Yes","Can_acquire","No"), "Digital_Object" = c("Born_digital","Digitised","Surrogate")))

#File_Format
prob.file_format = array(c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
                         dim = c(4, 3),
                         dimnames = list("File_Format" = c("Ubiquous_open","Ubiquous_notopen","Notubiquous_open","Notubiquous_notopen"), 
                                         "Digital_Object" = c("Born_digital","Digitised","Surrogate")))

#Identity
prob.identity = array(c(1, 0, 0.9, 0.1, 0, 1, 0, 1, 0,1, 0, 1), dim = c(2, 3, 2),
                      dimnames = list("Identity" = c("Yes","No"), "Content_Metadata" = c("Yes","Can_acquire","No"), "Info_Management" = c("Yes","No")))

#Intellectual_Control
prob.intellectual_control = array(c(1, 0, 0, 1, 0, 1, 0, 1), dim = c(2, 2, 2),
                                  dimnames = list("Intellectual_Control" = c("Yes","No"), "Identity" = c("Yes","No"), "Conditions_of_Use" = c("Yes","No")))

#Tech_Metadata
prob.tech_metadata = array(c(0.5, 0.5, 0.8, 0.2, 0.8, 0.2, 0.3, 0.7, 0.5, 0.5, 0.5, 0.5), dim = c(2, 3, 2),
                           dimnames = list("Tech_Metadata" = c("Yes","No"), "Digital_Object" = c("Born_digital","Digitised","Surrogate"), "Technical_Skills" = c("Good","Poor")))

#Tools_to_Render
prob.tools_to_render = array(c(1, 0, 0.9, 0.1, 0.5, 0.5, 0.1, 0.9, 0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0, 1), dim = c(2, 4, 2),
                             dimnames = list("Tools_to_Render" = c("Yes","No"), 
                                             "File_Format" = c("Ubiquous_open","Ubiquous_notopen","Notubiquous_open","Notubiquous_notopen"), 
                                             "Technical_Skills" = c("Good","Poor")))

#Obscolescence
prob.obsolescence = array(c(0.5, 0.5, 0.8, 0.2, 0.95, 0.05, 0.3, 0.7, 0.5, 0.5, 0.95, 0.05), dim = c(2, 3, 2),
                          dimnames = list("Obsolescence" = c("Yes","No"), "Storage_Medium" = c("A","B","C"), "Technical_Skills" = c("Good","Poor")))

#Integrity
prob.integrity = array(c(1, 0, 0.5, 0.5, 0.9, 0.1, 0.4, 0.6, 0.9, 0.1, 0.4, 0.6, 0.5, 0.5, 0.2, 0.8, 0, 1, 0, 1, 0, 1, 0, 1),dim = c(2, 2, 2, 3),
                       dimnames = list("Integrity" = c("Yes","No"),"Info_Management" = c("Yes","No"), "System_Security" = c("Good","Poor"), "Checksum" = c("Yes","No_but_generated","No")))

#Bit_Preservation
prob.bit_preservation = array(c(0, 1, 0, 1, 0.9, 0.1, 0.1, 0.9, 0, 1, 0, 1, 1, 0, 0.8, 0.2), dim = c(2, 2, 2, 2),
                              dimnames = list("Bit_Preservation" = c("Yes","No"), "Integrity" = c("Yes","No"),"Obsolescence" = c("Yes","No"), "Storage_Life" = c("<Year",">Year")))

#Renderability
prob.renderability = array(c(1, 0, 0, 1, 0, 1, 0, 1, 0.2, 0.8, 0, 1, 0, 1, 0, 1), dim = c(2, 2, 2, 2),
                           dimnames = list("Renderability" = c("Yes","No"),"Bit_Preservation" = c("Yes","No"), "Tools_to_Render" = c("Yes","No"),"Tech_Metadata" = c("Yes","No")))

#Storage_Life
prob.storage_life = array(c(0, 1, 0, 1, 0.8, 0.2, 0.5, 0.5, 0, 1, 0, 1, 0.9, 0.1, 0.8, 0.2, 0, 1, 0, 1, 0.2, 0.8, 0.1, 0.9,
                            0, 1, 0, 1, 0.7, 0.3, 0.5, 0.5, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
                          dim = c(2, 2, 2, 2, 3),
                          dimnames = list("Storage_Life" = c("<Year",">Year"),"Physical_Disaster" = c("High","Low"), "ReplicationAndRefreshment" = c("Yes","No"), "Op_Environment" = c("Good","Poor"),  "Storage_Medium" = c("A","B","C")))


model.fit <- custom.fit(TNA.dag,
                        dist=list("Op_Environment"=prob.op_environment,
                                  "Storage_Medium"=prob.storage_medium,
                                  "ReplicationAndRefreshment"=prob.replication_refreshment,
                                  "System_Security"=prob.system_security,
                                  "Digital_Object"=prob.digital_object,
                                  "Content_Metadata"=prob.content_metadata,
                                  "Tech_Metadata"=prob.tech_metadata,
                                  "Checksum"=prob.checksum,
                                  "File_Format"=prob.file_format,
                                  "Identity"=prob.identity,
                                  "Intellectual_Control"=prob.intellectual_control,
                                  "Tools_to_Render"=prob.tools_to_render,
                                  "Integrity"=prob.integrity,
                                  "Conditions_of_Use"=prob.conditions_of_use,
                                  "Renderability"=prob.renderability,
                                  "Bit_Preservation"=prob.bit_preservation,
                                  "Obsolescence"=prob.obsolescence,
                                  "Physical_Disaster"=prob.physical_disaster,
                                  "Info_Management"=prob.info_management,
                                  "Storage_Life"=prob.storage_life,
                                  "Technical_Skills"=prob.technical_skills))

write.bif("Model_copy.bif", model.fit)

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


