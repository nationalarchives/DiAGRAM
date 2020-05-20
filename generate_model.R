
# The following script builds a dummy model which can be used for development.

# This script was built for DiAGRAM by the University of Warwick and The National
# Archive.

# @author: Stephen James Krol, Monash University, Melbourne
# @email: stephen.james.krol@gmail.com

# Updated 20/05/2020 to relfect latest model and elicitation data by HM at The National Archives UK


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



# Create prob tables for each node with TNA default values in

#Digital_Object - updated with TNA data 20/05
prob.digital_object = matrix(c(0.0473, 0.2034, 0.7493), ncol = 3, 
                             dimnames = list(NULL,c("Born_digital","Digitised","Surrogate"))) 

#Technical_Skills
prob.technical_skills = matrix(c(0.5,0.5), ncol = 2, 
                               dimnames = list(NULL,c("Good","Poor"))) 

#Op_Environment
prob.op_environment = matrix(c(0.5,0.5), ncol = 2, 
                             dimnames = list(NULL,c("Yes","No")))

#System_Security
prob.system_security = matrix(c(0.5,0.5), ncol = 2, 
                              dimnames = list(NULL,c("Good","Poor")))

#ReplicationAndRefreshment
prob.replication_refreshment = matrix(c(0.5,0.5), ncol = 2, 
                                      dimnames = list(NULL,c("Good","Poor"))) #0327 state names

#Storage_Medium
prob.storage_medium = matrix(c(0.90, 0.05, 0.05), ncol = 3,   
                             dimnames = list(NULL,c("A","B","C"))) #0327 probs

#Checksum
prob.checksum = matrix(c(0.90, 0.05, 0.05), ncol = 3, 
                       dimnames = list(NULL,c("Yes","Self_Generated","No"))) 

#Physical_Distaster
prob.physical_disaster = matrix(c(0.01,0.99), ncol = 2, 
                                dimnames = list(NULL,c("Yes","No"))) #0327 state names

#Info_Management
prob.info_management = matrix(c(0.5,0.5), ncol = 2, 
                              dimnames = list(NULL,c("Sufficient","Insufficient"))) #0327 probs

#Conditions_of_Use - updated with elicitation data 20/05
prob.conditions_of_use = array(c(0.5636, 0.4364, 0.7227, 0.2773, 0.7989, 0.2010), dim = c(2, 3),
                               dimnames = list("Conditions_of_Use" = c("Yes","No"), "Digital_Object" = c("Born_digital","Digitised","Surrogate")))

#Content_Metadata - updated with elicitation data 20/05
prob.content_metadata = array(c(0.5117, 0.4883, 0.7422, 0.2578, 0.7870, 0.2130), dim = c(2, 3),
                              dimnames = list("Content_Metadata" = c("Yes","No"), "Digital_Object" = c("Born_digital","Digitised","Surrogate"))) #0327 state numbers and probs

#File_Format - updated with TNA data 20/05
prob.file_format = array(c(0.8826, 0.1174, 1, 0, 1, 0),
                         dim = c(2, 3),
                         dimnames = list("File_Format" = c("Yes","No"),
                                         "Digital_Object" = c("Born_digital","Digitised","Surrogate"))) #0327 state names and numbers

#Identity - updated with elicitation data 20/05
prob.identity = array(c(1, 0, 0.6002, 0.3998, 0, 1, 0, 1), dim = c(2, 2, 2),
                      dimnames = list("Identity" = c("Yes","No"), "Content_Metadata" = c("Yes","No"), "Info_Management" = c("Sufficient","Insufficient"))) #0327 state names and numbers and probs

#Intellectual_Control
prob.intellectual_control = array(c(1, 0, 0, 1, 0, 1, 0, 1), dim = c(2, 2, 2),
                                  dimnames = list("Intellectual_Control" = c("Yes","No"), "Identity" = c("Yes","No"), "Conditions_of_Use" = c("Yes","No")))

#Tech_Metadata - updated with elicitation data 20/05
prob.tech_metadata = array(c(0.6911, 0.3089, 0.4062, 0.5968), dim = c(2, 2),
                           dimnames = list("Tech_Metadata" = c("Sufficient","Insufficient"), "Technical_Skills" = c("Good","Poor"))) #0327 state numbers and probs

#Tools_to_Render - updated with elicitation data 20/05
prob.tools_to_render = array(c(0.8366, 0.1634, 0.4864, 0.5136, 0.8366, 0.1634, 0, 1), dim = c(2, 2, 2),
                             dimnames = list("Tools_to_Render" = c("Yes","No"), 
                                             "File_Format" = c("Yes","No"),  "Technical_Skills" = c("Good","Poor"))) #0327 state numbers and probs

#Obscolescence - updated with elicitation data and cloud data 20/05
prob.obsolescence = array(c(0.3547, 0.6453, 0.1751, 0.8249, 0.001, 0.999, 0.7288, 0.2712, 0.3412, 0.6588, 0.001, 0.999), dim = c(2, 3, 2),
                          dimnames = list("Obsolescence" = c("Yes","No"), "Storage_Medium" = c("A","B","C"), "Technical_Skills" = c("Good","Poor"))) #0327 probs

#Integrity - updated with elicitation data 20/05 CHECK THIS
prob.integrity = array(c(1, 0, 0, 1, 0.6415, 0.3585, 0, 1, 0.9487, 0.0513, 0, 1, 0.5903, 0.4097, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),dim = c(2, 2, 2, 3),
                       dimnames = list("Integrity" = c("Yes","No"),"Info_Management" = c("Sufficient","Insufficient"), "System_Security" = c("Good","Poor"),
                                       "Checksum" = c("Yes","Self_Generated","No"))) #0327 probs

#Bit_Preservation - updated with elicitation data 20/05
prob.bit_preservation = array(c(0, 1, 0, 1, 1, 0, 0.7691, 0.2309, 0, 1, 0, 1, 0, 1, 0, 1), dim = c(2, 2, 2, 2),
                              dimnames = list("Bit_Preservation" = c("Yes","No"), "Integrity" = c("Yes","No"),
                                              "Obsolescence" = c("Yes","No"), "Storage_Life" = c("Yes","No"))) #0327 state names and probs

#Renderability - updated with elicitation data 20/05
prob.renderability = array(c(1, 0, 0, 1, 0, 1, 0, 1, 0.5638, 0.4362, 0, 1, 0, 1, 0, 1), dim = c(2, 2, 2, 2),
                           dimnames = list("Renderability" = c("Yes","No"),"Bit_Preservation" = c("Yes","No"),
                                           "Tools_to_Render" = c("Yes","No"),"Tech_Metadata" = c("Sufficient","Insufficient"))) #0327 probs

#Storage_Life - updated with elicitation data 20/05 CHECK THIS
prob.storage_life = array(c(1,0,1,0,0.8097,0.1903,0.8097,0.1903,0.5353,0.4647,1,0,0.3183,0.6817,0.8097,0.1903,
                            1,0,1,0,0.9599,0.0401,0.9599,0.0401,0.7824,0.2176,1,0,0.5335,0.4665,0.9599,0.0401,
                            1,0,1,0,1,0,1,0,1,0,1,0),
                          dim = c(2, 2, 2, 2, 3),
                          dimnames = list("Storage_Life" = c("Yes","No"),"Physical_Disaster" = c("Yes","No"),
                                          "ReplicationAndRefreshment" = c("Good","Poor"), "Op_Environment" = c("Yes","No"),
                                          "Storage_Medium" = c("A","B","C"))) #0327 state names and probs


TNA.model <- custom.fit(TNA.dag,
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


library("gRain")
graphviz.chart(TNA.model, scale=c(0.6,1), grid=TRUE, bar.col="darkgreen", strip.bg="lightskyblue", main="DiAGRAM")

#write.bif("Model.bif", TNA.model)


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


