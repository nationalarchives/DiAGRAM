# keys for the nodes a user can edit for a simple model
# sorted by order they appear in front end
.user_nodes = c(
  "Digital_Object", "Storage_Medium",
  "Rep_and_Refresh", "Op_Environment",
  "Physical_Disaster", "Checksum",
  "System_Security", "Info_Management",
  "Technical_Skills"
)

# map node keys to human readable values
.node_map = stats::setNames(
  c(
    "Operating Environment", "Integrity",
    "System Security", "Information Management",
    "Storage Medium", "Replication and Refreshment",
    "Digital Object", "Content Metadata",
    "Technical Metadata", "File format", "Checksum",
    "Obsolescence", "Tools to Render", "Intellectual Control",
    "Conditions of Use", "Renderability", "Bit Preservation",
    "Identity", "Physical Disaster", "Storage Life", "Technical Skills"
  ),
  c(
    "Op_Environment", "Integrity",
    "System_Security", "Info_Management",
    "Storage_Medium", "Rep_and_Refresh",
    "Digital_Object", "Content_Metadata",
    "Tech_Metadata", "File_Format", "Checksum",
    "Obsolescence", "Tools_to_Render", "Intellectual_Control",
    "Conditions_of_Use", "Renderability", "Bit_Preservation",
    "Identity", "Physical_Disaster", "Storage_Life", "Technical_Skills"
  )
)

# map user node keys to human readable values
.user_node_map = .node_map[.user_nodes]

# map back from human readable values to keys
.reverse_node_map = stats::setNames(names(.node_map), .node_map)

# map user nodes back from human readable values to keys
.reverse_user_node_map = .reverse_node_map[match(.user_nodes, .reverse_node_map)]
