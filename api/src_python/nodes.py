# Keys for nodes the user can edit in the simple model,
# sorted by order they appear in the front end.
USER_NODES = [
    "Digital_Object", "Storage_Medium",
    "Rep_and_Refresh", "Op_Environment",
    "Physical_Disaster", "Checksum",
    "System_Security", "Info_Management",
    "Technical_Skills",
]

# Map node keys to human-readable values
NODE_MAP = {
    "Op_Environment": "Operating Environment",
    "Integrity": "Integrity",
    "System_Security": "System Security",
    "Info_Management": "Information Management",
    "Storage_Medium": "Storage Medium",
    "Rep_and_Refresh": "Replication and Refreshment",
    "Digital_Object": "Digital Object",
    "Content_Metadata": "Content Metadata",
    "Tech_Metadata": "Technical Metadata",
    "File_Format": "File format",
    "Checksum": "Checksum",
    "Obsolescence": "Obsolescence",
    "Tools_to_Render": "Tools to Render",
    "Intellectual_Control": "Intellectual Control",
    "Conditions_of_Use": "Conditions of Use",
    "Renderability": "Renderability",
    "Bit_Preservation": "Bit Preservation",
    "Identity": "Identity",
    "Physical_Disaster": "Physical Disaster",
    "Storage_Life": "Storage Life",
    "Technical_Skills": "Technical Skills",
}

# Map human-readable values back to node keys
REVERSE_NODE_MAP = {v: k for k, v in NODE_MAP.items()}

# Subset of NODE_MAP for user-editable nodes
USER_NODE_MAP = {k: NODE_MAP[k] for k in USER_NODES}

# Reverse map for user-editable nodes
REVERSE_USER_NODE_MAP = {v: k for k, v in USER_NODE_MAP.items()}

# For each user node, the state names as stored in the Bayesian network model
MODEL_PROB_NAMES = {
    "Technical_Skills": ["Good", "Poor"],
    "System_Security": ["Good", "Poor"],
    "Checksum": ["Yes", "Self_Generated", "No"],
    "Info_Management": ["Sufficient", "Insufficient"],
    "Digital_Object": ["Born_digital", "Digitised", "Surrogate"],
    "Storage_Medium": ["A", "B", "C"],
    "Rep_and_Refresh": ["Good", "Poor"],
    "Op_Environment": ["Yes", "No"],
    "Physical_Disaster": ["Yes", "No"],
}
