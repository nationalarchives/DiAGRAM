"""
Validate incoming JSON data against the expected DiAGRAM schema.
"""

from nodes import REVERSE_NODE_MAP, REVERSE_USER_NODE_MAP

_REQUIRED_KEYS = {
    "model_name", "scenario", "notes",
    "intellectual_control", "renderability",
    "response", "advanced", "is_advanced",
}

_VALID_PHYSICAL_DISASTER = {"Very Low", "Low", "Medium", "High"}

_VALID_TECHNICAL_SKILLS = {"None", "Basic", "Intermediate", "Advanced"}

_VALID_SYSTEM_SECURITY = [
    {"No", "Cyber Essentials", "Cyber Essentials Plus", "ISO 27001"},
    {"No test", "Critical issues outstanding", "Severe issues outstanding",
     "None, or only minor issues outstanding"},
    {"Not achieved", "Level 1", "Level 2", "Level 3", "Level 4"},
    {"No", "Yes"},
]

_VALID_INFO_MANAGEMENT = [
    {"Not achieved", "Level 1", "Level 2", "Level 3", "Level 4"},
    {"Not achieved", "Level 1", "Level 2", "Level 3", "Level 4"},
    {"Minimal awareness", "Awareness", "Basic", "Managed", "Optimized"},
]

_VALID_OP_ENV_BOOL = {"Yes", "No", "Not Applicable - we have copies offsite"}


def _validate_node(node_key: str, value) -> bool:
    """Validate a single node's response value. Returns True if valid."""
    try:
        if node_key == "Technical_Skills":
            if not isinstance(value, list) or len(value) != 10:
                return False
            return all(v in _VALID_TECHNICAL_SKILLS for v in value)

        if node_key == "Physical_Disaster":
            return isinstance(value, str) and value in _VALID_PHYSICAL_DISASTER

        if node_key == "System_Security":
            if not isinstance(value, (list, dict)) or len(value) != 4:
                return False
            items = list(value.values()) if isinstance(value, dict) else value
            return all(
                isinstance(items[i], str) and items[i] in _VALID_SYSTEM_SECURITY[i]
                for i in range(4)
            )

        if node_key in ("Checksum", "Digital_Object", "Storage_Medium"):
            if not isinstance(value, list) or len(value) != 3:
                return False
            return all(isinstance(v, (int, float)) for v in value)

        if node_key == "Info_Management":
            if not isinstance(value, (list, dict)) or len(value) != 3:
                return False
            items = list(value.values()) if isinstance(value, dict) else value
            r1, r2, r3 = items[0], items[1], items[2]
            if not (isinstance(r1, str) and r1 in _VALID_INFO_MANAGEMENT[0]):
                return False
            if not (isinstance(r2, str) and r2 in _VALID_INFO_MANAGEMENT[1]):
                return False
            if isinstance(r3, list):
                if len(r3) != 2:
                    return False
                return all(v in _VALID_INFO_MANAGEMENT[2] for v in r3)
            return isinstance(r3, str) and r3 in _VALID_INFO_MANAGEMENT[2]

        if node_key == "Op_Environment":
            if not isinstance(value, (list, dict)) or len(value) != 2:
                return False
            items = list(value.values()) if isinstance(value, dict) else value
            r1, r2 = items[0], items[1]
            if not isinstance(r1, (int, float)):
                return False
            if not (isinstance(r2, str) and r2 in _VALID_OP_ENV_BOOL):
                return False
            return True

        if node_key == "Rep_and_Refresh":
            if not isinstance(value, (list, dict)) or len(value) != 2:
                return False
            items = list(value.values()) if isinstance(value, dict) else value
            return all(isinstance(v, (int, float)) for v in items)

    except Exception:
        return False

    return True  # unknown node — pass through


def _validate_keys(obj: dict) -> bool:
    """Check that all required top-level keys are present."""
    return _REQUIRED_KEYS.issubset(obj.keys())


def _validate_simple_nodes(obj: dict) -> bool:
    response = obj.get("response", {})
    if not isinstance(response, dict):
        return False
    # Keys must be valid user node names
    valid_keys = set(REVERSE_USER_NODE_MAP.values())  # the node keys
    if not all(k in valid_keys for k in response.keys()):
        return False
    return all(_validate_node(k, v) for k, v in response.items())


def _validate_advanced_nodes(obj: dict) -> bool:
    advanced = obj.get("advanced")
    if not advanced:
        return True  # no advanced portion is fine
    if not isinstance(advanced, dict):
        return False
    valid_keys = set(REVERSE_NODE_MAP.values())
    return all(k in valid_keys for k in advanced.keys())


def _validate_object(obj: dict) -> bool:
    if not isinstance(obj, dict):
        return False
    if not _validate_keys(obj):
        return False
    if not _validate_simple_nodes(obj):
        return False
    if not _validate_advanced_nodes(obj):
        return False
    return True


def validate(parsed_json) -> bool:
    """Return True if parsed_json (list or dict) passes all validation checks."""
    if isinstance(parsed_json, list):
        return all(_validate_object(o) for o in parsed_json)
    return _validate_object(parsed_json)
