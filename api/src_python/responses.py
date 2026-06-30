"""
Parse and classify incoming JSON request bodies into structured response objects.
"""

from nodes import USER_NODES, REVERSE_NODE_MAP


def _is_array(parsed_json) -> bool:
    """Return True if parsed_json is a JSON array (list without string keys)."""
    return isinstance(parsed_json, list)


def _unpack_json(obj):
    """
    Recursively convert unnamed lists (JSON arrays) to plain Python lists,
    keeping named dicts as dicts.  This mirrors R's unpack_json behaviour.
    """
    if isinstance(obj, dict):
        return {k: _unpack_json(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [_unpack_json(v) for v in obj]
    return obj


def _is_advanced_flag(obj) -> bool:
    return bool(obj.get("is_advanced", False))


def advanced_flags(parsed_json) -> list:
    """Return a list of booleans indicating which objects are advanced models."""
    if _is_array(parsed_json):
        return [_is_advanced_flag(o) for o in parsed_json]
    return [_is_advanced_flag(parsed_json)]


def _create_simple_responses(obj: dict) -> dict:
    """Build a simple_responses object from a single parsed JSON object."""
    response = _unpack_json(obj.get("response", {}))
    return {
        "type": "simple_responses",
        "data": {"response": response},
        "model_name": obj.get("model_name") or obj.get("modelname"),
        "scenario": obj.get("scenario", ""),
        "notes": obj.get("notes", ""),
        "intellectual_control": obj.get("intellectual_control"),
        "renderability": obj.get("renderability"),
    }


def _create_advanced_responses(obj: dict) -> dict:
    """Build an advanced_responses object from a single parsed JSON object."""
    response = _unpack_json(obj.get("response", {}))
    advanced = _unpack_json(obj.get("advanced", {})) or {}

    # Nodes that appear in both simple and advanced: advanced wins
    overlapping = set(response.keys()) & set(advanced.keys())
    simple_part = {k: v for k, v in response.items() if k not in overlapping}

    return {
        "type": "advanced_responses",
        "data": {
            "simple": {"response": simple_part},
            "advanced": advanced,
        },
        "model_name": obj.get("model_name") or obj.get("modelname"),
        "scenario": obj.get("scenario", ""),
        "notes": obj.get("notes", ""),
        "intellectual_control": obj.get("intellectual_control"),
        "renderability": obj.get("renderability"),
    }


def _extract_single(obj: dict) -> dict:
    if _is_advanced_flag(obj):
        return _create_advanced_responses(obj)
    return _create_simple_responses(obj)


def extract_responses(parsed_json) -> list:
    """Return a list of response objects (simple or advanced) from parsed JSON."""
    if _is_array(parsed_json):
        return [_extract_single(o) for o in parsed_json]
    return [_extract_single(parsed_json)]


def is_array(parsed_json) -> bool:
    return _is_array(parsed_json)
