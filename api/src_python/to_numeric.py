"""
Convert user-provided responses from the front-end form into scalar numeric
values that can then be converted into Bayesian-network probability tables.
"""

OPTION_VALS = {
    "technical_skills": {
        "None": 0, "Basic": 3, "Intermediate": 6, "Advanced": 10,
    },
    "physical_disaster": {
        "Very Low": 0.05, "Low": 0.5, "Medium": 2, "High": 5,
    },
    "system_security": [
        {
            "No": 0, "Cyber Essentials": 10,
            "Cyber Essentials Plus": 40, "ISO 27001": 70,
        },
        {
            "No test": 0, "Critical issues outstanding": 5,
            "Severe issues outstanding": 10,
            "None, or only minor issues outstanding": 15,
        },
        {
            "Not achieved": 0, "Level 1": 2,
            "Level 2": 4, "Level 3": 7, "Level 4": 10,
        },
        {
            "No": 0, "Yes": 5,
        },
    ],
    "info_management": [
        {
            "Not achieved": 0, "Level 1": 7,
            "Level 2": 14, "Level 3": 21, "Level 4": 28,
        },
        {
            "Not achieved": 0, "Level 1": 8,
            "Level 2": 16, "Level 3": 16, "Level 4": 16,
        },
        {
            "Minimal awareness": 0, "Awareness": 7,
            "Basic": 14, "Managed": 21, "Optimized": 28,
        },
    ],
}


def _to_numeric_technical_skills(res):
    """res: list of 10 skill-level strings."""
    opts = OPTION_VALS["technical_skills"]
    if not isinstance(res, list) or len(res) != 10:
        raise ValueError(f"technical_skills: expected list of 10, got {res!r}")
    if not all(r in opts for r in res):
        bad = [r for r in res if r not in opts]
        raise ValueError(f"technical_skills: invalid values {bad!r}")
    return float(sum(opts[r] for r in res))


def _to_numeric_physical_disaster(res):
    """res: single string from {'Very Low','Low','Medium','High'}."""
    opts = OPTION_VALS["physical_disaster"]
    if res not in opts:
        raise ValueError(f"physical_disaster: invalid value {res!r}")
    return float(opts[res])


def _to_numeric_system_security(res):
    """res: list/dict of 4 strings, one per security component."""
    opts = OPTION_VALS["system_security"]
    if not isinstance(res, (list, dict)) or len(res) != 4:
        raise ValueError(f"system_security: expected list of 4, got {res!r}")
    items = [_get_numbered(res, i) for i in range(4)]
    total = 0.0
    for i, (r, o) in enumerate(zip(items, opts)):
        if r not in o:
            raise ValueError(f"system_security[{i}]: invalid value {r!r}")
        total += o[r]
    return total


def _to_numeric_checksum(res):
    """res: list of 3 numeric values summing to 100."""
    if not isinstance(res, list) or len(res) != 3:
        raise ValueError(f"checksum: expected list of 3, got {res!r}")
    return [float(v) for v in res]


def _to_numeric_digital_object(res):
    """res: list of 3 numeric values."""
    if not isinstance(res, list) or len(res) != 3:
        raise ValueError(f"digital_object: expected list of 3, got {res!r}")
    return [float(v) for v in res]


def _to_numeric_storage_medium(res):
    """res: list of 3 numeric values."""
    if not isinstance(res, list) or len(res) != 3:
        raise ValueError(f"storage_medium: expected list of 3, got {res!r}")
    return [float(v) for v in res]


def _get_numbered(res, idx: int):
    """Get item at position idx from a list or a dict with '1'-based string keys."""
    if isinstance(res, list):
        return res[idx]
    # Try string key first (JSON object keys are always strings)
    v = res.get(str(idx + 1))
    if v is None:
        v = res.get(idx + 1)
    return v


def _to_numeric_info_management(res):
    """res: dict/list of 3 components; component 3 has 2 items."""
    opts = OPTION_VALS["info_management"]
    if not isinstance(res, (list, dict)) or len(res) != 3:
        raise ValueError(f"info_management: expected 3 items, got {res!r}")
    r1 = _get_numbered(res, 0)
    r2 = _get_numbered(res, 1)
    r3 = _get_numbered(res, 2)

    total = opts[0].get(r1, 0) + opts[1].get(r2, 0)
    if isinstance(r3, list):
        total += sum(opts[2].get(v, 0) for v in r3)
    else:
        total += opts[2].get(r3, 0)
    return float(total)


def _to_numeric_op_environment(res):
    """res: dict/list of 2: [percentage (0–100), Yes/No/NA string]."""
    r1 = _get_numbered(res, 0)
    r2 = _get_numbered(res, 1)
    r1 = float(r1)
    if r1 == 100 or r2 == "Yes":
        return 100.0
    return r1


def _to_numeric_rep_and_refresh(res):
    """res: dict/list of 2 numeric percentages."""
    r1 = _get_numbered(res, 0)
    r2 = _get_numbered(res, 1)
    return float(r1) * float(r2) / 100.0


_NODE_TO_NUMERIC = {
    "Technical_Skills": _to_numeric_technical_skills,
    "Physical_Disaster": _to_numeric_physical_disaster,
    "System_Security": _to_numeric_system_security,
    "Checksum": _to_numeric_checksum,
    "Digital_Object": _to_numeric_digital_object,
    "Storage_Medium": _to_numeric_storage_medium,
    "Info_Management": _to_numeric_info_management,
    "Op_Environment": _to_numeric_op_environment,
    "Rep_and_Refresh": _to_numeric_rep_and_refresh,
}


def to_numeric_simple(response_data: dict) -> dict:
    """Convert a dict of {node_name: raw_response} to numeric values.

    Returns a dict of {node_name: numeric_or_list}.
    """
    result = {}
    for node, res in response_data.items():
        if node in _NODE_TO_NUMERIC:
            result[node] = _NODE_TO_NUMERIC[node](res)
    return result
