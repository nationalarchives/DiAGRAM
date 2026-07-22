"""
Bayesian-network model loading, CPT modification and inference.

Uses the built-in bn_inference module (pure Python, no pgmpy/numpy/scipy).
"""

import os
from copy import deepcopy
from itertools import product as cartesian_product

from bn_inference import BayesianNetwork, Factor, parse_bif
from nodes import USER_NODES, NODE_MAP
from to_numeric import to_numeric_simple
from to_probability import (
    numeric_to_probability_simple,
    numeric_to_probability_advanced,
)

_MODEL_PATH = os.path.join(
    os.path.dirname(__file__), "assets", "model", "model.bif"
)

_base_model: BayesianNetwork | None = None


def load_default_model() -> BayesianNetwork:
    """Load (or return cached) the default Bayesian network from the BIF file."""
    global _base_model
    if _base_model is None:
        _base_model = parse_bif(_MODEL_PATH)
    return _base_model


def _apply_cpds(model: BayesianNetwork, cpds: dict) -> None:
    for node_name, factor in cpds.items():
        model.set_cpd(node_name, factor)


def _factor_to_json(factor: Factor) -> dict:
    """Convert a Factor to the columnar JSON format the front-end expects.

    Root node  → {state: prob, ...}   (scalar, matching R's auto_unbox=TRUE behaviour)
    Conditional → {parent: [...states...], ..., state: [...probs...], ...}
    """
    var = factor.variables[0]
    parents = factor.variables[1:]
    var_states = factor.state_names[var]

    if not parents:
        # Return scalars to match R's jsonlite auto_unbox=TRUE serialisation
        return {s: factor.values.get((i,), 0.0) for i, s in enumerate(var_states)}

    # Enumerate parent combinations (same cartesian-product order as pgmpy)
    parent_state_lists = [range(len(factor.state_names[p])) for p in parents]
    all_combos = list(cartesian_product(*parent_state_lists))

    result = {}
    for pi, parent in enumerate(parents):
        result[parent] = [factor.state_names[parent][c[pi]] for c in all_combos]
    for si, state in enumerate(var_states):
        result[state] = [factor.values.get((si,) + c, 0.0) for c in all_combos]

    return result


def model_to_json(model: BayesianNetwork) -> dict:
    """Serialise all node CPDs to JSON-compatible dicts."""
    return {var: _factor_to_json(model.get_cpd(var)) for var in model.nodes()}


def score_model_(responses: dict, base_model: BayesianNetwork = None) -> dict:
    """Score the model from a single parsed response object.

    Returns:
        {
            "intellectual_control": float (0–100),
            "renderability": float (0–100),
            "nodes": dict,
        }
    """
    if base_model is None:
        base_model = load_default_model()

    model = deepcopy(base_model)
    resp_type = responses.get("type")

    if resp_type == "simple_responses":
        response_data = responses["data"]["response"]
        numeric = to_numeric_simple(response_data)
        cpds = numeric_to_probability_simple(numeric, base_model)
        _apply_cpds(model, cpds)

    elif resp_type == "advanced_responses":
        simple_data = responses["data"]["simple"]["response"]
        advanced_data = responses["data"]["advanced"]
        numeric = to_numeric_simple(simple_data)
        cpds = numeric_to_probability_advanced(numeric, advanced_data, base_model)
        _apply_cpds(model, cpds)

    else:
        raise ValueError(f"Unknown response type: {resp_type!r}")

    nodes_json = model_to_json(model)

    ic_dist = model.query("Intellectual_Control")
    r_dist = model.query("Renderability")

    return {
        "intellectual_control": round(100.0 * ic_dist.get("Yes", 0.0), 4),
        "renderability": round(100.0 * r_dist.get("Yes", 0.0), 4),
        "nodes": nodes_json,
    }
