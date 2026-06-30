"""
Convert numeric response values to Factor objects suitable for replacing
node priors/CPTs in the BayesianNetwork (pure Python, no pgmpy/numpy).
"""

from itertools import product as cartesian_product

from bn_inference import Factor, BayesianNetwork, make_root_factor, make_conditional_factor
from nodes import MODEL_PROB_NAMES


def _scalar_to_prob(value, states: list) -> list:
    """Convert a scalar 0–100 value (or list) to a normalised probability list."""
    if isinstance(value, list):
        total = sum(value)
        if total == 0:
            return [1.0 / len(value)] * len(value)
        return [v / total for v in value]
    p = max(0.0, min(1.0, value / 100.0))
    return [p, 1.0 - p]


def numeric_to_probability_simple(numeric_responses: dict, base_model: BayesianNetwork = None) -> dict:
    """Convert {node_name: numeric} → {node_name: Factor} for user-editable root nodes."""
    cpds = {}
    for node, value in numeric_responses.items():
        if node not in MODEL_PROB_NAMES:
            continue
        states = MODEL_PROB_NAMES[node]
        probs = _scalar_to_prob(value, states)
        cpds[node] = make_root_factor(node, states, probs)
    return cpds


def build_cpd_from_advanced_data(node_name: str, json_data: dict, base_model: BayesianNetwork) -> Factor:
    """Build a Factor from advanced-model JSON CPT data.

    Root node JSON: {state_name: probability}
    Conditional JSON: {parent_col: [states...], ..., outcome_col: [probs...], ...}
    """
    all_nodes = set(base_model.nodes())

    def norm(s):
        return str(s).replace(" ", "_")

    # Identify parent columns (lists of strings) vs outcome columns (lists of numbers)
    parent_keys = []
    state_keys = []
    for k, v in json_data.items():
        if isinstance(v, list):
            if v and isinstance(v[0], str):
                parent_keys.append(k)
            else:
                state_keys.append(k)
        elif isinstance(v, (int, float)):
            state_keys.append(k)

    node_states = base_model.get_state_names(node_name)

    if not parent_keys:
        # Root node: {state: prob}
        probs = []
        for state in node_states:
            if state in json_data:
                probs.append(float(json_data[state]))
            else:
                space_state = state.replace("_", " ")
                if space_state in json_data:
                    probs.append(float(json_data[space_state]))
                else:
                    raise ValueError(f"State '{state}' not found in advanced data for '{node_name}'")
        total = sum(probs)
        if total > 0:
            probs = [p / total for p in probs]
        return make_root_factor(node_name, node_states, probs)

    # Conditional node
    parent_model_names = []
    for pk in parent_keys:
        normalized = norm(pk)
        if normalized in all_nodes and normalized != node_name:
            parent_model_names.append(normalized)
        elif pk in all_nodes and pk != node_name:
            parent_model_names.append(pk)
        else:
            raise ValueError(f"Parent column '{pk}' for node '{node_name}' not found in model nodes")

    parent_states_map = {pm: base_model.get_state_names(pm) for pm in parent_model_names}

    # Build lookup: tuple(parent_state_names) → {state_key: prob}
    n_rows = len(json_data[state_keys[0]])
    lookup = {}
    for i in range(n_rows):
        combo = tuple(norm(json_data[pk][i]) for pk in parent_keys)
        lookup[combo] = {sk: float(json_data[sk][i]) for sk in state_keys}

    # Map state_keys to node_states names
    state_key_to_state = {}
    for sk in state_keys:
        norm_sk = norm(sk)
        if norm_sk in node_states:
            state_key_to_state[sk] = norm_sk
        elif sk in node_states:
            state_key_to_state[sk] = sk
        else:
            raise ValueError(f"Outcome column '{sk}' not recognised as a state of '{node_name}'")

    rows = []
    for combo in cartesian_product(*[parent_states_map[pm] for pm in parent_model_names]):
        probs_for_combo = lookup.get(combo)
        if probs_for_combo is None:
            raise ValueError(f"Parent combo {combo} not found in advanced data for '{node_name}'")
        probs = [0.0] * len(node_states)
        for sk, prob in probs_for_combo.items():
            state = state_key_to_state[sk]
            probs[node_states.index(state)] = prob
        rows.append((combo, probs))

    return make_conditional_factor(node_name, node_states, parent_model_names, parent_states_map, rows)


def numeric_to_probability_advanced(
    simple_numeric: dict, advanced_data: dict, base_model: BayesianNetwork
) -> dict:
    """Build Factor dict for an advanced model, merging simple + advanced CPDs."""
    simple_cpds = numeric_to_probability_simple(simple_numeric, base_model)
    advanced_cpds = {}
    for node_name, node_data in advanced_data.items():
        if isinstance(node_data, list):
            continue
        try:
            advanced_cpds[node_name] = build_cpd_from_advanced_data(node_name, node_data, base_model)
        except Exception as exc:
            raise ValueError(f"Error building CPD for '{node_name}': {exc}") from exc
    return {**simple_cpds, **advanced_cpds}
