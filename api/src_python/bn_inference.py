"""
Pure-Python Bayesian Network inference — no pgmpy, numpy, scipy or pandas.

Provides:
  - BIF file parser
  - Factor: a probability table over a set of discrete variables
  - BayesianNetwork: load, modify CPDs, query marginals via Variable Elimination
"""

import re
from itertools import product as cartesian_product


# ─────────────────────────────────────────────────────────────────────────────
# Factor
# ─────────────────────────────────────────────────────────────────────────────

class Factor:
    """A probability table over an ordered set of discrete variables.

    ``values`` maps ``tuple[int, ...]`` of state indices to probability floats.
    Index order matches the order of ``variables``.
    """

    __slots__ = ("variables", "state_names", "cardinalities", "values")

    def __init__(self, variables: list, state_names: dict, values: dict):
        self.variables = list(variables)
        self.state_names = {v: list(state_names[v]) for v in variables}
        self.cardinalities = [len(state_names[v]) for v in variables]
        self.values = dict(values)

    def __repr__(self):
        return f"Factor({self.variables})"


def _multiply(f1: Factor, f2: Factor) -> Factor:
    """Return the point-wise product of two factors."""
    # Union of variables preserving order
    all_vars = list(f1.variables)
    for v in f2.variables:
        if v not in all_vars:
            all_vars.append(v)

    all_states = {v: (f1.state_names.get(v) or f2.state_names[v]) for v in all_vars}
    cards = [len(all_states[v]) for v in all_vars]

    f1_idx = [all_vars.index(v) for v in f1.variables]
    f2_idx = [all_vars.index(v) for v in f2.variables]

    result = {}
    for combo in cartesian_product(*[range(c) for c in cards]):
        k1 = tuple(combo[i] for i in f1_idx)
        k2 = tuple(combo[i] for i in f2_idx)
        p = f1.values.get(k1, 0.0) * f2.values.get(k2, 0.0)
        if p != 0.0:
            result[combo] = p

    return Factor(all_vars, all_states, result)


def _marginalize(f: Factor, var: str) -> Factor:
    """Return a new factor with ``var`` summed out."""
    idx = f.variables.index(var)
    new_vars = [v for v in f.variables if v != var]
    new_states = {v: f.state_names[v] for v in new_vars}
    result: dict = {}
    for key, p in f.values.items():
        new_key = key[:idx] + key[idx + 1:]
        result[new_key] = result.get(new_key, 0.0) + p
    return Factor(new_vars, new_states, result)


def _normalize(f: Factor) -> Factor:
    total = sum(f.values.values())
    if total == 0:
        return f
    return Factor(f.variables, f.state_names, {k: v / total for k, v in f.values.items()})


# ─────────────────────────────────────────────────────────────────────────────
# BayesianNetwork
# ─────────────────────────────────────────────────────────────────────────────

class BayesianNetwork:
    """Discrete Bayesian Network with Variable Elimination inference."""

    def __init__(self):
        self._state_names: dict = {}   # {varname: [state, ...]}
        self._parents: dict = {}       # {varname: [parent, ...]}
        self._cpd_factors: dict = {}   # {varname: Factor}

    def nodes(self) -> list:
        return list(self._state_names.keys())

    def get_state_names(self, var: str) -> list:
        return list(self._state_names[var])

    def get_parents(self, var: str) -> list:
        return list(self._parents.get(var, []))

    def get_cpd(self, var: str) -> Factor:
        return self._cpd_factors[var]

    def set_cpd(self, var: str, factor: Factor) -> None:
        self._cpd_factors[var] = factor

    def query(self, query_var: str) -> dict:
        """Return the marginal probability distribution for ``query_var``
        as ``{state_name: probability}`` using Variable Elimination."""
        factors = list(self._cpd_factors.values())
        all_vars = {v for f in factors for v in f.variables}
        eliminate = list(all_vars - {query_var})
        eliminate = _greedy_order(eliminate, factors)

        for var in eliminate:
            relevant = [f for f in factors if var in f.variables]
            rest = [f for f in factors if var not in f.variables]
            combined = relevant[0]
            for f in relevant[1:]:
                combined = _multiply(combined, f)
            combined = _marginalize(combined, var)
            factors = rest + [combined]

        result = factors[0]
        for f in factors[1:]:
            result = _multiply(result, f)
        result = _normalize(result)

        states = self._state_names[query_var]
        return {states[i]: result.values.get((i,), 0.0) for i in range(len(states))}


def _greedy_order(variables: list, factors: list) -> list:
    """Min-degree greedy elimination ordering (sufficient for small networks)."""
    adj = {v: set() for v in variables}
    for f in factors:
        fvars = set(f.variables)
        for v in f.variables:
            if v in adj:
                adj[v] |= (fvars - {v}) & set(adj)

    remaining = list(variables)
    order = []
    while remaining:
        remaining_set = set(remaining)
        best = min(remaining, key=lambda v: len(adj.get(v, set()) & remaining_set))
        order.append(best)
        remaining.remove(best)
        nbrs = adj.get(best, set()) & remaining_set
        for u in nbrs:
            adj[u] = (adj[u] | nbrs) - {u}
    return order


# ─────────────────────────────────────────────────────────────────────────────
# Factor construction helpers
# ─────────────────────────────────────────────────────────────────────────────

def make_root_factor(var: str, states: list, probs: list) -> Factor:
    """Build a root-node Factor from a list of probabilities."""
    return Factor([var], {var: states}, {(i,): float(p) for i, p in enumerate(probs)})


def make_conditional_factor(
    var: str,
    var_states: list,
    parents: list,
    parent_states_map: dict,
    rows: list,
) -> Factor:
    """Build a conditional Factor.

    ``rows`` is a list of ``(parent_state_combo_tuple, probs_list)`` where
    ``parent_state_combo_tuple`` contains one state name per parent and
    ``probs_list`` contains P(var=si | combo) for each state si in var_states.
    """
    all_vars = [var] + list(parents)
    all_states = {var: var_states, **{p: parent_states_map[p] for p in parents}}
    values: dict = {}
    for combo_names, probs in rows:
        pidx = tuple(parent_states_map[p].index(s) for p, s in zip(parents, combo_names))
        for i, p in enumerate(probs):
            values[(i,) + pidx] = float(p)
    return Factor(all_vars, all_states, values)


# ─────────────────────────────────────────────────────────────────────────────
# BIF file parser
# ─────────────────────────────────────────────────────────────────────────────

def _tokenize(content: str) -> list:
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    return re.findall(r'[\w.+\-]+|[{}()\[\]|,;]', content)


def parse_bif(filepath: str) -> BayesianNetwork:
    """Parse a BIF-format Bayesian network file and return a BayesianNetwork."""
    with open(filepath, encoding='utf-8') as fh:
        content = fh.read()

    tokens = _tokenize(content)
    pos = [0]

    def peek():
        return tokens[pos[0]] if pos[0] < len(tokens) else None

    def consume():
        t = tokens[pos[0]]
        pos[0] += 1
        return t

    def expect(val):
        t = consume()
        if t != val:
            raise ValueError(f"BIF parse error: expected {val!r}, got {t!r}")
        return t

    bn = BayesianNetwork()

    while pos[0] < len(tokens):
        t = consume()

        if t == 'network':
            consume()       # name
            expect('{')
            expect('}')

        elif t == 'variable':
            name = consume()
            expect('{')
            expect('type')
            expect('discrete')
            expect('[')
            consume()       # cardinality (we'll infer from state list)
            expect(']')
            expect('{')
            states = []
            while peek() != '}':
                states.append(consume())
                if peek() == ',':
                    consume()
            expect('}')
            expect(';')
            expect('}')
            bn._state_names[name] = states

        elif t == 'probability':
            expect('(')
            var = consume()
            parents = []
            if peek() == '|':
                consume()
                while peek() != ')':
                    parents.append(consume())
                    if peek() == ',':
                        consume()
            expect(')')
            expect('{')
            bn._parents[var] = parents

            if not parents:
                expect('table')
                probs = []
                while peek() != ';':
                    probs.append(float(consume()))
                    if peek() == ',':
                        consume()
                expect(';')
                expect('}')
                bn._cpd_factors[var] = make_root_factor(var, bn._state_names[var], probs)

            else:
                parent_states_map = {p: bn._state_names[p] for p in parents}
                rows = []
                while peek() == '(':
                    expect('(')
                    combo = []
                    while peek() != ')':
                        combo.append(consume())
                        if peek() == ',':
                            consume()
                    expect(')')
                    probs = []
                    while peek() != ';':
                        probs.append(float(consume()))
                        if peek() == ',':
                            consume()
                    expect(';')
                    rows.append((tuple(combo), probs))
                expect('}')
                bn._cpd_factors[var] = make_conditional_factor(
                    var, bn._state_names[var], parents, parent_states_map, rows,
                )

    return bn
