# CS305-Assignment4
# Semantic Analyzer for LCD using Scheme

Overview
This project focuses on implementing a semantic analyzer for the LCD language using Scheme. The analyzer enforces semantic rules on LCD programs, which are represented as nested lists in Scheme.

📌 Key Features
functions:

find-undeclared-identifiers

Detects identifiers used without being declared.
find-multiple-declarations

Identifies identifiers declared multiple times.
check-identifier-usage

Ensures inputs are used, nodes/outputs are assigned, and assignments are unique.
check-inputs-in-evaluation

Checks inputs for correct and unique assignments during evaluation.
check-incorrect-assignments

Validates that inputs, nodes, and outputs are assigned correctly in their respective blocks.
🛠️ Semantic Rules
Rule 1: No undeclared identifiers.
Rule 2: No multiple declarations.
Rule 3: Every input must be used.
Rule 4: Nodes and outputs must be assigned.
Rule 5: No multiple assignments to nodes/outputs.
Rule 6: Inputs must be assigned in every evaluation.
Rule 7: No multiple assignments to inputs in evaluations.
Rule 8: Inputs cannot be assigned in circuit design.
Rule 9: Nodes/outputs cannot be assigned in evaluations
