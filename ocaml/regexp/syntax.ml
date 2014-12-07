type regular_expression =
  | Epsilon
  | Character of int
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
