#lang brag
santa-program : (left-paren | right-paren)*
closed-paren  : left-paren right-paren | right-paren left-paren
left-paren    : LEFT
right-paren   : RIGHT
