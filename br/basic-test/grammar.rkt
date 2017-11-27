#lang brag
program : sum*
sum : [sum "+"] mult
mult : [mult "*"] var
@var : "a" | "b" | "c" | "d"