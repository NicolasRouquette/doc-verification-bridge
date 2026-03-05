/-
  Entry point for experiments executable.
-/
import Experiments.ExperimentsCore

def main (args : List String) : IO UInt32 :=
  experimentsMain args
