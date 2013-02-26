module Fasta(fastacode2resname) where

import Data.Map as Map

-- NOTE: copied from hPDB temporarily, until it is released

codebook_standard_protein = [
  ('A', "ALA"),
  ('C', "CYS"),
  ('D', "ASP"),
  ('E', "GLU"),
  ('F', "PHE"),
             
  ('G', "GLY"),
  ('H', "HIS"),
  ('I', "ILE"),
  ('K', "LYS"),
  ('L', "LEU"),
             
  ('M', "MET"),
  ('N', "ASN"),
  ('P', "PRO"),
  ('Q', "GLN"),
  ('R', "ARG"),
             
  ('S', "SER"),
  ('T', "THR"),
  ('V', "VAL"),
  ('W', "TRP"),
  ('Y', "TYR")]

fastacode2resnameDictionary = Map.fromList codebook_standard_protein

fastacode2resname code    = Map.findWithDefault defaultResname   code    fastacode2resnameDictionary

defaultResname   = "UNK"

