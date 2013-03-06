-- | Module for conversions from single to three-letter aminoacid codes.
module Util.Fasta(fastacode2resname) where

import Data.Map as Map

-- NOTE: copied from hPDB temporarily, until it is released

-- | Codebook matching single and three-letter residue codes.
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

-- | Dictionary of codes created from `codebook_standard_protein`.
fastacode2resnameDictionary = Map.fromList codebook_standard_protein

-- | Returns PDB three-letter code for a given single-letter FASTA code.
fastacode2resname code    = Map.findWithDefault defaultResname   code    fastacode2resnameDictionary

-- | Default three-letter code returned for unknown aminoacid code.
defaultResname   = "UNK"

