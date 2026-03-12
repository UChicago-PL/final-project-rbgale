module Verify (verify) where

import qualified Data.Map.Strict as M
import Types (Literal(..), Formula, Assignment)

verify :: Assignment -> Formula -> Bool
verify assignment = all (any litIsTrue) where
    litIsTrue (Pos var) = M.findWithDefault False var assignment
    litIsTrue (Neg var) = not (M.findWithDefault True var assignment)
