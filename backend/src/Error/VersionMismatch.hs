module Error.VersionMismatch where

import Lib (ETagged)

-- TODO: Change ETagged to Versioned
data VersionMismatch a = VersionMismatch (ETagged a)
