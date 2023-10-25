module Token where


data Token  = Atom String
            | Var String
            | If
            | Comma
            | Period
            | Paren'Open
            | Paren'Close
            | Underscore
            | Line
            | Equal
            | EOF
  deriving (Eq, Show)
