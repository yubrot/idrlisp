module Idrboot

export
readBootCode : IO (Provider String)
readBootCode =
  case !(readFile "../lispboot/boot.lisp") of
    Right r => pure $ Provide r
    Left err => pure $ Error $ show err

