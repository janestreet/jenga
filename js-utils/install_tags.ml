let package_name = "jenga"

let sections =
  [ ("lib",
    [ ("built_lib_jenga_lib", None)
    ; ("built_lib_tenacious_lib", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_jenga", Some "jenga")
    ],
    [])
  ]
