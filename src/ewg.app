{application, ewg,
 [{description, "ewg"},
  {vsn, "0.01"},
  {modules, [
    ewg,
    ewg_app,
    ewg_sup,
    ewg_web,
    ewg_deps
  ]},
  {registered, []},
  {mod, {ewg_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
