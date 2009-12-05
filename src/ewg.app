{application, ewg,
 [{description, "ewg"},
  {vsn, "0.1"},
  {modules, [
    ewg,
    ewg_app,
    ewg_sup,
    ewg_deps,
    ewg_resource
  ]},
  {registered, []},
  {mod, {ewg_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
