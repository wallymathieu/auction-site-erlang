{application, auction_site,
 [{vsn, "1.0.0"},
  {modules, [auction_site, auction_site_serv, auction_site_sup, auction_site_supersup, auction_site_worker_sup]},
  {registered, [auction_site]},
  {mod, {auction_site, []}}
 ]}.
