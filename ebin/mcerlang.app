%%% -*- mode:erlang -*-
{application, mcerlang,
 [
  % A quick description of the application.
  {description, "Erlang binary protocol memcached client"},

  % The version of the applicaton
  {vsn, "0.1.0"},

  % All modules used by the application. 
  {modules,
   [
    mcerlang
   ]},
  
  % A list of the registered processes in your application.  Used to prevent collisions. 
  {registered, []},

  % This is a list of the applications that your application depends on. This list must be filled out
  % carefully so that dependency resolution systems can function properly.
  {applications, [kernel, stdlib]},

  % configuration parameters similar to those in the config file specified
  % on the command line. can be fetched with gas:get_env
  {env, []}
 ]
}.

