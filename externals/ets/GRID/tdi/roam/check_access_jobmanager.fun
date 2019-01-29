public fun check_access_jobmanager(in _name)
{
  _site_resource=getenv("ROAM_SITE_RESOURCE");
  _jobmanager_resource=getenv("ROAM_JOBMANAGER_RESOURCE");
  if (len(_site_resource) > 0)
  {
    if (check_access(_site_resource,"Access","nobody")) abort(1);
    if (check_access(_site_resource,"Noaccess",_name)) abort(2);
    if (!check_access(_site_resource,"Access","everyone") && !check_access(_site_resource,"Access",_name)) abort(3);
  }
  if (check_access(_jobmanager_resource,"Execute","nobody")) abort(4);
  _aux="";
  if (len(_jobmanager_resource) > 0)
  {
    if (check_access(_jobmanager_resource,"Execute",_name, _aux) && len(_aux) > 0) 
      return(_aux);
  }
  abort(5);
}
