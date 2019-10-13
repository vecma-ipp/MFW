## Standalone module
- List of standalone programs, wrapped with external codes, and their tests:

    ets, equilupdate, gaussian_sources, chease, bohmgb, gem0 and imp4dv.

- Workflows codes (as a box) using loop over transport, equilibrium and turbulence codes:
  - loop_bgb : ets + chease + bohmgb
  - loop_gem0: ets + chese + gem0 + imp4dv
  They are operating on the followings inputs:
    - Mandaotry:
    
      ets_coreprof_in.cpo
      
      ets_equilibrium_in.cpo
      
      ets_coreimpur_in.cpo
      
      ets_coretransp_in.cpo
      
      ets_toroidfield_in.cpo
      
    - Optional, otherwise will be genreated by gaussian_source:
    
      ets_coresource_in.cpo
      
  The output is ets_coreprof_out

### Example of usage 
To test ETS with AUG data.

In `bin` folder, link or copy:
- cpo inputs from `../workflows/AUG_28906_6/`
- ets.xml and ets.xsd parameter files from `../workflows/`

And then run:
> ./${SYS}/ets_test
