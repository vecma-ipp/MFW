 module copy_structures
 !------------------------------------------------------------------
 ! module for granular copying of euitm structures
 !------------------------------------------------------------------

   use euitm_schemas

   interface copy_cpo
      module procedure copy_type_integer
      module procedure copy_type_float
      module procedure copy_type_complex
      module procedure copy_type_array3dcplx_type
      module procedure copy_type_array3dflt_type
      module procedure copy_type_array3dint_type
      module procedure copy_type_array4dflt_type
      module procedure copy_type_array5dflt_type
      module procedure copy_type_array6dflt_type
      module procedure copy_type_array7dflt_type
      module procedure copy_type_matcplx_type
      module procedure copy_type_matflt_type
      module procedure copy_type_matint_type
      module procedure copy_type_veccplx_type
      module procedure copy_type_vecflt_type
      module procedure copy_type_vecint_type
      module procedure copy_type_vecstring_type
      module procedure copy_type_amns
      module procedure copy_arr_type_amns
      module procedure copy_type_antennas
      module procedure copy_arr_type_antennas
      module procedure copy_type_bb_shield
      module procedure copy_arr_type_bb_shield
      module procedure copy_type_compositionc
      module procedure copy_arr_type_compositionc
      module procedure copy_type_coredelta
      module procedure copy_arr_type_coredelta
      module procedure copy_type_corefast
      module procedure copy_arr_type_corefast
      module procedure copy_type_coreimpur
      module procedure copy_arr_type_coreimpur
      module procedure copy_type_coreneutrals
      module procedure copy_arr_type_coreneutrals
      module procedure copy_type_coreprof
      module procedure copy_arr_type_coreprof
      module procedure copy_type_coresource
      module procedure copy_arr_type_coresource
      module procedure copy_type_coretransp
      module procedure copy_arr_type_coretransp
      module procedure copy_type_cxdiag
      module procedure copy_arr_type_cxdiag
      module procedure copy_type_distribution
      module procedure copy_arr_type_distribution
      module procedure copy_type_distsource
      module procedure copy_arr_type_distsource
      module procedure copy_type_ecediag
      module procedure copy_arr_type_ecediag
      module procedure copy_type_edge
      module procedure copy_arr_type_edge
      module procedure copy_type_efcc
      module procedure copy_arr_type_efcc
      module procedure copy_type_equilibrium
      module procedure copy_arr_type_equilibrium
      module procedure copy_type_fusiondiag
      module procedure copy_arr_type_fusiondiag
      module procedure copy_type_halphadiag
      module procedure copy_arr_type_halphadiag
      module procedure copy_type_heat_sources
      module procedure copy_arr_type_heat_sources
      module procedure copy_type_interfdiag
      module procedure copy_arr_type_interfdiag
      module procedure copy_type_ironmodel
      module procedure copy_arr_type_ironmodel
      module procedure copy_type_langmuirdiag
      module procedure copy_arr_type_langmuirdiag
      module procedure copy_type_launchs
      module procedure copy_arr_type_launchs
      module procedure copy_type_lithiumdiag
      module procedure copy_arr_type_lithiumdiag
      module procedure copy_type_magdiag
      module procedure copy_arr_type_magdiag
      module procedure copy_type_mhd
      module procedure copy_arr_type_mhd
      module procedure copy_type_msediag
      module procedure copy_arr_type_msediag
      module procedure copy_type_nbi
      module procedure copy_arr_type_nbi
      module procedure copy_type_neoclassic
      module procedure copy_arr_type_neoclassic
      module procedure copy_type_ntm
      module procedure copy_arr_type_ntm
      module procedure copy_type_orbit
      module procedure copy_arr_type_orbit
      module procedure copy_type_pellets
      module procedure copy_arr_type_pellets
      module procedure copy_type_pfsystems
      module procedure copy_arr_type_pfsystems
      module procedure copy_type_polardiag
      module procedure copy_arr_type_polardiag
      module procedure copy_type_power_conv
      module procedure copy_arr_type_power_conv
      module procedure copy_type_reflectomet
      module procedure copy_arr_type_reflectomet
      module procedure copy_type_rfadiag
      module procedure copy_arr_type_rfadiag
      module procedure copy_type_sawteeth
      module procedure copy_arr_type_sawteeth
      module procedure copy_type_scenario
      module procedure copy_arr_type_scenario
      module procedure copy_type_solcurdiag
      module procedure copy_arr_type_solcurdiag
      module procedure copy_type_temporary
      module procedure copy_arr_type_temporary
      module procedure copy_type_topinfo
      module procedure copy_arr_type_topinfo
      module procedure copy_type_toroidfield
      module procedure copy_arr_type_toroidfield
      module procedure copy_type_tsdiag
      module procedure copy_arr_type_tsdiag
      module procedure copy_type_turbulence
      module procedure copy_arr_type_turbulence
      module procedure copy_type_wall
      module procedure copy_arr_type_wall
      module procedure copy_type_waves
      module procedure copy_arr_type_waves
      module procedure copy_type_amns_constituentType
      module procedure copy_arr_type_amns_constituentType
      module procedure copy_type_amns_processType
      module procedure copy_arr_type_amns_processType
      module procedure copy_type_antenna_ec
      module procedure copy_arr_type_antenna_ec
      module procedure copy_type_antenna_ic
      module procedure copy_arr_type_antenna_ic
      module procedure copy_type_antenna_lh
      module procedure copy_arr_type_antenna_lh
      module procedure copy_type_antennaic_setup
      module procedure copy_arr_type_antennaic_setup
      module procedure copy_type_antennalh_setup
      module procedure copy_arr_type_antennalh_setup
      module procedure copy_type_b0r0
      module procedure copy_arr_type_b0r0
      module procedure copy_type_bb
      module procedure copy_arr_type_bb
      module procedure copy_type_bb_dimension
      module procedure copy_arr_type_bb_dimension
      module procedure copy_type_bb_geometry
      module procedure copy_arr_type_bb_geometry
      module procedure copy_type_bb_specs
      module procedure copy_arr_type_bb_specs
      module procedure copy_type_beamletgroup
      module procedure copy_arr_type_beamletgroup
      module procedure copy_type_beamlets
      module procedure copy_arr_type_beamlets
      module procedure copy_type_beamtracing
      module procedure copy_arr_type_beamtracing
      module procedure copy_type_boundary
      module procedure copy_arr_type_boundary
      module procedure copy_type_boundary_neutrals
      module procedure copy_arr_type_boundary_neutrals
      module procedure copy_type_boundaryel
      module procedure copy_arr_type_boundaryel
      module procedure copy_type_boundaryimp
      module procedure copy_arr_type_boundaryimp
      module procedure copy_type_boundaryion
      module procedure copy_arr_type_boundaryion
      module procedure copy_type_bpol_probes
      module procedure copy_arr_type_bpol_probes
      module procedure copy_type_calorimetry_heat_source
      module procedure copy_arr_type_calorimetry_heat_source
      module procedure copy_type_circuits
      module procedure copy_arr_type_circuits
      module procedure copy_type_circularcoil
      module procedure copy_arr_type_circularcoil
      module procedure copy_type_clusters
      module procedure copy_arr_type_clusters
      module procedure copy_type_codeparam
      module procedure copy_arr_type_codeparam
      module procedure copy_type_coefficients_neutrals
      module procedure copy_arr_type_coefficients_neutrals
      module procedure copy_type_coherentwave
      module procedure copy_arr_type_coherentwave
      module procedure copy_type_coil
      module procedure copy_arr_type_coil
      module procedure copy_type_com
      module procedure copy_arr_type_com
      module procedure copy_type_complexgrid
      module procedure copy_arr_type_complexgrid
      module procedure copy_type_complexgrid_geo_global
      module procedure copy_arr_type_complexgrid_geo_global
      module procedure copy_type_complexgrid_indexlist
      module procedure copy_arr_type_complexgrid_indexlist
      module procedure copy_type_complexgrid_metric
      module procedure copy_arr_type_complexgrid_metric
      module procedure copy_type_complexgrid_objectlist
      module procedure copy_arr_type_complexgrid_objectlist
      module procedure copy_type_complexgrid_scalar
      module procedure copy_arr_type_complexgrid_scalar
      module procedure copy_type_complexgrid_scalar_cplx
      module procedure copy_arr_type_complexgrid_scalar_cplx
      module procedure copy_type_complexgrid_scalar_int
      module procedure copy_arr_type_complexgrid_scalar_int
      module procedure copy_type_complexgrid_scalar_simplestruct
      module procedure copy_arr_type_complexgrid_scalar_simplestruct
      module procedure copy_type_complexgrid_space
      module procedure copy_arr_type_complexgrid_space
      module procedure copy_type_complexgrid_subgrid
      module procedure copy_arr_type_complexgrid_subgrid
      module procedure copy_type_complexgrid_vector
      module procedure copy_arr_type_complexgrid_vector
      module procedure copy_type_complexgrid_vector_simplestruct
      module procedure copy_arr_type_complexgrid_vector_simplestruct
      module procedure copy_type_composition
      module procedure copy_arr_type_composition
      module procedure copy_type_composition_neutrals
      module procedure copy_arr_type_composition_neutrals
      module procedure copy_type_composition_neutrals_neutcomp
      module procedure copy_arr_type_composition_neutrals_neutcomp
      module procedure copy_type_composition_neutralscomp
      module procedure copy_arr_type_composition_neutralscomp
      module procedure copy_type_compositions_type
      module procedure copy_arr_type_compositions_type
      module procedure copy_type_compound_desc
      module procedure copy_arr_type_compound_desc
      module procedure copy_type_coord_sys
      module procedure copy_arr_type_coord_sys
      module procedure copy_type_coordinates
      module procedure copy_arr_type_coordinates
      module procedure copy_type_coords
      module procedure copy_arr_type_coords
      module procedure copy_type_coredelta_values
      module procedure copy_arr_type_coredelta_values
      module procedure copy_type_coredelta_values_impurity
      module procedure copy_arr_type_coredelta_values_impurity
      module procedure copy_type_corefast_values
      module procedure copy_arr_type_corefast_values
      module procedure copy_type_corefield
      module procedure copy_arr_type_corefield
      module procedure copy_type_corefieldion
      module procedure copy_arr_type_corefieldion
      module procedure copy_type_corefieldneutral
      module procedure copy_arr_type_corefieldneutral
      module procedure copy_type_corefieldneutrale
      module procedure copy_arr_type_corefieldneutrale
      module procedure copy_type_corefieldneutralv
      module procedure copy_arr_type_corefieldneutralv
      module procedure copy_type_corefieldneutralv0
      module procedure copy_arr_type_corefieldneutralv0
      module procedure copy_type_coreimpurdiag_sum_radiation
      module procedure copy_arr_type_coreimpurdiag_sum_radiation
      module procedure copy_type_coreimpurediag_energy
      module procedure copy_arr_type_coreimpurediag_energy
      module procedure copy_type_coreimpurediag_radiation
      module procedure copy_arr_type_coreimpurediag_radiation
      module procedure copy_type_coreimpurediag_sum
      module procedure copy_arr_type_coreimpurediag_sum
      module procedure copy_type_coreimpurediag_sum_energy
      module procedure copy_arr_type_coreimpurediag_sum_energy
      module procedure copy_type_coreimpurediag_type
      module procedure copy_arr_type_coreimpurediag_type
      module procedure copy_type_coreimpurediagprof_type
      module procedure copy_arr_type_coreimpurediagprof_type
      module procedure copy_type_coreimpurediagsum_type
      module procedure copy_arr_type_coreimpurediagsum_type
      module procedure copy_type_coreneutrals_atomlist
      module procedure copy_arr_type_coreneutrals_atomlist
      module procedure copy_type_coreneutrals_neutraltype
      module procedure copy_arr_type_coreneutrals_neutraltype
      module procedure copy_type_coreprofile
      module procedure copy_arr_type_coreprofile
      module procedure copy_type_coreprofion
      module procedure copy_arr_type_coreprofion
      module procedure copy_type_coresource_values
      module procedure copy_arr_type_coresource_values
      module procedure copy_type_coretransel
      module procedure copy_arr_type_coretransel
      module procedure copy_type_coretransimp
      module procedure copy_arr_type_coretransimp
      module procedure copy_type_coretransion
      module procedure copy_arr_type_coretransion
      module procedure copy_type_coretransp_values
      module procedure copy_arr_type_coretransp_values
      module procedure copy_type_current
      module procedure copy_arr_type_current
      module procedure copy_type_cxmeasure
      module procedure copy_arr_type_cxmeasure
      module procedure copy_type_cxsetup
      module procedure copy_arr_type_cxsetup
      module procedure copy_type_data_release
      module procedure copy_arr_type_data_release
      module procedure copy_type_datainfo
      module procedure copy_arr_type_datainfo
      module procedure copy_type_desc_coils
      module procedure copy_arr_type_desc_coils
      module procedure copy_type_desc_impur
      module procedure copy_arr_type_desc_impur
      module procedure copy_type_desc_iron
      module procedure copy_arr_type_desc_iron
      module procedure copy_type_desc_pfcoils
      module procedure copy_arr_type_desc_pfcoils
      module procedure copy_type_desc_supply
      module procedure copy_arr_type_desc_supply
      module procedure copy_type_diag_func
      module procedure copy_arr_type_diag_func
      module procedure copy_type_dist_collisional_transfer_0d
      module procedure copy_arr_type_dist_collisional_transfer_0d
      module procedure copy_type_dist_collisional_transfer_1d
      module procedure copy_arr_type_dist_collisional_transfer_1d
      module procedure copy_type_dist_collisional_transfer_2d
      module procedure copy_arr_type_dist_collisional_transfer_2d
      module procedure copy_type_dist_distrivec_distfunc_fexp_param
      module procedure copy_arr_type_dist_distrivec_distfunc_fexp_param
      module procedure copy_type_dist_ff
      module procedure copy_arr_type_dist_ff
      module procedure copy_type_dist_func
      module procedure copy_arr_type_dist_func
      module procedure copy_type_dist_geometry_0d
      module procedure copy_arr_type_dist_geometry_0d
      module procedure copy_type_dist_geometry_1d
      module procedure copy_arr_type_dist_geometry_1d
      module procedure copy_type_dist_geometry_2d
      module procedure copy_arr_type_dist_geometry_2d
      module procedure copy_type_dist_global_param
      module procedure copy_arr_type_dist_global_param
      module procedure copy_type_dist_global_param_collisions_z
      module procedure copy_arr_type_dist_global_param_collisions_z
      module procedure copy_type_dist_grid_info
      module procedure copy_arr_type_dist_grid_info
      module procedure copy_type_dist_profile_values_1d
      module procedure copy_arr_type_dist_profile_values_1d
      module procedure copy_type_dist_profile_values_2d
      module procedure copy_arr_type_dist_profile_values_2d
      module procedure copy_type_dist_profiles2d_collisions_z
      module procedure copy_arr_type_dist_profiles2d_collisions_z
      module procedure copy_type_dist_profiles_1d
      module procedure copy_arr_type_dist_profiles_1d
      module procedure copy_type_dist_profiles_1d_collisions_z
      module procedure copy_arr_type_dist_profiles_1d_collisions_z
      module procedure copy_type_dist_profiles_2d
      module procedure copy_arr_type_dist_profiles_2d
      module procedure copy_type_dist_sources_0d
      module procedure copy_arr_type_dist_sources_0d
      module procedure copy_type_dist_sources_1d
      module procedure copy_arr_type_dist_sources_1d
      module procedure copy_type_dist_sources_reference
      module procedure copy_arr_type_dist_sources_reference
      module procedure copy_type_dist_state_0d
      module procedure copy_arr_type_dist_state_0d
      module procedure copy_type_dist_state_1d
      module procedure copy_arr_type_dist_state_1d
      module procedure copy_type_dist_state_2d
      module procedure copy_arr_type_dist_state_2d
      module procedure copy_type_dist_thermalised_1d
      module procedure copy_arr_type_dist_thermalised_1d
      module procedure copy_type_distri_vec
      module procedure copy_arr_type_distri_vec
      module procedure copy_type_distsource_global_param
      module procedure copy_arr_type_distsource_global_param
      module procedure copy_type_distsource_line_src_prof
      module procedure copy_arr_type_distsource_line_src_prof
      module procedure copy_type_distsource_profiles_1d
      module procedure copy_arr_type_distsource_profiles_1d
      module procedure copy_type_distsource_profiles_2d
      module procedure copy_arr_type_distsource_profiles_2d
      module procedure copy_type_distsource_source
      module procedure copy_arr_type_distsource_source
      module procedure copy_type_divergence
      module procedure copy_arr_type_divergence
      module procedure copy_type_e_components
      module procedure copy_arr_type_e_components
      module procedure copy_type_ecemeasure
      module procedure copy_arr_type_ecemeasure
      module procedure copy_type_ecesetup
      module procedure copy_arr_type_ecesetup
      module procedure copy_type_edge_fluid
      module procedure copy_arr_type_edge_fluid
      module procedure copy_type_edge_fluid_scalar
      module procedure copy_arr_type_edge_fluid_scalar
      module procedure copy_type_edge_fluid_scalar_simplestruct
      module procedure copy_arr_type_edge_fluid_scalar_simplestruct
      module procedure copy_type_edge_fluid_scalar_transpcoeff
      module procedure copy_arr_type_edge_fluid_scalar_transpcoeff
      module procedure copy_type_edge_fluid_vector
      module procedure copy_arr_type_edge_fluid_vector
      module procedure copy_type_edge_fluid_vector_simplestruct
      module procedure copy_arr_type_edge_fluid_vector_simplestruct
      module procedure copy_type_edge_kinetic
      module procedure copy_arr_type_edge_kinetic
      module procedure copy_type_edge_kinetic_distribution
      module procedure copy_arr_type_edge_kinetic_distribution
      module procedure copy_type_edges
      module procedure copy_arr_type_edges
      module procedure copy_type_edgespecies
      module procedure copy_arr_type_edgespecies
      module procedure copy_type_element_desc
      module procedure copy_arr_type_element_desc
      module procedure copy_type_entry_def
      module procedure copy_arr_type_entry_def
      module procedure copy_type_enum_instance
      module procedure copy_arr_type_enum_instance
      module procedure copy_type_eqconstraint
      module procedure copy_arr_type_eqconstraint
      module procedure copy_type_eqgeometry
      module procedure copy_arr_type_eqgeometry
      module procedure copy_type_eqmes0D
      module procedure copy_arr_type_eqmes0D
      module procedure copy_type_eqmes1D
      module procedure copy_arr_type_eqmes1D
      module procedure copy_type_equatorial_plane
      module procedure copy_arr_type_equatorial_plane
      module procedure copy_type_equilibrium_profiles2d_grid
      module procedure copy_arr_type_equilibrium_profiles2d_grid
      module procedure copy_type_equilibrium_profiles_2d
      module procedure copy_arr_type_equilibrium_profiles_2d
      module procedure copy_type_exp0D
      module procedure copy_arr_type_exp0D
      module procedure copy_type_exp1D
      module procedure copy_arr_type_exp1D
      module procedure copy_type_exp2D
      module procedure copy_arr_type_exp2D
      module procedure copy_type_f_expansion
      module procedure copy_arr_type_f_expansion
      module procedure copy_type_fast_thermal_separation_filter
      module procedure copy_arr_type_fast_thermal_separation_filter
      module procedure copy_type_filter
      module procedure copy_arr_type_filter
      module procedure copy_type_flat_polygon
      module procedure copy_arr_type_flat_polygon
      module procedure copy_type_flush
      module procedure copy_arr_type_flush
      module procedure copy_type_flux_loops
      module procedure copy_arr_type_flux_loops
      module procedure copy_type_fluxel
      module procedure copy_arr_type_fluxel
      module procedure copy_type_fluximp
      module procedure copy_arr_type_fluximp
      module procedure copy_type_fluxion
      module procedure copy_arr_type_fluxion
      module procedure copy_type_focussing
      module procedure copy_arr_type_focussing
      module procedure copy_type_fullwave
      module procedure copy_arr_type_fullwave
      module procedure copy_type_fusiondiag_colli_3d
      module procedure copy_arr_type_fusiondiag_colli_3d
      module procedure copy_type_fusiondiag_colli_circ
      module procedure copy_arr_type_fusiondiag_colli_circ
      module procedure copy_type_fusiondiag_colli_poly
      module procedure copy_arr_type_fusiondiag_colli_poly
      module procedure copy_type_fusiondiag_collimator
      module procedure copy_arr_type_fusiondiag_collimator
      module procedure copy_type_fusiondiag_colliunit_circ
      module procedure copy_arr_type_fusiondiag_colliunit_circ
      module procedure copy_type_fusiondiag_colliunit_poly
      module procedure copy_arr_type_fusiondiag_colliunit_poly
      module procedure copy_type_fusiondiag_counts
      module procedure copy_arr_type_fusiondiag_counts
      module procedure copy_type_fusiondiag_ct_chords
      module procedure copy_arr_type_fusiondiag_ct_chords
      module procedure copy_type_fusiondiag_ct_energy
      module procedure copy_arr_type_fusiondiag_ct_energy
      module procedure copy_type_fusiondiag_detect_ct_energy
      module procedure copy_arr_type_fusiondiag_detect_ct_energy
      module procedure copy_type_fusiondiag_emissivity1d
      module procedure copy_arr_type_fusiondiag_emissivity1d
      module procedure copy_type_fusiondiag_emissivity2d
      module procedure copy_arr_type_fusiondiag_emissivity2d
      module procedure copy_type_fusiondiag_fus_product
      module procedure copy_arr_type_fusiondiag_fus_product
      module procedure copy_type_fusiondiag_spec1d
      module procedure copy_arr_type_fusiondiag_spec1d
      module procedure copy_type_fusiondiag_spec2d
      module procedure copy_arr_type_fusiondiag_spec2d
      module procedure copy_type_fusiondiag_voxels
      module procedure copy_arr_type_fusiondiag_voxels
      module procedure copy_type_geom
      module procedure copy_arr_type_geom
      module procedure copy_type_geom_iron
      module procedure copy_arr_type_geom_iron
      module procedure copy_type_global_param
      module procedure copy_arr_type_global_param
      module procedure copy_type_globalparam
      module procedure copy_arr_type_globalparam
      module procedure copy_type_halpha_setup
      module procedure copy_arr_type_halpha_setup
      module procedure copy_type_hcll
      module procedure copy_arr_type_hcll
      module procedure copy_type_hcll_bb
      module procedure copy_arr_type_hcll_bb
      module procedure copy_type_hcllbb_specs
      module procedure copy_arr_type_hcllbb_specs
      module procedure copy_type_holes
      module procedure copy_arr_type_holes
      module procedure copy_type_identifier
      module procedure copy_arr_type_identifier
      module procedure copy_type_impcoeff
      module procedure copy_arr_type_impcoeff
      module procedure copy_type_impurities
      module procedure copy_arr_type_impurities
      module procedure copy_type_impurity_type
      module procedure copy_arr_type_impurity_type
      module procedure copy_type_inj_spec
      module procedure copy_arr_type_inj_spec
      module procedure copy_type_ions
      module procedure copy_arr_type_ions
      module procedure copy_type_isoflux
      module procedure copy_arr_type_isoflux
      module procedure copy_type_jni
      module procedure copy_arr_type_jni
      module procedure copy_type_lang_derived
      module procedure copy_arr_type_lang_derived
      module procedure copy_type_lang_measure
      module procedure copy_arr_type_lang_measure
      module procedure copy_type_launchangles
      module procedure copy_arr_type_launchangles
      module procedure copy_type_launchs_parallel
      module procedure copy_arr_type_launchs_parallel
      module procedure copy_type_launchs_phi_theta
      module procedure copy_arr_type_launchs_phi_theta
      module procedure copy_type_launchs_rfbeam
      module procedure copy_arr_type_launchs_rfbeam
      module procedure copy_type_launchs_rfbeam_phaseellipse
      module procedure copy_arr_type_launchs_rfbeam_phaseellipse
      module procedure copy_type_launchs_rfbeam_spot
      module procedure copy_arr_type_launchs_rfbeam_spot
      module procedure copy_type_launchsignal
      module procedure copy_arr_type_launchsignal
      module procedure copy_type_limiter_unit
      module procedure copy_arr_type_limiter_unit
      module procedure copy_type_limits
      module procedure copy_arr_type_limits
      module procedure copy_type_lineintegraldiag
      module procedure copy_arr_type_lineintegraldiag
      module procedure copy_type_lithmeasure
      module procedure copy_arr_type_lithmeasure
      module procedure copy_type_lithsetup
      module procedure copy_arr_type_lithsetup
      module procedure copy_type_local
      module procedure copy_arr_type_local
      module procedure copy_type_mag_axis
      module procedure copy_arr_type_mag_axis
      module procedure copy_type_magnet_iron
      module procedure copy_arr_type_magnet_iron
      module procedure copy_type_magnetise
      module procedure copy_arr_type_magnetise
      module procedure copy_type_mat_lim
      module procedure copy_arr_type_mat_lim
      module procedure copy_type_mdinfo
      module procedure copy_arr_type_mdinfo
      module procedure copy_type_mhd_ideal_wall2d
      module procedure copy_arr_type_mhd_ideal_wall2d
      module procedure copy_type_mhd_mode
      module procedure copy_arr_type_mhd_mode
      module procedure copy_type_mhd_plasma
      module procedure copy_arr_type_mhd_plasma
      module procedure copy_type_mhd_res_wall2d
      module procedure copy_arr_type_mhd_res_wall2d
      module procedure copy_type_mhd_vacuum
      module procedure copy_arr_type_mhd_vacuum
      module procedure copy_type_mhd_vector
      module procedure copy_arr_type_mhd_vector
      module procedure copy_type_mode_lipb
      module procedure copy_arr_type_mode_lipb
      module procedure copy_type_mode_mech
      module procedure copy_arr_type_mode_mech
      module procedure copy_type_mode_neutr
      module procedure copy_arr_type_mode_neutr
      module procedure copy_type_mode_th_hyd
      module procedure copy_arr_type_mode_th_hyd
      module procedure copy_type_mode_therm
      module procedure copy_arr_type_mode_therm
      module procedure copy_type_mode_tritium
      module procedure copy_arr_type_mode_tritium
      module procedure copy_type_modules
      module procedure copy_arr_type_modules
      module procedure copy_type_msediag_emiss_chord
      module procedure copy_arr_type_msediag_emiss_chord
      module procedure copy_type_msediag_emissivity
      module procedure copy_arr_type_msediag_emissivity
      module procedure copy_type_msediag_polarization
      module procedure copy_arr_type_msediag_polarization
      module procedure copy_type_msediag_radia_chord
      module procedure copy_arr_type_msediag_radia_chord
      module procedure copy_type_msediag_radiance
      module procedure copy_arr_type_msediag_radiance
      module procedure copy_type_msediag_setup
      module procedure copy_arr_type_msediag_setup
      module procedure copy_type_msediag_setup_polarimetry
      module procedure copy_arr_type_msediag_setup_polarimetry
      module procedure copy_type_msediag_stokes
      module procedure copy_arr_type_msediag_stokes
      module procedure copy_type_nbi_nbi_unit_wall
      module procedure copy_arr_type_nbi_nbi_unit_wall
      module procedure copy_type_nbi_nbi_unit_wall_surface
      module procedure copy_arr_type_nbi_nbi_unit_wall_surface
      module procedure copy_type_nbi_unit
      module procedure copy_arr_type_nbi_unit
      module procedure copy_type_ne_transp
      module procedure copy_arr_type_ne_transp
      module procedure copy_type_neoclassic_impurity
      module procedure copy_arr_type_neoclassic_impurity
      module procedure copy_type_neut_results
      module procedure copy_arr_type_neut_results
      module procedure copy_type_neutral_complex_type
      module procedure copy_arr_type_neutral_complex_type
      module procedure copy_type_neutro_resul
      module procedure copy_arr_type_neutro_resul
      module procedure copy_type_ni_transp
      module procedure copy_arr_type_ni_transp
      module procedure copy_type_ntm_mode
      module procedure copy_arr_type_ntm_mode
      module procedure copy_type_ntm_mode_evolution
      module procedure copy_arr_type_ntm_mode_evolution
      module procedure copy_type_ntm_mode_evolution_island
      module procedure copy_arr_type_ntm_mode_evolution_island
      module procedure copy_type_ntm_mode_full_evol
      module procedure copy_arr_type_ntm_mode_full_evol
      module procedure copy_type_ntm_mode_full_evol_island
      module procedure copy_arr_type_ntm_mode_full_evol_island
      module procedure copy_type_ntm_mode_onset
      module procedure copy_arr_type_ntm_mode_onset
      module procedure copy_type_nuclei
      module procedure copy_arr_type_nuclei
      module procedure copy_type_objects
      module procedure copy_arr_type_objects
      module procedure copy_type_offdiagel
      module procedure copy_arr_type_offdiagel
      module procedure copy_type_offdiagion
      module procedure copy_arr_type_offdiagion
      module procedure copy_type_omnigen_surf
      module procedure copy_arr_type_omnigen_surf
      module procedure copy_type_orbit_global_param
      module procedure copy_arr_type_orbit_global_param
      module procedure copy_type_orbit_midplane
      module procedure copy_arr_type_orbit_midplane
      module procedure copy_type_orbit_pos
      module procedure copy_arr_type_orbit_pos
      module procedure copy_type_orbit_special_pos
      module procedure copy_arr_type_orbit_special_pos
      module procedure copy_type_orbit_turning_pts
      module procedure copy_arr_type_orbit_turning_pts
      module procedure copy_type_origin
      module procedure copy_arr_type_origin
      module procedure copy_type_param
      module procedure copy_arr_type_param
      module procedure copy_type_parameters
      module procedure copy_arr_type_parameters
      module procedure copy_type_pellet
      module procedure copy_arr_type_pellet
      module procedure copy_type_pellet_angles
      module procedure copy_arr_type_pellet_angles
      module procedure copy_type_pellet_deposition
      module procedure copy_arr_type_pellet_deposition
      module procedure copy_type_pellet_elements
      module procedure copy_arr_type_pellet_elements
      module procedure copy_type_pellet_geometry
      module procedure copy_arr_type_pellet_geometry
      module procedure copy_type_pellet_impurity
      module procedure copy_arr_type_pellet_impurity
      module procedure copy_type_pellet_pathprofiles
      module procedure copy_arr_type_pellet_pathprofiles
      module procedure copy_type_pellet_shape
      module procedure copy_arr_type_pellet_shape
      module procedure copy_type_permeability
      module procedure copy_arr_type_permeability
      module procedure copy_type_pfcircuits
      module procedure copy_arr_type_pfcircuits
      module procedure copy_type_pfcoils
      module procedure copy_arr_type_pfcoils
      module procedure copy_type_pfelement
      module procedure copy_arr_type_pfelement
      module procedure copy_type_pfgeometry
      module procedure copy_arr_type_pfgeometry
      module procedure copy_type_pfpageometry
      module procedure copy_arr_type_pfpageometry
      module procedure copy_type_pfpassive
      module procedure copy_arr_type_pfpassive
      module procedure copy_type_pfpassive_current
      module procedure copy_arr_type_pfpassive_current
      module procedure copy_type_pfsupplies
      module procedure copy_arr_type_pfsupplies
      module procedure copy_type_phaseellipse
      module procedure copy_arr_type_phaseellipse
      module procedure copy_type_planecoil
      module procedure copy_arr_type_planecoil
      module procedure copy_type_plasmaComplexType
      module procedure copy_arr_type_plasmaComplexType
      module procedure copy_type_plasmaedge
      module procedure copy_arr_type_plasmaedge
      module procedure copy_type_pol_decomp
      module procedure copy_arr_type_pol_decomp
      module procedure copy_type_polarimetry
      module procedure copy_arr_type_polarimetry
      module procedure copy_type_polarization
      module procedure copy_arr_type_polarization
      module procedure copy_type_power_conv_component
      module procedure copy_arr_type_power_conv_component
      module procedure copy_type_power_exchange
      module procedure copy_arr_type_power_exchange
      module procedure copy_type_powerflow
      module procedure copy_arr_type_powerflow
      module procedure copy_type_profiles1d
      module procedure copy_arr_type_profiles1d
      module procedure copy_type_profiles_1d
      module procedure copy_arr_type_profiles_1d
      module procedure copy_type_psi
      module procedure copy_arr_type_psi
      module procedure copy_type_putinfo
      module procedure copy_arr_type_putinfo
      module procedure copy_type_q
      module procedure copy_arr_type_q
      module procedure copy_type_reacprodType
      module procedure copy_arr_type_reacprodType
      module procedure copy_type_react
      module procedure copy_arr_type_react
      module procedure copy_type_rectanglexyz
      module procedure copy_arr_type_rectanglexyz
      module procedure copy_type_recycling_neutrals
      module procedure copy_arr_type_recycling_neutrals
      module procedure copy_type_reduced
      module procedure copy_arr_type_reduced
      module procedure copy_type_refl_receive
      module procedure copy_arr_type_refl_receive
      module procedure copy_type_reflectometry_antennas
      module procedure copy_arr_type_reflectometry_antennas
      module procedure copy_type_reflectometry_radfield
      module procedure copy_arr_type_reflectometry_radfield
      module procedure copy_type_reflectometry_radfield_gaussian
      module procedure copy_arr_type_reflectometry_radfield_gaussian
      module procedure copy_type_reflectometry_radifield_efield
      module procedure copy_arr_type_reflectometry_radifield_efield
      module procedure copy_type_reggrid
      module procedure copy_arr_type_reggrid
      module procedure copy_type_rfameasure
      module procedure copy_arr_type_rfameasure
      module procedure copy_type_rfasetup
      module procedure copy_arr_type_rfasetup
      module procedure copy_type_rfbeam
      module procedure copy_arr_type_rfbeam
      module procedure copy_type_rz0D
      module procedure copy_arr_type_rz0D
      module procedure copy_type_rz1D
      module procedure copy_arr_type_rz1D
      module procedure copy_type_rz1D_npoints
      module procedure copy_arr_type_rz1D_npoints
      module procedure copy_type_rz1Dexp
      module procedure copy_arr_type_rz1Dexp
      module procedure copy_type_rz2D
      module procedure copy_arr_type_rz2D
      module procedure copy_type_rz3D
      module procedure copy_arr_type_rz3D
      module procedure copy_type_rzphi0D
      module procedure copy_arr_type_rzphi0D
      module procedure copy_type_rzphi1D
      module procedure copy_arr_type_rzphi1D
      module procedure copy_type_rzphi1Dexp
      module procedure copy_arr_type_rzphi1Dexp
      module procedure copy_type_rzphi1Dexperimental
      module procedure copy_arr_type_rzphi1Dexperimental
      module procedure copy_type_rzphi2D
      module procedure copy_arr_type_rzphi2D
      module procedure copy_type_rzphi3D
      module procedure copy_arr_type_rzphi3D
      module procedure copy_type_rzphidrdzdphi1D
      module procedure copy_arr_type_rzphidrdzdphi1D
      module procedure copy_type_sawteeth_diags
      module procedure copy_arr_type_sawteeth_diags
      module procedure copy_type_sawteeth_profiles1d
      module procedure copy_arr_type_sawteeth_profiles1d
      module procedure copy_type_scenario_centre
      module procedure copy_arr_type_scenario_centre
      module procedure copy_type_scenario_composition
      module procedure copy_arr_type_scenario_composition
      module procedure copy_type_scenario_configuration
      module procedure copy_arr_type_scenario_configuration
      module procedure copy_type_scenario_confinement
      module procedure copy_arr_type_scenario_confinement
      module procedure copy_type_scenario_currents
      module procedure copy_arr_type_scenario_currents
      module procedure copy_type_scenario_edge
      module procedure copy_arr_type_scenario_edge
      module procedure copy_type_scenario_energy
      module procedure copy_arr_type_scenario_energy
      module procedure copy_type_scenario_global
      module procedure copy_arr_type_scenario_global
      module procedure copy_type_scenario_heat_power
      module procedure copy_arr_type_scenario_heat_power
      module procedure copy_type_scenario_int
      module procedure copy_arr_type_scenario_int
      module procedure copy_type_scenario_itb
      module procedure copy_arr_type_scenario_itb
      module procedure copy_type_scenario_lim_div_wall
      module procedure copy_arr_type_scenario_lim_div_wall
      module procedure copy_type_scenario_line_ave
      module procedure copy_arr_type_scenario_line_ave
      module procedure copy_type_scenario_neutron
      module procedure copy_arr_type_scenario_neutron
      module procedure copy_type_scenario_ninety_five
      module procedure copy_arr_type_scenario_ninety_five
      module procedure copy_type_scenario_pedestal
      module procedure copy_arr_type_scenario_pedestal
      module procedure copy_type_scenario_reactor
      module procedure copy_arr_type_scenario_reactor
      module procedure copy_type_scenario_ref
      module procedure copy_arr_type_scenario_ref
      module procedure copy_type_scenario_references
      module procedure copy_arr_type_scenario_references
      module procedure copy_type_scenario_sol
      module procedure copy_arr_type_scenario_sol
      module procedure copy_type_scenario_vol_ave
      module procedure copy_arr_type_scenario_vol_ave
      module procedure copy_type_setup_bprobe
      module procedure copy_arr_type_setup_bprobe
      module procedure copy_type_setup_floops
      module procedure copy_arr_type_setup_floops
      module procedure copy_type_setup_line
      module procedure copy_arr_type_setup_line
      module procedure copy_type_setup_line_exp
      module procedure copy_arr_type_setup_line_exp
      module procedure copy_type_shield
      module procedure copy_arr_type_shield
      module procedure copy_type_shield_specs
      module procedure copy_arr_type_shield_specs
      module procedure copy_type_simp_apert
      module procedure copy_arr_type_simp_apert
      module procedure copy_type_solcurdiag_sol_current
      module procedure copy_arr_type_solcurdiag_sol_current
      module procedure copy_type_solcurdiag_sol_current_setup
      module procedure copy_arr_type_solcurdiag_sol_current_setup
      module procedure copy_type_source_imp
      module procedure copy_arr_type_source_imp
      module procedure copy_type_source_ion
      module procedure copy_arr_type_source_ion
      module procedure copy_type_source_rate
      module procedure copy_arr_type_source_rate
      module procedure copy_type_source_vec
      module procedure copy_arr_type_source_vec
      module procedure copy_type_sourceel
      module procedure copy_arr_type_sourceel
      module procedure copy_type_sourceimp
      module procedure copy_arr_type_sourceimp
      module procedure copy_type_sourceion
      module procedure copy_arr_type_sourceion
      module procedure copy_type_species_desc
      module procedure copy_arr_type_species_desc
      module procedure copy_type_species_reference
      module procedure copy_arr_type_species_reference
      module procedure copy_type_spectral
      module procedure copy_arr_type_spectral
      module procedure copy_type_spectrum
      module procedure copy_arr_type_spectrum
      module procedure copy_type_spot
      module procedure copy_arr_type_spot
      module procedure copy_type_sputtering_neutrals
      module procedure copy_arr_type_sputtering_neutrals
      module procedure copy_type_straps
      module procedure copy_arr_type_straps
      module procedure copy_type_structure_cs
      module procedure copy_arr_type_structure_cs
      module procedure copy_type_t_series_cplx
      module procedure copy_arr_type_t_series_cplx
      module procedure copy_type_t_series_real
      module procedure copy_arr_type_t_series_real
      module procedure copy_type_table
      module procedure copy_arr_type_table
      module procedure copy_type_tables
      module procedure copy_arr_type_tables
      module procedure copy_type_tables_coord
      module procedure copy_arr_type_tables_coord
      module procedure copy_type_temporary_nt
      module procedure copy_arr_type_temporary_nt
      module procedure copy_type_temporary_nt_0dc
      module procedure copy_arr_type_temporary_nt_0dc
      module procedure copy_type_temporary_nt_0di
      module procedure copy_arr_type_temporary_nt_0di
      module procedure copy_type_temporary_nt_0dr
      module procedure copy_arr_type_temporary_nt_0dr
      module procedure copy_type_temporary_nt_0ds
      module procedure copy_arr_type_temporary_nt_0ds
      module procedure copy_type_temporary_nt_1dc
      module procedure copy_arr_type_temporary_nt_1dc
      module procedure copy_type_temporary_nt_1di
      module procedure copy_arr_type_temporary_nt_1di
      module procedure copy_type_temporary_nt_1dr
      module procedure copy_arr_type_temporary_nt_1dr
      module procedure copy_type_temporary_nt_1ds
      module procedure copy_arr_type_temporary_nt_1ds
      module procedure copy_type_temporary_nt_2dc
      module procedure copy_arr_type_temporary_nt_2dc
      module procedure copy_type_temporary_nt_2di
      module procedure copy_arr_type_temporary_nt_2di
      module procedure copy_type_temporary_nt_2dr
      module procedure copy_arr_type_temporary_nt_2dr
      module procedure copy_type_temporary_nt_3dc
      module procedure copy_arr_type_temporary_nt_3dc
      module procedure copy_type_temporary_nt_3di
      module procedure copy_arr_type_temporary_nt_3di
      module procedure copy_type_temporary_nt_3dr
      module procedure copy_arr_type_temporary_nt_3dr
      module procedure copy_type_temporary_nt_4dr
      module procedure copy_arr_type_temporary_nt_4dr
      module procedure copy_type_temporary_t
      module procedure copy_arr_type_temporary_t
      module procedure copy_type_temporary_t_0dc
      module procedure copy_arr_type_temporary_t_0dc
      module procedure copy_type_temporary_t_0di
      module procedure copy_arr_type_temporary_t_0di
      module procedure copy_type_temporary_t_0dr
      module procedure copy_arr_type_temporary_t_0dr
      module procedure copy_type_temporary_t_0ds
      module procedure copy_arr_type_temporary_t_0ds
      module procedure copy_type_temporary_t_1dc
      module procedure copy_arr_type_temporary_t_1dc
      module procedure copy_type_temporary_t_1di
      module procedure copy_arr_type_temporary_t_1di
      module procedure copy_type_temporary_t_1dr
      module procedure copy_arr_type_temporary_t_1dr
      module procedure copy_type_temporary_t_2dc
      module procedure copy_arr_type_temporary_t_2dc
      module procedure copy_type_temporary_t_2di
      module procedure copy_arr_type_temporary_t_2di
      module procedure copy_type_temporary_t_2dr
      module procedure copy_arr_type_temporary_t_2dr
      module procedure copy_type_temporary_t_3dc
      module procedure copy_arr_type_temporary_t_3dc
      module procedure copy_type_temporary_t_3di
      module procedure copy_arr_type_temporary_t_3di
      module procedure copy_type_temporary_t_3dr
      module procedure copy_arr_type_temporary_t_3dr
      module procedure copy_type_temporary_t_4dr
      module procedure copy_arr_type_temporary_t_4dr
      module procedure copy_type_tf_desc_tfcoils
      module procedure copy_arr_type_tf_desc_tfcoils
      module procedure copy_type_tf_desc_tfcoils_board
      module procedure copy_arr_type_tf_desc_tfcoils_board
      module procedure copy_type_tf_structure
      module procedure copy_arr_type_tf_structure
      module procedure copy_type_theta_info
      module procedure copy_arr_type_theta_info
      module procedure copy_type_topo_regions
      module procedure copy_arr_type_topo_regions
      module procedure copy_type_toroid_field
      module procedure copy_arr_type_toroid_field
      module procedure copy_type_trace
      module procedure copy_arr_type_trace
      module procedure copy_type_transcoefel
      module procedure copy_arr_type_transcoefel
      module procedure copy_type_transcoefimp
      module procedure copy_arr_type_transcoefimp
      module procedure copy_type_transcoefion
      module procedure copy_arr_type_transcoefion
      module procedure copy_type_transcoefvtor
      module procedure copy_arr_type_transcoefvtor
      module procedure copy_type_trap_type
      module procedure copy_arr_type_trap_type
      module procedure copy_type_trianglexyz
      module procedure copy_arr_type_trianglexyz
      module procedure copy_type_tsmeasure
      module procedure copy_arr_type_tsmeasure
      module procedure copy_type_tssetup
      module procedure copy_arr_type_tssetup
      module procedure copy_type_turbcomposition
      module procedure copy_arr_type_turbcomposition
      module procedure copy_type_turbcoordsys
      module procedure copy_arr_type_turbcoordsys
      module procedure copy_type_turbenv1d
      module procedure copy_arr_type_turbenv1d
      module procedure copy_type_turbgrid
      module procedure copy_arr_type_turbgrid
      module procedure copy_type_turbspec1d
      module procedure copy_arr_type_turbspec1d
      module procedure copy_type_turbvar0d
      module procedure copy_arr_type_turbvar0d
      module procedure copy_type_turbvar1d
      module procedure copy_arr_type_turbvar1d
      module procedure copy_type_turbvar2d
      module procedure copy_arr_type_turbvar2d
      module procedure copy_type_turbvar3d
      module procedure copy_arr_type_turbvar3d
      module procedure copy_type_turbvar4d
      module procedure copy_arr_type_turbvar4d
      module procedure copy_type_turbvar5d
      module procedure copy_arr_type_turbvar5d
      module procedure copy_type_version_ind
      module procedure copy_arr_type_version_ind
      module procedure copy_type_wall2d
      module procedure copy_arr_type_wall2d
      module procedure copy_type_wall2d_mhd
      module procedure copy_arr_type_wall2d_mhd
      module procedure copy_type_wall3d
      module procedure copy_arr_type_wall3d
      module procedure copy_type_wall_blocks
      module procedure copy_arr_type_wall_blocks
      module procedure copy_type_wall_blocks_unit
      module procedure copy_arr_type_wall_blocks_unit
      module procedure copy_type_wall_limiter
      module procedure copy_arr_type_wall_limiter
      module procedure copy_type_wall_types
      module procedure copy_arr_type_wall_types
      module procedure copy_type_wall_types_layers
      module procedure copy_arr_type_wall_types_layers
      module procedure copy_type_wall_unitsComplexType
      module procedure copy_arr_type_wall_unitsComplexType
      module procedure copy_type_wall_unitsComplexType_layers
      module procedure copy_arr_type_wall_unitsComplexType_layers
      module procedure copy_type_wall_vessel
      module procedure copy_arr_type_wall_vessel
      module procedure copy_type_wall_vessel_annular
      module procedure copy_arr_type_wall_vessel_annular
      module procedure copy_type_wall_vessel_unit
      module procedure copy_arr_type_wall_vessel_unit
      module procedure copy_type_wall_wall0d
      module procedure copy_arr_type_wall_wall0d
      module procedure copy_type_wall_wall0d_plasma
      module procedure copy_arr_type_wall_wall0d_plasma
      module procedure copy_type_wall_wall2d_vessel_radial_build
      module procedure copy_arr_type_wall_wall2d_vessel_radial_build
      module procedure copy_type_waveguides
      module procedure copy_arr_type_waveguides
      module procedure copy_type_waves_global_param
      module procedure copy_arr_type_waves_global_param
      module procedure copy_type_waves_grid_1d
      module procedure copy_arr_type_waves_grid_1d
      module procedure copy_type_waves_grid_2d
      module procedure copy_arr_type_waves_grid_2d
      module procedure copy_type_waves_profiles_1d
      module procedure copy_arr_type_waves_profiles_1d
      module procedure copy_type_waves_profiles_2d
      module procedure copy_arr_type_waves_profiles_2d
      module procedure copy_type_waves_rtposition
      module procedure copy_arr_type_waves_rtposition
      module procedure copy_type_waves_rtwavevector
      module procedure copy_arr_type_waves_rtwavevector
      module procedure copy_type_weighted_markers
      module procedure copy_arr_type_weighted_markers
      module procedure copy_type_whatref
      module procedure copy_arr_type_whatref
      module procedure copy_type_width
      module procedure copy_arr_type_width
      module procedure copy_type_xpts
      module procedure copy_arr_type_xpts
      module procedure copy_type_xyz0D
      module procedure copy_arr_type_xyz0D
   end interface
 
   integer, parameter, private :: iu6 = 6
   integer, private :: verbose = 0

 contains


   subroutine set_copy_verbosity(verbosity)

     implicit none

     integer, intent(in) :: verbosity

     if (verbosity < 0) then
       verbose = 0
     else
       verbose = verbosity
     end if

   end subroutine set_copy_verbosity

   subroutine copy_type_integer(structure_in, structure_out)

     implicit none

     integer, intent(in) :: structure_in
     integer, intent(inout) :: structure_out

     if (structure_in /= -999999999) then
       structure_out = structure_in
     end if

   end subroutine copy_type_integer

   subroutine copy_type_float(structure_in, structure_out)

     implicit none

     real(euitm_r8) :: structure_in
     real(euitm_r8) :: structure_out

     if (structure_in /= -9.0D40) then
       structure_out = structure_in
     end if

   end subroutine copy_type_float

   subroutine copy_type_complex(structure_in, structure_out)

     implicit none

     complex(euitm_r8) :: structure_in
     complex(euitm_r8) :: structure_out

     if (structure_in /= (-9.0D40,-9.0D40)) then
       structure_out = structure_in
     end if

   end subroutine copy_type_complex

   subroutine copy_type_array3dcplx_type(structure_in, structure_out)
 
     implicit none

     complex(euitm_r8), pointer :: structure_in(:,:,:)
     complex(euitm_r8), pointer :: structure_out(:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array3dcplx_type

   subroutine copy_type_array3dflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:)
     real(euitm_r8), pointer :: structure_out(:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array3dflt_type

   subroutine copy_type_array3dint_type(structure_in, structure_out)
 
     implicit none

     integer, pointer :: structure_in(:,:,:)
     integer, pointer :: structure_out(:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array3dint_type

   subroutine copy_type_array4dflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:)
     real(euitm_r8), pointer :: structure_out(:,:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3), &
         size(structure_in, 4)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array4dflt_type

   subroutine copy_type_array5dflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:,:)
     real(euitm_r8), pointer :: structure_out(:,:,:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3), &
         size(structure_in, 4), &
         size(structure_in, 5)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array5dflt_type

   subroutine copy_type_array6dflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:,:,:)
     real(euitm_r8), pointer :: structure_out(:,:,:,:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3), &
         size(structure_in, 4), &
         size(structure_in, 5), &
         size(structure_in, 6)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array6dflt_type

   subroutine copy_type_array7dflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:,:,:,:)
     real(euitm_r8), pointer :: structure_out(:,:,:,:,:,:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2), &
         size(structure_in, 3), &
         size(structure_in, 4), &
         size(structure_in, 5), &
         size(structure_in, 6), &
         size(structure_in, 7)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_array7dflt_type

   subroutine copy_type_matcplx_type(structure_in, structure_out)
 
     implicit none

     complex(euitm_r8), pointer :: structure_in(:,:)
     complex(euitm_r8), pointer :: structure_out(:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_matcplx_type

   subroutine copy_type_matflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:)
     real(euitm_r8), pointer :: structure_out(:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_matflt_type

   subroutine copy_type_matint_type(structure_in, structure_out)
 
     implicit none

     integer, pointer :: structure_in(:,:)
     integer, pointer :: structure_out(:,:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1), &
         size(structure_in, 2)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_matint_type

   subroutine copy_type_veccplx_type(structure_in, structure_out)
 
     implicit none

     complex(euitm_r8), pointer :: structure_in(:)
     complex(euitm_r8), pointer :: structure_out(:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_veccplx_type

   subroutine copy_type_vecflt_type(structure_in, structure_out)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:)
     real(euitm_r8), pointer :: structure_out(:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_vecflt_type

   subroutine copy_type_vecint_type(structure_in, structure_out)
 
     implicit none

     integer, pointer :: structure_in(:)
     integer, pointer :: structure_out(:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_vecint_type

   subroutine copy_type_vecstring_type(structure_in, structure_out)
 
     implicit none

     character(len = 132), pointer :: structure_in(:)
     character(len = 132), pointer :: structure_out(:)
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         if (size(structure_in) &
          /= size(structure_out)) &
          deallocate(structure_out)
       end if
       if (.not. associated(structure_out)) then
         allocate(structure_out(size(structure_in, 1)))

       end if
       structure_out = structure_in
     end if
 
   end subroutine copy_type_vecstring_type

   subroutine copy_type_amns(structure_in, structure_out)
 
     implicit none
 
     type (type_amns), intent(in) :: structure_in
     type (type_amns), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied amns%datainfo'

     call copy_type_vecstring_type(structure_in%version, structure_out%version)
     if (verbose > 0) write(iu6, *) 'copied amns%version'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied amns%source'

     call copy_type_integer(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied amns%zn'

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied amns%amn'

     call copy_arr_type_amns_processType(structure_in%process, structure_out%process)
     if (verbose > 0) write(iu6, *) 'copied amns%process'

     call copy_arr_type_tables(structure_in%tables, structure_out%tables)
     if (verbose > 0) write(iu6, *) 'copied amns%tables'

     call copy_arr_type_tables_coord(structure_in%tables_coord, structure_out%tables_coord)
     if (verbose > 0) write(iu6, *) 'copied amns%tables_coord'

     call copy_arr_type_version_ind(structure_in%version_ind, structure_out%version_ind)
     if (verbose > 0) write(iu6, *) 'copied amns%version_ind'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied amns%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied amns%time'

   end subroutine copy_type_amns

   subroutine copy_arr_type_amns(structure_in, structure_out)

     implicit none

     type (type_amns), pointer :: structure_in(:)
     type (type_amns), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_amns(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_amns'
     end if

   end subroutine copy_arr_type_amns

   subroutine copy_type_antennas(structure_in, structure_out)
 
     implicit none
 
     type (type_antennas), intent(in) :: structure_in
     type (type_antennas), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied antennas%datainfo'

     call copy_arr_type_antenna_ec(structure_in%antenna_ec, structure_out%antenna_ec)
     if (verbose > 0) write(iu6, *) 'copied antennas%antenna_ec'

     call copy_arr_type_antenna_ic(structure_in%antenna_ic, structure_out%antenna_ic)
     if (verbose > 0) write(iu6, *) 'copied antennas%antenna_ic'

     call copy_arr_type_antenna_lh(structure_in%antenna_lh, structure_out%antenna_lh)
     if (verbose > 0) write(iu6, *) 'copied antennas%antenna_lh'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied antennas%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied antennas%time'

   end subroutine copy_type_antennas

   subroutine copy_arr_type_antennas(structure_in, structure_out)

     implicit none

     type (type_antennas), pointer :: structure_in(:)
     type (type_antennas), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_antennas(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_antennas'
     end if

   end subroutine copy_arr_type_antennas

   subroutine copy_type_bb_shield(structure_in, structure_out)
 
     implicit none
 
     type (type_bb_shield), intent(in) :: structure_in
     type (type_bb_shield), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%datainfo'

     call copy_type_vecstring_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%type'

     call copy_type_limits(structure_in%limits, structure_out%limits)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%limits'

     call copy_type_float(structure_in%li6_enrich, structure_out%li6_enrich)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%li6_enrich'

     call copy_type_geom(structure_in%geom, structure_out%geom)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%geom'

     call copy_type_neut_results(structure_in%neut_results, structure_out%neut_results)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%neut_results'

     call copy_type_shield(structure_in%shield, structure_out%shield)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%shield'

     call copy_type_bb(structure_in%bb, structure_out%bb)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%bb'

     call copy_type_hcll(structure_in%hcll, structure_out%hcll)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%hcll'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied bb_shield%time'

   end subroutine copy_type_bb_shield

   subroutine copy_arr_type_bb_shield(structure_in, structure_out)

     implicit none

     type (type_bb_shield), pointer :: structure_in(:)
     type (type_bb_shield), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_bb_shield(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_bb_shield'
     end if

   end subroutine copy_arr_type_bb_shield

   subroutine copy_type_compositionc(structure_in, structure_out)
 
     implicit none
 
     type (type_compositionc), intent(in) :: structure_in
     type (type_compositionc), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied compositionc%datainfo'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied compositionc%compositions'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied compositionc%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied compositionc%time'

   end subroutine copy_type_compositionc

   subroutine copy_arr_type_compositionc(structure_in, structure_out)

     implicit none

     type (type_compositionc), pointer :: structure_in(:)
     type (type_compositionc), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_compositionc(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_compositionc'
     end if

   end subroutine copy_arr_type_compositionc

   subroutine copy_type_coredelta(structure_in, structure_out)
 
     implicit none
 
     type (type_coredelta), intent(in) :: structure_in
     type (type_coredelta), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied coredelta%datainfo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied coredelta%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied coredelta%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coredelta%compositions'

     call copy_arr_type_coredelta_values(structure_in%values, structure_out%values)
     if (verbose > 0) write(iu6, *) 'copied coredelta%values'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coredelta%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied coredelta%time'

   end subroutine copy_type_coredelta

   subroutine copy_arr_type_coredelta(structure_in, structure_out)

     implicit none

     type (type_coredelta), pointer :: structure_in(:)
     type (type_coredelta), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coredelta(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coredelta'
     end if

   end subroutine copy_arr_type_coredelta

   subroutine copy_type_corefast(structure_in, structure_out)
 
     implicit none
 
     type (type_corefast), intent(in) :: structure_in
     type (type_corefast), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied corefast%datainfo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied corefast%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied corefast%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied corefast%compositions'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied corefast%toroid_field'

     call copy_arr_type_corefast_values(structure_in%values, structure_out%values)
     if (verbose > 0) write(iu6, *) 'copied corefast%values'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied corefast%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied corefast%time'

   end subroutine copy_type_corefast

   subroutine copy_arr_type_corefast(structure_in, structure_out)

     implicit none

     type (type_corefast), pointer :: structure_in(:)
     type (type_corefast), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefast(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefast'
     end if

   end subroutine copy_arr_type_corefast

   subroutine copy_type_coreimpur(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpur), intent(in) :: structure_in
     type (type_coreimpur), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%datainfo'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%rho_tor'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%area'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%source'

     call copy_type_vecint_type(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%flag'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%compositions'

     call copy_type_vecstring_type(structure_in%atomic_data, structure_out%atomic_data)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%atomic_data'

     call copy_arr_type_impurity_type(structure_in%impurity, structure_out%impurity)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%impurity'

     call copy_type_coreimpurediag_type(structure_in%diagnostic, structure_out%diagnostic)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%diagnostic'

     call copy_type_coreimpurediag_sum(structure_in%diagnosticsum, structure_out%diagnosticsum)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%diagnosticsum'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied coreimpur%time'

   end subroutine copy_type_coreimpur

   subroutine copy_arr_type_coreimpur(structure_in, structure_out)

     implicit none

     type (type_coreimpur), pointer :: structure_in(:)
     type (type_coreimpur), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpur(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpur'
     end if

   end subroutine copy_arr_type_coreimpur

   subroutine copy_type_coreneutrals(structure_in, structure_out)
 
     implicit none
 
     type (type_coreneutrals), intent(in) :: structure_in
     type (type_coreneutrals), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%datainfo'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%area'

     call copy_type_composition_neutrals(structure_in%neutcompo, structure_out%neutcompo)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%neutcompo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%compositions'

     call copy_arr_type_neutral_complex_type(structure_in%profiles, structure_out%profiles)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%profiles'

     call copy_arr_type_coefficients_neutrals(structure_in%ioncoeff, structure_out%ioncoeff)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%ioncoeff'

     call copy_arr_type_impcoeff(structure_in%impcoeff, structure_out%impcoeff)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%impcoeff'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals%time'

   end subroutine copy_type_coreneutrals

   subroutine copy_arr_type_coreneutrals(structure_in, structure_out)

     implicit none

     type (type_coreneutrals), pointer :: structure_in(:)
     type (type_coreneutrals), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreneutrals(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreneutrals'
     end if

   end subroutine copy_arr_type_coreneutrals

   subroutine copy_type_coreprof(structure_in, structure_out)
 
     implicit none
 
     type (type_coreprof), intent(in) :: structure_in
     type (type_coreprof), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied coreprof%datainfo'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied coreprof%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied coreprof%rho_tor'

     call copy_type_vecflt_type(structure_in%drho_dt, structure_out%drho_dt)
     if (verbose > 0) write(iu6, *) 'copied coreprof%drho_dt'

     call copy_type_toroid_field(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied coreprof%toroid_field'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied coreprof%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied coreprof%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coreprof%compositions'

     call copy_type_psi(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied coreprof%psi'

     call copy_type_corefield(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied coreprof%te'

     call copy_type_corefieldion(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied coreprof%ti'

     call copy_type_corefield(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied coreprof%ne'

     call copy_type_corefieldion(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied coreprof%ni'

     call copy_type_corefieldion(structure_in%vtor, structure_out%vtor)
     if (verbose > 0) write(iu6, *) 'copied coreprof%vtor'

     call copy_type_profiles1d(structure_in%profiles1d, structure_out%profiles1d)
     if (verbose > 0) write(iu6, *) 'copied coreprof%profiles1d'

     call copy_type_globalparam(structure_in%globalparam, structure_out%globalparam)
     if (verbose > 0) write(iu6, *) 'copied coreprof%globalparam'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coreprof%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied coreprof%time'

   end subroutine copy_type_coreprof

   subroutine copy_arr_type_coreprof(structure_in, structure_out)

     implicit none

     type (type_coreprof), pointer :: structure_in(:)
     type (type_coreprof), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreprof(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreprof'
     end if

   end subroutine copy_arr_type_coreprof

   subroutine copy_type_coresource(structure_in, structure_out)
 
     implicit none
 
     type (type_coresource), intent(in) :: structure_in
     type (type_coresource), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied coresource%datainfo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied coresource%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied coresource%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coresource%compositions'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied coresource%toroid_field'

     call copy_arr_type_coresource_values(structure_in%values, structure_out%values)
     if (verbose > 0) write(iu6, *) 'copied coresource%values'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coresource%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied coresource%time'

   end subroutine copy_type_coresource

   subroutine copy_arr_type_coresource(structure_in, structure_out)

     implicit none

     type (type_coresource), pointer :: structure_in(:)
     type (type_coresource), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coresource(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coresource'
     end if

   end subroutine copy_arr_type_coresource

   subroutine copy_type_coretransp(structure_in, structure_out)
 
     implicit none
 
     type (type_coretransp), intent(in) :: structure_in
     type (type_coretransp), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied coretransp%datainfo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied coretransp%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied coretransp%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coretransp%compositions'

     call copy_arr_type_coretransp_values(structure_in%values, structure_out%values)
     if (verbose > 0) write(iu6, *) 'copied coretransp%values'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coretransp%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied coretransp%time'

   end subroutine copy_type_coretransp

   subroutine copy_arr_type_coretransp(structure_in, structure_out)

     implicit none

     type (type_coretransp), pointer :: structure_in(:)
     type (type_coretransp), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coretransp(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coretransp'
     end if

   end subroutine copy_arr_type_coretransp

   subroutine copy_type_cxdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_cxdiag), intent(in) :: structure_in
     type (type_cxdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied cxdiag%datainfo'

     call copy_type_cxsetup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied cxdiag%setup'

     call copy_type_cxmeasure(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied cxdiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied cxdiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied cxdiag%time'

   end subroutine copy_type_cxdiag

   subroutine copy_arr_type_cxdiag(structure_in, structure_out)

     implicit none

     type (type_cxdiag), pointer :: structure_in(:)
     type (type_cxdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_cxdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_cxdiag'
     end if

   end subroutine copy_arr_type_cxdiag

   subroutine copy_type_distribution(structure_in, structure_out)
 
     implicit none
 
     type (type_distribution), intent(in) :: structure_in
     type (type_distribution), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied distribution%datainfo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied distribution%composition'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied distribution%compositions'

     call copy_arr_type_distri_vec(structure_in%distri_vec, structure_out%distri_vec)
     if (verbose > 0) write(iu6, *) 'copied distribution%distri_vec'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied distribution%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied distribution%time'

   end subroutine copy_type_distribution

   subroutine copy_arr_type_distribution(structure_in, structure_out)

     implicit none

     type (type_distribution), pointer :: structure_in(:)
     type (type_distribution), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distribution(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distribution'
     end if

   end subroutine copy_arr_type_distribution

   subroutine copy_type_distsource(structure_in, structure_out)
 
     implicit none
 
     type (type_distsource), intent(in) :: structure_in
     type (type_distsource), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied distsource%datainfo'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied distsource%composition'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied distsource%compositions'

     call copy_arr_type_distsource_source(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied distsource%source'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied distsource%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied distsource%time'

   end subroutine copy_type_distsource

   subroutine copy_arr_type_distsource(structure_in, structure_out)

     implicit none

     type (type_distsource), pointer :: structure_in(:)
     type (type_distsource), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distsource(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distsource'
     end if

   end subroutine copy_arr_type_distsource

   subroutine copy_type_ecediag(structure_in, structure_out)
 
     implicit none
 
     type (type_ecediag), intent(in) :: structure_in
     type (type_ecediag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied ecediag%datainfo'

     call copy_type_ecesetup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied ecediag%setup'

     call copy_type_ecemeasure(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied ecediag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied ecediag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied ecediag%time'

   end subroutine copy_type_ecediag

   subroutine copy_arr_type_ecediag(structure_in, structure_out)

     implicit none

     type (type_ecediag), pointer :: structure_in(:)
     type (type_ecediag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ecediag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ecediag'
     end if

   end subroutine copy_arr_type_ecediag

   subroutine copy_type_edge(structure_in, structure_out)
 
     implicit none
 
     type (type_edge), intent(in) :: structure_in
     type (type_edge), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied edge%datainfo'

     call copy_type_complexgrid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied edge%grid'

     call copy_arr_type_species_desc(structure_in%species, structure_out%species)
     if (verbose > 0) write(iu6, *) 'copied edge%species'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied edge%compositions'

     call copy_type_edge_fluid(structure_in%fluid, structure_out%fluid)
     if (verbose > 0) write(iu6, *) 'copied edge%fluid'

     call copy_type_edge_kinetic(structure_in%kinetic, structure_out%kinetic)
     if (verbose > 0) write(iu6, *) 'copied edge%kinetic'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied edge%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied edge%time'

   end subroutine copy_type_edge

   subroutine copy_arr_type_edge(structure_in, structure_out)

     implicit none

     type (type_edge), pointer :: structure_in(:)
     type (type_edge), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge'
     end if

   end subroutine copy_arr_type_edge

   subroutine copy_type_efcc(structure_in, structure_out)
 
     implicit none
 
     type (type_efcc), intent(in) :: structure_in
     type (type_efcc), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied efcc%datainfo'

     call copy_arr_type_coil(structure_in%coil, structure_out%coil)
     if (verbose > 0) write(iu6, *) 'copied efcc%coil'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied efcc%time'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied efcc%codeparam'

   end subroutine copy_type_efcc

   subroutine copy_arr_type_efcc(structure_in, structure_out)

     implicit none

     type (type_efcc), pointer :: structure_in(:)
     type (type_efcc), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_efcc(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_efcc'
     end if

   end subroutine copy_arr_type_efcc

   subroutine copy_type_equilibrium(structure_in, structure_out)
 
     implicit none
 
     type (type_equilibrium), intent(in) :: structure_in
     type (type_equilibrium), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%datainfo'

     call copy_type_eqconstraint(structure_in%eqconstraint, structure_out%eqconstraint)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%eqconstraint'

     call copy_type_eqgeometry(structure_in%eqgeometry, structure_out%eqgeometry)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%eqgeometry'

     call copy_type_flush(structure_in%flush, structure_out%flush)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%flush'

     call copy_type_global_param(structure_in%global_param, structure_out%global_param)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%global_param'

     call copy_type_profiles_1d(structure_in%profiles_1d, structure_out%profiles_1d)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%profiles_1d'

     call copy_arr_type_equilibrium_profiles_2d(structure_in%profiles_2d, structure_out%profiles_2d)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%profiles_2d'

     call copy_type_coord_sys(structure_in%coord_sys, structure_out%coord_sys)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%coord_sys'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%time'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied equilibrium%codeparam'

   end subroutine copy_type_equilibrium

   subroutine copy_arr_type_equilibrium(structure_in, structure_out)

     implicit none

     type (type_equilibrium), pointer :: structure_in(:)
     type (type_equilibrium), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_equilibrium(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_equilibrium'
     end if

   end subroutine copy_arr_type_equilibrium

   subroutine copy_type_fusiondiag(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag), intent(in) :: structure_in
     type (type_fusiondiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag%datainfo'

     call copy_arr_type_fusiondiag_fus_product(structure_in%fus_product, structure_out%fus_product)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag%fus_product'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag%time'

   end subroutine copy_type_fusiondiag

   subroutine copy_arr_type_fusiondiag(structure_in, structure_out)

     implicit none

     type (type_fusiondiag), pointer :: structure_in(:)
     type (type_fusiondiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag'
     end if

   end subroutine copy_arr_type_fusiondiag

   subroutine copy_type_halphadiag(structure_in, structure_out)
 
     implicit none
 
     type (type_halphadiag), intent(in) :: structure_in
     type (type_halphadiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied halphadiag%datainfo'

     call copy_type_halpha_setup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied halphadiag%setup'

     call copy_type_exp1D(structure_in%intensity, structure_out%intensity)
     if (verbose > 0) write(iu6, *) 'copied halphadiag%intensity'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied halphadiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied halphadiag%time'

   end subroutine copy_type_halphadiag

   subroutine copy_arr_type_halphadiag(structure_in, structure_out)

     implicit none

     type (type_halphadiag), pointer :: structure_in(:)
     type (type_halphadiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_halphadiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_halphadiag'
     end if

   end subroutine copy_arr_type_halphadiag

   subroutine copy_type_heat_sources(structure_in, structure_out)
 
     implicit none
 
     type (type_heat_sources), intent(in) :: structure_in
     type (type_heat_sources), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied heat_sources%datainfo'

     call copy_arr_type_calorimetry_heat_source(structure_in%sources, structure_out%sources)
     if (verbose > 0) write(iu6, *) 'copied heat_sources%sources'

     call copy_arr_type_calorimetry_heat_source(structure_in%sinks, structure_out%sinks)
     if (verbose > 0) write(iu6, *) 'copied heat_sources%sinks'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied heat_sources%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied heat_sources%time'

   end subroutine copy_type_heat_sources

   subroutine copy_arr_type_heat_sources(structure_in, structure_out)

     implicit none

     type (type_heat_sources), pointer :: structure_in(:)
     type (type_heat_sources), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_heat_sources(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_heat_sources'
     end if

   end subroutine copy_arr_type_heat_sources

   subroutine copy_type_interfdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_interfdiag), intent(in) :: structure_in
     type (type_interfdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied interfdiag%datainfo'

     call copy_type_vecstring_type(structure_in%expression, structure_out%expression)
     if (verbose > 0) write(iu6, *) 'copied interfdiag%expression'

     call copy_type_setup_line(structure_in%setup_line, structure_out%setup_line)
     if (verbose > 0) write(iu6, *) 'copied interfdiag%setup_line'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied interfdiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied interfdiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied interfdiag%time'

   end subroutine copy_type_interfdiag

   subroutine copy_arr_type_interfdiag(structure_in, structure_out)

     implicit none

     type (type_interfdiag), pointer :: structure_in(:)
     type (type_interfdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_interfdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_interfdiag'
     end if

   end subroutine copy_arr_type_interfdiag

   subroutine copy_type_ironmodel(structure_in, structure_out)
 
     implicit none
 
     type (type_ironmodel), intent(in) :: structure_in
     type (type_ironmodel), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied ironmodel%datainfo'

     call copy_type_desc_iron(structure_in%desc_iron, structure_out%desc_iron)
     if (verbose > 0) write(iu6, *) 'copied ironmodel%desc_iron'

     call copy_type_magnetise(structure_in%magnetise, structure_out%magnetise)
     if (verbose > 0) write(iu6, *) 'copied ironmodel%magnetise'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied ironmodel%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied ironmodel%time'

   end subroutine copy_type_ironmodel

   subroutine copy_arr_type_ironmodel(structure_in, structure_out)

     implicit none

     type (type_ironmodel), pointer :: structure_in(:)
     type (type_ironmodel), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ironmodel(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ironmodel'
     end if

   end subroutine copy_arr_type_ironmodel

   subroutine copy_type_langmuirdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_langmuirdiag), intent(in) :: structure_in
     type (type_langmuirdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%datainfo'

     call copy_type_lang_measure(structure_in%potential, structure_out%potential)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%potential'

     call copy_type_lang_measure(structure_in%bias, structure_out%bias)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%bias'

     call copy_type_lang_measure(structure_in%jsat, structure_out%jsat)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%jsat'

     call copy_type_lang_derived(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%ne'

     call copy_type_lang_derived(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%te'

     call copy_type_lang_derived(structure_in%machpar, structure_out%machpar)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%machpar'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied langmuirdiag%time'

   end subroutine copy_type_langmuirdiag

   subroutine copy_arr_type_langmuirdiag(structure_in, structure_out)

     implicit none

     type (type_langmuirdiag), pointer :: structure_in(:)
     type (type_langmuirdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_langmuirdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_langmuirdiag'
     end if

   end subroutine copy_arr_type_langmuirdiag

   subroutine copy_type_launchs(structure_in, structure_out)
 
     implicit none
 
     type (type_launchs), intent(in) :: structure_in
     type (type_launchs), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied launchs%datainfo'

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied launchs%name'

     call copy_type_vecstring_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied launchs%type'

     call copy_type_vecflt_type(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied launchs%frequency'

     call copy_type_vecint_type(structure_in%mode, structure_out%mode)
     if (verbose > 0) write(iu6, *) 'copied launchs%mode'

     call copy_type_rzphi1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied launchs%position'

     call copy_type_spectrum(structure_in%spectrum, structure_out%spectrum)
     if (verbose > 0) write(iu6, *) 'copied launchs%spectrum'

     call copy_type_launchs_rfbeam(structure_in%beam, structure_out%beam)
     if (verbose > 0) write(iu6, *) 'copied launchs%beam'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied launchs%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied launchs%time'

   end subroutine copy_type_launchs

   subroutine copy_arr_type_launchs(structure_in, structure_out)

     implicit none

     type (type_launchs), pointer :: structure_in(:)
     type (type_launchs), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchs(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchs'
     end if

   end subroutine copy_arr_type_launchs

   subroutine copy_type_lithiumdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_lithiumdiag), intent(in) :: structure_in
     type (type_lithiumdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied lithiumdiag%datainfo'

     call copy_type_lithsetup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied lithiumdiag%setup'

     call copy_type_lithmeasure(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied lithiumdiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied lithiumdiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied lithiumdiag%time'

   end subroutine copy_type_lithiumdiag

   subroutine copy_arr_type_lithiumdiag(structure_in, structure_out)

     implicit none

     type (type_lithiumdiag), pointer :: structure_in(:)
     type (type_lithiumdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_lithiumdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_lithiumdiag'
     end if

   end subroutine copy_arr_type_lithiumdiag

   subroutine copy_type_magdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_magdiag), intent(in) :: structure_in
     type (type_magdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied magdiag%datainfo'

     call copy_type_exp0D(structure_in%ip, structure_out%ip)
     if (verbose > 0) write(iu6, *) 'copied magdiag%ip'

     call copy_type_exp0D(structure_in%diamagflux, structure_out%diamagflux)
     if (verbose > 0) write(iu6, *) 'copied magdiag%diamagflux'

     call copy_type_exp0D(structure_in%diamagener, structure_out%diamagener)
     if (verbose > 0) write(iu6, *) 'copied magdiag%diamagener'

     call copy_type_flux_loops(structure_in%flux_loops, structure_out%flux_loops)
     if (verbose > 0) write(iu6, *) 'copied magdiag%flux_loops'

     call copy_type_bpol_probes(structure_in%bpol_probes, structure_out%bpol_probes)
     if (verbose > 0) write(iu6, *) 'copied magdiag%bpol_probes'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied magdiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied magdiag%time'

   end subroutine copy_type_magdiag

   subroutine copy_arr_type_magdiag(structure_in, structure_out)

     implicit none

     type (type_magdiag), pointer :: structure_in(:)
     type (type_magdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_magdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_magdiag'
     end if

   end subroutine copy_arr_type_magdiag

   subroutine copy_type_mhd(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd), intent(in) :: structure_in
     type (type_mhd), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied mhd%datainfo'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied mhd%toroid_field'

     call copy_arr_type_mhd_mode(structure_in%n, structure_out%n)
     if (verbose > 0) write(iu6, *) 'copied mhd%n'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied mhd%time'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied mhd%codeparam'

   end subroutine copy_type_mhd

   subroutine copy_arr_type_mhd(structure_in, structure_out)

     implicit none

     type (type_mhd), pointer :: structure_in(:)
     type (type_mhd), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd'
     end if

   end subroutine copy_arr_type_mhd

   subroutine copy_type_msediag(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag), intent(in) :: structure_in
     type (type_msediag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied msediag%datainfo'

     call copy_type_polarimetry(structure_in%polarimetry, structure_out%polarimetry)
     if (verbose > 0) write(iu6, *) 'copied msediag%polarimetry'

     call copy_type_spectral(structure_in%spectral, structure_out%spectral)
     if (verbose > 0) write(iu6, *) 'copied msediag%spectral'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied msediag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied msediag%time'

   end subroutine copy_type_msediag

   subroutine copy_arr_type_msediag(structure_in, structure_out)

     implicit none

     type (type_msediag), pointer :: structure_in(:)
     type (type_msediag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag'
     end if

   end subroutine copy_arr_type_msediag

   subroutine copy_type_nbi(structure_in, structure_out)
 
     implicit none
 
     type (type_nbi), intent(in) :: structure_in
     type (type_nbi), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied nbi%datainfo'

     call copy_arr_type_nbi_unit(structure_in%nbi_unit, structure_out%nbi_unit)
     if (verbose > 0) write(iu6, *) 'copied nbi%nbi_unit'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied nbi%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied nbi%time'

   end subroutine copy_type_nbi

   subroutine copy_arr_type_nbi(structure_in, structure_out)

     implicit none

     type (type_nbi), pointer :: structure_in(:)
     type (type_nbi), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_nbi(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_nbi'
     end if

   end subroutine copy_arr_type_nbi

   subroutine copy_type_neoclassic(structure_in, structure_out)
 
     implicit none
 
     type (type_neoclassic), intent(in) :: structure_in
     type (type_neoclassic), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%datainfo'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%rho_tor'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%composition'

     call copy_type_desc_impur(structure_in%desc_impur, structure_out%desc_impur)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%desc_impur'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%compositions'

     call copy_type_transcoefion(structure_in%ni_neo, structure_out%ni_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%ni_neo'

     call copy_type_transcoefel(structure_in%ne_neo, structure_out%ne_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%ne_neo'

     call copy_arr_type_transcoefimp(structure_in%nz_neo, structure_out%nz_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%nz_neo'

     call copy_type_transcoefion(structure_in%ti_neo, structure_out%ti_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%ti_neo'

     call copy_type_transcoefel(structure_in%te_neo, structure_out%te_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%te_neo'

     call copy_arr_type_transcoefimp(structure_in%tz_neo, structure_out%tz_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%tz_neo'

     call copy_type_transcoefel(structure_in%mtor_neo, structure_out%mtor_neo)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%mtor_neo'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%sigma'

     call copy_type_vecflt_type(structure_in%jboot, structure_out%jboot)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%jboot'

     call copy_type_vecflt_type(structure_in%er, structure_out%er)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%er'

     call copy_type_matflt_type(structure_in%vpol, structure_out%vpol)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%vpol'

     call copy_type_matflt_type(structure_in%vtor, structure_out%vtor)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%vtor'

     call copy_type_matflt_type(structure_in%mach, structure_out%mach)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%mach'

     call copy_type_vecflt_type(structure_in%utheta_e, structure_out%utheta_e)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%utheta_e'

     call copy_type_matflt_type(structure_in%utheta_i, structure_out%utheta_i)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%utheta_i'

     call copy_type_matflt_type(structure_in%viscosity_par, structure_out%viscosity_par)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%viscosity_par'

     call copy_arr_type_neoclassic_impurity(structure_in%impurity, structure_out%impurity)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%impurity'

     call copy_type_array3dflt_type(structure_in%fext, structure_out%fext)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%fext'

     call copy_type_vecflt_type(structure_in%jext, structure_out%jext)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%jext'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%time'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied neoclassic%codeparam'

   end subroutine copy_type_neoclassic

   subroutine copy_arr_type_neoclassic(structure_in, structure_out)

     implicit none

     type (type_neoclassic), pointer :: structure_in(:)
     type (type_neoclassic), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_neoclassic(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_neoclassic'
     end if

   end subroutine copy_arr_type_neoclassic

   subroutine copy_type_ntm(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm), intent(in) :: structure_in
     type (type_ntm), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied ntm%datainfo'

     call copy_arr_type_ntm_mode(structure_in%mode, structure_out%mode)
     if (verbose > 0) write(iu6, *) 'copied ntm%mode'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied ntm%time'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied ntm%codeparam'

   end subroutine copy_type_ntm

   subroutine copy_arr_type_ntm(structure_in, structure_out)

     implicit none

     type (type_ntm), pointer :: structure_in(:)
     type (type_ntm), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm'
     end if

   end subroutine copy_arr_type_ntm

   subroutine copy_type_orbit(structure_in, structure_out)
 
     implicit none
 
     type (type_orbit), intent(in) :: structure_in
     type (type_orbit), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied orbit%datainfo'

     call copy_type_com(structure_in%com, structure_out%com)
     if (verbose > 0) write(iu6, *) 'copied orbit%com'

     call copy_type_trace(structure_in%trace, structure_out%trace)
     if (verbose > 0) write(iu6, *) 'copied orbit%trace'

     call copy_type_orbit_global_param(structure_in%global_param, structure_out%global_param)
     if (verbose > 0) write(iu6, *) 'copied orbit%global_param'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied orbit%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied orbit%time'

   end subroutine copy_type_orbit

   subroutine copy_arr_type_orbit(structure_in, structure_out)

     implicit none

     type (type_orbit), pointer :: structure_in(:)
     type (type_orbit), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_orbit(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_orbit'
     end if

   end subroutine copy_arr_type_orbit

   subroutine copy_type_pellets(structure_in, structure_out)
 
     implicit none
 
     type (type_pellets), intent(in) :: structure_in
     type (type_pellets), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied pellets%datainfo'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied pellets%compositions'

     call copy_arr_type_pellet(structure_in%pellet, structure_out%pellet)
     if (verbose > 0) write(iu6, *) 'copied pellets%pellet'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied pellets%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied pellets%time'

   end subroutine copy_type_pellets

   subroutine copy_arr_type_pellets(structure_in, structure_out)

     implicit none

     type (type_pellets), pointer :: structure_in(:)
     type (type_pellets), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellets(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellets'
     end if

   end subroutine copy_arr_type_pellets

   subroutine copy_type_pfsystems(structure_in, structure_out)
 
     implicit none
 
     type (type_pfsystems), intent(in) :: structure_in
     type (type_pfsystems), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%datainfo'

     call copy_type_pfcoils(structure_in%pfcoils, structure_out%pfcoils)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%pfcoils'

     call copy_type_pfpassive(structure_in%pfpassive, structure_out%pfpassive)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%pfpassive'

     call copy_type_pfcircuits(structure_in%pfcircuits, structure_out%pfcircuits)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%pfcircuits'

     call copy_type_pfsupplies(structure_in%pfsupplies, structure_out%pfsupplies)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%pfsupplies'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied pfsystems%time'

   end subroutine copy_type_pfsystems

   subroutine copy_arr_type_pfsystems(structure_in, structure_out)

     implicit none

     type (type_pfsystems), pointer :: structure_in(:)
     type (type_pfsystems), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfsystems(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfsystems'
     end if

   end subroutine copy_arr_type_pfsystems

   subroutine copy_type_polardiag(structure_in, structure_out)
 
     implicit none
 
     type (type_polardiag), intent(in) :: structure_in
     type (type_polardiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied polardiag%datainfo'

     call copy_type_vecstring_type(structure_in%expression, structure_out%expression)
     if (verbose > 0) write(iu6, *) 'copied polardiag%expression'

     call copy_type_setup_line(structure_in%setup_line, structure_out%setup_line)
     if (verbose > 0) write(iu6, *) 'copied polardiag%setup_line'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied polardiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied polardiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied polardiag%time'

   end subroutine copy_type_polardiag

   subroutine copy_arr_type_polardiag(structure_in, structure_out)

     implicit none

     type (type_polardiag), pointer :: structure_in(:)
     type (type_polardiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_polardiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_polardiag'
     end if

   end subroutine copy_arr_type_polardiag

   subroutine copy_type_power_conv(structure_in, structure_out)
 
     implicit none
 
     type (type_power_conv), intent(in) :: structure_in
     type (type_power_conv), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied power_conv%datainfo'

     call copy_type_vecstring_type(structure_in%cycle_type, structure_out%cycle_type)
     if (verbose > 0) write(iu6, *) 'copied power_conv%cycle_type'

     call copy_arr_type_circuits(structure_in%circuits, structure_out%circuits)
     if (verbose > 0) write(iu6, *) 'copied power_conv%circuits'

     call copy_type_float(structure_in%power_recirc, structure_out%power_recirc)
     if (verbose > 0) write(iu6, *) 'copied power_conv%power_recirc'

     call copy_type_float(structure_in%power_net, structure_out%power_net)
     if (verbose > 0) write(iu6, *) 'copied power_conv%power_net'

     call copy_type_float(structure_in%power_int, structure_out%power_int)
     if (verbose > 0) write(iu6, *) 'copied power_conv%power_int'

     call copy_type_float(structure_in%efficiency, structure_out%efficiency)
     if (verbose > 0) write(iu6, *) 'copied power_conv%efficiency'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied power_conv%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied power_conv%time'

   end subroutine copy_type_power_conv

   subroutine copy_arr_type_power_conv(structure_in, structure_out)

     implicit none

     type (type_power_conv), pointer :: structure_in(:)
     type (type_power_conv), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_power_conv(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_power_conv'
     end if

   end subroutine copy_arr_type_power_conv

   subroutine copy_type_reflectomet(structure_in, structure_out)
 
     implicit none
 
     type (type_reflectomet), intent(in) :: structure_in
     type (type_reflectomet), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied reflectomet%datainfo'

     call copy_arr_type_refl_receive(structure_in%refl_receive, structure_out%refl_receive)
     if (verbose > 0) write(iu6, *) 'copied reflectomet%refl_receive'

     call copy_arr_type_reflectometry_antennas(structure_in%antennas, structure_out%antennas)
     if (verbose > 0) write(iu6, *) 'copied reflectomet%antennas'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied reflectomet%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied reflectomet%time'

   end subroutine copy_type_reflectomet

   subroutine copy_arr_type_reflectomet(structure_in, structure_out)

     implicit none

     type (type_reflectomet), pointer :: structure_in(:)
     type (type_reflectomet), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reflectomet(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reflectomet'
     end if

   end subroutine copy_arr_type_reflectomet

   subroutine copy_type_rfadiag(structure_in, structure_out)
 
     implicit none
 
     type (type_rfadiag), intent(in) :: structure_in
     type (type_rfadiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied rfadiag%datainfo'

     call copy_type_rfasetup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied rfadiag%setup'

     call copy_type_rfameasure(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied rfadiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied rfadiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied rfadiag%time'

   end subroutine copy_type_rfadiag

   subroutine copy_arr_type_rfadiag(structure_in, structure_out)

     implicit none

     type (type_rfadiag), pointer :: structure_in(:)
     type (type_rfadiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rfadiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rfadiag'
     end if

   end subroutine copy_arr_type_rfadiag

   subroutine copy_type_sawteeth(structure_in, structure_out)
 
     implicit none
 
     type (type_sawteeth), intent(in) :: structure_in
     type (type_sawteeth), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%datainfo'

     call copy_type_integer(structure_in%crash_trig, structure_out%crash_trig)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%crash_trig'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%composition'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%rho_tor'

     call copy_type_sawteeth_profiles1d(structure_in%profiles1d, structure_out%profiles1d)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%profiles1d'

     call copy_type_sawteeth_diags(structure_in%diags, structure_out%diags)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%diags'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied sawteeth%time'

   end subroutine copy_type_sawteeth

   subroutine copy_arr_type_sawteeth(structure_in, structure_out)

     implicit none

     type (type_sawteeth), pointer :: structure_in(:)
     type (type_sawteeth), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sawteeth(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sawteeth'
     end if

   end subroutine copy_arr_type_sawteeth

   subroutine copy_type_scenario(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario), intent(in) :: structure_in
     type (type_scenario), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied scenario%datainfo'

     call copy_type_scenario_centre(structure_in%centre, structure_out%centre)
     if (verbose > 0) write(iu6, *) 'copied scenario%centre'

     call copy_type_scenario_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied scenario%composition'

     call copy_type_scenario_configuration(structure_in%configs, structure_out%configs)
     if (verbose > 0) write(iu6, *) 'copied scenario%configs'

     call copy_type_scenario_confinement(structure_in%confinement, structure_out%confinement)
     if (verbose > 0) write(iu6, *) 'copied scenario%confinement'

     call copy_type_scenario_currents(structure_in%currents, structure_out%currents)
     if (verbose > 0) write(iu6, *) 'copied scenario%currents'

     call copy_type_scenario_edge(structure_in%edge, structure_out%edge)
     if (verbose > 0) write(iu6, *) 'copied scenario%edge'

     call copy_type_scenario_energy(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied scenario%energy'

     call copy_type_eqgeometry(structure_in%eqgeometry, structure_out%eqgeometry)
     if (verbose > 0) write(iu6, *) 'copied scenario%eqgeometry'

     call copy_type_scenario_global(structure_in%global_param, structure_out%global_param)
     if (verbose > 0) write(iu6, *) 'copied scenario%global_param'

     call copy_type_scenario_heat_power(structure_in%heat_power, structure_out%heat_power)
     if (verbose > 0) write(iu6, *) 'copied scenario%heat_power'

     call copy_type_scenario_itb(structure_in%itb, structure_out%itb)
     if (verbose > 0) write(iu6, *) 'copied scenario%itb'

     call copy_type_scenario_lim_div_wall(structure_in%lim_div_wall, structure_out%lim_div_wall)
     if (verbose > 0) write(iu6, *) 'copied scenario%lim_div_wall'

     call copy_type_scenario_line_ave(structure_in%line_ave, structure_out%line_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario%line_ave'

     call copy_type_scenario_neutron(structure_in%neutron, structure_out%neutron)
     if (verbose > 0) write(iu6, *) 'copied scenario%neutron'

     call copy_type_scenario_ninety_five(structure_in%ninety_five, structure_out%ninety_five)
     if (verbose > 0) write(iu6, *) 'copied scenario%ninety_five'

     call copy_type_scenario_pedestal(structure_in%pedestal, structure_out%pedestal)
     if (verbose > 0) write(iu6, *) 'copied scenario%pedestal'

     call copy_type_scenario_references(structure_in%references, structure_out%references)
     if (verbose > 0) write(iu6, *) 'copied scenario%references'

     call copy_type_scenario_reactor(structure_in%reactor, structure_out%reactor)
     if (verbose > 0) write(iu6, *) 'copied scenario%reactor'

     call copy_type_scenario_sol(structure_in%sol, structure_out%sol)
     if (verbose > 0) write(iu6, *) 'copied scenario%sol'

     call copy_type_scenario_vol_ave(structure_in%vol_ave, structure_out%vol_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario%vol_ave'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied scenario%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied scenario%time'

   end subroutine copy_type_scenario

   subroutine copy_arr_type_scenario(structure_in, structure_out)

     implicit none

     type (type_scenario), pointer :: structure_in(:)
     type (type_scenario), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario'
     end if

   end subroutine copy_arr_type_scenario

   subroutine copy_type_solcurdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_solcurdiag), intent(in) :: structure_in
     type (type_solcurdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag%datainfo'

     call copy_arr_type_solcurdiag_sol_current(structure_in%sol_current, structure_out%sol_current)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag%sol_current'

     call copy_arr_type_clusters(structure_in%clusters, structure_out%clusters)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag%clusters'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag%time'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag%codeparam'

   end subroutine copy_type_solcurdiag

   subroutine copy_arr_type_solcurdiag(structure_in, structure_out)

     implicit none

     type (type_solcurdiag), pointer :: structure_in(:)
     type (type_solcurdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_solcurdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_solcurdiag'
     end if

   end subroutine copy_arr_type_solcurdiag

   subroutine copy_type_temporary(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary), intent(in) :: structure_in
     type (type_temporary), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied temporary%datainfo'

     call copy_type_temporary_nt(structure_in%non_timed, structure_out%non_timed)
     if (verbose > 0) write(iu6, *) 'copied temporary%non_timed'

     call copy_type_temporary_t(structure_in%timed, structure_out%timed)
     if (verbose > 0) write(iu6, *) 'copied temporary%timed'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied temporary%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied temporary%time'

   end subroutine copy_type_temporary

   subroutine copy_arr_type_temporary(structure_in, structure_out)

     implicit none

     type (type_temporary), pointer :: structure_in(:)
     type (type_temporary), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary'
     end if

   end subroutine copy_arr_type_temporary

   subroutine copy_type_topinfo(structure_in, structure_out)
 
     implicit none
 
     type (type_topinfo), intent(in) :: structure_in
     type (type_topinfo), intent(inout) :: structure_out
 
     call copy_type_vecstring_type(structure_in%dataprovider, structure_out%dataprovider)
     if (verbose > 0) write(iu6, *) 'copied topinfo%dataprovider'

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied topinfo%description'

     call copy_type_vecstring_type(structure_in%firstputdate, structure_out%firstputdate)
     if (verbose > 0) write(iu6, *) 'copied topinfo%firstputdate'

     call copy_type_vecstring_type(structure_in%lastupdate, structure_out%lastupdate)
     if (verbose > 0) write(iu6, *) 'copied topinfo%lastupdate'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied topinfo%source'

     call copy_type_vecstring_type(structure_in%comment, structure_out%comment)
     if (verbose > 0) write(iu6, *) 'copied topinfo%comment'

     call copy_type_vecstring_type(structure_in%dataversion, structure_out%dataversion)
     if (verbose > 0) write(iu6, *) 'copied topinfo%dataversion'

     call copy_type_vecstring_type(structure_in%workflow, structure_out%workflow)
     if (verbose > 0) write(iu6, *) 'copied topinfo%workflow'

     call copy_type_entry_def(structure_in%entry, structure_out%entry)
     if (verbose > 0) write(iu6, *) 'copied topinfo%entry'

     call copy_type_entry_def(structure_in%parent_entry, structure_out%parent_entry)
     if (verbose > 0) write(iu6, *) 'copied topinfo%parent_entry'

     call copy_type_mdinfo(structure_in%mdinfo, structure_out%mdinfo)
     if (verbose > 0) write(iu6, *) 'copied topinfo%mdinfo'

   end subroutine copy_type_topinfo

   subroutine copy_arr_type_topinfo(structure_in, structure_out)

     implicit none

     type (type_topinfo), pointer :: structure_in(:)
     type (type_topinfo), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_topinfo(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_topinfo'
     end if

   end subroutine copy_arr_type_topinfo

   subroutine copy_type_toroidfield(structure_in, structure_out)
 
     implicit none
 
     type (type_toroidfield), intent(in) :: structure_in
     type (type_toroidfield), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%datainfo'

     call copy_type_tf_desc_tfcoils(structure_in%desc_tfcoils, structure_out%desc_tfcoils)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%desc_tfcoils'

     call copy_type_integer(structure_in%nturns, structure_out%nturns)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%nturns'

     call copy_type_integer(structure_in%ncoils, structure_out%ncoils)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%ncoils'

     call copy_type_exp0D(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%current'

     call copy_type_exp0D(structure_in%bvac_r, structure_out%bvac_r)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%bvac_r'

     call copy_type_float(structure_in%r0, structure_out%r0)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%r0'

     call copy_type_float(structure_in%p_cryo, structure_out%p_cryo)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%p_cryo'

     call copy_type_float(structure_in%wp_nh_max, structure_out%wp_nh_max)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%wp_nh_max'

     call copy_type_float(structure_in%tfc_nh, structure_out%tfc_nh)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%tfc_nh'

     call copy_type_float(structure_in%neut_flux_inb, structure_out%neut_flux_inb)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%neut_flux_inb'

     call copy_type_float(structure_in%neut_flux_outb, structure_out%neut_flux_outb)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%neut_flux_outb'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied toroidfield%time'

   end subroutine copy_type_toroidfield

   subroutine copy_arr_type_toroidfield(structure_in, structure_out)

     implicit none

     type (type_toroidfield), pointer :: structure_in(:)
     type (type_toroidfield), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_toroidfield(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_toroidfield'
     end if

   end subroutine copy_arr_type_toroidfield

   subroutine copy_type_tsdiag(structure_in, structure_out)
 
     implicit none
 
     type (type_tsdiag), intent(in) :: structure_in
     type (type_tsdiag), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied tsdiag%datainfo'

     call copy_type_tssetup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied tsdiag%setup'

     call copy_type_tsmeasure(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied tsdiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied tsdiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied tsdiag%time'

   end subroutine copy_type_tsdiag

   subroutine copy_arr_type_tsdiag(structure_in, structure_out)

     implicit none

     type (type_tsdiag), pointer :: structure_in(:)
     type (type_tsdiag), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tsdiag(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tsdiag'
     end if

   end subroutine copy_arr_type_tsdiag

   subroutine copy_type_turbulence(structure_in, structure_out)
 
     implicit none
 
     type (type_turbulence), intent(in) :: structure_in
     type (type_turbulence), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied turbulence%datainfo'

     call copy_type_turbcomposition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied turbulence%composition'

     call copy_type_turbcoordsys(structure_in%coordsys, structure_out%coordsys)
     if (verbose > 0) write(iu6, *) 'copied turbulence%coordsys'

     call copy_type_turbvar0d(structure_in%var0d, structure_out%var0d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%var0d'

     call copy_type_turbvar1d(structure_in%var1d, structure_out%var1d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%var1d'

     call copy_type_turbvar2d(structure_in%var2d, structure_out%var2d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%var2d'

     call copy_type_turbvar3d(structure_in%var3d, structure_out%var3d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%var3d'

     call copy_type_turbvar4d(structure_in%var4d, structure_out%var4d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%var4d'

     call copy_type_turbvar5d(structure_in%var5d, structure_out%var5d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%var5d'

     call copy_type_turbspec1d(structure_in%spec1d, structure_out%spec1d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%spec1d'

     call copy_type_turbenv1d(structure_in%env1d, structure_out%env1d)
     if (verbose > 0) write(iu6, *) 'copied turbulence%env1d'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied turbulence%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied turbulence%time'

   end subroutine copy_type_turbulence

   subroutine copy_arr_type_turbulence(structure_in, structure_out)

     implicit none

     type (type_turbulence), pointer :: structure_in(:)
     type (type_turbulence), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbulence(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbulence'
     end if

   end subroutine copy_arr_type_turbulence

   subroutine copy_type_wall(structure_in, structure_out)
 
     implicit none
 
     type (type_wall), intent(in) :: structure_in
     type (type_wall), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied wall%datainfo'

     call copy_type_wall_wall0d(structure_in%wall0d, structure_out%wall0d)
     if (verbose > 0) write(iu6, *) 'copied wall%wall0d'

     call copy_type_wall2d_mhd(structure_in%wall2d_mhd, structure_out%wall2d_mhd)
     if (verbose > 0) write(iu6, *) 'copied wall%wall2d_mhd'

     call copy_arr_type_wall2d(structure_in%wall2d, structure_out%wall2d)
     if (verbose > 0) write(iu6, *) 'copied wall%wall2d'

     call copy_arr_type_wall3d(structure_in%wall3d, structure_out%wall3d)
     if (verbose > 0) write(iu6, *) 'copied wall%wall3d'

     call copy_arr_type_wall_types(structure_in%wall_types, structure_out%wall_types)
     if (verbose > 0) write(iu6, *) 'copied wall%wall_types'

     call copy_arr_type_compound_desc(structure_in%compounds, structure_out%compounds)
     if (verbose > 0) write(iu6, *) 'copied wall%compounds'

     call copy_arr_type_element_desc(structure_in%elements, structure_out%elements)
     if (verbose > 0) write(iu6, *) 'copied wall%elements'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied wall%compositions'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied wall%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied wall%time'

   end subroutine copy_type_wall

   subroutine copy_arr_type_wall(structure_in, structure_out)

     implicit none

     type (type_wall), pointer :: structure_in(:)
     type (type_wall), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall'
     end if

   end subroutine copy_arr_type_wall

   subroutine copy_type_waves(structure_in, structure_out)
 
     implicit none
 
     type (type_waves), intent(in) :: structure_in
     type (type_waves), intent(inout) :: structure_out
 
     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied waves%datainfo'

     call copy_arr_type_coherentwave(structure_in%coherentwave, structure_out%coherentwave)
     if (verbose > 0) write(iu6, *) 'copied waves%coherentwave'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied waves%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied waves%time'

   end subroutine copy_type_waves

   subroutine copy_arr_type_waves(structure_in, structure_out)

     implicit none

     type (type_waves), pointer :: structure_in(:)
     type (type_waves), pointer :: structure_out(:)
     integer :: i

     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves(structure_in(i),structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves'
     end if

   end subroutine copy_arr_type_waves

   subroutine copy_type_amns_constituentType(structure_in, structure_out)

     implicit none

     type (type_amns_constituentType), intent(in) :: structure_in
     type (type_amns_constituentType), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied amns_constituentType%label'

     call copy_type_integer(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied amns_constituentType%zn'

     call copy_type_integer(structure_in%mn, structure_out%mn)
     if (verbose > 0) write(iu6, *) 'copied amns_constituentType%mn'

     call copy_type_float(structure_in%multiplicity, structure_out%multiplicity)
     if (verbose > 0) write(iu6, *) 'copied amns_constituentType%multiplicity'

   end subroutine copy_type_amns_constituentType

   subroutine copy_arr_type_amns_constituentType(structure_in, structure_out)
 
     implicit none
 
     type (type_amns_constituentType), pointer :: structure_in(:)
     type (type_amns_constituentType), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_amns_constituentType(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_amns_constituentType'
     end if

   end subroutine copy_arr_type_amns_constituentType

   subroutine copy_type_amns_processType(structure_in, structure_out)

     implicit none

     type (type_amns_processType), intent(in) :: structure_in
     type (type_amns_processType), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%proc_label, structure_out%proc_label)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%proc_label'

     call copy_arr_type_reacprodType(structure_in%reactant, structure_out%reactant)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%reactant'

     call copy_arr_type_reacprodType(structure_in%product, structure_out%product)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%product'

     call copy_type_vecstring_type(structure_in%sup_string, structure_out%sup_string)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%sup_string'

     call copy_type_vecflt_type(structure_in%sup_real, structure_out%sup_real)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%sup_real'

     call copy_type_vecint_type(structure_in%sup_int, structure_out%sup_int)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%sup_int'

     call copy_type_identifier(structure_in%quality, structure_out%quality)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%quality'

     call copy_type_vecstring_type(structure_in%err_proc_label, structure_out%err_proc_label)
     if (verbose > 0) write(iu6, *) 'copied amns_processType%err_proc_label'

   end subroutine copy_type_amns_processType

   subroutine copy_arr_type_amns_processType(structure_in, structure_out)
 
     implicit none
 
     type (type_amns_processType), pointer :: structure_in(:)
     type (type_amns_processType), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_amns_processType(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_amns_processType'
     end if

   end subroutine copy_arr_type_amns_processType

   subroutine copy_type_antenna_ec(structure_in, structure_out)

     implicit none

     type (type_antenna_ec), intent(in) :: structure_in
     type (type_antenna_ec), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%name'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%frequency'

     call copy_type_exp0D(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%power'

     call copy_type_integer(structure_in%mode, structure_out%mode)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%mode'

     call copy_type_rzphi0D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%position'

     call copy_type_launchangles(structure_in%launchangles, structure_out%launchangles)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%launchangles'

     call copy_type_rfbeam(structure_in%beam, structure_out%beam)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%beam'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied antenna_ec%codeparam'

   end subroutine copy_type_antenna_ec

   subroutine copy_arr_type_antenna_ec(structure_in, structure_out)
 
     implicit none
 
     type (type_antenna_ec), pointer :: structure_in(:)
     type (type_antenna_ec), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_antenna_ec(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_antenna_ec'
     end if

   end subroutine copy_arr_type_antenna_ec

   subroutine copy_type_antenna_ic(structure_in, structure_out)

     implicit none

     type (type_antenna_ic), intent(in) :: structure_in
     type (type_antenna_ic), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%name'

     call copy_type_exp0D(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%frequency'

     call copy_type_exp0D(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%power'

     call copy_type_vecint_type(structure_in%ntor, structure_out%ntor)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%ntor'

     call copy_type_vecflt_type(structure_in%power_ntor, structure_out%power_ntor)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%power_ntor'

     call copy_type_antennaic_setup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%setup'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied antenna_ic%codeparam'

   end subroutine copy_type_antenna_ic

   subroutine copy_arr_type_antenna_ic(structure_in, structure_out)
 
     implicit none
 
     type (type_antenna_ic), pointer :: structure_in(:)
     type (type_antenna_ic), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_antenna_ic(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_antenna_ic'
     end if

   end subroutine copy_arr_type_antenna_ic

   subroutine copy_type_antenna_lh(structure_in, structure_out)

     implicit none

     type (type_antenna_lh), intent(in) :: structure_in
     type (type_antenna_lh), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%name'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%frequency'

     call copy_type_exp0D(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%power'

     call copy_type_float(structure_in%n_par, structure_out%n_par)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%n_par'

     call copy_type_rzphi0D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%position'

     call copy_type_antennalh_setup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%setup'

     call copy_type_plasmaedge(structure_in%plasmaedge, structure_out%plasmaedge)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%plasmaedge'

     call copy_type_rfbeam(structure_in%beam, structure_out%beam)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%beam'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied antenna_lh%codeparam'

   end subroutine copy_type_antenna_lh

   subroutine copy_arr_type_antenna_lh(structure_in, structure_out)
 
     implicit none
 
     type (type_antenna_lh), pointer :: structure_in(:)
     type (type_antenna_lh), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_antenna_lh(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_antenna_lh'
     end if

   end subroutine copy_arr_type_antenna_lh

   subroutine copy_type_antennaic_setup(structure_in, structure_out)

     implicit none

     type (type_antennaic_setup), intent(in) :: structure_in
     type (type_antennaic_setup), intent(inout) :: structure_out

     call copy_arr_type_straps(structure_in%straps, structure_out%straps)
     if (verbose > 0) write(iu6, *) 'copied antennaic_setup%straps'

     call copy_type_current(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied antennaic_setup%current'

   end subroutine copy_type_antennaic_setup

   subroutine copy_arr_type_antennaic_setup(structure_in, structure_out)
 
     implicit none
 
     type (type_antennaic_setup), pointer :: structure_in(:)
     type (type_antennaic_setup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_antennaic_setup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_antennaic_setup'
     end if

   end subroutine copy_arr_type_antennaic_setup

   subroutine copy_type_antennalh_setup(structure_in, structure_out)

     implicit none

     type (type_antennalh_setup), intent(in) :: structure_in
     type (type_antennalh_setup), intent(inout) :: structure_out

     call copy_type_modules(structure_in%modules, structure_out%modules)
     if (verbose > 0) write(iu6, *) 'copied antennalh_setup%modules'

   end subroutine copy_type_antennalh_setup

   subroutine copy_arr_type_antennalh_setup(structure_in, structure_out)
 
     implicit none
 
     type (type_antennalh_setup), pointer :: structure_in(:)
     type (type_antennalh_setup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_antennalh_setup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_antennalh_setup'
     end if

   end subroutine copy_arr_type_antennalh_setup

   subroutine copy_type_b0r0(structure_in, structure_out)

     implicit none

     type (type_b0r0), intent(in) :: structure_in
     type (type_b0r0), intent(inout) :: structure_out

     call copy_type_float(structure_in%r0, structure_out%r0)
     if (verbose > 0) write(iu6, *) 'copied b0r0%r0'

     call copy_type_float(structure_in%b0, structure_out%b0)
     if (verbose > 0) write(iu6, *) 'copied b0r0%b0'

   end subroutine copy_type_b0r0

   subroutine copy_arr_type_b0r0(structure_in, structure_out)
 
     implicit none
 
     type (type_b0r0), pointer :: structure_in(:)
     type (type_b0r0), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_b0r0(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_b0r0'
     end if

   end subroutine copy_arr_type_b0r0

   subroutine copy_type_bb(structure_in, structure_out)

     implicit none

     type (type_bb), intent(in) :: structure_in
     type (type_bb), intent(inout) :: structure_out

     call copy_type_float(structure_in%nb_bb, structure_out%nb_bb)
     if (verbose > 0) write(iu6, *) 'copied bb%nb_bb'

     call copy_type_float(structure_in%nb_bb_polcut, structure_out%nb_bb_polcut)
     if (verbose > 0) write(iu6, *) 'copied bb%nb_bb_polcut'

     call copy_type_float(structure_in%teta_bb, structure_out%teta_bb)
     if (verbose > 0) write(iu6, *) 'copied bb%teta_bb'

     call copy_type_float(structure_in%tbr, structure_out%tbr)
     if (verbose > 0) write(iu6, *) 'copied bb%tbr'

     call copy_type_neutro_resul(structure_in%neutro_resul, structure_out%neutro_resul)
     if (verbose > 0) write(iu6, *) 'copied bb%neutro_resul'

     call copy_type_bb_specs(structure_in%inboard, structure_out%inboard)
     if (verbose > 0) write(iu6, *) 'copied bb%inboard'

     call copy_type_bb_specs(structure_in%outboard, structure_out%outboard)
     if (verbose > 0) write(iu6, *) 'copied bb%outboard'

   end subroutine copy_type_bb

   subroutine copy_arr_type_bb(structure_in, structure_out)
 
     implicit none
 
     type (type_bb), pointer :: structure_in(:)
     type (type_bb), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_bb(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_bb'
     end if

   end subroutine copy_arr_type_bb

   subroutine copy_type_bb_dimension(structure_in, structure_out)

     implicit none

     type (type_bb_dimension), intent(in) :: structure_in
     type (type_bb_dimension), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%radial, structure_out%radial)
     if (verbose > 0) write(iu6, *) 'copied bb_dimension%radial'

     call copy_type_vecflt_type(structure_in%toroidal, structure_out%toroidal)
     if (verbose > 0) write(iu6, *) 'copied bb_dimension%toroidal'

     call copy_type_vecflt_type(structure_in%poloidal, structure_out%poloidal)
     if (verbose > 0) write(iu6, *) 'copied bb_dimension%poloidal'

   end subroutine copy_type_bb_dimension

   subroutine copy_arr_type_bb_dimension(structure_in, structure_out)
 
     implicit none
 
     type (type_bb_dimension), pointer :: structure_in(:)
     type (type_bb_dimension), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_bb_dimension(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_bb_dimension'
     end if

   end subroutine copy_arr_type_bb_dimension

   subroutine copy_type_bb_geometry(structure_in, structure_out)

     implicit none

     type (type_bb_geometry), intent(in) :: structure_in
     type (type_bb_geometry), intent(inout) :: structure_out

     call copy_type_float(structure_in%dr_fw, structure_out%dr_fw)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_fw'

     call copy_type_float(structure_in%dr_bz, structure_out%dr_bz)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_bz'

     call copy_type_float(structure_in%dr_bp, structure_out%dr_bp)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_bp'

     call copy_type_vecflt_type(structure_in%dr_bp_plates, structure_out%dr_bp_plates)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_bp_plates'

     call copy_type_vecflt_type(structure_in%dr_bp_he, structure_out%dr_bp_he)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_bp_he'

     call copy_type_float(structure_in%dr_man, structure_out%dr_man)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_man'

     call copy_type_float(structure_in%dt_sw, structure_out%dt_sw)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dt_sw'

     call copy_type_float(structure_in%dt_bz, structure_out%dt_bz)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dt_bz'

     call copy_type_float(structure_in%dp_bz, structure_out%dp_bz)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dp_bz'

     call copy_type_bb_dimension(structure_in%top_cap_dim, structure_out%top_cap_dim)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%top_cap_dim'

     call copy_type_bb_dimension(structure_in%bot_cap_dim, structure_out%bot_cap_dim)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%bot_cap_dim'

     call copy_type_float(structure_in%a_fw_ch, structure_out%a_fw_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%a_fw_ch'

     call copy_type_float(structure_in%b_fw_ch, structure_out%b_fw_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%b_fw_ch'

     call copy_type_float(structure_in%td_tc_ch, structure_out%td_tc_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%td_tc_ch'

     call copy_type_float(structure_in%rd_tc_ch, structure_out%rd_tc_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%rd_tc_ch'

     call copy_type_float(structure_in%td_bc_ch, structure_out%td_bc_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%td_bc_ch'

     call copy_type_float(structure_in%rd_bc_ch, structure_out%rd_bc_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%rd_bc_ch'

     call copy_type_float(structure_in%n_fw_ch, structure_out%n_fw_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_fw_ch'

     call copy_type_float(structure_in%n_fw_circ, structure_out%n_fw_circ)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_fw_circ'

     call copy_type_float(structure_in%a_sg_ch, structure_out%a_sg_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%a_sg_ch'

     call copy_type_float(structure_in%b_sg_ch, structure_out%b_sg_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%b_sg_ch'

     call copy_type_float(structure_in%n_sg_ch, structure_out%n_sg_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_sg_ch'

     call copy_type_float(structure_in%sg_thick, structure_out%sg_thick)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%sg_thick'

     call copy_type_float(structure_in%sg_weld, structure_out%sg_weld)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%sg_weld'

     call copy_type_float(structure_in%sg_in_out, structure_out%sg_in_out)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%sg_in_out'

     call copy_type_float(structure_in%r_sg_cp, structure_out%r_sg_cp)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%r_sg_cp'

     call copy_type_float(structure_in%cp_tor_gap, structure_out%cp_tor_gap)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%cp_tor_gap'

     call copy_type_float(structure_in%a_cp_ch, structure_out%a_cp_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%a_cp_ch'

     call copy_type_float(structure_in%b_cp_ch, structure_out%b_cp_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%b_cp_ch'

     call copy_type_float(structure_in%n_cp_ch, structure_out%n_cp_ch)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_cp_ch'

     call copy_type_float(structure_in%cp_thick, structure_out%cp_thick)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%cp_thick'

     call copy_type_float(structure_in%n_pol_bu, structure_out%n_pol_bu)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_pol_bu'

     call copy_type_float(structure_in%n_tor_bu, structure_out%n_tor_bu)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_tor_bu'

     call copy_type_float(structure_in%n_cp_bu, structure_out%n_cp_bu)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%n_cp_bu'

     call copy_type_float(structure_in%cp_in_out, structure_out%cp_in_out)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%cp_in_out'

     call copy_type_float(structure_in%he_man_tck, structure_out%he_man_tck)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%he_man_tck'

     call copy_type_float(structure_in%man_tck, structure_out%man_tck)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%man_tck'

     call copy_type_float(structure_in%pbli_bptb_od, structure_out%pbli_bptb_od)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%pbli_bptb_od'

     call copy_type_float(structure_in%pbli_bptb_id, structure_out%pbli_bptb_id)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%pbli_bptb_id'

     call copy_type_float(structure_in%he_bptb_od, structure_out%he_bptb_od)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%he_bptb_od'

     call copy_type_float(structure_in%he_bptb_id, structure_out%he_bptb_id)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%he_bptb_id'

     call copy_type_float(structure_in%dr_max_fw, structure_out%dr_max_fw)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_max_fw'

     call copy_type_float(structure_in%dr_fwpl, structure_out%dr_fwpl)
     if (verbose > 0) write(iu6, *) 'copied bb_geometry%dr_fwpl'

   end subroutine copy_type_bb_geometry

   subroutine copy_arr_type_bb_geometry(structure_in, structure_out)
 
     implicit none
 
     type (type_bb_geometry), pointer :: structure_in(:)
     type (type_bb_geometry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_bb_geometry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_bb_geometry'
     end if

   end subroutine copy_arr_type_bb_geometry

   subroutine copy_type_bb_specs(structure_in, structure_out)

     implicit none

     type (type_bb_specs), intent(in) :: structure_in
     type (type_bb_specs), intent(inout) :: structure_out

     call copy_type_float(structure_in%nbb, structure_out%nbb)
     if (verbose > 0) write(iu6, *) 'copied bb_specs%nbb'

     call copy_type_float(structure_in%r1, structure_out%r1)
     if (verbose > 0) write(iu6, *) 'copied bb_specs%r1'

     call copy_type_float(structure_in%r2, structure_out%r2)
     if (verbose > 0) write(iu6, *) 'copied bb_specs%r2'

     call copy_type_bb_dimension(structure_in%dimension, structure_out%dimension)
     if (verbose > 0) write(iu6, *) 'copied bb_specs%dimension'

   end subroutine copy_type_bb_specs

   subroutine copy_arr_type_bb_specs(structure_in, structure_out)
 
     implicit none
 
     type (type_bb_specs), pointer :: structure_in(:)
     type (type_bb_specs), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_bb_specs(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_bb_specs'
     end if

   end subroutine copy_arr_type_bb_specs

   subroutine copy_type_beamletgroup(structure_in, structure_out)

     implicit none

     type (type_beamletgroup), intent(in) :: structure_in
     type (type_beamletgroup), intent(inout) :: structure_out

     call copy_type_rzphi0D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%position'

     call copy_type_float(structure_in%tang_rad, structure_out%tang_rad)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%tang_rad'

     call copy_type_float(structure_in%angle, structure_out%angle)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%angle'

     call copy_type_integer(structure_in%direction, structure_out%direction)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%direction'

     call copy_type_float(structure_in%width_horiz, structure_out%width_horiz)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%width_horiz'

     call copy_type_float(structure_in%width_vert, structure_out%width_vert)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%width_vert'

     call copy_type_focussing(structure_in%focussing, structure_out%focussing)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%focussing'

     call copy_type_divergence(structure_in%divergence, structure_out%divergence)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%divergence'

     call copy_type_beamlets(structure_in%beamlets, structure_out%beamlets)
     if (verbose > 0) write(iu6, *) 'copied beamletgroup%beamlets'

   end subroutine copy_type_beamletgroup

   subroutine copy_arr_type_beamletgroup(structure_in, structure_out)
 
     implicit none
 
     type (type_beamletgroup), pointer :: structure_in(:)
     type (type_beamletgroup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_beamletgroup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_beamletgroup'
     end if

   end subroutine copy_arr_type_beamletgroup

   subroutine copy_type_beamlets(structure_in, structure_out)

     implicit none

     type (type_beamlets), intent(in) :: structure_in
     type (type_beamlets), intent(inout) :: structure_out

     call copy_type_rzphi1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied beamlets%position'

     call copy_type_vecflt_type(structure_in%tang_rad_blt, structure_out%tang_rad_blt)
     if (verbose > 0) write(iu6, *) 'copied beamlets%tang_rad_blt'

     call copy_type_vecflt_type(structure_in%angle_blt, structure_out%angle_blt)
     if (verbose > 0) write(iu6, *) 'copied beamlets%angle_blt'

     call copy_type_vecflt_type(structure_in%pow_frc_blt, structure_out%pow_frc_blt)
     if (verbose > 0) write(iu6, *) 'copied beamlets%pow_frc_blt'

   end subroutine copy_type_beamlets

   subroutine copy_arr_type_beamlets(structure_in, structure_out)
 
     implicit none
 
     type (type_beamlets), pointer :: structure_in(:)
     type (type_beamlets), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_beamlets(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_beamlets'
     end if

   end subroutine copy_arr_type_beamlets

   subroutine copy_type_beamtracing(structure_in, structure_out)

     implicit none

     type (type_beamtracing), intent(in) :: structure_in
     type (type_beamtracing), intent(inout) :: structure_out

     call copy_type_integer(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%npoints'

     call copy_type_float(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%power'

     call copy_type_vecflt_type(structure_in%dnpar, structure_out%dnpar)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%dnpar'

     call copy_type_vecflt_type(structure_in%length, structure_out%length)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%length'

     call copy_type_waves_rtposition(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%position'

     call copy_type_waves_rtwavevector(structure_in%wavevector, structure_out%wavevector)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%wavevector'

     call copy_type_polarization(structure_in%polarization, structure_out%polarization)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%polarization'

     call copy_type_powerflow(structure_in%powerflow, structure_out%powerflow)
     if (verbose > 0) write(iu6, *) 'copied beamtracing%powerflow'

   end subroutine copy_type_beamtracing

   subroutine copy_arr_type_beamtracing(structure_in, structure_out)
 
     implicit none
 
     type (type_beamtracing), pointer :: structure_in(:)
     type (type_beamtracing), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_beamtracing(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_beamtracing'
     end if

   end subroutine copy_arr_type_beamtracing

   subroutine copy_type_boundary(structure_in, structure_out)

     implicit none

     type (type_boundary), intent(in) :: structure_in
     type (type_boundary), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied boundary%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied boundary%source'

     call copy_type_integer(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied boundary%type'

     call copy_type_float(structure_in%rho, structure_out%rho)
     if (verbose > 0) write(iu6, *) 'copied boundary%rho'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied boundary%codeparam'

   end subroutine copy_type_boundary

   subroutine copy_arr_type_boundary(structure_in, structure_out)
 
     implicit none
 
     type (type_boundary), pointer :: structure_in(:)
     type (type_boundary), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_boundary(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_boundary'
     end if

   end subroutine copy_arr_type_boundary

   subroutine copy_type_boundary_neutrals(structure_in, structure_out)

     implicit none

     type (type_boundary_neutrals), intent(in) :: structure_in
     type (type_boundary_neutrals), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied boundary_neutrals%value'

     call copy_type_integer(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied boundary_neutrals%type'

     call copy_type_float(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied boundary_neutrals%rho_tor'

   end subroutine copy_type_boundary_neutrals

   subroutine copy_arr_type_boundary_neutrals(structure_in, structure_out)
 
     implicit none
 
     type (type_boundary_neutrals), pointer :: structure_in(:)
     type (type_boundary_neutrals), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_boundary_neutrals(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_boundary_neutrals'
     end if

   end subroutine copy_arr_type_boundary_neutrals

   subroutine copy_type_boundaryel(structure_in, structure_out)

     implicit none

     type (type_boundaryel), intent(in) :: structure_in
     type (type_boundaryel), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied boundaryel%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied boundaryel%source'

     call copy_type_integer(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied boundaryel%type'

     call copy_type_float(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied boundaryel%rho_tor'

   end subroutine copy_type_boundaryel

   subroutine copy_arr_type_boundaryel(structure_in, structure_out)
 
     implicit none
 
     type (type_boundaryel), pointer :: structure_in(:)
     type (type_boundaryel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_boundaryel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_boundaryel'
     end if

   end subroutine copy_arr_type_boundaryel

   subroutine copy_type_boundaryimp(structure_in, structure_out)

     implicit none

     type (type_boundaryimp), intent(in) :: structure_in
     type (type_boundaryimp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied boundaryimp%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied boundaryimp%source'

     call copy_type_vecint_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied boundaryimp%type'

     call copy_type_vecflt_type(structure_in%rho, structure_out%rho)
     if (verbose > 0) write(iu6, *) 'copied boundaryimp%rho'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied boundaryimp%codeparam'

   end subroutine copy_type_boundaryimp

   subroutine copy_arr_type_boundaryimp(structure_in, structure_out)
 
     implicit none
 
     type (type_boundaryimp), pointer :: structure_in(:)
     type (type_boundaryimp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_boundaryimp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_boundaryimp'
     end if

   end subroutine copy_arr_type_boundaryimp

   subroutine copy_type_boundaryion(structure_in, structure_out)

     implicit none

     type (type_boundaryion), intent(in) :: structure_in
     type (type_boundaryion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied boundaryion%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied boundaryion%source'

     call copy_type_vecint_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied boundaryion%type'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied boundaryion%rho_tor'

   end subroutine copy_type_boundaryion

   subroutine copy_arr_type_boundaryion(structure_in, structure_out)
 
     implicit none
 
     type (type_boundaryion), pointer :: structure_in(:)
     type (type_boundaryion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_boundaryion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_boundaryion'
     end if

   end subroutine copy_arr_type_boundaryion

   subroutine copy_type_bpol_probes(structure_in, structure_out)

     implicit none

     type (type_bpol_probes), intent(in) :: structure_in
     type (type_bpol_probes), intent(inout) :: structure_out

     call copy_type_setup_bprobe(structure_in%setup_bprobe, structure_out%setup_bprobe)
     if (verbose > 0) write(iu6, *) 'copied bpol_probes%setup_bprobe'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied bpol_probes%measure'

   end subroutine copy_type_bpol_probes

   subroutine copy_arr_type_bpol_probes(structure_in, structure_out)
 
     implicit none
 
     type (type_bpol_probes), pointer :: structure_in(:)
     type (type_bpol_probes), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_bpol_probes(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_bpol_probes'
     end if

   end subroutine copy_arr_type_bpol_probes

   subroutine copy_type_calorimetry_heat_source(structure_in, structure_out)

     implicit none

     type (type_calorimetry_heat_source), intent(in) :: structure_in
     type (type_calorimetry_heat_source), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%name'

     call copy_type_float(structure_in%temp_in, structure_out%temp_in)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%temp_in'

     call copy_type_float(structure_in%temp_out, structure_out%temp_out)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%temp_out'

     call copy_type_float(structure_in%press_in, structure_out%press_in)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%press_in'

     call copy_type_float(structure_in%press_out, structure_out%press_out)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%press_out'

     call copy_type_float(structure_in%flow, structure_out%flow)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%flow'

     call copy_type_float(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied calorimetry_heat_source%power'

   end subroutine copy_type_calorimetry_heat_source

   subroutine copy_arr_type_calorimetry_heat_source(structure_in, structure_out)
 
     implicit none
 
     type (type_calorimetry_heat_source), pointer :: structure_in(:)
     type (type_calorimetry_heat_source), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_calorimetry_heat_source(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_calorimetry_heat_source'
     end if

   end subroutine copy_arr_type_calorimetry_heat_source

   subroutine copy_type_circuits(structure_in, structure_out)

     implicit none

     type (type_circuits), intent(in) :: structure_in
     type (type_circuits), intent(inout) :: structure_out

     call copy_arr_type_power_conv_component(structure_in%component, structure_out%component)
     if (verbose > 0) write(iu6, *) 'copied circuits%component'

     call copy_type_float(structure_in%power_net, structure_out%power_net)
     if (verbose > 0) write(iu6, *) 'copied circuits%power_net'

     call copy_type_float(structure_in%power_int, structure_out%power_int)
     if (verbose > 0) write(iu6, *) 'copied circuits%power_int'

     call copy_type_float(structure_in%efficiency, structure_out%efficiency)
     if (verbose > 0) write(iu6, *) 'copied circuits%efficiency'

   end subroutine copy_type_circuits

   subroutine copy_arr_type_circuits(structure_in, structure_out)
 
     implicit none
 
     type (type_circuits), pointer :: structure_in(:)
     type (type_circuits), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_circuits(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_circuits'
     end if

   end subroutine copy_arr_type_circuits

   subroutine copy_type_circularcoil(structure_in, structure_out)

     implicit none

     type (type_circularcoil), intent(in) :: structure_in
     type (type_circularcoil), intent(inout) :: structure_out

     call copy_type_rz0D(structure_in%centre, structure_out%centre)
     if (verbose > 0) write(iu6, *) 'copied circularcoil%centre'

     call copy_type_float(structure_in%hlength, structure_out%hlength)
     if (verbose > 0) write(iu6, *) 'copied circularcoil%hlength'

     call copy_type_float(structure_in%radialhwidth, structure_out%radialhwidth)
     if (verbose > 0) write(iu6, *) 'copied circularcoil%radialhwidth'

   end subroutine copy_type_circularcoil

   subroutine copy_arr_type_circularcoil(structure_in, structure_out)
 
     implicit none
 
     type (type_circularcoil), pointer :: structure_in(:)
     type (type_circularcoil), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_circularcoil(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_circularcoil'
     end if

   end subroutine copy_arr_type_circularcoil

   subroutine copy_type_clusters(structure_in, structure_out)

     implicit none

     type (type_clusters), intent(in) :: structure_in
     type (type_clusters), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied clusters%name'

     call copy_type_integer(structure_in%start, structure_out%start)
     if (verbose > 0) write(iu6, *) 'copied clusters%start'

     call copy_type_integer(structure_in%finish, structure_out%finish)
     if (verbose > 0) write(iu6, *) 'copied clusters%finish'

   end subroutine copy_type_clusters

   subroutine copy_arr_type_clusters(structure_in, structure_out)
 
     implicit none
 
     type (type_clusters), pointer :: structure_in(:)
     type (type_clusters), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_clusters(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_clusters'
     end if

   end subroutine copy_arr_type_clusters

   subroutine copy_type_codeparam(structure_in, structure_out)

     implicit none

     type (type_codeparam), intent(in) :: structure_in
     type (type_codeparam), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%codename, structure_out%codename)
     if (verbose > 0) write(iu6, *) 'copied codeparam%codename'

     call copy_type_vecstring_type(structure_in%codeversion, structure_out%codeversion)
     if (verbose > 0) write(iu6, *) 'copied codeparam%codeversion'

     call copy_type_vecstring_type(structure_in%parameters, structure_out%parameters)
     if (verbose > 0) write(iu6, *) 'copied codeparam%parameters'

     call copy_type_vecstring_type(structure_in%output_diag, structure_out%output_diag)
     if (verbose > 0) write(iu6, *) 'copied codeparam%output_diag'

     call copy_type_integer(structure_in%output_flag, structure_out%output_flag)
     if (verbose > 0) write(iu6, *) 'copied codeparam%output_flag'

   end subroutine copy_type_codeparam

   subroutine copy_arr_type_codeparam(structure_in, structure_out)
 
     implicit none
 
     type (type_codeparam), pointer :: structure_in(:)
     type (type_codeparam), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_codeparam(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_codeparam'
     end if

   end subroutine copy_arr_type_codeparam

   subroutine copy_type_coefficients_neutrals(structure_in, structure_out)

     implicit none

     type (type_coefficients_neutrals), intent(in) :: structure_in
     type (type_coefficients_neutrals), intent(inout) :: structure_out

     call copy_type_recycling_neutrals(structure_in%recycling, structure_out%recycling)
     if (verbose > 0) write(iu6, *) 'copied coefficients_neutrals%recycling'

     call copy_type_sputtering_neutrals(structure_in%sputtering, structure_out%sputtering)
     if (verbose > 0) write(iu6, *) 'copied coefficients_neutrals%sputtering'

   end subroutine copy_type_coefficients_neutrals

   subroutine copy_arr_type_coefficients_neutrals(structure_in, structure_out)
 
     implicit none
 
     type (type_coefficients_neutrals), pointer :: structure_in(:)
     type (type_coefficients_neutrals), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coefficients_neutrals(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coefficients_neutrals'
     end if

   end subroutine copy_arr_type_coefficients_neutrals

   subroutine copy_type_coherentwave(structure_in, structure_out)

     implicit none

     type (type_coherentwave), intent(in) :: structure_in
     type (type_coherentwave), intent(inout) :: structure_out

     call copy_type_enum_instance(structure_in%wave_id, structure_out%wave_id)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%wave_id'

     call copy_type_composition(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%composition'

     call copy_type_compositions_type(structure_in%compositions, structure_out%compositions)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%compositions'

     call copy_type_waves_global_param(structure_in%global_param, structure_out%global_param)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%global_param'

     call copy_type_waves_grid_1d(structure_in%grid_1d, structure_out%grid_1d)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%grid_1d'

     call copy_type_waves_grid_2d(structure_in%grid_2d, structure_out%grid_2d)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%grid_2d'

     call copy_type_waves_profiles_1d(structure_in%profiles_1d, structure_out%profiles_1d)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%profiles_1d'

     call copy_type_waves_profiles_2d(structure_in%profiles_2d, structure_out%profiles_2d)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%profiles_2d'

     call copy_arr_type_beamtracing(structure_in%beamtracing, structure_out%beamtracing)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%beamtracing'

     call copy_type_fullwave(structure_in%fullwave, structure_out%fullwave)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%fullwave'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coherentwave%codeparam'

   end subroutine copy_type_coherentwave

   subroutine copy_arr_type_coherentwave(structure_in, structure_out)
 
     implicit none
 
     type (type_coherentwave), pointer :: structure_in(:)
     type (type_coherentwave), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coherentwave(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coherentwave'
     end if

   end subroutine copy_arr_type_coherentwave

   subroutine copy_type_coil(structure_in, structure_out)

     implicit none

     type (type_coil), intent(in) :: structure_in
     type (type_coil), intent(inout) :: structure_out

     call copy_type_desc_coils(structure_in%desc_coils, structure_out%desc_coils)
     if (verbose > 0) write(iu6, *) 'copied coil%desc_coils'

     call copy_type_exp1D(structure_in%coilcurrent, structure_out%coilcurrent)
     if (verbose > 0) write(iu6, *) 'copied coil%coilcurrent'

     call copy_type_exp1D(structure_in%coilvoltage, structure_out%coilvoltage)
     if (verbose > 0) write(iu6, *) 'copied coil%coilvoltage'

   end subroutine copy_type_coil

   subroutine copy_arr_type_coil(structure_in, structure_out)
 
     implicit none
 
     type (type_coil), pointer :: structure_in(:)
     type (type_coil), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coil(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coil'
     end if

   end subroutine copy_arr_type_coil

   subroutine copy_type_com(structure_in, structure_out)

     implicit none

     type (type_com), intent(in) :: structure_in
     type (type_com), intent(inout) :: structure_out

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied com%amn'

     call copy_type_float(structure_in%zion, structure_out%zion)
     if (verbose > 0) write(iu6, *) 'copied com%zion'

     call copy_type_vecflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied com%energy'

     call copy_type_vecflt_type(structure_in%magn_mom, structure_out%magn_mom)
     if (verbose > 0) write(iu6, *) 'copied com%magn_mom'

     call copy_type_vecflt_type(structure_in%p_phi, structure_out%p_phi)
     if (verbose > 0) write(iu6, *) 'copied com%p_phi'

     call copy_type_vecint_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied com%sigma'

   end subroutine copy_type_com

   subroutine copy_arr_type_com(structure_in, structure_out)
 
     implicit none
 
     type (type_com), pointer :: structure_in(:)
     type (type_com), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_com(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_com'
     end if

   end subroutine copy_arr_type_com

   subroutine copy_type_complexgrid(structure_in, structure_out)

     implicit none

     type (type_complexgrid), intent(in) :: structure_in
     type (type_complexgrid), intent(inout) :: structure_out

     call copy_type_integer(structure_in%uid, structure_out%uid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%uid'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%id'

     call copy_arr_type_complexgrid_space(structure_in%spaces, structure_out%spaces)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%spaces'

     call copy_arr_type_complexgrid_subgrid(structure_in%subgrids, structure_out%subgrids)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%subgrids'

     call copy_type_complexgrid_metric(structure_in%metric, structure_out%metric)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%metric'

     call copy_arr_type_complexgrid_geo_global(structure_in%geo, structure_out%geo)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%geo'

     call copy_arr_type_complexgrid_vector(structure_in%bases, structure_out%bases)
     if (verbose > 0) write(iu6, *) 'copied complexgrid%bases'

   end subroutine copy_type_complexgrid

   subroutine copy_arr_type_complexgrid(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid), pointer :: structure_in(:)
     type (type_complexgrid), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid'
     end if

   end subroutine copy_arr_type_complexgrid

   subroutine copy_type_complexgrid_geo_global(structure_in, structure_out)

     implicit none

     type (type_complexgrid_geo_global), intent(in) :: structure_in
     type (type_complexgrid_geo_global), intent(inout) :: structure_out

     call copy_type_integer(structure_in%geotype, structure_out%geotype)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_geo_global%geotype'

     call copy_type_vecstring_type(structure_in%geotypeid, structure_out%geotypeid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_geo_global%geotypeid'

     call copy_type_vecint_type(structure_in%coordtype, structure_out%coordtype)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_geo_global%coordtype'

     call copy_arr_type_complexgrid_scalar(structure_in%geo_matrix, structure_out%geo_matrix)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_geo_global%geo_matrix'

     call copy_arr_type_complexgrid_scalar(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_geo_global%measure'

   end subroutine copy_type_complexgrid_geo_global

   subroutine copy_arr_type_complexgrid_geo_global(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_geo_global), pointer :: structure_in(:)
     type (type_complexgrid_geo_global), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_geo_global(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_geo_global'
     end if

   end subroutine copy_arr_type_complexgrid_geo_global

   subroutine copy_type_complexgrid_indexlist(structure_in, structure_out)

     implicit none

     type (type_complexgrid_indexlist), intent(in) :: structure_in
     type (type_complexgrid_indexlist), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%range, structure_out%range)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_indexlist%range'

     call copy_type_vecint_type(structure_in%ind, structure_out%ind)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_indexlist%ind'

   end subroutine copy_type_complexgrid_indexlist

   subroutine copy_arr_type_complexgrid_indexlist(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_indexlist), pointer :: structure_in(:)
     type (type_complexgrid_indexlist), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_indexlist(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_indexlist'
     end if

   end subroutine copy_arr_type_complexgrid_indexlist

   subroutine copy_type_complexgrid_metric(structure_in, structure_out)

     implicit none

     type (type_complexgrid_metric), intent(in) :: structure_in
     type (type_complexgrid_metric), intent(inout) :: structure_out

     call copy_arr_type_complexgrid_scalar(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%measure'

     call copy_arr_type_complexgrid_scalar(structure_in%g11, structure_out%g11)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%g11'

     call copy_arr_type_complexgrid_scalar(structure_in%g12, structure_out%g12)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%g12'

     call copy_arr_type_complexgrid_scalar(structure_in%g13, structure_out%g13)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%g13'

     call copy_arr_type_complexgrid_scalar(structure_in%g22, structure_out%g22)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%g22'

     call copy_arr_type_complexgrid_scalar(structure_in%g23, structure_out%g23)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%g23'

     call copy_arr_type_complexgrid_scalar(structure_in%g33, structure_out%g33)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%g33'

     call copy_arr_type_complexgrid_scalar(structure_in%jacobian, structure_out%jacobian)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_metric%jacobian'

   end subroutine copy_type_complexgrid_metric

   subroutine copy_arr_type_complexgrid_metric(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_metric), pointer :: structure_in(:)
     type (type_complexgrid_metric), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_metric(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_metric'
     end if

   end subroutine copy_arr_type_complexgrid_metric

   subroutine copy_type_complexgrid_objectlist(structure_in, structure_out)

     implicit none

     type (type_complexgrid_objectlist), intent(in) :: structure_in
     type (type_complexgrid_objectlist), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%cls, structure_out%cls)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_objectlist%cls'

     call copy_arr_type_complexgrid_indexlist(structure_in%indset, structure_out%indset)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_objectlist%indset'

     call copy_type_matint_type(structure_in%ind, structure_out%ind)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_objectlist%ind'

   end subroutine copy_type_complexgrid_objectlist

   subroutine copy_arr_type_complexgrid_objectlist(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_objectlist), pointer :: structure_in(:)
     type (type_complexgrid_objectlist), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_objectlist(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_objectlist'
     end if

   end subroutine copy_arr_type_complexgrid_objectlist

   subroutine copy_type_complexgrid_scalar(structure_in, structure_out)

     implicit none

     type (type_complexgrid_scalar), intent(in) :: structure_in
     type (type_complexgrid_scalar), intent(inout) :: structure_out

     call copy_type_integer(structure_in%griduid, structure_out%griduid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar%griduid'

     call copy_type_integer(structure_in%subgrid, structure_out%subgrid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar%subgrid'

     call copy_type_vecflt_type(structure_in%scalar, structure_out%scalar)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar%scalar'

     call copy_type_matflt_type(structure_in%vector, structure_out%vector)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar%vector'

     call copy_type_array3dflt_type(structure_in%matrix, structure_out%matrix)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar%matrix'

   end subroutine copy_type_complexgrid_scalar

   subroutine copy_arr_type_complexgrid_scalar(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_scalar), pointer :: structure_in(:)
     type (type_complexgrid_scalar), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_scalar(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_scalar'
     end if

   end subroutine copy_arr_type_complexgrid_scalar

   subroutine copy_type_complexgrid_scalar_cplx(structure_in, structure_out)

     implicit none

     type (type_complexgrid_scalar_cplx), intent(in) :: structure_in
     type (type_complexgrid_scalar_cplx), intent(inout) :: structure_out

     call copy_type_integer(structure_in%griduid, structure_out%griduid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_cplx%griduid'

     call copy_type_integer(structure_in%subgrid, structure_out%subgrid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_cplx%subgrid'

     call copy_type_veccplx_type(structure_in%scalar, structure_out%scalar)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_cplx%scalar'

     call copy_type_matcplx_type(structure_in%vector, structure_out%vector)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_cplx%vector'

     call copy_type_array3dcplx_type(structure_in%matrix, structure_out%matrix)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_cplx%matrix'

   end subroutine copy_type_complexgrid_scalar_cplx

   subroutine copy_arr_type_complexgrid_scalar_cplx(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_scalar_cplx), pointer :: structure_in(:)
     type (type_complexgrid_scalar_cplx), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_scalar_cplx(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_scalar_cplx'
     end if

   end subroutine copy_arr_type_complexgrid_scalar_cplx

   subroutine copy_type_complexgrid_scalar_int(structure_in, structure_out)

     implicit none

     type (type_complexgrid_scalar_int), intent(in) :: structure_in
     type (type_complexgrid_scalar_int), intent(inout) :: structure_out

     call copy_type_integer(structure_in%griduid, structure_out%griduid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_int%griduid'

     call copy_type_integer(structure_in%subgrid, structure_out%subgrid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_int%subgrid'

     call copy_type_vecint_type(structure_in%scalar, structure_out%scalar)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_int%scalar'

     call copy_type_matint_type(structure_in%vector, structure_out%vector)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_int%vector'

     call copy_type_array3dint_type(structure_in%matrix, structure_out%matrix)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_int%matrix'

   end subroutine copy_type_complexgrid_scalar_int

   subroutine copy_arr_type_complexgrid_scalar_int(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_scalar_int), pointer :: structure_in(:)
     type (type_complexgrid_scalar_int), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_scalar_int(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_scalar_int'
     end if

   end subroutine copy_arr_type_complexgrid_scalar_int

   subroutine copy_type_complexgrid_scalar_simplestruct(structure_in, structure_out)

     implicit none

     type (type_complexgrid_scalar_simplestruct), intent(in) :: structure_in
     type (type_complexgrid_scalar_simplestruct), intent(inout) :: structure_out

     call copy_type_integer(structure_in%subgrid, structure_out%subgrid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_simplestruct%subgrid'

     call copy_type_vecflt_type(structure_in%scalar, structure_out%scalar)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_simplestruct%scalar'

     call copy_type_matflt_type(structure_in%vector, structure_out%vector)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_simplestruct%vector'

     call copy_type_array3dflt_type(structure_in%matrix, structure_out%matrix)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_scalar_simplestruct%matrix'

   end subroutine copy_type_complexgrid_scalar_simplestruct

   subroutine copy_arr_type_complexgrid_scalar_simplestruct(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_scalar_simplestruct), pointer :: structure_in(:)
     type (type_complexgrid_scalar_simplestruct), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_scalar_simplestruct(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_scalar_simplestruct'
     end if

   end subroutine copy_arr_type_complexgrid_scalar_simplestruct

   subroutine copy_type_complexgrid_space(structure_in, structure_out)

     implicit none

     type (type_complexgrid_space), intent(in) :: structure_in
     type (type_complexgrid_space), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%geotype, structure_out%geotype)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_space%geotype'

     call copy_type_vecstring_type(structure_in%geotypeid, structure_out%geotypeid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_space%geotypeid'

     call copy_type_matint_type(structure_in%coordtype, structure_out%coordtype)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_space%coordtype'

     call copy_arr_type_objects(structure_in%objects, structure_out%objects)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_space%objects'

     call copy_type_vecint_type(structure_in%xpoints, structure_out%xpoints)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_space%xpoints'

   end subroutine copy_type_complexgrid_space

   subroutine copy_arr_type_complexgrid_space(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_space), pointer :: structure_in(:)
     type (type_complexgrid_space), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_space(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_space'
     end if

   end subroutine copy_arr_type_complexgrid_space

   subroutine copy_type_complexgrid_subgrid(structure_in, structure_out)

     implicit none

     type (type_complexgrid_subgrid), intent(in) :: structure_in
     type (type_complexgrid_subgrid), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_subgrid%id'

     call copy_arr_type_complexgrid_objectlist(structure_in%list, structure_out%list)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_subgrid%list'

   end subroutine copy_type_complexgrid_subgrid

   subroutine copy_arr_type_complexgrid_subgrid(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_subgrid), pointer :: structure_in(:)
     type (type_complexgrid_subgrid), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_subgrid(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_subgrid'
     end if

   end subroutine copy_arr_type_complexgrid_subgrid

   subroutine copy_type_complexgrid_vector(structure_in, structure_out)

     implicit none

     type (type_complexgrid_vector), intent(in) :: structure_in
     type (type_complexgrid_vector), intent(inout) :: structure_out

     call copy_type_integer(structure_in%griduid, structure_out%griduid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector%griduid'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector%label'

     call copy_arr_type_complexgrid_scalar(structure_in%comp, structure_out%comp)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector%comp'

     call copy_type_vecint_type(structure_in%align, structure_out%align)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector%align'

     call copy_type_vecstring_type(structure_in%alignid, structure_out%alignid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector%alignid'

     call copy_type_integer(structure_in%basis, structure_out%basis)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector%basis'

   end subroutine copy_type_complexgrid_vector

   subroutine copy_arr_type_complexgrid_vector(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_vector), pointer :: structure_in(:)
     type (type_complexgrid_vector), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_vector(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_vector'
     end if

   end subroutine copy_arr_type_complexgrid_vector

   subroutine copy_type_complexgrid_vector_simplestruct(structure_in, structure_out)

     implicit none

     type (type_complexgrid_vector_simplestruct), intent(in) :: structure_in
     type (type_complexgrid_vector_simplestruct), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector_simplestruct%label'

     call copy_arr_type_complexgrid_scalar(structure_in%comp, structure_out%comp)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector_simplestruct%comp'

     call copy_type_vecint_type(structure_in%align, structure_out%align)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector_simplestruct%align'

     call copy_type_vecstring_type(structure_in%alignid, structure_out%alignid)
     if (verbose > 0) write(iu6, *) 'copied complexgrid_vector_simplestruct%alignid'

   end subroutine copy_type_complexgrid_vector_simplestruct

   subroutine copy_arr_type_complexgrid_vector_simplestruct(structure_in, structure_out)
 
     implicit none
 
     type (type_complexgrid_vector_simplestruct), pointer :: structure_in(:)
     type (type_complexgrid_vector_simplestruct), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_complexgrid_vector_simplestruct(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_complexgrid_vector_simplestruct'
     end if

   end subroutine copy_arr_type_complexgrid_vector_simplestruct

   subroutine copy_type_composition(structure_in, structure_out)

     implicit none

     type (type_composition), intent(in) :: structure_in
     type (type_composition), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied composition%amn'

     call copy_type_vecflt_type(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied composition%zn'

     call copy_type_vecflt_type(structure_in%zion, structure_out%zion)
     if (verbose > 0) write(iu6, *) 'copied composition%zion'

     call copy_type_vecint_type(structure_in%imp_flag, structure_out%imp_flag)
     if (verbose > 0) write(iu6, *) 'copied composition%imp_flag'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied composition%label'

   end subroutine copy_type_composition

   subroutine copy_arr_type_composition(structure_in, structure_out)
 
     implicit none
 
     type (type_composition), pointer :: structure_in(:)
     type (type_composition), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_composition(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_composition'
     end if

   end subroutine copy_arr_type_composition

   subroutine copy_type_composition_neutrals(structure_in, structure_out)

     implicit none

     type (type_composition_neutrals), intent(in) :: structure_in
     type (type_composition_neutrals), intent(inout) :: structure_out

     call copy_arr_type_coreneutrals_atomlist(structure_in%atomlist, structure_out%atomlist)
     if (verbose > 0) write(iu6, *) 'copied composition_neutrals%atomlist'

     call copy_arr_type_composition_neutralscomp(structure_in%neutral, structure_out%neutral)
     if (verbose > 0) write(iu6, *) 'copied composition_neutrals%neutral'

   end subroutine copy_type_composition_neutrals

   subroutine copy_arr_type_composition_neutrals(structure_in, structure_out)
 
     implicit none
 
     type (type_composition_neutrals), pointer :: structure_in(:)
     type (type_composition_neutrals), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_composition_neutrals(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_composition_neutrals'
     end if

   end subroutine copy_arr_type_composition_neutrals

   subroutine copy_type_composition_neutrals_neutcomp(structure_in, structure_out)

     implicit none

     type (type_composition_neutrals_neutcomp), intent(in) :: structure_in
     type (type_composition_neutrals_neutcomp), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nucindex, structure_out%nucindex)
     if (verbose > 0) write(iu6, *) 'copied composition_neutrals_neutcomp%nucindex'

     call copy_type_integer(structure_in%multiplicity, structure_out%multiplicity)
     if (verbose > 0) write(iu6, *) 'copied composition_neutrals_neutcomp%multiplicity'

   end subroutine copy_type_composition_neutrals_neutcomp

   subroutine copy_arr_type_composition_neutrals_neutcomp(structure_in, structure_out)
 
     implicit none
 
     type (type_composition_neutrals_neutcomp), pointer :: structure_in(:)
     type (type_composition_neutrals_neutcomp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_composition_neutrals_neutcomp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_composition_neutrals_neutcomp'
     end if

   end subroutine copy_arr_type_composition_neutrals_neutcomp

   subroutine copy_type_composition_neutralscomp(structure_in, structure_out)

     implicit none

     type (type_composition_neutralscomp), intent(in) :: structure_in
     type (type_composition_neutralscomp), intent(inout) :: structure_out

     call copy_arr_type_composition_neutrals_neutcomp(structure_in%neutcomp, structure_out%neutcomp)
     if (verbose > 0) write(iu6, *) 'copied composition_neutralscomp%neutcomp'

     call copy_arr_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied composition_neutralscomp%type'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied composition_neutralscomp%label'

   end subroutine copy_type_composition_neutralscomp

   subroutine copy_arr_type_composition_neutralscomp(structure_in, structure_out)
 
     implicit none
 
     type (type_composition_neutralscomp), pointer :: structure_in(:)
     type (type_composition_neutralscomp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_composition_neutralscomp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_composition_neutralscomp'
     end if

   end subroutine copy_arr_type_composition_neutralscomp

   subroutine copy_type_compositions_type(structure_in, structure_out)

     implicit none

     type (type_compositions_type), intent(in) :: structure_in
     type (type_compositions_type), intent(inout) :: structure_out

     call copy_arr_type_nuclei(structure_in%nuclei, structure_out%nuclei)
     if (verbose > 0) write(iu6, *) 'copied compositions_type%nuclei'

     call copy_arr_type_ions(structure_in%ions, structure_out%ions)
     if (verbose > 0) write(iu6, *) 'copied compositions_type%ions'

     call copy_arr_type_impurities(structure_in%impurities, structure_out%impurities)
     if (verbose > 0) write(iu6, *) 'copied compositions_type%impurities'

     call copy_arr_type_composition_neutralscomp(structure_in%neutralscomp, structure_out%neutralscomp)
     if (verbose > 0) write(iu6, *) 'copied compositions_type%neutralscomp'

     call copy_arr_type_edgespecies(structure_in%edgespecies, structure_out%edgespecies)
     if (verbose > 0) write(iu6, *) 'copied compositions_type%edgespecies'

     call copy_type_identifier(structure_in%signature, structure_out%signature)
     if (verbose > 0) write(iu6, *) 'copied compositions_type%signature'

   end subroutine copy_type_compositions_type

   subroutine copy_arr_type_compositions_type(structure_in, structure_out)
 
     implicit none
 
     type (type_compositions_type), pointer :: structure_in(:)
     type (type_compositions_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_compositions_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_compositions_type'
     end if

   end subroutine copy_arr_type_compositions_type

   subroutine copy_type_compound_desc(structure_in, structure_out)

     implicit none

     type (type_compound_desc), intent(in) :: structure_in
     type (type_compound_desc), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied compound_desc%label'

     call copy_type_vecflt_type(structure_in%stochiometry, structure_out%stochiometry)
     if (verbose > 0) write(iu6, *) 'copied compound_desc%stochiometry'

     call copy_type_float(structure_in%density, structure_out%density)
     if (verbose > 0) write(iu6, *) 'copied compound_desc%density'

     call copy_type_float(structure_in%heat_cap, structure_out%heat_cap)
     if (verbose > 0) write(iu6, *) 'copied compound_desc%heat_cap'

     call copy_type_vecflt_type(structure_in%heat_cond, structure_out%heat_cond)
     if (verbose > 0) write(iu6, *) 'copied compound_desc%heat_cond'

     call copy_type_matflt_type(structure_in%surf_recrate, structure_out%surf_recrate)
     if (verbose > 0) write(iu6, *) 'copied compound_desc%surf_recrate'

   end subroutine copy_type_compound_desc

   subroutine copy_arr_type_compound_desc(structure_in, structure_out)
 
     implicit none
 
     type (type_compound_desc), pointer :: structure_in(:)
     type (type_compound_desc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_compound_desc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_compound_desc'
     end if

   end subroutine copy_arr_type_compound_desc

   subroutine copy_type_coord_sys(structure_in, structure_out)

     implicit none

     type (type_coord_sys), intent(in) :: structure_in
     type (type_coord_sys), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%grid_type, structure_out%grid_type)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%grid_type'

     call copy_type_reggrid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%grid'

     call copy_type_matflt_type(structure_in%jacobian, structure_out%jacobian)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%jacobian'

     call copy_type_matflt_type(structure_in%g_11, structure_out%g_11)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%g_11'

     call copy_type_matflt_type(structure_in%g_12, structure_out%g_12)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%g_12'

     call copy_type_matflt_type(structure_in%g_13, structure_out%g_13)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%g_13'

     call copy_type_matflt_type(structure_in%g_22, structure_out%g_22)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%g_22'

     call copy_type_matflt_type(structure_in%g_23, structure_out%g_23)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%g_23'

     call copy_type_matflt_type(structure_in%g_33, structure_out%g_33)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%g_33'

     call copy_type_rz2D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied coord_sys%position'

   end subroutine copy_type_coord_sys

   subroutine copy_arr_type_coord_sys(structure_in, structure_out)
 
     implicit none
 
     type (type_coord_sys), pointer :: structure_in(:)
     type (type_coord_sys), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coord_sys(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coord_sys'
     end if

   end subroutine copy_arr_type_coord_sys

   subroutine copy_type_coordinates(structure_in, structure_out)

     implicit none

     type (type_coordinates), intent(in) :: structure_in
     type (type_coordinates), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied coordinates%theta'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied coordinates%phi'

   end subroutine copy_type_coordinates

   subroutine copy_arr_type_coordinates(structure_in, structure_out)
 
     implicit none
 
     type (type_coordinates), pointer :: structure_in(:)
     type (type_coordinates), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coordinates(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coordinates'
     end if

   end subroutine copy_arr_type_coordinates

   subroutine copy_type_coords(structure_in, structure_out)

     implicit none

     type (type_coords), intent(in) :: structure_in
     type (type_coords), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%coord, structure_out%coord)
     if (verbose > 0) write(iu6, *) 'copied coords%coord'

     call copy_type_vecstring_type(structure_in%coord_label, structure_out%coord_label)
     if (verbose > 0) write(iu6, *) 'copied coords%coord_label'

     call copy_type_vecint_type(structure_in%extrap_type, structure_out%extrap_type)
     if (verbose > 0) write(iu6, *) 'copied coords%extrap_type'

     call copy_type_integer(structure_in%interp_type, structure_out%interp_type)
     if (verbose > 0) write(iu6, *) 'copied coords%interp_type'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied coords%label'

     call copy_type_vecstring_type(structure_in%unit, structure_out%unit)
     if (verbose > 0) write(iu6, *) 'copied coords%unit'

     call copy_type_integer(structure_in%transform, structure_out%transform)
     if (verbose > 0) write(iu6, *) 'copied coords%transform'

     call copy_type_integer(structure_in%spacing, structure_out%spacing)
     if (verbose > 0) write(iu6, *) 'copied coords%spacing'

   end subroutine copy_type_coords

   subroutine copy_arr_type_coords(structure_in, structure_out)
 
     implicit none
 
     type (type_coords), pointer :: structure_in(:)
     type (type_coords), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coords(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coords'
     end if

   end subroutine copy_arr_type_coords

   subroutine copy_type_coredelta_values(structure_in, structure_out)

     implicit none

     type (type_coredelta_values), intent(in) :: structure_in
     type (type_coredelta_values), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%deltaid, structure_out%deltaid)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%deltaid'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%area'

     call copy_type_vecflt_type(structure_in%delta_psi, structure_out%delta_psi)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%delta_psi'

     call copy_type_vecflt_type(structure_in%delta_te, structure_out%delta_te)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%delta_te'

     call copy_type_matflt_type(structure_in%delta_ti, structure_out%delta_ti)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%delta_ti'

     call copy_type_vecflt_type(structure_in%delta_ne, structure_out%delta_ne)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%delta_ne'

     call copy_type_matflt_type(structure_in%delta_ni, structure_out%delta_ni)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%delta_ni'

     call copy_arr_type_coredelta_values_impurity(structure_in%impurity, structure_out%impurity)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%impurity'

     call copy_type_matflt_type(structure_in%delta_vtor, structure_out%delta_vtor)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%delta_vtor'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values%codeparam'

   end subroutine copy_type_coredelta_values

   subroutine copy_arr_type_coredelta_values(structure_in, structure_out)
 
     implicit none
 
     type (type_coredelta_values), pointer :: structure_in(:)
     type (type_coredelta_values), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coredelta_values(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coredelta_values'
     end if

   end subroutine copy_arr_type_coredelta_values

   subroutine copy_type_coredelta_values_impurity(structure_in, structure_out)

     implicit none

     type (type_coredelta_values_impurity), intent(in) :: structure_in
     type (type_coredelta_values_impurity), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%delta_tz, structure_out%delta_tz)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values_impurity%delta_tz'

     call copy_type_matflt_type(structure_in%delta_nz, structure_out%delta_nz)
     if (verbose > 0) write(iu6, *) 'copied coredelta_values_impurity%delta_nz'

   end subroutine copy_type_coredelta_values_impurity

   subroutine copy_arr_type_coredelta_values_impurity(structure_in, structure_out)
 
     implicit none
 
     type (type_coredelta_values_impurity), pointer :: structure_in(:)
     type (type_coredelta_values_impurity), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coredelta_values_impurity(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coredelta_values_impurity'
     end if

   end subroutine copy_arr_type_coredelta_values_impurity

   subroutine copy_type_corefast_values(structure_in, structure_out)

     implicit none

     type (type_corefast_values), intent(in) :: structure_in
     type (type_corefast_values), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%fastid, structure_out%fastid)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%fastid'

     call copy_type_fast_thermal_separation_filter(structure_in%filter, structure_out%filter)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%filter'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%area'

     call copy_type_vecflt_type(structure_in%j, structure_out%j)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%j'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%sigma'

     call copy_type_matflt_type(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%ni'

     call copy_type_vecflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%ne'

     call copy_type_matflt_type(structure_in%nz, structure_out%nz)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%nz'

     call copy_type_matflt_type(structure_in%pi, structure_out%pi)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%pi'

     call copy_type_vecflt_type(structure_in%pe, structure_out%pe)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%pe'

     call copy_type_matflt_type(structure_in%pz, structure_out%pz)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%pz'

     call copy_type_matflt_type(structure_in%pi_para, structure_out%pi_para)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%pi_para'

     call copy_type_vecflt_type(structure_in%pe_para, structure_out%pe_para)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%pe_para'

     call copy_type_matflt_type(structure_in%pz_para, structure_out%pz_para)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%pz_para'

     call copy_type_matflt_type(structure_in%ui, structure_out%ui)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%ui'

     call copy_type_matflt_type(structure_in%uz, structure_out%uz)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%uz'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied corefast_values%codeparam'

   end subroutine copy_type_corefast_values

   subroutine copy_arr_type_corefast_values(structure_in, structure_out)
 
     implicit none
 
     type (type_corefast_values), pointer :: structure_in(:)
     type (type_corefast_values), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefast_values(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefast_values'
     end if

   end subroutine copy_arr_type_corefast_values

   subroutine copy_type_corefield(structure_in, structure_out)

     implicit none

     type (type_corefield), intent(in) :: structure_in
     type (type_corefield), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied corefield%value'

     call copy_type_vecflt_type(structure_in%ddrho, structure_out%ddrho)
     if (verbose > 0) write(iu6, *) 'copied corefield%ddrho'

     call copy_type_vecflt_type(structure_in%d2drho2, structure_out%d2drho2)
     if (verbose > 0) write(iu6, *) 'copied corefield%d2drho2'

     call copy_type_vecflt_type(structure_in%ddt, structure_out%ddt)
     if (verbose > 0) write(iu6, *) 'copied corefield%ddt'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied corefield%source'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied corefield%flag'

     call copy_type_boundaryel(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied corefield%boundary'

     call copy_type_sourceel(structure_in%source_term, structure_out%source_term)
     if (verbose > 0) write(iu6, *) 'copied corefield%source_term'

     call copy_type_coretransel(structure_in%transp_coef, structure_out%transp_coef)
     if (verbose > 0) write(iu6, *) 'copied corefield%transp_coef'

     call copy_type_fluxel(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied corefield%flux'

     call copy_type_vecflt_type(structure_in%flux_dv_surf, structure_out%flux_dv_surf)
     if (verbose > 0) write(iu6, *) 'copied corefield%flux_dv_surf'

     call copy_type_vecflt_type(structure_in%time_deriv, structure_out%time_deriv)
     if (verbose > 0) write(iu6, *) 'copied corefield%time_deriv'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied corefield%codeparam'

   end subroutine copy_type_corefield

   subroutine copy_arr_type_corefield(structure_in, structure_out)
 
     implicit none
 
     type (type_corefield), pointer :: structure_in(:)
     type (type_corefield), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefield(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefield'
     end if

   end subroutine copy_arr_type_corefield

   subroutine copy_type_corefieldion(structure_in, structure_out)

     implicit none

     type (type_corefieldion), intent(in) :: structure_in
     type (type_corefieldion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%value'

     call copy_type_matflt_type(structure_in%ddrho, structure_out%ddrho)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%ddrho'

     call copy_type_matflt_type(structure_in%d2drho2, structure_out%d2drho2)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%d2drho2'

     call copy_type_matflt_type(structure_in%ddt, structure_out%ddt)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%ddt'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%source'

     call copy_type_vecint_type(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%flag'

     call copy_type_boundaryion(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%boundary'

     call copy_type_sourceion(structure_in%source_term, structure_out%source_term)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%source_term'

     call copy_type_coretransion(structure_in%transp_coef, structure_out%transp_coef)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%transp_coef'

     call copy_type_fluxion(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%flux'

     call copy_type_matflt_type(structure_in%flux_dv_surf, structure_out%flux_dv_surf)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%flux_dv_surf'

     call copy_type_matflt_type(structure_in%time_deriv, structure_out%time_deriv)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%time_deriv'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied corefieldion%codeparam'

   end subroutine copy_type_corefieldion

   subroutine copy_arr_type_corefieldion(structure_in, structure_out)
 
     implicit none
 
     type (type_corefieldion), pointer :: structure_in(:)
     type (type_corefieldion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefieldion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefieldion'
     end if

   end subroutine copy_arr_type_corefieldion

   subroutine copy_type_corefieldneutral(structure_in, structure_out)

     implicit none

     type (type_corefieldneutral), intent(in) :: structure_in
     type (type_corefieldneutral), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutral%value'

     call copy_type_vecflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutral%flux'

     call copy_type_boundary_neutrals(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutral%boundary'

   end subroutine copy_type_corefieldneutral

   subroutine copy_arr_type_corefieldneutral(structure_in, structure_out)
 
     implicit none
 
     type (type_corefieldneutral), pointer :: structure_in(:)
     type (type_corefieldneutral), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefieldneutral(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefieldneutral'
     end if

   end subroutine copy_arr_type_corefieldneutral

   subroutine copy_type_corefieldneutrale(structure_in, structure_out)

     implicit none

     type (type_corefieldneutrale), intent(in) :: structure_in
     type (type_corefieldneutrale), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutrale%value'

     call copy_type_vecflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutrale%flux'

     call copy_type_boundary_neutrals(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutrale%boundary'

   end subroutine copy_type_corefieldneutrale

   subroutine copy_arr_type_corefieldneutrale(structure_in, structure_out)
 
     implicit none
 
     type (type_corefieldneutrale), pointer :: structure_in(:)
     type (type_corefieldneutrale), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefieldneutrale(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefieldneutrale'
     end if

   end subroutine copy_arr_type_corefieldneutrale

   subroutine copy_type_corefieldneutralv(structure_in, structure_out)

     implicit none

     type (type_corefieldneutralv), intent(in) :: structure_in
     type (type_corefieldneutralv), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutralv%value'

     call copy_type_boundary_neutrals(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutralv%boundary'

   end subroutine copy_type_corefieldneutralv

   subroutine copy_arr_type_corefieldneutralv(structure_in, structure_out)
 
     implicit none
 
     type (type_corefieldneutralv), pointer :: structure_in(:)
     type (type_corefieldneutralv), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefieldneutralv(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefieldneutralv'
     end if

   end subroutine copy_arr_type_corefieldneutralv

   subroutine copy_type_corefieldneutralv0(structure_in, structure_out)

     implicit none

     type (type_corefieldneutralv0), intent(in) :: structure_in
     type (type_corefieldneutralv0), intent(inout) :: structure_out

     call copy_type_corefieldneutralv(structure_in%toroidal, structure_out%toroidal)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutralv0%toroidal'

     call copy_type_corefieldneutralv(structure_in%poloidal, structure_out%poloidal)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutralv0%poloidal'

     call copy_type_corefieldneutralv(structure_in%radial, structure_out%radial)
     if (verbose > 0) write(iu6, *) 'copied corefieldneutralv0%radial'

   end subroutine copy_type_corefieldneutralv0

   subroutine copy_arr_type_corefieldneutralv0(structure_in, structure_out)
 
     implicit none
 
     type (type_corefieldneutralv0), pointer :: structure_in(:)
     type (type_corefieldneutralv0), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_corefieldneutralv0(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_corefieldneutralv0'
     end if

   end subroutine copy_arr_type_corefieldneutralv0

   subroutine copy_type_coreimpurdiag_sum_radiation(structure_in, structure_out)

     implicit none

     type (type_coreimpurdiag_sum_radiation), intent(in) :: structure_in
     type (type_coreimpurdiag_sum_radiation), intent(inout) :: structure_out

     call copy_type_coreimpurediagsum_type(structure_in%line_rad, structure_out%line_rad)
     if (verbose > 0) write(iu6, *) 'copied coreimpurdiag_sum_radiation%line_rad'

     call copy_type_coreimpurediagsum_type(structure_in%brem_radrec, structure_out%brem_radrec)
     if (verbose > 0) write(iu6, *) 'copied coreimpurdiag_sum_radiation%brem_radrec'

     call copy_type_coreimpurediagsum_type(structure_in%sum, structure_out%sum)
     if (verbose > 0) write(iu6, *) 'copied coreimpurdiag_sum_radiation%sum'

   end subroutine copy_type_coreimpurdiag_sum_radiation

   subroutine copy_arr_type_coreimpurdiag_sum_radiation(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurdiag_sum_radiation), pointer :: structure_in(:)
     type (type_coreimpurdiag_sum_radiation), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurdiag_sum_radiation(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurdiag_sum_radiation'
     end if

   end subroutine copy_arr_type_coreimpurdiag_sum_radiation

   subroutine copy_type_coreimpurediag_energy(structure_in, structure_out)

     implicit none

     type (type_coreimpurediag_energy), intent(in) :: structure_in
     type (type_coreimpurediag_energy), intent(inout) :: structure_out

     call copy_type_coreimpurediagprof_type(structure_in%ionization, structure_out%ionization)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_energy%ionization'

     call copy_type_coreimpurediagprof_type(structure_in%recombin, structure_out%recombin)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_energy%recombin'

     call copy_type_coreimpurediagprof_type(structure_in%sum, structure_out%sum)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_energy%sum'

   end subroutine copy_type_coreimpurediag_energy

   subroutine copy_arr_type_coreimpurediag_energy(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediag_energy), pointer :: structure_in(:)
     type (type_coreimpurediag_energy), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediag_energy(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediag_energy'
     end if

   end subroutine copy_arr_type_coreimpurediag_energy

   subroutine copy_type_coreimpurediag_radiation(structure_in, structure_out)

     implicit none

     type (type_coreimpurediag_radiation), intent(in) :: structure_in
     type (type_coreimpurediag_radiation), intent(inout) :: structure_out

     call copy_type_coreimpurediagprof_type(structure_in%line_rad, structure_out%line_rad)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_radiation%line_rad'

     call copy_type_coreimpurediagprof_type(structure_in%brem_radrec, structure_out%brem_radrec)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_radiation%brem_radrec'

     call copy_type_coreimpurediagprof_type(structure_in%sum, structure_out%sum)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_radiation%sum'

   end subroutine copy_type_coreimpurediag_radiation

   subroutine copy_arr_type_coreimpurediag_radiation(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediag_radiation), pointer :: structure_in(:)
     type (type_coreimpurediag_radiation), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediag_radiation(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediag_radiation'
     end if

   end subroutine copy_arr_type_coreimpurediag_radiation

   subroutine copy_type_coreimpurediag_sum(structure_in, structure_out)

     implicit none

     type (type_coreimpurediag_sum), intent(in) :: structure_in
     type (type_coreimpurediag_sum), intent(inout) :: structure_out

     call copy_type_coreimpurdiag_sum_radiation(structure_in%radiation, structure_out%radiation)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_sum%radiation'

     call copy_type_coreimpurediag_sum_energy(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_sum%energy'

   end subroutine copy_type_coreimpurediag_sum

   subroutine copy_arr_type_coreimpurediag_sum(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediag_sum), pointer :: structure_in(:)
     type (type_coreimpurediag_sum), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediag_sum(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediag_sum'
     end if

   end subroutine copy_arr_type_coreimpurediag_sum

   subroutine copy_type_coreimpurediag_sum_energy(structure_in, structure_out)

     implicit none

     type (type_coreimpurediag_sum_energy), intent(in) :: structure_in
     type (type_coreimpurediag_sum_energy), intent(inout) :: structure_out

     call copy_type_coreimpurediagsum_type(structure_in%ionization, structure_out%ionization)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_sum_energy%ionization'

     call copy_type_coreimpurediagsum_type(structure_in%recombin, structure_out%recombin)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_sum_energy%recombin'

     call copy_type_coreimpurediagsum_type(structure_in%sum, structure_out%sum)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_sum_energy%sum'

   end subroutine copy_type_coreimpurediag_sum_energy

   subroutine copy_arr_type_coreimpurediag_sum_energy(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediag_sum_energy), pointer :: structure_in(:)
     type (type_coreimpurediag_sum_energy), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediag_sum_energy(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediag_sum_energy'
     end if

   end subroutine copy_arr_type_coreimpurediag_sum_energy

   subroutine copy_type_coreimpurediag_type(structure_in, structure_out)

     implicit none

     type (type_coreimpurediag_type), intent(in) :: structure_in
     type (type_coreimpurediag_type), intent(inout) :: structure_out

     call copy_type_coreimpurediag_radiation(structure_in%radiation, structure_out%radiation)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_type%radiation'

     call copy_type_coreimpurediag_energy(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediag_type%energy'

   end subroutine copy_type_coreimpurediag_type

   subroutine copy_arr_type_coreimpurediag_type(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediag_type), pointer :: structure_in(:)
     type (type_coreimpurediag_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediag_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediag_type'
     end if

   end subroutine copy_arr_type_coreimpurediag_type

   subroutine copy_type_coreimpurediagprof_type(structure_in, structure_out)

     implicit none

     type (type_coreimpurediagprof_type), intent(in) :: structure_in
     type (type_coreimpurediagprof_type), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%profile, structure_out%profile)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediagprof_type%profile'

     call copy_type_matflt_type(structure_in%integral, structure_out%integral)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediagprof_type%integral'

   end subroutine copy_type_coreimpurediagprof_type

   subroutine copy_arr_type_coreimpurediagprof_type(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediagprof_type), pointer :: structure_in(:)
     type (type_coreimpurediagprof_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediagprof_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediagprof_type'
     end if

   end subroutine copy_arr_type_coreimpurediagprof_type

   subroutine copy_type_coreimpurediagsum_type(structure_in, structure_out)

     implicit none

     type (type_coreimpurediagsum_type), intent(in) :: structure_in
     type (type_coreimpurediagsum_type), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%profile, structure_out%profile)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediagsum_type%profile'

     call copy_type_vecflt_type(structure_in%integral, structure_out%integral)
     if (verbose > 0) write(iu6, *) 'copied coreimpurediagsum_type%integral'

   end subroutine copy_type_coreimpurediagsum_type

   subroutine copy_arr_type_coreimpurediagsum_type(structure_in, structure_out)
 
     implicit none
 
     type (type_coreimpurediagsum_type), pointer :: structure_in(:)
     type (type_coreimpurediagsum_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreimpurediagsum_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreimpurediagsum_type'
     end if

   end subroutine copy_arr_type_coreimpurediagsum_type

   subroutine copy_type_coreneutrals_atomlist(structure_in, structure_out)

     implicit none

     type (type_coreneutrals_atomlist), intent(in) :: structure_in
     type (type_coreneutrals_atomlist), intent(inout) :: structure_out

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_atomlist%amn'

     call copy_type_float(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_atomlist%zn'

     call copy_type_identifier(structure_in%ionimptype, structure_out%ionimptype)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_atomlist%ionimptype'

     call copy_type_integer(structure_in%ionimpindex, structure_out%ionimpindex)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_atomlist%ionimpindex'

   end subroutine copy_type_coreneutrals_atomlist

   subroutine copy_arr_type_coreneutrals_atomlist(structure_in, structure_out)
 
     implicit none
 
     type (type_coreneutrals_atomlist), pointer :: structure_in(:)
     type (type_coreneutrals_atomlist), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreneutrals_atomlist(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreneutrals_atomlist'
     end if

   end subroutine copy_arr_type_coreneutrals_atomlist

   subroutine copy_type_coreneutrals_neutraltype(structure_in, structure_out)

     implicit none

     type (type_coreneutrals_neutraltype), intent(in) :: structure_in
     type (type_coreneutrals_neutraltype), intent(inout) :: structure_out

     call copy_type_corefieldneutral(structure_in%n0, structure_out%n0)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_neutraltype%n0'

     call copy_type_corefieldneutrale(structure_in%t0, structure_out%t0)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_neutraltype%t0'

     call copy_type_corefieldneutralv0(structure_in%v0, structure_out%v0)
     if (verbose > 0) write(iu6, *) 'copied coreneutrals_neutraltype%v0'

   end subroutine copy_type_coreneutrals_neutraltype

   subroutine copy_arr_type_coreneutrals_neutraltype(structure_in, structure_out)
 
     implicit none
 
     type (type_coreneutrals_neutraltype), pointer :: structure_in(:)
     type (type_coreneutrals_neutraltype), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreneutrals_neutraltype(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreneutrals_neutraltype'
     end if

   end subroutine copy_arr_type_coreneutrals_neutraltype

   subroutine copy_type_coreprofile(structure_in, structure_out)

     implicit none

     type (type_coreprofile), intent(in) :: structure_in
     type (type_coreprofile), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied coreprofile%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied coreprofile%source'

   end subroutine copy_type_coreprofile

   subroutine copy_arr_type_coreprofile(structure_in, structure_out)
 
     implicit none
 
     type (type_coreprofile), pointer :: structure_in(:)
     type (type_coreprofile), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreprofile(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreprofile'
     end if

   end subroutine copy_arr_type_coreprofile

   subroutine copy_type_coreprofion(structure_in, structure_out)

     implicit none

     type (type_coreprofion), intent(in) :: structure_in
     type (type_coreprofion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied coreprofion%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied coreprofion%source'

   end subroutine copy_type_coreprofion

   subroutine copy_arr_type_coreprofion(structure_in, structure_out)
 
     implicit none
 
     type (type_coreprofion), pointer :: structure_in(:)
     type (type_coreprofion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coreprofion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coreprofion'
     end if

   end subroutine copy_arr_type_coreprofion

   subroutine copy_type_coresource_values(structure_in, structure_out)

     implicit none

     type (type_coresource_values), intent(in) :: structure_in
     type (type_coresource_values), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%sourceid, structure_out%sourceid)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%sourceid'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%area'

     call copy_type_vecflt_type(structure_in%j, structure_out%j)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%j'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%sigma'

     call copy_type_source_ion(structure_in%si, structure_out%si)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%si'

     call copy_type_source_vec(structure_in%se, structure_out%se)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%se'

     call copy_arr_type_source_imp(structure_in%sz, structure_out%sz)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%sz'

     call copy_type_source_ion(structure_in%qi, structure_out%qi)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%qi'

     call copy_type_source_vec(structure_in%qe, structure_out%qe)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%qe'

     call copy_arr_type_source_imp(structure_in%qz, structure_out%qz)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%qz'

     call copy_type_source_ion(structure_in%ui, structure_out%ui)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%ui'

     call copy_type_source_vec(structure_in%ujxb, structure_out%ujxb)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%ujxb'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coresource_values%codeparam'

   end subroutine copy_type_coresource_values

   subroutine copy_arr_type_coresource_values(structure_in, structure_out)
 
     implicit none
 
     type (type_coresource_values), pointer :: structure_in(:)
     type (type_coresource_values), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coresource_values(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coresource_values'
     end if

   end subroutine copy_arr_type_coresource_values

   subroutine copy_type_coretransel(structure_in, structure_out)

     implicit none

     type (type_coretransel), intent(in) :: structure_in
     type (type_coretransel), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%diff, structure_out%diff)
     if (verbose > 0) write(iu6, *) 'copied coretransel%diff'

     call copy_type_vecflt_type(structure_in%vconv, structure_out%vconv)
     if (verbose > 0) write(iu6, *) 'copied coretransel%vconv'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied coretransel%source'

   end subroutine copy_type_coretransel

   subroutine copy_arr_type_coretransel(structure_in, structure_out)
 
     implicit none
 
     type (type_coretransel), pointer :: structure_in(:)
     type (type_coretransel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coretransel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coretransel'
     end if

   end subroutine copy_arr_type_coretransel

   subroutine copy_type_coretransimp(structure_in, structure_out)

     implicit none

     type (type_coretransimp), intent(in) :: structure_in
     type (type_coretransimp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%diff, structure_out%diff)
     if (verbose > 0) write(iu6, *) 'copied coretransimp%diff'

     call copy_type_matflt_type(structure_in%vconv, structure_out%vconv)
     if (verbose > 0) write(iu6, *) 'copied coretransimp%vconv'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied coretransimp%source'

   end subroutine copy_type_coretransimp

   subroutine copy_arr_type_coretransimp(structure_in, structure_out)
 
     implicit none
 
     type (type_coretransimp), pointer :: structure_in(:)
     type (type_coretransimp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coretransimp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coretransimp'
     end if

   end subroutine copy_arr_type_coretransimp

   subroutine copy_type_coretransion(structure_in, structure_out)

     implicit none

     type (type_coretransion), intent(in) :: structure_in
     type (type_coretransion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%diff, structure_out%diff)
     if (verbose > 0) write(iu6, *) 'copied coretransion%diff'

     call copy_type_matflt_type(structure_in%vconv, structure_out%vconv)
     if (verbose > 0) write(iu6, *) 'copied coretransion%vconv'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied coretransion%source'

   end subroutine copy_type_coretransion

   subroutine copy_arr_type_coretransion(structure_in, structure_out)
 
     implicit none
 
     type (type_coretransion), pointer :: structure_in(:)
     type (type_coretransion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coretransion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coretransion'
     end if

   end subroutine copy_arr_type_coretransion

   subroutine copy_type_coretransp_values(structure_in, structure_out)

     implicit none

     type (type_coretransp_values), intent(in) :: structure_in
     type (type_coretransp_values), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%transportid, structure_out%transportid)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%transportid'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%rho_tor'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%area'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%sigma'

     call copy_type_ni_transp(structure_in%ni_transp, structure_out%ni_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%ni_transp'

     call copy_type_ne_transp(structure_in%ne_transp, structure_out%ne_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%ne_transp'

     call copy_arr_type_transcoefimp(structure_in%nz_transp, structure_out%nz_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%nz_transp'

     call copy_type_transcoefion(structure_in%ti_transp, structure_out%ti_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%ti_transp'

     call copy_type_transcoefel(structure_in%te_transp, structure_out%te_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%te_transp'

     call copy_arr_type_transcoefimp(structure_in%tz_transp, structure_out%tz_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%tz_transp'

     call copy_type_transcoefvtor(structure_in%vtor_transp, structure_out%vtor_transp)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%vtor_transp'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied coretransp_values%codeparam'

   end subroutine copy_type_coretransp_values

   subroutine copy_arr_type_coretransp_values(structure_in, structure_out)
 
     implicit none
 
     type (type_coretransp_values), pointer :: structure_in(:)
     type (type_coretransp_values), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_coretransp_values(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_coretransp_values'
     end if

   end subroutine copy_arr_type_coretransp_values

   subroutine copy_type_current(structure_in, structure_out)

     implicit none

     type (type_current), intent(in) :: structure_in
     type (type_current), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%mpol, structure_out%mpol)
     if (verbose > 0) write(iu6, *) 'copied current%mpol'

     call copy_type_vecint_type(structure_in%ntor, structure_out%ntor)
     if (verbose > 0) write(iu6, *) 'copied current%ntor'

     call copy_type_exp1D(structure_in%spectrum, structure_out%spectrum)
     if (verbose > 0) write(iu6, *) 'copied current%spectrum'

     call copy_type_rz0D(structure_in%rz_reference, structure_out%rz_reference)
     if (verbose > 0) write(iu6, *) 'copied current%rz_reference'

   end subroutine copy_type_current

   subroutine copy_arr_type_current(structure_in, structure_out)
 
     implicit none
 
     type (type_current), pointer :: structure_in(:)
     type (type_current), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_current(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_current'
     end if

   end subroutine copy_arr_type_current

   subroutine copy_type_cxmeasure(structure_in, structure_out)

     implicit none

     type (type_cxmeasure), intent(in) :: structure_in
     type (type_cxmeasure), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied cxmeasure%ti'

     call copy_type_exp1D(structure_in%vtor, structure_out%vtor)
     if (verbose > 0) write(iu6, *) 'copied cxmeasure%vtor'

     call copy_type_exp1D(structure_in%vpol, structure_out%vpol)
     if (verbose > 0) write(iu6, *) 'copied cxmeasure%vpol'

   end subroutine copy_type_cxmeasure

   subroutine copy_arr_type_cxmeasure(structure_in, structure_out)
 
     implicit none
 
     type (type_cxmeasure), pointer :: structure_in(:)
     type (type_cxmeasure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_cxmeasure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_cxmeasure'
     end if

   end subroutine copy_arr_type_cxmeasure

   subroutine copy_type_cxsetup(structure_in, structure_out)

     implicit none

     type (type_cxsetup), intent(in) :: structure_in
     type (type_cxsetup), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied cxsetup%amn'

     call copy_type_vecflt_type(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied cxsetup%zn'

     call copy_type_rzphi1Dexp(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied cxsetup%position'

   end subroutine copy_type_cxsetup

   subroutine copy_arr_type_cxsetup(structure_in, structure_out)
 
     implicit none
 
     type (type_cxsetup), pointer :: structure_in(:)
     type (type_cxsetup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_cxsetup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_cxsetup'
     end if

   end subroutine copy_arr_type_cxsetup

   subroutine copy_type_data_release(structure_in, structure_out)

     implicit none

     type (type_data_release), intent(in) :: structure_in
     type (type_data_release), intent(inout) :: structure_out

     call copy_type_integer(structure_in%shot, structure_out%shot)
     if (verbose > 0) write(iu6, *) 'copied data_release%shot'

     call copy_type_integer(structure_in%run, structure_out%run)
     if (verbose > 0) write(iu6, *) 'copied data_release%run'

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied data_release%description'

   end subroutine copy_type_data_release

   subroutine copy_arr_type_data_release(structure_in, structure_out)
 
     implicit none
 
     type (type_data_release), pointer :: structure_in(:)
     type (type_data_release), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_data_release(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_data_release'
     end if

   end subroutine copy_arr_type_data_release

   subroutine copy_type_datainfo(structure_in, structure_out)

     implicit none

     type (type_datainfo), intent(in) :: structure_in
     type (type_datainfo), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%dataprovider, structure_out%dataprovider)
     if (verbose > 0) write(iu6, *) 'copied datainfo%dataprovider'

     call copy_type_vecstring_type(structure_in%putdate, structure_out%putdate)
     if (verbose > 0) write(iu6, *) 'copied datainfo%putdate'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied datainfo%source'

     call copy_type_vecstring_type(structure_in%comment, structure_out%comment)
     if (verbose > 0) write(iu6, *) 'copied datainfo%comment'

     call copy_type_integer(structure_in%cocos, structure_out%cocos)
     if (verbose > 0) write(iu6, *) 'copied datainfo%cocos'

     call copy_type_integer(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied datainfo%id'

     call copy_type_integer(structure_in%isref, structure_out%isref)
     if (verbose > 0) write(iu6, *) 'copied datainfo%isref'

     call copy_type_whatref(structure_in%whatref, structure_out%whatref)
     if (verbose > 0) write(iu6, *) 'copied datainfo%whatref'

     call copy_type_putinfo(structure_in%putinfo, structure_out%putinfo)
     if (verbose > 0) write(iu6, *) 'copied datainfo%putinfo'

   end subroutine copy_type_datainfo

   subroutine copy_arr_type_datainfo(structure_in, structure_out)
 
     implicit none
 
     type (type_datainfo), pointer :: structure_in(:)
     type (type_datainfo), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_datainfo(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_datainfo'
     end if

   end subroutine copy_arr_type_datainfo

   subroutine copy_type_desc_coils(structure_in, structure_out)

     implicit none

     type (type_desc_coils), intent(in) :: structure_in
     type (type_desc_coils), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied desc_coils%name'

     call copy_type_float(structure_in%res, structure_out%res)
     if (verbose > 0) write(iu6, *) 'copied desc_coils%res'

     call copy_type_integer(structure_in%nturns, structure_out%nturns)
     if (verbose > 0) write(iu6, *) 'copied desc_coils%nturns'

     call copy_type_vecstring_type(structure_in%closed, structure_out%closed)
     if (verbose > 0) write(iu6, *) 'copied desc_coils%closed'

     call copy_arr_type_edges(structure_in%edges, structure_out%edges)
     if (verbose > 0) write(iu6, *) 'copied desc_coils%edges'

   end subroutine copy_type_desc_coils

   subroutine copy_arr_type_desc_coils(structure_in, structure_out)
 
     implicit none
 
     type (type_desc_coils), pointer :: structure_in(:)
     type (type_desc_coils), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_desc_coils(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_desc_coils'
     end if

   end subroutine copy_arr_type_desc_coils

   subroutine copy_type_desc_impur(structure_in, structure_out)

     implicit none

     type (type_desc_impur), intent(in) :: structure_in
     type (type_desc_impur), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%amn'

     call copy_type_vecint_type(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%zn'

     call copy_type_vecint_type(structure_in%i_ion, structure_out%i_ion)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%i_ion'

     call copy_type_vecint_type(structure_in%nzimp, structure_out%nzimp)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%nzimp'

     call copy_type_matint_type(structure_in%zmin, structure_out%zmin)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%zmin'

     call copy_type_matint_type(structure_in%zmax, structure_out%zmax)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%zmax'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied desc_impur%label'

   end subroutine copy_type_desc_impur

   subroutine copy_arr_type_desc_impur(structure_in, structure_out)
 
     implicit none
 
     type (type_desc_impur), pointer :: structure_in(:)
     type (type_desc_impur), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_desc_impur(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_desc_impur'
     end if

   end subroutine copy_arr_type_desc_impur

   subroutine copy_type_desc_iron(structure_in, structure_out)

     implicit none

     type (type_desc_iron), intent(in) :: structure_in
     type (type_desc_iron), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied desc_iron%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied desc_iron%id'

     call copy_type_permeability(structure_in%permeability, structure_out%permeability)
     if (verbose > 0) write(iu6, *) 'copied desc_iron%permeability'

     call copy_type_geom_iron(structure_in%geom_iron, structure_out%geom_iron)
     if (verbose > 0) write(iu6, *) 'copied desc_iron%geom_iron'

   end subroutine copy_type_desc_iron

   subroutine copy_arr_type_desc_iron(structure_in, structure_out)
 
     implicit none
 
     type (type_desc_iron), pointer :: structure_in(:)
     type (type_desc_iron), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_desc_iron(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_desc_iron'
     end if

   end subroutine copy_arr_type_desc_iron

   subroutine copy_type_desc_pfcoils(structure_in, structure_out)

     implicit none

     type (type_desc_pfcoils), intent(in) :: structure_in
     type (type_desc_pfcoils), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%id'

     call copy_type_vecflt_type(structure_in%res, structure_out%res)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%res'

     call copy_type_vecflt_type(structure_in%emax, structure_out%emax)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%emax'

     call copy_type_structure_cs(structure_in%structure_cs, structure_out%structure_cs)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%structure_cs'

     call copy_type_float(structure_in%pol_flux_cs, structure_out%pol_flux_cs)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%pol_flux_cs'

     call copy_type_vecint_type(structure_in%nelement, structure_out%nelement)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%nelement'

     call copy_type_pfelement(structure_in%pfelement, structure_out%pfelement)
     if (verbose > 0) write(iu6, *) 'copied desc_pfcoils%pfelement'

   end subroutine copy_type_desc_pfcoils

   subroutine copy_arr_type_desc_pfcoils(structure_in, structure_out)
 
     implicit none
 
     type (type_desc_pfcoils), pointer :: structure_in(:)
     type (type_desc_pfcoils), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_desc_pfcoils(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_desc_pfcoils'
     end if

   end subroutine copy_arr_type_desc_pfcoils

   subroutine copy_type_desc_supply(structure_in, structure_out)

     implicit none

     type (type_desc_supply), intent(in) :: structure_in
     type (type_desc_supply), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%id'

     call copy_type_vecstring_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%type'

     call copy_type_vecflt_type(structure_in%delay, structure_out%delay)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%delay'

     call copy_type_filter(structure_in%filter, structure_out%filter)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%filter'

     call copy_type_vecflt_type(structure_in%imin, structure_out%imin)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%imin'

     call copy_type_vecflt_type(structure_in%imax, structure_out%imax)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%imax'

     call copy_type_vecflt_type(structure_in%res, structure_out%res)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%res'

     call copy_type_vecflt_type(structure_in%umin, structure_out%umin)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%umin'

     call copy_type_vecflt_type(structure_in%umax, structure_out%umax)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%umax'

     call copy_type_vecflt_type(structure_in%emax, structure_out%emax)
     if (verbose > 0) write(iu6, *) 'copied desc_supply%emax'

   end subroutine copy_type_desc_supply

   subroutine copy_arr_type_desc_supply(structure_in, structure_out)
 
     implicit none
 
     type (type_desc_supply), pointer :: structure_in(:)
     type (type_desc_supply), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_desc_supply(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_desc_supply'
     end if

   end subroutine copy_arr_type_desc_supply

   subroutine copy_type_diag_func(structure_in, structure_out)

     implicit none

     type (type_diag_func), intent(in) :: structure_in
     type (type_diag_func), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied diag_func%description'

     call copy_type_matflt_type(structure_in%transf_mat, structure_out%transf_mat)
     if (verbose > 0) write(iu6, *) 'copied diag_func%transf_mat'

   end subroutine copy_type_diag_func

   subroutine copy_arr_type_diag_func(structure_in, structure_out)
 
     implicit none
 
     type (type_diag_func), pointer :: structure_in(:)
     type (type_diag_func), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_diag_func(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_diag_func'
     end if

   end subroutine copy_arr_type_diag_func

   subroutine copy_type_dist_collisional_transfer_0d(structure_in, structure_out)

     implicit none

     type (type_dist_collisional_transfer_0d), intent(in) :: structure_in
     type (type_dist_collisional_transfer_0d), intent(inout) :: structure_out

     call copy_type_float(structure_in%power_th, structure_out%power_th)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_0d%power_th'

     call copy_type_float(structure_in%power_fast, structure_out%power_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_0d%power_fast'

     call copy_type_float(structure_in%torque_th, structure_out%torque_th)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_0d%torque_th'

     call copy_type_float(structure_in%torque_fast, structure_out%torque_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_0d%torque_fast'

   end subroutine copy_type_dist_collisional_transfer_0d

   subroutine copy_arr_type_dist_collisional_transfer_0d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_collisional_transfer_0d), pointer :: structure_in(:)
     type (type_dist_collisional_transfer_0d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_collisional_transfer_0d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_collisional_transfer_0d'
     end if

   end subroutine copy_arr_type_dist_collisional_transfer_0d

   subroutine copy_type_dist_collisional_transfer_1d(structure_in, structure_out)

     implicit none

     type (type_dist_collisional_transfer_1d), intent(in) :: structure_in
     type (type_dist_collisional_transfer_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%power_th, structure_out%power_th)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_1d%power_th'

     call copy_type_vecflt_type(structure_in%power_fast, structure_out%power_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_1d%power_fast'

     call copy_type_vecflt_type(structure_in%torque_th, structure_out%torque_th)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_1d%torque_th'

     call copy_type_vecflt_type(structure_in%torque_fast, structure_out%torque_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_1d%torque_fast'

   end subroutine copy_type_dist_collisional_transfer_1d

   subroutine copy_arr_type_dist_collisional_transfer_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_collisional_transfer_1d), pointer :: structure_in(:)
     type (type_dist_collisional_transfer_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_collisional_transfer_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_collisional_transfer_1d'
     end if

   end subroutine copy_arr_type_dist_collisional_transfer_1d

   subroutine copy_type_dist_collisional_transfer_2d(structure_in, structure_out)

     implicit none

     type (type_dist_collisional_transfer_2d), intent(in) :: structure_in
     type (type_dist_collisional_transfer_2d), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%power_th, structure_out%power_th)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_2d%power_th'

     call copy_type_matflt_type(structure_in%power_fast, structure_out%power_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_2d%power_fast'

     call copy_type_matflt_type(structure_in%torque_th, structure_out%torque_th)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_2d%torque_th'

     call copy_type_matflt_type(structure_in%torque_fast, structure_out%torque_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_collisional_transfer_2d%torque_fast'

   end subroutine copy_type_dist_collisional_transfer_2d

   subroutine copy_arr_type_dist_collisional_transfer_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_collisional_transfer_2d), pointer :: structure_in(:)
     type (type_dist_collisional_transfer_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_collisional_transfer_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_collisional_transfer_2d'
     end if

   end subroutine copy_arr_type_dist_collisional_transfer_2d

   subroutine copy_type_dist_distrivec_distfunc_fexp_param(structure_in, structure_out)

     implicit none

     type (type_dist_distrivec_distfunc_fexp_param), intent(in) :: structure_in
     type (type_dist_distrivec_distfunc_fexp_param), intent(inout) :: structure_out

     call copy_type_equatorial_plane(structure_in%equatorial, structure_out%equatorial)
     if (verbose > 0) write(iu6, *) 'copied dist_distrivec_distfunc_fexp_param%equatorial'

     call copy_type_vecflt_type(structure_in%temperature, structure_out%temperature)
     if (verbose > 0) write(iu6, *) 'copied dist_distrivec_distfunc_fexp_param%temperature'

   end subroutine copy_type_dist_distrivec_distfunc_fexp_param

   subroutine copy_arr_type_dist_distrivec_distfunc_fexp_param(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_distrivec_distfunc_fexp_param), pointer :: structure_in(:)
     type (type_dist_distrivec_distfunc_fexp_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_distrivec_distfunc_fexp_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_distrivec_distfunc_fexp_param'
     end if

   end subroutine copy_arr_type_dist_distrivec_distfunc_fexp_param

   subroutine copy_type_dist_ff(structure_in, structure_out)

     implicit none

     type (type_dist_ff), intent(in) :: structure_in
     type (type_dist_ff), intent(inout) :: structure_out

     call copy_type_dist_grid_info(structure_in%grid_info, structure_out%grid_info)
     if (verbose > 0) write(iu6, *) 'copied dist_ff%grid_info'

     call copy_arr_type_topo_regions(structure_in%topo_regions, structure_out%topo_regions)
     if (verbose > 0) write(iu6, *) 'copied dist_ff%topo_regions'

   end subroutine copy_type_dist_ff

   subroutine copy_arr_type_dist_ff(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_ff), pointer :: structure_in(:)
     type (type_dist_ff), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_ff(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_ff'
     end if

   end subroutine copy_arr_type_dist_ff

   subroutine copy_type_dist_func(structure_in, structure_out)

     implicit none

     type (type_dist_func), intent(in) :: structure_in
     type (type_dist_func), intent(inout) :: structure_out

     call copy_type_integer(structure_in%is_delta_f, structure_out%is_delta_f)
     if (verbose > 0) write(iu6, *) 'copied dist_func%is_delta_f'

     call copy_type_weighted_markers(structure_in%markers, structure_out%markers)
     if (verbose > 0) write(iu6, *) 'copied dist_func%markers'

     call copy_arr_type_dist_ff(structure_in%f_expan_topo, structure_out%f_expan_topo)
     if (verbose > 0) write(iu6, *) 'copied dist_func%f_expan_topo'

     call copy_arr_type_f_expansion(structure_in%f_expansion, structure_out%f_expansion)
     if (verbose > 0) write(iu6, *) 'copied dist_func%f_expansion'

   end subroutine copy_type_dist_func

   subroutine copy_arr_type_dist_func(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_func), pointer :: structure_in(:)
     type (type_dist_func), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_func(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_func'
     end if

   end subroutine copy_arr_type_dist_func

   subroutine copy_type_dist_geometry_0d(structure_in, structure_out)

     implicit none

     type (type_dist_geometry_0d), intent(in) :: structure_in
     type (type_dist_geometry_0d), intent(inout) :: structure_out

     call copy_type_rz0D(structure_in%mag_axis, structure_out%mag_axis)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_0d%mag_axis'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_0d%toroid_field'

   end subroutine copy_type_dist_geometry_0d

   subroutine copy_arr_type_dist_geometry_0d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_geometry_0d), pointer :: structure_in(:)
     type (type_dist_geometry_0d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_geometry_0d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_geometry_0d'
     end if

   end subroutine copy_arr_type_dist_geometry_0d

   subroutine copy_type_dist_geometry_1d(structure_in, structure_out)

     implicit none

     type (type_dist_geometry_1d), intent(in) :: structure_in
     type (type_dist_geometry_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_1d%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_1d%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_1d%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_1d%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_1d%area'

   end subroutine copy_type_dist_geometry_1d

   subroutine copy_arr_type_dist_geometry_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_geometry_1d), pointer :: structure_in(:)
     type (type_dist_geometry_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_geometry_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_geometry_1d'
     end if

   end subroutine copy_arr_type_dist_geometry_1d

   subroutine copy_type_dist_geometry_2d(structure_in, structure_out)

     implicit none

     type (type_dist_geometry_2d), intent(in) :: structure_in
     type (type_dist_geometry_2d), intent(inout) :: structure_out

     call copy_type_integer(structure_in%coord_type, structure_out%coord_type)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%coord_type'

     call copy_type_matflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%r'

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%z'

     call copy_type_matflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%rho_tor'

     call copy_type_matflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%psi'

     call copy_type_matflt_type(structure_in%theta_geom, structure_out%theta_geom)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%theta_geom'

     call copy_type_matflt_type(structure_in%theta_strt, structure_out%theta_strt)
     if (verbose > 0) write(iu6, *) 'copied dist_geometry_2d%theta_strt'

   end subroutine copy_type_dist_geometry_2d

   subroutine copy_arr_type_dist_geometry_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_geometry_2d), pointer :: structure_in(:)
     type (type_dist_geometry_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_geometry_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_geometry_2d'
     end if

   end subroutine copy_arr_type_dist_geometry_2d

   subroutine copy_type_dist_global_param(structure_in, structure_out)

     implicit none

     type (type_dist_global_param), intent(in) :: structure_in
     type (type_dist_global_param), intent(inout) :: structure_out

     call copy_type_dist_geometry_0d(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param%geometry'

     call copy_type_dist_state_0d(structure_in%state, structure_out%state)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param%state'

     call copy_type_dist_collisional_transfer_0d(structure_in%collisions_e, structure_out%collisions_e)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param%collisions_e'

     call copy_arr_type_dist_collisional_transfer_0d(structure_in%collisions_i, structure_out%collisions_i)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param%collisions_i'

     call copy_arr_type_dist_global_param_collisions_z(structure_in%collisions_z, structure_out%collisions_z)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param%collisions_z'

     call copy_arr_type_dist_sources_0d(structure_in%sources, structure_out%sources)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param%sources'

   end subroutine copy_type_dist_global_param

   subroutine copy_arr_type_dist_global_param(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_global_param), pointer :: structure_in(:)
     type (type_dist_global_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_global_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_global_param'
     end if

   end subroutine copy_arr_type_dist_global_param

   subroutine copy_type_dist_global_param_collisions_z(structure_in, structure_out)

     implicit none

     type (type_dist_global_param_collisions_z), intent(in) :: structure_in
     type (type_dist_global_param_collisions_z), intent(inout) :: structure_out

     call copy_arr_type_dist_collisional_transfer_0d(structure_in%charge_state, structure_out%charge_state)
     if (verbose > 0) write(iu6, *) 'copied dist_global_param_collisions_z%charge_state'

   end subroutine copy_type_dist_global_param_collisions_z

   subroutine copy_arr_type_dist_global_param_collisions_z(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_global_param_collisions_z), pointer :: structure_in(:)
     type (type_dist_global_param_collisions_z), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_global_param_collisions_z(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_global_param_collisions_z'
     end if

   end subroutine copy_arr_type_dist_global_param_collisions_z

   subroutine copy_type_dist_grid_info(structure_in, structure_out)

     implicit none

     type (type_dist_grid_info), intent(in) :: structure_in
     type (type_dist_grid_info), intent(inout) :: structure_out

     call copy_type_integer(structure_in%grid_type, structure_out%grid_type)
     if (verbose > 0) write(iu6, *) 'copied dist_grid_info%grid_type'

     call copy_type_integer(structure_in%ngriddim, structure_out%ngriddim)
     if (verbose > 0) write(iu6, *) 'copied dist_grid_info%ngriddim'

     call copy_type_vecint_type(structure_in%grid_coord, structure_out%grid_coord)
     if (verbose > 0) write(iu6, *) 'copied dist_grid_info%grid_coord'

     call copy_type_integer(structure_in%thin_orbits, structure_out%thin_orbits)
     if (verbose > 0) write(iu6, *) 'copied dist_grid_info%thin_orbits'

     call copy_type_vecstring_type(structure_in%topology, structure_out%topology)
     if (verbose > 0) write(iu6, *) 'copied dist_grid_info%topology'

     call copy_arr_type_omnigen_surf(structure_in%omnigen_surf, structure_out%omnigen_surf)
     if (verbose > 0) write(iu6, *) 'copied dist_grid_info%omnigen_surf'

   end subroutine copy_type_dist_grid_info

   subroutine copy_arr_type_dist_grid_info(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_grid_info), pointer :: structure_in(:)
     type (type_dist_grid_info), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_grid_info(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_grid_info'
     end if

   end subroutine copy_arr_type_dist_grid_info

   subroutine copy_type_dist_profile_values_1d(structure_in, structure_out)

     implicit none

     type (type_dist_profile_values_1d), intent(in) :: structure_in
     type (type_dist_profile_values_1d), intent(inout) :: structure_out

     call copy_type_dist_state_1d(structure_in%state, structure_out%state)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_1d%state'

     call copy_type_dist_collisional_transfer_1d(structure_in%collisions_e, structure_out%collisions_e)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_1d%collisions_e'

     call copy_arr_type_dist_collisional_transfer_1d(structure_in%collisions_i, structure_out%collisions_i)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_1d%collisions_i'

     call copy_arr_type_dist_profiles_1d_collisions_z(structure_in%collisions_z, structure_out%collisions_z)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_1d%collisions_z'

     call copy_arr_type_dist_sources_1d(structure_in%sources, structure_out%sources)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_1d%sources'

   end subroutine copy_type_dist_profile_values_1d

   subroutine copy_arr_type_dist_profile_values_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_profile_values_1d), pointer :: structure_in(:)
     type (type_dist_profile_values_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_profile_values_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_profile_values_1d'
     end if

   end subroutine copy_arr_type_dist_profile_values_1d

   subroutine copy_type_dist_profile_values_2d(structure_in, structure_out)

     implicit none

     type (type_dist_profile_values_2d), intent(in) :: structure_in
     type (type_dist_profile_values_2d), intent(inout) :: structure_out

     call copy_type_dist_state_2d(structure_in%state, structure_out%state)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_2d%state'

     call copy_type_dist_collisional_transfer_2d(structure_in%collisions_e, structure_out%collisions_e)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_2d%collisions_e'

     call copy_arr_type_dist_collisional_transfer_2d(structure_in%collisions_i, structure_out%collisions_i)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_2d%collisions_i'

     call copy_arr_type_dist_profiles2d_collisions_z(structure_in%collisions_z, structure_out%collisions_z)
     if (verbose > 0) write(iu6, *) 'copied dist_profile_values_2d%collisions_z'

   end subroutine copy_type_dist_profile_values_2d

   subroutine copy_arr_type_dist_profile_values_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_profile_values_2d), pointer :: structure_in(:)
     type (type_dist_profile_values_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_profile_values_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_profile_values_2d'
     end if

   end subroutine copy_arr_type_dist_profile_values_2d

   subroutine copy_type_dist_profiles2d_collisions_z(structure_in, structure_out)

     implicit none

     type (type_dist_profiles2d_collisions_z), intent(in) :: structure_in
     type (type_dist_profiles2d_collisions_z), intent(inout) :: structure_out

     call copy_arr_type_dist_collisional_transfer_2d(structure_in%charge_state, structure_out%charge_state)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles2d_collisions_z%charge_state'

   end subroutine copy_type_dist_profiles2d_collisions_z

   subroutine copy_arr_type_dist_profiles2d_collisions_z(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_profiles2d_collisions_z), pointer :: structure_in(:)
     type (type_dist_profiles2d_collisions_z), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_profiles2d_collisions_z(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_profiles2d_collisions_z'
     end if

   end subroutine copy_arr_type_dist_profiles2d_collisions_z

   subroutine copy_type_dist_profiles_1d(structure_in, structure_out)

     implicit none

     type (type_dist_profiles_1d), intent(in) :: structure_in
     type (type_dist_profiles_1d), intent(inout) :: structure_out

     call copy_type_dist_geometry_1d(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%geometry'

     call copy_type_dist_state_1d(structure_in%state, structure_out%state)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%state'

     call copy_type_dist_collisional_transfer_1d(structure_in%collisions_e, structure_out%collisions_e)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%collisions_e'

     call copy_arr_type_dist_collisional_transfer_1d(structure_in%collisions_i, structure_out%collisions_i)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%collisions_i'

     call copy_arr_type_dist_profiles_1d_collisions_z(structure_in%collisions_z, structure_out%collisions_z)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%collisions_z'

     call copy_type_dist_thermalised_1d(structure_in%thermalised, structure_out%thermalised)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%thermalised'

     call copy_arr_type_dist_sources_1d(structure_in%sources, structure_out%sources)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%sources'

     call copy_type_dist_profile_values_1d(structure_in%trapped, structure_out%trapped)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%trapped'

     call copy_type_dist_profile_values_1d(structure_in%co_passing, structure_out%co_passing)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%co_passing'

     call copy_type_dist_profile_values_1d(structure_in%cntr_passing, structure_out%cntr_passing)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d%cntr_passing'

   end subroutine copy_type_dist_profiles_1d

   subroutine copy_arr_type_dist_profiles_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_profiles_1d), pointer :: structure_in(:)
     type (type_dist_profiles_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_profiles_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_profiles_1d'
     end if

   end subroutine copy_arr_type_dist_profiles_1d

   subroutine copy_type_dist_profiles_1d_collisions_z(structure_in, structure_out)

     implicit none

     type (type_dist_profiles_1d_collisions_z), intent(in) :: structure_in
     type (type_dist_profiles_1d_collisions_z), intent(inout) :: structure_out

     call copy_arr_type_dist_collisional_transfer_1d(structure_in%charge_state, structure_out%charge_state)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_1d_collisions_z%charge_state'

   end subroutine copy_type_dist_profiles_1d_collisions_z

   subroutine copy_arr_type_dist_profiles_1d_collisions_z(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_profiles_1d_collisions_z), pointer :: structure_in(:)
     type (type_dist_profiles_1d_collisions_z), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_profiles_1d_collisions_z(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_profiles_1d_collisions_z'
     end if

   end subroutine copy_arr_type_dist_profiles_1d_collisions_z

   subroutine copy_type_dist_profiles_2d(structure_in, structure_out)

     implicit none

     type (type_dist_profiles_2d), intent(in) :: structure_in
     type (type_dist_profiles_2d), intent(inout) :: structure_out

     call copy_type_dist_geometry_2d(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%geometry'

     call copy_type_dist_state_2d(structure_in%state, structure_out%state)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%state'

     call copy_type_dist_collisional_transfer_2d(structure_in%collisions_e, structure_out%collisions_e)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%collisions_e'

     call copy_arr_type_dist_collisional_transfer_2d(structure_in%collisions_i, structure_out%collisions_i)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%collisions_i'

     call copy_arr_type_dist_profiles2d_collisions_z(structure_in%collisions_z, structure_out%collisions_z)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%collisions_z'

     call copy_type_dist_profile_values_2d(structure_in%trapped, structure_out%trapped)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%trapped'

     call copy_type_dist_profile_values_2d(structure_in%co_passing, structure_out%co_passing)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%co_passing'

     call copy_type_dist_profile_values_2d(structure_in%cntr_passing, structure_out%cntr_passing)
     if (verbose > 0) write(iu6, *) 'copied dist_profiles_2d%cntr_passing'

   end subroutine copy_type_dist_profiles_2d

   subroutine copy_arr_type_dist_profiles_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_profiles_2d), pointer :: structure_in(:)
     type (type_dist_profiles_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_profiles_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_profiles_2d'
     end if

   end subroutine copy_arr_type_dist_profiles_2d

   subroutine copy_type_dist_sources_0d(structure_in, structure_out)

     implicit none

     type (type_dist_sources_0d), intent(in) :: structure_in
     type (type_dist_sources_0d), intent(inout) :: structure_out

     call copy_type_dist_sources_reference(structure_in%source_ref, structure_out%source_ref)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_0d%source_ref'

     call copy_type_float(structure_in%particle, structure_out%particle)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_0d%particle'

     call copy_type_float(structure_in%momentum, structure_out%momentum)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_0d%momentum'

     call copy_type_float(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_0d%energy'

   end subroutine copy_type_dist_sources_0d

   subroutine copy_arr_type_dist_sources_0d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_sources_0d), pointer :: structure_in(:)
     type (type_dist_sources_0d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_sources_0d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_sources_0d'
     end if

   end subroutine copy_arr_type_dist_sources_0d

   subroutine copy_type_dist_sources_1d(structure_in, structure_out)

     implicit none

     type (type_dist_sources_1d), intent(in) :: structure_in
     type (type_dist_sources_1d), intent(inout) :: structure_out

     call copy_type_dist_sources_reference(structure_in%source_ref, structure_out%source_ref)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_1d%source_ref'

     call copy_type_vecflt_type(structure_in%particle, structure_out%particle)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_1d%particle'

     call copy_type_vecflt_type(structure_in%momentum, structure_out%momentum)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_1d%momentum'

     call copy_type_vecflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_1d%energy'

   end subroutine copy_type_dist_sources_1d

   subroutine copy_arr_type_dist_sources_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_sources_1d), pointer :: structure_in(:)
     type (type_dist_sources_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_sources_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_sources_1d'
     end if

   end subroutine copy_arr_type_dist_sources_1d

   subroutine copy_type_dist_sources_reference(structure_in, structure_out)

     implicit none

     type (type_dist_sources_reference), intent(in) :: structure_in
     type (type_dist_sources_reference), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_reference%type'

     call copy_type_vecint_type(structure_in%index_waveid, structure_out%index_waveid)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_reference%index_waveid'

     call copy_type_vecint_type(structure_in%index_srcid, structure_out%index_srcid)
     if (verbose > 0) write(iu6, *) 'copied dist_sources_reference%index_srcid'

   end subroutine copy_type_dist_sources_reference

   subroutine copy_arr_type_dist_sources_reference(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_sources_reference), pointer :: structure_in(:)
     type (type_dist_sources_reference), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_sources_reference(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_sources_reference'
     end if

   end subroutine copy_arr_type_dist_sources_reference

   subroutine copy_type_dist_state_0d(structure_in, structure_out)

     implicit none

     type (type_dist_state_0d), intent(in) :: structure_in
     type (type_dist_state_0d), intent(inout) :: structure_out

     call copy_type_float(structure_in%n_particles, structure_out%n_particles)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%n_particles'

     call copy_type_float(structure_in%n_part_fast, structure_out%n_part_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%n_part_fast'

     call copy_type_float(structure_in%enrg, structure_out%enrg)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%enrg'

     call copy_type_float(structure_in%enrg_fast, structure_out%enrg_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%enrg_fast'

     call copy_type_float(structure_in%enrg_fast_pa, structure_out%enrg_fast_pa)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%enrg_fast_pa'

     call copy_type_float(structure_in%momentm_fast, structure_out%momentm_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%momentm_fast'

     call copy_type_float(structure_in%current_dr, structure_out%current_dr)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%current_dr'

     call copy_type_float(structure_in%torque_jrxb, structure_out%torque_jrxb)
     if (verbose > 0) write(iu6, *) 'copied dist_state_0d%torque_jrxb'

   end subroutine copy_type_dist_state_0d

   subroutine copy_arr_type_dist_state_0d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_state_0d), pointer :: structure_in(:)
     type (type_dist_state_0d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_state_0d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_state_0d'
     end if

   end subroutine copy_arr_type_dist_state_0d

   subroutine copy_type_dist_state_1d(structure_in, structure_out)

     implicit none

     type (type_dist_state_1d), intent(in) :: structure_in
     type (type_dist_state_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%dens, structure_out%dens)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%dens'

     call copy_type_vecflt_type(structure_in%dens_fast, structure_out%dens_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%dens_fast'

     call copy_type_vecflt_type(structure_in%pres, structure_out%pres)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%pres'

     call copy_type_vecflt_type(structure_in%pres_fast, structure_out%pres_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%pres_fast'

     call copy_type_vecflt_type(structure_in%pres_fast_pa, structure_out%pres_fast_pa)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%pres_fast_pa'

     call copy_type_vecflt_type(structure_in%momentm_fast, structure_out%momentm_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%momentm_fast'

     call copy_type_vecflt_type(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%current'

     call copy_type_vecflt_type(structure_in%current_fast, structure_out%current_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%current_fast'

     call copy_type_vecflt_type(structure_in%torque_jrxb, structure_out%torque_jrxb)
     if (verbose > 0) write(iu6, *) 'copied dist_state_1d%torque_jrxb'

   end subroutine copy_type_dist_state_1d

   subroutine copy_arr_type_dist_state_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_state_1d), pointer :: structure_in(:)
     type (type_dist_state_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_state_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_state_1d'
     end if

   end subroutine copy_arr_type_dist_state_1d

   subroutine copy_type_dist_state_2d(structure_in, structure_out)

     implicit none

     type (type_dist_state_2d), intent(in) :: structure_in
     type (type_dist_state_2d), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%dens, structure_out%dens)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%dens'

     call copy_type_matflt_type(structure_in%dens_fast, structure_out%dens_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%dens_fast'

     call copy_type_matflt_type(structure_in%pres, structure_out%pres)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%pres'

     call copy_type_matflt_type(structure_in%pres_fast, structure_out%pres_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%pres_fast'

     call copy_type_matflt_type(structure_in%pres_fast_pa, structure_out%pres_fast_pa)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%pres_fast_pa'

     call copy_type_matflt_type(structure_in%momentm_fast, structure_out%momentm_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%momentm_fast'

     call copy_type_matflt_type(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%current'

     call copy_type_matflt_type(structure_in%current_fast, structure_out%current_fast)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%current_fast'

     call copy_type_matflt_type(structure_in%torque_jrxb, structure_out%torque_jrxb)
     if (verbose > 0) write(iu6, *) 'copied dist_state_2d%torque_jrxb'

   end subroutine copy_type_dist_state_2d

   subroutine copy_arr_type_dist_state_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_state_2d), pointer :: structure_in(:)
     type (type_dist_state_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_state_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_state_2d'
     end if

   end subroutine copy_arr_type_dist_state_2d

   subroutine copy_type_dist_thermalised_1d(structure_in, structure_out)

     implicit none

     type (type_dist_thermalised_1d), intent(in) :: structure_in
     type (type_dist_thermalised_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%particle, structure_out%particle)
     if (verbose > 0) write(iu6, *) 'copied dist_thermalised_1d%particle'

     call copy_type_vecflt_type(structure_in%momentum, structure_out%momentum)
     if (verbose > 0) write(iu6, *) 'copied dist_thermalised_1d%momentum'

     call copy_type_vecflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied dist_thermalised_1d%energy'

   end subroutine copy_type_dist_thermalised_1d

   subroutine copy_arr_type_dist_thermalised_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_dist_thermalised_1d), pointer :: structure_in(:)
     type (type_dist_thermalised_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_dist_thermalised_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_dist_thermalised_1d'
     end if

   end subroutine copy_arr_type_dist_thermalised_1d

   subroutine copy_type_distri_vec(structure_in, structure_out)

     implicit none

     type (type_distri_vec), intent(in) :: structure_in
     type (type_distri_vec), intent(inout) :: structure_out

     call copy_arr_type_enum_instance(structure_in%wave_id, structure_out%wave_id)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%wave_id'

     call copy_arr_type_enum_instance(structure_in%source_id, structure_out%source_id)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%source_id'

     call copy_type_species_reference(structure_in%species, structure_out%species)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%species'

     call copy_type_integer(structure_in%gyro_type, structure_out%gyro_type)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%gyro_type'

     call copy_type_fast_thermal_separation_filter(structure_in%fast_filter, structure_out%fast_filter)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%fast_filter'

     call copy_type_dist_global_param(structure_in%global_param, structure_out%global_param)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%global_param'

     call copy_type_dist_profiles_1d(structure_in%profiles_1d, structure_out%profiles_1d)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%profiles_1d'

     call copy_type_dist_profiles_2d(structure_in%profiles_2d, structure_out%profiles_2d)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%profiles_2d'

     call copy_type_dist_func(structure_in%dist_func, structure_out%dist_func)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%dist_func'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied distri_vec%codeparam'

   end subroutine copy_type_distri_vec

   subroutine copy_arr_type_distri_vec(structure_in, structure_out)
 
     implicit none
 
     type (type_distri_vec), pointer :: structure_in(:)
     type (type_distri_vec), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distri_vec(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distri_vec'
     end if

   end subroutine copy_arr_type_distri_vec

   subroutine copy_type_distsource_global_param(structure_in, structure_out)

     implicit none

     type (type_distsource_global_param), intent(in) :: structure_in
     type (type_distsource_global_param), intent(inout) :: structure_out

     call copy_type_exp0D(structure_in%src_pow, structure_out%src_pow)
     if (verbose > 0) write(iu6, *) 'copied distsource_global_param%src_pow'

     call copy_type_exp0D(structure_in%src_rate, structure_out%src_rate)
     if (verbose > 0) write(iu6, *) 'copied distsource_global_param%src_rate'

     call copy_type_rz0D(structure_in%mag_axis, structure_out%mag_axis)
     if (verbose > 0) write(iu6, *) 'copied distsource_global_param%mag_axis'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied distsource_global_param%toroid_field'

   end subroutine copy_type_distsource_global_param

   subroutine copy_arr_type_distsource_global_param(structure_in, structure_out)
 
     implicit none
 
     type (type_distsource_global_param), pointer :: structure_in(:)
     type (type_distsource_global_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distsource_global_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distsource_global_param'
     end if

   end subroutine copy_arr_type_distsource_global_param

   subroutine copy_type_distsource_line_src_prof(structure_in, structure_out)

     implicit none

     type (type_distsource_line_src_prof), intent(in) :: structure_in
     type (type_distsource_line_src_prof), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%psi'

     call copy_type_vecflt_type(structure_in%R, structure_out%R)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%R'

     call copy_type_vecflt_type(structure_in%Z, structure_out%Z)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%Z'

     call copy_type_vecflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%theta'

     call copy_type_vecflt_type(structure_in%theta_id, structure_out%theta_id)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%theta_id'

     call copy_type_matflt_type(structure_in%th2th_pol, structure_out%th2th_pol)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%th2th_pol'

     call copy_type_vecflt_type(structure_in%pitch, structure_out%pitch)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%pitch'

     call copy_type_vecflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%energy'

     call copy_type_vecflt_type(structure_in%ang_momentum, structure_out%ang_momentum)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%ang_momentum'

     call copy_type_vecflt_type(structure_in%src_rate, structure_out%src_rate)
     if (verbose > 0) write(iu6, *) 'copied distsource_line_src_prof%src_rate'

   end subroutine copy_type_distsource_line_src_prof

   subroutine copy_arr_type_distsource_line_src_prof(structure_in, structure_out)
 
     implicit none
 
     type (type_distsource_line_src_prof), pointer :: structure_in(:)
     type (type_distsource_line_src_prof), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distsource_line_src_prof(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distsource_line_src_prof'
     end if

   end subroutine copy_arr_type_distsource_line_src_prof

   subroutine copy_type_distsource_profiles_1d(structure_in, structure_out)

     implicit none

     type (type_distsource_profiles_1d), intent(in) :: structure_in
     type (type_distsource_profiles_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%area'

     call copy_type_exp1D(structure_in%pow_den, structure_out%pow_den)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%pow_den'

     call copy_type_exp1D(structure_in%trq_den, structure_out%trq_den)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%trq_den'

     call copy_type_exp1D(structure_in%src_rate, structure_out%src_rate)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_1d%src_rate'

   end subroutine copy_type_distsource_profiles_1d

   subroutine copy_arr_type_distsource_profiles_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_distsource_profiles_1d), pointer :: structure_in(:)
     type (type_distsource_profiles_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distsource_profiles_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distsource_profiles_1d'
     end if

   end subroutine copy_arr_type_distsource_profiles_1d

   subroutine copy_type_distsource_profiles_2d(structure_in, structure_out)

     implicit none

     type (type_distsource_profiles_2d), intent(in) :: structure_in
     type (type_distsource_profiles_2d), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%grid_coord, structure_out%grid_coord)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%grid_coord'

     call copy_type_matflt_type(structure_in%dim1, structure_out%dim1)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%dim1'

     call copy_type_matflt_type(structure_in%dim2, structure_out%dim2)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%dim2'

     call copy_type_matflt_type(structure_in%g11, structure_out%g11)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%g11'

     call copy_type_matflt_type(structure_in%g12, structure_out%g12)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%g12'

     call copy_type_matflt_type(structure_in%g21, structure_out%g21)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%g21'

     call copy_type_matflt_type(structure_in%g22, structure_out%g22)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%g22'

     call copy_type_exp2D(structure_in%pow_den, structure_out%pow_den)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%pow_den'

     call copy_type_exp2D(structure_in%src_rate, structure_out%src_rate)
     if (verbose > 0) write(iu6, *) 'copied distsource_profiles_2d%src_rate'

   end subroutine copy_type_distsource_profiles_2d

   subroutine copy_arr_type_distsource_profiles_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_distsource_profiles_2d), pointer :: structure_in(:)
     type (type_distsource_profiles_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distsource_profiles_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distsource_profiles_2d'
     end if

   end subroutine copy_arr_type_distsource_profiles_2d

   subroutine copy_type_distsource_source(structure_in, structure_out)

     implicit none

     type (type_distsource_source), intent(in) :: structure_in
     type (type_distsource_source), intent(inout) :: structure_out

     call copy_arr_type_enum_instance(structure_in%source_id, structure_out%source_id)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%source_id'

     call copy_type_species_reference(structure_in%species, structure_out%species)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%species'

     call copy_type_integer(structure_in%gyro_type, structure_out%gyro_type)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%gyro_type'

     call copy_type_distsource_global_param(structure_in%global_param, structure_out%global_param)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%global_param'

     call copy_type_distsource_profiles_1d(structure_in%profiles_1d, structure_out%profiles_1d)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%profiles_1d'

     call copy_type_distsource_profiles_2d(structure_in%profiles_2d, structure_out%profiles_2d)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%profiles_2d'

     call copy_arr_type_distsource_line_src_prof(structure_in%line_srcprof, structure_out%line_srcprof)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%line_srcprof'

     call copy_type_source_rate(structure_in%source_rate, structure_out%source_rate)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%source_rate'

     call copy_type_weighted_markers(structure_in%markers, structure_out%markers)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%markers'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied distsource_source%codeparam'

   end subroutine copy_type_distsource_source

   subroutine copy_arr_type_distsource_source(structure_in, structure_out)
 
     implicit none
 
     type (type_distsource_source), pointer :: structure_in(:)
     type (type_distsource_source), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_distsource_source(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_distsource_source'
     end if

   end subroutine copy_arr_type_distsource_source

   subroutine copy_type_divergence(structure_in, structure_out)

     implicit none

     type (type_divergence), intent(in) :: structure_in
     type (type_divergence), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%frac_divcomp, structure_out%frac_divcomp)
     if (verbose > 0) write(iu6, *) 'copied divergence%frac_divcomp'

     call copy_type_vecflt_type(structure_in%div_vert, structure_out%div_vert)
     if (verbose > 0) write(iu6, *) 'copied divergence%div_vert'

     call copy_type_vecflt_type(structure_in%div_horiz, structure_out%div_horiz)
     if (verbose > 0) write(iu6, *) 'copied divergence%div_horiz'

   end subroutine copy_type_divergence

   subroutine copy_arr_type_divergence(structure_in, structure_out)
 
     implicit none
 
     type (type_divergence), pointer :: structure_in(:)
     type (type_divergence), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_divergence(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_divergence'
     end if

   end subroutine copy_arr_type_divergence

   subroutine copy_type_e_components(structure_in, structure_out)

     implicit none

     type (type_e_components), intent(in) :: structure_in
     type (type_e_components), intent(inout) :: structure_out

     call copy_type_complexgrid_scalar_cplx(structure_in%e_plus, structure_out%e_plus)
     if (verbose > 0) write(iu6, *) 'copied e_components%e_plus'

     call copy_type_complexgrid_scalar_cplx(structure_in%e_minus, structure_out%e_minus)
     if (verbose > 0) write(iu6, *) 'copied e_components%e_minus'

     call copy_type_complexgrid_scalar_cplx(structure_in%e_para, structure_out%e_para)
     if (verbose > 0) write(iu6, *) 'copied e_components%e_para'

     call copy_type_complexgrid_scalar_cplx(structure_in%e_norm, structure_out%e_norm)
     if (verbose > 0) write(iu6, *) 'copied e_components%e_norm'

     call copy_type_complexgrid_scalar_cplx(structure_in%e_binorm, structure_out%e_binorm)
     if (verbose > 0) write(iu6, *) 'copied e_components%e_binorm'

     call copy_type_complexgrid_scalar_cplx(structure_in%b_norm, structure_out%b_norm)
     if (verbose > 0) write(iu6, *) 'copied e_components%b_norm'

     call copy_type_complexgrid_scalar_cplx(structure_in%b_binorm, structure_out%b_binorm)
     if (verbose > 0) write(iu6, *) 'copied e_components%b_binorm'

     call copy_type_complexgrid_scalar_cplx(structure_in%b_para, structure_out%b_para)
     if (verbose > 0) write(iu6, *) 'copied e_components%b_para'

     call copy_type_complexgrid_scalar_cplx(structure_in%k_perp, structure_out%k_perp)
     if (verbose > 0) write(iu6, *) 'copied e_components%k_perp'

   end subroutine copy_type_e_components

   subroutine copy_arr_type_e_components(structure_in, structure_out)
 
     implicit none
 
     type (type_e_components), pointer :: structure_in(:)
     type (type_e_components), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_e_components(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_e_components'
     end if

   end subroutine copy_arr_type_e_components

   subroutine copy_type_ecemeasure(structure_in, structure_out)

     implicit none

     type (type_ecemeasure), intent(in) :: structure_in
     type (type_ecemeasure), intent(inout) :: structure_out

     call copy_type_integer(structure_in%harmonic, structure_out%harmonic)
     if (verbose > 0) write(iu6, *) 'copied ecemeasure%harmonic'

     call copy_type_rzphi1Dexp(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied ecemeasure%position'

     call copy_type_exp1D(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied ecemeasure%te'

   end subroutine copy_type_ecemeasure

   subroutine copy_arr_type_ecemeasure(structure_in, structure_out)
 
     implicit none
 
     type (type_ecemeasure), pointer :: structure_in(:)
     type (type_ecemeasure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ecemeasure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ecemeasure'
     end if

   end subroutine copy_arr_type_ecemeasure

   subroutine copy_type_ecesetup(structure_in, structure_out)

     implicit none

     type (type_ecesetup), intent(in) :: structure_in
     type (type_ecesetup), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied ecesetup%frequency'

     call copy_type_setup_line_exp(structure_in%los, structure_out%los)
     if (verbose > 0) write(iu6, *) 'copied ecesetup%los'

   end subroutine copy_type_ecesetup

   subroutine copy_arr_type_ecesetup(structure_in, structure_out)
 
     implicit none
 
     type (type_ecesetup), pointer :: structure_in(:)
     type (type_ecesetup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ecesetup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ecesetup'
     end if

   end subroutine copy_arr_type_ecesetup

   subroutine copy_type_edge_fluid(structure_in, structure_out)

     implicit none

     type (type_edge_fluid), intent(in) :: structure_in
     type (type_edge_fluid), intent(inout) :: structure_out

     call copy_type_edge_fluid_scalar_simplestruct(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%ne'

     call copy_arr_type_edge_fluid_scalar(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%ni'

     call copy_type_edge_fluid_vector_simplestruct(structure_in%ve, structure_out%ve)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%ve'

     call copy_arr_type_edge_fluid_vector(structure_in%vi, structure_out%vi)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%vi'

     call copy_type_edge_fluid_scalar_simplestruct(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%te'

     call copy_arr_type_edge_fluid_scalar(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%ti'

     call copy_type_edge_fluid_vector_simplestruct(structure_in%te_aniso, structure_out%te_aniso)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%te_aniso'

     call copy_arr_type_edge_fluid_vector(structure_in%ti_aniso, structure_out%ti_aniso)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%ti_aniso'

     call copy_type_edge_fluid_scalar_simplestruct(structure_in%po, structure_out%po)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%po'

     call copy_type_edge_fluid_vector_simplestruct(structure_in%j, structure_out%j)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%j'

     call copy_arr_type_complexgrid_vector(structure_in%b, structure_out%b)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid%b'

   end subroutine copy_type_edge_fluid

   subroutine copy_arr_type_edge_fluid(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_fluid), pointer :: structure_in(:)
     type (type_edge_fluid), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_fluid(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_fluid'
     end if

   end subroutine copy_arr_type_edge_fluid

   subroutine copy_type_edge_fluid_scalar(structure_in, structure_out)

     implicit none

     type (type_edge_fluid_scalar), intent(in) :: structure_in
     type (type_edge_fluid_scalar), intent(inout) :: structure_out

     call copy_arr_type_complexgrid_scalar(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar%value'

     call copy_arr_type_complexgrid_scalar(structure_in%bndvalue, structure_out%bndvalue)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar%bndvalue'

     call copy_arr_type_complexgrid_vector(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar%flux'

     call copy_arr_type_complexgrid_vector(structure_in%bndflux, structure_out%bndflux)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar%bndflux'

     call copy_arr_type_edge_fluid_scalar_transpcoeff(structure_in%transpcoeff, structure_out%transpcoeff)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar%transpcoeff'

     call copy_arr_type_complexgrid_scalar(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar%source'

   end subroutine copy_type_edge_fluid_scalar

   subroutine copy_arr_type_edge_fluid_scalar(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_fluid_scalar), pointer :: structure_in(:)
     type (type_edge_fluid_scalar), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_fluid_scalar(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_fluid_scalar'
     end if

   end subroutine copy_arr_type_edge_fluid_scalar

   subroutine copy_type_edge_fluid_scalar_simplestruct(structure_in, structure_out)

     implicit none

     type (type_edge_fluid_scalar_simplestruct), intent(in) :: structure_in
     type (type_edge_fluid_scalar_simplestruct), intent(inout) :: structure_out

     call copy_arr_type_complexgrid_scalar(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_simplestruct%value'

     call copy_arr_type_complexgrid_scalar(structure_in%bndvalue, structure_out%bndvalue)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_simplestruct%bndvalue'

     call copy_arr_type_complexgrid_vector(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_simplestruct%flux'

     call copy_arr_type_complexgrid_vector(structure_in%bndflux, structure_out%bndflux)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_simplestruct%bndflux'

     call copy_arr_type_edge_fluid_scalar_transpcoeff(structure_in%transpcoeff, structure_out%transpcoeff)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_simplestruct%transpcoeff'

     call copy_arr_type_complexgrid_scalar(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_simplestruct%source'

   end subroutine copy_type_edge_fluid_scalar_simplestruct

   subroutine copy_arr_type_edge_fluid_scalar_simplestruct(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_fluid_scalar_simplestruct), pointer :: structure_in(:)
     type (type_edge_fluid_scalar_simplestruct), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_fluid_scalar_simplestruct(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_fluid_scalar_simplestruct'
     end if

   end subroutine copy_arr_type_edge_fluid_scalar_simplestruct

   subroutine copy_type_edge_fluid_scalar_transpcoeff(structure_in, structure_out)

     implicit none

     type (type_edge_fluid_scalar_transpcoeff), intent(in) :: structure_in
     type (type_edge_fluid_scalar_transpcoeff), intent(inout) :: structure_out

     call copy_type_complexgrid_vector_simplestruct(structure_in%d, structure_out%d)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_transpcoeff%d'

     call copy_type_complexgrid_vector_simplestruct(structure_in%v, structure_out%v)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_scalar_transpcoeff%v'

   end subroutine copy_type_edge_fluid_scalar_transpcoeff

   subroutine copy_arr_type_edge_fluid_scalar_transpcoeff(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_fluid_scalar_transpcoeff), pointer :: structure_in(:)
     type (type_edge_fluid_scalar_transpcoeff), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_fluid_scalar_transpcoeff(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_fluid_scalar_transpcoeff'
     end if

   end subroutine copy_arr_type_edge_fluid_scalar_transpcoeff

   subroutine copy_type_edge_fluid_vector(structure_in, structure_out)

     implicit none

     type (type_edge_fluid_vector), intent(in) :: structure_in
     type (type_edge_fluid_vector), intent(inout) :: structure_out

     call copy_type_integer(structure_in%griduid, structure_out%griduid)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector%griduid'

     call copy_type_integer(structure_in%basis, structure_out%basis)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector%basis'

     call copy_type_vecint_type(structure_in%align, structure_out%align)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector%align'

     call copy_type_vecstring_type(structure_in%alignid, structure_out%alignid)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector%alignid'

     call copy_arr_type_edge_fluid_scalar(structure_in%comps, structure_out%comps)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector%comps'

   end subroutine copy_type_edge_fluid_vector

   subroutine copy_arr_type_edge_fluid_vector(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_fluid_vector), pointer :: structure_in(:)
     type (type_edge_fluid_vector), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_fluid_vector(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_fluid_vector'
     end if

   end subroutine copy_arr_type_edge_fluid_vector

   subroutine copy_type_edge_fluid_vector_simplestruct(structure_in, structure_out)

     implicit none

     type (type_edge_fluid_vector_simplestruct), intent(in) :: structure_in
     type (type_edge_fluid_vector_simplestruct), intent(inout) :: structure_out

     call copy_type_integer(structure_in%griduid, structure_out%griduid)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector_simplestruct%griduid'

     call copy_type_integer(structure_in%basis, structure_out%basis)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector_simplestruct%basis'

     call copy_arr_type_edge_fluid_scalar(structure_in%comps, structure_out%comps)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector_simplestruct%comps'

     call copy_type_vecint_type(structure_in%align, structure_out%align)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector_simplestruct%align'

     call copy_type_vecstring_type(structure_in%alignid, structure_out%alignid)
     if (verbose > 0) write(iu6, *) 'copied edge_fluid_vector_simplestruct%alignid'

   end subroutine copy_type_edge_fluid_vector_simplestruct

   subroutine copy_arr_type_edge_fluid_vector_simplestruct(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_fluid_vector_simplestruct), pointer :: structure_in(:)
     type (type_edge_fluid_vector_simplestruct), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_fluid_vector_simplestruct(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_fluid_vector_simplestruct'
     end if

   end subroutine copy_arr_type_edge_fluid_vector_simplestruct

   subroutine copy_type_edge_kinetic(structure_in, structure_out)

     implicit none

     type (type_edge_kinetic), intent(in) :: structure_in
     type (type_edge_kinetic), intent(inout) :: structure_out

     call copy_arr_type_edge_kinetic_distribution(structure_in%f, structure_out%f)
     if (verbose > 0) write(iu6, *) 'copied edge_kinetic%f'

   end subroutine copy_type_edge_kinetic

   subroutine copy_arr_type_edge_kinetic(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_kinetic), pointer :: structure_in(:)
     type (type_edge_kinetic), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_kinetic(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_kinetic'
     end if

   end subroutine copy_arr_type_edge_kinetic

   subroutine copy_type_edge_kinetic_distribution(structure_in, structure_out)

     implicit none

     type (type_edge_kinetic_distribution), intent(in) :: structure_in
     type (type_edge_kinetic_distribution), intent(inout) :: structure_out

     call copy_arr_type_complexgrid_scalar(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied edge_kinetic_distribution%value'

     call copy_arr_type_complexgrid_scalar(structure_in%bndvalue, structure_out%bndvalue)
     if (verbose > 0) write(iu6, *) 'copied edge_kinetic_distribution%bndvalue'

     call copy_arr_type_complexgrid_vector(structure_in%fluxes, structure_out%fluxes)
     if (verbose > 0) write(iu6, *) 'copied edge_kinetic_distribution%fluxes'

     call copy_arr_type_complexgrid_scalar(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied edge_kinetic_distribution%source'

   end subroutine copy_type_edge_kinetic_distribution

   subroutine copy_arr_type_edge_kinetic_distribution(structure_in, structure_out)
 
     implicit none
 
     type (type_edge_kinetic_distribution), pointer :: structure_in(:)
     type (type_edge_kinetic_distribution), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edge_kinetic_distribution(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edge_kinetic_distribution'
     end if

   end subroutine copy_arr_type_edge_kinetic_distribution

   subroutine copy_type_edges(structure_in, structure_out)

     implicit none

     type (type_edges), intent(in) :: structure_in
     type (type_edges), intent(inout) :: structure_out

     call copy_type_rzphi1D(structure_in%edge_rzphi, structure_out%edge_rzphi)
     if (verbose > 0) write(iu6, *) 'copied edges%edge_rzphi'

   end subroutine copy_type_edges

   subroutine copy_arr_type_edges(structure_in, structure_out)
 
     implicit none
 
     type (type_edges), pointer :: structure_in(:)
     type (type_edges), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edges(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edges'
     end if

   end subroutine copy_arr_type_edges

   subroutine copy_type_edgespecies(structure_in, structure_out)

     implicit none

     type (type_edgespecies), intent(in) :: structure_in
     type (type_edgespecies), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nucindex, structure_out%nucindex)
     if (verbose > 0) write(iu6, *) 'copied edgespecies%nucindex'

     call copy_type_float(structure_in%zmin, structure_out%zmin)
     if (verbose > 0) write(iu6, *) 'copied edgespecies%zmin'

     call copy_type_float(structure_in%zmax, structure_out%zmax)
     if (verbose > 0) write(iu6, *) 'copied edgespecies%zmax'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied edgespecies%label'

   end subroutine copy_type_edgespecies

   subroutine copy_arr_type_edgespecies(structure_in, structure_out)
 
     implicit none
 
     type (type_edgespecies), pointer :: structure_in(:)
     type (type_edgespecies), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_edgespecies(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_edgespecies'
     end if

   end subroutine copy_arr_type_edgespecies

   subroutine copy_type_element_desc(structure_in, structure_out)

     implicit none

     type (type_element_desc), intent(in) :: structure_in
     type (type_element_desc), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nucindex, structure_out%nucindex)
     if (verbose > 0) write(iu6, *) 'copied element_desc%nucindex'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied element_desc%label'

     call copy_type_float(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied element_desc%zn'

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied element_desc%amn'

   end subroutine copy_type_element_desc

   subroutine copy_arr_type_element_desc(structure_in, structure_out)
 
     implicit none
 
     type (type_element_desc), pointer :: structure_in(:)
     type (type_element_desc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_element_desc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_element_desc'
     end if

   end subroutine copy_arr_type_element_desc

   subroutine copy_type_entry_def(structure_in, structure_out)

     implicit none

     type (type_entry_def), intent(in) :: structure_in
     type (type_entry_def), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%user, structure_out%user)
     if (verbose > 0) write(iu6, *) 'copied entry_def%user'

     call copy_type_vecstring_type(structure_in%machine, structure_out%machine)
     if (verbose > 0) write(iu6, *) 'copied entry_def%machine'

     call copy_type_integer(structure_in%shot, structure_out%shot)
     if (verbose > 0) write(iu6, *) 'copied entry_def%shot'

     call copy_type_integer(structure_in%run, structure_out%run)
     if (verbose > 0) write(iu6, *) 'copied entry_def%run'

   end subroutine copy_type_entry_def

   subroutine copy_arr_type_entry_def(structure_in, structure_out)
 
     implicit none
 
     type (type_entry_def), pointer :: structure_in(:)
     type (type_entry_def), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_entry_def(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_entry_def'
     end if

   end subroutine copy_arr_type_entry_def

   subroutine copy_type_enum_instance(structure_in, structure_out)

     implicit none

     type (type_enum_instance), intent(in) :: structure_in
     type (type_enum_instance), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied enum_instance%type'

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied enum_instance%name'

     call copy_type_integer(structure_in%index, structure_out%index)
     if (verbose > 0) write(iu6, *) 'copied enum_instance%index'

   end subroutine copy_type_enum_instance

   subroutine copy_arr_type_enum_instance(structure_in, structure_out)
 
     implicit none
 
     type (type_enum_instance), pointer :: structure_in(:)
     type (type_enum_instance), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_enum_instance(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_enum_instance'
     end if

   end subroutine copy_arr_type_enum_instance

   subroutine copy_type_eqconstraint(structure_in, structure_out)

     implicit none

     type (type_eqconstraint), intent(in) :: structure_in
     type (type_eqconstraint), intent(inout) :: structure_out

     call copy_type_eqmes1D(structure_in%bpol, structure_out%bpol)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%bpol'

     call copy_type_eqmes0D(structure_in%bvac_r, structure_out%bvac_r)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%bvac_r'

     call copy_type_eqmes0D(structure_in%diamagflux, structure_out%diamagflux)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%diamagflux'

     call copy_type_eqmes1D(structure_in%faraday, structure_out%faraday)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%faraday'

     call copy_type_eqmes1D(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%flux'

     call copy_type_eqmes0D(structure_in%i_plasma, structure_out%i_plasma)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%i_plasma'

     call copy_type_isoflux(structure_in%isoflux, structure_out%isoflux)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%isoflux'

     call copy_type_eqmes1D(structure_in%jsurf, structure_out%jsurf)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%jsurf'

     call copy_type_magnet_iron(structure_in%magnet_iron, structure_out%magnet_iron)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%magnet_iron'

     call copy_type_eqmes1D(structure_in%mse, structure_out%mse)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%mse'

     call copy_type_eqmes1D(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%ne'

     call copy_type_eqmes1D(structure_in%pfcurrent, structure_out%pfcurrent)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%pfcurrent'

     call copy_type_eqmes1D(structure_in%pressure, structure_out%pressure)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%pressure'

     call copy_type_q(structure_in%q, structure_out%q)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%q'

     call copy_type_xpts(structure_in%xpts, structure_out%xpts)
     if (verbose > 0) write(iu6, *) 'copied eqconstraint%xpts'

   end subroutine copy_type_eqconstraint

   subroutine copy_arr_type_eqconstraint(structure_in, structure_out)
 
     implicit none
 
     type (type_eqconstraint), pointer :: structure_in(:)
     type (type_eqconstraint), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_eqconstraint(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_eqconstraint'
     end if

   end subroutine copy_arr_type_eqconstraint

   subroutine copy_type_eqgeometry(structure_in, structure_out)

     implicit none

     type (type_eqgeometry), intent(in) :: structure_in
     type (type_eqgeometry), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%source'

     call copy_type_integer(structure_in%boundarytype, structure_out%boundarytype)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%boundarytype'

     call copy_arr_type_rz1Dexp(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%boundary'

     call copy_type_rz0D(structure_in%geom_axis, structure_out%geom_axis)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%geom_axis'

     call copy_type_float(structure_in%a_minor, structure_out%a_minor)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%a_minor'

     call copy_type_float(structure_in%elongation, structure_out%elongation)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%elongation'

     call copy_type_float(structure_in%elong_upper, structure_out%elong_upper)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%elong_upper'

     call copy_type_float(structure_in%elong_lower, structure_out%elong_lower)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%elong_lower'

     call copy_type_float(structure_in%tria_upper, structure_out%tria_upper)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%tria_upper'

     call copy_type_float(structure_in%tria_lower, structure_out%tria_lower)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%tria_lower'

     call copy_arr_type_rz1Dexp(structure_in%xpts, structure_out%xpts)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%xpts'

     call copy_type_rz0D(structure_in%left_low_st, structure_out%left_low_st)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%left_low_st'

     call copy_type_rz0D(structure_in%right_low_st, structure_out%right_low_st)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%right_low_st'

     call copy_type_rz0D(structure_in%left_up_st, structure_out%left_up_st)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%left_up_st'

     call copy_type_rz0D(structure_in%right_up_st, structure_out%right_up_st)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%right_up_st'

     call copy_type_rz0D(structure_in%active_limit, structure_out%active_limit)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%active_limit'

     call copy_type_float(structure_in%ang_lcms_upo, structure_out%ang_lcms_upo)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%ang_lcms_upo'

     call copy_type_float(structure_in%ang_lcms_upi, structure_out%ang_lcms_upi)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%ang_lcms_upi'

     call copy_type_float(structure_in%ang_lcms_lwo, structure_out%ang_lcms_lwo)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%ang_lcms_lwo'

     call copy_type_float(structure_in%ang_lcms_lwi, structure_out%ang_lcms_lwi)
     if (verbose > 0) write(iu6, *) 'copied eqgeometry%ang_lcms_lwi'

   end subroutine copy_type_eqgeometry

   subroutine copy_arr_type_eqgeometry(structure_in, structure_out)
 
     implicit none
 
     type (type_eqgeometry), pointer :: structure_in(:)
     type (type_eqgeometry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_eqgeometry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_eqgeometry'
     end if

   end subroutine copy_arr_type_eqgeometry

   subroutine copy_type_eqmes0D(structure_in, structure_out)

     implicit none

     type (type_eqmes0D), intent(in) :: structure_in
     type (type_eqmes0D), intent(inout) :: structure_out

     call copy_type_float(structure_in%measured, structure_out%measured)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%measured'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%source'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%time'

     call copy_type_integer(structure_in%exact, structure_out%exact)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%exact'

     call copy_type_float(structure_in%weight, structure_out%weight)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%weight'

     call copy_type_float(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%sigma'

     call copy_type_float(structure_in%calculated, structure_out%calculated)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%calculated'

     call copy_type_float(structure_in%chi2, structure_out%chi2)
     if (verbose > 0) write(iu6, *) 'copied eqmes0D%chi2'

   end subroutine copy_type_eqmes0D

   subroutine copy_arr_type_eqmes0D(structure_in, structure_out)
 
     implicit none
 
     type (type_eqmes0D), pointer :: structure_in(:)
     type (type_eqmes0D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_eqmes0D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_eqmes0D'
     end if

   end subroutine copy_arr_type_eqmes0D

   subroutine copy_type_eqmes1D(structure_in, structure_out)

     implicit none

     type (type_eqmes1D), intent(in) :: structure_in
     type (type_eqmes1D), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%measured, structure_out%measured)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%measured'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%source'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%time'

     call copy_type_vecint_type(structure_in%exact, structure_out%exact)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%exact'

     call copy_type_vecflt_type(structure_in%weight, structure_out%weight)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%weight'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%sigma'

     call copy_type_vecflt_type(structure_in%calculated, structure_out%calculated)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%calculated'

     call copy_type_vecflt_type(structure_in%chi2, structure_out%chi2)
     if (verbose > 0) write(iu6, *) 'copied eqmes1D%chi2'

   end subroutine copy_type_eqmes1D

   subroutine copy_arr_type_eqmes1D(structure_in, structure_out)
 
     implicit none
 
     type (type_eqmes1D), pointer :: structure_in(:)
     type (type_eqmes1D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_eqmes1D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_eqmes1D'
     end if

   end subroutine copy_arr_type_eqmes1D

   subroutine copy_type_equatorial_plane(structure_in, structure_out)

     implicit none

     type (type_equatorial_plane), intent(in) :: structure_in
     type (type_equatorial_plane), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied equatorial_plane%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied equatorial_plane%z'

     call copy_type_vecflt_type(structure_in%s, structure_out%s)
     if (verbose > 0) write(iu6, *) 'copied equatorial_plane%s'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied equatorial_plane%rho_tor'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied equatorial_plane%psi'

     call copy_type_vecflt_type(structure_in%b_mod, structure_out%b_mod)
     if (verbose > 0) write(iu6, *) 'copied equatorial_plane%b_mod'

   end subroutine copy_type_equatorial_plane

   subroutine copy_arr_type_equatorial_plane(structure_in, structure_out)
 
     implicit none
 
     type (type_equatorial_plane), pointer :: structure_in(:)
     type (type_equatorial_plane), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_equatorial_plane(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_equatorial_plane'
     end if

   end subroutine copy_arr_type_equatorial_plane

   subroutine copy_type_equilibrium_profiles2d_grid(structure_in, structure_out)

     implicit none

     type (type_equilibrium_profiles2d_grid), intent(in) :: structure_in
     type (type_equilibrium_profiles2d_grid), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%dim1, structure_out%dim1)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles2d_grid%dim1'

     call copy_type_vecflt_type(structure_in%dim2, structure_out%dim2)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles2d_grid%dim2'

     call copy_type_matint_type(structure_in%connect, structure_out%connect)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles2d_grid%connect'

   end subroutine copy_type_equilibrium_profiles2d_grid

   subroutine copy_arr_type_equilibrium_profiles2d_grid(structure_in, structure_out)
 
     implicit none
 
     type (type_equilibrium_profiles2d_grid), pointer :: structure_in(:)
     type (type_equilibrium_profiles2d_grid), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_equilibrium_profiles2d_grid(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_equilibrium_profiles2d_grid'
     end if

   end subroutine copy_arr_type_equilibrium_profiles2d_grid

   subroutine copy_type_equilibrium_profiles_2d(structure_in, structure_out)

     implicit none

     type (type_equilibrium_profiles_2d), intent(in) :: structure_in
     type (type_equilibrium_profiles_2d), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%grid_type, structure_out%grid_type)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%grid_type'

     call copy_type_equilibrium_profiles2d_grid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%grid'

     call copy_type_matflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%r'

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%z'

     call copy_type_matflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%psi'

     call copy_type_matflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%theta'

     call copy_type_matflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%phi'

     call copy_type_matflt_type(structure_in%jphi, structure_out%jphi)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%jphi'

     call copy_type_matflt_type(structure_in%jpar, structure_out%jpar)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%jpar'

     call copy_type_matflt_type(structure_in%br, structure_out%br)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%br'

     call copy_type_matflt_type(structure_in%bz, structure_out%bz)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%bz'

     call copy_type_matflt_type(structure_in%bphi, structure_out%bphi)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%bphi'

     call copy_type_matflt_type(structure_in%vphi, structure_out%vphi)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%vphi'

     call copy_type_matflt_type(structure_in%vtheta, structure_out%vtheta)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%vtheta'

     call copy_type_matflt_type(structure_in%rho_mass, structure_out%rho_mass)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%rho_mass'

     call copy_type_matflt_type(structure_in%pressure, structure_out%pressure)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%pressure'

     call copy_type_matflt_type(structure_in%temperature, structure_out%temperature)
     if (verbose > 0) write(iu6, *) 'copied equilibrium_profiles_2d%temperature'

   end subroutine copy_type_equilibrium_profiles_2d

   subroutine copy_arr_type_equilibrium_profiles_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_equilibrium_profiles_2d), pointer :: structure_in(:)
     type (type_equilibrium_profiles_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_equilibrium_profiles_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_equilibrium_profiles_2d'
     end if

   end subroutine copy_arr_type_equilibrium_profiles_2d

   subroutine copy_type_exp0D(structure_in, structure_out)

     implicit none

     type (type_exp0D), intent(in) :: structure_in
     type (type_exp0D), intent(inout) :: structure_out

     call copy_type_float(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied exp0D%value'

     call copy_type_float(structure_in%abserror, structure_out%abserror)
     if (verbose > 0) write(iu6, *) 'copied exp0D%abserror'

     call copy_type_float(structure_in%relerror, structure_out%relerror)
     if (verbose > 0) write(iu6, *) 'copied exp0D%relerror'

   end subroutine copy_type_exp0D

   subroutine copy_arr_type_exp0D(structure_in, structure_out)
 
     implicit none
 
     type (type_exp0D), pointer :: structure_in(:)
     type (type_exp0D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_exp0D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_exp0D'
     end if

   end subroutine copy_arr_type_exp0D

   subroutine copy_type_exp1D(structure_in, structure_out)

     implicit none

     type (type_exp1D), intent(in) :: structure_in
     type (type_exp1D), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied exp1D%value'

     call copy_type_vecflt_type(structure_in%abserror, structure_out%abserror)
     if (verbose > 0) write(iu6, *) 'copied exp1D%abserror'

     call copy_type_vecflt_type(structure_in%relerror, structure_out%relerror)
     if (verbose > 0) write(iu6, *) 'copied exp1D%relerror'

   end subroutine copy_type_exp1D

   subroutine copy_arr_type_exp1D(structure_in, structure_out)
 
     implicit none
 
     type (type_exp1D), pointer :: structure_in(:)
     type (type_exp1D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_exp1D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_exp1D'
     end if

   end subroutine copy_arr_type_exp1D

   subroutine copy_type_exp2D(structure_in, structure_out)

     implicit none

     type (type_exp2D), intent(in) :: structure_in
     type (type_exp2D), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied exp2D%value'

     call copy_type_matflt_type(structure_in%abserror, structure_out%abserror)
     if (verbose > 0) write(iu6, *) 'copied exp2D%abserror'

     call copy_type_matflt_type(structure_in%relerror, structure_out%relerror)
     if (verbose > 0) write(iu6, *) 'copied exp2D%relerror'

   end subroutine copy_type_exp2D

   subroutine copy_arr_type_exp2D(structure_in, structure_out)
 
     implicit none
 
     type (type_exp2D), pointer :: structure_in(:)
     type (type_exp2D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_exp2D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_exp2D'
     end if

   end subroutine copy_arr_type_exp2D

   subroutine copy_type_f_expansion(structure_in, structure_out)

     implicit none

     type (type_f_expansion), intent(in) :: structure_in
     type (type_f_expansion), intent(inout) :: structure_out

     call copy_type_complexgrid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied f_expansion%grid'

     call copy_type_complexgrid_scalar(structure_in%values, structure_out%values)
     if (verbose > 0) write(iu6, *) 'copied f_expansion%values'

     call copy_type_dist_distrivec_distfunc_fexp_param(structure_in%parameters, structure_out%parameters)
     if (verbose > 0) write(iu6, *) 'copied f_expansion%parameters'

   end subroutine copy_type_f_expansion

   subroutine copy_arr_type_f_expansion(structure_in, structure_out)
 
     implicit none
 
     type (type_f_expansion), pointer :: structure_in(:)
     type (type_f_expansion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_f_expansion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_f_expansion'
     end if

   end subroutine copy_arr_type_f_expansion

   subroutine copy_type_fast_thermal_separation_filter(structure_in, structure_out)

     implicit none

     type (type_fast_thermal_separation_filter), intent(in) :: structure_in
     type (type_fast_thermal_separation_filter), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%method, structure_out%method)
     if (verbose > 0) write(iu6, *) 'copied fast_thermal_separation_filter%method'

     call copy_type_vecflt_type(structure_in%energy_sep, structure_out%energy_sep)
     if (verbose > 0) write(iu6, *) 'copied fast_thermal_separation_filter%energy_sep'

   end subroutine copy_type_fast_thermal_separation_filter

   subroutine copy_arr_type_fast_thermal_separation_filter(structure_in, structure_out)
 
     implicit none
 
     type (type_fast_thermal_separation_filter), pointer :: structure_in(:)
     type (type_fast_thermal_separation_filter), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fast_thermal_separation_filter(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fast_thermal_separation_filter'
     end if

   end subroutine copy_arr_type_fast_thermal_separation_filter

   subroutine copy_type_filter(structure_in, structure_out)

     implicit none

     type (type_filter), intent(in) :: structure_in
     type (type_filter), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%num, structure_out%num)
     if (verbose > 0) write(iu6, *) 'copied filter%num'

     call copy_type_matflt_type(structure_in%den, structure_out%den)
     if (verbose > 0) write(iu6, *) 'copied filter%den'

   end subroutine copy_type_filter

   subroutine copy_arr_type_filter(structure_in, structure_out)
 
     implicit none
 
     type (type_filter), pointer :: structure_in(:)
     type (type_filter), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_filter(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_filter'
     end if

   end subroutine copy_arr_type_filter

   subroutine copy_type_flat_polygon(structure_in, structure_out)

     implicit none

     type (type_flat_polygon), intent(in) :: structure_in
     type (type_flat_polygon), intent(inout) :: structure_out

     call copy_type_xyz0D(structure_in%origin, structure_out%origin)
     if (verbose > 0) write(iu6, *) 'copied flat_polygon%origin'

     call copy_type_xyz0D(structure_in%basis1, structure_out%basis1)
     if (verbose > 0) write(iu6, *) 'copied flat_polygon%basis1'

     call copy_type_xyz0D(structure_in%basis2, structure_out%basis2)
     if (verbose > 0) write(iu6, *) 'copied flat_polygon%basis2'

     call copy_type_vecflt_type(structure_in%coord1, structure_out%coord1)
     if (verbose > 0) write(iu6, *) 'copied flat_polygon%coord1'

     call copy_type_vecflt_type(structure_in%coord2, structure_out%coord2)
     if (verbose > 0) write(iu6, *) 'copied flat_polygon%coord2'

   end subroutine copy_type_flat_polygon

   subroutine copy_arr_type_flat_polygon(structure_in, structure_out)
 
     implicit none
 
     type (type_flat_polygon), pointer :: structure_in(:)
     type (type_flat_polygon), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_flat_polygon(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_flat_polygon'
     end if

   end subroutine copy_arr_type_flat_polygon

   subroutine copy_type_flush(structure_in, structure_out)

     implicit none

     type (type_flush), intent(in) :: structure_in
     type (type_flush), intent(inout) :: structure_out

     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied flush%datainfo'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied flush%position'

     call copy_type_matflt_type(structure_in%coef, structure_out%coef)
     if (verbose > 0) write(iu6, *) 'copied flush%coef'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied flush%codeparam'

   end subroutine copy_type_flush

   subroutine copy_arr_type_flush(structure_in, structure_out)
 
     implicit none
 
     type (type_flush), pointer :: structure_in(:)
     type (type_flush), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_flush(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_flush'
     end if

   end subroutine copy_arr_type_flush

   subroutine copy_type_flux_loops(structure_in, structure_out)

     implicit none

     type (type_flux_loops), intent(in) :: structure_in
     type (type_flux_loops), intent(inout) :: structure_out

     call copy_type_setup_floops(structure_in%setup_floops, structure_out%setup_floops)
     if (verbose > 0) write(iu6, *) 'copied flux_loops%setup_floops'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied flux_loops%measure'

   end subroutine copy_type_flux_loops

   subroutine copy_arr_type_flux_loops(structure_in, structure_out)
 
     implicit none
 
     type (type_flux_loops), pointer :: structure_in(:)
     type (type_flux_loops), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_flux_loops(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_flux_loops'
     end if

   end subroutine copy_arr_type_flux_loops

   subroutine copy_type_fluxel(structure_in, structure_out)

     implicit none

     type (type_fluxel), intent(in) :: structure_in
     type (type_fluxel), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%flux_dv, structure_out%flux_dv)
     if (verbose > 0) write(iu6, *) 'copied fluxel%flux_dv'

     call copy_type_vecflt_type(structure_in%flux_interp, structure_out%flux_interp)
     if (verbose > 0) write(iu6, *) 'copied fluxel%flux_interp'

   end subroutine copy_type_fluxel

   subroutine copy_arr_type_fluxel(structure_in, structure_out)
 
     implicit none
 
     type (type_fluxel), pointer :: structure_in(:)
     type (type_fluxel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fluxel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fluxel'
     end if

   end subroutine copy_arr_type_fluxel

   subroutine copy_type_fluximp(structure_in, structure_out)

     implicit none

     type (type_fluximp), intent(in) :: structure_in
     type (type_fluximp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%flux_dv, structure_out%flux_dv)
     if (verbose > 0) write(iu6, *) 'copied fluximp%flux_dv'

     call copy_type_matflt_type(structure_in%flux_interp, structure_out%flux_interp)
     if (verbose > 0) write(iu6, *) 'copied fluximp%flux_interp'

   end subroutine copy_type_fluximp

   subroutine copy_arr_type_fluximp(structure_in, structure_out)
 
     implicit none
 
     type (type_fluximp), pointer :: structure_in(:)
     type (type_fluximp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fluximp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fluximp'
     end if

   end subroutine copy_arr_type_fluximp

   subroutine copy_type_fluxion(structure_in, structure_out)

     implicit none

     type (type_fluxion), intent(in) :: structure_in
     type (type_fluxion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%flux_dv, structure_out%flux_dv)
     if (verbose > 0) write(iu6, *) 'copied fluxion%flux_dv'

     call copy_type_matflt_type(structure_in%flux_interp, structure_out%flux_interp)
     if (verbose > 0) write(iu6, *) 'copied fluxion%flux_interp'

   end subroutine copy_type_fluxion

   subroutine copy_arr_type_fluxion(structure_in, structure_out)
 
     implicit none
 
     type (type_fluxion), pointer :: structure_in(:)
     type (type_fluxion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fluxion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fluxion'
     end if

   end subroutine copy_arr_type_fluxion

   subroutine copy_type_focussing(structure_in, structure_out)

     implicit none

     type (type_focussing), intent(in) :: structure_in
     type (type_focussing), intent(inout) :: structure_out

     call copy_type_float(structure_in%focal_len_hz, structure_out%focal_len_hz)
     if (verbose > 0) write(iu6, *) 'copied focussing%focal_len_hz'

     call copy_type_float(structure_in%focal_len_vc, structure_out%focal_len_vc)
     if (verbose > 0) write(iu6, *) 'copied focussing%focal_len_vc'

     call copy_type_float(structure_in%width_min_hz, structure_out%width_min_hz)
     if (verbose > 0) write(iu6, *) 'copied focussing%width_min_hz'

     call copy_type_float(structure_in%width_min_vc, structure_out%width_min_vc)
     if (verbose > 0) write(iu6, *) 'copied focussing%width_min_vc'

   end subroutine copy_type_focussing

   subroutine copy_arr_type_focussing(structure_in, structure_out)
 
     implicit none
 
     type (type_focussing), pointer :: structure_in(:)
     type (type_focussing), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_focussing(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_focussing'
     end if

   end subroutine copy_arr_type_focussing

   subroutine copy_type_fullwave(structure_in, structure_out)

     implicit none

     type (type_fullwave), intent(in) :: structure_in
     type (type_fullwave), intent(inout) :: structure_out

     call copy_type_complexgrid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied fullwave%grid'

     call copy_type_e_components(structure_in%e_components, structure_out%e_components)
     if (verbose > 0) write(iu6, *) 'copied fullwave%e_components'

     call copy_type_pol_decomp(structure_in%pol_decomp, structure_out%pol_decomp)
     if (verbose > 0) write(iu6, *) 'copied fullwave%pol_decomp'

     call copy_type_local(structure_in%local, structure_out%local)
     if (verbose > 0) write(iu6, *) 'copied fullwave%local'

   end subroutine copy_type_fullwave

   subroutine copy_arr_type_fullwave(structure_in, structure_out)
 
     implicit none
 
     type (type_fullwave), pointer :: structure_in(:)
     type (type_fullwave), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fullwave(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fullwave'
     end if

   end subroutine copy_arr_type_fullwave

   subroutine copy_type_fusiondiag_colli_3d(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_colli_3d), intent(in) :: structure_in
     type (type_fusiondiag_colli_3d), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_3d%name'

     call copy_arr_type_fusiondiag_voxels(structure_in%voxels, structure_out%voxels)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_3d%voxels'

   end subroutine copy_type_fusiondiag_colli_3d

   subroutine copy_arr_type_fusiondiag_colli_3d(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_colli_3d), pointer :: structure_in(:)
     type (type_fusiondiag_colli_3d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_colli_3d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_colli_3d'
     end if

   end subroutine copy_arr_type_fusiondiag_colli_3d

   subroutine copy_type_fusiondiag_colli_circ(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_colli_circ), intent(in) :: structure_in
     type (type_fusiondiag_colli_circ), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_circ%name'

     call copy_type_setup_line(structure_in%setup_line, structure_out%setup_line)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_circ%setup_line'

     call copy_arr_type_fusiondiag_colliunit_circ(structure_in%colliunit, structure_out%colliunit)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_circ%colliunit'

   end subroutine copy_type_fusiondiag_colli_circ

   subroutine copy_arr_type_fusiondiag_colli_circ(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_colli_circ), pointer :: structure_in(:)
     type (type_fusiondiag_colli_circ), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_colli_circ(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_colli_circ'
     end if

   end subroutine copy_arr_type_fusiondiag_colli_circ

   subroutine copy_type_fusiondiag_colli_poly(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_colli_poly), intent(in) :: structure_in
     type (type_fusiondiag_colli_poly), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_poly%name'

     call copy_type_setup_line(structure_in%setup_line, structure_out%setup_line)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_poly%setup_line'

     call copy_arr_type_fusiondiag_colliunit_poly(structure_in%colliunit, structure_out%colliunit)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colli_poly%colliunit'

   end subroutine copy_type_fusiondiag_colli_poly

   subroutine copy_arr_type_fusiondiag_colli_poly(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_colli_poly), pointer :: structure_in(:)
     type (type_fusiondiag_colli_poly), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_colli_poly(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_colli_poly'
     end if

   end subroutine copy_arr_type_fusiondiag_colli_poly

   subroutine copy_type_fusiondiag_collimator(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_collimator), intent(in) :: structure_in
     type (type_fusiondiag_collimator), intent(inout) :: structure_out

     call copy_arr_type_fusiondiag_colli_circ(structure_in%colli_circ, structure_out%colli_circ)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_collimator%colli_circ'

     call copy_arr_type_fusiondiag_colli_poly(structure_in%colli_poly, structure_out%colli_poly)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_collimator%colli_poly'

     call copy_arr_type_fusiondiag_colli_3d(structure_in%colli_3d, structure_out%colli_3d)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_collimator%colli_3d'

   end subroutine copy_type_fusiondiag_collimator

   subroutine copy_arr_type_fusiondiag_collimator(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_collimator), pointer :: structure_in(:)
     type (type_fusiondiag_collimator), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_collimator(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_collimator'
     end if

   end subroutine copy_arr_type_fusiondiag_collimator

   subroutine copy_type_fusiondiag_colliunit_circ(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_colliunit_circ), intent(in) :: structure_in
     type (type_fusiondiag_colliunit_circ), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%radius, structure_out%radius)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colliunit_circ%radius'

     call copy_type_rzphi1D(structure_in%centre, structure_out%centre)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colliunit_circ%centre'

   end subroutine copy_type_fusiondiag_colliunit_circ

   subroutine copy_arr_type_fusiondiag_colliunit_circ(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_colliunit_circ), pointer :: structure_in(:)
     type (type_fusiondiag_colliunit_circ), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_colliunit_circ(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_colliunit_circ'
     end if

   end subroutine copy_arr_type_fusiondiag_colliunit_circ

   subroutine copy_type_fusiondiag_colliunit_poly(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_colliunit_poly), intent(in) :: structure_in
     type (type_fusiondiag_colliunit_poly), intent(inout) :: structure_out

     call copy_type_float(structure_in%dimension, structure_out%dimension)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colliunit_poly%dimension'

     call copy_type_rzphi2D(structure_in%nodes, structure_out%nodes)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_colliunit_poly%nodes'

   end subroutine copy_type_fusiondiag_colliunit_poly

   subroutine copy_arr_type_fusiondiag_colliunit_poly(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_colliunit_poly), pointer :: structure_in(:)
     type (type_fusiondiag_colliunit_poly), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_colliunit_poly(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_colliunit_poly'
     end if

   end subroutine copy_arr_type_fusiondiag_colliunit_poly

   subroutine copy_type_fusiondiag_counts(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_counts), intent(in) :: structure_in
     type (type_fusiondiag_counts), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%units, structure_out%units)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_counts%units'

     call copy_arr_type_fusiondiag_ct_chords(structure_in%ct_chords, structure_out%ct_chords)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_counts%ct_chords'

     call copy_arr_type_fusiondiag_ct_energy(structure_in%ct_energy, structure_out%ct_energy)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_counts%ct_energy'

     call copy_arr_type_fusiondiag_detect_ct_energy(structure_in%detect_ct, structure_out%detect_ct)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_counts%detect_ct'

   end subroutine copy_type_fusiondiag_counts

   subroutine copy_arr_type_fusiondiag_counts(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_counts), pointer :: structure_in(:)
     type (type_fusiondiag_counts), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_counts(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_counts'
     end if

   end subroutine copy_arr_type_fusiondiag_counts

   subroutine copy_type_fusiondiag_ct_chords(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_ct_chords), intent(in) :: structure_in
     type (type_fusiondiag_ct_chords), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_ct_chords%name'

     call copy_type_exp0D(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_ct_chords%energy'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_ct_chords%measure'

   end subroutine copy_type_fusiondiag_ct_chords

   subroutine copy_arr_type_fusiondiag_ct_chords(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_ct_chords), pointer :: structure_in(:)
     type (type_fusiondiag_ct_chords), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_ct_chords(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_ct_chords'
     end if

   end subroutine copy_arr_type_fusiondiag_ct_chords

   subroutine copy_type_fusiondiag_ct_energy(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_ct_energy), intent(in) :: structure_in
     type (type_fusiondiag_ct_energy), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_ct_energy%energy'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_ct_energy%measure'

   end subroutine copy_type_fusiondiag_ct_energy

   subroutine copy_arr_type_fusiondiag_ct_energy(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_ct_energy), pointer :: structure_in(:)
     type (type_fusiondiag_ct_energy), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_ct_energy(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_ct_energy'
     end if

   end subroutine copy_arr_type_fusiondiag_ct_energy

   subroutine copy_type_fusiondiag_detect_ct_energy(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_detect_ct_energy), intent(in) :: structure_in
     type (type_fusiondiag_detect_ct_energy), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_detect_ct_energy%energy'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_detect_ct_energy%measure'

     call copy_type_diag_func(structure_in%diag_func, structure_out%diag_func)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_detect_ct_energy%diag_func'

   end subroutine copy_type_fusiondiag_detect_ct_energy

   subroutine copy_arr_type_fusiondiag_detect_ct_energy(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_detect_ct_energy), pointer :: structure_in(:)
     type (type_fusiondiag_detect_ct_energy), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_detect_ct_energy(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_detect_ct_energy'
     end if

   end subroutine copy_arr_type_fusiondiag_detect_ct_energy

   subroutine copy_type_fusiondiag_emissivity1d(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_emissivity1d), intent(in) :: structure_in
     type (type_fusiondiag_emissivity1d), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%units, structure_out%units)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity1d%units'

     call copy_type_exp1D(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity1d%r'

     call copy_type_exp1D(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity1d%z'

     call copy_arr_type_fusiondiag_spec1d(structure_in%spec1d, structure_out%spec1d)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity1d%spec1d'

   end subroutine copy_type_fusiondiag_emissivity1d

   subroutine copy_arr_type_fusiondiag_emissivity1d(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_emissivity1d), pointer :: structure_in(:)
     type (type_fusiondiag_emissivity1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_emissivity1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_emissivity1d'
     end if

   end subroutine copy_arr_type_fusiondiag_emissivity1d

   subroutine copy_type_fusiondiag_emissivity2d(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_emissivity2d), intent(in) :: structure_in
     type (type_fusiondiag_emissivity2d), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%units, structure_out%units)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity2d%units'

     call copy_type_exp2D(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity2d%r'

     call copy_type_exp2D(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity2d%z'

     call copy_arr_type_fusiondiag_spec2d(structure_in%spec2d, structure_out%spec2d)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_emissivity2d%spec2d'

   end subroutine copy_type_fusiondiag_emissivity2d

   subroutine copy_arr_type_fusiondiag_emissivity2d(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_emissivity2d), pointer :: structure_in(:)
     type (type_fusiondiag_emissivity2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_emissivity2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_emissivity2d'
     end if

   end subroutine copy_arr_type_fusiondiag_emissivity2d

   subroutine copy_type_fusiondiag_fus_product(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_fus_product), intent(in) :: structure_in
     type (type_fusiondiag_fus_product), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%product, structure_out%product)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%product'

     call copy_type_vecstring_type(structure_in%reaction, structure_out%reaction)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%reaction'

     call copy_type_fusiondiag_collimator(structure_in%collimator, structure_out%collimator)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%collimator'

     call copy_type_fusiondiag_counts(structure_in%counts, structure_out%counts)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%counts'

     call copy_type_fusiondiag_emissivity1d(structure_in%emissivity1d, structure_out%emissivity1d)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%emissivity1d'

     call copy_type_fusiondiag_emissivity2d(structure_in%emissivity2d, structure_out%emissivity2d)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%emissivity2d'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_fus_product%codeparam'

   end subroutine copy_type_fusiondiag_fus_product

   subroutine copy_arr_type_fusiondiag_fus_product(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_fus_product), pointer :: structure_in(:)
     type (type_fusiondiag_fus_product), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_fus_product(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_fus_product'
     end if

   end subroutine copy_arr_type_fusiondiag_fus_product

   subroutine copy_type_fusiondiag_spec1d(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_spec1d), intent(in) :: structure_in
     type (type_fusiondiag_spec1d), intent(inout) :: structure_out

     call copy_type_exp0D(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_spec1d%energy'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_spec1d%measure'

   end subroutine copy_type_fusiondiag_spec1d

   subroutine copy_arr_type_fusiondiag_spec1d(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_spec1d), pointer :: structure_in(:)
     type (type_fusiondiag_spec1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_spec1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_spec1d'
     end if

   end subroutine copy_arr_type_fusiondiag_spec1d

   subroutine copy_type_fusiondiag_spec2d(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_spec2d), intent(in) :: structure_in
     type (type_fusiondiag_spec2d), intent(inout) :: structure_out

     call copy_type_exp0D(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_spec2d%energy'

     call copy_type_exp2D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_spec2d%measure'

   end subroutine copy_type_fusiondiag_spec2d

   subroutine copy_arr_type_fusiondiag_spec2d(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_spec2d), pointer :: structure_in(:)
     type (type_fusiondiag_spec2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_spec2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_spec2d'
     end if

   end subroutine copy_arr_type_fusiondiag_spec2d

   subroutine copy_type_fusiondiag_voxels(structure_in, structure_out)

     implicit none

     type (type_fusiondiag_voxels), intent(in) :: structure_in
     type (type_fusiondiag_voxels), intent(inout) :: structure_out

     call copy_type_rzphi0D(structure_in%centre, structure_out%centre)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_voxels%centre'

     call copy_type_rzphi0D(structure_in%direction, structure_out%direction)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_voxels%direction'

     call copy_type_float(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_voxels%volume'

     call copy_type_float(structure_in%solid_angle, structure_out%solid_angle)
     if (verbose > 0) write(iu6, *) 'copied fusiondiag_voxels%solid_angle'

   end subroutine copy_type_fusiondiag_voxels

   subroutine copy_arr_type_fusiondiag_voxels(structure_in, structure_out)
 
     implicit none
 
     type (type_fusiondiag_voxels), pointer :: structure_in(:)
     type (type_fusiondiag_voxels), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_fusiondiag_voxels(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_fusiondiag_voxels'
     end if

   end subroutine copy_arr_type_fusiondiag_voxels

   subroutine copy_type_geom(structure_in, structure_out)

     implicit none

     type (type_geom), intent(in) :: structure_in
     type (type_geom), intent(inout) :: structure_out

     call copy_type_float(structure_in%dr_bb_sh_ib, structure_out%dr_bb_sh_ib)
     if (verbose > 0) write(iu6, *) 'copied geom%dr_bb_sh_ib'

     call copy_type_float(structure_in%dr_sh_vv_ib, structure_out%dr_sh_vv_ib)
     if (verbose > 0) write(iu6, *) 'copied geom%dr_sh_vv_ib'

     call copy_type_float(structure_in%dr_bb_sh_ob, structure_out%dr_bb_sh_ob)
     if (verbose > 0) write(iu6, *) 'copied geom%dr_bb_sh_ob'

     call copy_type_float(structure_in%dr_sh_vv_ob, structure_out%dr_sh_vv_ob)
     if (verbose > 0) write(iu6, *) 'copied geom%dr_sh_vv_ob'

     call copy_type_float(structure_in%dr_bb__sh_ib, structure_out%dr_bb__sh_ib)
     if (verbose > 0) write(iu6, *) 'copied geom%dr_bb__sh_ib'

     call copy_type_float(structure_in%dr_bb__sh_ob, structure_out%dr_bb__sh_ob)
     if (verbose > 0) write(iu6, *) 'copied geom%dr_bb__sh_ob'

     call copy_type_float(structure_in%delta_int, structure_out%delta_int)
     if (verbose > 0) write(iu6, *) 'copied geom%delta_int'

   end subroutine copy_type_geom

   subroutine copy_arr_type_geom(structure_in, structure_out)
 
     implicit none
 
     type (type_geom), pointer :: structure_in(:)
     type (type_geom), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_geom(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_geom'
     end if

   end subroutine copy_arr_type_geom

   subroutine copy_type_geom_iron(structure_in, structure_out)

     implicit none

     type (type_geom_iron), intent(in) :: structure_in
     type (type_geom_iron), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied geom_iron%npoints'

     call copy_type_rz2D(structure_in%rzcoordinate, structure_out%rzcoordinate)
     if (verbose > 0) write(iu6, *) 'copied geom_iron%rzcoordinate'

   end subroutine copy_type_geom_iron

   subroutine copy_arr_type_geom_iron(structure_in, structure_out)
 
     implicit none
 
     type (type_geom_iron), pointer :: structure_in(:)
     type (type_geom_iron), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_geom_iron(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_geom_iron'
     end if

   end subroutine copy_arr_type_geom_iron

   subroutine copy_type_global_param(structure_in, structure_out)

     implicit none

     type (type_global_param), intent(in) :: structure_in
     type (type_global_param), intent(inout) :: structure_out

     call copy_type_float(structure_in%beta_pol, structure_out%beta_pol)
     if (verbose > 0) write(iu6, *) 'copied global_param%beta_pol'

     call copy_type_float(structure_in%beta_tor, structure_out%beta_tor)
     if (verbose > 0) write(iu6, *) 'copied global_param%beta_tor'

     call copy_type_float(structure_in%beta_normal, structure_out%beta_normal)
     if (verbose > 0) write(iu6, *) 'copied global_param%beta_normal'

     call copy_type_float(structure_in%i_plasma, structure_out%i_plasma)
     if (verbose > 0) write(iu6, *) 'copied global_param%i_plasma'

     call copy_type_float(structure_in%li, structure_out%li)
     if (verbose > 0) write(iu6, *) 'copied global_param%li'

     call copy_type_float(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied global_param%volume'

     call copy_type_float(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied global_param%area'

     call copy_type_float(structure_in%psi_ax, structure_out%psi_ax)
     if (verbose > 0) write(iu6, *) 'copied global_param%psi_ax'

     call copy_type_float(structure_in%psi_bound, structure_out%psi_bound)
     if (verbose > 0) write(iu6, *) 'copied global_param%psi_bound'

     call copy_type_mag_axis(structure_in%mag_axis, structure_out%mag_axis)
     if (verbose > 0) write(iu6, *) 'copied global_param%mag_axis'

     call copy_type_float(structure_in%q_95, structure_out%q_95)
     if (verbose > 0) write(iu6, *) 'copied global_param%q_95'

     call copy_type_float(structure_in%q_min, structure_out%q_min)
     if (verbose > 0) write(iu6, *) 'copied global_param%q_min'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied global_param%toroid_field'

     call copy_type_float(structure_in%w_mhd, structure_out%w_mhd)
     if (verbose > 0) write(iu6, *) 'copied global_param%w_mhd'

     call copy_type_float(structure_in%gamma, structure_out%gamma)
     if (verbose > 0) write(iu6, *) 'copied global_param%gamma'

   end subroutine copy_type_global_param

   subroutine copy_arr_type_global_param(structure_in, structure_out)
 
     implicit none
 
     type (type_global_param), pointer :: structure_in(:)
     type (type_global_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_global_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_global_param'
     end if

   end subroutine copy_arr_type_global_param

   subroutine copy_type_globalparam(structure_in, structure_out)

     implicit none

     type (type_globalparam), intent(in) :: structure_in
     type (type_globalparam), intent(inout) :: structure_out

     call copy_type_float(structure_in%current_tot, structure_out%current_tot)
     if (verbose > 0) write(iu6, *) 'copied globalparam%current_tot'

     call copy_type_float(structure_in%current_bnd, structure_out%current_bnd)
     if (verbose > 0) write(iu6, *) 'copied globalparam%current_bnd'

     call copy_type_float(structure_in%current_ni, structure_out%current_ni)
     if (verbose > 0) write(iu6, *) 'copied globalparam%current_ni'

     call copy_type_float(structure_in%vloop, structure_out%vloop)
     if (verbose > 0) write(iu6, *) 'copied globalparam%vloop'

     call copy_type_float(structure_in%li, structure_out%li)
     if (verbose > 0) write(iu6, *) 'copied globalparam%li'

     call copy_type_float(structure_in%beta_tor, structure_out%beta_tor)
     if (verbose > 0) write(iu6, *) 'copied globalparam%beta_tor'

     call copy_type_float(structure_in%beta_normal, structure_out%beta_normal)
     if (verbose > 0) write(iu6, *) 'copied globalparam%beta_normal'

     call copy_type_float(structure_in%beta_pol, structure_out%beta_pol)
     if (verbose > 0) write(iu6, *) 'copied globalparam%beta_pol'

     call copy_type_float(structure_in%w_dia, structure_out%w_dia)
     if (verbose > 0) write(iu6, *) 'copied globalparam%w_dia'

     call copy_type_rz0D(structure_in%geom_axis, structure_out%geom_axis)
     if (verbose > 0) write(iu6, *) 'copied globalparam%geom_axis'

   end subroutine copy_type_globalparam

   subroutine copy_arr_type_globalparam(structure_in, structure_out)
 
     implicit none
 
     type (type_globalparam), pointer :: structure_in(:)
     type (type_globalparam), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_globalparam(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_globalparam'
     end if

   end subroutine copy_arr_type_globalparam

   subroutine copy_type_halpha_setup(structure_in, structure_out)

     implicit none

     type (type_halpha_setup), intent(in) :: structure_in
     type (type_halpha_setup), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied halpha_setup%name'

     call copy_type_rzphi1D(structure_in%pivot_point, structure_out%pivot_point)
     if (verbose > 0) write(iu6, *) 'copied halpha_setup%pivot_point'

     call copy_type_vecflt_type(structure_in%horchordang, structure_out%horchordang)
     if (verbose > 0) write(iu6, *) 'copied halpha_setup%horchordang'

     call copy_type_vecflt_type(structure_in%verchordang, structure_out%verchordang)
     if (verbose > 0) write(iu6, *) 'copied halpha_setup%verchordang'

     call copy_type_rzphi1D(structure_in%second_point, structure_out%second_point)
     if (verbose > 0) write(iu6, *) 'copied halpha_setup%second_point'

     call copy_type_exp1D(structure_in%solidangle, structure_out%solidangle)
     if (verbose > 0) write(iu6, *) 'copied halpha_setup%solidangle'

   end subroutine copy_type_halpha_setup

   subroutine copy_arr_type_halpha_setup(structure_in, structure_out)
 
     implicit none
 
     type (type_halpha_setup), pointer :: structure_in(:)
     type (type_halpha_setup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_halpha_setup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_halpha_setup'
     end if

   end subroutine copy_arr_type_halpha_setup

   subroutine copy_type_hcll(structure_in, structure_out)

     implicit none

     type (type_hcll), intent(in) :: structure_in
     type (type_hcll), intent(inout) :: structure_out

     call copy_type_mat_lim(structure_in%mat_lim, structure_out%mat_lim)
     if (verbose > 0) write(iu6, *) 'copied hcll%mat_lim'

     call copy_type_hcll_bb(structure_in%hcll_bb, structure_out%hcll_bb)
     if (verbose > 0) write(iu6, *) 'copied hcll%hcll_bb'

   end subroutine copy_type_hcll

   subroutine copy_arr_type_hcll(structure_in, structure_out)
 
     implicit none
 
     type (type_hcll), pointer :: structure_in(:)
     type (type_hcll), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_hcll(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_hcll'
     end if

   end subroutine copy_arr_type_hcll

   subroutine copy_type_hcll_bb(structure_in, structure_out)

     implicit none

     type (type_hcll_bb), intent(in) :: structure_in
     type (type_hcll_bb), intent(inout) :: structure_out

     call copy_type_float(structure_in%bb_lifetime, structure_out%bb_lifetime)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%bb_lifetime'

     call copy_type_float(structure_in%he_inl_t, structure_out%he_inl_t)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%he_inl_t'

     call copy_type_float(structure_in%he_fr, structure_out%he_fr)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%he_fr'

     call copy_type_float(structure_in%he_inl_p, structure_out%he_inl_p)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%he_inl_p'

     call copy_type_float(structure_in%loca_des_p, structure_out%loca_des_p)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%loca_des_p'

     call copy_type_float(structure_in%he_dp, structure_out%he_dp)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%he_dp'

     call copy_type_float(structure_in%lipb_dp, structure_out%lipb_dp)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%lipb_dp'

     call copy_type_react(structure_in%react, structure_out%react)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%react'

     call copy_type_hcllbb_specs(structure_in%inboard, structure_out%inboard)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%inboard'

     call copy_type_hcllbb_specs(structure_in%outboard, structure_out%outboard)
     if (verbose > 0) write(iu6, *) 'copied hcll_bb%outboard'

   end subroutine copy_type_hcll_bb

   subroutine copy_arr_type_hcll_bb(structure_in, structure_out)
 
     implicit none
 
     type (type_hcll_bb), pointer :: structure_in(:)
     type (type_hcll_bb), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_hcll_bb(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_hcll_bb'
     end if

   end subroutine copy_arr_type_hcll_bb

   subroutine copy_type_hcllbb_specs(structure_in, structure_out)

     implicit none

     type (type_hcllbb_specs), intent(in) :: structure_in
     type (type_hcllbb_specs), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%mass, structure_out%mass)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mass'

     call copy_type_vecflt_type(structure_in%dr, structure_out%dr)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%dr'

     call copy_type_vecflt_type(structure_in%mat, structure_out%mat)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mat'

     call copy_type_matflt_type(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%composition'

     call copy_type_bb_geometry(structure_in%mod_geom, structure_out%mod_geom)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_geom'

     call copy_type_mode_neutr(structure_in%mod_neutr, structure_out%mod_neutr)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_neutr'

     call copy_type_mode_therm(structure_in%mod_therm, structure_out%mod_therm)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_therm'

     call copy_type_mode_th_hyd(structure_in%mod_th_hyd, structure_out%mod_th_hyd)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_th_hyd'

     call copy_type_mode_mech(structure_in%mod_mech, structure_out%mod_mech)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_mech'

     call copy_type_mode_lipb(structure_in%mod_lipb, structure_out%mod_lipb)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_lipb'

     call copy_type_mode_tritium(structure_in%mod_tritium, structure_out%mod_tritium)
     if (verbose > 0) write(iu6, *) 'copied hcllbb_specs%mod_tritium'

   end subroutine copy_type_hcllbb_specs

   subroutine copy_arr_type_hcllbb_specs(structure_in, structure_out)
 
     implicit none
 
     type (type_hcllbb_specs), pointer :: structure_in(:)
     type (type_hcllbb_specs), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_hcllbb_specs(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_hcllbb_specs'
     end if

   end subroutine copy_arr_type_hcllbb_specs

   subroutine copy_type_holes(structure_in, structure_out)

     implicit none

     type (type_holes), intent(in) :: structure_in
     type (type_holes), intent(inout) :: structure_out

     call copy_type_integer(structure_in%n_holes, structure_out%n_holes)
     if (verbose > 0) write(iu6, *) 'copied holes%n_holes'

     call copy_type_coordinates(structure_in%coordinates, structure_out%coordinates)
     if (verbose > 0) write(iu6, *) 'copied holes%coordinates'

     call copy_type_width(structure_in%width, structure_out%width)
     if (verbose > 0) write(iu6, *) 'copied holes%width'

     call copy_type_vecflt_type(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied holes%eta'

   end subroutine copy_type_holes

   subroutine copy_arr_type_holes(structure_in, structure_out)
 
     implicit none
 
     type (type_holes), pointer :: structure_in(:)
     type (type_holes), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_holes(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_holes'
     end if

   end subroutine copy_arr_type_holes

   subroutine copy_type_identifier(structure_in, structure_out)

     implicit none

     type (type_identifier), intent(in) :: structure_in
     type (type_identifier), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied identifier%id'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied identifier%flag'

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied identifier%description'

   end subroutine copy_type_identifier

   subroutine copy_arr_type_identifier(structure_in, structure_out)
 
     implicit none
 
     type (type_identifier), pointer :: structure_in(:)
     type (type_identifier), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_identifier(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_identifier'
     end if

   end subroutine copy_arr_type_identifier

   subroutine copy_type_impcoeff(structure_in, structure_out)

     implicit none

     type (type_impcoeff), intent(in) :: structure_in
     type (type_impcoeff), intent(inout) :: structure_out

     call copy_arr_type_coefficients_neutrals(structure_in%chargestate, structure_out%chargestate)
     if (verbose > 0) write(iu6, *) 'copied impcoeff%chargestate'

   end subroutine copy_type_impcoeff

   subroutine copy_arr_type_impcoeff(structure_in, structure_out)
 
     implicit none
 
     type (type_impcoeff), pointer :: structure_in(:)
     type (type_impcoeff), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_impcoeff(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_impcoeff'
     end if

   end subroutine copy_arr_type_impcoeff

   subroutine copy_type_impurities(structure_in, structure_out)

     implicit none

     type (type_impurities), intent(in) :: structure_in
     type (type_impurities), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nucindex, structure_out%nucindex)
     if (verbose > 0) write(iu6, *) 'copied impurities%nucindex'

     call copy_type_integer(structure_in%i_ion, structure_out%i_ion)
     if (verbose > 0) write(iu6, *) 'copied impurities%i_ion'

     call copy_type_integer(structure_in%nzimp, structure_out%nzimp)
     if (verbose > 0) write(iu6, *) 'copied impurities%nzimp'

     call copy_type_vecflt_type(structure_in%zmin, structure_out%zmin)
     if (verbose > 0) write(iu6, *) 'copied impurities%zmin'

     call copy_type_vecflt_type(structure_in%zmax, structure_out%zmax)
     if (verbose > 0) write(iu6, *) 'copied impurities%zmax'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied impurities%label'

   end subroutine copy_type_impurities

   subroutine copy_arr_type_impurities(structure_in, structure_out)
 
     implicit none
 
     type (type_impurities), pointer :: structure_in(:)
     type (type_impurities), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_impurities(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_impurities'
     end if

   end subroutine copy_arr_type_impurities

   subroutine copy_type_impurity_type(structure_in, structure_out)

     implicit none

     type (type_impurity_type), intent(in) :: structure_in
     type (type_impurity_type), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%z'

     call copy_type_matflt_type(structure_in%zsq, structure_out%zsq)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%zsq'

     call copy_type_matflt_type(structure_in%nz, structure_out%nz)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%nz'

     call copy_type_matflt_type(structure_in%tz, structure_out%tz)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%tz'

     call copy_type_sourceimp(structure_in%source_term, structure_out%source_term)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%source_term'

     call copy_type_boundaryimp(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%boundary'

     call copy_type_coretransimp(structure_in%transp_coef, structure_out%transp_coef)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%transp_coef'

     call copy_type_fluximp(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%flux'

     call copy_type_matflt_type(structure_in%time_deriv, structure_out%time_deriv)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%time_deriv'

     call copy_type_coreimpurediag_type(structure_in%diagnostic, structure_out%diagnostic)
     if (verbose > 0) write(iu6, *) 'copied impurity_type%diagnostic'

   end subroutine copy_type_impurity_type

   subroutine copy_arr_type_impurity_type(structure_in, structure_out)
 
     implicit none
 
     type (type_impurity_type), pointer :: structure_in(:)
     type (type_impurity_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_impurity_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_impurity_type'
     end if

   end subroutine copy_arr_type_impurity_type

   subroutine copy_type_inj_spec(structure_in, structure_out)

     implicit none

     type (type_inj_spec), intent(in) :: structure_in
     type (type_inj_spec), intent(inout) :: structure_out

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied inj_spec%amn'

     call copy_type_float(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied inj_spec%zn'

   end subroutine copy_type_inj_spec

   subroutine copy_arr_type_inj_spec(structure_in, structure_out)
 
     implicit none
 
     type (type_inj_spec), pointer :: structure_in(:)
     type (type_inj_spec), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_inj_spec(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_inj_spec'
     end if

   end subroutine copy_arr_type_inj_spec

   subroutine copy_type_ions(structure_in, structure_out)

     implicit none

     type (type_ions), intent(in) :: structure_in
     type (type_ions), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nucindex, structure_out%nucindex)
     if (verbose > 0) write(iu6, *) 'copied ions%nucindex'

     call copy_type_float(structure_in%zion, structure_out%zion)
     if (verbose > 0) write(iu6, *) 'copied ions%zion'

     call copy_type_integer(structure_in%imp_flag, structure_out%imp_flag)
     if (verbose > 0) write(iu6, *) 'copied ions%imp_flag'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied ions%label'

   end subroutine copy_type_ions

   subroutine copy_arr_type_ions(structure_in, structure_out)
 
     implicit none
 
     type (type_ions), pointer :: structure_in(:)
     type (type_ions), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ions(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ions'
     end if

   end subroutine copy_arr_type_ions

   subroutine copy_type_isoflux(structure_in, structure_out)

     implicit none

     type (type_isoflux), intent(in) :: structure_in
     type (type_isoflux), intent(inout) :: structure_out

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied isoflux%position'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied isoflux%source'

     call copy_type_vecflt_type(structure_in%weight, structure_out%weight)
     if (verbose > 0) write(iu6, *) 'copied isoflux%weight'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied isoflux%sigma'

     call copy_type_vecflt_type(structure_in%calculated, structure_out%calculated)
     if (verbose > 0) write(iu6, *) 'copied isoflux%calculated'

     call copy_type_vecflt_type(structure_in%chi2, structure_out%chi2)
     if (verbose > 0) write(iu6, *) 'copied isoflux%chi2'

   end subroutine copy_type_isoflux

   subroutine copy_arr_type_isoflux(structure_in, structure_out)
 
     implicit none
 
     type (type_isoflux), pointer :: structure_in(:)
     type (type_isoflux), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_isoflux(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_isoflux'
     end if

   end subroutine copy_arr_type_isoflux

   subroutine copy_type_jni(structure_in, structure_out)

     implicit none

     type (type_jni), intent(in) :: structure_in
     type (type_jni), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied jni%value'

     call copy_type_vecflt_type(structure_in%integral, structure_out%integral)
     if (verbose > 0) write(iu6, *) 'copied jni%integral'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied jni%source'

   end subroutine copy_type_jni

   subroutine copy_arr_type_jni(structure_in, structure_out)
 
     implicit none
 
     type (type_jni), pointer :: structure_in(:)
     type (type_jni), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_jni(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_jni'
     end if

   end subroutine copy_arr_type_jni

   subroutine copy_type_lang_derived(structure_in, structure_out)

     implicit none

     type (type_lang_derived), intent(in) :: structure_in
     type (type_lang_derived), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied lang_derived%source'

     call copy_type_rzphi1Dexp(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied lang_derived%position'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied lang_derived%measure'

   end subroutine copy_type_lang_derived

   subroutine copy_arr_type_lang_derived(structure_in, structure_out)
 
     implicit none
 
     type (type_lang_derived), pointer :: structure_in(:)
     type (type_lang_derived), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_lang_derived(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_lang_derived'
     end if

   end subroutine copy_arr_type_lang_derived

   subroutine copy_type_lang_measure(structure_in, structure_out)

     implicit none

     type (type_lang_measure), intent(in) :: structure_in
     type (type_lang_measure), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied lang_measure%name'

     call copy_type_vecstring_type(structure_in%direction, structure_out%direction)
     if (verbose > 0) write(iu6, *) 'copied lang_measure%direction'

     call copy_type_exp1D(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied lang_measure%area'

     call copy_type_rzphi1Dexp(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied lang_measure%position'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied lang_measure%measure'

   end subroutine copy_type_lang_measure

   subroutine copy_arr_type_lang_measure(structure_in, structure_out)
 
     implicit none
 
     type (type_lang_measure), pointer :: structure_in(:)
     type (type_lang_measure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_lang_measure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_lang_measure'
     end if

   end subroutine copy_arr_type_lang_measure

   subroutine copy_type_launchangles(structure_in, structure_out)

     implicit none

     type (type_launchangles), intent(in) :: structure_in
     type (type_launchangles), intent(inout) :: structure_out

     call copy_type_float(structure_in%alpha, structure_out%alpha)
     if (verbose > 0) write(iu6, *) 'copied launchangles%alpha'

     call copy_type_float(structure_in%beta, structure_out%beta)
     if (verbose > 0) write(iu6, *) 'copied launchangles%beta'

   end subroutine copy_type_launchangles

   subroutine copy_arr_type_launchangles(structure_in, structure_out)
 
     implicit none
 
     type (type_launchangles), pointer :: structure_in(:)
     type (type_launchangles), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchangles(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchangles'
     end if

   end subroutine copy_arr_type_launchangles

   subroutine copy_type_launchs_parallel(structure_in, structure_out)

     implicit none

     type (type_launchs_parallel), intent(in) :: structure_in
     type (type_launchs_parallel), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%nn_par, structure_out%nn_par)
     if (verbose > 0) write(iu6, *) 'copied launchs_parallel%nn_par'

     call copy_type_matflt_type(structure_in%n_par, structure_out%n_par)
     if (verbose > 0) write(iu6, *) 'copied launchs_parallel%n_par'

     call copy_type_vecflt_type(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied launchs_parallel%power'

   end subroutine copy_type_launchs_parallel

   subroutine copy_arr_type_launchs_parallel(structure_in, structure_out)
 
     implicit none
 
     type (type_launchs_parallel), pointer :: structure_in(:)
     type (type_launchs_parallel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchs_parallel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchs_parallel'
     end if

   end subroutine copy_arr_type_launchs_parallel

   subroutine copy_type_launchs_phi_theta(structure_in, structure_out)

     implicit none

     type (type_launchs_phi_theta), intent(in) :: structure_in
     type (type_launchs_phi_theta), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%nn_phi, structure_out%nn_phi)
     if (verbose > 0) write(iu6, *) 'copied launchs_phi_theta%nn_phi'

     call copy_type_vecint_type(structure_in%nn_theta, structure_out%nn_theta)
     if (verbose > 0) write(iu6, *) 'copied launchs_phi_theta%nn_theta'

     call copy_type_matflt_type(structure_in%n_phi, structure_out%n_phi)
     if (verbose > 0) write(iu6, *) 'copied launchs_phi_theta%n_phi'

     call copy_type_matflt_type(structure_in%n_theta, structure_out%n_theta)
     if (verbose > 0) write(iu6, *) 'copied launchs_phi_theta%n_theta'

     call copy_type_array3dflt_type(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied launchs_phi_theta%power'

   end subroutine copy_type_launchs_phi_theta

   subroutine copy_arr_type_launchs_phi_theta(structure_in, structure_out)
 
     implicit none
 
     type (type_launchs_phi_theta), pointer :: structure_in(:)
     type (type_launchs_phi_theta), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchs_phi_theta(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchs_phi_theta'
     end if

   end subroutine copy_arr_type_launchs_phi_theta

   subroutine copy_type_launchs_rfbeam(structure_in, structure_out)

     implicit none

     type (type_launchs_rfbeam), intent(in) :: structure_in
     type (type_launchs_rfbeam), intent(inout) :: structure_out

     call copy_type_launchs_rfbeam_spot(structure_in%spot, structure_out%spot)
     if (verbose > 0) write(iu6, *) 'copied launchs_rfbeam%spot'

     call copy_type_launchs_rfbeam_phaseellipse(structure_in%phaseellipse, structure_out%phaseellipse)
     if (verbose > 0) write(iu6, *) 'copied launchs_rfbeam%phaseellipse'

   end subroutine copy_type_launchs_rfbeam

   subroutine copy_arr_type_launchs_rfbeam(structure_in, structure_out)
 
     implicit none
 
     type (type_launchs_rfbeam), pointer :: structure_in(:)
     type (type_launchs_rfbeam), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchs_rfbeam(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchs_rfbeam'
     end if

   end subroutine copy_arr_type_launchs_rfbeam

   subroutine copy_type_launchs_rfbeam_phaseellipse(structure_in, structure_out)

     implicit none

     type (type_launchs_rfbeam_phaseellipse), intent(in) :: structure_in
     type (type_launchs_rfbeam_phaseellipse), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%invcurvrad, structure_out%invcurvrad)
     if (verbose > 0) write(iu6, *) 'copied launchs_rfbeam_phaseellipse%invcurvrad'

     call copy_type_vecflt_type(structure_in%angle, structure_out%angle)
     if (verbose > 0) write(iu6, *) 'copied launchs_rfbeam_phaseellipse%angle'

   end subroutine copy_type_launchs_rfbeam_phaseellipse

   subroutine copy_arr_type_launchs_rfbeam_phaseellipse(structure_in, structure_out)
 
     implicit none
 
     type (type_launchs_rfbeam_phaseellipse), pointer :: structure_in(:)
     type (type_launchs_rfbeam_phaseellipse), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchs_rfbeam_phaseellipse(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchs_rfbeam_phaseellipse'
     end if

   end subroutine copy_arr_type_launchs_rfbeam_phaseellipse

   subroutine copy_type_launchs_rfbeam_spot(structure_in, structure_out)

     implicit none

     type (type_launchs_rfbeam_spot), intent(in) :: structure_in
     type (type_launchs_rfbeam_spot), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%waist, structure_out%waist)
     if (verbose > 0) write(iu6, *) 'copied launchs_rfbeam_spot%waist'

     call copy_type_vecflt_type(structure_in%angle, structure_out%angle)
     if (verbose > 0) write(iu6, *) 'copied launchs_rfbeam_spot%angle'

   end subroutine copy_type_launchs_rfbeam_spot

   subroutine copy_arr_type_launchs_rfbeam_spot(structure_in, structure_out)
 
     implicit none
 
     type (type_launchs_rfbeam_spot), pointer :: structure_in(:)
     type (type_launchs_rfbeam_spot), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchs_rfbeam_spot(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchs_rfbeam_spot'
     end if

   end subroutine copy_arr_type_launchs_rfbeam_spot

   subroutine copy_type_launchsignal(structure_in, structure_out)

     implicit none

     type (type_launchsignal), intent(in) :: structure_in
     type (type_launchsignal), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%time_launch, structure_out%time_launch)
     if (verbose > 0) write(iu6, *) 'copied launchsignal%time_launch'

     call copy_type_vecflt_type(structure_in%freq, structure_out%freq)
     if (verbose > 0) write(iu6, *) 'copied launchsignal%freq'

     call copy_type_vecflt_type(structure_in%amplitude, structure_out%amplitude)
     if (verbose > 0) write(iu6, *) 'copied launchsignal%amplitude'

     call copy_type_vecflt_type(structure_in%phase, structure_out%phase)
     if (verbose > 0) write(iu6, *) 'copied launchsignal%phase'

   end subroutine copy_type_launchsignal

   subroutine copy_arr_type_launchsignal(structure_in, structure_out)
 
     implicit none
 
     type (type_launchsignal), pointer :: structure_in(:)
     type (type_launchsignal), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_launchsignal(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_launchsignal'
     end if

   end subroutine copy_arr_type_launchsignal

   subroutine copy_type_limiter_unit(structure_in, structure_out)

     implicit none

     type (type_limiter_unit), intent(in) :: structure_in
     type (type_limiter_unit), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied limiter_unit%name'

     call copy_type_vecstring_type(structure_in%closed, structure_out%closed)
     if (verbose > 0) write(iu6, *) 'copied limiter_unit%closed'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied limiter_unit%position'

     call copy_type_float(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied limiter_unit%eta'

     call copy_type_float(structure_in%delta, structure_out%delta)
     if (verbose > 0) write(iu6, *) 'copied limiter_unit%delta'

     call copy_type_float(structure_in%permeability, structure_out%permeability)
     if (verbose > 0) write(iu6, *) 'copied limiter_unit%permeability'

   end subroutine copy_type_limiter_unit

   subroutine copy_arr_type_limiter_unit(structure_in, structure_out)
 
     implicit none
 
     type (type_limiter_unit), pointer :: structure_in(:)
     type (type_limiter_unit), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_limiter_unit(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_limiter_unit'
     end if

   end subroutine copy_arr_type_limiter_unit

   subroutine copy_type_limits(structure_in, structure_out)

     implicit none

     type (type_limits), intent(in) :: structure_in
     type (type_limits), intent(inout) :: structure_out

     call copy_type_float(structure_in%fw_dpa, structure_out%fw_dpa)
     if (verbose > 0) write(iu6, *) 'copied limits%fw_dpa'

     call copy_type_float(structure_in%he_appm, structure_out%he_appm)
     if (verbose > 0) write(iu6, *) 'copied limits%he_appm'

     call copy_type_float(structure_in%ins_dose, structure_out%ins_dose)
     if (verbose > 0) write(iu6, *) 'copied limits%ins_dose'

     call copy_type_float(structure_in%fn_flu, structure_out%fn_flu)
     if (verbose > 0) write(iu6, *) 'copied limits%fn_flu'

     call copy_type_float(structure_in%dpa_cu, structure_out%dpa_cu)
     if (verbose > 0) write(iu6, *) 'copied limits%dpa_cu'

     call copy_type_float(structure_in%wp_nh, structure_out%wp_nh)
     if (verbose > 0) write(iu6, *) 'copied limits%wp_nh'

   end subroutine copy_type_limits

   subroutine copy_arr_type_limits(structure_in, structure_out)
 
     implicit none
 
     type (type_limits), pointer :: structure_in(:)
     type (type_limits), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_limits(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_limits'
     end if

   end subroutine copy_arr_type_limits

   subroutine copy_type_lineintegraldiag(structure_in, structure_out)

     implicit none

     type (type_lineintegraldiag), intent(in) :: structure_in
     type (type_lineintegraldiag), intent(inout) :: structure_out

     call copy_type_datainfo(structure_in%datainfo, structure_out%datainfo)
     if (verbose > 0) write(iu6, *) 'copied lineintegraldiag%datainfo'

     call copy_type_vecstring_type(structure_in%expression, structure_out%expression)
     if (verbose > 0) write(iu6, *) 'copied lineintegraldiag%expression'

     call copy_type_setup_line(structure_in%setup_line, structure_out%setup_line)
     if (verbose > 0) write(iu6, *) 'copied lineintegraldiag%setup_line'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied lineintegraldiag%measure'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied lineintegraldiag%codeparam'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied lineintegraldiag%time'

   end subroutine copy_type_lineintegraldiag

   subroutine copy_arr_type_lineintegraldiag(structure_in, structure_out)
 
     implicit none
 
     type (type_lineintegraldiag), pointer :: structure_in(:)
     type (type_lineintegraldiag), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_lineintegraldiag(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_lineintegraldiag'
     end if

   end subroutine copy_arr_type_lineintegraldiag

   subroutine copy_type_lithmeasure(structure_in, structure_out)

     implicit none

     type (type_lithmeasure), intent(in) :: structure_in
     type (type_lithmeasure), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied lithmeasure%ne'

   end subroutine copy_type_lithmeasure

   subroutine copy_arr_type_lithmeasure(structure_in, structure_out)
 
     implicit none
 
     type (type_lithmeasure), pointer :: structure_in(:)
     type (type_lithmeasure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_lithmeasure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_lithmeasure'
     end if

   end subroutine copy_arr_type_lithmeasure

   subroutine copy_type_lithsetup(structure_in, structure_out)

     implicit none

     type (type_lithsetup), intent(in) :: structure_in
     type (type_lithsetup), intent(inout) :: structure_out

     call copy_type_rzphi1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied lithsetup%position'

   end subroutine copy_type_lithsetup

   subroutine copy_arr_type_lithsetup(structure_in, structure_out)
 
     implicit none
 
     type (type_lithsetup), pointer :: structure_in(:)
     type (type_lithsetup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_lithsetup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_lithsetup'
     end if

   end subroutine copy_arr_type_lithsetup

   subroutine copy_type_local(structure_in, structure_out)

     implicit none

     type (type_local), intent(in) :: structure_in
     type (type_local), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%e_plus, structure_out%e_plus)
     if (verbose > 0) write(iu6, *) 'copied local%e_plus'

     call copy_type_array3dflt_type(structure_in%e_plus_ph, structure_out%e_plus_ph)
     if (verbose > 0) write(iu6, *) 'copied local%e_plus_ph'

     call copy_type_array3dflt_type(structure_in%e_minus, structure_out%e_minus)
     if (verbose > 0) write(iu6, *) 'copied local%e_minus'

     call copy_type_array3dflt_type(structure_in%e_minus_ph, structure_out%e_minus_ph)
     if (verbose > 0) write(iu6, *) 'copied local%e_minus_ph'

     call copy_type_array3dint_type(structure_in%e_norm, structure_out%e_norm)
     if (verbose > 0) write(iu6, *) 'copied local%e_norm'

     call copy_type_array3dflt_type(structure_in%enorm_ph, structure_out%enorm_ph)
     if (verbose > 0) write(iu6, *) 'copied local%enorm_ph'

     call copy_type_array3dflt_type(structure_in%e_binorm, structure_out%e_binorm)
     if (verbose > 0) write(iu6, *) 'copied local%e_binorm'

     call copy_type_array3dflt_type(structure_in%e_binorm_ph, structure_out%e_binorm_ph)
     if (verbose > 0) write(iu6, *) 'copied local%e_binorm_ph'

     call copy_type_array3dflt_type(structure_in%e_para, structure_out%e_para)
     if (verbose > 0) write(iu6, *) 'copied local%e_para'

     call copy_type_array3dflt_type(structure_in%e_para_ph, structure_out%e_para_ph)
     if (verbose > 0) write(iu6, *) 'copied local%e_para_ph'

     call copy_type_array3dflt_type(structure_in%b_norm, structure_out%b_norm)
     if (verbose > 0) write(iu6, *) 'copied local%b_norm'

     call copy_type_array3dflt_type(structure_in%b_norm_ph, structure_out%b_norm_ph)
     if (verbose > 0) write(iu6, *) 'copied local%b_norm_ph'

     call copy_type_array3dflt_type(structure_in%b_binorm, structure_out%b_binorm)
     if (verbose > 0) write(iu6, *) 'copied local%b_binorm'

     call copy_type_array3dflt_type(structure_in%b_binorm_ph, structure_out%b_binorm_ph)
     if (verbose > 0) write(iu6, *) 'copied local%b_binorm_ph'

     call copy_type_array3dflt_type(structure_in%b_para, structure_out%b_para)
     if (verbose > 0) write(iu6, *) 'copied local%b_para'

     call copy_type_array3dflt_type(structure_in%b_para_ph, structure_out%b_para_ph)
     if (verbose > 0) write(iu6, *) 'copied local%b_para_ph'

     call copy_type_array3dflt_type(structure_in%k_perp, structure_out%k_perp)
     if (verbose > 0) write(iu6, *) 'copied local%k_perp'

   end subroutine copy_type_local

   subroutine copy_arr_type_local(structure_in, structure_out)
 
     implicit none
 
     type (type_local), pointer :: structure_in(:)
     type (type_local), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_local(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_local'
     end if

   end subroutine copy_arr_type_local

   subroutine copy_type_mag_axis(structure_in, structure_out)

     implicit none

     type (type_mag_axis), intent(in) :: structure_in
     type (type_mag_axis), intent(inout) :: structure_out

     call copy_type_rz0D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied mag_axis%position'

     call copy_type_float(structure_in%bphi, structure_out%bphi)
     if (verbose > 0) write(iu6, *) 'copied mag_axis%bphi'

     call copy_type_float(structure_in%q, structure_out%q)
     if (verbose > 0) write(iu6, *) 'copied mag_axis%q'

   end subroutine copy_type_mag_axis

   subroutine copy_arr_type_mag_axis(structure_in, structure_out)
 
     implicit none
 
     type (type_mag_axis), pointer :: structure_in(:)
     type (type_mag_axis), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mag_axis(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mag_axis'
     end if

   end subroutine copy_arr_type_mag_axis

   subroutine copy_type_magnet_iron(structure_in, structure_out)

     implicit none

     type (type_magnet_iron), intent(in) :: structure_in
     type (type_magnet_iron), intent(inout) :: structure_out

     call copy_type_eqmes1D(structure_in%mr, structure_out%mr)
     if (verbose > 0) write(iu6, *) 'copied magnet_iron%mr'

     call copy_type_eqmes1D(structure_in%mz, structure_out%mz)
     if (verbose > 0) write(iu6, *) 'copied magnet_iron%mz'

   end subroutine copy_type_magnet_iron

   subroutine copy_arr_type_magnet_iron(structure_in, structure_out)
 
     implicit none
 
     type (type_magnet_iron), pointer :: structure_in(:)
     type (type_magnet_iron), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_magnet_iron(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_magnet_iron'
     end if

   end subroutine copy_arr_type_magnet_iron

   subroutine copy_type_magnetise(structure_in, structure_out)

     implicit none

     type (type_magnetise), intent(in) :: structure_in
     type (type_magnetise), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%mr, structure_out%mr)
     if (verbose > 0) write(iu6, *) 'copied magnetise%mr'

     call copy_type_exp1D(structure_in%mz, structure_out%mz)
     if (verbose > 0) write(iu6, *) 'copied magnetise%mz'

   end subroutine copy_type_magnetise

   subroutine copy_arr_type_magnetise(structure_in, structure_out)
 
     implicit none
 
     type (type_magnetise), pointer :: structure_in(:)
     type (type_magnetise), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_magnetise(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_magnetise'
     end if

   end subroutine copy_arr_type_magnetise

   subroutine copy_type_mat_lim(structure_in, structure_out)

     implicit none

     type (type_mat_lim), intent(in) :: structure_in
     type (type_mat_lim), intent(inout) :: structure_out

     call copy_type_float(structure_in%cool_t_lim, structure_out%cool_t_lim)
     if (verbose > 0) write(iu6, *) 'copied mat_lim%cool_t_lim'

     call copy_type_float(structure_in%steel_t_lim, structure_out%steel_t_lim)
     if (verbose > 0) write(iu6, *) 'copied mat_lim%steel_t_lim'

     call copy_type_float(structure_in%lipb_t_lim, structure_out%lipb_t_lim)
     if (verbose > 0) write(iu6, *) 'copied mat_lim%lipb_t_lim'

   end subroutine copy_type_mat_lim

   subroutine copy_arr_type_mat_lim(structure_in, structure_out)
 
     implicit none
 
     type (type_mat_lim), pointer :: structure_in(:)
     type (type_mat_lim), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mat_lim(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mat_lim'
     end if

   end subroutine copy_arr_type_mat_lim

   subroutine copy_type_mdinfo(structure_in, structure_out)

     implicit none

     type (type_mdinfo), intent(in) :: structure_in
     type (type_mdinfo), intent(inout) :: structure_out

     call copy_type_integer(structure_in%shot_min, structure_out%shot_min)
     if (verbose > 0) write(iu6, *) 'copied mdinfo%shot_min'

     call copy_type_integer(structure_in%shot_max, structure_out%shot_max)
     if (verbose > 0) write(iu6, *) 'copied mdinfo%shot_max'

     call copy_type_entry_def(structure_in%md_entry, structure_out%md_entry)
     if (verbose > 0) write(iu6, *) 'copied mdinfo%md_entry'

   end subroutine copy_type_mdinfo

   subroutine copy_arr_type_mdinfo(structure_in, structure_out)
 
     implicit none
 
     type (type_mdinfo), pointer :: structure_in(:)
     type (type_mdinfo), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mdinfo(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mdinfo'
     end if

   end subroutine copy_arr_type_mdinfo

   subroutine copy_type_mhd_ideal_wall2d(structure_in, structure_out)

     implicit none

     type (type_mhd_ideal_wall2d), intent(in) :: structure_in
     type (type_mhd_ideal_wall2d), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%walltype, structure_out%walltype)
     if (verbose > 0) write(iu6, *) 'copied mhd_ideal_wall2d%walltype'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied mhd_ideal_wall2d%position'

   end subroutine copy_type_mhd_ideal_wall2d

   subroutine copy_arr_type_mhd_ideal_wall2d(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd_ideal_wall2d), pointer :: structure_in(:)
     type (type_mhd_ideal_wall2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd_ideal_wall2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd_ideal_wall2d'
     end if

   end subroutine copy_arr_type_mhd_ideal_wall2d

   subroutine copy_type_mhd_mode(structure_in, structure_out)

     implicit none

     type (type_mhd_mode), intent(in) :: structure_in
     type (type_mhd_mode), intent(inout) :: structure_out

     call copy_type_integer(structure_in%modenum, structure_out%modenum)
     if (verbose > 0) write(iu6, *) 'copied mhd_mode%modenum'

     call copy_type_float(structure_in%growthrate, structure_out%growthrate)
     if (verbose > 0) write(iu6, *) 'copied mhd_mode%growthrate'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied mhd_mode%frequency'

     call copy_type_mhd_plasma(structure_in%plasma, structure_out%plasma)
     if (verbose > 0) write(iu6, *) 'copied mhd_mode%plasma'

     call copy_type_mhd_vacuum(structure_in%vacuum, structure_out%vacuum)
     if (verbose > 0) write(iu6, *) 'copied mhd_mode%vacuum'

   end subroutine copy_type_mhd_mode

   subroutine copy_arr_type_mhd_mode(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd_mode), pointer :: structure_in(:)
     type (type_mhd_mode), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd_mode(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd_mode'
     end if

   end subroutine copy_arr_type_mhd_mode

   subroutine copy_type_mhd_plasma(structure_in, structure_out)

     implicit none

     type (type_mhd_plasma), intent(in) :: structure_in
     type (type_mhd_plasma), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%psi'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%rho_tor'

     call copy_type_matflt_type(structure_in%m, structure_out%m)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%m'

     call copy_type_matcplx_type(structure_in%disp_perp, structure_out%disp_perp)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%disp_perp'

     call copy_type_matcplx_type(structure_in%disp_par, structure_out%disp_par)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%disp_par'

     call copy_type_vecflt_type(structure_in%tau_alfven, structure_out%tau_alfven)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%tau_alfven'

     call copy_type_vecflt_type(structure_in%tau_res, structure_out%tau_res)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%tau_res'

     call copy_type_coord_sys(structure_in%coord_sys, structure_out%coord_sys)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%coord_sys'

     call copy_type_mhd_vector(structure_in%a_pert, structure_out%a_pert)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%a_pert'

     call copy_type_mhd_vector(structure_in%b_pert, structure_out%b_pert)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%b_pert'

     call copy_type_mhd_vector(structure_in%v_pert, structure_out%v_pert)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%v_pert'

     call copy_type_matcplx_type(structure_in%p_pert, structure_out%p_pert)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%p_pert'

     call copy_type_matcplx_type(structure_in%rho_mass_per, structure_out%rho_mass_per)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%rho_mass_per'

     call copy_type_matcplx_type(structure_in%temp_per, structure_out%temp_per)
     if (verbose > 0) write(iu6, *) 'copied mhd_plasma%temp_per'

   end subroutine copy_type_mhd_plasma

   subroutine copy_arr_type_mhd_plasma(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd_plasma), pointer :: structure_in(:)
     type (type_mhd_plasma), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd_plasma(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd_plasma'
     end if

   end subroutine copy_arr_type_mhd_plasma

   subroutine copy_type_mhd_res_wall2d(structure_in, structure_out)

     implicit none

     type (type_mhd_res_wall2d), intent(in) :: structure_in
     type (type_mhd_res_wall2d), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%walltype, structure_out%walltype)
     if (verbose > 0) write(iu6, *) 'copied mhd_res_wall2d%walltype'

     call copy_type_float(structure_in%delta, structure_out%delta)
     if (verbose > 0) write(iu6, *) 'copied mhd_res_wall2d%delta'

     call copy_type_float(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied mhd_res_wall2d%eta'

     call copy_type_integer(structure_in%npoloidal, structure_out%npoloidal)
     if (verbose > 0) write(iu6, *) 'copied mhd_res_wall2d%npoloidal'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied mhd_res_wall2d%position'

     call copy_type_holes(structure_in%holes, structure_out%holes)
     if (verbose > 0) write(iu6, *) 'copied mhd_res_wall2d%holes'

   end subroutine copy_type_mhd_res_wall2d

   subroutine copy_arr_type_mhd_res_wall2d(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd_res_wall2d), pointer :: structure_in(:)
     type (type_mhd_res_wall2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd_res_wall2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd_res_wall2d'
     end if

   end subroutine copy_arr_type_mhd_res_wall2d

   subroutine copy_type_mhd_vacuum(structure_in, structure_out)

     implicit none

     type (type_mhd_vacuum), intent(in) :: structure_in
     type (type_mhd_vacuum), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%m, structure_out%m)
     if (verbose > 0) write(iu6, *) 'copied mhd_vacuum%m'

     call copy_type_coord_sys(structure_in%coord_sys, structure_out%coord_sys)
     if (verbose > 0) write(iu6, *) 'copied mhd_vacuum%coord_sys'

     call copy_type_mhd_vector(structure_in%a_pert, structure_out%a_pert)
     if (verbose > 0) write(iu6, *) 'copied mhd_vacuum%a_pert'

     call copy_type_mhd_vector(structure_in%b_pert, structure_out%b_pert)
     if (verbose > 0) write(iu6, *) 'copied mhd_vacuum%b_pert'

   end subroutine copy_type_mhd_vacuum

   subroutine copy_arr_type_mhd_vacuum(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd_vacuum), pointer :: structure_in(:)
     type (type_mhd_vacuum), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd_vacuum(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd_vacuum'
     end if

   end subroutine copy_arr_type_mhd_vacuum

   subroutine copy_type_mhd_vector(structure_in, structure_out)

     implicit none

     type (type_mhd_vector), intent(in) :: structure_in
     type (type_mhd_vector), intent(inout) :: structure_out

     call copy_type_matcplx_type(structure_in%coord1, structure_out%coord1)
     if (verbose > 0) write(iu6, *) 'copied mhd_vector%coord1'

     call copy_type_matcplx_type(structure_in%coord2, structure_out%coord2)
     if (verbose > 0) write(iu6, *) 'copied mhd_vector%coord2'

     call copy_type_matcplx_type(structure_in%coord3, structure_out%coord3)
     if (verbose > 0) write(iu6, *) 'copied mhd_vector%coord3'

   end subroutine copy_type_mhd_vector

   subroutine copy_arr_type_mhd_vector(structure_in, structure_out)
 
     implicit none
 
     type (type_mhd_vector), pointer :: structure_in(:)
     type (type_mhd_vector), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mhd_vector(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mhd_vector'
     end if

   end subroutine copy_arr_type_mhd_vector

   subroutine copy_type_mode_lipb(structure_in, structure_out)

     implicit none

     type (type_mode_lipb), intent(in) :: structure_in
     type (type_mode_lipb), intent(inout) :: structure_out

     call copy_type_float(structure_in%lp_rec_day, structure_out%lp_rec_day)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%lp_rec_day'

     call copy_type_vecflt_type(structure_in%bb_lp_fr, structure_out%bb_lp_fr)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%bb_lp_fr'

     call copy_type_float(structure_in%lp_inl_p, structure_out%lp_inl_p)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%lp_inl_p'

     call copy_type_float(structure_in%bu_dp_lp, structure_out%bu_dp_lp)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%bu_dp_lp'

     call copy_type_float(structure_in%man_dp_lp, structure_out%man_dp_lp)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%man_dp_lp'

     call copy_type_float(structure_in%tot_dp_lp, structure_out%tot_dp_lp)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%tot_dp_lp'

     call copy_type_float(structure_in%bu_lp_ave_t, structure_out%bu_lp_ave_t)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%bu_lp_ave_t'

     call copy_type_float(structure_in%bu_lp_max_t, structure_out%bu_lp_max_t)
     if (verbose > 0) write(iu6, *) 'copied mode_lipb%bu_lp_max_t'

   end subroutine copy_type_mode_lipb

   subroutine copy_arr_type_mode_lipb(structure_in, structure_out)
 
     implicit none
 
     type (type_mode_lipb), pointer :: structure_in(:)
     type (type_mode_lipb), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mode_lipb(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mode_lipb'
     end if

   end subroutine copy_arr_type_mode_lipb

   subroutine copy_type_mode_mech(structure_in, structure_out)

     implicit none

     type (type_mode_mech), intent(in) :: structure_in
     type (type_mode_mech), intent(inout) :: structure_out

     call copy_type_float(structure_in%fw_min_ts_mg, structure_out%fw_min_ts_mg)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%fw_min_ts_mg'

     call copy_type_float(structure_in%fw_min_bd_mg, structure_out%fw_min_bd_mg)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%fw_min_bd_mg'

     call copy_type_float(structure_in%sg_min_ts_mg, structure_out%sg_min_ts_mg)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%sg_min_ts_mg'

     call copy_type_float(structure_in%sg_min_bd_mg, structure_out%sg_min_bd_mg)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%sg_min_bd_mg'

     call copy_type_float(structure_in%cp_min_ts_mg, structure_out%cp_min_ts_mg)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%cp_min_ts_mg'

     call copy_type_float(structure_in%cp_min_bd_mg, structure_out%cp_min_bd_mg)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%cp_min_bd_mg'

     call copy_type_float(structure_in%min_ts_mg_ac, structure_out%min_ts_mg_ac)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%min_ts_mg_ac'

     call copy_type_float(structure_in%min_bd_mg_ac, structure_out%min_bd_mg_ac)
     if (verbose > 0) write(iu6, *) 'copied mode_mech%min_bd_mg_ac'

   end subroutine copy_type_mode_mech

   subroutine copy_arr_type_mode_mech(structure_in, structure_out)
 
     implicit none
 
     type (type_mode_mech), pointer :: structure_in(:)
     type (type_mode_mech), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mode_mech(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mode_mech'
     end if

   end subroutine copy_arr_type_mode_mech

   subroutine copy_type_mode_neutr(structure_in, structure_out)

     implicit none

     type (type_mode_neutr), intent(in) :: structure_in
     type (type_mode_neutr), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied mode_neutr%r'

     call copy_type_vecflt_type(structure_in%pd_rad, structure_out%pd_rad)
     if (verbose > 0) write(iu6, *) 'copied mode_neutr%pd_rad'

     call copy_type_vecflt_type(structure_in%lipb_coef_pd, structure_out%lipb_coef_pd)
     if (verbose > 0) write(iu6, *) 'copied mode_neutr%lipb_coef_pd'

     call copy_type_vecflt_type(structure_in%steel_coef_pd, structure_out%steel_coef_pd)
     if (verbose > 0) write(iu6, *) 'copied mode_neutr%steel_coef_pd'

     call copy_type_power_exchange(structure_in%pow_exchange, structure_out%pow_exchange)
     if (verbose > 0) write(iu6, *) 'copied mode_neutr%pow_exchange'

   end subroutine copy_type_mode_neutr

   subroutine copy_arr_type_mode_neutr(structure_in, structure_out)
 
     implicit none
 
     type (type_mode_neutr), pointer :: structure_in(:)
     type (type_mode_neutr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mode_neutr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mode_neutr'
     end if

   end subroutine copy_arr_type_mode_neutr

   subroutine copy_type_mode_th_hyd(structure_in, structure_out)

     implicit none

     type (type_mode_th_hyd), intent(in) :: structure_in
     type (type_mode_th_hyd), intent(inout) :: structure_out

     call copy_type_float(structure_in%fw_dp_he, structure_out%fw_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%fw_dp_he'

     call copy_type_float(structure_in%sg_dp_he, structure_out%sg_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%sg_dp_he'

     call copy_type_float(structure_in%cp_dp_he, structure_out%cp_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%cp_dp_he'

     call copy_type_float(structure_in%man_dp_he, structure_out%man_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%man_dp_he'

     call copy_type_float(structure_in%tot_dp_he, structure_out%tot_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%tot_dp_he'

     call copy_type_float(structure_in%bp_dp_he, structure_out%bp_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%bp_dp_he'

     call copy_type_float(structure_in%circ_dp_he, structure_out%circ_dp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_th_hyd%circ_dp_he'

   end subroutine copy_type_mode_th_hyd

   subroutine copy_arr_type_mode_th_hyd(structure_in, structure_out)
 
     implicit none
 
     type (type_mode_th_hyd), pointer :: structure_in(:)
     type (type_mode_th_hyd), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mode_th_hyd(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mode_th_hyd'
     end if

   end subroutine copy_arr_type_mode_th_hyd

   subroutine copy_type_mode_therm(structure_in, structure_out)

     implicit none

     type (type_mode_therm), intent(in) :: structure_in
     type (type_mode_therm), intent(inout) :: structure_out

     call copy_type_float(structure_in%he_fr, structure_out%he_fr)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%he_fr'

     call copy_type_float(structure_in%perc_bp_he, structure_out%perc_bp_he)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%perc_bp_he'

     call copy_type_float(structure_in%he_out_t, structure_out%he_out_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%he_out_t'

     call copy_type_float(structure_in%fw_he_out_t, structure_out%fw_he_out_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%fw_he_out_t'

     call copy_type_float(structure_in%sg_he_out_t, structure_out%sg_he_out_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%sg_he_out_t'

     call copy_type_float(structure_in%cp_he_out_t, structure_out%cp_he_out_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%cp_he_out_t'

     call copy_type_float(structure_in%fw_st_max_t, structure_out%fw_st_max_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%fw_st_max_t'

     call copy_type_float(structure_in%sg_st_max_t, structure_out%sg_st_max_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%sg_st_max_t'

     call copy_type_float(structure_in%cp_st_max_t, structure_out%cp_st_max_t)
     if (verbose > 0) write(iu6, *) 'copied mode_therm%cp_st_max_t'

   end subroutine copy_type_mode_therm

   subroutine copy_arr_type_mode_therm(structure_in, structure_out)
 
     implicit none
 
     type (type_mode_therm), pointer :: structure_in(:)
     type (type_mode_therm), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mode_therm(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mode_therm'
     end if

   end subroutine copy_arr_type_mode_therm

   subroutine copy_type_mode_tritium(structure_in, structure_out)

     implicit none

     type (type_mode_tritium), intent(in) :: structure_in
     type (type_mode_tritium), intent(inout) :: structure_out

     call copy_type_float(structure_in%t_conc_lipb, structure_out%t_conc_lipb)
     if (verbose > 0) write(iu6, *) 'copied mode_tritium%t_conc_lipb'

     call copy_type_float(structure_in%t_conc_he, structure_out%t_conc_he)
     if (verbose > 0) write(iu6, *) 'copied mode_tritium%t_conc_he'

   end subroutine copy_type_mode_tritium

   subroutine copy_arr_type_mode_tritium(structure_in, structure_out)
 
     implicit none
 
     type (type_mode_tritium), pointer :: structure_in(:)
     type (type_mode_tritium), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_mode_tritium(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_mode_tritium'
     end if

   end subroutine copy_arr_type_mode_tritium

   subroutine copy_type_modules(structure_in, structure_out)

     implicit none

     type (type_modules), intent(in) :: structure_in
     type (type_modules), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nma_theta, structure_out%nma_theta)
     if (verbose > 0) write(iu6, *) 'copied modules%nma_theta'

     call copy_type_integer(structure_in%nma_phi, structure_out%nma_phi)
     if (verbose > 0) write(iu6, *) 'copied modules%nma_phi'

     call copy_type_vecint_type(structure_in%ima_theta, structure_out%ima_theta)
     if (verbose > 0) write(iu6, *) 'copied modules%ima_theta'

     call copy_type_vecint_type(structure_in%ima_phi, structure_out%ima_phi)
     if (verbose > 0) write(iu6, *) 'copied modules%ima_phi'

     call copy_type_float(structure_in%sm_theta, structure_out%sm_theta)
     if (verbose > 0) write(iu6, *) 'copied modules%sm_theta'

     call copy_type_exp1D(structure_in%amplitude, structure_out%amplitude)
     if (verbose > 0) write(iu6, *) 'copied modules%amplitude'

     call copy_type_exp1D(structure_in%phase, structure_out%phase)
     if (verbose > 0) write(iu6, *) 'copied modules%phase'

     call copy_type_waveguides(structure_in%waveguides, structure_out%waveguides)
     if (verbose > 0) write(iu6, *) 'copied modules%waveguides'

   end subroutine copy_type_modules

   subroutine copy_arr_type_modules(structure_in, structure_out)
 
     implicit none
 
     type (type_modules), pointer :: structure_in(:)
     type (type_modules), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_modules(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_modules'
     end if

   end subroutine copy_arr_type_modules

   subroutine copy_type_msediag_emiss_chord(structure_in, structure_out)

     implicit none

     type (type_msediag_emiss_chord), intent(in) :: structure_in
     type (type_msediag_emiss_chord), intent(inout) :: structure_out

     call copy_type_float(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied msediag_emiss_chord%volume'

     call copy_type_rzphi1D(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied msediag_emiss_chord%setup'

     call copy_arr_type_msediag_polarization(structure_in%polarization, structure_out%polarization)
     if (verbose > 0) write(iu6, *) 'copied msediag_emiss_chord%polarization'

     call copy_type_vecflt_type(structure_in%quantiaxis, structure_out%quantiaxis)
     if (verbose > 0) write(iu6, *) 'copied msediag_emiss_chord%quantiaxis'

   end subroutine copy_type_msediag_emiss_chord

   subroutine copy_arr_type_msediag_emiss_chord(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_emiss_chord), pointer :: structure_in(:)
     type (type_msediag_emiss_chord), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_emiss_chord(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_emiss_chord'
     end if

   end subroutine copy_arr_type_msediag_emiss_chord

   subroutine copy_type_msediag_emissivity(structure_in, structure_out)

     implicit none

     type (type_msediag_emissivity), intent(in) :: structure_in
     type (type_msediag_emissivity), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%wavelength, structure_out%wavelength)
     if (verbose > 0) write(iu6, *) 'copied msediag_emissivity%wavelength'

     call copy_arr_type_msediag_emiss_chord(structure_in%emiss_chord, structure_out%emiss_chord)
     if (verbose > 0) write(iu6, *) 'copied msediag_emissivity%emiss_chord'

   end subroutine copy_type_msediag_emissivity

   subroutine copy_arr_type_msediag_emissivity(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_emissivity), pointer :: structure_in(:)
     type (type_msediag_emissivity), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_emissivity(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_emissivity'
     end if

   end subroutine copy_arr_type_msediag_emissivity

   subroutine copy_type_msediag_polarization(structure_in, structure_out)

     implicit none

     type (type_msediag_polarization), intent(in) :: structure_in
     type (type_msediag_polarization), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied msediag_polarization%type'

     call copy_type_matflt_type(structure_in%spec_emiss, structure_out%spec_emiss)
     if (verbose > 0) write(iu6, *) 'copied msediag_polarization%spec_emiss'

   end subroutine copy_type_msediag_polarization

   subroutine copy_arr_type_msediag_polarization(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_polarization), pointer :: structure_in(:)
     type (type_msediag_polarization), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_polarization(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_polarization'
     end if

   end subroutine copy_arr_type_msediag_polarization

   subroutine copy_type_msediag_radia_chord(structure_in, structure_out)

     implicit none

     type (type_msediag_radia_chord), intent(in) :: structure_in
     type (type_msediag_radia_chord), intent(inout) :: structure_out

     call copy_type_msediag_setup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied msediag_radia_chord%setup'

     call copy_arr_type_msediag_stokes(structure_in%stokes, structure_out%stokes)
     if (verbose > 0) write(iu6, *) 'copied msediag_radia_chord%stokes'

     call copy_type_exp1D(structure_in%totradiance, structure_out%totradiance)
     if (verbose > 0) write(iu6, *) 'copied msediag_radia_chord%totradiance'

   end subroutine copy_type_msediag_radia_chord

   subroutine copy_arr_type_msediag_radia_chord(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_radia_chord), pointer :: structure_in(:)
     type (type_msediag_radia_chord), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_radia_chord(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_radia_chord'
     end if

   end subroutine copy_arr_type_msediag_radia_chord

   subroutine copy_type_msediag_radiance(structure_in, structure_out)

     implicit none

     type (type_msediag_radiance), intent(in) :: structure_in
     type (type_msediag_radiance), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%wavelength, structure_out%wavelength)
     if (verbose > 0) write(iu6, *) 'copied msediag_radiance%wavelength'

     call copy_arr_type_msediag_radia_chord(structure_in%radia_chord, structure_out%radia_chord)
     if (verbose > 0) write(iu6, *) 'copied msediag_radiance%radia_chord'

   end subroutine copy_type_msediag_radiance

   subroutine copy_arr_type_msediag_radiance(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_radiance), pointer :: structure_in(:)
     type (type_msediag_radiance), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_radiance(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_radiance'
     end if

   end subroutine copy_arr_type_msediag_radiance

   subroutine copy_type_msediag_setup(structure_in, structure_out)

     implicit none

     type (type_msediag_setup), intent(in) :: structure_in
     type (type_msediag_setup), intent(inout) :: structure_out

     call copy_type_rzphi0D(structure_in%pivot_point, structure_out%pivot_point)
     if (verbose > 0) write(iu6, *) 'copied msediag_setup%pivot_point'

     call copy_type_float(structure_in%horchordang, structure_out%horchordang)
     if (verbose > 0) write(iu6, *) 'copied msediag_setup%horchordang'

     call copy_type_float(structure_in%verchordang, structure_out%verchordang)
     if (verbose > 0) write(iu6, *) 'copied msediag_setup%verchordang'

     call copy_type_rzphi0D(structure_in%second_point, structure_out%second_point)
     if (verbose > 0) write(iu6, *) 'copied msediag_setup%second_point'

   end subroutine copy_type_msediag_setup

   subroutine copy_arr_type_msediag_setup(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_setup), pointer :: structure_in(:)
     type (type_msediag_setup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_setup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_setup'
     end if

   end subroutine copy_arr_type_msediag_setup

   subroutine copy_type_msediag_setup_polarimetry(structure_in, structure_out)

     implicit none

     type (type_msediag_setup_polarimetry), intent(in) :: structure_in
     type (type_msediag_setup_polarimetry), intent(inout) :: structure_out

     call copy_type_rzphidrdzdphi1D(structure_in%rzgamma, structure_out%rzgamma)
     if (verbose > 0) write(iu6, *) 'copied msediag_setup_polarimetry%rzgamma'

     call copy_type_matflt_type(structure_in%geom_coef, structure_out%geom_coef)
     if (verbose > 0) write(iu6, *) 'copied msediag_setup_polarimetry%geom_coef'

   end subroutine copy_type_msediag_setup_polarimetry

   subroutine copy_arr_type_msediag_setup_polarimetry(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_setup_polarimetry), pointer :: structure_in(:)
     type (type_msediag_setup_polarimetry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_setup_polarimetry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_setup_polarimetry'
     end if

   end subroutine copy_arr_type_msediag_setup_polarimetry

   subroutine copy_type_msediag_stokes(structure_in, structure_out)

     implicit none

     type (type_msediag_stokes), intent(in) :: structure_in
     type (type_msediag_stokes), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied msediag_stokes%type'

     call copy_type_matflt_type(structure_in%vector, structure_out%vector)
     if (verbose > 0) write(iu6, *) 'copied msediag_stokes%vector'

   end subroutine copy_type_msediag_stokes

   subroutine copy_arr_type_msediag_stokes(structure_in, structure_out)
 
     implicit none
 
     type (type_msediag_stokes), pointer :: structure_in(:)
     type (type_msediag_stokes), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_msediag_stokes(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_msediag_stokes'
     end if

   end subroutine copy_arr_type_msediag_stokes

   subroutine copy_type_nbi_nbi_unit_wall(structure_in, structure_out)

     implicit none

     type (type_nbi_nbi_unit_wall), intent(in) :: structure_in
     type (type_nbi_nbi_unit_wall), intent(inout) :: structure_out

     call copy_type_nbi_nbi_unit_wall_surface(structure_in%surface, structure_out%surface)
     if (verbose > 0) write(iu6, *) 'copied nbi_nbi_unit_wall%surface'

     call copy_arr_type_flat_polygon(structure_in%collimator, structure_out%collimator)
     if (verbose > 0) write(iu6, *) 'copied nbi_nbi_unit_wall%collimator'

   end subroutine copy_type_nbi_nbi_unit_wall

   subroutine copy_arr_type_nbi_nbi_unit_wall(structure_in, structure_out)
 
     implicit none
 
     type (type_nbi_nbi_unit_wall), pointer :: structure_in(:)
     type (type_nbi_nbi_unit_wall), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_nbi_nbi_unit_wall(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_nbi_nbi_unit_wall'
     end if

   end subroutine copy_arr_type_nbi_nbi_unit_wall

   subroutine copy_type_nbi_nbi_unit_wall_surface(structure_in, structure_out)

     implicit none

     type (type_nbi_nbi_unit_wall_surface), intent(in) :: structure_in
     type (type_nbi_nbi_unit_wall_surface), intent(inout) :: structure_out

     call copy_arr_type_trianglexyz(structure_in%triangle, structure_out%triangle)
     if (verbose > 0) write(iu6, *) 'copied nbi_nbi_unit_wall_surface%triangle'

     call copy_arr_type_rectanglexyz(structure_in%rectangle, structure_out%rectangle)
     if (verbose > 0) write(iu6, *) 'copied nbi_nbi_unit_wall_surface%rectangle'

   end subroutine copy_type_nbi_nbi_unit_wall_surface

   subroutine copy_arr_type_nbi_nbi_unit_wall_surface(structure_in, structure_out)
 
     implicit none
 
     type (type_nbi_nbi_unit_wall_surface), pointer :: structure_in(:)
     type (type_nbi_nbi_unit_wall_surface), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_nbi_nbi_unit_wall_surface(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_nbi_nbi_unit_wall_surface'
     end if

   end subroutine copy_arr_type_nbi_nbi_unit_wall_surface

   subroutine copy_type_nbi_unit(structure_in, structure_out)

     implicit none

     type (type_nbi_unit), intent(in) :: structure_in
     type (type_nbi_unit), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%name'

     call copy_type_inj_spec(structure_in%inj_spec, structure_out%inj_spec)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%inj_spec'

     call copy_type_exp0D(structure_in%pow_unit, structure_out%pow_unit)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%pow_unit'

     call copy_type_exp0D(structure_in%inj_eng_unit, structure_out%inj_eng_unit)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%inj_eng_unit'

     call copy_type_exp1D(structure_in%beamcurrfrac, structure_out%beamcurrfrac)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%beamcurrfrac'

     call copy_type_exp1D(structure_in%beampowrfrac, structure_out%beampowrfrac)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%beampowrfrac'

     call copy_arr_type_beamletgroup(structure_in%beamletgroup, structure_out%beamletgroup)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%beamletgroup'

     call copy_type_nbi_nbi_unit_wall(structure_in%wall, structure_out%wall)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%wall'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied nbi_unit%codeparam'

   end subroutine copy_type_nbi_unit

   subroutine copy_arr_type_nbi_unit(structure_in, structure_out)
 
     implicit none
 
     type (type_nbi_unit), pointer :: structure_in(:)
     type (type_nbi_unit), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_nbi_unit(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_nbi_unit'
     end if

   end subroutine copy_arr_type_nbi_unit

   subroutine copy_type_ne_transp(structure_in, structure_out)

     implicit none

     type (type_ne_transp), intent(in) :: structure_in
     type (type_ne_transp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%diff_eff, structure_out%diff_eff)
     if (verbose > 0) write(iu6, *) 'copied ne_transp%diff_eff'

     call copy_type_matflt_type(structure_in%vconv_eff, structure_out%vconv_eff)
     if (verbose > 0) write(iu6, *) 'copied ne_transp%vconv_eff'

     call copy_type_vecflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied ne_transp%flux'

     call copy_type_offdiagel(structure_in%off_diagonal, structure_out%off_diagonal)
     if (verbose > 0) write(iu6, *) 'copied ne_transp%off_diagonal'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied ne_transp%flag'

   end subroutine copy_type_ne_transp

   subroutine copy_arr_type_ne_transp(structure_in, structure_out)
 
     implicit none
 
     type (type_ne_transp), pointer :: structure_in(:)
     type (type_ne_transp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ne_transp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ne_transp'
     end if

   end subroutine copy_arr_type_ne_transp

   subroutine copy_type_neoclassic_impurity(structure_in, structure_out)

     implicit none

     type (type_neoclassic_impurity), intent(in) :: structure_in
     type (type_neoclassic_impurity), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%utheta_z, structure_out%utheta_z)
     if (verbose > 0) write(iu6, *) 'copied neoclassic_impurity%utheta_z'

   end subroutine copy_type_neoclassic_impurity

   subroutine copy_arr_type_neoclassic_impurity(structure_in, structure_out)
 
     implicit none
 
     type (type_neoclassic_impurity), pointer :: structure_in(:)
     type (type_neoclassic_impurity), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_neoclassic_impurity(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_neoclassic_impurity'
     end if

   end subroutine copy_arr_type_neoclassic_impurity

   subroutine copy_type_neut_results(structure_in, structure_out)

     implicit none

     type (type_neut_results), intent(in) :: structure_in
     type (type_neut_results), intent(inout) :: structure_out

     call copy_type_float(structure_in%tbr_bk, structure_out%tbr_bk)
     if (verbose > 0) write(iu6, *) 'copied neut_results%tbr_bk'

     call copy_type_float(structure_in%tbr_bk_inb, structure_out%tbr_bk_inb)
     if (verbose > 0) write(iu6, *) 'copied neut_results%tbr_bk_inb'

     call copy_type_float(structure_in%tbr_bk_outb, structure_out%tbr_bk_outb)
     if (verbose > 0) write(iu6, *) 'copied neut_results%tbr_bk_outb'

     call copy_type_float(structure_in%me_bk, structure_out%me_bk)
     if (verbose > 0) write(iu6, *) 'copied neut_results%me_bk'

     call copy_type_float(structure_in%me_shield, structure_out%me_shield)
     if (verbose > 0) write(iu6, *) 'copied neut_results%me_shield'

     call copy_type_float(structure_in%he_appm_res, structure_out%he_appm_res)
     if (verbose > 0) write(iu6, *) 'copied neut_results%he_appm_res'

     call copy_type_float(structure_in%ins_dose_max, structure_out%ins_dose_max)
     if (verbose > 0) write(iu6, *) 'copied neut_results%ins_dose_max'

     call copy_type_float(structure_in%fn_flu_max, structure_out%fn_flu_max)
     if (verbose > 0) write(iu6, *) 'copied neut_results%fn_flu_max'

     call copy_type_float(structure_in%dpa_cu_max, structure_out%dpa_cu_max)
     if (verbose > 0) write(iu6, *) 'copied neut_results%dpa_cu_max'

     call copy_type_float(structure_in%fn_flux_bz, structure_out%fn_flux_bz)
     if (verbose > 0) write(iu6, *) 'copied neut_results%fn_flux_bz'

     call copy_type_float(structure_in%fn_flux_bp, structure_out%fn_flux_bp)
     if (verbose > 0) write(iu6, *) 'copied neut_results%fn_flux_bp'

     call copy_type_float(structure_in%fn_flux_man, structure_out%fn_flux_man)
     if (verbose > 0) write(iu6, *) 'copied neut_results%fn_flux_man'

     call copy_type_float(structure_in%fn_flux_sh, structure_out%fn_flux_sh)
     if (verbose > 0) write(iu6, *) 'copied neut_results%fn_flux_sh'

     call copy_type_float(structure_in%p_nh_bk, structure_out%p_nh_bk)
     if (verbose > 0) write(iu6, *) 'copied neut_results%p_nh_bk'

     call copy_type_float(structure_in%p_nh_sh, structure_out%p_nh_sh)
     if (verbose > 0) write(iu6, *) 'copied neut_results%p_nh_sh'

   end subroutine copy_type_neut_results

   subroutine copy_arr_type_neut_results(structure_in, structure_out)
 
     implicit none
 
     type (type_neut_results), pointer :: structure_in(:)
     type (type_neut_results), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_neut_results(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_neut_results'
     end if

   end subroutine copy_arr_type_neut_results

   subroutine copy_type_neutral_complex_type(structure_in, structure_out)

     implicit none

     type (type_neutral_complex_type), intent(in) :: structure_in
     type (type_neutral_complex_type), intent(inout) :: structure_out

     call copy_arr_type_coreneutrals_neutraltype(structure_in%neutraltype, structure_out%neutraltype)
     if (verbose > 0) write(iu6, *) 'copied neutral_complex_type%neutraltype'

     call copy_type_vecflt_type(structure_in%prad0, structure_out%prad0)
     if (verbose > 0) write(iu6, *) 'copied neutral_complex_type%prad0'

   end subroutine copy_type_neutral_complex_type

   subroutine copy_arr_type_neutral_complex_type(structure_in, structure_out)
 
     implicit none
 
     type (type_neutral_complex_type), pointer :: structure_in(:)
     type (type_neutral_complex_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_neutral_complex_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_neutral_complex_type'
     end if

   end subroutine copy_arr_type_neutral_complex_type

   subroutine copy_type_neutro_resul(structure_in, structure_out)

     implicit none

     type (type_neutro_resul), intent(in) :: structure_in
     type (type_neutro_resul), intent(inout) :: structure_out

     call copy_type_float(structure_in%nwl_max, structure_out%nwl_max)
     if (verbose > 0) write(iu6, *) 'copied neutro_resul%nwl_max'

     call copy_type_vecflt_type(structure_in%nwl_pol_prof, structure_out%nwl_pol_prof)
     if (verbose > 0) write(iu6, *) 'copied neutro_resul%nwl_pol_prof'

   end subroutine copy_type_neutro_resul

   subroutine copy_arr_type_neutro_resul(structure_in, structure_out)
 
     implicit none
 
     type (type_neutro_resul), pointer :: structure_in(:)
     type (type_neutro_resul), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_neutro_resul(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_neutro_resul'
     end if

   end subroutine copy_arr_type_neutro_resul

   subroutine copy_type_ni_transp(structure_in, structure_out)

     implicit none

     type (type_ni_transp), intent(in) :: structure_in
     type (type_ni_transp), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%diff_eff, structure_out%diff_eff)
     if (verbose > 0) write(iu6, *) 'copied ni_transp%diff_eff'

     call copy_type_array3dflt_type(structure_in%vconv_eff, structure_out%vconv_eff)
     if (verbose > 0) write(iu6, *) 'copied ni_transp%vconv_eff'

     call copy_type_matflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied ni_transp%flux'

     call copy_type_offdiagion(structure_in%off_diagonal, structure_out%off_diagonal)
     if (verbose > 0) write(iu6, *) 'copied ni_transp%off_diagonal'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied ni_transp%flag'

   end subroutine copy_type_ni_transp

   subroutine copy_arr_type_ni_transp(structure_in, structure_out)
 
     implicit none
 
     type (type_ni_transp), pointer :: structure_in(:)
     type (type_ni_transp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ni_transp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ni_transp'
     end if

   end subroutine copy_arr_type_ni_transp

   subroutine copy_type_ntm_mode(structure_in, structure_out)

     implicit none

     type (type_ntm_mode), intent(in) :: structure_in
     type (type_ntm_mode), intent(inout) :: structure_out

     call copy_type_ntm_mode_onset(structure_in%onset, structure_out%onset)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode%onset'

     call copy_type_ntm_mode_full_evol(structure_in%full_evol, structure_out%full_evol)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode%full_evol'

     call copy_type_ntm_mode_evolution(structure_in%evolution, structure_out%evolution)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode%evolution'

   end subroutine copy_type_ntm_mode

   subroutine copy_arr_type_ntm_mode(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm_mode), pointer :: structure_in(:)
     type (type_ntm_mode), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm_mode(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm_mode'
     end if

   end subroutine copy_arr_type_ntm_mode

   subroutine copy_type_ntm_mode_evolution(structure_in, structure_out)

     implicit none

     type (type_ntm_mode_evolution), intent(in) :: structure_in
     type (type_ntm_mode_evolution), intent(inout) :: structure_out

     call copy_type_float(structure_in%w, structure_out%w)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%w'

     call copy_type_float(structure_in%dwdt, structure_out%dwdt)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%dwdt'

     call copy_type_float(structure_in%phase, structure_out%phase)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%phase'

     call copy_type_float(structure_in%dphasedt, structure_out%dphasedt)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%dphasedt'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%frequency'

     call copy_type_float(structure_in%dfrequencydt, structure_out%dfrequencydt)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%dfrequencydt'

     call copy_type_ntm_mode_evolution_island(structure_in%island, structure_out%island)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%island'

     call copy_type_integer(structure_in%n, structure_out%n)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%n'

     call copy_type_integer(structure_in%m, structure_out%m)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%m'

     call copy_type_vecflt_type(structure_in%deltaw_value, structure_out%deltaw_value)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%deltaw_value'

     call copy_type_vecstring_type(structure_in%deltaw_name, structure_out%deltaw_name)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%deltaw_name'

     call copy_type_vecflt_type(structure_in%torque_value, structure_out%torque_value)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%torque_value'

     call copy_type_vecstring_type(structure_in%torque_name, structure_out%torque_name)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%torque_name'

     call copy_type_vecflt_type(structure_in%delta_diff, structure_out%delta_diff)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%delta_diff'

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%description'

     call copy_type_float(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution%rho_tor'

   end subroutine copy_type_ntm_mode_evolution

   subroutine copy_arr_type_ntm_mode_evolution(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm_mode_evolution), pointer :: structure_in(:)
     type (type_ntm_mode_evolution), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm_mode_evolution(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm_mode_evolution'
     end if

   end subroutine copy_arr_type_ntm_mode_evolution

   subroutine copy_type_ntm_mode_evolution_island(structure_in, structure_out)

     implicit none

     type (type_ntm_mode_evolution_island), intent(in) :: structure_in
     type (type_ntm_mode_evolution_island), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution_island%geometry'

     call copy_type_vecflt_type(structure_in%coord_values, structure_out%coord_values)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution_island%coord_values'

     call copy_type_vecstring_type(structure_in%coord_desc, structure_out%coord_desc)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_evolution_island%coord_desc'

   end subroutine copy_type_ntm_mode_evolution_island

   subroutine copy_arr_type_ntm_mode_evolution_island(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm_mode_evolution_island), pointer :: structure_in(:)
     type (type_ntm_mode_evolution_island), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm_mode_evolution_island(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm_mode_evolution_island'
     end if

   end subroutine copy_arr_type_ntm_mode_evolution_island

   subroutine copy_type_ntm_mode_full_evol(structure_in, structure_out)

     implicit none

     type (type_ntm_mode_full_evol), intent(in) :: structure_in
     type (type_ntm_mode_full_evol), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%time_evol, structure_out%time_evol)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%time_evol'

     call copy_type_vecflt_type(structure_in%w, structure_out%w)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%w'

     call copy_type_vecflt_type(structure_in%dwdt, structure_out%dwdt)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%dwdt'

     call copy_type_vecflt_type(structure_in%phase, structure_out%phase)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%phase'

     call copy_type_vecflt_type(structure_in%dphasedt, structure_out%dphasedt)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%dphasedt'

     call copy_type_vecflt_type(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%frequency'

     call copy_type_vecflt_type(structure_in%dfrequencydt, structure_out%dfrequencydt)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%dfrequencydt'

     call copy_type_ntm_mode_full_evol_island(structure_in%island, structure_out%island)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%island'

     call copy_type_integer(structure_in%n, structure_out%n)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%n'

     call copy_type_integer(structure_in%m, structure_out%m)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%m'

     call copy_type_matflt_type(structure_in%deltaw_value, structure_out%deltaw_value)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%deltaw_value'

     call copy_type_vecstring_type(structure_in%deltaw_name, structure_out%deltaw_name)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%deltaw_name'

     call copy_type_matflt_type(structure_in%torque_value, structure_out%torque_value)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%torque_value'

     call copy_type_vecstring_type(structure_in%torque_name, structure_out%torque_name)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%torque_name'

     call copy_type_matflt_type(structure_in%delta_diff, structure_out%delta_diff)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%delta_diff'

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%description'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol%rho_tor'

   end subroutine copy_type_ntm_mode_full_evol

   subroutine copy_arr_type_ntm_mode_full_evol(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm_mode_full_evol), pointer :: structure_in(:)
     type (type_ntm_mode_full_evol), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm_mode_full_evol(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm_mode_full_evol'
     end if

   end subroutine copy_arr_type_ntm_mode_full_evol

   subroutine copy_type_ntm_mode_full_evol_island(structure_in, structure_out)

     implicit none

     type (type_ntm_mode_full_evol_island), intent(in) :: structure_in
     type (type_ntm_mode_full_evol_island), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol_island%geometry'

     call copy_type_matflt_type(structure_in%coord_values, structure_out%coord_values)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol_island%coord_values'

     call copy_type_vecstring_type(structure_in%coord_desc, structure_out%coord_desc)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_full_evol_island%coord_desc'

   end subroutine copy_type_ntm_mode_full_evol_island

   subroutine copy_arr_type_ntm_mode_full_evol_island(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm_mode_full_evol_island), pointer :: structure_in(:)
     type (type_ntm_mode_full_evol_island), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm_mode_full_evol_island(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm_mode_full_evol_island'
     end if

   end subroutine copy_arr_type_ntm_mode_full_evol_island

   subroutine copy_type_ntm_mode_onset(structure_in, structure_out)

     implicit none

     type (type_ntm_mode_onset), intent(in) :: structure_in
     type (type_ntm_mode_onset), intent(inout) :: structure_out

     call copy_type_float(structure_in%w, structure_out%w)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%w'

     call copy_type_float(structure_in%time_onset, structure_out%time_onset)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%time_onset'

     call copy_type_float(structure_in%time_offset, structure_out%time_offset)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%time_offset'

     call copy_type_float(structure_in%phase, structure_out%phase)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%phase'

     call copy_type_integer(structure_in%n, structure_out%n)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%n'

     call copy_type_integer(structure_in%m, structure_out%m)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%m'

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied ntm_mode_onset%description'

   end subroutine copy_type_ntm_mode_onset

   subroutine copy_arr_type_ntm_mode_onset(structure_in, structure_out)
 
     implicit none
 
     type (type_ntm_mode_onset), pointer :: structure_in(:)
     type (type_ntm_mode_onset), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_ntm_mode_onset(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_ntm_mode_onset'
     end if

   end subroutine copy_arr_type_ntm_mode_onset

   subroutine copy_type_nuclei(structure_in, structure_out)

     implicit none

     type (type_nuclei), intent(in) :: structure_in
     type (type_nuclei), intent(inout) :: structure_out

     call copy_type_float(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied nuclei%zn'

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied nuclei%amn'

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied nuclei%label'

   end subroutine copy_type_nuclei

   subroutine copy_arr_type_nuclei(structure_in, structure_out)
 
     implicit none
 
     type (type_nuclei), pointer :: structure_in(:)
     type (type_nuclei), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_nuclei(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_nuclei'
     end if

   end subroutine copy_arr_type_nuclei

   subroutine copy_type_objects(structure_in, structure_out)

     implicit none

     type (type_objects), intent(in) :: structure_in
     type (type_objects), intent(inout) :: structure_out

     call copy_type_matint_type(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied objects%boundary'

     call copy_type_array3dint_type(structure_in%neighbour, structure_out%neighbour)
     if (verbose > 0) write(iu6, *) 'copied objects%neighbour'

     call copy_type_array4dflt_type(structure_in%geo, structure_out%geo)
     if (verbose > 0) write(iu6, *) 'copied objects%geo'

     call copy_type_matflt_type(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied objects%measure'

   end subroutine copy_type_objects

   subroutine copy_arr_type_objects(structure_in, structure_out)
 
     implicit none
 
     type (type_objects), pointer :: structure_in(:)
     type (type_objects), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_objects(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_objects'
     end if

   end subroutine copy_arr_type_objects

   subroutine copy_type_offdiagel(structure_in, structure_out)

     implicit none

     type (type_offdiagel), intent(in) :: structure_in
     type (type_offdiagel), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%d_ni, structure_out%d_ni)
     if (verbose > 0) write(iu6, *) 'copied offdiagel%d_ni'

     call copy_type_matflt_type(structure_in%d_ti, structure_out%d_ti)
     if (verbose > 0) write(iu6, *) 'copied offdiagel%d_ti'

     call copy_type_vecflt_type(structure_in%d_ne, structure_out%d_ne)
     if (verbose > 0) write(iu6, *) 'copied offdiagel%d_ne'

     call copy_type_vecflt_type(structure_in%d_te, structure_out%d_te)
     if (verbose > 0) write(iu6, *) 'copied offdiagel%d_te'

     call copy_type_vecflt_type(structure_in%d_epar, structure_out%d_epar)
     if (verbose > 0) write(iu6, *) 'copied offdiagel%d_epar'

     call copy_type_vecflt_type(structure_in%d_mtor, structure_out%d_mtor)
     if (verbose > 0) write(iu6, *) 'copied offdiagel%d_mtor'

   end subroutine copy_type_offdiagel

   subroutine copy_arr_type_offdiagel(structure_in, structure_out)
 
     implicit none
 
     type (type_offdiagel), pointer :: structure_in(:)
     type (type_offdiagel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_offdiagel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_offdiagel'
     end if

   end subroutine copy_arr_type_offdiagel

   subroutine copy_type_offdiagion(structure_in, structure_out)

     implicit none

     type (type_offdiagion), intent(in) :: structure_in
     type (type_offdiagion), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%d_ni, structure_out%d_ni)
     if (verbose > 0) write(iu6, *) 'copied offdiagion%d_ni'

     call copy_type_array3dflt_type(structure_in%d_ti, structure_out%d_ti)
     if (verbose > 0) write(iu6, *) 'copied offdiagion%d_ti'

     call copy_type_matflt_type(structure_in%d_ne, structure_out%d_ne)
     if (verbose > 0) write(iu6, *) 'copied offdiagion%d_ne'

     call copy_type_matflt_type(structure_in%d_te, structure_out%d_te)
     if (verbose > 0) write(iu6, *) 'copied offdiagion%d_te'

     call copy_type_matflt_type(structure_in%d_epar, structure_out%d_epar)
     if (verbose > 0) write(iu6, *) 'copied offdiagion%d_epar'

     call copy_type_matflt_type(structure_in%d_mtor, structure_out%d_mtor)
     if (verbose > 0) write(iu6, *) 'copied offdiagion%d_mtor'

   end subroutine copy_type_offdiagion

   subroutine copy_arr_type_offdiagion(structure_in, structure_out)
 
     implicit none
 
     type (type_offdiagion), pointer :: structure_in(:)
     type (type_offdiagion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_offdiagion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_offdiagion'
     end if

   end subroutine copy_arr_type_offdiagion

   subroutine copy_type_omnigen_surf(structure_in, structure_out)

     implicit none

     type (type_omnigen_surf), intent(in) :: structure_in
     type (type_omnigen_surf), intent(inout) :: structure_out

     call copy_type_rz1D(structure_in%rz, structure_out%rz)
     if (verbose > 0) write(iu6, *) 'copied omnigen_surf%rz'

     call copy_type_vecflt_type(structure_in%s, structure_out%s)
     if (verbose > 0) write(iu6, *) 'copied omnigen_surf%s'

   end subroutine copy_type_omnigen_surf

   subroutine copy_arr_type_omnigen_surf(structure_in, structure_out)
 
     implicit none
 
     type (type_omnigen_surf), pointer :: structure_in(:)
     type (type_omnigen_surf), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_omnigen_surf(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_omnigen_surf'
     end if

   end subroutine copy_arr_type_omnigen_surf

   subroutine copy_type_orbit_global_param(structure_in, structure_out)

     implicit none

     type (type_orbit_global_param), intent(in) :: structure_in
     type (type_orbit_global_param), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%orbit_type, structure_out%orbit_type)
     if (verbose > 0) write(iu6, *) 'copied orbit_global_param%orbit_type'

     call copy_type_vecflt_type(structure_in%omega_b, structure_out%omega_b)
     if (verbose > 0) write(iu6, *) 'copied orbit_global_param%omega_b'

     call copy_type_vecflt_type(structure_in%omega_phi, structure_out%omega_phi)
     if (verbose > 0) write(iu6, *) 'copied orbit_global_param%omega_phi'

     call copy_type_vecflt_type(structure_in%omega_c_av, structure_out%omega_c_av)
     if (verbose > 0) write(iu6, *) 'copied orbit_global_param%omega_c_av'

     call copy_type_orbit_special_pos(structure_in%special_pos, structure_out%special_pos)
     if (verbose > 0) write(iu6, *) 'copied orbit_global_param%special_pos'

   end subroutine copy_type_orbit_global_param

   subroutine copy_arr_type_orbit_global_param(structure_in, structure_out)
 
     implicit none
 
     type (type_orbit_global_param), pointer :: structure_in(:)
     type (type_orbit_global_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_orbit_global_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_orbit_global_param'
     end if

   end subroutine copy_arr_type_orbit_global_param

   subroutine copy_type_orbit_midplane(structure_in, structure_out)

     implicit none

     type (type_orbit_midplane), intent(in) :: structure_in
     type (type_orbit_midplane), intent(inout) :: structure_out

     call copy_type_orbit_pos(structure_in%outer, structure_out%outer)
     if (verbose > 0) write(iu6, *) 'copied orbit_midplane%outer'

     call copy_type_orbit_pos(structure_in%inner, structure_out%inner)
     if (verbose > 0) write(iu6, *) 'copied orbit_midplane%inner'

   end subroutine copy_type_orbit_midplane

   subroutine copy_arr_type_orbit_midplane(structure_in, structure_out)
 
     implicit none
 
     type (type_orbit_midplane), pointer :: structure_in(:)
     type (type_orbit_midplane), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_orbit_midplane(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_orbit_midplane'
     end if

   end subroutine copy_arr_type_orbit_midplane

   subroutine copy_type_orbit_pos(structure_in, structure_out)

     implicit none

     type (type_orbit_pos), intent(in) :: structure_in
     type (type_orbit_pos), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied orbit_pos%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied orbit_pos%z'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied orbit_pos%phi'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied orbit_pos%psi'

     call copy_type_vecflt_type(structure_in%theta_b, structure_out%theta_b)
     if (verbose > 0) write(iu6, *) 'copied orbit_pos%theta_b'

   end subroutine copy_type_orbit_pos

   subroutine copy_arr_type_orbit_pos(structure_in, structure_out)
 
     implicit none
 
     type (type_orbit_pos), pointer :: structure_in(:)
     type (type_orbit_pos), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_orbit_pos(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_orbit_pos'
     end if

   end subroutine copy_arr_type_orbit_pos

   subroutine copy_type_orbit_special_pos(structure_in, structure_out)

     implicit none

     type (type_orbit_special_pos), intent(in) :: structure_in
     type (type_orbit_special_pos), intent(inout) :: structure_out

     call copy_type_orbit_midplane(structure_in%midplane, structure_out%midplane)
     if (verbose > 0) write(iu6, *) 'copied orbit_special_pos%midplane'

     call copy_type_orbit_turning_pts(structure_in%turning_pts, structure_out%turning_pts)
     if (verbose > 0) write(iu6, *) 'copied orbit_special_pos%turning_pts'

   end subroutine copy_type_orbit_special_pos

   subroutine copy_arr_type_orbit_special_pos(structure_in, structure_out)
 
     implicit none
 
     type (type_orbit_special_pos), pointer :: structure_in(:)
     type (type_orbit_special_pos), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_orbit_special_pos(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_orbit_special_pos'
     end if

   end subroutine copy_arr_type_orbit_special_pos

   subroutine copy_type_orbit_turning_pts(structure_in, structure_out)

     implicit none

     type (type_orbit_turning_pts), intent(in) :: structure_in
     type (type_orbit_turning_pts), intent(inout) :: structure_out

     call copy_type_orbit_pos(structure_in%upper, structure_out%upper)
     if (verbose > 0) write(iu6, *) 'copied orbit_turning_pts%upper'

     call copy_type_orbit_pos(structure_in%lower, structure_out%lower)
     if (verbose > 0) write(iu6, *) 'copied orbit_turning_pts%lower'

   end subroutine copy_type_orbit_turning_pts

   subroutine copy_arr_type_orbit_turning_pts(structure_in, structure_out)
 
     implicit none
 
     type (type_orbit_turning_pts), pointer :: structure_in(:)
     type (type_orbit_turning_pts), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_orbit_turning_pts(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_orbit_turning_pts'
     end if

   end subroutine copy_arr_type_orbit_turning_pts

   subroutine copy_type_origin(structure_in, structure_out)

     implicit none

     type (type_origin), intent(in) :: structure_in
     type (type_origin), intent(inout) :: structure_out

     call copy_type_rzphi0D(structure_in%refpos, structure_out%refpos)
     if (verbose > 0) write(iu6, *) 'copied origin%refpos'

     call copy_type_float(structure_in%alpha, structure_out%alpha)
     if (verbose > 0) write(iu6, *) 'copied origin%alpha'

     call copy_type_float(structure_in%beta, structure_out%beta)
     if (verbose > 0) write(iu6, *) 'copied origin%beta'

     call copy_type_float(structure_in%gamma, structure_out%gamma)
     if (verbose > 0) write(iu6, *) 'copied origin%gamma'

   end subroutine copy_type_origin

   subroutine copy_arr_type_origin(structure_in, structure_out)
 
     implicit none
 
     type (type_origin), pointer :: structure_in(:)
     type (type_origin), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_origin(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_origin'
     end if

   end subroutine copy_arr_type_origin

   subroutine copy_type_param(structure_in, structure_out)

     implicit none

     type (type_param), intent(in) :: structure_in
     type (type_param), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%parameters, structure_out%parameters)
     if (verbose > 0) write(iu6, *) 'copied param%parameters'

     call copy_type_vecstring_type(structure_in%default_param, structure_out%default_param)
     if (verbose > 0) write(iu6, *) 'copied param%default_param'

     call copy_type_vecstring_type(structure_in%schema, structure_out%schema)
     if (verbose > 0) write(iu6, *) 'copied param%schema'

   end subroutine copy_type_param

   subroutine copy_arr_type_param(structure_in, structure_out)
 
     implicit none
 
     type (type_param), pointer :: structure_in(:)
     type (type_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_param'
     end if

   end subroutine copy_arr_type_param

   subroutine copy_type_parameters(structure_in, structure_out)

     implicit none

     type (type_parameters), intent(in) :: structure_in
     type (type_parameters), intent(inout) :: structure_out

     call copy_type_equatorial_plane(structure_in%equatorial, structure_out%equatorial)
     if (verbose > 0) write(iu6, *) 'copied parameters%equatorial'

   end subroutine copy_type_parameters

   subroutine copy_arr_type_parameters(structure_in, structure_out)
 
     implicit none
 
     type (type_parameters), pointer :: structure_in(:)
     type (type_parameters), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_parameters(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_parameters'
     end if

   end subroutine copy_arr_type_parameters

   subroutine copy_type_pellet(structure_in, structure_out)

     implicit none

     type (type_pellet), intent(in) :: structure_in
     type (type_pellet), intent(inout) :: structure_out

     call copy_type_pellet_shape(structure_in%shape, structure_out%shape)
     if (verbose > 0) write(iu6, *) 'copied pellet%shape'

     call copy_type_pellet_elements(structure_in%elements, structure_out%elements)
     if (verbose > 0) write(iu6, *) 'copied pellet%elements'

     call copy_type_pellet_geometry(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied pellet%geometry'

     call copy_type_pellet_pathprofiles(structure_in%pathprofiles, structure_out%pathprofiles)
     if (verbose > 0) write(iu6, *) 'copied pellet%pathprofiles'

     call copy_type_pellet_deposition(structure_in%deposition, structure_out%deposition)
     if (verbose > 0) write(iu6, *) 'copied pellet%deposition'

   end subroutine copy_type_pellet

   subroutine copy_arr_type_pellet(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet), pointer :: structure_in(:)
     type (type_pellet), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet'
     end if

   end subroutine copy_arr_type_pellet

   subroutine copy_type_pellet_angles(structure_in, structure_out)

     implicit none

     type (type_pellet_angles), intent(in) :: structure_in
     type (type_pellet_angles), intent(inout) :: structure_out

     call copy_type_float(structure_in%horizontal, structure_out%horizontal)
     if (verbose > 0) write(iu6, *) 'copied pellet_angles%horizontal'

     call copy_type_float(structure_in%vertical, structure_out%vertical)
     if (verbose > 0) write(iu6, *) 'copied pellet_angles%vertical'

   end subroutine copy_type_pellet_angles

   subroutine copy_arr_type_pellet_angles(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_angles), pointer :: structure_in(:)
     type (type_pellet_angles), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_angles(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_angles'
     end if

   end subroutine copy_arr_type_pellet_angles

   subroutine copy_type_pellet_deposition(structure_in, structure_out)

     implicit none

     type (type_pellet_deposition), intent(in) :: structure_in
     type (type_pellet_deposition), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_pol, structure_out%rho_pol)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%rho_pol'

     call copy_type_vecflt_type(structure_in%delta_ne, structure_out%delta_ne)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%delta_ne'

     call copy_type_vecflt_type(structure_in%delta_te, structure_out%delta_te)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%delta_te'

     call copy_type_matflt_type(structure_in%delta_ni, structure_out%delta_ni)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%delta_ni'

     call copy_type_matflt_type(structure_in%delta_ti, structure_out%delta_ti)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%delta_ti'

     call copy_type_matflt_type(structure_in%delta_vtor, structure_out%delta_vtor)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%delta_vtor'

     call copy_arr_type_pellet_impurity(structure_in%impurity, structure_out%impurity)
     if (verbose > 0) write(iu6, *) 'copied pellet_deposition%impurity'

   end subroutine copy_type_pellet_deposition

   subroutine copy_arr_type_pellet_deposition(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_deposition), pointer :: structure_in(:)
     type (type_pellet_deposition), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_deposition(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_deposition'
     end if

   end subroutine copy_arr_type_pellet_deposition

   subroutine copy_type_pellet_elements(structure_in, structure_out)

     implicit none

     type (type_pellet_elements), intent(in) :: structure_in
     type (type_pellet_elements), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%nucindex, structure_out%nucindex)
     if (verbose > 0) write(iu6, *) 'copied pellet_elements%nucindex'

     call copy_type_vecflt_type(structure_in%density, structure_out%density)
     if (verbose > 0) write(iu6, *) 'copied pellet_elements%density'

     call copy_type_vecflt_type(structure_in%fraction, structure_out%fraction)
     if (verbose > 0) write(iu6, *) 'copied pellet_elements%fraction'

     call copy_type_vecflt_type(structure_in%subl_energy, structure_out%subl_energy)
     if (verbose > 0) write(iu6, *) 'copied pellet_elements%subl_energy'

   end subroutine copy_type_pellet_elements

   subroutine copy_arr_type_pellet_elements(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_elements), pointer :: structure_in(:)
     type (type_pellet_elements), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_elements(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_elements'
     end if

   end subroutine copy_arr_type_pellet_elements

   subroutine copy_type_pellet_geometry(structure_in, structure_out)

     implicit none

     type (type_pellet_geometry), intent(in) :: structure_in
     type (type_pellet_geometry), intent(inout) :: structure_out

     call copy_type_rzphi0D(structure_in%pivot_point, structure_out%pivot_point)
     if (verbose > 0) write(iu6, *) 'copied pellet_geometry%pivot_point'

     call copy_type_rzphi0D(structure_in%second_point, structure_out%second_point)
     if (verbose > 0) write(iu6, *) 'copied pellet_geometry%second_point'

     call copy_type_float(structure_in%velocity, structure_out%velocity)
     if (verbose > 0) write(iu6, *) 'copied pellet_geometry%velocity'

     call copy_type_pellet_angles(structure_in%angles, structure_out%angles)
     if (verbose > 0) write(iu6, *) 'copied pellet_geometry%angles'

   end subroutine copy_type_pellet_geometry

   subroutine copy_arr_type_pellet_geometry(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_geometry), pointer :: structure_in(:)
     type (type_pellet_geometry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_geometry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_geometry'
     end if

   end subroutine copy_arr_type_pellet_geometry

   subroutine copy_type_pellet_impurity(structure_in, structure_out)

     implicit none

     type (type_pellet_impurity), intent(in) :: structure_in
     type (type_pellet_impurity), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%delta_nz, structure_out%delta_nz)
     if (verbose > 0) write(iu6, *) 'copied pellet_impurity%delta_nz'

   end subroutine copy_type_pellet_impurity

   subroutine copy_arr_type_pellet_impurity(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_impurity), pointer :: structure_in(:)
     type (type_pellet_impurity), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_impurity(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_impurity'
     end if

   end subroutine copy_arr_type_pellet_impurity

   subroutine copy_type_pellet_pathprofiles(structure_in, structure_out)

     implicit none

     type (type_pellet_pathprofiles), intent(in) :: structure_in
     type (type_pellet_pathprofiles), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%distance, structure_out%distance)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%distance'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_pol, structure_out%rho_pol)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%rho_pol'

     call copy_type_vecflt_type(structure_in%velocity, structure_out%velocity)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%velocity'

     call copy_type_vecflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%ne'

     call copy_type_vecflt_type(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%te'

     call copy_type_vecflt_type(structure_in%abl_rate, structure_out%abl_rate)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%abl_rate'

     call copy_type_vecflt_type(structure_in%abl_particles, structure_out%abl_particles)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%abl_particles'

     call copy_type_vecflt_type(structure_in%delta_drift, structure_out%delta_drift)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%delta_drift'

     call copy_type_rzphi1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied pellet_pathprofiles%position'

   end subroutine copy_type_pellet_pathprofiles

   subroutine copy_arr_type_pellet_pathprofiles(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_pathprofiles), pointer :: structure_in(:)
     type (type_pellet_pathprofiles), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_pathprofiles(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_pathprofiles'
     end if

   end subroutine copy_arr_type_pellet_pathprofiles

   subroutine copy_type_pellet_shape(structure_in, structure_out)

     implicit none

     type (type_pellet_shape), intent(in) :: structure_in
     type (type_pellet_shape), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied pellet_shape%type'

     call copy_type_vecflt_type(structure_in%dimensions, structure_out%dimensions)
     if (verbose > 0) write(iu6, *) 'copied pellet_shape%dimensions'

   end subroutine copy_type_pellet_shape

   subroutine copy_arr_type_pellet_shape(structure_in, structure_out)
 
     implicit none
 
     type (type_pellet_shape), pointer :: structure_in(:)
     type (type_pellet_shape), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pellet_shape(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pellet_shape'
     end if

   end subroutine copy_arr_type_pellet_shape

   subroutine copy_type_permeability(structure_in, structure_out)

     implicit none

     type (type_permeability), intent(in) :: structure_in
     type (type_permeability), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%b, structure_out%b)
     if (verbose > 0) write(iu6, *) 'copied permeability%b'

     call copy_type_matflt_type(structure_in%mur, structure_out%mur)
     if (verbose > 0) write(iu6, *) 'copied permeability%mur'

   end subroutine copy_type_permeability

   subroutine copy_arr_type_permeability(structure_in, structure_out)
 
     implicit none
 
     type (type_permeability), pointer :: structure_in(:)
     type (type_permeability), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_permeability(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_permeability'
     end if

   end subroutine copy_arr_type_permeability

   subroutine copy_type_pfcircuits(structure_in, structure_out)

     implicit none

     type (type_pfcircuits), intent(in) :: structure_in
     type (type_pfcircuits), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied pfcircuits%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied pfcircuits%id'

     call copy_type_vecstring_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied pfcircuits%type'

     call copy_type_vecint_type(structure_in%nnodes, structure_out%nnodes)
     if (verbose > 0) write(iu6, *) 'copied pfcircuits%nnodes'

     call copy_type_array3dint_type(structure_in%connections, structure_out%connections)
     if (verbose > 0) write(iu6, *) 'copied pfcircuits%connections'

   end subroutine copy_type_pfcircuits

   subroutine copy_arr_type_pfcircuits(structure_in, structure_out)
 
     implicit none
 
     type (type_pfcircuits), pointer :: structure_in(:)
     type (type_pfcircuits), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfcircuits(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfcircuits'
     end if

   end subroutine copy_arr_type_pfcircuits

   subroutine copy_type_pfcoils(structure_in, structure_out)

     implicit none

     type (type_pfcoils), intent(in) :: structure_in
     type (type_pfcoils), intent(inout) :: structure_out

     call copy_type_desc_pfcoils(structure_in%desc_pfcoils, structure_out%desc_pfcoils)
     if (verbose > 0) write(iu6, *) 'copied pfcoils%desc_pfcoils'

     call copy_type_exp1D(structure_in%coilcurrent, structure_out%coilcurrent)
     if (verbose > 0) write(iu6, *) 'copied pfcoils%coilcurrent'

     call copy_type_exp1D(structure_in%coilvoltage, structure_out%coilvoltage)
     if (verbose > 0) write(iu6, *) 'copied pfcoils%coilvoltage'

     call copy_type_float(structure_in%p_cryo, structure_out%p_cryo)
     if (verbose > 0) write(iu6, *) 'copied pfcoils%p_cryo'

     call copy_type_vecflt_type(structure_in%p_nh, structure_out%p_nh)
     if (verbose > 0) write(iu6, *) 'copied pfcoils%p_nh'

   end subroutine copy_type_pfcoils

   subroutine copy_arr_type_pfcoils(structure_in, structure_out)
 
     implicit none
 
     type (type_pfcoils), pointer :: structure_in(:)
     type (type_pfcoils), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfcoils(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfcoils'
     end if

   end subroutine copy_arr_type_pfcoils

   subroutine copy_type_pfelement(structure_in, structure_out)

     implicit none

     type (type_pfelement), intent(in) :: structure_in
     type (type_pfelement), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied pfelement%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied pfelement%id'

     call copy_type_matflt_type(structure_in%turnsign, structure_out%turnsign)
     if (verbose > 0) write(iu6, *) 'copied pfelement%turnsign'

     call copy_type_matflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied pfelement%area'

     call copy_type_pfgeometry(structure_in%pfgeometry, structure_out%pfgeometry)
     if (verbose > 0) write(iu6, *) 'copied pfelement%pfgeometry'

   end subroutine copy_type_pfelement

   subroutine copy_arr_type_pfelement(structure_in, structure_out)
 
     implicit none
 
     type (type_pfelement), pointer :: structure_in(:)
     type (type_pfelement), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfelement(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfelement'
     end if

   end subroutine copy_arr_type_pfelement

   subroutine copy_type_pfgeometry(structure_in, structure_out)

     implicit none

     type (type_pfgeometry), intent(in) :: structure_in
     type (type_pfgeometry), intent(inout) :: structure_out

     call copy_type_matint_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied pfgeometry%type'

     call copy_type_matint_type(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied pfgeometry%npoints'

     call copy_type_rz3D(structure_in%rzcoordinate, structure_out%rzcoordinate)
     if (verbose > 0) write(iu6, *) 'copied pfgeometry%rzcoordinate'

     call copy_type_array3dflt_type(structure_in%rzdrdz, structure_out%rzdrdz)
     if (verbose > 0) write(iu6, *) 'copied pfgeometry%rzdrdz'

   end subroutine copy_type_pfgeometry

   subroutine copy_arr_type_pfgeometry(structure_in, structure_out)
 
     implicit none
 
     type (type_pfgeometry), pointer :: structure_in(:)
     type (type_pfgeometry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfgeometry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfgeometry'
     end if

   end subroutine copy_arr_type_pfgeometry

   subroutine copy_type_pfpageometry(structure_in, structure_out)

     implicit none

     type (type_pfpageometry), intent(in) :: structure_in
     type (type_pfpageometry), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied pfpageometry%type'

     call copy_type_vecint_type(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied pfpageometry%npoints'

     call copy_type_rz2D(structure_in%rzcoordinate, structure_out%rzcoordinate)
     if (verbose > 0) write(iu6, *) 'copied pfpageometry%rzcoordinate'

     call copy_type_matflt_type(structure_in%rzdrdz, structure_out%rzdrdz)
     if (verbose > 0) write(iu6, *) 'copied pfpageometry%rzdrdz'

   end subroutine copy_type_pfpageometry

   subroutine copy_arr_type_pfpageometry(structure_in, structure_out)
 
     implicit none
 
     type (type_pfpageometry), pointer :: structure_in(:)
     type (type_pfpageometry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfpageometry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfpageometry'
     end if

   end subroutine copy_arr_type_pfpageometry

   subroutine copy_type_pfpassive(structure_in, structure_out)

     implicit none

     type (type_pfpassive), intent(in) :: structure_in
     type (type_pfpassive), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied pfpassive%name'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied pfpassive%area'

     call copy_type_vecflt_type(structure_in%res, structure_out%res)
     if (verbose > 0) write(iu6, *) 'copied pfpassive%res'

     call copy_type_vecflt_type(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied pfpassive%eta'

     call copy_type_pfpassive_current(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied pfpassive%current'

     call copy_type_pfpageometry(structure_in%pfpageometry, structure_out%pfpageometry)
     if (verbose > 0) write(iu6, *) 'copied pfpassive%pfpageometry'

   end subroutine copy_type_pfpassive

   subroutine copy_arr_type_pfpassive(structure_in, structure_out)
 
     implicit none
 
     type (type_pfpassive), pointer :: structure_in(:)
     type (type_pfpassive), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfpassive(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfpassive'
     end if

   end subroutine copy_arr_type_pfpassive

   subroutine copy_type_pfpassive_current(structure_in, structure_out)

     implicit none

     type (type_pfpassive_current), intent(in) :: structure_in
     type (type_pfpassive_current), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%toroidal, structure_out%toroidal)
     if (verbose > 0) write(iu6, *) 'copied pfpassive_current%toroidal'

     call copy_type_exp1D(structure_in%poloidal, structure_out%poloidal)
     if (verbose > 0) write(iu6, *) 'copied pfpassive_current%poloidal'

   end subroutine copy_type_pfpassive_current

   subroutine copy_arr_type_pfpassive_current(structure_in, structure_out)
 
     implicit none
 
     type (type_pfpassive_current), pointer :: structure_in(:)
     type (type_pfpassive_current), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfpassive_current(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfpassive_current'
     end if

   end subroutine copy_arr_type_pfpassive_current

   subroutine copy_type_pfsupplies(structure_in, structure_out)

     implicit none

     type (type_pfsupplies), intent(in) :: structure_in
     type (type_pfsupplies), intent(inout) :: structure_out

     call copy_type_desc_supply(structure_in%desc_supply, structure_out%desc_supply)
     if (verbose > 0) write(iu6, *) 'copied pfsupplies%desc_supply'

     call copy_type_exp1D(structure_in%voltage, structure_out%voltage)
     if (verbose > 0) write(iu6, *) 'copied pfsupplies%voltage'

     call copy_type_exp1D(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied pfsupplies%current'

   end subroutine copy_type_pfsupplies

   subroutine copy_arr_type_pfsupplies(structure_in, structure_out)
 
     implicit none
 
     type (type_pfsupplies), pointer :: structure_in(:)
     type (type_pfsupplies), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pfsupplies(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pfsupplies'
     end if

   end subroutine copy_arr_type_pfsupplies

   subroutine copy_type_phaseellipse(structure_in, structure_out)

     implicit none

     type (type_phaseellipse), intent(in) :: structure_in
     type (type_phaseellipse), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%invcurvrad, structure_out%invcurvrad)
     if (verbose > 0) write(iu6, *) 'copied phaseellipse%invcurvrad'

     call copy_type_float(structure_in%angle, structure_out%angle)
     if (verbose > 0) write(iu6, *) 'copied phaseellipse%angle'

   end subroutine copy_type_phaseellipse

   subroutine copy_arr_type_phaseellipse(structure_in, structure_out)
 
     implicit none
 
     type (type_phaseellipse), pointer :: structure_in(:)
     type (type_phaseellipse), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_phaseellipse(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_phaseellipse'
     end if

   end subroutine copy_arr_type_phaseellipse

   subroutine copy_type_planecoil(structure_in, structure_out)

     implicit none

     type (type_planecoil), intent(in) :: structure_in
     type (type_planecoil), intent(inout) :: structure_out

     call copy_type_rz1D(structure_in%coordinates, structure_out%coordinates)
     if (verbose > 0) write(iu6, *) 'copied planecoil%coordinates'

     call copy_type_vecflt_type(structure_in%hlength, structure_out%hlength)
     if (verbose > 0) write(iu6, *) 'copied planecoil%hlength'

     call copy_type_vecflt_type(structure_in%radialhwidth, structure_out%radialhwidth)
     if (verbose > 0) write(iu6, *) 'copied planecoil%radialhwidth'

   end subroutine copy_type_planecoil

   subroutine copy_arr_type_planecoil(structure_in, structure_out)
 
     implicit none
 
     type (type_planecoil), pointer :: structure_in(:)
     type (type_planecoil), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_planecoil(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_planecoil'
     end if

   end subroutine copy_arr_type_planecoil

   subroutine copy_type_plasmaComplexType(structure_in, structure_out)

     implicit none

     type (type_plasmaComplexType), intent(in) :: structure_in
     type (type_plasmaComplexType), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%species, structure_out%species)
     if (verbose > 0) write(iu6, *) 'copied plasmaComplexType%species'

     call copy_type_matflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied plasmaComplexType%flux'

     call copy_type_matflt_type(structure_in%b, structure_out%b)
     if (verbose > 0) write(iu6, *) 'copied plasmaComplexType%b'

     call copy_type_matflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied plasmaComplexType%energy'

   end subroutine copy_type_plasmaComplexType

   subroutine copy_arr_type_plasmaComplexType(structure_in, structure_out)
 
     implicit none
 
     type (type_plasmaComplexType), pointer :: structure_in(:)
     type (type_plasmaComplexType), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_plasmaComplexType(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_plasmaComplexType'
     end if

   end subroutine copy_arr_type_plasmaComplexType

   subroutine copy_type_plasmaedge(structure_in, structure_out)

     implicit none

     type (type_plasmaedge), intent(in) :: structure_in
     type (type_plasmaedge), intent(inout) :: structure_out

     call copy_type_integer(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied plasmaedge%npoints'

     call copy_type_vecflt_type(structure_in%distance, structure_out%distance)
     if (verbose > 0) write(iu6, *) 'copied plasmaedge%distance'

     call copy_type_vecflt_type(structure_in%density, structure_out%density)
     if (verbose > 0) write(iu6, *) 'copied plasmaedge%density'

   end subroutine copy_type_plasmaedge

   subroutine copy_arr_type_plasmaedge(structure_in, structure_out)
 
     implicit none
 
     type (type_plasmaedge), pointer :: structure_in(:)
     type (type_plasmaedge), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_plasmaedge(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_plasmaedge'
     end if

   end subroutine copy_arr_type_plasmaedge

   subroutine copy_type_pol_decomp(structure_in, structure_out)

     implicit none

     type (type_pol_decomp), intent(in) :: structure_in
     type (type_pol_decomp), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%mpol, structure_out%mpol)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%mpol'

     call copy_type_array3dflt_type(structure_in%e_plus, structure_out%e_plus)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_plus'

     call copy_type_array3dflt_type(structure_in%e_plus_ph, structure_out%e_plus_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_plus_ph'

     call copy_type_array3dflt_type(structure_in%e_minus, structure_out%e_minus)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_minus'

     call copy_type_array3dflt_type(structure_in%e_minus_ph, structure_out%e_minus_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_minus_ph'

     call copy_type_array3dflt_type(structure_in%e_norm, structure_out%e_norm)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_norm'

     call copy_type_array3dflt_type(structure_in%e_norm_ph, structure_out%e_norm_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_norm_ph'

     call copy_type_array3dflt_type(structure_in%e_binorm, structure_out%e_binorm)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_binorm'

     call copy_type_array3dflt_type(structure_in%e_binorm_ph, structure_out%e_binorm_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_binorm_ph'

     call copy_type_array3dflt_type(structure_in%e_para, structure_out%e_para)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_para'

     call copy_type_array3dflt_type(structure_in%e_para_ph, structure_out%e_para_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%e_para_ph'

     call copy_type_array3dflt_type(structure_in%b_norm, structure_out%b_norm)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%b_norm'

     call copy_type_array3dflt_type(structure_in%b_norm_ph, structure_out%b_norm_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%b_norm_ph'

     call copy_type_array3dflt_type(structure_in%b_binorm, structure_out%b_binorm)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%b_binorm'

     call copy_type_array3dflt_type(structure_in%b_binorm_ph, structure_out%b_binorm_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%b_binorm_ph'

     call copy_type_array3dflt_type(structure_in%b_para, structure_out%b_para)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%b_para'

     call copy_type_array3dflt_type(structure_in%b_para_ph, structure_out%b_para_ph)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%b_para_ph'

     call copy_type_array3dflt_type(structure_in%k_perp, structure_out%k_perp)
     if (verbose > 0) write(iu6, *) 'copied pol_decomp%k_perp'

   end subroutine copy_type_pol_decomp

   subroutine copy_arr_type_pol_decomp(structure_in, structure_out)
 
     implicit none
 
     type (type_pol_decomp), pointer :: structure_in(:)
     type (type_pol_decomp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_pol_decomp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_pol_decomp'
     end if

   end subroutine copy_arr_type_pol_decomp

   subroutine copy_type_polarimetry(structure_in, structure_out)

     implicit none

     type (type_polarimetry), intent(in) :: structure_in
     type (type_polarimetry), intent(inout) :: structure_out

     call copy_type_msediag_setup_polarimetry(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied polarimetry%setup'

     call copy_type_exp1D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied polarimetry%measure'

   end subroutine copy_type_polarimetry

   subroutine copy_arr_type_polarimetry(structure_in, structure_out)
 
     implicit none
 
     type (type_polarimetry), pointer :: structure_in(:)
     type (type_polarimetry), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_polarimetry(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_polarimetry'
     end if

   end subroutine copy_arr_type_polarimetry

   subroutine copy_type_polarization(structure_in, structure_out)

     implicit none

     type (type_polarization), intent(in) :: structure_in
     type (type_polarization), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%epol_p_re, structure_out%epol_p_re)
     if (verbose > 0) write(iu6, *) 'copied polarization%epol_p_re'

     call copy_type_vecflt_type(structure_in%epol_p_im, structure_out%epol_p_im)
     if (verbose > 0) write(iu6, *) 'copied polarization%epol_p_im'

     call copy_type_vecflt_type(structure_in%epol_m_re, structure_out%epol_m_re)
     if (verbose > 0) write(iu6, *) 'copied polarization%epol_m_re'

     call copy_type_vecflt_type(structure_in%epol_m_im, structure_out%epol_m_im)
     if (verbose > 0) write(iu6, *) 'copied polarization%epol_m_im'

     call copy_type_vecflt_type(structure_in%epol_par_re, structure_out%epol_par_re)
     if (verbose > 0) write(iu6, *) 'copied polarization%epol_par_re'

     call copy_type_vecflt_type(structure_in%epol_par_im, structure_out%epol_par_im)
     if (verbose > 0) write(iu6, *) 'copied polarization%epol_par_im'

   end subroutine copy_type_polarization

   subroutine copy_arr_type_polarization(structure_in, structure_out)
 
     implicit none
 
     type (type_polarization), pointer :: structure_in(:)
     type (type_polarization), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_polarization(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_polarization'
     end if

   end subroutine copy_arr_type_polarization

   subroutine copy_type_power_conv_component(structure_in, structure_out)

     implicit none

     type (type_power_conv_component), intent(in) :: structure_in
     type (type_power_conv_component), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%name'

     call copy_type_float(structure_in%temp_in, structure_out%temp_in)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%temp_in'

     call copy_type_float(structure_in%temp_out, structure_out%temp_out)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%temp_out'

     call copy_type_float(structure_in%press_in, structure_out%press_in)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%press_in'

     call copy_type_float(structure_in%press_out, structure_out%press_out)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%press_out'

     call copy_type_float(structure_in%power, structure_out%power)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%power'

     call copy_type_float(structure_in%flow, structure_out%flow)
     if (verbose > 0) write(iu6, *) 'copied power_conv_component%flow'

   end subroutine copy_type_power_conv_component

   subroutine copy_arr_type_power_conv_component(structure_in, structure_out)
 
     implicit none
 
     type (type_power_conv_component), pointer :: structure_in(:)
     type (type_power_conv_component), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_power_conv_component(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_power_conv_component'
     end if

   end subroutine copy_arr_type_power_conv_component

   subroutine copy_type_power_exchange(structure_in, structure_out)

     implicit none

     type (type_power_exchange), intent(in) :: structure_in
     type (type_power_exchange), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%dep_pow, structure_out%dep_pow)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_pow'

     call copy_type_float(structure_in%dep_fw, structure_out%dep_fw)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_fw'

     call copy_type_float(structure_in%dep_sg, structure_out%dep_sg)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_sg'

     call copy_type_float(structure_in%dep_cp, structure_out%dep_cp)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_cp'

     call copy_type_float(structure_in%dep_lp, structure_out%dep_lp)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_lp'

     call copy_type_float(structure_in%dep_man, structure_out%dep_man)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_man'

     call copy_type_float(structure_in%dep_pl, structure_out%dep_pl)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%dep_pl'

     call copy_type_float(structure_in%rec_fw, structure_out%rec_fw)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%rec_fw'

     call copy_type_float(structure_in%rec_sg, structure_out%rec_sg)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%rec_sg'

     call copy_type_float(structure_in%rec_cp, structure_out%rec_cp)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%rec_cp'

     call copy_type_float(structure_in%pow_dens_fw, structure_out%pow_dens_fw)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%pow_dens_fw'

     call copy_type_float(structure_in%pow_dens_bz, structure_out%pow_dens_bz)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%pow_dens_bz'

     call copy_type_float(structure_in%pow_dens_bz10, structure_out%pow_dens_bz10)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%pow_dens_bz10'

     call copy_type_float(structure_in%pow_dens_bp, structure_out%pow_dens_bp)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%pow_dens_bp'

     call copy_type_float(structure_in%pow_dens_man, structure_out%pow_dens_man)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%pow_dens_man'

     call copy_type_float(structure_in%pow_dens_sh, structure_out%pow_dens_sh)
     if (verbose > 0) write(iu6, *) 'copied power_exchange%pow_dens_sh'

   end subroutine copy_type_power_exchange

   subroutine copy_arr_type_power_exchange(structure_in, structure_out)
 
     implicit none
 
     type (type_power_exchange), pointer :: structure_in(:)
     type (type_power_exchange), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_power_exchange(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_power_exchange'
     end if

   end subroutine copy_arr_type_power_exchange

   subroutine copy_type_powerflow(structure_in, structure_out)

     implicit none

     type (type_powerflow), intent(in) :: structure_in
     type (type_powerflow), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%phi_perp, structure_out%phi_perp)
     if (verbose > 0) write(iu6, *) 'copied powerflow%phi_perp'

     call copy_type_vecflt_type(structure_in%phi_par, structure_out%phi_par)
     if (verbose > 0) write(iu6, *) 'copied powerflow%phi_par'

     call copy_type_vecflt_type(structure_in%power_e, structure_out%power_e)
     if (verbose > 0) write(iu6, *) 'copied powerflow%power_e'

     call copy_type_matflt_type(structure_in%power_i, structure_out%power_i)
     if (verbose > 0) write(iu6, *) 'copied powerflow%power_i'

   end subroutine copy_type_powerflow

   subroutine copy_arr_type_powerflow(structure_in, structure_out)
 
     implicit none
 
     type (type_powerflow), pointer :: structure_in(:)
     type (type_powerflow), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_powerflow(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_powerflow'
     end if

   end subroutine copy_arr_type_powerflow

   subroutine copy_type_profiles1d(structure_in, structure_out)

     implicit none

     type (type_profiles1d), intent(in) :: structure_in
     type (type_profiles1d), intent(inout) :: structure_out

     call copy_type_coreprofile(structure_in%pe, structure_out%pe)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%pe'

     call copy_type_coreprofile(structure_in%dpedt, structure_out%dpedt)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%dpedt'

     call copy_type_coreprofion(structure_in%pi, structure_out%pi)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%pi'

     call copy_type_coreprofile(structure_in%pi_tot, structure_out%pi_tot)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%pi_tot'

     call copy_type_coreprofile(structure_in%dpi_totdt, structure_out%dpi_totdt)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%dpi_totdt'

     call copy_type_coreprofile(structure_in%pr_th, structure_out%pr_th)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%pr_th'

     call copy_type_coreprofile(structure_in%pr_perp, structure_out%pr_perp)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%pr_perp'

     call copy_type_coreprofile(structure_in%pr_parallel, structure_out%pr_parallel)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%pr_parallel'

     call copy_type_coreprofile(structure_in%jtot, structure_out%jtot)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%jtot'

     call copy_type_coreprofile(structure_in%jni, structure_out%jni)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%jni'

     call copy_type_coreprofile(structure_in%jphi, structure_out%jphi)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%jphi'

     call copy_type_coreprofile(structure_in%joh, structure_out%joh)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%joh'

     call copy_type_coreprofile(structure_in%vloop, structure_out%vloop)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%vloop'

     call copy_type_coreprofile(structure_in%sigmapar, structure_out%sigmapar)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%sigmapar'

     call copy_type_sourceel(structure_in%qoh, structure_out%qoh)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%qoh'

     call copy_type_coreprofile(structure_in%qei, structure_out%qei)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%qei'

     call copy_type_coreprofile(structure_in%eparallel, structure_out%eparallel)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%eparallel'

     call copy_type_coreprofile(structure_in%e_b, structure_out%e_b)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%e_b'

     call copy_type_coreprofile(structure_in%q, structure_out%q)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%q'

     call copy_type_coreprofile(structure_in%shear, structure_out%shear)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%shear'

     call copy_type_coreprofion(structure_in%ns, structure_out%ns)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%ns'

     call copy_type_coreprofion(structure_in%mtor, structure_out%mtor)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%mtor'

     call copy_type_coreprofion(structure_in%wtor, structure_out%wtor)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%wtor'

     call copy_type_coreprofion(structure_in%vpol, structure_out%vpol)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%vpol'

     call copy_type_coreprofile(structure_in%zeff, structure_out%zeff)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%zeff'

     call copy_type_coreprofile(structure_in%bpol, structure_out%bpol)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%bpol'

     call copy_type_coreprofile(structure_in%dvprimedt, structure_out%dvprimedt)
     if (verbose > 0) write(iu6, *) 'copied profiles1d%dvprimedt'

   end subroutine copy_type_profiles1d

   subroutine copy_arr_type_profiles1d(structure_in, structure_out)
 
     implicit none
 
     type (type_profiles1d), pointer :: structure_in(:)
     type (type_profiles1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_profiles1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_profiles1d'
     end if

   end subroutine copy_arr_type_profiles1d

   subroutine copy_type_profiles_1d(structure_in, structure_out)

     implicit none

     type (type_profiles_1d), intent(in) :: structure_in
     type (type_profiles_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%psi'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%phi'

     call copy_type_vecflt_type(structure_in%pressure, structure_out%pressure)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%pressure'

     call copy_type_vecflt_type(structure_in%F_dia, structure_out%F_dia)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%F_dia'

     call copy_type_vecflt_type(structure_in%pprime, structure_out%pprime)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%pprime'

     call copy_type_vecflt_type(structure_in%ffprime, structure_out%ffprime)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%ffprime'

     call copy_type_vecflt_type(structure_in%jphi, structure_out%jphi)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%jphi'

     call copy_type_vecflt_type(structure_in%jparallel, structure_out%jparallel)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%jparallel'

     call copy_type_vecflt_type(structure_in%q, structure_out%q)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%q'

     call copy_type_vecflt_type(structure_in%shear, structure_out%shear)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%shear'

     call copy_type_vecflt_type(structure_in%r_inboard, structure_out%r_inboard)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%r_inboard'

     call copy_type_vecflt_type(structure_in%r_outboard, structure_out%r_outboard)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%r_outboard'

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%rho_tor'

     call copy_type_vecflt_type(structure_in%dpsidrho_tor, structure_out%dpsidrho_tor)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%dpsidrho_tor'

     call copy_type_vecflt_type(structure_in%rho_vol, structure_out%rho_vol)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%rho_vol'

     call copy_type_vecflt_type(structure_in%beta_pol, structure_out%beta_pol)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%beta_pol'

     call copy_type_vecflt_type(structure_in%li, structure_out%li)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%li'

     call copy_type_vecflt_type(structure_in%elongation, structure_out%elongation)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%elongation'

     call copy_type_vecflt_type(structure_in%tria_upper, structure_out%tria_upper)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%tria_upper'

     call copy_type_vecflt_type(structure_in%tria_lower, structure_out%tria_lower)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%tria_lower'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%volume'

     call copy_type_vecflt_type(structure_in%vprime, structure_out%vprime)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%vprime'

     call copy_type_vecflt_type(structure_in%dvdrho, structure_out%dvdrho)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%dvdrho'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%area'

     call copy_type_vecflt_type(structure_in%aprime, structure_out%aprime)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%aprime'

     call copy_type_vecflt_type(structure_in%surface, structure_out%surface)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%surface'

     call copy_type_vecflt_type(structure_in%ftrap, structure_out%ftrap)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%ftrap'

     call copy_type_vecflt_type(structure_in%gm1, structure_out%gm1)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm1'

     call copy_type_vecflt_type(structure_in%gm2, structure_out%gm2)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm2'

     call copy_type_vecflt_type(structure_in%gm3, structure_out%gm3)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm3'

     call copy_type_vecflt_type(structure_in%gm4, structure_out%gm4)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm4'

     call copy_type_vecflt_type(structure_in%gm5, structure_out%gm5)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm5'

     call copy_type_vecflt_type(structure_in%gm6, structure_out%gm6)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm6'

     call copy_type_vecflt_type(structure_in%gm7, structure_out%gm7)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm7'

     call copy_type_vecflt_type(structure_in%gm8, structure_out%gm8)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm8'

     call copy_type_vecflt_type(structure_in%gm9, structure_out%gm9)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%gm9'

     call copy_type_vecflt_type(structure_in%b_av, structure_out%b_av)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%b_av'

     call copy_type_vecflt_type(structure_in%b_min, structure_out%b_min)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%b_min'

     call copy_type_vecflt_type(structure_in%b_max, structure_out%b_max)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%b_max'

     call copy_type_vecflt_type(structure_in%omega, structure_out%omega)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%omega'

     call copy_type_vecflt_type(structure_in%omegaprime, structure_out%omegaprime)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%omegaprime'

     call copy_type_vecflt_type(structure_in%mach_a, structure_out%mach_a)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%mach_a'

     call copy_type_vecflt_type(structure_in%phi_flow, structure_out%phi_flow)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%phi_flow'

     call copy_type_vecflt_type(structure_in%s_flow, structure_out%s_flow)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%s_flow'

     call copy_type_vecflt_type(structure_in%h_flow, structure_out%h_flow)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%h_flow'

     call copy_type_vecflt_type(structure_in%rho_mass, structure_out%rho_mass)
     if (verbose > 0) write(iu6, *) 'copied profiles_1d%rho_mass'

   end subroutine copy_type_profiles_1d

   subroutine copy_arr_type_profiles_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_profiles_1d), pointer :: structure_in(:)
     type (type_profiles_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_profiles_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_profiles_1d'
     end if

   end subroutine copy_arr_type_profiles_1d

   subroutine copy_type_psi(structure_in, structure_out)

     implicit none

     type (type_psi), intent(in) :: structure_in
     type (type_psi), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied psi%value'

     call copy_type_vecflt_type(structure_in%ddrho, structure_out%ddrho)
     if (verbose > 0) write(iu6, *) 'copied psi%ddrho'

     call copy_type_vecflt_type(structure_in%d2drho2, structure_out%d2drho2)
     if (verbose > 0) write(iu6, *) 'copied psi%d2drho2'

     call copy_type_vecflt_type(structure_in%ddt_rhotorn, structure_out%ddt_rhotorn)
     if (verbose > 0) write(iu6, *) 'copied psi%ddt_rhotorn'

     call copy_type_vecflt_type(structure_in%ddt_phi, structure_out%ddt_phi)
     if (verbose > 0) write(iu6, *) 'copied psi%ddt_phi'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied psi%source'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied psi%flag'

     call copy_type_boundary(structure_in%boundary, structure_out%boundary)
     if (verbose > 0) write(iu6, *) 'copied psi%boundary'

     call copy_type_jni(structure_in%jni, structure_out%jni)
     if (verbose > 0) write(iu6, *) 'copied psi%jni'

     call copy_type_coreprofile(structure_in%sigma_par, structure_out%sigma_par)
     if (verbose > 0) write(iu6, *) 'copied psi%sigma_par'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied psi%codeparam'

   end subroutine copy_type_psi

   subroutine copy_arr_type_psi(structure_in, structure_out)
 
     implicit none
 
     type (type_psi), pointer :: structure_in(:)
     type (type_psi), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_psi(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_psi'
     end if

   end subroutine copy_arr_type_psi

   subroutine copy_type_putinfo(structure_in, structure_out)

     implicit none

     type (type_putinfo), intent(in) :: structure_in
     type (type_putinfo), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%putmethod, structure_out%putmethod)
     if (verbose > 0) write(iu6, *) 'copied putinfo%putmethod'

     call copy_type_vecstring_type(structure_in%putaccess, structure_out%putaccess)
     if (verbose > 0) write(iu6, *) 'copied putinfo%putaccess'

     call copy_type_vecstring_type(structure_in%putlocation, structure_out%putlocation)
     if (verbose > 0) write(iu6, *) 'copied putinfo%putlocation'

     call copy_type_vecstring_type(structure_in%rights, structure_out%rights)
     if (verbose > 0) write(iu6, *) 'copied putinfo%rights'

   end subroutine copy_type_putinfo

   subroutine copy_arr_type_putinfo(structure_in, structure_out)
 
     implicit none
 
     type (type_putinfo), pointer :: structure_in(:)
     type (type_putinfo), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_putinfo(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_putinfo'
     end if

   end subroutine copy_arr_type_putinfo

   subroutine copy_type_q(structure_in, structure_out)

     implicit none

     type (type_q), intent(in) :: structure_in
     type (type_q), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%qvalue, structure_out%qvalue)
     if (verbose > 0) write(iu6, *) 'copied q%qvalue'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied q%position'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied q%source'

     call copy_type_integer(structure_in%exact, structure_out%exact)
     if (verbose > 0) write(iu6, *) 'copied q%exact'

     call copy_type_vecflt_type(structure_in%weight, structure_out%weight)
     if (verbose > 0) write(iu6, *) 'copied q%weight'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied q%sigma'

     call copy_type_vecflt_type(structure_in%calculated, structure_out%calculated)
     if (verbose > 0) write(iu6, *) 'copied q%calculated'

     call copy_type_vecflt_type(structure_in%chi2, structure_out%chi2)
     if (verbose > 0) write(iu6, *) 'copied q%chi2'

   end subroutine copy_type_q

   subroutine copy_arr_type_q(structure_in, structure_out)
 
     implicit none
 
     type (type_q), pointer :: structure_in(:)
     type (type_q), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_q(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_q'
     end if

   end subroutine copy_arr_type_q

   subroutine copy_type_reacprodType(structure_in, structure_out)

     implicit none

     type (type_reacprodType), intent(in) :: structure_in
     type (type_reacprodType), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%label'

     call copy_arr_type_amns_constituentType(structure_in%constituents, structure_out%constituents)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%constituents'

     call copy_type_identifier(structure_in%role, structure_out%role)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%role'

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%amn'

     call copy_type_integer(structure_in%relative, structure_out%relative)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%relative'

     call copy_type_float(structure_in%za, structure_out%za)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%za'

     call copy_type_float(structure_in%multiplicity, structure_out%multiplicity)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%multiplicity'

     call copy_type_vecint_type(structure_in%metastable, structure_out%metastable)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%metastable'

     call copy_type_vecstring_type(structure_in%metastable_label, structure_out%metastable_label)
     if (verbose > 0) write(iu6, *) 'copied reacprodType%metastable_label'

   end subroutine copy_type_reacprodType

   subroutine copy_arr_type_reacprodType(structure_in, structure_out)
 
     implicit none
 
     type (type_reacprodType), pointer :: structure_in(:)
     type (type_reacprodType), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reacprodType(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reacprodType'
     end if

   end subroutine copy_arr_type_reacprodType

   subroutine copy_type_react(structure_in, structure_out)

     implicit none

     type (type_react), intent(in) :: structure_in
     type (type_react), intent(inout) :: structure_out

     call copy_type_float(structure_in%he_fr, structure_out%he_fr)
     if (verbose > 0) write(iu6, *) 'copied react%he_fr'

     call copy_type_float(structure_in%lp_fr, structure_out%lp_fr)
     if (verbose > 0) write(iu6, *) 'copied react%lp_fr'

     call copy_type_float(structure_in%he_dp, structure_out%he_dp)
     if (verbose > 0) write(iu6, *) 'copied react%he_dp'

     call copy_type_float(structure_in%lipb_dp, structure_out%lipb_dp)
     if (verbose > 0) write(iu6, *) 'copied react%lipb_dp'

   end subroutine copy_type_react

   subroutine copy_arr_type_react(structure_in, structure_out)
 
     implicit none
 
     type (type_react), pointer :: structure_in(:)
     type (type_react), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_react(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_react'
     end if

   end subroutine copy_arr_type_react

   subroutine copy_type_rectanglexyz(structure_in, structure_out)

     implicit none

     type (type_rectanglexyz), intent(in) :: structure_in
     type (type_rectanglexyz), intent(inout) :: structure_out

     call copy_type_xyz0D(structure_in%point01, structure_out%point01)
     if (verbose > 0) write(iu6, *) 'copied rectanglexyz%point01'

     call copy_type_xyz0D(structure_in%point11, structure_out%point11)
     if (verbose > 0) write(iu6, *) 'copied rectanglexyz%point11'

     call copy_type_xyz0D(structure_in%point10, structure_out%point10)
     if (verbose > 0) write(iu6, *) 'copied rectanglexyz%point10'

   end subroutine copy_type_rectanglexyz

   subroutine copy_arr_type_rectanglexyz(structure_in, structure_out)
 
     implicit none
 
     type (type_rectanglexyz), pointer :: structure_in(:)
     type (type_rectanglexyz), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rectanglexyz(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rectanglexyz'
     end if

   end subroutine copy_arr_type_rectanglexyz

   subroutine copy_type_recycling_neutrals(structure_in, structure_out)

     implicit none

     type (type_recycling_neutrals), intent(in) :: structure_in
     type (type_recycling_neutrals), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%particles, structure_out%particles)
     if (verbose > 0) write(iu6, *) 'copied recycling_neutrals%particles'

     call copy_type_vecflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied recycling_neutrals%energy'

   end subroutine copy_type_recycling_neutrals

   subroutine copy_arr_type_recycling_neutrals(structure_in, structure_out)
 
     implicit none
 
     type (type_recycling_neutrals), pointer :: structure_in(:)
     type (type_recycling_neutrals), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_recycling_neutrals(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_recycling_neutrals'
     end if

   end subroutine copy_arr_type_recycling_neutrals

   subroutine copy_type_reduced(structure_in, structure_out)

     implicit none

     type (type_reduced), intent(in) :: structure_in
     type (type_reduced), intent(inout) :: structure_out

     call copy_type_float(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied reduced%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied reduced%source'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied reduced%time'

   end subroutine copy_type_reduced

   subroutine copy_arr_type_reduced(structure_in, structure_out)
 
     implicit none
 
     type (type_reduced), pointer :: structure_in(:)
     type (type_reduced), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reduced(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reduced'
     end if

   end subroutine copy_arr_type_reduced

   subroutine copy_type_refl_receive(structure_in, structure_out)

     implicit none

     type (type_refl_receive), intent(in) :: structure_in
     type (type_refl_receive), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied refl_receive%name'

     call copy_type_t_series_real(structure_in%raw_signal, structure_out%raw_signal)
     if (verbose > 0) write(iu6, *) 'copied refl_receive%raw_signal'

     call copy_type_t_series_real(structure_in%io_signal, structure_out%io_signal)
     if (verbose > 0) write(iu6, *) 'copied refl_receive%io_signal'

     call copy_type_t_series_cplx(structure_in%iq_receiver, structure_out%iq_receiver)
     if (verbose > 0) write(iu6, *) 'copied refl_receive%iq_receiver'

     call copy_type_integer(structure_in%antenna_ind, structure_out%antenna_ind)
     if (verbose > 0) write(iu6, *) 'copied refl_receive%antenna_ind'

   end subroutine copy_type_refl_receive

   subroutine copy_arr_type_refl_receive(structure_in, structure_out)
 
     implicit none
 
     type (type_refl_receive), pointer :: structure_in(:)
     type (type_refl_receive), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_refl_receive(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_refl_receive'
     end if

   end subroutine copy_arr_type_refl_receive

   subroutine copy_type_reflectometry_antennas(structure_in, structure_out)

     implicit none

     type (type_reflectometry_antennas), intent(in) :: structure_in
     type (type_reflectometry_antennas), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_antennas%name'

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_antennas%type'

     call copy_type_origin(structure_in%origin, structure_out%origin)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_antennas%origin'

     call copy_type_reflectometry_radfield(structure_in%radfield, structure_out%radfield)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_antennas%radfield'

     call copy_type_float(structure_in%geometry, structure_out%geometry)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_antennas%geometry'

     call copy_type_launchsignal(structure_in%launchsignal, structure_out%launchsignal)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_antennas%launchsignal'

   end subroutine copy_type_reflectometry_antennas

   subroutine copy_arr_type_reflectometry_antennas(structure_in, structure_out)
 
     implicit none
 
     type (type_reflectometry_antennas), pointer :: structure_in(:)
     type (type_reflectometry_antennas), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reflectometry_antennas(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reflectometry_antennas'
     end if

   end subroutine copy_arr_type_reflectometry_antennas

   subroutine copy_type_reflectometry_radfield(structure_in, structure_out)

     implicit none

     type (type_reflectometry_radfield), intent(in) :: structure_in
     type (type_reflectometry_radfield), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield%type'

     call copy_type_vecflt_type(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield%position'

     call copy_arr_type_reflectometry_radfield_gaussian(structure_in%gaussian, structure_out%gaussian)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield%gaussian'

     call copy_arr_type_reflectometry_radifield_efield(structure_in%efield, structure_out%efield)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield%efield'

   end subroutine copy_type_reflectometry_radfield

   subroutine copy_arr_type_reflectometry_radfield(structure_in, structure_out)
 
     implicit none
 
     type (type_reflectometry_radfield), pointer :: structure_in(:)
     type (type_reflectometry_radfield), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reflectometry_radfield(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reflectometry_radfield'
     end if

   end subroutine copy_arr_type_reflectometry_radfield

   subroutine copy_type_reflectometry_radfield_gaussian(structure_in, structure_out)

     implicit none

     type (type_reflectometry_radfield_gaussian), intent(in) :: structure_in
     type (type_reflectometry_radfield_gaussian), intent(inout) :: structure_out

     call copy_type_simp_apert(structure_in%aperture, structure_out%aperture)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield_gaussian%aperture'

     call copy_type_vecflt_type(structure_in%waistsize, structure_out%waistsize)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield_gaussian%waistsize'

     call copy_type_vecflt_type(structure_in%waistzpos, structure_out%waistzpos)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield_gaussian%waistzpos'

     call copy_type_vecflt_type(structure_in%tiltangle, structure_out%tiltangle)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield_gaussian%tiltangle'

     call copy_type_vecflt_type(structure_in%polar_angle, structure_out%polar_angle)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield_gaussian%polar_angle'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radfield_gaussian%frequency'

   end subroutine copy_type_reflectometry_radfield_gaussian

   subroutine copy_arr_type_reflectometry_radfield_gaussian(structure_in, structure_out)
 
     implicit none
 
     type (type_reflectometry_radfield_gaussian), pointer :: structure_in(:)
     type (type_reflectometry_radfield_gaussian), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reflectometry_radfield_gaussian(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reflectometry_radfield_gaussian'
     end if

   end subroutine copy_arr_type_reflectometry_radfield_gaussian

   subroutine copy_type_reflectometry_radifield_efield(structure_in, structure_out)

     implicit none

     type (type_reflectometry_radifield_efield), intent(in) :: structure_in
     type (type_reflectometry_radifield_efield), intent(inout) :: structure_out

     call copy_type_reggrid(structure_in%grid2d, structure_out%grid2d)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radifield_efield%grid2d'

     call copy_type_matcplx_type(structure_in%e1, structure_out%e1)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radifield_efield%e1'

     call copy_type_matcplx_type(structure_in%e2, structure_out%e2)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radifield_efield%e2'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied reflectometry_radifield_efield%frequency'

   end subroutine copy_type_reflectometry_radifield_efield

   subroutine copy_arr_type_reflectometry_radifield_efield(structure_in, structure_out)
 
     implicit none
 
     type (type_reflectometry_radifield_efield), pointer :: structure_in(:)
     type (type_reflectometry_radifield_efield), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reflectometry_radifield_efield(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reflectometry_radifield_efield'
     end if

   end subroutine copy_arr_type_reflectometry_radifield_efield

   subroutine copy_type_reggrid(structure_in, structure_out)

     implicit none

     type (type_reggrid), intent(in) :: structure_in
     type (type_reggrid), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%dim1, structure_out%dim1)
     if (verbose > 0) write(iu6, *) 'copied reggrid%dim1'

     call copy_type_vecflt_type(structure_in%dim2, structure_out%dim2)
     if (verbose > 0) write(iu6, *) 'copied reggrid%dim2'

   end subroutine copy_type_reggrid

   subroutine copy_arr_type_reggrid(structure_in, structure_out)
 
     implicit none
 
     type (type_reggrid), pointer :: structure_in(:)
     type (type_reggrid), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_reggrid(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_reggrid'
     end if

   end subroutine copy_arr_type_reggrid

   subroutine copy_type_rfameasure(structure_in, structure_out)

     implicit none

     type (type_rfameasure), intent(in) :: structure_in
     type (type_rfameasure), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied rfameasure%ti'

   end subroutine copy_type_rfameasure

   subroutine copy_arr_type_rfameasure(structure_in, structure_out)
 
     implicit none
 
     type (type_rfameasure), pointer :: structure_in(:)
     type (type_rfameasure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rfameasure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rfameasure'
     end if

   end subroutine copy_arr_type_rfameasure

   subroutine copy_type_rfasetup(structure_in, structure_out)

     implicit none

     type (type_rfasetup), intent(in) :: structure_in
     type (type_rfasetup), intent(inout) :: structure_out

     call copy_type_rzphi1Dexp(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied rfasetup%position'

   end subroutine copy_type_rfasetup

   subroutine copy_arr_type_rfasetup(structure_in, structure_out)
 
     implicit none
 
     type (type_rfasetup), pointer :: structure_in(:)
     type (type_rfasetup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rfasetup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rfasetup'
     end if

   end subroutine copy_arr_type_rfasetup

   subroutine copy_type_rfbeam(structure_in, structure_out)

     implicit none

     type (type_rfbeam), intent(in) :: structure_in
     type (type_rfbeam), intent(inout) :: structure_out

     call copy_type_spot(structure_in%spot, structure_out%spot)
     if (verbose > 0) write(iu6, *) 'copied rfbeam%spot'

     call copy_type_phaseellipse(structure_in%phaseellipse, structure_out%phaseellipse)
     if (verbose > 0) write(iu6, *) 'copied rfbeam%phaseellipse'

   end subroutine copy_type_rfbeam

   subroutine copy_arr_type_rfbeam(structure_in, structure_out)
 
     implicit none
 
     type (type_rfbeam), pointer :: structure_in(:)
     type (type_rfbeam), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rfbeam(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rfbeam'
     end if

   end subroutine copy_arr_type_rfbeam

   subroutine copy_type_rz0D(structure_in, structure_out)

     implicit none

     type (type_rz0D), intent(in) :: structure_in
     type (type_rz0D), intent(inout) :: structure_out

     call copy_type_float(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rz0D%r'

     call copy_type_float(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rz0D%z'

   end subroutine copy_type_rz0D

   subroutine copy_arr_type_rz0D(structure_in, structure_out)
 
     implicit none
 
     type (type_rz0D), pointer :: structure_in(:)
     type (type_rz0D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rz0D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rz0D'
     end if

   end subroutine copy_arr_type_rz0D

   subroutine copy_type_rz1D(structure_in, structure_out)

     implicit none

     type (type_rz1D), intent(in) :: structure_in
     type (type_rz1D), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rz1D%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rz1D%z'

   end subroutine copy_type_rz1D

   subroutine copy_arr_type_rz1D(structure_in, structure_out)
 
     implicit none
 
     type (type_rz1D), pointer :: structure_in(:)
     type (type_rz1D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rz1D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rz1D'
     end if

   end subroutine copy_arr_type_rz1D

   subroutine copy_type_rz1D_npoints(structure_in, structure_out)

     implicit none

     type (type_rz1D_npoints), intent(in) :: structure_in
     type (type_rz1D_npoints), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rz1D_npoints%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rz1D_npoints%z'

     call copy_type_integer(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied rz1D_npoints%npoints'

   end subroutine copy_type_rz1D_npoints

   subroutine copy_arr_type_rz1D_npoints(structure_in, structure_out)
 
     implicit none
 
     type (type_rz1D_npoints), pointer :: structure_in(:)
     type (type_rz1D_npoints), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rz1D_npoints(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rz1D_npoints'
     end if

   end subroutine copy_arr_type_rz1D_npoints

   subroutine copy_type_rz1Dexp(structure_in, structure_out)

     implicit none

     type (type_rz1Dexp), intent(in) :: structure_in
     type (type_rz1Dexp), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rz1Dexp%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rz1Dexp%z'

   end subroutine copy_type_rz1Dexp

   subroutine copy_arr_type_rz1Dexp(structure_in, structure_out)
 
     implicit none
 
     type (type_rz1Dexp), pointer :: structure_in(:)
     type (type_rz1Dexp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rz1Dexp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rz1Dexp'
     end if

   end subroutine copy_arr_type_rz1Dexp

   subroutine copy_type_rz2D(structure_in, structure_out)

     implicit none

     type (type_rz2D), intent(in) :: structure_in
     type (type_rz2D), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rz2D%r'

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rz2D%z'

   end subroutine copy_type_rz2D

   subroutine copy_arr_type_rz2D(structure_in, structure_out)
 
     implicit none
 
     type (type_rz2D), pointer :: structure_in(:)
     type (type_rz2D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rz2D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rz2D'
     end if

   end subroutine copy_arr_type_rz2D

   subroutine copy_type_rz3D(structure_in, structure_out)

     implicit none

     type (type_rz3D), intent(in) :: structure_in
     type (type_rz3D), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rz3D%r'

     call copy_type_array3dflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rz3D%z'

   end subroutine copy_type_rz3D

   subroutine copy_arr_type_rz3D(structure_in, structure_out)
 
     implicit none
 
     type (type_rz3D), pointer :: structure_in(:)
     type (type_rz3D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rz3D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rz3D'
     end if

   end subroutine copy_arr_type_rz3D

   subroutine copy_type_rzphi0D(structure_in, structure_out)

     implicit none

     type (type_rzphi0D), intent(in) :: structure_in
     type (type_rzphi0D), intent(inout) :: structure_out

     call copy_type_float(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphi0D%r'

     call copy_type_float(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphi0D%z'

     call copy_type_float(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphi0D%phi'

   end subroutine copy_type_rzphi0D

   subroutine copy_arr_type_rzphi0D(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphi0D), pointer :: structure_in(:)
     type (type_rzphi0D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphi0D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphi0D'
     end if

   end subroutine copy_arr_type_rzphi0D

   subroutine copy_type_rzphi1D(structure_in, structure_out)

     implicit none

     type (type_rzphi1D), intent(in) :: structure_in
     type (type_rzphi1D), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphi1D%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphi1D%z'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphi1D%phi'

   end subroutine copy_type_rzphi1D

   subroutine copy_arr_type_rzphi1D(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphi1D), pointer :: structure_in(:)
     type (type_rzphi1D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphi1D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphi1D'
     end if

   end subroutine copy_arr_type_rzphi1D

   subroutine copy_type_rzphi1Dexp(structure_in, structure_out)

     implicit none

     type (type_rzphi1Dexp), intent(in) :: structure_in
     type (type_rzphi1Dexp), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphi1Dexp%r'

     call copy_type_exp1D(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphi1Dexp%z'

     call copy_type_exp1D(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphi1Dexp%phi'

   end subroutine copy_type_rzphi1Dexp

   subroutine copy_arr_type_rzphi1Dexp(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphi1Dexp), pointer :: structure_in(:)
     type (type_rzphi1Dexp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphi1Dexp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphi1Dexp'
     end if

   end subroutine copy_arr_type_rzphi1Dexp

   subroutine copy_type_rzphi1Dexperimental(structure_in, structure_out)

     implicit none

     type (type_rzphi1Dexperimental), intent(in) :: structure_in
     type (type_rzphi1Dexperimental), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphi1Dexperimental%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphi1Dexperimental%z'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphi1Dexperimental%phi'

   end subroutine copy_type_rzphi1Dexperimental

   subroutine copy_arr_type_rzphi1Dexperimental(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphi1Dexperimental), pointer :: structure_in(:)
     type (type_rzphi1Dexperimental), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphi1Dexperimental(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphi1Dexperimental'
     end if

   end subroutine copy_arr_type_rzphi1Dexperimental

   subroutine copy_type_rzphi2D(structure_in, structure_out)

     implicit none

     type (type_rzphi2D), intent(in) :: structure_in
     type (type_rzphi2D), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphi2D%r'

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphi2D%z'

     call copy_type_matflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphi2D%phi'

   end subroutine copy_type_rzphi2D

   subroutine copy_arr_type_rzphi2D(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphi2D), pointer :: structure_in(:)
     type (type_rzphi2D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphi2D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphi2D'
     end if

   end subroutine copy_arr_type_rzphi2D

   subroutine copy_type_rzphi3D(structure_in, structure_out)

     implicit none

     type (type_rzphi3D), intent(in) :: structure_in
     type (type_rzphi3D), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphi3D%r'

     call copy_type_array3dflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphi3D%z'

     call copy_type_array3dflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphi3D%phi'

   end subroutine copy_type_rzphi3D

   subroutine copy_arr_type_rzphi3D(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphi3D), pointer :: structure_in(:)
     type (type_rzphi3D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphi3D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphi3D'
     end if

   end subroutine copy_arr_type_rzphi3D

   subroutine copy_type_rzphidrdzdphi1D(structure_in, structure_out)

     implicit none

     type (type_rzphidrdzdphi1D), intent(in) :: structure_in
     type (type_rzphidrdzdphi1D), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied rzphidrdzdphi1D%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied rzphidrdzdphi1D%z'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied rzphidrdzdphi1D%phi'

     call copy_type_vecflt_type(structure_in%dr, structure_out%dr)
     if (verbose > 0) write(iu6, *) 'copied rzphidrdzdphi1D%dr'

     call copy_type_vecflt_type(structure_in%dz, structure_out%dz)
     if (verbose > 0) write(iu6, *) 'copied rzphidrdzdphi1D%dz'

     call copy_type_vecflt_type(structure_in%dphi, structure_out%dphi)
     if (verbose > 0) write(iu6, *) 'copied rzphidrdzdphi1D%dphi'

   end subroutine copy_type_rzphidrdzdphi1D

   subroutine copy_arr_type_rzphidrdzdphi1D(structure_in, structure_out)
 
     implicit none
 
     type (type_rzphidrdzdphi1D), pointer :: structure_in(:)
     type (type_rzphidrdzdphi1D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_rzphidrdzdphi1D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_rzphidrdzdphi1D'
     end if

   end subroutine copy_arr_type_rzphidrdzdphi1D

   subroutine copy_type_sawteeth_diags(structure_in, structure_out)

     implicit none

     type (type_sawteeth_diags), intent(in) :: structure_in
     type (type_sawteeth_diags), intent(inout) :: structure_out

     call copy_type_float(structure_in%shear1, structure_out%shear1)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_diags%shear1'

     call copy_type_float(structure_in%rhotorn_q1, structure_out%rhotorn_q1)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_diags%rhotorn_q1'

     call copy_type_float(structure_in%rhotorn_inv, structure_out%rhotorn_inv)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_diags%rhotorn_inv'

     call copy_type_float(structure_in%rhotorn_mix, structure_out%rhotorn_mix)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_diags%rhotorn_mix'

   end subroutine copy_type_sawteeth_diags

   subroutine copy_arr_type_sawteeth_diags(structure_in, structure_out)
 
     implicit none
 
     type (type_sawteeth_diags), pointer :: structure_in(:)
     type (type_sawteeth_diags), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sawteeth_diags(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sawteeth_diags'
     end if

   end subroutine copy_arr_type_sawteeth_diags

   subroutine copy_type_sawteeth_profiles1d(structure_in, structure_out)

     implicit none

     type (type_sawteeth_profiles1d), intent(in) :: structure_in
     type (type_sawteeth_profiles1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%ne'

     call copy_type_matflt_type(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%ni'

     call copy_type_vecflt_type(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%te'

     call copy_type_matflt_type(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%ti'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%psi'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%phi'

     call copy_type_vecflt_type(structure_in%psistar, structure_out%psistar)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%psistar'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%volume'

     call copy_type_vecflt_type(structure_in%q, structure_out%q)
     if (verbose > 0) write(iu6, *) 'copied sawteeth_profiles1d%q'

   end subroutine copy_type_sawteeth_profiles1d

   subroutine copy_arr_type_sawteeth_profiles1d(structure_in, structure_out)
 
     implicit none
 
     type (type_sawteeth_profiles1d), pointer :: structure_in(:)
     type (type_sawteeth_profiles1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sawteeth_profiles1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sawteeth_profiles1d'
     end if

   end subroutine copy_arr_type_sawteeth_profiles1d

   subroutine copy_type_scenario_centre(structure_in, structure_out)

     implicit none

     type (type_scenario_centre), intent(in) :: structure_in
     type (type_scenario_centre), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%te0, structure_out%te0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%te0'

     call copy_type_scenario_ref(structure_in%ti0, structure_out%ti0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%ti0'

     call copy_type_scenario_ref(structure_in%ne0, structure_out%ne0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%ne0'

     call copy_type_scenario_ref(structure_in%ni0, structure_out%ni0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%ni0'

     call copy_type_scenario_ref(structure_in%shift0, structure_out%shift0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%shift0'

     call copy_type_scenario_ref(structure_in%psi0, structure_out%psi0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%psi0'

     call copy_type_scenario_ref(structure_in%phi0, structure_out%phi0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%phi0'

     call copy_type_scenario_ref(structure_in%q0, structure_out%q0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%q0'

     call copy_type_scenario_ref(structure_in%Rmag, structure_out%Rmag)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%Rmag'

     call copy_type_scenario_ref(structure_in%Zmag, structure_out%Zmag)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%Zmag'

     call copy_type_scenario_ref(structure_in%vtor_0, structure_out%vtor_0)
     if (verbose > 0) write(iu6, *) 'copied scenario_centre%vtor_0'

   end subroutine copy_type_scenario_centre

   subroutine copy_arr_type_scenario_centre(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_centre), pointer :: structure_in(:)
     type (type_scenario_centre), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_centre(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_centre'
     end if

   end subroutine copy_arr_type_scenario_centre

   subroutine copy_type_scenario_composition(structure_in, structure_out)

     implicit none

     type (type_scenario_composition), intent(in) :: structure_in
     type (type_scenario_composition), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%amn'

     call copy_type_vecflt_type(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%zn'

     call copy_type_vecflt_type(structure_in%zion, structure_out%zion)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%zion'

     call copy_type_vecint_type(structure_in%imp_flag, structure_out%imp_flag)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%imp_flag'

     call copy_type_vecint_type(structure_in%rot_imp_flag, structure_out%rot_imp_flag)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%rot_imp_flag'

     call copy_type_vecflt_type(structure_in%pellet_amn, structure_out%pellet_amn)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%pellet_amn'

     call copy_type_vecflt_type(structure_in%pellet_zn, structure_out%pellet_zn)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%pellet_zn'

     call copy_type_vecflt_type(structure_in%nbi_amn, structure_out%nbi_amn)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%nbi_amn'

     call copy_type_vecflt_type(structure_in%nbi_zn, structure_out%nbi_zn)
     if (verbose > 0) write(iu6, *) 'copied scenario_composition%nbi_zn'

   end subroutine copy_type_scenario_composition

   subroutine copy_arr_type_scenario_composition(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_composition), pointer :: structure_in(:)
     type (type_scenario_composition), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_composition(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_composition'
     end if

   end subroutine copy_arr_type_scenario_composition

   subroutine copy_type_scenario_configuration(structure_in, structure_out)

     implicit none

     type (type_scenario_configuration), intent(in) :: structure_in
     type (type_scenario_configuration), intent(inout) :: structure_out

     call copy_type_scenario_int(structure_in%config, structure_out%config)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%config'

     call copy_type_vecstring_type(structure_in%lmode_sc, structure_out%lmode_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%lmode_sc'

     call copy_type_vecstring_type(structure_in%hmode_sc, structure_out%hmode_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%hmode_sc'

     call copy_type_vecstring_type(structure_in%core_sc, structure_out%core_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%core_sc'

     call copy_type_vecstring_type(structure_in%pedestal_sc, structure_out%pedestal_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%pedestal_sc'

     call copy_type_vecstring_type(structure_in%helium_sc, structure_out%helium_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%helium_sc'

     call copy_type_vecstring_type(structure_in%impurity_sc, structure_out%impurity_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%impurity_sc'

     call copy_type_vecstring_type(structure_in%l2h_sc, structure_out%l2h_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%l2h_sc'

     call copy_type_vecstring_type(structure_in%tor_rot_sc, structure_out%tor_rot_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%tor_rot_sc'

     call copy_type_vecstring_type(structure_in%wall_mat, structure_out%wall_mat)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%wall_mat'

     call copy_type_vecstring_type(structure_in%evap_mat, structure_out%evap_mat)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%evap_mat'

     call copy_type_vecstring_type(structure_in%lim_mat, structure_out%lim_mat)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%lim_mat'

     call copy_type_vecstring_type(structure_in%div_mat, structure_out%div_mat)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%div_mat'

     call copy_type_vecstring_type(structure_in%coordinate, structure_out%coordinate)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%coordinate'

     call copy_type_scenario_ref(structure_in%ecrh_freq, structure_out%ecrh_freq)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%ecrh_freq'

     call copy_type_scenario_ref(structure_in%ecrh_loc, structure_out%ecrh_loc)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%ecrh_loc'

     call copy_type_scenario_int(structure_in%ecrh_mode, structure_out%ecrh_mode)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%ecrh_mode'

     call copy_type_scenario_ref(structure_in%ecrh_tor_ang, structure_out%ecrh_tor_ang)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%ecrh_tor_ang'

     call copy_type_scenario_ref(structure_in%ecrh_pol_ang, structure_out%ecrh_pol_ang)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%ecrh_pol_ang'

     call copy_type_scenario_int(structure_in%ecrh_harm, structure_out%ecrh_harm)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%ecrh_harm'

     call copy_type_scenario_ref(structure_in%enbi, structure_out%enbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%enbi'

     call copy_type_scenario_ref(structure_in%r_nbi, structure_out%r_nbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%r_nbi'

     call copy_type_scenario_int(structure_in%grad_b_drift, structure_out%grad_b_drift)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%grad_b_drift'

     call copy_type_scenario_ref(structure_in%icrh_freq, structure_out%icrh_freq)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%icrh_freq'

     call copy_type_vecstring_type(structure_in%icrh_scheme, structure_out%icrh_scheme)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%icrh_scheme'

     call copy_type_scenario_ref(structure_in%icrh_phase, structure_out%icrh_phase)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%icrh_phase'

     call copy_type_scenario_ref(structure_in%LH_freq, structure_out%LH_freq)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%LH_freq'

     call copy_type_scenario_ref(structure_in%LH_npar, structure_out%LH_npar)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%LH_npar'

     call copy_type_scenario_ref(structure_in%pellet_ang, structure_out%pellet_ang)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%pellet_ang'

     call copy_type_scenario_ref(structure_in%pellet_v, structure_out%pellet_v)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%pellet_v'

     call copy_type_scenario_ref(structure_in%pellet_nba, structure_out%pellet_nba)
     if (verbose > 0) write(iu6, *) 'copied scenario_configuration%pellet_nba'

   end subroutine copy_type_scenario_configuration

   subroutine copy_arr_type_scenario_configuration(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_configuration), pointer :: structure_in(:)
     type (type_scenario_configuration), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_configuration(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_configuration'
     end if

   end subroutine copy_arr_type_scenario_configuration

   subroutine copy_type_scenario_confinement(structure_in, structure_out)

     implicit none

     type (type_scenario_confinement), intent(in) :: structure_in
     type (type_scenario_confinement), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%tau_e, structure_out%tau_e)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_e'

     call copy_type_scenario_ref(structure_in%tau_l_sc, structure_out%tau_l_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_l_sc'

     call copy_type_scenario_ref(structure_in%tau_h_sc, structure_out%tau_h_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_h_sc'

     call copy_type_scenario_ref(structure_in%tau_he, structure_out%tau_he)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_he'

     call copy_type_scenario_ref(structure_in%tau_e_ee, structure_out%tau_e_ee)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_e_ee'

     call copy_type_scenario_ref(structure_in%tau_e_ii, structure_out%tau_e_ii)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_e_ii'

     call copy_type_scenario_ref(structure_in%tau_e_ei, structure_out%tau_e_ei)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_e_ei'

     call copy_type_scenario_ref(structure_in%tau_cur_diff, structure_out%tau_cur_diff)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_cur_diff'

     call copy_type_scenario_ref(structure_in%tau_i_rol, structure_out%tau_i_rol)
     if (verbose > 0) write(iu6, *) 'copied scenario_confinement%tau_i_rol'

   end subroutine copy_type_scenario_confinement

   subroutine copy_arr_type_scenario_confinement(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_confinement), pointer :: structure_in(:)
     type (type_scenario_confinement), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_confinement(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_confinement'
     end if

   end subroutine copy_arr_type_scenario_confinement

   subroutine copy_type_scenario_currents(structure_in, structure_out)

     implicit none

     type (type_scenario_currents), intent(in) :: structure_in
     type (type_scenario_currents), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%RR, structure_out%RR)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%RR'

     call copy_type_scenario_ref(structure_in%i_align, structure_out%i_align)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_align'

     call copy_type_scenario_ref(structure_in%i_boot, structure_out%i_boot)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_boot'

     call copy_type_scenario_ref(structure_in%i_cd_tot, structure_out%i_cd_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_cd_tot'

     call copy_type_scenario_ref(structure_in%i_eccd, structure_out%i_eccd)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_eccd'

     call copy_type_scenario_ref(structure_in%i_fast_ion, structure_out%i_fast_ion)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_fast_ion'

     call copy_type_scenario_ref(structure_in%i_fwcd, structure_out%i_fwcd)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_fwcd'

     call copy_type_scenario_ref(structure_in%i_lhcd, structure_out%i_lhcd)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_lhcd'

     call copy_type_scenario_ref(structure_in%i_nbicd, structure_out%i_nbicd)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_nbicd'

     call copy_type_scenario_ref(structure_in%i_ni_tot, structure_out%i_ni_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_ni_tot'

     call copy_type_scenario_ref(structure_in%i_ohm, structure_out%i_ohm)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_ohm'

     call copy_type_scenario_ref(structure_in%i_par, structure_out%i_par)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_par'

     call copy_type_scenario_ref(structure_in%i_runaway, structure_out%i_runaway)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%i_runaway'

     call copy_type_scenario_ref(structure_in%v_loop, structure_out%v_loop)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%v_loop'

     call copy_type_scenario_ref(structure_in%v_meas, structure_out%v_meas)
     if (verbose > 0) write(iu6, *) 'copied scenario_currents%v_meas'

   end subroutine copy_type_scenario_currents

   subroutine copy_arr_type_scenario_currents(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_currents), pointer :: structure_in(:)
     type (type_scenario_currents), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_currents(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_currents'
     end if

   end subroutine copy_arr_type_scenario_currents

   subroutine copy_type_scenario_edge(structure_in, structure_out)

     implicit none

     type (type_scenario_edge), intent(in) :: structure_in
     type (type_scenario_edge), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%te_edge, structure_out%te_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%te_edge'

     call copy_type_scenario_ref(structure_in%ti_edge, structure_out%ti_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%ti_edge'

     call copy_type_scenario_ref(structure_in%ne_edge, structure_out%ne_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%ne_edge'

     call copy_type_scenario_ref(structure_in%ni_edge, structure_out%ni_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%ni_edge'

     call copy_type_scenario_ref(structure_in%psi_edge, structure_out%psi_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%psi_edge'

     call copy_type_scenario_ref(structure_in%phi_edge, structure_out%phi_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%phi_edge'

     call copy_type_scenario_ref(structure_in%rho_edge, structure_out%rho_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%rho_edge'

     call copy_type_scenario_ref(structure_in%drho_edge_dt, structure_out%drho_edge_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%drho_edge_dt'

     call copy_type_scenario_ref(structure_in%q_edge, structure_out%q_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%q_edge'

     call copy_type_scenario_ref(structure_in%neutral_flux, structure_out%neutral_flux)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%neutral_flux'

     call copy_type_scenario_ref(structure_in%phi_plasma, structure_out%phi_plasma)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%phi_plasma'

     call copy_type_scenario_ref(structure_in%vtor_edge, structure_out%vtor_edge)
     if (verbose > 0) write(iu6, *) 'copied scenario_edge%vtor_edge'

   end subroutine copy_type_scenario_edge

   subroutine copy_arr_type_scenario_edge(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_edge), pointer :: structure_in(:)
     type (type_scenario_edge), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_edge(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_edge'
     end if

   end subroutine copy_arr_type_scenario_edge

   subroutine copy_type_scenario_energy(structure_in, structure_out)

     implicit none

     type (type_scenario_energy), intent(in) :: structure_in
     type (type_scenario_energy), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%w_tot, structure_out%w_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%w_tot'

     call copy_type_scenario_ref(structure_in%w_b_pol, structure_out%w_b_pol)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%w_b_pol'

     call copy_type_scenario_ref(structure_in%w_dia, structure_out%w_dia)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%w_dia'

     call copy_type_scenario_ref(structure_in%dwdia_dt, structure_out%dwdia_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%dwdia_dt'

     call copy_type_scenario_ref(structure_in%w_b_tor_pla, structure_out%w_b_tor_pla)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%w_b_tor_pla'

     call copy_type_scenario_ref(structure_in%w_th, structure_out%w_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%w_th'

     call copy_type_scenario_ref(structure_in%dwtot_dt, structure_out%dwtot_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%dwtot_dt'

     call copy_type_scenario_ref(structure_in%dwbpol_dt, structure_out%dwbpol_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%dwbpol_dt'

     call copy_type_scenario_ref(structure_in%dwbtorpla_dt, structure_out%dwbtorpla_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%dwbtorpla_dt'

     call copy_type_scenario_ref(structure_in%dwth_dt, structure_out%dwth_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%dwth_dt'

     call copy_type_scenario_ref(structure_in%esup_icrhtot, structure_out%esup_icrhtot)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%esup_icrhtot'

     call copy_type_scenario_ref(structure_in%esup_icrhper, structure_out%esup_icrhper)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%esup_icrhper'

     call copy_type_scenario_ref(structure_in%esup_nbitot, structure_out%esup_nbitot)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%esup_nbitot'

     call copy_type_scenario_ref(structure_in%esup_nbiperp, structure_out%esup_nbiperp)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%esup_nbiperp'

     call copy_type_scenario_ref(structure_in%esup_lhcd, structure_out%esup_lhcd)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%esup_lhcd'

     call copy_type_scenario_ref(structure_in%esup_alpha, structure_out%esup_alpha)
     if (verbose > 0) write(iu6, *) 'copied scenario_energy%esup_alpha'

   end subroutine copy_type_scenario_energy

   subroutine copy_arr_type_scenario_energy(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_energy), pointer :: structure_in(:)
     type (type_scenario_energy), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_energy(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_energy'
     end if

   end subroutine copy_arr_type_scenario_energy

   subroutine copy_type_scenario_global(structure_in, structure_out)

     implicit none

     type (type_scenario_global), intent(in) :: structure_in
     type (type_scenario_global), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%ip, structure_out%ip)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%ip'

     call copy_type_scenario_ref(structure_in%dip_dt, structure_out%dip_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%dip_dt'

     call copy_type_scenario_ref(structure_in%beta_pol, structure_out%beta_pol)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%beta_pol'

     call copy_type_scenario_ref(structure_in%beta_tor, structure_out%beta_tor)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%beta_tor'

     call copy_type_scenario_ref(structure_in%beta_normal, structure_out%beta_normal)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%beta_normal'

     call copy_type_scenario_ref(structure_in%li, structure_out%li)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%li'

     call copy_type_scenario_ref(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%volume'

     call copy_type_scenario_ref(structure_in%area_pol, structure_out%area_pol)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%area_pol'

     call copy_type_scenario_ref(structure_in%area_ext, structure_out%area_ext)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%area_ext'

     call copy_type_scenario_ref(structure_in%len_sepa, structure_out%len_sepa)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%len_sepa'

     call copy_type_scenario_ref(structure_in%beta_pol_th, structure_out%beta_pol_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%beta_pol_th'

     call copy_type_scenario_ref(structure_in%beta_tor_th, structure_out%beta_tor_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%beta_tor_th'

     call copy_type_scenario_ref(structure_in%beta_n_th, structure_out%beta_n_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%beta_n_th'

     call copy_type_scenario_ref(structure_in%disruption, structure_out%disruption)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%disruption'

     call copy_type_scenario_ref(structure_in%mode_h, structure_out%mode_h)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%mode_h'

     call copy_type_scenario_ref(structure_in%s_alpha, structure_out%s_alpha)
     if (verbose > 0) write(iu6, *) 'copied scenario_global%s_alpha'

   end subroutine copy_type_scenario_global

   subroutine copy_arr_type_scenario_global(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_global), pointer :: structure_in(:)
     type (type_scenario_global), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_global(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_global'
     end if

   end subroutine copy_arr_type_scenario_global

   subroutine copy_type_scenario_heat_power(structure_in, structure_out)

     implicit none

     type (type_scenario_heat_power), intent(in) :: structure_in
     type (type_scenario_heat_power), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%plh, structure_out%plh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%plh'

     call copy_type_scenario_ref(structure_in%pohmic, structure_out%pohmic)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pohmic'

     call copy_type_scenario_ref(structure_in%picrh, structure_out%picrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%picrh'

     call copy_type_scenario_ref(structure_in%pecrh, structure_out%pecrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pecrh'

     call copy_type_scenario_ref(structure_in%pnbi, structure_out%pnbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pnbi'

     call copy_type_scenario_ref(structure_in%pnbi_co_cur, structure_out%pnbi_co_cur)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pnbi_co_cur'

     call copy_type_scenario_ref(structure_in%pnbi_counter, structure_out%pnbi_counter)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pnbi_counter'

     call copy_type_scenario_ref(structure_in%plh_th, structure_out%plh_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%plh_th'

     call copy_type_scenario_ref(structure_in%picrh_th, structure_out%picrh_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%picrh_th'

     call copy_type_scenario_ref(structure_in%pecrh_th, structure_out%pecrh_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pecrh_th'

     call copy_type_scenario_ref(structure_in%pnbi_th, structure_out%pnbi_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pnbi_th'

     call copy_type_scenario_ref(structure_in%ploss_icrh, structure_out%ploss_icrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%ploss_icrh'

     call copy_type_scenario_ref(structure_in%ploss_nbi, structure_out%ploss_nbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%ploss_nbi'

     call copy_type_scenario_ref(structure_in%pbrem, structure_out%pbrem)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pbrem'

     call copy_type_scenario_ref(structure_in%pcyclo, structure_out%pcyclo)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pcyclo'

     call copy_type_scenario_ref(structure_in%prad, structure_out%prad)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%prad'

     call copy_type_scenario_ref(structure_in%pdd_fus, structure_out%pdd_fus)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pdd_fus'

     call copy_type_scenario_ref(structure_in%pei, structure_out%pei)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pei'

     call copy_type_scenario_ref(structure_in%pel_tot, structure_out%pel_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pel_tot'

     call copy_type_scenario_ref(structure_in%pel_fus, structure_out%pel_fus)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pel_fus'

     call copy_type_scenario_ref(structure_in%pel_icrh, structure_out%pel_icrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pel_icrh'

     call copy_type_scenario_ref(structure_in%pel_nbi, structure_out%pel_nbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pel_nbi'

     call copy_type_scenario_ref(structure_in%pfus_dt, structure_out%pfus_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pfus_dt'

     call copy_type_scenario_ref(structure_in%ploss_fus, structure_out%ploss_fus)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%ploss_fus'

     call copy_type_scenario_ref(structure_in%pfus_nbi, structure_out%pfus_nbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pfus_nbi'

     call copy_type_scenario_ref(structure_in%pfus_th, structure_out%pfus_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pfus_th'

     call copy_type_scenario_ref(structure_in%padd_tot, structure_out%padd_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%padd_tot'

     call copy_type_scenario_ref(structure_in%pion_tot, structure_out%pion_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pion_tot'

     call copy_type_scenario_ref(structure_in%pion_fus, structure_out%pion_fus)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pion_fus'

     call copy_type_scenario_ref(structure_in%pion_icrh, structure_out%pion_icrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pion_icrh'

     call copy_type_scenario_ref(structure_in%pion_nbi, structure_out%pion_nbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pion_nbi'

     call copy_type_scenario_ref(structure_in%pioniz, structure_out%pioniz)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%pioniz'

     call copy_type_scenario_ref(structure_in%ploss, structure_out%ploss)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%ploss'

     call copy_type_scenario_ref(structure_in%p_wth, structure_out%p_wth)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%p_wth'

     call copy_type_scenario_ref(structure_in%p_w, structure_out%p_w)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%p_w'

     call copy_type_scenario_ref(structure_in%p_l2h_thr, structure_out%p_l2h_thr)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%p_l2h_thr'

     call copy_type_scenario_ref(structure_in%p_l2h_sc, structure_out%p_l2h_sc)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%p_l2h_sc'

     call copy_type_scenario_ref(structure_in%p_nbi_icrh, structure_out%p_nbi_icrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_heat_power%p_nbi_icrh'

   end subroutine copy_type_scenario_heat_power

   subroutine copy_arr_type_scenario_heat_power(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_heat_power), pointer :: structure_in(:)
     type (type_scenario_heat_power), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_heat_power(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_heat_power'
     end if

   end subroutine copy_arr_type_scenario_heat_power

   subroutine copy_type_scenario_int(structure_in, structure_out)

     implicit none

     type (type_scenario_int), intent(in) :: structure_in
     type (type_scenario_int), intent(inout) :: structure_out

     call copy_type_integer(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied scenario_int%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied scenario_int%source'

   end subroutine copy_type_scenario_int

   subroutine copy_arr_type_scenario_int(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_int), pointer :: structure_in(:)
     type (type_scenario_int), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_int(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_int'
     end if

   end subroutine copy_arr_type_scenario_int

   subroutine copy_type_scenario_itb(structure_in, structure_out)

     implicit none

     type (type_scenario_itb), intent(in) :: structure_in
     type (type_scenario_itb), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%q_min, structure_out%q_min)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%q_min'

     call copy_type_scenario_ref(structure_in%te_itb, structure_out%te_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%te_itb'

     call copy_type_scenario_ref(structure_in%ti_itb, structure_out%ti_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%ti_itb'

     call copy_type_scenario_ref(structure_in%ne_itb, structure_out%ne_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%ne_itb'

     call copy_type_scenario_ref(structure_in%ni_itb, structure_out%ni_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%ni_itb'

     call copy_type_scenario_ref(structure_in%psi_itb, structure_out%psi_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%psi_itb'

     call copy_type_scenario_ref(structure_in%phi_itb, structure_out%phi_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%phi_itb'

     call copy_type_scenario_ref(structure_in%rho_itb, structure_out%rho_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%rho_itb'

     call copy_type_scenario_ref(structure_in%h_itb, structure_out%h_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%h_itb'

     call copy_type_scenario_ref(structure_in%width_itb, structure_out%width_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%width_itb'

     call copy_type_scenario_ref(structure_in%vtor_itb, structure_out%vtor_itb)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%vtor_itb'

     call copy_type_scenario_int(structure_in%itb_type, structure_out%itb_type)
     if (verbose > 0) write(iu6, *) 'copied scenario_itb%itb_type'

   end subroutine copy_type_scenario_itb

   subroutine copy_arr_type_scenario_itb(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_itb), pointer :: structure_in(:)
     type (type_scenario_itb), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_itb(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_itb'
     end if

   end subroutine copy_arr_type_scenario_itb

   subroutine copy_type_scenario_lim_div_wall(structure_in, structure_out)

     implicit none

     type (type_scenario_lim_div_wall), intent(in) :: structure_in
     type (type_scenario_lim_div_wall), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%te_lim_div, structure_out%te_lim_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%te_lim_div'

     call copy_type_scenario_ref(structure_in%ti_lim_div, structure_out%ti_lim_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%ti_lim_div'

     call copy_type_scenario_ref(structure_in%ne_lim_div, structure_out%ne_lim_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%ne_lim_div'

     call copy_type_scenario_ref(structure_in%ni_lim_div, structure_out%ni_lim_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%ni_lim_div'

     call copy_type_scenario_ref(structure_in%q_peak_div, structure_out%q_peak_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%q_peak_div'

     call copy_type_scenario_ref(structure_in%q_peak_wall, structure_out%q_peak_wall)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%q_peak_wall'

     call copy_type_scenario_ref(structure_in%surf_temp, structure_out%surf_temp)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%surf_temp'

     call copy_type_scenario_ref(structure_in%p_lim_div, structure_out%p_lim_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_lim_div'

     call copy_type_scenario_ref(structure_in%p_rad_div, structure_out%p_rad_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_rad_div'

     call copy_type_scenario_ref(structure_in%p_neut_div, structure_out%p_neut_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_neut_div'

     call copy_type_scenario_ref(structure_in%p_wall, structure_out%p_wall)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_wall'

     call copy_type_scenario_ref(structure_in%wall_temp, structure_out%wall_temp)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%wall_temp'

     call copy_type_scenario_ref(structure_in%wall_state, structure_out%wall_state)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%wall_state'

     call copy_type_scenario_ref(structure_in%detach_state, structure_out%detach_state)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%detach_state'

     call copy_type_scenario_ref(structure_in%pump_flux, structure_out%pump_flux)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%pump_flux'

     call copy_type_scenario_ref(structure_in%p_rad_fw, structure_out%p_rad_fw)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_rad_fw'

     call copy_type_scenario_ref(structure_in%p_cond_fw, structure_out%p_cond_fw)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_cond_fw'

     call copy_type_scenario_ref(structure_in%div_wetted, structure_out%div_wetted)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%div_wetted'

     call copy_type_scenario_ref(structure_in%gas_puff, structure_out%gas_puff)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%gas_puff'

     call copy_type_scenario_ref(structure_in%ar_concentr, structure_out%ar_concentr)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%ar_concentr'

     call copy_type_scenario_ref(structure_in%part_exhaust, structure_out%part_exhaust)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%part_exhaust'

     call copy_type_scenario_ref(structure_in%f_inner, structure_out%f_inner)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%f_inner'

     call copy_type_scenario_ref(structure_in%f_outer, structure_out%f_outer)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%f_outer'

     call copy_type_scenario_ref(structure_in%f_pfr, structure_out%f_pfr)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%f_pfr'

     call copy_type_scenario_ref(structure_in%f_rad_fw, structure_out%f_rad_fw)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%f_rad_fw'

     call copy_type_vecflt_type(structure_in%q_div, structure_out%q_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%q_div'

     call copy_type_scenario_ref(structure_in%p_cond_div, structure_out%p_cond_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_cond_div'

     call copy_type_float(structure_in%pol_ext, structure_out%pol_ext)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%pol_ext'

     call copy_type_float(structure_in%flux_exp, structure_out%flux_exp)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%flux_exp'

     call copy_type_float(structure_in%tilt_angle, structure_out%tilt_angle)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%tilt_angle'

     call copy_type_float(structure_in%n_div, structure_out%n_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%n_div'

     call copy_type_float(structure_in%div_dz, structure_out%div_dz)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%div_dz'

     call copy_type_float(structure_in%div_dro, structure_out%div_dro)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%div_dro'

     call copy_type_float(structure_in%div_dri, structure_out%div_dri)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%div_dri'

     call copy_type_scenario_ref(structure_in%p_nh_div, structure_out%p_nh_div)
     if (verbose > 0) write(iu6, *) 'copied scenario_lim_div_wall%p_nh_div'

   end subroutine copy_type_scenario_lim_div_wall

   subroutine copy_arr_type_scenario_lim_div_wall(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_lim_div_wall), pointer :: structure_in(:)
     type (type_scenario_lim_div_wall), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_lim_div_wall(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_lim_div_wall'
     end if

   end subroutine copy_arr_type_scenario_lim_div_wall

   subroutine copy_type_scenario_line_ave(structure_in, structure_out)

     implicit none

     type (type_scenario_line_ave), intent(in) :: structure_in
     type (type_scenario_line_ave), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%ne_line, structure_out%ne_line)
     if (verbose > 0) write(iu6, *) 'copied scenario_line_ave%ne_line'

     call copy_type_scenario_ref(structure_in%zeff_line, structure_out%zeff_line)
     if (verbose > 0) write(iu6, *) 'copied scenario_line_ave%zeff_line'

     call copy_type_scenario_ref(structure_in%ne_zeff_line, structure_out%ne_zeff_line)
     if (verbose > 0) write(iu6, *) 'copied scenario_line_ave%ne_zeff_line'

     call copy_type_scenario_ref(structure_in%dne_line_dt, structure_out%dne_line_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_line_ave%dne_line_dt'

   end subroutine copy_type_scenario_line_ave

   subroutine copy_arr_type_scenario_line_ave(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_line_ave), pointer :: structure_in(:)
     type (type_scenario_line_ave), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_line_ave(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_line_ave'
     end if

   end subroutine copy_arr_type_scenario_line_ave

   subroutine copy_type_scenario_neutron(structure_in, structure_out)

     implicit none

     type (type_scenario_neutron), intent(in) :: structure_in
     type (type_scenario_neutron), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%ndd_tot, structure_out%ndd_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_neutron%ndd_tot'

     call copy_type_scenario_ref(structure_in%ndd_th, structure_out%ndd_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_neutron%ndd_th'

     call copy_type_scenario_ref(structure_in%ndd_nbi_th, structure_out%ndd_nbi_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_neutron%ndd_nbi_th'

     call copy_type_scenario_ref(structure_in%ndd_nbi_nbi, structure_out%ndd_nbi_nbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_neutron%ndd_nbi_nbi'

     call copy_type_scenario_ref(structure_in%ndt_tot, structure_out%ndt_tot)
     if (verbose > 0) write(iu6, *) 'copied scenario_neutron%ndt_tot'

     call copy_type_scenario_ref(structure_in%ndt_th, structure_out%ndt_th)
     if (verbose > 0) write(iu6, *) 'copied scenario_neutron%ndt_th'

   end subroutine copy_type_scenario_neutron

   subroutine copy_arr_type_scenario_neutron(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_neutron), pointer :: structure_in(:)
     type (type_scenario_neutron), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_neutron(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_neutron'
     end if

   end subroutine copy_arr_type_scenario_neutron

   subroutine copy_type_scenario_ninety_five(structure_in, structure_out)

     implicit none

     type (type_scenario_ninety_five), intent(in) :: structure_in
     type (type_scenario_ninety_five), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%q_95, structure_out%q_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%q_95'

     call copy_type_scenario_ref(structure_in%elong_95, structure_out%elong_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%elong_95'

     call copy_type_scenario_ref(structure_in%tria_95, structure_out%tria_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%tria_95'

     call copy_type_scenario_ref(structure_in%tria_up_95, structure_out%tria_up_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%tria_up_95'

     call copy_type_scenario_ref(structure_in%tria_lo_95, structure_out%tria_lo_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%tria_lo_95'

     call copy_type_scenario_ref(structure_in%te_95, structure_out%te_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%te_95'

     call copy_type_scenario_ref(structure_in%ti_95, structure_out%ti_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%ti_95'

     call copy_type_scenario_ref(structure_in%ne_95, structure_out%ne_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%ne_95'

     call copy_type_scenario_ref(structure_in%ni_95, structure_out%ni_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%ni_95'

     call copy_type_scenario_ref(structure_in%phi_95, structure_out%phi_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%phi_95'

     call copy_type_scenario_ref(structure_in%rho_95, structure_out%rho_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%rho_95'

     call copy_type_scenario_ref(structure_in%vtor_95, structure_out%vtor_95)
     if (verbose > 0) write(iu6, *) 'copied scenario_ninety_five%vtor_95'

   end subroutine copy_type_scenario_ninety_five

   subroutine copy_arr_type_scenario_ninety_five(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_ninety_five), pointer :: structure_in(:)
     type (type_scenario_ninety_five), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_ninety_five(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_ninety_five'
     end if

   end subroutine copy_arr_type_scenario_ninety_five

   subroutine copy_type_scenario_pedestal(structure_in, structure_out)

     implicit none

     type (type_scenario_pedestal), intent(in) :: structure_in
     type (type_scenario_pedestal), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%te_ped, structure_out%te_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%te_ped'

     call copy_type_scenario_ref(structure_in%ti_ped, structure_out%ti_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%ti_ped'

     call copy_type_scenario_ref(structure_in%ne_ped, structure_out%ne_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%ne_ped'

     call copy_type_scenario_ref(structure_in%ni_ped, structure_out%ni_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%ni_ped'

     call copy_type_scenario_ref(structure_in%psi_ped, structure_out%psi_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%psi_ped'

     call copy_type_scenario_ref(structure_in%phi_ped, structure_out%phi_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%phi_ped'

     call copy_type_scenario_ref(structure_in%rho_ped, structure_out%rho_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%rho_ped'

     call copy_type_scenario_ref(structure_in%q_ped, structure_out%q_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%q_ped'

     call copy_type_scenario_ref(structure_in%pressure_ped, structure_out%pressure_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%pressure_ped'

     call copy_type_scenario_ref(structure_in%vtor_ped, structure_out%vtor_ped)
     if (verbose > 0) write(iu6, *) 'copied scenario_pedestal%vtor_ped'

   end subroutine copy_type_scenario_pedestal

   subroutine copy_arr_type_scenario_pedestal(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_pedestal), pointer :: structure_in(:)
     type (type_scenario_pedestal), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_pedestal(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_pedestal'
     end if

   end subroutine copy_arr_type_scenario_pedestal

   subroutine copy_type_scenario_reactor(structure_in, structure_out)

     implicit none

     type (type_scenario_reactor), intent(in) :: structure_in
     type (type_scenario_reactor), intent(inout) :: structure_out

     call copy_type_float(structure_in%pnetwork, structure_out%pnetwork)
     if (verbose > 0) write(iu6, *) 'copied scenario_reactor%pnetwork'

   end subroutine copy_type_scenario_reactor

   subroutine copy_arr_type_scenario_reactor(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_reactor), pointer :: structure_in(:)
     type (type_scenario_reactor), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_reactor(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_reactor'
     end if

   end subroutine copy_arr_type_scenario_reactor

   subroutine copy_type_scenario_ref(structure_in, structure_out)

     implicit none

     type (type_scenario_ref), intent(in) :: structure_in
     type (type_scenario_ref), intent(inout) :: structure_out

     call copy_type_float(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied scenario_ref%value'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied scenario_ref%source'

   end subroutine copy_type_scenario_ref

   subroutine copy_arr_type_scenario_ref(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_ref), pointer :: structure_in(:)
     type (type_scenario_ref), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_ref(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_ref'
     end if

   end subroutine copy_arr_type_scenario_ref

   subroutine copy_type_scenario_references(structure_in, structure_out)

     implicit none

     type (type_scenario_references), intent(in) :: structure_in
     type (type_scenario_references), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%plh, structure_out%plh)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%plh'

     call copy_type_scenario_ref(structure_in%picrh, structure_out%picrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%picrh'

     call copy_type_scenario_ref(structure_in%pecrh, structure_out%pecrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%pecrh'

     call copy_type_scenario_ref(structure_in%pnbi, structure_out%pnbi)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%pnbi'

     call copy_type_scenario_ref(structure_in%ip, structure_out%ip)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%ip'

     call copy_type_scenario_ref(structure_in%bvac_r, structure_out%bvac_r)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%bvac_r'

     call copy_type_scenario_ref(structure_in%zeffl, structure_out%zeffl)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%zeffl'

     call copy_type_scenario_ref(structure_in%nbar, structure_out%nbar)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%nbar'

     call copy_type_scenario_ref(structure_in%xecrh, structure_out%xecrh)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%xecrh'

     call copy_type_scenario_ref(structure_in%pol_flux, structure_out%pol_flux)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%pol_flux'

     call copy_type_scenario_ref(structure_in%enhancement, structure_out%enhancement)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%enhancement'

     call copy_type_scenario_ref(structure_in%isotopic, structure_out%isotopic)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%isotopic'

     call copy_type_scenario_ref(structure_in%nbi_td_ratio, structure_out%nbi_td_ratio)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%nbi_td_ratio'

     call copy_type_scenario_ref(structure_in%gas_puff, structure_out%gas_puff)
     if (verbose > 0) write(iu6, *) 'copied scenario_references%gas_puff'

   end subroutine copy_type_scenario_references

   subroutine copy_arr_type_scenario_references(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_references), pointer :: structure_in(:)
     type (type_scenario_references), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_references(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_references'
     end if

   end subroutine copy_arr_type_scenario_references

   subroutine copy_type_scenario_sol(structure_in, structure_out)

     implicit none

     type (type_scenario_sol), intent(in) :: structure_in
     type (type_scenario_sol), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%l_te_sol, structure_out%l_te_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%l_te_sol'

     call copy_type_scenario_ref(structure_in%l_ti_sol, structure_out%l_ti_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%l_ti_sol'

     call copy_type_scenario_ref(structure_in%l_ne_sol, structure_out%l_ne_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%l_ne_sol'

     call copy_type_scenario_ref(structure_in%l_ni_sol, structure_out%l_ni_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%l_ni_sol'

     call copy_type_scenario_ref(structure_in%l_qe_sol, structure_out%l_qe_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%l_qe_sol'

     call copy_type_scenario_ref(structure_in%l_qi_sol, structure_out%l_qi_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%l_qi_sol'

     call copy_type_scenario_ref(structure_in%p_rad_sol, structure_out%p_rad_sol)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%p_rad_sol'

     call copy_type_float(structure_in%p_neut, structure_out%p_neut)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%p_neut'

     call copy_type_scenario_ref(structure_in%gas_puff, structure_out%gas_puff)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%gas_puff'

     call copy_type_float(structure_in%delta_r_in, structure_out%delta_r_in)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%delta_r_in'

     call copy_type_float(structure_in%delta_r_out, structure_out%delta_r_out)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%delta_r_out'

     call copy_type_float(structure_in%r_in, structure_out%r_in)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%r_in'

     call copy_type_float(structure_in%r_out, structure_out%r_out)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%r_out'

     call copy_type_float(structure_in%sol_width, structure_out%sol_width)
     if (verbose > 0) write(iu6, *) 'copied scenario_sol%sol_width'

   end subroutine copy_type_scenario_sol

   subroutine copy_arr_type_scenario_sol(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_sol), pointer :: structure_in(:)
     type (type_scenario_sol), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_sol(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_sol'
     end if

   end subroutine copy_arr_type_scenario_sol

   subroutine copy_type_scenario_vol_ave(structure_in, structure_out)

     implicit none

     type (type_scenario_vol_ave), intent(in) :: structure_in
     type (type_scenario_vol_ave), intent(inout) :: structure_out

     call copy_type_scenario_ref(structure_in%te_ave, structure_out%te_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%te_ave'

     call copy_type_scenario_ref(structure_in%ti_ave, structure_out%ti_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%ti_ave'

     call copy_type_scenario_ref(structure_in%ne_ave, structure_out%ne_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%ne_ave'

     call copy_type_scenario_ref(structure_in%dne_ave_dt, structure_out%dne_ave_dt)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%dne_ave_dt'

     call copy_type_scenario_ref(structure_in%ni_ave, structure_out%ni_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%ni_ave'

     call copy_type_scenario_ref(structure_in%zeff_ave, structure_out%zeff_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%zeff_ave'

     call copy_type_scenario_ref(structure_in%ti_o_te_ave, structure_out%ti_o_te_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%ti_o_te_ave'

     call copy_type_scenario_ref(structure_in%meff_ave, structure_out%meff_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%meff_ave'

     call copy_type_scenario_ref(structure_in%pellet_flux, structure_out%pellet_flux)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%pellet_flux'

     call copy_type_vecflt_type(structure_in%nions_ave, structure_out%nions_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%nions_ave'

     call copy_type_scenario_ref(structure_in%omega_ave, structure_out%omega_ave)
     if (verbose > 0) write(iu6, *) 'copied scenario_vol_ave%omega_ave'

   end subroutine copy_type_scenario_vol_ave

   subroutine copy_arr_type_scenario_vol_ave(structure_in, structure_out)
 
     implicit none
 
     type (type_scenario_vol_ave), pointer :: structure_in(:)
     type (type_scenario_vol_ave), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_scenario_vol_ave(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_scenario_vol_ave'
     end if

   end subroutine copy_arr_type_scenario_vol_ave

   subroutine copy_type_setup_bprobe(structure_in, structure_out)

     implicit none

     type (type_setup_bprobe), intent(in) :: structure_in
     type (type_setup_bprobe), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%id'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%position'

     call copy_type_vecflt_type(structure_in%polangle, structure_out%polangle)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%polangle'

     call copy_type_vecflt_type(structure_in%torangle, structure_out%torangle)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%torangle'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%area'

     call copy_type_vecflt_type(structure_in%length, structure_out%length)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%length'

     call copy_type_vecint_type(structure_in%turns, structure_out%turns)
     if (verbose > 0) write(iu6, *) 'copied setup_bprobe%turns'

   end subroutine copy_type_setup_bprobe

   subroutine copy_arr_type_setup_bprobe(structure_in, structure_out)
 
     implicit none
 
     type (type_setup_bprobe), pointer :: structure_in(:)
     type (type_setup_bprobe), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_setup_bprobe(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_setup_bprobe'
     end if

   end subroutine copy_arr_type_setup_bprobe

   subroutine copy_type_setup_floops(structure_in, structure_out)

     implicit none

     type (type_setup_floops), intent(in) :: structure_in
     type (type_setup_floops), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied setup_floops%name'

     call copy_type_vecstring_type(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied setup_floops%id'

     call copy_type_rzphi2D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied setup_floops%position'

     call copy_type_vecint_type(structure_in%npoints, structure_out%npoints)
     if (verbose > 0) write(iu6, *) 'copied setup_floops%npoints'

   end subroutine copy_type_setup_floops

   subroutine copy_arr_type_setup_floops(structure_in, structure_out)
 
     implicit none
 
     type (type_setup_floops), pointer :: structure_in(:)
     type (type_setup_floops), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_setup_floops(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_setup_floops'
     end if

   end subroutine copy_arr_type_setup_floops

   subroutine copy_type_setup_line(structure_in, structure_out)

     implicit none

     type (type_setup_line), intent(in) :: structure_in
     type (type_setup_line), intent(inout) :: structure_out

     call copy_type_rzphi1D(structure_in%pivot_point, structure_out%pivot_point)
     if (verbose > 0) write(iu6, *) 'copied setup_line%pivot_point'

     call copy_type_vecflt_type(structure_in%horchordang1, structure_out%horchordang1)
     if (verbose > 0) write(iu6, *) 'copied setup_line%horchordang1'

     call copy_type_vecflt_type(structure_in%verchordang1, structure_out%verchordang1)
     if (verbose > 0) write(iu6, *) 'copied setup_line%verchordang1'

     call copy_type_vecflt_type(structure_in%width, structure_out%width)
     if (verbose > 0) write(iu6, *) 'copied setup_line%width'

     call copy_type_rzphi1D(structure_in%second_point, structure_out%second_point)
     if (verbose > 0) write(iu6, *) 'copied setup_line%second_point'

     call copy_type_vecflt_type(structure_in%horchordang2, structure_out%horchordang2)
     if (verbose > 0) write(iu6, *) 'copied setup_line%horchordang2'

     call copy_type_vecflt_type(structure_in%verchordang2, structure_out%verchordang2)
     if (verbose > 0) write(iu6, *) 'copied setup_line%verchordang2'

     call copy_type_rzphi1D(structure_in%third_point, structure_out%third_point)
     if (verbose > 0) write(iu6, *) 'copied setup_line%third_point'

     call copy_type_integer(structure_in%nchordpoints, structure_out%nchordpoints)
     if (verbose > 0) write(iu6, *) 'copied setup_line%nchordpoints'

   end subroutine copy_type_setup_line

   subroutine copy_arr_type_setup_line(structure_in, structure_out)
 
     implicit none
 
     type (type_setup_line), pointer :: structure_in(:)
     type (type_setup_line), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_setup_line(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_setup_line'
     end if

   end subroutine copy_arr_type_setup_line

   subroutine copy_type_setup_line_exp(structure_in, structure_out)

     implicit none

     type (type_setup_line_exp), intent(in) :: structure_in
     type (type_setup_line_exp), intent(inout) :: structure_out

     call copy_type_rzphi1Dexperimental(structure_in%pivot_point, structure_out%pivot_point)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%pivot_point'

     call copy_type_vecflt_type(structure_in%horchordang1, structure_out%horchordang1)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%horchordang1'

     call copy_type_vecflt_type(structure_in%verchordang1, structure_out%verchordang1)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%verchordang1'

     call copy_type_vecflt_type(structure_in%width, structure_out%width)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%width'

     call copy_type_rzphi1Dexperimental(structure_in%second_point, structure_out%second_point)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%second_point'

     call copy_type_vecflt_type(structure_in%horchordang2, structure_out%horchordang2)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%horchordang2'

     call copy_type_vecflt_type(structure_in%verchordang2, structure_out%verchordang2)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%verchordang2'

     call copy_type_rzphi1Dexperimental(structure_in%third_point, structure_out%third_point)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%third_point'

     call copy_type_integer(structure_in%nchordpoints, structure_out%nchordpoints)
     if (verbose > 0) write(iu6, *) 'copied setup_line_exp%nchordpoints'

   end subroutine copy_type_setup_line_exp

   subroutine copy_arr_type_setup_line_exp(structure_in, structure_out)
 
     implicit none
 
     type (type_setup_line_exp), pointer :: structure_in(:)
     type (type_setup_line_exp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_setup_line_exp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_setup_line_exp'
     end if

   end subroutine copy_arr_type_setup_line_exp

   subroutine copy_type_shield(structure_in, structure_out)

     implicit none

     type (type_shield), intent(in) :: structure_in
     type (type_shield), intent(inout) :: structure_out

     call copy_type_shield_specs(structure_in%inboard, structure_out%inboard)
     if (verbose > 0) write(iu6, *) 'copied shield%inboard'

     call copy_type_shield_specs(structure_in%outboard, structure_out%outboard)
     if (verbose > 0) write(iu6, *) 'copied shield%outboard'

   end subroutine copy_type_shield

   subroutine copy_arr_type_shield(structure_in, structure_out)
 
     implicit none
 
     type (type_shield), pointer :: structure_in(:)
     type (type_shield), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_shield(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_shield'
     end if

   end subroutine copy_arr_type_shield

   subroutine copy_type_shield_specs(structure_in, structure_out)

     implicit none

     type (type_shield_specs), intent(in) :: structure_in
     type (type_shield_specs), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nmat, structure_out%nmat)
     if (verbose > 0) write(iu6, *) 'copied shield_specs%nmat'

     call copy_type_vecflt_type(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied shield_specs%composition'

     call copy_type_float(structure_in%r1, structure_out%r1)
     if (verbose > 0) write(iu6, *) 'copied shield_specs%r1'

     call copy_type_float(structure_in%r2, structure_out%r2)
     if (verbose > 0) write(iu6, *) 'copied shield_specs%r2'

     call copy_type_float(structure_in%mass, structure_out%mass)
     if (verbose > 0) write(iu6, *) 'copied shield_specs%mass'

   end subroutine copy_type_shield_specs

   subroutine copy_arr_type_shield_specs(structure_in, structure_out)
 
     implicit none
 
     type (type_shield_specs), pointer :: structure_in(:)
     type (type_shield_specs), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_shield_specs(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_shield_specs'
     end if

   end subroutine copy_arr_type_shield_specs

   subroutine copy_type_simp_apert(structure_in, structure_out)

     implicit none

     type (type_simp_apert), intent(in) :: structure_in
     type (type_simp_apert), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied simp_apert%type'

     call copy_type_vecflt_type(structure_in%sizes, structure_out%sizes)
     if (verbose > 0) write(iu6, *) 'copied simp_apert%sizes'

     call copy_type_float(structure_in%angle, structure_out%angle)
     if (verbose > 0) write(iu6, *) 'copied simp_apert%angle'

   end subroutine copy_type_simp_apert

   subroutine copy_arr_type_simp_apert(structure_in, structure_out)
 
     implicit none
 
     type (type_simp_apert), pointer :: structure_in(:)
     type (type_simp_apert), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_simp_apert(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_simp_apert'
     end if

   end subroutine copy_arr_type_simp_apert

   subroutine copy_type_solcurdiag_sol_current(structure_in, structure_out)

     implicit none

     type (type_solcurdiag_sol_current), intent(in) :: structure_in
     type (type_solcurdiag_sol_current), intent(inout) :: structure_out

     call copy_type_solcurdiag_sol_current_setup(structure_in%setup, structure_out%setup)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag_sol_current%setup'

     call copy_type_exp0D(structure_in%measure, structure_out%measure)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag_sol_current%measure'

   end subroutine copy_type_solcurdiag_sol_current

   subroutine copy_arr_type_solcurdiag_sol_current(structure_in, structure_out)
 
     implicit none
 
     type (type_solcurdiag_sol_current), pointer :: structure_in(:)
     type (type_solcurdiag_sol_current), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_solcurdiag_sol_current(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_solcurdiag_sol_current'
     end if

   end subroutine copy_arr_type_solcurdiag_sol_current

   subroutine copy_type_solcurdiag_sol_current_setup(structure_in, structure_out)

     implicit none

     type (type_solcurdiag_sol_current_setup), intent(in) :: structure_in
     type (type_solcurdiag_sol_current_setup), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag_sol_current_setup%name'

     call copy_type_integer(structure_in%id, structure_out%id)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag_sol_current_setup%id'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag_sol_current_setup%position'

     call copy_type_integer(structure_in%tiles_turn, structure_out%tiles_turn)
     if (verbose > 0) write(iu6, *) 'copied solcurdiag_sol_current_setup%tiles_turn'

   end subroutine copy_type_solcurdiag_sol_current_setup

   subroutine copy_arr_type_solcurdiag_sol_current_setup(structure_in, structure_out)
 
     implicit none
 
     type (type_solcurdiag_sol_current_setup), pointer :: structure_in(:)
     type (type_solcurdiag_sol_current_setup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_solcurdiag_sol_current_setup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_solcurdiag_sol_current_setup'
     end if

   end subroutine copy_arr_type_solcurdiag_sol_current_setup

   subroutine copy_type_source_imp(structure_in, structure_out)

     implicit none

     type (type_source_imp), intent(in) :: structure_in
     type (type_source_imp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%exp, structure_out%exp)
     if (verbose > 0) write(iu6, *) 'copied source_imp%exp'

     call copy_type_matflt_type(structure_in%imp, structure_out%imp)
     if (verbose > 0) write(iu6, *) 'copied source_imp%imp'

   end subroutine copy_type_source_imp

   subroutine copy_arr_type_source_imp(structure_in, structure_out)
 
     implicit none
 
     type (type_source_imp), pointer :: structure_in(:)
     type (type_source_imp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_source_imp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_source_imp'
     end if

   end subroutine copy_arr_type_source_imp

   subroutine copy_type_source_ion(structure_in, structure_out)

     implicit none

     type (type_source_ion), intent(in) :: structure_in
     type (type_source_ion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%exp, structure_out%exp)
     if (verbose > 0) write(iu6, *) 'copied source_ion%exp'

     call copy_type_matflt_type(structure_in%imp, structure_out%imp)
     if (verbose > 0) write(iu6, *) 'copied source_ion%imp'

   end subroutine copy_type_source_ion

   subroutine copy_arr_type_source_ion(structure_in, structure_out)
 
     implicit none
 
     type (type_source_ion), pointer :: structure_in(:)
     type (type_source_ion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_source_ion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_source_ion'
     end if

   end subroutine copy_arr_type_source_ion

   subroutine copy_type_source_rate(structure_in, structure_out)

     implicit none

     type (type_source_rate), intent(in) :: structure_in
     type (type_source_rate), intent(inout) :: structure_out

     call copy_type_complexgrid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied source_rate%grid'

     call copy_type_complexgrid_scalar(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied source_rate%value'

     call copy_type_vecint_type(structure_in%discrete, structure_out%discrete)
     if (verbose > 0) write(iu6, *) 'copied source_rate%discrete'

     call copy_type_parameters(structure_in%parameters, structure_out%parameters)
     if (verbose > 0) write(iu6, *) 'copied source_rate%parameters'

   end subroutine copy_type_source_rate

   subroutine copy_arr_type_source_rate(structure_in, structure_out)
 
     implicit none
 
     type (type_source_rate), pointer :: structure_in(:)
     type (type_source_rate), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_source_rate(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_source_rate'
     end if

   end subroutine copy_arr_type_source_rate

   subroutine copy_type_source_vec(structure_in, structure_out)

     implicit none

     type (type_source_vec), intent(in) :: structure_in
     type (type_source_vec), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%exp, structure_out%exp)
     if (verbose > 0) write(iu6, *) 'copied source_vec%exp'

     call copy_type_vecflt_type(structure_in%imp, structure_out%imp)
     if (verbose > 0) write(iu6, *) 'copied source_vec%imp'

   end subroutine copy_type_source_vec

   subroutine copy_arr_type_source_vec(structure_in, structure_out)
 
     implicit none
 
     type (type_source_vec), pointer :: structure_in(:)
     type (type_source_vec), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_source_vec(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_source_vec'
     end if

   end subroutine copy_arr_type_source_vec

   subroutine copy_type_sourceel(structure_in, structure_out)

     implicit none

     type (type_sourceel), intent(in) :: structure_in
     type (type_sourceel), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied sourceel%value'

     call copy_type_vecflt_type(structure_in%integral, structure_out%integral)
     if (verbose > 0) write(iu6, *) 'copied sourceel%integral'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied sourceel%source'

   end subroutine copy_type_sourceel

   subroutine copy_arr_type_sourceel(structure_in, structure_out)
 
     implicit none
 
     type (type_sourceel), pointer :: structure_in(:)
     type (type_sourceel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sourceel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sourceel'
     end if

   end subroutine copy_arr_type_sourceel

   subroutine copy_type_sourceimp(structure_in, structure_out)

     implicit none

     type (type_sourceimp), intent(in) :: structure_in
     type (type_sourceimp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied sourceimp%value'

     call copy_type_matflt_type(structure_in%integral, structure_out%integral)
     if (verbose > 0) write(iu6, *) 'copied sourceimp%integral'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied sourceimp%source'

   end subroutine copy_type_sourceimp

   subroutine copy_arr_type_sourceimp(structure_in, structure_out)
 
     implicit none
 
     type (type_sourceimp), pointer :: structure_in(:)
     type (type_sourceimp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sourceimp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sourceimp'
     end if

   end subroutine copy_arr_type_sourceimp

   subroutine copy_type_sourceion(structure_in, structure_out)

     implicit none

     type (type_sourceion), intent(in) :: structure_in
     type (type_sourceion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied sourceion%value'

     call copy_type_matflt_type(structure_in%integral, structure_out%integral)
     if (verbose > 0) write(iu6, *) 'copied sourceion%integral'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied sourceion%source'

   end subroutine copy_type_sourceion

   subroutine copy_arr_type_sourceion(structure_in, structure_out)
 
     implicit none
 
     type (type_sourceion), pointer :: structure_in(:)
     type (type_sourceion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sourceion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sourceion'
     end if

   end subroutine copy_arr_type_sourceion

   subroutine copy_type_species_desc(structure_in, structure_out)

     implicit none

     type (type_species_desc), intent(in) :: structure_in
     type (type_species_desc), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied species_desc%label'

     call copy_type_float(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied species_desc%amn'

     call copy_type_float(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied species_desc%zn'

     call copy_type_float(structure_in%zmin, structure_out%zmin)
     if (verbose > 0) write(iu6, *) 'copied species_desc%zmin'

     call copy_type_float(structure_in%zmax, structure_out%zmax)
     if (verbose > 0) write(iu6, *) 'copied species_desc%zmax'

   end subroutine copy_type_species_desc

   subroutine copy_arr_type_species_desc(structure_in, structure_out)
 
     implicit none
 
     type (type_species_desc), pointer :: structure_in(:)
     type (type_species_desc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_species_desc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_species_desc'
     end if

   end subroutine copy_arr_type_species_desc

   subroutine copy_type_species_reference(structure_in, structure_out)

     implicit none

     type (type_species_reference), intent(in) :: structure_in
     type (type_species_reference), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied species_reference%type'

     call copy_type_integer(structure_in%index, structure_out%index)
     if (verbose > 0) write(iu6, *) 'copied species_reference%index'

   end subroutine copy_type_species_reference

   subroutine copy_arr_type_species_reference(structure_in, structure_out)
 
     implicit none
 
     type (type_species_reference), pointer :: structure_in(:)
     type (type_species_reference), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_species_reference(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_species_reference'
     end if

   end subroutine copy_arr_type_species_reference

   subroutine copy_type_spectral(structure_in, structure_out)

     implicit none

     type (type_spectral), intent(in) :: structure_in
     type (type_spectral), intent(inout) :: structure_out

     call copy_type_msediag_emissivity(structure_in%emissivity, structure_out%emissivity)
     if (verbose > 0) write(iu6, *) 'copied spectral%emissivity'

     call copy_type_msediag_radiance(structure_in%radiance, structure_out%radiance)
     if (verbose > 0) write(iu6, *) 'copied spectral%radiance'

     call copy_type_codeparam(structure_in%codeparam, structure_out%codeparam)
     if (verbose > 0) write(iu6, *) 'copied spectral%codeparam'

   end subroutine copy_type_spectral

   subroutine copy_arr_type_spectral(structure_in, structure_out)
 
     implicit none
 
     type (type_spectral), pointer :: structure_in(:)
     type (type_spectral), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_spectral(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_spectral'
     end if

   end subroutine copy_arr_type_spectral

   subroutine copy_type_spectrum(structure_in, structure_out)

     implicit none

     type (type_spectrum), intent(in) :: structure_in
     type (type_spectrum), intent(inout) :: structure_out

     call copy_type_launchs_phi_theta(structure_in%phi_theta, structure_out%phi_theta)
     if (verbose > 0) write(iu6, *) 'copied spectrum%phi_theta'

     call copy_type_launchs_parallel(structure_in%parallel, structure_out%parallel)
     if (verbose > 0) write(iu6, *) 'copied spectrum%parallel'

   end subroutine copy_type_spectrum

   subroutine copy_arr_type_spectrum(structure_in, structure_out)
 
     implicit none
 
     type (type_spectrum), pointer :: structure_in(:)
     type (type_spectrum), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_spectrum(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_spectrum'
     end if

   end subroutine copy_arr_type_spectrum

   subroutine copy_type_spot(structure_in, structure_out)

     implicit none

     type (type_spot), intent(in) :: structure_in
     type (type_spot), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%size, structure_out%size)
     if (verbose > 0) write(iu6, *) 'copied spot%size'

     call copy_type_float(structure_in%angle, structure_out%angle)
     if (verbose > 0) write(iu6, *) 'copied spot%angle'

   end subroutine copy_type_spot

   subroutine copy_arr_type_spot(structure_in, structure_out)
 
     implicit none
 
     type (type_spot), pointer :: structure_in(:)
     type (type_spot), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_spot(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_spot'
     end if

   end subroutine copy_arr_type_spot

   subroutine copy_type_sputtering_neutrals(structure_in, structure_out)

     implicit none

     type (type_sputtering_neutrals), intent(in) :: structure_in
     type (type_sputtering_neutrals), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%physical, structure_out%physical)
     if (verbose > 0) write(iu6, *) 'copied sputtering_neutrals%physical'

     call copy_type_vecflt_type(structure_in%chemical, structure_out%chemical)
     if (verbose > 0) write(iu6, *) 'copied sputtering_neutrals%chemical'

   end subroutine copy_type_sputtering_neutrals

   subroutine copy_arr_type_sputtering_neutrals(structure_in, structure_out)
 
     implicit none
 
     type (type_sputtering_neutrals), pointer :: structure_in(:)
     type (type_sputtering_neutrals), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_sputtering_neutrals(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_sputtering_neutrals'
     end if

   end subroutine copy_arr_type_sputtering_neutrals

   subroutine copy_type_straps(structure_in, structure_out)

     implicit none

     type (type_straps), intent(in) :: structure_in
     type (type_straps), intent(inout) :: structure_out

     call copy_type_exp0D(structure_in%current, structure_out%current)
     if (verbose > 0) write(iu6, *) 'copied straps%current'

     call copy_type_exp0D(structure_in%phase, structure_out%phase)
     if (verbose > 0) write(iu6, *) 'copied straps%phase'

     call copy_type_float(structure_in%phi_centre, structure_out%phi_centre)
     if (verbose > 0) write(iu6, *) 'copied straps%phi_centre'

     call copy_type_float(structure_in%width, structure_out%width)
     if (verbose > 0) write(iu6, *) 'copied straps%width'

     call copy_type_float(structure_in%dist2wall, structure_out%dist2wall)
     if (verbose > 0) write(iu6, *) 'copied straps%dist2wall'

     call copy_type_rz1D(structure_in%coord_strap, structure_out%coord_strap)
     if (verbose > 0) write(iu6, *) 'copied straps%coord_strap'

   end subroutine copy_type_straps

   subroutine copy_arr_type_straps(structure_in, structure_out)
 
     implicit none
 
     type (type_straps), pointer :: structure_in(:)
     type (type_straps), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_straps(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_straps'
     end if

   end subroutine copy_arr_type_straps

   subroutine copy_type_structure_cs(structure_in, structure_out)

     implicit none

     type (type_structure_cs), intent(in) :: structure_in
     type (type_structure_cs), intent(inout) :: structure_out

     call copy_type_float(structure_in%gaptf, structure_out%gaptf)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%gaptf'

     call copy_type_float(structure_in%ri, structure_out%ri)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%ri'

     call copy_type_float(structure_in%re, structure_out%re)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%re'

     call copy_type_float(structure_in%jcable, structure_out%jcable)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%jcable'

     call copy_type_float(structure_in%current_nom, structure_out%current_nom)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%current_nom'

     call copy_type_float(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%sigma'

     call copy_type_float(structure_in%tiso, structure_out%tiso)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%tiso'

     call copy_type_float(structure_in%nlay, structure_out%nlay)
     if (verbose > 0) write(iu6, *) 'copied structure_cs%nlay'

   end subroutine copy_type_structure_cs

   subroutine copy_arr_type_structure_cs(structure_in, structure_out)
 
     implicit none
 
     type (type_structure_cs), pointer :: structure_in(:)
     type (type_structure_cs), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_structure_cs(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_structure_cs'
     end if

   end subroutine copy_arr_type_structure_cs

   subroutine copy_type_t_series_cplx(structure_in, structure_out)

     implicit none

     type (type_t_series_cplx), intent(in) :: structure_in
     type (type_t_series_cplx), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%time_wind, structure_out%time_wind)
     if (verbose > 0) write(iu6, *) 'copied t_series_cplx%time_wind'

     call copy_type_vecflt_type(structure_in%values_re, structure_out%values_re)
     if (verbose > 0) write(iu6, *) 'copied t_series_cplx%values_re'

     call copy_type_vecflt_type(structure_in%values_im, structure_out%values_im)
     if (verbose > 0) write(iu6, *) 'copied t_series_cplx%values_im'

   end subroutine copy_type_t_series_cplx

   subroutine copy_arr_type_t_series_cplx(structure_in, structure_out)
 
     implicit none
 
     type (type_t_series_cplx), pointer :: structure_in(:)
     type (type_t_series_cplx), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_t_series_cplx(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_t_series_cplx'
     end if

   end subroutine copy_arr_type_t_series_cplx

   subroutine copy_type_t_series_real(structure_in, structure_out)

     implicit none

     type (type_t_series_real), intent(in) :: structure_in
     type (type_t_series_real), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%time_wind, structure_out%time_wind)
     if (verbose > 0) write(iu6, *) 'copied t_series_real%time_wind'

     call copy_type_vecflt_type(structure_in%values, structure_out%values)
     if (verbose > 0) write(iu6, *) 'copied t_series_real%values'

   end subroutine copy_type_t_series_real

   subroutine copy_arr_type_t_series_real(structure_in, structure_out)
 
     implicit none
 
     type (type_t_series_real), pointer :: structure_in(:)
     type (type_t_series_real), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_t_series_real(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_t_series_real'
     end if

   end subroutine copy_arr_type_t_series_real

   subroutine copy_type_table(structure_in, structure_out)

     implicit none

     type (type_table), intent(in) :: structure_in
     type (type_table), intent(inout) :: structure_out

     call copy_type_integer(structure_in%filled, structure_out%filled)
     if (verbose > 0) write(iu6, *) 'copied table%filled'

     call copy_type_float(structure_in%table_0d, structure_out%table_0d)
     if (verbose > 0) write(iu6, *) 'copied table%table_0d'

     call copy_type_vecflt_type(structure_in%table_1d, structure_out%table_1d)
     if (verbose > 0) write(iu6, *) 'copied table%table_1d'

     call copy_type_matflt_type(structure_in%table_2d, structure_out%table_2d)
     if (verbose > 0) write(iu6, *) 'copied table%table_2d'

     call copy_type_array3dflt_type(structure_in%table_3d, structure_out%table_3d)
     if (verbose > 0) write(iu6, *) 'copied table%table_3d'

     call copy_type_array4dflt_type(structure_in%table_4d, structure_out%table_4d)
     if (verbose > 0) write(iu6, *) 'copied table%table_4d'

     call copy_type_array5dflt_type(structure_in%table_5d, structure_out%table_5d)
     if (verbose > 0) write(iu6, *) 'copied table%table_5d'

     call copy_type_array6dflt_type(structure_in%table_6d, structure_out%table_6d)
     if (verbose > 0) write(iu6, *) 'copied table%table_6d'

     call copy_type_vecstring_type(structure_in%coord1_str, structure_out%coord1_str)
     if (verbose > 0) write(iu6, *) 'copied table%coord1_str'

     call copy_type_vecstring_type(structure_in%coord2_str, structure_out%coord2_str)
     if (verbose > 0) write(iu6, *) 'copied table%coord2_str'

     call copy_type_vecstring_type(structure_in%coord3_str, structure_out%coord3_str)
     if (verbose > 0) write(iu6, *) 'copied table%coord3_str'

     call copy_type_vecstring_type(structure_in%coord4_str, structure_out%coord4_str)
     if (verbose > 0) write(iu6, *) 'copied table%coord4_str'

     call copy_type_vecstring_type(structure_in%coord5_str, structure_out%coord5_str)
     if (verbose > 0) write(iu6, *) 'copied table%coord5_str'

     call copy_type_vecstring_type(structure_in%coord6_str, structure_out%coord6_str)
     if (verbose > 0) write(iu6, *) 'copied table%coord6_str'

     call copy_type_identifier(structure_in%quality, structure_out%quality)
     if (verbose > 0) write(iu6, *) 'copied table%quality'

   end subroutine copy_type_table

   subroutine copy_arr_type_table(structure_in, structure_out)
 
     implicit none
 
     type (type_table), pointer :: structure_in(:)
     type (type_table), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_table(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_table'
     end if

   end subroutine copy_arr_type_table

   subroutine copy_type_tables(structure_in, structure_out)

     implicit none

     type (type_tables), intent(in) :: structure_in
     type (type_tables), intent(inout) :: structure_out

     call copy_type_integer(structure_in%ndim, structure_out%ndim)
     if (verbose > 0) write(iu6, *) 'copied tables%ndim'

     call copy_type_integer(structure_in%coord_index, structure_out%coord_index)
     if (verbose > 0) write(iu6, *) 'copied tables%coord_index'

     call copy_type_vecstring_type(structure_in%result_label, structure_out%result_label)
     if (verbose > 0) write(iu6, *) 'copied tables%result_label'

     call copy_type_vecstring_type(structure_in%result_unit, structure_out%result_unit)
     if (verbose > 0) write(iu6, *) 'copied tables%result_unit'

     call copy_type_integer(structure_in%result_trans, structure_out%result_trans)
     if (verbose > 0) write(iu6, *) 'copied tables%result_trans'

     call copy_type_vecint_type(structure_in%zmin, structure_out%zmin)
     if (verbose > 0) write(iu6, *) 'copied tables%zmin'

     call copy_type_vecint_type(structure_in%zmax, structure_out%zmax)
     if (verbose > 0) write(iu6, *) 'copied tables%zmax'

     call copy_type_vecstring_type(structure_in%state_label, structure_out%state_label)
     if (verbose > 0) write(iu6, *) 'copied tables%state_label'

     call copy_arr_type_table(structure_in%table, structure_out%table)
     if (verbose > 0) write(iu6, *) 'copied tables%table'

     call copy_type_vecstring_type(structure_in%data_source, structure_out%data_source)
     if (verbose > 0) write(iu6, *) 'copied tables%data_source'

     call copy_type_vecstring_type(structure_in%data_provide, structure_out%data_provide)
     if (verbose > 0) write(iu6, *) 'copied tables%data_provide'

     call copy_type_vecstring_type(structure_in%data_citation, structure_out%data_citation)
     if (verbose > 0) write(iu6, *) 'copied tables%data_citation'

   end subroutine copy_type_tables

   subroutine copy_arr_type_tables(structure_in, structure_out)
 
     implicit none
 
     type (type_tables), pointer :: structure_in(:)
     type (type_tables), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tables(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tables'
     end if

   end subroutine copy_arr_type_tables

   subroutine copy_type_tables_coord(structure_in, structure_out)

     implicit none

     type (type_tables_coord), intent(in) :: structure_in
     type (type_tables_coord), intent(inout) :: structure_out

     call copy_arr_type_coords(structure_in%coords, structure_out%coords)
     if (verbose > 0) write(iu6, *) 'copied tables_coord%coords'

   end subroutine copy_type_tables_coord

   subroutine copy_arr_type_tables_coord(structure_in, structure_out)
 
     implicit none
 
     type (type_tables_coord), pointer :: structure_in(:)
     type (type_tables_coord), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tables_coord(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tables_coord'
     end if

   end subroutine copy_arr_type_tables_coord

   subroutine copy_type_temporary_nt(structure_in, structure_out)

     implicit none

     type (type_temporary_nt), intent(in) :: structure_in
     type (type_temporary_nt), intent(inout) :: structure_out

     call copy_arr_type_temporary_nt_0dr(structure_in%float0d, structure_out%float0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%float0d'

     call copy_arr_type_temporary_nt_0di(structure_in%integer0d, structure_out%integer0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%integer0d'

     call copy_arr_type_temporary_nt_0dc(structure_in%complex0d, structure_out%complex0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%complex0d'

     call copy_arr_type_temporary_nt_0ds(structure_in%string0d, structure_out%string0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%string0d'

     call copy_arr_type_temporary_nt_1dr(structure_in%float1d, structure_out%float1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%float1d'

     call copy_arr_type_temporary_nt_1di(structure_in%integer1d, structure_out%integer1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%integer1d'

     call copy_arr_type_temporary_nt_1dr(structure_in%string1d, structure_out%string1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%string1d'

     call copy_arr_type_temporary_nt_1dc(structure_in%complex1d, structure_out%complex1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%complex1d'

     call copy_arr_type_temporary_nt_2dr(structure_in%float2d, structure_out%float2d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%float2d'

     call copy_arr_type_temporary_nt_2di(structure_in%integer2d, structure_out%integer2d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%integer2d'

     call copy_arr_type_temporary_nt_2dc(structure_in%complex2d, structure_out%complex2d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%complex2d'

     call copy_arr_type_temporary_nt_3dr(structure_in%float3d, structure_out%float3d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%float3d'

     call copy_arr_type_temporary_nt_3di(structure_in%integer3d, structure_out%integer3d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%integer3d'

     call copy_arr_type_temporary_nt_3dc(structure_in%complex3d, structure_out%complex3d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%complex3d'

     call copy_arr_type_temporary_nt_4dr(structure_in%float4d, structure_out%float4d)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt%float4d'

   end subroutine copy_type_temporary_nt

   subroutine copy_arr_type_temporary_nt(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt), pointer :: structure_in(:)
     type (type_temporary_nt), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt'
     end if

   end subroutine copy_arr_type_temporary_nt

   subroutine copy_type_temporary_nt_0dc(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_0dc), intent(in) :: structure_in
     type (type_temporary_nt_0dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0dc%identifier'

     call copy_type_complex(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0dc%value'

   end subroutine copy_type_temporary_nt_0dc

   subroutine copy_arr_type_temporary_nt_0dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_0dc), pointer :: structure_in(:)
     type (type_temporary_nt_0dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_0dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_0dc'
     end if

   end subroutine copy_arr_type_temporary_nt_0dc

   subroutine copy_type_temporary_nt_0di(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_0di), intent(in) :: structure_in
     type (type_temporary_nt_0di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0di%identifier'

     call copy_type_integer(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0di%value'

   end subroutine copy_type_temporary_nt_0di

   subroutine copy_arr_type_temporary_nt_0di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_0di), pointer :: structure_in(:)
     type (type_temporary_nt_0di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_0di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_0di'
     end if

   end subroutine copy_arr_type_temporary_nt_0di

   subroutine copy_type_temporary_nt_0dr(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_0dr), intent(in) :: structure_in
     type (type_temporary_nt_0dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0dr%identifier'

     call copy_type_float(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0dr%value'

   end subroutine copy_type_temporary_nt_0dr

   subroutine copy_arr_type_temporary_nt_0dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_0dr), pointer :: structure_in(:)
     type (type_temporary_nt_0dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_0dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_0dr'
     end if

   end subroutine copy_arr_type_temporary_nt_0dr

   subroutine copy_type_temporary_nt_0ds(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_0ds), intent(in) :: structure_in
     type (type_temporary_nt_0ds), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0ds%identifier'

     call copy_type_vecstring_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_0ds%value'

   end subroutine copy_type_temporary_nt_0ds

   subroutine copy_arr_type_temporary_nt_0ds(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_0ds), pointer :: structure_in(:)
     type (type_temporary_nt_0ds), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_0ds(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_0ds'
     end if

   end subroutine copy_arr_type_temporary_nt_0ds

   subroutine copy_type_temporary_nt_1dc(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_1dc), intent(in) :: structure_in
     type (type_temporary_nt_1dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1dc%identifier'

     call copy_type_veccplx_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1dc%value'

   end subroutine copy_type_temporary_nt_1dc

   subroutine copy_arr_type_temporary_nt_1dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_1dc), pointer :: structure_in(:)
     type (type_temporary_nt_1dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_1dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_1dc'
     end if

   end subroutine copy_arr_type_temporary_nt_1dc

   subroutine copy_type_temporary_nt_1di(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_1di), intent(in) :: structure_in
     type (type_temporary_nt_1di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1di%identifier'

     call copy_type_vecint_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1di%value'

   end subroutine copy_type_temporary_nt_1di

   subroutine copy_arr_type_temporary_nt_1di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_1di), pointer :: structure_in(:)
     type (type_temporary_nt_1di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_1di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_1di'
     end if

   end subroutine copy_arr_type_temporary_nt_1di

   subroutine copy_type_temporary_nt_1dr(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_1dr), intent(in) :: structure_in
     type (type_temporary_nt_1dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1dr%identifier'

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1dr%value'

   end subroutine copy_type_temporary_nt_1dr

   subroutine copy_arr_type_temporary_nt_1dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_1dr), pointer :: structure_in(:)
     type (type_temporary_nt_1dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_1dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_1dr'
     end if

   end subroutine copy_arr_type_temporary_nt_1dr

   subroutine copy_type_temporary_nt_1ds(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_1ds), intent(in) :: structure_in
     type (type_temporary_nt_1ds), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1ds%identifier'

     call copy_type_vecstring_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_1ds%value'

   end subroutine copy_type_temporary_nt_1ds

   subroutine copy_arr_type_temporary_nt_1ds(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_1ds), pointer :: structure_in(:)
     type (type_temporary_nt_1ds), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_1ds(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_1ds'
     end if

   end subroutine copy_arr_type_temporary_nt_1ds

   subroutine copy_type_temporary_nt_2dc(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_2dc), intent(in) :: structure_in
     type (type_temporary_nt_2dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_2dc%identifier'

     call copy_type_matcplx_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_2dc%value'

   end subroutine copy_type_temporary_nt_2dc

   subroutine copy_arr_type_temporary_nt_2dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_2dc), pointer :: structure_in(:)
     type (type_temporary_nt_2dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_2dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_2dc'
     end if

   end subroutine copy_arr_type_temporary_nt_2dc

   subroutine copy_type_temporary_nt_2di(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_2di), intent(in) :: structure_in
     type (type_temporary_nt_2di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_2di%identifier'

     call copy_type_matint_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_2di%value'

   end subroutine copy_type_temporary_nt_2di

   subroutine copy_arr_type_temporary_nt_2di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_2di), pointer :: structure_in(:)
     type (type_temporary_nt_2di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_2di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_2di'
     end if

   end subroutine copy_arr_type_temporary_nt_2di

   subroutine copy_type_temporary_nt_2dr(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_2dr), intent(in) :: structure_in
     type (type_temporary_nt_2dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_2dr%identifier'

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_2dr%value'

   end subroutine copy_type_temporary_nt_2dr

   subroutine copy_arr_type_temporary_nt_2dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_2dr), pointer :: structure_in(:)
     type (type_temporary_nt_2dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_2dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_2dr'
     end if

   end subroutine copy_arr_type_temporary_nt_2dr

   subroutine copy_type_temporary_nt_3dc(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_3dc), intent(in) :: structure_in
     type (type_temporary_nt_3dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_3dc%identifier'

     call copy_type_array3dcplx_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_3dc%value'

   end subroutine copy_type_temporary_nt_3dc

   subroutine copy_arr_type_temporary_nt_3dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_3dc), pointer :: structure_in(:)
     type (type_temporary_nt_3dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_3dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_3dc'
     end if

   end subroutine copy_arr_type_temporary_nt_3dc

   subroutine copy_type_temporary_nt_3di(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_3di), intent(in) :: structure_in
     type (type_temporary_nt_3di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_3di%identifier'

     call copy_type_array3dint_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_3di%value'

   end subroutine copy_type_temporary_nt_3di

   subroutine copy_arr_type_temporary_nt_3di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_3di), pointer :: structure_in(:)
     type (type_temporary_nt_3di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_3di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_3di'
     end if

   end subroutine copy_arr_type_temporary_nt_3di

   subroutine copy_type_temporary_nt_3dr(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_3dr), intent(in) :: structure_in
     type (type_temporary_nt_3dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_3dr%identifier'

     call copy_type_array3dflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_3dr%value'

   end subroutine copy_type_temporary_nt_3dr

   subroutine copy_arr_type_temporary_nt_3dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_3dr), pointer :: structure_in(:)
     type (type_temporary_nt_3dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_3dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_3dr'
     end if

   end subroutine copy_arr_type_temporary_nt_3dr

   subroutine copy_type_temporary_nt_4dr(structure_in, structure_out)

     implicit none

     type (type_temporary_nt_4dr), intent(in) :: structure_in
     type (type_temporary_nt_4dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_4dr%identifier'

     call copy_type_array4dflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_nt_4dr%value'

   end subroutine copy_type_temporary_nt_4dr

   subroutine copy_arr_type_temporary_nt_4dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_nt_4dr), pointer :: structure_in(:)
     type (type_temporary_nt_4dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_nt_4dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_nt_4dr'
     end if

   end subroutine copy_arr_type_temporary_nt_4dr

   subroutine copy_type_temporary_t(structure_in, structure_out)

     implicit none

     type (type_temporary_t), intent(in) :: structure_in
     type (type_temporary_t), intent(inout) :: structure_out

     call copy_arr_type_temporary_t_0dr(structure_in%float0d, structure_out%float0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%float0d'

     call copy_arr_type_temporary_t_0di(structure_in%integer0d, structure_out%integer0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%integer0d'

     call copy_arr_type_temporary_t_0dc(structure_in%complex0d, structure_out%complex0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%complex0d'

     call copy_arr_type_temporary_t_0ds(structure_in%string0d, structure_out%string0d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%string0d'

     call copy_arr_type_temporary_t_1dr(structure_in%float1d, structure_out%float1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%float1d'

     call copy_arr_type_temporary_t_1di(structure_in%integer1d, structure_out%integer1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%integer1d'

     call copy_arr_type_temporary_t_1dc(structure_in%complex1d, structure_out%complex1d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%complex1d'

     call copy_arr_type_temporary_t_2dr(structure_in%float2d, structure_out%float2d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%float2d'

     call copy_arr_type_temporary_t_2di(structure_in%integer2d, structure_out%integer2d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%integer2d'

     call copy_arr_type_temporary_t_2dc(structure_in%complex2d, structure_out%complex2d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%complex2d'

     call copy_arr_type_temporary_t_3dr(structure_in%float3d, structure_out%float3d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%float3d'

     call copy_arr_type_temporary_t_3di(structure_in%integer3d, structure_out%integer3d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%integer3d'

     call copy_arr_type_temporary_t_3dc(structure_in%complex3d, structure_out%complex3d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%complex3d'

     call copy_arr_type_temporary_t_4dr(structure_in%float4d, structure_out%float4d)
     if (verbose > 0) write(iu6, *) 'copied temporary_t%float4d'

   end subroutine copy_type_temporary_t

   subroutine copy_arr_type_temporary_t(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t), pointer :: structure_in(:)
     type (type_temporary_t), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t'
     end if

   end subroutine copy_arr_type_temporary_t

   subroutine copy_type_temporary_t_0dc(structure_in, structure_out)

     implicit none

     type (type_temporary_t_0dc), intent(in) :: structure_in
     type (type_temporary_t_0dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0dc%identifier'

     call copy_type_complex(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0dc%value'

   end subroutine copy_type_temporary_t_0dc

   subroutine copy_arr_type_temporary_t_0dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_0dc), pointer :: structure_in(:)
     type (type_temporary_t_0dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_0dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_0dc'
     end if

   end subroutine copy_arr_type_temporary_t_0dc

   subroutine copy_type_temporary_t_0di(structure_in, structure_out)

     implicit none

     type (type_temporary_t_0di), intent(in) :: structure_in
     type (type_temporary_t_0di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0di%identifier'

     call copy_type_integer(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0di%value'

   end subroutine copy_type_temporary_t_0di

   subroutine copy_arr_type_temporary_t_0di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_0di), pointer :: structure_in(:)
     type (type_temporary_t_0di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_0di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_0di'
     end if

   end subroutine copy_arr_type_temporary_t_0di

   subroutine copy_type_temporary_t_0dr(structure_in, structure_out)

     implicit none

     type (type_temporary_t_0dr), intent(in) :: structure_in
     type (type_temporary_t_0dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0dr%identifier'

     call copy_type_float(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0dr%value'

   end subroutine copy_type_temporary_t_0dr

   subroutine copy_arr_type_temporary_t_0dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_0dr), pointer :: structure_in(:)
     type (type_temporary_t_0dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_0dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_0dr'
     end if

   end subroutine copy_arr_type_temporary_t_0dr

   subroutine copy_type_temporary_t_0ds(structure_in, structure_out)

     implicit none

     type (type_temporary_t_0ds), intent(in) :: structure_in
     type (type_temporary_t_0ds), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0ds%identifier'

     call copy_type_vecstring_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_0ds%value'

   end subroutine copy_type_temporary_t_0ds

   subroutine copy_arr_type_temporary_t_0ds(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_0ds), pointer :: structure_in(:)
     type (type_temporary_t_0ds), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_0ds(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_0ds'
     end if

   end subroutine copy_arr_type_temporary_t_0ds

   subroutine copy_type_temporary_t_1dc(structure_in, structure_out)

     implicit none

     type (type_temporary_t_1dc), intent(in) :: structure_in
     type (type_temporary_t_1dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_1dc%identifier'

     call copy_type_veccplx_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_1dc%value'

   end subroutine copy_type_temporary_t_1dc

   subroutine copy_arr_type_temporary_t_1dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_1dc), pointer :: structure_in(:)
     type (type_temporary_t_1dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_1dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_1dc'
     end if

   end subroutine copy_arr_type_temporary_t_1dc

   subroutine copy_type_temporary_t_1di(structure_in, structure_out)

     implicit none

     type (type_temporary_t_1di), intent(in) :: structure_in
     type (type_temporary_t_1di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_1di%identifier'

     call copy_type_vecint_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_1di%value'

   end subroutine copy_type_temporary_t_1di

   subroutine copy_arr_type_temporary_t_1di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_1di), pointer :: structure_in(:)
     type (type_temporary_t_1di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_1di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_1di'
     end if

   end subroutine copy_arr_type_temporary_t_1di

   subroutine copy_type_temporary_t_1dr(structure_in, structure_out)

     implicit none

     type (type_temporary_t_1dr), intent(in) :: structure_in
     type (type_temporary_t_1dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_1dr%identifier'

     call copy_type_vecflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_1dr%value'

   end subroutine copy_type_temporary_t_1dr

   subroutine copy_arr_type_temporary_t_1dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_1dr), pointer :: structure_in(:)
     type (type_temporary_t_1dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_1dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_1dr'
     end if

   end subroutine copy_arr_type_temporary_t_1dr

   subroutine copy_type_temporary_t_2dc(structure_in, structure_out)

     implicit none

     type (type_temporary_t_2dc), intent(in) :: structure_in
     type (type_temporary_t_2dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_2dc%identifier'

     call copy_type_matcplx_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_2dc%value'

   end subroutine copy_type_temporary_t_2dc

   subroutine copy_arr_type_temporary_t_2dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_2dc), pointer :: structure_in(:)
     type (type_temporary_t_2dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_2dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_2dc'
     end if

   end subroutine copy_arr_type_temporary_t_2dc

   subroutine copy_type_temporary_t_2di(structure_in, structure_out)

     implicit none

     type (type_temporary_t_2di), intent(in) :: structure_in
     type (type_temporary_t_2di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_2di%identifier'

     call copy_type_matint_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_2di%value'

   end subroutine copy_type_temporary_t_2di

   subroutine copy_arr_type_temporary_t_2di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_2di), pointer :: structure_in(:)
     type (type_temporary_t_2di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_2di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_2di'
     end if

   end subroutine copy_arr_type_temporary_t_2di

   subroutine copy_type_temporary_t_2dr(structure_in, structure_out)

     implicit none

     type (type_temporary_t_2dr), intent(in) :: structure_in
     type (type_temporary_t_2dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_2dr%identifier'

     call copy_type_matflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_2dr%value'

   end subroutine copy_type_temporary_t_2dr

   subroutine copy_arr_type_temporary_t_2dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_2dr), pointer :: structure_in(:)
     type (type_temporary_t_2dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_2dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_2dr'
     end if

   end subroutine copy_arr_type_temporary_t_2dr

   subroutine copy_type_temporary_t_3dc(structure_in, structure_out)

     implicit none

     type (type_temporary_t_3dc), intent(in) :: structure_in
     type (type_temporary_t_3dc), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_3dc%identifier'

     call copy_type_array3dcplx_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_3dc%value'

   end subroutine copy_type_temporary_t_3dc

   subroutine copy_arr_type_temporary_t_3dc(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_3dc), pointer :: structure_in(:)
     type (type_temporary_t_3dc), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_3dc(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_3dc'
     end if

   end subroutine copy_arr_type_temporary_t_3dc

   subroutine copy_type_temporary_t_3di(structure_in, structure_out)

     implicit none

     type (type_temporary_t_3di), intent(in) :: structure_in
     type (type_temporary_t_3di), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_3di%identifier'

     call copy_type_array3dint_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_3di%value'

   end subroutine copy_type_temporary_t_3di

   subroutine copy_arr_type_temporary_t_3di(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_3di), pointer :: structure_in(:)
     type (type_temporary_t_3di), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_3di(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_3di'
     end if

   end subroutine copy_arr_type_temporary_t_3di

   subroutine copy_type_temporary_t_3dr(structure_in, structure_out)

     implicit none

     type (type_temporary_t_3dr), intent(in) :: structure_in
     type (type_temporary_t_3dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_3dr%identifier'

     call copy_type_array3dflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_3dr%value'

   end subroutine copy_type_temporary_t_3dr

   subroutine copy_arr_type_temporary_t_3dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_3dr), pointer :: structure_in(:)
     type (type_temporary_t_3dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_3dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_3dr'
     end if

   end subroutine copy_arr_type_temporary_t_3dr

   subroutine copy_type_temporary_t_4dr(structure_in, structure_out)

     implicit none

     type (type_temporary_t_4dr), intent(in) :: structure_in
     type (type_temporary_t_4dr), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%identifier, structure_out%identifier)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_4dr%identifier'

     call copy_type_array4dflt_type(structure_in%value, structure_out%value)
     if (verbose > 0) write(iu6, *) 'copied temporary_t_4dr%value'

   end subroutine copy_type_temporary_t_4dr

   subroutine copy_arr_type_temporary_t_4dr(structure_in, structure_out)
 
     implicit none
 
     type (type_temporary_t_4dr), pointer :: structure_in(:)
     type (type_temporary_t_4dr), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_temporary_t_4dr(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_temporary_t_4dr'
     end if

   end subroutine copy_arr_type_temporary_t_4dr

   subroutine copy_type_tf_desc_tfcoils(structure_in, structure_out)

     implicit none

     type (type_tf_desc_tfcoils), intent(in) :: structure_in
     type (type_tf_desc_tfcoils), intent(inout) :: structure_out

     call copy_type_integer(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils%type'

     call copy_type_float(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils%phi'

     call copy_type_circularcoil(structure_in%circularcoil, structure_out%circularcoil)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils%circularcoil'

     call copy_type_planecoil(structure_in%planecoil, structure_out%planecoil)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils%planecoil'

     call copy_type_tf_structure(structure_in%inboard, structure_out%inboard)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils%inboard'

     call copy_type_tf_structure(structure_in%outboard, structure_out%outboard)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils%outboard'

   end subroutine copy_type_tf_desc_tfcoils

   subroutine copy_arr_type_tf_desc_tfcoils(structure_in, structure_out)
 
     implicit none
 
     type (type_tf_desc_tfcoils), pointer :: structure_in(:)
     type (type_tf_desc_tfcoils), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tf_desc_tfcoils(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tf_desc_tfcoils'
     end if

   end subroutine copy_arr_type_tf_desc_tfcoils

   subroutine copy_type_tf_desc_tfcoils_board(structure_in, structure_out)

     implicit none

     type (type_tf_desc_tfcoils_board), intent(in) :: structure_in
     type (type_tf_desc_tfcoils_board), intent(inout) :: structure_out

     call copy_type_tf_structure(structure_in%structure, structure_out%structure)
     if (verbose > 0) write(iu6, *) 'copied tf_desc_tfcoils_board%structure'

   end subroutine copy_type_tf_desc_tfcoils_board

   subroutine copy_arr_type_tf_desc_tfcoils_board(structure_in, structure_out)
 
     implicit none
 
     type (type_tf_desc_tfcoils_board), pointer :: structure_in(:)
     type (type_tf_desc_tfcoils_board), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tf_desc_tfcoils_board(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tf_desc_tfcoils_board'
     end if

   end subroutine copy_arr_type_tf_desc_tfcoils_board

   subroutine copy_type_tf_structure(structure_in, structure_out)

     implicit none

     type (type_tf_structure), intent(in) :: structure_in
     type (type_tf_structure), intent(inout) :: structure_out

     call copy_type_float(structure_in%jcable, structure_out%jcable)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%jcable'

     call copy_type_float(structure_in%tisotf, structure_out%tisotf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%tisotf'

     call copy_type_float(structure_in%efcasing, structure_out%efcasing)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%efcasing'

     call copy_type_float(structure_in%escasing, structure_out%escasing)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%escasing'

     call copy_type_float(structure_in%sigjackettf, structure_out%sigjackettf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%sigjackettf'

     call copy_type_float(structure_in%sigvaulttf, structure_out%sigvaulttf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%sigvaulttf'

     call copy_type_float(structure_in%ktf, structure_out%ktf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%ktf'

     call copy_type_float(structure_in%ritf, structure_out%ritf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%ritf'

     call copy_type_float(structure_in%riitf, structure_out%riitf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%riitf'

     call copy_type_float(structure_in%retf, structure_out%retf)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%retf'

     call copy_type_float(structure_in%he_fraction, structure_out%he_fraction)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%he_fraction'

     call copy_type_float(structure_in%ss_fraction, structure_out%ss_fraction)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%ss_fraction'

     call copy_type_float(structure_in%pow_dens_wp, structure_out%pow_dens_wp)
     if (verbose > 0) write(iu6, *) 'copied tf_structure%pow_dens_wp'

   end subroutine copy_type_tf_structure

   subroutine copy_arr_type_tf_structure(structure_in, structure_out)
 
     implicit none
 
     type (type_tf_structure), pointer :: structure_in(:)
     type (type_tf_structure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tf_structure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tf_structure'
     end if

   end subroutine copy_arr_type_tf_structure

   subroutine copy_type_theta_info(structure_in, structure_out)

     implicit none

     type (type_theta_info), intent(in) :: structure_in
     type (type_theta_info), intent(inout) :: structure_out

     call copy_type_integer(structure_in%angl_type, structure_out%angl_type)
     if (verbose > 0) write(iu6, *) 'copied theta_info%angl_type'

     call copy_type_matflt_type(structure_in%th2th_pol, structure_out%th2th_pol)
     if (verbose > 0) write(iu6, *) 'copied theta_info%th2th_pol'

   end subroutine copy_type_theta_info

   subroutine copy_arr_type_theta_info(structure_in, structure_out)
 
     implicit none
 
     type (type_theta_info), pointer :: structure_in(:)
     type (type_theta_info), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_theta_info(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_theta_info'
     end if

   end subroutine copy_arr_type_theta_info

   subroutine copy_type_topo_regions(structure_in, structure_out)

     implicit none

     type (type_topo_regions), intent(in) :: structure_in
     type (type_topo_regions), intent(inout) :: structure_out

     call copy_type_integer(structure_in%ind_omnigen, structure_out%ind_omnigen)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%ind_omnigen'

     call copy_type_array6dflt_type(structure_in%dim1, structure_out%dim1)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%dim1'

     call copy_type_array6dflt_type(structure_in%dim2, structure_out%dim2)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%dim2'

     call copy_type_array6dflt_type(structure_in%dim3, structure_out%dim3)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%dim3'

     call copy_type_array6dflt_type(structure_in%dim4, structure_out%dim4)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%dim4'

     call copy_type_array6dflt_type(structure_in%dim5, structure_out%dim5)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%dim5'

     call copy_type_array6dflt_type(structure_in%dim6, structure_out%dim6)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%dim6'

     call copy_type_array6dflt_type(structure_in%jacobian, structure_out%jacobian)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%jacobian'

     call copy_type_array6dflt_type(structure_in%distfunc, structure_out%distfunc)
     if (verbose > 0) write(iu6, *) 'copied topo_regions%distfunc'

   end subroutine copy_type_topo_regions

   subroutine copy_arr_type_topo_regions(structure_in, structure_out)
 
     implicit none
 
     type (type_topo_regions), pointer :: structure_in(:)
     type (type_topo_regions), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_topo_regions(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_topo_regions'
     end if

   end subroutine copy_arr_type_topo_regions

   subroutine copy_type_toroid_field(structure_in, structure_out)

     implicit none

     type (type_toroid_field), intent(in) :: structure_in
     type (type_toroid_field), intent(inout) :: structure_out

     call copy_type_float(structure_in%b0, structure_out%b0)
     if (verbose > 0) write(iu6, *) 'copied toroid_field%b0'

     call copy_type_float(structure_in%b0prime, structure_out%b0prime)
     if (verbose > 0) write(iu6, *) 'copied toroid_field%b0prime'

     call copy_type_float(structure_in%r0, structure_out%r0)
     if (verbose > 0) write(iu6, *) 'copied toroid_field%r0'

     call copy_type_float(structure_in%time, structure_out%time)
     if (verbose > 0) write(iu6, *) 'copied toroid_field%time'

   end subroutine copy_type_toroid_field

   subroutine copy_arr_type_toroid_field(structure_in, structure_out)
 
     implicit none
 
     type (type_toroid_field), pointer :: structure_in(:)
     type (type_toroid_field), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_toroid_field(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_toroid_field'
     end if

   end subroutine copy_arr_type_toroid_field

   subroutine copy_type_trace(structure_in, structure_out)

     implicit none

     type (type_trace), intent(in) :: structure_in
     type (type_trace), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%time_orb, structure_out%time_orb)
     if (verbose > 0) write(iu6, *) 'copied trace%time_orb'

     call copy_type_vecint_type(structure_in%ntorb, structure_out%ntorb)
     if (verbose > 0) write(iu6, *) 'copied trace%ntorb'

     call copy_type_matflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied trace%r'

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied trace%z'

     call copy_type_matflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied trace%phi'

     call copy_type_matflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied trace%psi'

     call copy_type_matflt_type(structure_in%theta_b, structure_out%theta_b)
     if (verbose > 0) write(iu6, *) 'copied trace%theta_b'

     call copy_type_matflt_type(structure_in%v_parallel, structure_out%v_parallel)
     if (verbose > 0) write(iu6, *) 'copied trace%v_parallel'

     call copy_type_matflt_type(structure_in%v_perp, structure_out%v_perp)
     if (verbose > 0) write(iu6, *) 'copied trace%v_perp'

   end subroutine copy_type_trace

   subroutine copy_arr_type_trace(structure_in, structure_out)
 
     implicit none
 
     type (type_trace), pointer :: structure_in(:)
     type (type_trace), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_trace(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_trace'
     end if

   end subroutine copy_arr_type_trace

   subroutine copy_type_transcoefel(structure_in, structure_out)

     implicit none

     type (type_transcoefel), intent(in) :: structure_in
     type (type_transcoefel), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%diff_eff, structure_out%diff_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefel%diff_eff'

     call copy_type_vecflt_type(structure_in%vconv_eff, structure_out%vconv_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefel%vconv_eff'

     call copy_type_vecflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied transcoefel%flux'

     call copy_type_offdiagel(structure_in%off_diagonal, structure_out%off_diagonal)
     if (verbose > 0) write(iu6, *) 'copied transcoefel%off_diagonal'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied transcoefel%flag'

   end subroutine copy_type_transcoefel

   subroutine copy_arr_type_transcoefel(structure_in, structure_out)
 
     implicit none
 
     type (type_transcoefel), pointer :: structure_in(:)
     type (type_transcoefel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_transcoefel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_transcoefel'
     end if

   end subroutine copy_arr_type_transcoefel

   subroutine copy_type_transcoefimp(structure_in, structure_out)

     implicit none

     type (type_transcoefimp), intent(in) :: structure_in
     type (type_transcoefimp), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%diff_eff, structure_out%diff_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefimp%diff_eff'

     call copy_type_matflt_type(structure_in%vconv_eff, structure_out%vconv_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefimp%vconv_eff'

     call copy_type_matflt_type(structure_in%exchange, structure_out%exchange)
     if (verbose > 0) write(iu6, *) 'copied transcoefimp%exchange'

     call copy_type_matflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied transcoefimp%flux'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied transcoefimp%flag'

   end subroutine copy_type_transcoefimp

   subroutine copy_arr_type_transcoefimp(structure_in, structure_out)
 
     implicit none
 
     type (type_transcoefimp), pointer :: structure_in(:)
     type (type_transcoefimp), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_transcoefimp(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_transcoefimp'
     end if

   end subroutine copy_arr_type_transcoefimp

   subroutine copy_type_transcoefion(structure_in, structure_out)

     implicit none

     type (type_transcoefion), intent(in) :: structure_in
     type (type_transcoefion), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%diff_eff, structure_out%diff_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%diff_eff'

     call copy_type_matflt_type(structure_in%vconv_eff, structure_out%vconv_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%vconv_eff'

     call copy_type_matflt_type(structure_in%exchange, structure_out%exchange)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%exchange'

     call copy_type_matflt_type(structure_in%qgi, structure_out%qgi)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%qgi'

     call copy_type_matflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%flux'

     call copy_type_offdiagion(structure_in%off_diagonal, structure_out%off_diagonal)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%off_diagonal'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied transcoefion%flag'

   end subroutine copy_type_transcoefion

   subroutine copy_arr_type_transcoefion(structure_in, structure_out)
 
     implicit none
 
     type (type_transcoefion), pointer :: structure_in(:)
     type (type_transcoefion), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_transcoefion(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_transcoefion'
     end if

   end subroutine copy_arr_type_transcoefion

   subroutine copy_type_transcoefvtor(structure_in, structure_out)

     implicit none

     type (type_transcoefvtor), intent(in) :: structure_in
     type (type_transcoefvtor), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%diff_eff, structure_out%diff_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefvtor%diff_eff'

     call copy_type_matflt_type(structure_in%vconv_eff, structure_out%vconv_eff)
     if (verbose > 0) write(iu6, *) 'copied transcoefvtor%vconv_eff'

     call copy_type_matflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied transcoefvtor%flux'

     call copy_type_offdiagion(structure_in%off_diagonal, structure_out%off_diagonal)
     if (verbose > 0) write(iu6, *) 'copied transcoefvtor%off_diagonal'

     call copy_type_integer(structure_in%flag, structure_out%flag)
     if (verbose > 0) write(iu6, *) 'copied transcoefvtor%flag'

   end subroutine copy_type_transcoefvtor

   subroutine copy_arr_type_transcoefvtor(structure_in, structure_out)
 
     implicit none
 
     type (type_transcoefvtor), pointer :: structure_in(:)
     type (type_transcoefvtor), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_transcoefvtor(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_transcoefvtor'
     end if

   end subroutine copy_arr_type_transcoefvtor

   subroutine copy_type_trap_type(structure_in, structure_out)

     implicit none

     type (type_trap_type), intent(in) :: structure_in
     type (type_trap_type), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%trap_id, structure_out%trap_id)
     if (verbose > 0) write(iu6, *) 'copied trap_type%trap_id'

     call copy_type_integer(structure_in%compound, structure_out%compound)
     if (verbose > 0) write(iu6, *) 'copied trap_type%compound'

     call copy_type_integer(structure_in%gas_species, structure_out%gas_species)
     if (verbose > 0) write(iu6, *) 'copied trap_type%gas_species'

     call copy_type_float(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied trap_type%energy'

     call copy_type_matflt_type(structure_in%fill_factor, structure_out%fill_factor)
     if (verbose > 0) write(iu6, *) 'copied trap_type%fill_factor'

     call copy_type_matflt_type(structure_in%density, structure_out%density)
     if (verbose > 0) write(iu6, *) 'copied trap_type%density'

   end subroutine copy_type_trap_type

   subroutine copy_arr_type_trap_type(structure_in, structure_out)
 
     implicit none
 
     type (type_trap_type), pointer :: structure_in(:)
     type (type_trap_type), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_trap_type(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_trap_type'
     end if

   end subroutine copy_arr_type_trap_type

   subroutine copy_type_trianglexyz(structure_in, structure_out)

     implicit none

     type (type_trianglexyz), intent(in) :: structure_in
     type (type_trianglexyz), intent(inout) :: structure_out

     call copy_type_xyz0D(structure_in%point1, structure_out%point1)
     if (verbose > 0) write(iu6, *) 'copied trianglexyz%point1'

     call copy_type_xyz0D(structure_in%point2, structure_out%point2)
     if (verbose > 0) write(iu6, *) 'copied trianglexyz%point2'

     call copy_type_xyz0D(structure_in%point3, structure_out%point3)
     if (verbose > 0) write(iu6, *) 'copied trianglexyz%point3'

   end subroutine copy_type_trianglexyz

   subroutine copy_arr_type_trianglexyz(structure_in, structure_out)
 
     implicit none
 
     type (type_trianglexyz), pointer :: structure_in(:)
     type (type_trianglexyz), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_trianglexyz(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_trianglexyz'
     end if

   end subroutine copy_arr_type_trianglexyz

   subroutine copy_type_tsmeasure(structure_in, structure_out)

     implicit none

     type (type_tsmeasure), intent(in) :: structure_in
     type (type_tsmeasure), intent(inout) :: structure_out

     call copy_type_exp1D(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied tsmeasure%te'

     call copy_type_exp1D(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied tsmeasure%ne'

   end subroutine copy_type_tsmeasure

   subroutine copy_arr_type_tsmeasure(structure_in, structure_out)
 
     implicit none
 
     type (type_tsmeasure), pointer :: structure_in(:)
     type (type_tsmeasure), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tsmeasure(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tsmeasure'
     end if

   end subroutine copy_arr_type_tsmeasure

   subroutine copy_type_tssetup(structure_in, structure_out)

     implicit none

     type (type_tssetup), intent(in) :: structure_in
     type (type_tssetup), intent(inout) :: structure_out

     call copy_type_rzphi1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied tssetup%position'

   end subroutine copy_type_tssetup

   subroutine copy_arr_type_tssetup(structure_in, structure_out)
 
     implicit none
 
     type (type_tssetup), pointer :: structure_in(:)
     type (type_tssetup), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_tssetup(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_tssetup'
     end if

   end subroutine copy_arr_type_tssetup

   subroutine copy_type_turbcomposition(structure_in, structure_out)

     implicit none

     type (type_turbcomposition), intent(in) :: structure_in
     type (type_turbcomposition), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%amn, structure_out%amn)
     if (verbose > 0) write(iu6, *) 'copied turbcomposition%amn'

     call copy_type_vecflt_type(structure_in%zn, structure_out%zn)
     if (verbose > 0) write(iu6, *) 'copied turbcomposition%zn'

     call copy_type_vecflt_type(structure_in%zion, structure_out%zion)
     if (verbose > 0) write(iu6, *) 'copied turbcomposition%zion'

     call copy_type_vecflt_type(structure_in%ie_mass, structure_out%ie_mass)
     if (verbose > 0) write(iu6, *) 'copied turbcomposition%ie_mass'

   end subroutine copy_type_turbcomposition

   subroutine copy_arr_type_turbcomposition(structure_in, structure_out)
 
     implicit none
 
     type (type_turbcomposition), pointer :: structure_in(:)
     type (type_turbcomposition), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbcomposition(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbcomposition'
     end if

   end subroutine copy_arr_type_turbcomposition

   subroutine copy_type_turbcoordsys(structure_in, structure_out)

     implicit none

     type (type_turbcoordsys), intent(in) :: structure_in
     type (type_turbcoordsys), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%grid_type, structure_out%grid_type)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%grid_type'

     call copy_type_turbgrid(structure_in%turbgrid, structure_out%turbgrid)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%turbgrid'

     call copy_type_matflt_type(structure_in%jacobian, structure_out%jacobian)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%jacobian'

     call copy_type_matflt_type(structure_in%g_11, structure_out%g_11)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%g_11'

     call copy_type_matflt_type(structure_in%g_12, structure_out%g_12)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%g_12'

     call copy_type_matflt_type(structure_in%g_13, structure_out%g_13)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%g_13'

     call copy_type_matflt_type(structure_in%g_22, structure_out%g_22)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%g_22'

     call copy_type_matflt_type(structure_in%g_23, structure_out%g_23)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%g_23'

     call copy_type_matflt_type(structure_in%g_33, structure_out%g_33)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%g_33'

     call copy_type_rzphi3D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied turbcoordsys%position'

   end subroutine copy_type_turbcoordsys

   subroutine copy_arr_type_turbcoordsys(structure_in, structure_out)
 
     implicit none
 
     type (type_turbcoordsys), pointer :: structure_in(:)
     type (type_turbcoordsys), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbcoordsys(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbcoordsys'
     end if

   end subroutine copy_arr_type_turbcoordsys

   subroutine copy_type_turbenv1d(structure_in, structure_out)

     implicit none

     type (type_turbenv1d), intent(in) :: structure_in
     type (type_turbenv1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%theta'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%phi'

     call copy_type_vecflt_type(structure_in%vor, structure_out%vor)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%vor'

     call copy_type_vecflt_type(structure_in%jpl, structure_out%jpl)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%jpl'

     call copy_type_vecflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%ne'

     call copy_type_vecflt_type(structure_in%he, structure_out%he)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%he'

     call copy_type_vecflt_type(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%te'

     call copy_type_matflt_type(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%ni'

     call copy_type_matflt_type(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%ti'

     call copy_type_matflt_type(structure_in%ui, structure_out%ui)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%ui'

     call copy_type_vecflt_type(structure_in%fe, structure_out%fe)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%fe'

     call copy_type_vecflt_type(structure_in%qe, structure_out%qe)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%qe'

     call copy_type_matflt_type(structure_in%qi, structure_out%qi)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%qi'

     call copy_type_vecflt_type(structure_in%me, structure_out%me)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%me'

     call copy_type_matflt_type(structure_in%mi, structure_out%mi)
     if (verbose > 0) write(iu6, *) 'copied turbenv1d%mi'

   end subroutine copy_type_turbenv1d

   subroutine copy_arr_type_turbenv1d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbenv1d), pointer :: structure_in(:)
     type (type_turbenv1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbenv1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbenv1d'
     end if

   end subroutine copy_arr_type_turbenv1d

   subroutine copy_type_turbgrid(structure_in, structure_out)

     implicit none

     type (type_turbgrid), intent(in) :: structure_in
     type (type_turbgrid), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%dim1, structure_out%dim1)
     if (verbose > 0) write(iu6, *) 'copied turbgrid%dim1'

     call copy_type_vecflt_type(structure_in%dim2, structure_out%dim2)
     if (verbose > 0) write(iu6, *) 'copied turbgrid%dim2'

     call copy_type_vecflt_type(structure_in%dim3, structure_out%dim3)
     if (verbose > 0) write(iu6, *) 'copied turbgrid%dim3'

     call copy_type_vecflt_type(structure_in%dim_v1, structure_out%dim_v1)
     if (verbose > 0) write(iu6, *) 'copied turbgrid%dim_v1'

     call copy_type_vecflt_type(structure_in%dim_v2, structure_out%dim_v2)
     if (verbose > 0) write(iu6, *) 'copied turbgrid%dim_v2'

   end subroutine copy_type_turbgrid

   subroutine copy_arr_type_turbgrid(structure_in, structure_out)
 
     implicit none
 
     type (type_turbgrid), pointer :: structure_in(:)
     type (type_turbgrid), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbgrid(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbgrid'
     end if

   end subroutine copy_arr_type_turbgrid

   subroutine copy_type_turbspec1d(structure_in, structure_out)

     implicit none

     type (type_turbspec1d), intent(in) :: structure_in
     type (type_turbspec1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%kperp, structure_out%kperp)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%kperp'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%phi'

     call copy_type_vecflt_type(structure_in%vor, structure_out%vor)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%vor'

     call copy_type_vecflt_type(structure_in%b, structure_out%b)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%b'

     call copy_type_vecflt_type(structure_in%jpl, structure_out%jpl)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%jpl'

     call copy_type_vecflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%ne'

     call copy_type_vecflt_type(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%te'

     call copy_type_matflt_type(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%ti'

     call copy_type_vecflt_type(structure_in%fe, structure_out%fe)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%fe'

     call copy_type_vecflt_type(structure_in%qe, structure_out%qe)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%qe'

     call copy_type_matflt_type(structure_in%qi, structure_out%qi)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%qi'

     call copy_type_vecflt_type(structure_in%me, structure_out%me)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%me'

     call copy_type_matflt_type(structure_in%mi, structure_out%mi)
     if (verbose > 0) write(iu6, *) 'copied turbspec1d%mi'

   end subroutine copy_type_turbspec1d

   subroutine copy_arr_type_turbspec1d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbspec1d), pointer :: structure_in(:)
     type (type_turbspec1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbspec1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbspec1d'
     end if

   end subroutine copy_arr_type_turbspec1d

   subroutine copy_type_turbvar0d(structure_in, structure_out)

     implicit none

     type (type_turbvar0d), intent(in) :: structure_in
     type (type_turbvar0d), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%dtime_type, structure_out%dtime_type)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%dtime_type'

     call copy_type_vecflt_type(structure_in%dtime, structure_out%dtime)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%dtime'

     call copy_type_vecflt_type(structure_in%en_exb, structure_out%en_exb)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_exb'

     call copy_type_vecflt_type(structure_in%en_mag, structure_out%en_mag)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_mag'

     call copy_type_vecflt_type(structure_in%en_el_th, structure_out%en_el_th)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_el_th'

     call copy_type_matflt_type(structure_in%en_ion_th, structure_out%en_ion_th)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_ion_th'

     call copy_type_vecflt_type(structure_in%en_el_par, structure_out%en_el_par)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_el_par'

     call copy_type_matflt_type(structure_in%en_ion_par, structure_out%en_ion_par)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_ion_par'

     call copy_type_vecflt_type(structure_in%en_tot, structure_out%en_tot)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%en_tot'

     call copy_type_vecflt_type(structure_in%fl_el, structure_out%fl_el)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_el'

     call copy_type_vecflt_type(structure_in%fl_heatel, structure_out%fl_heatel)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_heatel'

     call copy_type_matflt_type(structure_in%fl_ion, structure_out%fl_ion)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_ion'

     call copy_type_matflt_type(structure_in%fl_heation, structure_out%fl_heation)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_heation'

     call copy_type_vecflt_type(structure_in%fl_magel, structure_out%fl_magel)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_magel'

     call copy_type_vecflt_type(structure_in%fl_magheatel, structure_out%fl_magheatel)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_magheatel'

     call copy_type_matflt_type(structure_in%fl_magion, structure_out%fl_magion)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%fl_magion'

     call copy_type_matflt_type(structure_in%flmagheation, structure_out%flmagheation)
     if (verbose > 0) write(iu6, *) 'copied turbvar0d%flmagheation'

   end subroutine copy_type_turbvar0d

   subroutine copy_arr_type_turbvar0d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbvar0d), pointer :: structure_in(:)
     type (type_turbvar0d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbvar0d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbvar0d'
     end if

   end subroutine copy_arr_type_turbvar0d

   subroutine copy_type_turbvar1d(structure_in, structure_out)

     implicit none

     type (type_turbvar1d), intent(in) :: structure_in
     type (type_turbvar1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%phi'

     call copy_type_vecflt_type(structure_in%er, structure_out%er)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%er'

     call copy_type_vecflt_type(structure_in%vor, structure_out%vor)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%vor'

     call copy_type_vecflt_type(structure_in%apl, structure_out%apl)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%apl'

     call copy_type_vecflt_type(structure_in%jpl, structure_out%jpl)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%jpl'

     call copy_type_vecflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%ne'

     call copy_type_vecflt_type(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%te'

     call copy_type_matflt_type(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%ni'

     call copy_type_matflt_type(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%ti'

     call copy_type_matflt_type(structure_in%ui, structure_out%ui)
     if (verbose > 0) write(iu6, *) 'copied turbvar1d%ui'

   end subroutine copy_type_turbvar1d

   subroutine copy_arr_type_turbvar1d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbvar1d), pointer :: structure_in(:)
     type (type_turbvar1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbvar1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbvar1d'
     end if

   end subroutine copy_arr_type_turbvar1d

   subroutine copy_type_turbvar2d(structure_in, structure_out)

     implicit none

     type (type_turbvar2d), intent(in) :: structure_in
     type (type_turbvar2d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%theta'

     call copy_type_matflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%phi'

     call copy_type_matflt_type(structure_in%apl, structure_out%apl)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%apl'

     call copy_type_matflt_type(structure_in%jpl, structure_out%jpl)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%jpl'

     call copy_type_matflt_type(structure_in%vor, structure_out%vor)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%vor'

     call copy_type_matflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%ne'

     call copy_type_matflt_type(structure_in%te, structure_out%te)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%te'

     call copy_type_array3dflt_type(structure_in%ni, structure_out%ni)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%ni'

     call copy_type_array3dflt_type(structure_in%ti, structure_out%ti)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%ti'

     call copy_type_array3dflt_type(structure_in%ui, structure_out%ui)
     if (verbose > 0) write(iu6, *) 'copied turbvar2d%ui'

   end subroutine copy_type_turbvar2d

   subroutine copy_arr_type_turbvar2d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbvar2d), pointer :: structure_in(:)
     type (type_turbvar2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbvar2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbvar2d'
     end if

   end subroutine copy_arr_type_turbvar2d

   subroutine copy_type_turbvar3d(structure_in, structure_out)

     implicit none

     type (type_turbvar3d), intent(in) :: structure_in
     type (type_turbvar3d), intent(inout) :: structure_out

     call copy_type_array3dflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied turbvar3d%phi'

     call copy_type_array3dflt_type(structure_in%vor, structure_out%vor)
     if (verbose > 0) write(iu6, *) 'copied turbvar3d%vor'

     call copy_type_array3dflt_type(structure_in%jpl, structure_out%jpl)
     if (verbose > 0) write(iu6, *) 'copied turbvar3d%jpl'

     call copy_type_array3dflt_type(structure_in%ne, structure_out%ne)
     if (verbose > 0) write(iu6, *) 'copied turbvar3d%ne'

   end subroutine copy_type_turbvar3d

   subroutine copy_arr_type_turbvar3d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbvar3d), pointer :: structure_in(:)
     type (type_turbvar3d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbvar3d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbvar3d'
     end if

   end subroutine copy_arr_type_turbvar3d

   subroutine copy_type_turbvar4d(structure_in, structure_out)

     implicit none

     type (type_turbvar4d), intent(in) :: structure_in
     type (type_turbvar4d), intent(inout) :: structure_out

     call copy_type_array4dflt_type(structure_in%fe, structure_out%fe)
     if (verbose > 0) write(iu6, *) 'copied turbvar4d%fe'

     call copy_type_array5dflt_type(structure_in%fi, structure_out%fi)
     if (verbose > 0) write(iu6, *) 'copied turbvar4d%fi'

   end subroutine copy_type_turbvar4d

   subroutine copy_arr_type_turbvar4d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbvar4d), pointer :: structure_in(:)
     type (type_turbvar4d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbvar4d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbvar4d'
     end if

   end subroutine copy_arr_type_turbvar4d

   subroutine copy_type_turbvar5d(structure_in, structure_out)

     implicit none

     type (type_turbvar5d), intent(in) :: structure_in
     type (type_turbvar5d), intent(inout) :: structure_out

     call copy_type_array5dflt_type(structure_in%fe, structure_out%fe)
     if (verbose > 0) write(iu6, *) 'copied turbvar5d%fe'

     call copy_type_array6dflt_type(structure_in%fi, structure_out%fi)
     if (verbose > 0) write(iu6, *) 'copied turbvar5d%fi'

   end subroutine copy_type_turbvar5d

   subroutine copy_arr_type_turbvar5d(structure_in, structure_out)
 
     implicit none
 
     type (type_turbvar5d), pointer :: structure_in(:)
     type (type_turbvar5d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_turbvar5d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_turbvar5d'
     end if

   end subroutine copy_arr_type_turbvar5d

   subroutine copy_type_version_ind(structure_in, structure_out)

     implicit none

     type (type_version_ind), intent(in) :: structure_in
     type (type_version_ind), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%description, structure_out%description)
     if (verbose > 0) write(iu6, *) 'copied version_ind%description'

     call copy_type_vecstring_type(structure_in%releasedate, structure_out%releasedate)
     if (verbose > 0) write(iu6, *) 'copied version_ind%releasedate'

     call copy_arr_type_data_release(structure_in%data_release, structure_out%data_release)
     if (verbose > 0) write(iu6, *) 'copied version_ind%data_release'

   end subroutine copy_type_version_ind

   subroutine copy_arr_type_version_ind(structure_in, structure_out)
 
     implicit none
 
     type (type_version_ind), pointer :: structure_in(:)
     type (type_version_ind), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_version_ind(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_version_ind'
     end if

   end subroutine copy_arr_type_version_ind

   subroutine copy_type_wall2d(structure_in, structure_out)

     implicit none

     type (type_wall2d), intent(in) :: structure_in
     type (type_wall2d), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%wall_id, structure_out%wall_id)
     if (verbose > 0) write(iu6, *) 'copied wall2d%wall_id'

     call copy_type_wall_limiter(structure_in%limiter, structure_out%limiter)
     if (verbose > 0) write(iu6, *) 'copied wall2d%limiter'

     call copy_type_wall_vessel(structure_in%vessel, structure_out%vessel)
     if (verbose > 0) write(iu6, *) 'copied wall2d%vessel'

     call copy_arr_type_plasmaComplexType(structure_in%plasma, structure_out%plasma)
     if (verbose > 0) write(iu6, *) 'copied wall2d%plasma'

     call copy_arr_type_wall_unitsComplexType(structure_in%wall_state, structure_out%wall_state)
     if (verbose > 0) write(iu6, *) 'copied wall2d%wall_state'

   end subroutine copy_type_wall2d

   subroutine copy_arr_type_wall2d(structure_in, structure_out)
 
     implicit none
 
     type (type_wall2d), pointer :: structure_in(:)
     type (type_wall2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall2d'
     end if

   end subroutine copy_arr_type_wall2d

   subroutine copy_type_wall2d_mhd(structure_in, structure_out)

     implicit none

     type (type_wall2d_mhd), intent(in) :: structure_in
     type (type_wall2d_mhd), intent(inout) :: structure_out

     call copy_arr_type_mhd_res_wall2d(structure_in%res_wall, structure_out%res_wall)
     if (verbose > 0) write(iu6, *) 'copied wall2d_mhd%res_wall'

     call copy_type_mhd_ideal_wall2d(structure_in%ideal_wall, structure_out%ideal_wall)
     if (verbose > 0) write(iu6, *) 'copied wall2d_mhd%ideal_wall'

   end subroutine copy_type_wall2d_mhd

   subroutine copy_arr_type_wall2d_mhd(structure_in, structure_out)
 
     implicit none
 
     type (type_wall2d_mhd), pointer :: structure_in(:)
     type (type_wall2d_mhd), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall2d_mhd(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall2d_mhd'
     end if

   end subroutine copy_arr_type_wall2d_mhd

   subroutine copy_type_wall3d(structure_in, structure_out)

     implicit none

     type (type_wall3d), intent(in) :: structure_in
     type (type_wall3d), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%wall_id, structure_out%wall_id)
     if (verbose > 0) write(iu6, *) 'copied wall3d%wall_id'

     call copy_type_complexgrid(structure_in%grid, structure_out%grid)
     if (verbose > 0) write(iu6, *) 'copied wall3d%grid'

     call copy_arr_type_plasmaComplexType(structure_in%plasma, structure_out%plasma)
     if (verbose > 0) write(iu6, *) 'copied wall3d%plasma'

     call copy_arr_type_wall_unitsComplexType(structure_in%wall_state, structure_out%wall_state)
     if (verbose > 0) write(iu6, *) 'copied wall3d%wall_state'

     call copy_type_integer(structure_in%basis_index, structure_out%basis_index)
     if (verbose > 0) write(iu6, *) 'copied wall3d%basis_index'

   end subroutine copy_type_wall3d

   subroutine copy_arr_type_wall3d(structure_in, structure_out)
 
     implicit none
 
     type (type_wall3d), pointer :: structure_in(:)
     type (type_wall3d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall3d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall3d'
     end if

   end subroutine copy_arr_type_wall3d

   subroutine copy_type_wall_blocks(structure_in, structure_out)

     implicit none

     type (type_wall_blocks), intent(in) :: structure_in
     type (type_wall_blocks), intent(inout) :: structure_out

     call copy_arr_type_wall_blocks_unit(structure_in%blocks_unit, structure_out%blocks_unit)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks%blocks_unit'

   end subroutine copy_type_wall_blocks

   subroutine copy_arr_type_wall_blocks(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_blocks), pointer :: structure_in(:)
     type (type_wall_blocks), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_blocks(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_blocks'
     end if

   end subroutine copy_arr_type_wall_blocks

   subroutine copy_type_wall_blocks_unit(structure_in, structure_out)

     implicit none

     type (type_wall_blocks_unit), intent(in) :: structure_in
     type (type_wall_blocks_unit), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks_unit%name'

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks_unit%position'

     call copy_type_float(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks_unit%eta'

     call copy_type_float(structure_in%permeability, structure_out%permeability)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks_unit%permeability'

     call copy_type_float(structure_in%j_phi, structure_out%j_phi)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks_unit%j_phi'

     call copy_type_float(structure_in%resistance, structure_out%resistance)
     if (verbose > 0) write(iu6, *) 'copied wall_blocks_unit%resistance'

   end subroutine copy_type_wall_blocks_unit

   subroutine copy_arr_type_wall_blocks_unit(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_blocks_unit), pointer :: structure_in(:)
     type (type_wall_blocks_unit), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_blocks_unit(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_blocks_unit'
     end if

   end subroutine copy_arr_type_wall_blocks_unit

   subroutine copy_type_wall_limiter(structure_in, structure_out)

     implicit none

     type (type_wall_limiter), intent(in) :: structure_in
     type (type_wall_limiter), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%limiter_id, structure_out%limiter_id)
     if (verbose > 0) write(iu6, *) 'copied wall_limiter%limiter_id'

     call copy_arr_type_limiter_unit(structure_in%limiter_unit, structure_out%limiter_unit)
     if (verbose > 0) write(iu6, *) 'copied wall_limiter%limiter_unit'

   end subroutine copy_type_wall_limiter

   subroutine copy_arr_type_wall_limiter(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_limiter), pointer :: structure_in(:)
     type (type_wall_limiter), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_limiter(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_limiter'
     end if

   end subroutine copy_arr_type_wall_limiter

   subroutine copy_type_wall_types(structure_in, structure_out)

     implicit none

     type (type_wall_types), intent(in) :: structure_in
     type (type_wall_types), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%label, structure_out%label)
     if (verbose > 0) write(iu6, *) 'copied wall_types%label'

     call copy_arr_type_wall_types_layers(structure_in%layers, structure_out%layers)
     if (verbose > 0) write(iu6, *) 'copied wall_types%layers'

   end subroutine copy_type_wall_types

   subroutine copy_arr_type_wall_types(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_types), pointer :: structure_in(:)
     type (type_wall_types), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_types(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_types'
     end if

   end subroutine copy_arr_type_wall_types

   subroutine copy_type_wall_types_layers(structure_in, structure_out)

     implicit none

     type (type_wall_types_layers), intent(in) :: structure_in
     type (type_wall_types_layers), intent(inout) :: structure_out

     call copy_type_float(structure_in%thickness, structure_out%thickness)
     if (verbose > 0) write(iu6, *) 'copied wall_types_layers%thickness'

     call copy_type_vecflt_type(structure_in%chem_comp, structure_out%chem_comp)
     if (verbose > 0) write(iu6, *) 'copied wall_types_layers%chem_comp'

   end subroutine copy_type_wall_types_layers

   subroutine copy_arr_type_wall_types_layers(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_types_layers), pointer :: structure_in(:)
     type (type_wall_types_layers), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_types_layers(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_types_layers'
     end if

   end subroutine copy_arr_type_wall_types_layers

   subroutine copy_type_wall_unitsComplexType(structure_in, structure_out)

     implicit none

     type (type_wall_unitsComplexType), intent(in) :: structure_in
     type (type_wall_unitsComplexType), intent(inout) :: structure_out

     call copy_type_integer(structure_in%wall_type, structure_out%wall_type)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType%wall_type'

     call copy_type_integer(structure_in%n_depo_layer, structure_out%n_depo_layer)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType%n_depo_layer'

     call copy_arr_type_wall_unitsComplexType_layers(structure_in%layers, structure_out%layers)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType%layers'

     call copy_type_complexgrid_scalar(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType%eta'

     call copy_type_complexgrid_scalar(structure_in%permeability, structure_out%permeability)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType%permeability'

     call copy_type_complexgrid_vector(structure_in%j, structure_out%j)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType%j'

   end subroutine copy_type_wall_unitsComplexType

   subroutine copy_arr_type_wall_unitsComplexType(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_unitsComplexType), pointer :: structure_in(:)
     type (type_wall_unitsComplexType), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_unitsComplexType(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_unitsComplexType'
     end if

   end subroutine copy_arr_type_wall_unitsComplexType

   subroutine copy_type_wall_unitsComplexType_layers(structure_in, structure_out)

     implicit none

     type (type_wall_unitsComplexType_layers), intent(in) :: structure_in
     type (type_wall_unitsComplexType_layers), intent(inout) :: structure_out

     call copy_type_vecint_type(structure_in%elements, structure_out%elements)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%elements'

     call copy_type_vecint_type(structure_in%gases, structure_out%gases)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%gases'

     call copy_type_vecint_type(structure_in%compounds, structure_out%compounds)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%compounds'

     call copy_type_matflt_type(structure_in%density, structure_out%density)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%density'

     call copy_type_matflt_type(structure_in%dx, structure_out%dx)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%dx'

     call copy_type_vecflt_type(structure_in%thickness, structure_out%thickness)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%thickness'

     call copy_type_array3dflt_type(structure_in%roughness, structure_out%roughness)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%roughness'

     call copy_type_array3dflt_type(structure_in%porosity, structure_out%porosity)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%porosity'

     call copy_type_matflt_type(structure_in%dpa, structure_out%dpa)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%dpa'

     call copy_type_matflt_type(structure_in%temperature, structure_out%temperature)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%temperature'

     call copy_type_array3dflt_type(structure_in%element_frac, structure_out%element_frac)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%element_frac'

     call copy_type_array3dflt_type(structure_in%chem_comp, structure_out%chem_comp)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%chem_comp'

     call copy_type_array4dflt_type(structure_in%bulk_D, structure_out%bulk_D)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%bulk_D'

     call copy_type_array4dflt_type(structure_in%surface_D, structure_out%surface_D)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%surface_D'

     call copy_type_array4dflt_type(structure_in%bulk_solute, structure_out%bulk_solute)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%bulk_solute'

     call copy_type_array4dflt_type(structure_in%surf_solute, structure_out%surf_solute)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%surf_solute'

     call copy_type_array3dflt_type(structure_in%pore_content, structure_out%pore_content)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%pore_content'

     call copy_arr_type_trap_type(structure_in%trap_type, structure_out%trap_type)
     if (verbose > 0) write(iu6, *) 'copied wall_unitsComplexType_layers%trap_type'

   end subroutine copy_type_wall_unitsComplexType_layers

   subroutine copy_arr_type_wall_unitsComplexType_layers(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_unitsComplexType_layers), pointer :: structure_in(:)
     type (type_wall_unitsComplexType_layers), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_unitsComplexType_layers(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_unitsComplexType_layers'
     end if

   end subroutine copy_arr_type_wall_unitsComplexType_layers

   subroutine copy_type_wall_vessel(structure_in, structure_out)

     implicit none

     type (type_wall_vessel), intent(in) :: structure_in
     type (type_wall_vessel), intent(inout) :: structure_out

     call copy_type_identifier(structure_in%vessel_id, structure_out%vessel_id)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel%vessel_id'

     call copy_arr_type_wall_vessel_unit(structure_in%vessel_unit, structure_out%vessel_unit)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel%vessel_unit'

   end subroutine copy_type_wall_vessel

   subroutine copy_arr_type_wall_vessel(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_vessel), pointer :: structure_in(:)
     type (type_wall_vessel), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_vessel(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_vessel'
     end if

   end subroutine copy_arr_type_wall_vessel

   subroutine copy_type_wall_vessel_annular(structure_in, structure_out)

     implicit none

     type (type_wall_vessel_annular), intent(in) :: structure_in
     type (type_wall_vessel_annular), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_annular%name'

     call copy_type_rz1D(structure_in%inside, structure_out%inside)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_annular%inside'

     call copy_type_rz1D(structure_in%outside, structure_out%outside)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_annular%outside'

     call copy_type_float(structure_in%eta, structure_out%eta)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_annular%eta'

     call copy_type_float(structure_in%permeability, structure_out%permeability)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_annular%permeability'

   end subroutine copy_type_wall_vessel_annular

   subroutine copy_arr_type_wall_vessel_annular(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_vessel_annular), pointer :: structure_in(:)
     type (type_wall_vessel_annular), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_vessel_annular(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_vessel_annular'
     end if

   end subroutine copy_arr_type_wall_vessel_annular

   subroutine copy_type_wall_vessel_unit(structure_in, structure_out)

     implicit none

     type (type_wall_vessel_unit), intent(in) :: structure_in
     type (type_wall_vessel_unit), intent(inout) :: structure_out

     call copy_type_wall_vessel_annular(structure_in%annular, structure_out%annular)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_unit%annular'

     call copy_type_wall_blocks(structure_in%blocks, structure_out%blocks)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_unit%blocks'

     call copy_type_wall_wall2d_vessel_radial_build(structure_in%radial_build, structure_out%radial_build)
     if (verbose > 0) write(iu6, *) 'copied wall_vessel_unit%radial_build'

   end subroutine copy_type_wall_vessel_unit

   subroutine copy_arr_type_wall_vessel_unit(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_vessel_unit), pointer :: structure_in(:)
     type (type_wall_vessel_unit), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_vessel_unit(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_vessel_unit'
     end if

   end subroutine copy_arr_type_wall_vessel_unit

   subroutine copy_type_wall_wall0d(structure_in, structure_out)

     implicit none

     type (type_wall_wall0d), intent(in) :: structure_in
     type (type_wall_wall0d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%pumping_speed, structure_out%pumping_speed)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%pumping_speed'

     call copy_type_vecflt_type(structure_in%gas_puff, structure_out%gas_puff)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%gas_puff'

     call copy_type_vecflt_type(structure_in%wall_inventory, structure_out%wall_inventory)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%wall_inventory'

     call copy_type_vecflt_type(structure_in%recycling_coefficient, structure_out%recycling_coefficient)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%recycling_coefficient'

     call copy_type_float(structure_in%wall_temperature, structure_out%wall_temperature)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%wall_temperature'

     call copy_type_float(structure_in%power_from_plasma, structure_out%power_from_plasma)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%power_from_plasma'

     call copy_type_float(structure_in%power_to_cooling, structure_out%power_to_cooling)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%power_to_cooling'

     call copy_type_wall_wall0d_plasma(structure_in%plasma, structure_out%plasma)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d%plasma'

   end subroutine copy_type_wall_wall0d

   subroutine copy_arr_type_wall_wall0d(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_wall0d), pointer :: structure_in(:)
     type (type_wall_wall0d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_wall0d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_wall0d'
     end if

   end subroutine copy_arr_type_wall_wall0d

   subroutine copy_type_wall_wall0d_plasma(structure_in, structure_out)

     implicit none

     type (type_wall_wall0d_plasma), intent(in) :: structure_in
     type (type_wall_wall0d_plasma), intent(inout) :: structure_out

     call copy_type_matint_type(structure_in%species_index, structure_out%species_index)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d_plasma%species_index'

     call copy_type_vecflt_type(structure_in%flux, structure_out%flux)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d_plasma%flux'

     call copy_type_vecflt_type(structure_in%energy, structure_out%energy)
     if (verbose > 0) write(iu6, *) 'copied wall_wall0d_plasma%energy'

   end subroutine copy_type_wall_wall0d_plasma

   subroutine copy_arr_type_wall_wall0d_plasma(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_wall0d_plasma), pointer :: structure_in(:)
     type (type_wall_wall0d_plasma), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_wall0d_plasma(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_wall0d_plasma'
     end if

   end subroutine copy_arr_type_wall_wall0d_plasma

   subroutine copy_type_wall_wall2d_vessel_radial_build(structure_in, structure_out)

     implicit none

     type (type_wall_wall2d_vessel_radial_build), intent(in) :: structure_in
     type (type_wall_wall2d_vessel_radial_build), intent(inout) :: structure_out

     call copy_type_float(structure_in%r1_inb, structure_out%r1_inb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%r1_inb'

     call copy_type_float(structure_in%r2_inb, structure_out%r2_inb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%r2_inb'

     call copy_type_float(structure_in%r1_outb, structure_out%r1_outb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%r1_outb'

     call copy_type_float(structure_in%r2_outb, structure_out%r2_outb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%r2_outb'

     call copy_type_float(structure_in%raddim, structure_out%raddim)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%raddim'

     call copy_type_float(structure_in%nmat, structure_out%nmat)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%nmat'

     call copy_type_vecflt_type(structure_in%composition, structure_out%composition)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%composition'

     call copy_type_float(structure_in%pow_dens_inb, structure_out%pow_dens_inb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%pow_dens_inb'

     call copy_type_float(structure_in%pow_dens_outb, structure_out%pow_dens_outb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%pow_dens_outb'

     call copy_type_float(structure_in%fn_flux_inb, structure_out%fn_flux_inb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%fn_flux_inb'

     call copy_type_float(structure_in%fn_flux_outb, structure_out%fn_flux_outb)
     if (verbose > 0) write(iu6, *) 'copied wall_wall2d_vessel_radial_build%fn_flux_outb'

   end subroutine copy_type_wall_wall2d_vessel_radial_build

   subroutine copy_arr_type_wall_wall2d_vessel_radial_build(structure_in, structure_out)
 
     implicit none
 
     type (type_wall_wall2d_vessel_radial_build), pointer :: structure_in(:)
     type (type_wall_wall2d_vessel_radial_build), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_wall_wall2d_vessel_radial_build(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_wall_wall2d_vessel_radial_build'
     end if

   end subroutine copy_arr_type_wall_wall2d_vessel_radial_build

   subroutine copy_type_waveguides(structure_in, structure_out)

     implicit none

     type (type_waveguides), intent(in) :: structure_in
     type (type_waveguides), intent(inout) :: structure_out

     call copy_type_integer(structure_in%nwm_theta, structure_out%nwm_theta)
     if (verbose > 0) write(iu6, *) 'copied waveguides%nwm_theta'

     call copy_type_integer(structure_in%nwm_phi, structure_out%nwm_phi)
     if (verbose > 0) write(iu6, *) 'copied waveguides%nwm_phi'

     call copy_type_vecint_type(structure_in%mask, structure_out%mask)
     if (verbose > 0) write(iu6, *) 'copied waveguides%mask'

     call copy_type_integer(structure_in%npwbm_phi, structure_out%npwbm_phi)
     if (verbose > 0) write(iu6, *) 'copied waveguides%npwbm_phi'

     call copy_type_integer(structure_in%npwe_phi, structure_out%npwe_phi)
     if (verbose > 0) write(iu6, *) 'copied waveguides%npwe_phi'

     call copy_type_float(structure_in%sw_theta, structure_out%sw_theta)
     if (verbose > 0) write(iu6, *) 'copied waveguides%sw_theta'

     call copy_type_float(structure_in%hw_theta, structure_out%hw_theta)
     if (verbose > 0) write(iu6, *) 'copied waveguides%hw_theta'

     call copy_type_float(structure_in%bwa, structure_out%bwa)
     if (verbose > 0) write(iu6, *) 'copied waveguides%bwa'

     call copy_type_float(structure_in%biwp, structure_out%biwp)
     if (verbose > 0) write(iu6, *) 'copied waveguides%biwp'

     call copy_type_float(structure_in%bewp, structure_out%bewp)
     if (verbose > 0) write(iu6, *) 'copied waveguides%bewp'

     call copy_type_vecflt_type(structure_in%e_phi, structure_out%e_phi)
     if (verbose > 0) write(iu6, *) 'copied waveguides%e_phi'

     call copy_type_vecflt_type(structure_in%scl, structure_out%scl)
     if (verbose > 0) write(iu6, *) 'copied waveguides%scl'

   end subroutine copy_type_waveguides

   subroutine copy_arr_type_waveguides(structure_in, structure_out)
 
     implicit none
 
     type (type_waveguides), pointer :: structure_in(:)
     type (type_waveguides), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waveguides(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waveguides'
     end if

   end subroutine copy_arr_type_waveguides

   subroutine copy_type_waves_global_param(structure_in, structure_out)

     implicit none

     type (type_waves_global_param), intent(in) :: structure_in
     type (type_waves_global_param), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%name, structure_out%name)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%name'

     call copy_type_vecstring_type(structure_in%type, structure_out%type)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%type'

     call copy_type_vecint_type(structure_in%f_assumption, structure_out%f_assumption)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%f_assumption'

     call copy_type_integer(structure_in%code_type, structure_out%code_type)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%code_type'

     call copy_type_float(structure_in%frequency, structure_out%frequency)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%frequency'

     call copy_type_vecint_type(structure_in%ntor, structure_out%ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%ntor'

     call copy_type_float(structure_in%power_tot, structure_out%power_tot)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%power_tot'

     call copy_type_vecflt_type(structure_in%p_frac_ntor, structure_out%p_frac_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%p_frac_ntor'

     call copy_type_float(structure_in%pow_e, structure_out%pow_e)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_e'

     call copy_type_vecflt_type(structure_in%pow_i, structure_out%pow_i)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_i'

     call copy_type_matflt_type(structure_in%pow_z, structure_out%pow_z)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_z'

     call copy_type_float(structure_in%pow_fe, structure_out%pow_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_fe'

     call copy_type_vecflt_type(structure_in%pow_fi, structure_out%pow_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_fi'

     call copy_type_matflt_type(structure_in%pow_fz, structure_out%pow_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_fz'

     call copy_type_vecflt_type(structure_in%pow_ntor_e, structure_out%pow_ntor_e)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_ntor_e'

     call copy_type_matflt_type(structure_in%pow_ntor_i, structure_out%pow_ntor_i)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_ntor_i'

     call copy_type_array3dflt_type(structure_in%pow_ntor_z, structure_out%pow_ntor_z)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_ntor_z'

     call copy_type_vecflt_type(structure_in%pow_ntor_fe, structure_out%pow_ntor_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_ntor_fe'

     call copy_type_matflt_type(structure_in%pow_ntor_fi, structure_out%pow_ntor_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_ntor_fi'

     call copy_type_array3dflt_type(structure_in%pow_ntor_fz, structure_out%pow_ntor_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%pow_ntor_fz'

     call copy_type_float(structure_in%cur_tor, structure_out%cur_tor)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%cur_tor'

     call copy_type_vecflt_type(structure_in%cur_tor_ntor, structure_out%cur_tor_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%cur_tor_ntor'

     call copy_type_rz0D(structure_in%mag_axis, structure_out%mag_axis)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%mag_axis'

     call copy_type_b0r0(structure_in%toroid_field, structure_out%toroid_field)
     if (verbose > 0) write(iu6, *) 'copied waves_global_param%toroid_field'

   end subroutine copy_type_waves_global_param

   subroutine copy_arr_type_waves_global_param(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_global_param), pointer :: structure_in(:)
     type (type_waves_global_param), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_global_param(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_global_param'
     end if

   end subroutine copy_arr_type_waves_global_param

   subroutine copy_type_waves_grid_1d(structure_in, structure_out)

     implicit none

     type (type_waves_grid_1d), intent(in) :: structure_in
     type (type_waves_grid_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_1d%rho_tor'

     call copy_type_vecflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_1d%rho_tor_norm'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_1d%psi'

     call copy_type_vecflt_type(structure_in%volume, structure_out%volume)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_1d%volume'

     call copy_type_vecflt_type(structure_in%area, structure_out%area)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_1d%area'

   end subroutine copy_type_waves_grid_1d

   subroutine copy_arr_type_waves_grid_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_grid_1d), pointer :: structure_in(:)
     type (type_waves_grid_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_grid_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_grid_1d'
     end if

   end subroutine copy_arr_type_waves_grid_1d

   subroutine copy_type_waves_grid_2d(structure_in, structure_out)

     implicit none

     type (type_waves_grid_2d), intent(in) :: structure_in
     type (type_waves_grid_2d), intent(inout) :: structure_out

     call copy_type_integer(structure_in%grid_type, structure_out%grid_type)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%grid_type'

     call copy_type_matflt_type(structure_in%rho_tor_norm, structure_out%rho_tor_norm)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%rho_tor_norm'

     call copy_type_matflt_type(structure_in%rho_tor, structure_out%rho_tor)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%rho_tor'

     call copy_type_matflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%psi'

     call copy_type_matflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%theta'

     call copy_type_matflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%r'

     call copy_type_matflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%z'

     call copy_type_theta_info(structure_in%theta_info, structure_out%theta_info)
     if (verbose > 0) write(iu6, *) 'copied waves_grid_2d%theta_info'

   end subroutine copy_type_waves_grid_2d

   subroutine copy_arr_type_waves_grid_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_grid_2d), pointer :: structure_in(:)
     type (type_waves_grid_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_grid_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_grid_2d'
     end if

   end subroutine copy_arr_type_waves_grid_2d

   subroutine copy_type_waves_profiles_1d(structure_in, structure_out)

     implicit none

     type (type_waves_profiles_1d), intent(in) :: structure_in
     type (type_waves_profiles_1d), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%powd_tot, structure_out%powd_tot)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_tot'

     call copy_type_vecflt_type(structure_in%powd_e, structure_out%powd_e)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_e'

     call copy_type_matflt_type(structure_in%powd_i, structure_out%powd_i)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_i'

     call copy_type_array3dflt_type(structure_in%powd_z, structure_out%powd_z)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_z'

     call copy_type_vecflt_type(structure_in%powd_fe, structure_out%powd_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_fe'

     call copy_type_matflt_type(structure_in%powd_fi, structure_out%powd_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_fi'

     call copy_type_array3dflt_type(structure_in%powd_fz, structure_out%powd_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_fz'

     call copy_type_matflt_type(structure_in%powd_ntor, structure_out%powd_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor'

     call copy_type_matflt_type(structure_in%powd_ntor_e, structure_out%powd_ntor_e)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor_e'

     call copy_type_array3dflt_type(structure_in%powd_ntor_i, structure_out%powd_ntor_i)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor_i'

     call copy_type_array4dflt_type(structure_in%powd_ntor_z, structure_out%powd_ntor_z)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor_z'

     call copy_type_matflt_type(structure_in%powd_ntor_fe, structure_out%powd_ntor_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor_fe'

     call copy_type_array3dflt_type(structure_in%powd_ntor_fi, structure_out%powd_ntor_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor_fi'

     call copy_type_array4dflt_type(structure_in%powd_ntor_fz, structure_out%powd_ntor_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%powd_ntor_fz'

     call copy_type_vecflt_type(structure_in%curd_tor, structure_out%curd_tor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%curd_tor'

     call copy_type_matflt_type(structure_in%curd_torntor, structure_out%curd_torntor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%curd_torntor'

     call copy_type_vecflt_type(structure_in%pow_tot, structure_out%pow_tot)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_tot'

     call copy_type_vecflt_type(structure_in%pow_e, structure_out%pow_e)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_e'

     call copy_type_matflt_type(structure_in%pow_i, structure_out%pow_i)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_i'

     call copy_type_array3dflt_type(structure_in%pow_z, structure_out%pow_z)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_z'

     call copy_type_vecflt_type(structure_in%pow_fe, structure_out%pow_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_fe'

     call copy_type_matflt_type(structure_in%pow_fi, structure_out%pow_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_fi'

     call copy_type_array3dflt_type(structure_in%pow_fz, structure_out%pow_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_fz'

     call copy_type_matflt_type(structure_in%pow_ntor, structure_out%pow_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor'

     call copy_type_matflt_type(structure_in%pow_ntor_e, structure_out%pow_ntor_e)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor_e'

     call copy_type_array3dflt_type(structure_in%pow_ntor_i, structure_out%pow_ntor_i)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor_i'

     call copy_type_array3dflt_type(structure_in%pow_ntor_z, structure_out%pow_ntor_z)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor_z'

     call copy_type_matflt_type(structure_in%pow_ntor_fe, structure_out%pow_ntor_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor_fe'

     call copy_type_array3dflt_type(structure_in%pow_ntor_fi, structure_out%pow_ntor_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor_fi'

     call copy_type_array3dflt_type(structure_in%pow_ntor_fz, structure_out%pow_ntor_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%pow_ntor_fz'

     call copy_type_vecflt_type(structure_in%curd_par, structure_out%curd_par)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%curd_par'

     call copy_type_matflt_type(structure_in%curd_parntor, structure_out%curd_parntor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%curd_parntor'

     call copy_type_vecflt_type(structure_in%cur_tor, structure_out%cur_tor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%cur_tor'

     call copy_type_matflt_type(structure_in%cur_tor_ntor, structure_out%cur_tor_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%cur_tor_ntor'

     call copy_type_matflt_type(structure_in%e_plus_ave, structure_out%e_plus_ave)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%e_plus_ave'

     call copy_type_matflt_type(structure_in%e_minus_ave, structure_out%e_minus_ave)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%e_minus_ave'

     call copy_type_matflt_type(structure_in%e_para_ave, structure_out%e_para_ave)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%e_para_ave'

     call copy_type_matflt_type(structure_in%k_perp_ave, structure_out%k_perp_ave)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_1d%k_perp_ave'

   end subroutine copy_type_waves_profiles_1d

   subroutine copy_arr_type_waves_profiles_1d(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_profiles_1d), pointer :: structure_in(:)
     type (type_waves_profiles_1d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_profiles_1d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_profiles_1d'
     end if

   end subroutine copy_arr_type_waves_profiles_1d

   subroutine copy_type_waves_profiles_2d(structure_in, structure_out)

     implicit none

     type (type_waves_profiles_2d), intent(in) :: structure_in
     type (type_waves_profiles_2d), intent(inout) :: structure_out

     call copy_type_matflt_type(structure_in%powd_tot, structure_out%powd_tot)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_tot'

     call copy_type_matflt_type(structure_in%powd_e, structure_out%powd_e)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_e'

     call copy_type_array3dflt_type(structure_in%powd_i, structure_out%powd_i)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_i'

     call copy_type_array4dflt_type(structure_in%powd_z, structure_out%powd_z)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_z'

     call copy_type_matflt_type(structure_in%powd_fe, structure_out%powd_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_fe'

     call copy_type_array3dflt_type(structure_in%powd_fi, structure_out%powd_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_fi'

     call copy_type_array4dflt_type(structure_in%powd_fz, structure_out%powd_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_fz'

     call copy_type_array3dflt_type(structure_in%powd_ntor, structure_out%powd_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor'

     call copy_type_array3dflt_type(structure_in%powd_ntor_e, structure_out%powd_ntor_e)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor_e'

     call copy_type_array4dflt_type(structure_in%powd_ntor_i, structure_out%powd_ntor_i)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor_i'

     call copy_type_array5dflt_type(structure_in%powd_ntor_z, structure_out%powd_ntor_z)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor_z'

     call copy_type_array3dflt_type(structure_in%powd_ntor_fe, structure_out%powd_ntor_fe)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor_fe'

     call copy_type_array4dflt_type(structure_in%powd_ntor_fi, structure_out%powd_ntor_fi)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor_fi'

     call copy_type_array5dflt_type(structure_in%powd_ntor_fz, structure_out%powd_ntor_fz)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_ntor_fz'

     call copy_type_array5dflt_type(structure_in%powd_iharm, structure_out%powd_iharm)
     if (verbose > 0) write(iu6, *) 'copied waves_profiles_2d%powd_iharm'

   end subroutine copy_type_waves_profiles_2d

   subroutine copy_arr_type_waves_profiles_2d(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_profiles_2d), pointer :: structure_in(:)
     type (type_waves_profiles_2d), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_profiles_2d(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_profiles_2d'
     end if

   end subroutine copy_arr_type_waves_profiles_2d

   subroutine copy_type_waves_rtposition(structure_in, structure_out)

     implicit none

     type (type_waves_rtposition), intent(in) :: structure_in
     type (type_waves_rtposition), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%r, structure_out%r)
     if (verbose > 0) write(iu6, *) 'copied waves_rtposition%r'

     call copy_type_vecflt_type(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied waves_rtposition%z'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied waves_rtposition%phi'

     call copy_type_vecflt_type(structure_in%psi, structure_out%psi)
     if (verbose > 0) write(iu6, *) 'copied waves_rtposition%psi'

     call copy_type_vecflt_type(structure_in%theta, structure_out%theta)
     if (verbose > 0) write(iu6, *) 'copied waves_rtposition%theta'

   end subroutine copy_type_waves_rtposition

   subroutine copy_arr_type_waves_rtposition(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_rtposition), pointer :: structure_in(:)
     type (type_waves_rtposition), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_rtposition(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_rtposition'
     end if

   end subroutine copy_arr_type_waves_rtposition

   subroutine copy_type_waves_rtwavevector(structure_in, structure_out)

     implicit none

     type (type_waves_rtwavevector), intent(in) :: structure_in
     type (type_waves_rtwavevector), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%kr, structure_out%kr)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%kr'

     call copy_type_vecflt_type(structure_in%kz, structure_out%kz)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%kz'

     call copy_type_vecflt_type(structure_in%kphi, structure_out%kphi)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%kphi'

     call copy_type_vecflt_type(structure_in%npar, structure_out%npar)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%npar'

     call copy_type_vecflt_type(structure_in%nperp, structure_out%nperp)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%nperp'

     call copy_type_vecflt_type(structure_in%ntor, structure_out%ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%ntor'

     call copy_type_integer(structure_in%var_ntor, structure_out%var_ntor)
     if (verbose > 0) write(iu6, *) 'copied waves_rtwavevector%var_ntor'

   end subroutine copy_type_waves_rtwavevector

   subroutine copy_arr_type_waves_rtwavevector(structure_in, structure_out)
 
     implicit none
 
     type (type_waves_rtwavevector), pointer :: structure_in(:)
     type (type_waves_rtwavevector), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_waves_rtwavevector(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_waves_rtwavevector'
     end if

   end subroutine copy_arr_type_waves_rtwavevector

   subroutine copy_type_weighted_markers(structure_in, structure_out)

     implicit none

     type (type_weighted_markers), intent(in) :: structure_in
     type (type_weighted_markers), intent(inout) :: structure_out

     call copy_arr_type_identifier(structure_in%variable_ids, structure_out%variable_ids)
     if (verbose > 0) write(iu6, *) 'copied weighted_markers%variable_ids'

     call copy_type_matflt_type(structure_in%coord, structure_out%coord)
     if (verbose > 0) write(iu6, *) 'copied weighted_markers%coord'

     call copy_type_vecflt_type(structure_in%weight, structure_out%weight)
     if (verbose > 0) write(iu6, *) 'copied weighted_markers%weight'

   end subroutine copy_type_weighted_markers

   subroutine copy_arr_type_weighted_markers(structure_in, structure_out)
 
     implicit none
 
     type (type_weighted_markers), pointer :: structure_in(:)
     type (type_weighted_markers), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_weighted_markers(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_weighted_markers'
     end if

   end subroutine copy_arr_type_weighted_markers

   subroutine copy_type_whatref(structure_in, structure_out)

     implicit none

     type (type_whatref), intent(in) :: structure_in
     type (type_whatref), intent(inout) :: structure_out

     call copy_type_vecstring_type(structure_in%user, structure_out%user)
     if (verbose > 0) write(iu6, *) 'copied whatref%user'

     call copy_type_vecstring_type(structure_in%machine, structure_out%machine)
     if (verbose > 0) write(iu6, *) 'copied whatref%machine'

     call copy_type_integer(structure_in%shot, structure_out%shot)
     if (verbose > 0) write(iu6, *) 'copied whatref%shot'

     call copy_type_integer(structure_in%run, structure_out%run)
     if (verbose > 0) write(iu6, *) 'copied whatref%run'

     call copy_type_integer(structure_in%occurrence, structure_out%occurrence)
     if (verbose > 0) write(iu6, *) 'copied whatref%occurrence'

   end subroutine copy_type_whatref

   subroutine copy_arr_type_whatref(structure_in, structure_out)
 
     implicit none
 
     type (type_whatref), pointer :: structure_in(:)
     type (type_whatref), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_whatref(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_whatref'
     end if

   end subroutine copy_arr_type_whatref

   subroutine copy_type_width(structure_in, structure_out)

     implicit none

     type (type_width), intent(in) :: structure_in
     type (type_width), intent(inout) :: structure_out

     call copy_type_vecflt_type(structure_in%dtheta, structure_out%dtheta)
     if (verbose > 0) write(iu6, *) 'copied width%dtheta'

     call copy_type_vecflt_type(structure_in%phi, structure_out%phi)
     if (verbose > 0) write(iu6, *) 'copied width%phi'

   end subroutine copy_type_width

   subroutine copy_arr_type_width(structure_in, structure_out)
 
     implicit none
 
     type (type_width), pointer :: structure_in(:)
     type (type_width), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_width(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_width'
     end if

   end subroutine copy_arr_type_width

   subroutine copy_type_xpts(structure_in, structure_out)

     implicit none

     type (type_xpts), intent(in) :: structure_in
     type (type_xpts), intent(inout) :: structure_out

     call copy_type_rz1D(structure_in%position, structure_out%position)
     if (verbose > 0) write(iu6, *) 'copied xpts%position'

     call copy_type_vecstring_type(structure_in%source, structure_out%source)
     if (verbose > 0) write(iu6, *) 'copied xpts%source'

     call copy_type_vecflt_type(structure_in%weight, structure_out%weight)
     if (verbose > 0) write(iu6, *) 'copied xpts%weight'

     call copy_type_vecflt_type(structure_in%sigma, structure_out%sigma)
     if (verbose > 0) write(iu6, *) 'copied xpts%sigma'

     call copy_type_vecflt_type(structure_in%calculated, structure_out%calculated)
     if (verbose > 0) write(iu6, *) 'copied xpts%calculated'

     call copy_type_vecflt_type(structure_in%chi2, structure_out%chi2)
     if (verbose > 0) write(iu6, *) 'copied xpts%chi2'

   end subroutine copy_type_xpts

   subroutine copy_arr_type_xpts(structure_in, structure_out)
 
     implicit none
 
     type (type_xpts), pointer :: structure_in(:)
     type (type_xpts), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_xpts(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_xpts'
     end if

   end subroutine copy_arr_type_xpts

   subroutine copy_type_xyz0D(structure_in, structure_out)

     implicit none

     type (type_xyz0D), intent(in) :: structure_in
     type (type_xyz0D), intent(inout) :: structure_out

     call copy_type_float(structure_in%x, structure_out%x)
     if (verbose > 0) write(iu6, *) 'copied xyz0D%x'

     call copy_type_float(structure_in%y, structure_out%y)
     if (verbose > 0) write(iu6, *) 'copied xyz0D%y'

     call copy_type_float(structure_in%z, structure_out%z)
     if (verbose > 0) write(iu6, *) 'copied xyz0D%z'

   end subroutine copy_type_xyz0D

   subroutine copy_arr_type_xyz0D(structure_in, structure_out)
 
     implicit none
 
     type (type_xyz0D), pointer :: structure_in(:)
     type (type_xyz0D), pointer :: structure_out(:)
     integer :: i
 
     if (associated(structure_in)) then
       if (associated(structure_out)) then
         deallocate(structure_out)
       end if
       allocate(structure_out(size(structure_in)))
       do i = 1, size(structure_in)
         call copy_type_xyz0D(structure_in(i), structure_out(i))
       end do
       if (verbose > 0) write(iu6, *)'copied array of type_xyz0D'
     end if

   end subroutine copy_arr_type_xyz0D


 end module copy_structures
