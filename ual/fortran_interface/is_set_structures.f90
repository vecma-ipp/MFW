 module is_set_structures
 !------------------------------------------------------------------
 ! module for granular checking whether itm structures
 ! have been set
 !------------------------------------------------------------------

   use euitm_schemas

   interface is_set_cpo
      module procedure is_set_type_integer
      module procedure is_set_type_float
      module procedure is_set_type_complex
      module procedure is_set_type_array3dcplx_type
      module procedure is_set_type_array3dflt_type
      module procedure is_set_type_array3dint_type
      module procedure is_set_type_array4dflt_type
      module procedure is_set_type_array5dflt_type
      module procedure is_set_type_array6dflt_type
      module procedure is_set_type_array7dflt_type
      module procedure is_set_type_matcplx_type
      module procedure is_set_type_matflt_type
      module procedure is_set_type_matint_type
      module procedure is_set_type_veccplx_type
      module procedure is_set_type_vecflt_type
      module procedure is_set_type_vecint_type
      module procedure is_set_type_vecstring_type
      module procedure is_set_type_amns
      module procedure is_set_arr_type_amns
      module procedure is_set_type_antennas
      module procedure is_set_arr_type_antennas
      module procedure is_set_type_bb_shield
      module procedure is_set_arr_type_bb_shield
      module procedure is_set_type_compositionc
      module procedure is_set_arr_type_compositionc
      module procedure is_set_type_coredelta
      module procedure is_set_arr_type_coredelta
      module procedure is_set_type_corefast
      module procedure is_set_arr_type_corefast
      module procedure is_set_type_coreimpur
      module procedure is_set_arr_type_coreimpur
      module procedure is_set_type_coreneutrals
      module procedure is_set_arr_type_coreneutrals
      module procedure is_set_type_coreprof
      module procedure is_set_arr_type_coreprof
      module procedure is_set_type_coresource
      module procedure is_set_arr_type_coresource
      module procedure is_set_type_coretransp
      module procedure is_set_arr_type_coretransp
      module procedure is_set_type_cxdiag
      module procedure is_set_arr_type_cxdiag
      module procedure is_set_type_distribution
      module procedure is_set_arr_type_distribution
      module procedure is_set_type_distsource
      module procedure is_set_arr_type_distsource
      module procedure is_set_type_ecediag
      module procedure is_set_arr_type_ecediag
      module procedure is_set_type_edge
      module procedure is_set_arr_type_edge
      module procedure is_set_type_efcc
      module procedure is_set_arr_type_efcc
      module procedure is_set_type_equilibrium
      module procedure is_set_arr_type_equilibrium
      module procedure is_set_type_fusiondiag
      module procedure is_set_arr_type_fusiondiag
      module procedure is_set_type_halphadiag
      module procedure is_set_arr_type_halphadiag
      module procedure is_set_type_heat_sources
      module procedure is_set_arr_type_heat_sources
      module procedure is_set_type_interfdiag
      module procedure is_set_arr_type_interfdiag
      module procedure is_set_type_ironmodel
      module procedure is_set_arr_type_ironmodel
      module procedure is_set_type_langmuirdiag
      module procedure is_set_arr_type_langmuirdiag
      module procedure is_set_type_launchs
      module procedure is_set_arr_type_launchs
      module procedure is_set_type_lithiumdiag
      module procedure is_set_arr_type_lithiumdiag
      module procedure is_set_type_magdiag
      module procedure is_set_arr_type_magdiag
      module procedure is_set_type_mhd
      module procedure is_set_arr_type_mhd
      module procedure is_set_type_msediag
      module procedure is_set_arr_type_msediag
      module procedure is_set_type_nbi
      module procedure is_set_arr_type_nbi
      module procedure is_set_type_neoclassic
      module procedure is_set_arr_type_neoclassic
      module procedure is_set_type_ntm
      module procedure is_set_arr_type_ntm
      module procedure is_set_type_orbit
      module procedure is_set_arr_type_orbit
      module procedure is_set_type_pellets
      module procedure is_set_arr_type_pellets
      module procedure is_set_type_pfsystems
      module procedure is_set_arr_type_pfsystems
      module procedure is_set_type_polardiag
      module procedure is_set_arr_type_polardiag
      module procedure is_set_type_power_conv
      module procedure is_set_arr_type_power_conv
      module procedure is_set_type_reflectomet
      module procedure is_set_arr_type_reflectomet
      module procedure is_set_type_rfadiag
      module procedure is_set_arr_type_rfadiag
      module procedure is_set_type_sawteeth
      module procedure is_set_arr_type_sawteeth
      module procedure is_set_type_scenario
      module procedure is_set_arr_type_scenario
      module procedure is_set_type_solcurdiag
      module procedure is_set_arr_type_solcurdiag
      module procedure is_set_type_temporary
      module procedure is_set_arr_type_temporary
      module procedure is_set_type_topinfo
      module procedure is_set_arr_type_topinfo
      module procedure is_set_type_toroidfield
      module procedure is_set_arr_type_toroidfield
      module procedure is_set_type_tsdiag
      module procedure is_set_arr_type_tsdiag
      module procedure is_set_type_turbulence
      module procedure is_set_arr_type_turbulence
      module procedure is_set_type_wall
      module procedure is_set_arr_type_wall
      module procedure is_set_type_waves
      module procedure is_set_arr_type_waves
      module procedure is_set_type_amns_constituentType
      module procedure is_set_arr_type_amns_constituentType
      module procedure is_set_type_amns_processType
      module procedure is_set_arr_type_amns_processType
      module procedure is_set_type_antenna_ec
      module procedure is_set_arr_type_antenna_ec
      module procedure is_set_type_antenna_ic
      module procedure is_set_arr_type_antenna_ic
      module procedure is_set_type_antenna_lh
      module procedure is_set_arr_type_antenna_lh
      module procedure is_set_type_antennaic_setup
      module procedure is_set_arr_type_antennaic_setup
      module procedure is_set_type_antennalh_setup
      module procedure is_set_arr_type_antennalh_setup
      module procedure is_set_type_b0r0
      module procedure is_set_arr_type_b0r0
      module procedure is_set_type_bb
      module procedure is_set_arr_type_bb
      module procedure is_set_type_bb_dimension
      module procedure is_set_arr_type_bb_dimension
      module procedure is_set_type_bb_geometry
      module procedure is_set_arr_type_bb_geometry
      module procedure is_set_type_bb_specs
      module procedure is_set_arr_type_bb_specs
      module procedure is_set_type_beamletgroup
      module procedure is_set_arr_type_beamletgroup
      module procedure is_set_type_beamlets
      module procedure is_set_arr_type_beamlets
      module procedure is_set_type_beamtracing
      module procedure is_set_arr_type_beamtracing
      module procedure is_set_type_boundary
      module procedure is_set_arr_type_boundary
      module procedure is_set_type_boundary_neutrals
      module procedure is_set_arr_type_boundary_neutrals
      module procedure is_set_type_boundaryel
      module procedure is_set_arr_type_boundaryel
      module procedure is_set_type_boundaryimp
      module procedure is_set_arr_type_boundaryimp
      module procedure is_set_type_boundaryion
      module procedure is_set_arr_type_boundaryion
      module procedure is_set_type_bpol_probes
      module procedure is_set_arr_type_bpol_probes
      module procedure is_set_type_calorimetry_heat_source
      module procedure is_set_arr_type_calorimetry_heat_source
      module procedure is_set_type_circuits
      module procedure is_set_arr_type_circuits
      module procedure is_set_type_circularcoil
      module procedure is_set_arr_type_circularcoil
      module procedure is_set_type_clusters
      module procedure is_set_arr_type_clusters
      module procedure is_set_type_codeparam
      module procedure is_set_arr_type_codeparam
      module procedure is_set_type_coefficients_neutrals
      module procedure is_set_arr_type_coefficients_neutrals
      module procedure is_set_type_coherentwave
      module procedure is_set_arr_type_coherentwave
      module procedure is_set_type_coil
      module procedure is_set_arr_type_coil
      module procedure is_set_type_com
      module procedure is_set_arr_type_com
      module procedure is_set_type_complexgrid
      module procedure is_set_arr_type_complexgrid
      module procedure is_set_type_complexgrid_geo_global
      module procedure is_set_arr_type_complexgrid_geo_global
      module procedure is_set_type_complexgrid_indexlist
      module procedure is_set_arr_type_complexgrid_indexlist
      module procedure is_set_type_complexgrid_metric
      module procedure is_set_arr_type_complexgrid_metric
      module procedure is_set_type_complexgrid_objectlist
      module procedure is_set_arr_type_complexgrid_objectlist
      module procedure is_set_type_complexgrid_scalar
      module procedure is_set_arr_type_complexgrid_scalar
      module procedure is_set_type_complexgrid_scalar_cplx
      module procedure is_set_arr_type_complexgrid_scalar_cplx
      module procedure is_set_type_complexgrid_scalar_int
      module procedure is_set_arr_type_complexgrid_scalar_int
      module procedure is_set_type_complexgrid_scalar_simplestruct
      module procedure is_set_arr_type_complexgrid_scalar_simplestruct
      module procedure is_set_type_complexgrid_space
      module procedure is_set_arr_type_complexgrid_space
      module procedure is_set_type_complexgrid_subgrid
      module procedure is_set_arr_type_complexgrid_subgrid
      module procedure is_set_type_complexgrid_vector
      module procedure is_set_arr_type_complexgrid_vector
      module procedure is_set_type_complexgrid_vector_simplestruct
      module procedure is_set_arr_type_complexgrid_vector_simplestruct
      module procedure is_set_type_composition
      module procedure is_set_arr_type_composition
      module procedure is_set_type_composition_neutrals
      module procedure is_set_arr_type_composition_neutrals
      module procedure is_set_type_composition_neutrals_neutcomp
      module procedure is_set_arr_type_composition_neutrals_neutcomp
      module procedure is_set_type_composition_neutralscomp
      module procedure is_set_arr_type_composition_neutralscomp
      module procedure is_set_type_compositions_type
      module procedure is_set_arr_type_compositions_type
      module procedure is_set_type_compound_desc
      module procedure is_set_arr_type_compound_desc
      module procedure is_set_type_coord_sys
      module procedure is_set_arr_type_coord_sys
      module procedure is_set_type_coordinates
      module procedure is_set_arr_type_coordinates
      module procedure is_set_type_coords
      module procedure is_set_arr_type_coords
      module procedure is_set_type_coredelta_values
      module procedure is_set_arr_type_coredelta_values
      module procedure is_set_type_coredelta_values_impurity
      module procedure is_set_arr_type_coredelta_values_impurity
      module procedure is_set_type_corefast_values
      module procedure is_set_arr_type_corefast_values
      module procedure is_set_type_corefield
      module procedure is_set_arr_type_corefield
      module procedure is_set_type_corefieldion
      module procedure is_set_arr_type_corefieldion
      module procedure is_set_type_corefieldneutral
      module procedure is_set_arr_type_corefieldneutral
      module procedure is_set_type_corefieldneutrale
      module procedure is_set_arr_type_corefieldneutrale
      module procedure is_set_type_corefieldneutralv
      module procedure is_set_arr_type_corefieldneutralv
      module procedure is_set_type_corefieldneutralv0
      module procedure is_set_arr_type_corefieldneutralv0
      module procedure is_set_type_coreimpurdiag_sum_radiation
      module procedure is_set_arr_type_coreimpurdiag_sum_radiation
      module procedure is_set_type_coreimpurediag_energy
      module procedure is_set_arr_type_coreimpurediag_energy
      module procedure is_set_type_coreimpurediag_radiation
      module procedure is_set_arr_type_coreimpurediag_radiation
      module procedure is_set_type_coreimpurediag_sum
      module procedure is_set_arr_type_coreimpurediag_sum
      module procedure is_set_type_coreimpurediag_sum_energy
      module procedure is_set_arr_type_coreimpurediag_sum_energy
      module procedure is_set_type_coreimpurediag_type
      module procedure is_set_arr_type_coreimpurediag_type
      module procedure is_set_type_coreimpurediagprof_type
      module procedure is_set_arr_type_coreimpurediagprof_type
      module procedure is_set_type_coreimpurediagsum_type
      module procedure is_set_arr_type_coreimpurediagsum_type
      module procedure is_set_type_coreneutrals_atomlist
      module procedure is_set_arr_type_coreneutrals_atomlist
      module procedure is_set_type_coreneutrals_neutraltype
      module procedure is_set_arr_type_coreneutrals_neutraltype
      module procedure is_set_type_coreprofile
      module procedure is_set_arr_type_coreprofile
      module procedure is_set_type_coreprofion
      module procedure is_set_arr_type_coreprofion
      module procedure is_set_type_coresource_values
      module procedure is_set_arr_type_coresource_values
      module procedure is_set_type_coretransel
      module procedure is_set_arr_type_coretransel
      module procedure is_set_type_coretransimp
      module procedure is_set_arr_type_coretransimp
      module procedure is_set_type_coretransion
      module procedure is_set_arr_type_coretransion
      module procedure is_set_type_coretransp_values
      module procedure is_set_arr_type_coretransp_values
      module procedure is_set_type_current
      module procedure is_set_arr_type_current
      module procedure is_set_type_cxmeasure
      module procedure is_set_arr_type_cxmeasure
      module procedure is_set_type_cxsetup
      module procedure is_set_arr_type_cxsetup
      module procedure is_set_type_data_release
      module procedure is_set_arr_type_data_release
      module procedure is_set_type_datainfo
      module procedure is_set_arr_type_datainfo
      module procedure is_set_type_desc_coils
      module procedure is_set_arr_type_desc_coils
      module procedure is_set_type_desc_impur
      module procedure is_set_arr_type_desc_impur
      module procedure is_set_type_desc_iron
      module procedure is_set_arr_type_desc_iron
      module procedure is_set_type_desc_pfcoils
      module procedure is_set_arr_type_desc_pfcoils
      module procedure is_set_type_desc_supply
      module procedure is_set_arr_type_desc_supply
      module procedure is_set_type_diag_func
      module procedure is_set_arr_type_diag_func
      module procedure is_set_type_dist_collisional_transfer_0d
      module procedure is_set_arr_type_dist_collisional_transfer_0d
      module procedure is_set_type_dist_collisional_transfer_1d
      module procedure is_set_arr_type_dist_collisional_transfer_1d
      module procedure is_set_type_dist_collisional_transfer_2d
      module procedure is_set_arr_type_dist_collisional_transfer_2d
      module procedure is_set_type_dist_distrivec_distfunc_fexp_param
      module procedure is_set_arr_type_dist_distrivec_distfunc_fexp_param
      module procedure is_set_type_dist_ff
      module procedure is_set_arr_type_dist_ff
      module procedure is_set_type_dist_func
      module procedure is_set_arr_type_dist_func
      module procedure is_set_type_dist_geometry_0d
      module procedure is_set_arr_type_dist_geometry_0d
      module procedure is_set_type_dist_geometry_1d
      module procedure is_set_arr_type_dist_geometry_1d
      module procedure is_set_type_dist_geometry_2d
      module procedure is_set_arr_type_dist_geometry_2d
      module procedure is_set_type_dist_global_param
      module procedure is_set_arr_type_dist_global_param
      module procedure is_set_type_dist_global_param_collisions_z
      module procedure is_set_arr_type_dist_global_param_collisions_z
      module procedure is_set_type_dist_grid_info
      module procedure is_set_arr_type_dist_grid_info
      module procedure is_set_type_dist_profile_values_1d
      module procedure is_set_arr_type_dist_profile_values_1d
      module procedure is_set_type_dist_profile_values_2d
      module procedure is_set_arr_type_dist_profile_values_2d
      module procedure is_set_type_dist_profiles2d_collisions_z
      module procedure is_set_arr_type_dist_profiles2d_collisions_z
      module procedure is_set_type_dist_profiles_1d
      module procedure is_set_arr_type_dist_profiles_1d
      module procedure is_set_type_dist_profiles_1d_collisions_z
      module procedure is_set_arr_type_dist_profiles_1d_collisions_z
      module procedure is_set_type_dist_profiles_2d
      module procedure is_set_arr_type_dist_profiles_2d
      module procedure is_set_type_dist_sources_0d
      module procedure is_set_arr_type_dist_sources_0d
      module procedure is_set_type_dist_sources_1d
      module procedure is_set_arr_type_dist_sources_1d
      module procedure is_set_type_dist_sources_reference
      module procedure is_set_arr_type_dist_sources_reference
      module procedure is_set_type_dist_state_0d
      module procedure is_set_arr_type_dist_state_0d
      module procedure is_set_type_dist_state_1d
      module procedure is_set_arr_type_dist_state_1d
      module procedure is_set_type_dist_state_2d
      module procedure is_set_arr_type_dist_state_2d
      module procedure is_set_type_dist_thermalised_1d
      module procedure is_set_arr_type_dist_thermalised_1d
      module procedure is_set_type_distri_vec
      module procedure is_set_arr_type_distri_vec
      module procedure is_set_type_distsource_global_param
      module procedure is_set_arr_type_distsource_global_param
      module procedure is_set_type_distsource_line_src_prof
      module procedure is_set_arr_type_distsource_line_src_prof
      module procedure is_set_type_distsource_profiles_1d
      module procedure is_set_arr_type_distsource_profiles_1d
      module procedure is_set_type_distsource_profiles_2d
      module procedure is_set_arr_type_distsource_profiles_2d
      module procedure is_set_type_distsource_source
      module procedure is_set_arr_type_distsource_source
      module procedure is_set_type_divergence
      module procedure is_set_arr_type_divergence
      module procedure is_set_type_e_components
      module procedure is_set_arr_type_e_components
      module procedure is_set_type_ecemeasure
      module procedure is_set_arr_type_ecemeasure
      module procedure is_set_type_ecesetup
      module procedure is_set_arr_type_ecesetup
      module procedure is_set_type_edge_fluid
      module procedure is_set_arr_type_edge_fluid
      module procedure is_set_type_edge_fluid_scalar
      module procedure is_set_arr_type_edge_fluid_scalar
      module procedure is_set_type_edge_fluid_scalar_simplestruct
      module procedure is_set_arr_type_edge_fluid_scalar_simplestruct
      module procedure is_set_type_edge_fluid_scalar_transpcoeff
      module procedure is_set_arr_type_edge_fluid_scalar_transpcoeff
      module procedure is_set_type_edge_fluid_vector
      module procedure is_set_arr_type_edge_fluid_vector
      module procedure is_set_type_edge_fluid_vector_simplestruct
      module procedure is_set_arr_type_edge_fluid_vector_simplestruct
      module procedure is_set_type_edge_kinetic
      module procedure is_set_arr_type_edge_kinetic
      module procedure is_set_type_edge_kinetic_distribution
      module procedure is_set_arr_type_edge_kinetic_distribution
      module procedure is_set_type_edges
      module procedure is_set_arr_type_edges
      module procedure is_set_type_edgespecies
      module procedure is_set_arr_type_edgespecies
      module procedure is_set_type_element_desc
      module procedure is_set_arr_type_element_desc
      module procedure is_set_type_entry_def
      module procedure is_set_arr_type_entry_def
      module procedure is_set_type_enum_instance
      module procedure is_set_arr_type_enum_instance
      module procedure is_set_type_eqconstraint
      module procedure is_set_arr_type_eqconstraint
      module procedure is_set_type_eqgeometry
      module procedure is_set_arr_type_eqgeometry
      module procedure is_set_type_eqmes0D
      module procedure is_set_arr_type_eqmes0D
      module procedure is_set_type_eqmes1D
      module procedure is_set_arr_type_eqmes1D
      module procedure is_set_type_equatorial_plane
      module procedure is_set_arr_type_equatorial_plane
      module procedure is_set_type_equilibrium_profiles2d_grid
      module procedure is_set_arr_type_equilibrium_profiles2d_grid
      module procedure is_set_type_equilibrium_profiles_2d
      module procedure is_set_arr_type_equilibrium_profiles_2d
      module procedure is_set_type_exp0D
      module procedure is_set_arr_type_exp0D
      module procedure is_set_type_exp1D
      module procedure is_set_arr_type_exp1D
      module procedure is_set_type_exp2D
      module procedure is_set_arr_type_exp2D
      module procedure is_set_type_f_expansion
      module procedure is_set_arr_type_f_expansion
      module procedure is_set_type_fast_thermal_separation_filter
      module procedure is_set_arr_type_fast_thermal_separation_filter
      module procedure is_set_type_filter
      module procedure is_set_arr_type_filter
      module procedure is_set_type_flat_polygon
      module procedure is_set_arr_type_flat_polygon
      module procedure is_set_type_flush
      module procedure is_set_arr_type_flush
      module procedure is_set_type_flux_loops
      module procedure is_set_arr_type_flux_loops
      module procedure is_set_type_fluxel
      module procedure is_set_arr_type_fluxel
      module procedure is_set_type_fluximp
      module procedure is_set_arr_type_fluximp
      module procedure is_set_type_fluxion
      module procedure is_set_arr_type_fluxion
      module procedure is_set_type_focussing
      module procedure is_set_arr_type_focussing
      module procedure is_set_type_fullwave
      module procedure is_set_arr_type_fullwave
      module procedure is_set_type_fusiondiag_colli_3d
      module procedure is_set_arr_type_fusiondiag_colli_3d
      module procedure is_set_type_fusiondiag_colli_circ
      module procedure is_set_arr_type_fusiondiag_colli_circ
      module procedure is_set_type_fusiondiag_colli_poly
      module procedure is_set_arr_type_fusiondiag_colli_poly
      module procedure is_set_type_fusiondiag_collimator
      module procedure is_set_arr_type_fusiondiag_collimator
      module procedure is_set_type_fusiondiag_colliunit_circ
      module procedure is_set_arr_type_fusiondiag_colliunit_circ
      module procedure is_set_type_fusiondiag_colliunit_poly
      module procedure is_set_arr_type_fusiondiag_colliunit_poly
      module procedure is_set_type_fusiondiag_counts
      module procedure is_set_arr_type_fusiondiag_counts
      module procedure is_set_type_fusiondiag_ct_chords
      module procedure is_set_arr_type_fusiondiag_ct_chords
      module procedure is_set_type_fusiondiag_ct_energy
      module procedure is_set_arr_type_fusiondiag_ct_energy
      module procedure is_set_type_fusiondiag_detect_ct_energy
      module procedure is_set_arr_type_fusiondiag_detect_ct_energy
      module procedure is_set_type_fusiondiag_emissivity1d
      module procedure is_set_arr_type_fusiondiag_emissivity1d
      module procedure is_set_type_fusiondiag_emissivity2d
      module procedure is_set_arr_type_fusiondiag_emissivity2d
      module procedure is_set_type_fusiondiag_fus_product
      module procedure is_set_arr_type_fusiondiag_fus_product
      module procedure is_set_type_fusiondiag_spec1d
      module procedure is_set_arr_type_fusiondiag_spec1d
      module procedure is_set_type_fusiondiag_spec2d
      module procedure is_set_arr_type_fusiondiag_spec2d
      module procedure is_set_type_fusiondiag_voxels
      module procedure is_set_arr_type_fusiondiag_voxels
      module procedure is_set_type_geom
      module procedure is_set_arr_type_geom
      module procedure is_set_type_geom_iron
      module procedure is_set_arr_type_geom_iron
      module procedure is_set_type_global_param
      module procedure is_set_arr_type_global_param
      module procedure is_set_type_globalparam
      module procedure is_set_arr_type_globalparam
      module procedure is_set_type_halpha_setup
      module procedure is_set_arr_type_halpha_setup
      module procedure is_set_type_hcll
      module procedure is_set_arr_type_hcll
      module procedure is_set_type_hcll_bb
      module procedure is_set_arr_type_hcll_bb
      module procedure is_set_type_hcllbb_specs
      module procedure is_set_arr_type_hcllbb_specs
      module procedure is_set_type_holes
      module procedure is_set_arr_type_holes
      module procedure is_set_type_identifier
      module procedure is_set_arr_type_identifier
      module procedure is_set_type_impcoeff
      module procedure is_set_arr_type_impcoeff
      module procedure is_set_type_impurities
      module procedure is_set_arr_type_impurities
      module procedure is_set_type_impurity_type
      module procedure is_set_arr_type_impurity_type
      module procedure is_set_type_inj_spec
      module procedure is_set_arr_type_inj_spec
      module procedure is_set_type_ions
      module procedure is_set_arr_type_ions
      module procedure is_set_type_isoflux
      module procedure is_set_arr_type_isoflux
      module procedure is_set_type_jni
      module procedure is_set_arr_type_jni
      module procedure is_set_type_lang_derived
      module procedure is_set_arr_type_lang_derived
      module procedure is_set_type_lang_measure
      module procedure is_set_arr_type_lang_measure
      module procedure is_set_type_launchangles
      module procedure is_set_arr_type_launchangles
      module procedure is_set_type_launchs_parallel
      module procedure is_set_arr_type_launchs_parallel
      module procedure is_set_type_launchs_phi_theta
      module procedure is_set_arr_type_launchs_phi_theta
      module procedure is_set_type_launchs_rfbeam
      module procedure is_set_arr_type_launchs_rfbeam
      module procedure is_set_type_launchs_rfbeam_phaseellipse
      module procedure is_set_arr_type_launchs_rfbeam_phaseellipse
      module procedure is_set_type_launchs_rfbeam_spot
      module procedure is_set_arr_type_launchs_rfbeam_spot
      module procedure is_set_type_launchsignal
      module procedure is_set_arr_type_launchsignal
      module procedure is_set_type_limiter_unit
      module procedure is_set_arr_type_limiter_unit
      module procedure is_set_type_limits
      module procedure is_set_arr_type_limits
      module procedure is_set_type_lineintegraldiag
      module procedure is_set_arr_type_lineintegraldiag
      module procedure is_set_type_lithmeasure
      module procedure is_set_arr_type_lithmeasure
      module procedure is_set_type_lithsetup
      module procedure is_set_arr_type_lithsetup
      module procedure is_set_type_local
      module procedure is_set_arr_type_local
      module procedure is_set_type_mag_axis
      module procedure is_set_arr_type_mag_axis
      module procedure is_set_type_magnet_iron
      module procedure is_set_arr_type_magnet_iron
      module procedure is_set_type_magnetise
      module procedure is_set_arr_type_magnetise
      module procedure is_set_type_mat_lim
      module procedure is_set_arr_type_mat_lim
      module procedure is_set_type_mdinfo
      module procedure is_set_arr_type_mdinfo
      module procedure is_set_type_mhd_ideal_wall2d
      module procedure is_set_arr_type_mhd_ideal_wall2d
      module procedure is_set_type_mhd_mode
      module procedure is_set_arr_type_mhd_mode
      module procedure is_set_type_mhd_plasma
      module procedure is_set_arr_type_mhd_plasma
      module procedure is_set_type_mhd_res_wall2d
      module procedure is_set_arr_type_mhd_res_wall2d
      module procedure is_set_type_mhd_vacuum
      module procedure is_set_arr_type_mhd_vacuum
      module procedure is_set_type_mhd_vector
      module procedure is_set_arr_type_mhd_vector
      module procedure is_set_type_mode_lipb
      module procedure is_set_arr_type_mode_lipb
      module procedure is_set_type_mode_mech
      module procedure is_set_arr_type_mode_mech
      module procedure is_set_type_mode_neutr
      module procedure is_set_arr_type_mode_neutr
      module procedure is_set_type_mode_th_hyd
      module procedure is_set_arr_type_mode_th_hyd
      module procedure is_set_type_mode_therm
      module procedure is_set_arr_type_mode_therm
      module procedure is_set_type_mode_tritium
      module procedure is_set_arr_type_mode_tritium
      module procedure is_set_type_modules
      module procedure is_set_arr_type_modules
      module procedure is_set_type_msediag_emiss_chord
      module procedure is_set_arr_type_msediag_emiss_chord
      module procedure is_set_type_msediag_emissivity
      module procedure is_set_arr_type_msediag_emissivity
      module procedure is_set_type_msediag_polarization
      module procedure is_set_arr_type_msediag_polarization
      module procedure is_set_type_msediag_radia_chord
      module procedure is_set_arr_type_msediag_radia_chord
      module procedure is_set_type_msediag_radiance
      module procedure is_set_arr_type_msediag_radiance
      module procedure is_set_type_msediag_setup
      module procedure is_set_arr_type_msediag_setup
      module procedure is_set_type_msediag_setup_polarimetry
      module procedure is_set_arr_type_msediag_setup_polarimetry
      module procedure is_set_type_msediag_stokes
      module procedure is_set_arr_type_msediag_stokes
      module procedure is_set_type_nbi_nbi_unit_wall
      module procedure is_set_arr_type_nbi_nbi_unit_wall
      module procedure is_set_type_nbi_nbi_unit_wall_surface
      module procedure is_set_arr_type_nbi_nbi_unit_wall_surface
      module procedure is_set_type_nbi_unit
      module procedure is_set_arr_type_nbi_unit
      module procedure is_set_type_ne_transp
      module procedure is_set_arr_type_ne_transp
      module procedure is_set_type_neoclassic_impurity
      module procedure is_set_arr_type_neoclassic_impurity
      module procedure is_set_type_neut_results
      module procedure is_set_arr_type_neut_results
      module procedure is_set_type_neutral_complex_type
      module procedure is_set_arr_type_neutral_complex_type
      module procedure is_set_type_neutro_resul
      module procedure is_set_arr_type_neutro_resul
      module procedure is_set_type_ni_transp
      module procedure is_set_arr_type_ni_transp
      module procedure is_set_type_ntm_mode
      module procedure is_set_arr_type_ntm_mode
      module procedure is_set_type_ntm_mode_evolution
      module procedure is_set_arr_type_ntm_mode_evolution
      module procedure is_set_type_ntm_mode_evolution_island
      module procedure is_set_arr_type_ntm_mode_evolution_island
      module procedure is_set_type_ntm_mode_full_evol
      module procedure is_set_arr_type_ntm_mode_full_evol
      module procedure is_set_type_ntm_mode_full_evol_island
      module procedure is_set_arr_type_ntm_mode_full_evol_island
      module procedure is_set_type_ntm_mode_onset
      module procedure is_set_arr_type_ntm_mode_onset
      module procedure is_set_type_nuclei
      module procedure is_set_arr_type_nuclei
      module procedure is_set_type_objects
      module procedure is_set_arr_type_objects
      module procedure is_set_type_offdiagel
      module procedure is_set_arr_type_offdiagel
      module procedure is_set_type_offdiagion
      module procedure is_set_arr_type_offdiagion
      module procedure is_set_type_omnigen_surf
      module procedure is_set_arr_type_omnigen_surf
      module procedure is_set_type_orbit_global_param
      module procedure is_set_arr_type_orbit_global_param
      module procedure is_set_type_orbit_midplane
      module procedure is_set_arr_type_orbit_midplane
      module procedure is_set_type_orbit_pos
      module procedure is_set_arr_type_orbit_pos
      module procedure is_set_type_orbit_special_pos
      module procedure is_set_arr_type_orbit_special_pos
      module procedure is_set_type_orbit_turning_pts
      module procedure is_set_arr_type_orbit_turning_pts
      module procedure is_set_type_origin
      module procedure is_set_arr_type_origin
      module procedure is_set_type_param
      module procedure is_set_arr_type_param
      module procedure is_set_type_parameters
      module procedure is_set_arr_type_parameters
      module procedure is_set_type_pellet
      module procedure is_set_arr_type_pellet
      module procedure is_set_type_pellet_angles
      module procedure is_set_arr_type_pellet_angles
      module procedure is_set_type_pellet_deposition
      module procedure is_set_arr_type_pellet_deposition
      module procedure is_set_type_pellet_elements
      module procedure is_set_arr_type_pellet_elements
      module procedure is_set_type_pellet_geometry
      module procedure is_set_arr_type_pellet_geometry
      module procedure is_set_type_pellet_impurity
      module procedure is_set_arr_type_pellet_impurity
      module procedure is_set_type_pellet_pathprofiles
      module procedure is_set_arr_type_pellet_pathprofiles
      module procedure is_set_type_pellet_shape
      module procedure is_set_arr_type_pellet_shape
      module procedure is_set_type_permeability
      module procedure is_set_arr_type_permeability
      module procedure is_set_type_pfcircuits
      module procedure is_set_arr_type_pfcircuits
      module procedure is_set_type_pfcoils
      module procedure is_set_arr_type_pfcoils
      module procedure is_set_type_pfelement
      module procedure is_set_arr_type_pfelement
      module procedure is_set_type_pfgeometry
      module procedure is_set_arr_type_pfgeometry
      module procedure is_set_type_pfpageometry
      module procedure is_set_arr_type_pfpageometry
      module procedure is_set_type_pfpassive
      module procedure is_set_arr_type_pfpassive
      module procedure is_set_type_pfpassive_current
      module procedure is_set_arr_type_pfpassive_current
      module procedure is_set_type_pfsupplies
      module procedure is_set_arr_type_pfsupplies
      module procedure is_set_type_phaseellipse
      module procedure is_set_arr_type_phaseellipse
      module procedure is_set_type_planecoil
      module procedure is_set_arr_type_planecoil
      module procedure is_set_type_plasmaComplexType
      module procedure is_set_arr_type_plasmaComplexType
      module procedure is_set_type_plasmaedge
      module procedure is_set_arr_type_plasmaedge
      module procedure is_set_type_pol_decomp
      module procedure is_set_arr_type_pol_decomp
      module procedure is_set_type_polarimetry
      module procedure is_set_arr_type_polarimetry
      module procedure is_set_type_polarization
      module procedure is_set_arr_type_polarization
      module procedure is_set_type_power_conv_component
      module procedure is_set_arr_type_power_conv_component
      module procedure is_set_type_power_exchange
      module procedure is_set_arr_type_power_exchange
      module procedure is_set_type_powerflow
      module procedure is_set_arr_type_powerflow
      module procedure is_set_type_profiles1d
      module procedure is_set_arr_type_profiles1d
      module procedure is_set_type_profiles_1d
      module procedure is_set_arr_type_profiles_1d
      module procedure is_set_type_psi
      module procedure is_set_arr_type_psi
      module procedure is_set_type_putinfo
      module procedure is_set_arr_type_putinfo
      module procedure is_set_type_q
      module procedure is_set_arr_type_q
      module procedure is_set_type_reacprodType
      module procedure is_set_arr_type_reacprodType
      module procedure is_set_type_react
      module procedure is_set_arr_type_react
      module procedure is_set_type_rectanglexyz
      module procedure is_set_arr_type_rectanglexyz
      module procedure is_set_type_recycling_neutrals
      module procedure is_set_arr_type_recycling_neutrals
      module procedure is_set_type_reduced
      module procedure is_set_arr_type_reduced
      module procedure is_set_type_refl_receive
      module procedure is_set_arr_type_refl_receive
      module procedure is_set_type_reflectometry_antennas
      module procedure is_set_arr_type_reflectometry_antennas
      module procedure is_set_type_reflectometry_radfield
      module procedure is_set_arr_type_reflectometry_radfield
      module procedure is_set_type_reflectometry_radfield_gaussian
      module procedure is_set_arr_type_reflectometry_radfield_gaussian
      module procedure is_set_type_reflectometry_radifield_efield
      module procedure is_set_arr_type_reflectometry_radifield_efield
      module procedure is_set_type_reggrid
      module procedure is_set_arr_type_reggrid
      module procedure is_set_type_rfameasure
      module procedure is_set_arr_type_rfameasure
      module procedure is_set_type_rfasetup
      module procedure is_set_arr_type_rfasetup
      module procedure is_set_type_rfbeam
      module procedure is_set_arr_type_rfbeam
      module procedure is_set_type_rz0D
      module procedure is_set_arr_type_rz0D
      module procedure is_set_type_rz1D
      module procedure is_set_arr_type_rz1D
      module procedure is_set_type_rz1D_npoints
      module procedure is_set_arr_type_rz1D_npoints
      module procedure is_set_type_rz1Dexp
      module procedure is_set_arr_type_rz1Dexp
      module procedure is_set_type_rz2D
      module procedure is_set_arr_type_rz2D
      module procedure is_set_type_rz3D
      module procedure is_set_arr_type_rz3D
      module procedure is_set_type_rzphi0D
      module procedure is_set_arr_type_rzphi0D
      module procedure is_set_type_rzphi1D
      module procedure is_set_arr_type_rzphi1D
      module procedure is_set_type_rzphi1Dexp
      module procedure is_set_arr_type_rzphi1Dexp
      module procedure is_set_type_rzphi1Dexperimental
      module procedure is_set_arr_type_rzphi1Dexperimental
      module procedure is_set_type_rzphi2D
      module procedure is_set_arr_type_rzphi2D
      module procedure is_set_type_rzphi3D
      module procedure is_set_arr_type_rzphi3D
      module procedure is_set_type_rzphidrdzdphi1D
      module procedure is_set_arr_type_rzphidrdzdphi1D
      module procedure is_set_type_sawteeth_diags
      module procedure is_set_arr_type_sawteeth_diags
      module procedure is_set_type_sawteeth_profiles1d
      module procedure is_set_arr_type_sawteeth_profiles1d
      module procedure is_set_type_scenario_centre
      module procedure is_set_arr_type_scenario_centre
      module procedure is_set_type_scenario_composition
      module procedure is_set_arr_type_scenario_composition
      module procedure is_set_type_scenario_configuration
      module procedure is_set_arr_type_scenario_configuration
      module procedure is_set_type_scenario_confinement
      module procedure is_set_arr_type_scenario_confinement
      module procedure is_set_type_scenario_currents
      module procedure is_set_arr_type_scenario_currents
      module procedure is_set_type_scenario_edge
      module procedure is_set_arr_type_scenario_edge
      module procedure is_set_type_scenario_energy
      module procedure is_set_arr_type_scenario_energy
      module procedure is_set_type_scenario_global
      module procedure is_set_arr_type_scenario_global
      module procedure is_set_type_scenario_heat_power
      module procedure is_set_arr_type_scenario_heat_power
      module procedure is_set_type_scenario_int
      module procedure is_set_arr_type_scenario_int
      module procedure is_set_type_scenario_itb
      module procedure is_set_arr_type_scenario_itb
      module procedure is_set_type_scenario_lim_div_wall
      module procedure is_set_arr_type_scenario_lim_div_wall
      module procedure is_set_type_scenario_line_ave
      module procedure is_set_arr_type_scenario_line_ave
      module procedure is_set_type_scenario_neutron
      module procedure is_set_arr_type_scenario_neutron
      module procedure is_set_type_scenario_ninety_five
      module procedure is_set_arr_type_scenario_ninety_five
      module procedure is_set_type_scenario_pedestal
      module procedure is_set_arr_type_scenario_pedestal
      module procedure is_set_type_scenario_reactor
      module procedure is_set_arr_type_scenario_reactor
      module procedure is_set_type_scenario_ref
      module procedure is_set_arr_type_scenario_ref
      module procedure is_set_type_scenario_references
      module procedure is_set_arr_type_scenario_references
      module procedure is_set_type_scenario_sol
      module procedure is_set_arr_type_scenario_sol
      module procedure is_set_type_scenario_vol_ave
      module procedure is_set_arr_type_scenario_vol_ave
      module procedure is_set_type_setup_bprobe
      module procedure is_set_arr_type_setup_bprobe
      module procedure is_set_type_setup_floops
      module procedure is_set_arr_type_setup_floops
      module procedure is_set_type_setup_line
      module procedure is_set_arr_type_setup_line
      module procedure is_set_type_setup_line_exp
      module procedure is_set_arr_type_setup_line_exp
      module procedure is_set_type_shield
      module procedure is_set_arr_type_shield
      module procedure is_set_type_shield_specs
      module procedure is_set_arr_type_shield_specs
      module procedure is_set_type_simp_apert
      module procedure is_set_arr_type_simp_apert
      module procedure is_set_type_solcurdiag_sol_current
      module procedure is_set_arr_type_solcurdiag_sol_current
      module procedure is_set_type_solcurdiag_sol_current_setup
      module procedure is_set_arr_type_solcurdiag_sol_current_setup
      module procedure is_set_type_source_imp
      module procedure is_set_arr_type_source_imp
      module procedure is_set_type_source_ion
      module procedure is_set_arr_type_source_ion
      module procedure is_set_type_source_rate
      module procedure is_set_arr_type_source_rate
      module procedure is_set_type_source_vec
      module procedure is_set_arr_type_source_vec
      module procedure is_set_type_sourceel
      module procedure is_set_arr_type_sourceel
      module procedure is_set_type_sourceimp
      module procedure is_set_arr_type_sourceimp
      module procedure is_set_type_sourceion
      module procedure is_set_arr_type_sourceion
      module procedure is_set_type_species_desc
      module procedure is_set_arr_type_species_desc
      module procedure is_set_type_species_reference
      module procedure is_set_arr_type_species_reference
      module procedure is_set_type_spectral
      module procedure is_set_arr_type_spectral
      module procedure is_set_type_spectrum
      module procedure is_set_arr_type_spectrum
      module procedure is_set_type_spot
      module procedure is_set_arr_type_spot
      module procedure is_set_type_sputtering_neutrals
      module procedure is_set_arr_type_sputtering_neutrals
      module procedure is_set_type_straps
      module procedure is_set_arr_type_straps
      module procedure is_set_type_structure_cs
      module procedure is_set_arr_type_structure_cs
      module procedure is_set_type_t_series_cplx
      module procedure is_set_arr_type_t_series_cplx
      module procedure is_set_type_t_series_real
      module procedure is_set_arr_type_t_series_real
      module procedure is_set_type_table
      module procedure is_set_arr_type_table
      module procedure is_set_type_tables
      module procedure is_set_arr_type_tables
      module procedure is_set_type_tables_coord
      module procedure is_set_arr_type_tables_coord
      module procedure is_set_type_temporary_nt
      module procedure is_set_arr_type_temporary_nt
      module procedure is_set_type_temporary_nt_0dc
      module procedure is_set_arr_type_temporary_nt_0dc
      module procedure is_set_type_temporary_nt_0di
      module procedure is_set_arr_type_temporary_nt_0di
      module procedure is_set_type_temporary_nt_0dr
      module procedure is_set_arr_type_temporary_nt_0dr
      module procedure is_set_type_temporary_nt_0ds
      module procedure is_set_arr_type_temporary_nt_0ds
      module procedure is_set_type_temporary_nt_1dc
      module procedure is_set_arr_type_temporary_nt_1dc
      module procedure is_set_type_temporary_nt_1di
      module procedure is_set_arr_type_temporary_nt_1di
      module procedure is_set_type_temporary_nt_1dr
      module procedure is_set_arr_type_temporary_nt_1dr
      module procedure is_set_type_temporary_nt_1ds
      module procedure is_set_arr_type_temporary_nt_1ds
      module procedure is_set_type_temporary_nt_2dc
      module procedure is_set_arr_type_temporary_nt_2dc
      module procedure is_set_type_temporary_nt_2di
      module procedure is_set_arr_type_temporary_nt_2di
      module procedure is_set_type_temporary_nt_2dr
      module procedure is_set_arr_type_temporary_nt_2dr
      module procedure is_set_type_temporary_nt_3dc
      module procedure is_set_arr_type_temporary_nt_3dc
      module procedure is_set_type_temporary_nt_3di
      module procedure is_set_arr_type_temporary_nt_3di
      module procedure is_set_type_temporary_nt_3dr
      module procedure is_set_arr_type_temporary_nt_3dr
      module procedure is_set_type_temporary_nt_4dr
      module procedure is_set_arr_type_temporary_nt_4dr
      module procedure is_set_type_temporary_t
      module procedure is_set_arr_type_temporary_t
      module procedure is_set_type_temporary_t_0dc
      module procedure is_set_arr_type_temporary_t_0dc
      module procedure is_set_type_temporary_t_0di
      module procedure is_set_arr_type_temporary_t_0di
      module procedure is_set_type_temporary_t_0dr
      module procedure is_set_arr_type_temporary_t_0dr
      module procedure is_set_type_temporary_t_0ds
      module procedure is_set_arr_type_temporary_t_0ds
      module procedure is_set_type_temporary_t_1dc
      module procedure is_set_arr_type_temporary_t_1dc
      module procedure is_set_type_temporary_t_1di
      module procedure is_set_arr_type_temporary_t_1di
      module procedure is_set_type_temporary_t_1dr
      module procedure is_set_arr_type_temporary_t_1dr
      module procedure is_set_type_temporary_t_2dc
      module procedure is_set_arr_type_temporary_t_2dc
      module procedure is_set_type_temporary_t_2di
      module procedure is_set_arr_type_temporary_t_2di
      module procedure is_set_type_temporary_t_2dr
      module procedure is_set_arr_type_temporary_t_2dr
      module procedure is_set_type_temporary_t_3dc
      module procedure is_set_arr_type_temporary_t_3dc
      module procedure is_set_type_temporary_t_3di
      module procedure is_set_arr_type_temporary_t_3di
      module procedure is_set_type_temporary_t_3dr
      module procedure is_set_arr_type_temporary_t_3dr
      module procedure is_set_type_temporary_t_4dr
      module procedure is_set_arr_type_temporary_t_4dr
      module procedure is_set_type_tf_desc_tfcoils
      module procedure is_set_arr_type_tf_desc_tfcoils
      module procedure is_set_type_tf_desc_tfcoils_board
      module procedure is_set_arr_type_tf_desc_tfcoils_board
      module procedure is_set_type_tf_structure
      module procedure is_set_arr_type_tf_structure
      module procedure is_set_type_theta_info
      module procedure is_set_arr_type_theta_info
      module procedure is_set_type_topo_regions
      module procedure is_set_arr_type_topo_regions
      module procedure is_set_type_toroid_field
      module procedure is_set_arr_type_toroid_field
      module procedure is_set_type_trace
      module procedure is_set_arr_type_trace
      module procedure is_set_type_transcoefel
      module procedure is_set_arr_type_transcoefel
      module procedure is_set_type_transcoefimp
      module procedure is_set_arr_type_transcoefimp
      module procedure is_set_type_transcoefion
      module procedure is_set_arr_type_transcoefion
      module procedure is_set_type_transcoefvtor
      module procedure is_set_arr_type_transcoefvtor
      module procedure is_set_type_trap_type
      module procedure is_set_arr_type_trap_type
      module procedure is_set_type_trianglexyz
      module procedure is_set_arr_type_trianglexyz
      module procedure is_set_type_tsmeasure
      module procedure is_set_arr_type_tsmeasure
      module procedure is_set_type_tssetup
      module procedure is_set_arr_type_tssetup
      module procedure is_set_type_turbcomposition
      module procedure is_set_arr_type_turbcomposition
      module procedure is_set_type_turbcoordsys
      module procedure is_set_arr_type_turbcoordsys
      module procedure is_set_type_turbenv1d
      module procedure is_set_arr_type_turbenv1d
      module procedure is_set_type_turbgrid
      module procedure is_set_arr_type_turbgrid
      module procedure is_set_type_turbspec1d
      module procedure is_set_arr_type_turbspec1d
      module procedure is_set_type_turbvar0d
      module procedure is_set_arr_type_turbvar0d
      module procedure is_set_type_turbvar1d
      module procedure is_set_arr_type_turbvar1d
      module procedure is_set_type_turbvar2d
      module procedure is_set_arr_type_turbvar2d
      module procedure is_set_type_turbvar3d
      module procedure is_set_arr_type_turbvar3d
      module procedure is_set_type_turbvar4d
      module procedure is_set_arr_type_turbvar4d
      module procedure is_set_type_turbvar5d
      module procedure is_set_arr_type_turbvar5d
      module procedure is_set_type_version_ind
      module procedure is_set_arr_type_version_ind
      module procedure is_set_type_wall2d
      module procedure is_set_arr_type_wall2d
      module procedure is_set_type_wall2d_mhd
      module procedure is_set_arr_type_wall2d_mhd
      module procedure is_set_type_wall3d
      module procedure is_set_arr_type_wall3d
      module procedure is_set_type_wall_blocks
      module procedure is_set_arr_type_wall_blocks
      module procedure is_set_type_wall_blocks_unit
      module procedure is_set_arr_type_wall_blocks_unit
      module procedure is_set_type_wall_limiter
      module procedure is_set_arr_type_wall_limiter
      module procedure is_set_type_wall_types
      module procedure is_set_arr_type_wall_types
      module procedure is_set_type_wall_types_layers
      module procedure is_set_arr_type_wall_types_layers
      module procedure is_set_type_wall_unitsComplexType
      module procedure is_set_arr_type_wall_unitsComplexType
      module procedure is_set_type_wall_unitsComplexType_layers
      module procedure is_set_arr_type_wall_unitsComplexType_layers
      module procedure is_set_type_wall_vessel
      module procedure is_set_arr_type_wall_vessel
      module procedure is_set_type_wall_vessel_annular
      module procedure is_set_arr_type_wall_vessel_annular
      module procedure is_set_type_wall_vessel_unit
      module procedure is_set_arr_type_wall_vessel_unit
      module procedure is_set_type_wall_wall0d
      module procedure is_set_arr_type_wall_wall0d
      module procedure is_set_type_wall_wall0d_plasma
      module procedure is_set_arr_type_wall_wall0d_plasma
      module procedure is_set_type_wall_wall2d_vessel_radial_build
      module procedure is_set_arr_type_wall_wall2d_vessel_radial_build
      module procedure is_set_type_waveguides
      module procedure is_set_arr_type_waveguides
      module procedure is_set_type_waves_global_param
      module procedure is_set_arr_type_waves_global_param
      module procedure is_set_type_waves_grid_1d
      module procedure is_set_arr_type_waves_grid_1d
      module procedure is_set_type_waves_grid_2d
      module procedure is_set_arr_type_waves_grid_2d
      module procedure is_set_type_waves_profiles_1d
      module procedure is_set_arr_type_waves_profiles_1d
      module procedure is_set_type_waves_profiles_2d
      module procedure is_set_arr_type_waves_profiles_2d
      module procedure is_set_type_waves_rtposition
      module procedure is_set_arr_type_waves_rtposition
      module procedure is_set_type_waves_rtwavevector
      module procedure is_set_arr_type_waves_rtwavevector
      module procedure is_set_type_weighted_markers
      module procedure is_set_arr_type_weighted_markers
      module procedure is_set_type_whatref
      module procedure is_set_arr_type_whatref
      module procedure is_set_type_width
      module procedure is_set_arr_type_width
      module procedure is_set_type_xpts
      module procedure is_set_arr_type_xpts
      module procedure is_set_type_xyz0D
      module procedure is_set_arr_type_xyz0D
   end interface
 
   integer, parameter, private :: iu6 = 6

 contains


   subroutine is_set_type_integer(structure_in, name)

     implicit none

     integer, intent(in) :: structure_in
     character(len = *), intent(in) :: name
 
     if (structure_in /= -999999999) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_integer

   subroutine is_set_type_float(structure_in, name)

     implicit none

     real(euitm_r8) :: structure_in
     character(len = *), intent(in) :: name
 
     if (structure_in /= -9.0D40) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_float

   subroutine is_set_type_complex(structure_in, name)

     implicit none

     complex(euitm_r8) :: structure_in
     character(len = *), intent(in) :: name
 
     if (structure_in /= (-9.0D40,-9.0D40)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_complex

   subroutine is_set_type_array3dcplx_type(structure_in, name)
 
     implicit none

     complex(euitm_r8), pointer :: structure_in(:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array3dcplx_type

   subroutine is_set_type_array3dflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array3dflt_type

   subroutine is_set_type_array3dint_type(structure_in, name)
 
     implicit none

     integer, pointer :: structure_in(:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array3dint_type

   subroutine is_set_type_array4dflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array4dflt_type

   subroutine is_set_type_array5dflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array5dflt_type

   subroutine is_set_type_array6dflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array6dflt_type

   subroutine is_set_type_array7dflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:,:,:,:,:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_array7dflt_type

   subroutine is_set_type_matcplx_type(structure_in, name)
 
     implicit none

     complex(euitm_r8), pointer :: structure_in(:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_matcplx_type

   subroutine is_set_type_matflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_matflt_type

   subroutine is_set_type_matint_type(structure_in, name)
 
     implicit none

     integer, pointer :: structure_in(:,:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_matint_type

   subroutine is_set_type_veccplx_type(structure_in, name)
 
     implicit none

     complex(euitm_r8), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_veccplx_type

   subroutine is_set_type_vecflt_type(structure_in, name)
 
     implicit none

     real(euitm_r8), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_vecflt_type

   subroutine is_set_type_vecint_type(structure_in, name)
 
     implicit none

     integer, pointer :: structure_in(:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_vecint_type

   subroutine is_set_type_vecstring_type(structure_in, name)
 
     implicit none

     character(len = 132), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_type_vecstring_type

   subroutine is_set_type_amns(structure_in, name)

     implicit none

     type (type_amns), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%version, trim(adjustl(name)) // '%version')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_arr_type_amns_processType(structure_in%process, trim(adjustl(name)) // '%process')
     call is_set_arr_type_tables(structure_in%tables, trim(adjustl(name)) // '%tables')
     call is_set_arr_type_tables_coord(structure_in%tables_coord, trim(adjustl(name)) // '%tables_coord')
     call is_set_arr_type_version_ind(structure_in%version_ind, trim(adjustl(name)) // '%version_ind')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_amns

   subroutine is_set_arr_type_amns(structure_in, name)

     implicit none

     type (type_amns), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_amns(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_amns

   subroutine is_set_type_antennas(structure_in, name)

     implicit none

     type (type_antennas), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_antenna_ec(structure_in%antenna_ec, trim(adjustl(name)) // '%antenna_ec')
     call is_set_arr_type_antenna_ic(structure_in%antenna_ic, trim(adjustl(name)) // '%antenna_ic')
     call is_set_arr_type_antenna_lh(structure_in%antenna_lh, trim(adjustl(name)) // '%antenna_lh')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_antennas

   subroutine is_set_arr_type_antennas(structure_in, name)

     implicit none

     type (type_antennas), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_antennas(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_antennas

   subroutine is_set_type_bb_shield(structure_in, name)

     implicit none

     type (type_bb_shield), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_limits(structure_in%limits, trim(adjustl(name)) // '%limits')
     call is_set_type_float(structure_in%li6_enrich, trim(adjustl(name)) // '%li6_enrich')
     call is_set_type_geom(structure_in%geom, trim(adjustl(name)) // '%geom')
     call is_set_type_neut_results(structure_in%neut_results, trim(adjustl(name)) // '%neut_results')
     call is_set_type_shield(structure_in%shield, trim(adjustl(name)) // '%shield')
     call is_set_type_bb(structure_in%bb, trim(adjustl(name)) // '%bb')
     call is_set_type_hcll(structure_in%hcll, trim(adjustl(name)) // '%hcll')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_bb_shield

   subroutine is_set_arr_type_bb_shield(structure_in, name)

     implicit none

     type (type_bb_shield), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_bb_shield(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_bb_shield

   subroutine is_set_type_compositionc(structure_in, name)

     implicit none

     type (type_compositionc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_compositionc

   subroutine is_set_arr_type_compositionc(structure_in, name)

     implicit none

     type (type_compositionc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_compositionc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_compositionc

   subroutine is_set_type_coredelta(structure_in, name)

     implicit none

     type (type_coredelta), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_arr_type_coredelta_values(structure_in%values, trim(adjustl(name)) // '%values')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_coredelta

   subroutine is_set_arr_type_coredelta(structure_in, name)

     implicit none

     type (type_coredelta), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coredelta(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coredelta

   subroutine is_set_type_corefast(structure_in, name)

     implicit none

     type (type_corefast), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')
     call is_set_arr_type_corefast_values(structure_in%values, trim(adjustl(name)) // '%values')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_corefast

   subroutine is_set_arr_type_corefast(structure_in, name)

     implicit none

     type (type_corefast), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefast(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefast

   subroutine is_set_type_coreimpur(structure_in, name)

     implicit none

     type (type_coreimpur), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecint_type(structure_in%flag, trim(adjustl(name)) // '%flag')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_vecstring_type(structure_in%atomic_data, trim(adjustl(name)) // '%atomic_data')
     call is_set_arr_type_impurity_type(structure_in%impurity, trim(adjustl(name)) // '%impurity')
     call is_set_type_coreimpurediag_type(structure_in%diagnostic, trim(adjustl(name)) // '%diagnostic')
     call is_set_type_coreimpurediag_sum(structure_in%diagnosticsum, trim(adjustl(name)) // '%diagnosticsum')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_coreimpur

   subroutine is_set_arr_type_coreimpur(structure_in, name)

     implicit none

     type (type_coreimpur), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpur(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpur

   subroutine is_set_type_coreneutrals(structure_in, name)

     implicit none

     type (type_coreneutrals), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_composition_neutrals(structure_in%neutcompo, trim(adjustl(name)) // '%neutcompo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_arr_type_neutral_complex_type(structure_in%profiles, trim(adjustl(name)) // '%profiles')
     call is_set_arr_type_coefficients_neutrals(structure_in%ioncoeff, trim(adjustl(name)) // '%ioncoeff')
     call is_set_arr_type_impcoeff(structure_in%impcoeff, trim(adjustl(name)) // '%impcoeff')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_coreneutrals

   subroutine is_set_arr_type_coreneutrals(structure_in, name)

     implicit none

     type (type_coreneutrals), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreneutrals(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreneutrals

   subroutine is_set_type_coreprof(structure_in, name)

     implicit none

     type (type_coreprof), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%drho_dt, trim(adjustl(name)) // '%drho_dt')
     call is_set_type_toroid_field(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_psi(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_corefield(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_corefieldion(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_corefield(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_corefieldion(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_corefieldion(structure_in%vtor, trim(adjustl(name)) // '%vtor')
     call is_set_type_profiles1d(structure_in%profiles1d, trim(adjustl(name)) // '%profiles1d')
     call is_set_type_globalparam(structure_in%globalparam, trim(adjustl(name)) // '%globalparam')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_coreprof

   subroutine is_set_arr_type_coreprof(structure_in, name)

     implicit none

     type (type_coreprof), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreprof(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreprof

   subroutine is_set_type_coresource(structure_in, name)

     implicit none

     type (type_coresource), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')
     call is_set_arr_type_coresource_values(structure_in%values, trim(adjustl(name)) // '%values')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_coresource

   subroutine is_set_arr_type_coresource(structure_in, name)

     implicit none

     type (type_coresource), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coresource(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coresource

   subroutine is_set_type_coretransp(structure_in, name)

     implicit none

     type (type_coretransp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_arr_type_coretransp_values(structure_in%values, trim(adjustl(name)) // '%values')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_coretransp

   subroutine is_set_arr_type_coretransp(structure_in, name)

     implicit none

     type (type_coretransp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coretransp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coretransp

   subroutine is_set_type_cxdiag(structure_in, name)

     implicit none

     type (type_cxdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_cxsetup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_cxmeasure(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_cxdiag

   subroutine is_set_arr_type_cxdiag(structure_in, name)

     implicit none

     type (type_cxdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_cxdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_cxdiag

   subroutine is_set_type_distribution(structure_in, name)

     implicit none

     type (type_distribution), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_arr_type_distri_vec(structure_in%distri_vec, trim(adjustl(name)) // '%distri_vec')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_distribution

   subroutine is_set_arr_type_distribution(structure_in, name)

     implicit none

     type (type_distribution), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distribution(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distribution

   subroutine is_set_type_distsource(structure_in, name)

     implicit none

     type (type_distsource), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_arr_type_distsource_source(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_distsource

   subroutine is_set_arr_type_distsource(structure_in, name)

     implicit none

     type (type_distsource), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distsource(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distsource

   subroutine is_set_type_ecediag(structure_in, name)

     implicit none

     type (type_ecediag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_ecesetup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_ecemeasure(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_ecediag

   subroutine is_set_arr_type_ecediag(structure_in, name)

     implicit none

     type (type_ecediag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ecediag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ecediag

   subroutine is_set_type_edge(structure_in, name)

     implicit none

     type (type_edge), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_complexgrid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_arr_type_species_desc(structure_in%species, trim(adjustl(name)) // '%species')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_edge_fluid(structure_in%fluid, trim(adjustl(name)) // '%fluid')
     call is_set_type_edge_kinetic(structure_in%kinetic, trim(adjustl(name)) // '%kinetic')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_edge

   subroutine is_set_arr_type_edge(structure_in, name)

     implicit none

     type (type_edge), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge

   subroutine is_set_type_efcc(structure_in, name)

     implicit none

     type (type_efcc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_coil(structure_in%coil, trim(adjustl(name)) // '%coil')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_efcc

   subroutine is_set_arr_type_efcc(structure_in, name)

     implicit none

     type (type_efcc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_efcc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_efcc

   subroutine is_set_type_equilibrium(structure_in, name)

     implicit none

     type (type_equilibrium), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_eqconstraint(structure_in%eqconstraint, trim(adjustl(name)) // '%eqconstraint')
     call is_set_type_eqgeometry(structure_in%eqgeometry, trim(adjustl(name)) // '%eqgeometry')
     call is_set_type_flush(structure_in%flush, trim(adjustl(name)) // '%flush')
     call is_set_type_global_param(structure_in%global_param, trim(adjustl(name)) // '%global_param')
     call is_set_type_profiles_1d(structure_in%profiles_1d, trim(adjustl(name)) // '%profiles_1d')
     call is_set_arr_type_equilibrium_profiles_2d(structure_in%profiles_2d, trim(adjustl(name)) // '%profiles_2d')
     call is_set_type_coord_sys(structure_in%coord_sys, trim(adjustl(name)) // '%coord_sys')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_equilibrium

   subroutine is_set_arr_type_equilibrium(structure_in, name)

     implicit none

     type (type_equilibrium), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_equilibrium(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_equilibrium

   subroutine is_set_type_fusiondiag(structure_in, name)

     implicit none

     type (type_fusiondiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_fusiondiag_fus_product(structure_in%fus_product, trim(adjustl(name)) // '%fus_product')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_fusiondiag

   subroutine is_set_arr_type_fusiondiag(structure_in, name)

     implicit none

     type (type_fusiondiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag

   subroutine is_set_type_halphadiag(structure_in, name)

     implicit none

     type (type_halphadiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_halpha_setup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_exp1D(structure_in%intensity, trim(adjustl(name)) // '%intensity')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_halphadiag

   subroutine is_set_arr_type_halphadiag(structure_in, name)

     implicit none

     type (type_halphadiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_halphadiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_halphadiag

   subroutine is_set_type_heat_sources(structure_in, name)

     implicit none

     type (type_heat_sources), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_calorimetry_heat_source(structure_in%sources, trim(adjustl(name)) // '%sources')
     call is_set_arr_type_calorimetry_heat_source(structure_in%sinks, trim(adjustl(name)) // '%sinks')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_heat_sources

   subroutine is_set_arr_type_heat_sources(structure_in, name)

     implicit none

     type (type_heat_sources), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_heat_sources(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_heat_sources

   subroutine is_set_type_interfdiag(structure_in, name)

     implicit none

     type (type_interfdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%expression, trim(adjustl(name)) // '%expression')
     call is_set_type_setup_line(structure_in%setup_line, trim(adjustl(name)) // '%setup_line')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_interfdiag

   subroutine is_set_arr_type_interfdiag(structure_in, name)

     implicit none

     type (type_interfdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_interfdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_interfdiag

   subroutine is_set_type_ironmodel(structure_in, name)

     implicit none

     type (type_ironmodel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_desc_iron(structure_in%desc_iron, trim(adjustl(name)) // '%desc_iron')
     call is_set_type_magnetise(structure_in%magnetise, trim(adjustl(name)) // '%magnetise')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_ironmodel

   subroutine is_set_arr_type_ironmodel(structure_in, name)

     implicit none

     type (type_ironmodel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ironmodel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ironmodel

   subroutine is_set_type_langmuirdiag(structure_in, name)

     implicit none

     type (type_langmuirdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_lang_measure(structure_in%potential, trim(adjustl(name)) // '%potential')
     call is_set_type_lang_measure(structure_in%bias, trim(adjustl(name)) // '%bias')
     call is_set_type_lang_measure(structure_in%jsat, trim(adjustl(name)) // '%jsat')
     call is_set_type_lang_derived(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_lang_derived(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_lang_derived(structure_in%machpar, trim(adjustl(name)) // '%machpar')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_langmuirdiag

   subroutine is_set_arr_type_langmuirdiag(structure_in, name)

     implicit none

     type (type_langmuirdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_langmuirdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_langmuirdiag

   subroutine is_set_type_launchs(structure_in, name)

     implicit none

     type (type_launchs), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_vecint_type(structure_in%mode, trim(adjustl(name)) // '%mode')
     call is_set_type_rzphi1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_spectrum(structure_in%spectrum, trim(adjustl(name)) // '%spectrum')
     call is_set_type_launchs_rfbeam(structure_in%beam, trim(adjustl(name)) // '%beam')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_launchs

   subroutine is_set_arr_type_launchs(structure_in, name)

     implicit none

     type (type_launchs), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchs(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchs

   subroutine is_set_type_lithiumdiag(structure_in, name)

     implicit none

     type (type_lithiumdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_lithsetup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_lithmeasure(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_lithiumdiag

   subroutine is_set_arr_type_lithiumdiag(structure_in, name)

     implicit none

     type (type_lithiumdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_lithiumdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_lithiumdiag

   subroutine is_set_type_magdiag(structure_in, name)

     implicit none

     type (type_magdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_exp0D(structure_in%ip, trim(adjustl(name)) // '%ip')
     call is_set_type_exp0D(structure_in%diamagflux, trim(adjustl(name)) // '%diamagflux')
     call is_set_type_exp0D(structure_in%diamagener, trim(adjustl(name)) // '%diamagener')
     call is_set_type_flux_loops(structure_in%flux_loops, trim(adjustl(name)) // '%flux_loops')
     call is_set_type_bpol_probes(structure_in%bpol_probes, trim(adjustl(name)) // '%bpol_probes')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_magdiag

   subroutine is_set_arr_type_magdiag(structure_in, name)

     implicit none

     type (type_magdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_magdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_magdiag

   subroutine is_set_type_mhd(structure_in, name)

     implicit none

     type (type_mhd), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')
     call is_set_arr_type_mhd_mode(structure_in%n, trim(adjustl(name)) // '%n')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_mhd

   subroutine is_set_arr_type_mhd(structure_in, name)

     implicit none

     type (type_mhd), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd

   subroutine is_set_type_msediag(structure_in, name)

     implicit none

     type (type_msediag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_polarimetry(structure_in%polarimetry, trim(adjustl(name)) // '%polarimetry')
     call is_set_type_spectral(structure_in%spectral, trim(adjustl(name)) // '%spectral')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_msediag

   subroutine is_set_arr_type_msediag(structure_in, name)

     implicit none

     type (type_msediag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag

   subroutine is_set_type_nbi(structure_in, name)

     implicit none

     type (type_nbi), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_nbi_unit(structure_in%nbi_unit, trim(adjustl(name)) // '%nbi_unit')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_nbi

   subroutine is_set_arr_type_nbi(structure_in, name)

     implicit none

     type (type_nbi), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_nbi(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_nbi

   subroutine is_set_type_neoclassic(structure_in, name)

     implicit none

     type (type_neoclassic), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_desc_impur(structure_in%desc_impur, trim(adjustl(name)) // '%desc_impur')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_transcoefion(structure_in%ni_neo, trim(adjustl(name)) // '%ni_neo')
     call is_set_type_transcoefel(structure_in%ne_neo, trim(adjustl(name)) // '%ne_neo')
     call is_set_arr_type_transcoefimp(structure_in%nz_neo, trim(adjustl(name)) // '%nz_neo')
     call is_set_type_transcoefion(structure_in%ti_neo, trim(adjustl(name)) // '%ti_neo')
     call is_set_type_transcoefel(structure_in%te_neo, trim(adjustl(name)) // '%te_neo')
     call is_set_arr_type_transcoefimp(structure_in%tz_neo, trim(adjustl(name)) // '%tz_neo')
     call is_set_type_transcoefel(structure_in%mtor_neo, trim(adjustl(name)) // '%mtor_neo')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_vecflt_type(structure_in%jboot, trim(adjustl(name)) // '%jboot')
     call is_set_type_vecflt_type(structure_in%er, trim(adjustl(name)) // '%er')
     call is_set_type_matflt_type(structure_in%vpol, trim(adjustl(name)) // '%vpol')
     call is_set_type_matflt_type(structure_in%vtor, trim(adjustl(name)) // '%vtor')
     call is_set_type_matflt_type(structure_in%mach, trim(adjustl(name)) // '%mach')
     call is_set_type_vecflt_type(structure_in%utheta_e, trim(adjustl(name)) // '%utheta_e')
     call is_set_type_matflt_type(structure_in%utheta_i, trim(adjustl(name)) // '%utheta_i')
     call is_set_type_matflt_type(structure_in%viscosity_par, trim(adjustl(name)) // '%viscosity_par')
     call is_set_arr_type_neoclassic_impurity(structure_in%impurity, trim(adjustl(name)) // '%impurity')
     call is_set_type_array3dflt_type(structure_in%fext, trim(adjustl(name)) // '%fext')
     call is_set_type_vecflt_type(structure_in%jext, trim(adjustl(name)) // '%jext')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_neoclassic

   subroutine is_set_arr_type_neoclassic(structure_in, name)

     implicit none

     type (type_neoclassic), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_neoclassic(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_neoclassic

   subroutine is_set_type_ntm(structure_in, name)

     implicit none

     type (type_ntm), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_ntm_mode(structure_in%mode, trim(adjustl(name)) // '%mode')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_ntm

   subroutine is_set_arr_type_ntm(structure_in, name)

     implicit none

     type (type_ntm), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm

   subroutine is_set_type_orbit(structure_in, name)

     implicit none

     type (type_orbit), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_com(structure_in%com, trim(adjustl(name)) // '%com')
     call is_set_type_trace(structure_in%trace, trim(adjustl(name)) // '%trace')
     call is_set_type_orbit_global_param(structure_in%global_param, trim(adjustl(name)) // '%global_param')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_orbit

   subroutine is_set_arr_type_orbit(structure_in, name)

     implicit none

     type (type_orbit), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_orbit(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_orbit

   subroutine is_set_type_pellets(structure_in, name)

     implicit none

     type (type_pellets), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_arr_type_pellet(structure_in%pellet, trim(adjustl(name)) // '%pellet')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_pellets

   subroutine is_set_arr_type_pellets(structure_in, name)

     implicit none

     type (type_pellets), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellets(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellets

   subroutine is_set_type_pfsystems(structure_in, name)

     implicit none

     type (type_pfsystems), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_pfcoils(structure_in%pfcoils, trim(adjustl(name)) // '%pfcoils')
     call is_set_type_pfpassive(structure_in%pfpassive, trim(adjustl(name)) // '%pfpassive')
     call is_set_type_pfcircuits(structure_in%pfcircuits, trim(adjustl(name)) // '%pfcircuits')
     call is_set_type_pfsupplies(structure_in%pfsupplies, trim(adjustl(name)) // '%pfsupplies')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_pfsystems

   subroutine is_set_arr_type_pfsystems(structure_in, name)

     implicit none

     type (type_pfsystems), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfsystems(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfsystems

   subroutine is_set_type_polardiag(structure_in, name)

     implicit none

     type (type_polardiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%expression, trim(adjustl(name)) // '%expression')
     call is_set_type_setup_line(structure_in%setup_line, trim(adjustl(name)) // '%setup_line')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_polardiag

   subroutine is_set_arr_type_polardiag(structure_in, name)

     implicit none

     type (type_polardiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_polardiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_polardiag

   subroutine is_set_type_power_conv(structure_in, name)

     implicit none

     type (type_power_conv), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%cycle_type, trim(adjustl(name)) // '%cycle_type')
     call is_set_arr_type_circuits(structure_in%circuits, trim(adjustl(name)) // '%circuits')
     call is_set_type_float(structure_in%power_recirc, trim(adjustl(name)) // '%power_recirc')
     call is_set_type_float(structure_in%power_net, trim(adjustl(name)) // '%power_net')
     call is_set_type_float(structure_in%power_int, trim(adjustl(name)) // '%power_int')
     call is_set_type_float(structure_in%efficiency, trim(adjustl(name)) // '%efficiency')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_power_conv

   subroutine is_set_arr_type_power_conv(structure_in, name)

     implicit none

     type (type_power_conv), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_power_conv(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_power_conv

   subroutine is_set_type_reflectomet(structure_in, name)

     implicit none

     type (type_reflectomet), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_refl_receive(structure_in%refl_receive, trim(adjustl(name)) // '%refl_receive')
     call is_set_arr_type_reflectometry_antennas(structure_in%antennas, trim(adjustl(name)) // '%antennas')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_reflectomet

   subroutine is_set_arr_type_reflectomet(structure_in, name)

     implicit none

     type (type_reflectomet), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reflectomet(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reflectomet

   subroutine is_set_type_rfadiag(structure_in, name)

     implicit none

     type (type_rfadiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_rfasetup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_rfameasure(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_rfadiag

   subroutine is_set_arr_type_rfadiag(structure_in, name)

     implicit none

     type (type_rfadiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rfadiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rfadiag

   subroutine is_set_type_sawteeth(structure_in, name)

     implicit none

     type (type_sawteeth), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_integer(structure_in%crash_trig, trim(adjustl(name)) // '%crash_trig')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_sawteeth_profiles1d(structure_in%profiles1d, trim(adjustl(name)) // '%profiles1d')
     call is_set_type_sawteeth_diags(structure_in%diags, trim(adjustl(name)) // '%diags')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_sawteeth

   subroutine is_set_arr_type_sawteeth(structure_in, name)

     implicit none

     type (type_sawteeth), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sawteeth(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sawteeth

   subroutine is_set_type_scenario(structure_in, name)

     implicit none

     type (type_scenario), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_scenario_centre(structure_in%centre, trim(adjustl(name)) // '%centre')
     call is_set_type_scenario_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_scenario_configuration(structure_in%configs, trim(adjustl(name)) // '%configs')
     call is_set_type_scenario_confinement(structure_in%confinement, trim(adjustl(name)) // '%confinement')
     call is_set_type_scenario_currents(structure_in%currents, trim(adjustl(name)) // '%currents')
     call is_set_type_scenario_edge(structure_in%edge, trim(adjustl(name)) // '%edge')
     call is_set_type_scenario_energy(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_eqgeometry(structure_in%eqgeometry, trim(adjustl(name)) // '%eqgeometry')
     call is_set_type_scenario_global(structure_in%global_param, trim(adjustl(name)) // '%global_param')
     call is_set_type_scenario_heat_power(structure_in%heat_power, trim(adjustl(name)) // '%heat_power')
     call is_set_type_scenario_itb(structure_in%itb, trim(adjustl(name)) // '%itb')
     call is_set_type_scenario_lim_div_wall(structure_in%lim_div_wall, trim(adjustl(name)) // '%lim_div_wall')
     call is_set_type_scenario_line_ave(structure_in%line_ave, trim(adjustl(name)) // '%line_ave')
     call is_set_type_scenario_neutron(structure_in%neutron, trim(adjustl(name)) // '%neutron')
     call is_set_type_scenario_ninety_five(structure_in%ninety_five, trim(adjustl(name)) // '%ninety_five')
     call is_set_type_scenario_pedestal(structure_in%pedestal, trim(adjustl(name)) // '%pedestal')
     call is_set_type_scenario_references(structure_in%references, trim(adjustl(name)) // '%references')
     call is_set_type_scenario_reactor(structure_in%reactor, trim(adjustl(name)) // '%reactor')
     call is_set_type_scenario_sol(structure_in%sol, trim(adjustl(name)) // '%sol')
     call is_set_type_scenario_vol_ave(structure_in%vol_ave, trim(adjustl(name)) // '%vol_ave')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_scenario

   subroutine is_set_arr_type_scenario(structure_in, name)

     implicit none

     type (type_scenario), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario

   subroutine is_set_type_solcurdiag(structure_in, name)

     implicit none

     type (type_solcurdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_solcurdiag_sol_current(structure_in%sol_current, trim(adjustl(name)) // '%sol_current')
     call is_set_arr_type_clusters(structure_in%clusters, trim(adjustl(name)) // '%clusters')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_solcurdiag

   subroutine is_set_arr_type_solcurdiag(structure_in, name)

     implicit none

     type (type_solcurdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_solcurdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_solcurdiag

   subroutine is_set_type_temporary(structure_in, name)

     implicit none

     type (type_temporary), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_temporary_nt(structure_in%non_timed, trim(adjustl(name)) // '%non_timed')
     call is_set_type_temporary_t(structure_in%timed, trim(adjustl(name)) // '%timed')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_temporary

   subroutine is_set_arr_type_temporary(structure_in, name)

     implicit none

     type (type_temporary), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary

   subroutine is_set_type_topinfo(structure_in, name)

     implicit none

     type (type_topinfo), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%dataprovider, trim(adjustl(name)) // '%dataprovider')
     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')
     call is_set_type_vecstring_type(structure_in%firstputdate, trim(adjustl(name)) // '%firstputdate')
     call is_set_type_vecstring_type(structure_in%lastupdate, trim(adjustl(name)) // '%lastupdate')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecstring_type(structure_in%comment, trim(adjustl(name)) // '%comment')
     call is_set_type_vecstring_type(structure_in%dataversion, trim(adjustl(name)) // '%dataversion')
     call is_set_type_vecstring_type(structure_in%workflow, trim(adjustl(name)) // '%workflow')
     call is_set_type_entry_def(structure_in%entry, trim(adjustl(name)) // '%entry')
     call is_set_type_entry_def(structure_in%parent_entry, trim(adjustl(name)) // '%parent_entry')
     call is_set_type_mdinfo(structure_in%mdinfo, trim(adjustl(name)) // '%mdinfo')

   end subroutine is_set_type_topinfo

   subroutine is_set_arr_type_topinfo(structure_in, name)

     implicit none

     type (type_topinfo), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_topinfo(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_topinfo

   subroutine is_set_type_toroidfield(structure_in, name)

     implicit none

     type (type_toroidfield), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_tf_desc_tfcoils(structure_in%desc_tfcoils, trim(adjustl(name)) // '%desc_tfcoils')
     call is_set_type_integer(structure_in%nturns, trim(adjustl(name)) // '%nturns')
     call is_set_type_integer(structure_in%ncoils, trim(adjustl(name)) // '%ncoils')
     call is_set_type_exp0D(structure_in%current, trim(adjustl(name)) // '%current')
     call is_set_type_exp0D(structure_in%bvac_r, trim(adjustl(name)) // '%bvac_r')
     call is_set_type_float(structure_in%r0, trim(adjustl(name)) // '%r0')
     call is_set_type_float(structure_in%p_cryo, trim(adjustl(name)) // '%p_cryo')
     call is_set_type_float(structure_in%wp_nh_max, trim(adjustl(name)) // '%wp_nh_max')
     call is_set_type_float(structure_in%tfc_nh, trim(adjustl(name)) // '%tfc_nh')
     call is_set_type_float(structure_in%neut_flux_inb, trim(adjustl(name)) // '%neut_flux_inb')
     call is_set_type_float(structure_in%neut_flux_outb, trim(adjustl(name)) // '%neut_flux_outb')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_toroidfield

   subroutine is_set_arr_type_toroidfield(structure_in, name)

     implicit none

     type (type_toroidfield), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_toroidfield(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_toroidfield

   subroutine is_set_type_tsdiag(structure_in, name)

     implicit none

     type (type_tsdiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_tssetup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_tsmeasure(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_tsdiag

   subroutine is_set_arr_type_tsdiag(structure_in, name)

     implicit none

     type (type_tsdiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tsdiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tsdiag

   subroutine is_set_type_turbulence(structure_in, name)

     implicit none

     type (type_turbulence), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_turbcomposition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_turbcoordsys(structure_in%coordsys, trim(adjustl(name)) // '%coordsys')
     call is_set_type_turbvar0d(structure_in%var0d, trim(adjustl(name)) // '%var0d')
     call is_set_type_turbvar1d(structure_in%var1d, trim(adjustl(name)) // '%var1d')
     call is_set_type_turbvar2d(structure_in%var2d, trim(adjustl(name)) // '%var2d')
     call is_set_type_turbvar3d(structure_in%var3d, trim(adjustl(name)) // '%var3d')
     call is_set_type_turbvar4d(structure_in%var4d, trim(adjustl(name)) // '%var4d')
     call is_set_type_turbvar5d(structure_in%var5d, trim(adjustl(name)) // '%var5d')
     call is_set_type_turbspec1d(structure_in%spec1d, trim(adjustl(name)) // '%spec1d')
     call is_set_type_turbenv1d(structure_in%env1d, trim(adjustl(name)) // '%env1d')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_turbulence

   subroutine is_set_arr_type_turbulence(structure_in, name)

     implicit none

     type (type_turbulence), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbulence(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbulence

   subroutine is_set_type_wall(structure_in, name)

     implicit none

     type (type_wall), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_wall_wall0d(structure_in%wall0d, trim(adjustl(name)) // '%wall0d')
     call is_set_type_wall2d_mhd(structure_in%wall2d_mhd, trim(adjustl(name)) // '%wall2d_mhd')
     call is_set_arr_type_wall2d(structure_in%wall2d, trim(adjustl(name)) // '%wall2d')
     call is_set_arr_type_wall3d(structure_in%wall3d, trim(adjustl(name)) // '%wall3d')
     call is_set_arr_type_wall_types(structure_in%wall_types, trim(adjustl(name)) // '%wall_types')
     call is_set_arr_type_compound_desc(structure_in%compounds, trim(adjustl(name)) // '%compounds')
     call is_set_arr_type_element_desc(structure_in%elements, trim(adjustl(name)) // '%elements')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_wall

   subroutine is_set_arr_type_wall(structure_in, name)

     implicit none

     type (type_wall), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall

   subroutine is_set_type_waves(structure_in, name)

     implicit none

     type (type_waves), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_arr_type_coherentwave(structure_in%coherentwave, trim(adjustl(name)) // '%coherentwave')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_waves

   subroutine is_set_arr_type_waves(structure_in, name)

     implicit none

     type (type_waves), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves

   subroutine is_set_type_amns_constituentType(structure_in, name)

     implicit none

     type (type_amns_constituentType), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_type_integer(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_integer(structure_in%mn, trim(adjustl(name)) // '%mn')
     call is_set_type_float(structure_in%multiplicity, trim(adjustl(name)) // '%multiplicity')

   end subroutine is_set_type_amns_constituentType

   subroutine is_set_arr_type_amns_constituentType(structure_in, name)
 
     implicit none
 
     type (type_amns_constituentType), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_amns_constituentType(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_amns_constituentType

   subroutine is_set_type_amns_processType(structure_in, name)

     implicit none

     type (type_amns_processType), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%proc_label, trim(adjustl(name)) // '%proc_label')
     call is_set_arr_type_reacprodType(structure_in%reactant, trim(adjustl(name)) // '%reactant')
     call is_set_arr_type_reacprodType(structure_in%product, trim(adjustl(name)) // '%product')
     call is_set_type_vecstring_type(structure_in%sup_string, trim(adjustl(name)) // '%sup_string')
     call is_set_type_vecflt_type(structure_in%sup_real, trim(adjustl(name)) // '%sup_real')
     call is_set_type_vecint_type(structure_in%sup_int, trim(adjustl(name)) // '%sup_int')
     call is_set_type_identifier(structure_in%quality, trim(adjustl(name)) // '%quality')
     call is_set_type_vecstring_type(structure_in%err_proc_label, trim(adjustl(name)) // '%err_proc_label')

   end subroutine is_set_type_amns_processType

   subroutine is_set_arr_type_amns_processType(structure_in, name)
 
     implicit none
 
     type (type_amns_processType), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_amns_processType(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_amns_processType

   subroutine is_set_type_antenna_ec(structure_in, name)

     implicit none

     type (type_antenna_ec), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_exp0D(structure_in%power, trim(adjustl(name)) // '%power')
     call is_set_type_integer(structure_in%mode, trim(adjustl(name)) // '%mode')
     call is_set_type_rzphi0D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_launchangles(structure_in%launchangles, trim(adjustl(name)) // '%launchangles')
     call is_set_type_rfbeam(structure_in%beam, trim(adjustl(name)) // '%beam')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_antenna_ec

   subroutine is_set_arr_type_antenna_ec(structure_in, name)
 
     implicit none
 
     type (type_antenna_ec), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_antenna_ec(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_antenna_ec

   subroutine is_set_type_antenna_ic(structure_in, name)

     implicit none

     type (type_antenna_ic), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_exp0D(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_exp0D(structure_in%power, trim(adjustl(name)) // '%power')
     call is_set_type_vecint_type(structure_in%ntor, trim(adjustl(name)) // '%ntor')
     call is_set_type_vecflt_type(structure_in%power_ntor, trim(adjustl(name)) // '%power_ntor')
     call is_set_type_antennaic_setup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_antenna_ic

   subroutine is_set_arr_type_antenna_ic(structure_in, name)
 
     implicit none
 
     type (type_antenna_ic), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_antenna_ic(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_antenna_ic

   subroutine is_set_type_antenna_lh(structure_in, name)

     implicit none

     type (type_antenna_lh), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_exp0D(structure_in%power, trim(adjustl(name)) // '%power')
     call is_set_type_float(structure_in%n_par, trim(adjustl(name)) // '%n_par')
     call is_set_type_rzphi0D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_antennalh_setup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_plasmaedge(structure_in%plasmaedge, trim(adjustl(name)) // '%plasmaedge')
     call is_set_type_rfbeam(structure_in%beam, trim(adjustl(name)) // '%beam')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_antenna_lh

   subroutine is_set_arr_type_antenna_lh(structure_in, name)
 
     implicit none
 
     type (type_antenna_lh), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_antenna_lh(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_antenna_lh

   subroutine is_set_type_antennaic_setup(structure_in, name)

     implicit none

     type (type_antennaic_setup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_straps(structure_in%straps, trim(adjustl(name)) // '%straps')
     call is_set_type_current(structure_in%current, trim(adjustl(name)) // '%current')

   end subroutine is_set_type_antennaic_setup

   subroutine is_set_arr_type_antennaic_setup(structure_in, name)
 
     implicit none
 
     type (type_antennaic_setup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_antennaic_setup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_antennaic_setup

   subroutine is_set_type_antennalh_setup(structure_in, name)

     implicit none

     type (type_antennalh_setup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_modules(structure_in%modules, trim(adjustl(name)) // '%modules')

   end subroutine is_set_type_antennalh_setup

   subroutine is_set_arr_type_antennalh_setup(structure_in, name)
 
     implicit none
 
     type (type_antennalh_setup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_antennalh_setup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_antennalh_setup

   subroutine is_set_type_b0r0(structure_in, name)

     implicit none

     type (type_b0r0), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%r0, trim(adjustl(name)) // '%r0')
     call is_set_type_float(structure_in%b0, trim(adjustl(name)) // '%b0')

   end subroutine is_set_type_b0r0

   subroutine is_set_arr_type_b0r0(structure_in, name)
 
     implicit none
 
     type (type_b0r0), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_b0r0(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_b0r0

   subroutine is_set_type_bb(structure_in, name)

     implicit none

     type (type_bb), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%nb_bb, trim(adjustl(name)) // '%nb_bb')
     call is_set_type_float(structure_in%nb_bb_polcut, trim(adjustl(name)) // '%nb_bb_polcut')
     call is_set_type_float(structure_in%teta_bb, trim(adjustl(name)) // '%teta_bb')
     call is_set_type_float(structure_in%tbr, trim(adjustl(name)) // '%tbr')
     call is_set_type_neutro_resul(structure_in%neutro_resul, trim(adjustl(name)) // '%neutro_resul')
     call is_set_type_bb_specs(structure_in%inboard, trim(adjustl(name)) // '%inboard')
     call is_set_type_bb_specs(structure_in%outboard, trim(adjustl(name)) // '%outboard')

   end subroutine is_set_type_bb

   subroutine is_set_arr_type_bb(structure_in, name)
 
     implicit none
 
     type (type_bb), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_bb(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_bb

   subroutine is_set_type_bb_dimension(structure_in, name)

     implicit none

     type (type_bb_dimension), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%radial, trim(adjustl(name)) // '%radial')
     call is_set_type_vecflt_type(structure_in%toroidal, trim(adjustl(name)) // '%toroidal')
     call is_set_type_vecflt_type(structure_in%poloidal, trim(adjustl(name)) // '%poloidal')

   end subroutine is_set_type_bb_dimension

   subroutine is_set_arr_type_bb_dimension(structure_in, name)
 
     implicit none
 
     type (type_bb_dimension), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_bb_dimension(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_bb_dimension

   subroutine is_set_type_bb_geometry(structure_in, name)

     implicit none

     type (type_bb_geometry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%dr_fw, trim(adjustl(name)) // '%dr_fw')
     call is_set_type_float(structure_in%dr_bz, trim(adjustl(name)) // '%dr_bz')
     call is_set_type_float(structure_in%dr_bp, trim(adjustl(name)) // '%dr_bp')
     call is_set_type_vecflt_type(structure_in%dr_bp_plates, trim(adjustl(name)) // '%dr_bp_plates')
     call is_set_type_vecflt_type(structure_in%dr_bp_he, trim(adjustl(name)) // '%dr_bp_he')
     call is_set_type_float(structure_in%dr_man, trim(adjustl(name)) // '%dr_man')
     call is_set_type_float(structure_in%dt_sw, trim(adjustl(name)) // '%dt_sw')
     call is_set_type_float(structure_in%dt_bz, trim(adjustl(name)) // '%dt_bz')
     call is_set_type_float(structure_in%dp_bz, trim(adjustl(name)) // '%dp_bz')
     call is_set_type_bb_dimension(structure_in%top_cap_dim, trim(adjustl(name)) // '%top_cap_dim')
     call is_set_type_bb_dimension(structure_in%bot_cap_dim, trim(adjustl(name)) // '%bot_cap_dim')
     call is_set_type_float(structure_in%a_fw_ch, trim(adjustl(name)) // '%a_fw_ch')
     call is_set_type_float(structure_in%b_fw_ch, trim(adjustl(name)) // '%b_fw_ch')
     call is_set_type_float(structure_in%td_tc_ch, trim(adjustl(name)) // '%td_tc_ch')
     call is_set_type_float(structure_in%rd_tc_ch, trim(adjustl(name)) // '%rd_tc_ch')
     call is_set_type_float(structure_in%td_bc_ch, trim(adjustl(name)) // '%td_bc_ch')
     call is_set_type_float(structure_in%rd_bc_ch, trim(adjustl(name)) // '%rd_bc_ch')
     call is_set_type_float(structure_in%n_fw_ch, trim(adjustl(name)) // '%n_fw_ch')
     call is_set_type_float(structure_in%n_fw_circ, trim(adjustl(name)) // '%n_fw_circ')
     call is_set_type_float(structure_in%a_sg_ch, trim(adjustl(name)) // '%a_sg_ch')
     call is_set_type_float(structure_in%b_sg_ch, trim(adjustl(name)) // '%b_sg_ch')
     call is_set_type_float(structure_in%n_sg_ch, trim(adjustl(name)) // '%n_sg_ch')
     call is_set_type_float(structure_in%sg_thick, trim(adjustl(name)) // '%sg_thick')
     call is_set_type_float(structure_in%sg_weld, trim(adjustl(name)) // '%sg_weld')
     call is_set_type_float(structure_in%sg_in_out, trim(adjustl(name)) // '%sg_in_out')
     call is_set_type_float(structure_in%r_sg_cp, trim(adjustl(name)) // '%r_sg_cp')
     call is_set_type_float(structure_in%cp_tor_gap, trim(adjustl(name)) // '%cp_tor_gap')
     call is_set_type_float(structure_in%a_cp_ch, trim(adjustl(name)) // '%a_cp_ch')
     call is_set_type_float(structure_in%b_cp_ch, trim(adjustl(name)) // '%b_cp_ch')
     call is_set_type_float(structure_in%n_cp_ch, trim(adjustl(name)) // '%n_cp_ch')
     call is_set_type_float(structure_in%cp_thick, trim(adjustl(name)) // '%cp_thick')
     call is_set_type_float(structure_in%n_pol_bu, trim(adjustl(name)) // '%n_pol_bu')
     call is_set_type_float(structure_in%n_tor_bu, trim(adjustl(name)) // '%n_tor_bu')
     call is_set_type_float(structure_in%n_cp_bu, trim(adjustl(name)) // '%n_cp_bu')
     call is_set_type_float(structure_in%cp_in_out, trim(adjustl(name)) // '%cp_in_out')
     call is_set_type_float(structure_in%he_man_tck, trim(adjustl(name)) // '%he_man_tck')
     call is_set_type_float(structure_in%man_tck, trim(adjustl(name)) // '%man_tck')
     call is_set_type_float(structure_in%pbli_bptb_od, trim(adjustl(name)) // '%pbli_bptb_od')
     call is_set_type_float(structure_in%pbli_bptb_id, trim(adjustl(name)) // '%pbli_bptb_id')
     call is_set_type_float(structure_in%he_bptb_od, trim(adjustl(name)) // '%he_bptb_od')
     call is_set_type_float(structure_in%he_bptb_id, trim(adjustl(name)) // '%he_bptb_id')
     call is_set_type_float(structure_in%dr_max_fw, trim(adjustl(name)) // '%dr_max_fw')
     call is_set_type_float(structure_in%dr_fwpl, trim(adjustl(name)) // '%dr_fwpl')

   end subroutine is_set_type_bb_geometry

   subroutine is_set_arr_type_bb_geometry(structure_in, name)
 
     implicit none
 
     type (type_bb_geometry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_bb_geometry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_bb_geometry

   subroutine is_set_type_bb_specs(structure_in, name)

     implicit none

     type (type_bb_specs), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%nbb, trim(adjustl(name)) // '%nbb')
     call is_set_type_float(structure_in%r1, trim(adjustl(name)) // '%r1')
     call is_set_type_float(structure_in%r2, trim(adjustl(name)) // '%r2')
     call is_set_type_bb_dimension(structure_in%dimension, trim(adjustl(name)) // '%dimension')

   end subroutine is_set_type_bb_specs

   subroutine is_set_arr_type_bb_specs(structure_in, name)
 
     implicit none
 
     type (type_bb_specs), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_bb_specs(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_bb_specs

   subroutine is_set_type_beamletgroup(structure_in, name)

     implicit none

     type (type_beamletgroup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi0D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_float(structure_in%tang_rad, trim(adjustl(name)) // '%tang_rad')
     call is_set_type_float(structure_in%angle, trim(adjustl(name)) // '%angle')
     call is_set_type_integer(structure_in%direction, trim(adjustl(name)) // '%direction')
     call is_set_type_float(structure_in%width_horiz, trim(adjustl(name)) // '%width_horiz')
     call is_set_type_float(structure_in%width_vert, trim(adjustl(name)) // '%width_vert')
     call is_set_type_focussing(structure_in%focussing, trim(adjustl(name)) // '%focussing')
     call is_set_type_divergence(structure_in%divergence, trim(adjustl(name)) // '%divergence')
     call is_set_type_beamlets(structure_in%beamlets, trim(adjustl(name)) // '%beamlets')

   end subroutine is_set_type_beamletgroup

   subroutine is_set_arr_type_beamletgroup(structure_in, name)
 
     implicit none
 
     type (type_beamletgroup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_beamletgroup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_beamletgroup

   subroutine is_set_type_beamlets(structure_in, name)

     implicit none

     type (type_beamlets), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_vecflt_type(structure_in%tang_rad_blt, trim(adjustl(name)) // '%tang_rad_blt')
     call is_set_type_vecflt_type(structure_in%angle_blt, trim(adjustl(name)) // '%angle_blt')
     call is_set_type_vecflt_type(structure_in%pow_frc_blt, trim(adjustl(name)) // '%pow_frc_blt')

   end subroutine is_set_type_beamlets

   subroutine is_set_arr_type_beamlets(structure_in, name)
 
     implicit none
 
     type (type_beamlets), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_beamlets(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_beamlets

   subroutine is_set_type_beamtracing(structure_in, name)

     implicit none

     type (type_beamtracing), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%npoints, trim(adjustl(name)) // '%npoints')
     call is_set_type_float(structure_in%power, trim(adjustl(name)) // '%power')
     call is_set_type_vecflt_type(structure_in%dnpar, trim(adjustl(name)) // '%dnpar')
     call is_set_type_vecflt_type(structure_in%length, trim(adjustl(name)) // '%length')
     call is_set_type_waves_rtposition(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_waves_rtwavevector(structure_in%wavevector, trim(adjustl(name)) // '%wavevector')
     call is_set_type_polarization(structure_in%polarization, trim(adjustl(name)) // '%polarization')
     call is_set_type_powerflow(structure_in%powerflow, trim(adjustl(name)) // '%powerflow')

   end subroutine is_set_type_beamtracing

   subroutine is_set_arr_type_beamtracing(structure_in, name)
 
     implicit none
 
     type (type_beamtracing), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_beamtracing(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_beamtracing

   subroutine is_set_type_boundary(structure_in, name)

     implicit none

     type (type_boundary), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_float(structure_in%rho, trim(adjustl(name)) // '%rho')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_boundary

   subroutine is_set_arr_type_boundary(structure_in, name)
 
     implicit none
 
     type (type_boundary), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_boundary(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_boundary

   subroutine is_set_type_boundary_neutrals(structure_in, name)

     implicit none

     type (type_boundary_neutrals), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_integer(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_float(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')

   end subroutine is_set_type_boundary_neutrals

   subroutine is_set_arr_type_boundary_neutrals(structure_in, name)
 
     implicit none
 
     type (type_boundary_neutrals), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_boundary_neutrals(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_boundary_neutrals

   subroutine is_set_type_boundaryel(structure_in, name)

     implicit none

     type (type_boundaryel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_float(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')

   end subroutine is_set_type_boundaryel

   subroutine is_set_arr_type_boundaryel(structure_in, name)
 
     implicit none
 
     type (type_boundaryel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_boundaryel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_boundaryel

   subroutine is_set_type_boundaryimp(structure_in, name)

     implicit none

     type (type_boundaryimp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecint_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%rho, trim(adjustl(name)) // '%rho')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_boundaryimp

   subroutine is_set_arr_type_boundaryimp(structure_in, name)
 
     implicit none
 
     type (type_boundaryimp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_boundaryimp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_boundaryimp

   subroutine is_set_type_boundaryion(structure_in, name)

     implicit none

     type (type_boundaryion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecint_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')

   end subroutine is_set_type_boundaryion

   subroutine is_set_arr_type_boundaryion(structure_in, name)
 
     implicit none
 
     type (type_boundaryion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_boundaryion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_boundaryion

   subroutine is_set_type_bpol_probes(structure_in, name)

     implicit none

     type (type_bpol_probes), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_setup_bprobe(structure_in%setup_bprobe, trim(adjustl(name)) // '%setup_bprobe')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_bpol_probes

   subroutine is_set_arr_type_bpol_probes(structure_in, name)
 
     implicit none
 
     type (type_bpol_probes), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_bpol_probes(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_bpol_probes

   subroutine is_set_type_calorimetry_heat_source(structure_in, name)

     implicit none

     type (type_calorimetry_heat_source), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_float(structure_in%temp_in, trim(adjustl(name)) // '%temp_in')
     call is_set_type_float(structure_in%temp_out, trim(adjustl(name)) // '%temp_out')
     call is_set_type_float(structure_in%press_in, trim(adjustl(name)) // '%press_in')
     call is_set_type_float(structure_in%press_out, trim(adjustl(name)) // '%press_out')
     call is_set_type_float(structure_in%flow, trim(adjustl(name)) // '%flow')
     call is_set_type_float(structure_in%power, trim(adjustl(name)) // '%power')

   end subroutine is_set_type_calorimetry_heat_source

   subroutine is_set_arr_type_calorimetry_heat_source(structure_in, name)
 
     implicit none
 
     type (type_calorimetry_heat_source), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_calorimetry_heat_source(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_calorimetry_heat_source

   subroutine is_set_type_circuits(structure_in, name)

     implicit none

     type (type_circuits), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_power_conv_component(structure_in%component, trim(adjustl(name)) // '%component')
     call is_set_type_float(structure_in%power_net, trim(adjustl(name)) // '%power_net')
     call is_set_type_float(structure_in%power_int, trim(adjustl(name)) // '%power_int')
     call is_set_type_float(structure_in%efficiency, trim(adjustl(name)) // '%efficiency')

   end subroutine is_set_type_circuits

   subroutine is_set_arr_type_circuits(structure_in, name)
 
     implicit none
 
     type (type_circuits), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_circuits(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_circuits

   subroutine is_set_type_circularcoil(structure_in, name)

     implicit none

     type (type_circularcoil), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz0D(structure_in%centre, trim(adjustl(name)) // '%centre')
     call is_set_type_float(structure_in%hlength, trim(adjustl(name)) // '%hlength')
     call is_set_type_float(structure_in%radialhwidth, trim(adjustl(name)) // '%radialhwidth')

   end subroutine is_set_type_circularcoil

   subroutine is_set_arr_type_circularcoil(structure_in, name)
 
     implicit none
 
     type (type_circularcoil), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_circularcoil(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_circularcoil

   subroutine is_set_type_clusters(structure_in, name)

     implicit none

     type (type_clusters), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_integer(structure_in%start, trim(adjustl(name)) // '%start')
     call is_set_type_integer(structure_in%finish, trim(adjustl(name)) // '%finish')

   end subroutine is_set_type_clusters

   subroutine is_set_arr_type_clusters(structure_in, name)
 
     implicit none
 
     type (type_clusters), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_clusters(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_clusters

   subroutine is_set_type_codeparam(structure_in, name)

     implicit none

     type (type_codeparam), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%codename, trim(adjustl(name)) // '%codename')
     call is_set_type_vecstring_type(structure_in%codeversion, trim(adjustl(name)) // '%codeversion')
     call is_set_type_vecstring_type(structure_in%parameters, trim(adjustl(name)) // '%parameters')
     call is_set_type_vecstring_type(structure_in%output_diag, trim(adjustl(name)) // '%output_diag')
     call is_set_type_integer(structure_in%output_flag, trim(adjustl(name)) // '%output_flag')

   end subroutine is_set_type_codeparam

   subroutine is_set_arr_type_codeparam(structure_in, name)
 
     implicit none
 
     type (type_codeparam), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_codeparam(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_codeparam

   subroutine is_set_type_coefficients_neutrals(structure_in, name)

     implicit none

     type (type_coefficients_neutrals), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_recycling_neutrals(structure_in%recycling, trim(adjustl(name)) // '%recycling')
     call is_set_type_sputtering_neutrals(structure_in%sputtering, trim(adjustl(name)) // '%sputtering')

   end subroutine is_set_type_coefficients_neutrals

   subroutine is_set_arr_type_coefficients_neutrals(structure_in, name)
 
     implicit none
 
     type (type_coefficients_neutrals), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coefficients_neutrals(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coefficients_neutrals

   subroutine is_set_type_coherentwave(structure_in, name)

     implicit none

     type (type_coherentwave), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_enum_instance(structure_in%wave_id, trim(adjustl(name)) // '%wave_id')
     call is_set_type_composition(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_compositions_type(structure_in%compositions, trim(adjustl(name)) // '%compositions')
     call is_set_type_waves_global_param(structure_in%global_param, trim(adjustl(name)) // '%global_param')
     call is_set_type_waves_grid_1d(structure_in%grid_1d, trim(adjustl(name)) // '%grid_1d')
     call is_set_type_waves_grid_2d(structure_in%grid_2d, trim(adjustl(name)) // '%grid_2d')
     call is_set_type_waves_profiles_1d(structure_in%profiles_1d, trim(adjustl(name)) // '%profiles_1d')
     call is_set_type_waves_profiles_2d(structure_in%profiles_2d, trim(adjustl(name)) // '%profiles_2d')
     call is_set_arr_type_beamtracing(structure_in%beamtracing, trim(adjustl(name)) // '%beamtracing')
     call is_set_type_fullwave(structure_in%fullwave, trim(adjustl(name)) // '%fullwave')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_coherentwave

   subroutine is_set_arr_type_coherentwave(structure_in, name)
 
     implicit none
 
     type (type_coherentwave), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coherentwave(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coherentwave

   subroutine is_set_type_coil(structure_in, name)

     implicit none

     type (type_coil), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_desc_coils(structure_in%desc_coils, trim(adjustl(name)) // '%desc_coils')
     call is_set_type_exp1D(structure_in%coilcurrent, trim(adjustl(name)) // '%coilcurrent')
     call is_set_type_exp1D(structure_in%coilvoltage, trim(adjustl(name)) // '%coilvoltage')

   end subroutine is_set_type_coil

   subroutine is_set_arr_type_coil(structure_in, name)
 
     implicit none
 
     type (type_coil), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coil(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coil

   subroutine is_set_type_com(structure_in, name)

     implicit none

     type (type_com), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_float(structure_in%zion, trim(adjustl(name)) // '%zion')
     call is_set_type_vecflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_vecflt_type(structure_in%magn_mom, trim(adjustl(name)) // '%magn_mom')
     call is_set_type_vecflt_type(structure_in%p_phi, trim(adjustl(name)) // '%p_phi')
     call is_set_type_vecint_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')

   end subroutine is_set_type_com

   subroutine is_set_arr_type_com(structure_in, name)
 
     implicit none
 
     type (type_com), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_com(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_com

   subroutine is_set_type_complexgrid(structure_in, name)

     implicit none

     type (type_complexgrid), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%uid, trim(adjustl(name)) // '%uid')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_arr_type_complexgrid_space(structure_in%spaces, trim(adjustl(name)) // '%spaces')
     call is_set_arr_type_complexgrid_subgrid(structure_in%subgrids, trim(adjustl(name)) // '%subgrids')
     call is_set_type_complexgrid_metric(structure_in%metric, trim(adjustl(name)) // '%metric')
     call is_set_arr_type_complexgrid_geo_global(structure_in%geo, trim(adjustl(name)) // '%geo')
     call is_set_arr_type_complexgrid_vector(structure_in%bases, trim(adjustl(name)) // '%bases')

   end subroutine is_set_type_complexgrid

   subroutine is_set_arr_type_complexgrid(structure_in, name)
 
     implicit none
 
     type (type_complexgrid), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid

   subroutine is_set_type_complexgrid_geo_global(structure_in, name)

     implicit none

     type (type_complexgrid_geo_global), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%geotype, trim(adjustl(name)) // '%geotype')
     call is_set_type_vecstring_type(structure_in%geotypeid, trim(adjustl(name)) // '%geotypeid')
     call is_set_type_vecint_type(structure_in%coordtype, trim(adjustl(name)) // '%coordtype')
     call is_set_arr_type_complexgrid_scalar(structure_in%geo_matrix, trim(adjustl(name)) // '%geo_matrix')
     call is_set_arr_type_complexgrid_scalar(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_complexgrid_geo_global

   subroutine is_set_arr_type_complexgrid_geo_global(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_geo_global), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_geo_global(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_geo_global

   subroutine is_set_type_complexgrid_indexlist(structure_in, name)

     implicit none

     type (type_complexgrid_indexlist), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%range, trim(adjustl(name)) // '%range')
     call is_set_type_vecint_type(structure_in%ind, trim(adjustl(name)) // '%ind')

   end subroutine is_set_type_complexgrid_indexlist

   subroutine is_set_arr_type_complexgrid_indexlist(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_indexlist), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_indexlist(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_indexlist

   subroutine is_set_type_complexgrid_metric(structure_in, name)

     implicit none

     type (type_complexgrid_metric), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_complexgrid_scalar(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_arr_type_complexgrid_scalar(structure_in%g11, trim(adjustl(name)) // '%g11')
     call is_set_arr_type_complexgrid_scalar(structure_in%g12, trim(adjustl(name)) // '%g12')
     call is_set_arr_type_complexgrid_scalar(structure_in%g13, trim(adjustl(name)) // '%g13')
     call is_set_arr_type_complexgrid_scalar(structure_in%g22, trim(adjustl(name)) // '%g22')
     call is_set_arr_type_complexgrid_scalar(structure_in%g23, trim(adjustl(name)) // '%g23')
     call is_set_arr_type_complexgrid_scalar(structure_in%g33, trim(adjustl(name)) // '%g33')
     call is_set_arr_type_complexgrid_scalar(structure_in%jacobian, trim(adjustl(name)) // '%jacobian')

   end subroutine is_set_type_complexgrid_metric

   subroutine is_set_arr_type_complexgrid_metric(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_metric), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_metric(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_metric

   subroutine is_set_type_complexgrid_objectlist(structure_in, name)

     implicit none

     type (type_complexgrid_objectlist), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%cls, trim(adjustl(name)) // '%cls')
     call is_set_arr_type_complexgrid_indexlist(structure_in%indset, trim(adjustl(name)) // '%indset')
     call is_set_type_matint_type(structure_in%ind, trim(adjustl(name)) // '%ind')

   end subroutine is_set_type_complexgrid_objectlist

   subroutine is_set_arr_type_complexgrid_objectlist(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_objectlist), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_objectlist(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_objectlist

   subroutine is_set_type_complexgrid_scalar(structure_in, name)

     implicit none

     type (type_complexgrid_scalar), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%griduid, trim(adjustl(name)) // '%griduid')
     call is_set_type_integer(structure_in%subgrid, trim(adjustl(name)) // '%subgrid')
     call is_set_type_vecflt_type(structure_in%scalar, trim(adjustl(name)) // '%scalar')
     call is_set_type_matflt_type(structure_in%vector, trim(adjustl(name)) // '%vector')
     call is_set_type_array3dflt_type(structure_in%matrix, trim(adjustl(name)) // '%matrix')

   end subroutine is_set_type_complexgrid_scalar

   subroutine is_set_arr_type_complexgrid_scalar(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_scalar), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_scalar(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_scalar

   subroutine is_set_type_complexgrid_scalar_cplx(structure_in, name)

     implicit none

     type (type_complexgrid_scalar_cplx), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%griduid, trim(adjustl(name)) // '%griduid')
     call is_set_type_integer(structure_in%subgrid, trim(adjustl(name)) // '%subgrid')
     call is_set_type_veccplx_type(structure_in%scalar, trim(adjustl(name)) // '%scalar')
     call is_set_type_matcplx_type(structure_in%vector, trim(adjustl(name)) // '%vector')
     call is_set_type_array3dcplx_type(structure_in%matrix, trim(adjustl(name)) // '%matrix')

   end subroutine is_set_type_complexgrid_scalar_cplx

   subroutine is_set_arr_type_complexgrid_scalar_cplx(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_scalar_cplx), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_scalar_cplx(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_scalar_cplx

   subroutine is_set_type_complexgrid_scalar_int(structure_in, name)

     implicit none

     type (type_complexgrid_scalar_int), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%griduid, trim(adjustl(name)) // '%griduid')
     call is_set_type_integer(structure_in%subgrid, trim(adjustl(name)) // '%subgrid')
     call is_set_type_vecint_type(structure_in%scalar, trim(adjustl(name)) // '%scalar')
     call is_set_type_matint_type(structure_in%vector, trim(adjustl(name)) // '%vector')
     call is_set_type_array3dint_type(structure_in%matrix, trim(adjustl(name)) // '%matrix')

   end subroutine is_set_type_complexgrid_scalar_int

   subroutine is_set_arr_type_complexgrid_scalar_int(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_scalar_int), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_scalar_int(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_scalar_int

   subroutine is_set_type_complexgrid_scalar_simplestruct(structure_in, name)

     implicit none

     type (type_complexgrid_scalar_simplestruct), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%subgrid, trim(adjustl(name)) // '%subgrid')
     call is_set_type_vecflt_type(structure_in%scalar, trim(adjustl(name)) // '%scalar')
     call is_set_type_matflt_type(structure_in%vector, trim(adjustl(name)) // '%vector')
     call is_set_type_array3dflt_type(structure_in%matrix, trim(adjustl(name)) // '%matrix')

   end subroutine is_set_type_complexgrid_scalar_simplestruct

   subroutine is_set_arr_type_complexgrid_scalar_simplestruct(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_scalar_simplestruct), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_scalar_simplestruct(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_scalar_simplestruct

   subroutine is_set_type_complexgrid_space(structure_in, name)

     implicit none

     type (type_complexgrid_space), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%geotype, trim(adjustl(name)) // '%geotype')
     call is_set_type_vecstring_type(structure_in%geotypeid, trim(adjustl(name)) // '%geotypeid')
     call is_set_type_matint_type(structure_in%coordtype, trim(adjustl(name)) // '%coordtype')
     call is_set_arr_type_objects(structure_in%objects, trim(adjustl(name)) // '%objects')
     call is_set_type_vecint_type(structure_in%xpoints, trim(adjustl(name)) // '%xpoints')

   end subroutine is_set_type_complexgrid_space

   subroutine is_set_arr_type_complexgrid_space(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_space), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_space(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_space

   subroutine is_set_type_complexgrid_subgrid(structure_in, name)

     implicit none

     type (type_complexgrid_subgrid), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_arr_type_complexgrid_objectlist(structure_in%list, trim(adjustl(name)) // '%list')

   end subroutine is_set_type_complexgrid_subgrid

   subroutine is_set_arr_type_complexgrid_subgrid(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_subgrid), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_subgrid(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_subgrid

   subroutine is_set_type_complexgrid_vector(structure_in, name)

     implicit none

     type (type_complexgrid_vector), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%griduid, trim(adjustl(name)) // '%griduid')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_arr_type_complexgrid_scalar(structure_in%comp, trim(adjustl(name)) // '%comp')
     call is_set_type_vecint_type(structure_in%align, trim(adjustl(name)) // '%align')
     call is_set_type_vecstring_type(structure_in%alignid, trim(adjustl(name)) // '%alignid')
     call is_set_type_integer(structure_in%basis, trim(adjustl(name)) // '%basis')

   end subroutine is_set_type_complexgrid_vector

   subroutine is_set_arr_type_complexgrid_vector(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_vector), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_vector(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_vector

   subroutine is_set_type_complexgrid_vector_simplestruct(structure_in, name)

     implicit none

     type (type_complexgrid_vector_simplestruct), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_arr_type_complexgrid_scalar(structure_in%comp, trim(adjustl(name)) // '%comp')
     call is_set_type_vecint_type(structure_in%align, trim(adjustl(name)) // '%align')
     call is_set_type_vecstring_type(structure_in%alignid, trim(adjustl(name)) // '%alignid')

   end subroutine is_set_type_complexgrid_vector_simplestruct

   subroutine is_set_arr_type_complexgrid_vector_simplestruct(structure_in, name)
 
     implicit none
 
     type (type_complexgrid_vector_simplestruct), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_complexgrid_vector_simplestruct(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_complexgrid_vector_simplestruct

   subroutine is_set_type_composition(structure_in, name)

     implicit none

     type (type_composition), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_vecflt_type(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_vecflt_type(structure_in%zion, trim(adjustl(name)) // '%zion')
     call is_set_type_vecint_type(structure_in%imp_flag, trim(adjustl(name)) // '%imp_flag')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_composition

   subroutine is_set_arr_type_composition(structure_in, name)
 
     implicit none
 
     type (type_composition), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_composition(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_composition

   subroutine is_set_type_composition_neutrals(structure_in, name)

     implicit none

     type (type_composition_neutrals), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_coreneutrals_atomlist(structure_in%atomlist, trim(adjustl(name)) // '%atomlist')
     call is_set_arr_type_composition_neutralscomp(structure_in%neutral, trim(adjustl(name)) // '%neutral')

   end subroutine is_set_type_composition_neutrals

   subroutine is_set_arr_type_composition_neutrals(structure_in, name)
 
     implicit none
 
     type (type_composition_neutrals), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_composition_neutrals(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_composition_neutrals

   subroutine is_set_type_composition_neutrals_neutcomp(structure_in, name)

     implicit none

     type (type_composition_neutrals_neutcomp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nucindex, trim(adjustl(name)) // '%nucindex')
     call is_set_type_integer(structure_in%multiplicity, trim(adjustl(name)) // '%multiplicity')

   end subroutine is_set_type_composition_neutrals_neutcomp

   subroutine is_set_arr_type_composition_neutrals_neutcomp(structure_in, name)
 
     implicit none
 
     type (type_composition_neutrals_neutcomp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_composition_neutrals_neutcomp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_composition_neutrals_neutcomp

   subroutine is_set_type_composition_neutralscomp(structure_in, name)

     implicit none

     type (type_composition_neutralscomp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_composition_neutrals_neutcomp(structure_in%neutcomp, trim(adjustl(name)) // '%neutcomp')
     call is_set_arr_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_composition_neutralscomp

   subroutine is_set_arr_type_composition_neutralscomp(structure_in, name)
 
     implicit none
 
     type (type_composition_neutralscomp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_composition_neutralscomp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_composition_neutralscomp

   subroutine is_set_type_compositions_type(structure_in, name)

     implicit none

     type (type_compositions_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_nuclei(structure_in%nuclei, trim(adjustl(name)) // '%nuclei')
     call is_set_arr_type_ions(structure_in%ions, trim(adjustl(name)) // '%ions')
     call is_set_arr_type_impurities(structure_in%impurities, trim(adjustl(name)) // '%impurities')
     call is_set_arr_type_composition_neutralscomp(structure_in%neutralscomp, trim(adjustl(name)) // '%neutralscomp')
     call is_set_arr_type_edgespecies(structure_in%edgespecies, trim(adjustl(name)) // '%edgespecies')
     call is_set_type_identifier(structure_in%signature, trim(adjustl(name)) // '%signature')

   end subroutine is_set_type_compositions_type

   subroutine is_set_arr_type_compositions_type(structure_in, name)
 
     implicit none
 
     type (type_compositions_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_compositions_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_compositions_type

   subroutine is_set_type_compound_desc(structure_in, name)

     implicit none

     type (type_compound_desc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_type_vecflt_type(structure_in%stochiometry, trim(adjustl(name)) // '%stochiometry')
     call is_set_type_float(structure_in%density, trim(adjustl(name)) // '%density')
     call is_set_type_float(structure_in%heat_cap, trim(adjustl(name)) // '%heat_cap')
     call is_set_type_vecflt_type(structure_in%heat_cond, trim(adjustl(name)) // '%heat_cond')
     call is_set_type_matflt_type(structure_in%surf_recrate, trim(adjustl(name)) // '%surf_recrate')

   end subroutine is_set_type_compound_desc

   subroutine is_set_arr_type_compound_desc(structure_in, name)
 
     implicit none
 
     type (type_compound_desc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_compound_desc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_compound_desc

   subroutine is_set_type_coord_sys(structure_in, name)

     implicit none

     type (type_coord_sys), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%grid_type, trim(adjustl(name)) // '%grid_type')
     call is_set_type_reggrid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_type_matflt_type(structure_in%jacobian, trim(adjustl(name)) // '%jacobian')
     call is_set_type_matflt_type(structure_in%g_11, trim(adjustl(name)) // '%g_11')
     call is_set_type_matflt_type(structure_in%g_12, trim(adjustl(name)) // '%g_12')
     call is_set_type_matflt_type(structure_in%g_13, trim(adjustl(name)) // '%g_13')
     call is_set_type_matflt_type(structure_in%g_22, trim(adjustl(name)) // '%g_22')
     call is_set_type_matflt_type(structure_in%g_23, trim(adjustl(name)) // '%g_23')
     call is_set_type_matflt_type(structure_in%g_33, trim(adjustl(name)) // '%g_33')
     call is_set_type_rz2D(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_coord_sys

   subroutine is_set_arr_type_coord_sys(structure_in, name)
 
     implicit none
 
     type (type_coord_sys), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coord_sys(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coord_sys

   subroutine is_set_type_coordinates(structure_in, name)

     implicit none

     type (type_coordinates), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_coordinates

   subroutine is_set_arr_type_coordinates(structure_in, name)
 
     implicit none
 
     type (type_coordinates), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coordinates(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coordinates

   subroutine is_set_type_coords(structure_in, name)

     implicit none

     type (type_coords), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%coord, trim(adjustl(name)) // '%coord')
     call is_set_type_vecstring_type(structure_in%coord_label, trim(adjustl(name)) // '%coord_label')
     call is_set_type_vecint_type(structure_in%extrap_type, trim(adjustl(name)) // '%extrap_type')
     call is_set_type_integer(structure_in%interp_type, trim(adjustl(name)) // '%interp_type')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_type_vecstring_type(structure_in%unit, trim(adjustl(name)) // '%unit')
     call is_set_type_integer(structure_in%transform, trim(adjustl(name)) // '%transform')
     call is_set_type_integer(structure_in%spacing, trim(adjustl(name)) // '%spacing')

   end subroutine is_set_type_coords

   subroutine is_set_arr_type_coords(structure_in, name)
 
     implicit none
 
     type (type_coords), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coords(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coords

   subroutine is_set_type_coredelta_values(structure_in, name)

     implicit none

     type (type_coredelta_values), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%deltaid, trim(adjustl(name)) // '%deltaid')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%delta_psi, trim(adjustl(name)) // '%delta_psi')
     call is_set_type_vecflt_type(structure_in%delta_te, trim(adjustl(name)) // '%delta_te')
     call is_set_type_matflt_type(structure_in%delta_ti, trim(adjustl(name)) // '%delta_ti')
     call is_set_type_vecflt_type(structure_in%delta_ne, trim(adjustl(name)) // '%delta_ne')
     call is_set_type_matflt_type(structure_in%delta_ni, trim(adjustl(name)) // '%delta_ni')
     call is_set_arr_type_coredelta_values_impurity(structure_in%impurity, trim(adjustl(name)) // '%impurity')
     call is_set_type_matflt_type(structure_in%delta_vtor, trim(adjustl(name)) // '%delta_vtor')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_coredelta_values

   subroutine is_set_arr_type_coredelta_values(structure_in, name)
 
     implicit none
 
     type (type_coredelta_values), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coredelta_values(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coredelta_values

   subroutine is_set_type_coredelta_values_impurity(structure_in, name)

     implicit none

     type (type_coredelta_values_impurity), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%delta_tz, trim(adjustl(name)) // '%delta_tz')
     call is_set_type_matflt_type(structure_in%delta_nz, trim(adjustl(name)) // '%delta_nz')

   end subroutine is_set_type_coredelta_values_impurity

   subroutine is_set_arr_type_coredelta_values_impurity(structure_in, name)
 
     implicit none
 
     type (type_coredelta_values_impurity), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coredelta_values_impurity(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coredelta_values_impurity

   subroutine is_set_type_corefast_values(structure_in, name)

     implicit none

     type (type_corefast_values), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%fastid, trim(adjustl(name)) // '%fastid')
     call is_set_type_fast_thermal_separation_filter(structure_in%filter, trim(adjustl(name)) // '%filter')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%j, trim(adjustl(name)) // '%j')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_matflt_type(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_vecflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_matflt_type(structure_in%nz, trim(adjustl(name)) // '%nz')
     call is_set_type_matflt_type(structure_in%pi, trim(adjustl(name)) // '%pi')
     call is_set_type_vecflt_type(structure_in%pe, trim(adjustl(name)) // '%pe')
     call is_set_type_matflt_type(structure_in%pz, trim(adjustl(name)) // '%pz')
     call is_set_type_matflt_type(structure_in%pi_para, trim(adjustl(name)) // '%pi_para')
     call is_set_type_vecflt_type(structure_in%pe_para, trim(adjustl(name)) // '%pe_para')
     call is_set_type_matflt_type(structure_in%pz_para, trim(adjustl(name)) // '%pz_para')
     call is_set_type_matflt_type(structure_in%ui, trim(adjustl(name)) // '%ui')
     call is_set_type_matflt_type(structure_in%uz, trim(adjustl(name)) // '%uz')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_corefast_values

   subroutine is_set_arr_type_corefast_values(structure_in, name)
 
     implicit none
 
     type (type_corefast_values), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefast_values(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefast_values

   subroutine is_set_type_corefield(structure_in, name)

     implicit none

     type (type_corefield), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%ddrho, trim(adjustl(name)) // '%ddrho')
     call is_set_type_vecflt_type(structure_in%d2drho2, trim(adjustl(name)) // '%d2drho2')
     call is_set_type_vecflt_type(structure_in%ddt, trim(adjustl(name)) // '%ddt')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')
     call is_set_type_boundaryel(structure_in%boundary, trim(adjustl(name)) // '%boundary')
     call is_set_type_sourceel(structure_in%source_term, trim(adjustl(name)) // '%source_term')
     call is_set_type_coretransel(structure_in%transp_coef, trim(adjustl(name)) // '%transp_coef')
     call is_set_type_fluxel(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_vecflt_type(structure_in%flux_dv_surf, trim(adjustl(name)) // '%flux_dv_surf')
     call is_set_type_vecflt_type(structure_in%time_deriv, trim(adjustl(name)) // '%time_deriv')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_corefield

   subroutine is_set_arr_type_corefield(structure_in, name)
 
     implicit none
 
     type (type_corefield), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefield(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefield

   subroutine is_set_type_corefieldion(structure_in, name)

     implicit none

     type (type_corefieldion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_matflt_type(structure_in%ddrho, trim(adjustl(name)) // '%ddrho')
     call is_set_type_matflt_type(structure_in%d2drho2, trim(adjustl(name)) // '%d2drho2')
     call is_set_type_matflt_type(structure_in%ddt, trim(adjustl(name)) // '%ddt')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecint_type(structure_in%flag, trim(adjustl(name)) // '%flag')
     call is_set_type_boundaryion(structure_in%boundary, trim(adjustl(name)) // '%boundary')
     call is_set_type_sourceion(structure_in%source_term, trim(adjustl(name)) // '%source_term')
     call is_set_type_coretransion(structure_in%transp_coef, trim(adjustl(name)) // '%transp_coef')
     call is_set_type_fluxion(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_matflt_type(structure_in%flux_dv_surf, trim(adjustl(name)) // '%flux_dv_surf')
     call is_set_type_matflt_type(structure_in%time_deriv, trim(adjustl(name)) // '%time_deriv')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_corefieldion

   subroutine is_set_arr_type_corefieldion(structure_in, name)
 
     implicit none
 
     type (type_corefieldion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefieldion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefieldion

   subroutine is_set_type_corefieldneutral(structure_in, name)

     implicit none

     type (type_corefieldneutral), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_boundary_neutrals(structure_in%boundary, trim(adjustl(name)) // '%boundary')

   end subroutine is_set_type_corefieldneutral

   subroutine is_set_arr_type_corefieldneutral(structure_in, name)
 
     implicit none
 
     type (type_corefieldneutral), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefieldneutral(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefieldneutral

   subroutine is_set_type_corefieldneutrale(structure_in, name)

     implicit none

     type (type_corefieldneutrale), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_boundary_neutrals(structure_in%boundary, trim(adjustl(name)) // '%boundary')

   end subroutine is_set_type_corefieldneutrale

   subroutine is_set_arr_type_corefieldneutrale(structure_in, name)
 
     implicit none
 
     type (type_corefieldneutrale), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefieldneutrale(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefieldneutrale

   subroutine is_set_type_corefieldneutralv(structure_in, name)

     implicit none

     type (type_corefieldneutralv), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_boundary_neutrals(structure_in%boundary, trim(adjustl(name)) // '%boundary')

   end subroutine is_set_type_corefieldneutralv

   subroutine is_set_arr_type_corefieldneutralv(structure_in, name)
 
     implicit none
 
     type (type_corefieldneutralv), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefieldneutralv(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefieldneutralv

   subroutine is_set_type_corefieldneutralv0(structure_in, name)

     implicit none

     type (type_corefieldneutralv0), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_corefieldneutralv(structure_in%toroidal, trim(adjustl(name)) // '%toroidal')
     call is_set_type_corefieldneutralv(structure_in%poloidal, trim(adjustl(name)) // '%poloidal')
     call is_set_type_corefieldneutralv(structure_in%radial, trim(adjustl(name)) // '%radial')

   end subroutine is_set_type_corefieldneutralv0

   subroutine is_set_arr_type_corefieldneutralv0(structure_in, name)
 
     implicit none
 
     type (type_corefieldneutralv0), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_corefieldneutralv0(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_corefieldneutralv0

   subroutine is_set_type_coreimpurdiag_sum_radiation(structure_in, name)

     implicit none

     type (type_coreimpurdiag_sum_radiation), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreimpurediagsum_type(structure_in%line_rad, trim(adjustl(name)) // '%line_rad')
     call is_set_type_coreimpurediagsum_type(structure_in%brem_radrec, trim(adjustl(name)) // '%brem_radrec')
     call is_set_type_coreimpurediagsum_type(structure_in%sum, trim(adjustl(name)) // '%sum')

   end subroutine is_set_type_coreimpurdiag_sum_radiation

   subroutine is_set_arr_type_coreimpurdiag_sum_radiation(structure_in, name)
 
     implicit none
 
     type (type_coreimpurdiag_sum_radiation), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurdiag_sum_radiation(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurdiag_sum_radiation

   subroutine is_set_type_coreimpurediag_energy(structure_in, name)

     implicit none

     type (type_coreimpurediag_energy), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreimpurediagprof_type(structure_in%ionization, trim(adjustl(name)) // '%ionization')
     call is_set_type_coreimpurediagprof_type(structure_in%recombin, trim(adjustl(name)) // '%recombin')
     call is_set_type_coreimpurediagprof_type(structure_in%sum, trim(adjustl(name)) // '%sum')

   end subroutine is_set_type_coreimpurediag_energy

   subroutine is_set_arr_type_coreimpurediag_energy(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediag_energy), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediag_energy(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediag_energy

   subroutine is_set_type_coreimpurediag_radiation(structure_in, name)

     implicit none

     type (type_coreimpurediag_radiation), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreimpurediagprof_type(structure_in%line_rad, trim(adjustl(name)) // '%line_rad')
     call is_set_type_coreimpurediagprof_type(structure_in%brem_radrec, trim(adjustl(name)) // '%brem_radrec')
     call is_set_type_coreimpurediagprof_type(structure_in%sum, trim(adjustl(name)) // '%sum')

   end subroutine is_set_type_coreimpurediag_radiation

   subroutine is_set_arr_type_coreimpurediag_radiation(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediag_radiation), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediag_radiation(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediag_radiation

   subroutine is_set_type_coreimpurediag_sum(structure_in, name)

     implicit none

     type (type_coreimpurediag_sum), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreimpurdiag_sum_radiation(structure_in%radiation, trim(adjustl(name)) // '%radiation')
     call is_set_type_coreimpurediag_sum_energy(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_coreimpurediag_sum

   subroutine is_set_arr_type_coreimpurediag_sum(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediag_sum), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediag_sum(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediag_sum

   subroutine is_set_type_coreimpurediag_sum_energy(structure_in, name)

     implicit none

     type (type_coreimpurediag_sum_energy), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreimpurediagsum_type(structure_in%ionization, trim(adjustl(name)) // '%ionization')
     call is_set_type_coreimpurediagsum_type(structure_in%recombin, trim(adjustl(name)) // '%recombin')
     call is_set_type_coreimpurediagsum_type(structure_in%sum, trim(adjustl(name)) // '%sum')

   end subroutine is_set_type_coreimpurediag_sum_energy

   subroutine is_set_arr_type_coreimpurediag_sum_energy(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediag_sum_energy), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediag_sum_energy(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediag_sum_energy

   subroutine is_set_type_coreimpurediag_type(structure_in, name)

     implicit none

     type (type_coreimpurediag_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreimpurediag_radiation(structure_in%radiation, trim(adjustl(name)) // '%radiation')
     call is_set_type_coreimpurediag_energy(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_coreimpurediag_type

   subroutine is_set_arr_type_coreimpurediag_type(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediag_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediag_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediag_type

   subroutine is_set_type_coreimpurediagprof_type(structure_in, name)

     implicit none

     type (type_coreimpurediagprof_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%profile, trim(adjustl(name)) // '%profile')
     call is_set_type_matflt_type(structure_in%integral, trim(adjustl(name)) // '%integral')

   end subroutine is_set_type_coreimpurediagprof_type

   subroutine is_set_arr_type_coreimpurediagprof_type(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediagprof_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediagprof_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediagprof_type

   subroutine is_set_type_coreimpurediagsum_type(structure_in, name)

     implicit none

     type (type_coreimpurediagsum_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%profile, trim(adjustl(name)) // '%profile')
     call is_set_type_vecflt_type(structure_in%integral, trim(adjustl(name)) // '%integral')

   end subroutine is_set_type_coreimpurediagsum_type

   subroutine is_set_arr_type_coreimpurediagsum_type(structure_in, name)
 
     implicit none
 
     type (type_coreimpurediagsum_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreimpurediagsum_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreimpurediagsum_type

   subroutine is_set_type_coreneutrals_atomlist(structure_in, name)

     implicit none

     type (type_coreneutrals_atomlist), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_float(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_identifier(structure_in%ionimptype, trim(adjustl(name)) // '%ionimptype')
     call is_set_type_integer(structure_in%ionimpindex, trim(adjustl(name)) // '%ionimpindex')

   end subroutine is_set_type_coreneutrals_atomlist

   subroutine is_set_arr_type_coreneutrals_atomlist(structure_in, name)
 
     implicit none
 
     type (type_coreneutrals_atomlist), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreneutrals_atomlist(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreneutrals_atomlist

   subroutine is_set_type_coreneutrals_neutraltype(structure_in, name)

     implicit none

     type (type_coreneutrals_neutraltype), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_corefieldneutral(structure_in%n0, trim(adjustl(name)) // '%n0')
     call is_set_type_corefieldneutrale(structure_in%t0, trim(adjustl(name)) // '%t0')
     call is_set_type_corefieldneutralv0(structure_in%v0, trim(adjustl(name)) // '%v0')

   end subroutine is_set_type_coreneutrals_neutraltype

   subroutine is_set_arr_type_coreneutrals_neutraltype(structure_in, name)
 
     implicit none
 
     type (type_coreneutrals_neutraltype), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreneutrals_neutraltype(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreneutrals_neutraltype

   subroutine is_set_type_coreprofile(structure_in, name)

     implicit none

     type (type_coreprofile), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_coreprofile

   subroutine is_set_arr_type_coreprofile(structure_in, name)
 
     implicit none
 
     type (type_coreprofile), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreprofile(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreprofile

   subroutine is_set_type_coreprofion(structure_in, name)

     implicit none

     type (type_coreprofion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_coreprofion

   subroutine is_set_arr_type_coreprofion(structure_in, name)
 
     implicit none
 
     type (type_coreprofion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coreprofion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coreprofion

   subroutine is_set_type_coresource_values(structure_in, name)

     implicit none

     type (type_coresource_values), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%sourceid, trim(adjustl(name)) // '%sourceid')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%j, trim(adjustl(name)) // '%j')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_source_ion(structure_in%si, trim(adjustl(name)) // '%si')
     call is_set_type_source_vec(structure_in%se, trim(adjustl(name)) // '%se')
     call is_set_arr_type_source_imp(structure_in%sz, trim(adjustl(name)) // '%sz')
     call is_set_type_source_ion(structure_in%qi, trim(adjustl(name)) // '%qi')
     call is_set_type_source_vec(structure_in%qe, trim(adjustl(name)) // '%qe')
     call is_set_arr_type_source_imp(structure_in%qz, trim(adjustl(name)) // '%qz')
     call is_set_type_source_ion(structure_in%ui, trim(adjustl(name)) // '%ui')
     call is_set_type_source_vec(structure_in%ujxb, trim(adjustl(name)) // '%ujxb')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_coresource_values

   subroutine is_set_arr_type_coresource_values(structure_in, name)
 
     implicit none
 
     type (type_coresource_values), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coresource_values(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coresource_values

   subroutine is_set_type_coretransel(structure_in, name)

     implicit none

     type (type_coretransel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%diff, trim(adjustl(name)) // '%diff')
     call is_set_type_vecflt_type(structure_in%vconv, trim(adjustl(name)) // '%vconv')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_coretransel

   subroutine is_set_arr_type_coretransel(structure_in, name)
 
     implicit none
 
     type (type_coretransel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coretransel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coretransel

   subroutine is_set_type_coretransimp(structure_in, name)

     implicit none

     type (type_coretransimp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%diff, trim(adjustl(name)) // '%diff')
     call is_set_type_matflt_type(structure_in%vconv, trim(adjustl(name)) // '%vconv')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_coretransimp

   subroutine is_set_arr_type_coretransimp(structure_in, name)
 
     implicit none
 
     type (type_coretransimp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coretransimp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coretransimp

   subroutine is_set_type_coretransion(structure_in, name)

     implicit none

     type (type_coretransion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%diff, trim(adjustl(name)) // '%diff')
     call is_set_type_matflt_type(structure_in%vconv, trim(adjustl(name)) // '%vconv')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_coretransion

   subroutine is_set_arr_type_coretransion(structure_in, name)
 
     implicit none
 
     type (type_coretransion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coretransion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coretransion

   subroutine is_set_type_coretransp_values(structure_in, name)

     implicit none

     type (type_coretransp_values), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%transportid, trim(adjustl(name)) // '%transportid')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_ni_transp(structure_in%ni_transp, trim(adjustl(name)) // '%ni_transp')
     call is_set_type_ne_transp(structure_in%ne_transp, trim(adjustl(name)) // '%ne_transp')
     call is_set_arr_type_transcoefimp(structure_in%nz_transp, trim(adjustl(name)) // '%nz_transp')
     call is_set_type_transcoefion(structure_in%ti_transp, trim(adjustl(name)) // '%ti_transp')
     call is_set_type_transcoefel(structure_in%te_transp, trim(adjustl(name)) // '%te_transp')
     call is_set_arr_type_transcoefimp(structure_in%tz_transp, trim(adjustl(name)) // '%tz_transp')
     call is_set_type_transcoefvtor(structure_in%vtor_transp, trim(adjustl(name)) // '%vtor_transp')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_coretransp_values

   subroutine is_set_arr_type_coretransp_values(structure_in, name)
 
     implicit none
 
     type (type_coretransp_values), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_coretransp_values(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_coretransp_values

   subroutine is_set_type_current(structure_in, name)

     implicit none

     type (type_current), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%mpol, trim(adjustl(name)) // '%mpol')
     call is_set_type_vecint_type(structure_in%ntor, trim(adjustl(name)) // '%ntor')
     call is_set_type_exp1D(structure_in%spectrum, trim(adjustl(name)) // '%spectrum')
     call is_set_type_rz0D(structure_in%rz_reference, trim(adjustl(name)) // '%rz_reference')

   end subroutine is_set_type_current

   subroutine is_set_arr_type_current(structure_in, name)
 
     implicit none
 
     type (type_current), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_current(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_current

   subroutine is_set_type_cxmeasure(structure_in, name)

     implicit none

     type (type_cxmeasure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_exp1D(structure_in%vtor, trim(adjustl(name)) // '%vtor')
     call is_set_type_exp1D(structure_in%vpol, trim(adjustl(name)) // '%vpol')

   end subroutine is_set_type_cxmeasure

   subroutine is_set_arr_type_cxmeasure(structure_in, name)
 
     implicit none
 
     type (type_cxmeasure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_cxmeasure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_cxmeasure

   subroutine is_set_type_cxsetup(structure_in, name)

     implicit none

     type (type_cxsetup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_vecflt_type(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_rzphi1Dexp(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_cxsetup

   subroutine is_set_arr_type_cxsetup(structure_in, name)
 
     implicit none
 
     type (type_cxsetup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_cxsetup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_cxsetup

   subroutine is_set_type_data_release(structure_in, name)

     implicit none

     type (type_data_release), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%shot, trim(adjustl(name)) // '%shot')
     call is_set_type_integer(structure_in%run, trim(adjustl(name)) // '%run')
     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')

   end subroutine is_set_type_data_release

   subroutine is_set_arr_type_data_release(structure_in, name)
 
     implicit none
 
     type (type_data_release), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_data_release(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_data_release

   subroutine is_set_type_datainfo(structure_in, name)

     implicit none

     type (type_datainfo), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%dataprovider, trim(adjustl(name)) // '%dataprovider')
     call is_set_type_vecstring_type(structure_in%putdate, trim(adjustl(name)) // '%putdate')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecstring_type(structure_in%comment, trim(adjustl(name)) // '%comment')
     call is_set_type_integer(structure_in%cocos, trim(adjustl(name)) // '%cocos')
     call is_set_type_integer(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_integer(structure_in%isref, trim(adjustl(name)) // '%isref')
     call is_set_type_whatref(structure_in%whatref, trim(adjustl(name)) // '%whatref')
     call is_set_type_putinfo(structure_in%putinfo, trim(adjustl(name)) // '%putinfo')

   end subroutine is_set_type_datainfo

   subroutine is_set_arr_type_datainfo(structure_in, name)
 
     implicit none
 
     type (type_datainfo), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_datainfo(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_datainfo

   subroutine is_set_type_desc_coils(structure_in, name)

     implicit none

     type (type_desc_coils), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_float(structure_in%res, trim(adjustl(name)) // '%res')
     call is_set_type_integer(structure_in%nturns, trim(adjustl(name)) // '%nturns')
     call is_set_type_vecstring_type(structure_in%closed, trim(adjustl(name)) // '%closed')
     call is_set_arr_type_edges(structure_in%edges, trim(adjustl(name)) // '%edges')

   end subroutine is_set_type_desc_coils

   subroutine is_set_arr_type_desc_coils(structure_in, name)
 
     implicit none
 
     type (type_desc_coils), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_desc_coils(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_desc_coils

   subroutine is_set_type_desc_impur(structure_in, name)

     implicit none

     type (type_desc_impur), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_vecint_type(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_vecint_type(structure_in%i_ion, trim(adjustl(name)) // '%i_ion')
     call is_set_type_vecint_type(structure_in%nzimp, trim(adjustl(name)) // '%nzimp')
     call is_set_type_matint_type(structure_in%zmin, trim(adjustl(name)) // '%zmin')
     call is_set_type_matint_type(structure_in%zmax, trim(adjustl(name)) // '%zmax')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_desc_impur

   subroutine is_set_arr_type_desc_impur(structure_in, name)
 
     implicit none
 
     type (type_desc_impur), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_desc_impur(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_desc_impur

   subroutine is_set_type_desc_iron(structure_in, name)

     implicit none

     type (type_desc_iron), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_permeability(structure_in%permeability, trim(adjustl(name)) // '%permeability')
     call is_set_type_geom_iron(structure_in%geom_iron, trim(adjustl(name)) // '%geom_iron')

   end subroutine is_set_type_desc_iron

   subroutine is_set_arr_type_desc_iron(structure_in, name)
 
     implicit none
 
     type (type_desc_iron), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_desc_iron(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_desc_iron

   subroutine is_set_type_desc_pfcoils(structure_in, name)

     implicit none

     type (type_desc_pfcoils), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_vecflt_type(structure_in%res, trim(adjustl(name)) // '%res')
     call is_set_type_vecflt_type(structure_in%emax, trim(adjustl(name)) // '%emax')
     call is_set_type_structure_cs(structure_in%structure_cs, trim(adjustl(name)) // '%structure_cs')
     call is_set_type_float(structure_in%pol_flux_cs, trim(adjustl(name)) // '%pol_flux_cs')
     call is_set_type_vecint_type(structure_in%nelement, trim(adjustl(name)) // '%nelement')
     call is_set_type_pfelement(structure_in%pfelement, trim(adjustl(name)) // '%pfelement')

   end subroutine is_set_type_desc_pfcoils

   subroutine is_set_arr_type_desc_pfcoils(structure_in, name)
 
     implicit none
 
     type (type_desc_pfcoils), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_desc_pfcoils(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_desc_pfcoils

   subroutine is_set_type_desc_supply(structure_in, name)

     implicit none

     type (type_desc_supply), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_vecstring_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%delay, trim(adjustl(name)) // '%delay')
     call is_set_type_filter(structure_in%filter, trim(adjustl(name)) // '%filter')
     call is_set_type_vecflt_type(structure_in%imin, trim(adjustl(name)) // '%imin')
     call is_set_type_vecflt_type(structure_in%imax, trim(adjustl(name)) // '%imax')
     call is_set_type_vecflt_type(structure_in%res, trim(adjustl(name)) // '%res')
     call is_set_type_vecflt_type(structure_in%umin, trim(adjustl(name)) // '%umin')
     call is_set_type_vecflt_type(structure_in%umax, trim(adjustl(name)) // '%umax')
     call is_set_type_vecflt_type(structure_in%emax, trim(adjustl(name)) // '%emax')

   end subroutine is_set_type_desc_supply

   subroutine is_set_arr_type_desc_supply(structure_in, name)
 
     implicit none
 
     type (type_desc_supply), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_desc_supply(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_desc_supply

   subroutine is_set_type_diag_func(structure_in, name)

     implicit none

     type (type_diag_func), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')
     call is_set_type_matflt_type(structure_in%transf_mat, trim(adjustl(name)) // '%transf_mat')

   end subroutine is_set_type_diag_func

   subroutine is_set_arr_type_diag_func(structure_in, name)
 
     implicit none
 
     type (type_diag_func), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_diag_func(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_diag_func

   subroutine is_set_type_dist_collisional_transfer_0d(structure_in, name)

     implicit none

     type (type_dist_collisional_transfer_0d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%power_th, trim(adjustl(name)) // '%power_th')
     call is_set_type_float(structure_in%power_fast, trim(adjustl(name)) // '%power_fast')
     call is_set_type_float(structure_in%torque_th, trim(adjustl(name)) // '%torque_th')
     call is_set_type_float(structure_in%torque_fast, trim(adjustl(name)) // '%torque_fast')

   end subroutine is_set_type_dist_collisional_transfer_0d

   subroutine is_set_arr_type_dist_collisional_transfer_0d(structure_in, name)
 
     implicit none
 
     type (type_dist_collisional_transfer_0d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_collisional_transfer_0d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_collisional_transfer_0d

   subroutine is_set_type_dist_collisional_transfer_1d(structure_in, name)

     implicit none

     type (type_dist_collisional_transfer_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%power_th, trim(adjustl(name)) // '%power_th')
     call is_set_type_vecflt_type(structure_in%power_fast, trim(adjustl(name)) // '%power_fast')
     call is_set_type_vecflt_type(structure_in%torque_th, trim(adjustl(name)) // '%torque_th')
     call is_set_type_vecflt_type(structure_in%torque_fast, trim(adjustl(name)) // '%torque_fast')

   end subroutine is_set_type_dist_collisional_transfer_1d

   subroutine is_set_arr_type_dist_collisional_transfer_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_collisional_transfer_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_collisional_transfer_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_collisional_transfer_1d

   subroutine is_set_type_dist_collisional_transfer_2d(structure_in, name)

     implicit none

     type (type_dist_collisional_transfer_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%power_th, trim(adjustl(name)) // '%power_th')
     call is_set_type_matflt_type(structure_in%power_fast, trim(adjustl(name)) // '%power_fast')
     call is_set_type_matflt_type(structure_in%torque_th, trim(adjustl(name)) // '%torque_th')
     call is_set_type_matflt_type(structure_in%torque_fast, trim(adjustl(name)) // '%torque_fast')

   end subroutine is_set_type_dist_collisional_transfer_2d

   subroutine is_set_arr_type_dist_collisional_transfer_2d(structure_in, name)
 
     implicit none
 
     type (type_dist_collisional_transfer_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_collisional_transfer_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_collisional_transfer_2d

   subroutine is_set_type_dist_distrivec_distfunc_fexp_param(structure_in, name)

     implicit none

     type (type_dist_distrivec_distfunc_fexp_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_equatorial_plane(structure_in%equatorial, trim(adjustl(name)) // '%equatorial')
     call is_set_type_vecflt_type(structure_in%temperature, trim(adjustl(name)) // '%temperature')

   end subroutine is_set_type_dist_distrivec_distfunc_fexp_param

   subroutine is_set_arr_type_dist_distrivec_distfunc_fexp_param(structure_in, name)
 
     implicit none
 
     type (type_dist_distrivec_distfunc_fexp_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_distrivec_distfunc_fexp_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_distrivec_distfunc_fexp_param

   subroutine is_set_type_dist_ff(structure_in, name)

     implicit none

     type (type_dist_ff), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_grid_info(structure_in%grid_info, trim(adjustl(name)) // '%grid_info')
     call is_set_arr_type_topo_regions(structure_in%topo_regions, trim(adjustl(name)) // '%topo_regions')

   end subroutine is_set_type_dist_ff

   subroutine is_set_arr_type_dist_ff(structure_in, name)
 
     implicit none
 
     type (type_dist_ff), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_ff(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_ff

   subroutine is_set_type_dist_func(structure_in, name)

     implicit none

     type (type_dist_func), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%is_delta_f, trim(adjustl(name)) // '%is_delta_f')
     call is_set_type_weighted_markers(structure_in%markers, trim(adjustl(name)) // '%markers')
     call is_set_arr_type_dist_ff(structure_in%f_expan_topo, trim(adjustl(name)) // '%f_expan_topo')
     call is_set_arr_type_f_expansion(structure_in%f_expansion, trim(adjustl(name)) // '%f_expansion')

   end subroutine is_set_type_dist_func

   subroutine is_set_arr_type_dist_func(structure_in, name)
 
     implicit none
 
     type (type_dist_func), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_func(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_func

   subroutine is_set_type_dist_geometry_0d(structure_in, name)

     implicit none

     type (type_dist_geometry_0d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz0D(structure_in%mag_axis, trim(adjustl(name)) // '%mag_axis')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')

   end subroutine is_set_type_dist_geometry_0d

   subroutine is_set_arr_type_dist_geometry_0d(structure_in, name)
 
     implicit none
 
     type (type_dist_geometry_0d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_geometry_0d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_geometry_0d

   subroutine is_set_type_dist_geometry_1d(structure_in, name)

     implicit none

     type (type_dist_geometry_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')

   end subroutine is_set_type_dist_geometry_1d

   subroutine is_set_arr_type_dist_geometry_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_geometry_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_geometry_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_geometry_1d

   subroutine is_set_type_dist_geometry_2d(structure_in, name)

     implicit none

     type (type_dist_geometry_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%coord_type, trim(adjustl(name)) // '%coord_type')
     call is_set_type_matflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_matflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_matflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_matflt_type(structure_in%theta_geom, trim(adjustl(name)) // '%theta_geom')
     call is_set_type_matflt_type(structure_in%theta_strt, trim(adjustl(name)) // '%theta_strt')

   end subroutine is_set_type_dist_geometry_2d

   subroutine is_set_arr_type_dist_geometry_2d(structure_in, name)
 
     implicit none
 
     type (type_dist_geometry_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_geometry_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_geometry_2d

   subroutine is_set_type_dist_global_param(structure_in, name)

     implicit none

     type (type_dist_global_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_geometry_0d(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_dist_state_0d(structure_in%state, trim(adjustl(name)) // '%state')
     call is_set_type_dist_collisional_transfer_0d(structure_in%collisions_e, trim(adjustl(name)) // '%collisions_e')
     call is_set_arr_type_dist_collisional_transfer_0d(structure_in%collisions_i, trim(adjustl(name)) // '%collisions_i')
     call is_set_arr_type_dist_global_param_collisions_z(structure_in%collisions_z, trim(adjustl(name)) // '%collisions_z')
     call is_set_arr_type_dist_sources_0d(structure_in%sources, trim(adjustl(name)) // '%sources')

   end subroutine is_set_type_dist_global_param

   subroutine is_set_arr_type_dist_global_param(structure_in, name)
 
     implicit none
 
     type (type_dist_global_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_global_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_global_param

   subroutine is_set_type_dist_global_param_collisions_z(structure_in, name)

     implicit none

     type (type_dist_global_param_collisions_z), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_dist_collisional_transfer_0d(structure_in%charge_state, trim(adjustl(name)) // '%charge_state')

   end subroutine is_set_type_dist_global_param_collisions_z

   subroutine is_set_arr_type_dist_global_param_collisions_z(structure_in, name)
 
     implicit none
 
     type (type_dist_global_param_collisions_z), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_global_param_collisions_z(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_global_param_collisions_z

   subroutine is_set_type_dist_grid_info(structure_in, name)

     implicit none

     type (type_dist_grid_info), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%grid_type, trim(adjustl(name)) // '%grid_type')
     call is_set_type_integer(structure_in%ngriddim, trim(adjustl(name)) // '%ngriddim')
     call is_set_type_vecint_type(structure_in%grid_coord, trim(adjustl(name)) // '%grid_coord')
     call is_set_type_integer(structure_in%thin_orbits, trim(adjustl(name)) // '%thin_orbits')
     call is_set_type_vecstring_type(structure_in%topology, trim(adjustl(name)) // '%topology')
     call is_set_arr_type_omnigen_surf(structure_in%omnigen_surf, trim(adjustl(name)) // '%omnigen_surf')

   end subroutine is_set_type_dist_grid_info

   subroutine is_set_arr_type_dist_grid_info(structure_in, name)
 
     implicit none
 
     type (type_dist_grid_info), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_grid_info(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_grid_info

   subroutine is_set_type_dist_profile_values_1d(structure_in, name)

     implicit none

     type (type_dist_profile_values_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_state_1d(structure_in%state, trim(adjustl(name)) // '%state')
     call is_set_type_dist_collisional_transfer_1d(structure_in%collisions_e, trim(adjustl(name)) // '%collisions_e')
     call is_set_arr_type_dist_collisional_transfer_1d(structure_in%collisions_i, trim(adjustl(name)) // '%collisions_i')
     call is_set_arr_type_dist_profiles_1d_collisions_z(structure_in%collisions_z, trim(adjustl(name)) // '%collisions_z')
     call is_set_arr_type_dist_sources_1d(structure_in%sources, trim(adjustl(name)) // '%sources')

   end subroutine is_set_type_dist_profile_values_1d

   subroutine is_set_arr_type_dist_profile_values_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_profile_values_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_profile_values_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_profile_values_1d

   subroutine is_set_type_dist_profile_values_2d(structure_in, name)

     implicit none

     type (type_dist_profile_values_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_state_2d(structure_in%state, trim(adjustl(name)) // '%state')
     call is_set_type_dist_collisional_transfer_2d(structure_in%collisions_e, trim(adjustl(name)) // '%collisions_e')
     call is_set_arr_type_dist_collisional_transfer_2d(structure_in%collisions_i, trim(adjustl(name)) // '%collisions_i')
     call is_set_arr_type_dist_profiles2d_collisions_z(structure_in%collisions_z, trim(adjustl(name)) // '%collisions_z')

   end subroutine is_set_type_dist_profile_values_2d

   subroutine is_set_arr_type_dist_profile_values_2d(structure_in, name)
 
     implicit none
 
     type (type_dist_profile_values_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_profile_values_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_profile_values_2d

   subroutine is_set_type_dist_profiles2d_collisions_z(structure_in, name)

     implicit none

     type (type_dist_profiles2d_collisions_z), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_dist_collisional_transfer_2d(structure_in%charge_state, trim(adjustl(name)) // '%charge_state')

   end subroutine is_set_type_dist_profiles2d_collisions_z

   subroutine is_set_arr_type_dist_profiles2d_collisions_z(structure_in, name)
 
     implicit none
 
     type (type_dist_profiles2d_collisions_z), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_profiles2d_collisions_z(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_profiles2d_collisions_z

   subroutine is_set_type_dist_profiles_1d(structure_in, name)

     implicit none

     type (type_dist_profiles_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_geometry_1d(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_dist_state_1d(structure_in%state, trim(adjustl(name)) // '%state')
     call is_set_type_dist_collisional_transfer_1d(structure_in%collisions_e, trim(adjustl(name)) // '%collisions_e')
     call is_set_arr_type_dist_collisional_transfer_1d(structure_in%collisions_i, trim(adjustl(name)) // '%collisions_i')
     call is_set_arr_type_dist_profiles_1d_collisions_z(structure_in%collisions_z, trim(adjustl(name)) // '%collisions_z')
     call is_set_type_dist_thermalised_1d(structure_in%thermalised, trim(adjustl(name)) // '%thermalised')
     call is_set_arr_type_dist_sources_1d(structure_in%sources, trim(adjustl(name)) // '%sources')
     call is_set_type_dist_profile_values_1d(structure_in%trapped, trim(adjustl(name)) // '%trapped')
     call is_set_type_dist_profile_values_1d(structure_in%co_passing, trim(adjustl(name)) // '%co_passing')
     call is_set_type_dist_profile_values_1d(structure_in%cntr_passing, trim(adjustl(name)) // '%cntr_passing')

   end subroutine is_set_type_dist_profiles_1d

   subroutine is_set_arr_type_dist_profiles_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_profiles_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_profiles_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_profiles_1d

   subroutine is_set_type_dist_profiles_1d_collisions_z(structure_in, name)

     implicit none

     type (type_dist_profiles_1d_collisions_z), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_dist_collisional_transfer_1d(structure_in%charge_state, trim(adjustl(name)) // '%charge_state')

   end subroutine is_set_type_dist_profiles_1d_collisions_z

   subroutine is_set_arr_type_dist_profiles_1d_collisions_z(structure_in, name)
 
     implicit none
 
     type (type_dist_profiles_1d_collisions_z), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_profiles_1d_collisions_z(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_profiles_1d_collisions_z

   subroutine is_set_type_dist_profiles_2d(structure_in, name)

     implicit none

     type (type_dist_profiles_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_geometry_2d(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_dist_state_2d(structure_in%state, trim(adjustl(name)) // '%state')
     call is_set_type_dist_collisional_transfer_2d(structure_in%collisions_e, trim(adjustl(name)) // '%collisions_e')
     call is_set_arr_type_dist_collisional_transfer_2d(structure_in%collisions_i, trim(adjustl(name)) // '%collisions_i')
     call is_set_arr_type_dist_profiles2d_collisions_z(structure_in%collisions_z, trim(adjustl(name)) // '%collisions_z')
     call is_set_type_dist_profile_values_2d(structure_in%trapped, trim(adjustl(name)) // '%trapped')
     call is_set_type_dist_profile_values_2d(structure_in%co_passing, trim(adjustl(name)) // '%co_passing')
     call is_set_type_dist_profile_values_2d(structure_in%cntr_passing, trim(adjustl(name)) // '%cntr_passing')

   end subroutine is_set_type_dist_profiles_2d

   subroutine is_set_arr_type_dist_profiles_2d(structure_in, name)
 
     implicit none
 
     type (type_dist_profiles_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_profiles_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_profiles_2d

   subroutine is_set_type_dist_sources_0d(structure_in, name)

     implicit none

     type (type_dist_sources_0d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_sources_reference(structure_in%source_ref, trim(adjustl(name)) // '%source_ref')
     call is_set_type_float(structure_in%particle, trim(adjustl(name)) // '%particle')
     call is_set_type_float(structure_in%momentum, trim(adjustl(name)) // '%momentum')
     call is_set_type_float(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_dist_sources_0d

   subroutine is_set_arr_type_dist_sources_0d(structure_in, name)
 
     implicit none
 
     type (type_dist_sources_0d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_sources_0d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_sources_0d

   subroutine is_set_type_dist_sources_1d(structure_in, name)

     implicit none

     type (type_dist_sources_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_dist_sources_reference(structure_in%source_ref, trim(adjustl(name)) // '%source_ref')
     call is_set_type_vecflt_type(structure_in%particle, trim(adjustl(name)) // '%particle')
     call is_set_type_vecflt_type(structure_in%momentum, trim(adjustl(name)) // '%momentum')
     call is_set_type_vecflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_dist_sources_1d

   subroutine is_set_arr_type_dist_sources_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_sources_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_sources_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_sources_1d

   subroutine is_set_type_dist_sources_reference(structure_in, name)

     implicit none

     type (type_dist_sources_reference), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecint_type(structure_in%index_waveid, trim(adjustl(name)) // '%index_waveid')
     call is_set_type_vecint_type(structure_in%index_srcid, trim(adjustl(name)) // '%index_srcid')

   end subroutine is_set_type_dist_sources_reference

   subroutine is_set_arr_type_dist_sources_reference(structure_in, name)
 
     implicit none
 
     type (type_dist_sources_reference), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_sources_reference(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_sources_reference

   subroutine is_set_type_dist_state_0d(structure_in, name)

     implicit none

     type (type_dist_state_0d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%n_particles, trim(adjustl(name)) // '%n_particles')
     call is_set_type_float(structure_in%n_part_fast, trim(adjustl(name)) // '%n_part_fast')
     call is_set_type_float(structure_in%enrg, trim(adjustl(name)) // '%enrg')
     call is_set_type_float(structure_in%enrg_fast, trim(adjustl(name)) // '%enrg_fast')
     call is_set_type_float(structure_in%enrg_fast_pa, trim(adjustl(name)) // '%enrg_fast_pa')
     call is_set_type_float(structure_in%momentm_fast, trim(adjustl(name)) // '%momentm_fast')
     call is_set_type_float(structure_in%current_dr, trim(adjustl(name)) // '%current_dr')
     call is_set_type_float(structure_in%torque_jrxb, trim(adjustl(name)) // '%torque_jrxb')

   end subroutine is_set_type_dist_state_0d

   subroutine is_set_arr_type_dist_state_0d(structure_in, name)
 
     implicit none
 
     type (type_dist_state_0d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_state_0d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_state_0d

   subroutine is_set_type_dist_state_1d(structure_in, name)

     implicit none

     type (type_dist_state_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%dens, trim(adjustl(name)) // '%dens')
     call is_set_type_vecflt_type(structure_in%dens_fast, trim(adjustl(name)) // '%dens_fast')
     call is_set_type_vecflt_type(structure_in%pres, trim(adjustl(name)) // '%pres')
     call is_set_type_vecflt_type(structure_in%pres_fast, trim(adjustl(name)) // '%pres_fast')
     call is_set_type_vecflt_type(structure_in%pres_fast_pa, trim(adjustl(name)) // '%pres_fast_pa')
     call is_set_type_vecflt_type(structure_in%momentm_fast, trim(adjustl(name)) // '%momentm_fast')
     call is_set_type_vecflt_type(structure_in%current, trim(adjustl(name)) // '%current')
     call is_set_type_vecflt_type(structure_in%current_fast, trim(adjustl(name)) // '%current_fast')
     call is_set_type_vecflt_type(structure_in%torque_jrxb, trim(adjustl(name)) // '%torque_jrxb')

   end subroutine is_set_type_dist_state_1d

   subroutine is_set_arr_type_dist_state_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_state_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_state_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_state_1d

   subroutine is_set_type_dist_state_2d(structure_in, name)

     implicit none

     type (type_dist_state_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%dens, trim(adjustl(name)) // '%dens')
     call is_set_type_matflt_type(structure_in%dens_fast, trim(adjustl(name)) // '%dens_fast')
     call is_set_type_matflt_type(structure_in%pres, trim(adjustl(name)) // '%pres')
     call is_set_type_matflt_type(structure_in%pres_fast, trim(adjustl(name)) // '%pres_fast')
     call is_set_type_matflt_type(structure_in%pres_fast_pa, trim(adjustl(name)) // '%pres_fast_pa')
     call is_set_type_matflt_type(structure_in%momentm_fast, trim(adjustl(name)) // '%momentm_fast')
     call is_set_type_matflt_type(structure_in%current, trim(adjustl(name)) // '%current')
     call is_set_type_matflt_type(structure_in%current_fast, trim(adjustl(name)) // '%current_fast')
     call is_set_type_matflt_type(structure_in%torque_jrxb, trim(adjustl(name)) // '%torque_jrxb')

   end subroutine is_set_type_dist_state_2d

   subroutine is_set_arr_type_dist_state_2d(structure_in, name)
 
     implicit none
 
     type (type_dist_state_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_state_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_state_2d

   subroutine is_set_type_dist_thermalised_1d(structure_in, name)

     implicit none

     type (type_dist_thermalised_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%particle, trim(adjustl(name)) // '%particle')
     call is_set_type_vecflt_type(structure_in%momentum, trim(adjustl(name)) // '%momentum')
     call is_set_type_vecflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_dist_thermalised_1d

   subroutine is_set_arr_type_dist_thermalised_1d(structure_in, name)
 
     implicit none
 
     type (type_dist_thermalised_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_dist_thermalised_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_dist_thermalised_1d

   subroutine is_set_type_distri_vec(structure_in, name)

     implicit none

     type (type_distri_vec), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_enum_instance(structure_in%wave_id, trim(adjustl(name)) // '%wave_id')
     call is_set_arr_type_enum_instance(structure_in%source_id, trim(adjustl(name)) // '%source_id')
     call is_set_type_species_reference(structure_in%species, trim(adjustl(name)) // '%species')
     call is_set_type_integer(structure_in%gyro_type, trim(adjustl(name)) // '%gyro_type')
     call is_set_type_fast_thermal_separation_filter(structure_in%fast_filter, trim(adjustl(name)) // '%fast_filter')
     call is_set_type_dist_global_param(structure_in%global_param, trim(adjustl(name)) // '%global_param')
     call is_set_type_dist_profiles_1d(structure_in%profiles_1d, trim(adjustl(name)) // '%profiles_1d')
     call is_set_type_dist_profiles_2d(structure_in%profiles_2d, trim(adjustl(name)) // '%profiles_2d')
     call is_set_type_dist_func(structure_in%dist_func, trim(adjustl(name)) // '%dist_func')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_distri_vec

   subroutine is_set_arr_type_distri_vec(structure_in, name)
 
     implicit none
 
     type (type_distri_vec), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distri_vec(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distri_vec

   subroutine is_set_type_distsource_global_param(structure_in, name)

     implicit none

     type (type_distsource_global_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp0D(structure_in%src_pow, trim(adjustl(name)) // '%src_pow')
     call is_set_type_exp0D(structure_in%src_rate, trim(adjustl(name)) // '%src_rate')
     call is_set_type_rz0D(structure_in%mag_axis, trim(adjustl(name)) // '%mag_axis')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')

   end subroutine is_set_type_distsource_global_param

   subroutine is_set_arr_type_distsource_global_param(structure_in, name)
 
     implicit none
 
     type (type_distsource_global_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distsource_global_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distsource_global_param

   subroutine is_set_type_distsource_line_src_prof(structure_in, name)

     implicit none

     type (type_distsource_line_src_prof), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%R, trim(adjustl(name)) // '%R')
     call is_set_type_vecflt_type(structure_in%Z, trim(adjustl(name)) // '%Z')
     call is_set_type_vecflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')
     call is_set_type_vecflt_type(structure_in%theta_id, trim(adjustl(name)) // '%theta_id')
     call is_set_type_matflt_type(structure_in%th2th_pol, trim(adjustl(name)) // '%th2th_pol')
     call is_set_type_vecflt_type(structure_in%pitch, trim(adjustl(name)) // '%pitch')
     call is_set_type_vecflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_vecflt_type(structure_in%ang_momentum, trim(adjustl(name)) // '%ang_momentum')
     call is_set_type_vecflt_type(structure_in%src_rate, trim(adjustl(name)) // '%src_rate')

   end subroutine is_set_type_distsource_line_src_prof

   subroutine is_set_arr_type_distsource_line_src_prof(structure_in, name)
 
     implicit none
 
     type (type_distsource_line_src_prof), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distsource_line_src_prof(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distsource_line_src_prof

   subroutine is_set_type_distsource_profiles_1d(structure_in, name)

     implicit none

     type (type_distsource_profiles_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_exp1D(structure_in%pow_den, trim(adjustl(name)) // '%pow_den')
     call is_set_type_exp1D(structure_in%trq_den, trim(adjustl(name)) // '%trq_den')
     call is_set_type_exp1D(structure_in%src_rate, trim(adjustl(name)) // '%src_rate')

   end subroutine is_set_type_distsource_profiles_1d

   subroutine is_set_arr_type_distsource_profiles_1d(structure_in, name)
 
     implicit none
 
     type (type_distsource_profiles_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distsource_profiles_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distsource_profiles_1d

   subroutine is_set_type_distsource_profiles_2d(structure_in, name)

     implicit none

     type (type_distsource_profiles_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%grid_coord, trim(adjustl(name)) // '%grid_coord')
     call is_set_type_matflt_type(structure_in%dim1, trim(adjustl(name)) // '%dim1')
     call is_set_type_matflt_type(structure_in%dim2, trim(adjustl(name)) // '%dim2')
     call is_set_type_matflt_type(structure_in%g11, trim(adjustl(name)) // '%g11')
     call is_set_type_matflt_type(structure_in%g12, trim(adjustl(name)) // '%g12')
     call is_set_type_matflt_type(structure_in%g21, trim(adjustl(name)) // '%g21')
     call is_set_type_matflt_type(structure_in%g22, trim(adjustl(name)) // '%g22')
     call is_set_type_exp2D(structure_in%pow_den, trim(adjustl(name)) // '%pow_den')
     call is_set_type_exp2D(structure_in%src_rate, trim(adjustl(name)) // '%src_rate')

   end subroutine is_set_type_distsource_profiles_2d

   subroutine is_set_arr_type_distsource_profiles_2d(structure_in, name)
 
     implicit none
 
     type (type_distsource_profiles_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distsource_profiles_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distsource_profiles_2d

   subroutine is_set_type_distsource_source(structure_in, name)

     implicit none

     type (type_distsource_source), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_enum_instance(structure_in%source_id, trim(adjustl(name)) // '%source_id')
     call is_set_type_species_reference(structure_in%species, trim(adjustl(name)) // '%species')
     call is_set_type_integer(structure_in%gyro_type, trim(adjustl(name)) // '%gyro_type')
     call is_set_type_distsource_global_param(structure_in%global_param, trim(adjustl(name)) // '%global_param')
     call is_set_type_distsource_profiles_1d(structure_in%profiles_1d, trim(adjustl(name)) // '%profiles_1d')
     call is_set_type_distsource_profiles_2d(structure_in%profiles_2d, trim(adjustl(name)) // '%profiles_2d')
     call is_set_arr_type_distsource_line_src_prof(structure_in%line_srcprof, trim(adjustl(name)) // '%line_srcprof')
     call is_set_type_source_rate(structure_in%source_rate, trim(adjustl(name)) // '%source_rate')
     call is_set_type_weighted_markers(structure_in%markers, trim(adjustl(name)) // '%markers')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_distsource_source

   subroutine is_set_arr_type_distsource_source(structure_in, name)
 
     implicit none
 
     type (type_distsource_source), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_distsource_source(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_distsource_source

   subroutine is_set_type_divergence(structure_in, name)

     implicit none

     type (type_divergence), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%frac_divcomp, trim(adjustl(name)) // '%frac_divcomp')
     call is_set_type_vecflt_type(structure_in%div_vert, trim(adjustl(name)) // '%div_vert')
     call is_set_type_vecflt_type(structure_in%div_horiz, trim(adjustl(name)) // '%div_horiz')

   end subroutine is_set_type_divergence

   subroutine is_set_arr_type_divergence(structure_in, name)
 
     implicit none
 
     type (type_divergence), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_divergence(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_divergence

   subroutine is_set_type_e_components(structure_in, name)

     implicit none

     type (type_e_components), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_complexgrid_scalar_cplx(structure_in%e_plus, trim(adjustl(name)) // '%e_plus')
     call is_set_type_complexgrid_scalar_cplx(structure_in%e_minus, trim(adjustl(name)) // '%e_minus')
     call is_set_type_complexgrid_scalar_cplx(structure_in%e_para, trim(adjustl(name)) // '%e_para')
     call is_set_type_complexgrid_scalar_cplx(structure_in%e_norm, trim(adjustl(name)) // '%e_norm')
     call is_set_type_complexgrid_scalar_cplx(structure_in%e_binorm, trim(adjustl(name)) // '%e_binorm')
     call is_set_type_complexgrid_scalar_cplx(structure_in%b_norm, trim(adjustl(name)) // '%b_norm')
     call is_set_type_complexgrid_scalar_cplx(structure_in%b_binorm, trim(adjustl(name)) // '%b_binorm')
     call is_set_type_complexgrid_scalar_cplx(structure_in%b_para, trim(adjustl(name)) // '%b_para')
     call is_set_type_complexgrid_scalar_cplx(structure_in%k_perp, trim(adjustl(name)) // '%k_perp')

   end subroutine is_set_type_e_components

   subroutine is_set_arr_type_e_components(structure_in, name)
 
     implicit none
 
     type (type_e_components), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_e_components(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_e_components

   subroutine is_set_type_ecemeasure(structure_in, name)

     implicit none

     type (type_ecemeasure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%harmonic, trim(adjustl(name)) // '%harmonic')
     call is_set_type_rzphi1Dexp(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_exp1D(structure_in%te, trim(adjustl(name)) // '%te')

   end subroutine is_set_type_ecemeasure

   subroutine is_set_arr_type_ecemeasure(structure_in, name)
 
     implicit none
 
     type (type_ecemeasure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ecemeasure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ecemeasure

   subroutine is_set_type_ecesetup(structure_in, name)

     implicit none

     type (type_ecesetup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_setup_line_exp(structure_in%los, trim(adjustl(name)) // '%los')

   end subroutine is_set_type_ecesetup

   subroutine is_set_arr_type_ecesetup(structure_in, name)
 
     implicit none
 
     type (type_ecesetup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ecesetup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ecesetup

   subroutine is_set_type_edge_fluid(structure_in, name)

     implicit none

     type (type_edge_fluid), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_edge_fluid_scalar_simplestruct(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_arr_type_edge_fluid_scalar(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_edge_fluid_vector_simplestruct(structure_in%ve, trim(adjustl(name)) // '%ve')
     call is_set_arr_type_edge_fluid_vector(structure_in%vi, trim(adjustl(name)) // '%vi')
     call is_set_type_edge_fluid_scalar_simplestruct(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_arr_type_edge_fluid_scalar(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_edge_fluid_vector_simplestruct(structure_in%te_aniso, trim(adjustl(name)) // '%te_aniso')
     call is_set_arr_type_edge_fluid_vector(structure_in%ti_aniso, trim(adjustl(name)) // '%ti_aniso')
     call is_set_type_edge_fluid_scalar_simplestruct(structure_in%po, trim(adjustl(name)) // '%po')
     call is_set_type_edge_fluid_vector_simplestruct(structure_in%j, trim(adjustl(name)) // '%j')
     call is_set_arr_type_complexgrid_vector(structure_in%b, trim(adjustl(name)) // '%b')

   end subroutine is_set_type_edge_fluid

   subroutine is_set_arr_type_edge_fluid(structure_in, name)
 
     implicit none
 
     type (type_edge_fluid), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_fluid(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_fluid

   subroutine is_set_type_edge_fluid_scalar(structure_in, name)

     implicit none

     type (type_edge_fluid_scalar), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_complexgrid_scalar(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_arr_type_complexgrid_scalar(structure_in%bndvalue, trim(adjustl(name)) // '%bndvalue')
     call is_set_arr_type_complexgrid_vector(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_arr_type_complexgrid_vector(structure_in%bndflux, trim(adjustl(name)) // '%bndflux')
     call is_set_arr_type_edge_fluid_scalar_transpcoeff(structure_in%transpcoeff, trim(adjustl(name)) // '%transpcoeff')
     call is_set_arr_type_complexgrid_scalar(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_edge_fluid_scalar

   subroutine is_set_arr_type_edge_fluid_scalar(structure_in, name)
 
     implicit none
 
     type (type_edge_fluid_scalar), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_fluid_scalar(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_fluid_scalar

   subroutine is_set_type_edge_fluid_scalar_simplestruct(structure_in, name)

     implicit none

     type (type_edge_fluid_scalar_simplestruct), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_complexgrid_scalar(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_arr_type_complexgrid_scalar(structure_in%bndvalue, trim(adjustl(name)) // '%bndvalue')
     call is_set_arr_type_complexgrid_vector(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_arr_type_complexgrid_vector(structure_in%bndflux, trim(adjustl(name)) // '%bndflux')
     call is_set_arr_type_edge_fluid_scalar_transpcoeff(structure_in%transpcoeff, trim(adjustl(name)) // '%transpcoeff')
     call is_set_arr_type_complexgrid_scalar(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_edge_fluid_scalar_simplestruct

   subroutine is_set_arr_type_edge_fluid_scalar_simplestruct(structure_in, name)
 
     implicit none
 
     type (type_edge_fluid_scalar_simplestruct), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_fluid_scalar_simplestruct(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_fluid_scalar_simplestruct

   subroutine is_set_type_edge_fluid_scalar_transpcoeff(structure_in, name)

     implicit none

     type (type_edge_fluid_scalar_transpcoeff), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_complexgrid_vector_simplestruct(structure_in%d, trim(adjustl(name)) // '%d')
     call is_set_type_complexgrid_vector_simplestruct(structure_in%v, trim(adjustl(name)) // '%v')

   end subroutine is_set_type_edge_fluid_scalar_transpcoeff

   subroutine is_set_arr_type_edge_fluid_scalar_transpcoeff(structure_in, name)
 
     implicit none
 
     type (type_edge_fluid_scalar_transpcoeff), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_fluid_scalar_transpcoeff(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_fluid_scalar_transpcoeff

   subroutine is_set_type_edge_fluid_vector(structure_in, name)

     implicit none

     type (type_edge_fluid_vector), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%griduid, trim(adjustl(name)) // '%griduid')
     call is_set_type_integer(structure_in%basis, trim(adjustl(name)) // '%basis')
     call is_set_type_vecint_type(structure_in%align, trim(adjustl(name)) // '%align')
     call is_set_type_vecstring_type(structure_in%alignid, trim(adjustl(name)) // '%alignid')
     call is_set_arr_type_edge_fluid_scalar(structure_in%comps, trim(adjustl(name)) // '%comps')

   end subroutine is_set_type_edge_fluid_vector

   subroutine is_set_arr_type_edge_fluid_vector(structure_in, name)
 
     implicit none
 
     type (type_edge_fluid_vector), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_fluid_vector(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_fluid_vector

   subroutine is_set_type_edge_fluid_vector_simplestruct(structure_in, name)

     implicit none

     type (type_edge_fluid_vector_simplestruct), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%griduid, trim(adjustl(name)) // '%griduid')
     call is_set_type_integer(structure_in%basis, trim(adjustl(name)) // '%basis')
     call is_set_arr_type_edge_fluid_scalar(structure_in%comps, trim(adjustl(name)) // '%comps')
     call is_set_type_vecint_type(structure_in%align, trim(adjustl(name)) // '%align')
     call is_set_type_vecstring_type(structure_in%alignid, trim(adjustl(name)) // '%alignid')

   end subroutine is_set_type_edge_fluid_vector_simplestruct

   subroutine is_set_arr_type_edge_fluid_vector_simplestruct(structure_in, name)
 
     implicit none
 
     type (type_edge_fluid_vector_simplestruct), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_fluid_vector_simplestruct(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_fluid_vector_simplestruct

   subroutine is_set_type_edge_kinetic(structure_in, name)

     implicit none

     type (type_edge_kinetic), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_edge_kinetic_distribution(structure_in%f, trim(adjustl(name)) // '%f')

   end subroutine is_set_type_edge_kinetic

   subroutine is_set_arr_type_edge_kinetic(structure_in, name)
 
     implicit none
 
     type (type_edge_kinetic), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_kinetic(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_kinetic

   subroutine is_set_type_edge_kinetic_distribution(structure_in, name)

     implicit none

     type (type_edge_kinetic_distribution), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_complexgrid_scalar(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_arr_type_complexgrid_scalar(structure_in%bndvalue, trim(adjustl(name)) // '%bndvalue')
     call is_set_arr_type_complexgrid_vector(structure_in%fluxes, trim(adjustl(name)) // '%fluxes')
     call is_set_arr_type_complexgrid_scalar(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_edge_kinetic_distribution

   subroutine is_set_arr_type_edge_kinetic_distribution(structure_in, name)
 
     implicit none
 
     type (type_edge_kinetic_distribution), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edge_kinetic_distribution(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edge_kinetic_distribution

   subroutine is_set_type_edges(structure_in, name)

     implicit none

     type (type_edges), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1D(structure_in%edge_rzphi, trim(adjustl(name)) // '%edge_rzphi')

   end subroutine is_set_type_edges

   subroutine is_set_arr_type_edges(structure_in, name)
 
     implicit none
 
     type (type_edges), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edges(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edges

   subroutine is_set_type_edgespecies(structure_in, name)

     implicit none

     type (type_edgespecies), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nucindex, trim(adjustl(name)) // '%nucindex')
     call is_set_type_float(structure_in%zmin, trim(adjustl(name)) // '%zmin')
     call is_set_type_float(structure_in%zmax, trim(adjustl(name)) // '%zmax')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_edgespecies

   subroutine is_set_arr_type_edgespecies(structure_in, name)
 
     implicit none
 
     type (type_edgespecies), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_edgespecies(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_edgespecies

   subroutine is_set_type_element_desc(structure_in, name)

     implicit none

     type (type_element_desc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nucindex, trim(adjustl(name)) // '%nucindex')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_type_float(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')

   end subroutine is_set_type_element_desc

   subroutine is_set_arr_type_element_desc(structure_in, name)
 
     implicit none
 
     type (type_element_desc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_element_desc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_element_desc

   subroutine is_set_type_entry_def(structure_in, name)

     implicit none

     type (type_entry_def), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%user, trim(adjustl(name)) // '%user')
     call is_set_type_vecstring_type(structure_in%machine, trim(adjustl(name)) // '%machine')
     call is_set_type_integer(structure_in%shot, trim(adjustl(name)) // '%shot')
     call is_set_type_integer(structure_in%run, trim(adjustl(name)) // '%run')

   end subroutine is_set_type_entry_def

   subroutine is_set_arr_type_entry_def(structure_in, name)
 
     implicit none
 
     type (type_entry_def), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_entry_def(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_entry_def

   subroutine is_set_type_enum_instance(structure_in, name)

     implicit none

     type (type_enum_instance), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_integer(structure_in%index, trim(adjustl(name)) // '%index')

   end subroutine is_set_type_enum_instance

   subroutine is_set_arr_type_enum_instance(structure_in, name)
 
     implicit none
 
     type (type_enum_instance), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_enum_instance(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_enum_instance

   subroutine is_set_type_eqconstraint(structure_in, name)

     implicit none

     type (type_eqconstraint), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_eqmes1D(structure_in%bpol, trim(adjustl(name)) // '%bpol')
     call is_set_type_eqmes0D(structure_in%bvac_r, trim(adjustl(name)) // '%bvac_r')
     call is_set_type_eqmes0D(structure_in%diamagflux, trim(adjustl(name)) // '%diamagflux')
     call is_set_type_eqmes1D(structure_in%faraday, trim(adjustl(name)) // '%faraday')
     call is_set_type_eqmes1D(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_eqmes0D(structure_in%i_plasma, trim(adjustl(name)) // '%i_plasma')
     call is_set_type_isoflux(structure_in%isoflux, trim(adjustl(name)) // '%isoflux')
     call is_set_type_eqmes1D(structure_in%jsurf, trim(adjustl(name)) // '%jsurf')
     call is_set_type_magnet_iron(structure_in%magnet_iron, trim(adjustl(name)) // '%magnet_iron')
     call is_set_type_eqmes1D(structure_in%mse, trim(adjustl(name)) // '%mse')
     call is_set_type_eqmes1D(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_eqmes1D(structure_in%pfcurrent, trim(adjustl(name)) // '%pfcurrent')
     call is_set_type_eqmes1D(structure_in%pressure, trim(adjustl(name)) // '%pressure')
     call is_set_type_q(structure_in%q, trim(adjustl(name)) // '%q')
     call is_set_type_xpts(structure_in%xpts, trim(adjustl(name)) // '%xpts')

   end subroutine is_set_type_eqconstraint

   subroutine is_set_arr_type_eqconstraint(structure_in, name)
 
     implicit none
 
     type (type_eqconstraint), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_eqconstraint(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_eqconstraint

   subroutine is_set_type_eqgeometry(structure_in, name)

     implicit none

     type (type_eqgeometry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%boundarytype, trim(adjustl(name)) // '%boundarytype')
     call is_set_arr_type_rz1Dexp(structure_in%boundary, trim(adjustl(name)) // '%boundary')
     call is_set_type_rz0D(structure_in%geom_axis, trim(adjustl(name)) // '%geom_axis')
     call is_set_type_float(structure_in%a_minor, trim(adjustl(name)) // '%a_minor')
     call is_set_type_float(structure_in%elongation, trim(adjustl(name)) // '%elongation')
     call is_set_type_float(structure_in%elong_upper, trim(adjustl(name)) // '%elong_upper')
     call is_set_type_float(structure_in%elong_lower, trim(adjustl(name)) // '%elong_lower')
     call is_set_type_float(structure_in%tria_upper, trim(adjustl(name)) // '%tria_upper')
     call is_set_type_float(structure_in%tria_lower, trim(adjustl(name)) // '%tria_lower')
     call is_set_arr_type_rz1Dexp(structure_in%xpts, trim(adjustl(name)) // '%xpts')
     call is_set_type_rz0D(structure_in%left_low_st, trim(adjustl(name)) // '%left_low_st')
     call is_set_type_rz0D(structure_in%right_low_st, trim(adjustl(name)) // '%right_low_st')
     call is_set_type_rz0D(structure_in%left_up_st, trim(adjustl(name)) // '%left_up_st')
     call is_set_type_rz0D(structure_in%right_up_st, trim(adjustl(name)) // '%right_up_st')
     call is_set_type_rz0D(structure_in%active_limit, trim(adjustl(name)) // '%active_limit')
     call is_set_type_float(structure_in%ang_lcms_upo, trim(adjustl(name)) // '%ang_lcms_upo')
     call is_set_type_float(structure_in%ang_lcms_upi, trim(adjustl(name)) // '%ang_lcms_upi')
     call is_set_type_float(structure_in%ang_lcms_lwo, trim(adjustl(name)) // '%ang_lcms_lwo')
     call is_set_type_float(structure_in%ang_lcms_lwi, trim(adjustl(name)) // '%ang_lcms_lwi')

   end subroutine is_set_type_eqgeometry

   subroutine is_set_arr_type_eqgeometry(structure_in, name)
 
     implicit none
 
     type (type_eqgeometry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_eqgeometry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_eqgeometry

   subroutine is_set_type_eqmes0D(structure_in, name)

     implicit none

     type (type_eqmes0D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%measured, trim(adjustl(name)) // '%measured')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_integer(structure_in%exact, trim(adjustl(name)) // '%exact')
     call is_set_type_float(structure_in%weight, trim(adjustl(name)) // '%weight')
     call is_set_type_float(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_float(structure_in%calculated, trim(adjustl(name)) // '%calculated')
     call is_set_type_float(structure_in%chi2, trim(adjustl(name)) // '%chi2')

   end subroutine is_set_type_eqmes0D

   subroutine is_set_arr_type_eqmes0D(structure_in, name)
 
     implicit none
 
     type (type_eqmes0D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_eqmes0D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_eqmes0D

   subroutine is_set_type_eqmes1D(structure_in, name)

     implicit none

     type (type_eqmes1D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%measured, trim(adjustl(name)) // '%measured')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')
     call is_set_type_vecint_type(structure_in%exact, trim(adjustl(name)) // '%exact')
     call is_set_type_vecflt_type(structure_in%weight, trim(adjustl(name)) // '%weight')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_vecflt_type(structure_in%calculated, trim(adjustl(name)) // '%calculated')
     call is_set_type_vecflt_type(structure_in%chi2, trim(adjustl(name)) // '%chi2')

   end subroutine is_set_type_eqmes1D

   subroutine is_set_arr_type_eqmes1D(structure_in, name)
 
     implicit none
 
     type (type_eqmes1D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_eqmes1D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_eqmes1D

   subroutine is_set_type_equatorial_plane(structure_in, name)

     implicit none

     type (type_equatorial_plane), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_vecflt_type(structure_in%s, trim(adjustl(name)) // '%s')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%b_mod, trim(adjustl(name)) // '%b_mod')

   end subroutine is_set_type_equatorial_plane

   subroutine is_set_arr_type_equatorial_plane(structure_in, name)
 
     implicit none
 
     type (type_equatorial_plane), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_equatorial_plane(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_equatorial_plane

   subroutine is_set_type_equilibrium_profiles2d_grid(structure_in, name)

     implicit none

     type (type_equilibrium_profiles2d_grid), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%dim1, trim(adjustl(name)) // '%dim1')
     call is_set_type_vecflt_type(structure_in%dim2, trim(adjustl(name)) // '%dim2')
     call is_set_type_matint_type(structure_in%connect, trim(adjustl(name)) // '%connect')

   end subroutine is_set_type_equilibrium_profiles2d_grid

   subroutine is_set_arr_type_equilibrium_profiles2d_grid(structure_in, name)
 
     implicit none
 
     type (type_equilibrium_profiles2d_grid), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_equilibrium_profiles2d_grid(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_equilibrium_profiles2d_grid

   subroutine is_set_type_equilibrium_profiles_2d(structure_in, name)

     implicit none

     type (type_equilibrium_profiles_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%grid_type, trim(adjustl(name)) // '%grid_type')
     call is_set_type_equilibrium_profiles2d_grid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_type_matflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_matflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_matflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')
     call is_set_type_matflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_matflt_type(structure_in%jphi, trim(adjustl(name)) // '%jphi')
     call is_set_type_matflt_type(structure_in%jpar, trim(adjustl(name)) // '%jpar')
     call is_set_type_matflt_type(structure_in%br, trim(adjustl(name)) // '%br')
     call is_set_type_matflt_type(structure_in%bz, trim(adjustl(name)) // '%bz')
     call is_set_type_matflt_type(structure_in%bphi, trim(adjustl(name)) // '%bphi')
     call is_set_type_matflt_type(structure_in%vphi, trim(adjustl(name)) // '%vphi')
     call is_set_type_matflt_type(structure_in%vtheta, trim(adjustl(name)) // '%vtheta')
     call is_set_type_matflt_type(structure_in%rho_mass, trim(adjustl(name)) // '%rho_mass')
     call is_set_type_matflt_type(structure_in%pressure, trim(adjustl(name)) // '%pressure')
     call is_set_type_matflt_type(structure_in%temperature, trim(adjustl(name)) // '%temperature')

   end subroutine is_set_type_equilibrium_profiles_2d

   subroutine is_set_arr_type_equilibrium_profiles_2d(structure_in, name)
 
     implicit none
 
     type (type_equilibrium_profiles_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_equilibrium_profiles_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_equilibrium_profiles_2d

   subroutine is_set_type_exp0D(structure_in, name)

     implicit none

     type (type_exp0D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_float(structure_in%abserror, trim(adjustl(name)) // '%abserror')
     call is_set_type_float(structure_in%relerror, trim(adjustl(name)) // '%relerror')

   end subroutine is_set_type_exp0D

   subroutine is_set_arr_type_exp0D(structure_in, name)
 
     implicit none
 
     type (type_exp0D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_exp0D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_exp0D

   subroutine is_set_type_exp1D(structure_in, name)

     implicit none

     type (type_exp1D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%abserror, trim(adjustl(name)) // '%abserror')
     call is_set_type_vecflt_type(structure_in%relerror, trim(adjustl(name)) // '%relerror')

   end subroutine is_set_type_exp1D

   subroutine is_set_arr_type_exp1D(structure_in, name)
 
     implicit none
 
     type (type_exp1D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_exp1D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_exp1D

   subroutine is_set_type_exp2D(structure_in, name)

     implicit none

     type (type_exp2D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_matflt_type(structure_in%abserror, trim(adjustl(name)) // '%abserror')
     call is_set_type_matflt_type(structure_in%relerror, trim(adjustl(name)) // '%relerror')

   end subroutine is_set_type_exp2D

   subroutine is_set_arr_type_exp2D(structure_in, name)
 
     implicit none
 
     type (type_exp2D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_exp2D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_exp2D

   subroutine is_set_type_f_expansion(structure_in, name)

     implicit none

     type (type_f_expansion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_complexgrid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_type_complexgrid_scalar(structure_in%values, trim(adjustl(name)) // '%values')
     call is_set_type_dist_distrivec_distfunc_fexp_param(structure_in%parameters, trim(adjustl(name)) // '%parameters')

   end subroutine is_set_type_f_expansion

   subroutine is_set_arr_type_f_expansion(structure_in, name)
 
     implicit none
 
     type (type_f_expansion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_f_expansion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_f_expansion

   subroutine is_set_type_fast_thermal_separation_filter(structure_in, name)

     implicit none

     type (type_fast_thermal_separation_filter), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%method, trim(adjustl(name)) // '%method')
     call is_set_type_vecflt_type(structure_in%energy_sep, trim(adjustl(name)) // '%energy_sep')

   end subroutine is_set_type_fast_thermal_separation_filter

   subroutine is_set_arr_type_fast_thermal_separation_filter(structure_in, name)
 
     implicit none
 
     type (type_fast_thermal_separation_filter), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fast_thermal_separation_filter(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fast_thermal_separation_filter

   subroutine is_set_type_filter(structure_in, name)

     implicit none

     type (type_filter), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%num, trim(adjustl(name)) // '%num')
     call is_set_type_matflt_type(structure_in%den, trim(adjustl(name)) // '%den')

   end subroutine is_set_type_filter

   subroutine is_set_arr_type_filter(structure_in, name)
 
     implicit none
 
     type (type_filter), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_filter(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_filter

   subroutine is_set_type_flat_polygon(structure_in, name)

     implicit none

     type (type_flat_polygon), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_xyz0D(structure_in%origin, trim(adjustl(name)) // '%origin')
     call is_set_type_xyz0D(structure_in%basis1, trim(adjustl(name)) // '%basis1')
     call is_set_type_xyz0D(structure_in%basis2, trim(adjustl(name)) // '%basis2')
     call is_set_type_vecflt_type(structure_in%coord1, trim(adjustl(name)) // '%coord1')
     call is_set_type_vecflt_type(structure_in%coord2, trim(adjustl(name)) // '%coord2')

   end subroutine is_set_type_flat_polygon

   subroutine is_set_arr_type_flat_polygon(structure_in, name)
 
     implicit none
 
     type (type_flat_polygon), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_flat_polygon(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_flat_polygon

   subroutine is_set_type_flush(structure_in, name)

     implicit none

     type (type_flush), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_matflt_type(structure_in%coef, trim(adjustl(name)) // '%coef')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_flush

   subroutine is_set_arr_type_flush(structure_in, name)
 
     implicit none
 
     type (type_flush), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_flush(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_flush

   subroutine is_set_type_flux_loops(structure_in, name)

     implicit none

     type (type_flux_loops), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_setup_floops(structure_in%setup_floops, trim(adjustl(name)) // '%setup_floops')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_flux_loops

   subroutine is_set_arr_type_flux_loops(structure_in, name)
 
     implicit none
 
     type (type_flux_loops), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_flux_loops(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_flux_loops

   subroutine is_set_type_fluxel(structure_in, name)

     implicit none

     type (type_fluxel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%flux_dv, trim(adjustl(name)) // '%flux_dv')
     call is_set_type_vecflt_type(structure_in%flux_interp, trim(adjustl(name)) // '%flux_interp')

   end subroutine is_set_type_fluxel

   subroutine is_set_arr_type_fluxel(structure_in, name)
 
     implicit none
 
     type (type_fluxel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fluxel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fluxel

   subroutine is_set_type_fluximp(structure_in, name)

     implicit none

     type (type_fluximp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%flux_dv, trim(adjustl(name)) // '%flux_dv')
     call is_set_type_matflt_type(structure_in%flux_interp, trim(adjustl(name)) // '%flux_interp')

   end subroutine is_set_type_fluximp

   subroutine is_set_arr_type_fluximp(structure_in, name)
 
     implicit none
 
     type (type_fluximp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fluximp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fluximp

   subroutine is_set_type_fluxion(structure_in, name)

     implicit none

     type (type_fluxion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%flux_dv, trim(adjustl(name)) // '%flux_dv')
     call is_set_type_matflt_type(structure_in%flux_interp, trim(adjustl(name)) // '%flux_interp')

   end subroutine is_set_type_fluxion

   subroutine is_set_arr_type_fluxion(structure_in, name)
 
     implicit none
 
     type (type_fluxion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fluxion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fluxion

   subroutine is_set_type_focussing(structure_in, name)

     implicit none

     type (type_focussing), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%focal_len_hz, trim(adjustl(name)) // '%focal_len_hz')
     call is_set_type_float(structure_in%focal_len_vc, trim(adjustl(name)) // '%focal_len_vc')
     call is_set_type_float(structure_in%width_min_hz, trim(adjustl(name)) // '%width_min_hz')
     call is_set_type_float(structure_in%width_min_vc, trim(adjustl(name)) // '%width_min_vc')

   end subroutine is_set_type_focussing

   subroutine is_set_arr_type_focussing(structure_in, name)
 
     implicit none
 
     type (type_focussing), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_focussing(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_focussing

   subroutine is_set_type_fullwave(structure_in, name)

     implicit none

     type (type_fullwave), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_complexgrid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_type_e_components(structure_in%e_components, trim(adjustl(name)) // '%e_components')
     call is_set_type_pol_decomp(structure_in%pol_decomp, trim(adjustl(name)) // '%pol_decomp')
     call is_set_type_local(structure_in%local, trim(adjustl(name)) // '%local')

   end subroutine is_set_type_fullwave

   subroutine is_set_arr_type_fullwave(structure_in, name)
 
     implicit none
 
     type (type_fullwave), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fullwave(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fullwave

   subroutine is_set_type_fusiondiag_colli_3d(structure_in, name)

     implicit none

     type (type_fusiondiag_colli_3d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_arr_type_fusiondiag_voxels(structure_in%voxels, trim(adjustl(name)) // '%voxels')

   end subroutine is_set_type_fusiondiag_colli_3d

   subroutine is_set_arr_type_fusiondiag_colli_3d(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_colli_3d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_colli_3d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_colli_3d

   subroutine is_set_type_fusiondiag_colli_circ(structure_in, name)

     implicit none

     type (type_fusiondiag_colli_circ), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_setup_line(structure_in%setup_line, trim(adjustl(name)) // '%setup_line')
     call is_set_arr_type_fusiondiag_colliunit_circ(structure_in%colliunit, trim(adjustl(name)) // '%colliunit')

   end subroutine is_set_type_fusiondiag_colli_circ

   subroutine is_set_arr_type_fusiondiag_colli_circ(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_colli_circ), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_colli_circ(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_colli_circ

   subroutine is_set_type_fusiondiag_colli_poly(structure_in, name)

     implicit none

     type (type_fusiondiag_colli_poly), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_setup_line(structure_in%setup_line, trim(adjustl(name)) // '%setup_line')
     call is_set_arr_type_fusiondiag_colliunit_poly(structure_in%colliunit, trim(adjustl(name)) // '%colliunit')

   end subroutine is_set_type_fusiondiag_colli_poly

   subroutine is_set_arr_type_fusiondiag_colli_poly(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_colli_poly), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_colli_poly(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_colli_poly

   subroutine is_set_type_fusiondiag_collimator(structure_in, name)

     implicit none

     type (type_fusiondiag_collimator), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_fusiondiag_colli_circ(structure_in%colli_circ, trim(adjustl(name)) // '%colli_circ')
     call is_set_arr_type_fusiondiag_colli_poly(structure_in%colli_poly, trim(adjustl(name)) // '%colli_poly')
     call is_set_arr_type_fusiondiag_colli_3d(structure_in%colli_3d, trim(adjustl(name)) // '%colli_3d')

   end subroutine is_set_type_fusiondiag_collimator

   subroutine is_set_arr_type_fusiondiag_collimator(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_collimator), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_collimator(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_collimator

   subroutine is_set_type_fusiondiag_colliunit_circ(structure_in, name)

     implicit none

     type (type_fusiondiag_colliunit_circ), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%radius, trim(adjustl(name)) // '%radius')
     call is_set_type_rzphi1D(structure_in%centre, trim(adjustl(name)) // '%centre')

   end subroutine is_set_type_fusiondiag_colliunit_circ

   subroutine is_set_arr_type_fusiondiag_colliunit_circ(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_colliunit_circ), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_colliunit_circ(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_colliunit_circ

   subroutine is_set_type_fusiondiag_colliunit_poly(structure_in, name)

     implicit none

     type (type_fusiondiag_colliunit_poly), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%dimension, trim(adjustl(name)) // '%dimension')
     call is_set_type_rzphi2D(structure_in%nodes, trim(adjustl(name)) // '%nodes')

   end subroutine is_set_type_fusiondiag_colliunit_poly

   subroutine is_set_arr_type_fusiondiag_colliunit_poly(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_colliunit_poly), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_colliunit_poly(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_colliunit_poly

   subroutine is_set_type_fusiondiag_counts(structure_in, name)

     implicit none

     type (type_fusiondiag_counts), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%units, trim(adjustl(name)) // '%units')
     call is_set_arr_type_fusiondiag_ct_chords(structure_in%ct_chords, trim(adjustl(name)) // '%ct_chords')
     call is_set_arr_type_fusiondiag_ct_energy(structure_in%ct_energy, trim(adjustl(name)) // '%ct_energy')
     call is_set_arr_type_fusiondiag_detect_ct_energy(structure_in%detect_ct, trim(adjustl(name)) // '%detect_ct')

   end subroutine is_set_type_fusiondiag_counts

   subroutine is_set_arr_type_fusiondiag_counts(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_counts), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_counts(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_counts

   subroutine is_set_type_fusiondiag_ct_chords(structure_in, name)

     implicit none

     type (type_fusiondiag_ct_chords), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_exp0D(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_fusiondiag_ct_chords

   subroutine is_set_arr_type_fusiondiag_ct_chords(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_ct_chords), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_ct_chords(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_ct_chords

   subroutine is_set_type_fusiondiag_ct_energy(structure_in, name)

     implicit none

     type (type_fusiondiag_ct_energy), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_fusiondiag_ct_energy

   subroutine is_set_arr_type_fusiondiag_ct_energy(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_ct_energy), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_ct_energy(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_ct_energy

   subroutine is_set_type_fusiondiag_detect_ct_energy(structure_in, name)

     implicit none

     type (type_fusiondiag_detect_ct_energy), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_diag_func(structure_in%diag_func, trim(adjustl(name)) // '%diag_func')

   end subroutine is_set_type_fusiondiag_detect_ct_energy

   subroutine is_set_arr_type_fusiondiag_detect_ct_energy(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_detect_ct_energy), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_detect_ct_energy(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_detect_ct_energy

   subroutine is_set_type_fusiondiag_emissivity1d(structure_in, name)

     implicit none

     type (type_fusiondiag_emissivity1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%units, trim(adjustl(name)) // '%units')
     call is_set_type_exp1D(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_exp1D(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_arr_type_fusiondiag_spec1d(structure_in%spec1d, trim(adjustl(name)) // '%spec1d')

   end subroutine is_set_type_fusiondiag_emissivity1d

   subroutine is_set_arr_type_fusiondiag_emissivity1d(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_emissivity1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_emissivity1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_emissivity1d

   subroutine is_set_type_fusiondiag_emissivity2d(structure_in, name)

     implicit none

     type (type_fusiondiag_emissivity2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%units, trim(adjustl(name)) // '%units')
     call is_set_type_exp2D(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_exp2D(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_arr_type_fusiondiag_spec2d(structure_in%spec2d, trim(adjustl(name)) // '%spec2d')

   end subroutine is_set_type_fusiondiag_emissivity2d

   subroutine is_set_arr_type_fusiondiag_emissivity2d(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_emissivity2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_emissivity2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_emissivity2d

   subroutine is_set_type_fusiondiag_fus_product(structure_in, name)

     implicit none

     type (type_fusiondiag_fus_product), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%product, trim(adjustl(name)) // '%product')
     call is_set_type_vecstring_type(structure_in%reaction, trim(adjustl(name)) // '%reaction')
     call is_set_type_fusiondiag_collimator(structure_in%collimator, trim(adjustl(name)) // '%collimator')
     call is_set_type_fusiondiag_counts(structure_in%counts, trim(adjustl(name)) // '%counts')
     call is_set_type_fusiondiag_emissivity1d(structure_in%emissivity1d, trim(adjustl(name)) // '%emissivity1d')
     call is_set_type_fusiondiag_emissivity2d(structure_in%emissivity2d, trim(adjustl(name)) // '%emissivity2d')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_fusiondiag_fus_product

   subroutine is_set_arr_type_fusiondiag_fus_product(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_fus_product), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_fus_product(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_fus_product

   subroutine is_set_type_fusiondiag_spec1d(structure_in, name)

     implicit none

     type (type_fusiondiag_spec1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp0D(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_fusiondiag_spec1d

   subroutine is_set_arr_type_fusiondiag_spec1d(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_spec1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_spec1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_spec1d

   subroutine is_set_type_fusiondiag_spec2d(structure_in, name)

     implicit none

     type (type_fusiondiag_spec2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp0D(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_exp2D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_fusiondiag_spec2d

   subroutine is_set_arr_type_fusiondiag_spec2d(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_spec2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_spec2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_spec2d

   subroutine is_set_type_fusiondiag_voxels(structure_in, name)

     implicit none

     type (type_fusiondiag_voxels), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi0D(structure_in%centre, trim(adjustl(name)) // '%centre')
     call is_set_type_rzphi0D(structure_in%direction, trim(adjustl(name)) // '%direction')
     call is_set_type_float(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_float(structure_in%solid_angle, trim(adjustl(name)) // '%solid_angle')

   end subroutine is_set_type_fusiondiag_voxels

   subroutine is_set_arr_type_fusiondiag_voxels(structure_in, name)
 
     implicit none
 
     type (type_fusiondiag_voxels), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_fusiondiag_voxels(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_fusiondiag_voxels

   subroutine is_set_type_geom(structure_in, name)

     implicit none

     type (type_geom), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%dr_bb_sh_ib, trim(adjustl(name)) // '%dr_bb_sh_ib')
     call is_set_type_float(structure_in%dr_sh_vv_ib, trim(adjustl(name)) // '%dr_sh_vv_ib')
     call is_set_type_float(structure_in%dr_bb_sh_ob, trim(adjustl(name)) // '%dr_bb_sh_ob')
     call is_set_type_float(structure_in%dr_sh_vv_ob, trim(adjustl(name)) // '%dr_sh_vv_ob')
     call is_set_type_float(structure_in%dr_bb__sh_ib, trim(adjustl(name)) // '%dr_bb__sh_ib')
     call is_set_type_float(structure_in%dr_bb__sh_ob, trim(adjustl(name)) // '%dr_bb__sh_ob')
     call is_set_type_float(structure_in%delta_int, trim(adjustl(name)) // '%delta_int')

   end subroutine is_set_type_geom

   subroutine is_set_arr_type_geom(structure_in, name)
 
     implicit none
 
     type (type_geom), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_geom(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_geom

   subroutine is_set_type_geom_iron(structure_in, name)

     implicit none

     type (type_geom_iron), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%npoints, trim(adjustl(name)) // '%npoints')
     call is_set_type_rz2D(structure_in%rzcoordinate, trim(adjustl(name)) // '%rzcoordinate')

   end subroutine is_set_type_geom_iron

   subroutine is_set_arr_type_geom_iron(structure_in, name)
 
     implicit none
 
     type (type_geom_iron), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_geom_iron(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_geom_iron

   subroutine is_set_type_global_param(structure_in, name)

     implicit none

     type (type_global_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%beta_pol, trim(adjustl(name)) // '%beta_pol')
     call is_set_type_float(structure_in%beta_tor, trim(adjustl(name)) // '%beta_tor')
     call is_set_type_float(structure_in%beta_normal, trim(adjustl(name)) // '%beta_normal')
     call is_set_type_float(structure_in%i_plasma, trim(adjustl(name)) // '%i_plasma')
     call is_set_type_float(structure_in%li, trim(adjustl(name)) // '%li')
     call is_set_type_float(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_float(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_float(structure_in%psi_ax, trim(adjustl(name)) // '%psi_ax')
     call is_set_type_float(structure_in%psi_bound, trim(adjustl(name)) // '%psi_bound')
     call is_set_type_mag_axis(structure_in%mag_axis, trim(adjustl(name)) // '%mag_axis')
     call is_set_type_float(structure_in%q_95, trim(adjustl(name)) // '%q_95')
     call is_set_type_float(structure_in%q_min, trim(adjustl(name)) // '%q_min')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')
     call is_set_type_float(structure_in%w_mhd, trim(adjustl(name)) // '%w_mhd')
     call is_set_type_float(structure_in%gamma, trim(adjustl(name)) // '%gamma')

   end subroutine is_set_type_global_param

   subroutine is_set_arr_type_global_param(structure_in, name)
 
     implicit none
 
     type (type_global_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_global_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_global_param

   subroutine is_set_type_globalparam(structure_in, name)

     implicit none

     type (type_globalparam), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%current_tot, trim(adjustl(name)) // '%current_tot')
     call is_set_type_float(structure_in%current_bnd, trim(adjustl(name)) // '%current_bnd')
     call is_set_type_float(structure_in%current_ni, trim(adjustl(name)) // '%current_ni')
     call is_set_type_float(structure_in%vloop, trim(adjustl(name)) // '%vloop')
     call is_set_type_float(structure_in%li, trim(adjustl(name)) // '%li')
     call is_set_type_float(structure_in%beta_tor, trim(adjustl(name)) // '%beta_tor')
     call is_set_type_float(structure_in%beta_normal, trim(adjustl(name)) // '%beta_normal')
     call is_set_type_float(structure_in%beta_pol, trim(adjustl(name)) // '%beta_pol')
     call is_set_type_float(structure_in%w_dia, trim(adjustl(name)) // '%w_dia')
     call is_set_type_rz0D(structure_in%geom_axis, trim(adjustl(name)) // '%geom_axis')

   end subroutine is_set_type_globalparam

   subroutine is_set_arr_type_globalparam(structure_in, name)
 
     implicit none
 
     type (type_globalparam), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_globalparam(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_globalparam

   subroutine is_set_type_halpha_setup(structure_in, name)

     implicit none

     type (type_halpha_setup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_rzphi1D(structure_in%pivot_point, trim(adjustl(name)) // '%pivot_point')
     call is_set_type_vecflt_type(structure_in%horchordang, trim(adjustl(name)) // '%horchordang')
     call is_set_type_vecflt_type(structure_in%verchordang, trim(adjustl(name)) // '%verchordang')
     call is_set_type_rzphi1D(structure_in%second_point, trim(adjustl(name)) // '%second_point')
     call is_set_type_exp1D(structure_in%solidangle, trim(adjustl(name)) // '%solidangle')

   end subroutine is_set_type_halpha_setup

   subroutine is_set_arr_type_halpha_setup(structure_in, name)
 
     implicit none
 
     type (type_halpha_setup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_halpha_setup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_halpha_setup

   subroutine is_set_type_hcll(structure_in, name)

     implicit none

     type (type_hcll), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_mat_lim(structure_in%mat_lim, trim(adjustl(name)) // '%mat_lim')
     call is_set_type_hcll_bb(structure_in%hcll_bb, trim(adjustl(name)) // '%hcll_bb')

   end subroutine is_set_type_hcll

   subroutine is_set_arr_type_hcll(structure_in, name)
 
     implicit none
 
     type (type_hcll), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_hcll(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_hcll

   subroutine is_set_type_hcll_bb(structure_in, name)

     implicit none

     type (type_hcll_bb), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%bb_lifetime, trim(adjustl(name)) // '%bb_lifetime')
     call is_set_type_float(structure_in%he_inl_t, trim(adjustl(name)) // '%he_inl_t')
     call is_set_type_float(structure_in%he_fr, trim(adjustl(name)) // '%he_fr')
     call is_set_type_float(structure_in%he_inl_p, trim(adjustl(name)) // '%he_inl_p')
     call is_set_type_float(structure_in%loca_des_p, trim(adjustl(name)) // '%loca_des_p')
     call is_set_type_float(structure_in%he_dp, trim(adjustl(name)) // '%he_dp')
     call is_set_type_float(structure_in%lipb_dp, trim(adjustl(name)) // '%lipb_dp')
     call is_set_type_react(structure_in%react, trim(adjustl(name)) // '%react')
     call is_set_type_hcllbb_specs(structure_in%inboard, trim(adjustl(name)) // '%inboard')
     call is_set_type_hcllbb_specs(structure_in%outboard, trim(adjustl(name)) // '%outboard')

   end subroutine is_set_type_hcll_bb

   subroutine is_set_arr_type_hcll_bb(structure_in, name)
 
     implicit none
 
     type (type_hcll_bb), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_hcll_bb(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_hcll_bb

   subroutine is_set_type_hcllbb_specs(structure_in, name)

     implicit none

     type (type_hcllbb_specs), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%mass, trim(adjustl(name)) // '%mass')
     call is_set_type_vecflt_type(structure_in%dr, trim(adjustl(name)) // '%dr')
     call is_set_type_vecflt_type(structure_in%mat, trim(adjustl(name)) // '%mat')
     call is_set_type_matflt_type(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_bb_geometry(structure_in%mod_geom, trim(adjustl(name)) // '%mod_geom')
     call is_set_type_mode_neutr(structure_in%mod_neutr, trim(adjustl(name)) // '%mod_neutr')
     call is_set_type_mode_therm(structure_in%mod_therm, trim(adjustl(name)) // '%mod_therm')
     call is_set_type_mode_th_hyd(structure_in%mod_th_hyd, trim(adjustl(name)) // '%mod_th_hyd')
     call is_set_type_mode_mech(structure_in%mod_mech, trim(adjustl(name)) // '%mod_mech')
     call is_set_type_mode_lipb(structure_in%mod_lipb, trim(adjustl(name)) // '%mod_lipb')
     call is_set_type_mode_tritium(structure_in%mod_tritium, trim(adjustl(name)) // '%mod_tritium')

   end subroutine is_set_type_hcllbb_specs

   subroutine is_set_arr_type_hcllbb_specs(structure_in, name)
 
     implicit none
 
     type (type_hcllbb_specs), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_hcllbb_specs(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_hcllbb_specs

   subroutine is_set_type_holes(structure_in, name)

     implicit none

     type (type_holes), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%n_holes, trim(adjustl(name)) // '%n_holes')
     call is_set_type_coordinates(structure_in%coordinates, trim(adjustl(name)) // '%coordinates')
     call is_set_type_width(structure_in%width, trim(adjustl(name)) // '%width')
     call is_set_type_vecflt_type(structure_in%eta, trim(adjustl(name)) // '%eta')

   end subroutine is_set_type_holes

   subroutine is_set_arr_type_holes(structure_in, name)
 
     implicit none
 
     type (type_holes), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_holes(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_holes

   subroutine is_set_type_identifier(structure_in, name)

     implicit none

     type (type_identifier), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')
     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')

   end subroutine is_set_type_identifier

   subroutine is_set_arr_type_identifier(structure_in, name)
 
     implicit none
 
     type (type_identifier), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_identifier(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_identifier

   subroutine is_set_type_impcoeff(structure_in, name)

     implicit none

     type (type_impcoeff), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_coefficients_neutrals(structure_in%chargestate, trim(adjustl(name)) // '%chargestate')

   end subroutine is_set_type_impcoeff

   subroutine is_set_arr_type_impcoeff(structure_in, name)
 
     implicit none
 
     type (type_impcoeff), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_impcoeff(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_impcoeff

   subroutine is_set_type_impurities(structure_in, name)

     implicit none

     type (type_impurities), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nucindex, trim(adjustl(name)) // '%nucindex')
     call is_set_type_integer(structure_in%i_ion, trim(adjustl(name)) // '%i_ion')
     call is_set_type_integer(structure_in%nzimp, trim(adjustl(name)) // '%nzimp')
     call is_set_type_vecflt_type(structure_in%zmin, trim(adjustl(name)) // '%zmin')
     call is_set_type_vecflt_type(structure_in%zmax, trim(adjustl(name)) // '%zmax')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_impurities

   subroutine is_set_arr_type_impurities(structure_in, name)
 
     implicit none
 
     type (type_impurities), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_impurities(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_impurities

   subroutine is_set_type_impurity_type(structure_in, name)

     implicit none

     type (type_impurity_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_matflt_type(structure_in%zsq, trim(adjustl(name)) // '%zsq')
     call is_set_type_matflt_type(structure_in%nz, trim(adjustl(name)) // '%nz')
     call is_set_type_matflt_type(structure_in%tz, trim(adjustl(name)) // '%tz')
     call is_set_type_sourceimp(structure_in%source_term, trim(adjustl(name)) // '%source_term')
     call is_set_type_boundaryimp(structure_in%boundary, trim(adjustl(name)) // '%boundary')
     call is_set_type_coretransimp(structure_in%transp_coef, trim(adjustl(name)) // '%transp_coef')
     call is_set_type_fluximp(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_matflt_type(structure_in%time_deriv, trim(adjustl(name)) // '%time_deriv')
     call is_set_type_coreimpurediag_type(structure_in%diagnostic, trim(adjustl(name)) // '%diagnostic')

   end subroutine is_set_type_impurity_type

   subroutine is_set_arr_type_impurity_type(structure_in, name)
 
     implicit none
 
     type (type_impurity_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_impurity_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_impurity_type

   subroutine is_set_type_inj_spec(structure_in, name)

     implicit none

     type (type_inj_spec), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_float(structure_in%zn, trim(adjustl(name)) // '%zn')

   end subroutine is_set_type_inj_spec

   subroutine is_set_arr_type_inj_spec(structure_in, name)
 
     implicit none
 
     type (type_inj_spec), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_inj_spec(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_inj_spec

   subroutine is_set_type_ions(structure_in, name)

     implicit none

     type (type_ions), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nucindex, trim(adjustl(name)) // '%nucindex')
     call is_set_type_float(structure_in%zion, trim(adjustl(name)) // '%zion')
     call is_set_type_integer(structure_in%imp_flag, trim(adjustl(name)) // '%imp_flag')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_ions

   subroutine is_set_arr_type_ions(structure_in, name)
 
     implicit none
 
     type (type_ions), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ions(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ions

   subroutine is_set_type_isoflux(structure_in, name)

     implicit none

     type (type_isoflux), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecflt_type(structure_in%weight, trim(adjustl(name)) // '%weight')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_vecflt_type(structure_in%calculated, trim(adjustl(name)) // '%calculated')
     call is_set_type_vecflt_type(structure_in%chi2, trim(adjustl(name)) // '%chi2')

   end subroutine is_set_type_isoflux

   subroutine is_set_arr_type_isoflux(structure_in, name)
 
     implicit none
 
     type (type_isoflux), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_isoflux(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_isoflux

   subroutine is_set_type_jni(structure_in, name)

     implicit none

     type (type_jni), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%integral, trim(adjustl(name)) // '%integral')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_jni

   subroutine is_set_arr_type_jni(structure_in, name)
 
     implicit none
 
     type (type_jni), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_jni(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_jni

   subroutine is_set_type_lang_derived(structure_in, name)

     implicit none

     type (type_lang_derived), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_rzphi1Dexp(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_lang_derived

   subroutine is_set_arr_type_lang_derived(structure_in, name)
 
     implicit none
 
     type (type_lang_derived), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_lang_derived(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_lang_derived

   subroutine is_set_type_lang_measure(structure_in, name)

     implicit none

     type (type_lang_measure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%direction, trim(adjustl(name)) // '%direction')
     call is_set_type_exp1D(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_rzphi1Dexp(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_lang_measure

   subroutine is_set_arr_type_lang_measure(structure_in, name)
 
     implicit none
 
     type (type_lang_measure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_lang_measure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_lang_measure

   subroutine is_set_type_launchangles(structure_in, name)

     implicit none

     type (type_launchangles), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%alpha, trim(adjustl(name)) // '%alpha')
     call is_set_type_float(structure_in%beta, trim(adjustl(name)) // '%beta')

   end subroutine is_set_type_launchangles

   subroutine is_set_arr_type_launchangles(structure_in, name)
 
     implicit none
 
     type (type_launchangles), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchangles(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchangles

   subroutine is_set_type_launchs_parallel(structure_in, name)

     implicit none

     type (type_launchs_parallel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%nn_par, trim(adjustl(name)) // '%nn_par')
     call is_set_type_matflt_type(structure_in%n_par, trim(adjustl(name)) // '%n_par')
     call is_set_type_vecflt_type(structure_in%power, trim(adjustl(name)) // '%power')

   end subroutine is_set_type_launchs_parallel

   subroutine is_set_arr_type_launchs_parallel(structure_in, name)
 
     implicit none
 
     type (type_launchs_parallel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchs_parallel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchs_parallel

   subroutine is_set_type_launchs_phi_theta(structure_in, name)

     implicit none

     type (type_launchs_phi_theta), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%nn_phi, trim(adjustl(name)) // '%nn_phi')
     call is_set_type_vecint_type(structure_in%nn_theta, trim(adjustl(name)) // '%nn_theta')
     call is_set_type_matflt_type(structure_in%n_phi, trim(adjustl(name)) // '%n_phi')
     call is_set_type_matflt_type(structure_in%n_theta, trim(adjustl(name)) // '%n_theta')
     call is_set_type_array3dflt_type(structure_in%power, trim(adjustl(name)) // '%power')

   end subroutine is_set_type_launchs_phi_theta

   subroutine is_set_arr_type_launchs_phi_theta(structure_in, name)
 
     implicit none
 
     type (type_launchs_phi_theta), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchs_phi_theta(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchs_phi_theta

   subroutine is_set_type_launchs_rfbeam(structure_in, name)

     implicit none

     type (type_launchs_rfbeam), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_launchs_rfbeam_spot(structure_in%spot, trim(adjustl(name)) // '%spot')
     call is_set_type_launchs_rfbeam_phaseellipse(structure_in%phaseellipse, trim(adjustl(name)) // '%phaseellipse')

   end subroutine is_set_type_launchs_rfbeam

   subroutine is_set_arr_type_launchs_rfbeam(structure_in, name)
 
     implicit none
 
     type (type_launchs_rfbeam), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchs_rfbeam(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchs_rfbeam

   subroutine is_set_type_launchs_rfbeam_phaseellipse(structure_in, name)

     implicit none

     type (type_launchs_rfbeam_phaseellipse), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%invcurvrad, trim(adjustl(name)) // '%invcurvrad')
     call is_set_type_vecflt_type(structure_in%angle, trim(adjustl(name)) // '%angle')

   end subroutine is_set_type_launchs_rfbeam_phaseellipse

   subroutine is_set_arr_type_launchs_rfbeam_phaseellipse(structure_in, name)
 
     implicit none
 
     type (type_launchs_rfbeam_phaseellipse), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchs_rfbeam_phaseellipse(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchs_rfbeam_phaseellipse

   subroutine is_set_type_launchs_rfbeam_spot(structure_in, name)

     implicit none

     type (type_launchs_rfbeam_spot), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%waist, trim(adjustl(name)) // '%waist')
     call is_set_type_vecflt_type(structure_in%angle, trim(adjustl(name)) // '%angle')

   end subroutine is_set_type_launchs_rfbeam_spot

   subroutine is_set_arr_type_launchs_rfbeam_spot(structure_in, name)
 
     implicit none
 
     type (type_launchs_rfbeam_spot), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchs_rfbeam_spot(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchs_rfbeam_spot

   subroutine is_set_type_launchsignal(structure_in, name)

     implicit none

     type (type_launchsignal), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%time_launch, trim(adjustl(name)) // '%time_launch')
     call is_set_type_vecflt_type(structure_in%freq, trim(adjustl(name)) // '%freq')
     call is_set_type_vecflt_type(structure_in%amplitude, trim(adjustl(name)) // '%amplitude')
     call is_set_type_vecflt_type(structure_in%phase, trim(adjustl(name)) // '%phase')

   end subroutine is_set_type_launchsignal

   subroutine is_set_arr_type_launchsignal(structure_in, name)
 
     implicit none
 
     type (type_launchsignal), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_launchsignal(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_launchsignal

   subroutine is_set_type_limiter_unit(structure_in, name)

     implicit none

     type (type_limiter_unit), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%closed, trim(adjustl(name)) // '%closed')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_float(structure_in%eta, trim(adjustl(name)) // '%eta')
     call is_set_type_float(structure_in%delta, trim(adjustl(name)) // '%delta')
     call is_set_type_float(structure_in%permeability, trim(adjustl(name)) // '%permeability')

   end subroutine is_set_type_limiter_unit

   subroutine is_set_arr_type_limiter_unit(structure_in, name)
 
     implicit none
 
     type (type_limiter_unit), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_limiter_unit(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_limiter_unit

   subroutine is_set_type_limits(structure_in, name)

     implicit none

     type (type_limits), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%fw_dpa, trim(adjustl(name)) // '%fw_dpa')
     call is_set_type_float(structure_in%he_appm, trim(adjustl(name)) // '%he_appm')
     call is_set_type_float(structure_in%ins_dose, trim(adjustl(name)) // '%ins_dose')
     call is_set_type_float(structure_in%fn_flu, trim(adjustl(name)) // '%fn_flu')
     call is_set_type_float(structure_in%dpa_cu, trim(adjustl(name)) // '%dpa_cu')
     call is_set_type_float(structure_in%wp_nh, trim(adjustl(name)) // '%wp_nh')

   end subroutine is_set_type_limits

   subroutine is_set_arr_type_limits(structure_in, name)
 
     implicit none
 
     type (type_limits), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_limits(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_limits

   subroutine is_set_type_lineintegraldiag(structure_in, name)

     implicit none

     type (type_lineintegraldiag), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_datainfo(structure_in%datainfo, trim(adjustl(name)) // '%datainfo')
     call is_set_type_vecstring_type(structure_in%expression, trim(adjustl(name)) // '%expression')
     call is_set_type_setup_line(structure_in%setup_line, trim(adjustl(name)) // '%setup_line')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_lineintegraldiag

   subroutine is_set_arr_type_lineintegraldiag(structure_in, name)
 
     implicit none
 
     type (type_lineintegraldiag), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_lineintegraldiag(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_lineintegraldiag

   subroutine is_set_type_lithmeasure(structure_in, name)

     implicit none

     type (type_lithmeasure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%ne, trim(adjustl(name)) // '%ne')

   end subroutine is_set_type_lithmeasure

   subroutine is_set_arr_type_lithmeasure(structure_in, name)
 
     implicit none
 
     type (type_lithmeasure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_lithmeasure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_lithmeasure

   subroutine is_set_type_lithsetup(structure_in, name)

     implicit none

     type (type_lithsetup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1D(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_lithsetup

   subroutine is_set_arr_type_lithsetup(structure_in, name)
 
     implicit none
 
     type (type_lithsetup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_lithsetup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_lithsetup

   subroutine is_set_type_local(structure_in, name)

     implicit none

     type (type_local), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%e_plus, trim(adjustl(name)) // '%e_plus')
     call is_set_type_array3dflt_type(structure_in%e_plus_ph, trim(adjustl(name)) // '%e_plus_ph')
     call is_set_type_array3dflt_type(structure_in%e_minus, trim(adjustl(name)) // '%e_minus')
     call is_set_type_array3dflt_type(structure_in%e_minus_ph, trim(adjustl(name)) // '%e_minus_ph')
     call is_set_type_array3dint_type(structure_in%e_norm, trim(adjustl(name)) // '%e_norm')
     call is_set_type_array3dflt_type(structure_in%enorm_ph, trim(adjustl(name)) // '%enorm_ph')
     call is_set_type_array3dflt_type(structure_in%e_binorm, trim(adjustl(name)) // '%e_binorm')
     call is_set_type_array3dflt_type(structure_in%e_binorm_ph, trim(adjustl(name)) // '%e_binorm_ph')
     call is_set_type_array3dflt_type(structure_in%e_para, trim(adjustl(name)) // '%e_para')
     call is_set_type_array3dflt_type(structure_in%e_para_ph, trim(adjustl(name)) // '%e_para_ph')
     call is_set_type_array3dflt_type(structure_in%b_norm, trim(adjustl(name)) // '%b_norm')
     call is_set_type_array3dflt_type(structure_in%b_norm_ph, trim(adjustl(name)) // '%b_norm_ph')
     call is_set_type_array3dflt_type(structure_in%b_binorm, trim(adjustl(name)) // '%b_binorm')
     call is_set_type_array3dflt_type(structure_in%b_binorm_ph, trim(adjustl(name)) // '%b_binorm_ph')
     call is_set_type_array3dflt_type(structure_in%b_para, trim(adjustl(name)) // '%b_para')
     call is_set_type_array3dflt_type(structure_in%b_para_ph, trim(adjustl(name)) // '%b_para_ph')
     call is_set_type_array3dflt_type(structure_in%k_perp, trim(adjustl(name)) // '%k_perp')

   end subroutine is_set_type_local

   subroutine is_set_arr_type_local(structure_in, name)
 
     implicit none
 
     type (type_local), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_local(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_local

   subroutine is_set_type_mag_axis(structure_in, name)

     implicit none

     type (type_mag_axis), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz0D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_float(structure_in%bphi, trim(adjustl(name)) // '%bphi')
     call is_set_type_float(structure_in%q, trim(adjustl(name)) // '%q')

   end subroutine is_set_type_mag_axis

   subroutine is_set_arr_type_mag_axis(structure_in, name)
 
     implicit none
 
     type (type_mag_axis), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mag_axis(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mag_axis

   subroutine is_set_type_magnet_iron(structure_in, name)

     implicit none

     type (type_magnet_iron), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_eqmes1D(structure_in%mr, trim(adjustl(name)) // '%mr')
     call is_set_type_eqmes1D(structure_in%mz, trim(adjustl(name)) // '%mz')

   end subroutine is_set_type_magnet_iron

   subroutine is_set_arr_type_magnet_iron(structure_in, name)
 
     implicit none
 
     type (type_magnet_iron), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_magnet_iron(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_magnet_iron

   subroutine is_set_type_magnetise(structure_in, name)

     implicit none

     type (type_magnetise), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%mr, trim(adjustl(name)) // '%mr')
     call is_set_type_exp1D(structure_in%mz, trim(adjustl(name)) // '%mz')

   end subroutine is_set_type_magnetise

   subroutine is_set_arr_type_magnetise(structure_in, name)
 
     implicit none
 
     type (type_magnetise), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_magnetise(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_magnetise

   subroutine is_set_type_mat_lim(structure_in, name)

     implicit none

     type (type_mat_lim), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%cool_t_lim, trim(adjustl(name)) // '%cool_t_lim')
     call is_set_type_float(structure_in%steel_t_lim, trim(adjustl(name)) // '%steel_t_lim')
     call is_set_type_float(structure_in%lipb_t_lim, trim(adjustl(name)) // '%lipb_t_lim')

   end subroutine is_set_type_mat_lim

   subroutine is_set_arr_type_mat_lim(structure_in, name)
 
     implicit none
 
     type (type_mat_lim), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mat_lim(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mat_lim

   subroutine is_set_type_mdinfo(structure_in, name)

     implicit none

     type (type_mdinfo), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%shot_min, trim(adjustl(name)) // '%shot_min')
     call is_set_type_integer(structure_in%shot_max, trim(adjustl(name)) // '%shot_max')
     call is_set_type_entry_def(structure_in%md_entry, trim(adjustl(name)) // '%md_entry')

   end subroutine is_set_type_mdinfo

   subroutine is_set_arr_type_mdinfo(structure_in, name)
 
     implicit none
 
     type (type_mdinfo), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mdinfo(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mdinfo

   subroutine is_set_type_mhd_ideal_wall2d(structure_in, name)

     implicit none

     type (type_mhd_ideal_wall2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%walltype, trim(adjustl(name)) // '%walltype')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_mhd_ideal_wall2d

   subroutine is_set_arr_type_mhd_ideal_wall2d(structure_in, name)
 
     implicit none
 
     type (type_mhd_ideal_wall2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd_ideal_wall2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd_ideal_wall2d

   subroutine is_set_type_mhd_mode(structure_in, name)

     implicit none

     type (type_mhd_mode), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%modenum, trim(adjustl(name)) // '%modenum')
     call is_set_type_float(structure_in%growthrate, trim(adjustl(name)) // '%growthrate')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_mhd_plasma(structure_in%plasma, trim(adjustl(name)) // '%plasma')
     call is_set_type_mhd_vacuum(structure_in%vacuum, trim(adjustl(name)) // '%vacuum')

   end subroutine is_set_type_mhd_mode

   subroutine is_set_arr_type_mhd_mode(structure_in, name)
 
     implicit none
 
     type (type_mhd_mode), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd_mode(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd_mode

   subroutine is_set_type_mhd_plasma(structure_in, name)

     implicit none

     type (type_mhd_plasma), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_matflt_type(structure_in%m, trim(adjustl(name)) // '%m')
     call is_set_type_matcplx_type(structure_in%disp_perp, trim(adjustl(name)) // '%disp_perp')
     call is_set_type_matcplx_type(structure_in%disp_par, trim(adjustl(name)) // '%disp_par')
     call is_set_type_vecflt_type(structure_in%tau_alfven, trim(adjustl(name)) // '%tau_alfven')
     call is_set_type_vecflt_type(structure_in%tau_res, trim(adjustl(name)) // '%tau_res')
     call is_set_type_coord_sys(structure_in%coord_sys, trim(adjustl(name)) // '%coord_sys')
     call is_set_type_mhd_vector(structure_in%a_pert, trim(adjustl(name)) // '%a_pert')
     call is_set_type_mhd_vector(structure_in%b_pert, trim(adjustl(name)) // '%b_pert')
     call is_set_type_mhd_vector(structure_in%v_pert, trim(adjustl(name)) // '%v_pert')
     call is_set_type_matcplx_type(structure_in%p_pert, trim(adjustl(name)) // '%p_pert')
     call is_set_type_matcplx_type(structure_in%rho_mass_per, trim(adjustl(name)) // '%rho_mass_per')
     call is_set_type_matcplx_type(structure_in%temp_per, trim(adjustl(name)) // '%temp_per')

   end subroutine is_set_type_mhd_plasma

   subroutine is_set_arr_type_mhd_plasma(structure_in, name)
 
     implicit none
 
     type (type_mhd_plasma), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd_plasma(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd_plasma

   subroutine is_set_type_mhd_res_wall2d(structure_in, name)

     implicit none

     type (type_mhd_res_wall2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%walltype, trim(adjustl(name)) // '%walltype')
     call is_set_type_float(structure_in%delta, trim(adjustl(name)) // '%delta')
     call is_set_type_float(structure_in%eta, trim(adjustl(name)) // '%eta')
     call is_set_type_integer(structure_in%npoloidal, trim(adjustl(name)) // '%npoloidal')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_holes(structure_in%holes, trim(adjustl(name)) // '%holes')

   end subroutine is_set_type_mhd_res_wall2d

   subroutine is_set_arr_type_mhd_res_wall2d(structure_in, name)
 
     implicit none
 
     type (type_mhd_res_wall2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd_res_wall2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd_res_wall2d

   subroutine is_set_type_mhd_vacuum(structure_in, name)

     implicit none

     type (type_mhd_vacuum), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%m, trim(adjustl(name)) // '%m')
     call is_set_type_coord_sys(structure_in%coord_sys, trim(adjustl(name)) // '%coord_sys')
     call is_set_type_mhd_vector(structure_in%a_pert, trim(adjustl(name)) // '%a_pert')
     call is_set_type_mhd_vector(structure_in%b_pert, trim(adjustl(name)) // '%b_pert')

   end subroutine is_set_type_mhd_vacuum

   subroutine is_set_arr_type_mhd_vacuum(structure_in, name)
 
     implicit none
 
     type (type_mhd_vacuum), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd_vacuum(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd_vacuum

   subroutine is_set_type_mhd_vector(structure_in, name)

     implicit none

     type (type_mhd_vector), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matcplx_type(structure_in%coord1, trim(adjustl(name)) // '%coord1')
     call is_set_type_matcplx_type(structure_in%coord2, trim(adjustl(name)) // '%coord2')
     call is_set_type_matcplx_type(structure_in%coord3, trim(adjustl(name)) // '%coord3')

   end subroutine is_set_type_mhd_vector

   subroutine is_set_arr_type_mhd_vector(structure_in, name)
 
     implicit none
 
     type (type_mhd_vector), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mhd_vector(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mhd_vector

   subroutine is_set_type_mode_lipb(structure_in, name)

     implicit none

     type (type_mode_lipb), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%lp_rec_day, trim(adjustl(name)) // '%lp_rec_day')
     call is_set_type_vecflt_type(structure_in%bb_lp_fr, trim(adjustl(name)) // '%bb_lp_fr')
     call is_set_type_float(structure_in%lp_inl_p, trim(adjustl(name)) // '%lp_inl_p')
     call is_set_type_float(structure_in%bu_dp_lp, trim(adjustl(name)) // '%bu_dp_lp')
     call is_set_type_float(structure_in%man_dp_lp, trim(adjustl(name)) // '%man_dp_lp')
     call is_set_type_float(structure_in%tot_dp_lp, trim(adjustl(name)) // '%tot_dp_lp')
     call is_set_type_float(structure_in%bu_lp_ave_t, trim(adjustl(name)) // '%bu_lp_ave_t')
     call is_set_type_float(structure_in%bu_lp_max_t, trim(adjustl(name)) // '%bu_lp_max_t')

   end subroutine is_set_type_mode_lipb

   subroutine is_set_arr_type_mode_lipb(structure_in, name)
 
     implicit none
 
     type (type_mode_lipb), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mode_lipb(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mode_lipb

   subroutine is_set_type_mode_mech(structure_in, name)

     implicit none

     type (type_mode_mech), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%fw_min_ts_mg, trim(adjustl(name)) // '%fw_min_ts_mg')
     call is_set_type_float(structure_in%fw_min_bd_mg, trim(adjustl(name)) // '%fw_min_bd_mg')
     call is_set_type_float(structure_in%sg_min_ts_mg, trim(adjustl(name)) // '%sg_min_ts_mg')
     call is_set_type_float(structure_in%sg_min_bd_mg, trim(adjustl(name)) // '%sg_min_bd_mg')
     call is_set_type_float(structure_in%cp_min_ts_mg, trim(adjustl(name)) // '%cp_min_ts_mg')
     call is_set_type_float(structure_in%cp_min_bd_mg, trim(adjustl(name)) // '%cp_min_bd_mg')
     call is_set_type_float(structure_in%min_ts_mg_ac, trim(adjustl(name)) // '%min_ts_mg_ac')
     call is_set_type_float(structure_in%min_bd_mg_ac, trim(adjustl(name)) // '%min_bd_mg_ac')

   end subroutine is_set_type_mode_mech

   subroutine is_set_arr_type_mode_mech(structure_in, name)
 
     implicit none
 
     type (type_mode_mech), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mode_mech(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mode_mech

   subroutine is_set_type_mode_neutr(structure_in, name)

     implicit none

     type (type_mode_neutr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%pd_rad, trim(adjustl(name)) // '%pd_rad')
     call is_set_type_vecflt_type(structure_in%lipb_coef_pd, trim(adjustl(name)) // '%lipb_coef_pd')
     call is_set_type_vecflt_type(structure_in%steel_coef_pd, trim(adjustl(name)) // '%steel_coef_pd')
     call is_set_type_power_exchange(structure_in%pow_exchange, trim(adjustl(name)) // '%pow_exchange')

   end subroutine is_set_type_mode_neutr

   subroutine is_set_arr_type_mode_neutr(structure_in, name)
 
     implicit none
 
     type (type_mode_neutr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mode_neutr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mode_neutr

   subroutine is_set_type_mode_th_hyd(structure_in, name)

     implicit none

     type (type_mode_th_hyd), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%fw_dp_he, trim(adjustl(name)) // '%fw_dp_he')
     call is_set_type_float(structure_in%sg_dp_he, trim(adjustl(name)) // '%sg_dp_he')
     call is_set_type_float(structure_in%cp_dp_he, trim(adjustl(name)) // '%cp_dp_he')
     call is_set_type_float(structure_in%man_dp_he, trim(adjustl(name)) // '%man_dp_he')
     call is_set_type_float(structure_in%tot_dp_he, trim(adjustl(name)) // '%tot_dp_he')
     call is_set_type_float(structure_in%bp_dp_he, trim(adjustl(name)) // '%bp_dp_he')
     call is_set_type_float(structure_in%circ_dp_he, trim(adjustl(name)) // '%circ_dp_he')

   end subroutine is_set_type_mode_th_hyd

   subroutine is_set_arr_type_mode_th_hyd(structure_in, name)
 
     implicit none
 
     type (type_mode_th_hyd), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mode_th_hyd(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mode_th_hyd

   subroutine is_set_type_mode_therm(structure_in, name)

     implicit none

     type (type_mode_therm), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%he_fr, trim(adjustl(name)) // '%he_fr')
     call is_set_type_float(structure_in%perc_bp_he, trim(adjustl(name)) // '%perc_bp_he')
     call is_set_type_float(structure_in%he_out_t, trim(adjustl(name)) // '%he_out_t')
     call is_set_type_float(structure_in%fw_he_out_t, trim(adjustl(name)) // '%fw_he_out_t')
     call is_set_type_float(structure_in%sg_he_out_t, trim(adjustl(name)) // '%sg_he_out_t')
     call is_set_type_float(structure_in%cp_he_out_t, trim(adjustl(name)) // '%cp_he_out_t')
     call is_set_type_float(structure_in%fw_st_max_t, trim(adjustl(name)) // '%fw_st_max_t')
     call is_set_type_float(structure_in%sg_st_max_t, trim(adjustl(name)) // '%sg_st_max_t')
     call is_set_type_float(structure_in%cp_st_max_t, trim(adjustl(name)) // '%cp_st_max_t')

   end subroutine is_set_type_mode_therm

   subroutine is_set_arr_type_mode_therm(structure_in, name)
 
     implicit none
 
     type (type_mode_therm), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mode_therm(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mode_therm

   subroutine is_set_type_mode_tritium(structure_in, name)

     implicit none

     type (type_mode_tritium), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%t_conc_lipb, trim(adjustl(name)) // '%t_conc_lipb')
     call is_set_type_float(structure_in%t_conc_he, trim(adjustl(name)) // '%t_conc_he')

   end subroutine is_set_type_mode_tritium

   subroutine is_set_arr_type_mode_tritium(structure_in, name)
 
     implicit none
 
     type (type_mode_tritium), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_mode_tritium(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_mode_tritium

   subroutine is_set_type_modules(structure_in, name)

     implicit none

     type (type_modules), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nma_theta, trim(adjustl(name)) // '%nma_theta')
     call is_set_type_integer(structure_in%nma_phi, trim(adjustl(name)) // '%nma_phi')
     call is_set_type_vecint_type(structure_in%ima_theta, trim(adjustl(name)) // '%ima_theta')
     call is_set_type_vecint_type(structure_in%ima_phi, trim(adjustl(name)) // '%ima_phi')
     call is_set_type_float(structure_in%sm_theta, trim(adjustl(name)) // '%sm_theta')
     call is_set_type_exp1D(structure_in%amplitude, trim(adjustl(name)) // '%amplitude')
     call is_set_type_exp1D(structure_in%phase, trim(adjustl(name)) // '%phase')
     call is_set_type_waveguides(structure_in%waveguides, trim(adjustl(name)) // '%waveguides')

   end subroutine is_set_type_modules

   subroutine is_set_arr_type_modules(structure_in, name)
 
     implicit none
 
     type (type_modules), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_modules(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_modules

   subroutine is_set_type_msediag_emiss_chord(structure_in, name)

     implicit none

     type (type_msediag_emiss_chord), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_rzphi1D(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_arr_type_msediag_polarization(structure_in%polarization, trim(adjustl(name)) // '%polarization')
     call is_set_type_vecflt_type(structure_in%quantiaxis, trim(adjustl(name)) // '%quantiaxis')

   end subroutine is_set_type_msediag_emiss_chord

   subroutine is_set_arr_type_msediag_emiss_chord(structure_in, name)
 
     implicit none
 
     type (type_msediag_emiss_chord), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_emiss_chord(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_emiss_chord

   subroutine is_set_type_msediag_emissivity(structure_in, name)

     implicit none

     type (type_msediag_emissivity), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%wavelength, trim(adjustl(name)) // '%wavelength')
     call is_set_arr_type_msediag_emiss_chord(structure_in%emiss_chord, trim(adjustl(name)) // '%emiss_chord')

   end subroutine is_set_type_msediag_emissivity

   subroutine is_set_arr_type_msediag_emissivity(structure_in, name)
 
     implicit none
 
     type (type_msediag_emissivity), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_emissivity(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_emissivity

   subroutine is_set_type_msediag_polarization(structure_in, name)

     implicit none

     type (type_msediag_polarization), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_matflt_type(structure_in%spec_emiss, trim(adjustl(name)) // '%spec_emiss')

   end subroutine is_set_type_msediag_polarization

   subroutine is_set_arr_type_msediag_polarization(structure_in, name)
 
     implicit none
 
     type (type_msediag_polarization), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_polarization(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_polarization

   subroutine is_set_type_msediag_radia_chord(structure_in, name)

     implicit none

     type (type_msediag_radia_chord), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_msediag_setup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_arr_type_msediag_stokes(structure_in%stokes, trim(adjustl(name)) // '%stokes')
     call is_set_type_exp1D(structure_in%totradiance, trim(adjustl(name)) // '%totradiance')

   end subroutine is_set_type_msediag_radia_chord

   subroutine is_set_arr_type_msediag_radia_chord(structure_in, name)
 
     implicit none
 
     type (type_msediag_radia_chord), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_radia_chord(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_radia_chord

   subroutine is_set_type_msediag_radiance(structure_in, name)

     implicit none

     type (type_msediag_radiance), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%wavelength, trim(adjustl(name)) // '%wavelength')
     call is_set_arr_type_msediag_radia_chord(structure_in%radia_chord, trim(adjustl(name)) // '%radia_chord')

   end subroutine is_set_type_msediag_radiance

   subroutine is_set_arr_type_msediag_radiance(structure_in, name)
 
     implicit none
 
     type (type_msediag_radiance), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_radiance(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_radiance

   subroutine is_set_type_msediag_setup(structure_in, name)

     implicit none

     type (type_msediag_setup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi0D(structure_in%pivot_point, trim(adjustl(name)) // '%pivot_point')
     call is_set_type_float(structure_in%horchordang, trim(adjustl(name)) // '%horchordang')
     call is_set_type_float(structure_in%verchordang, trim(adjustl(name)) // '%verchordang')
     call is_set_type_rzphi0D(structure_in%second_point, trim(adjustl(name)) // '%second_point')

   end subroutine is_set_type_msediag_setup

   subroutine is_set_arr_type_msediag_setup(structure_in, name)
 
     implicit none
 
     type (type_msediag_setup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_setup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_setup

   subroutine is_set_type_msediag_setup_polarimetry(structure_in, name)

     implicit none

     type (type_msediag_setup_polarimetry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphidrdzdphi1D(structure_in%rzgamma, trim(adjustl(name)) // '%rzgamma')
     call is_set_type_matflt_type(structure_in%geom_coef, trim(adjustl(name)) // '%geom_coef')

   end subroutine is_set_type_msediag_setup_polarimetry

   subroutine is_set_arr_type_msediag_setup_polarimetry(structure_in, name)
 
     implicit none
 
     type (type_msediag_setup_polarimetry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_setup_polarimetry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_setup_polarimetry

   subroutine is_set_type_msediag_stokes(structure_in, name)

     implicit none

     type (type_msediag_stokes), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_matflt_type(structure_in%vector, trim(adjustl(name)) // '%vector')

   end subroutine is_set_type_msediag_stokes

   subroutine is_set_arr_type_msediag_stokes(structure_in, name)
 
     implicit none
 
     type (type_msediag_stokes), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_msediag_stokes(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_msediag_stokes

   subroutine is_set_type_nbi_nbi_unit_wall(structure_in, name)

     implicit none

     type (type_nbi_nbi_unit_wall), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_nbi_nbi_unit_wall_surface(structure_in%surface, trim(adjustl(name)) // '%surface')
     call is_set_arr_type_flat_polygon(structure_in%collimator, trim(adjustl(name)) // '%collimator')

   end subroutine is_set_type_nbi_nbi_unit_wall

   subroutine is_set_arr_type_nbi_nbi_unit_wall(structure_in, name)
 
     implicit none
 
     type (type_nbi_nbi_unit_wall), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_nbi_nbi_unit_wall(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_nbi_nbi_unit_wall

   subroutine is_set_type_nbi_nbi_unit_wall_surface(structure_in, name)

     implicit none

     type (type_nbi_nbi_unit_wall_surface), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_trianglexyz(structure_in%triangle, trim(adjustl(name)) // '%triangle')
     call is_set_arr_type_rectanglexyz(structure_in%rectangle, trim(adjustl(name)) // '%rectangle')

   end subroutine is_set_type_nbi_nbi_unit_wall_surface

   subroutine is_set_arr_type_nbi_nbi_unit_wall_surface(structure_in, name)
 
     implicit none
 
     type (type_nbi_nbi_unit_wall_surface), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_nbi_nbi_unit_wall_surface(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_nbi_nbi_unit_wall_surface

   subroutine is_set_type_nbi_unit(structure_in, name)

     implicit none

     type (type_nbi_unit), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_inj_spec(structure_in%inj_spec, trim(adjustl(name)) // '%inj_spec')
     call is_set_type_exp0D(structure_in%pow_unit, trim(adjustl(name)) // '%pow_unit')
     call is_set_type_exp0D(structure_in%inj_eng_unit, trim(adjustl(name)) // '%inj_eng_unit')
     call is_set_type_exp1D(structure_in%beamcurrfrac, trim(adjustl(name)) // '%beamcurrfrac')
     call is_set_type_exp1D(structure_in%beampowrfrac, trim(adjustl(name)) // '%beampowrfrac')
     call is_set_arr_type_beamletgroup(structure_in%beamletgroup, trim(adjustl(name)) // '%beamletgroup')
     call is_set_type_nbi_nbi_unit_wall(structure_in%wall, trim(adjustl(name)) // '%wall')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_nbi_unit

   subroutine is_set_arr_type_nbi_unit(structure_in, name)
 
     implicit none
 
     type (type_nbi_unit), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_nbi_unit(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_nbi_unit

   subroutine is_set_type_ne_transp(structure_in, name)

     implicit none

     type (type_ne_transp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%diff_eff, trim(adjustl(name)) // '%diff_eff')
     call is_set_type_matflt_type(structure_in%vconv_eff, trim(adjustl(name)) // '%vconv_eff')
     call is_set_type_vecflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_offdiagel(structure_in%off_diagonal, trim(adjustl(name)) // '%off_diagonal')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')

   end subroutine is_set_type_ne_transp

   subroutine is_set_arr_type_ne_transp(structure_in, name)
 
     implicit none
 
     type (type_ne_transp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ne_transp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ne_transp

   subroutine is_set_type_neoclassic_impurity(structure_in, name)

     implicit none

     type (type_neoclassic_impurity), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%utheta_z, trim(adjustl(name)) // '%utheta_z')

   end subroutine is_set_type_neoclassic_impurity

   subroutine is_set_arr_type_neoclassic_impurity(structure_in, name)
 
     implicit none
 
     type (type_neoclassic_impurity), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_neoclassic_impurity(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_neoclassic_impurity

   subroutine is_set_type_neut_results(structure_in, name)

     implicit none

     type (type_neut_results), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%tbr_bk, trim(adjustl(name)) // '%tbr_bk')
     call is_set_type_float(structure_in%tbr_bk_inb, trim(adjustl(name)) // '%tbr_bk_inb')
     call is_set_type_float(structure_in%tbr_bk_outb, trim(adjustl(name)) // '%tbr_bk_outb')
     call is_set_type_float(structure_in%me_bk, trim(adjustl(name)) // '%me_bk')
     call is_set_type_float(structure_in%me_shield, trim(adjustl(name)) // '%me_shield')
     call is_set_type_float(structure_in%he_appm_res, trim(adjustl(name)) // '%he_appm_res')
     call is_set_type_float(structure_in%ins_dose_max, trim(adjustl(name)) // '%ins_dose_max')
     call is_set_type_float(structure_in%fn_flu_max, trim(adjustl(name)) // '%fn_flu_max')
     call is_set_type_float(structure_in%dpa_cu_max, trim(adjustl(name)) // '%dpa_cu_max')
     call is_set_type_float(structure_in%fn_flux_bz, trim(adjustl(name)) // '%fn_flux_bz')
     call is_set_type_float(structure_in%fn_flux_bp, trim(adjustl(name)) // '%fn_flux_bp')
     call is_set_type_float(structure_in%fn_flux_man, trim(adjustl(name)) // '%fn_flux_man')
     call is_set_type_float(structure_in%fn_flux_sh, trim(adjustl(name)) // '%fn_flux_sh')
     call is_set_type_float(structure_in%p_nh_bk, trim(adjustl(name)) // '%p_nh_bk')
     call is_set_type_float(structure_in%p_nh_sh, trim(adjustl(name)) // '%p_nh_sh')

   end subroutine is_set_type_neut_results

   subroutine is_set_arr_type_neut_results(structure_in, name)
 
     implicit none
 
     type (type_neut_results), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_neut_results(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_neut_results

   subroutine is_set_type_neutral_complex_type(structure_in, name)

     implicit none

     type (type_neutral_complex_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_coreneutrals_neutraltype(structure_in%neutraltype, trim(adjustl(name)) // '%neutraltype')
     call is_set_type_vecflt_type(structure_in%prad0, trim(adjustl(name)) // '%prad0')

   end subroutine is_set_type_neutral_complex_type

   subroutine is_set_arr_type_neutral_complex_type(structure_in, name)
 
     implicit none
 
     type (type_neutral_complex_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_neutral_complex_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_neutral_complex_type

   subroutine is_set_type_neutro_resul(structure_in, name)

     implicit none

     type (type_neutro_resul), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%nwl_max, trim(adjustl(name)) // '%nwl_max')
     call is_set_type_vecflt_type(structure_in%nwl_pol_prof, trim(adjustl(name)) // '%nwl_pol_prof')

   end subroutine is_set_type_neutro_resul

   subroutine is_set_arr_type_neutro_resul(structure_in, name)
 
     implicit none
 
     type (type_neutro_resul), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_neutro_resul(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_neutro_resul

   subroutine is_set_type_ni_transp(structure_in, name)

     implicit none

     type (type_ni_transp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%diff_eff, trim(adjustl(name)) // '%diff_eff')
     call is_set_type_array3dflt_type(structure_in%vconv_eff, trim(adjustl(name)) // '%vconv_eff')
     call is_set_type_matflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_offdiagion(structure_in%off_diagonal, trim(adjustl(name)) // '%off_diagonal')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')

   end subroutine is_set_type_ni_transp

   subroutine is_set_arr_type_ni_transp(structure_in, name)
 
     implicit none
 
     type (type_ni_transp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ni_transp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ni_transp

   subroutine is_set_type_ntm_mode(structure_in, name)

     implicit none

     type (type_ntm_mode), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_ntm_mode_onset(structure_in%onset, trim(adjustl(name)) // '%onset')
     call is_set_type_ntm_mode_full_evol(structure_in%full_evol, trim(adjustl(name)) // '%full_evol')
     call is_set_type_ntm_mode_evolution(structure_in%evolution, trim(adjustl(name)) // '%evolution')

   end subroutine is_set_type_ntm_mode

   subroutine is_set_arr_type_ntm_mode(structure_in, name)
 
     implicit none
 
     type (type_ntm_mode), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm_mode(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm_mode

   subroutine is_set_type_ntm_mode_evolution(structure_in, name)

     implicit none

     type (type_ntm_mode_evolution), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%w, trim(adjustl(name)) // '%w')
     call is_set_type_float(structure_in%dwdt, trim(adjustl(name)) // '%dwdt')
     call is_set_type_float(structure_in%phase, trim(adjustl(name)) // '%phase')
     call is_set_type_float(structure_in%dphasedt, trim(adjustl(name)) // '%dphasedt')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_float(structure_in%dfrequencydt, trim(adjustl(name)) // '%dfrequencydt')
     call is_set_type_ntm_mode_evolution_island(structure_in%island, trim(adjustl(name)) // '%island')
     call is_set_type_integer(structure_in%n, trim(adjustl(name)) // '%n')
     call is_set_type_integer(structure_in%m, trim(adjustl(name)) // '%m')
     call is_set_type_vecflt_type(structure_in%deltaw_value, trim(adjustl(name)) // '%deltaw_value')
     call is_set_type_vecstring_type(structure_in%deltaw_name, trim(adjustl(name)) // '%deltaw_name')
     call is_set_type_vecflt_type(structure_in%torque_value, trim(adjustl(name)) // '%torque_value')
     call is_set_type_vecstring_type(structure_in%torque_name, trim(adjustl(name)) // '%torque_name')
     call is_set_type_vecflt_type(structure_in%delta_diff, trim(adjustl(name)) // '%delta_diff')
     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')
     call is_set_type_float(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')

   end subroutine is_set_type_ntm_mode_evolution

   subroutine is_set_arr_type_ntm_mode_evolution(structure_in, name)
 
     implicit none
 
     type (type_ntm_mode_evolution), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm_mode_evolution(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm_mode_evolution

   subroutine is_set_type_ntm_mode_evolution_island(structure_in, name)

     implicit none

     type (type_ntm_mode_evolution_island), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_vecflt_type(structure_in%coord_values, trim(adjustl(name)) // '%coord_values')
     call is_set_type_vecstring_type(structure_in%coord_desc, trim(adjustl(name)) // '%coord_desc')

   end subroutine is_set_type_ntm_mode_evolution_island

   subroutine is_set_arr_type_ntm_mode_evolution_island(structure_in, name)
 
     implicit none
 
     type (type_ntm_mode_evolution_island), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm_mode_evolution_island(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm_mode_evolution_island

   subroutine is_set_type_ntm_mode_full_evol(structure_in, name)

     implicit none

     type (type_ntm_mode_full_evol), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%time_evol, trim(adjustl(name)) // '%time_evol')
     call is_set_type_vecflt_type(structure_in%w, trim(adjustl(name)) // '%w')
     call is_set_type_vecflt_type(structure_in%dwdt, trim(adjustl(name)) // '%dwdt')
     call is_set_type_vecflt_type(structure_in%phase, trim(adjustl(name)) // '%phase')
     call is_set_type_vecflt_type(structure_in%dphasedt, trim(adjustl(name)) // '%dphasedt')
     call is_set_type_vecflt_type(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_vecflt_type(structure_in%dfrequencydt, trim(adjustl(name)) // '%dfrequencydt')
     call is_set_type_ntm_mode_full_evol_island(structure_in%island, trim(adjustl(name)) // '%island')
     call is_set_type_integer(structure_in%n, trim(adjustl(name)) // '%n')
     call is_set_type_integer(structure_in%m, trim(adjustl(name)) // '%m')
     call is_set_type_matflt_type(structure_in%deltaw_value, trim(adjustl(name)) // '%deltaw_value')
     call is_set_type_vecstring_type(structure_in%deltaw_name, trim(adjustl(name)) // '%deltaw_name')
     call is_set_type_matflt_type(structure_in%torque_value, trim(adjustl(name)) // '%torque_value')
     call is_set_type_vecstring_type(structure_in%torque_name, trim(adjustl(name)) // '%torque_name')
     call is_set_type_matflt_type(structure_in%delta_diff, trim(adjustl(name)) // '%delta_diff')
     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')

   end subroutine is_set_type_ntm_mode_full_evol

   subroutine is_set_arr_type_ntm_mode_full_evol(structure_in, name)
 
     implicit none
 
     type (type_ntm_mode_full_evol), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm_mode_full_evol(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm_mode_full_evol

   subroutine is_set_type_ntm_mode_full_evol_island(structure_in, name)

     implicit none

     type (type_ntm_mode_full_evol_island), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_matflt_type(structure_in%coord_values, trim(adjustl(name)) // '%coord_values')
     call is_set_type_vecstring_type(structure_in%coord_desc, trim(adjustl(name)) // '%coord_desc')

   end subroutine is_set_type_ntm_mode_full_evol_island

   subroutine is_set_arr_type_ntm_mode_full_evol_island(structure_in, name)
 
     implicit none
 
     type (type_ntm_mode_full_evol_island), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm_mode_full_evol_island(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm_mode_full_evol_island

   subroutine is_set_type_ntm_mode_onset(structure_in, name)

     implicit none

     type (type_ntm_mode_onset), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%w, trim(adjustl(name)) // '%w')
     call is_set_type_float(structure_in%time_onset, trim(adjustl(name)) // '%time_onset')
     call is_set_type_float(structure_in%time_offset, trim(adjustl(name)) // '%time_offset')
     call is_set_type_float(structure_in%phase, trim(adjustl(name)) // '%phase')
     call is_set_type_integer(structure_in%n, trim(adjustl(name)) // '%n')
     call is_set_type_integer(structure_in%m, trim(adjustl(name)) // '%m')
     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')

   end subroutine is_set_type_ntm_mode_onset

   subroutine is_set_arr_type_ntm_mode_onset(structure_in, name)
 
     implicit none
 
     type (type_ntm_mode_onset), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_ntm_mode_onset(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_ntm_mode_onset

   subroutine is_set_type_nuclei(structure_in, name)

     implicit none

     type (type_nuclei), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')

   end subroutine is_set_type_nuclei

   subroutine is_set_arr_type_nuclei(structure_in, name)
 
     implicit none
 
     type (type_nuclei), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_nuclei(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_nuclei

   subroutine is_set_type_objects(structure_in, name)

     implicit none

     type (type_objects), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matint_type(structure_in%boundary, trim(adjustl(name)) // '%boundary')
     call is_set_type_array3dint_type(structure_in%neighbour, trim(adjustl(name)) // '%neighbour')
     call is_set_type_array4dflt_type(structure_in%geo, trim(adjustl(name)) // '%geo')
     call is_set_type_matflt_type(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_objects

   subroutine is_set_arr_type_objects(structure_in, name)
 
     implicit none
 
     type (type_objects), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_objects(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_objects

   subroutine is_set_type_offdiagel(structure_in, name)

     implicit none

     type (type_offdiagel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%d_ni, trim(adjustl(name)) // '%d_ni')
     call is_set_type_matflt_type(structure_in%d_ti, trim(adjustl(name)) // '%d_ti')
     call is_set_type_vecflt_type(structure_in%d_ne, trim(adjustl(name)) // '%d_ne')
     call is_set_type_vecflt_type(structure_in%d_te, trim(adjustl(name)) // '%d_te')
     call is_set_type_vecflt_type(structure_in%d_epar, trim(adjustl(name)) // '%d_epar')
     call is_set_type_vecflt_type(structure_in%d_mtor, trim(adjustl(name)) // '%d_mtor')

   end subroutine is_set_type_offdiagel

   subroutine is_set_arr_type_offdiagel(structure_in, name)
 
     implicit none
 
     type (type_offdiagel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_offdiagel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_offdiagel

   subroutine is_set_type_offdiagion(structure_in, name)

     implicit none

     type (type_offdiagion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%d_ni, trim(adjustl(name)) // '%d_ni')
     call is_set_type_array3dflt_type(structure_in%d_ti, trim(adjustl(name)) // '%d_ti')
     call is_set_type_matflt_type(structure_in%d_ne, trim(adjustl(name)) // '%d_ne')
     call is_set_type_matflt_type(structure_in%d_te, trim(adjustl(name)) // '%d_te')
     call is_set_type_matflt_type(structure_in%d_epar, trim(adjustl(name)) // '%d_epar')
     call is_set_type_matflt_type(structure_in%d_mtor, trim(adjustl(name)) // '%d_mtor')

   end subroutine is_set_type_offdiagion

   subroutine is_set_arr_type_offdiagion(structure_in, name)
 
     implicit none
 
     type (type_offdiagion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_offdiagion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_offdiagion

   subroutine is_set_type_omnigen_surf(structure_in, name)

     implicit none

     type (type_omnigen_surf), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz1D(structure_in%rz, trim(adjustl(name)) // '%rz')
     call is_set_type_vecflt_type(structure_in%s, trim(adjustl(name)) // '%s')

   end subroutine is_set_type_omnigen_surf

   subroutine is_set_arr_type_omnigen_surf(structure_in, name)
 
     implicit none
 
     type (type_omnigen_surf), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_omnigen_surf(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_omnigen_surf

   subroutine is_set_type_orbit_global_param(structure_in, name)

     implicit none

     type (type_orbit_global_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%orbit_type, trim(adjustl(name)) // '%orbit_type')
     call is_set_type_vecflt_type(structure_in%omega_b, trim(adjustl(name)) // '%omega_b')
     call is_set_type_vecflt_type(structure_in%omega_phi, trim(adjustl(name)) // '%omega_phi')
     call is_set_type_vecflt_type(structure_in%omega_c_av, trim(adjustl(name)) // '%omega_c_av')
     call is_set_type_orbit_special_pos(structure_in%special_pos, trim(adjustl(name)) // '%special_pos')

   end subroutine is_set_type_orbit_global_param

   subroutine is_set_arr_type_orbit_global_param(structure_in, name)
 
     implicit none
 
     type (type_orbit_global_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_orbit_global_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_orbit_global_param

   subroutine is_set_type_orbit_midplane(structure_in, name)

     implicit none

     type (type_orbit_midplane), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_orbit_pos(structure_in%outer, trim(adjustl(name)) // '%outer')
     call is_set_type_orbit_pos(structure_in%inner, trim(adjustl(name)) // '%inner')

   end subroutine is_set_type_orbit_midplane

   subroutine is_set_arr_type_orbit_midplane(structure_in, name)
 
     implicit none
 
     type (type_orbit_midplane), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_orbit_midplane(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_orbit_midplane

   subroutine is_set_type_orbit_pos(structure_in, name)

     implicit none

     type (type_orbit_pos), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%theta_b, trim(adjustl(name)) // '%theta_b')

   end subroutine is_set_type_orbit_pos

   subroutine is_set_arr_type_orbit_pos(structure_in, name)
 
     implicit none
 
     type (type_orbit_pos), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_orbit_pos(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_orbit_pos

   subroutine is_set_type_orbit_special_pos(structure_in, name)

     implicit none

     type (type_orbit_special_pos), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_orbit_midplane(structure_in%midplane, trim(adjustl(name)) // '%midplane')
     call is_set_type_orbit_turning_pts(structure_in%turning_pts, trim(adjustl(name)) // '%turning_pts')

   end subroutine is_set_type_orbit_special_pos

   subroutine is_set_arr_type_orbit_special_pos(structure_in, name)
 
     implicit none
 
     type (type_orbit_special_pos), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_orbit_special_pos(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_orbit_special_pos

   subroutine is_set_type_orbit_turning_pts(structure_in, name)

     implicit none

     type (type_orbit_turning_pts), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_orbit_pos(structure_in%upper, trim(adjustl(name)) // '%upper')
     call is_set_type_orbit_pos(structure_in%lower, trim(adjustl(name)) // '%lower')

   end subroutine is_set_type_orbit_turning_pts

   subroutine is_set_arr_type_orbit_turning_pts(structure_in, name)
 
     implicit none
 
     type (type_orbit_turning_pts), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_orbit_turning_pts(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_orbit_turning_pts

   subroutine is_set_type_origin(structure_in, name)

     implicit none

     type (type_origin), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi0D(structure_in%refpos, trim(adjustl(name)) // '%refpos')
     call is_set_type_float(structure_in%alpha, trim(adjustl(name)) // '%alpha')
     call is_set_type_float(structure_in%beta, trim(adjustl(name)) // '%beta')
     call is_set_type_float(structure_in%gamma, trim(adjustl(name)) // '%gamma')

   end subroutine is_set_type_origin

   subroutine is_set_arr_type_origin(structure_in, name)
 
     implicit none
 
     type (type_origin), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_origin(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_origin

   subroutine is_set_type_param(structure_in, name)

     implicit none

     type (type_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%parameters, trim(adjustl(name)) // '%parameters')
     call is_set_type_vecstring_type(structure_in%default_param, trim(adjustl(name)) // '%default_param')
     call is_set_type_vecstring_type(structure_in%schema, trim(adjustl(name)) // '%schema')

   end subroutine is_set_type_param

   subroutine is_set_arr_type_param(structure_in, name)
 
     implicit none
 
     type (type_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_param

   subroutine is_set_type_parameters(structure_in, name)

     implicit none

     type (type_parameters), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_equatorial_plane(structure_in%equatorial, trim(adjustl(name)) // '%equatorial')

   end subroutine is_set_type_parameters

   subroutine is_set_arr_type_parameters(structure_in, name)
 
     implicit none
 
     type (type_parameters), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_parameters(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_parameters

   subroutine is_set_type_pellet(structure_in, name)

     implicit none

     type (type_pellet), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_pellet_shape(structure_in%shape, trim(adjustl(name)) // '%shape')
     call is_set_type_pellet_elements(structure_in%elements, trim(adjustl(name)) // '%elements')
     call is_set_type_pellet_geometry(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_pellet_pathprofiles(structure_in%pathprofiles, trim(adjustl(name)) // '%pathprofiles')
     call is_set_type_pellet_deposition(structure_in%deposition, trim(adjustl(name)) // '%deposition')

   end subroutine is_set_type_pellet

   subroutine is_set_arr_type_pellet(structure_in, name)
 
     implicit none
 
     type (type_pellet), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet

   subroutine is_set_type_pellet_angles(structure_in, name)

     implicit none

     type (type_pellet_angles), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%horizontal, trim(adjustl(name)) // '%horizontal')
     call is_set_type_float(structure_in%vertical, trim(adjustl(name)) // '%vertical')

   end subroutine is_set_type_pellet_angles

   subroutine is_set_arr_type_pellet_angles(structure_in, name)
 
     implicit none
 
     type (type_pellet_angles), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_angles(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_angles

   subroutine is_set_type_pellet_deposition(structure_in, name)

     implicit none

     type (type_pellet_deposition), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_pol, trim(adjustl(name)) // '%rho_pol')
     call is_set_type_vecflt_type(structure_in%delta_ne, trim(adjustl(name)) // '%delta_ne')
     call is_set_type_vecflt_type(structure_in%delta_te, trim(adjustl(name)) // '%delta_te')
     call is_set_type_matflt_type(structure_in%delta_ni, trim(adjustl(name)) // '%delta_ni')
     call is_set_type_matflt_type(structure_in%delta_ti, trim(adjustl(name)) // '%delta_ti')
     call is_set_type_matflt_type(structure_in%delta_vtor, trim(adjustl(name)) // '%delta_vtor')
     call is_set_arr_type_pellet_impurity(structure_in%impurity, trim(adjustl(name)) // '%impurity')

   end subroutine is_set_type_pellet_deposition

   subroutine is_set_arr_type_pellet_deposition(structure_in, name)
 
     implicit none
 
     type (type_pellet_deposition), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_deposition(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_deposition

   subroutine is_set_type_pellet_elements(structure_in, name)

     implicit none

     type (type_pellet_elements), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%nucindex, trim(adjustl(name)) // '%nucindex')
     call is_set_type_vecflt_type(structure_in%density, trim(adjustl(name)) // '%density')
     call is_set_type_vecflt_type(structure_in%fraction, trim(adjustl(name)) // '%fraction')
     call is_set_type_vecflt_type(structure_in%subl_energy, trim(adjustl(name)) // '%subl_energy')

   end subroutine is_set_type_pellet_elements

   subroutine is_set_arr_type_pellet_elements(structure_in, name)
 
     implicit none
 
     type (type_pellet_elements), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_elements(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_elements

   subroutine is_set_type_pellet_geometry(structure_in, name)

     implicit none

     type (type_pellet_geometry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi0D(structure_in%pivot_point, trim(adjustl(name)) // '%pivot_point')
     call is_set_type_rzphi0D(structure_in%second_point, trim(adjustl(name)) // '%second_point')
     call is_set_type_float(structure_in%velocity, trim(adjustl(name)) // '%velocity')
     call is_set_type_pellet_angles(structure_in%angles, trim(adjustl(name)) // '%angles')

   end subroutine is_set_type_pellet_geometry

   subroutine is_set_arr_type_pellet_geometry(structure_in, name)
 
     implicit none
 
     type (type_pellet_geometry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_geometry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_geometry

   subroutine is_set_type_pellet_impurity(structure_in, name)

     implicit none

     type (type_pellet_impurity), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%delta_nz, trim(adjustl(name)) // '%delta_nz')

   end subroutine is_set_type_pellet_impurity

   subroutine is_set_arr_type_pellet_impurity(structure_in, name)
 
     implicit none
 
     type (type_pellet_impurity), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_impurity(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_impurity

   subroutine is_set_type_pellet_pathprofiles(structure_in, name)

     implicit none

     type (type_pellet_pathprofiles), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%distance, trim(adjustl(name)) // '%distance')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_pol, trim(adjustl(name)) // '%rho_pol')
     call is_set_type_vecflt_type(structure_in%velocity, trim(adjustl(name)) // '%velocity')
     call is_set_type_vecflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_vecflt_type(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_vecflt_type(structure_in%abl_rate, trim(adjustl(name)) // '%abl_rate')
     call is_set_type_vecflt_type(structure_in%abl_particles, trim(adjustl(name)) // '%abl_particles')
     call is_set_type_vecflt_type(structure_in%delta_drift, trim(adjustl(name)) // '%delta_drift')
     call is_set_type_rzphi1D(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_pellet_pathprofiles

   subroutine is_set_arr_type_pellet_pathprofiles(structure_in, name)
 
     implicit none
 
     type (type_pellet_pathprofiles), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_pathprofiles(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_pathprofiles

   subroutine is_set_type_pellet_shape(structure_in, name)

     implicit none

     type (type_pellet_shape), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%dimensions, trim(adjustl(name)) // '%dimensions')

   end subroutine is_set_type_pellet_shape

   subroutine is_set_arr_type_pellet_shape(structure_in, name)
 
     implicit none
 
     type (type_pellet_shape), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pellet_shape(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pellet_shape

   subroutine is_set_type_permeability(structure_in, name)

     implicit none

     type (type_permeability), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%b, trim(adjustl(name)) // '%b')
     call is_set_type_matflt_type(structure_in%mur, trim(adjustl(name)) // '%mur')

   end subroutine is_set_type_permeability

   subroutine is_set_arr_type_permeability(structure_in, name)
 
     implicit none
 
     type (type_permeability), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_permeability(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_permeability

   subroutine is_set_type_pfcircuits(structure_in, name)

     implicit none

     type (type_pfcircuits), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_vecstring_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecint_type(structure_in%nnodes, trim(adjustl(name)) // '%nnodes')
     call is_set_type_array3dint_type(structure_in%connections, trim(adjustl(name)) // '%connections')

   end subroutine is_set_type_pfcircuits

   subroutine is_set_arr_type_pfcircuits(structure_in, name)
 
     implicit none
 
     type (type_pfcircuits), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfcircuits(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfcircuits

   subroutine is_set_type_pfcoils(structure_in, name)

     implicit none

     type (type_pfcoils), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_desc_pfcoils(structure_in%desc_pfcoils, trim(adjustl(name)) // '%desc_pfcoils')
     call is_set_type_exp1D(structure_in%coilcurrent, trim(adjustl(name)) // '%coilcurrent')
     call is_set_type_exp1D(structure_in%coilvoltage, trim(adjustl(name)) // '%coilvoltage')
     call is_set_type_float(structure_in%p_cryo, trim(adjustl(name)) // '%p_cryo')
     call is_set_type_vecflt_type(structure_in%p_nh, trim(adjustl(name)) // '%p_nh')

   end subroutine is_set_type_pfcoils

   subroutine is_set_arr_type_pfcoils(structure_in, name)
 
     implicit none
 
     type (type_pfcoils), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfcoils(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfcoils

   subroutine is_set_type_pfelement(structure_in, name)

     implicit none

     type (type_pfelement), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_matflt_type(structure_in%turnsign, trim(adjustl(name)) // '%turnsign')
     call is_set_type_matflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_pfgeometry(structure_in%pfgeometry, trim(adjustl(name)) // '%pfgeometry')

   end subroutine is_set_type_pfelement

   subroutine is_set_arr_type_pfelement(structure_in, name)
 
     implicit none
 
     type (type_pfelement), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfelement(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfelement

   subroutine is_set_type_pfgeometry(structure_in, name)

     implicit none

     type (type_pfgeometry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matint_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_matint_type(structure_in%npoints, trim(adjustl(name)) // '%npoints')
     call is_set_type_rz3D(structure_in%rzcoordinate, trim(adjustl(name)) // '%rzcoordinate')
     call is_set_type_array3dflt_type(structure_in%rzdrdz, trim(adjustl(name)) // '%rzdrdz')

   end subroutine is_set_type_pfgeometry

   subroutine is_set_arr_type_pfgeometry(structure_in, name)
 
     implicit none
 
     type (type_pfgeometry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfgeometry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfgeometry

   subroutine is_set_type_pfpageometry(structure_in, name)

     implicit none

     type (type_pfpageometry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecint_type(structure_in%npoints, trim(adjustl(name)) // '%npoints')
     call is_set_type_rz2D(structure_in%rzcoordinate, trim(adjustl(name)) // '%rzcoordinate')
     call is_set_type_matflt_type(structure_in%rzdrdz, trim(adjustl(name)) // '%rzdrdz')

   end subroutine is_set_type_pfpageometry

   subroutine is_set_arr_type_pfpageometry(structure_in, name)
 
     implicit none
 
     type (type_pfpageometry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfpageometry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfpageometry

   subroutine is_set_type_pfpassive(structure_in, name)

     implicit none

     type (type_pfpassive), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%res, trim(adjustl(name)) // '%res')
     call is_set_type_vecflt_type(structure_in%eta, trim(adjustl(name)) // '%eta')
     call is_set_type_pfpassive_current(structure_in%current, trim(adjustl(name)) // '%current')
     call is_set_type_pfpageometry(structure_in%pfpageometry, trim(adjustl(name)) // '%pfpageometry')

   end subroutine is_set_type_pfpassive

   subroutine is_set_arr_type_pfpassive(structure_in, name)
 
     implicit none
 
     type (type_pfpassive), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfpassive(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfpassive

   subroutine is_set_type_pfpassive_current(structure_in, name)

     implicit none

     type (type_pfpassive_current), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%toroidal, trim(adjustl(name)) // '%toroidal')
     call is_set_type_exp1D(structure_in%poloidal, trim(adjustl(name)) // '%poloidal')

   end subroutine is_set_type_pfpassive_current

   subroutine is_set_arr_type_pfpassive_current(structure_in, name)
 
     implicit none
 
     type (type_pfpassive_current), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfpassive_current(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfpassive_current

   subroutine is_set_type_pfsupplies(structure_in, name)

     implicit none

     type (type_pfsupplies), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_desc_supply(structure_in%desc_supply, trim(adjustl(name)) // '%desc_supply')
     call is_set_type_exp1D(structure_in%voltage, trim(adjustl(name)) // '%voltage')
     call is_set_type_exp1D(structure_in%current, trim(adjustl(name)) // '%current')

   end subroutine is_set_type_pfsupplies

   subroutine is_set_arr_type_pfsupplies(structure_in, name)
 
     implicit none
 
     type (type_pfsupplies), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pfsupplies(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pfsupplies

   subroutine is_set_type_phaseellipse(structure_in, name)

     implicit none

     type (type_phaseellipse), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%invcurvrad, trim(adjustl(name)) // '%invcurvrad')
     call is_set_type_float(structure_in%angle, trim(adjustl(name)) // '%angle')

   end subroutine is_set_type_phaseellipse

   subroutine is_set_arr_type_phaseellipse(structure_in, name)
 
     implicit none
 
     type (type_phaseellipse), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_phaseellipse(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_phaseellipse

   subroutine is_set_type_planecoil(structure_in, name)

     implicit none

     type (type_planecoil), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz1D(structure_in%coordinates, trim(adjustl(name)) // '%coordinates')
     call is_set_type_vecflt_type(structure_in%hlength, trim(adjustl(name)) // '%hlength')
     call is_set_type_vecflt_type(structure_in%radialhwidth, trim(adjustl(name)) // '%radialhwidth')

   end subroutine is_set_type_planecoil

   subroutine is_set_arr_type_planecoil(structure_in, name)
 
     implicit none
 
     type (type_planecoil), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_planecoil(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_planecoil

   subroutine is_set_type_plasmaComplexType(structure_in, name)

     implicit none

     type (type_plasmaComplexType), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%species, trim(adjustl(name)) // '%species')
     call is_set_type_matflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_matflt_type(structure_in%b, trim(adjustl(name)) // '%b')
     call is_set_type_matflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_plasmaComplexType

   subroutine is_set_arr_type_plasmaComplexType(structure_in, name)
 
     implicit none
 
     type (type_plasmaComplexType), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_plasmaComplexType(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_plasmaComplexType

   subroutine is_set_type_plasmaedge(structure_in, name)

     implicit none

     type (type_plasmaedge), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%npoints, trim(adjustl(name)) // '%npoints')
     call is_set_type_vecflt_type(structure_in%distance, trim(adjustl(name)) // '%distance')
     call is_set_type_vecflt_type(structure_in%density, trim(adjustl(name)) // '%density')

   end subroutine is_set_type_plasmaedge

   subroutine is_set_arr_type_plasmaedge(structure_in, name)
 
     implicit none
 
     type (type_plasmaedge), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_plasmaedge(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_plasmaedge

   subroutine is_set_type_pol_decomp(structure_in, name)

     implicit none

     type (type_pol_decomp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%mpol, trim(adjustl(name)) // '%mpol')
     call is_set_type_array3dflt_type(structure_in%e_plus, trim(adjustl(name)) // '%e_plus')
     call is_set_type_array3dflt_type(structure_in%e_plus_ph, trim(adjustl(name)) // '%e_plus_ph')
     call is_set_type_array3dflt_type(structure_in%e_minus, trim(adjustl(name)) // '%e_minus')
     call is_set_type_array3dflt_type(structure_in%e_minus_ph, trim(adjustl(name)) // '%e_minus_ph')
     call is_set_type_array3dflt_type(structure_in%e_norm, trim(adjustl(name)) // '%e_norm')
     call is_set_type_array3dflt_type(structure_in%e_norm_ph, trim(adjustl(name)) // '%e_norm_ph')
     call is_set_type_array3dflt_type(structure_in%e_binorm, trim(adjustl(name)) // '%e_binorm')
     call is_set_type_array3dflt_type(structure_in%e_binorm_ph, trim(adjustl(name)) // '%e_binorm_ph')
     call is_set_type_array3dflt_type(structure_in%e_para, trim(adjustl(name)) // '%e_para')
     call is_set_type_array3dflt_type(structure_in%e_para_ph, trim(adjustl(name)) // '%e_para_ph')
     call is_set_type_array3dflt_type(structure_in%b_norm, trim(adjustl(name)) // '%b_norm')
     call is_set_type_array3dflt_type(structure_in%b_norm_ph, trim(adjustl(name)) // '%b_norm_ph')
     call is_set_type_array3dflt_type(structure_in%b_binorm, trim(adjustl(name)) // '%b_binorm')
     call is_set_type_array3dflt_type(structure_in%b_binorm_ph, trim(adjustl(name)) // '%b_binorm_ph')
     call is_set_type_array3dflt_type(structure_in%b_para, trim(adjustl(name)) // '%b_para')
     call is_set_type_array3dflt_type(structure_in%b_para_ph, trim(adjustl(name)) // '%b_para_ph')
     call is_set_type_array3dflt_type(structure_in%k_perp, trim(adjustl(name)) // '%k_perp')

   end subroutine is_set_type_pol_decomp

   subroutine is_set_arr_type_pol_decomp(structure_in, name)
 
     implicit none
 
     type (type_pol_decomp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_pol_decomp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_pol_decomp

   subroutine is_set_type_polarimetry(structure_in, name)

     implicit none

     type (type_polarimetry), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_msediag_setup_polarimetry(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_exp1D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_polarimetry

   subroutine is_set_arr_type_polarimetry(structure_in, name)
 
     implicit none
 
     type (type_polarimetry), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_polarimetry(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_polarimetry

   subroutine is_set_type_polarization(structure_in, name)

     implicit none

     type (type_polarization), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%epol_p_re, trim(adjustl(name)) // '%epol_p_re')
     call is_set_type_vecflt_type(structure_in%epol_p_im, trim(adjustl(name)) // '%epol_p_im')
     call is_set_type_vecflt_type(structure_in%epol_m_re, trim(adjustl(name)) // '%epol_m_re')
     call is_set_type_vecflt_type(structure_in%epol_m_im, trim(adjustl(name)) // '%epol_m_im')
     call is_set_type_vecflt_type(structure_in%epol_par_re, trim(adjustl(name)) // '%epol_par_re')
     call is_set_type_vecflt_type(structure_in%epol_par_im, trim(adjustl(name)) // '%epol_par_im')

   end subroutine is_set_type_polarization

   subroutine is_set_arr_type_polarization(structure_in, name)
 
     implicit none
 
     type (type_polarization), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_polarization(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_polarization

   subroutine is_set_type_power_conv_component(structure_in, name)

     implicit none

     type (type_power_conv_component), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_float(structure_in%temp_in, trim(adjustl(name)) // '%temp_in')
     call is_set_type_float(structure_in%temp_out, trim(adjustl(name)) // '%temp_out')
     call is_set_type_float(structure_in%press_in, trim(adjustl(name)) // '%press_in')
     call is_set_type_float(structure_in%press_out, trim(adjustl(name)) // '%press_out')
     call is_set_type_float(structure_in%power, trim(adjustl(name)) // '%power')
     call is_set_type_float(structure_in%flow, trim(adjustl(name)) // '%flow')

   end subroutine is_set_type_power_conv_component

   subroutine is_set_arr_type_power_conv_component(structure_in, name)
 
     implicit none
 
     type (type_power_conv_component), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_power_conv_component(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_power_conv_component

   subroutine is_set_type_power_exchange(structure_in, name)

     implicit none

     type (type_power_exchange), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%dep_pow, trim(adjustl(name)) // '%dep_pow')
     call is_set_type_float(structure_in%dep_fw, trim(adjustl(name)) // '%dep_fw')
     call is_set_type_float(structure_in%dep_sg, trim(adjustl(name)) // '%dep_sg')
     call is_set_type_float(structure_in%dep_cp, trim(adjustl(name)) // '%dep_cp')
     call is_set_type_float(structure_in%dep_lp, trim(adjustl(name)) // '%dep_lp')
     call is_set_type_float(structure_in%dep_man, trim(adjustl(name)) // '%dep_man')
     call is_set_type_float(structure_in%dep_pl, trim(adjustl(name)) // '%dep_pl')
     call is_set_type_float(structure_in%rec_fw, trim(adjustl(name)) // '%rec_fw')
     call is_set_type_float(structure_in%rec_sg, trim(adjustl(name)) // '%rec_sg')
     call is_set_type_float(structure_in%rec_cp, trim(adjustl(name)) // '%rec_cp')
     call is_set_type_float(structure_in%pow_dens_fw, trim(adjustl(name)) // '%pow_dens_fw')
     call is_set_type_float(structure_in%pow_dens_bz, trim(adjustl(name)) // '%pow_dens_bz')
     call is_set_type_float(structure_in%pow_dens_bz10, trim(adjustl(name)) // '%pow_dens_bz10')
     call is_set_type_float(structure_in%pow_dens_bp, trim(adjustl(name)) // '%pow_dens_bp')
     call is_set_type_float(structure_in%pow_dens_man, trim(adjustl(name)) // '%pow_dens_man')
     call is_set_type_float(structure_in%pow_dens_sh, trim(adjustl(name)) // '%pow_dens_sh')

   end subroutine is_set_type_power_exchange

   subroutine is_set_arr_type_power_exchange(structure_in, name)
 
     implicit none
 
     type (type_power_exchange), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_power_exchange(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_power_exchange

   subroutine is_set_type_powerflow(structure_in, name)

     implicit none

     type (type_powerflow), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%phi_perp, trim(adjustl(name)) // '%phi_perp')
     call is_set_type_vecflt_type(structure_in%phi_par, trim(adjustl(name)) // '%phi_par')
     call is_set_type_vecflt_type(structure_in%power_e, trim(adjustl(name)) // '%power_e')
     call is_set_type_matflt_type(structure_in%power_i, trim(adjustl(name)) // '%power_i')

   end subroutine is_set_type_powerflow

   subroutine is_set_arr_type_powerflow(structure_in, name)
 
     implicit none
 
     type (type_powerflow), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_powerflow(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_powerflow

   subroutine is_set_type_profiles1d(structure_in, name)

     implicit none

     type (type_profiles1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_coreprofile(structure_in%pe, trim(adjustl(name)) // '%pe')
     call is_set_type_coreprofile(structure_in%dpedt, trim(adjustl(name)) // '%dpedt')
     call is_set_type_coreprofion(structure_in%pi, trim(adjustl(name)) // '%pi')
     call is_set_type_coreprofile(structure_in%pi_tot, trim(adjustl(name)) // '%pi_tot')
     call is_set_type_coreprofile(structure_in%dpi_totdt, trim(adjustl(name)) // '%dpi_totdt')
     call is_set_type_coreprofile(structure_in%pr_th, trim(adjustl(name)) // '%pr_th')
     call is_set_type_coreprofile(structure_in%pr_perp, trim(adjustl(name)) // '%pr_perp')
     call is_set_type_coreprofile(structure_in%pr_parallel, trim(adjustl(name)) // '%pr_parallel')
     call is_set_type_coreprofile(structure_in%jtot, trim(adjustl(name)) // '%jtot')
     call is_set_type_coreprofile(structure_in%jni, trim(adjustl(name)) // '%jni')
     call is_set_type_coreprofile(structure_in%jphi, trim(adjustl(name)) // '%jphi')
     call is_set_type_coreprofile(structure_in%joh, trim(adjustl(name)) // '%joh')
     call is_set_type_coreprofile(structure_in%vloop, trim(adjustl(name)) // '%vloop')
     call is_set_type_coreprofile(structure_in%sigmapar, trim(adjustl(name)) // '%sigmapar')
     call is_set_type_sourceel(structure_in%qoh, trim(adjustl(name)) // '%qoh')
     call is_set_type_coreprofile(structure_in%qei, trim(adjustl(name)) // '%qei')
     call is_set_type_coreprofile(structure_in%eparallel, trim(adjustl(name)) // '%eparallel')
     call is_set_type_coreprofile(structure_in%e_b, trim(adjustl(name)) // '%e_b')
     call is_set_type_coreprofile(structure_in%q, trim(adjustl(name)) // '%q')
     call is_set_type_coreprofile(structure_in%shear, trim(adjustl(name)) // '%shear')
     call is_set_type_coreprofion(structure_in%ns, trim(adjustl(name)) // '%ns')
     call is_set_type_coreprofion(structure_in%mtor, trim(adjustl(name)) // '%mtor')
     call is_set_type_coreprofion(structure_in%wtor, trim(adjustl(name)) // '%wtor')
     call is_set_type_coreprofion(structure_in%vpol, trim(adjustl(name)) // '%vpol')
     call is_set_type_coreprofile(structure_in%zeff, trim(adjustl(name)) // '%zeff')
     call is_set_type_coreprofile(structure_in%bpol, trim(adjustl(name)) // '%bpol')
     call is_set_type_coreprofile(structure_in%dvprimedt, trim(adjustl(name)) // '%dvprimedt')

   end subroutine is_set_type_profiles1d

   subroutine is_set_arr_type_profiles1d(structure_in, name)
 
     implicit none
 
     type (type_profiles1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_profiles1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_profiles1d

   subroutine is_set_type_profiles_1d(structure_in, name)

     implicit none

     type (type_profiles_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%pressure, trim(adjustl(name)) // '%pressure')
     call is_set_type_vecflt_type(structure_in%F_dia, trim(adjustl(name)) // '%F_dia')
     call is_set_type_vecflt_type(structure_in%pprime, trim(adjustl(name)) // '%pprime')
     call is_set_type_vecflt_type(structure_in%ffprime, trim(adjustl(name)) // '%ffprime')
     call is_set_type_vecflt_type(structure_in%jphi, trim(adjustl(name)) // '%jphi')
     call is_set_type_vecflt_type(structure_in%jparallel, trim(adjustl(name)) // '%jparallel')
     call is_set_type_vecflt_type(structure_in%q, trim(adjustl(name)) // '%q')
     call is_set_type_vecflt_type(structure_in%shear, trim(adjustl(name)) // '%shear')
     call is_set_type_vecflt_type(structure_in%r_inboard, trim(adjustl(name)) // '%r_inboard')
     call is_set_type_vecflt_type(structure_in%r_outboard, trim(adjustl(name)) // '%r_outboard')
     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%dpsidrho_tor, trim(adjustl(name)) // '%dpsidrho_tor')
     call is_set_type_vecflt_type(structure_in%rho_vol, trim(adjustl(name)) // '%rho_vol')
     call is_set_type_vecflt_type(structure_in%beta_pol, trim(adjustl(name)) // '%beta_pol')
     call is_set_type_vecflt_type(structure_in%li, trim(adjustl(name)) // '%li')
     call is_set_type_vecflt_type(structure_in%elongation, trim(adjustl(name)) // '%elongation')
     call is_set_type_vecflt_type(structure_in%tria_upper, trim(adjustl(name)) // '%tria_upper')
     call is_set_type_vecflt_type(structure_in%tria_lower, trim(adjustl(name)) // '%tria_lower')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%vprime, trim(adjustl(name)) // '%vprime')
     call is_set_type_vecflt_type(structure_in%dvdrho, trim(adjustl(name)) // '%dvdrho')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%aprime, trim(adjustl(name)) // '%aprime')
     call is_set_type_vecflt_type(structure_in%surface, trim(adjustl(name)) // '%surface')
     call is_set_type_vecflt_type(structure_in%ftrap, trim(adjustl(name)) // '%ftrap')
     call is_set_type_vecflt_type(structure_in%gm1, trim(adjustl(name)) // '%gm1')
     call is_set_type_vecflt_type(structure_in%gm2, trim(adjustl(name)) // '%gm2')
     call is_set_type_vecflt_type(structure_in%gm3, trim(adjustl(name)) // '%gm3')
     call is_set_type_vecflt_type(structure_in%gm4, trim(adjustl(name)) // '%gm4')
     call is_set_type_vecflt_type(structure_in%gm5, trim(adjustl(name)) // '%gm5')
     call is_set_type_vecflt_type(structure_in%gm6, trim(adjustl(name)) // '%gm6')
     call is_set_type_vecflt_type(structure_in%gm7, trim(adjustl(name)) // '%gm7')
     call is_set_type_vecflt_type(structure_in%gm8, trim(adjustl(name)) // '%gm8')
     call is_set_type_vecflt_type(structure_in%gm9, trim(adjustl(name)) // '%gm9')
     call is_set_type_vecflt_type(structure_in%b_av, trim(adjustl(name)) // '%b_av')
     call is_set_type_vecflt_type(structure_in%b_min, trim(adjustl(name)) // '%b_min')
     call is_set_type_vecflt_type(structure_in%b_max, trim(adjustl(name)) // '%b_max')
     call is_set_type_vecflt_type(structure_in%omega, trim(adjustl(name)) // '%omega')
     call is_set_type_vecflt_type(structure_in%omegaprime, trim(adjustl(name)) // '%omegaprime')
     call is_set_type_vecflt_type(structure_in%mach_a, trim(adjustl(name)) // '%mach_a')
     call is_set_type_vecflt_type(structure_in%phi_flow, trim(adjustl(name)) // '%phi_flow')
     call is_set_type_vecflt_type(structure_in%s_flow, trim(adjustl(name)) // '%s_flow')
     call is_set_type_vecflt_type(structure_in%h_flow, trim(adjustl(name)) // '%h_flow')
     call is_set_type_vecflt_type(structure_in%rho_mass, trim(adjustl(name)) // '%rho_mass')

   end subroutine is_set_type_profiles_1d

   subroutine is_set_arr_type_profiles_1d(structure_in, name)
 
     implicit none
 
     type (type_profiles_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_profiles_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_profiles_1d

   subroutine is_set_type_psi(structure_in, name)

     implicit none

     type (type_psi), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%ddrho, trim(adjustl(name)) // '%ddrho')
     call is_set_type_vecflt_type(structure_in%d2drho2, trim(adjustl(name)) // '%d2drho2')
     call is_set_type_vecflt_type(structure_in%ddt_rhotorn, trim(adjustl(name)) // '%ddt_rhotorn')
     call is_set_type_vecflt_type(structure_in%ddt_phi, trim(adjustl(name)) // '%ddt_phi')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')
     call is_set_type_boundary(structure_in%boundary, trim(adjustl(name)) // '%boundary')
     call is_set_type_jni(structure_in%jni, trim(adjustl(name)) // '%jni')
     call is_set_type_coreprofile(structure_in%sigma_par, trim(adjustl(name)) // '%sigma_par')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_psi

   subroutine is_set_arr_type_psi(structure_in, name)
 
     implicit none
 
     type (type_psi), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_psi(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_psi

   subroutine is_set_type_putinfo(structure_in, name)

     implicit none

     type (type_putinfo), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%putmethod, trim(adjustl(name)) // '%putmethod')
     call is_set_type_vecstring_type(structure_in%putaccess, trim(adjustl(name)) // '%putaccess')
     call is_set_type_vecstring_type(structure_in%putlocation, trim(adjustl(name)) // '%putlocation')
     call is_set_type_vecstring_type(structure_in%rights, trim(adjustl(name)) // '%rights')

   end subroutine is_set_type_putinfo

   subroutine is_set_arr_type_putinfo(structure_in, name)
 
     implicit none
 
     type (type_putinfo), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_putinfo(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_putinfo

   subroutine is_set_type_q(structure_in, name)

     implicit none

     type (type_q), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%qvalue, trim(adjustl(name)) // '%qvalue')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_integer(structure_in%exact, trim(adjustl(name)) // '%exact')
     call is_set_type_vecflt_type(structure_in%weight, trim(adjustl(name)) // '%weight')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_vecflt_type(structure_in%calculated, trim(adjustl(name)) // '%calculated')
     call is_set_type_vecflt_type(structure_in%chi2, trim(adjustl(name)) // '%chi2')

   end subroutine is_set_type_q

   subroutine is_set_arr_type_q(structure_in, name)
 
     implicit none
 
     type (type_q), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_q(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_q

   subroutine is_set_type_reacprodType(structure_in, name)

     implicit none

     type (type_reacprodType), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_arr_type_amns_constituentType(structure_in%constituents, trim(adjustl(name)) // '%constituents')
     call is_set_type_identifier(structure_in%role, trim(adjustl(name)) // '%role')
     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_integer(structure_in%relative, trim(adjustl(name)) // '%relative')
     call is_set_type_float(structure_in%za, trim(adjustl(name)) // '%za')
     call is_set_type_float(structure_in%multiplicity, trim(adjustl(name)) // '%multiplicity')
     call is_set_type_vecint_type(structure_in%metastable, trim(adjustl(name)) // '%metastable')
     call is_set_type_vecstring_type(structure_in%metastable_label, trim(adjustl(name)) // '%metastable_label')

   end subroutine is_set_type_reacprodType

   subroutine is_set_arr_type_reacprodType(structure_in, name)
 
     implicit none
 
     type (type_reacprodType), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reacprodType(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reacprodType

   subroutine is_set_type_react(structure_in, name)

     implicit none

     type (type_react), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%he_fr, trim(adjustl(name)) // '%he_fr')
     call is_set_type_float(structure_in%lp_fr, trim(adjustl(name)) // '%lp_fr')
     call is_set_type_float(structure_in%he_dp, trim(adjustl(name)) // '%he_dp')
     call is_set_type_float(structure_in%lipb_dp, trim(adjustl(name)) // '%lipb_dp')

   end subroutine is_set_type_react

   subroutine is_set_arr_type_react(structure_in, name)
 
     implicit none
 
     type (type_react), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_react(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_react

   subroutine is_set_type_rectanglexyz(structure_in, name)

     implicit none

     type (type_rectanglexyz), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_xyz0D(structure_in%point01, trim(adjustl(name)) // '%point01')
     call is_set_type_xyz0D(structure_in%point11, trim(adjustl(name)) // '%point11')
     call is_set_type_xyz0D(structure_in%point10, trim(adjustl(name)) // '%point10')

   end subroutine is_set_type_rectanglexyz

   subroutine is_set_arr_type_rectanglexyz(structure_in, name)
 
     implicit none
 
     type (type_rectanglexyz), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rectanglexyz(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rectanglexyz

   subroutine is_set_type_recycling_neutrals(structure_in, name)

     implicit none

     type (type_recycling_neutrals), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%particles, trim(adjustl(name)) // '%particles')
     call is_set_type_vecflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_recycling_neutrals

   subroutine is_set_arr_type_recycling_neutrals(structure_in, name)
 
     implicit none
 
     type (type_recycling_neutrals), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_recycling_neutrals(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_recycling_neutrals

   subroutine is_set_type_reduced(structure_in, name)

     implicit none

     type (type_reduced), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_reduced

   subroutine is_set_arr_type_reduced(structure_in, name)
 
     implicit none
 
     type (type_reduced), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reduced(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reduced

   subroutine is_set_type_refl_receive(structure_in, name)

     implicit none

     type (type_refl_receive), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_t_series_real(structure_in%raw_signal, trim(adjustl(name)) // '%raw_signal')
     call is_set_type_t_series_real(structure_in%io_signal, trim(adjustl(name)) // '%io_signal')
     call is_set_type_t_series_cplx(structure_in%iq_receiver, trim(adjustl(name)) // '%iq_receiver')
     call is_set_type_integer(structure_in%antenna_ind, trim(adjustl(name)) // '%antenna_ind')

   end subroutine is_set_type_refl_receive

   subroutine is_set_arr_type_refl_receive(structure_in, name)
 
     implicit none
 
     type (type_refl_receive), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_refl_receive(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_refl_receive

   subroutine is_set_type_reflectometry_antennas(structure_in, name)

     implicit none

     type (type_reflectometry_antennas), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_origin(structure_in%origin, trim(adjustl(name)) // '%origin')
     call is_set_type_reflectometry_radfield(structure_in%radfield, trim(adjustl(name)) // '%radfield')
     call is_set_type_float(structure_in%geometry, trim(adjustl(name)) // '%geometry')
     call is_set_type_launchsignal(structure_in%launchsignal, trim(adjustl(name)) // '%launchsignal')

   end subroutine is_set_type_reflectometry_antennas

   subroutine is_set_arr_type_reflectometry_antennas(structure_in, name)
 
     implicit none
 
     type (type_reflectometry_antennas), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reflectometry_antennas(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reflectometry_antennas

   subroutine is_set_type_reflectometry_radfield(structure_in, name)

     implicit none

     type (type_reflectometry_radfield), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_arr_type_reflectometry_radfield_gaussian(structure_in%gaussian, trim(adjustl(name)) // '%gaussian')
     call is_set_arr_type_reflectometry_radifield_efield(structure_in%efield, trim(adjustl(name)) // '%efield')

   end subroutine is_set_type_reflectometry_radfield

   subroutine is_set_arr_type_reflectometry_radfield(structure_in, name)
 
     implicit none
 
     type (type_reflectometry_radfield), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reflectometry_radfield(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reflectometry_radfield

   subroutine is_set_type_reflectometry_radfield_gaussian(structure_in, name)

     implicit none

     type (type_reflectometry_radfield_gaussian), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_simp_apert(structure_in%aperture, trim(adjustl(name)) // '%aperture')
     call is_set_type_vecflt_type(structure_in%waistsize, trim(adjustl(name)) // '%waistsize')
     call is_set_type_vecflt_type(structure_in%waistzpos, trim(adjustl(name)) // '%waistzpos')
     call is_set_type_vecflt_type(structure_in%tiltangle, trim(adjustl(name)) // '%tiltangle')
     call is_set_type_vecflt_type(structure_in%polar_angle, trim(adjustl(name)) // '%polar_angle')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')

   end subroutine is_set_type_reflectometry_radfield_gaussian

   subroutine is_set_arr_type_reflectometry_radfield_gaussian(structure_in, name)
 
     implicit none
 
     type (type_reflectometry_radfield_gaussian), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reflectometry_radfield_gaussian(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reflectometry_radfield_gaussian

   subroutine is_set_type_reflectometry_radifield_efield(structure_in, name)

     implicit none

     type (type_reflectometry_radifield_efield), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_reggrid(structure_in%grid2d, trim(adjustl(name)) // '%grid2d')
     call is_set_type_matcplx_type(structure_in%e1, trim(adjustl(name)) // '%e1')
     call is_set_type_matcplx_type(structure_in%e2, trim(adjustl(name)) // '%e2')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')

   end subroutine is_set_type_reflectometry_radifield_efield

   subroutine is_set_arr_type_reflectometry_radifield_efield(structure_in, name)
 
     implicit none
 
     type (type_reflectometry_radifield_efield), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reflectometry_radifield_efield(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reflectometry_radifield_efield

   subroutine is_set_type_reggrid(structure_in, name)

     implicit none

     type (type_reggrid), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%dim1, trim(adjustl(name)) // '%dim1')
     call is_set_type_vecflt_type(structure_in%dim2, trim(adjustl(name)) // '%dim2')

   end subroutine is_set_type_reggrid

   subroutine is_set_arr_type_reggrid(structure_in, name)
 
     implicit none
 
     type (type_reggrid), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_reggrid(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_reggrid

   subroutine is_set_type_rfameasure(structure_in, name)

     implicit none

     type (type_rfameasure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%ti, trim(adjustl(name)) // '%ti')

   end subroutine is_set_type_rfameasure

   subroutine is_set_arr_type_rfameasure(structure_in, name)
 
     implicit none
 
     type (type_rfameasure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rfameasure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rfameasure

   subroutine is_set_type_rfasetup(structure_in, name)

     implicit none

     type (type_rfasetup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1Dexp(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_rfasetup

   subroutine is_set_arr_type_rfasetup(structure_in, name)
 
     implicit none
 
     type (type_rfasetup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rfasetup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rfasetup

   subroutine is_set_type_rfbeam(structure_in, name)

     implicit none

     type (type_rfbeam), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_spot(structure_in%spot, trim(adjustl(name)) // '%spot')
     call is_set_type_phaseellipse(structure_in%phaseellipse, trim(adjustl(name)) // '%phaseellipse')

   end subroutine is_set_type_rfbeam

   subroutine is_set_arr_type_rfbeam(structure_in, name)
 
     implicit none
 
     type (type_rfbeam), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rfbeam(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rfbeam

   subroutine is_set_type_rz0D(structure_in, name)

     implicit none

     type (type_rz0D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_float(structure_in%z, trim(adjustl(name)) // '%z')

   end subroutine is_set_type_rz0D

   subroutine is_set_arr_type_rz0D(structure_in, name)
 
     implicit none
 
     type (type_rz0D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rz0D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rz0D

   subroutine is_set_type_rz1D(structure_in, name)

     implicit none

     type (type_rz1D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')

   end subroutine is_set_type_rz1D

   subroutine is_set_arr_type_rz1D(structure_in, name)
 
     implicit none
 
     type (type_rz1D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rz1D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rz1D

   subroutine is_set_type_rz1D_npoints(structure_in, name)

     implicit none

     type (type_rz1D_npoints), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_integer(structure_in%npoints, trim(adjustl(name)) // '%npoints')

   end subroutine is_set_type_rz1D_npoints

   subroutine is_set_arr_type_rz1D_npoints(structure_in, name)
 
     implicit none
 
     type (type_rz1D_npoints), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rz1D_npoints(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rz1D_npoints

   subroutine is_set_type_rz1Dexp(structure_in, name)

     implicit none

     type (type_rz1Dexp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')

   end subroutine is_set_type_rz1Dexp

   subroutine is_set_arr_type_rz1Dexp(structure_in, name)
 
     implicit none
 
     type (type_rz1Dexp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rz1Dexp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rz1Dexp

   subroutine is_set_type_rz2D(structure_in, name)

     implicit none

     type (type_rz2D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')

   end subroutine is_set_type_rz2D

   subroutine is_set_arr_type_rz2D(structure_in, name)
 
     implicit none
 
     type (type_rz2D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rz2D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rz2D

   subroutine is_set_type_rz3D(structure_in, name)

     implicit none

     type (type_rz3D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_array3dflt_type(structure_in%z, trim(adjustl(name)) // '%z')

   end subroutine is_set_type_rz3D

   subroutine is_set_arr_type_rz3D(structure_in, name)
 
     implicit none
 
     type (type_rz3D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rz3D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rz3D

   subroutine is_set_type_rzphi0D(structure_in, name)

     implicit none

     type (type_rzphi0D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_float(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_float(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_rzphi0D

   subroutine is_set_arr_type_rzphi0D(structure_in, name)
 
     implicit none
 
     type (type_rzphi0D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphi0D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphi0D

   subroutine is_set_type_rzphi1D(structure_in, name)

     implicit none

     type (type_rzphi1D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_rzphi1D

   subroutine is_set_arr_type_rzphi1D(structure_in, name)
 
     implicit none
 
     type (type_rzphi1D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphi1D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphi1D

   subroutine is_set_type_rzphi1Dexp(structure_in, name)

     implicit none

     type (type_rzphi1Dexp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_exp1D(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_exp1D(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_rzphi1Dexp

   subroutine is_set_arr_type_rzphi1Dexp(structure_in, name)
 
     implicit none
 
     type (type_rzphi1Dexp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphi1Dexp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphi1Dexp

   subroutine is_set_type_rzphi1Dexperimental(structure_in, name)

     implicit none

     type (type_rzphi1Dexperimental), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_rzphi1Dexperimental

   subroutine is_set_arr_type_rzphi1Dexperimental(structure_in, name)
 
     implicit none
 
     type (type_rzphi1Dexperimental), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphi1Dexperimental(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphi1Dexperimental

   subroutine is_set_type_rzphi2D(structure_in, name)

     implicit none

     type (type_rzphi2D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_matflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_rzphi2D

   subroutine is_set_arr_type_rzphi2D(structure_in, name)
 
     implicit none
 
     type (type_rzphi2D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphi2D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphi2D

   subroutine is_set_type_rzphi3D(structure_in, name)

     implicit none

     type (type_rzphi3D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_array3dflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_array3dflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_rzphi3D

   subroutine is_set_arr_type_rzphi3D(structure_in, name)
 
     implicit none
 
     type (type_rzphi3D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphi3D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphi3D

   subroutine is_set_type_rzphidrdzdphi1D(structure_in, name)

     implicit none

     type (type_rzphidrdzdphi1D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%dr, trim(adjustl(name)) // '%dr')
     call is_set_type_vecflt_type(structure_in%dz, trim(adjustl(name)) // '%dz')
     call is_set_type_vecflt_type(structure_in%dphi, trim(adjustl(name)) // '%dphi')

   end subroutine is_set_type_rzphidrdzdphi1D

   subroutine is_set_arr_type_rzphidrdzdphi1D(structure_in, name)
 
     implicit none
 
     type (type_rzphidrdzdphi1D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_rzphidrdzdphi1D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_rzphidrdzdphi1D

   subroutine is_set_type_sawteeth_diags(structure_in, name)

     implicit none

     type (type_sawteeth_diags), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%shear1, trim(adjustl(name)) // '%shear1')
     call is_set_type_float(structure_in%rhotorn_q1, trim(adjustl(name)) // '%rhotorn_q1')
     call is_set_type_float(structure_in%rhotorn_inv, trim(adjustl(name)) // '%rhotorn_inv')
     call is_set_type_float(structure_in%rhotorn_mix, trim(adjustl(name)) // '%rhotorn_mix')

   end subroutine is_set_type_sawteeth_diags

   subroutine is_set_arr_type_sawteeth_diags(structure_in, name)
 
     implicit none
 
     type (type_sawteeth_diags), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sawteeth_diags(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sawteeth_diags

   subroutine is_set_type_sawteeth_profiles1d(structure_in, name)

     implicit none

     type (type_sawteeth_profiles1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_matflt_type(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_vecflt_type(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_matflt_type(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%psistar, trim(adjustl(name)) // '%psistar')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%q, trim(adjustl(name)) // '%q')

   end subroutine is_set_type_sawteeth_profiles1d

   subroutine is_set_arr_type_sawteeth_profiles1d(structure_in, name)
 
     implicit none
 
     type (type_sawteeth_profiles1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sawteeth_profiles1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sawteeth_profiles1d

   subroutine is_set_type_scenario_centre(structure_in, name)

     implicit none

     type (type_scenario_centre), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%te0, trim(adjustl(name)) // '%te0')
     call is_set_type_scenario_ref(structure_in%ti0, trim(adjustl(name)) // '%ti0')
     call is_set_type_scenario_ref(structure_in%ne0, trim(adjustl(name)) // '%ne0')
     call is_set_type_scenario_ref(structure_in%ni0, trim(adjustl(name)) // '%ni0')
     call is_set_type_scenario_ref(structure_in%shift0, trim(adjustl(name)) // '%shift0')
     call is_set_type_scenario_ref(structure_in%psi0, trim(adjustl(name)) // '%psi0')
     call is_set_type_scenario_ref(structure_in%phi0, trim(adjustl(name)) // '%phi0')
     call is_set_type_scenario_ref(structure_in%q0, trim(adjustl(name)) // '%q0')
     call is_set_type_scenario_ref(structure_in%Rmag, trim(adjustl(name)) // '%Rmag')
     call is_set_type_scenario_ref(structure_in%Zmag, trim(adjustl(name)) // '%Zmag')
     call is_set_type_scenario_ref(structure_in%vtor_0, trim(adjustl(name)) // '%vtor_0')

   end subroutine is_set_type_scenario_centre

   subroutine is_set_arr_type_scenario_centre(structure_in, name)
 
     implicit none
 
     type (type_scenario_centre), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_centre(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_centre

   subroutine is_set_type_scenario_composition(structure_in, name)

     implicit none

     type (type_scenario_composition), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_vecflt_type(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_vecflt_type(structure_in%zion, trim(adjustl(name)) // '%zion')
     call is_set_type_vecint_type(structure_in%imp_flag, trim(adjustl(name)) // '%imp_flag')
     call is_set_type_vecint_type(structure_in%rot_imp_flag, trim(adjustl(name)) // '%rot_imp_flag')
     call is_set_type_vecflt_type(structure_in%pellet_amn, trim(adjustl(name)) // '%pellet_amn')
     call is_set_type_vecflt_type(structure_in%pellet_zn, trim(adjustl(name)) // '%pellet_zn')
     call is_set_type_vecflt_type(structure_in%nbi_amn, trim(adjustl(name)) // '%nbi_amn')
     call is_set_type_vecflt_type(structure_in%nbi_zn, trim(adjustl(name)) // '%nbi_zn')

   end subroutine is_set_type_scenario_composition

   subroutine is_set_arr_type_scenario_composition(structure_in, name)
 
     implicit none
 
     type (type_scenario_composition), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_composition(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_composition

   subroutine is_set_type_scenario_configuration(structure_in, name)

     implicit none

     type (type_scenario_configuration), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_int(structure_in%config, trim(adjustl(name)) // '%config')
     call is_set_type_vecstring_type(structure_in%lmode_sc, trim(adjustl(name)) // '%lmode_sc')
     call is_set_type_vecstring_type(structure_in%hmode_sc, trim(adjustl(name)) // '%hmode_sc')
     call is_set_type_vecstring_type(structure_in%core_sc, trim(adjustl(name)) // '%core_sc')
     call is_set_type_vecstring_type(structure_in%pedestal_sc, trim(adjustl(name)) // '%pedestal_sc')
     call is_set_type_vecstring_type(structure_in%helium_sc, trim(adjustl(name)) // '%helium_sc')
     call is_set_type_vecstring_type(structure_in%impurity_sc, trim(adjustl(name)) // '%impurity_sc')
     call is_set_type_vecstring_type(structure_in%l2h_sc, trim(adjustl(name)) // '%l2h_sc')
     call is_set_type_vecstring_type(structure_in%tor_rot_sc, trim(adjustl(name)) // '%tor_rot_sc')
     call is_set_type_vecstring_type(structure_in%wall_mat, trim(adjustl(name)) // '%wall_mat')
     call is_set_type_vecstring_type(structure_in%evap_mat, trim(adjustl(name)) // '%evap_mat')
     call is_set_type_vecstring_type(structure_in%lim_mat, trim(adjustl(name)) // '%lim_mat')
     call is_set_type_vecstring_type(structure_in%div_mat, trim(adjustl(name)) // '%div_mat')
     call is_set_type_vecstring_type(structure_in%coordinate, trim(adjustl(name)) // '%coordinate')
     call is_set_type_scenario_ref(structure_in%ecrh_freq, trim(adjustl(name)) // '%ecrh_freq')
     call is_set_type_scenario_ref(structure_in%ecrh_loc, trim(adjustl(name)) // '%ecrh_loc')
     call is_set_type_scenario_int(structure_in%ecrh_mode, trim(adjustl(name)) // '%ecrh_mode')
     call is_set_type_scenario_ref(structure_in%ecrh_tor_ang, trim(adjustl(name)) // '%ecrh_tor_ang')
     call is_set_type_scenario_ref(structure_in%ecrh_pol_ang, trim(adjustl(name)) // '%ecrh_pol_ang')
     call is_set_type_scenario_int(structure_in%ecrh_harm, trim(adjustl(name)) // '%ecrh_harm')
     call is_set_type_scenario_ref(structure_in%enbi, trim(adjustl(name)) // '%enbi')
     call is_set_type_scenario_ref(structure_in%r_nbi, trim(adjustl(name)) // '%r_nbi')
     call is_set_type_scenario_int(structure_in%grad_b_drift, trim(adjustl(name)) // '%grad_b_drift')
     call is_set_type_scenario_ref(structure_in%icrh_freq, trim(adjustl(name)) // '%icrh_freq')
     call is_set_type_vecstring_type(structure_in%icrh_scheme, trim(adjustl(name)) // '%icrh_scheme')
     call is_set_type_scenario_ref(structure_in%icrh_phase, trim(adjustl(name)) // '%icrh_phase')
     call is_set_type_scenario_ref(structure_in%LH_freq, trim(adjustl(name)) // '%LH_freq')
     call is_set_type_scenario_ref(structure_in%LH_npar, trim(adjustl(name)) // '%LH_npar')
     call is_set_type_scenario_ref(structure_in%pellet_ang, trim(adjustl(name)) // '%pellet_ang')
     call is_set_type_scenario_ref(structure_in%pellet_v, trim(adjustl(name)) // '%pellet_v')
     call is_set_type_scenario_ref(structure_in%pellet_nba, trim(adjustl(name)) // '%pellet_nba')

   end subroutine is_set_type_scenario_configuration

   subroutine is_set_arr_type_scenario_configuration(structure_in, name)
 
     implicit none
 
     type (type_scenario_configuration), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_configuration(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_configuration

   subroutine is_set_type_scenario_confinement(structure_in, name)

     implicit none

     type (type_scenario_confinement), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%tau_e, trim(adjustl(name)) // '%tau_e')
     call is_set_type_scenario_ref(structure_in%tau_l_sc, trim(adjustl(name)) // '%tau_l_sc')
     call is_set_type_scenario_ref(structure_in%tau_h_sc, trim(adjustl(name)) // '%tau_h_sc')
     call is_set_type_scenario_ref(structure_in%tau_he, trim(adjustl(name)) // '%tau_he')
     call is_set_type_scenario_ref(structure_in%tau_e_ee, trim(adjustl(name)) // '%tau_e_ee')
     call is_set_type_scenario_ref(structure_in%tau_e_ii, trim(adjustl(name)) // '%tau_e_ii')
     call is_set_type_scenario_ref(structure_in%tau_e_ei, trim(adjustl(name)) // '%tau_e_ei')
     call is_set_type_scenario_ref(structure_in%tau_cur_diff, trim(adjustl(name)) // '%tau_cur_diff')
     call is_set_type_scenario_ref(structure_in%tau_i_rol, trim(adjustl(name)) // '%tau_i_rol')

   end subroutine is_set_type_scenario_confinement

   subroutine is_set_arr_type_scenario_confinement(structure_in, name)
 
     implicit none
 
     type (type_scenario_confinement), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_confinement(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_confinement

   subroutine is_set_type_scenario_currents(structure_in, name)

     implicit none

     type (type_scenario_currents), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%RR, trim(adjustl(name)) // '%RR')
     call is_set_type_scenario_ref(structure_in%i_align, trim(adjustl(name)) // '%i_align')
     call is_set_type_scenario_ref(structure_in%i_boot, trim(adjustl(name)) // '%i_boot')
     call is_set_type_scenario_ref(structure_in%i_cd_tot, trim(adjustl(name)) // '%i_cd_tot')
     call is_set_type_scenario_ref(structure_in%i_eccd, trim(adjustl(name)) // '%i_eccd')
     call is_set_type_scenario_ref(structure_in%i_fast_ion, trim(adjustl(name)) // '%i_fast_ion')
     call is_set_type_scenario_ref(structure_in%i_fwcd, trim(adjustl(name)) // '%i_fwcd')
     call is_set_type_scenario_ref(structure_in%i_lhcd, trim(adjustl(name)) // '%i_lhcd')
     call is_set_type_scenario_ref(structure_in%i_nbicd, trim(adjustl(name)) // '%i_nbicd')
     call is_set_type_scenario_ref(structure_in%i_ni_tot, trim(adjustl(name)) // '%i_ni_tot')
     call is_set_type_scenario_ref(structure_in%i_ohm, trim(adjustl(name)) // '%i_ohm')
     call is_set_type_scenario_ref(structure_in%i_par, trim(adjustl(name)) // '%i_par')
     call is_set_type_scenario_ref(structure_in%i_runaway, trim(adjustl(name)) // '%i_runaway')
     call is_set_type_scenario_ref(structure_in%v_loop, trim(adjustl(name)) // '%v_loop')
     call is_set_type_scenario_ref(structure_in%v_meas, trim(adjustl(name)) // '%v_meas')

   end subroutine is_set_type_scenario_currents

   subroutine is_set_arr_type_scenario_currents(structure_in, name)
 
     implicit none
 
     type (type_scenario_currents), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_currents(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_currents

   subroutine is_set_type_scenario_edge(structure_in, name)

     implicit none

     type (type_scenario_edge), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%te_edge, trim(adjustl(name)) // '%te_edge')
     call is_set_type_scenario_ref(structure_in%ti_edge, trim(adjustl(name)) // '%ti_edge')
     call is_set_type_scenario_ref(structure_in%ne_edge, trim(adjustl(name)) // '%ne_edge')
     call is_set_type_scenario_ref(structure_in%ni_edge, trim(adjustl(name)) // '%ni_edge')
     call is_set_type_scenario_ref(structure_in%psi_edge, trim(adjustl(name)) // '%psi_edge')
     call is_set_type_scenario_ref(structure_in%phi_edge, trim(adjustl(name)) // '%phi_edge')
     call is_set_type_scenario_ref(structure_in%rho_edge, trim(adjustl(name)) // '%rho_edge')
     call is_set_type_scenario_ref(structure_in%drho_edge_dt, trim(adjustl(name)) // '%drho_edge_dt')
     call is_set_type_scenario_ref(structure_in%q_edge, trim(adjustl(name)) // '%q_edge')
     call is_set_type_scenario_ref(structure_in%neutral_flux, trim(adjustl(name)) // '%neutral_flux')
     call is_set_type_scenario_ref(structure_in%phi_plasma, trim(adjustl(name)) // '%phi_plasma')
     call is_set_type_scenario_ref(structure_in%vtor_edge, trim(adjustl(name)) // '%vtor_edge')

   end subroutine is_set_type_scenario_edge

   subroutine is_set_arr_type_scenario_edge(structure_in, name)
 
     implicit none
 
     type (type_scenario_edge), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_edge(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_edge

   subroutine is_set_type_scenario_energy(structure_in, name)

     implicit none

     type (type_scenario_energy), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%w_tot, trim(adjustl(name)) // '%w_tot')
     call is_set_type_scenario_ref(structure_in%w_b_pol, trim(adjustl(name)) // '%w_b_pol')
     call is_set_type_scenario_ref(structure_in%w_dia, trim(adjustl(name)) // '%w_dia')
     call is_set_type_scenario_ref(structure_in%dwdia_dt, trim(adjustl(name)) // '%dwdia_dt')
     call is_set_type_scenario_ref(structure_in%w_b_tor_pla, trim(adjustl(name)) // '%w_b_tor_pla')
     call is_set_type_scenario_ref(structure_in%w_th, trim(adjustl(name)) // '%w_th')
     call is_set_type_scenario_ref(structure_in%dwtot_dt, trim(adjustl(name)) // '%dwtot_dt')
     call is_set_type_scenario_ref(structure_in%dwbpol_dt, trim(adjustl(name)) // '%dwbpol_dt')
     call is_set_type_scenario_ref(structure_in%dwbtorpla_dt, trim(adjustl(name)) // '%dwbtorpla_dt')
     call is_set_type_scenario_ref(structure_in%dwth_dt, trim(adjustl(name)) // '%dwth_dt')
     call is_set_type_scenario_ref(structure_in%esup_icrhtot, trim(adjustl(name)) // '%esup_icrhtot')
     call is_set_type_scenario_ref(structure_in%esup_icrhper, trim(adjustl(name)) // '%esup_icrhper')
     call is_set_type_scenario_ref(structure_in%esup_nbitot, trim(adjustl(name)) // '%esup_nbitot')
     call is_set_type_scenario_ref(structure_in%esup_nbiperp, trim(adjustl(name)) // '%esup_nbiperp')
     call is_set_type_scenario_ref(structure_in%esup_lhcd, trim(adjustl(name)) // '%esup_lhcd')
     call is_set_type_scenario_ref(structure_in%esup_alpha, trim(adjustl(name)) // '%esup_alpha')

   end subroutine is_set_type_scenario_energy

   subroutine is_set_arr_type_scenario_energy(structure_in, name)
 
     implicit none
 
     type (type_scenario_energy), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_energy(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_energy

   subroutine is_set_type_scenario_global(structure_in, name)

     implicit none

     type (type_scenario_global), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%ip, trim(adjustl(name)) // '%ip')
     call is_set_type_scenario_ref(structure_in%dip_dt, trim(adjustl(name)) // '%dip_dt')
     call is_set_type_scenario_ref(structure_in%beta_pol, trim(adjustl(name)) // '%beta_pol')
     call is_set_type_scenario_ref(structure_in%beta_tor, trim(adjustl(name)) // '%beta_tor')
     call is_set_type_scenario_ref(structure_in%beta_normal, trim(adjustl(name)) // '%beta_normal')
     call is_set_type_scenario_ref(structure_in%li, trim(adjustl(name)) // '%li')
     call is_set_type_scenario_ref(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_scenario_ref(structure_in%area_pol, trim(adjustl(name)) // '%area_pol')
     call is_set_type_scenario_ref(structure_in%area_ext, trim(adjustl(name)) // '%area_ext')
     call is_set_type_scenario_ref(structure_in%len_sepa, trim(adjustl(name)) // '%len_sepa')
     call is_set_type_scenario_ref(structure_in%beta_pol_th, trim(adjustl(name)) // '%beta_pol_th')
     call is_set_type_scenario_ref(structure_in%beta_tor_th, trim(adjustl(name)) // '%beta_tor_th')
     call is_set_type_scenario_ref(structure_in%beta_n_th, trim(adjustl(name)) // '%beta_n_th')
     call is_set_type_scenario_ref(structure_in%disruption, trim(adjustl(name)) // '%disruption')
     call is_set_type_scenario_ref(structure_in%mode_h, trim(adjustl(name)) // '%mode_h')
     call is_set_type_scenario_ref(structure_in%s_alpha, trim(adjustl(name)) // '%s_alpha')

   end subroutine is_set_type_scenario_global

   subroutine is_set_arr_type_scenario_global(structure_in, name)
 
     implicit none
 
     type (type_scenario_global), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_global(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_global

   subroutine is_set_type_scenario_heat_power(structure_in, name)

     implicit none

     type (type_scenario_heat_power), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%plh, trim(adjustl(name)) // '%plh')
     call is_set_type_scenario_ref(structure_in%pohmic, trim(adjustl(name)) // '%pohmic')
     call is_set_type_scenario_ref(structure_in%picrh, trim(adjustl(name)) // '%picrh')
     call is_set_type_scenario_ref(structure_in%pecrh, trim(adjustl(name)) // '%pecrh')
     call is_set_type_scenario_ref(structure_in%pnbi, trim(adjustl(name)) // '%pnbi')
     call is_set_type_scenario_ref(structure_in%pnbi_co_cur, trim(adjustl(name)) // '%pnbi_co_cur')
     call is_set_type_scenario_ref(structure_in%pnbi_counter, trim(adjustl(name)) // '%pnbi_counter')
     call is_set_type_scenario_ref(structure_in%plh_th, trim(adjustl(name)) // '%plh_th')
     call is_set_type_scenario_ref(structure_in%picrh_th, trim(adjustl(name)) // '%picrh_th')
     call is_set_type_scenario_ref(structure_in%pecrh_th, trim(adjustl(name)) // '%pecrh_th')
     call is_set_type_scenario_ref(structure_in%pnbi_th, trim(adjustl(name)) // '%pnbi_th')
     call is_set_type_scenario_ref(structure_in%ploss_icrh, trim(adjustl(name)) // '%ploss_icrh')
     call is_set_type_scenario_ref(structure_in%ploss_nbi, trim(adjustl(name)) // '%ploss_nbi')
     call is_set_type_scenario_ref(structure_in%pbrem, trim(adjustl(name)) // '%pbrem')
     call is_set_type_scenario_ref(structure_in%pcyclo, trim(adjustl(name)) // '%pcyclo')
     call is_set_type_scenario_ref(structure_in%prad, trim(adjustl(name)) // '%prad')
     call is_set_type_scenario_ref(structure_in%pdd_fus, trim(adjustl(name)) // '%pdd_fus')
     call is_set_type_scenario_ref(structure_in%pei, trim(adjustl(name)) // '%pei')
     call is_set_type_scenario_ref(structure_in%pel_tot, trim(adjustl(name)) // '%pel_tot')
     call is_set_type_scenario_ref(structure_in%pel_fus, trim(adjustl(name)) // '%pel_fus')
     call is_set_type_scenario_ref(structure_in%pel_icrh, trim(adjustl(name)) // '%pel_icrh')
     call is_set_type_scenario_ref(structure_in%pel_nbi, trim(adjustl(name)) // '%pel_nbi')
     call is_set_type_scenario_ref(structure_in%pfus_dt, trim(adjustl(name)) // '%pfus_dt')
     call is_set_type_scenario_ref(structure_in%ploss_fus, trim(adjustl(name)) // '%ploss_fus')
     call is_set_type_scenario_ref(structure_in%pfus_nbi, trim(adjustl(name)) // '%pfus_nbi')
     call is_set_type_scenario_ref(structure_in%pfus_th, trim(adjustl(name)) // '%pfus_th')
     call is_set_type_scenario_ref(structure_in%padd_tot, trim(adjustl(name)) // '%padd_tot')
     call is_set_type_scenario_ref(structure_in%pion_tot, trim(adjustl(name)) // '%pion_tot')
     call is_set_type_scenario_ref(structure_in%pion_fus, trim(adjustl(name)) // '%pion_fus')
     call is_set_type_scenario_ref(structure_in%pion_icrh, trim(adjustl(name)) // '%pion_icrh')
     call is_set_type_scenario_ref(structure_in%pion_nbi, trim(adjustl(name)) // '%pion_nbi')
     call is_set_type_scenario_ref(structure_in%pioniz, trim(adjustl(name)) // '%pioniz')
     call is_set_type_scenario_ref(structure_in%ploss, trim(adjustl(name)) // '%ploss')
     call is_set_type_scenario_ref(structure_in%p_wth, trim(adjustl(name)) // '%p_wth')
     call is_set_type_scenario_ref(structure_in%p_w, trim(adjustl(name)) // '%p_w')
     call is_set_type_scenario_ref(structure_in%p_l2h_thr, trim(adjustl(name)) // '%p_l2h_thr')
     call is_set_type_scenario_ref(structure_in%p_l2h_sc, trim(adjustl(name)) // '%p_l2h_sc')
     call is_set_type_scenario_ref(structure_in%p_nbi_icrh, trim(adjustl(name)) // '%p_nbi_icrh')

   end subroutine is_set_type_scenario_heat_power

   subroutine is_set_arr_type_scenario_heat_power(structure_in, name)
 
     implicit none
 
     type (type_scenario_heat_power), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_heat_power(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_heat_power

   subroutine is_set_type_scenario_int(structure_in, name)

     implicit none

     type (type_scenario_int), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_scenario_int

   subroutine is_set_arr_type_scenario_int(structure_in, name)
 
     implicit none
 
     type (type_scenario_int), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_int(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_int

   subroutine is_set_type_scenario_itb(structure_in, name)

     implicit none

     type (type_scenario_itb), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%q_min, trim(adjustl(name)) // '%q_min')
     call is_set_type_scenario_ref(structure_in%te_itb, trim(adjustl(name)) // '%te_itb')
     call is_set_type_scenario_ref(structure_in%ti_itb, trim(adjustl(name)) // '%ti_itb')
     call is_set_type_scenario_ref(structure_in%ne_itb, trim(adjustl(name)) // '%ne_itb')
     call is_set_type_scenario_ref(structure_in%ni_itb, trim(adjustl(name)) // '%ni_itb')
     call is_set_type_scenario_ref(structure_in%psi_itb, trim(adjustl(name)) // '%psi_itb')
     call is_set_type_scenario_ref(structure_in%phi_itb, trim(adjustl(name)) // '%phi_itb')
     call is_set_type_scenario_ref(structure_in%rho_itb, trim(adjustl(name)) // '%rho_itb')
     call is_set_type_scenario_ref(structure_in%h_itb, trim(adjustl(name)) // '%h_itb')
     call is_set_type_scenario_ref(structure_in%width_itb, trim(adjustl(name)) // '%width_itb')
     call is_set_type_scenario_ref(structure_in%vtor_itb, trim(adjustl(name)) // '%vtor_itb')
     call is_set_type_scenario_int(structure_in%itb_type, trim(adjustl(name)) // '%itb_type')

   end subroutine is_set_type_scenario_itb

   subroutine is_set_arr_type_scenario_itb(structure_in, name)
 
     implicit none
 
     type (type_scenario_itb), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_itb(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_itb

   subroutine is_set_type_scenario_lim_div_wall(structure_in, name)

     implicit none

     type (type_scenario_lim_div_wall), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%te_lim_div, trim(adjustl(name)) // '%te_lim_div')
     call is_set_type_scenario_ref(structure_in%ti_lim_div, trim(adjustl(name)) // '%ti_lim_div')
     call is_set_type_scenario_ref(structure_in%ne_lim_div, trim(adjustl(name)) // '%ne_lim_div')
     call is_set_type_scenario_ref(structure_in%ni_lim_div, trim(adjustl(name)) // '%ni_lim_div')
     call is_set_type_scenario_ref(structure_in%q_peak_div, trim(adjustl(name)) // '%q_peak_div')
     call is_set_type_scenario_ref(structure_in%q_peak_wall, trim(adjustl(name)) // '%q_peak_wall')
     call is_set_type_scenario_ref(structure_in%surf_temp, trim(adjustl(name)) // '%surf_temp')
     call is_set_type_scenario_ref(structure_in%p_lim_div, trim(adjustl(name)) // '%p_lim_div')
     call is_set_type_scenario_ref(structure_in%p_rad_div, trim(adjustl(name)) // '%p_rad_div')
     call is_set_type_scenario_ref(structure_in%p_neut_div, trim(adjustl(name)) // '%p_neut_div')
     call is_set_type_scenario_ref(structure_in%p_wall, trim(adjustl(name)) // '%p_wall')
     call is_set_type_scenario_ref(structure_in%wall_temp, trim(adjustl(name)) // '%wall_temp')
     call is_set_type_scenario_ref(structure_in%wall_state, trim(adjustl(name)) // '%wall_state')
     call is_set_type_scenario_ref(structure_in%detach_state, trim(adjustl(name)) // '%detach_state')
     call is_set_type_scenario_ref(structure_in%pump_flux, trim(adjustl(name)) // '%pump_flux')
     call is_set_type_scenario_ref(structure_in%p_rad_fw, trim(adjustl(name)) // '%p_rad_fw')
     call is_set_type_scenario_ref(structure_in%p_cond_fw, trim(adjustl(name)) // '%p_cond_fw')
     call is_set_type_scenario_ref(structure_in%div_wetted, trim(adjustl(name)) // '%div_wetted')
     call is_set_type_scenario_ref(structure_in%gas_puff, trim(adjustl(name)) // '%gas_puff')
     call is_set_type_scenario_ref(structure_in%ar_concentr, trim(adjustl(name)) // '%ar_concentr')
     call is_set_type_scenario_ref(structure_in%part_exhaust, trim(adjustl(name)) // '%part_exhaust')
     call is_set_type_scenario_ref(structure_in%f_inner, trim(adjustl(name)) // '%f_inner')
     call is_set_type_scenario_ref(structure_in%f_outer, trim(adjustl(name)) // '%f_outer')
     call is_set_type_scenario_ref(structure_in%f_pfr, trim(adjustl(name)) // '%f_pfr')
     call is_set_type_scenario_ref(structure_in%f_rad_fw, trim(adjustl(name)) // '%f_rad_fw')
     call is_set_type_vecflt_type(structure_in%q_div, trim(adjustl(name)) // '%q_div')
     call is_set_type_scenario_ref(structure_in%p_cond_div, trim(adjustl(name)) // '%p_cond_div')
     call is_set_type_float(structure_in%pol_ext, trim(adjustl(name)) // '%pol_ext')
     call is_set_type_float(structure_in%flux_exp, trim(adjustl(name)) // '%flux_exp')
     call is_set_type_float(structure_in%tilt_angle, trim(adjustl(name)) // '%tilt_angle')
     call is_set_type_float(structure_in%n_div, trim(adjustl(name)) // '%n_div')
     call is_set_type_float(structure_in%div_dz, trim(adjustl(name)) // '%div_dz')
     call is_set_type_float(structure_in%div_dro, trim(adjustl(name)) // '%div_dro')
     call is_set_type_float(structure_in%div_dri, trim(adjustl(name)) // '%div_dri')
     call is_set_type_scenario_ref(structure_in%p_nh_div, trim(adjustl(name)) // '%p_nh_div')

   end subroutine is_set_type_scenario_lim_div_wall

   subroutine is_set_arr_type_scenario_lim_div_wall(structure_in, name)
 
     implicit none
 
     type (type_scenario_lim_div_wall), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_lim_div_wall(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_lim_div_wall

   subroutine is_set_type_scenario_line_ave(structure_in, name)

     implicit none

     type (type_scenario_line_ave), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%ne_line, trim(adjustl(name)) // '%ne_line')
     call is_set_type_scenario_ref(structure_in%zeff_line, trim(adjustl(name)) // '%zeff_line')
     call is_set_type_scenario_ref(structure_in%ne_zeff_line, trim(adjustl(name)) // '%ne_zeff_line')
     call is_set_type_scenario_ref(structure_in%dne_line_dt, trim(adjustl(name)) // '%dne_line_dt')

   end subroutine is_set_type_scenario_line_ave

   subroutine is_set_arr_type_scenario_line_ave(structure_in, name)
 
     implicit none
 
     type (type_scenario_line_ave), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_line_ave(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_line_ave

   subroutine is_set_type_scenario_neutron(structure_in, name)

     implicit none

     type (type_scenario_neutron), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%ndd_tot, trim(adjustl(name)) // '%ndd_tot')
     call is_set_type_scenario_ref(structure_in%ndd_th, trim(adjustl(name)) // '%ndd_th')
     call is_set_type_scenario_ref(structure_in%ndd_nbi_th, trim(adjustl(name)) // '%ndd_nbi_th')
     call is_set_type_scenario_ref(structure_in%ndd_nbi_nbi, trim(adjustl(name)) // '%ndd_nbi_nbi')
     call is_set_type_scenario_ref(structure_in%ndt_tot, trim(adjustl(name)) // '%ndt_tot')
     call is_set_type_scenario_ref(structure_in%ndt_th, trim(adjustl(name)) // '%ndt_th')

   end subroutine is_set_type_scenario_neutron

   subroutine is_set_arr_type_scenario_neutron(structure_in, name)
 
     implicit none
 
     type (type_scenario_neutron), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_neutron(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_neutron

   subroutine is_set_type_scenario_ninety_five(structure_in, name)

     implicit none

     type (type_scenario_ninety_five), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%q_95, trim(adjustl(name)) // '%q_95')
     call is_set_type_scenario_ref(structure_in%elong_95, trim(adjustl(name)) // '%elong_95')
     call is_set_type_scenario_ref(structure_in%tria_95, trim(adjustl(name)) // '%tria_95')
     call is_set_type_scenario_ref(structure_in%tria_up_95, trim(adjustl(name)) // '%tria_up_95')
     call is_set_type_scenario_ref(structure_in%tria_lo_95, trim(adjustl(name)) // '%tria_lo_95')
     call is_set_type_scenario_ref(structure_in%te_95, trim(adjustl(name)) // '%te_95')
     call is_set_type_scenario_ref(structure_in%ti_95, trim(adjustl(name)) // '%ti_95')
     call is_set_type_scenario_ref(structure_in%ne_95, trim(adjustl(name)) // '%ne_95')
     call is_set_type_scenario_ref(structure_in%ni_95, trim(adjustl(name)) // '%ni_95')
     call is_set_type_scenario_ref(structure_in%phi_95, trim(adjustl(name)) // '%phi_95')
     call is_set_type_scenario_ref(structure_in%rho_95, trim(adjustl(name)) // '%rho_95')
     call is_set_type_scenario_ref(structure_in%vtor_95, trim(adjustl(name)) // '%vtor_95')

   end subroutine is_set_type_scenario_ninety_five

   subroutine is_set_arr_type_scenario_ninety_five(structure_in, name)
 
     implicit none
 
     type (type_scenario_ninety_five), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_ninety_five(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_ninety_five

   subroutine is_set_type_scenario_pedestal(structure_in, name)

     implicit none

     type (type_scenario_pedestal), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%te_ped, trim(adjustl(name)) // '%te_ped')
     call is_set_type_scenario_ref(structure_in%ti_ped, trim(adjustl(name)) // '%ti_ped')
     call is_set_type_scenario_ref(structure_in%ne_ped, trim(adjustl(name)) // '%ne_ped')
     call is_set_type_scenario_ref(structure_in%ni_ped, trim(adjustl(name)) // '%ni_ped')
     call is_set_type_scenario_ref(structure_in%psi_ped, trim(adjustl(name)) // '%psi_ped')
     call is_set_type_scenario_ref(structure_in%phi_ped, trim(adjustl(name)) // '%phi_ped')
     call is_set_type_scenario_ref(structure_in%rho_ped, trim(adjustl(name)) // '%rho_ped')
     call is_set_type_scenario_ref(structure_in%q_ped, trim(adjustl(name)) // '%q_ped')
     call is_set_type_scenario_ref(structure_in%pressure_ped, trim(adjustl(name)) // '%pressure_ped')
     call is_set_type_scenario_ref(structure_in%vtor_ped, trim(adjustl(name)) // '%vtor_ped')

   end subroutine is_set_type_scenario_pedestal

   subroutine is_set_arr_type_scenario_pedestal(structure_in, name)
 
     implicit none
 
     type (type_scenario_pedestal), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_pedestal(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_pedestal

   subroutine is_set_type_scenario_reactor(structure_in, name)

     implicit none

     type (type_scenario_reactor), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%pnetwork, trim(adjustl(name)) // '%pnetwork')

   end subroutine is_set_type_scenario_reactor

   subroutine is_set_arr_type_scenario_reactor(structure_in, name)
 
     implicit none
 
     type (type_scenario_reactor), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_reactor(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_reactor

   subroutine is_set_type_scenario_ref(structure_in, name)

     implicit none

     type (type_scenario_ref), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_scenario_ref

   subroutine is_set_arr_type_scenario_ref(structure_in, name)
 
     implicit none
 
     type (type_scenario_ref), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_ref(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_ref

   subroutine is_set_type_scenario_references(structure_in, name)

     implicit none

     type (type_scenario_references), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%plh, trim(adjustl(name)) // '%plh')
     call is_set_type_scenario_ref(structure_in%picrh, trim(adjustl(name)) // '%picrh')
     call is_set_type_scenario_ref(structure_in%pecrh, trim(adjustl(name)) // '%pecrh')
     call is_set_type_scenario_ref(structure_in%pnbi, trim(adjustl(name)) // '%pnbi')
     call is_set_type_scenario_ref(structure_in%ip, trim(adjustl(name)) // '%ip')
     call is_set_type_scenario_ref(structure_in%bvac_r, trim(adjustl(name)) // '%bvac_r')
     call is_set_type_scenario_ref(structure_in%zeffl, trim(adjustl(name)) // '%zeffl')
     call is_set_type_scenario_ref(structure_in%nbar, trim(adjustl(name)) // '%nbar')
     call is_set_type_scenario_ref(structure_in%xecrh, trim(adjustl(name)) // '%xecrh')
     call is_set_type_scenario_ref(structure_in%pol_flux, trim(adjustl(name)) // '%pol_flux')
     call is_set_type_scenario_ref(structure_in%enhancement, trim(adjustl(name)) // '%enhancement')
     call is_set_type_scenario_ref(structure_in%isotopic, trim(adjustl(name)) // '%isotopic')
     call is_set_type_scenario_ref(structure_in%nbi_td_ratio, trim(adjustl(name)) // '%nbi_td_ratio')
     call is_set_type_scenario_ref(structure_in%gas_puff, trim(adjustl(name)) // '%gas_puff')

   end subroutine is_set_type_scenario_references

   subroutine is_set_arr_type_scenario_references(structure_in, name)
 
     implicit none
 
     type (type_scenario_references), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_references(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_references

   subroutine is_set_type_scenario_sol(structure_in, name)

     implicit none

     type (type_scenario_sol), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%l_te_sol, trim(adjustl(name)) // '%l_te_sol')
     call is_set_type_scenario_ref(structure_in%l_ti_sol, trim(adjustl(name)) // '%l_ti_sol')
     call is_set_type_scenario_ref(structure_in%l_ne_sol, trim(adjustl(name)) // '%l_ne_sol')
     call is_set_type_scenario_ref(structure_in%l_ni_sol, trim(adjustl(name)) // '%l_ni_sol')
     call is_set_type_scenario_ref(structure_in%l_qe_sol, trim(adjustl(name)) // '%l_qe_sol')
     call is_set_type_scenario_ref(structure_in%l_qi_sol, trim(adjustl(name)) // '%l_qi_sol')
     call is_set_type_scenario_ref(structure_in%p_rad_sol, trim(adjustl(name)) // '%p_rad_sol')
     call is_set_type_float(structure_in%p_neut, trim(adjustl(name)) // '%p_neut')
     call is_set_type_scenario_ref(structure_in%gas_puff, trim(adjustl(name)) // '%gas_puff')
     call is_set_type_float(structure_in%delta_r_in, trim(adjustl(name)) // '%delta_r_in')
     call is_set_type_float(structure_in%delta_r_out, trim(adjustl(name)) // '%delta_r_out')
     call is_set_type_float(structure_in%r_in, trim(adjustl(name)) // '%r_in')
     call is_set_type_float(structure_in%r_out, trim(adjustl(name)) // '%r_out')
     call is_set_type_float(structure_in%sol_width, trim(adjustl(name)) // '%sol_width')

   end subroutine is_set_type_scenario_sol

   subroutine is_set_arr_type_scenario_sol(structure_in, name)
 
     implicit none
 
     type (type_scenario_sol), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_sol(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_sol

   subroutine is_set_type_scenario_vol_ave(structure_in, name)

     implicit none

     type (type_scenario_vol_ave), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_scenario_ref(structure_in%te_ave, trim(adjustl(name)) // '%te_ave')
     call is_set_type_scenario_ref(structure_in%ti_ave, trim(adjustl(name)) // '%ti_ave')
     call is_set_type_scenario_ref(structure_in%ne_ave, trim(adjustl(name)) // '%ne_ave')
     call is_set_type_scenario_ref(structure_in%dne_ave_dt, trim(adjustl(name)) // '%dne_ave_dt')
     call is_set_type_scenario_ref(structure_in%ni_ave, trim(adjustl(name)) // '%ni_ave')
     call is_set_type_scenario_ref(structure_in%zeff_ave, trim(adjustl(name)) // '%zeff_ave')
     call is_set_type_scenario_ref(structure_in%ti_o_te_ave, trim(adjustl(name)) // '%ti_o_te_ave')
     call is_set_type_scenario_ref(structure_in%meff_ave, trim(adjustl(name)) // '%meff_ave')
     call is_set_type_scenario_ref(structure_in%pellet_flux, trim(adjustl(name)) // '%pellet_flux')
     call is_set_type_vecflt_type(structure_in%nions_ave, trim(adjustl(name)) // '%nions_ave')
     call is_set_type_scenario_ref(structure_in%omega_ave, trim(adjustl(name)) // '%omega_ave')

   end subroutine is_set_type_scenario_vol_ave

   subroutine is_set_arr_type_scenario_vol_ave(structure_in, name)
 
     implicit none
 
     type (type_scenario_vol_ave), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_scenario_vol_ave(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_scenario_vol_ave

   subroutine is_set_type_setup_bprobe(structure_in, name)

     implicit none

     type (type_setup_bprobe), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_vecflt_type(structure_in%polangle, trim(adjustl(name)) // '%polangle')
     call is_set_type_vecflt_type(structure_in%torangle, trim(adjustl(name)) // '%torangle')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')
     call is_set_type_vecflt_type(structure_in%length, trim(adjustl(name)) // '%length')
     call is_set_type_vecint_type(structure_in%turns, trim(adjustl(name)) // '%turns')

   end subroutine is_set_type_setup_bprobe

   subroutine is_set_arr_type_setup_bprobe(structure_in, name)
 
     implicit none
 
     type (type_setup_bprobe), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_setup_bprobe(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_setup_bprobe

   subroutine is_set_type_setup_floops(structure_in, name)

     implicit none

     type (type_setup_floops), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_rzphi2D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_vecint_type(structure_in%npoints, trim(adjustl(name)) // '%npoints')

   end subroutine is_set_type_setup_floops

   subroutine is_set_arr_type_setup_floops(structure_in, name)
 
     implicit none
 
     type (type_setup_floops), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_setup_floops(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_setup_floops

   subroutine is_set_type_setup_line(structure_in, name)

     implicit none

     type (type_setup_line), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1D(structure_in%pivot_point, trim(adjustl(name)) // '%pivot_point')
     call is_set_type_vecflt_type(structure_in%horchordang1, trim(adjustl(name)) // '%horchordang1')
     call is_set_type_vecflt_type(structure_in%verchordang1, trim(adjustl(name)) // '%verchordang1')
     call is_set_type_vecflt_type(structure_in%width, trim(adjustl(name)) // '%width')
     call is_set_type_rzphi1D(structure_in%second_point, trim(adjustl(name)) // '%second_point')
     call is_set_type_vecflt_type(structure_in%horchordang2, trim(adjustl(name)) // '%horchordang2')
     call is_set_type_vecflt_type(structure_in%verchordang2, trim(adjustl(name)) // '%verchordang2')
     call is_set_type_rzphi1D(structure_in%third_point, trim(adjustl(name)) // '%third_point')
     call is_set_type_integer(structure_in%nchordpoints, trim(adjustl(name)) // '%nchordpoints')

   end subroutine is_set_type_setup_line

   subroutine is_set_arr_type_setup_line(structure_in, name)
 
     implicit none
 
     type (type_setup_line), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_setup_line(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_setup_line

   subroutine is_set_type_setup_line_exp(structure_in, name)

     implicit none

     type (type_setup_line_exp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1Dexperimental(structure_in%pivot_point, trim(adjustl(name)) // '%pivot_point')
     call is_set_type_vecflt_type(structure_in%horchordang1, trim(adjustl(name)) // '%horchordang1')
     call is_set_type_vecflt_type(structure_in%verchordang1, trim(adjustl(name)) // '%verchordang1')
     call is_set_type_vecflt_type(structure_in%width, trim(adjustl(name)) // '%width')
     call is_set_type_rzphi1Dexperimental(structure_in%second_point, trim(adjustl(name)) // '%second_point')
     call is_set_type_vecflt_type(structure_in%horchordang2, trim(adjustl(name)) // '%horchordang2')
     call is_set_type_vecflt_type(structure_in%verchordang2, trim(adjustl(name)) // '%verchordang2')
     call is_set_type_rzphi1Dexperimental(structure_in%third_point, trim(adjustl(name)) // '%third_point')
     call is_set_type_integer(structure_in%nchordpoints, trim(adjustl(name)) // '%nchordpoints')

   end subroutine is_set_type_setup_line_exp

   subroutine is_set_arr_type_setup_line_exp(structure_in, name)
 
     implicit none
 
     type (type_setup_line_exp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_setup_line_exp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_setup_line_exp

   subroutine is_set_type_shield(structure_in, name)

     implicit none

     type (type_shield), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_shield_specs(structure_in%inboard, trim(adjustl(name)) // '%inboard')
     call is_set_type_shield_specs(structure_in%outboard, trim(adjustl(name)) // '%outboard')

   end subroutine is_set_type_shield

   subroutine is_set_arr_type_shield(structure_in, name)
 
     implicit none
 
     type (type_shield), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_shield(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_shield

   subroutine is_set_type_shield_specs(structure_in, name)

     implicit none

     type (type_shield_specs), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nmat, trim(adjustl(name)) // '%nmat')
     call is_set_type_vecflt_type(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_float(structure_in%r1, trim(adjustl(name)) // '%r1')
     call is_set_type_float(structure_in%r2, trim(adjustl(name)) // '%r2')
     call is_set_type_float(structure_in%mass, trim(adjustl(name)) // '%mass')

   end subroutine is_set_type_shield_specs

   subroutine is_set_arr_type_shield_specs(structure_in, name)
 
     implicit none
 
     type (type_shield_specs), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_shield_specs(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_shield_specs

   subroutine is_set_type_simp_apert(structure_in, name)

     implicit none

     type (type_simp_apert), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecflt_type(structure_in%sizes, trim(adjustl(name)) // '%sizes')
     call is_set_type_float(structure_in%angle, trim(adjustl(name)) // '%angle')

   end subroutine is_set_type_simp_apert

   subroutine is_set_arr_type_simp_apert(structure_in, name)
 
     implicit none
 
     type (type_simp_apert), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_simp_apert(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_simp_apert

   subroutine is_set_type_solcurdiag_sol_current(structure_in, name)

     implicit none

     type (type_solcurdiag_sol_current), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_solcurdiag_sol_current_setup(structure_in%setup, trim(adjustl(name)) // '%setup')
     call is_set_type_exp0D(structure_in%measure, trim(adjustl(name)) // '%measure')

   end subroutine is_set_type_solcurdiag_sol_current

   subroutine is_set_arr_type_solcurdiag_sol_current(structure_in, name)
 
     implicit none
 
     type (type_solcurdiag_sol_current), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_solcurdiag_sol_current(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_solcurdiag_sol_current

   subroutine is_set_type_solcurdiag_sol_current_setup(structure_in, name)

     implicit none

     type (type_solcurdiag_sol_current_setup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_integer(structure_in%id, trim(adjustl(name)) // '%id')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_integer(structure_in%tiles_turn, trim(adjustl(name)) // '%tiles_turn')

   end subroutine is_set_type_solcurdiag_sol_current_setup

   subroutine is_set_arr_type_solcurdiag_sol_current_setup(structure_in, name)
 
     implicit none
 
     type (type_solcurdiag_sol_current_setup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_solcurdiag_sol_current_setup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_solcurdiag_sol_current_setup

   subroutine is_set_type_source_imp(structure_in, name)

     implicit none

     type (type_source_imp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%exp, trim(adjustl(name)) // '%exp')
     call is_set_type_matflt_type(structure_in%imp, trim(adjustl(name)) // '%imp')

   end subroutine is_set_type_source_imp

   subroutine is_set_arr_type_source_imp(structure_in, name)
 
     implicit none
 
     type (type_source_imp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_source_imp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_source_imp

   subroutine is_set_type_source_ion(structure_in, name)

     implicit none

     type (type_source_ion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%exp, trim(adjustl(name)) // '%exp')
     call is_set_type_matflt_type(structure_in%imp, trim(adjustl(name)) // '%imp')

   end subroutine is_set_type_source_ion

   subroutine is_set_arr_type_source_ion(structure_in, name)
 
     implicit none
 
     type (type_source_ion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_source_ion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_source_ion

   subroutine is_set_type_source_rate(structure_in, name)

     implicit none

     type (type_source_rate), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_complexgrid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_type_complexgrid_scalar(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecint_type(structure_in%discrete, trim(adjustl(name)) // '%discrete')
     call is_set_type_parameters(structure_in%parameters, trim(adjustl(name)) // '%parameters')

   end subroutine is_set_type_source_rate

   subroutine is_set_arr_type_source_rate(structure_in, name)
 
     implicit none
 
     type (type_source_rate), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_source_rate(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_source_rate

   subroutine is_set_type_source_vec(structure_in, name)

     implicit none

     type (type_source_vec), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%exp, trim(adjustl(name)) // '%exp')
     call is_set_type_vecflt_type(structure_in%imp, trim(adjustl(name)) // '%imp')

   end subroutine is_set_type_source_vec

   subroutine is_set_arr_type_source_vec(structure_in, name)
 
     implicit none
 
     type (type_source_vec), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_source_vec(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_source_vec

   subroutine is_set_type_sourceel(structure_in, name)

     implicit none

     type (type_sourceel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_vecflt_type(structure_in%integral, trim(adjustl(name)) // '%integral')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_sourceel

   subroutine is_set_arr_type_sourceel(structure_in, name)
 
     implicit none
 
     type (type_sourceel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sourceel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sourceel

   subroutine is_set_type_sourceimp(structure_in, name)

     implicit none

     type (type_sourceimp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_matflt_type(structure_in%integral, trim(adjustl(name)) // '%integral')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_sourceimp

   subroutine is_set_arr_type_sourceimp(structure_in, name)
 
     implicit none
 
     type (type_sourceimp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sourceimp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sourceimp

   subroutine is_set_type_sourceion(structure_in, name)

     implicit none

     type (type_sourceion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')
     call is_set_type_matflt_type(structure_in%integral, trim(adjustl(name)) // '%integral')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')

   end subroutine is_set_type_sourceion

   subroutine is_set_arr_type_sourceion(structure_in, name)
 
     implicit none
 
     type (type_sourceion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sourceion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sourceion

   subroutine is_set_type_species_desc(structure_in, name)

     implicit none

     type (type_species_desc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_type_float(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_float(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_float(structure_in%zmin, trim(adjustl(name)) // '%zmin')
     call is_set_type_float(structure_in%zmax, trim(adjustl(name)) // '%zmax')

   end subroutine is_set_type_species_desc

   subroutine is_set_arr_type_species_desc(structure_in, name)
 
     implicit none
 
     type (type_species_desc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_species_desc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_species_desc

   subroutine is_set_type_species_reference(structure_in, name)

     implicit none

     type (type_species_reference), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_integer(structure_in%index, trim(adjustl(name)) // '%index')

   end subroutine is_set_type_species_reference

   subroutine is_set_arr_type_species_reference(structure_in, name)
 
     implicit none
 
     type (type_species_reference), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_species_reference(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_species_reference

   subroutine is_set_type_spectral(structure_in, name)

     implicit none

     type (type_spectral), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_msediag_emissivity(structure_in%emissivity, trim(adjustl(name)) // '%emissivity')
     call is_set_type_msediag_radiance(structure_in%radiance, trim(adjustl(name)) // '%radiance')
     call is_set_type_codeparam(structure_in%codeparam, trim(adjustl(name)) // '%codeparam')

   end subroutine is_set_type_spectral

   subroutine is_set_arr_type_spectral(structure_in, name)
 
     implicit none
 
     type (type_spectral), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_spectral(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_spectral

   subroutine is_set_type_spectrum(structure_in, name)

     implicit none

     type (type_spectrum), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_launchs_phi_theta(structure_in%phi_theta, trim(adjustl(name)) // '%phi_theta')
     call is_set_type_launchs_parallel(structure_in%parallel, trim(adjustl(name)) // '%parallel')

   end subroutine is_set_type_spectrum

   subroutine is_set_arr_type_spectrum(structure_in, name)
 
     implicit none
 
     type (type_spectrum), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_spectrum(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_spectrum

   subroutine is_set_type_spot(structure_in, name)

     implicit none

     type (type_spot), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%size, trim(adjustl(name)) // '%size')
     call is_set_type_float(structure_in%angle, trim(adjustl(name)) // '%angle')

   end subroutine is_set_type_spot

   subroutine is_set_arr_type_spot(structure_in, name)
 
     implicit none
 
     type (type_spot), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_spot(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_spot

   subroutine is_set_type_sputtering_neutrals(structure_in, name)

     implicit none

     type (type_sputtering_neutrals), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%physical, trim(adjustl(name)) // '%physical')
     call is_set_type_vecflt_type(structure_in%chemical, trim(adjustl(name)) // '%chemical')

   end subroutine is_set_type_sputtering_neutrals

   subroutine is_set_arr_type_sputtering_neutrals(structure_in, name)
 
     implicit none
 
     type (type_sputtering_neutrals), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_sputtering_neutrals(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_sputtering_neutrals

   subroutine is_set_type_straps(structure_in, name)

     implicit none

     type (type_straps), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp0D(structure_in%current, trim(adjustl(name)) // '%current')
     call is_set_type_exp0D(structure_in%phase, trim(adjustl(name)) // '%phase')
     call is_set_type_float(structure_in%phi_centre, trim(adjustl(name)) // '%phi_centre')
     call is_set_type_float(structure_in%width, trim(adjustl(name)) // '%width')
     call is_set_type_float(structure_in%dist2wall, trim(adjustl(name)) // '%dist2wall')
     call is_set_type_rz1D(structure_in%coord_strap, trim(adjustl(name)) // '%coord_strap')

   end subroutine is_set_type_straps

   subroutine is_set_arr_type_straps(structure_in, name)
 
     implicit none
 
     type (type_straps), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_straps(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_straps

   subroutine is_set_type_structure_cs(structure_in, name)

     implicit none

     type (type_structure_cs), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%gaptf, trim(adjustl(name)) // '%gaptf')
     call is_set_type_float(structure_in%ri, trim(adjustl(name)) // '%ri')
     call is_set_type_float(structure_in%re, trim(adjustl(name)) // '%re')
     call is_set_type_float(structure_in%jcable, trim(adjustl(name)) // '%jcable')
     call is_set_type_float(structure_in%current_nom, trim(adjustl(name)) // '%current_nom')
     call is_set_type_float(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_float(structure_in%tiso, trim(adjustl(name)) // '%tiso')
     call is_set_type_float(structure_in%nlay, trim(adjustl(name)) // '%nlay')

   end subroutine is_set_type_structure_cs

   subroutine is_set_arr_type_structure_cs(structure_in, name)
 
     implicit none
 
     type (type_structure_cs), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_structure_cs(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_structure_cs

   subroutine is_set_type_t_series_cplx(structure_in, name)

     implicit none

     type (type_t_series_cplx), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%time_wind, trim(adjustl(name)) // '%time_wind')
     call is_set_type_vecflt_type(structure_in%values_re, trim(adjustl(name)) // '%values_re')
     call is_set_type_vecflt_type(structure_in%values_im, trim(adjustl(name)) // '%values_im')

   end subroutine is_set_type_t_series_cplx

   subroutine is_set_arr_type_t_series_cplx(structure_in, name)
 
     implicit none
 
     type (type_t_series_cplx), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_t_series_cplx(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_t_series_cplx

   subroutine is_set_type_t_series_real(structure_in, name)

     implicit none

     type (type_t_series_real), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%time_wind, trim(adjustl(name)) // '%time_wind')
     call is_set_type_vecflt_type(structure_in%values, trim(adjustl(name)) // '%values')

   end subroutine is_set_type_t_series_real

   subroutine is_set_arr_type_t_series_real(structure_in, name)
 
     implicit none
 
     type (type_t_series_real), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_t_series_real(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_t_series_real

   subroutine is_set_type_table(structure_in, name)

     implicit none

     type (type_table), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%filled, trim(adjustl(name)) // '%filled')
     call is_set_type_float(structure_in%table_0d, trim(adjustl(name)) // '%table_0d')
     call is_set_type_vecflt_type(structure_in%table_1d, trim(adjustl(name)) // '%table_1d')
     call is_set_type_matflt_type(structure_in%table_2d, trim(adjustl(name)) // '%table_2d')
     call is_set_type_array3dflt_type(structure_in%table_3d, trim(adjustl(name)) // '%table_3d')
     call is_set_type_array4dflt_type(structure_in%table_4d, trim(adjustl(name)) // '%table_4d')
     call is_set_type_array5dflt_type(structure_in%table_5d, trim(adjustl(name)) // '%table_5d')
     call is_set_type_array6dflt_type(structure_in%table_6d, trim(adjustl(name)) // '%table_6d')
     call is_set_type_vecstring_type(structure_in%coord1_str, trim(adjustl(name)) // '%coord1_str')
     call is_set_type_vecstring_type(structure_in%coord2_str, trim(adjustl(name)) // '%coord2_str')
     call is_set_type_vecstring_type(structure_in%coord3_str, trim(adjustl(name)) // '%coord3_str')
     call is_set_type_vecstring_type(structure_in%coord4_str, trim(adjustl(name)) // '%coord4_str')
     call is_set_type_vecstring_type(structure_in%coord5_str, trim(adjustl(name)) // '%coord5_str')
     call is_set_type_vecstring_type(structure_in%coord6_str, trim(adjustl(name)) // '%coord6_str')
     call is_set_type_identifier(structure_in%quality, trim(adjustl(name)) // '%quality')

   end subroutine is_set_type_table

   subroutine is_set_arr_type_table(structure_in, name)
 
     implicit none
 
     type (type_table), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_table(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_table

   subroutine is_set_type_tables(structure_in, name)

     implicit none

     type (type_tables), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%ndim, trim(adjustl(name)) // '%ndim')
     call is_set_type_integer(structure_in%coord_index, trim(adjustl(name)) // '%coord_index')
     call is_set_type_vecstring_type(structure_in%result_label, trim(adjustl(name)) // '%result_label')
     call is_set_type_vecstring_type(structure_in%result_unit, trim(adjustl(name)) // '%result_unit')
     call is_set_type_integer(structure_in%result_trans, trim(adjustl(name)) // '%result_trans')
     call is_set_type_vecint_type(structure_in%zmin, trim(adjustl(name)) // '%zmin')
     call is_set_type_vecint_type(structure_in%zmax, trim(adjustl(name)) // '%zmax')
     call is_set_type_vecstring_type(structure_in%state_label, trim(adjustl(name)) // '%state_label')
     call is_set_arr_type_table(structure_in%table, trim(adjustl(name)) // '%table')
     call is_set_type_vecstring_type(structure_in%data_source, trim(adjustl(name)) // '%data_source')
     call is_set_type_vecstring_type(structure_in%data_provide, trim(adjustl(name)) // '%data_provide')
     call is_set_type_vecstring_type(structure_in%data_citation, trim(adjustl(name)) // '%data_citation')

   end subroutine is_set_type_tables

   subroutine is_set_arr_type_tables(structure_in, name)
 
     implicit none
 
     type (type_tables), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tables(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tables

   subroutine is_set_type_tables_coord(structure_in, name)

     implicit none

     type (type_tables_coord), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_coords(structure_in%coords, trim(adjustl(name)) // '%coords')

   end subroutine is_set_type_tables_coord

   subroutine is_set_arr_type_tables_coord(structure_in, name)
 
     implicit none
 
     type (type_tables_coord), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tables_coord(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tables_coord

   subroutine is_set_type_temporary_nt(structure_in, name)

     implicit none

     type (type_temporary_nt), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_temporary_nt_0dr(structure_in%float0d, trim(adjustl(name)) // '%float0d')
     call is_set_arr_type_temporary_nt_0di(structure_in%integer0d, trim(adjustl(name)) // '%integer0d')
     call is_set_arr_type_temporary_nt_0dc(structure_in%complex0d, trim(adjustl(name)) // '%complex0d')
     call is_set_arr_type_temporary_nt_0ds(structure_in%string0d, trim(adjustl(name)) // '%string0d')
     call is_set_arr_type_temporary_nt_1dr(structure_in%float1d, trim(adjustl(name)) // '%float1d')
     call is_set_arr_type_temporary_nt_1di(structure_in%integer1d, trim(adjustl(name)) // '%integer1d')
     call is_set_arr_type_temporary_nt_1dr(structure_in%string1d, trim(adjustl(name)) // '%string1d')
     call is_set_arr_type_temporary_nt_1dc(structure_in%complex1d, trim(adjustl(name)) // '%complex1d')
     call is_set_arr_type_temporary_nt_2dr(structure_in%float2d, trim(adjustl(name)) // '%float2d')
     call is_set_arr_type_temporary_nt_2di(structure_in%integer2d, trim(adjustl(name)) // '%integer2d')
     call is_set_arr_type_temporary_nt_2dc(structure_in%complex2d, trim(adjustl(name)) // '%complex2d')
     call is_set_arr_type_temporary_nt_3dr(structure_in%float3d, trim(adjustl(name)) // '%float3d')
     call is_set_arr_type_temporary_nt_3di(structure_in%integer3d, trim(adjustl(name)) // '%integer3d')
     call is_set_arr_type_temporary_nt_3dc(structure_in%complex3d, trim(adjustl(name)) // '%complex3d')
     call is_set_arr_type_temporary_nt_4dr(structure_in%float4d, trim(adjustl(name)) // '%float4d')

   end subroutine is_set_type_temporary_nt

   subroutine is_set_arr_type_temporary_nt(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt

   subroutine is_set_type_temporary_nt_0dc(structure_in, name)

     implicit none

     type (type_temporary_nt_0dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_complex(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_0dc

   subroutine is_set_arr_type_temporary_nt_0dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_0dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_0dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_0dc

   subroutine is_set_type_temporary_nt_0di(structure_in, name)

     implicit none

     type (type_temporary_nt_0di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_integer(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_0di

   subroutine is_set_arr_type_temporary_nt_0di(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_0di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_0di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_0di

   subroutine is_set_type_temporary_nt_0dr(structure_in, name)

     implicit none

     type (type_temporary_nt_0dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_float(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_0dr

   subroutine is_set_arr_type_temporary_nt_0dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_0dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_0dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_0dr

   subroutine is_set_type_temporary_nt_0ds(structure_in, name)

     implicit none

     type (type_temporary_nt_0ds), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecstring_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_0ds

   subroutine is_set_arr_type_temporary_nt_0ds(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_0ds), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_0ds(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_0ds

   subroutine is_set_type_temporary_nt_1dc(structure_in, name)

     implicit none

     type (type_temporary_nt_1dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_veccplx_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_1dc

   subroutine is_set_arr_type_temporary_nt_1dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_1dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_1dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_1dc

   subroutine is_set_type_temporary_nt_1di(structure_in, name)

     implicit none

     type (type_temporary_nt_1di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecint_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_1di

   subroutine is_set_arr_type_temporary_nt_1di(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_1di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_1di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_1di

   subroutine is_set_type_temporary_nt_1dr(structure_in, name)

     implicit none

     type (type_temporary_nt_1dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_1dr

   subroutine is_set_arr_type_temporary_nt_1dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_1dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_1dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_1dr

   subroutine is_set_type_temporary_nt_1ds(structure_in, name)

     implicit none

     type (type_temporary_nt_1ds), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecstring_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_1ds

   subroutine is_set_arr_type_temporary_nt_1ds(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_1ds), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_1ds(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_1ds

   subroutine is_set_type_temporary_nt_2dc(structure_in, name)

     implicit none

     type (type_temporary_nt_2dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_matcplx_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_2dc

   subroutine is_set_arr_type_temporary_nt_2dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_2dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_2dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_2dc

   subroutine is_set_type_temporary_nt_2di(structure_in, name)

     implicit none

     type (type_temporary_nt_2di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_matint_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_2di

   subroutine is_set_arr_type_temporary_nt_2di(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_2di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_2di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_2di

   subroutine is_set_type_temporary_nt_2dr(structure_in, name)

     implicit none

     type (type_temporary_nt_2dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_2dr

   subroutine is_set_arr_type_temporary_nt_2dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_2dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_2dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_2dr

   subroutine is_set_type_temporary_nt_3dc(structure_in, name)

     implicit none

     type (type_temporary_nt_3dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array3dcplx_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_3dc

   subroutine is_set_arr_type_temporary_nt_3dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_3dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_3dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_3dc

   subroutine is_set_type_temporary_nt_3di(structure_in, name)

     implicit none

     type (type_temporary_nt_3di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array3dint_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_3di

   subroutine is_set_arr_type_temporary_nt_3di(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_3di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_3di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_3di

   subroutine is_set_type_temporary_nt_3dr(structure_in, name)

     implicit none

     type (type_temporary_nt_3dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array3dflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_3dr

   subroutine is_set_arr_type_temporary_nt_3dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_3dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_3dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_3dr

   subroutine is_set_type_temporary_nt_4dr(structure_in, name)

     implicit none

     type (type_temporary_nt_4dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array4dflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_nt_4dr

   subroutine is_set_arr_type_temporary_nt_4dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_nt_4dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_nt_4dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_nt_4dr

   subroutine is_set_type_temporary_t(structure_in, name)

     implicit none

     type (type_temporary_t), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_temporary_t_0dr(structure_in%float0d, trim(adjustl(name)) // '%float0d')
     call is_set_arr_type_temporary_t_0di(structure_in%integer0d, trim(adjustl(name)) // '%integer0d')
     call is_set_arr_type_temporary_t_0dc(structure_in%complex0d, trim(adjustl(name)) // '%complex0d')
     call is_set_arr_type_temporary_t_0ds(structure_in%string0d, trim(adjustl(name)) // '%string0d')
     call is_set_arr_type_temporary_t_1dr(structure_in%float1d, trim(adjustl(name)) // '%float1d')
     call is_set_arr_type_temporary_t_1di(structure_in%integer1d, trim(adjustl(name)) // '%integer1d')
     call is_set_arr_type_temporary_t_1dc(structure_in%complex1d, trim(adjustl(name)) // '%complex1d')
     call is_set_arr_type_temporary_t_2dr(structure_in%float2d, trim(adjustl(name)) // '%float2d')
     call is_set_arr_type_temporary_t_2di(structure_in%integer2d, trim(adjustl(name)) // '%integer2d')
     call is_set_arr_type_temporary_t_2dc(structure_in%complex2d, trim(adjustl(name)) // '%complex2d')
     call is_set_arr_type_temporary_t_3dr(structure_in%float3d, trim(adjustl(name)) // '%float3d')
     call is_set_arr_type_temporary_t_3di(structure_in%integer3d, trim(adjustl(name)) // '%integer3d')
     call is_set_arr_type_temporary_t_3dc(structure_in%complex3d, trim(adjustl(name)) // '%complex3d')
     call is_set_arr_type_temporary_t_4dr(structure_in%float4d, trim(adjustl(name)) // '%float4d')

   end subroutine is_set_type_temporary_t

   subroutine is_set_arr_type_temporary_t(structure_in, name)
 
     implicit none
 
     type (type_temporary_t), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t

   subroutine is_set_type_temporary_t_0dc(structure_in, name)

     implicit none

     type (type_temporary_t_0dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_complex(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_0dc

   subroutine is_set_arr_type_temporary_t_0dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_0dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_0dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_0dc

   subroutine is_set_type_temporary_t_0di(structure_in, name)

     implicit none

     type (type_temporary_t_0di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_integer(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_0di

   subroutine is_set_arr_type_temporary_t_0di(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_0di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_0di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_0di

   subroutine is_set_type_temporary_t_0dr(structure_in, name)

     implicit none

     type (type_temporary_t_0dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_float(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_0dr

   subroutine is_set_arr_type_temporary_t_0dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_0dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_0dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_0dr

   subroutine is_set_type_temporary_t_0ds(structure_in, name)

     implicit none

     type (type_temporary_t_0ds), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecstring_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_0ds

   subroutine is_set_arr_type_temporary_t_0ds(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_0ds), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_0ds(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_0ds

   subroutine is_set_type_temporary_t_1dc(structure_in, name)

     implicit none

     type (type_temporary_t_1dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_veccplx_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_1dc

   subroutine is_set_arr_type_temporary_t_1dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_1dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_1dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_1dc

   subroutine is_set_type_temporary_t_1di(structure_in, name)

     implicit none

     type (type_temporary_t_1di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecint_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_1di

   subroutine is_set_arr_type_temporary_t_1di(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_1di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_1di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_1di

   subroutine is_set_type_temporary_t_1dr(structure_in, name)

     implicit none

     type (type_temporary_t_1dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_vecflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_1dr

   subroutine is_set_arr_type_temporary_t_1dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_1dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_1dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_1dr

   subroutine is_set_type_temporary_t_2dc(structure_in, name)

     implicit none

     type (type_temporary_t_2dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_matcplx_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_2dc

   subroutine is_set_arr_type_temporary_t_2dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_2dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_2dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_2dc

   subroutine is_set_type_temporary_t_2di(structure_in, name)

     implicit none

     type (type_temporary_t_2di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_matint_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_2di

   subroutine is_set_arr_type_temporary_t_2di(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_2di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_2di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_2di

   subroutine is_set_type_temporary_t_2dr(structure_in, name)

     implicit none

     type (type_temporary_t_2dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_matflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_2dr

   subroutine is_set_arr_type_temporary_t_2dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_2dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_2dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_2dr

   subroutine is_set_type_temporary_t_3dc(structure_in, name)

     implicit none

     type (type_temporary_t_3dc), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array3dcplx_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_3dc

   subroutine is_set_arr_type_temporary_t_3dc(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_3dc), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_3dc(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_3dc

   subroutine is_set_type_temporary_t_3di(structure_in, name)

     implicit none

     type (type_temporary_t_3di), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array3dint_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_3di

   subroutine is_set_arr_type_temporary_t_3di(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_3di), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_3di(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_3di

   subroutine is_set_type_temporary_t_3dr(structure_in, name)

     implicit none

     type (type_temporary_t_3dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array3dflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_3dr

   subroutine is_set_arr_type_temporary_t_3dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_3dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_3dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_3dr

   subroutine is_set_type_temporary_t_4dr(structure_in, name)

     implicit none

     type (type_temporary_t_4dr), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%identifier, trim(adjustl(name)) // '%identifier')
     call is_set_type_array4dflt_type(structure_in%value, trim(adjustl(name)) // '%value')

   end subroutine is_set_type_temporary_t_4dr

   subroutine is_set_arr_type_temporary_t_4dr(structure_in, name)
 
     implicit none
 
     type (type_temporary_t_4dr), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_temporary_t_4dr(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_temporary_t_4dr

   subroutine is_set_type_tf_desc_tfcoils(structure_in, name)

     implicit none

     type (type_tf_desc_tfcoils), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_float(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_circularcoil(structure_in%circularcoil, trim(adjustl(name)) // '%circularcoil')
     call is_set_type_planecoil(structure_in%planecoil, trim(adjustl(name)) // '%planecoil')
     call is_set_type_tf_structure(structure_in%inboard, trim(adjustl(name)) // '%inboard')
     call is_set_type_tf_structure(structure_in%outboard, trim(adjustl(name)) // '%outboard')

   end subroutine is_set_type_tf_desc_tfcoils

   subroutine is_set_arr_type_tf_desc_tfcoils(structure_in, name)
 
     implicit none
 
     type (type_tf_desc_tfcoils), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tf_desc_tfcoils(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tf_desc_tfcoils

   subroutine is_set_type_tf_desc_tfcoils_board(structure_in, name)

     implicit none

     type (type_tf_desc_tfcoils_board), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_tf_structure(structure_in%structure, trim(adjustl(name)) // '%structure')

   end subroutine is_set_type_tf_desc_tfcoils_board

   subroutine is_set_arr_type_tf_desc_tfcoils_board(structure_in, name)
 
     implicit none
 
     type (type_tf_desc_tfcoils_board), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tf_desc_tfcoils_board(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tf_desc_tfcoils_board

   subroutine is_set_type_tf_structure(structure_in, name)

     implicit none

     type (type_tf_structure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%jcable, trim(adjustl(name)) // '%jcable')
     call is_set_type_float(structure_in%tisotf, trim(adjustl(name)) // '%tisotf')
     call is_set_type_float(structure_in%efcasing, trim(adjustl(name)) // '%efcasing')
     call is_set_type_float(structure_in%escasing, trim(adjustl(name)) // '%escasing')
     call is_set_type_float(structure_in%sigjackettf, trim(adjustl(name)) // '%sigjackettf')
     call is_set_type_float(structure_in%sigvaulttf, trim(adjustl(name)) // '%sigvaulttf')
     call is_set_type_float(structure_in%ktf, trim(adjustl(name)) // '%ktf')
     call is_set_type_float(structure_in%ritf, trim(adjustl(name)) // '%ritf')
     call is_set_type_float(structure_in%riitf, trim(adjustl(name)) // '%riitf')
     call is_set_type_float(structure_in%retf, trim(adjustl(name)) // '%retf')
     call is_set_type_float(structure_in%he_fraction, trim(adjustl(name)) // '%he_fraction')
     call is_set_type_float(structure_in%ss_fraction, trim(adjustl(name)) // '%ss_fraction')
     call is_set_type_float(structure_in%pow_dens_wp, trim(adjustl(name)) // '%pow_dens_wp')

   end subroutine is_set_type_tf_structure

   subroutine is_set_arr_type_tf_structure(structure_in, name)
 
     implicit none
 
     type (type_tf_structure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tf_structure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tf_structure

   subroutine is_set_type_theta_info(structure_in, name)

     implicit none

     type (type_theta_info), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%angl_type, trim(adjustl(name)) // '%angl_type')
     call is_set_type_matflt_type(structure_in%th2th_pol, trim(adjustl(name)) // '%th2th_pol')

   end subroutine is_set_type_theta_info

   subroutine is_set_arr_type_theta_info(structure_in, name)
 
     implicit none
 
     type (type_theta_info), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_theta_info(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_theta_info

   subroutine is_set_type_topo_regions(structure_in, name)

     implicit none

     type (type_topo_regions), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%ind_omnigen, trim(adjustl(name)) // '%ind_omnigen')
     call is_set_type_array6dflt_type(structure_in%dim1, trim(adjustl(name)) // '%dim1')
     call is_set_type_array6dflt_type(structure_in%dim2, trim(adjustl(name)) // '%dim2')
     call is_set_type_array6dflt_type(structure_in%dim3, trim(adjustl(name)) // '%dim3')
     call is_set_type_array6dflt_type(structure_in%dim4, trim(adjustl(name)) // '%dim4')
     call is_set_type_array6dflt_type(structure_in%dim5, trim(adjustl(name)) // '%dim5')
     call is_set_type_array6dflt_type(structure_in%dim6, trim(adjustl(name)) // '%dim6')
     call is_set_type_array6dflt_type(structure_in%jacobian, trim(adjustl(name)) // '%jacobian')
     call is_set_type_array6dflt_type(structure_in%distfunc, trim(adjustl(name)) // '%distfunc')

   end subroutine is_set_type_topo_regions

   subroutine is_set_arr_type_topo_regions(structure_in, name)
 
     implicit none
 
     type (type_topo_regions), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_topo_regions(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_topo_regions

   subroutine is_set_type_toroid_field(structure_in, name)

     implicit none

     type (type_toroid_field), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%b0, trim(adjustl(name)) // '%b0')
     call is_set_type_float(structure_in%b0prime, trim(adjustl(name)) // '%b0prime')
     call is_set_type_float(structure_in%r0, trim(adjustl(name)) // '%r0')
     call is_set_type_float(structure_in%time, trim(adjustl(name)) // '%time')

   end subroutine is_set_type_toroid_field

   subroutine is_set_arr_type_toroid_field(structure_in, name)
 
     implicit none
 
     type (type_toroid_field), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_toroid_field(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_toroid_field

   subroutine is_set_type_trace(structure_in, name)

     implicit none

     type (type_trace), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%time_orb, trim(adjustl(name)) // '%time_orb')
     call is_set_type_vecint_type(structure_in%ntorb, trim(adjustl(name)) // '%ntorb')
     call is_set_type_matflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_matflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_matflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_matflt_type(structure_in%theta_b, trim(adjustl(name)) // '%theta_b')
     call is_set_type_matflt_type(structure_in%v_parallel, trim(adjustl(name)) // '%v_parallel')
     call is_set_type_matflt_type(structure_in%v_perp, trim(adjustl(name)) // '%v_perp')

   end subroutine is_set_type_trace

   subroutine is_set_arr_type_trace(structure_in, name)
 
     implicit none
 
     type (type_trace), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_trace(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_trace

   subroutine is_set_type_transcoefel(structure_in, name)

     implicit none

     type (type_transcoefel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%diff_eff, trim(adjustl(name)) // '%diff_eff')
     call is_set_type_vecflt_type(structure_in%vconv_eff, trim(adjustl(name)) // '%vconv_eff')
     call is_set_type_vecflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_offdiagel(structure_in%off_diagonal, trim(adjustl(name)) // '%off_diagonal')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')

   end subroutine is_set_type_transcoefel

   subroutine is_set_arr_type_transcoefel(structure_in, name)
 
     implicit none
 
     type (type_transcoefel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_transcoefel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_transcoefel

   subroutine is_set_type_transcoefimp(structure_in, name)

     implicit none

     type (type_transcoefimp), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%diff_eff, trim(adjustl(name)) // '%diff_eff')
     call is_set_type_matflt_type(structure_in%vconv_eff, trim(adjustl(name)) // '%vconv_eff')
     call is_set_type_matflt_type(structure_in%exchange, trim(adjustl(name)) // '%exchange')
     call is_set_type_matflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')

   end subroutine is_set_type_transcoefimp

   subroutine is_set_arr_type_transcoefimp(structure_in, name)
 
     implicit none
 
     type (type_transcoefimp), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_transcoefimp(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_transcoefimp

   subroutine is_set_type_transcoefion(structure_in, name)

     implicit none

     type (type_transcoefion), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%diff_eff, trim(adjustl(name)) // '%diff_eff')
     call is_set_type_matflt_type(structure_in%vconv_eff, trim(adjustl(name)) // '%vconv_eff')
     call is_set_type_matflt_type(structure_in%exchange, trim(adjustl(name)) // '%exchange')
     call is_set_type_matflt_type(structure_in%qgi, trim(adjustl(name)) // '%qgi')
     call is_set_type_matflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_offdiagion(structure_in%off_diagonal, trim(adjustl(name)) // '%off_diagonal')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')

   end subroutine is_set_type_transcoefion

   subroutine is_set_arr_type_transcoefion(structure_in, name)
 
     implicit none
 
     type (type_transcoefion), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_transcoefion(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_transcoefion

   subroutine is_set_type_transcoefvtor(structure_in, name)

     implicit none

     type (type_transcoefvtor), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%diff_eff, trim(adjustl(name)) // '%diff_eff')
     call is_set_type_matflt_type(structure_in%vconv_eff, trim(adjustl(name)) // '%vconv_eff')
     call is_set_type_matflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_offdiagion(structure_in%off_diagonal, trim(adjustl(name)) // '%off_diagonal')
     call is_set_type_integer(structure_in%flag, trim(adjustl(name)) // '%flag')

   end subroutine is_set_type_transcoefvtor

   subroutine is_set_arr_type_transcoefvtor(structure_in, name)
 
     implicit none
 
     type (type_transcoefvtor), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_transcoefvtor(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_transcoefvtor

   subroutine is_set_type_trap_type(structure_in, name)

     implicit none

     type (type_trap_type), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%trap_id, trim(adjustl(name)) // '%trap_id')
     call is_set_type_integer(structure_in%compound, trim(adjustl(name)) // '%compound')
     call is_set_type_integer(structure_in%gas_species, trim(adjustl(name)) // '%gas_species')
     call is_set_type_float(structure_in%energy, trim(adjustl(name)) // '%energy')
     call is_set_type_matflt_type(structure_in%fill_factor, trim(adjustl(name)) // '%fill_factor')
     call is_set_type_matflt_type(structure_in%density, trim(adjustl(name)) // '%density')

   end subroutine is_set_type_trap_type

   subroutine is_set_arr_type_trap_type(structure_in, name)
 
     implicit none
 
     type (type_trap_type), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_trap_type(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_trap_type

   subroutine is_set_type_trianglexyz(structure_in, name)

     implicit none

     type (type_trianglexyz), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_xyz0D(structure_in%point1, trim(adjustl(name)) // '%point1')
     call is_set_type_xyz0D(structure_in%point2, trim(adjustl(name)) // '%point2')
     call is_set_type_xyz0D(structure_in%point3, trim(adjustl(name)) // '%point3')

   end subroutine is_set_type_trianglexyz

   subroutine is_set_arr_type_trianglexyz(structure_in, name)
 
     implicit none
 
     type (type_trianglexyz), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_trianglexyz(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_trianglexyz

   subroutine is_set_type_tsmeasure(structure_in, name)

     implicit none

     type (type_tsmeasure), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_exp1D(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_exp1D(structure_in%ne, trim(adjustl(name)) // '%ne')

   end subroutine is_set_type_tsmeasure

   subroutine is_set_arr_type_tsmeasure(structure_in, name)
 
     implicit none
 
     type (type_tsmeasure), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tsmeasure(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tsmeasure

   subroutine is_set_type_tssetup(structure_in, name)

     implicit none

     type (type_tssetup), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rzphi1D(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_tssetup

   subroutine is_set_arr_type_tssetup(structure_in, name)
 
     implicit none
 
     type (type_tssetup), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_tssetup(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_tssetup

   subroutine is_set_type_turbcomposition(structure_in, name)

     implicit none

     type (type_turbcomposition), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%amn, trim(adjustl(name)) // '%amn')
     call is_set_type_vecflt_type(structure_in%zn, trim(adjustl(name)) // '%zn')
     call is_set_type_vecflt_type(structure_in%zion, trim(adjustl(name)) // '%zion')
     call is_set_type_vecflt_type(structure_in%ie_mass, trim(adjustl(name)) // '%ie_mass')

   end subroutine is_set_type_turbcomposition

   subroutine is_set_arr_type_turbcomposition(structure_in, name)
 
     implicit none
 
     type (type_turbcomposition), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbcomposition(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbcomposition

   subroutine is_set_type_turbcoordsys(structure_in, name)

     implicit none

     type (type_turbcoordsys), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%grid_type, trim(adjustl(name)) // '%grid_type')
     call is_set_type_turbgrid(structure_in%turbgrid, trim(adjustl(name)) // '%turbgrid')
     call is_set_type_matflt_type(structure_in%jacobian, trim(adjustl(name)) // '%jacobian')
     call is_set_type_matflt_type(structure_in%g_11, trim(adjustl(name)) // '%g_11')
     call is_set_type_matflt_type(structure_in%g_12, trim(adjustl(name)) // '%g_12')
     call is_set_type_matflt_type(structure_in%g_13, trim(adjustl(name)) // '%g_13')
     call is_set_type_matflt_type(structure_in%g_22, trim(adjustl(name)) // '%g_22')
     call is_set_type_matflt_type(structure_in%g_23, trim(adjustl(name)) // '%g_23')
     call is_set_type_matflt_type(structure_in%g_33, trim(adjustl(name)) // '%g_33')
     call is_set_type_rzphi3D(structure_in%position, trim(adjustl(name)) // '%position')

   end subroutine is_set_type_turbcoordsys

   subroutine is_set_arr_type_turbcoordsys(structure_in, name)
 
     implicit none
 
     type (type_turbcoordsys), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbcoordsys(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbcoordsys

   subroutine is_set_type_turbenv1d(structure_in, name)

     implicit none

     type (type_turbenv1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%vor, trim(adjustl(name)) // '%vor')
     call is_set_type_vecflt_type(structure_in%jpl, trim(adjustl(name)) // '%jpl')
     call is_set_type_vecflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_vecflt_type(structure_in%he, trim(adjustl(name)) // '%he')
     call is_set_type_vecflt_type(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_matflt_type(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_matflt_type(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_matflt_type(structure_in%ui, trim(adjustl(name)) // '%ui')
     call is_set_type_vecflt_type(structure_in%fe, trim(adjustl(name)) // '%fe')
     call is_set_type_vecflt_type(structure_in%qe, trim(adjustl(name)) // '%qe')
     call is_set_type_matflt_type(structure_in%qi, trim(adjustl(name)) // '%qi')
     call is_set_type_vecflt_type(structure_in%me, trim(adjustl(name)) // '%me')
     call is_set_type_matflt_type(structure_in%mi, trim(adjustl(name)) // '%mi')

   end subroutine is_set_type_turbenv1d

   subroutine is_set_arr_type_turbenv1d(structure_in, name)
 
     implicit none
 
     type (type_turbenv1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbenv1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbenv1d

   subroutine is_set_type_turbgrid(structure_in, name)

     implicit none

     type (type_turbgrid), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%dim1, trim(adjustl(name)) // '%dim1')
     call is_set_type_vecflt_type(structure_in%dim2, trim(adjustl(name)) // '%dim2')
     call is_set_type_vecflt_type(structure_in%dim3, trim(adjustl(name)) // '%dim3')
     call is_set_type_vecflt_type(structure_in%dim_v1, trim(adjustl(name)) // '%dim_v1')
     call is_set_type_vecflt_type(structure_in%dim_v2, trim(adjustl(name)) // '%dim_v2')

   end subroutine is_set_type_turbgrid

   subroutine is_set_arr_type_turbgrid(structure_in, name)
 
     implicit none
 
     type (type_turbgrid), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbgrid(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbgrid

   subroutine is_set_type_turbspec1d(structure_in, name)

     implicit none

     type (type_turbspec1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%kperp, trim(adjustl(name)) // '%kperp')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%vor, trim(adjustl(name)) // '%vor')
     call is_set_type_vecflt_type(structure_in%b, trim(adjustl(name)) // '%b')
     call is_set_type_vecflt_type(structure_in%jpl, trim(adjustl(name)) // '%jpl')
     call is_set_type_vecflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_vecflt_type(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_matflt_type(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_vecflt_type(structure_in%fe, trim(adjustl(name)) // '%fe')
     call is_set_type_vecflt_type(structure_in%qe, trim(adjustl(name)) // '%qe')
     call is_set_type_matflt_type(structure_in%qi, trim(adjustl(name)) // '%qi')
     call is_set_type_vecflt_type(structure_in%me, trim(adjustl(name)) // '%me')
     call is_set_type_matflt_type(structure_in%mi, trim(adjustl(name)) // '%mi')

   end subroutine is_set_type_turbspec1d

   subroutine is_set_arr_type_turbspec1d(structure_in, name)
 
     implicit none
 
     type (type_turbspec1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbspec1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbspec1d

   subroutine is_set_type_turbvar0d(structure_in, name)

     implicit none

     type (type_turbvar0d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%dtime_type, trim(adjustl(name)) // '%dtime_type')
     call is_set_type_vecflt_type(structure_in%dtime, trim(adjustl(name)) // '%dtime')
     call is_set_type_vecflt_type(structure_in%en_exb, trim(adjustl(name)) // '%en_exb')
     call is_set_type_vecflt_type(structure_in%en_mag, trim(adjustl(name)) // '%en_mag')
     call is_set_type_vecflt_type(structure_in%en_el_th, trim(adjustl(name)) // '%en_el_th')
     call is_set_type_matflt_type(structure_in%en_ion_th, trim(adjustl(name)) // '%en_ion_th')
     call is_set_type_vecflt_type(structure_in%en_el_par, trim(adjustl(name)) // '%en_el_par')
     call is_set_type_matflt_type(structure_in%en_ion_par, trim(adjustl(name)) // '%en_ion_par')
     call is_set_type_vecflt_type(structure_in%en_tot, trim(adjustl(name)) // '%en_tot')
     call is_set_type_vecflt_type(structure_in%fl_el, trim(adjustl(name)) // '%fl_el')
     call is_set_type_vecflt_type(structure_in%fl_heatel, trim(adjustl(name)) // '%fl_heatel')
     call is_set_type_matflt_type(structure_in%fl_ion, trim(adjustl(name)) // '%fl_ion')
     call is_set_type_matflt_type(structure_in%fl_heation, trim(adjustl(name)) // '%fl_heation')
     call is_set_type_vecflt_type(structure_in%fl_magel, trim(adjustl(name)) // '%fl_magel')
     call is_set_type_vecflt_type(structure_in%fl_magheatel, trim(adjustl(name)) // '%fl_magheatel')
     call is_set_type_matflt_type(structure_in%fl_magion, trim(adjustl(name)) // '%fl_magion')
     call is_set_type_matflt_type(structure_in%flmagheation, trim(adjustl(name)) // '%flmagheation')

   end subroutine is_set_type_turbvar0d

   subroutine is_set_arr_type_turbvar0d(structure_in, name)
 
     implicit none
 
     type (type_turbvar0d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbvar0d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbvar0d

   subroutine is_set_type_turbvar1d(structure_in, name)

     implicit none

     type (type_turbvar1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%er, trim(adjustl(name)) // '%er')
     call is_set_type_vecflt_type(structure_in%vor, trim(adjustl(name)) // '%vor')
     call is_set_type_vecflt_type(structure_in%apl, trim(adjustl(name)) // '%apl')
     call is_set_type_vecflt_type(structure_in%jpl, trim(adjustl(name)) // '%jpl')
     call is_set_type_vecflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_vecflt_type(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_matflt_type(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_matflt_type(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_matflt_type(structure_in%ui, trim(adjustl(name)) // '%ui')

   end subroutine is_set_type_turbvar1d

   subroutine is_set_arr_type_turbvar1d(structure_in, name)
 
     implicit none
 
     type (type_turbvar1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbvar1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbvar1d

   subroutine is_set_type_turbvar2d(structure_in, name)

     implicit none

     type (type_turbvar2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')
     call is_set_type_matflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_matflt_type(structure_in%apl, trim(adjustl(name)) // '%apl')
     call is_set_type_matflt_type(structure_in%jpl, trim(adjustl(name)) // '%jpl')
     call is_set_type_matflt_type(structure_in%vor, trim(adjustl(name)) // '%vor')
     call is_set_type_matflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')
     call is_set_type_matflt_type(structure_in%te, trim(adjustl(name)) // '%te')
     call is_set_type_array3dflt_type(structure_in%ni, trim(adjustl(name)) // '%ni')
     call is_set_type_array3dflt_type(structure_in%ti, trim(adjustl(name)) // '%ti')
     call is_set_type_array3dflt_type(structure_in%ui, trim(adjustl(name)) // '%ui')

   end subroutine is_set_type_turbvar2d

   subroutine is_set_arr_type_turbvar2d(structure_in, name)
 
     implicit none
 
     type (type_turbvar2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbvar2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbvar2d

   subroutine is_set_type_turbvar3d(structure_in, name)

     implicit none

     type (type_turbvar3d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array3dflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_array3dflt_type(structure_in%vor, trim(adjustl(name)) // '%vor')
     call is_set_type_array3dflt_type(structure_in%jpl, trim(adjustl(name)) // '%jpl')
     call is_set_type_array3dflt_type(structure_in%ne, trim(adjustl(name)) // '%ne')

   end subroutine is_set_type_turbvar3d

   subroutine is_set_arr_type_turbvar3d(structure_in, name)
 
     implicit none
 
     type (type_turbvar3d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbvar3d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbvar3d

   subroutine is_set_type_turbvar4d(structure_in, name)

     implicit none

     type (type_turbvar4d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array4dflt_type(structure_in%fe, trim(adjustl(name)) // '%fe')
     call is_set_type_array5dflt_type(structure_in%fi, trim(adjustl(name)) // '%fi')

   end subroutine is_set_type_turbvar4d

   subroutine is_set_arr_type_turbvar4d(structure_in, name)
 
     implicit none
 
     type (type_turbvar4d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbvar4d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbvar4d

   subroutine is_set_type_turbvar5d(structure_in, name)

     implicit none

     type (type_turbvar5d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_array5dflt_type(structure_in%fe, trim(adjustl(name)) // '%fe')
     call is_set_type_array6dflt_type(structure_in%fi, trim(adjustl(name)) // '%fi')

   end subroutine is_set_type_turbvar5d

   subroutine is_set_arr_type_turbvar5d(structure_in, name)
 
     implicit none
 
     type (type_turbvar5d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_turbvar5d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_turbvar5d

   subroutine is_set_type_version_ind(structure_in, name)

     implicit none

     type (type_version_ind), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%description, trim(adjustl(name)) // '%description')
     call is_set_type_vecstring_type(structure_in%releasedate, trim(adjustl(name)) // '%releasedate')
     call is_set_arr_type_data_release(structure_in%data_release, trim(adjustl(name)) // '%data_release')

   end subroutine is_set_type_version_ind

   subroutine is_set_arr_type_version_ind(structure_in, name)
 
     implicit none
 
     type (type_version_ind), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_version_ind(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_version_ind

   subroutine is_set_type_wall2d(structure_in, name)

     implicit none

     type (type_wall2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%wall_id, trim(adjustl(name)) // '%wall_id')
     call is_set_type_wall_limiter(structure_in%limiter, trim(adjustl(name)) // '%limiter')
     call is_set_type_wall_vessel(structure_in%vessel, trim(adjustl(name)) // '%vessel')
     call is_set_arr_type_plasmaComplexType(structure_in%plasma, trim(adjustl(name)) // '%plasma')
     call is_set_arr_type_wall_unitsComplexType(structure_in%wall_state, trim(adjustl(name)) // '%wall_state')

   end subroutine is_set_type_wall2d

   subroutine is_set_arr_type_wall2d(structure_in, name)
 
     implicit none
 
     type (type_wall2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall2d

   subroutine is_set_type_wall2d_mhd(structure_in, name)

     implicit none

     type (type_wall2d_mhd), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_mhd_res_wall2d(structure_in%res_wall, trim(adjustl(name)) // '%res_wall')
     call is_set_type_mhd_ideal_wall2d(structure_in%ideal_wall, trim(adjustl(name)) // '%ideal_wall')

   end subroutine is_set_type_wall2d_mhd

   subroutine is_set_arr_type_wall2d_mhd(structure_in, name)
 
     implicit none
 
     type (type_wall2d_mhd), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall2d_mhd(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall2d_mhd

   subroutine is_set_type_wall3d(structure_in, name)

     implicit none

     type (type_wall3d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%wall_id, trim(adjustl(name)) // '%wall_id')
     call is_set_type_complexgrid(structure_in%grid, trim(adjustl(name)) // '%grid')
     call is_set_arr_type_plasmaComplexType(structure_in%plasma, trim(adjustl(name)) // '%plasma')
     call is_set_arr_type_wall_unitsComplexType(structure_in%wall_state, trim(adjustl(name)) // '%wall_state')
     call is_set_type_integer(structure_in%basis_index, trim(adjustl(name)) // '%basis_index')

   end subroutine is_set_type_wall3d

   subroutine is_set_arr_type_wall3d(structure_in, name)
 
     implicit none
 
     type (type_wall3d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall3d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall3d

   subroutine is_set_type_wall_blocks(structure_in, name)

     implicit none

     type (type_wall_blocks), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_wall_blocks_unit(structure_in%blocks_unit, trim(adjustl(name)) // '%blocks_unit')

   end subroutine is_set_type_wall_blocks

   subroutine is_set_arr_type_wall_blocks(structure_in, name)
 
     implicit none
 
     type (type_wall_blocks), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_blocks(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_blocks

   subroutine is_set_type_wall_blocks_unit(structure_in, name)

     implicit none

     type (type_wall_blocks_unit), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_float(structure_in%eta, trim(adjustl(name)) // '%eta')
     call is_set_type_float(structure_in%permeability, trim(adjustl(name)) // '%permeability')
     call is_set_type_float(structure_in%j_phi, trim(adjustl(name)) // '%j_phi')
     call is_set_type_float(structure_in%resistance, trim(adjustl(name)) // '%resistance')

   end subroutine is_set_type_wall_blocks_unit

   subroutine is_set_arr_type_wall_blocks_unit(structure_in, name)
 
     implicit none
 
     type (type_wall_blocks_unit), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_blocks_unit(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_blocks_unit

   subroutine is_set_type_wall_limiter(structure_in, name)

     implicit none

     type (type_wall_limiter), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%limiter_id, trim(adjustl(name)) // '%limiter_id')
     call is_set_arr_type_limiter_unit(structure_in%limiter_unit, trim(adjustl(name)) // '%limiter_unit')

   end subroutine is_set_type_wall_limiter

   subroutine is_set_arr_type_wall_limiter(structure_in, name)
 
     implicit none
 
     type (type_wall_limiter), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_limiter(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_limiter

   subroutine is_set_type_wall_types(structure_in, name)

     implicit none

     type (type_wall_types), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%label, trim(adjustl(name)) // '%label')
     call is_set_arr_type_wall_types_layers(structure_in%layers, trim(adjustl(name)) // '%layers')

   end subroutine is_set_type_wall_types

   subroutine is_set_arr_type_wall_types(structure_in, name)
 
     implicit none
 
     type (type_wall_types), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_types(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_types

   subroutine is_set_type_wall_types_layers(structure_in, name)

     implicit none

     type (type_wall_types_layers), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%thickness, trim(adjustl(name)) // '%thickness')
     call is_set_type_vecflt_type(structure_in%chem_comp, trim(adjustl(name)) // '%chem_comp')

   end subroutine is_set_type_wall_types_layers

   subroutine is_set_arr_type_wall_types_layers(structure_in, name)
 
     implicit none
 
     type (type_wall_types_layers), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_types_layers(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_types_layers

   subroutine is_set_type_wall_unitsComplexType(structure_in, name)

     implicit none

     type (type_wall_unitsComplexType), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%wall_type, trim(adjustl(name)) // '%wall_type')
     call is_set_type_integer(structure_in%n_depo_layer, trim(adjustl(name)) // '%n_depo_layer')
     call is_set_arr_type_wall_unitsComplexType_layers(structure_in%layers, trim(adjustl(name)) // '%layers')
     call is_set_type_complexgrid_scalar(structure_in%eta, trim(adjustl(name)) // '%eta')
     call is_set_type_complexgrid_scalar(structure_in%permeability, trim(adjustl(name)) // '%permeability')
     call is_set_type_complexgrid_vector(structure_in%j, trim(adjustl(name)) // '%j')

   end subroutine is_set_type_wall_unitsComplexType

   subroutine is_set_arr_type_wall_unitsComplexType(structure_in, name)
 
     implicit none
 
     type (type_wall_unitsComplexType), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_unitsComplexType(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_unitsComplexType

   subroutine is_set_type_wall_unitsComplexType_layers(structure_in, name)

     implicit none

     type (type_wall_unitsComplexType_layers), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecint_type(structure_in%elements, trim(adjustl(name)) // '%elements')
     call is_set_type_vecint_type(structure_in%gases, trim(adjustl(name)) // '%gases')
     call is_set_type_vecint_type(structure_in%compounds, trim(adjustl(name)) // '%compounds')
     call is_set_type_matflt_type(structure_in%density, trim(adjustl(name)) // '%density')
     call is_set_type_matflt_type(structure_in%dx, trim(adjustl(name)) // '%dx')
     call is_set_type_vecflt_type(structure_in%thickness, trim(adjustl(name)) // '%thickness')
     call is_set_type_array3dflt_type(structure_in%roughness, trim(adjustl(name)) // '%roughness')
     call is_set_type_array3dflt_type(structure_in%porosity, trim(adjustl(name)) // '%porosity')
     call is_set_type_matflt_type(structure_in%dpa, trim(adjustl(name)) // '%dpa')
     call is_set_type_matflt_type(structure_in%temperature, trim(adjustl(name)) // '%temperature')
     call is_set_type_array3dflt_type(structure_in%element_frac, trim(adjustl(name)) // '%element_frac')
     call is_set_type_array3dflt_type(structure_in%chem_comp, trim(adjustl(name)) // '%chem_comp')
     call is_set_type_array4dflt_type(structure_in%bulk_D, trim(adjustl(name)) // '%bulk_D')
     call is_set_type_array4dflt_type(structure_in%surface_D, trim(adjustl(name)) // '%surface_D')
     call is_set_type_array4dflt_type(structure_in%bulk_solute, trim(adjustl(name)) // '%bulk_solute')
     call is_set_type_array4dflt_type(structure_in%surf_solute, trim(adjustl(name)) // '%surf_solute')
     call is_set_type_array3dflt_type(structure_in%pore_content, trim(adjustl(name)) // '%pore_content')
     call is_set_arr_type_trap_type(structure_in%trap_type, trim(adjustl(name)) // '%trap_type')

   end subroutine is_set_type_wall_unitsComplexType_layers

   subroutine is_set_arr_type_wall_unitsComplexType_layers(structure_in, name)
 
     implicit none
 
     type (type_wall_unitsComplexType_layers), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_unitsComplexType_layers(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_unitsComplexType_layers

   subroutine is_set_type_wall_vessel(structure_in, name)

     implicit none

     type (type_wall_vessel), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_identifier(structure_in%vessel_id, trim(adjustl(name)) // '%vessel_id')
     call is_set_arr_type_wall_vessel_unit(structure_in%vessel_unit, trim(adjustl(name)) // '%vessel_unit')

   end subroutine is_set_type_wall_vessel

   subroutine is_set_arr_type_wall_vessel(structure_in, name)
 
     implicit none
 
     type (type_wall_vessel), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_vessel(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_vessel

   subroutine is_set_type_wall_vessel_annular(structure_in, name)

     implicit none

     type (type_wall_vessel_annular), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_rz1D(structure_in%inside, trim(adjustl(name)) // '%inside')
     call is_set_type_rz1D(structure_in%outside, trim(adjustl(name)) // '%outside')
     call is_set_type_float(structure_in%eta, trim(adjustl(name)) // '%eta')
     call is_set_type_float(structure_in%permeability, trim(adjustl(name)) // '%permeability')

   end subroutine is_set_type_wall_vessel_annular

   subroutine is_set_arr_type_wall_vessel_annular(structure_in, name)
 
     implicit none
 
     type (type_wall_vessel_annular), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_vessel_annular(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_vessel_annular

   subroutine is_set_type_wall_vessel_unit(structure_in, name)

     implicit none

     type (type_wall_vessel_unit), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_wall_vessel_annular(structure_in%annular, trim(adjustl(name)) // '%annular')
     call is_set_type_wall_blocks(structure_in%blocks, trim(adjustl(name)) // '%blocks')
     call is_set_type_wall_wall2d_vessel_radial_build(structure_in%radial_build, trim(adjustl(name)) // '%radial_build')

   end subroutine is_set_type_wall_vessel_unit

   subroutine is_set_arr_type_wall_vessel_unit(structure_in, name)
 
     implicit none
 
     type (type_wall_vessel_unit), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_vessel_unit(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_vessel_unit

   subroutine is_set_type_wall_wall0d(structure_in, name)

     implicit none

     type (type_wall_wall0d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%pumping_speed, trim(adjustl(name)) // '%pumping_speed')
     call is_set_type_vecflt_type(structure_in%gas_puff, trim(adjustl(name)) // '%gas_puff')
     call is_set_type_vecflt_type(structure_in%wall_inventory, trim(adjustl(name)) // '%wall_inventory')
     call is_set_type_vecflt_type(structure_in%recycling_coefficient, trim(adjustl(name)) // '%recycling_coefficient')
     call is_set_type_float(structure_in%wall_temperature, trim(adjustl(name)) // '%wall_temperature')
     call is_set_type_float(structure_in%power_from_plasma, trim(adjustl(name)) // '%power_from_plasma')
     call is_set_type_float(structure_in%power_to_cooling, trim(adjustl(name)) // '%power_to_cooling')
     call is_set_type_wall_wall0d_plasma(structure_in%plasma, trim(adjustl(name)) // '%plasma')

   end subroutine is_set_type_wall_wall0d

   subroutine is_set_arr_type_wall_wall0d(structure_in, name)
 
     implicit none
 
     type (type_wall_wall0d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_wall0d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_wall0d

   subroutine is_set_type_wall_wall0d_plasma(structure_in, name)

     implicit none

     type (type_wall_wall0d_plasma), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matint_type(structure_in%species_index, trim(adjustl(name)) // '%species_index')
     call is_set_type_vecflt_type(structure_in%flux, trim(adjustl(name)) // '%flux')
     call is_set_type_vecflt_type(structure_in%energy, trim(adjustl(name)) // '%energy')

   end subroutine is_set_type_wall_wall0d_plasma

   subroutine is_set_arr_type_wall_wall0d_plasma(structure_in, name)
 
     implicit none
 
     type (type_wall_wall0d_plasma), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_wall0d_plasma(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_wall0d_plasma

   subroutine is_set_type_wall_wall2d_vessel_radial_build(structure_in, name)

     implicit none

     type (type_wall_wall2d_vessel_radial_build), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%r1_inb, trim(adjustl(name)) // '%r1_inb')
     call is_set_type_float(structure_in%r2_inb, trim(adjustl(name)) // '%r2_inb')
     call is_set_type_float(structure_in%r1_outb, trim(adjustl(name)) // '%r1_outb')
     call is_set_type_float(structure_in%r2_outb, trim(adjustl(name)) // '%r2_outb')
     call is_set_type_float(structure_in%raddim, trim(adjustl(name)) // '%raddim')
     call is_set_type_float(structure_in%nmat, trim(adjustl(name)) // '%nmat')
     call is_set_type_vecflt_type(structure_in%composition, trim(adjustl(name)) // '%composition')
     call is_set_type_float(structure_in%pow_dens_inb, trim(adjustl(name)) // '%pow_dens_inb')
     call is_set_type_float(structure_in%pow_dens_outb, trim(adjustl(name)) // '%pow_dens_outb')
     call is_set_type_float(structure_in%fn_flux_inb, trim(adjustl(name)) // '%fn_flux_inb')
     call is_set_type_float(structure_in%fn_flux_outb, trim(adjustl(name)) // '%fn_flux_outb')

   end subroutine is_set_type_wall_wall2d_vessel_radial_build

   subroutine is_set_arr_type_wall_wall2d_vessel_radial_build(structure_in, name)
 
     implicit none
 
     type (type_wall_wall2d_vessel_radial_build), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_wall_wall2d_vessel_radial_build(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_wall_wall2d_vessel_radial_build

   subroutine is_set_type_waveguides(structure_in, name)

     implicit none

     type (type_waveguides), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%nwm_theta, trim(adjustl(name)) // '%nwm_theta')
     call is_set_type_integer(structure_in%nwm_phi, trim(adjustl(name)) // '%nwm_phi')
     call is_set_type_vecint_type(structure_in%mask, trim(adjustl(name)) // '%mask')
     call is_set_type_integer(structure_in%npwbm_phi, trim(adjustl(name)) // '%npwbm_phi')
     call is_set_type_integer(structure_in%npwe_phi, trim(adjustl(name)) // '%npwe_phi')
     call is_set_type_float(structure_in%sw_theta, trim(adjustl(name)) // '%sw_theta')
     call is_set_type_float(structure_in%hw_theta, trim(adjustl(name)) // '%hw_theta')
     call is_set_type_float(structure_in%bwa, trim(adjustl(name)) // '%bwa')
     call is_set_type_float(structure_in%biwp, trim(adjustl(name)) // '%biwp')
     call is_set_type_float(structure_in%bewp, trim(adjustl(name)) // '%bewp')
     call is_set_type_vecflt_type(structure_in%e_phi, trim(adjustl(name)) // '%e_phi')
     call is_set_type_vecflt_type(structure_in%scl, trim(adjustl(name)) // '%scl')

   end subroutine is_set_type_waveguides

   subroutine is_set_arr_type_waveguides(structure_in, name)
 
     implicit none
 
     type (type_waveguides), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waveguides(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waveguides

   subroutine is_set_type_waves_global_param(structure_in, name)

     implicit none

     type (type_waves_global_param), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%name, trim(adjustl(name)) // '%name')
     call is_set_type_vecstring_type(structure_in%type, trim(adjustl(name)) // '%type')
     call is_set_type_vecint_type(structure_in%f_assumption, trim(adjustl(name)) // '%f_assumption')
     call is_set_type_integer(structure_in%code_type, trim(adjustl(name)) // '%code_type')
     call is_set_type_float(structure_in%frequency, trim(adjustl(name)) // '%frequency')
     call is_set_type_vecint_type(structure_in%ntor, trim(adjustl(name)) // '%ntor')
     call is_set_type_float(structure_in%power_tot, trim(adjustl(name)) // '%power_tot')
     call is_set_type_vecflt_type(structure_in%p_frac_ntor, trim(adjustl(name)) // '%p_frac_ntor')
     call is_set_type_float(structure_in%pow_e, trim(adjustl(name)) // '%pow_e')
     call is_set_type_vecflt_type(structure_in%pow_i, trim(adjustl(name)) // '%pow_i')
     call is_set_type_matflt_type(structure_in%pow_z, trim(adjustl(name)) // '%pow_z')
     call is_set_type_float(structure_in%pow_fe, trim(adjustl(name)) // '%pow_fe')
     call is_set_type_vecflt_type(structure_in%pow_fi, trim(adjustl(name)) // '%pow_fi')
     call is_set_type_matflt_type(structure_in%pow_fz, trim(adjustl(name)) // '%pow_fz')
     call is_set_type_vecflt_type(structure_in%pow_ntor_e, trim(adjustl(name)) // '%pow_ntor_e')
     call is_set_type_matflt_type(structure_in%pow_ntor_i, trim(adjustl(name)) // '%pow_ntor_i')
     call is_set_type_array3dflt_type(structure_in%pow_ntor_z, trim(adjustl(name)) // '%pow_ntor_z')
     call is_set_type_vecflt_type(structure_in%pow_ntor_fe, trim(adjustl(name)) // '%pow_ntor_fe')
     call is_set_type_matflt_type(structure_in%pow_ntor_fi, trim(adjustl(name)) // '%pow_ntor_fi')
     call is_set_type_array3dflt_type(structure_in%pow_ntor_fz, trim(adjustl(name)) // '%pow_ntor_fz')
     call is_set_type_float(structure_in%cur_tor, trim(adjustl(name)) // '%cur_tor')
     call is_set_type_vecflt_type(structure_in%cur_tor_ntor, trim(adjustl(name)) // '%cur_tor_ntor')
     call is_set_type_rz0D(structure_in%mag_axis, trim(adjustl(name)) // '%mag_axis')
     call is_set_type_b0r0(structure_in%toroid_field, trim(adjustl(name)) // '%toroid_field')

   end subroutine is_set_type_waves_global_param

   subroutine is_set_arr_type_waves_global_param(structure_in, name)
 
     implicit none
 
     type (type_waves_global_param), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_global_param(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_global_param

   subroutine is_set_type_waves_grid_1d(structure_in, name)

     implicit none

     type (type_waves_grid_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_vecflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%volume, trim(adjustl(name)) // '%volume')
     call is_set_type_vecflt_type(structure_in%area, trim(adjustl(name)) // '%area')

   end subroutine is_set_type_waves_grid_1d

   subroutine is_set_arr_type_waves_grid_1d(structure_in, name)
 
     implicit none
 
     type (type_waves_grid_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_grid_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_grid_1d

   subroutine is_set_type_waves_grid_2d(structure_in, name)

     implicit none

     type (type_waves_grid_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_integer(structure_in%grid_type, trim(adjustl(name)) // '%grid_type')
     call is_set_type_matflt_type(structure_in%rho_tor_norm, trim(adjustl(name)) // '%rho_tor_norm')
     call is_set_type_matflt_type(structure_in%rho_tor, trim(adjustl(name)) // '%rho_tor')
     call is_set_type_matflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_matflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')
     call is_set_type_matflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_matflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_theta_info(structure_in%theta_info, trim(adjustl(name)) // '%theta_info')

   end subroutine is_set_type_waves_grid_2d

   subroutine is_set_arr_type_waves_grid_2d(structure_in, name)
 
     implicit none
 
     type (type_waves_grid_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_grid_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_grid_2d

   subroutine is_set_type_waves_profiles_1d(structure_in, name)

     implicit none

     type (type_waves_profiles_1d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%powd_tot, trim(adjustl(name)) // '%powd_tot')
     call is_set_type_vecflt_type(structure_in%powd_e, trim(adjustl(name)) // '%powd_e')
     call is_set_type_matflt_type(structure_in%powd_i, trim(adjustl(name)) // '%powd_i')
     call is_set_type_array3dflt_type(structure_in%powd_z, trim(adjustl(name)) // '%powd_z')
     call is_set_type_vecflt_type(structure_in%powd_fe, trim(adjustl(name)) // '%powd_fe')
     call is_set_type_matflt_type(structure_in%powd_fi, trim(adjustl(name)) // '%powd_fi')
     call is_set_type_array3dflt_type(structure_in%powd_fz, trim(adjustl(name)) // '%powd_fz')
     call is_set_type_matflt_type(structure_in%powd_ntor, trim(adjustl(name)) // '%powd_ntor')
     call is_set_type_matflt_type(structure_in%powd_ntor_e, trim(adjustl(name)) // '%powd_ntor_e')
     call is_set_type_array3dflt_type(structure_in%powd_ntor_i, trim(adjustl(name)) // '%powd_ntor_i')
     call is_set_type_array4dflt_type(structure_in%powd_ntor_z, trim(adjustl(name)) // '%powd_ntor_z')
     call is_set_type_matflt_type(structure_in%powd_ntor_fe, trim(adjustl(name)) // '%powd_ntor_fe')
     call is_set_type_array3dflt_type(structure_in%powd_ntor_fi, trim(adjustl(name)) // '%powd_ntor_fi')
     call is_set_type_array4dflt_type(structure_in%powd_ntor_fz, trim(adjustl(name)) // '%powd_ntor_fz')
     call is_set_type_vecflt_type(structure_in%curd_tor, trim(adjustl(name)) // '%curd_tor')
     call is_set_type_matflt_type(structure_in%curd_torntor, trim(adjustl(name)) // '%curd_torntor')
     call is_set_type_vecflt_type(structure_in%pow_tot, trim(adjustl(name)) // '%pow_tot')
     call is_set_type_vecflt_type(structure_in%pow_e, trim(adjustl(name)) // '%pow_e')
     call is_set_type_matflt_type(structure_in%pow_i, trim(adjustl(name)) // '%pow_i')
     call is_set_type_array3dflt_type(structure_in%pow_z, trim(adjustl(name)) // '%pow_z')
     call is_set_type_vecflt_type(structure_in%pow_fe, trim(adjustl(name)) // '%pow_fe')
     call is_set_type_matflt_type(structure_in%pow_fi, trim(adjustl(name)) // '%pow_fi')
     call is_set_type_array3dflt_type(structure_in%pow_fz, trim(adjustl(name)) // '%pow_fz')
     call is_set_type_matflt_type(structure_in%pow_ntor, trim(adjustl(name)) // '%pow_ntor')
     call is_set_type_matflt_type(structure_in%pow_ntor_e, trim(adjustl(name)) // '%pow_ntor_e')
     call is_set_type_array3dflt_type(structure_in%pow_ntor_i, trim(adjustl(name)) // '%pow_ntor_i')
     call is_set_type_array3dflt_type(structure_in%pow_ntor_z, trim(adjustl(name)) // '%pow_ntor_z')
     call is_set_type_matflt_type(structure_in%pow_ntor_fe, trim(adjustl(name)) // '%pow_ntor_fe')
     call is_set_type_array3dflt_type(structure_in%pow_ntor_fi, trim(adjustl(name)) // '%pow_ntor_fi')
     call is_set_type_array3dflt_type(structure_in%pow_ntor_fz, trim(adjustl(name)) // '%pow_ntor_fz')
     call is_set_type_vecflt_type(structure_in%curd_par, trim(adjustl(name)) // '%curd_par')
     call is_set_type_matflt_type(structure_in%curd_parntor, trim(adjustl(name)) // '%curd_parntor')
     call is_set_type_vecflt_type(structure_in%cur_tor, trim(adjustl(name)) // '%cur_tor')
     call is_set_type_matflt_type(structure_in%cur_tor_ntor, trim(adjustl(name)) // '%cur_tor_ntor')
     call is_set_type_matflt_type(structure_in%e_plus_ave, trim(adjustl(name)) // '%e_plus_ave')
     call is_set_type_matflt_type(structure_in%e_minus_ave, trim(adjustl(name)) // '%e_minus_ave')
     call is_set_type_matflt_type(structure_in%e_para_ave, trim(adjustl(name)) // '%e_para_ave')
     call is_set_type_matflt_type(structure_in%k_perp_ave, trim(adjustl(name)) // '%k_perp_ave')

   end subroutine is_set_type_waves_profiles_1d

   subroutine is_set_arr_type_waves_profiles_1d(structure_in, name)
 
     implicit none
 
     type (type_waves_profiles_1d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_profiles_1d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_profiles_1d

   subroutine is_set_type_waves_profiles_2d(structure_in, name)

     implicit none

     type (type_waves_profiles_2d), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_matflt_type(structure_in%powd_tot, trim(adjustl(name)) // '%powd_tot')
     call is_set_type_matflt_type(structure_in%powd_e, trim(adjustl(name)) // '%powd_e')
     call is_set_type_array3dflt_type(structure_in%powd_i, trim(adjustl(name)) // '%powd_i')
     call is_set_type_array4dflt_type(structure_in%powd_z, trim(adjustl(name)) // '%powd_z')
     call is_set_type_matflt_type(structure_in%powd_fe, trim(adjustl(name)) // '%powd_fe')
     call is_set_type_array3dflt_type(structure_in%powd_fi, trim(adjustl(name)) // '%powd_fi')
     call is_set_type_array4dflt_type(structure_in%powd_fz, trim(adjustl(name)) // '%powd_fz')
     call is_set_type_array3dflt_type(structure_in%powd_ntor, trim(adjustl(name)) // '%powd_ntor')
     call is_set_type_array3dflt_type(structure_in%powd_ntor_e, trim(adjustl(name)) // '%powd_ntor_e')
     call is_set_type_array4dflt_type(structure_in%powd_ntor_i, trim(adjustl(name)) // '%powd_ntor_i')
     call is_set_type_array5dflt_type(structure_in%powd_ntor_z, trim(adjustl(name)) // '%powd_ntor_z')
     call is_set_type_array3dflt_type(structure_in%powd_ntor_fe, trim(adjustl(name)) // '%powd_ntor_fe')
     call is_set_type_array4dflt_type(structure_in%powd_ntor_fi, trim(adjustl(name)) // '%powd_ntor_fi')
     call is_set_type_array5dflt_type(structure_in%powd_ntor_fz, trim(adjustl(name)) // '%powd_ntor_fz')
     call is_set_type_array5dflt_type(structure_in%powd_iharm, trim(adjustl(name)) // '%powd_iharm')

   end subroutine is_set_type_waves_profiles_2d

   subroutine is_set_arr_type_waves_profiles_2d(structure_in, name)
 
     implicit none
 
     type (type_waves_profiles_2d), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_profiles_2d(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_profiles_2d

   subroutine is_set_type_waves_rtposition(structure_in, name)

     implicit none

     type (type_waves_rtposition), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%r, trim(adjustl(name)) // '%r')
     call is_set_type_vecflt_type(structure_in%z, trim(adjustl(name)) // '%z')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')
     call is_set_type_vecflt_type(structure_in%psi, trim(adjustl(name)) // '%psi')
     call is_set_type_vecflt_type(structure_in%theta, trim(adjustl(name)) // '%theta')

   end subroutine is_set_type_waves_rtposition

   subroutine is_set_arr_type_waves_rtposition(structure_in, name)
 
     implicit none
 
     type (type_waves_rtposition), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_rtposition(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_rtposition

   subroutine is_set_type_waves_rtwavevector(structure_in, name)

     implicit none

     type (type_waves_rtwavevector), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%kr, trim(adjustl(name)) // '%kr')
     call is_set_type_vecflt_type(structure_in%kz, trim(adjustl(name)) // '%kz')
     call is_set_type_vecflt_type(structure_in%kphi, trim(adjustl(name)) // '%kphi')
     call is_set_type_vecflt_type(structure_in%npar, trim(adjustl(name)) // '%npar')
     call is_set_type_vecflt_type(structure_in%nperp, trim(adjustl(name)) // '%nperp')
     call is_set_type_vecflt_type(structure_in%ntor, trim(adjustl(name)) // '%ntor')
     call is_set_type_integer(structure_in%var_ntor, trim(adjustl(name)) // '%var_ntor')

   end subroutine is_set_type_waves_rtwavevector

   subroutine is_set_arr_type_waves_rtwavevector(structure_in, name)
 
     implicit none
 
     type (type_waves_rtwavevector), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_waves_rtwavevector(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_waves_rtwavevector

   subroutine is_set_type_weighted_markers(structure_in, name)

     implicit none

     type (type_weighted_markers), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_arr_type_identifier(structure_in%variable_ids, trim(adjustl(name)) // '%variable_ids')
     call is_set_type_matflt_type(structure_in%coord, trim(adjustl(name)) // '%coord')
     call is_set_type_vecflt_type(structure_in%weight, trim(adjustl(name)) // '%weight')

   end subroutine is_set_type_weighted_markers

   subroutine is_set_arr_type_weighted_markers(structure_in, name)
 
     implicit none
 
     type (type_weighted_markers), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_weighted_markers(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_weighted_markers

   subroutine is_set_type_whatref(structure_in, name)

     implicit none

     type (type_whatref), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecstring_type(structure_in%user, trim(adjustl(name)) // '%user')
     call is_set_type_vecstring_type(structure_in%machine, trim(adjustl(name)) // '%machine')
     call is_set_type_integer(structure_in%shot, trim(adjustl(name)) // '%shot')
     call is_set_type_integer(structure_in%run, trim(adjustl(name)) // '%run')
     call is_set_type_integer(structure_in%occurrence, trim(adjustl(name)) // '%occurrence')

   end subroutine is_set_type_whatref

   subroutine is_set_arr_type_whatref(structure_in, name)
 
     implicit none
 
     type (type_whatref), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_whatref(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_whatref

   subroutine is_set_type_width(structure_in, name)

     implicit none

     type (type_width), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_vecflt_type(structure_in%dtheta, trim(adjustl(name)) // '%dtheta')
     call is_set_type_vecflt_type(structure_in%phi, trim(adjustl(name)) // '%phi')

   end subroutine is_set_type_width

   subroutine is_set_arr_type_width(structure_in, name)
 
     implicit none
 
     type (type_width), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_width(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_width

   subroutine is_set_type_xpts(structure_in, name)

     implicit none

     type (type_xpts), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_rz1D(structure_in%position, trim(adjustl(name)) // '%position')
     call is_set_type_vecstring_type(structure_in%source, trim(adjustl(name)) // '%source')
     call is_set_type_vecflt_type(structure_in%weight, trim(adjustl(name)) // '%weight')
     call is_set_type_vecflt_type(structure_in%sigma, trim(adjustl(name)) // '%sigma')
     call is_set_type_vecflt_type(structure_in%calculated, trim(adjustl(name)) // '%calculated')
     call is_set_type_vecflt_type(structure_in%chi2, trim(adjustl(name)) // '%chi2')

   end subroutine is_set_type_xpts

   subroutine is_set_arr_type_xpts(structure_in, name)
 
     implicit none
 
     type (type_xpts), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_xpts(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_xpts

   subroutine is_set_type_xyz0D(structure_in, name)

     implicit none

     type (type_xyz0D), intent(in) :: structure_in
     character(len = *), intent(in) :: name

     call is_set_type_float(structure_in%x, trim(adjustl(name)) // '%x')
     call is_set_type_float(structure_in%y, trim(adjustl(name)) // '%y')
     call is_set_type_float(structure_in%z, trim(adjustl(name)) // '%z')

   end subroutine is_set_type_xyz0D

   subroutine is_set_arr_type_xyz0D(structure_in, name)
 
     implicit none
 
     type (type_xyz0D), pointer :: structure_in(:)
     character(len = *), intent(in) :: name
     integer :: i
 
     if (associated(structure_in)) then
       write(iu6, *) trim(adjustl(name)) // ': T'
       do i = 1, size(structure_in)
         call is_set_type_xyz0D(structure_in(i), name)
       end do
     else
       write(iu6, *) trim(adjustl(name)) // ': F'
     end if

   end subroutine is_set_arr_type_xyz0D


 end module is_set_structures
