 module deallocate_structures
 !------------------------------------------------------------------
 ! module for granular deallocation of euitm structures
 !------------------------------------------------------------------

   use euitm_schemas

   interface deallocate_cpo
      module procedure deallocate_type_array3dcplx_type
      module procedure deallocate_type_array3dflt_type
      module procedure deallocate_type_array3dint_type
      module procedure deallocate_type_array4dflt_type
      module procedure deallocate_type_array5dflt_type
      module procedure deallocate_type_array6dflt_type
      module procedure deallocate_type_array7dflt_type
      module procedure deallocate_type_matcplx_type
      module procedure deallocate_type_matflt_type
      module procedure deallocate_type_matint_type
      module procedure deallocate_type_veccplx_type
      module procedure deallocate_type_vecflt_type
      module procedure deallocate_type_vecint_type
      module procedure deallocate_type_vecstring_type
      module procedure deallocate_type_amns
      module procedure deallocate_arr_type_amns
      module procedure deallocate_type_antennas
      module procedure deallocate_arr_type_antennas
      module procedure deallocate_type_bb_shield
      module procedure deallocate_arr_type_bb_shield
      module procedure deallocate_type_compositionc
      module procedure deallocate_arr_type_compositionc
      module procedure deallocate_type_coredelta
      module procedure deallocate_arr_type_coredelta
      module procedure deallocate_type_corefast
      module procedure deallocate_arr_type_corefast
      module procedure deallocate_type_coreimpur
      module procedure deallocate_arr_type_coreimpur
      module procedure deallocate_type_coreneutrals
      module procedure deallocate_arr_type_coreneutrals
      module procedure deallocate_type_coreprof
      module procedure deallocate_arr_type_coreprof
      module procedure deallocate_type_coresource
      module procedure deallocate_arr_type_coresource
      module procedure deallocate_type_coretransp
      module procedure deallocate_arr_type_coretransp
      module procedure deallocate_type_cxdiag
      module procedure deallocate_arr_type_cxdiag
      module procedure deallocate_type_distribution
      module procedure deallocate_arr_type_distribution
      module procedure deallocate_type_distsource
      module procedure deallocate_arr_type_distsource
      module procedure deallocate_type_ecediag
      module procedure deallocate_arr_type_ecediag
      module procedure deallocate_type_edge
      module procedure deallocate_arr_type_edge
      module procedure deallocate_type_efcc
      module procedure deallocate_arr_type_efcc
      module procedure deallocate_type_equilibrium
      module procedure deallocate_arr_type_equilibrium
      module procedure deallocate_type_fusiondiag
      module procedure deallocate_arr_type_fusiondiag
      module procedure deallocate_type_halphadiag
      module procedure deallocate_arr_type_halphadiag
      module procedure deallocate_type_heat_sources
      module procedure deallocate_arr_type_heat_sources
      module procedure deallocate_type_interfdiag
      module procedure deallocate_arr_type_interfdiag
      module procedure deallocate_type_ironmodel
      module procedure deallocate_arr_type_ironmodel
      module procedure deallocate_type_langmuirdiag
      module procedure deallocate_arr_type_langmuirdiag
      module procedure deallocate_type_launchs
      module procedure deallocate_arr_type_launchs
      module procedure deallocate_type_lithiumdiag
      module procedure deallocate_arr_type_lithiumdiag
      module procedure deallocate_type_magdiag
      module procedure deallocate_arr_type_magdiag
      module procedure deallocate_type_mhd
      module procedure deallocate_arr_type_mhd
      module procedure deallocate_type_msediag
      module procedure deallocate_arr_type_msediag
      module procedure deallocate_type_nbi
      module procedure deallocate_arr_type_nbi
      module procedure deallocate_type_neoclassic
      module procedure deallocate_arr_type_neoclassic
      module procedure deallocate_type_ntm
      module procedure deallocate_arr_type_ntm
      module procedure deallocate_type_orbit
      module procedure deallocate_arr_type_orbit
      module procedure deallocate_type_pellets
      module procedure deallocate_arr_type_pellets
      module procedure deallocate_type_pfsystems
      module procedure deallocate_arr_type_pfsystems
      module procedure deallocate_type_polardiag
      module procedure deallocate_arr_type_polardiag
      module procedure deallocate_type_power_conv
      module procedure deallocate_arr_type_power_conv
      module procedure deallocate_type_reflectomet
      module procedure deallocate_arr_type_reflectomet
      module procedure deallocate_type_rfadiag
      module procedure deallocate_arr_type_rfadiag
      module procedure deallocate_type_sawteeth
      module procedure deallocate_arr_type_sawteeth
      module procedure deallocate_type_scenario
      module procedure deallocate_arr_type_scenario
      module procedure deallocate_type_solcurdiag
      module procedure deallocate_arr_type_solcurdiag
      module procedure deallocate_type_temporary
      module procedure deallocate_arr_type_temporary
      module procedure deallocate_type_topinfo
      module procedure deallocate_arr_type_topinfo
      module procedure deallocate_type_toroidfield
      module procedure deallocate_arr_type_toroidfield
      module procedure deallocate_type_tsdiag
      module procedure deallocate_arr_type_tsdiag
      module procedure deallocate_type_turbulence
      module procedure deallocate_arr_type_turbulence
      module procedure deallocate_type_wall
      module procedure deallocate_arr_type_wall
      module procedure deallocate_type_waves
      module procedure deallocate_arr_type_waves
      module procedure deallocate_type_amns_constituentType
      module procedure deallocate_arr_type_amns_constituentType
      module procedure deallocate_type_amns_processType
      module procedure deallocate_arr_type_amns_processType
      module procedure deallocate_type_antenna_ec
      module procedure deallocate_arr_type_antenna_ec
      module procedure deallocate_type_antenna_ic
      module procedure deallocate_arr_type_antenna_ic
      module procedure deallocate_type_antenna_lh
      module procedure deallocate_arr_type_antenna_lh
      module procedure deallocate_type_antennaic_setup
      module procedure deallocate_arr_type_antennaic_setup
      module procedure deallocate_type_antennalh_setup
      module procedure deallocate_arr_type_antennalh_setup
      module procedure deallocate_type_b0r0
      module procedure deallocate_arr_type_b0r0
      module procedure deallocate_type_bb
      module procedure deallocate_arr_type_bb
      module procedure deallocate_type_bb_dimension
      module procedure deallocate_arr_type_bb_dimension
      module procedure deallocate_type_bb_geometry
      module procedure deallocate_arr_type_bb_geometry
      module procedure deallocate_type_bb_specs
      module procedure deallocate_arr_type_bb_specs
      module procedure deallocate_type_beamletgroup
      module procedure deallocate_arr_type_beamletgroup
      module procedure deallocate_type_beamlets
      module procedure deallocate_arr_type_beamlets
      module procedure deallocate_type_beamtracing
      module procedure deallocate_arr_type_beamtracing
      module procedure deallocate_type_boundary
      module procedure deallocate_arr_type_boundary
      module procedure deallocate_type_boundary_neutrals
      module procedure deallocate_arr_type_boundary_neutrals
      module procedure deallocate_type_boundaryel
      module procedure deallocate_arr_type_boundaryel
      module procedure deallocate_type_boundaryimp
      module procedure deallocate_arr_type_boundaryimp
      module procedure deallocate_type_boundaryion
      module procedure deallocate_arr_type_boundaryion
      module procedure deallocate_type_bpol_probes
      module procedure deallocate_arr_type_bpol_probes
      module procedure deallocate_type_calorimetry_heat_source
      module procedure deallocate_arr_type_calorimetry_heat_source
      module procedure deallocate_type_circuits
      module procedure deallocate_arr_type_circuits
      module procedure deallocate_type_circularcoil
      module procedure deallocate_arr_type_circularcoil
      module procedure deallocate_type_clusters
      module procedure deallocate_arr_type_clusters
      module procedure deallocate_type_codeparam
      module procedure deallocate_arr_type_codeparam
      module procedure deallocate_type_coefficients_neutrals
      module procedure deallocate_arr_type_coefficients_neutrals
      module procedure deallocate_type_coherentwave
      module procedure deallocate_arr_type_coherentwave
      module procedure deallocate_type_coil
      module procedure deallocate_arr_type_coil
      module procedure deallocate_type_com
      module procedure deallocate_arr_type_com
      module procedure deallocate_type_complexgrid
      module procedure deallocate_arr_type_complexgrid
      module procedure deallocate_type_complexgrid_geo_global
      module procedure deallocate_arr_type_complexgrid_geo_global
      module procedure deallocate_type_complexgrid_indexlist
      module procedure deallocate_arr_type_complexgrid_indexlist
      module procedure deallocate_type_complexgrid_metric
      module procedure deallocate_arr_type_complexgrid_metric
      module procedure deallocate_type_complexgrid_objectlist
      module procedure deallocate_arr_type_complexgrid_objectlist
      module procedure deallocate_type_complexgrid_scalar
      module procedure deallocate_arr_type_complexgrid_scalar
      module procedure deallocate_type_complexgrid_scalar_cplx
      module procedure deallocate_arr_type_complexgrid_scalar_cplx
      module procedure deallocate_type_complexgrid_scalar_int
      module procedure deallocate_arr_type_complexgrid_scalar_int
      module procedure deallocate_type_complexgrid_scalar_simplestruct
      module procedure deallocate_arr_type_complexgrid_scalar_simplestruct
      module procedure deallocate_type_complexgrid_space
      module procedure deallocate_arr_type_complexgrid_space
      module procedure deallocate_type_complexgrid_subgrid
      module procedure deallocate_arr_type_complexgrid_subgrid
      module procedure deallocate_type_complexgrid_vector
      module procedure deallocate_arr_type_complexgrid_vector
      module procedure deallocate_type_complexgrid_vector_simplestruct
      module procedure deallocate_arr_type_complexgrid_vector_simplestruct
      module procedure deallocate_type_composition
      module procedure deallocate_arr_type_composition
      module procedure deallocate_type_composition_neutrals
      module procedure deallocate_arr_type_composition_neutrals
      module procedure deallocate_type_composition_neutrals_neutcomp
      module procedure deallocate_arr_type_composition_neutrals_neutcomp
      module procedure deallocate_type_composition_neutralscomp
      module procedure deallocate_arr_type_composition_neutralscomp
      module procedure deallocate_type_compositions_type
      module procedure deallocate_arr_type_compositions_type
      module procedure deallocate_type_compound_desc
      module procedure deallocate_arr_type_compound_desc
      module procedure deallocate_type_coord_sys
      module procedure deallocate_arr_type_coord_sys
      module procedure deallocate_type_coordinates
      module procedure deallocate_arr_type_coordinates
      module procedure deallocate_type_coords
      module procedure deallocate_arr_type_coords
      module procedure deallocate_type_coredelta_values
      module procedure deallocate_arr_type_coredelta_values
      module procedure deallocate_type_coredelta_values_impurity
      module procedure deallocate_arr_type_coredelta_values_impurity
      module procedure deallocate_type_corefast_values
      module procedure deallocate_arr_type_corefast_values
      module procedure deallocate_type_corefield
      module procedure deallocate_arr_type_corefield
      module procedure deallocate_type_corefieldion
      module procedure deallocate_arr_type_corefieldion
      module procedure deallocate_type_corefieldneutral
      module procedure deallocate_arr_type_corefieldneutral
      module procedure deallocate_type_corefieldneutrale
      module procedure deallocate_arr_type_corefieldneutrale
      module procedure deallocate_type_corefieldneutralv
      module procedure deallocate_arr_type_corefieldneutralv
      module procedure deallocate_type_corefieldneutralv0
      module procedure deallocate_arr_type_corefieldneutralv0
      module procedure deallocate_type_coreimpurdiag_sum_radiation
      module procedure deallocate_arr_type_coreimpurdiag_sum_radiation
      module procedure deallocate_type_coreimpurediag_energy
      module procedure deallocate_arr_type_coreimpurediag_energy
      module procedure deallocate_type_coreimpurediag_radiation
      module procedure deallocate_arr_type_coreimpurediag_radiation
      module procedure deallocate_type_coreimpurediag_sum
      module procedure deallocate_arr_type_coreimpurediag_sum
      module procedure deallocate_type_coreimpurediag_sum_energy
      module procedure deallocate_arr_type_coreimpurediag_sum_energy
      module procedure deallocate_type_coreimpurediag_type
      module procedure deallocate_arr_type_coreimpurediag_type
      module procedure deallocate_type_coreimpurediagprof_type
      module procedure deallocate_arr_type_coreimpurediagprof_type
      module procedure deallocate_type_coreimpurediagsum_type
      module procedure deallocate_arr_type_coreimpurediagsum_type
      module procedure deallocate_type_coreneutrals_atomlist
      module procedure deallocate_arr_type_coreneutrals_atomlist
      module procedure deallocate_type_coreneutrals_neutraltype
      module procedure deallocate_arr_type_coreneutrals_neutraltype
      module procedure deallocate_type_coreprofile
      module procedure deallocate_arr_type_coreprofile
      module procedure deallocate_type_coreprofion
      module procedure deallocate_arr_type_coreprofion
      module procedure deallocate_type_coresource_values
      module procedure deallocate_arr_type_coresource_values
      module procedure deallocate_type_coretransel
      module procedure deallocate_arr_type_coretransel
      module procedure deallocate_type_coretransimp
      module procedure deallocate_arr_type_coretransimp
      module procedure deallocate_type_coretransion
      module procedure deallocate_arr_type_coretransion
      module procedure deallocate_type_coretransp_values
      module procedure deallocate_arr_type_coretransp_values
      module procedure deallocate_type_current
      module procedure deallocate_arr_type_current
      module procedure deallocate_type_cxmeasure
      module procedure deallocate_arr_type_cxmeasure
      module procedure deallocate_type_cxsetup
      module procedure deallocate_arr_type_cxsetup
      module procedure deallocate_type_data_release
      module procedure deallocate_arr_type_data_release
      module procedure deallocate_type_datainfo
      module procedure deallocate_arr_type_datainfo
      module procedure deallocate_type_desc_coils
      module procedure deallocate_arr_type_desc_coils
      module procedure deallocate_type_desc_impur
      module procedure deallocate_arr_type_desc_impur
      module procedure deallocate_type_desc_iron
      module procedure deallocate_arr_type_desc_iron
      module procedure deallocate_type_desc_pfcoils
      module procedure deallocate_arr_type_desc_pfcoils
      module procedure deallocate_type_desc_supply
      module procedure deallocate_arr_type_desc_supply
      module procedure deallocate_type_diag_func
      module procedure deallocate_arr_type_diag_func
      module procedure deallocate_type_dist_collisional_transfer_0d
      module procedure deallocate_arr_type_dist_collisional_transfer_0d
      module procedure deallocate_type_dist_collisional_transfer_1d
      module procedure deallocate_arr_type_dist_collisional_transfer_1d
      module procedure deallocate_type_dist_collisional_transfer_2d
      module procedure deallocate_arr_type_dist_collisional_transfer_2d
      module procedure deallocate_type_dist_distrivec_distfunc_fexp_param
      module procedure deallocate_arr_type_dist_distrivec_distfunc_fexp_param
      module procedure deallocate_type_dist_ff
      module procedure deallocate_arr_type_dist_ff
      module procedure deallocate_type_dist_func
      module procedure deallocate_arr_type_dist_func
      module procedure deallocate_type_dist_geometry_0d
      module procedure deallocate_arr_type_dist_geometry_0d
      module procedure deallocate_type_dist_geometry_1d
      module procedure deallocate_arr_type_dist_geometry_1d
      module procedure deallocate_type_dist_geometry_2d
      module procedure deallocate_arr_type_dist_geometry_2d
      module procedure deallocate_type_dist_global_param
      module procedure deallocate_arr_type_dist_global_param
      module procedure deallocate_type_dist_global_param_collisions_z
      module procedure deallocate_arr_type_dist_global_param_collisions_z
      module procedure deallocate_type_dist_grid_info
      module procedure deallocate_arr_type_dist_grid_info
      module procedure deallocate_type_dist_profile_values_1d
      module procedure deallocate_arr_type_dist_profile_values_1d
      module procedure deallocate_type_dist_profile_values_2d
      module procedure deallocate_arr_type_dist_profile_values_2d
      module procedure deallocate_type_dist_profiles2d_collisions_z
      module procedure deallocate_arr_type_dist_profiles2d_collisions_z
      module procedure deallocate_type_dist_profiles_1d
      module procedure deallocate_arr_type_dist_profiles_1d
      module procedure deallocate_type_dist_profiles_1d_collisions_z
      module procedure deallocate_arr_type_dist_profiles_1d_collisions_z
      module procedure deallocate_type_dist_profiles_2d
      module procedure deallocate_arr_type_dist_profiles_2d
      module procedure deallocate_type_dist_sources_0d
      module procedure deallocate_arr_type_dist_sources_0d
      module procedure deallocate_type_dist_sources_1d
      module procedure deallocate_arr_type_dist_sources_1d
      module procedure deallocate_type_dist_sources_reference
      module procedure deallocate_arr_type_dist_sources_reference
      module procedure deallocate_type_dist_state_0d
      module procedure deallocate_arr_type_dist_state_0d
      module procedure deallocate_type_dist_state_1d
      module procedure deallocate_arr_type_dist_state_1d
      module procedure deallocate_type_dist_state_2d
      module procedure deallocate_arr_type_dist_state_2d
      module procedure deallocate_type_dist_thermalised_1d
      module procedure deallocate_arr_type_dist_thermalised_1d
      module procedure deallocate_type_distri_vec
      module procedure deallocate_arr_type_distri_vec
      module procedure deallocate_type_distsource_global_param
      module procedure deallocate_arr_type_distsource_global_param
      module procedure deallocate_type_distsource_line_src_prof
      module procedure deallocate_arr_type_distsource_line_src_prof
      module procedure deallocate_type_distsource_profiles_1d
      module procedure deallocate_arr_type_distsource_profiles_1d
      module procedure deallocate_type_distsource_profiles_2d
      module procedure deallocate_arr_type_distsource_profiles_2d
      module procedure deallocate_type_distsource_source
      module procedure deallocate_arr_type_distsource_source
      module procedure deallocate_type_divergence
      module procedure deallocate_arr_type_divergence
      module procedure deallocate_type_e_components
      module procedure deallocate_arr_type_e_components
      module procedure deallocate_type_ecemeasure
      module procedure deallocate_arr_type_ecemeasure
      module procedure deallocate_type_ecesetup
      module procedure deallocate_arr_type_ecesetup
      module procedure deallocate_type_edge_fluid
      module procedure deallocate_arr_type_edge_fluid
      module procedure deallocate_type_edge_fluid_scalar
      module procedure deallocate_arr_type_edge_fluid_scalar
      module procedure deallocate_type_edge_fluid_scalar_simplestruct
      module procedure deallocate_arr_type_edge_fluid_scalar_simplestruct
      module procedure deallocate_type_edge_fluid_scalar_transpcoeff
      module procedure deallocate_arr_type_edge_fluid_scalar_transpcoeff
      module procedure deallocate_type_edge_fluid_vector
      module procedure deallocate_arr_type_edge_fluid_vector
      module procedure deallocate_type_edge_fluid_vector_simplestruct
      module procedure deallocate_arr_type_edge_fluid_vector_simplestruct
      module procedure deallocate_type_edge_kinetic
      module procedure deallocate_arr_type_edge_kinetic
      module procedure deallocate_type_edge_kinetic_distribution
      module procedure deallocate_arr_type_edge_kinetic_distribution
      module procedure deallocate_type_edges
      module procedure deallocate_arr_type_edges
      module procedure deallocate_type_edgespecies
      module procedure deallocate_arr_type_edgespecies
      module procedure deallocate_type_element_desc
      module procedure deallocate_arr_type_element_desc
      module procedure deallocate_type_entry_def
      module procedure deallocate_arr_type_entry_def
      module procedure deallocate_type_enum_instance
      module procedure deallocate_arr_type_enum_instance
      module procedure deallocate_type_eqconstraint
      module procedure deallocate_arr_type_eqconstraint
      module procedure deallocate_type_eqgeometry
      module procedure deallocate_arr_type_eqgeometry
      module procedure deallocate_type_eqmes0D
      module procedure deallocate_arr_type_eqmes0D
      module procedure deallocate_type_eqmes1D
      module procedure deallocate_arr_type_eqmes1D
      module procedure deallocate_type_equatorial_plane
      module procedure deallocate_arr_type_equatorial_plane
      module procedure deallocate_type_equilibrium_profiles2d_grid
      module procedure deallocate_arr_type_equilibrium_profiles2d_grid
      module procedure deallocate_type_equilibrium_profiles_2d
      module procedure deallocate_arr_type_equilibrium_profiles_2d
      module procedure deallocate_type_exp0D
      module procedure deallocate_arr_type_exp0D
      module procedure deallocate_type_exp1D
      module procedure deallocate_arr_type_exp1D
      module procedure deallocate_type_exp2D
      module procedure deallocate_arr_type_exp2D
      module procedure deallocate_type_f_expansion
      module procedure deallocate_arr_type_f_expansion
      module procedure deallocate_type_fast_thermal_separation_filter
      module procedure deallocate_arr_type_fast_thermal_separation_filter
      module procedure deallocate_type_filter
      module procedure deallocate_arr_type_filter
      module procedure deallocate_type_flat_polygon
      module procedure deallocate_arr_type_flat_polygon
      module procedure deallocate_type_flush
      module procedure deallocate_arr_type_flush
      module procedure deallocate_type_flux_loops
      module procedure deallocate_arr_type_flux_loops
      module procedure deallocate_type_fluxel
      module procedure deallocate_arr_type_fluxel
      module procedure deallocate_type_fluximp
      module procedure deallocate_arr_type_fluximp
      module procedure deallocate_type_fluxion
      module procedure deallocate_arr_type_fluxion
      module procedure deallocate_type_focussing
      module procedure deallocate_arr_type_focussing
      module procedure deallocate_type_fullwave
      module procedure deallocate_arr_type_fullwave
      module procedure deallocate_type_fusiondiag_colli_3d
      module procedure deallocate_arr_type_fusiondiag_colli_3d
      module procedure deallocate_type_fusiondiag_colli_circ
      module procedure deallocate_arr_type_fusiondiag_colli_circ
      module procedure deallocate_type_fusiondiag_colli_poly
      module procedure deallocate_arr_type_fusiondiag_colli_poly
      module procedure deallocate_type_fusiondiag_collimator
      module procedure deallocate_arr_type_fusiondiag_collimator
      module procedure deallocate_type_fusiondiag_colliunit_circ
      module procedure deallocate_arr_type_fusiondiag_colliunit_circ
      module procedure deallocate_type_fusiondiag_colliunit_poly
      module procedure deallocate_arr_type_fusiondiag_colliunit_poly
      module procedure deallocate_type_fusiondiag_counts
      module procedure deallocate_arr_type_fusiondiag_counts
      module procedure deallocate_type_fusiondiag_ct_chords
      module procedure deallocate_arr_type_fusiondiag_ct_chords
      module procedure deallocate_type_fusiondiag_ct_energy
      module procedure deallocate_arr_type_fusiondiag_ct_energy
      module procedure deallocate_type_fusiondiag_detect_ct_energy
      module procedure deallocate_arr_type_fusiondiag_detect_ct_energy
      module procedure deallocate_type_fusiondiag_emissivity1d
      module procedure deallocate_arr_type_fusiondiag_emissivity1d
      module procedure deallocate_type_fusiondiag_emissivity2d
      module procedure deallocate_arr_type_fusiondiag_emissivity2d
      module procedure deallocate_type_fusiondiag_fus_product
      module procedure deallocate_arr_type_fusiondiag_fus_product
      module procedure deallocate_type_fusiondiag_spec1d
      module procedure deallocate_arr_type_fusiondiag_spec1d
      module procedure deallocate_type_fusiondiag_spec2d
      module procedure deallocate_arr_type_fusiondiag_spec2d
      module procedure deallocate_type_fusiondiag_voxels
      module procedure deallocate_arr_type_fusiondiag_voxels
      module procedure deallocate_type_geom
      module procedure deallocate_arr_type_geom
      module procedure deallocate_type_geom_iron
      module procedure deallocate_arr_type_geom_iron
      module procedure deallocate_type_global_param
      module procedure deallocate_arr_type_global_param
      module procedure deallocate_type_globalparam
      module procedure deallocate_arr_type_globalparam
      module procedure deallocate_type_halpha_setup
      module procedure deallocate_arr_type_halpha_setup
      module procedure deallocate_type_hcll
      module procedure deallocate_arr_type_hcll
      module procedure deallocate_type_hcll_bb
      module procedure deallocate_arr_type_hcll_bb
      module procedure deallocate_type_hcllbb_specs
      module procedure deallocate_arr_type_hcllbb_specs
      module procedure deallocate_type_holes
      module procedure deallocate_arr_type_holes
      module procedure deallocate_type_identifier
      module procedure deallocate_arr_type_identifier
      module procedure deallocate_type_impcoeff
      module procedure deallocate_arr_type_impcoeff
      module procedure deallocate_type_impurities
      module procedure deallocate_arr_type_impurities
      module procedure deallocate_type_impurity_type
      module procedure deallocate_arr_type_impurity_type
      module procedure deallocate_type_inj_spec
      module procedure deallocate_arr_type_inj_spec
      module procedure deallocate_type_ions
      module procedure deallocate_arr_type_ions
      module procedure deallocate_type_isoflux
      module procedure deallocate_arr_type_isoflux
      module procedure deallocate_type_jni
      module procedure deallocate_arr_type_jni
      module procedure deallocate_type_lang_derived
      module procedure deallocate_arr_type_lang_derived
      module procedure deallocate_type_lang_measure
      module procedure deallocate_arr_type_lang_measure
      module procedure deallocate_type_launchangles
      module procedure deallocate_arr_type_launchangles
      module procedure deallocate_type_launchs_parallel
      module procedure deallocate_arr_type_launchs_parallel
      module procedure deallocate_type_launchs_phi_theta
      module procedure deallocate_arr_type_launchs_phi_theta
      module procedure deallocate_type_launchs_rfbeam
      module procedure deallocate_arr_type_launchs_rfbeam
      module procedure deallocate_type_launchs_rfbeam_phaseellipse
      module procedure deallocate_arr_type_launchs_rfbeam_phaseellipse
      module procedure deallocate_type_launchs_rfbeam_spot
      module procedure deallocate_arr_type_launchs_rfbeam_spot
      module procedure deallocate_type_launchsignal
      module procedure deallocate_arr_type_launchsignal
      module procedure deallocate_type_limiter_unit
      module procedure deallocate_arr_type_limiter_unit
      module procedure deallocate_type_limits
      module procedure deallocate_arr_type_limits
      module procedure deallocate_type_lineintegraldiag
      module procedure deallocate_arr_type_lineintegraldiag
      module procedure deallocate_type_lithmeasure
      module procedure deallocate_arr_type_lithmeasure
      module procedure deallocate_type_lithsetup
      module procedure deallocate_arr_type_lithsetup
      module procedure deallocate_type_local
      module procedure deallocate_arr_type_local
      module procedure deallocate_type_mag_axis
      module procedure deallocate_arr_type_mag_axis
      module procedure deallocate_type_magnet_iron
      module procedure deallocate_arr_type_magnet_iron
      module procedure deallocate_type_magnetise
      module procedure deallocate_arr_type_magnetise
      module procedure deallocate_type_mat_lim
      module procedure deallocate_arr_type_mat_lim
      module procedure deallocate_type_mdinfo
      module procedure deallocate_arr_type_mdinfo
      module procedure deallocate_type_mhd_ideal_wall2d
      module procedure deallocate_arr_type_mhd_ideal_wall2d
      module procedure deallocate_type_mhd_mode
      module procedure deallocate_arr_type_mhd_mode
      module procedure deallocate_type_mhd_plasma
      module procedure deallocate_arr_type_mhd_plasma
      module procedure deallocate_type_mhd_res_wall2d
      module procedure deallocate_arr_type_mhd_res_wall2d
      module procedure deallocate_type_mhd_vacuum
      module procedure deallocate_arr_type_mhd_vacuum
      module procedure deallocate_type_mhd_vector
      module procedure deallocate_arr_type_mhd_vector
      module procedure deallocate_type_mode_lipb
      module procedure deallocate_arr_type_mode_lipb
      module procedure deallocate_type_mode_mech
      module procedure deallocate_arr_type_mode_mech
      module procedure deallocate_type_mode_neutr
      module procedure deallocate_arr_type_mode_neutr
      module procedure deallocate_type_mode_th_hyd
      module procedure deallocate_arr_type_mode_th_hyd
      module procedure deallocate_type_mode_therm
      module procedure deallocate_arr_type_mode_therm
      module procedure deallocate_type_mode_tritium
      module procedure deallocate_arr_type_mode_tritium
      module procedure deallocate_type_modules
      module procedure deallocate_arr_type_modules
      module procedure deallocate_type_msediag_emiss_chord
      module procedure deallocate_arr_type_msediag_emiss_chord
      module procedure deallocate_type_msediag_emissivity
      module procedure deallocate_arr_type_msediag_emissivity
      module procedure deallocate_type_msediag_polarization
      module procedure deallocate_arr_type_msediag_polarization
      module procedure deallocate_type_msediag_radia_chord
      module procedure deallocate_arr_type_msediag_radia_chord
      module procedure deallocate_type_msediag_radiance
      module procedure deallocate_arr_type_msediag_radiance
      module procedure deallocate_type_msediag_setup
      module procedure deallocate_arr_type_msediag_setup
      module procedure deallocate_type_msediag_setup_polarimetry
      module procedure deallocate_arr_type_msediag_setup_polarimetry
      module procedure deallocate_type_msediag_stokes
      module procedure deallocate_arr_type_msediag_stokes
      module procedure deallocate_type_nbi_nbi_unit_wall
      module procedure deallocate_arr_type_nbi_nbi_unit_wall
      module procedure deallocate_type_nbi_nbi_unit_wall_surface
      module procedure deallocate_arr_type_nbi_nbi_unit_wall_surface
      module procedure deallocate_type_nbi_unit
      module procedure deallocate_arr_type_nbi_unit
      module procedure deallocate_type_ne_transp
      module procedure deallocate_arr_type_ne_transp
      module procedure deallocate_type_neoclassic_impurity
      module procedure deallocate_arr_type_neoclassic_impurity
      module procedure deallocate_type_neut_results
      module procedure deallocate_arr_type_neut_results
      module procedure deallocate_type_neutral_complex_type
      module procedure deallocate_arr_type_neutral_complex_type
      module procedure deallocate_type_neutro_resul
      module procedure deallocate_arr_type_neutro_resul
      module procedure deallocate_type_ni_transp
      module procedure deallocate_arr_type_ni_transp
      module procedure deallocate_type_ntm_mode
      module procedure deallocate_arr_type_ntm_mode
      module procedure deallocate_type_ntm_mode_evolution
      module procedure deallocate_arr_type_ntm_mode_evolution
      module procedure deallocate_type_ntm_mode_evolution_island
      module procedure deallocate_arr_type_ntm_mode_evolution_island
      module procedure deallocate_type_ntm_mode_full_evol
      module procedure deallocate_arr_type_ntm_mode_full_evol
      module procedure deallocate_type_ntm_mode_full_evol_island
      module procedure deallocate_arr_type_ntm_mode_full_evol_island
      module procedure deallocate_type_ntm_mode_onset
      module procedure deallocate_arr_type_ntm_mode_onset
      module procedure deallocate_type_nuclei
      module procedure deallocate_arr_type_nuclei
      module procedure deallocate_type_objects
      module procedure deallocate_arr_type_objects
      module procedure deallocate_type_offdiagel
      module procedure deallocate_arr_type_offdiagel
      module procedure deallocate_type_offdiagion
      module procedure deallocate_arr_type_offdiagion
      module procedure deallocate_type_omnigen_surf
      module procedure deallocate_arr_type_omnigen_surf
      module procedure deallocate_type_orbit_global_param
      module procedure deallocate_arr_type_orbit_global_param
      module procedure deallocate_type_orbit_midplane
      module procedure deallocate_arr_type_orbit_midplane
      module procedure deallocate_type_orbit_pos
      module procedure deallocate_arr_type_orbit_pos
      module procedure deallocate_type_orbit_special_pos
      module procedure deallocate_arr_type_orbit_special_pos
      module procedure deallocate_type_orbit_turning_pts
      module procedure deallocate_arr_type_orbit_turning_pts
      module procedure deallocate_type_origin
      module procedure deallocate_arr_type_origin
      module procedure deallocate_type_param
      module procedure deallocate_arr_type_param
      module procedure deallocate_type_parameters
      module procedure deallocate_arr_type_parameters
      module procedure deallocate_type_pellet
      module procedure deallocate_arr_type_pellet
      module procedure deallocate_type_pellet_angles
      module procedure deallocate_arr_type_pellet_angles
      module procedure deallocate_type_pellet_deposition
      module procedure deallocate_arr_type_pellet_deposition
      module procedure deallocate_type_pellet_elements
      module procedure deallocate_arr_type_pellet_elements
      module procedure deallocate_type_pellet_geometry
      module procedure deallocate_arr_type_pellet_geometry
      module procedure deallocate_type_pellet_impurity
      module procedure deallocate_arr_type_pellet_impurity
      module procedure deallocate_type_pellet_pathprofiles
      module procedure deallocate_arr_type_pellet_pathprofiles
      module procedure deallocate_type_pellet_shape
      module procedure deallocate_arr_type_pellet_shape
      module procedure deallocate_type_permeability
      module procedure deallocate_arr_type_permeability
      module procedure deallocate_type_pfcircuits
      module procedure deallocate_arr_type_pfcircuits
      module procedure deallocate_type_pfcoils
      module procedure deallocate_arr_type_pfcoils
      module procedure deallocate_type_pfelement
      module procedure deallocate_arr_type_pfelement
      module procedure deallocate_type_pfgeometry
      module procedure deallocate_arr_type_pfgeometry
      module procedure deallocate_type_pfpageometry
      module procedure deallocate_arr_type_pfpageometry
      module procedure deallocate_type_pfpassive
      module procedure deallocate_arr_type_pfpassive
      module procedure deallocate_type_pfpassive_current
      module procedure deallocate_arr_type_pfpassive_current
      module procedure deallocate_type_pfsupplies
      module procedure deallocate_arr_type_pfsupplies
      module procedure deallocate_type_phaseellipse
      module procedure deallocate_arr_type_phaseellipse
      module procedure deallocate_type_planecoil
      module procedure deallocate_arr_type_planecoil
      module procedure deallocate_type_plasmaComplexType
      module procedure deallocate_arr_type_plasmaComplexType
      module procedure deallocate_type_plasmaedge
      module procedure deallocate_arr_type_plasmaedge
      module procedure deallocate_type_pol_decomp
      module procedure deallocate_arr_type_pol_decomp
      module procedure deallocate_type_polarimetry
      module procedure deallocate_arr_type_polarimetry
      module procedure deallocate_type_polarization
      module procedure deallocate_arr_type_polarization
      module procedure deallocate_type_power_conv_component
      module procedure deallocate_arr_type_power_conv_component
      module procedure deallocate_type_power_exchange
      module procedure deallocate_arr_type_power_exchange
      module procedure deallocate_type_powerflow
      module procedure deallocate_arr_type_powerflow
      module procedure deallocate_type_profiles1d
      module procedure deallocate_arr_type_profiles1d
      module procedure deallocate_type_profiles_1d
      module procedure deallocate_arr_type_profiles_1d
      module procedure deallocate_type_psi
      module procedure deallocate_arr_type_psi
      module procedure deallocate_type_putinfo
      module procedure deallocate_arr_type_putinfo
      module procedure deallocate_type_q
      module procedure deallocate_arr_type_q
      module procedure deallocate_type_reacprodType
      module procedure deallocate_arr_type_reacprodType
      module procedure deallocate_type_react
      module procedure deallocate_arr_type_react
      module procedure deallocate_type_rectanglexyz
      module procedure deallocate_arr_type_rectanglexyz
      module procedure deallocate_type_recycling_neutrals
      module procedure deallocate_arr_type_recycling_neutrals
      module procedure deallocate_type_reduced
      module procedure deallocate_arr_type_reduced
      module procedure deallocate_type_refl_receive
      module procedure deallocate_arr_type_refl_receive
      module procedure deallocate_type_reflectometry_antennas
      module procedure deallocate_arr_type_reflectometry_antennas
      module procedure deallocate_type_reflectometry_radfield
      module procedure deallocate_arr_type_reflectometry_radfield
      module procedure deallocate_type_reflectometry_radfield_gaussian
      module procedure deallocate_arr_type_reflectometry_radfield_gaussian
      module procedure deallocate_type_reflectometry_radifield_efield
      module procedure deallocate_arr_type_reflectometry_radifield_efield
      module procedure deallocate_type_reggrid
      module procedure deallocate_arr_type_reggrid
      module procedure deallocate_type_rfameasure
      module procedure deallocate_arr_type_rfameasure
      module procedure deallocate_type_rfasetup
      module procedure deallocate_arr_type_rfasetup
      module procedure deallocate_type_rfbeam
      module procedure deallocate_arr_type_rfbeam
      module procedure deallocate_type_rz0D
      module procedure deallocate_arr_type_rz0D
      module procedure deallocate_type_rz1D
      module procedure deallocate_arr_type_rz1D
      module procedure deallocate_type_rz1D_npoints
      module procedure deallocate_arr_type_rz1D_npoints
      module procedure deallocate_type_rz1Dexp
      module procedure deallocate_arr_type_rz1Dexp
      module procedure deallocate_type_rz2D
      module procedure deallocate_arr_type_rz2D
      module procedure deallocate_type_rz3D
      module procedure deallocate_arr_type_rz3D
      module procedure deallocate_type_rzphi0D
      module procedure deallocate_arr_type_rzphi0D
      module procedure deallocate_type_rzphi1D
      module procedure deallocate_arr_type_rzphi1D
      module procedure deallocate_type_rzphi1Dexp
      module procedure deallocate_arr_type_rzphi1Dexp
      module procedure deallocate_type_rzphi1Dexperimental
      module procedure deallocate_arr_type_rzphi1Dexperimental
      module procedure deallocate_type_rzphi2D
      module procedure deallocate_arr_type_rzphi2D
      module procedure deallocate_type_rzphi3D
      module procedure deallocate_arr_type_rzphi3D
      module procedure deallocate_type_rzphidrdzdphi1D
      module procedure deallocate_arr_type_rzphidrdzdphi1D
      module procedure deallocate_type_sawteeth_diags
      module procedure deallocate_arr_type_sawteeth_diags
      module procedure deallocate_type_sawteeth_profiles1d
      module procedure deallocate_arr_type_sawteeth_profiles1d
      module procedure deallocate_type_scenario_centre
      module procedure deallocate_arr_type_scenario_centre
      module procedure deallocate_type_scenario_composition
      module procedure deallocate_arr_type_scenario_composition
      module procedure deallocate_type_scenario_configuration
      module procedure deallocate_arr_type_scenario_configuration
      module procedure deallocate_type_scenario_confinement
      module procedure deallocate_arr_type_scenario_confinement
      module procedure deallocate_type_scenario_currents
      module procedure deallocate_arr_type_scenario_currents
      module procedure deallocate_type_scenario_edge
      module procedure deallocate_arr_type_scenario_edge
      module procedure deallocate_type_scenario_energy
      module procedure deallocate_arr_type_scenario_energy
      module procedure deallocate_type_scenario_global
      module procedure deallocate_arr_type_scenario_global
      module procedure deallocate_type_scenario_heat_power
      module procedure deallocate_arr_type_scenario_heat_power
      module procedure deallocate_type_scenario_int
      module procedure deallocate_arr_type_scenario_int
      module procedure deallocate_type_scenario_itb
      module procedure deallocate_arr_type_scenario_itb
      module procedure deallocate_type_scenario_lim_div_wall
      module procedure deallocate_arr_type_scenario_lim_div_wall
      module procedure deallocate_type_scenario_line_ave
      module procedure deallocate_arr_type_scenario_line_ave
      module procedure deallocate_type_scenario_neutron
      module procedure deallocate_arr_type_scenario_neutron
      module procedure deallocate_type_scenario_ninety_five
      module procedure deallocate_arr_type_scenario_ninety_five
      module procedure deallocate_type_scenario_pedestal
      module procedure deallocate_arr_type_scenario_pedestal
      module procedure deallocate_type_scenario_reactor
      module procedure deallocate_arr_type_scenario_reactor
      module procedure deallocate_type_scenario_ref
      module procedure deallocate_arr_type_scenario_ref
      module procedure deallocate_type_scenario_references
      module procedure deallocate_arr_type_scenario_references
      module procedure deallocate_type_scenario_sol
      module procedure deallocate_arr_type_scenario_sol
      module procedure deallocate_type_scenario_vol_ave
      module procedure deallocate_arr_type_scenario_vol_ave
      module procedure deallocate_type_setup_bprobe
      module procedure deallocate_arr_type_setup_bprobe
      module procedure deallocate_type_setup_floops
      module procedure deallocate_arr_type_setup_floops
      module procedure deallocate_type_setup_line
      module procedure deallocate_arr_type_setup_line
      module procedure deallocate_type_setup_line_exp
      module procedure deallocate_arr_type_setup_line_exp
      module procedure deallocate_type_shield
      module procedure deallocate_arr_type_shield
      module procedure deallocate_type_shield_specs
      module procedure deallocate_arr_type_shield_specs
      module procedure deallocate_type_simp_apert
      module procedure deallocate_arr_type_simp_apert
      module procedure deallocate_type_solcurdiag_sol_current
      module procedure deallocate_arr_type_solcurdiag_sol_current
      module procedure deallocate_type_solcurdiag_sol_current_setup
      module procedure deallocate_arr_type_solcurdiag_sol_current_setup
      module procedure deallocate_type_source_imp
      module procedure deallocate_arr_type_source_imp
      module procedure deallocate_type_source_ion
      module procedure deallocate_arr_type_source_ion
      module procedure deallocate_type_source_rate
      module procedure deallocate_arr_type_source_rate
      module procedure deallocate_type_source_vec
      module procedure deallocate_arr_type_source_vec
      module procedure deallocate_type_sourceel
      module procedure deallocate_arr_type_sourceel
      module procedure deallocate_type_sourceimp
      module procedure deallocate_arr_type_sourceimp
      module procedure deallocate_type_sourceion
      module procedure deallocate_arr_type_sourceion
      module procedure deallocate_type_species_desc
      module procedure deallocate_arr_type_species_desc
      module procedure deallocate_type_species_reference
      module procedure deallocate_arr_type_species_reference
      module procedure deallocate_type_spectral
      module procedure deallocate_arr_type_spectral
      module procedure deallocate_type_spectrum
      module procedure deallocate_arr_type_spectrum
      module procedure deallocate_type_spot
      module procedure deallocate_arr_type_spot
      module procedure deallocate_type_sputtering_neutrals
      module procedure deallocate_arr_type_sputtering_neutrals
      module procedure deallocate_type_straps
      module procedure deallocate_arr_type_straps
      module procedure deallocate_type_structure_cs
      module procedure deallocate_arr_type_structure_cs
      module procedure deallocate_type_t_series_cplx
      module procedure deallocate_arr_type_t_series_cplx
      module procedure deallocate_type_t_series_real
      module procedure deallocate_arr_type_t_series_real
      module procedure deallocate_type_table
      module procedure deallocate_arr_type_table
      module procedure deallocate_type_tables
      module procedure deallocate_arr_type_tables
      module procedure deallocate_type_tables_coord
      module procedure deallocate_arr_type_tables_coord
      module procedure deallocate_type_temporary_nt
      module procedure deallocate_arr_type_temporary_nt
      module procedure deallocate_type_temporary_nt_0dc
      module procedure deallocate_arr_type_temporary_nt_0dc
      module procedure deallocate_type_temporary_nt_0di
      module procedure deallocate_arr_type_temporary_nt_0di
      module procedure deallocate_type_temporary_nt_0dr
      module procedure deallocate_arr_type_temporary_nt_0dr
      module procedure deallocate_type_temporary_nt_0ds
      module procedure deallocate_arr_type_temporary_nt_0ds
      module procedure deallocate_type_temporary_nt_1dc
      module procedure deallocate_arr_type_temporary_nt_1dc
      module procedure deallocate_type_temporary_nt_1di
      module procedure deallocate_arr_type_temporary_nt_1di
      module procedure deallocate_type_temporary_nt_1dr
      module procedure deallocate_arr_type_temporary_nt_1dr
      module procedure deallocate_type_temporary_nt_1ds
      module procedure deallocate_arr_type_temporary_nt_1ds
      module procedure deallocate_type_temporary_nt_2dc
      module procedure deallocate_arr_type_temporary_nt_2dc
      module procedure deallocate_type_temporary_nt_2di
      module procedure deallocate_arr_type_temporary_nt_2di
      module procedure deallocate_type_temporary_nt_2dr
      module procedure deallocate_arr_type_temporary_nt_2dr
      module procedure deallocate_type_temporary_nt_3dc
      module procedure deallocate_arr_type_temporary_nt_3dc
      module procedure deallocate_type_temporary_nt_3di
      module procedure deallocate_arr_type_temporary_nt_3di
      module procedure deallocate_type_temporary_nt_3dr
      module procedure deallocate_arr_type_temporary_nt_3dr
      module procedure deallocate_type_temporary_nt_4dr
      module procedure deallocate_arr_type_temporary_nt_4dr
      module procedure deallocate_type_temporary_t
      module procedure deallocate_arr_type_temporary_t
      module procedure deallocate_type_temporary_t_0dc
      module procedure deallocate_arr_type_temporary_t_0dc
      module procedure deallocate_type_temporary_t_0di
      module procedure deallocate_arr_type_temporary_t_0di
      module procedure deallocate_type_temporary_t_0dr
      module procedure deallocate_arr_type_temporary_t_0dr
      module procedure deallocate_type_temporary_t_0ds
      module procedure deallocate_arr_type_temporary_t_0ds
      module procedure deallocate_type_temporary_t_1dc
      module procedure deallocate_arr_type_temporary_t_1dc
      module procedure deallocate_type_temporary_t_1di
      module procedure deallocate_arr_type_temporary_t_1di
      module procedure deallocate_type_temporary_t_1dr
      module procedure deallocate_arr_type_temporary_t_1dr
      module procedure deallocate_type_temporary_t_2dc
      module procedure deallocate_arr_type_temporary_t_2dc
      module procedure deallocate_type_temporary_t_2di
      module procedure deallocate_arr_type_temporary_t_2di
      module procedure deallocate_type_temporary_t_2dr
      module procedure deallocate_arr_type_temporary_t_2dr
      module procedure deallocate_type_temporary_t_3dc
      module procedure deallocate_arr_type_temporary_t_3dc
      module procedure deallocate_type_temporary_t_3di
      module procedure deallocate_arr_type_temporary_t_3di
      module procedure deallocate_type_temporary_t_3dr
      module procedure deallocate_arr_type_temporary_t_3dr
      module procedure deallocate_type_temporary_t_4dr
      module procedure deallocate_arr_type_temporary_t_4dr
      module procedure deallocate_type_tf_desc_tfcoils
      module procedure deallocate_arr_type_tf_desc_tfcoils
      module procedure deallocate_type_tf_desc_tfcoils_board
      module procedure deallocate_arr_type_tf_desc_tfcoils_board
      module procedure deallocate_type_tf_structure
      module procedure deallocate_arr_type_tf_structure
      module procedure deallocate_type_theta_info
      module procedure deallocate_arr_type_theta_info
      module procedure deallocate_type_topo_regions
      module procedure deallocate_arr_type_topo_regions
      module procedure deallocate_type_toroid_field
      module procedure deallocate_arr_type_toroid_field
      module procedure deallocate_type_trace
      module procedure deallocate_arr_type_trace
      module procedure deallocate_type_transcoefel
      module procedure deallocate_arr_type_transcoefel
      module procedure deallocate_type_transcoefimp
      module procedure deallocate_arr_type_transcoefimp
      module procedure deallocate_type_transcoefion
      module procedure deallocate_arr_type_transcoefion
      module procedure deallocate_type_transcoefvtor
      module procedure deallocate_arr_type_transcoefvtor
      module procedure deallocate_type_trap_type
      module procedure deallocate_arr_type_trap_type
      module procedure deallocate_type_trianglexyz
      module procedure deallocate_arr_type_trianglexyz
      module procedure deallocate_type_tsmeasure
      module procedure deallocate_arr_type_tsmeasure
      module procedure deallocate_type_tssetup
      module procedure deallocate_arr_type_tssetup
      module procedure deallocate_type_turbcomposition
      module procedure deallocate_arr_type_turbcomposition
      module procedure deallocate_type_turbcoordsys
      module procedure deallocate_arr_type_turbcoordsys
      module procedure deallocate_type_turbenv1d
      module procedure deallocate_arr_type_turbenv1d
      module procedure deallocate_type_turbgrid
      module procedure deallocate_arr_type_turbgrid
      module procedure deallocate_type_turbspec1d
      module procedure deallocate_arr_type_turbspec1d
      module procedure deallocate_type_turbvar0d
      module procedure deallocate_arr_type_turbvar0d
      module procedure deallocate_type_turbvar1d
      module procedure deallocate_arr_type_turbvar1d
      module procedure deallocate_type_turbvar2d
      module procedure deallocate_arr_type_turbvar2d
      module procedure deallocate_type_turbvar3d
      module procedure deallocate_arr_type_turbvar3d
      module procedure deallocate_type_turbvar4d
      module procedure deallocate_arr_type_turbvar4d
      module procedure deallocate_type_turbvar5d
      module procedure deallocate_arr_type_turbvar5d
      module procedure deallocate_type_version_ind
      module procedure deallocate_arr_type_version_ind
      module procedure deallocate_type_wall2d
      module procedure deallocate_arr_type_wall2d
      module procedure deallocate_type_wall2d_mhd
      module procedure deallocate_arr_type_wall2d_mhd
      module procedure deallocate_type_wall3d
      module procedure deallocate_arr_type_wall3d
      module procedure deallocate_type_wall_blocks
      module procedure deallocate_arr_type_wall_blocks
      module procedure deallocate_type_wall_blocks_unit
      module procedure deallocate_arr_type_wall_blocks_unit
      module procedure deallocate_type_wall_limiter
      module procedure deallocate_arr_type_wall_limiter
      module procedure deallocate_type_wall_types
      module procedure deallocate_arr_type_wall_types
      module procedure deallocate_type_wall_types_layers
      module procedure deallocate_arr_type_wall_types_layers
      module procedure deallocate_type_wall_unitsComplexType
      module procedure deallocate_arr_type_wall_unitsComplexType
      module procedure deallocate_type_wall_unitsComplexType_layers
      module procedure deallocate_arr_type_wall_unitsComplexType_layers
      module procedure deallocate_type_wall_vessel
      module procedure deallocate_arr_type_wall_vessel
      module procedure deallocate_type_wall_vessel_annular
      module procedure deallocate_arr_type_wall_vessel_annular
      module procedure deallocate_type_wall_vessel_unit
      module procedure deallocate_arr_type_wall_vessel_unit
      module procedure deallocate_type_wall_wall0d
      module procedure deallocate_arr_type_wall_wall0d
      module procedure deallocate_type_wall_wall0d_plasma
      module procedure deallocate_arr_type_wall_wall0d_plasma
      module procedure deallocate_type_wall_wall2d_vessel_radial_build
      module procedure deallocate_arr_type_wall_wall2d_vessel_radial_build
      module procedure deallocate_type_waveguides
      module procedure deallocate_arr_type_waveguides
      module procedure deallocate_type_waves_global_param
      module procedure deallocate_arr_type_waves_global_param
      module procedure deallocate_type_waves_grid_1d
      module procedure deallocate_arr_type_waves_grid_1d
      module procedure deallocate_type_waves_grid_2d
      module procedure deallocate_arr_type_waves_grid_2d
      module procedure deallocate_type_waves_profiles_1d
      module procedure deallocate_arr_type_waves_profiles_1d
      module procedure deallocate_type_waves_profiles_2d
      module procedure deallocate_arr_type_waves_profiles_2d
      module procedure deallocate_type_waves_rtposition
      module procedure deallocate_arr_type_waves_rtposition
      module procedure deallocate_type_waves_rtwavevector
      module procedure deallocate_arr_type_waves_rtwavevector
      module procedure deallocate_type_weighted_markers
      module procedure deallocate_arr_type_weighted_markers
      module procedure deallocate_type_whatref
      module procedure deallocate_arr_type_whatref
      module procedure deallocate_type_width
      module procedure deallocate_arr_type_width
      module procedure deallocate_type_xpts
      module procedure deallocate_arr_type_xpts
      module procedure deallocate_type_xyz0D
      module procedure deallocate_arr_type_xyz0D
   end interface

   integer, parameter, private :: iu6 = 6
   integer, private :: verbose = 0

 contains


   subroutine set_deallocate_verbosity(verbosity)

     implicit none

     integer, intent(in) :: verbosity

     if (verbosity < 0) then
       verbose = 0
     else
       verbose = verbosity
     end if

   end subroutine set_deallocate_verbosity

   subroutine deallocate_type_array3dcplx_type(structure)

     implicit none

     complex(euitm_r8), pointer :: structure(:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array3dcplx_type

   subroutine deallocate_type_array3dflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array3dflt_type

   subroutine deallocate_type_array3dint_type(structure)

     implicit none

     integer, pointer :: structure(:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array3dint_type

   subroutine deallocate_type_array4dflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:,:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array4dflt_type

   subroutine deallocate_type_array5dflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:,:,:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array5dflt_type

   subroutine deallocate_type_array6dflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:,:,:,:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array6dflt_type

   subroutine deallocate_type_array7dflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:,:,:,:,:,:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_array7dflt_type

   subroutine deallocate_type_matcplx_type(structure)

     implicit none

     complex(euitm_r8), pointer :: structure(:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_matcplx_type

   subroutine deallocate_type_matflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_matflt_type

   subroutine deallocate_type_matint_type(structure)

     implicit none

     integer, pointer :: structure(:,:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_matint_type

   subroutine deallocate_type_veccplx_type(structure)

     implicit none

     complex(euitm_r8), pointer :: structure(:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_veccplx_type

   subroutine deallocate_type_vecflt_type(structure)

     implicit none

     real(euitm_r8), pointer :: structure(:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_vecflt_type

   subroutine deallocate_type_vecint_type(structure)

     implicit none

     integer, pointer :: structure(:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_vecint_type

   subroutine deallocate_type_vecstring_type(structure)

     implicit none

     character(len = 132), pointer :: structure(:)

     if (associated(structure)) then
       deallocate(structure)
     end if

   end subroutine deallocate_type_vecstring_type

   subroutine deallocate_type_amns(structure)

     implicit none

     type (type_amns) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%version)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_arr_type_amns_processType(structure%process)
     call deallocate_arr_type_tables(structure%tables)
     call deallocate_arr_type_tables_coord(structure%tables_coord)
     call deallocate_arr_type_version_ind(structure%version_ind)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_amns

   subroutine deallocate_arr_type_amns(structure)

     implicit none

     type (type_amns), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_amns(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_amns'
     end if

   end subroutine deallocate_arr_type_amns

   subroutine deallocate_type_antennas(structure)

     implicit none

     type (type_antennas) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_antenna_ec(structure%antenna_ec)
     call deallocate_arr_type_antenna_ic(structure%antenna_ic)
     call deallocate_arr_type_antenna_lh(structure%antenna_lh)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_antennas

   subroutine deallocate_arr_type_antennas(structure)

     implicit none

     type (type_antennas), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_antennas(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_antennas'
     end if

   end subroutine deallocate_arr_type_antennas

   subroutine deallocate_type_bb_shield(structure)

     implicit none

     type (type_bb_shield) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%type)
     call deallocate_type_limits(structure%limits)
     call deallocate_type_geom(structure%geom)
     call deallocate_type_neut_results(structure%neut_results)
     call deallocate_type_shield(structure%shield)
     call deallocate_type_bb(structure%bb)
     call deallocate_type_hcll(structure%hcll)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_bb_shield

   subroutine deallocate_arr_type_bb_shield(structure)

     implicit none

     type (type_bb_shield), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_bb_shield(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_bb_shield'
     end if

   end subroutine deallocate_arr_type_bb_shield

   subroutine deallocate_type_compositionc(structure)

     implicit none

     type (type_compositionc) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_compositionc

   subroutine deallocate_arr_type_compositionc(structure)

     implicit none

     type (type_compositionc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_compositionc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_compositionc'
     end if

   end subroutine deallocate_arr_type_compositionc

   subroutine deallocate_type_coredelta(structure)

     implicit none

     type (type_coredelta) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_arr_type_coredelta_values(structure%values)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coredelta

   subroutine deallocate_arr_type_coredelta(structure)

     implicit none

     type (type_coredelta), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coredelta(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coredelta'
     end if

   end subroutine deallocate_arr_type_coredelta

   subroutine deallocate_type_corefast(structure)

     implicit none

     type (type_corefast) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_b0r0(structure%toroid_field)
     call deallocate_arr_type_corefast_values(structure%values)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_corefast

   subroutine deallocate_arr_type_corefast(structure)

     implicit none

     type (type_corefast), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefast(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefast'
     end if

   end subroutine deallocate_arr_type_corefast

   subroutine deallocate_type_coreimpur(structure)

     implicit none

     type (type_coreimpur) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecint_type(structure%flag)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_vecstring_type(structure%atomic_data)
     call deallocate_arr_type_impurity_type(structure%impurity)
     call deallocate_type_coreimpurediag_type(structure%diagnostic)
     call deallocate_type_coreimpurediag_sum(structure%diagnosticsum)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coreimpur

   subroutine deallocate_arr_type_coreimpur(structure)

     implicit none

     type (type_coreimpur), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpur(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpur'
     end if

   end subroutine deallocate_arr_type_coreimpur

   subroutine deallocate_type_coreneutrals(structure)

     implicit none

     type (type_coreneutrals) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_composition_neutrals(structure%neutcompo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_arr_type_neutral_complex_type(structure%profiles)
     call deallocate_arr_type_coefficients_neutrals(structure%ioncoeff)
     call deallocate_arr_type_impcoeff(structure%impcoeff)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coreneutrals

   subroutine deallocate_arr_type_coreneutrals(structure)

     implicit none

     type (type_coreneutrals), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreneutrals(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreneutrals'
     end if

   end subroutine deallocate_arr_type_coreneutrals

   subroutine deallocate_type_coreprof(structure)

     implicit none

     type (type_coreprof) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%drho_dt)
     call deallocate_type_toroid_field(structure%toroid_field)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_psi(structure%psi)
     call deallocate_type_corefield(structure%te)
     call deallocate_type_corefieldion(structure%ti)
     call deallocate_type_corefield(structure%ne)
     call deallocate_type_corefieldion(structure%ni)
     call deallocate_type_corefieldion(structure%vtor)
     call deallocate_type_profiles1d(structure%profiles1d)
     call deallocate_type_globalparam(structure%globalparam)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coreprof

   subroutine deallocate_arr_type_coreprof(structure)

     implicit none

     type (type_coreprof), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreprof(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreprof'
     end if

   end subroutine deallocate_arr_type_coreprof

   subroutine deallocate_type_coresource(structure)

     implicit none

     type (type_coresource) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_b0r0(structure%toroid_field)
     call deallocate_arr_type_coresource_values(structure%values)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coresource

   subroutine deallocate_arr_type_coresource(structure)

     implicit none

     type (type_coresource), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coresource(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coresource'
     end if

   end subroutine deallocate_arr_type_coresource

   subroutine deallocate_type_coretransp(structure)

     implicit none

     type (type_coretransp) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_arr_type_coretransp_values(structure%values)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coretransp

   subroutine deallocate_arr_type_coretransp(structure)

     implicit none

     type (type_coretransp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coretransp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coretransp'
     end if

   end subroutine deallocate_arr_type_coretransp

   subroutine deallocate_type_cxdiag(structure)

     implicit none

     type (type_cxdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_cxsetup(structure%setup)
     call deallocate_type_cxmeasure(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_cxdiag

   subroutine deallocate_arr_type_cxdiag(structure)

     implicit none

     type (type_cxdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_cxdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_cxdiag'
     end if

   end subroutine deallocate_arr_type_cxdiag

   subroutine deallocate_type_distribution(structure)

     implicit none

     type (type_distribution) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_arr_type_distri_vec(structure%distri_vec)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_distribution

   subroutine deallocate_arr_type_distribution(structure)

     implicit none

     type (type_distribution), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distribution(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distribution'
     end if

   end subroutine deallocate_arr_type_distribution

   subroutine deallocate_type_distsource(structure)

     implicit none

     type (type_distsource) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_arr_type_distsource_source(structure%source)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_distsource

   subroutine deallocate_arr_type_distsource(structure)

     implicit none

     type (type_distsource), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distsource(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distsource'
     end if

   end subroutine deallocate_arr_type_distsource

   subroutine deallocate_type_ecediag(structure)

     implicit none

     type (type_ecediag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_ecesetup(structure%setup)
     call deallocate_type_ecemeasure(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_ecediag

   subroutine deallocate_arr_type_ecediag(structure)

     implicit none

     type (type_ecediag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ecediag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ecediag'
     end if

   end subroutine deallocate_arr_type_ecediag

   subroutine deallocate_type_edge(structure)

     implicit none

     type (type_edge) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_complexgrid(structure%grid)
     call deallocate_arr_type_species_desc(structure%species)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_edge_fluid(structure%fluid)
     call deallocate_type_edge_kinetic(structure%kinetic)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_edge

   subroutine deallocate_arr_type_edge(structure)

     implicit none

     type (type_edge), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge'
     end if

   end subroutine deallocate_arr_type_edge

   subroutine deallocate_type_efcc(structure)

     implicit none

     type (type_efcc) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_coil(structure%coil)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_efcc

   subroutine deallocate_arr_type_efcc(structure)

     implicit none

     type (type_efcc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_efcc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_efcc'
     end if

   end subroutine deallocate_arr_type_efcc

   subroutine deallocate_type_equilibrium(structure)

     implicit none

     type (type_equilibrium) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_eqconstraint(structure%eqconstraint)
     call deallocate_type_eqgeometry(structure%eqgeometry)
     call deallocate_type_flush(structure%flush)
     call deallocate_type_global_param(structure%global_param)
     call deallocate_type_profiles_1d(structure%profiles_1d)
     call deallocate_arr_type_equilibrium_profiles_2d(structure%profiles_2d)
     call deallocate_type_coord_sys(structure%coord_sys)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_equilibrium

   subroutine deallocate_arr_type_equilibrium(structure)

     implicit none

     type (type_equilibrium), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_equilibrium(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_equilibrium'
     end if

   end subroutine deallocate_arr_type_equilibrium

   subroutine deallocate_type_fusiondiag(structure)

     implicit none

     type (type_fusiondiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_fusiondiag_fus_product(structure%fus_product)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_fusiondiag

   subroutine deallocate_arr_type_fusiondiag(structure)

     implicit none

     type (type_fusiondiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag'
     end if

   end subroutine deallocate_arr_type_fusiondiag

   subroutine deallocate_type_halphadiag(structure)

     implicit none

     type (type_halphadiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_halpha_setup(structure%setup)
     call deallocate_type_exp1D(structure%intensity)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_halphadiag

   subroutine deallocate_arr_type_halphadiag(structure)

     implicit none

     type (type_halphadiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_halphadiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_halphadiag'
     end if

   end subroutine deallocate_arr_type_halphadiag

   subroutine deallocate_type_heat_sources(structure)

     implicit none

     type (type_heat_sources) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_calorimetry_heat_source(structure%sources)
     call deallocate_arr_type_calorimetry_heat_source(structure%sinks)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_heat_sources

   subroutine deallocate_arr_type_heat_sources(structure)

     implicit none

     type (type_heat_sources), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_heat_sources(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_heat_sources'
     end if

   end subroutine deallocate_arr_type_heat_sources

   subroutine deallocate_type_interfdiag(structure)

     implicit none

     type (type_interfdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%expression)
     call deallocate_type_setup_line(structure%setup_line)
     call deallocate_type_exp1D(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_interfdiag

   subroutine deallocate_arr_type_interfdiag(structure)

     implicit none

     type (type_interfdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_interfdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_interfdiag'
     end if

   end subroutine deallocate_arr_type_interfdiag

   subroutine deallocate_type_ironmodel(structure)

     implicit none

     type (type_ironmodel) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_desc_iron(structure%desc_iron)
     call deallocate_type_magnetise(structure%magnetise)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_ironmodel

   subroutine deallocate_arr_type_ironmodel(structure)

     implicit none

     type (type_ironmodel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ironmodel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ironmodel'
     end if

   end subroutine deallocate_arr_type_ironmodel

   subroutine deallocate_type_langmuirdiag(structure)

     implicit none

     type (type_langmuirdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_lang_measure(structure%potential)
     call deallocate_type_lang_measure(structure%bias)
     call deallocate_type_lang_measure(structure%jsat)
     call deallocate_type_lang_derived(structure%ne)
     call deallocate_type_lang_derived(structure%te)
     call deallocate_type_lang_derived(structure%machpar)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_langmuirdiag

   subroutine deallocate_arr_type_langmuirdiag(structure)

     implicit none

     type (type_langmuirdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_langmuirdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_langmuirdiag'
     end if

   end subroutine deallocate_arr_type_langmuirdiag

   subroutine deallocate_type_launchs(structure)

     implicit none

     type (type_launchs) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%type)
     call deallocate_type_vecflt_type(structure%frequency)
     call deallocate_type_vecint_type(structure%mode)
     call deallocate_type_rzphi1D(structure%position)
     call deallocate_type_spectrum(structure%spectrum)
     call deallocate_type_launchs_rfbeam(structure%beam)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_launchs

   subroutine deallocate_arr_type_launchs(structure)

     implicit none

     type (type_launchs), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchs(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchs'
     end if

   end subroutine deallocate_arr_type_launchs

   subroutine deallocate_type_lithiumdiag(structure)

     implicit none

     type (type_lithiumdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_lithsetup(structure%setup)
     call deallocate_type_lithmeasure(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_lithiumdiag

   subroutine deallocate_arr_type_lithiumdiag(structure)

     implicit none

     type (type_lithiumdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_lithiumdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_lithiumdiag'
     end if

   end subroutine deallocate_arr_type_lithiumdiag

   subroutine deallocate_type_magdiag(structure)

     implicit none

     type (type_magdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_exp0D(structure%ip)
     call deallocate_type_exp0D(structure%diamagflux)
     call deallocate_type_exp0D(structure%diamagener)
     call deallocate_type_flux_loops(structure%flux_loops)
     call deallocate_type_bpol_probes(structure%bpol_probes)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_magdiag

   subroutine deallocate_arr_type_magdiag(structure)

     implicit none

     type (type_magdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_magdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_magdiag'
     end if

   end subroutine deallocate_arr_type_magdiag

   subroutine deallocate_type_mhd(structure)

     implicit none

     type (type_mhd) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_b0r0(structure%toroid_field)
     call deallocate_arr_type_mhd_mode(structure%n)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_mhd

   subroutine deallocate_arr_type_mhd(structure)

     implicit none

     type (type_mhd), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd'
     end if

   end subroutine deallocate_arr_type_mhd

   subroutine deallocate_type_msediag(structure)

     implicit none

     type (type_msediag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_polarimetry(structure%polarimetry)
     call deallocate_type_spectral(structure%spectral)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_msediag

   subroutine deallocate_arr_type_msediag(structure)

     implicit none

     type (type_msediag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag'
     end if

   end subroutine deallocate_arr_type_msediag

   subroutine deallocate_type_nbi(structure)

     implicit none

     type (type_nbi) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_nbi_unit(structure%nbi_unit)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_nbi

   subroutine deallocate_arr_type_nbi(structure)

     implicit none

     type (type_nbi), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_nbi(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_nbi'
     end if

   end subroutine deallocate_arr_type_nbi

   subroutine deallocate_type_neoclassic(structure)

     implicit none

     type (type_neoclassic) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_desc_impur(structure%desc_impur)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_transcoefion(structure%ni_neo)
     call deallocate_type_transcoefel(structure%ne_neo)
     call deallocate_arr_type_transcoefimp(structure%nz_neo)
     call deallocate_type_transcoefion(structure%ti_neo)
     call deallocate_type_transcoefel(structure%te_neo)
     call deallocate_arr_type_transcoefimp(structure%tz_neo)
     call deallocate_type_transcoefel(structure%mtor_neo)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_vecflt_type(structure%jboot)
     call deallocate_type_vecflt_type(structure%er)
     call deallocate_type_matflt_type(structure%vpol)
     call deallocate_type_matflt_type(structure%vtor)
     call deallocate_type_matflt_type(structure%mach)
     call deallocate_type_vecflt_type(structure%utheta_e)
     call deallocate_type_matflt_type(structure%utheta_i)
     call deallocate_type_matflt_type(structure%viscosity_par)
     call deallocate_arr_type_neoclassic_impurity(structure%impurity)
     call deallocate_type_array3dflt_type(structure%fext)
     call deallocate_type_vecflt_type(structure%jext)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_neoclassic

   subroutine deallocate_arr_type_neoclassic(structure)

     implicit none

     type (type_neoclassic), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_neoclassic(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_neoclassic'
     end if

   end subroutine deallocate_arr_type_neoclassic

   subroutine deallocate_type_ntm(structure)

     implicit none

     type (type_ntm) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_ntm_mode(structure%mode)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_ntm

   subroutine deallocate_arr_type_ntm(structure)

     implicit none

     type (type_ntm), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm'
     end if

   end subroutine deallocate_arr_type_ntm

   subroutine deallocate_type_orbit(structure)

     implicit none

     type (type_orbit) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_com(structure%com)
     call deallocate_type_trace(structure%trace)
     call deallocate_type_orbit_global_param(structure%global_param)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_orbit

   subroutine deallocate_arr_type_orbit(structure)

     implicit none

     type (type_orbit), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_orbit(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_orbit'
     end if

   end subroutine deallocate_arr_type_orbit

   subroutine deallocate_type_pellets(structure)

     implicit none

     type (type_pellets) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_arr_type_pellet(structure%pellet)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_pellets

   subroutine deallocate_arr_type_pellets(structure)

     implicit none

     type (type_pellets), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellets(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellets'
     end if

   end subroutine deallocate_arr_type_pellets

   subroutine deallocate_type_pfsystems(structure)

     implicit none

     type (type_pfsystems) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_pfcoils(structure%pfcoils)
     call deallocate_type_pfpassive(structure%pfpassive)
     call deallocate_type_pfcircuits(structure%pfcircuits)
     call deallocate_type_pfsupplies(structure%pfsupplies)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_pfsystems

   subroutine deallocate_arr_type_pfsystems(structure)

     implicit none

     type (type_pfsystems), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfsystems(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfsystems'
     end if

   end subroutine deallocate_arr_type_pfsystems

   subroutine deallocate_type_polardiag(structure)

     implicit none

     type (type_polardiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%expression)
     call deallocate_type_setup_line(structure%setup_line)
     call deallocate_type_exp1D(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_polardiag

   subroutine deallocate_arr_type_polardiag(structure)

     implicit none

     type (type_polardiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_polardiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_polardiag'
     end if

   end subroutine deallocate_arr_type_polardiag

   subroutine deallocate_type_power_conv(structure)

     implicit none

     type (type_power_conv) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%cycle_type)
     call deallocate_arr_type_circuits(structure%circuits)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_power_conv

   subroutine deallocate_arr_type_power_conv(structure)

     implicit none

     type (type_power_conv), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_power_conv(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_power_conv'
     end if

   end subroutine deallocate_arr_type_power_conv

   subroutine deallocate_type_reflectomet(structure)

     implicit none

     type (type_reflectomet) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_refl_receive(structure%refl_receive)
     call deallocate_arr_type_reflectometry_antennas(structure%antennas)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_reflectomet

   subroutine deallocate_arr_type_reflectomet(structure)

     implicit none

     type (type_reflectomet), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reflectomet(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reflectomet'
     end if

   end subroutine deallocate_arr_type_reflectomet

   subroutine deallocate_type_rfadiag(structure)

     implicit none

     type (type_rfadiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_rfasetup(structure%setup)
     call deallocate_type_rfameasure(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_rfadiag

   subroutine deallocate_arr_type_rfadiag(structure)

     implicit none

     type (type_rfadiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rfadiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rfadiag'
     end if

   end subroutine deallocate_arr_type_rfadiag

   subroutine deallocate_type_sawteeth(structure)

     implicit none

     type (type_sawteeth) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_sawteeth_profiles1d(structure%profiles1d)
     call deallocate_type_sawteeth_diags(structure%diags)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_sawteeth

   subroutine deallocate_arr_type_sawteeth(structure)

     implicit none

     type (type_sawteeth), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sawteeth(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sawteeth'
     end if

   end subroutine deallocate_arr_type_sawteeth

   subroutine deallocate_type_scenario(structure)

     implicit none

     type (type_scenario) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_scenario_centre(structure%centre)
     call deallocate_type_scenario_composition(structure%composition)
     call deallocate_type_scenario_configuration(structure%configs)
     call deallocate_type_scenario_confinement(structure%confinement)
     call deallocate_type_scenario_currents(structure%currents)
     call deallocate_type_scenario_edge(structure%edge)
     call deallocate_type_scenario_energy(structure%energy)
     call deallocate_type_eqgeometry(structure%eqgeometry)
     call deallocate_type_scenario_global(structure%global_param)
     call deallocate_type_scenario_heat_power(structure%heat_power)
     call deallocate_type_scenario_itb(structure%itb)
     call deallocate_type_scenario_lim_div_wall(structure%lim_div_wall)
     call deallocate_type_scenario_line_ave(structure%line_ave)
     call deallocate_type_scenario_neutron(structure%neutron)
     call deallocate_type_scenario_ninety_five(structure%ninety_five)
     call deallocate_type_scenario_pedestal(structure%pedestal)
     call deallocate_type_scenario_references(structure%references)
     call deallocate_type_scenario_reactor(structure%reactor)
     call deallocate_type_scenario_sol(structure%sol)
     call deallocate_type_scenario_vol_ave(structure%vol_ave)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_scenario

   subroutine deallocate_arr_type_scenario(structure)

     implicit none

     type (type_scenario), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario'
     end if

   end subroutine deallocate_arr_type_scenario

   subroutine deallocate_type_solcurdiag(structure)

     implicit none

     type (type_solcurdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_solcurdiag_sol_current(structure%sol_current)
     call deallocate_arr_type_clusters(structure%clusters)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_solcurdiag

   subroutine deallocate_arr_type_solcurdiag(structure)

     implicit none

     type (type_solcurdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_solcurdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_solcurdiag'
     end if

   end subroutine deallocate_arr_type_solcurdiag

   subroutine deallocate_type_temporary(structure)

     implicit none

     type (type_temporary) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_temporary_nt(structure%non_timed)
     call deallocate_type_temporary_t(structure%timed)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_temporary

   subroutine deallocate_arr_type_temporary(structure)

     implicit none

     type (type_temporary), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary'
     end if

   end subroutine deallocate_arr_type_temporary

   subroutine deallocate_type_topinfo(structure)

     implicit none

     type (type_topinfo) :: structure

     call deallocate_type_vecstring_type(structure%dataprovider)
     call deallocate_type_vecstring_type(structure%description)
     call deallocate_type_vecstring_type(structure%firstputdate)
     call deallocate_type_vecstring_type(structure%lastupdate)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecstring_type(structure%comment)
     call deallocate_type_vecstring_type(structure%dataversion)
     call deallocate_type_vecstring_type(structure%workflow)
     call deallocate_type_entry_def(structure%entry)
     call deallocate_type_entry_def(structure%parent_entry)
     call deallocate_type_mdinfo(structure%mdinfo)

   end subroutine deallocate_type_topinfo

   subroutine deallocate_arr_type_topinfo(structure)

     implicit none

     type (type_topinfo), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_topinfo(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_topinfo'
     end if

   end subroutine deallocate_arr_type_topinfo

   subroutine deallocate_type_toroidfield(structure)

     implicit none

     type (type_toroidfield) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_tf_desc_tfcoils(structure%desc_tfcoils)
     call deallocate_type_exp0D(structure%current)
     call deallocate_type_exp0D(structure%bvac_r)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_toroidfield

   subroutine deallocate_arr_type_toroidfield(structure)

     implicit none

     type (type_toroidfield), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_toroidfield(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_toroidfield'
     end if

   end subroutine deallocate_arr_type_toroidfield

   subroutine deallocate_type_tsdiag(structure)

     implicit none

     type (type_tsdiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_tssetup(structure%setup)
     call deallocate_type_tsmeasure(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_tsdiag

   subroutine deallocate_arr_type_tsdiag(structure)

     implicit none

     type (type_tsdiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tsdiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tsdiag'
     end if

   end subroutine deallocate_arr_type_tsdiag

   subroutine deallocate_type_turbulence(structure)

     implicit none

     type (type_turbulence) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_turbcomposition(structure%composition)
     call deallocate_type_turbcoordsys(structure%coordsys)
     call deallocate_type_turbvar0d(structure%var0d)
     call deallocate_type_turbvar1d(structure%var1d)
     call deallocate_type_turbvar2d(structure%var2d)
     call deallocate_type_turbvar3d(structure%var3d)
     call deallocate_type_turbvar4d(structure%var4d)
     call deallocate_type_turbvar5d(structure%var5d)
     call deallocate_type_turbspec1d(structure%spec1d)
     call deallocate_type_turbenv1d(structure%env1d)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_turbulence

   subroutine deallocate_arr_type_turbulence(structure)

     implicit none

     type (type_turbulence), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbulence(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbulence'
     end if

   end subroutine deallocate_arr_type_turbulence

   subroutine deallocate_type_wall(structure)

     implicit none

     type (type_wall) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_wall_wall0d(structure%wall0d)
     call deallocate_type_wall2d_mhd(structure%wall2d_mhd)
     call deallocate_arr_type_wall2d(structure%wall2d)
     call deallocate_arr_type_wall3d(structure%wall3d)
     call deallocate_arr_type_wall_types(structure%wall_types)
     call deallocate_arr_type_compound_desc(structure%compounds)
     call deallocate_arr_type_element_desc(structure%elements)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_wall

   subroutine deallocate_arr_type_wall(structure)

     implicit none

     type (type_wall), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall'
     end if

   end subroutine deallocate_arr_type_wall

   subroutine deallocate_type_waves(structure)

     implicit none

     type (type_waves) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_arr_type_coherentwave(structure%coherentwave)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_waves

   subroutine deallocate_arr_type_waves(structure)

     implicit none

     type (type_waves), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves'
     end if

   end subroutine deallocate_arr_type_waves

   subroutine deallocate_type_amns_constituentType(structure)

     implicit none

     type (type_amns_constituentType) :: structure

     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_amns_constituentType

   subroutine deallocate_arr_type_amns_constituentType(structure)

     implicit none

     type (type_amns_constituentType), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_amns_constituentType(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_amns_constituentType'
     end if

   end subroutine deallocate_arr_type_amns_constituentType

   subroutine deallocate_type_amns_processType(structure)

     implicit none

     type (type_amns_processType) :: structure

     call deallocate_type_vecstring_type(structure%proc_label)
     call deallocate_arr_type_reacprodType(structure%reactant)
     call deallocate_arr_type_reacprodType(structure%product)
     call deallocate_type_vecstring_type(structure%sup_string)
     call deallocate_type_vecflt_type(structure%sup_real)
     call deallocate_type_vecint_type(structure%sup_int)
     call deallocate_type_identifier(structure%quality)
     call deallocate_type_vecstring_type(structure%err_proc_label)

   end subroutine deallocate_type_amns_processType

   subroutine deallocate_arr_type_amns_processType(structure)

     implicit none

     type (type_amns_processType), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_amns_processType(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_amns_processType'
     end if

   end subroutine deallocate_arr_type_amns_processType

   subroutine deallocate_type_antenna_ec(structure)

     implicit none

     type (type_antenna_ec) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_exp0D(structure%power)
     call deallocate_type_rzphi0D(structure%position)
     call deallocate_type_launchangles(structure%launchangles)
     call deallocate_type_rfbeam(structure%beam)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_antenna_ec

   subroutine deallocate_arr_type_antenna_ec(structure)

     implicit none

     type (type_antenna_ec), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_antenna_ec(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_antenna_ec'
     end if

   end subroutine deallocate_arr_type_antenna_ec

   subroutine deallocate_type_antenna_ic(structure)

     implicit none

     type (type_antenna_ic) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_exp0D(structure%frequency)
     call deallocate_type_exp0D(structure%power)
     call deallocate_type_vecint_type(structure%ntor)
     call deallocate_type_vecflt_type(structure%power_ntor)
     call deallocate_type_antennaic_setup(structure%setup)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_antenna_ic

   subroutine deallocate_arr_type_antenna_ic(structure)

     implicit none

     type (type_antenna_ic), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_antenna_ic(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_antenna_ic'
     end if

   end subroutine deallocate_arr_type_antenna_ic

   subroutine deallocate_type_antenna_lh(structure)

     implicit none

     type (type_antenna_lh) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_exp0D(structure%power)
     call deallocate_type_rzphi0D(structure%position)
     call deallocate_type_antennalh_setup(structure%setup)
     call deallocate_type_plasmaedge(structure%plasmaedge)
     call deallocate_type_rfbeam(structure%beam)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_antenna_lh

   subroutine deallocate_arr_type_antenna_lh(structure)

     implicit none

     type (type_antenna_lh), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_antenna_lh(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_antenna_lh'
     end if

   end subroutine deallocate_arr_type_antenna_lh

   subroutine deallocate_type_antennaic_setup(structure)

     implicit none

     type (type_antennaic_setup) :: structure

     call deallocate_arr_type_straps(structure%straps)
     call deallocate_type_current(structure%current)

   end subroutine deallocate_type_antennaic_setup

   subroutine deallocate_arr_type_antennaic_setup(structure)

     implicit none

     type (type_antennaic_setup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_antennaic_setup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_antennaic_setup'
     end if

   end subroutine deallocate_arr_type_antennaic_setup

   subroutine deallocate_type_antennalh_setup(structure)

     implicit none

     type (type_antennalh_setup) :: structure

     call deallocate_type_modules(structure%modules)

   end subroutine deallocate_type_antennalh_setup

   subroutine deallocate_arr_type_antennalh_setup(structure)

     implicit none

     type (type_antennalh_setup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_antennalh_setup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_antennalh_setup'
     end if

   end subroutine deallocate_arr_type_antennalh_setup

   subroutine deallocate_type_b0r0(structure)

     implicit none

     type (type_b0r0) :: structure


   end subroutine deallocate_type_b0r0

   subroutine deallocate_arr_type_b0r0(structure)

     implicit none

     type (type_b0r0), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_b0r0(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_b0r0'
     end if

   end subroutine deallocate_arr_type_b0r0

   subroutine deallocate_type_bb(structure)

     implicit none

     type (type_bb) :: structure

     call deallocate_type_neutro_resul(structure%neutro_resul)
     call deallocate_type_bb_specs(structure%inboard)
     call deallocate_type_bb_specs(structure%outboard)

   end subroutine deallocate_type_bb

   subroutine deallocate_arr_type_bb(structure)

     implicit none

     type (type_bb), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_bb(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_bb'
     end if

   end subroutine deallocate_arr_type_bb

   subroutine deallocate_type_bb_dimension(structure)

     implicit none

     type (type_bb_dimension) :: structure

     call deallocate_type_vecflt_type(structure%radial)
     call deallocate_type_vecflt_type(structure%toroidal)
     call deallocate_type_vecflt_type(structure%poloidal)

   end subroutine deallocate_type_bb_dimension

   subroutine deallocate_arr_type_bb_dimension(structure)

     implicit none

     type (type_bb_dimension), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_bb_dimension(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_bb_dimension'
     end if

   end subroutine deallocate_arr_type_bb_dimension

   subroutine deallocate_type_bb_geometry(structure)

     implicit none

     type (type_bb_geometry) :: structure

     call deallocate_type_vecflt_type(structure%dr_bp_plates)
     call deallocate_type_vecflt_type(structure%dr_bp_he)
     call deallocate_type_bb_dimension(structure%top_cap_dim)
     call deallocate_type_bb_dimension(structure%bot_cap_dim)

   end subroutine deallocate_type_bb_geometry

   subroutine deallocate_arr_type_bb_geometry(structure)

     implicit none

     type (type_bb_geometry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_bb_geometry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_bb_geometry'
     end if

   end subroutine deallocate_arr_type_bb_geometry

   subroutine deallocate_type_bb_specs(structure)

     implicit none

     type (type_bb_specs) :: structure

     call deallocate_type_bb_dimension(structure%dimension)

   end subroutine deallocate_type_bb_specs

   subroutine deallocate_arr_type_bb_specs(structure)

     implicit none

     type (type_bb_specs), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_bb_specs(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_bb_specs'
     end if

   end subroutine deallocate_arr_type_bb_specs

   subroutine deallocate_type_beamletgroup(structure)

     implicit none

     type (type_beamletgroup) :: structure

     call deallocate_type_rzphi0D(structure%position)
     call deallocate_type_focussing(structure%focussing)
     call deallocate_type_divergence(structure%divergence)
     call deallocate_type_beamlets(structure%beamlets)

   end subroutine deallocate_type_beamletgroup

   subroutine deallocate_arr_type_beamletgroup(structure)

     implicit none

     type (type_beamletgroup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_beamletgroup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_beamletgroup'
     end if

   end subroutine deallocate_arr_type_beamletgroup

   subroutine deallocate_type_beamlets(structure)

     implicit none

     type (type_beamlets) :: structure

     call deallocate_type_rzphi1D(structure%position)
     call deallocate_type_vecflt_type(structure%tang_rad_blt)
     call deallocate_type_vecflt_type(structure%angle_blt)
     call deallocate_type_vecflt_type(structure%pow_frc_blt)

   end subroutine deallocate_type_beamlets

   subroutine deallocate_arr_type_beamlets(structure)

     implicit none

     type (type_beamlets), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_beamlets(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_beamlets'
     end if

   end subroutine deallocate_arr_type_beamlets

   subroutine deallocate_type_beamtracing(structure)

     implicit none

     type (type_beamtracing) :: structure

     call deallocate_type_vecflt_type(structure%dnpar)
     call deallocate_type_vecflt_type(structure%length)
     call deallocate_type_waves_rtposition(structure%position)
     call deallocate_type_waves_rtwavevector(structure%wavevector)
     call deallocate_type_polarization(structure%polarization)
     call deallocate_type_powerflow(structure%powerflow)

   end subroutine deallocate_type_beamtracing

   subroutine deallocate_arr_type_beamtracing(structure)

     implicit none

     type (type_beamtracing), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_beamtracing(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_beamtracing'
     end if

   end subroutine deallocate_arr_type_beamtracing

   subroutine deallocate_type_boundary(structure)

     implicit none

     type (type_boundary) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_boundary

   subroutine deallocate_arr_type_boundary(structure)

     implicit none

     type (type_boundary), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_boundary(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_boundary'
     end if

   end subroutine deallocate_arr_type_boundary

   subroutine deallocate_type_boundary_neutrals(structure)

     implicit none

     type (type_boundary_neutrals) :: structure

     call deallocate_type_vecflt_type(structure%value)

   end subroutine deallocate_type_boundary_neutrals

   subroutine deallocate_arr_type_boundary_neutrals(structure)

     implicit none

     type (type_boundary_neutrals), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_boundary_neutrals(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_boundary_neutrals'
     end if

   end subroutine deallocate_arr_type_boundary_neutrals

   subroutine deallocate_type_boundaryel(structure)

     implicit none

     type (type_boundaryel) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_boundaryel

   subroutine deallocate_arr_type_boundaryel(structure)

     implicit none

     type (type_boundaryel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_boundaryel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_boundaryel'
     end if

   end subroutine deallocate_arr_type_boundaryel

   subroutine deallocate_type_boundaryimp(structure)

     implicit none

     type (type_boundaryimp) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecint_type(structure%type)
     call deallocate_type_vecflt_type(structure%rho)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_boundaryimp

   subroutine deallocate_arr_type_boundaryimp(structure)

     implicit none

     type (type_boundaryimp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_boundaryimp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_boundaryimp'
     end if

   end subroutine deallocate_arr_type_boundaryimp

   subroutine deallocate_type_boundaryion(structure)

     implicit none

     type (type_boundaryion) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecint_type(structure%type)
     call deallocate_type_vecflt_type(structure%rho_tor)

   end subroutine deallocate_type_boundaryion

   subroutine deallocate_arr_type_boundaryion(structure)

     implicit none

     type (type_boundaryion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_boundaryion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_boundaryion'
     end if

   end subroutine deallocate_arr_type_boundaryion

   subroutine deallocate_type_bpol_probes(structure)

     implicit none

     type (type_bpol_probes) :: structure

     call deallocate_type_setup_bprobe(structure%setup_bprobe)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_bpol_probes

   subroutine deallocate_arr_type_bpol_probes(structure)

     implicit none

     type (type_bpol_probes), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_bpol_probes(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_bpol_probes'
     end if

   end subroutine deallocate_arr_type_bpol_probes

   subroutine deallocate_type_calorimetry_heat_source(structure)

     implicit none

     type (type_calorimetry_heat_source) :: structure

     call deallocate_type_vecstring_type(structure%name)

   end subroutine deallocate_type_calorimetry_heat_source

   subroutine deallocate_arr_type_calorimetry_heat_source(structure)

     implicit none

     type (type_calorimetry_heat_source), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_calorimetry_heat_source(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_calorimetry_heat_source'
     end if

   end subroutine deallocate_arr_type_calorimetry_heat_source

   subroutine deallocate_type_circuits(structure)

     implicit none

     type (type_circuits) :: structure

     call deallocate_arr_type_power_conv_component(structure%component)

   end subroutine deallocate_type_circuits

   subroutine deallocate_arr_type_circuits(structure)

     implicit none

     type (type_circuits), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_circuits(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_circuits'
     end if

   end subroutine deallocate_arr_type_circuits

   subroutine deallocate_type_circularcoil(structure)

     implicit none

     type (type_circularcoil) :: structure

     call deallocate_type_rz0D(structure%centre)

   end subroutine deallocate_type_circularcoil

   subroutine deallocate_arr_type_circularcoil(structure)

     implicit none

     type (type_circularcoil), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_circularcoil(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_circularcoil'
     end if

   end subroutine deallocate_arr_type_circularcoil

   subroutine deallocate_type_clusters(structure)

     implicit none

     type (type_clusters) :: structure

     call deallocate_type_vecstring_type(structure%name)

   end subroutine deallocate_type_clusters

   subroutine deallocate_arr_type_clusters(structure)

     implicit none

     type (type_clusters), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_clusters(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_clusters'
     end if

   end subroutine deallocate_arr_type_clusters

   subroutine deallocate_type_codeparam(structure)

     implicit none

     type (type_codeparam) :: structure

     call deallocate_type_vecstring_type(structure%codename)
     call deallocate_type_vecstring_type(structure%codeversion)
     call deallocate_type_vecstring_type(structure%parameters)
     call deallocate_type_vecstring_type(structure%output_diag)

   end subroutine deallocate_type_codeparam

   subroutine deallocate_arr_type_codeparam(structure)

     implicit none

     type (type_codeparam), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_codeparam(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_codeparam'
     end if

   end subroutine deallocate_arr_type_codeparam

   subroutine deallocate_type_coefficients_neutrals(structure)

     implicit none

     type (type_coefficients_neutrals) :: structure

     call deallocate_type_recycling_neutrals(structure%recycling)
     call deallocate_type_sputtering_neutrals(structure%sputtering)

   end subroutine deallocate_type_coefficients_neutrals

   subroutine deallocate_arr_type_coefficients_neutrals(structure)

     implicit none

     type (type_coefficients_neutrals), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coefficients_neutrals(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coefficients_neutrals'
     end if

   end subroutine deallocate_arr_type_coefficients_neutrals

   subroutine deallocate_type_coherentwave(structure)

     implicit none

     type (type_coherentwave) :: structure

     call deallocate_type_enum_instance(structure%wave_id)
     call deallocate_type_composition(structure%composition)
     call deallocate_type_compositions_type(structure%compositions)
     call deallocate_type_waves_global_param(structure%global_param)
     call deallocate_type_waves_grid_1d(structure%grid_1d)
     call deallocate_type_waves_grid_2d(structure%grid_2d)
     call deallocate_type_waves_profiles_1d(structure%profiles_1d)
     call deallocate_type_waves_profiles_2d(structure%profiles_2d)
     call deallocate_arr_type_beamtracing(structure%beamtracing)
     call deallocate_type_fullwave(structure%fullwave)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coherentwave

   subroutine deallocate_arr_type_coherentwave(structure)

     implicit none

     type (type_coherentwave), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coherentwave(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coherentwave'
     end if

   end subroutine deallocate_arr_type_coherentwave

   subroutine deallocate_type_coil(structure)

     implicit none

     type (type_coil) :: structure

     call deallocate_type_desc_coils(structure%desc_coils)
     call deallocate_type_exp1D(structure%coilcurrent)
     call deallocate_type_exp1D(structure%coilvoltage)

   end subroutine deallocate_type_coil

   subroutine deallocate_arr_type_coil(structure)

     implicit none

     type (type_coil), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coil(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coil'
     end if

   end subroutine deallocate_arr_type_coil

   subroutine deallocate_type_com(structure)

     implicit none

     type (type_com) :: structure

     call deallocate_type_vecflt_type(structure%energy)
     call deallocate_type_vecflt_type(structure%magn_mom)
     call deallocate_type_vecflt_type(structure%p_phi)
     call deallocate_type_vecint_type(structure%sigma)

   end subroutine deallocate_type_com

   subroutine deallocate_arr_type_com(structure)

     implicit none

     type (type_com), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_com(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_com'
     end if

   end subroutine deallocate_arr_type_com

   subroutine deallocate_type_complexgrid(structure)

     implicit none

     type (type_complexgrid) :: structure

     call deallocate_type_vecstring_type(structure%id)
     call deallocate_arr_type_complexgrid_space(structure%spaces)
     call deallocate_arr_type_complexgrid_subgrid(structure%subgrids)
     call deallocate_type_complexgrid_metric(structure%metric)
     call deallocate_arr_type_complexgrid_geo_global(structure%geo)
     call deallocate_arr_type_complexgrid_vector(structure%bases)

   end subroutine deallocate_type_complexgrid

   subroutine deallocate_arr_type_complexgrid(structure)

     implicit none

     type (type_complexgrid), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid'
     end if

   end subroutine deallocate_arr_type_complexgrid

   subroutine deallocate_type_complexgrid_geo_global(structure)

     implicit none

     type (type_complexgrid_geo_global) :: structure

     call deallocate_type_vecstring_type(structure%geotypeid)
     call deallocate_type_vecint_type(structure%coordtype)
     call deallocate_arr_type_complexgrid_scalar(structure%geo_matrix)
     call deallocate_arr_type_complexgrid_scalar(structure%measure)

   end subroutine deallocate_type_complexgrid_geo_global

   subroutine deallocate_arr_type_complexgrid_geo_global(structure)

     implicit none

     type (type_complexgrid_geo_global), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_geo_global(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_geo_global'
     end if

   end subroutine deallocate_arr_type_complexgrid_geo_global

   subroutine deallocate_type_complexgrid_indexlist(structure)

     implicit none

     type (type_complexgrid_indexlist) :: structure

     call deallocate_type_vecint_type(structure%range)
     call deallocate_type_vecint_type(structure%ind)

   end subroutine deallocate_type_complexgrid_indexlist

   subroutine deallocate_arr_type_complexgrid_indexlist(structure)

     implicit none

     type (type_complexgrid_indexlist), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_indexlist(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_indexlist'
     end if

   end subroutine deallocate_arr_type_complexgrid_indexlist

   subroutine deallocate_type_complexgrid_metric(structure)

     implicit none

     type (type_complexgrid_metric) :: structure

     call deallocate_arr_type_complexgrid_scalar(structure%measure)
     call deallocate_arr_type_complexgrid_scalar(structure%g11)
     call deallocate_arr_type_complexgrid_scalar(structure%g12)
     call deallocate_arr_type_complexgrid_scalar(structure%g13)
     call deallocate_arr_type_complexgrid_scalar(structure%g22)
     call deallocate_arr_type_complexgrid_scalar(structure%g23)
     call deallocate_arr_type_complexgrid_scalar(structure%g33)
     call deallocate_arr_type_complexgrid_scalar(structure%jacobian)

   end subroutine deallocate_type_complexgrid_metric

   subroutine deallocate_arr_type_complexgrid_metric(structure)

     implicit none

     type (type_complexgrid_metric), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_metric(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_metric'
     end if

   end subroutine deallocate_arr_type_complexgrid_metric

   subroutine deallocate_type_complexgrid_objectlist(structure)

     implicit none

     type (type_complexgrid_objectlist) :: structure

     call deallocate_type_vecint_type(structure%cls)
     call deallocate_arr_type_complexgrid_indexlist(structure%indset)
     call deallocate_type_matint_type(structure%ind)

   end subroutine deallocate_type_complexgrid_objectlist

   subroutine deallocate_arr_type_complexgrid_objectlist(structure)

     implicit none

     type (type_complexgrid_objectlist), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_objectlist(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_objectlist'
     end if

   end subroutine deallocate_arr_type_complexgrid_objectlist

   subroutine deallocate_type_complexgrid_scalar(structure)

     implicit none

     type (type_complexgrid_scalar) :: structure

     call deallocate_type_vecflt_type(structure%scalar)
     call deallocate_type_matflt_type(structure%vector)
     call deallocate_type_array3dflt_type(structure%matrix)

   end subroutine deallocate_type_complexgrid_scalar

   subroutine deallocate_arr_type_complexgrid_scalar(structure)

     implicit none

     type (type_complexgrid_scalar), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_scalar(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_scalar'
     end if

   end subroutine deallocate_arr_type_complexgrid_scalar

   subroutine deallocate_type_complexgrid_scalar_cplx(structure)

     implicit none

     type (type_complexgrid_scalar_cplx) :: structure

     call deallocate_type_veccplx_type(structure%scalar)
     call deallocate_type_matcplx_type(structure%vector)
     call deallocate_type_array3dcplx_type(structure%matrix)

   end subroutine deallocate_type_complexgrid_scalar_cplx

   subroutine deallocate_arr_type_complexgrid_scalar_cplx(structure)

     implicit none

     type (type_complexgrid_scalar_cplx), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_scalar_cplx(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_scalar_cplx'
     end if

   end subroutine deallocate_arr_type_complexgrid_scalar_cplx

   subroutine deallocate_type_complexgrid_scalar_int(structure)

     implicit none

     type (type_complexgrid_scalar_int) :: structure

     call deallocate_type_vecint_type(structure%scalar)
     call deallocate_type_matint_type(structure%vector)
     call deallocate_type_array3dint_type(structure%matrix)

   end subroutine deallocate_type_complexgrid_scalar_int

   subroutine deallocate_arr_type_complexgrid_scalar_int(structure)

     implicit none

     type (type_complexgrid_scalar_int), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_scalar_int(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_scalar_int'
     end if

   end subroutine deallocate_arr_type_complexgrid_scalar_int

   subroutine deallocate_type_complexgrid_scalar_simplestruct(structure)

     implicit none

     type (type_complexgrid_scalar_simplestruct) :: structure

     call deallocate_type_vecflt_type(structure%scalar)
     call deallocate_type_matflt_type(structure%vector)
     call deallocate_type_array3dflt_type(structure%matrix)

   end subroutine deallocate_type_complexgrid_scalar_simplestruct

   subroutine deallocate_arr_type_complexgrid_scalar_simplestruct(structure)

     implicit none

     type (type_complexgrid_scalar_simplestruct), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_scalar_simplestruct(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_scalar_simplestruct'
     end if

   end subroutine deallocate_arr_type_complexgrid_scalar_simplestruct

   subroutine deallocate_type_complexgrid_space(structure)

     implicit none

     type (type_complexgrid_space) :: structure

     call deallocate_type_vecint_type(structure%geotype)
     call deallocate_type_vecstring_type(structure%geotypeid)
     call deallocate_type_matint_type(structure%coordtype)
     call deallocate_arr_type_objects(structure%objects)
     call deallocate_type_vecint_type(structure%xpoints)

   end subroutine deallocate_type_complexgrid_space

   subroutine deallocate_arr_type_complexgrid_space(structure)

     implicit none

     type (type_complexgrid_space), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_space(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_space'
     end if

   end subroutine deallocate_arr_type_complexgrid_space

   subroutine deallocate_type_complexgrid_subgrid(structure)

     implicit none

     type (type_complexgrid_subgrid) :: structure

     call deallocate_type_vecstring_type(structure%id)
     call deallocate_arr_type_complexgrid_objectlist(structure%list)

   end subroutine deallocate_type_complexgrid_subgrid

   subroutine deallocate_arr_type_complexgrid_subgrid(structure)

     implicit none

     type (type_complexgrid_subgrid), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_subgrid(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_subgrid'
     end if

   end subroutine deallocate_arr_type_complexgrid_subgrid

   subroutine deallocate_type_complexgrid_vector(structure)

     implicit none

     type (type_complexgrid_vector) :: structure

     call deallocate_type_vecstring_type(structure%label)
     call deallocate_arr_type_complexgrid_scalar(structure%comp)
     call deallocate_type_vecint_type(structure%align)
     call deallocate_type_vecstring_type(structure%alignid)

   end subroutine deallocate_type_complexgrid_vector

   subroutine deallocate_arr_type_complexgrid_vector(structure)

     implicit none

     type (type_complexgrid_vector), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_vector(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_vector'
     end if

   end subroutine deallocate_arr_type_complexgrid_vector

   subroutine deallocate_type_complexgrid_vector_simplestruct(structure)

     implicit none

     type (type_complexgrid_vector_simplestruct) :: structure

     call deallocate_type_vecstring_type(structure%label)
     call deallocate_arr_type_complexgrid_scalar(structure%comp)
     call deallocate_type_vecint_type(structure%align)
     call deallocate_type_vecstring_type(structure%alignid)

   end subroutine deallocate_type_complexgrid_vector_simplestruct

   subroutine deallocate_arr_type_complexgrid_vector_simplestruct(structure)

     implicit none

     type (type_complexgrid_vector_simplestruct), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_complexgrid_vector_simplestruct(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_complexgrid_vector_simplestruct'
     end if

   end subroutine deallocate_arr_type_complexgrid_vector_simplestruct

   subroutine deallocate_type_composition(structure)

     implicit none

     type (type_composition) :: structure

     call deallocate_type_vecflt_type(structure%amn)
     call deallocate_type_vecflt_type(structure%zn)
     call deallocate_type_vecflt_type(structure%zion)
     call deallocate_type_vecint_type(structure%imp_flag)
     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_composition

   subroutine deallocate_arr_type_composition(structure)

     implicit none

     type (type_composition), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_composition(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_composition'
     end if

   end subroutine deallocate_arr_type_composition

   subroutine deallocate_type_composition_neutrals(structure)

     implicit none

     type (type_composition_neutrals) :: structure

     call deallocate_arr_type_coreneutrals_atomlist(structure%atomlist)
     call deallocate_arr_type_composition_neutralscomp(structure%neutral)

   end subroutine deallocate_type_composition_neutrals

   subroutine deallocate_arr_type_composition_neutrals(structure)

     implicit none

     type (type_composition_neutrals), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_composition_neutrals(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_composition_neutrals'
     end if

   end subroutine deallocate_arr_type_composition_neutrals

   subroutine deallocate_type_composition_neutrals_neutcomp(structure)

     implicit none

     type (type_composition_neutrals_neutcomp) :: structure


   end subroutine deallocate_type_composition_neutrals_neutcomp

   subroutine deallocate_arr_type_composition_neutrals_neutcomp(structure)

     implicit none

     type (type_composition_neutrals_neutcomp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_composition_neutrals_neutcomp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_composition_neutrals_neutcomp'
     end if

   end subroutine deallocate_arr_type_composition_neutrals_neutcomp

   subroutine deallocate_type_composition_neutralscomp(structure)

     implicit none

     type (type_composition_neutralscomp) :: structure

     call deallocate_arr_type_composition_neutrals_neutcomp(structure%neutcomp)
     call deallocate_arr_type_identifier(structure%type)
     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_composition_neutralscomp

   subroutine deallocate_arr_type_composition_neutralscomp(structure)

     implicit none

     type (type_composition_neutralscomp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_composition_neutralscomp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_composition_neutralscomp'
     end if

   end subroutine deallocate_arr_type_composition_neutralscomp

   subroutine deallocate_type_compositions_type(structure)

     implicit none

     type (type_compositions_type) :: structure

     call deallocate_arr_type_nuclei(structure%nuclei)
     call deallocate_arr_type_ions(structure%ions)
     call deallocate_arr_type_impurities(structure%impurities)
     call deallocate_arr_type_composition_neutralscomp(structure%neutralscomp)
     call deallocate_arr_type_edgespecies(structure%edgespecies)
     call deallocate_type_identifier(structure%signature)

   end subroutine deallocate_type_compositions_type

   subroutine deallocate_arr_type_compositions_type(structure)

     implicit none

     type (type_compositions_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_compositions_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_compositions_type'
     end if

   end subroutine deallocate_arr_type_compositions_type

   subroutine deallocate_type_compound_desc(structure)

     implicit none

     type (type_compound_desc) :: structure

     call deallocate_type_vecstring_type(structure%label)
     call deallocate_type_vecflt_type(structure%stochiometry)
     call deallocate_type_vecflt_type(structure%heat_cond)
     call deallocate_type_matflt_type(structure%surf_recrate)

   end subroutine deallocate_type_compound_desc

   subroutine deallocate_arr_type_compound_desc(structure)

     implicit none

     type (type_compound_desc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_compound_desc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_compound_desc'
     end if

   end subroutine deallocate_arr_type_compound_desc

   subroutine deallocate_type_coord_sys(structure)

     implicit none

     type (type_coord_sys) :: structure

     call deallocate_type_vecstring_type(structure%grid_type)
     call deallocate_type_reggrid(structure%grid)
     call deallocate_type_matflt_type(structure%jacobian)
     call deallocate_type_matflt_type(structure%g_11)
     call deallocate_type_matflt_type(structure%g_12)
     call deallocate_type_matflt_type(structure%g_13)
     call deallocate_type_matflt_type(structure%g_22)
     call deallocate_type_matflt_type(structure%g_23)
     call deallocate_type_matflt_type(structure%g_33)
     call deallocate_type_rz2D(structure%position)

   end subroutine deallocate_type_coord_sys

   subroutine deallocate_arr_type_coord_sys(structure)

     implicit none

     type (type_coord_sys), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coord_sys(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coord_sys'
     end if

   end subroutine deallocate_arr_type_coord_sys

   subroutine deallocate_type_coordinates(structure)

     implicit none

     type (type_coordinates) :: structure

     call deallocate_type_vecflt_type(structure%theta)
     call deallocate_type_vecflt_type(structure%phi)

   end subroutine deallocate_type_coordinates

   subroutine deallocate_arr_type_coordinates(structure)

     implicit none

     type (type_coordinates), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coordinates(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coordinates'
     end if

   end subroutine deallocate_arr_type_coordinates

   subroutine deallocate_type_coords(structure)

     implicit none

     type (type_coords) :: structure

     call deallocate_type_vecflt_type(structure%coord)
     call deallocate_type_vecstring_type(structure%coord_label)
     call deallocate_type_vecint_type(structure%extrap_type)
     call deallocate_type_vecstring_type(structure%label)
     call deallocate_type_vecstring_type(structure%unit)

   end subroutine deallocate_type_coords

   subroutine deallocate_arr_type_coords(structure)

     implicit none

     type (type_coords), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coords(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coords'
     end if

   end subroutine deallocate_arr_type_coords

   subroutine deallocate_type_coredelta_values(structure)

     implicit none

     type (type_coredelta_values) :: structure

     call deallocate_type_identifier(structure%deltaid)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%delta_psi)
     call deallocate_type_vecflt_type(structure%delta_te)
     call deallocate_type_matflt_type(structure%delta_ti)
     call deallocate_type_vecflt_type(structure%delta_ne)
     call deallocate_type_matflt_type(structure%delta_ni)
     call deallocate_arr_type_coredelta_values_impurity(structure%impurity)
     call deallocate_type_matflt_type(structure%delta_vtor)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coredelta_values

   subroutine deallocate_arr_type_coredelta_values(structure)

     implicit none

     type (type_coredelta_values), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coredelta_values(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coredelta_values'
     end if

   end subroutine deallocate_arr_type_coredelta_values

   subroutine deallocate_type_coredelta_values_impurity(structure)

     implicit none

     type (type_coredelta_values_impurity) :: structure

     call deallocate_type_matflt_type(structure%delta_tz)
     call deallocate_type_matflt_type(structure%delta_nz)

   end subroutine deallocate_type_coredelta_values_impurity

   subroutine deallocate_arr_type_coredelta_values_impurity(structure)

     implicit none

     type (type_coredelta_values_impurity), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coredelta_values_impurity(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coredelta_values_impurity'
     end if

   end subroutine deallocate_arr_type_coredelta_values_impurity

   subroutine deallocate_type_corefast_values(structure)

     implicit none

     type (type_corefast_values) :: structure

     call deallocate_type_identifier(structure%fastid)
     call deallocate_type_fast_thermal_separation_filter(structure%filter)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%j)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_matflt_type(structure%ni)
     call deallocate_type_vecflt_type(structure%ne)
     call deallocate_type_matflt_type(structure%nz)
     call deallocate_type_matflt_type(structure%pi)
     call deallocate_type_vecflt_type(structure%pe)
     call deallocate_type_matflt_type(structure%pz)
     call deallocate_type_matflt_type(structure%pi_para)
     call deallocate_type_vecflt_type(structure%pe_para)
     call deallocate_type_matflt_type(structure%pz_para)
     call deallocate_type_matflt_type(structure%ui)
     call deallocate_type_matflt_type(structure%uz)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_corefast_values

   subroutine deallocate_arr_type_corefast_values(structure)

     implicit none

     type (type_corefast_values), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefast_values(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefast_values'
     end if

   end subroutine deallocate_arr_type_corefast_values

   subroutine deallocate_type_corefield(structure)

     implicit none

     type (type_corefield) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%ddrho)
     call deallocate_type_vecflt_type(structure%d2drho2)
     call deallocate_type_vecflt_type(structure%ddt)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_boundaryel(structure%boundary)
     call deallocate_type_sourceel(structure%source_term)
     call deallocate_type_coretransel(structure%transp_coef)
     call deallocate_type_fluxel(structure%flux)
     call deallocate_type_vecflt_type(structure%flux_dv_surf)
     call deallocate_type_vecflt_type(structure%time_deriv)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_corefield

   subroutine deallocate_arr_type_corefield(structure)

     implicit none

     type (type_corefield), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefield(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefield'
     end if

   end subroutine deallocate_arr_type_corefield

   subroutine deallocate_type_corefieldion(structure)

     implicit none

     type (type_corefieldion) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_matflt_type(structure%ddrho)
     call deallocate_type_matflt_type(structure%d2drho2)
     call deallocate_type_matflt_type(structure%ddt)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecint_type(structure%flag)
     call deallocate_type_boundaryion(structure%boundary)
     call deallocate_type_sourceion(structure%source_term)
     call deallocate_type_coretransion(structure%transp_coef)
     call deallocate_type_fluxion(structure%flux)
     call deallocate_type_matflt_type(structure%flux_dv_surf)
     call deallocate_type_matflt_type(structure%time_deriv)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_corefieldion

   subroutine deallocate_arr_type_corefieldion(structure)

     implicit none

     type (type_corefieldion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefieldion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefieldion'
     end if

   end subroutine deallocate_arr_type_corefieldion

   subroutine deallocate_type_corefieldneutral(structure)

     implicit none

     type (type_corefieldneutral) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%flux)
     call deallocate_type_boundary_neutrals(structure%boundary)

   end subroutine deallocate_type_corefieldneutral

   subroutine deallocate_arr_type_corefieldneutral(structure)

     implicit none

     type (type_corefieldneutral), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefieldneutral(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefieldneutral'
     end if

   end subroutine deallocate_arr_type_corefieldneutral

   subroutine deallocate_type_corefieldneutrale(structure)

     implicit none

     type (type_corefieldneutrale) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%flux)
     call deallocate_type_boundary_neutrals(structure%boundary)

   end subroutine deallocate_type_corefieldneutrale

   subroutine deallocate_arr_type_corefieldneutrale(structure)

     implicit none

     type (type_corefieldneutrale), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefieldneutrale(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefieldneutrale'
     end if

   end subroutine deallocate_arr_type_corefieldneutrale

   subroutine deallocate_type_corefieldneutralv(structure)

     implicit none

     type (type_corefieldneutralv) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_boundary_neutrals(structure%boundary)

   end subroutine deallocate_type_corefieldneutralv

   subroutine deallocate_arr_type_corefieldneutralv(structure)

     implicit none

     type (type_corefieldneutralv), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefieldneutralv(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefieldneutralv'
     end if

   end subroutine deallocate_arr_type_corefieldneutralv

   subroutine deallocate_type_corefieldneutralv0(structure)

     implicit none

     type (type_corefieldneutralv0) :: structure

     call deallocate_type_corefieldneutralv(structure%toroidal)
     call deallocate_type_corefieldneutralv(structure%poloidal)
     call deallocate_type_corefieldneutralv(structure%radial)

   end subroutine deallocate_type_corefieldneutralv0

   subroutine deallocate_arr_type_corefieldneutralv0(structure)

     implicit none

     type (type_corefieldneutralv0), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_corefieldneutralv0(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_corefieldneutralv0'
     end if

   end subroutine deallocate_arr_type_corefieldneutralv0

   subroutine deallocate_type_coreimpurdiag_sum_radiation(structure)

     implicit none

     type (type_coreimpurdiag_sum_radiation) :: structure

     call deallocate_type_coreimpurediagsum_type(structure%line_rad)
     call deallocate_type_coreimpurediagsum_type(structure%brem_radrec)
     call deallocate_type_coreimpurediagsum_type(structure%sum)

   end subroutine deallocate_type_coreimpurdiag_sum_radiation

   subroutine deallocate_arr_type_coreimpurdiag_sum_radiation(structure)

     implicit none

     type (type_coreimpurdiag_sum_radiation), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurdiag_sum_radiation(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurdiag_sum_radiation'
     end if

   end subroutine deallocate_arr_type_coreimpurdiag_sum_radiation

   subroutine deallocate_type_coreimpurediag_energy(structure)

     implicit none

     type (type_coreimpurediag_energy) :: structure

     call deallocate_type_coreimpurediagprof_type(structure%ionization)
     call deallocate_type_coreimpurediagprof_type(structure%recombin)
     call deallocate_type_coreimpurediagprof_type(structure%sum)

   end subroutine deallocate_type_coreimpurediag_energy

   subroutine deallocate_arr_type_coreimpurediag_energy(structure)

     implicit none

     type (type_coreimpurediag_energy), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediag_energy(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediag_energy'
     end if

   end subroutine deallocate_arr_type_coreimpurediag_energy

   subroutine deallocate_type_coreimpurediag_radiation(structure)

     implicit none

     type (type_coreimpurediag_radiation) :: structure

     call deallocate_type_coreimpurediagprof_type(structure%line_rad)
     call deallocate_type_coreimpurediagprof_type(structure%brem_radrec)
     call deallocate_type_coreimpurediagprof_type(structure%sum)

   end subroutine deallocate_type_coreimpurediag_radiation

   subroutine deallocate_arr_type_coreimpurediag_radiation(structure)

     implicit none

     type (type_coreimpurediag_radiation), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediag_radiation(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediag_radiation'
     end if

   end subroutine deallocate_arr_type_coreimpurediag_radiation

   subroutine deallocate_type_coreimpurediag_sum(structure)

     implicit none

     type (type_coreimpurediag_sum) :: structure

     call deallocate_type_coreimpurdiag_sum_radiation(structure%radiation)
     call deallocate_type_coreimpurediag_sum_energy(structure%energy)

   end subroutine deallocate_type_coreimpurediag_sum

   subroutine deallocate_arr_type_coreimpurediag_sum(structure)

     implicit none

     type (type_coreimpurediag_sum), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediag_sum(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediag_sum'
     end if

   end subroutine deallocate_arr_type_coreimpurediag_sum

   subroutine deallocate_type_coreimpurediag_sum_energy(structure)

     implicit none

     type (type_coreimpurediag_sum_energy) :: structure

     call deallocate_type_coreimpurediagsum_type(structure%ionization)
     call deallocate_type_coreimpurediagsum_type(structure%recombin)
     call deallocate_type_coreimpurediagsum_type(structure%sum)

   end subroutine deallocate_type_coreimpurediag_sum_energy

   subroutine deallocate_arr_type_coreimpurediag_sum_energy(structure)

     implicit none

     type (type_coreimpurediag_sum_energy), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediag_sum_energy(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediag_sum_energy'
     end if

   end subroutine deallocate_arr_type_coreimpurediag_sum_energy

   subroutine deallocate_type_coreimpurediag_type(structure)

     implicit none

     type (type_coreimpurediag_type) :: structure

     call deallocate_type_coreimpurediag_radiation(structure%radiation)
     call deallocate_type_coreimpurediag_energy(structure%energy)

   end subroutine deallocate_type_coreimpurediag_type

   subroutine deallocate_arr_type_coreimpurediag_type(structure)

     implicit none

     type (type_coreimpurediag_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediag_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediag_type'
     end if

   end subroutine deallocate_arr_type_coreimpurediag_type

   subroutine deallocate_type_coreimpurediagprof_type(structure)

     implicit none

     type (type_coreimpurediagprof_type) :: structure

     call deallocate_type_matflt_type(structure%profile)
     call deallocate_type_matflt_type(structure%integral)

   end subroutine deallocate_type_coreimpurediagprof_type

   subroutine deallocate_arr_type_coreimpurediagprof_type(structure)

     implicit none

     type (type_coreimpurediagprof_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediagprof_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediagprof_type'
     end if

   end subroutine deallocate_arr_type_coreimpurediagprof_type

   subroutine deallocate_type_coreimpurediagsum_type(structure)

     implicit none

     type (type_coreimpurediagsum_type) :: structure

     call deallocate_type_vecflt_type(structure%profile)
     call deallocate_type_vecflt_type(structure%integral)

   end subroutine deallocate_type_coreimpurediagsum_type

   subroutine deallocate_arr_type_coreimpurediagsum_type(structure)

     implicit none

     type (type_coreimpurediagsum_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreimpurediagsum_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreimpurediagsum_type'
     end if

   end subroutine deallocate_arr_type_coreimpurediagsum_type

   subroutine deallocate_type_coreneutrals_atomlist(structure)

     implicit none

     type (type_coreneutrals_atomlist) :: structure

     call deallocate_type_identifier(structure%ionimptype)

   end subroutine deallocate_type_coreneutrals_atomlist

   subroutine deallocate_arr_type_coreneutrals_atomlist(structure)

     implicit none

     type (type_coreneutrals_atomlist), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreneutrals_atomlist(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreneutrals_atomlist'
     end if

   end subroutine deallocate_arr_type_coreneutrals_atomlist

   subroutine deallocate_type_coreneutrals_neutraltype(structure)

     implicit none

     type (type_coreneutrals_neutraltype) :: structure

     call deallocate_type_corefieldneutral(structure%n0)
     call deallocate_type_corefieldneutrale(structure%t0)
     call deallocate_type_corefieldneutralv0(structure%v0)

   end subroutine deallocate_type_coreneutrals_neutraltype

   subroutine deallocate_arr_type_coreneutrals_neutraltype(structure)

     implicit none

     type (type_coreneutrals_neutraltype), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreneutrals_neutraltype(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreneutrals_neutraltype'
     end if

   end subroutine deallocate_arr_type_coreneutrals_neutraltype

   subroutine deallocate_type_coreprofile(structure)

     implicit none

     type (type_coreprofile) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_coreprofile

   subroutine deallocate_arr_type_coreprofile(structure)

     implicit none

     type (type_coreprofile), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreprofile(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreprofile'
     end if

   end subroutine deallocate_arr_type_coreprofile

   subroutine deallocate_type_coreprofion(structure)

     implicit none

     type (type_coreprofion) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_coreprofion

   subroutine deallocate_arr_type_coreprofion(structure)

     implicit none

     type (type_coreprofion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coreprofion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coreprofion'
     end if

   end subroutine deallocate_arr_type_coreprofion

   subroutine deallocate_type_coresource_values(structure)

     implicit none

     type (type_coresource_values) :: structure

     call deallocate_type_identifier(structure%sourceid)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%j)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_source_ion(structure%si)
     call deallocate_type_source_vec(structure%se)
     call deallocate_arr_type_source_imp(structure%sz)
     call deallocate_type_source_ion(structure%qi)
     call deallocate_type_source_vec(structure%qe)
     call deallocate_arr_type_source_imp(structure%qz)
     call deallocate_type_source_ion(structure%ui)
     call deallocate_type_source_vec(structure%ujxb)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coresource_values

   subroutine deallocate_arr_type_coresource_values(structure)

     implicit none

     type (type_coresource_values), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coresource_values(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coresource_values'
     end if

   end subroutine deallocate_arr_type_coresource_values

   subroutine deallocate_type_coretransel(structure)

     implicit none

     type (type_coretransel) :: structure

     call deallocate_type_vecflt_type(structure%diff)
     call deallocate_type_vecflt_type(structure%vconv)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_coretransel

   subroutine deallocate_arr_type_coretransel(structure)

     implicit none

     type (type_coretransel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coretransel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coretransel'
     end if

   end subroutine deallocate_arr_type_coretransel

   subroutine deallocate_type_coretransimp(structure)

     implicit none

     type (type_coretransimp) :: structure

     call deallocate_type_matflt_type(structure%diff)
     call deallocate_type_matflt_type(structure%vconv)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_coretransimp

   subroutine deallocate_arr_type_coretransimp(structure)

     implicit none

     type (type_coretransimp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coretransimp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coretransimp'
     end if

   end subroutine deallocate_arr_type_coretransimp

   subroutine deallocate_type_coretransion(structure)

     implicit none

     type (type_coretransion) :: structure

     call deallocate_type_matflt_type(structure%diff)
     call deallocate_type_matflt_type(structure%vconv)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_coretransion

   subroutine deallocate_arr_type_coretransion(structure)

     implicit none

     type (type_coretransion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coretransion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coretransion'
     end if

   end subroutine deallocate_arr_type_coretransion

   subroutine deallocate_type_coretransp_values(structure)

     implicit none

     type (type_coretransp_values) :: structure

     call deallocate_type_identifier(structure%transportid)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_ni_transp(structure%ni_transp)
     call deallocate_type_ne_transp(structure%ne_transp)
     call deallocate_arr_type_transcoefimp(structure%nz_transp)
     call deallocate_type_transcoefion(structure%ti_transp)
     call deallocate_type_transcoefel(structure%te_transp)
     call deallocate_arr_type_transcoefimp(structure%tz_transp)
     call deallocate_type_transcoefvtor(structure%vtor_transp)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_coretransp_values

   subroutine deallocate_arr_type_coretransp_values(structure)

     implicit none

     type (type_coretransp_values), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_coretransp_values(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_coretransp_values'
     end if

   end subroutine deallocate_arr_type_coretransp_values

   subroutine deallocate_type_current(structure)

     implicit none

     type (type_current) :: structure

     call deallocate_type_vecint_type(structure%mpol)
     call deallocate_type_vecint_type(structure%ntor)
     call deallocate_type_exp1D(structure%spectrum)
     call deallocate_type_rz0D(structure%rz_reference)

   end subroutine deallocate_type_current

   subroutine deallocate_arr_type_current(structure)

     implicit none

     type (type_current), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_current(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_current'
     end if

   end subroutine deallocate_arr_type_current

   subroutine deallocate_type_cxmeasure(structure)

     implicit none

     type (type_cxmeasure) :: structure

     call deallocate_type_exp1D(structure%ti)
     call deallocate_type_exp1D(structure%vtor)
     call deallocate_type_exp1D(structure%vpol)

   end subroutine deallocate_type_cxmeasure

   subroutine deallocate_arr_type_cxmeasure(structure)

     implicit none

     type (type_cxmeasure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_cxmeasure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_cxmeasure'
     end if

   end subroutine deallocate_arr_type_cxmeasure

   subroutine deallocate_type_cxsetup(structure)

     implicit none

     type (type_cxsetup) :: structure

     call deallocate_type_vecflt_type(structure%amn)
     call deallocate_type_vecflt_type(structure%zn)
     call deallocate_type_rzphi1Dexp(structure%position)

   end subroutine deallocate_type_cxsetup

   subroutine deallocate_arr_type_cxsetup(structure)

     implicit none

     type (type_cxsetup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_cxsetup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_cxsetup'
     end if

   end subroutine deallocate_arr_type_cxsetup

   subroutine deallocate_type_data_release(structure)

     implicit none

     type (type_data_release) :: structure

     call deallocate_type_vecstring_type(structure%description)

   end subroutine deallocate_type_data_release

   subroutine deallocate_arr_type_data_release(structure)

     implicit none

     type (type_data_release), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_data_release(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_data_release'
     end if

   end subroutine deallocate_arr_type_data_release

   subroutine deallocate_type_datainfo(structure)

     implicit none

     type (type_datainfo) :: structure

     call deallocate_type_vecstring_type(structure%dataprovider)
     call deallocate_type_vecstring_type(structure%putdate)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecstring_type(structure%comment)
     call deallocate_type_whatref(structure%whatref)
     call deallocate_type_putinfo(structure%putinfo)

   end subroutine deallocate_type_datainfo

   subroutine deallocate_arr_type_datainfo(structure)

     implicit none

     type (type_datainfo), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_datainfo(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_datainfo'
     end if

   end subroutine deallocate_arr_type_datainfo

   subroutine deallocate_type_desc_coils(structure)

     implicit none

     type (type_desc_coils) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%closed)
     call deallocate_arr_type_edges(structure%edges)

   end subroutine deallocate_type_desc_coils

   subroutine deallocate_arr_type_desc_coils(structure)

     implicit none

     type (type_desc_coils), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_desc_coils(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_desc_coils'
     end if

   end subroutine deallocate_arr_type_desc_coils

   subroutine deallocate_type_desc_impur(structure)

     implicit none

     type (type_desc_impur) :: structure

     call deallocate_type_vecflt_type(structure%amn)
     call deallocate_type_vecint_type(structure%zn)
     call deallocate_type_vecint_type(structure%i_ion)
     call deallocate_type_vecint_type(structure%nzimp)
     call deallocate_type_matint_type(structure%zmin)
     call deallocate_type_matint_type(structure%zmax)
     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_desc_impur

   subroutine deallocate_arr_type_desc_impur(structure)

     implicit none

     type (type_desc_impur), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_desc_impur(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_desc_impur'
     end if

   end subroutine deallocate_arr_type_desc_impur

   subroutine deallocate_type_desc_iron(structure)

     implicit none

     type (type_desc_iron) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_permeability(structure%permeability)
     call deallocate_type_geom_iron(structure%geom_iron)

   end subroutine deallocate_type_desc_iron

   subroutine deallocate_arr_type_desc_iron(structure)

     implicit none

     type (type_desc_iron), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_desc_iron(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_desc_iron'
     end if

   end subroutine deallocate_arr_type_desc_iron

   subroutine deallocate_type_desc_pfcoils(structure)

     implicit none

     type (type_desc_pfcoils) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_vecflt_type(structure%res)
     call deallocate_type_vecflt_type(structure%emax)
     call deallocate_type_structure_cs(structure%structure_cs)
     call deallocate_type_vecint_type(structure%nelement)
     call deallocate_type_pfelement(structure%pfelement)

   end subroutine deallocate_type_desc_pfcoils

   subroutine deallocate_arr_type_desc_pfcoils(structure)

     implicit none

     type (type_desc_pfcoils), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_desc_pfcoils(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_desc_pfcoils'
     end if

   end subroutine deallocate_arr_type_desc_pfcoils

   subroutine deallocate_type_desc_supply(structure)

     implicit none

     type (type_desc_supply) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_vecstring_type(structure%type)
     call deallocate_type_vecflt_type(structure%delay)
     call deallocate_type_filter(structure%filter)
     call deallocate_type_vecflt_type(structure%imin)
     call deallocate_type_vecflt_type(structure%imax)
     call deallocate_type_vecflt_type(structure%res)
     call deallocate_type_vecflt_type(structure%umin)
     call deallocate_type_vecflt_type(structure%umax)
     call deallocate_type_vecflt_type(structure%emax)

   end subroutine deallocate_type_desc_supply

   subroutine deallocate_arr_type_desc_supply(structure)

     implicit none

     type (type_desc_supply), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_desc_supply(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_desc_supply'
     end if

   end subroutine deallocate_arr_type_desc_supply

   subroutine deallocate_type_diag_func(structure)

     implicit none

     type (type_diag_func) :: structure

     call deallocate_type_vecstring_type(structure%description)
     call deallocate_type_matflt_type(structure%transf_mat)

   end subroutine deallocate_type_diag_func

   subroutine deallocate_arr_type_diag_func(structure)

     implicit none

     type (type_diag_func), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_diag_func(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_diag_func'
     end if

   end subroutine deallocate_arr_type_diag_func

   subroutine deallocate_type_dist_collisional_transfer_0d(structure)

     implicit none

     type (type_dist_collisional_transfer_0d) :: structure


   end subroutine deallocate_type_dist_collisional_transfer_0d

   subroutine deallocate_arr_type_dist_collisional_transfer_0d(structure)

     implicit none

     type (type_dist_collisional_transfer_0d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_collisional_transfer_0d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_collisional_transfer_0d'
     end if

   end subroutine deallocate_arr_type_dist_collisional_transfer_0d

   subroutine deallocate_type_dist_collisional_transfer_1d(structure)

     implicit none

     type (type_dist_collisional_transfer_1d) :: structure

     call deallocate_type_vecflt_type(structure%power_th)
     call deallocate_type_vecflt_type(structure%power_fast)
     call deallocate_type_vecflt_type(structure%torque_th)
     call deallocate_type_vecflt_type(structure%torque_fast)

   end subroutine deallocate_type_dist_collisional_transfer_1d

   subroutine deallocate_arr_type_dist_collisional_transfer_1d(structure)

     implicit none

     type (type_dist_collisional_transfer_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_collisional_transfer_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_collisional_transfer_1d'
     end if

   end subroutine deallocate_arr_type_dist_collisional_transfer_1d

   subroutine deallocate_type_dist_collisional_transfer_2d(structure)

     implicit none

     type (type_dist_collisional_transfer_2d) :: structure

     call deallocate_type_matflt_type(structure%power_th)
     call deallocate_type_matflt_type(structure%power_fast)
     call deallocate_type_matflt_type(structure%torque_th)
     call deallocate_type_matflt_type(structure%torque_fast)

   end subroutine deallocate_type_dist_collisional_transfer_2d

   subroutine deallocate_arr_type_dist_collisional_transfer_2d(structure)

     implicit none

     type (type_dist_collisional_transfer_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_collisional_transfer_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_collisional_transfer_2d'
     end if

   end subroutine deallocate_arr_type_dist_collisional_transfer_2d

   subroutine deallocate_type_dist_distrivec_distfunc_fexp_param(structure)

     implicit none

     type (type_dist_distrivec_distfunc_fexp_param) :: structure

     call deallocate_type_equatorial_plane(structure%equatorial)
     call deallocate_type_vecflt_type(structure%temperature)

   end subroutine deallocate_type_dist_distrivec_distfunc_fexp_param

   subroutine deallocate_arr_type_dist_distrivec_distfunc_fexp_param(structure)

     implicit none

     type (type_dist_distrivec_distfunc_fexp_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_distrivec_distfunc_fexp_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_distrivec_distfunc_fexp_param'
     end if

   end subroutine deallocate_arr_type_dist_distrivec_distfunc_fexp_param

   subroutine deallocate_type_dist_ff(structure)

     implicit none

     type (type_dist_ff) :: structure

     call deallocate_type_dist_grid_info(structure%grid_info)
     call deallocate_arr_type_topo_regions(structure%topo_regions)

   end subroutine deallocate_type_dist_ff

   subroutine deallocate_arr_type_dist_ff(structure)

     implicit none

     type (type_dist_ff), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_ff(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_ff'
     end if

   end subroutine deallocate_arr_type_dist_ff

   subroutine deallocate_type_dist_func(structure)

     implicit none

     type (type_dist_func) :: structure

     call deallocate_type_weighted_markers(structure%markers)
     call deallocate_arr_type_dist_ff(structure%f_expan_topo)
     call deallocate_arr_type_f_expansion(structure%f_expansion)

   end subroutine deallocate_type_dist_func

   subroutine deallocate_arr_type_dist_func(structure)

     implicit none

     type (type_dist_func), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_func(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_func'
     end if

   end subroutine deallocate_arr_type_dist_func

   subroutine deallocate_type_dist_geometry_0d(structure)

     implicit none

     type (type_dist_geometry_0d) :: structure

     call deallocate_type_rz0D(structure%mag_axis)
     call deallocate_type_b0r0(structure%toroid_field)

   end subroutine deallocate_type_dist_geometry_0d

   subroutine deallocate_arr_type_dist_geometry_0d(structure)

     implicit none

     type (type_dist_geometry_0d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_geometry_0d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_geometry_0d'
     end if

   end subroutine deallocate_arr_type_dist_geometry_0d

   subroutine deallocate_type_dist_geometry_1d(structure)

     implicit none

     type (type_dist_geometry_1d) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)

   end subroutine deallocate_type_dist_geometry_1d

   subroutine deallocate_arr_type_dist_geometry_1d(structure)

     implicit none

     type (type_dist_geometry_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_geometry_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_geometry_1d'
     end if

   end subroutine deallocate_arr_type_dist_geometry_1d

   subroutine deallocate_type_dist_geometry_2d(structure)

     implicit none

     type (type_dist_geometry_2d) :: structure

     call deallocate_type_matflt_type(structure%r)
     call deallocate_type_matflt_type(structure%z)
     call deallocate_type_matflt_type(structure%rho_tor)
     call deallocate_type_matflt_type(structure%psi)
     call deallocate_type_matflt_type(structure%theta_geom)
     call deallocate_type_matflt_type(structure%theta_strt)

   end subroutine deallocate_type_dist_geometry_2d

   subroutine deallocate_arr_type_dist_geometry_2d(structure)

     implicit none

     type (type_dist_geometry_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_geometry_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_geometry_2d'
     end if

   end subroutine deallocate_arr_type_dist_geometry_2d

   subroutine deallocate_type_dist_global_param(structure)

     implicit none

     type (type_dist_global_param) :: structure

     call deallocate_type_dist_geometry_0d(structure%geometry)
     call deallocate_type_dist_state_0d(structure%state)
     call deallocate_type_dist_collisional_transfer_0d(structure%collisions_e)
     call deallocate_arr_type_dist_collisional_transfer_0d(structure%collisions_i)
     call deallocate_arr_type_dist_global_param_collisions_z(structure%collisions_z)
     call deallocate_arr_type_dist_sources_0d(structure%sources)

   end subroutine deallocate_type_dist_global_param

   subroutine deallocate_arr_type_dist_global_param(structure)

     implicit none

     type (type_dist_global_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_global_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_global_param'
     end if

   end subroutine deallocate_arr_type_dist_global_param

   subroutine deallocate_type_dist_global_param_collisions_z(structure)

     implicit none

     type (type_dist_global_param_collisions_z) :: structure

     call deallocate_arr_type_dist_collisional_transfer_0d(structure%charge_state)

   end subroutine deallocate_type_dist_global_param_collisions_z

   subroutine deallocate_arr_type_dist_global_param_collisions_z(structure)

     implicit none

     type (type_dist_global_param_collisions_z), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_global_param_collisions_z(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_global_param_collisions_z'
     end if

   end subroutine deallocate_arr_type_dist_global_param_collisions_z

   subroutine deallocate_type_dist_grid_info(structure)

     implicit none

     type (type_dist_grid_info) :: structure

     call deallocate_type_vecint_type(structure%grid_coord)
     call deallocate_type_vecstring_type(structure%topology)
     call deallocate_arr_type_omnigen_surf(structure%omnigen_surf)

   end subroutine deallocate_type_dist_grid_info

   subroutine deallocate_arr_type_dist_grid_info(structure)

     implicit none

     type (type_dist_grid_info), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_grid_info(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_grid_info'
     end if

   end subroutine deallocate_arr_type_dist_grid_info

   subroutine deallocate_type_dist_profile_values_1d(structure)

     implicit none

     type (type_dist_profile_values_1d) :: structure

     call deallocate_type_dist_state_1d(structure%state)
     call deallocate_type_dist_collisional_transfer_1d(structure%collisions_e)
     call deallocate_arr_type_dist_collisional_transfer_1d(structure%collisions_i)
     call deallocate_arr_type_dist_profiles_1d_collisions_z(structure%collisions_z)
     call deallocate_arr_type_dist_sources_1d(structure%sources)

   end subroutine deallocate_type_dist_profile_values_1d

   subroutine deallocate_arr_type_dist_profile_values_1d(structure)

     implicit none

     type (type_dist_profile_values_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_profile_values_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_profile_values_1d'
     end if

   end subroutine deallocate_arr_type_dist_profile_values_1d

   subroutine deallocate_type_dist_profile_values_2d(structure)

     implicit none

     type (type_dist_profile_values_2d) :: structure

     call deallocate_type_dist_state_2d(structure%state)
     call deallocate_type_dist_collisional_transfer_2d(structure%collisions_e)
     call deallocate_arr_type_dist_collisional_transfer_2d(structure%collisions_i)
     call deallocate_arr_type_dist_profiles2d_collisions_z(structure%collisions_z)

   end subroutine deallocate_type_dist_profile_values_2d

   subroutine deallocate_arr_type_dist_profile_values_2d(structure)

     implicit none

     type (type_dist_profile_values_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_profile_values_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_profile_values_2d'
     end if

   end subroutine deallocate_arr_type_dist_profile_values_2d

   subroutine deallocate_type_dist_profiles2d_collisions_z(structure)

     implicit none

     type (type_dist_profiles2d_collisions_z) :: structure

     call deallocate_arr_type_dist_collisional_transfer_2d(structure%charge_state)

   end subroutine deallocate_type_dist_profiles2d_collisions_z

   subroutine deallocate_arr_type_dist_profiles2d_collisions_z(structure)

     implicit none

     type (type_dist_profiles2d_collisions_z), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_profiles2d_collisions_z(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_profiles2d_collisions_z'
     end if

   end subroutine deallocate_arr_type_dist_profiles2d_collisions_z

   subroutine deallocate_type_dist_profiles_1d(structure)

     implicit none

     type (type_dist_profiles_1d) :: structure

     call deallocate_type_dist_geometry_1d(structure%geometry)
     call deallocate_type_dist_state_1d(structure%state)
     call deallocate_type_dist_collisional_transfer_1d(structure%collisions_e)
     call deallocate_arr_type_dist_collisional_transfer_1d(structure%collisions_i)
     call deallocate_arr_type_dist_profiles_1d_collisions_z(structure%collisions_z)
     call deallocate_type_dist_thermalised_1d(structure%thermalised)
     call deallocate_arr_type_dist_sources_1d(structure%sources)
     call deallocate_type_dist_profile_values_1d(structure%trapped)
     call deallocate_type_dist_profile_values_1d(structure%co_passing)
     call deallocate_type_dist_profile_values_1d(structure%cntr_passing)

   end subroutine deallocate_type_dist_profiles_1d

   subroutine deallocate_arr_type_dist_profiles_1d(structure)

     implicit none

     type (type_dist_profiles_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_profiles_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_profiles_1d'
     end if

   end subroutine deallocate_arr_type_dist_profiles_1d

   subroutine deallocate_type_dist_profiles_1d_collisions_z(structure)

     implicit none

     type (type_dist_profiles_1d_collisions_z) :: structure

     call deallocate_arr_type_dist_collisional_transfer_1d(structure%charge_state)

   end subroutine deallocate_type_dist_profiles_1d_collisions_z

   subroutine deallocate_arr_type_dist_profiles_1d_collisions_z(structure)

     implicit none

     type (type_dist_profiles_1d_collisions_z), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_profiles_1d_collisions_z(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_profiles_1d_collisions_z'
     end if

   end subroutine deallocate_arr_type_dist_profiles_1d_collisions_z

   subroutine deallocate_type_dist_profiles_2d(structure)

     implicit none

     type (type_dist_profiles_2d) :: structure

     call deallocate_type_dist_geometry_2d(structure%geometry)
     call deallocate_type_dist_state_2d(structure%state)
     call deallocate_type_dist_collisional_transfer_2d(structure%collisions_e)
     call deallocate_arr_type_dist_collisional_transfer_2d(structure%collisions_i)
     call deallocate_arr_type_dist_profiles2d_collisions_z(structure%collisions_z)
     call deallocate_type_dist_profile_values_2d(structure%trapped)
     call deallocate_type_dist_profile_values_2d(structure%co_passing)
     call deallocate_type_dist_profile_values_2d(structure%cntr_passing)

   end subroutine deallocate_type_dist_profiles_2d

   subroutine deallocate_arr_type_dist_profiles_2d(structure)

     implicit none

     type (type_dist_profiles_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_profiles_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_profiles_2d'
     end if

   end subroutine deallocate_arr_type_dist_profiles_2d

   subroutine deallocate_type_dist_sources_0d(structure)

     implicit none

     type (type_dist_sources_0d) :: structure

     call deallocate_type_dist_sources_reference(structure%source_ref)

   end subroutine deallocate_type_dist_sources_0d

   subroutine deallocate_arr_type_dist_sources_0d(structure)

     implicit none

     type (type_dist_sources_0d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_sources_0d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_sources_0d'
     end if

   end subroutine deallocate_arr_type_dist_sources_0d

   subroutine deallocate_type_dist_sources_1d(structure)

     implicit none

     type (type_dist_sources_1d) :: structure

     call deallocate_type_dist_sources_reference(structure%source_ref)
     call deallocate_type_vecflt_type(structure%particle)
     call deallocate_type_vecflt_type(structure%momentum)
     call deallocate_type_vecflt_type(structure%energy)

   end subroutine deallocate_type_dist_sources_1d

   subroutine deallocate_arr_type_dist_sources_1d(structure)

     implicit none

     type (type_dist_sources_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_sources_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_sources_1d'
     end if

   end subroutine deallocate_arr_type_dist_sources_1d

   subroutine deallocate_type_dist_sources_reference(structure)

     implicit none

     type (type_dist_sources_reference) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_vecint_type(structure%index_waveid)
     call deallocate_type_vecint_type(structure%index_srcid)

   end subroutine deallocate_type_dist_sources_reference

   subroutine deallocate_arr_type_dist_sources_reference(structure)

     implicit none

     type (type_dist_sources_reference), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_sources_reference(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_sources_reference'
     end if

   end subroutine deallocate_arr_type_dist_sources_reference

   subroutine deallocate_type_dist_state_0d(structure)

     implicit none

     type (type_dist_state_0d) :: structure


   end subroutine deallocate_type_dist_state_0d

   subroutine deallocate_arr_type_dist_state_0d(structure)

     implicit none

     type (type_dist_state_0d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_state_0d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_state_0d'
     end if

   end subroutine deallocate_arr_type_dist_state_0d

   subroutine deallocate_type_dist_state_1d(structure)

     implicit none

     type (type_dist_state_1d) :: structure

     call deallocate_type_vecflt_type(structure%dens)
     call deallocate_type_vecflt_type(structure%dens_fast)
     call deallocate_type_vecflt_type(structure%pres)
     call deallocate_type_vecflt_type(structure%pres_fast)
     call deallocate_type_vecflt_type(structure%pres_fast_pa)
     call deallocate_type_vecflt_type(structure%momentm_fast)
     call deallocate_type_vecflt_type(structure%current)
     call deallocate_type_vecflt_type(structure%current_fast)
     call deallocate_type_vecflt_type(structure%torque_jrxb)

   end subroutine deallocate_type_dist_state_1d

   subroutine deallocate_arr_type_dist_state_1d(structure)

     implicit none

     type (type_dist_state_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_state_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_state_1d'
     end if

   end subroutine deallocate_arr_type_dist_state_1d

   subroutine deallocate_type_dist_state_2d(structure)

     implicit none

     type (type_dist_state_2d) :: structure

     call deallocate_type_matflt_type(structure%dens)
     call deallocate_type_matflt_type(structure%dens_fast)
     call deallocate_type_matflt_type(structure%pres)
     call deallocate_type_matflt_type(structure%pres_fast)
     call deallocate_type_matflt_type(structure%pres_fast_pa)
     call deallocate_type_matflt_type(structure%momentm_fast)
     call deallocate_type_matflt_type(structure%current)
     call deallocate_type_matflt_type(structure%current_fast)
     call deallocate_type_matflt_type(structure%torque_jrxb)

   end subroutine deallocate_type_dist_state_2d

   subroutine deallocate_arr_type_dist_state_2d(structure)

     implicit none

     type (type_dist_state_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_state_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_state_2d'
     end if

   end subroutine deallocate_arr_type_dist_state_2d

   subroutine deallocate_type_dist_thermalised_1d(structure)

     implicit none

     type (type_dist_thermalised_1d) :: structure

     call deallocate_type_vecflt_type(structure%particle)
     call deallocate_type_vecflt_type(structure%momentum)
     call deallocate_type_vecflt_type(structure%energy)

   end subroutine deallocate_type_dist_thermalised_1d

   subroutine deallocate_arr_type_dist_thermalised_1d(structure)

     implicit none

     type (type_dist_thermalised_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_dist_thermalised_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_dist_thermalised_1d'
     end if

   end subroutine deallocate_arr_type_dist_thermalised_1d

   subroutine deallocate_type_distri_vec(structure)

     implicit none

     type (type_distri_vec) :: structure

     call deallocate_arr_type_enum_instance(structure%wave_id)
     call deallocate_arr_type_enum_instance(structure%source_id)
     call deallocate_type_species_reference(structure%species)
     call deallocate_type_fast_thermal_separation_filter(structure%fast_filter)
     call deallocate_type_dist_global_param(structure%global_param)
     call deallocate_type_dist_profiles_1d(structure%profiles_1d)
     call deallocate_type_dist_profiles_2d(structure%profiles_2d)
     call deallocate_type_dist_func(structure%dist_func)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_distri_vec

   subroutine deallocate_arr_type_distri_vec(structure)

     implicit none

     type (type_distri_vec), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distri_vec(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distri_vec'
     end if

   end subroutine deallocate_arr_type_distri_vec

   subroutine deallocate_type_distsource_global_param(structure)

     implicit none

     type (type_distsource_global_param) :: structure

     call deallocate_type_exp0D(structure%src_pow)
     call deallocate_type_exp0D(structure%src_rate)
     call deallocate_type_rz0D(structure%mag_axis)
     call deallocate_type_b0r0(structure%toroid_field)

   end subroutine deallocate_type_distsource_global_param

   subroutine deallocate_arr_type_distsource_global_param(structure)

     implicit none

     type (type_distsource_global_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distsource_global_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distsource_global_param'
     end if

   end subroutine deallocate_arr_type_distsource_global_param

   subroutine deallocate_type_distsource_line_src_prof(structure)

     implicit none

     type (type_distsource_line_src_prof) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%R)
     call deallocate_type_vecflt_type(structure%Z)
     call deallocate_type_vecflt_type(structure%theta)
     call deallocate_type_vecflt_type(structure%theta_id)
     call deallocate_type_matflt_type(structure%th2th_pol)
     call deallocate_type_vecflt_type(structure%pitch)
     call deallocate_type_vecflt_type(structure%energy)
     call deallocate_type_vecflt_type(structure%ang_momentum)
     call deallocate_type_vecflt_type(structure%src_rate)

   end subroutine deallocate_type_distsource_line_src_prof

   subroutine deallocate_arr_type_distsource_line_src_prof(structure)

     implicit none

     type (type_distsource_line_src_prof), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distsource_line_src_prof(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distsource_line_src_prof'
     end if

   end subroutine deallocate_arr_type_distsource_line_src_prof

   subroutine deallocate_type_distsource_profiles_1d(structure)

     implicit none

     type (type_distsource_profiles_1d) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_exp1D(structure%pow_den)
     call deallocate_type_exp1D(structure%trq_den)
     call deallocate_type_exp1D(structure%src_rate)

   end subroutine deallocate_type_distsource_profiles_1d

   subroutine deallocate_arr_type_distsource_profiles_1d(structure)

     implicit none

     type (type_distsource_profiles_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distsource_profiles_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distsource_profiles_1d'
     end if

   end subroutine deallocate_arr_type_distsource_profiles_1d

   subroutine deallocate_type_distsource_profiles_2d(structure)

     implicit none

     type (type_distsource_profiles_2d) :: structure

     call deallocate_type_vecint_type(structure%grid_coord)
     call deallocate_type_matflt_type(structure%dim1)
     call deallocate_type_matflt_type(structure%dim2)
     call deallocate_type_matflt_type(structure%g11)
     call deallocate_type_matflt_type(structure%g12)
     call deallocate_type_matflt_type(structure%g21)
     call deallocate_type_matflt_type(structure%g22)
     call deallocate_type_exp2D(structure%pow_den)
     call deallocate_type_exp2D(structure%src_rate)

   end subroutine deallocate_type_distsource_profiles_2d

   subroutine deallocate_arr_type_distsource_profiles_2d(structure)

     implicit none

     type (type_distsource_profiles_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distsource_profiles_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distsource_profiles_2d'
     end if

   end subroutine deallocate_arr_type_distsource_profiles_2d

   subroutine deallocate_type_distsource_source(structure)

     implicit none

     type (type_distsource_source) :: structure

     call deallocate_arr_type_enum_instance(structure%source_id)
     call deallocate_type_species_reference(structure%species)
     call deallocate_type_distsource_global_param(structure%global_param)
     call deallocate_type_distsource_profiles_1d(structure%profiles_1d)
     call deallocate_type_distsource_profiles_2d(structure%profiles_2d)
     call deallocate_arr_type_distsource_line_src_prof(structure%line_srcprof)
     call deallocate_type_source_rate(structure%source_rate)
     call deallocate_type_weighted_markers(structure%markers)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_distsource_source

   subroutine deallocate_arr_type_distsource_source(structure)

     implicit none

     type (type_distsource_source), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_distsource_source(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_distsource_source'
     end if

   end subroutine deallocate_arr_type_distsource_source

   subroutine deallocate_type_divergence(structure)

     implicit none

     type (type_divergence) :: structure

     call deallocate_type_vecflt_type(structure%frac_divcomp)
     call deallocate_type_vecflt_type(structure%div_vert)
     call deallocate_type_vecflt_type(structure%div_horiz)

   end subroutine deallocate_type_divergence

   subroutine deallocate_arr_type_divergence(structure)

     implicit none

     type (type_divergence), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_divergence(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_divergence'
     end if

   end subroutine deallocate_arr_type_divergence

   subroutine deallocate_type_e_components(structure)

     implicit none

     type (type_e_components) :: structure

     call deallocate_type_complexgrid_scalar_cplx(structure%e_plus)
     call deallocate_type_complexgrid_scalar_cplx(structure%e_minus)
     call deallocate_type_complexgrid_scalar_cplx(structure%e_para)
     call deallocate_type_complexgrid_scalar_cplx(structure%e_norm)
     call deallocate_type_complexgrid_scalar_cplx(structure%e_binorm)
     call deallocate_type_complexgrid_scalar_cplx(structure%b_norm)
     call deallocate_type_complexgrid_scalar_cplx(structure%b_binorm)
     call deallocate_type_complexgrid_scalar_cplx(structure%b_para)
     call deallocate_type_complexgrid_scalar_cplx(structure%k_perp)

   end subroutine deallocate_type_e_components

   subroutine deallocate_arr_type_e_components(structure)

     implicit none

     type (type_e_components), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_e_components(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_e_components'
     end if

   end subroutine deallocate_arr_type_e_components

   subroutine deallocate_type_ecemeasure(structure)

     implicit none

     type (type_ecemeasure) :: structure

     call deallocate_type_rzphi1Dexp(structure%position)
     call deallocate_type_exp1D(structure%te)

   end subroutine deallocate_type_ecemeasure

   subroutine deallocate_arr_type_ecemeasure(structure)

     implicit none

     type (type_ecemeasure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ecemeasure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ecemeasure'
     end if

   end subroutine deallocate_arr_type_ecemeasure

   subroutine deallocate_type_ecesetup(structure)

     implicit none

     type (type_ecesetup) :: structure

     call deallocate_type_vecflt_type(structure%frequency)
     call deallocate_type_setup_line_exp(structure%los)

   end subroutine deallocate_type_ecesetup

   subroutine deallocate_arr_type_ecesetup(structure)

     implicit none

     type (type_ecesetup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ecesetup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ecesetup'
     end if

   end subroutine deallocate_arr_type_ecesetup

   subroutine deallocate_type_edge_fluid(structure)

     implicit none

     type (type_edge_fluid) :: structure

     call deallocate_type_edge_fluid_scalar_simplestruct(structure%ne)
     call deallocate_arr_type_edge_fluid_scalar(structure%ni)
     call deallocate_type_edge_fluid_vector_simplestruct(structure%ve)
     call deallocate_arr_type_edge_fluid_vector(structure%vi)
     call deallocate_type_edge_fluid_scalar_simplestruct(structure%te)
     call deallocate_arr_type_edge_fluid_scalar(structure%ti)
     call deallocate_type_edge_fluid_vector_simplestruct(structure%te_aniso)
     call deallocate_arr_type_edge_fluid_vector(structure%ti_aniso)
     call deallocate_type_edge_fluid_scalar_simplestruct(structure%po)
     call deallocate_type_edge_fluid_vector_simplestruct(structure%j)
     call deallocate_arr_type_complexgrid_vector(structure%b)

   end subroutine deallocate_type_edge_fluid

   subroutine deallocate_arr_type_edge_fluid(structure)

     implicit none

     type (type_edge_fluid), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_fluid(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_fluid'
     end if

   end subroutine deallocate_arr_type_edge_fluid

   subroutine deallocate_type_edge_fluid_scalar(structure)

     implicit none

     type (type_edge_fluid_scalar) :: structure

     call deallocate_arr_type_complexgrid_scalar(structure%value)
     call deallocate_arr_type_complexgrid_scalar(structure%bndvalue)
     call deallocate_arr_type_complexgrid_vector(structure%flux)
     call deallocate_arr_type_complexgrid_vector(structure%bndflux)
     call deallocate_arr_type_edge_fluid_scalar_transpcoeff(structure%transpcoeff)
     call deallocate_arr_type_complexgrid_scalar(structure%source)

   end subroutine deallocate_type_edge_fluid_scalar

   subroutine deallocate_arr_type_edge_fluid_scalar(structure)

     implicit none

     type (type_edge_fluid_scalar), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_fluid_scalar(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_fluid_scalar'
     end if

   end subroutine deallocate_arr_type_edge_fluid_scalar

   subroutine deallocate_type_edge_fluid_scalar_simplestruct(structure)

     implicit none

     type (type_edge_fluid_scalar_simplestruct) :: structure

     call deallocate_arr_type_complexgrid_scalar(structure%value)
     call deallocate_arr_type_complexgrid_scalar(structure%bndvalue)
     call deallocate_arr_type_complexgrid_vector(structure%flux)
     call deallocate_arr_type_complexgrid_vector(structure%bndflux)
     call deallocate_arr_type_edge_fluid_scalar_transpcoeff(structure%transpcoeff)
     call deallocate_arr_type_complexgrid_scalar(structure%source)

   end subroutine deallocate_type_edge_fluid_scalar_simplestruct

   subroutine deallocate_arr_type_edge_fluid_scalar_simplestruct(structure)

     implicit none

     type (type_edge_fluid_scalar_simplestruct), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_fluid_scalar_simplestruct(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_fluid_scalar_simplestruct'
     end if

   end subroutine deallocate_arr_type_edge_fluid_scalar_simplestruct

   subroutine deallocate_type_edge_fluid_scalar_transpcoeff(structure)

     implicit none

     type (type_edge_fluid_scalar_transpcoeff) :: structure

     call deallocate_type_complexgrid_vector_simplestruct(structure%d)
     call deallocate_type_complexgrid_vector_simplestruct(structure%v)

   end subroutine deallocate_type_edge_fluid_scalar_transpcoeff

   subroutine deallocate_arr_type_edge_fluid_scalar_transpcoeff(structure)

     implicit none

     type (type_edge_fluid_scalar_transpcoeff), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_fluid_scalar_transpcoeff(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_fluid_scalar_transpcoeff'
     end if

   end subroutine deallocate_arr_type_edge_fluid_scalar_transpcoeff

   subroutine deallocate_type_edge_fluid_vector(structure)

     implicit none

     type (type_edge_fluid_vector) :: structure

     call deallocate_type_vecint_type(structure%align)
     call deallocate_type_vecstring_type(structure%alignid)
     call deallocate_arr_type_edge_fluid_scalar(structure%comps)

   end subroutine deallocate_type_edge_fluid_vector

   subroutine deallocate_arr_type_edge_fluid_vector(structure)

     implicit none

     type (type_edge_fluid_vector), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_fluid_vector(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_fluid_vector'
     end if

   end subroutine deallocate_arr_type_edge_fluid_vector

   subroutine deallocate_type_edge_fluid_vector_simplestruct(structure)

     implicit none

     type (type_edge_fluid_vector_simplestruct) :: structure

     call deallocate_arr_type_edge_fluid_scalar(structure%comps)
     call deallocate_type_vecint_type(structure%align)
     call deallocate_type_vecstring_type(structure%alignid)

   end subroutine deallocate_type_edge_fluid_vector_simplestruct

   subroutine deallocate_arr_type_edge_fluid_vector_simplestruct(structure)

     implicit none

     type (type_edge_fluid_vector_simplestruct), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_fluid_vector_simplestruct(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_fluid_vector_simplestruct'
     end if

   end subroutine deallocate_arr_type_edge_fluid_vector_simplestruct

   subroutine deallocate_type_edge_kinetic(structure)

     implicit none

     type (type_edge_kinetic) :: structure

     call deallocate_arr_type_edge_kinetic_distribution(structure%f)

   end subroutine deallocate_type_edge_kinetic

   subroutine deallocate_arr_type_edge_kinetic(structure)

     implicit none

     type (type_edge_kinetic), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_kinetic(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_kinetic'
     end if

   end subroutine deallocate_arr_type_edge_kinetic

   subroutine deallocate_type_edge_kinetic_distribution(structure)

     implicit none

     type (type_edge_kinetic_distribution) :: structure

     call deallocate_arr_type_complexgrid_scalar(structure%value)
     call deallocate_arr_type_complexgrid_scalar(structure%bndvalue)
     call deallocate_arr_type_complexgrid_vector(structure%fluxes)
     call deallocate_arr_type_complexgrid_scalar(structure%source)

   end subroutine deallocate_type_edge_kinetic_distribution

   subroutine deallocate_arr_type_edge_kinetic_distribution(structure)

     implicit none

     type (type_edge_kinetic_distribution), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edge_kinetic_distribution(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edge_kinetic_distribution'
     end if

   end subroutine deallocate_arr_type_edge_kinetic_distribution

   subroutine deallocate_type_edges(structure)

     implicit none

     type (type_edges) :: structure

     call deallocate_type_rzphi1D(structure%edge_rzphi)

   end subroutine deallocate_type_edges

   subroutine deallocate_arr_type_edges(structure)

     implicit none

     type (type_edges), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edges(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edges'
     end if

   end subroutine deallocate_arr_type_edges

   subroutine deallocate_type_edgespecies(structure)

     implicit none

     type (type_edgespecies) :: structure

     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_edgespecies

   subroutine deallocate_arr_type_edgespecies(structure)

     implicit none

     type (type_edgespecies), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_edgespecies(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_edgespecies'
     end if

   end subroutine deallocate_arr_type_edgespecies

   subroutine deallocate_type_element_desc(structure)

     implicit none

     type (type_element_desc) :: structure

     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_element_desc

   subroutine deallocate_arr_type_element_desc(structure)

     implicit none

     type (type_element_desc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_element_desc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_element_desc'
     end if

   end subroutine deallocate_arr_type_element_desc

   subroutine deallocate_type_entry_def(structure)

     implicit none

     type (type_entry_def) :: structure

     call deallocate_type_vecstring_type(structure%user)
     call deallocate_type_vecstring_type(structure%machine)

   end subroutine deallocate_type_entry_def

   subroutine deallocate_arr_type_entry_def(structure)

     implicit none

     type (type_entry_def), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_entry_def(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_entry_def'
     end if

   end subroutine deallocate_arr_type_entry_def

   subroutine deallocate_type_enum_instance(structure)

     implicit none

     type (type_enum_instance) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_vecstring_type(structure%name)

   end subroutine deallocate_type_enum_instance

   subroutine deallocate_arr_type_enum_instance(structure)

     implicit none

     type (type_enum_instance), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_enum_instance(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_enum_instance'
     end if

   end subroutine deallocate_arr_type_enum_instance

   subroutine deallocate_type_eqconstraint(structure)

     implicit none

     type (type_eqconstraint) :: structure

     call deallocate_type_eqmes1D(structure%bpol)
     call deallocate_type_eqmes0D(structure%bvac_r)
     call deallocate_type_eqmes0D(structure%diamagflux)
     call deallocate_type_eqmes1D(structure%faraday)
     call deallocate_type_eqmes1D(structure%flux)
     call deallocate_type_eqmes0D(structure%i_plasma)
     call deallocate_type_isoflux(structure%isoflux)
     call deallocate_type_eqmes1D(structure%jsurf)
     call deallocate_type_magnet_iron(structure%magnet_iron)
     call deallocate_type_eqmes1D(structure%mse)
     call deallocate_type_eqmes1D(structure%ne)
     call deallocate_type_eqmes1D(structure%pfcurrent)
     call deallocate_type_eqmes1D(structure%pressure)
     call deallocate_type_q(structure%q)
     call deallocate_type_xpts(structure%xpts)

   end subroutine deallocate_type_eqconstraint

   subroutine deallocate_arr_type_eqconstraint(structure)

     implicit none

     type (type_eqconstraint), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_eqconstraint(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_eqconstraint'
     end if

   end subroutine deallocate_arr_type_eqconstraint

   subroutine deallocate_type_eqgeometry(structure)

     implicit none

     type (type_eqgeometry) :: structure

     call deallocate_type_vecstring_type(structure%source)
     call deallocate_arr_type_rz1Dexp(structure%boundary)
     call deallocate_type_rz0D(structure%geom_axis)
     call deallocate_arr_type_rz1Dexp(structure%xpts)
     call deallocate_type_rz0D(structure%left_low_st)
     call deallocate_type_rz0D(structure%right_low_st)
     call deallocate_type_rz0D(structure%left_up_st)
     call deallocate_type_rz0D(structure%right_up_st)
     call deallocate_type_rz0D(structure%active_limit)

   end subroutine deallocate_type_eqgeometry

   subroutine deallocate_arr_type_eqgeometry(structure)

     implicit none

     type (type_eqgeometry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_eqgeometry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_eqgeometry'
     end if

   end subroutine deallocate_arr_type_eqgeometry

   subroutine deallocate_type_eqmes0D(structure)

     implicit none

     type (type_eqmes0D) :: structure

     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_eqmes0D

   subroutine deallocate_arr_type_eqmes0D(structure)

     implicit none

     type (type_eqmes0D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_eqmes0D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_eqmes0D'
     end if

   end subroutine deallocate_arr_type_eqmes0D

   subroutine deallocate_type_eqmes1D(structure)

     implicit none

     type (type_eqmes1D) :: structure

     call deallocate_type_vecflt_type(structure%measured)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecint_type(structure%exact)
     call deallocate_type_vecflt_type(structure%weight)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_vecflt_type(structure%calculated)
     call deallocate_type_vecflt_type(structure%chi2)

   end subroutine deallocate_type_eqmes1D

   subroutine deallocate_arr_type_eqmes1D(structure)

     implicit none

     type (type_eqmes1D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_eqmes1D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_eqmes1D'
     end if

   end subroutine deallocate_arr_type_eqmes1D

   subroutine deallocate_type_equatorial_plane(structure)

     implicit none

     type (type_equatorial_plane) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)
     call deallocate_type_vecflt_type(structure%s)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%b_mod)

   end subroutine deallocate_type_equatorial_plane

   subroutine deallocate_arr_type_equatorial_plane(structure)

     implicit none

     type (type_equatorial_plane), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_equatorial_plane(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_equatorial_plane'
     end if

   end subroutine deallocate_arr_type_equatorial_plane

   subroutine deallocate_type_equilibrium_profiles2d_grid(structure)

     implicit none

     type (type_equilibrium_profiles2d_grid) :: structure

     call deallocate_type_vecflt_type(structure%dim1)
     call deallocate_type_vecflt_type(structure%dim2)
     call deallocate_type_matint_type(structure%connect)

   end subroutine deallocate_type_equilibrium_profiles2d_grid

   subroutine deallocate_arr_type_equilibrium_profiles2d_grid(structure)

     implicit none

     type (type_equilibrium_profiles2d_grid), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_equilibrium_profiles2d_grid(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_equilibrium_profiles2d_grid'
     end if

   end subroutine deallocate_arr_type_equilibrium_profiles2d_grid

   subroutine deallocate_type_equilibrium_profiles_2d(structure)

     implicit none

     type (type_equilibrium_profiles_2d) :: structure

     call deallocate_type_vecstring_type(structure%grid_type)
     call deallocate_type_equilibrium_profiles2d_grid(structure%grid)
     call deallocate_type_matflt_type(structure%r)
     call deallocate_type_matflt_type(structure%z)
     call deallocate_type_matflt_type(structure%psi)
     call deallocate_type_matflt_type(structure%theta)
     call deallocate_type_matflt_type(structure%phi)
     call deallocate_type_matflt_type(structure%jphi)
     call deallocate_type_matflt_type(structure%jpar)
     call deallocate_type_matflt_type(structure%br)
     call deallocate_type_matflt_type(structure%bz)
     call deallocate_type_matflt_type(structure%bphi)
     call deallocate_type_matflt_type(structure%vphi)
     call deallocate_type_matflt_type(structure%vtheta)
     call deallocate_type_matflt_type(structure%rho_mass)
     call deallocate_type_matflt_type(structure%pressure)
     call deallocate_type_matflt_type(structure%temperature)

   end subroutine deallocate_type_equilibrium_profiles_2d

   subroutine deallocate_arr_type_equilibrium_profiles_2d(structure)

     implicit none

     type (type_equilibrium_profiles_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_equilibrium_profiles_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_equilibrium_profiles_2d'
     end if

   end subroutine deallocate_arr_type_equilibrium_profiles_2d

   subroutine deallocate_type_exp0D(structure)

     implicit none

     type (type_exp0D) :: structure


   end subroutine deallocate_type_exp0D

   subroutine deallocate_arr_type_exp0D(structure)

     implicit none

     type (type_exp0D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_exp0D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_exp0D'
     end if

   end subroutine deallocate_arr_type_exp0D

   subroutine deallocate_type_exp1D(structure)

     implicit none

     type (type_exp1D) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%abserror)
     call deallocate_type_vecflt_type(structure%relerror)

   end subroutine deallocate_type_exp1D

   subroutine deallocate_arr_type_exp1D(structure)

     implicit none

     type (type_exp1D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_exp1D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_exp1D'
     end if

   end subroutine deallocate_arr_type_exp1D

   subroutine deallocate_type_exp2D(structure)

     implicit none

     type (type_exp2D) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_matflt_type(structure%abserror)
     call deallocate_type_matflt_type(structure%relerror)

   end subroutine deallocate_type_exp2D

   subroutine deallocate_arr_type_exp2D(structure)

     implicit none

     type (type_exp2D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_exp2D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_exp2D'
     end if

   end subroutine deallocate_arr_type_exp2D

   subroutine deallocate_type_f_expansion(structure)

     implicit none

     type (type_f_expansion) :: structure

     call deallocate_type_complexgrid(structure%grid)
     call deallocate_type_complexgrid_scalar(structure%values)
     call deallocate_type_dist_distrivec_distfunc_fexp_param(structure%parameters)

   end subroutine deallocate_type_f_expansion

   subroutine deallocate_arr_type_f_expansion(structure)

     implicit none

     type (type_f_expansion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_f_expansion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_f_expansion'
     end if

   end subroutine deallocate_arr_type_f_expansion

   subroutine deallocate_type_fast_thermal_separation_filter(structure)

     implicit none

     type (type_fast_thermal_separation_filter) :: structure

     call deallocate_type_identifier(structure%method)
     call deallocate_type_vecflt_type(structure%energy_sep)

   end subroutine deallocate_type_fast_thermal_separation_filter

   subroutine deallocate_arr_type_fast_thermal_separation_filter(structure)

     implicit none

     type (type_fast_thermal_separation_filter), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fast_thermal_separation_filter(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fast_thermal_separation_filter'
     end if

   end subroutine deallocate_arr_type_fast_thermal_separation_filter

   subroutine deallocate_type_filter(structure)

     implicit none

     type (type_filter) :: structure

     call deallocate_type_matflt_type(structure%num)
     call deallocate_type_matflt_type(structure%den)

   end subroutine deallocate_type_filter

   subroutine deallocate_arr_type_filter(structure)

     implicit none

     type (type_filter), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_filter(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_filter'
     end if

   end subroutine deallocate_arr_type_filter

   subroutine deallocate_type_flat_polygon(structure)

     implicit none

     type (type_flat_polygon) :: structure

     call deallocate_type_xyz0D(structure%origin)
     call deallocate_type_xyz0D(structure%basis1)
     call deallocate_type_xyz0D(structure%basis2)
     call deallocate_type_vecflt_type(structure%coord1)
     call deallocate_type_vecflt_type(structure%coord2)

   end subroutine deallocate_type_flat_polygon

   subroutine deallocate_arr_type_flat_polygon(structure)

     implicit none

     type (type_flat_polygon), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_flat_polygon(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_flat_polygon'
     end if

   end subroutine deallocate_arr_type_flat_polygon

   subroutine deallocate_type_flush(structure)

     implicit none

     type (type_flush) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_rz1D(structure%position)
     call deallocate_type_matflt_type(structure%coef)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_flush

   subroutine deallocate_arr_type_flush(structure)

     implicit none

     type (type_flush), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_flush(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_flush'
     end if

   end subroutine deallocate_arr_type_flush

   subroutine deallocate_type_flux_loops(structure)

     implicit none

     type (type_flux_loops) :: structure

     call deallocate_type_setup_floops(structure%setup_floops)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_flux_loops

   subroutine deallocate_arr_type_flux_loops(structure)

     implicit none

     type (type_flux_loops), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_flux_loops(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_flux_loops'
     end if

   end subroutine deallocate_arr_type_flux_loops

   subroutine deallocate_type_fluxel(structure)

     implicit none

     type (type_fluxel) :: structure

     call deallocate_type_vecflt_type(structure%flux_dv)
     call deallocate_type_vecflt_type(structure%flux_interp)

   end subroutine deallocate_type_fluxel

   subroutine deallocate_arr_type_fluxel(structure)

     implicit none

     type (type_fluxel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fluxel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fluxel'
     end if

   end subroutine deallocate_arr_type_fluxel

   subroutine deallocate_type_fluximp(structure)

     implicit none

     type (type_fluximp) :: structure

     call deallocate_type_matflt_type(structure%flux_dv)
     call deallocate_type_matflt_type(structure%flux_interp)

   end subroutine deallocate_type_fluximp

   subroutine deallocate_arr_type_fluximp(structure)

     implicit none

     type (type_fluximp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fluximp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fluximp'
     end if

   end subroutine deallocate_arr_type_fluximp

   subroutine deallocate_type_fluxion(structure)

     implicit none

     type (type_fluxion) :: structure

     call deallocate_type_matflt_type(structure%flux_dv)
     call deallocate_type_matflt_type(structure%flux_interp)

   end subroutine deallocate_type_fluxion

   subroutine deallocate_arr_type_fluxion(structure)

     implicit none

     type (type_fluxion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fluxion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fluxion'
     end if

   end subroutine deallocate_arr_type_fluxion

   subroutine deallocate_type_focussing(structure)

     implicit none

     type (type_focussing) :: structure


   end subroutine deallocate_type_focussing

   subroutine deallocate_arr_type_focussing(structure)

     implicit none

     type (type_focussing), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_focussing(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_focussing'
     end if

   end subroutine deallocate_arr_type_focussing

   subroutine deallocate_type_fullwave(structure)

     implicit none

     type (type_fullwave) :: structure

     call deallocate_type_complexgrid(structure%grid)
     call deallocate_type_e_components(structure%e_components)
     call deallocate_type_pol_decomp(structure%pol_decomp)
     call deallocate_type_local(structure%local)

   end subroutine deallocate_type_fullwave

   subroutine deallocate_arr_type_fullwave(structure)

     implicit none

     type (type_fullwave), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fullwave(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fullwave'
     end if

   end subroutine deallocate_arr_type_fullwave

   subroutine deallocate_type_fusiondiag_colli_3d(structure)

     implicit none

     type (type_fusiondiag_colli_3d) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_arr_type_fusiondiag_voxels(structure%voxels)

   end subroutine deallocate_type_fusiondiag_colli_3d

   subroutine deallocate_arr_type_fusiondiag_colli_3d(structure)

     implicit none

     type (type_fusiondiag_colli_3d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_colli_3d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_colli_3d'
     end if

   end subroutine deallocate_arr_type_fusiondiag_colli_3d

   subroutine deallocate_type_fusiondiag_colli_circ(structure)

     implicit none

     type (type_fusiondiag_colli_circ) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_setup_line(structure%setup_line)
     call deallocate_arr_type_fusiondiag_colliunit_circ(structure%colliunit)

   end subroutine deallocate_type_fusiondiag_colli_circ

   subroutine deallocate_arr_type_fusiondiag_colli_circ(structure)

     implicit none

     type (type_fusiondiag_colli_circ), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_colli_circ(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_colli_circ'
     end if

   end subroutine deallocate_arr_type_fusiondiag_colli_circ

   subroutine deallocate_type_fusiondiag_colli_poly(structure)

     implicit none

     type (type_fusiondiag_colli_poly) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_setup_line(structure%setup_line)
     call deallocate_arr_type_fusiondiag_colliunit_poly(structure%colliunit)

   end subroutine deallocate_type_fusiondiag_colli_poly

   subroutine deallocate_arr_type_fusiondiag_colli_poly(structure)

     implicit none

     type (type_fusiondiag_colli_poly), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_colli_poly(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_colli_poly'
     end if

   end subroutine deallocate_arr_type_fusiondiag_colli_poly

   subroutine deallocate_type_fusiondiag_collimator(structure)

     implicit none

     type (type_fusiondiag_collimator) :: structure

     call deallocate_arr_type_fusiondiag_colli_circ(structure%colli_circ)
     call deallocate_arr_type_fusiondiag_colli_poly(structure%colli_poly)
     call deallocate_arr_type_fusiondiag_colli_3d(structure%colli_3d)

   end subroutine deallocate_type_fusiondiag_collimator

   subroutine deallocate_arr_type_fusiondiag_collimator(structure)

     implicit none

     type (type_fusiondiag_collimator), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_collimator(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_collimator'
     end if

   end subroutine deallocate_arr_type_fusiondiag_collimator

   subroutine deallocate_type_fusiondiag_colliunit_circ(structure)

     implicit none

     type (type_fusiondiag_colliunit_circ) :: structure

     call deallocate_type_vecflt_type(structure%radius)
     call deallocate_type_rzphi1D(structure%centre)

   end subroutine deallocate_type_fusiondiag_colliunit_circ

   subroutine deallocate_arr_type_fusiondiag_colliunit_circ(structure)

     implicit none

     type (type_fusiondiag_colliunit_circ), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_colliunit_circ(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_colliunit_circ'
     end if

   end subroutine deallocate_arr_type_fusiondiag_colliunit_circ

   subroutine deallocate_type_fusiondiag_colliunit_poly(structure)

     implicit none

     type (type_fusiondiag_colliunit_poly) :: structure

     call deallocate_type_rzphi2D(structure%nodes)

   end subroutine deallocate_type_fusiondiag_colliunit_poly

   subroutine deallocate_arr_type_fusiondiag_colliunit_poly(structure)

     implicit none

     type (type_fusiondiag_colliunit_poly), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_colliunit_poly(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_colliunit_poly'
     end if

   end subroutine deallocate_arr_type_fusiondiag_colliunit_poly

   subroutine deallocate_type_fusiondiag_counts(structure)

     implicit none

     type (type_fusiondiag_counts) :: structure

     call deallocate_type_vecstring_type(structure%units)
     call deallocate_arr_type_fusiondiag_ct_chords(structure%ct_chords)
     call deallocate_arr_type_fusiondiag_ct_energy(structure%ct_energy)
     call deallocate_arr_type_fusiondiag_detect_ct_energy(structure%detect_ct)

   end subroutine deallocate_type_fusiondiag_counts

   subroutine deallocate_arr_type_fusiondiag_counts(structure)

     implicit none

     type (type_fusiondiag_counts), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_counts(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_counts'
     end if

   end subroutine deallocate_arr_type_fusiondiag_counts

   subroutine deallocate_type_fusiondiag_ct_chords(structure)

     implicit none

     type (type_fusiondiag_ct_chords) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_exp0D(structure%energy)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_fusiondiag_ct_chords

   subroutine deallocate_arr_type_fusiondiag_ct_chords(structure)

     implicit none

     type (type_fusiondiag_ct_chords), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_ct_chords(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_ct_chords'
     end if

   end subroutine deallocate_arr_type_fusiondiag_ct_chords

   subroutine deallocate_type_fusiondiag_ct_energy(structure)

     implicit none

     type (type_fusiondiag_ct_energy) :: structure

     call deallocate_type_exp1D(structure%energy)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_fusiondiag_ct_energy

   subroutine deallocate_arr_type_fusiondiag_ct_energy(structure)

     implicit none

     type (type_fusiondiag_ct_energy), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_ct_energy(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_ct_energy'
     end if

   end subroutine deallocate_arr_type_fusiondiag_ct_energy

   subroutine deallocate_type_fusiondiag_detect_ct_energy(structure)

     implicit none

     type (type_fusiondiag_detect_ct_energy) :: structure

     call deallocate_type_exp1D(structure%energy)
     call deallocate_type_exp1D(structure%measure)
     call deallocate_type_diag_func(structure%diag_func)

   end subroutine deallocate_type_fusiondiag_detect_ct_energy

   subroutine deallocate_arr_type_fusiondiag_detect_ct_energy(structure)

     implicit none

     type (type_fusiondiag_detect_ct_energy), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_detect_ct_energy(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_detect_ct_energy'
     end if

   end subroutine deallocate_arr_type_fusiondiag_detect_ct_energy

   subroutine deallocate_type_fusiondiag_emissivity1d(structure)

     implicit none

     type (type_fusiondiag_emissivity1d) :: structure

     call deallocate_type_vecstring_type(structure%units)
     call deallocate_type_exp1D(structure%r)
     call deallocate_type_exp1D(structure%z)
     call deallocate_arr_type_fusiondiag_spec1d(structure%spec1d)

   end subroutine deallocate_type_fusiondiag_emissivity1d

   subroutine deallocate_arr_type_fusiondiag_emissivity1d(structure)

     implicit none

     type (type_fusiondiag_emissivity1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_emissivity1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_emissivity1d'
     end if

   end subroutine deallocate_arr_type_fusiondiag_emissivity1d

   subroutine deallocate_type_fusiondiag_emissivity2d(structure)

     implicit none

     type (type_fusiondiag_emissivity2d) :: structure

     call deallocate_type_vecstring_type(structure%units)
     call deallocate_type_exp2D(structure%r)
     call deallocate_type_exp2D(structure%z)
     call deallocate_arr_type_fusiondiag_spec2d(structure%spec2d)

   end subroutine deallocate_type_fusiondiag_emissivity2d

   subroutine deallocate_arr_type_fusiondiag_emissivity2d(structure)

     implicit none

     type (type_fusiondiag_emissivity2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_emissivity2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_emissivity2d'
     end if

   end subroutine deallocate_arr_type_fusiondiag_emissivity2d

   subroutine deallocate_type_fusiondiag_fus_product(structure)

     implicit none

     type (type_fusiondiag_fus_product) :: structure

     call deallocate_type_vecstring_type(structure%product)
     call deallocate_type_vecstring_type(structure%reaction)
     call deallocate_type_fusiondiag_collimator(structure%collimator)
     call deallocate_type_fusiondiag_counts(structure%counts)
     call deallocate_type_fusiondiag_emissivity1d(structure%emissivity1d)
     call deallocate_type_fusiondiag_emissivity2d(structure%emissivity2d)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_fusiondiag_fus_product

   subroutine deallocate_arr_type_fusiondiag_fus_product(structure)

     implicit none

     type (type_fusiondiag_fus_product), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_fus_product(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_fus_product'
     end if

   end subroutine deallocate_arr_type_fusiondiag_fus_product

   subroutine deallocate_type_fusiondiag_spec1d(structure)

     implicit none

     type (type_fusiondiag_spec1d) :: structure

     call deallocate_type_exp0D(structure%energy)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_fusiondiag_spec1d

   subroutine deallocate_arr_type_fusiondiag_spec1d(structure)

     implicit none

     type (type_fusiondiag_spec1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_spec1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_spec1d'
     end if

   end subroutine deallocate_arr_type_fusiondiag_spec1d

   subroutine deallocate_type_fusiondiag_spec2d(structure)

     implicit none

     type (type_fusiondiag_spec2d) :: structure

     call deallocate_type_exp0D(structure%energy)
     call deallocate_type_exp2D(structure%measure)

   end subroutine deallocate_type_fusiondiag_spec2d

   subroutine deallocate_arr_type_fusiondiag_spec2d(structure)

     implicit none

     type (type_fusiondiag_spec2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_spec2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_spec2d'
     end if

   end subroutine deallocate_arr_type_fusiondiag_spec2d

   subroutine deallocate_type_fusiondiag_voxels(structure)

     implicit none

     type (type_fusiondiag_voxels) :: structure

     call deallocate_type_rzphi0D(structure%centre)
     call deallocate_type_rzphi0D(structure%direction)

   end subroutine deallocate_type_fusiondiag_voxels

   subroutine deallocate_arr_type_fusiondiag_voxels(structure)

     implicit none

     type (type_fusiondiag_voxels), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_fusiondiag_voxels(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_fusiondiag_voxels'
     end if

   end subroutine deallocate_arr_type_fusiondiag_voxels

   subroutine deallocate_type_geom(structure)

     implicit none

     type (type_geom) :: structure


   end subroutine deallocate_type_geom

   subroutine deallocate_arr_type_geom(structure)

     implicit none

     type (type_geom), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_geom(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_geom'
     end if

   end subroutine deallocate_arr_type_geom

   subroutine deallocate_type_geom_iron(structure)

     implicit none

     type (type_geom_iron) :: structure

     call deallocate_type_vecint_type(structure%npoints)
     call deallocate_type_rz2D(structure%rzcoordinate)

   end subroutine deallocate_type_geom_iron

   subroutine deallocate_arr_type_geom_iron(structure)

     implicit none

     type (type_geom_iron), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_geom_iron(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_geom_iron'
     end if

   end subroutine deallocate_arr_type_geom_iron

   subroutine deallocate_type_global_param(structure)

     implicit none

     type (type_global_param) :: structure

     call deallocate_type_mag_axis(structure%mag_axis)
     call deallocate_type_b0r0(structure%toroid_field)

   end subroutine deallocate_type_global_param

   subroutine deallocate_arr_type_global_param(structure)

     implicit none

     type (type_global_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_global_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_global_param'
     end if

   end subroutine deallocate_arr_type_global_param

   subroutine deallocate_type_globalparam(structure)

     implicit none

     type (type_globalparam) :: structure

     call deallocate_type_rz0D(structure%geom_axis)

   end subroutine deallocate_type_globalparam

   subroutine deallocate_arr_type_globalparam(structure)

     implicit none

     type (type_globalparam), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_globalparam(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_globalparam'
     end if

   end subroutine deallocate_arr_type_globalparam

   subroutine deallocate_type_halpha_setup(structure)

     implicit none

     type (type_halpha_setup) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_rzphi1D(structure%pivot_point)
     call deallocate_type_vecflt_type(structure%horchordang)
     call deallocate_type_vecflt_type(structure%verchordang)
     call deallocate_type_rzphi1D(structure%second_point)
     call deallocate_type_exp1D(structure%solidangle)

   end subroutine deallocate_type_halpha_setup

   subroutine deallocate_arr_type_halpha_setup(structure)

     implicit none

     type (type_halpha_setup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_halpha_setup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_halpha_setup'
     end if

   end subroutine deallocate_arr_type_halpha_setup

   subroutine deallocate_type_hcll(structure)

     implicit none

     type (type_hcll) :: structure

     call deallocate_type_mat_lim(structure%mat_lim)
     call deallocate_type_hcll_bb(structure%hcll_bb)

   end subroutine deallocate_type_hcll

   subroutine deallocate_arr_type_hcll(structure)

     implicit none

     type (type_hcll), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_hcll(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_hcll'
     end if

   end subroutine deallocate_arr_type_hcll

   subroutine deallocate_type_hcll_bb(structure)

     implicit none

     type (type_hcll_bb) :: structure

     call deallocate_type_react(structure%react)
     call deallocate_type_hcllbb_specs(structure%inboard)
     call deallocate_type_hcllbb_specs(structure%outboard)

   end subroutine deallocate_type_hcll_bb

   subroutine deallocate_arr_type_hcll_bb(structure)

     implicit none

     type (type_hcll_bb), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_hcll_bb(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_hcll_bb'
     end if

   end subroutine deallocate_arr_type_hcll_bb

   subroutine deallocate_type_hcllbb_specs(structure)

     implicit none

     type (type_hcllbb_specs) :: structure

     call deallocate_type_vecflt_type(structure%mass)
     call deallocate_type_vecflt_type(structure%dr)
     call deallocate_type_vecflt_type(structure%mat)
     call deallocate_type_matflt_type(structure%composition)
     call deallocate_type_bb_geometry(structure%mod_geom)
     call deallocate_type_mode_neutr(structure%mod_neutr)
     call deallocate_type_mode_therm(structure%mod_therm)
     call deallocate_type_mode_th_hyd(structure%mod_th_hyd)
     call deallocate_type_mode_mech(structure%mod_mech)
     call deallocate_type_mode_lipb(structure%mod_lipb)
     call deallocate_type_mode_tritium(structure%mod_tritium)

   end subroutine deallocate_type_hcllbb_specs

   subroutine deallocate_arr_type_hcllbb_specs(structure)

     implicit none

     type (type_hcllbb_specs), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_hcllbb_specs(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_hcllbb_specs'
     end if

   end subroutine deallocate_arr_type_hcllbb_specs

   subroutine deallocate_type_holes(structure)

     implicit none

     type (type_holes) :: structure

     call deallocate_type_coordinates(structure%coordinates)
     call deallocate_type_width(structure%width)
     call deallocate_type_vecflt_type(structure%eta)

   end subroutine deallocate_type_holes

   subroutine deallocate_arr_type_holes(structure)

     implicit none

     type (type_holes), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_holes(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_holes'
     end if

   end subroutine deallocate_arr_type_holes

   subroutine deallocate_type_identifier(structure)

     implicit none

     type (type_identifier) :: structure

     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_vecstring_type(structure%description)

   end subroutine deallocate_type_identifier

   subroutine deallocate_arr_type_identifier(structure)

     implicit none

     type (type_identifier), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_identifier(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_identifier'
     end if

   end subroutine deallocate_arr_type_identifier

   subroutine deallocate_type_impcoeff(structure)

     implicit none

     type (type_impcoeff) :: structure

     call deallocate_arr_type_coefficients_neutrals(structure%chargestate)

   end subroutine deallocate_type_impcoeff

   subroutine deallocate_arr_type_impcoeff(structure)

     implicit none

     type (type_impcoeff), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_impcoeff(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_impcoeff'
     end if

   end subroutine deallocate_arr_type_impcoeff

   subroutine deallocate_type_impurities(structure)

     implicit none

     type (type_impurities) :: structure

     call deallocate_type_vecflt_type(structure%zmin)
     call deallocate_type_vecflt_type(structure%zmax)
     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_impurities

   subroutine deallocate_arr_type_impurities(structure)

     implicit none

     type (type_impurities), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_impurities(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_impurities'
     end if

   end subroutine deallocate_arr_type_impurities

   subroutine deallocate_type_impurity_type(structure)

     implicit none

     type (type_impurity_type) :: structure

     call deallocate_type_matflt_type(structure%z)
     call deallocate_type_matflt_type(structure%zsq)
     call deallocate_type_matflt_type(structure%nz)
     call deallocate_type_matflt_type(structure%tz)
     call deallocate_type_sourceimp(structure%source_term)
     call deallocate_type_boundaryimp(structure%boundary)
     call deallocate_type_coretransimp(structure%transp_coef)
     call deallocate_type_fluximp(structure%flux)
     call deallocate_type_matflt_type(structure%time_deriv)
     call deallocate_type_coreimpurediag_type(structure%diagnostic)

   end subroutine deallocate_type_impurity_type

   subroutine deallocate_arr_type_impurity_type(structure)

     implicit none

     type (type_impurity_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_impurity_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_impurity_type'
     end if

   end subroutine deallocate_arr_type_impurity_type

   subroutine deallocate_type_inj_spec(structure)

     implicit none

     type (type_inj_spec) :: structure


   end subroutine deallocate_type_inj_spec

   subroutine deallocate_arr_type_inj_spec(structure)

     implicit none

     type (type_inj_spec), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_inj_spec(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_inj_spec'
     end if

   end subroutine deallocate_arr_type_inj_spec

   subroutine deallocate_type_ions(structure)

     implicit none

     type (type_ions) :: structure

     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_ions

   subroutine deallocate_arr_type_ions(structure)

     implicit none

     type (type_ions), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ions(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ions'
     end if

   end subroutine deallocate_arr_type_ions

   subroutine deallocate_type_isoflux(structure)

     implicit none

     type (type_isoflux) :: structure

     call deallocate_type_rz1D(structure%position)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecflt_type(structure%weight)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_vecflt_type(structure%calculated)
     call deallocate_type_vecflt_type(structure%chi2)

   end subroutine deallocate_type_isoflux

   subroutine deallocate_arr_type_isoflux(structure)

     implicit none

     type (type_isoflux), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_isoflux(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_isoflux'
     end if

   end subroutine deallocate_arr_type_isoflux

   subroutine deallocate_type_jni(structure)

     implicit none

     type (type_jni) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%integral)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_jni

   subroutine deallocate_arr_type_jni(structure)

     implicit none

     type (type_jni), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_jni(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_jni'
     end if

   end subroutine deallocate_arr_type_jni

   subroutine deallocate_type_lang_derived(structure)

     implicit none

     type (type_lang_derived) :: structure

     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_rzphi1Dexp(structure%position)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_lang_derived

   subroutine deallocate_arr_type_lang_derived(structure)

     implicit none

     type (type_lang_derived), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_lang_derived(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_lang_derived'
     end if

   end subroutine deallocate_arr_type_lang_derived

   subroutine deallocate_type_lang_measure(structure)

     implicit none

     type (type_lang_measure) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%direction)
     call deallocate_type_exp1D(structure%area)
     call deallocate_type_rzphi1Dexp(structure%position)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_lang_measure

   subroutine deallocate_arr_type_lang_measure(structure)

     implicit none

     type (type_lang_measure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_lang_measure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_lang_measure'
     end if

   end subroutine deallocate_arr_type_lang_measure

   subroutine deallocate_type_launchangles(structure)

     implicit none

     type (type_launchangles) :: structure


   end subroutine deallocate_type_launchangles

   subroutine deallocate_arr_type_launchangles(structure)

     implicit none

     type (type_launchangles), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchangles(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchangles'
     end if

   end subroutine deallocate_arr_type_launchangles

   subroutine deallocate_type_launchs_parallel(structure)

     implicit none

     type (type_launchs_parallel) :: structure

     call deallocate_type_vecint_type(structure%nn_par)
     call deallocate_type_matflt_type(structure%n_par)
     call deallocate_type_vecflt_type(structure%power)

   end subroutine deallocate_type_launchs_parallel

   subroutine deallocate_arr_type_launchs_parallel(structure)

     implicit none

     type (type_launchs_parallel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchs_parallel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchs_parallel'
     end if

   end subroutine deallocate_arr_type_launchs_parallel

   subroutine deallocate_type_launchs_phi_theta(structure)

     implicit none

     type (type_launchs_phi_theta) :: structure

     call deallocate_type_vecint_type(structure%nn_phi)
     call deallocate_type_vecint_type(structure%nn_theta)
     call deallocate_type_matflt_type(structure%n_phi)
     call deallocate_type_matflt_type(structure%n_theta)
     call deallocate_type_array3dflt_type(structure%power)

   end subroutine deallocate_type_launchs_phi_theta

   subroutine deallocate_arr_type_launchs_phi_theta(structure)

     implicit none

     type (type_launchs_phi_theta), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchs_phi_theta(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchs_phi_theta'
     end if

   end subroutine deallocate_arr_type_launchs_phi_theta

   subroutine deallocate_type_launchs_rfbeam(structure)

     implicit none

     type (type_launchs_rfbeam) :: structure

     call deallocate_type_launchs_rfbeam_spot(structure%spot)
     call deallocate_type_launchs_rfbeam_phaseellipse(structure%phaseellipse)

   end subroutine deallocate_type_launchs_rfbeam

   subroutine deallocate_arr_type_launchs_rfbeam(structure)

     implicit none

     type (type_launchs_rfbeam), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchs_rfbeam(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchs_rfbeam'
     end if

   end subroutine deallocate_arr_type_launchs_rfbeam

   subroutine deallocate_type_launchs_rfbeam_phaseellipse(structure)

     implicit none

     type (type_launchs_rfbeam_phaseellipse) :: structure

     call deallocate_type_matflt_type(structure%invcurvrad)
     call deallocate_type_vecflt_type(structure%angle)

   end subroutine deallocate_type_launchs_rfbeam_phaseellipse

   subroutine deallocate_arr_type_launchs_rfbeam_phaseellipse(structure)

     implicit none

     type (type_launchs_rfbeam_phaseellipse), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchs_rfbeam_phaseellipse(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchs_rfbeam_phaseellipse'
     end if

   end subroutine deallocate_arr_type_launchs_rfbeam_phaseellipse

   subroutine deallocate_type_launchs_rfbeam_spot(structure)

     implicit none

     type (type_launchs_rfbeam_spot) :: structure

     call deallocate_type_matflt_type(structure%waist)
     call deallocate_type_vecflt_type(structure%angle)

   end subroutine deallocate_type_launchs_rfbeam_spot

   subroutine deallocate_arr_type_launchs_rfbeam_spot(structure)

     implicit none

     type (type_launchs_rfbeam_spot), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchs_rfbeam_spot(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchs_rfbeam_spot'
     end if

   end subroutine deallocate_arr_type_launchs_rfbeam_spot

   subroutine deallocate_type_launchsignal(structure)

     implicit none

     type (type_launchsignal) :: structure

     call deallocate_type_vecflt_type(structure%time_launch)
     call deallocate_type_vecflt_type(structure%freq)
     call deallocate_type_vecflt_type(structure%amplitude)
     call deallocate_type_vecflt_type(structure%phase)

   end subroutine deallocate_type_launchsignal

   subroutine deallocate_arr_type_launchsignal(structure)

     implicit none

     type (type_launchsignal), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_launchsignal(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_launchsignal'
     end if

   end subroutine deallocate_arr_type_launchsignal

   subroutine deallocate_type_limiter_unit(structure)

     implicit none

     type (type_limiter_unit) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%closed)
     call deallocate_type_rz1D(structure%position)

   end subroutine deallocate_type_limiter_unit

   subroutine deallocate_arr_type_limiter_unit(structure)

     implicit none

     type (type_limiter_unit), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_limiter_unit(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_limiter_unit'
     end if

   end subroutine deallocate_arr_type_limiter_unit

   subroutine deallocate_type_limits(structure)

     implicit none

     type (type_limits) :: structure


   end subroutine deallocate_type_limits

   subroutine deallocate_arr_type_limits(structure)

     implicit none

     type (type_limits), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_limits(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_limits'
     end if

   end subroutine deallocate_arr_type_limits

   subroutine deallocate_type_lineintegraldiag(structure)

     implicit none

     type (type_lineintegraldiag) :: structure

     call deallocate_type_datainfo(structure%datainfo)
     call deallocate_type_vecstring_type(structure%expression)
     call deallocate_type_setup_line(structure%setup_line)
     call deallocate_type_exp1D(structure%measure)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_lineintegraldiag

   subroutine deallocate_arr_type_lineintegraldiag(structure)

     implicit none

     type (type_lineintegraldiag), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_lineintegraldiag(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_lineintegraldiag'
     end if

   end subroutine deallocate_arr_type_lineintegraldiag

   subroutine deallocate_type_lithmeasure(structure)

     implicit none

     type (type_lithmeasure) :: structure

     call deallocate_type_exp1D(structure%ne)

   end subroutine deallocate_type_lithmeasure

   subroutine deallocate_arr_type_lithmeasure(structure)

     implicit none

     type (type_lithmeasure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_lithmeasure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_lithmeasure'
     end if

   end subroutine deallocate_arr_type_lithmeasure

   subroutine deallocate_type_lithsetup(structure)

     implicit none

     type (type_lithsetup) :: structure

     call deallocate_type_rzphi1D(structure%position)

   end subroutine deallocate_type_lithsetup

   subroutine deallocate_arr_type_lithsetup(structure)

     implicit none

     type (type_lithsetup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_lithsetup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_lithsetup'
     end if

   end subroutine deallocate_arr_type_lithsetup

   subroutine deallocate_type_local(structure)

     implicit none

     type (type_local) :: structure

     call deallocate_type_array3dflt_type(structure%e_plus)
     call deallocate_type_array3dflt_type(structure%e_plus_ph)
     call deallocate_type_array3dflt_type(structure%e_minus)
     call deallocate_type_array3dflt_type(structure%e_minus_ph)
     call deallocate_type_array3dint_type(structure%e_norm)
     call deallocate_type_array3dflt_type(structure%enorm_ph)
     call deallocate_type_array3dflt_type(structure%e_binorm)
     call deallocate_type_array3dflt_type(structure%e_binorm_ph)
     call deallocate_type_array3dflt_type(structure%e_para)
     call deallocate_type_array3dflt_type(structure%e_para_ph)
     call deallocate_type_array3dflt_type(structure%b_norm)
     call deallocate_type_array3dflt_type(structure%b_norm_ph)
     call deallocate_type_array3dflt_type(structure%b_binorm)
     call deallocate_type_array3dflt_type(structure%b_binorm_ph)
     call deallocate_type_array3dflt_type(structure%b_para)
     call deallocate_type_array3dflt_type(structure%b_para_ph)
     call deallocate_type_array3dflt_type(structure%k_perp)

   end subroutine deallocate_type_local

   subroutine deallocate_arr_type_local(structure)

     implicit none

     type (type_local), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_local(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_local'
     end if

   end subroutine deallocate_arr_type_local

   subroutine deallocate_type_mag_axis(structure)

     implicit none

     type (type_mag_axis) :: structure

     call deallocate_type_rz0D(structure%position)

   end subroutine deallocate_type_mag_axis

   subroutine deallocate_arr_type_mag_axis(structure)

     implicit none

     type (type_mag_axis), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mag_axis(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mag_axis'
     end if

   end subroutine deallocate_arr_type_mag_axis

   subroutine deallocate_type_magnet_iron(structure)

     implicit none

     type (type_magnet_iron) :: structure

     call deallocate_type_eqmes1D(structure%mr)
     call deallocate_type_eqmes1D(structure%mz)

   end subroutine deallocate_type_magnet_iron

   subroutine deallocate_arr_type_magnet_iron(structure)

     implicit none

     type (type_magnet_iron), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_magnet_iron(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_magnet_iron'
     end if

   end subroutine deallocate_arr_type_magnet_iron

   subroutine deallocate_type_magnetise(structure)

     implicit none

     type (type_magnetise) :: structure

     call deallocate_type_exp1D(structure%mr)
     call deallocate_type_exp1D(structure%mz)

   end subroutine deallocate_type_magnetise

   subroutine deallocate_arr_type_magnetise(structure)

     implicit none

     type (type_magnetise), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_magnetise(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_magnetise'
     end if

   end subroutine deallocate_arr_type_magnetise

   subroutine deallocate_type_mat_lim(structure)

     implicit none

     type (type_mat_lim) :: structure


   end subroutine deallocate_type_mat_lim

   subroutine deallocate_arr_type_mat_lim(structure)

     implicit none

     type (type_mat_lim), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mat_lim(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mat_lim'
     end if

   end subroutine deallocate_arr_type_mat_lim

   subroutine deallocate_type_mdinfo(structure)

     implicit none

     type (type_mdinfo) :: structure

     call deallocate_type_entry_def(structure%md_entry)

   end subroutine deallocate_type_mdinfo

   subroutine deallocate_arr_type_mdinfo(structure)

     implicit none

     type (type_mdinfo), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mdinfo(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mdinfo'
     end if

   end subroutine deallocate_arr_type_mdinfo

   subroutine deallocate_type_mhd_ideal_wall2d(structure)

     implicit none

     type (type_mhd_ideal_wall2d) :: structure

     call deallocate_type_identifier(structure%walltype)
     call deallocate_type_rz1D(structure%position)

   end subroutine deallocate_type_mhd_ideal_wall2d

   subroutine deallocate_arr_type_mhd_ideal_wall2d(structure)

     implicit none

     type (type_mhd_ideal_wall2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd_ideal_wall2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd_ideal_wall2d'
     end if

   end subroutine deallocate_arr_type_mhd_ideal_wall2d

   subroutine deallocate_type_mhd_mode(structure)

     implicit none

     type (type_mhd_mode) :: structure

     call deallocate_type_mhd_plasma(structure%plasma)
     call deallocate_type_mhd_vacuum(structure%vacuum)

   end subroutine deallocate_type_mhd_mode

   subroutine deallocate_arr_type_mhd_mode(structure)

     implicit none

     type (type_mhd_mode), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd_mode(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd_mode'
     end if

   end subroutine deallocate_arr_type_mhd_mode

   subroutine deallocate_type_mhd_plasma(structure)

     implicit none

     type (type_mhd_plasma) :: structure

     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_matflt_type(structure%m)
     call deallocate_type_matcplx_type(structure%disp_perp)
     call deallocate_type_matcplx_type(structure%disp_par)
     call deallocate_type_vecflt_type(structure%tau_alfven)
     call deallocate_type_vecflt_type(structure%tau_res)
     call deallocate_type_coord_sys(structure%coord_sys)
     call deallocate_type_mhd_vector(structure%a_pert)
     call deallocate_type_mhd_vector(structure%b_pert)
     call deallocate_type_mhd_vector(structure%v_pert)
     call deallocate_type_matcplx_type(structure%p_pert)
     call deallocate_type_matcplx_type(structure%rho_mass_per)
     call deallocate_type_matcplx_type(structure%temp_per)

   end subroutine deallocate_type_mhd_plasma

   subroutine deallocate_arr_type_mhd_plasma(structure)

     implicit none

     type (type_mhd_plasma), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd_plasma(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd_plasma'
     end if

   end subroutine deallocate_arr_type_mhd_plasma

   subroutine deallocate_type_mhd_res_wall2d(structure)

     implicit none

     type (type_mhd_res_wall2d) :: structure

     call deallocate_type_identifier(structure%walltype)
     call deallocate_type_rz1D(structure%position)
     call deallocate_type_holes(structure%holes)

   end subroutine deallocate_type_mhd_res_wall2d

   subroutine deallocate_arr_type_mhd_res_wall2d(structure)

     implicit none

     type (type_mhd_res_wall2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd_res_wall2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd_res_wall2d'
     end if

   end subroutine deallocate_arr_type_mhd_res_wall2d

   subroutine deallocate_type_mhd_vacuum(structure)

     implicit none

     type (type_mhd_vacuum) :: structure

     call deallocate_type_array3dflt_type(structure%m)
     call deallocate_type_coord_sys(structure%coord_sys)
     call deallocate_type_mhd_vector(structure%a_pert)
     call deallocate_type_mhd_vector(structure%b_pert)

   end subroutine deallocate_type_mhd_vacuum

   subroutine deallocate_arr_type_mhd_vacuum(structure)

     implicit none

     type (type_mhd_vacuum), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd_vacuum(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd_vacuum'
     end if

   end subroutine deallocate_arr_type_mhd_vacuum

   subroutine deallocate_type_mhd_vector(structure)

     implicit none

     type (type_mhd_vector) :: structure

     call deallocate_type_matcplx_type(structure%coord1)
     call deallocate_type_matcplx_type(structure%coord2)
     call deallocate_type_matcplx_type(structure%coord3)

   end subroutine deallocate_type_mhd_vector

   subroutine deallocate_arr_type_mhd_vector(structure)

     implicit none

     type (type_mhd_vector), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mhd_vector(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mhd_vector'
     end if

   end subroutine deallocate_arr_type_mhd_vector

   subroutine deallocate_type_mode_lipb(structure)

     implicit none

     type (type_mode_lipb) :: structure

     call deallocate_type_vecflt_type(structure%bb_lp_fr)

   end subroutine deallocate_type_mode_lipb

   subroutine deallocate_arr_type_mode_lipb(structure)

     implicit none

     type (type_mode_lipb), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mode_lipb(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mode_lipb'
     end if

   end subroutine deallocate_arr_type_mode_lipb

   subroutine deallocate_type_mode_mech(structure)

     implicit none

     type (type_mode_mech) :: structure


   end subroutine deallocate_type_mode_mech

   subroutine deallocate_arr_type_mode_mech(structure)

     implicit none

     type (type_mode_mech), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mode_mech(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mode_mech'
     end if

   end subroutine deallocate_arr_type_mode_mech

   subroutine deallocate_type_mode_neutr(structure)

     implicit none

     type (type_mode_neutr) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%pd_rad)
     call deallocate_type_vecflt_type(structure%lipb_coef_pd)
     call deallocate_type_vecflt_type(structure%steel_coef_pd)
     call deallocate_type_power_exchange(structure%pow_exchange)

   end subroutine deallocate_type_mode_neutr

   subroutine deallocate_arr_type_mode_neutr(structure)

     implicit none

     type (type_mode_neutr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mode_neutr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mode_neutr'
     end if

   end subroutine deallocate_arr_type_mode_neutr

   subroutine deallocate_type_mode_th_hyd(structure)

     implicit none

     type (type_mode_th_hyd) :: structure


   end subroutine deallocate_type_mode_th_hyd

   subroutine deallocate_arr_type_mode_th_hyd(structure)

     implicit none

     type (type_mode_th_hyd), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mode_th_hyd(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mode_th_hyd'
     end if

   end subroutine deallocate_arr_type_mode_th_hyd

   subroutine deallocate_type_mode_therm(structure)

     implicit none

     type (type_mode_therm) :: structure


   end subroutine deallocate_type_mode_therm

   subroutine deallocate_arr_type_mode_therm(structure)

     implicit none

     type (type_mode_therm), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mode_therm(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mode_therm'
     end if

   end subroutine deallocate_arr_type_mode_therm

   subroutine deallocate_type_mode_tritium(structure)

     implicit none

     type (type_mode_tritium) :: structure


   end subroutine deallocate_type_mode_tritium

   subroutine deallocate_arr_type_mode_tritium(structure)

     implicit none

     type (type_mode_tritium), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_mode_tritium(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_mode_tritium'
     end if

   end subroutine deallocate_arr_type_mode_tritium

   subroutine deallocate_type_modules(structure)

     implicit none

     type (type_modules) :: structure

     call deallocate_type_vecint_type(structure%ima_theta)
     call deallocate_type_vecint_type(structure%ima_phi)
     call deallocate_type_exp1D(structure%amplitude)
     call deallocate_type_exp1D(structure%phase)
     call deallocate_type_waveguides(structure%waveguides)

   end subroutine deallocate_type_modules

   subroutine deallocate_arr_type_modules(structure)

     implicit none

     type (type_modules), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_modules(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_modules'
     end if

   end subroutine deallocate_arr_type_modules

   subroutine deallocate_type_msediag_emiss_chord(structure)

     implicit none

     type (type_msediag_emiss_chord) :: structure

     call deallocate_type_rzphi1D(structure%setup)
     call deallocate_arr_type_msediag_polarization(structure%polarization)
     call deallocate_type_vecflt_type(structure%quantiaxis)

   end subroutine deallocate_type_msediag_emiss_chord

   subroutine deallocate_arr_type_msediag_emiss_chord(structure)

     implicit none

     type (type_msediag_emiss_chord), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_emiss_chord(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_emiss_chord'
     end if

   end subroutine deallocate_arr_type_msediag_emiss_chord

   subroutine deallocate_type_msediag_emissivity(structure)

     implicit none

     type (type_msediag_emissivity) :: structure

     call deallocate_type_vecflt_type(structure%wavelength)
     call deallocate_arr_type_msediag_emiss_chord(structure%emiss_chord)

   end subroutine deallocate_type_msediag_emissivity

   subroutine deallocate_arr_type_msediag_emissivity(structure)

     implicit none

     type (type_msediag_emissivity), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_emissivity(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_emissivity'
     end if

   end subroutine deallocate_arr_type_msediag_emissivity

   subroutine deallocate_type_msediag_polarization(structure)

     implicit none

     type (type_msediag_polarization) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_matflt_type(structure%spec_emiss)

   end subroutine deallocate_type_msediag_polarization

   subroutine deallocate_arr_type_msediag_polarization(structure)

     implicit none

     type (type_msediag_polarization), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_polarization(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_polarization'
     end if

   end subroutine deallocate_arr_type_msediag_polarization

   subroutine deallocate_type_msediag_radia_chord(structure)

     implicit none

     type (type_msediag_radia_chord) :: structure

     call deallocate_type_msediag_setup(structure%setup)
     call deallocate_arr_type_msediag_stokes(structure%stokes)
     call deallocate_type_exp1D(structure%totradiance)

   end subroutine deallocate_type_msediag_radia_chord

   subroutine deallocate_arr_type_msediag_radia_chord(structure)

     implicit none

     type (type_msediag_radia_chord), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_radia_chord(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_radia_chord'
     end if

   end subroutine deallocate_arr_type_msediag_radia_chord

   subroutine deallocate_type_msediag_radiance(structure)

     implicit none

     type (type_msediag_radiance) :: structure

     call deallocate_type_exp1D(structure%wavelength)
     call deallocate_arr_type_msediag_radia_chord(structure%radia_chord)

   end subroutine deallocate_type_msediag_radiance

   subroutine deallocate_arr_type_msediag_radiance(structure)

     implicit none

     type (type_msediag_radiance), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_radiance(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_radiance'
     end if

   end subroutine deallocate_arr_type_msediag_radiance

   subroutine deallocate_type_msediag_setup(structure)

     implicit none

     type (type_msediag_setup) :: structure

     call deallocate_type_rzphi0D(structure%pivot_point)
     call deallocate_type_rzphi0D(structure%second_point)

   end subroutine deallocate_type_msediag_setup

   subroutine deallocate_arr_type_msediag_setup(structure)

     implicit none

     type (type_msediag_setup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_setup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_setup'
     end if

   end subroutine deallocate_arr_type_msediag_setup

   subroutine deallocate_type_msediag_setup_polarimetry(structure)

     implicit none

     type (type_msediag_setup_polarimetry) :: structure

     call deallocate_type_rzphidrdzdphi1D(structure%rzgamma)
     call deallocate_type_matflt_type(structure%geom_coef)

   end subroutine deallocate_type_msediag_setup_polarimetry

   subroutine deallocate_arr_type_msediag_setup_polarimetry(structure)

     implicit none

     type (type_msediag_setup_polarimetry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_setup_polarimetry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_setup_polarimetry'
     end if

   end subroutine deallocate_arr_type_msediag_setup_polarimetry

   subroutine deallocate_type_msediag_stokes(structure)

     implicit none

     type (type_msediag_stokes) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_matflt_type(structure%vector)

   end subroutine deallocate_type_msediag_stokes

   subroutine deallocate_arr_type_msediag_stokes(structure)

     implicit none

     type (type_msediag_stokes), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_msediag_stokes(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_msediag_stokes'
     end if

   end subroutine deallocate_arr_type_msediag_stokes

   subroutine deallocate_type_nbi_nbi_unit_wall(structure)

     implicit none

     type (type_nbi_nbi_unit_wall) :: structure

     call deallocate_type_nbi_nbi_unit_wall_surface(structure%surface)
     call deallocate_arr_type_flat_polygon(structure%collimator)

   end subroutine deallocate_type_nbi_nbi_unit_wall

   subroutine deallocate_arr_type_nbi_nbi_unit_wall(structure)

     implicit none

     type (type_nbi_nbi_unit_wall), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_nbi_nbi_unit_wall(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_nbi_nbi_unit_wall'
     end if

   end subroutine deallocate_arr_type_nbi_nbi_unit_wall

   subroutine deallocate_type_nbi_nbi_unit_wall_surface(structure)

     implicit none

     type (type_nbi_nbi_unit_wall_surface) :: structure

     call deallocate_arr_type_trianglexyz(structure%triangle)
     call deallocate_arr_type_rectanglexyz(structure%rectangle)

   end subroutine deallocate_type_nbi_nbi_unit_wall_surface

   subroutine deallocate_arr_type_nbi_nbi_unit_wall_surface(structure)

     implicit none

     type (type_nbi_nbi_unit_wall_surface), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_nbi_nbi_unit_wall_surface(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_nbi_nbi_unit_wall_surface'
     end if

   end subroutine deallocate_arr_type_nbi_nbi_unit_wall_surface

   subroutine deallocate_type_nbi_unit(structure)

     implicit none

     type (type_nbi_unit) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_inj_spec(structure%inj_spec)
     call deallocate_type_exp0D(structure%pow_unit)
     call deallocate_type_exp0D(structure%inj_eng_unit)
     call deallocate_type_exp1D(structure%beamcurrfrac)
     call deallocate_type_exp1D(structure%beampowrfrac)
     call deallocate_arr_type_beamletgroup(structure%beamletgroup)
     call deallocate_type_nbi_nbi_unit_wall(structure%wall)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_nbi_unit

   subroutine deallocate_arr_type_nbi_unit(structure)

     implicit none

     type (type_nbi_unit), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_nbi_unit(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_nbi_unit'
     end if

   end subroutine deallocate_arr_type_nbi_unit

   subroutine deallocate_type_ne_transp(structure)

     implicit none

     type (type_ne_transp) :: structure

     call deallocate_type_matflt_type(structure%diff_eff)
     call deallocate_type_matflt_type(structure%vconv_eff)
     call deallocate_type_vecflt_type(structure%flux)
     call deallocate_type_offdiagel(structure%off_diagonal)

   end subroutine deallocate_type_ne_transp

   subroutine deallocate_arr_type_ne_transp(structure)

     implicit none

     type (type_ne_transp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ne_transp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ne_transp'
     end if

   end subroutine deallocate_arr_type_ne_transp

   subroutine deallocate_type_neoclassic_impurity(structure)

     implicit none

     type (type_neoclassic_impurity) :: structure

     call deallocate_type_matflt_type(structure%utheta_z)

   end subroutine deallocate_type_neoclassic_impurity

   subroutine deallocate_arr_type_neoclassic_impurity(structure)

     implicit none

     type (type_neoclassic_impurity), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_neoclassic_impurity(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_neoclassic_impurity'
     end if

   end subroutine deallocate_arr_type_neoclassic_impurity

   subroutine deallocate_type_neut_results(structure)

     implicit none

     type (type_neut_results) :: structure


   end subroutine deallocate_type_neut_results

   subroutine deallocate_arr_type_neut_results(structure)

     implicit none

     type (type_neut_results), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_neut_results(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_neut_results'
     end if

   end subroutine deallocate_arr_type_neut_results

   subroutine deallocate_type_neutral_complex_type(structure)

     implicit none

     type (type_neutral_complex_type) :: structure

     call deallocate_arr_type_coreneutrals_neutraltype(structure%neutraltype)
     call deallocate_type_vecflt_type(structure%prad0)

   end subroutine deallocate_type_neutral_complex_type

   subroutine deallocate_arr_type_neutral_complex_type(structure)

     implicit none

     type (type_neutral_complex_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_neutral_complex_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_neutral_complex_type'
     end if

   end subroutine deallocate_arr_type_neutral_complex_type

   subroutine deallocate_type_neutro_resul(structure)

     implicit none

     type (type_neutro_resul) :: structure

     call deallocate_type_vecflt_type(structure%nwl_pol_prof)

   end subroutine deallocate_type_neutro_resul

   subroutine deallocate_arr_type_neutro_resul(structure)

     implicit none

     type (type_neutro_resul), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_neutro_resul(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_neutro_resul'
     end if

   end subroutine deallocate_arr_type_neutro_resul

   subroutine deallocate_type_ni_transp(structure)

     implicit none

     type (type_ni_transp) :: structure

     call deallocate_type_array3dflt_type(structure%diff_eff)
     call deallocate_type_array3dflt_type(structure%vconv_eff)
     call deallocate_type_matflt_type(structure%flux)
     call deallocate_type_offdiagion(structure%off_diagonal)

   end subroutine deallocate_type_ni_transp

   subroutine deallocate_arr_type_ni_transp(structure)

     implicit none

     type (type_ni_transp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ni_transp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ni_transp'
     end if

   end subroutine deallocate_arr_type_ni_transp

   subroutine deallocate_type_ntm_mode(structure)

     implicit none

     type (type_ntm_mode) :: structure

     call deallocate_type_ntm_mode_onset(structure%onset)
     call deallocate_type_ntm_mode_full_evol(structure%full_evol)
     call deallocate_type_ntm_mode_evolution(structure%evolution)

   end subroutine deallocate_type_ntm_mode

   subroutine deallocate_arr_type_ntm_mode(structure)

     implicit none

     type (type_ntm_mode), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm_mode(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm_mode'
     end if

   end subroutine deallocate_arr_type_ntm_mode

   subroutine deallocate_type_ntm_mode_evolution(structure)

     implicit none

     type (type_ntm_mode_evolution) :: structure

     call deallocate_type_ntm_mode_evolution_island(structure%island)
     call deallocate_type_vecflt_type(structure%deltaw_value)
     call deallocate_type_vecstring_type(structure%deltaw_name)
     call deallocate_type_vecflt_type(structure%torque_value)
     call deallocate_type_vecstring_type(structure%torque_name)
     call deallocate_type_vecflt_type(structure%delta_diff)
     call deallocate_type_vecstring_type(structure%description)

   end subroutine deallocate_type_ntm_mode_evolution

   subroutine deallocate_arr_type_ntm_mode_evolution(structure)

     implicit none

     type (type_ntm_mode_evolution), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm_mode_evolution(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm_mode_evolution'
     end if

   end subroutine deallocate_arr_type_ntm_mode_evolution

   subroutine deallocate_type_ntm_mode_evolution_island(structure)

     implicit none

     type (type_ntm_mode_evolution_island) :: structure

     call deallocate_type_vecflt_type(structure%geometry)
     call deallocate_type_vecflt_type(structure%coord_values)
     call deallocate_type_vecstring_type(structure%coord_desc)

   end subroutine deallocate_type_ntm_mode_evolution_island

   subroutine deallocate_arr_type_ntm_mode_evolution_island(structure)

     implicit none

     type (type_ntm_mode_evolution_island), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm_mode_evolution_island(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm_mode_evolution_island'
     end if

   end subroutine deallocate_arr_type_ntm_mode_evolution_island

   subroutine deallocate_type_ntm_mode_full_evol(structure)

     implicit none

     type (type_ntm_mode_full_evol) :: structure

     call deallocate_type_vecflt_type(structure%time_evol)
     call deallocate_type_vecflt_type(structure%w)
     call deallocate_type_vecflt_type(structure%dwdt)
     call deallocate_type_vecflt_type(structure%phase)
     call deallocate_type_vecflt_type(structure%dphasedt)
     call deallocate_type_vecflt_type(structure%frequency)
     call deallocate_type_vecflt_type(structure%dfrequencydt)
     call deallocate_type_ntm_mode_full_evol_island(structure%island)
     call deallocate_type_matflt_type(structure%deltaw_value)
     call deallocate_type_vecstring_type(structure%deltaw_name)
     call deallocate_type_matflt_type(structure%torque_value)
     call deallocate_type_vecstring_type(structure%torque_name)
     call deallocate_type_matflt_type(structure%delta_diff)
     call deallocate_type_vecstring_type(structure%description)
     call deallocate_type_vecflt_type(structure%rho_tor)

   end subroutine deallocate_type_ntm_mode_full_evol

   subroutine deallocate_arr_type_ntm_mode_full_evol(structure)

     implicit none

     type (type_ntm_mode_full_evol), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm_mode_full_evol(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm_mode_full_evol'
     end if

   end subroutine deallocate_arr_type_ntm_mode_full_evol

   subroutine deallocate_type_ntm_mode_full_evol_island(structure)

     implicit none

     type (type_ntm_mode_full_evol_island) :: structure

     call deallocate_type_matflt_type(structure%geometry)
     call deallocate_type_matflt_type(structure%coord_values)
     call deallocate_type_vecstring_type(structure%coord_desc)

   end subroutine deallocate_type_ntm_mode_full_evol_island

   subroutine deallocate_arr_type_ntm_mode_full_evol_island(structure)

     implicit none

     type (type_ntm_mode_full_evol_island), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm_mode_full_evol_island(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm_mode_full_evol_island'
     end if

   end subroutine deallocate_arr_type_ntm_mode_full_evol_island

   subroutine deallocate_type_ntm_mode_onset(structure)

     implicit none

     type (type_ntm_mode_onset) :: structure

     call deallocate_type_vecstring_type(structure%description)

   end subroutine deallocate_type_ntm_mode_onset

   subroutine deallocate_arr_type_ntm_mode_onset(structure)

     implicit none

     type (type_ntm_mode_onset), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_ntm_mode_onset(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_ntm_mode_onset'
     end if

   end subroutine deallocate_arr_type_ntm_mode_onset

   subroutine deallocate_type_nuclei(structure)

     implicit none

     type (type_nuclei) :: structure

     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_nuclei

   subroutine deallocate_arr_type_nuclei(structure)

     implicit none

     type (type_nuclei), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_nuclei(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_nuclei'
     end if

   end subroutine deallocate_arr_type_nuclei

   subroutine deallocate_type_objects(structure)

     implicit none

     type (type_objects) :: structure

     call deallocate_type_matint_type(structure%boundary)
     call deallocate_type_array3dint_type(structure%neighbour)
     call deallocate_type_array4dflt_type(structure%geo)
     call deallocate_type_matflt_type(structure%measure)

   end subroutine deallocate_type_objects

   subroutine deallocate_arr_type_objects(structure)

     implicit none

     type (type_objects), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_objects(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_objects'
     end if

   end subroutine deallocate_arr_type_objects

   subroutine deallocate_type_offdiagel(structure)

     implicit none

     type (type_offdiagel) :: structure

     call deallocate_type_matflt_type(structure%d_ni)
     call deallocate_type_matflt_type(structure%d_ti)
     call deallocate_type_vecflt_type(structure%d_ne)
     call deallocate_type_vecflt_type(structure%d_te)
     call deallocate_type_vecflt_type(structure%d_epar)
     call deallocate_type_vecflt_type(structure%d_mtor)

   end subroutine deallocate_type_offdiagel

   subroutine deallocate_arr_type_offdiagel(structure)

     implicit none

     type (type_offdiagel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_offdiagel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_offdiagel'
     end if

   end subroutine deallocate_arr_type_offdiagel

   subroutine deallocate_type_offdiagion(structure)

     implicit none

     type (type_offdiagion) :: structure

     call deallocate_type_array3dflt_type(structure%d_ni)
     call deallocate_type_array3dflt_type(structure%d_ti)
     call deallocate_type_matflt_type(structure%d_ne)
     call deallocate_type_matflt_type(structure%d_te)
     call deallocate_type_matflt_type(structure%d_epar)
     call deallocate_type_matflt_type(structure%d_mtor)

   end subroutine deallocate_type_offdiagion

   subroutine deallocate_arr_type_offdiagion(structure)

     implicit none

     type (type_offdiagion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_offdiagion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_offdiagion'
     end if

   end subroutine deallocate_arr_type_offdiagion

   subroutine deallocate_type_omnigen_surf(structure)

     implicit none

     type (type_omnigen_surf) :: structure

     call deallocate_type_rz1D(structure%rz)
     call deallocate_type_vecflt_type(structure%s)

   end subroutine deallocate_type_omnigen_surf

   subroutine deallocate_arr_type_omnigen_surf(structure)

     implicit none

     type (type_omnigen_surf), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_omnigen_surf(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_omnigen_surf'
     end if

   end subroutine deallocate_arr_type_omnigen_surf

   subroutine deallocate_type_orbit_global_param(structure)

     implicit none

     type (type_orbit_global_param) :: structure

     call deallocate_type_vecint_type(structure%orbit_type)
     call deallocate_type_vecflt_type(structure%omega_b)
     call deallocate_type_vecflt_type(structure%omega_phi)
     call deallocate_type_vecflt_type(structure%omega_c_av)
     call deallocate_type_orbit_special_pos(structure%special_pos)

   end subroutine deallocate_type_orbit_global_param

   subroutine deallocate_arr_type_orbit_global_param(structure)

     implicit none

     type (type_orbit_global_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_orbit_global_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_orbit_global_param'
     end if

   end subroutine deallocate_arr_type_orbit_global_param

   subroutine deallocate_type_orbit_midplane(structure)

     implicit none

     type (type_orbit_midplane) :: structure

     call deallocate_type_orbit_pos(structure%outer)
     call deallocate_type_orbit_pos(structure%inner)

   end subroutine deallocate_type_orbit_midplane

   subroutine deallocate_arr_type_orbit_midplane(structure)

     implicit none

     type (type_orbit_midplane), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_orbit_midplane(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_orbit_midplane'
     end if

   end subroutine deallocate_arr_type_orbit_midplane

   subroutine deallocate_type_orbit_pos(structure)

     implicit none

     type (type_orbit_pos) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%theta_b)

   end subroutine deallocate_type_orbit_pos

   subroutine deallocate_arr_type_orbit_pos(structure)

     implicit none

     type (type_orbit_pos), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_orbit_pos(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_orbit_pos'
     end if

   end subroutine deallocate_arr_type_orbit_pos

   subroutine deallocate_type_orbit_special_pos(structure)

     implicit none

     type (type_orbit_special_pos) :: structure

     call deallocate_type_orbit_midplane(structure%midplane)
     call deallocate_type_orbit_turning_pts(structure%turning_pts)

   end subroutine deallocate_type_orbit_special_pos

   subroutine deallocate_arr_type_orbit_special_pos(structure)

     implicit none

     type (type_orbit_special_pos), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_orbit_special_pos(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_orbit_special_pos'
     end if

   end subroutine deallocate_arr_type_orbit_special_pos

   subroutine deallocate_type_orbit_turning_pts(structure)

     implicit none

     type (type_orbit_turning_pts) :: structure

     call deallocate_type_orbit_pos(structure%upper)
     call deallocate_type_orbit_pos(structure%lower)

   end subroutine deallocate_type_orbit_turning_pts

   subroutine deallocate_arr_type_orbit_turning_pts(structure)

     implicit none

     type (type_orbit_turning_pts), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_orbit_turning_pts(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_orbit_turning_pts'
     end if

   end subroutine deallocate_arr_type_orbit_turning_pts

   subroutine deallocate_type_origin(structure)

     implicit none

     type (type_origin) :: structure

     call deallocate_type_rzphi0D(structure%refpos)

   end subroutine deallocate_type_origin

   subroutine deallocate_arr_type_origin(structure)

     implicit none

     type (type_origin), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_origin(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_origin'
     end if

   end subroutine deallocate_arr_type_origin

   subroutine deallocate_type_param(structure)

     implicit none

     type (type_param) :: structure

     call deallocate_type_vecstring_type(structure%parameters)
     call deallocate_type_vecstring_type(structure%default_param)
     call deallocate_type_vecstring_type(structure%schema)

   end subroutine deallocate_type_param

   subroutine deallocate_arr_type_param(structure)

     implicit none

     type (type_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_param'
     end if

   end subroutine deallocate_arr_type_param

   subroutine deallocate_type_parameters(structure)

     implicit none

     type (type_parameters) :: structure

     call deallocate_type_equatorial_plane(structure%equatorial)

   end subroutine deallocate_type_parameters

   subroutine deallocate_arr_type_parameters(structure)

     implicit none

     type (type_parameters), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_parameters(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_parameters'
     end if

   end subroutine deallocate_arr_type_parameters

   subroutine deallocate_type_pellet(structure)

     implicit none

     type (type_pellet) :: structure

     call deallocate_type_pellet_shape(structure%shape)
     call deallocate_type_pellet_elements(structure%elements)
     call deallocate_type_pellet_geometry(structure%geometry)
     call deallocate_type_pellet_pathprofiles(structure%pathprofiles)
     call deallocate_type_pellet_deposition(structure%deposition)

   end subroutine deallocate_type_pellet

   subroutine deallocate_arr_type_pellet(structure)

     implicit none

     type (type_pellet), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet'
     end if

   end subroutine deallocate_arr_type_pellet

   subroutine deallocate_type_pellet_angles(structure)

     implicit none

     type (type_pellet_angles) :: structure


   end subroutine deallocate_type_pellet_angles

   subroutine deallocate_arr_type_pellet_angles(structure)

     implicit none

     type (type_pellet_angles), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_angles(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_angles'
     end if

   end subroutine deallocate_arr_type_pellet_angles

   subroutine deallocate_type_pellet_deposition(structure)

     implicit none

     type (type_pellet_deposition) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_pol)
     call deallocate_type_vecflt_type(structure%delta_ne)
     call deallocate_type_vecflt_type(structure%delta_te)
     call deallocate_type_matflt_type(structure%delta_ni)
     call deallocate_type_matflt_type(structure%delta_ti)
     call deallocate_type_matflt_type(structure%delta_vtor)
     call deallocate_arr_type_pellet_impurity(structure%impurity)

   end subroutine deallocate_type_pellet_deposition

   subroutine deallocate_arr_type_pellet_deposition(structure)

     implicit none

     type (type_pellet_deposition), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_deposition(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_deposition'
     end if

   end subroutine deallocate_arr_type_pellet_deposition

   subroutine deallocate_type_pellet_elements(structure)

     implicit none

     type (type_pellet_elements) :: structure

     call deallocate_type_vecint_type(structure%nucindex)
     call deallocate_type_vecflt_type(structure%density)
     call deallocate_type_vecflt_type(structure%fraction)
     call deallocate_type_vecflt_type(structure%subl_energy)

   end subroutine deallocate_type_pellet_elements

   subroutine deallocate_arr_type_pellet_elements(structure)

     implicit none

     type (type_pellet_elements), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_elements(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_elements'
     end if

   end subroutine deallocate_arr_type_pellet_elements

   subroutine deallocate_type_pellet_geometry(structure)

     implicit none

     type (type_pellet_geometry) :: structure

     call deallocate_type_rzphi0D(structure%pivot_point)
     call deallocate_type_rzphi0D(structure%second_point)
     call deallocate_type_pellet_angles(structure%angles)

   end subroutine deallocate_type_pellet_geometry

   subroutine deallocate_arr_type_pellet_geometry(structure)

     implicit none

     type (type_pellet_geometry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_geometry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_geometry'
     end if

   end subroutine deallocate_arr_type_pellet_geometry

   subroutine deallocate_type_pellet_impurity(structure)

     implicit none

     type (type_pellet_impurity) :: structure

     call deallocate_type_matflt_type(structure%delta_nz)

   end subroutine deallocate_type_pellet_impurity

   subroutine deallocate_arr_type_pellet_impurity(structure)

     implicit none

     type (type_pellet_impurity), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_impurity(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_impurity'
     end if

   end subroutine deallocate_arr_type_pellet_impurity

   subroutine deallocate_type_pellet_pathprofiles(structure)

     implicit none

     type (type_pellet_pathprofiles) :: structure

     call deallocate_type_vecflt_type(structure%distance)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_pol)
     call deallocate_type_vecflt_type(structure%velocity)
     call deallocate_type_vecflt_type(structure%ne)
     call deallocate_type_vecflt_type(structure%te)
     call deallocate_type_vecflt_type(structure%abl_rate)
     call deallocate_type_vecflt_type(structure%abl_particles)
     call deallocate_type_vecflt_type(structure%delta_drift)
     call deallocate_type_rzphi1D(structure%position)

   end subroutine deallocate_type_pellet_pathprofiles

   subroutine deallocate_arr_type_pellet_pathprofiles(structure)

     implicit none

     type (type_pellet_pathprofiles), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_pathprofiles(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_pathprofiles'
     end if

   end subroutine deallocate_arr_type_pellet_pathprofiles

   subroutine deallocate_type_pellet_shape(structure)

     implicit none

     type (type_pellet_shape) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_vecflt_type(structure%dimensions)

   end subroutine deallocate_type_pellet_shape

   subroutine deallocate_arr_type_pellet_shape(structure)

     implicit none

     type (type_pellet_shape), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pellet_shape(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pellet_shape'
     end if

   end subroutine deallocate_arr_type_pellet_shape

   subroutine deallocate_type_permeability(structure)

     implicit none

     type (type_permeability) :: structure

     call deallocate_type_matflt_type(structure%b)
     call deallocate_type_matflt_type(structure%mur)

   end subroutine deallocate_type_permeability

   subroutine deallocate_arr_type_permeability(structure)

     implicit none

     type (type_permeability), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_permeability(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_permeability'
     end if

   end subroutine deallocate_arr_type_permeability

   subroutine deallocate_type_pfcircuits(structure)

     implicit none

     type (type_pfcircuits) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_vecstring_type(structure%type)
     call deallocate_type_vecint_type(structure%nnodes)
     call deallocate_type_array3dint_type(structure%connections)

   end subroutine deallocate_type_pfcircuits

   subroutine deallocate_arr_type_pfcircuits(structure)

     implicit none

     type (type_pfcircuits), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfcircuits(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfcircuits'
     end if

   end subroutine deallocate_arr_type_pfcircuits

   subroutine deallocate_type_pfcoils(structure)

     implicit none

     type (type_pfcoils) :: structure

     call deallocate_type_desc_pfcoils(structure%desc_pfcoils)
     call deallocate_type_exp1D(structure%coilcurrent)
     call deallocate_type_exp1D(structure%coilvoltage)
     call deallocate_type_vecflt_type(structure%p_nh)

   end subroutine deallocate_type_pfcoils

   subroutine deallocate_arr_type_pfcoils(structure)

     implicit none

     type (type_pfcoils), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfcoils(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfcoils'
     end if

   end subroutine deallocate_arr_type_pfcoils

   subroutine deallocate_type_pfelement(structure)

     implicit none

     type (type_pfelement) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_matflt_type(structure%turnsign)
     call deallocate_type_matflt_type(structure%area)
     call deallocate_type_pfgeometry(structure%pfgeometry)

   end subroutine deallocate_type_pfelement

   subroutine deallocate_arr_type_pfelement(structure)

     implicit none

     type (type_pfelement), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfelement(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfelement'
     end if

   end subroutine deallocate_arr_type_pfelement

   subroutine deallocate_type_pfgeometry(structure)

     implicit none

     type (type_pfgeometry) :: structure

     call deallocate_type_matint_type(structure%type)
     call deallocate_type_matint_type(structure%npoints)
     call deallocate_type_rz3D(structure%rzcoordinate)
     call deallocate_type_array3dflt_type(structure%rzdrdz)

   end subroutine deallocate_type_pfgeometry

   subroutine deallocate_arr_type_pfgeometry(structure)

     implicit none

     type (type_pfgeometry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfgeometry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfgeometry'
     end if

   end subroutine deallocate_arr_type_pfgeometry

   subroutine deallocate_type_pfpageometry(structure)

     implicit none

     type (type_pfpageometry) :: structure

     call deallocate_type_vecint_type(structure%type)
     call deallocate_type_vecint_type(structure%npoints)
     call deallocate_type_rz2D(structure%rzcoordinate)
     call deallocate_type_matflt_type(structure%rzdrdz)

   end subroutine deallocate_type_pfpageometry

   subroutine deallocate_arr_type_pfpageometry(structure)

     implicit none

     type (type_pfpageometry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfpageometry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfpageometry'
     end if

   end subroutine deallocate_arr_type_pfpageometry

   subroutine deallocate_type_pfpassive(structure)

     implicit none

     type (type_pfpassive) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%res)
     call deallocate_type_vecflt_type(structure%eta)
     call deallocate_type_pfpassive_current(structure%current)
     call deallocate_type_pfpageometry(structure%pfpageometry)

   end subroutine deallocate_type_pfpassive

   subroutine deallocate_arr_type_pfpassive(structure)

     implicit none

     type (type_pfpassive), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfpassive(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfpassive'
     end if

   end subroutine deallocate_arr_type_pfpassive

   subroutine deallocate_type_pfpassive_current(structure)

     implicit none

     type (type_pfpassive_current) :: structure

     call deallocate_type_exp1D(structure%toroidal)
     call deallocate_type_exp1D(structure%poloidal)

   end subroutine deallocate_type_pfpassive_current

   subroutine deallocate_arr_type_pfpassive_current(structure)

     implicit none

     type (type_pfpassive_current), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfpassive_current(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfpassive_current'
     end if

   end subroutine deallocate_arr_type_pfpassive_current

   subroutine deallocate_type_pfsupplies(structure)

     implicit none

     type (type_pfsupplies) :: structure

     call deallocate_type_desc_supply(structure%desc_supply)
     call deallocate_type_exp1D(structure%voltage)
     call deallocate_type_exp1D(structure%current)

   end subroutine deallocate_type_pfsupplies

   subroutine deallocate_arr_type_pfsupplies(structure)

     implicit none

     type (type_pfsupplies), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pfsupplies(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pfsupplies'
     end if

   end subroutine deallocate_arr_type_pfsupplies

   subroutine deallocate_type_phaseellipse(structure)

     implicit none

     type (type_phaseellipse) :: structure

     call deallocate_type_vecflt_type(structure%invcurvrad)

   end subroutine deallocate_type_phaseellipse

   subroutine deallocate_arr_type_phaseellipse(structure)

     implicit none

     type (type_phaseellipse), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_phaseellipse(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_phaseellipse'
     end if

   end subroutine deallocate_arr_type_phaseellipse

   subroutine deallocate_type_planecoil(structure)

     implicit none

     type (type_planecoil) :: structure

     call deallocate_type_rz1D(structure%coordinates)
     call deallocate_type_vecflt_type(structure%hlength)
     call deallocate_type_vecflt_type(structure%radialhwidth)

   end subroutine deallocate_type_planecoil

   subroutine deallocate_arr_type_planecoil(structure)

     implicit none

     type (type_planecoil), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_planecoil(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_planecoil'
     end if

   end subroutine deallocate_arr_type_planecoil

   subroutine deallocate_type_plasmaComplexType(structure)

     implicit none

     type (type_plasmaComplexType) :: structure

     call deallocate_type_vecint_type(structure%species)
     call deallocate_type_matflt_type(structure%flux)
     call deallocate_type_matflt_type(structure%b)
     call deallocate_type_matflt_type(structure%energy)

   end subroutine deallocate_type_plasmaComplexType

   subroutine deallocate_arr_type_plasmaComplexType(structure)

     implicit none

     type (type_plasmaComplexType), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_plasmaComplexType(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_plasmaComplexType'
     end if

   end subroutine deallocate_arr_type_plasmaComplexType

   subroutine deallocate_type_plasmaedge(structure)

     implicit none

     type (type_plasmaedge) :: structure

     call deallocate_type_vecflt_type(structure%distance)
     call deallocate_type_vecflt_type(structure%density)

   end subroutine deallocate_type_plasmaedge

   subroutine deallocate_arr_type_plasmaedge(structure)

     implicit none

     type (type_plasmaedge), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_plasmaedge(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_plasmaedge'
     end if

   end subroutine deallocate_arr_type_plasmaedge

   subroutine deallocate_type_pol_decomp(structure)

     implicit none

     type (type_pol_decomp) :: structure

     call deallocate_type_vecint_type(structure%mpol)
     call deallocate_type_array3dflt_type(structure%e_plus)
     call deallocate_type_array3dflt_type(structure%e_plus_ph)
     call deallocate_type_array3dflt_type(structure%e_minus)
     call deallocate_type_array3dflt_type(structure%e_minus_ph)
     call deallocate_type_array3dflt_type(structure%e_norm)
     call deallocate_type_array3dflt_type(structure%e_norm_ph)
     call deallocate_type_array3dflt_type(structure%e_binorm)
     call deallocate_type_array3dflt_type(structure%e_binorm_ph)
     call deallocate_type_array3dflt_type(structure%e_para)
     call deallocate_type_array3dflt_type(structure%e_para_ph)
     call deallocate_type_array3dflt_type(structure%b_norm)
     call deallocate_type_array3dflt_type(structure%b_norm_ph)
     call deallocate_type_array3dflt_type(structure%b_binorm)
     call deallocate_type_array3dflt_type(structure%b_binorm_ph)
     call deallocate_type_array3dflt_type(structure%b_para)
     call deallocate_type_array3dflt_type(structure%b_para_ph)
     call deallocate_type_array3dflt_type(structure%k_perp)

   end subroutine deallocate_type_pol_decomp

   subroutine deallocate_arr_type_pol_decomp(structure)

     implicit none

     type (type_pol_decomp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_pol_decomp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_pol_decomp'
     end if

   end subroutine deallocate_arr_type_pol_decomp

   subroutine deallocate_type_polarimetry(structure)

     implicit none

     type (type_polarimetry) :: structure

     call deallocate_type_msediag_setup_polarimetry(structure%setup)
     call deallocate_type_exp1D(structure%measure)

   end subroutine deallocate_type_polarimetry

   subroutine deallocate_arr_type_polarimetry(structure)

     implicit none

     type (type_polarimetry), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_polarimetry(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_polarimetry'
     end if

   end subroutine deallocate_arr_type_polarimetry

   subroutine deallocate_type_polarization(structure)

     implicit none

     type (type_polarization) :: structure

     call deallocate_type_vecflt_type(structure%epol_p_re)
     call deallocate_type_vecflt_type(structure%epol_p_im)
     call deallocate_type_vecflt_type(structure%epol_m_re)
     call deallocate_type_vecflt_type(structure%epol_m_im)
     call deallocate_type_vecflt_type(structure%epol_par_re)
     call deallocate_type_vecflt_type(structure%epol_par_im)

   end subroutine deallocate_type_polarization

   subroutine deallocate_arr_type_polarization(structure)

     implicit none

     type (type_polarization), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_polarization(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_polarization'
     end if

   end subroutine deallocate_arr_type_polarization

   subroutine deallocate_type_power_conv_component(structure)

     implicit none

     type (type_power_conv_component) :: structure

     call deallocate_type_vecstring_type(structure%name)

   end subroutine deallocate_type_power_conv_component

   subroutine deallocate_arr_type_power_conv_component(structure)

     implicit none

     type (type_power_conv_component), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_power_conv_component(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_power_conv_component'
     end if

   end subroutine deallocate_arr_type_power_conv_component

   subroutine deallocate_type_power_exchange(structure)

     implicit none

     type (type_power_exchange) :: structure

     call deallocate_type_vecflt_type(structure%dep_pow)

   end subroutine deallocate_type_power_exchange

   subroutine deallocate_arr_type_power_exchange(structure)

     implicit none

     type (type_power_exchange), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_power_exchange(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_power_exchange'
     end if

   end subroutine deallocate_arr_type_power_exchange

   subroutine deallocate_type_powerflow(structure)

     implicit none

     type (type_powerflow) :: structure

     call deallocate_type_vecflt_type(structure%phi_perp)
     call deallocate_type_vecflt_type(structure%phi_par)
     call deallocate_type_vecflt_type(structure%power_e)
     call deallocate_type_matflt_type(structure%power_i)

   end subroutine deallocate_type_powerflow

   subroutine deallocate_arr_type_powerflow(structure)

     implicit none

     type (type_powerflow), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_powerflow(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_powerflow'
     end if

   end subroutine deallocate_arr_type_powerflow

   subroutine deallocate_type_profiles1d(structure)

     implicit none

     type (type_profiles1d) :: structure

     call deallocate_type_coreprofile(structure%pe)
     call deallocate_type_coreprofile(structure%dpedt)
     call deallocate_type_coreprofion(structure%pi)
     call deallocate_type_coreprofile(structure%pi_tot)
     call deallocate_type_coreprofile(structure%dpi_totdt)
     call deallocate_type_coreprofile(structure%pr_th)
     call deallocate_type_coreprofile(structure%pr_perp)
     call deallocate_type_coreprofile(structure%pr_parallel)
     call deallocate_type_coreprofile(structure%jtot)
     call deallocate_type_coreprofile(structure%jni)
     call deallocate_type_coreprofile(structure%jphi)
     call deallocate_type_coreprofile(structure%joh)
     call deallocate_type_coreprofile(structure%vloop)
     call deallocate_type_coreprofile(structure%sigmapar)
     call deallocate_type_sourceel(structure%qoh)
     call deallocate_type_coreprofile(structure%qei)
     call deallocate_type_coreprofile(structure%eparallel)
     call deallocate_type_coreprofile(structure%e_b)
     call deallocate_type_coreprofile(structure%q)
     call deallocate_type_coreprofile(structure%shear)
     call deallocate_type_coreprofion(structure%ns)
     call deallocate_type_coreprofion(structure%mtor)
     call deallocate_type_coreprofion(structure%wtor)
     call deallocate_type_coreprofion(structure%vpol)
     call deallocate_type_coreprofile(structure%zeff)
     call deallocate_type_coreprofile(structure%bpol)
     call deallocate_type_coreprofile(structure%dvprimedt)

   end subroutine deallocate_type_profiles1d

   subroutine deallocate_arr_type_profiles1d(structure)

     implicit none

     type (type_profiles1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_profiles1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_profiles1d'
     end if

   end subroutine deallocate_arr_type_profiles1d

   subroutine deallocate_type_profiles_1d(structure)

     implicit none

     type (type_profiles_1d) :: structure

     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%pressure)
     call deallocate_type_vecflt_type(structure%F_dia)
     call deallocate_type_vecflt_type(structure%pprime)
     call deallocate_type_vecflt_type(structure%ffprime)
     call deallocate_type_vecflt_type(structure%jphi)
     call deallocate_type_vecflt_type(structure%jparallel)
     call deallocate_type_vecflt_type(structure%q)
     call deallocate_type_vecflt_type(structure%shear)
     call deallocate_type_vecflt_type(structure%r_inboard)
     call deallocate_type_vecflt_type(structure%r_outboard)
     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%dpsidrho_tor)
     call deallocate_type_vecflt_type(structure%rho_vol)
     call deallocate_type_vecflt_type(structure%beta_pol)
     call deallocate_type_vecflt_type(structure%li)
     call deallocate_type_vecflt_type(structure%elongation)
     call deallocate_type_vecflt_type(structure%tria_upper)
     call deallocate_type_vecflt_type(structure%tria_lower)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%vprime)
     call deallocate_type_vecflt_type(structure%dvdrho)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%aprime)
     call deallocate_type_vecflt_type(structure%surface)
     call deallocate_type_vecflt_type(structure%ftrap)
     call deallocate_type_vecflt_type(structure%gm1)
     call deallocate_type_vecflt_type(structure%gm2)
     call deallocate_type_vecflt_type(structure%gm3)
     call deallocate_type_vecflt_type(structure%gm4)
     call deallocate_type_vecflt_type(structure%gm5)
     call deallocate_type_vecflt_type(structure%gm6)
     call deallocate_type_vecflt_type(structure%gm7)
     call deallocate_type_vecflt_type(structure%gm8)
     call deallocate_type_vecflt_type(structure%gm9)
     call deallocate_type_vecflt_type(structure%b_av)
     call deallocate_type_vecflt_type(structure%b_min)
     call deallocate_type_vecflt_type(structure%b_max)
     call deallocate_type_vecflt_type(structure%omega)
     call deallocate_type_vecflt_type(structure%omegaprime)
     call deallocate_type_vecflt_type(structure%mach_a)
     call deallocate_type_vecflt_type(structure%phi_flow)
     call deallocate_type_vecflt_type(structure%s_flow)
     call deallocate_type_vecflt_type(structure%h_flow)
     call deallocate_type_vecflt_type(structure%rho_mass)

   end subroutine deallocate_type_profiles_1d

   subroutine deallocate_arr_type_profiles_1d(structure)

     implicit none

     type (type_profiles_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_profiles_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_profiles_1d'
     end if

   end subroutine deallocate_arr_type_profiles_1d

   subroutine deallocate_type_psi(structure)

     implicit none

     type (type_psi) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%ddrho)
     call deallocate_type_vecflt_type(structure%d2drho2)
     call deallocate_type_vecflt_type(structure%ddt_rhotorn)
     call deallocate_type_vecflt_type(structure%ddt_phi)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_boundary(structure%boundary)
     call deallocate_type_jni(structure%jni)
     call deallocate_type_coreprofile(structure%sigma_par)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_psi

   subroutine deallocate_arr_type_psi(structure)

     implicit none

     type (type_psi), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_psi(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_psi'
     end if

   end subroutine deallocate_arr_type_psi

   subroutine deallocate_type_putinfo(structure)

     implicit none

     type (type_putinfo) :: structure

     call deallocate_type_vecstring_type(structure%putmethod)
     call deallocate_type_vecstring_type(structure%putaccess)
     call deallocate_type_vecstring_type(structure%putlocation)
     call deallocate_type_vecstring_type(structure%rights)

   end subroutine deallocate_type_putinfo

   subroutine deallocate_arr_type_putinfo(structure)

     implicit none

     type (type_putinfo), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_putinfo(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_putinfo'
     end if

   end subroutine deallocate_arr_type_putinfo

   subroutine deallocate_type_q(structure)

     implicit none

     type (type_q) :: structure

     call deallocate_type_vecflt_type(structure%qvalue)
     call deallocate_type_rz1D(structure%position)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecflt_type(structure%weight)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_vecflt_type(structure%calculated)
     call deallocate_type_vecflt_type(structure%chi2)

   end subroutine deallocate_type_q

   subroutine deallocate_arr_type_q(structure)

     implicit none

     type (type_q), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_q(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_q'
     end if

   end subroutine deallocate_arr_type_q

   subroutine deallocate_type_reacprodType(structure)

     implicit none

     type (type_reacprodType) :: structure

     call deallocate_type_vecstring_type(structure%label)
     call deallocate_arr_type_amns_constituentType(structure%constituents)
     call deallocate_type_identifier(structure%role)
     call deallocate_type_vecint_type(structure%metastable)
     call deallocate_type_vecstring_type(structure%metastable_label)

   end subroutine deallocate_type_reacprodType

   subroutine deallocate_arr_type_reacprodType(structure)

     implicit none

     type (type_reacprodType), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reacprodType(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reacprodType'
     end if

   end subroutine deallocate_arr_type_reacprodType

   subroutine deallocate_type_react(structure)

     implicit none

     type (type_react) :: structure


   end subroutine deallocate_type_react

   subroutine deallocate_arr_type_react(structure)

     implicit none

     type (type_react), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_react(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_react'
     end if

   end subroutine deallocate_arr_type_react

   subroutine deallocate_type_rectanglexyz(structure)

     implicit none

     type (type_rectanglexyz) :: structure

     call deallocate_type_xyz0D(structure%point01)
     call deallocate_type_xyz0D(structure%point11)
     call deallocate_type_xyz0D(structure%point10)

   end subroutine deallocate_type_rectanglexyz

   subroutine deallocate_arr_type_rectanglexyz(structure)

     implicit none

     type (type_rectanglexyz), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rectanglexyz(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rectanglexyz'
     end if

   end subroutine deallocate_arr_type_rectanglexyz

   subroutine deallocate_type_recycling_neutrals(structure)

     implicit none

     type (type_recycling_neutrals) :: structure

     call deallocate_type_vecflt_type(structure%particles)
     call deallocate_type_vecflt_type(structure%energy)

   end subroutine deallocate_type_recycling_neutrals

   subroutine deallocate_arr_type_recycling_neutrals(structure)

     implicit none

     type (type_recycling_neutrals), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_recycling_neutrals(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_recycling_neutrals'
     end if

   end subroutine deallocate_arr_type_recycling_neutrals

   subroutine deallocate_type_reduced(structure)

     implicit none

     type (type_reduced) :: structure

     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_reduced

   subroutine deallocate_arr_type_reduced(structure)

     implicit none

     type (type_reduced), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reduced(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reduced'
     end if

   end subroutine deallocate_arr_type_reduced

   subroutine deallocate_type_refl_receive(structure)

     implicit none

     type (type_refl_receive) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_t_series_real(structure%raw_signal)
     call deallocate_type_t_series_real(structure%io_signal)
     call deallocate_type_t_series_cplx(structure%iq_receiver)

   end subroutine deallocate_type_refl_receive

   subroutine deallocate_arr_type_refl_receive(structure)

     implicit none

     type (type_refl_receive), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_refl_receive(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_refl_receive'
     end if

   end subroutine deallocate_arr_type_refl_receive

   subroutine deallocate_type_reflectometry_antennas(structure)

     implicit none

     type (type_reflectometry_antennas) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_identifier(structure%type)
     call deallocate_type_origin(structure%origin)
     call deallocate_type_reflectometry_radfield(structure%radfield)
     call deallocate_type_launchsignal(structure%launchsignal)

   end subroutine deallocate_type_reflectometry_antennas

   subroutine deallocate_arr_type_reflectometry_antennas(structure)

     implicit none

     type (type_reflectometry_antennas), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reflectometry_antennas(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reflectometry_antennas'
     end if

   end subroutine deallocate_arr_type_reflectometry_antennas

   subroutine deallocate_type_reflectometry_radfield(structure)

     implicit none

     type (type_reflectometry_radfield) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_vecflt_type(structure%position)
     call deallocate_arr_type_reflectometry_radfield_gaussian(structure%gaussian)
     call deallocate_arr_type_reflectometry_radifield_efield(structure%efield)

   end subroutine deallocate_type_reflectometry_radfield

   subroutine deallocate_arr_type_reflectometry_radfield(structure)

     implicit none

     type (type_reflectometry_radfield), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reflectometry_radfield(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reflectometry_radfield'
     end if

   end subroutine deallocate_arr_type_reflectometry_radfield

   subroutine deallocate_type_reflectometry_radfield_gaussian(structure)

     implicit none

     type (type_reflectometry_radfield_gaussian) :: structure

     call deallocate_type_simp_apert(structure%aperture)
     call deallocate_type_vecflt_type(structure%waistsize)
     call deallocate_type_vecflt_type(structure%waistzpos)
     call deallocate_type_vecflt_type(structure%tiltangle)
     call deallocate_type_vecflt_type(structure%polar_angle)

   end subroutine deallocate_type_reflectometry_radfield_gaussian

   subroutine deallocate_arr_type_reflectometry_radfield_gaussian(structure)

     implicit none

     type (type_reflectometry_radfield_gaussian), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reflectometry_radfield_gaussian(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reflectometry_radfield_gaussian'
     end if

   end subroutine deallocate_arr_type_reflectometry_radfield_gaussian

   subroutine deallocate_type_reflectometry_radifield_efield(structure)

     implicit none

     type (type_reflectometry_radifield_efield) :: structure

     call deallocate_type_reggrid(structure%grid2d)
     call deallocate_type_matcplx_type(structure%e1)
     call deallocate_type_matcplx_type(structure%e2)

   end subroutine deallocate_type_reflectometry_radifield_efield

   subroutine deallocate_arr_type_reflectometry_radifield_efield(structure)

     implicit none

     type (type_reflectometry_radifield_efield), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reflectometry_radifield_efield(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reflectometry_radifield_efield'
     end if

   end subroutine deallocate_arr_type_reflectometry_radifield_efield

   subroutine deallocate_type_reggrid(structure)

     implicit none

     type (type_reggrid) :: structure

     call deallocate_type_vecflt_type(structure%dim1)
     call deallocate_type_vecflt_type(structure%dim2)

   end subroutine deallocate_type_reggrid

   subroutine deallocate_arr_type_reggrid(structure)

     implicit none

     type (type_reggrid), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_reggrid(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_reggrid'
     end if

   end subroutine deallocate_arr_type_reggrid

   subroutine deallocate_type_rfameasure(structure)

     implicit none

     type (type_rfameasure) :: structure

     call deallocate_type_exp1D(structure%ti)

   end subroutine deallocate_type_rfameasure

   subroutine deallocate_arr_type_rfameasure(structure)

     implicit none

     type (type_rfameasure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rfameasure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rfameasure'
     end if

   end subroutine deallocate_arr_type_rfameasure

   subroutine deallocate_type_rfasetup(structure)

     implicit none

     type (type_rfasetup) :: structure

     call deallocate_type_rzphi1Dexp(structure%position)

   end subroutine deallocate_type_rfasetup

   subroutine deallocate_arr_type_rfasetup(structure)

     implicit none

     type (type_rfasetup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rfasetup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rfasetup'
     end if

   end subroutine deallocate_arr_type_rfasetup

   subroutine deallocate_type_rfbeam(structure)

     implicit none

     type (type_rfbeam) :: structure

     call deallocate_type_spot(structure%spot)
     call deallocate_type_phaseellipse(structure%phaseellipse)

   end subroutine deallocate_type_rfbeam

   subroutine deallocate_arr_type_rfbeam(structure)

     implicit none

     type (type_rfbeam), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rfbeam(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rfbeam'
     end if

   end subroutine deallocate_arr_type_rfbeam

   subroutine deallocate_type_rz0D(structure)

     implicit none

     type (type_rz0D) :: structure


   end subroutine deallocate_type_rz0D

   subroutine deallocate_arr_type_rz0D(structure)

     implicit none

     type (type_rz0D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rz0D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rz0D'
     end if

   end subroutine deallocate_arr_type_rz0D

   subroutine deallocate_type_rz1D(structure)

     implicit none

     type (type_rz1D) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)

   end subroutine deallocate_type_rz1D

   subroutine deallocate_arr_type_rz1D(structure)

     implicit none

     type (type_rz1D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rz1D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rz1D'
     end if

   end subroutine deallocate_arr_type_rz1D

   subroutine deallocate_type_rz1D_npoints(structure)

     implicit none

     type (type_rz1D_npoints) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)

   end subroutine deallocate_type_rz1D_npoints

   subroutine deallocate_arr_type_rz1D_npoints(structure)

     implicit none

     type (type_rz1D_npoints), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rz1D_npoints(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rz1D_npoints'
     end if

   end subroutine deallocate_arr_type_rz1D_npoints

   subroutine deallocate_type_rz1Dexp(structure)

     implicit none

     type (type_rz1Dexp) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)

   end subroutine deallocate_type_rz1Dexp

   subroutine deallocate_arr_type_rz1Dexp(structure)

     implicit none

     type (type_rz1Dexp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rz1Dexp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rz1Dexp'
     end if

   end subroutine deallocate_arr_type_rz1Dexp

   subroutine deallocate_type_rz2D(structure)

     implicit none

     type (type_rz2D) :: structure

     call deallocate_type_matflt_type(structure%r)
     call deallocate_type_matflt_type(structure%z)

   end subroutine deallocate_type_rz2D

   subroutine deallocate_arr_type_rz2D(structure)

     implicit none

     type (type_rz2D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rz2D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rz2D'
     end if

   end subroutine deallocate_arr_type_rz2D

   subroutine deallocate_type_rz3D(structure)

     implicit none

     type (type_rz3D) :: structure

     call deallocate_type_array3dflt_type(structure%r)
     call deallocate_type_array3dflt_type(structure%z)

   end subroutine deallocate_type_rz3D

   subroutine deallocate_arr_type_rz3D(structure)

     implicit none

     type (type_rz3D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rz3D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rz3D'
     end if

   end subroutine deallocate_arr_type_rz3D

   subroutine deallocate_type_rzphi0D(structure)

     implicit none

     type (type_rzphi0D) :: structure


   end subroutine deallocate_type_rzphi0D

   subroutine deallocate_arr_type_rzphi0D(structure)

     implicit none

     type (type_rzphi0D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphi0D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphi0D'
     end if

   end subroutine deallocate_arr_type_rzphi0D

   subroutine deallocate_type_rzphi1D(structure)

     implicit none

     type (type_rzphi1D) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)
     call deallocate_type_vecflt_type(structure%phi)

   end subroutine deallocate_type_rzphi1D

   subroutine deallocate_arr_type_rzphi1D(structure)

     implicit none

     type (type_rzphi1D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphi1D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphi1D'
     end if

   end subroutine deallocate_arr_type_rzphi1D

   subroutine deallocate_type_rzphi1Dexp(structure)

     implicit none

     type (type_rzphi1Dexp) :: structure

     call deallocate_type_exp1D(structure%r)
     call deallocate_type_exp1D(structure%z)
     call deallocate_type_exp1D(structure%phi)

   end subroutine deallocate_type_rzphi1Dexp

   subroutine deallocate_arr_type_rzphi1Dexp(structure)

     implicit none

     type (type_rzphi1Dexp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphi1Dexp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphi1Dexp'
     end if

   end subroutine deallocate_arr_type_rzphi1Dexp

   subroutine deallocate_type_rzphi1Dexperimental(structure)

     implicit none

     type (type_rzphi1Dexperimental) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)
     call deallocate_type_vecflt_type(structure%phi)

   end subroutine deallocate_type_rzphi1Dexperimental

   subroutine deallocate_arr_type_rzphi1Dexperimental(structure)

     implicit none

     type (type_rzphi1Dexperimental), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphi1Dexperimental(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphi1Dexperimental'
     end if

   end subroutine deallocate_arr_type_rzphi1Dexperimental

   subroutine deallocate_type_rzphi2D(structure)

     implicit none

     type (type_rzphi2D) :: structure

     call deallocate_type_matflt_type(structure%r)
     call deallocate_type_matflt_type(structure%z)
     call deallocate_type_matflt_type(structure%phi)

   end subroutine deallocate_type_rzphi2D

   subroutine deallocate_arr_type_rzphi2D(structure)

     implicit none

     type (type_rzphi2D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphi2D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphi2D'
     end if

   end subroutine deallocate_arr_type_rzphi2D

   subroutine deallocate_type_rzphi3D(structure)

     implicit none

     type (type_rzphi3D) :: structure

     call deallocate_type_array3dflt_type(structure%r)
     call deallocate_type_array3dflt_type(structure%z)
     call deallocate_type_array3dflt_type(structure%phi)

   end subroutine deallocate_type_rzphi3D

   subroutine deallocate_arr_type_rzphi3D(structure)

     implicit none

     type (type_rzphi3D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphi3D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphi3D'
     end if

   end subroutine deallocate_arr_type_rzphi3D

   subroutine deallocate_type_rzphidrdzdphi1D(structure)

     implicit none

     type (type_rzphidrdzdphi1D) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%dr)
     call deallocate_type_vecflt_type(structure%dz)
     call deallocate_type_vecflt_type(structure%dphi)

   end subroutine deallocate_type_rzphidrdzdphi1D

   subroutine deallocate_arr_type_rzphidrdzdphi1D(structure)

     implicit none

     type (type_rzphidrdzdphi1D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_rzphidrdzdphi1D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_rzphidrdzdphi1D'
     end if

   end subroutine deallocate_arr_type_rzphidrdzdphi1D

   subroutine deallocate_type_sawteeth_diags(structure)

     implicit none

     type (type_sawteeth_diags) :: structure


   end subroutine deallocate_type_sawteeth_diags

   subroutine deallocate_arr_type_sawteeth_diags(structure)

     implicit none

     type (type_sawteeth_diags), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sawteeth_diags(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sawteeth_diags'
     end if

   end subroutine deallocate_arr_type_sawteeth_diags

   subroutine deallocate_type_sawteeth_profiles1d(structure)

     implicit none

     type (type_sawteeth_profiles1d) :: structure

     call deallocate_type_vecflt_type(structure%ne)
     call deallocate_type_matflt_type(structure%ni)
     call deallocate_type_vecflt_type(structure%te)
     call deallocate_type_matflt_type(structure%ti)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%psistar)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%q)

   end subroutine deallocate_type_sawteeth_profiles1d

   subroutine deallocate_arr_type_sawteeth_profiles1d(structure)

     implicit none

     type (type_sawteeth_profiles1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sawteeth_profiles1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sawteeth_profiles1d'
     end if

   end subroutine deallocate_arr_type_sawteeth_profiles1d

   subroutine deallocate_type_scenario_centre(structure)

     implicit none

     type (type_scenario_centre) :: structure

     call deallocate_type_scenario_ref(structure%te0)
     call deallocate_type_scenario_ref(structure%ti0)
     call deallocate_type_scenario_ref(structure%ne0)
     call deallocate_type_scenario_ref(structure%ni0)
     call deallocate_type_scenario_ref(structure%shift0)
     call deallocate_type_scenario_ref(structure%psi0)
     call deallocate_type_scenario_ref(structure%phi0)
     call deallocate_type_scenario_ref(structure%q0)
     call deallocate_type_scenario_ref(structure%Rmag)
     call deallocate_type_scenario_ref(structure%Zmag)
     call deallocate_type_scenario_ref(structure%vtor_0)

   end subroutine deallocate_type_scenario_centre

   subroutine deallocate_arr_type_scenario_centre(structure)

     implicit none

     type (type_scenario_centre), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_centre(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_centre'
     end if

   end subroutine deallocate_arr_type_scenario_centre

   subroutine deallocate_type_scenario_composition(structure)

     implicit none

     type (type_scenario_composition) :: structure

     call deallocate_type_vecflt_type(structure%amn)
     call deallocate_type_vecflt_type(structure%zn)
     call deallocate_type_vecflt_type(structure%zion)
     call deallocate_type_vecint_type(structure%imp_flag)
     call deallocate_type_vecint_type(structure%rot_imp_flag)
     call deallocate_type_vecflt_type(structure%pellet_amn)
     call deallocate_type_vecflt_type(structure%pellet_zn)
     call deallocate_type_vecflt_type(structure%nbi_amn)
     call deallocate_type_vecflt_type(structure%nbi_zn)

   end subroutine deallocate_type_scenario_composition

   subroutine deallocate_arr_type_scenario_composition(structure)

     implicit none

     type (type_scenario_composition), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_composition(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_composition'
     end if

   end subroutine deallocate_arr_type_scenario_composition

   subroutine deallocate_type_scenario_configuration(structure)

     implicit none

     type (type_scenario_configuration) :: structure

     call deallocate_type_scenario_int(structure%config)
     call deallocate_type_vecstring_type(structure%lmode_sc)
     call deallocate_type_vecstring_type(structure%hmode_sc)
     call deallocate_type_vecstring_type(structure%core_sc)
     call deallocate_type_vecstring_type(structure%pedestal_sc)
     call deallocate_type_vecstring_type(structure%helium_sc)
     call deallocate_type_vecstring_type(structure%impurity_sc)
     call deallocate_type_vecstring_type(structure%l2h_sc)
     call deallocate_type_vecstring_type(structure%tor_rot_sc)
     call deallocate_type_vecstring_type(structure%wall_mat)
     call deallocate_type_vecstring_type(structure%evap_mat)
     call deallocate_type_vecstring_type(structure%lim_mat)
     call deallocate_type_vecstring_type(structure%div_mat)
     call deallocate_type_vecstring_type(structure%coordinate)
     call deallocate_type_scenario_ref(structure%ecrh_freq)
     call deallocate_type_scenario_ref(structure%ecrh_loc)
     call deallocate_type_scenario_int(structure%ecrh_mode)
     call deallocate_type_scenario_ref(structure%ecrh_tor_ang)
     call deallocate_type_scenario_ref(structure%ecrh_pol_ang)
     call deallocate_type_scenario_int(structure%ecrh_harm)
     call deallocate_type_scenario_ref(structure%enbi)
     call deallocate_type_scenario_ref(structure%r_nbi)
     call deallocate_type_scenario_int(structure%grad_b_drift)
     call deallocate_type_scenario_ref(structure%icrh_freq)
     call deallocate_type_vecstring_type(structure%icrh_scheme)
     call deallocate_type_scenario_ref(structure%icrh_phase)
     call deallocate_type_scenario_ref(structure%LH_freq)
     call deallocate_type_scenario_ref(structure%LH_npar)
     call deallocate_type_scenario_ref(structure%pellet_ang)
     call deallocate_type_scenario_ref(structure%pellet_v)
     call deallocate_type_scenario_ref(structure%pellet_nba)

   end subroutine deallocate_type_scenario_configuration

   subroutine deallocate_arr_type_scenario_configuration(structure)

     implicit none

     type (type_scenario_configuration), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_configuration(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_configuration'
     end if

   end subroutine deallocate_arr_type_scenario_configuration

   subroutine deallocate_type_scenario_confinement(structure)

     implicit none

     type (type_scenario_confinement) :: structure

     call deallocate_type_scenario_ref(structure%tau_e)
     call deallocate_type_scenario_ref(structure%tau_l_sc)
     call deallocate_type_scenario_ref(structure%tau_h_sc)
     call deallocate_type_scenario_ref(structure%tau_he)
     call deallocate_type_scenario_ref(structure%tau_e_ee)
     call deallocate_type_scenario_ref(structure%tau_e_ii)
     call deallocate_type_scenario_ref(structure%tau_e_ei)
     call deallocate_type_scenario_ref(structure%tau_cur_diff)
     call deallocate_type_scenario_ref(structure%tau_i_rol)

   end subroutine deallocate_type_scenario_confinement

   subroutine deallocate_arr_type_scenario_confinement(structure)

     implicit none

     type (type_scenario_confinement), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_confinement(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_confinement'
     end if

   end subroutine deallocate_arr_type_scenario_confinement

   subroutine deallocate_type_scenario_currents(structure)

     implicit none

     type (type_scenario_currents) :: structure

     call deallocate_type_scenario_ref(structure%RR)
     call deallocate_type_scenario_ref(structure%i_align)
     call deallocate_type_scenario_ref(structure%i_boot)
     call deallocate_type_scenario_ref(structure%i_cd_tot)
     call deallocate_type_scenario_ref(structure%i_eccd)
     call deallocate_type_scenario_ref(structure%i_fast_ion)
     call deallocate_type_scenario_ref(structure%i_fwcd)
     call deallocate_type_scenario_ref(structure%i_lhcd)
     call deallocate_type_scenario_ref(structure%i_nbicd)
     call deallocate_type_scenario_ref(structure%i_ni_tot)
     call deallocate_type_scenario_ref(structure%i_ohm)
     call deallocate_type_scenario_ref(structure%i_par)
     call deallocate_type_scenario_ref(structure%i_runaway)
     call deallocate_type_scenario_ref(structure%v_loop)
     call deallocate_type_scenario_ref(structure%v_meas)

   end subroutine deallocate_type_scenario_currents

   subroutine deallocate_arr_type_scenario_currents(structure)

     implicit none

     type (type_scenario_currents), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_currents(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_currents'
     end if

   end subroutine deallocate_arr_type_scenario_currents

   subroutine deallocate_type_scenario_edge(structure)

     implicit none

     type (type_scenario_edge) :: structure

     call deallocate_type_scenario_ref(structure%te_edge)
     call deallocate_type_scenario_ref(structure%ti_edge)
     call deallocate_type_scenario_ref(structure%ne_edge)
     call deallocate_type_scenario_ref(structure%ni_edge)
     call deallocate_type_scenario_ref(structure%psi_edge)
     call deallocate_type_scenario_ref(structure%phi_edge)
     call deallocate_type_scenario_ref(structure%rho_edge)
     call deallocate_type_scenario_ref(structure%drho_edge_dt)
     call deallocate_type_scenario_ref(structure%q_edge)
     call deallocate_type_scenario_ref(structure%neutral_flux)
     call deallocate_type_scenario_ref(structure%phi_plasma)
     call deallocate_type_scenario_ref(structure%vtor_edge)

   end subroutine deallocate_type_scenario_edge

   subroutine deallocate_arr_type_scenario_edge(structure)

     implicit none

     type (type_scenario_edge), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_edge(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_edge'
     end if

   end subroutine deallocate_arr_type_scenario_edge

   subroutine deallocate_type_scenario_energy(structure)

     implicit none

     type (type_scenario_energy) :: structure

     call deallocate_type_scenario_ref(structure%w_tot)
     call deallocate_type_scenario_ref(structure%w_b_pol)
     call deallocate_type_scenario_ref(structure%w_dia)
     call deallocate_type_scenario_ref(structure%dwdia_dt)
     call deallocate_type_scenario_ref(structure%w_b_tor_pla)
     call deallocate_type_scenario_ref(structure%w_th)
     call deallocate_type_scenario_ref(structure%dwtot_dt)
     call deallocate_type_scenario_ref(structure%dwbpol_dt)
     call deallocate_type_scenario_ref(structure%dwbtorpla_dt)
     call deallocate_type_scenario_ref(structure%dwth_dt)
     call deallocate_type_scenario_ref(structure%esup_icrhtot)
     call deallocate_type_scenario_ref(structure%esup_icrhper)
     call deallocate_type_scenario_ref(structure%esup_nbitot)
     call deallocate_type_scenario_ref(structure%esup_nbiperp)
     call deallocate_type_scenario_ref(structure%esup_lhcd)
     call deallocate_type_scenario_ref(structure%esup_alpha)

   end subroutine deallocate_type_scenario_energy

   subroutine deallocate_arr_type_scenario_energy(structure)

     implicit none

     type (type_scenario_energy), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_energy(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_energy'
     end if

   end subroutine deallocate_arr_type_scenario_energy

   subroutine deallocate_type_scenario_global(structure)

     implicit none

     type (type_scenario_global) :: structure

     call deallocate_type_scenario_ref(structure%ip)
     call deallocate_type_scenario_ref(structure%dip_dt)
     call deallocate_type_scenario_ref(structure%beta_pol)
     call deallocate_type_scenario_ref(structure%beta_tor)
     call deallocate_type_scenario_ref(structure%beta_normal)
     call deallocate_type_scenario_ref(structure%li)
     call deallocate_type_scenario_ref(structure%volume)
     call deallocate_type_scenario_ref(structure%area_pol)
     call deallocate_type_scenario_ref(structure%area_ext)
     call deallocate_type_scenario_ref(structure%len_sepa)
     call deallocate_type_scenario_ref(structure%beta_pol_th)
     call deallocate_type_scenario_ref(structure%beta_tor_th)
     call deallocate_type_scenario_ref(structure%beta_n_th)
     call deallocate_type_scenario_ref(structure%disruption)
     call deallocate_type_scenario_ref(structure%mode_h)
     call deallocate_type_scenario_ref(structure%s_alpha)

   end subroutine deallocate_type_scenario_global

   subroutine deallocate_arr_type_scenario_global(structure)

     implicit none

     type (type_scenario_global), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_global(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_global'
     end if

   end subroutine deallocate_arr_type_scenario_global

   subroutine deallocate_type_scenario_heat_power(structure)

     implicit none

     type (type_scenario_heat_power) :: structure

     call deallocate_type_scenario_ref(structure%plh)
     call deallocate_type_scenario_ref(structure%pohmic)
     call deallocate_type_scenario_ref(structure%picrh)
     call deallocate_type_scenario_ref(structure%pecrh)
     call deallocate_type_scenario_ref(structure%pnbi)
     call deallocate_type_scenario_ref(structure%pnbi_co_cur)
     call deallocate_type_scenario_ref(structure%pnbi_counter)
     call deallocate_type_scenario_ref(structure%plh_th)
     call deallocate_type_scenario_ref(structure%picrh_th)
     call deallocate_type_scenario_ref(structure%pecrh_th)
     call deallocate_type_scenario_ref(structure%pnbi_th)
     call deallocate_type_scenario_ref(structure%ploss_icrh)
     call deallocate_type_scenario_ref(structure%ploss_nbi)
     call deallocate_type_scenario_ref(structure%pbrem)
     call deallocate_type_scenario_ref(structure%pcyclo)
     call deallocate_type_scenario_ref(structure%prad)
     call deallocate_type_scenario_ref(structure%pdd_fus)
     call deallocate_type_scenario_ref(structure%pei)
     call deallocate_type_scenario_ref(structure%pel_tot)
     call deallocate_type_scenario_ref(structure%pel_fus)
     call deallocate_type_scenario_ref(structure%pel_icrh)
     call deallocate_type_scenario_ref(structure%pel_nbi)
     call deallocate_type_scenario_ref(structure%pfus_dt)
     call deallocate_type_scenario_ref(structure%ploss_fus)
     call deallocate_type_scenario_ref(structure%pfus_nbi)
     call deallocate_type_scenario_ref(structure%pfus_th)
     call deallocate_type_scenario_ref(structure%padd_tot)
     call deallocate_type_scenario_ref(structure%pion_tot)
     call deallocate_type_scenario_ref(structure%pion_fus)
     call deallocate_type_scenario_ref(structure%pion_icrh)
     call deallocate_type_scenario_ref(structure%pion_nbi)
     call deallocate_type_scenario_ref(structure%pioniz)
     call deallocate_type_scenario_ref(structure%ploss)
     call deallocate_type_scenario_ref(structure%p_wth)
     call deallocate_type_scenario_ref(structure%p_w)
     call deallocate_type_scenario_ref(structure%p_l2h_thr)
     call deallocate_type_scenario_ref(structure%p_l2h_sc)
     call deallocate_type_scenario_ref(structure%p_nbi_icrh)

   end subroutine deallocate_type_scenario_heat_power

   subroutine deallocate_arr_type_scenario_heat_power(structure)

     implicit none

     type (type_scenario_heat_power), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_heat_power(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_heat_power'
     end if

   end subroutine deallocate_arr_type_scenario_heat_power

   subroutine deallocate_type_scenario_int(structure)

     implicit none

     type (type_scenario_int) :: structure

     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_scenario_int

   subroutine deallocate_arr_type_scenario_int(structure)

     implicit none

     type (type_scenario_int), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_int(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_int'
     end if

   end subroutine deallocate_arr_type_scenario_int

   subroutine deallocate_type_scenario_itb(structure)

     implicit none

     type (type_scenario_itb) :: structure

     call deallocate_type_scenario_ref(structure%q_min)
     call deallocate_type_scenario_ref(structure%te_itb)
     call deallocate_type_scenario_ref(structure%ti_itb)
     call deallocate_type_scenario_ref(structure%ne_itb)
     call deallocate_type_scenario_ref(structure%ni_itb)
     call deallocate_type_scenario_ref(structure%psi_itb)
     call deallocate_type_scenario_ref(structure%phi_itb)
     call deallocate_type_scenario_ref(structure%rho_itb)
     call deallocate_type_scenario_ref(structure%h_itb)
     call deallocate_type_scenario_ref(structure%width_itb)
     call deallocate_type_scenario_ref(structure%vtor_itb)
     call deallocate_type_scenario_int(structure%itb_type)

   end subroutine deallocate_type_scenario_itb

   subroutine deallocate_arr_type_scenario_itb(structure)

     implicit none

     type (type_scenario_itb), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_itb(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_itb'
     end if

   end subroutine deallocate_arr_type_scenario_itb

   subroutine deallocate_type_scenario_lim_div_wall(structure)

     implicit none

     type (type_scenario_lim_div_wall) :: structure

     call deallocate_type_scenario_ref(structure%te_lim_div)
     call deallocate_type_scenario_ref(structure%ti_lim_div)
     call deallocate_type_scenario_ref(structure%ne_lim_div)
     call deallocate_type_scenario_ref(structure%ni_lim_div)
     call deallocate_type_scenario_ref(structure%q_peak_div)
     call deallocate_type_scenario_ref(structure%q_peak_wall)
     call deallocate_type_scenario_ref(structure%surf_temp)
     call deallocate_type_scenario_ref(structure%p_lim_div)
     call deallocate_type_scenario_ref(structure%p_rad_div)
     call deallocate_type_scenario_ref(structure%p_neut_div)
     call deallocate_type_scenario_ref(structure%p_wall)
     call deallocate_type_scenario_ref(structure%wall_temp)
     call deallocate_type_scenario_ref(structure%wall_state)
     call deallocate_type_scenario_ref(structure%detach_state)
     call deallocate_type_scenario_ref(structure%pump_flux)
     call deallocate_type_scenario_ref(structure%p_rad_fw)
     call deallocate_type_scenario_ref(structure%p_cond_fw)
     call deallocate_type_scenario_ref(structure%div_wetted)
     call deallocate_type_scenario_ref(structure%gas_puff)
     call deallocate_type_scenario_ref(structure%ar_concentr)
     call deallocate_type_scenario_ref(structure%part_exhaust)
     call deallocate_type_scenario_ref(structure%f_inner)
     call deallocate_type_scenario_ref(structure%f_outer)
     call deallocate_type_scenario_ref(structure%f_pfr)
     call deallocate_type_scenario_ref(structure%f_rad_fw)
     call deallocate_type_vecflt_type(structure%q_div)
     call deallocate_type_scenario_ref(structure%p_cond_div)
     call deallocate_type_scenario_ref(structure%p_nh_div)

   end subroutine deallocate_type_scenario_lim_div_wall

   subroutine deallocate_arr_type_scenario_lim_div_wall(structure)

     implicit none

     type (type_scenario_lim_div_wall), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_lim_div_wall(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_lim_div_wall'
     end if

   end subroutine deallocate_arr_type_scenario_lim_div_wall

   subroutine deallocate_type_scenario_line_ave(structure)

     implicit none

     type (type_scenario_line_ave) :: structure

     call deallocate_type_scenario_ref(structure%ne_line)
     call deallocate_type_scenario_ref(structure%zeff_line)
     call deallocate_type_scenario_ref(structure%ne_zeff_line)
     call deallocate_type_scenario_ref(structure%dne_line_dt)

   end subroutine deallocate_type_scenario_line_ave

   subroutine deallocate_arr_type_scenario_line_ave(structure)

     implicit none

     type (type_scenario_line_ave), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_line_ave(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_line_ave'
     end if

   end subroutine deallocate_arr_type_scenario_line_ave

   subroutine deallocate_type_scenario_neutron(structure)

     implicit none

     type (type_scenario_neutron) :: structure

     call deallocate_type_scenario_ref(structure%ndd_tot)
     call deallocate_type_scenario_ref(structure%ndd_th)
     call deallocate_type_scenario_ref(structure%ndd_nbi_th)
     call deallocate_type_scenario_ref(structure%ndd_nbi_nbi)
     call deallocate_type_scenario_ref(structure%ndt_tot)
     call deallocate_type_scenario_ref(structure%ndt_th)

   end subroutine deallocate_type_scenario_neutron

   subroutine deallocate_arr_type_scenario_neutron(structure)

     implicit none

     type (type_scenario_neutron), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_neutron(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_neutron'
     end if

   end subroutine deallocate_arr_type_scenario_neutron

   subroutine deallocate_type_scenario_ninety_five(structure)

     implicit none

     type (type_scenario_ninety_five) :: structure

     call deallocate_type_scenario_ref(structure%q_95)
     call deallocate_type_scenario_ref(structure%elong_95)
     call deallocate_type_scenario_ref(structure%tria_95)
     call deallocate_type_scenario_ref(structure%tria_up_95)
     call deallocate_type_scenario_ref(structure%tria_lo_95)
     call deallocate_type_scenario_ref(structure%te_95)
     call deallocate_type_scenario_ref(structure%ti_95)
     call deallocate_type_scenario_ref(structure%ne_95)
     call deallocate_type_scenario_ref(structure%ni_95)
     call deallocate_type_scenario_ref(structure%phi_95)
     call deallocate_type_scenario_ref(structure%rho_95)
     call deallocate_type_scenario_ref(structure%vtor_95)

   end subroutine deallocate_type_scenario_ninety_five

   subroutine deallocate_arr_type_scenario_ninety_five(structure)

     implicit none

     type (type_scenario_ninety_five), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_ninety_five(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_ninety_five'
     end if

   end subroutine deallocate_arr_type_scenario_ninety_five

   subroutine deallocate_type_scenario_pedestal(structure)

     implicit none

     type (type_scenario_pedestal) :: structure

     call deallocate_type_scenario_ref(structure%te_ped)
     call deallocate_type_scenario_ref(structure%ti_ped)
     call deallocate_type_scenario_ref(structure%ne_ped)
     call deallocate_type_scenario_ref(structure%ni_ped)
     call deallocate_type_scenario_ref(structure%psi_ped)
     call deallocate_type_scenario_ref(structure%phi_ped)
     call deallocate_type_scenario_ref(structure%rho_ped)
     call deallocate_type_scenario_ref(structure%q_ped)
     call deallocate_type_scenario_ref(structure%pressure_ped)
     call deallocate_type_scenario_ref(structure%vtor_ped)

   end subroutine deallocate_type_scenario_pedestal

   subroutine deallocate_arr_type_scenario_pedestal(structure)

     implicit none

     type (type_scenario_pedestal), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_pedestal(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_pedestal'
     end if

   end subroutine deallocate_arr_type_scenario_pedestal

   subroutine deallocate_type_scenario_reactor(structure)

     implicit none

     type (type_scenario_reactor) :: structure


   end subroutine deallocate_type_scenario_reactor

   subroutine deallocate_arr_type_scenario_reactor(structure)

     implicit none

     type (type_scenario_reactor), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_reactor(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_reactor'
     end if

   end subroutine deallocate_arr_type_scenario_reactor

   subroutine deallocate_type_scenario_ref(structure)

     implicit none

     type (type_scenario_ref) :: structure

     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_scenario_ref

   subroutine deallocate_arr_type_scenario_ref(structure)

     implicit none

     type (type_scenario_ref), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_ref(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_ref'
     end if

   end subroutine deallocate_arr_type_scenario_ref

   subroutine deallocate_type_scenario_references(structure)

     implicit none

     type (type_scenario_references) :: structure

     call deallocate_type_scenario_ref(structure%plh)
     call deallocate_type_scenario_ref(structure%picrh)
     call deallocate_type_scenario_ref(structure%pecrh)
     call deallocate_type_scenario_ref(structure%pnbi)
     call deallocate_type_scenario_ref(structure%ip)
     call deallocate_type_scenario_ref(structure%bvac_r)
     call deallocate_type_scenario_ref(structure%zeffl)
     call deallocate_type_scenario_ref(structure%nbar)
     call deallocate_type_scenario_ref(structure%xecrh)
     call deallocate_type_scenario_ref(structure%pol_flux)
     call deallocate_type_scenario_ref(structure%enhancement)
     call deallocate_type_scenario_ref(structure%isotopic)
     call deallocate_type_scenario_ref(structure%nbi_td_ratio)
     call deallocate_type_scenario_ref(structure%gas_puff)

   end subroutine deallocate_type_scenario_references

   subroutine deallocate_arr_type_scenario_references(structure)

     implicit none

     type (type_scenario_references), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_references(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_references'
     end if

   end subroutine deallocate_arr_type_scenario_references

   subroutine deallocate_type_scenario_sol(structure)

     implicit none

     type (type_scenario_sol) :: structure

     call deallocate_type_scenario_ref(structure%l_te_sol)
     call deallocate_type_scenario_ref(structure%l_ti_sol)
     call deallocate_type_scenario_ref(structure%l_ne_sol)
     call deallocate_type_scenario_ref(structure%l_ni_sol)
     call deallocate_type_scenario_ref(structure%l_qe_sol)
     call deallocate_type_scenario_ref(structure%l_qi_sol)
     call deallocate_type_scenario_ref(structure%p_rad_sol)
     call deallocate_type_scenario_ref(structure%gas_puff)

   end subroutine deallocate_type_scenario_sol

   subroutine deallocate_arr_type_scenario_sol(structure)

     implicit none

     type (type_scenario_sol), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_sol(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_sol'
     end if

   end subroutine deallocate_arr_type_scenario_sol

   subroutine deallocate_type_scenario_vol_ave(structure)

     implicit none

     type (type_scenario_vol_ave) :: structure

     call deallocate_type_scenario_ref(structure%te_ave)
     call deallocate_type_scenario_ref(structure%ti_ave)
     call deallocate_type_scenario_ref(structure%ne_ave)
     call deallocate_type_scenario_ref(structure%dne_ave_dt)
     call deallocate_type_scenario_ref(structure%ni_ave)
     call deallocate_type_scenario_ref(structure%zeff_ave)
     call deallocate_type_scenario_ref(structure%ti_o_te_ave)
     call deallocate_type_scenario_ref(structure%meff_ave)
     call deallocate_type_scenario_ref(structure%pellet_flux)
     call deallocate_type_vecflt_type(structure%nions_ave)
     call deallocate_type_scenario_ref(structure%omega_ave)

   end subroutine deallocate_type_scenario_vol_ave

   subroutine deallocate_arr_type_scenario_vol_ave(structure)

     implicit none

     type (type_scenario_vol_ave), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_scenario_vol_ave(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_scenario_vol_ave'
     end if

   end subroutine deallocate_arr_type_scenario_vol_ave

   subroutine deallocate_type_setup_bprobe(structure)

     implicit none

     type (type_setup_bprobe) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_rz1D(structure%position)
     call deallocate_type_vecflt_type(structure%polangle)
     call deallocate_type_vecflt_type(structure%torangle)
     call deallocate_type_vecflt_type(structure%area)
     call deallocate_type_vecflt_type(structure%length)
     call deallocate_type_vecint_type(structure%turns)

   end subroutine deallocate_type_setup_bprobe

   subroutine deallocate_arr_type_setup_bprobe(structure)

     implicit none

     type (type_setup_bprobe), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_setup_bprobe(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_setup_bprobe'
     end if

   end subroutine deallocate_arr_type_setup_bprobe

   subroutine deallocate_type_setup_floops(structure)

     implicit none

     type (type_setup_floops) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%id)
     call deallocate_type_rzphi2D(structure%position)
     call deallocate_type_vecint_type(structure%npoints)

   end subroutine deallocate_type_setup_floops

   subroutine deallocate_arr_type_setup_floops(structure)

     implicit none

     type (type_setup_floops), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_setup_floops(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_setup_floops'
     end if

   end subroutine deallocate_arr_type_setup_floops

   subroutine deallocate_type_setup_line(structure)

     implicit none

     type (type_setup_line) :: structure

     call deallocate_type_rzphi1D(structure%pivot_point)
     call deallocate_type_vecflt_type(structure%horchordang1)
     call deallocate_type_vecflt_type(structure%verchordang1)
     call deallocate_type_vecflt_type(structure%width)
     call deallocate_type_rzphi1D(structure%second_point)
     call deallocate_type_vecflt_type(structure%horchordang2)
     call deallocate_type_vecflt_type(structure%verchordang2)
     call deallocate_type_rzphi1D(structure%third_point)

   end subroutine deallocate_type_setup_line

   subroutine deallocate_arr_type_setup_line(structure)

     implicit none

     type (type_setup_line), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_setup_line(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_setup_line'
     end if

   end subroutine deallocate_arr_type_setup_line

   subroutine deallocate_type_setup_line_exp(structure)

     implicit none

     type (type_setup_line_exp) :: structure

     call deallocate_type_rzphi1Dexperimental(structure%pivot_point)
     call deallocate_type_vecflt_type(structure%horchordang1)
     call deallocate_type_vecflt_type(structure%verchordang1)
     call deallocate_type_vecflt_type(structure%width)
     call deallocate_type_rzphi1Dexperimental(structure%second_point)
     call deallocate_type_vecflt_type(structure%horchordang2)
     call deallocate_type_vecflt_type(structure%verchordang2)
     call deallocate_type_rzphi1Dexperimental(structure%third_point)

   end subroutine deallocate_type_setup_line_exp

   subroutine deallocate_arr_type_setup_line_exp(structure)

     implicit none

     type (type_setup_line_exp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_setup_line_exp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_setup_line_exp'
     end if

   end subroutine deallocate_arr_type_setup_line_exp

   subroutine deallocate_type_shield(structure)

     implicit none

     type (type_shield) :: structure

     call deallocate_type_shield_specs(structure%inboard)
     call deallocate_type_shield_specs(structure%outboard)

   end subroutine deallocate_type_shield

   subroutine deallocate_arr_type_shield(structure)

     implicit none

     type (type_shield), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_shield(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_shield'
     end if

   end subroutine deallocate_arr_type_shield

   subroutine deallocate_type_shield_specs(structure)

     implicit none

     type (type_shield_specs) :: structure

     call deallocate_type_vecflt_type(structure%composition)

   end subroutine deallocate_type_shield_specs

   subroutine deallocate_arr_type_shield_specs(structure)

     implicit none

     type (type_shield_specs), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_shield_specs(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_shield_specs'
     end if

   end subroutine deallocate_arr_type_shield_specs

   subroutine deallocate_type_simp_apert(structure)

     implicit none

     type (type_simp_apert) :: structure

     call deallocate_type_identifier(structure%type)
     call deallocate_type_vecflt_type(structure%sizes)

   end subroutine deallocate_type_simp_apert

   subroutine deallocate_arr_type_simp_apert(structure)

     implicit none

     type (type_simp_apert), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_simp_apert(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_simp_apert'
     end if

   end subroutine deallocate_arr_type_simp_apert

   subroutine deallocate_type_solcurdiag_sol_current(structure)

     implicit none

     type (type_solcurdiag_sol_current) :: structure

     call deallocate_type_solcurdiag_sol_current_setup(structure%setup)
     call deallocate_type_exp0D(structure%measure)

   end subroutine deallocate_type_solcurdiag_sol_current

   subroutine deallocate_arr_type_solcurdiag_sol_current(structure)

     implicit none

     type (type_solcurdiag_sol_current), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_solcurdiag_sol_current(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_solcurdiag_sol_current'
     end if

   end subroutine deallocate_arr_type_solcurdiag_sol_current

   subroutine deallocate_type_solcurdiag_sol_current_setup(structure)

     implicit none

     type (type_solcurdiag_sol_current_setup) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_rz1D(structure%position)

   end subroutine deallocate_type_solcurdiag_sol_current_setup

   subroutine deallocate_arr_type_solcurdiag_sol_current_setup(structure)

     implicit none

     type (type_solcurdiag_sol_current_setup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_solcurdiag_sol_current_setup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_solcurdiag_sol_current_setup'
     end if

   end subroutine deallocate_arr_type_solcurdiag_sol_current_setup

   subroutine deallocate_type_source_imp(structure)

     implicit none

     type (type_source_imp) :: structure

     call deallocate_type_matflt_type(structure%exp)
     call deallocate_type_matflt_type(structure%imp)

   end subroutine deallocate_type_source_imp

   subroutine deallocate_arr_type_source_imp(structure)

     implicit none

     type (type_source_imp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_source_imp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_source_imp'
     end if

   end subroutine deallocate_arr_type_source_imp

   subroutine deallocate_type_source_ion(structure)

     implicit none

     type (type_source_ion) :: structure

     call deallocate_type_matflt_type(structure%exp)
     call deallocate_type_matflt_type(structure%imp)

   end subroutine deallocate_type_source_ion

   subroutine deallocate_arr_type_source_ion(structure)

     implicit none

     type (type_source_ion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_source_ion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_source_ion'
     end if

   end subroutine deallocate_arr_type_source_ion

   subroutine deallocate_type_source_rate(structure)

     implicit none

     type (type_source_rate) :: structure

     call deallocate_type_complexgrid(structure%grid)
     call deallocate_type_complexgrid_scalar(structure%value)
     call deallocate_type_vecint_type(structure%discrete)
     call deallocate_type_parameters(structure%parameters)

   end subroutine deallocate_type_source_rate

   subroutine deallocate_arr_type_source_rate(structure)

     implicit none

     type (type_source_rate), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_source_rate(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_source_rate'
     end if

   end subroutine deallocate_arr_type_source_rate

   subroutine deallocate_type_source_vec(structure)

     implicit none

     type (type_source_vec) :: structure

     call deallocate_type_vecflt_type(structure%exp)
     call deallocate_type_vecflt_type(structure%imp)

   end subroutine deallocate_type_source_vec

   subroutine deallocate_arr_type_source_vec(structure)

     implicit none

     type (type_source_vec), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_source_vec(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_source_vec'
     end if

   end subroutine deallocate_arr_type_source_vec

   subroutine deallocate_type_sourceel(structure)

     implicit none

     type (type_sourceel) :: structure

     call deallocate_type_vecflt_type(structure%value)
     call deallocate_type_vecflt_type(structure%integral)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_sourceel

   subroutine deallocate_arr_type_sourceel(structure)

     implicit none

     type (type_sourceel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sourceel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sourceel'
     end if

   end subroutine deallocate_arr_type_sourceel

   subroutine deallocate_type_sourceimp(structure)

     implicit none

     type (type_sourceimp) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_matflt_type(structure%integral)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_sourceimp

   subroutine deallocate_arr_type_sourceimp(structure)

     implicit none

     type (type_sourceimp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sourceimp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sourceimp'
     end if

   end subroutine deallocate_arr_type_sourceimp

   subroutine deallocate_type_sourceion(structure)

     implicit none

     type (type_sourceion) :: structure

     call deallocate_type_matflt_type(structure%value)
     call deallocate_type_matflt_type(structure%integral)
     call deallocate_type_vecstring_type(structure%source)

   end subroutine deallocate_type_sourceion

   subroutine deallocate_arr_type_sourceion(structure)

     implicit none

     type (type_sourceion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sourceion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sourceion'
     end if

   end subroutine deallocate_arr_type_sourceion

   subroutine deallocate_type_species_desc(structure)

     implicit none

     type (type_species_desc) :: structure

     call deallocate_type_vecstring_type(structure%label)

   end subroutine deallocate_type_species_desc

   subroutine deallocate_arr_type_species_desc(structure)

     implicit none

     type (type_species_desc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_species_desc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_species_desc'
     end if

   end subroutine deallocate_arr_type_species_desc

   subroutine deallocate_type_species_reference(structure)

     implicit none

     type (type_species_reference) :: structure

     call deallocate_type_identifier(structure%type)

   end subroutine deallocate_type_species_reference

   subroutine deallocate_arr_type_species_reference(structure)

     implicit none

     type (type_species_reference), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_species_reference(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_species_reference'
     end if

   end subroutine deallocate_arr_type_species_reference

   subroutine deallocate_type_spectral(structure)

     implicit none

     type (type_spectral) :: structure

     call deallocate_type_msediag_emissivity(structure%emissivity)
     call deallocate_type_msediag_radiance(structure%radiance)
     call deallocate_type_codeparam(structure%codeparam)

   end subroutine deallocate_type_spectral

   subroutine deallocate_arr_type_spectral(structure)

     implicit none

     type (type_spectral), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_spectral(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_spectral'
     end if

   end subroutine deallocate_arr_type_spectral

   subroutine deallocate_type_spectrum(structure)

     implicit none

     type (type_spectrum) :: structure

     call deallocate_type_launchs_phi_theta(structure%phi_theta)
     call deallocate_type_launchs_parallel(structure%parallel)

   end subroutine deallocate_type_spectrum

   subroutine deallocate_arr_type_spectrum(structure)

     implicit none

     type (type_spectrum), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_spectrum(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_spectrum'
     end if

   end subroutine deallocate_arr_type_spectrum

   subroutine deallocate_type_spot(structure)

     implicit none

     type (type_spot) :: structure

     call deallocate_type_vecflt_type(structure%size)

   end subroutine deallocate_type_spot

   subroutine deallocate_arr_type_spot(structure)

     implicit none

     type (type_spot), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_spot(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_spot'
     end if

   end subroutine deallocate_arr_type_spot

   subroutine deallocate_type_sputtering_neutrals(structure)

     implicit none

     type (type_sputtering_neutrals) :: structure

     call deallocate_type_vecflt_type(structure%physical)
     call deallocate_type_vecflt_type(structure%chemical)

   end subroutine deallocate_type_sputtering_neutrals

   subroutine deallocate_arr_type_sputtering_neutrals(structure)

     implicit none

     type (type_sputtering_neutrals), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_sputtering_neutrals(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_sputtering_neutrals'
     end if

   end subroutine deallocate_arr_type_sputtering_neutrals

   subroutine deallocate_type_straps(structure)

     implicit none

     type (type_straps) :: structure

     call deallocate_type_exp0D(structure%current)
     call deallocate_type_exp0D(structure%phase)
     call deallocate_type_rz1D(structure%coord_strap)

   end subroutine deallocate_type_straps

   subroutine deallocate_arr_type_straps(structure)

     implicit none

     type (type_straps), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_straps(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_straps'
     end if

   end subroutine deallocate_arr_type_straps

   subroutine deallocate_type_structure_cs(structure)

     implicit none

     type (type_structure_cs) :: structure


   end subroutine deallocate_type_structure_cs

   subroutine deallocate_arr_type_structure_cs(structure)

     implicit none

     type (type_structure_cs), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_structure_cs(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_structure_cs'
     end if

   end subroutine deallocate_arr_type_structure_cs

   subroutine deallocate_type_t_series_cplx(structure)

     implicit none

     type (type_t_series_cplx) :: structure

     call deallocate_type_vecflt_type(structure%time_wind)
     call deallocate_type_vecflt_type(structure%values_re)
     call deallocate_type_vecflt_type(structure%values_im)

   end subroutine deallocate_type_t_series_cplx

   subroutine deallocate_arr_type_t_series_cplx(structure)

     implicit none

     type (type_t_series_cplx), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_t_series_cplx(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_t_series_cplx'
     end if

   end subroutine deallocate_arr_type_t_series_cplx

   subroutine deallocate_type_t_series_real(structure)

     implicit none

     type (type_t_series_real) :: structure

     call deallocate_type_vecflt_type(structure%time_wind)
     call deallocate_type_vecflt_type(structure%values)

   end subroutine deallocate_type_t_series_real

   subroutine deallocate_arr_type_t_series_real(structure)

     implicit none

     type (type_t_series_real), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_t_series_real(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_t_series_real'
     end if

   end subroutine deallocate_arr_type_t_series_real

   subroutine deallocate_type_table(structure)

     implicit none

     type (type_table) :: structure

     call deallocate_type_vecflt_type(structure%table_1d)
     call deallocate_type_matflt_type(structure%table_2d)
     call deallocate_type_array3dflt_type(structure%table_3d)
     call deallocate_type_array4dflt_type(structure%table_4d)
     call deallocate_type_array5dflt_type(structure%table_5d)
     call deallocate_type_array6dflt_type(structure%table_6d)
     call deallocate_type_vecstring_type(structure%coord1_str)
     call deallocate_type_vecstring_type(structure%coord2_str)
     call deallocate_type_vecstring_type(structure%coord3_str)
     call deallocate_type_vecstring_type(structure%coord4_str)
     call deallocate_type_vecstring_type(structure%coord5_str)
     call deallocate_type_vecstring_type(structure%coord6_str)
     call deallocate_type_identifier(structure%quality)

   end subroutine deallocate_type_table

   subroutine deallocate_arr_type_table(structure)

     implicit none

     type (type_table), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_table(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_table'
     end if

   end subroutine deallocate_arr_type_table

   subroutine deallocate_type_tables(structure)

     implicit none

     type (type_tables) :: structure

     call deallocate_type_vecstring_type(structure%result_label)
     call deallocate_type_vecstring_type(structure%result_unit)
     call deallocate_type_vecint_type(structure%zmin)
     call deallocate_type_vecint_type(structure%zmax)
     call deallocate_type_vecstring_type(structure%state_label)
     call deallocate_arr_type_table(structure%table)
     call deallocate_type_vecstring_type(structure%data_source)
     call deallocate_type_vecstring_type(structure%data_provide)
     call deallocate_type_vecstring_type(structure%data_citation)

   end subroutine deallocate_type_tables

   subroutine deallocate_arr_type_tables(structure)

     implicit none

     type (type_tables), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tables(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tables'
     end if

   end subroutine deallocate_arr_type_tables

   subroutine deallocate_type_tables_coord(structure)

     implicit none

     type (type_tables_coord) :: structure

     call deallocate_arr_type_coords(structure%coords)

   end subroutine deallocate_type_tables_coord

   subroutine deallocate_arr_type_tables_coord(structure)

     implicit none

     type (type_tables_coord), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tables_coord(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tables_coord'
     end if

   end subroutine deallocate_arr_type_tables_coord

   subroutine deallocate_type_temporary_nt(structure)

     implicit none

     type (type_temporary_nt) :: structure

     call deallocate_arr_type_temporary_nt_0dr(structure%float0d)
     call deallocate_arr_type_temporary_nt_0di(structure%integer0d)
     call deallocate_arr_type_temporary_nt_0dc(structure%complex0d)
     call deallocate_arr_type_temporary_nt_0ds(structure%string0d)
     call deallocate_arr_type_temporary_nt_1dr(structure%float1d)
     call deallocate_arr_type_temporary_nt_1di(structure%integer1d)
     call deallocate_arr_type_temporary_nt_1dr(structure%string1d)
     call deallocate_arr_type_temporary_nt_1dc(structure%complex1d)
     call deallocate_arr_type_temporary_nt_2dr(structure%float2d)
     call deallocate_arr_type_temporary_nt_2di(structure%integer2d)
     call deallocate_arr_type_temporary_nt_2dc(structure%complex2d)
     call deallocate_arr_type_temporary_nt_3dr(structure%float3d)
     call deallocate_arr_type_temporary_nt_3di(structure%integer3d)
     call deallocate_arr_type_temporary_nt_3dc(structure%complex3d)
     call deallocate_arr_type_temporary_nt_4dr(structure%float4d)

   end subroutine deallocate_type_temporary_nt

   subroutine deallocate_arr_type_temporary_nt(structure)

     implicit none

     type (type_temporary_nt), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt'
     end if

   end subroutine deallocate_arr_type_temporary_nt

   subroutine deallocate_type_temporary_nt_0dc(structure)

     implicit none

     type (type_temporary_nt_0dc) :: structure

     call deallocate_type_identifier(structure%identifier)

   end subroutine deallocate_type_temporary_nt_0dc

   subroutine deallocate_arr_type_temporary_nt_0dc(structure)

     implicit none

     type (type_temporary_nt_0dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_0dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_0dc'
     end if

   end subroutine deallocate_arr_type_temporary_nt_0dc

   subroutine deallocate_type_temporary_nt_0di(structure)

     implicit none

     type (type_temporary_nt_0di) :: structure

     call deallocate_type_identifier(structure%identifier)

   end subroutine deallocate_type_temporary_nt_0di

   subroutine deallocate_arr_type_temporary_nt_0di(structure)

     implicit none

     type (type_temporary_nt_0di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_0di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_0di'
     end if

   end subroutine deallocate_arr_type_temporary_nt_0di

   subroutine deallocate_type_temporary_nt_0dr(structure)

     implicit none

     type (type_temporary_nt_0dr) :: structure

     call deallocate_type_identifier(structure%identifier)

   end subroutine deallocate_type_temporary_nt_0dr

   subroutine deallocate_arr_type_temporary_nt_0dr(structure)

     implicit none

     type (type_temporary_nt_0dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_0dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_0dr'
     end if

   end subroutine deallocate_arr_type_temporary_nt_0dr

   subroutine deallocate_type_temporary_nt_0ds(structure)

     implicit none

     type (type_temporary_nt_0ds) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecstring_type(structure%value)

   end subroutine deallocate_type_temporary_nt_0ds

   subroutine deallocate_arr_type_temporary_nt_0ds(structure)

     implicit none

     type (type_temporary_nt_0ds), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_0ds(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_0ds'
     end if

   end subroutine deallocate_arr_type_temporary_nt_0ds

   subroutine deallocate_type_temporary_nt_1dc(structure)

     implicit none

     type (type_temporary_nt_1dc) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_veccplx_type(structure%value)

   end subroutine deallocate_type_temporary_nt_1dc

   subroutine deallocate_arr_type_temporary_nt_1dc(structure)

     implicit none

     type (type_temporary_nt_1dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_1dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_1dc'
     end if

   end subroutine deallocate_arr_type_temporary_nt_1dc

   subroutine deallocate_type_temporary_nt_1di(structure)

     implicit none

     type (type_temporary_nt_1di) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecint_type(structure%value)

   end subroutine deallocate_type_temporary_nt_1di

   subroutine deallocate_arr_type_temporary_nt_1di(structure)

     implicit none

     type (type_temporary_nt_1di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_1di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_1di'
     end if

   end subroutine deallocate_arr_type_temporary_nt_1di

   subroutine deallocate_type_temporary_nt_1dr(structure)

     implicit none

     type (type_temporary_nt_1dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecflt_type(structure%value)

   end subroutine deallocate_type_temporary_nt_1dr

   subroutine deallocate_arr_type_temporary_nt_1dr(structure)

     implicit none

     type (type_temporary_nt_1dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_1dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_1dr'
     end if

   end subroutine deallocate_arr_type_temporary_nt_1dr

   subroutine deallocate_type_temporary_nt_1ds(structure)

     implicit none

     type (type_temporary_nt_1ds) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecstring_type(structure%value)

   end subroutine deallocate_type_temporary_nt_1ds

   subroutine deallocate_arr_type_temporary_nt_1ds(structure)

     implicit none

     type (type_temporary_nt_1ds), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_1ds(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_1ds'
     end if

   end subroutine deallocate_arr_type_temporary_nt_1ds

   subroutine deallocate_type_temporary_nt_2dc(structure)

     implicit none

     type (type_temporary_nt_2dc) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_matcplx_type(structure%value)

   end subroutine deallocate_type_temporary_nt_2dc

   subroutine deallocate_arr_type_temporary_nt_2dc(structure)

     implicit none

     type (type_temporary_nt_2dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_2dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_2dc'
     end if

   end subroutine deallocate_arr_type_temporary_nt_2dc

   subroutine deallocate_type_temporary_nt_2di(structure)

     implicit none

     type (type_temporary_nt_2di) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_matint_type(structure%value)

   end subroutine deallocate_type_temporary_nt_2di

   subroutine deallocate_arr_type_temporary_nt_2di(structure)

     implicit none

     type (type_temporary_nt_2di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_2di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_2di'
     end if

   end subroutine deallocate_arr_type_temporary_nt_2di

   subroutine deallocate_type_temporary_nt_2dr(structure)

     implicit none

     type (type_temporary_nt_2dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_matflt_type(structure%value)

   end subroutine deallocate_type_temporary_nt_2dr

   subroutine deallocate_arr_type_temporary_nt_2dr(structure)

     implicit none

     type (type_temporary_nt_2dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_2dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_2dr'
     end if

   end subroutine deallocate_arr_type_temporary_nt_2dr

   subroutine deallocate_type_temporary_nt_3dc(structure)

     implicit none

     type (type_temporary_nt_3dc) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array3dcplx_type(structure%value)

   end subroutine deallocate_type_temporary_nt_3dc

   subroutine deallocate_arr_type_temporary_nt_3dc(structure)

     implicit none

     type (type_temporary_nt_3dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_3dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_3dc'
     end if

   end subroutine deallocate_arr_type_temporary_nt_3dc

   subroutine deallocate_type_temporary_nt_3di(structure)

     implicit none

     type (type_temporary_nt_3di) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array3dint_type(structure%value)

   end subroutine deallocate_type_temporary_nt_3di

   subroutine deallocate_arr_type_temporary_nt_3di(structure)

     implicit none

     type (type_temporary_nt_3di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_3di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_3di'
     end if

   end subroutine deallocate_arr_type_temporary_nt_3di

   subroutine deallocate_type_temporary_nt_3dr(structure)

     implicit none

     type (type_temporary_nt_3dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array3dflt_type(structure%value)

   end subroutine deallocate_type_temporary_nt_3dr

   subroutine deallocate_arr_type_temporary_nt_3dr(structure)

     implicit none

     type (type_temporary_nt_3dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_3dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_3dr'
     end if

   end subroutine deallocate_arr_type_temporary_nt_3dr

   subroutine deallocate_type_temporary_nt_4dr(structure)

     implicit none

     type (type_temporary_nt_4dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array4dflt_type(structure%value)

   end subroutine deallocate_type_temporary_nt_4dr

   subroutine deallocate_arr_type_temporary_nt_4dr(structure)

     implicit none

     type (type_temporary_nt_4dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_nt_4dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_nt_4dr'
     end if

   end subroutine deallocate_arr_type_temporary_nt_4dr

   subroutine deallocate_type_temporary_t(structure)

     implicit none

     type (type_temporary_t) :: structure

     call deallocate_arr_type_temporary_t_0dr(structure%float0d)
     call deallocate_arr_type_temporary_t_0di(structure%integer0d)
     call deallocate_arr_type_temporary_t_0dc(structure%complex0d)
     call deallocate_arr_type_temporary_t_0ds(structure%string0d)
     call deallocate_arr_type_temporary_t_1dr(structure%float1d)
     call deallocate_arr_type_temporary_t_1di(structure%integer1d)
     call deallocate_arr_type_temporary_t_1dc(structure%complex1d)
     call deallocate_arr_type_temporary_t_2dr(structure%float2d)
     call deallocate_arr_type_temporary_t_2di(structure%integer2d)
     call deallocate_arr_type_temporary_t_2dc(structure%complex2d)
     call deallocate_arr_type_temporary_t_3dr(structure%float3d)
     call deallocate_arr_type_temporary_t_3di(structure%integer3d)
     call deallocate_arr_type_temporary_t_3dc(structure%complex3d)
     call deallocate_arr_type_temporary_t_4dr(structure%float4d)

   end subroutine deallocate_type_temporary_t

   subroutine deallocate_arr_type_temporary_t(structure)

     implicit none

     type (type_temporary_t), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t'
     end if

   end subroutine deallocate_arr_type_temporary_t

   subroutine deallocate_type_temporary_t_0dc(structure)

     implicit none

     type (type_temporary_t_0dc) :: structure

     call deallocate_type_identifier(structure%identifier)

   end subroutine deallocate_type_temporary_t_0dc

   subroutine deallocate_arr_type_temporary_t_0dc(structure)

     implicit none

     type (type_temporary_t_0dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_0dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_0dc'
     end if

   end subroutine deallocate_arr_type_temporary_t_0dc

   subroutine deallocate_type_temporary_t_0di(structure)

     implicit none

     type (type_temporary_t_0di) :: structure

     call deallocate_type_identifier(structure%identifier)

   end subroutine deallocate_type_temporary_t_0di

   subroutine deallocate_arr_type_temporary_t_0di(structure)

     implicit none

     type (type_temporary_t_0di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_0di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_0di'
     end if

   end subroutine deallocate_arr_type_temporary_t_0di

   subroutine deallocate_type_temporary_t_0dr(structure)

     implicit none

     type (type_temporary_t_0dr) :: structure

     call deallocate_type_identifier(structure%identifier)

   end subroutine deallocate_type_temporary_t_0dr

   subroutine deallocate_arr_type_temporary_t_0dr(structure)

     implicit none

     type (type_temporary_t_0dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_0dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_0dr'
     end if

   end subroutine deallocate_arr_type_temporary_t_0dr

   subroutine deallocate_type_temporary_t_0ds(structure)

     implicit none

     type (type_temporary_t_0ds) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecstring_type(structure%value)

   end subroutine deallocate_type_temporary_t_0ds

   subroutine deallocate_arr_type_temporary_t_0ds(structure)

     implicit none

     type (type_temporary_t_0ds), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_0ds(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_0ds'
     end if

   end subroutine deallocate_arr_type_temporary_t_0ds

   subroutine deallocate_type_temporary_t_1dc(structure)

     implicit none

     type (type_temporary_t_1dc) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_veccplx_type(structure%value)

   end subroutine deallocate_type_temporary_t_1dc

   subroutine deallocate_arr_type_temporary_t_1dc(structure)

     implicit none

     type (type_temporary_t_1dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_1dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_1dc'
     end if

   end subroutine deallocate_arr_type_temporary_t_1dc

   subroutine deallocate_type_temporary_t_1di(structure)

     implicit none

     type (type_temporary_t_1di) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecint_type(structure%value)

   end subroutine deallocate_type_temporary_t_1di

   subroutine deallocate_arr_type_temporary_t_1di(structure)

     implicit none

     type (type_temporary_t_1di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_1di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_1di'
     end if

   end subroutine deallocate_arr_type_temporary_t_1di

   subroutine deallocate_type_temporary_t_1dr(structure)

     implicit none

     type (type_temporary_t_1dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_vecflt_type(structure%value)

   end subroutine deallocate_type_temporary_t_1dr

   subroutine deallocate_arr_type_temporary_t_1dr(structure)

     implicit none

     type (type_temporary_t_1dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_1dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_1dr'
     end if

   end subroutine deallocate_arr_type_temporary_t_1dr

   subroutine deallocate_type_temporary_t_2dc(structure)

     implicit none

     type (type_temporary_t_2dc) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_matcplx_type(structure%value)

   end subroutine deallocate_type_temporary_t_2dc

   subroutine deallocate_arr_type_temporary_t_2dc(structure)

     implicit none

     type (type_temporary_t_2dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_2dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_2dc'
     end if

   end subroutine deallocate_arr_type_temporary_t_2dc

   subroutine deallocate_type_temporary_t_2di(structure)

     implicit none

     type (type_temporary_t_2di) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_matint_type(structure%value)

   end subroutine deallocate_type_temporary_t_2di

   subroutine deallocate_arr_type_temporary_t_2di(structure)

     implicit none

     type (type_temporary_t_2di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_2di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_2di'
     end if

   end subroutine deallocate_arr_type_temporary_t_2di

   subroutine deallocate_type_temporary_t_2dr(structure)

     implicit none

     type (type_temporary_t_2dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_matflt_type(structure%value)

   end subroutine deallocate_type_temporary_t_2dr

   subroutine deallocate_arr_type_temporary_t_2dr(structure)

     implicit none

     type (type_temporary_t_2dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_2dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_2dr'
     end if

   end subroutine deallocate_arr_type_temporary_t_2dr

   subroutine deallocate_type_temporary_t_3dc(structure)

     implicit none

     type (type_temporary_t_3dc) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array3dcplx_type(structure%value)

   end subroutine deallocate_type_temporary_t_3dc

   subroutine deallocate_arr_type_temporary_t_3dc(structure)

     implicit none

     type (type_temporary_t_3dc), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_3dc(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_3dc'
     end if

   end subroutine deallocate_arr_type_temporary_t_3dc

   subroutine deallocate_type_temporary_t_3di(structure)

     implicit none

     type (type_temporary_t_3di) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array3dint_type(structure%value)

   end subroutine deallocate_type_temporary_t_3di

   subroutine deallocate_arr_type_temporary_t_3di(structure)

     implicit none

     type (type_temporary_t_3di), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_3di(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_3di'
     end if

   end subroutine deallocate_arr_type_temporary_t_3di

   subroutine deallocate_type_temporary_t_3dr(structure)

     implicit none

     type (type_temporary_t_3dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array3dflt_type(structure%value)

   end subroutine deallocate_type_temporary_t_3dr

   subroutine deallocate_arr_type_temporary_t_3dr(structure)

     implicit none

     type (type_temporary_t_3dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_3dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_3dr'
     end if

   end subroutine deallocate_arr_type_temporary_t_3dr

   subroutine deallocate_type_temporary_t_4dr(structure)

     implicit none

     type (type_temporary_t_4dr) :: structure

     call deallocate_type_identifier(structure%identifier)
     call deallocate_type_array4dflt_type(structure%value)

   end subroutine deallocate_type_temporary_t_4dr

   subroutine deallocate_arr_type_temporary_t_4dr(structure)

     implicit none

     type (type_temporary_t_4dr), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_temporary_t_4dr(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_temporary_t_4dr'
     end if

   end subroutine deallocate_arr_type_temporary_t_4dr

   subroutine deallocate_type_tf_desc_tfcoils(structure)

     implicit none

     type (type_tf_desc_tfcoils) :: structure

     call deallocate_type_circularcoil(structure%circularcoil)
     call deallocate_type_planecoil(structure%planecoil)
     call deallocate_type_tf_structure(structure%inboard)
     call deallocate_type_tf_structure(structure%outboard)

   end subroutine deallocate_type_tf_desc_tfcoils

   subroutine deallocate_arr_type_tf_desc_tfcoils(structure)

     implicit none

     type (type_tf_desc_tfcoils), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tf_desc_tfcoils(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tf_desc_tfcoils'
     end if

   end subroutine deallocate_arr_type_tf_desc_tfcoils

   subroutine deallocate_type_tf_desc_tfcoils_board(structure)

     implicit none

     type (type_tf_desc_tfcoils_board) :: structure

     call deallocate_type_tf_structure(structure%structure)

   end subroutine deallocate_type_tf_desc_tfcoils_board

   subroutine deallocate_arr_type_tf_desc_tfcoils_board(structure)

     implicit none

     type (type_tf_desc_tfcoils_board), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tf_desc_tfcoils_board(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tf_desc_tfcoils_board'
     end if

   end subroutine deallocate_arr_type_tf_desc_tfcoils_board

   subroutine deallocate_type_tf_structure(structure)

     implicit none

     type (type_tf_structure) :: structure


   end subroutine deallocate_type_tf_structure

   subroutine deallocate_arr_type_tf_structure(structure)

     implicit none

     type (type_tf_structure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tf_structure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tf_structure'
     end if

   end subroutine deallocate_arr_type_tf_structure

   subroutine deallocate_type_theta_info(structure)

     implicit none

     type (type_theta_info) :: structure

     call deallocate_type_matflt_type(structure%th2th_pol)

   end subroutine deallocate_type_theta_info

   subroutine deallocate_arr_type_theta_info(structure)

     implicit none

     type (type_theta_info), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_theta_info(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_theta_info'
     end if

   end subroutine deallocate_arr_type_theta_info

   subroutine deallocate_type_topo_regions(structure)

     implicit none

     type (type_topo_regions) :: structure

     call deallocate_type_array6dflt_type(structure%dim1)
     call deallocate_type_array6dflt_type(structure%dim2)
     call deallocate_type_array6dflt_type(structure%dim3)
     call deallocate_type_array6dflt_type(structure%dim4)
     call deallocate_type_array6dflt_type(structure%dim5)
     call deallocate_type_array6dflt_type(structure%dim6)
     call deallocate_type_array6dflt_type(structure%jacobian)
     call deallocate_type_array6dflt_type(structure%distfunc)

   end subroutine deallocate_type_topo_regions

   subroutine deallocate_arr_type_topo_regions(structure)

     implicit none

     type (type_topo_regions), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_topo_regions(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_topo_regions'
     end if

   end subroutine deallocate_arr_type_topo_regions

   subroutine deallocate_type_toroid_field(structure)

     implicit none

     type (type_toroid_field) :: structure


   end subroutine deallocate_type_toroid_field

   subroutine deallocate_arr_type_toroid_field(structure)

     implicit none

     type (type_toroid_field), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_toroid_field(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_toroid_field'
     end if

   end subroutine deallocate_arr_type_toroid_field

   subroutine deallocate_type_trace(structure)

     implicit none

     type (type_trace) :: structure

     call deallocate_type_matflt_type(structure%time_orb)
     call deallocate_type_vecint_type(structure%ntorb)
     call deallocate_type_matflt_type(structure%r)
     call deallocate_type_matflt_type(structure%z)
     call deallocate_type_matflt_type(structure%phi)
     call deallocate_type_matflt_type(structure%psi)
     call deallocate_type_matflt_type(structure%theta_b)
     call deallocate_type_matflt_type(structure%v_parallel)
     call deallocate_type_matflt_type(structure%v_perp)

   end subroutine deallocate_type_trace

   subroutine deallocate_arr_type_trace(structure)

     implicit none

     type (type_trace), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_trace(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_trace'
     end if

   end subroutine deallocate_arr_type_trace

   subroutine deallocate_type_transcoefel(structure)

     implicit none

     type (type_transcoefel) :: structure

     call deallocate_type_vecflt_type(structure%diff_eff)
     call deallocate_type_vecflt_type(structure%vconv_eff)
     call deallocate_type_vecflt_type(structure%flux)
     call deallocate_type_offdiagel(structure%off_diagonal)

   end subroutine deallocate_type_transcoefel

   subroutine deallocate_arr_type_transcoefel(structure)

     implicit none

     type (type_transcoefel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_transcoefel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_transcoefel'
     end if

   end subroutine deallocate_arr_type_transcoefel

   subroutine deallocate_type_transcoefimp(structure)

     implicit none

     type (type_transcoefimp) :: structure

     call deallocate_type_matflt_type(structure%diff_eff)
     call deallocate_type_matflt_type(structure%vconv_eff)
     call deallocate_type_matflt_type(structure%exchange)
     call deallocate_type_matflt_type(structure%flux)

   end subroutine deallocate_type_transcoefimp

   subroutine deallocate_arr_type_transcoefimp(structure)

     implicit none

     type (type_transcoefimp), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_transcoefimp(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_transcoefimp'
     end if

   end subroutine deallocate_arr_type_transcoefimp

   subroutine deallocate_type_transcoefion(structure)

     implicit none

     type (type_transcoefion) :: structure

     call deallocate_type_matflt_type(structure%diff_eff)
     call deallocate_type_matflt_type(structure%vconv_eff)
     call deallocate_type_matflt_type(structure%exchange)
     call deallocate_type_matflt_type(structure%qgi)
     call deallocate_type_matflt_type(structure%flux)
     call deallocate_type_offdiagion(structure%off_diagonal)

   end subroutine deallocate_type_transcoefion

   subroutine deallocate_arr_type_transcoefion(structure)

     implicit none

     type (type_transcoefion), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_transcoefion(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_transcoefion'
     end if

   end subroutine deallocate_arr_type_transcoefion

   subroutine deallocate_type_transcoefvtor(structure)

     implicit none

     type (type_transcoefvtor) :: structure

     call deallocate_type_matflt_type(structure%diff_eff)
     call deallocate_type_matflt_type(structure%vconv_eff)
     call deallocate_type_matflt_type(structure%flux)
     call deallocate_type_offdiagion(structure%off_diagonal)

   end subroutine deallocate_type_transcoefvtor

   subroutine deallocate_arr_type_transcoefvtor(structure)

     implicit none

     type (type_transcoefvtor), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_transcoefvtor(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_transcoefvtor'
     end if

   end subroutine deallocate_arr_type_transcoefvtor

   subroutine deallocate_type_trap_type(structure)

     implicit none

     type (type_trap_type) :: structure

     call deallocate_type_identifier(structure%trap_id)
     call deallocate_type_matflt_type(structure%fill_factor)
     call deallocate_type_matflt_type(structure%density)

   end subroutine deallocate_type_trap_type

   subroutine deallocate_arr_type_trap_type(structure)

     implicit none

     type (type_trap_type), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_trap_type(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_trap_type'
     end if

   end subroutine deallocate_arr_type_trap_type

   subroutine deallocate_type_trianglexyz(structure)

     implicit none

     type (type_trianglexyz) :: structure

     call deallocate_type_xyz0D(structure%point1)
     call deallocate_type_xyz0D(structure%point2)
     call deallocate_type_xyz0D(structure%point3)

   end subroutine deallocate_type_trianglexyz

   subroutine deallocate_arr_type_trianglexyz(structure)

     implicit none

     type (type_trianglexyz), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_trianglexyz(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_trianglexyz'
     end if

   end subroutine deallocate_arr_type_trianglexyz

   subroutine deallocate_type_tsmeasure(structure)

     implicit none

     type (type_tsmeasure) :: structure

     call deallocate_type_exp1D(structure%te)
     call deallocate_type_exp1D(structure%ne)

   end subroutine deallocate_type_tsmeasure

   subroutine deallocate_arr_type_tsmeasure(structure)

     implicit none

     type (type_tsmeasure), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tsmeasure(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tsmeasure'
     end if

   end subroutine deallocate_arr_type_tsmeasure

   subroutine deallocate_type_tssetup(structure)

     implicit none

     type (type_tssetup) :: structure

     call deallocate_type_rzphi1D(structure%position)

   end subroutine deallocate_type_tssetup

   subroutine deallocate_arr_type_tssetup(structure)

     implicit none

     type (type_tssetup), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_tssetup(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_tssetup'
     end if

   end subroutine deallocate_arr_type_tssetup

   subroutine deallocate_type_turbcomposition(structure)

     implicit none

     type (type_turbcomposition) :: structure

     call deallocate_type_vecflt_type(structure%amn)
     call deallocate_type_vecflt_type(structure%zn)
     call deallocate_type_vecflt_type(structure%zion)
     call deallocate_type_vecflt_type(structure%ie_mass)

   end subroutine deallocate_type_turbcomposition

   subroutine deallocate_arr_type_turbcomposition(structure)

     implicit none

     type (type_turbcomposition), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbcomposition(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbcomposition'
     end if

   end subroutine deallocate_arr_type_turbcomposition

   subroutine deallocate_type_turbcoordsys(structure)

     implicit none

     type (type_turbcoordsys) :: structure

     call deallocate_type_vecstring_type(structure%grid_type)
     call deallocate_type_turbgrid(structure%turbgrid)
     call deallocate_type_matflt_type(structure%jacobian)
     call deallocate_type_matflt_type(structure%g_11)
     call deallocate_type_matflt_type(structure%g_12)
     call deallocate_type_matflt_type(structure%g_13)
     call deallocate_type_matflt_type(structure%g_22)
     call deallocate_type_matflt_type(structure%g_23)
     call deallocate_type_matflt_type(structure%g_33)
     call deallocate_type_rzphi3D(structure%position)

   end subroutine deallocate_type_turbcoordsys

   subroutine deallocate_arr_type_turbcoordsys(structure)

     implicit none

     type (type_turbcoordsys), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbcoordsys(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbcoordsys'
     end if

   end subroutine deallocate_arr_type_turbcoordsys

   subroutine deallocate_type_turbenv1d(structure)

     implicit none

     type (type_turbenv1d) :: structure

     call deallocate_type_vecflt_type(structure%theta)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%vor)
     call deallocate_type_vecflt_type(structure%jpl)
     call deallocate_type_vecflt_type(structure%ne)
     call deallocate_type_vecflt_type(structure%he)
     call deallocate_type_vecflt_type(structure%te)
     call deallocate_type_matflt_type(structure%ni)
     call deallocate_type_matflt_type(structure%ti)
     call deallocate_type_matflt_type(structure%ui)
     call deallocate_type_vecflt_type(structure%fe)
     call deallocate_type_vecflt_type(structure%qe)
     call deallocate_type_matflt_type(structure%qi)
     call deallocate_type_vecflt_type(structure%me)
     call deallocate_type_matflt_type(structure%mi)

   end subroutine deallocate_type_turbenv1d

   subroutine deallocate_arr_type_turbenv1d(structure)

     implicit none

     type (type_turbenv1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbenv1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbenv1d'
     end if

   end subroutine deallocate_arr_type_turbenv1d

   subroutine deallocate_type_turbgrid(structure)

     implicit none

     type (type_turbgrid) :: structure

     call deallocate_type_vecflt_type(structure%dim1)
     call deallocate_type_vecflt_type(structure%dim2)
     call deallocate_type_vecflt_type(structure%dim3)
     call deallocate_type_vecflt_type(structure%dim_v1)
     call deallocate_type_vecflt_type(structure%dim_v2)

   end subroutine deallocate_type_turbgrid

   subroutine deallocate_arr_type_turbgrid(structure)

     implicit none

     type (type_turbgrid), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbgrid(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbgrid'
     end if

   end subroutine deallocate_arr_type_turbgrid

   subroutine deallocate_type_turbspec1d(structure)

     implicit none

     type (type_turbspec1d) :: structure

     call deallocate_type_vecflt_type(structure%kperp)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%vor)
     call deallocate_type_vecflt_type(structure%b)
     call deallocate_type_vecflt_type(structure%jpl)
     call deallocate_type_vecflt_type(structure%ne)
     call deallocate_type_vecflt_type(structure%te)
     call deallocate_type_matflt_type(structure%ti)
     call deallocate_type_vecflt_type(structure%fe)
     call deallocate_type_vecflt_type(structure%qe)
     call deallocate_type_matflt_type(structure%qi)
     call deallocate_type_vecflt_type(structure%me)
     call deallocate_type_matflt_type(structure%mi)

   end subroutine deallocate_type_turbspec1d

   subroutine deallocate_arr_type_turbspec1d(structure)

     implicit none

     type (type_turbspec1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbspec1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbspec1d'
     end if

   end subroutine deallocate_arr_type_turbspec1d

   subroutine deallocate_type_turbvar0d(structure)

     implicit none

     type (type_turbvar0d) :: structure

     call deallocate_type_vecstring_type(structure%dtime_type)
     call deallocate_type_vecflt_type(structure%dtime)
     call deallocate_type_vecflt_type(structure%en_exb)
     call deallocate_type_vecflt_type(structure%en_mag)
     call deallocate_type_vecflt_type(structure%en_el_th)
     call deallocate_type_matflt_type(structure%en_ion_th)
     call deallocate_type_vecflt_type(structure%en_el_par)
     call deallocate_type_matflt_type(structure%en_ion_par)
     call deallocate_type_vecflt_type(structure%en_tot)
     call deallocate_type_vecflt_type(structure%fl_el)
     call deallocate_type_vecflt_type(structure%fl_heatel)
     call deallocate_type_matflt_type(structure%fl_ion)
     call deallocate_type_matflt_type(structure%fl_heation)
     call deallocate_type_vecflt_type(structure%fl_magel)
     call deallocate_type_vecflt_type(structure%fl_magheatel)
     call deallocate_type_matflt_type(structure%fl_magion)
     call deallocate_type_matflt_type(structure%flmagheation)

   end subroutine deallocate_type_turbvar0d

   subroutine deallocate_arr_type_turbvar0d(structure)

     implicit none

     type (type_turbvar0d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbvar0d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbvar0d'
     end if

   end subroutine deallocate_arr_type_turbvar0d

   subroutine deallocate_type_turbvar1d(structure)

     implicit none

     type (type_turbvar1d) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%er)
     call deallocate_type_vecflt_type(structure%vor)
     call deallocate_type_vecflt_type(structure%apl)
     call deallocate_type_vecflt_type(structure%jpl)
     call deallocate_type_vecflt_type(structure%ne)
     call deallocate_type_vecflt_type(structure%te)
     call deallocate_type_matflt_type(structure%ni)
     call deallocate_type_matflt_type(structure%ti)
     call deallocate_type_matflt_type(structure%ui)

   end subroutine deallocate_type_turbvar1d

   subroutine deallocate_arr_type_turbvar1d(structure)

     implicit none

     type (type_turbvar1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbvar1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbvar1d'
     end if

   end subroutine deallocate_arr_type_turbvar1d

   subroutine deallocate_type_turbvar2d(structure)

     implicit none

     type (type_turbvar2d) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%theta)
     call deallocate_type_matflt_type(structure%phi)
     call deallocate_type_matflt_type(structure%apl)
     call deallocate_type_matflt_type(structure%jpl)
     call deallocate_type_matflt_type(structure%vor)
     call deallocate_type_matflt_type(structure%ne)
     call deallocate_type_matflt_type(structure%te)
     call deallocate_type_array3dflt_type(structure%ni)
     call deallocate_type_array3dflt_type(structure%ti)
     call deallocate_type_array3dflt_type(structure%ui)

   end subroutine deallocate_type_turbvar2d

   subroutine deallocate_arr_type_turbvar2d(structure)

     implicit none

     type (type_turbvar2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbvar2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbvar2d'
     end if

   end subroutine deallocate_arr_type_turbvar2d

   subroutine deallocate_type_turbvar3d(structure)

     implicit none

     type (type_turbvar3d) :: structure

     call deallocate_type_array3dflt_type(structure%phi)
     call deallocate_type_array3dflt_type(structure%vor)
     call deallocate_type_array3dflt_type(structure%jpl)
     call deallocate_type_array3dflt_type(structure%ne)

   end subroutine deallocate_type_turbvar3d

   subroutine deallocate_arr_type_turbvar3d(structure)

     implicit none

     type (type_turbvar3d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbvar3d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbvar3d'
     end if

   end subroutine deallocate_arr_type_turbvar3d

   subroutine deallocate_type_turbvar4d(structure)

     implicit none

     type (type_turbvar4d) :: structure

     call deallocate_type_array4dflt_type(structure%fe)
     call deallocate_type_array5dflt_type(structure%fi)

   end subroutine deallocate_type_turbvar4d

   subroutine deallocate_arr_type_turbvar4d(structure)

     implicit none

     type (type_turbvar4d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbvar4d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbvar4d'
     end if

   end subroutine deallocate_arr_type_turbvar4d

   subroutine deallocate_type_turbvar5d(structure)

     implicit none

     type (type_turbvar5d) :: structure

     call deallocate_type_array5dflt_type(structure%fe)
     call deallocate_type_array6dflt_type(structure%fi)

   end subroutine deallocate_type_turbvar5d

   subroutine deallocate_arr_type_turbvar5d(structure)

     implicit none

     type (type_turbvar5d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_turbvar5d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_turbvar5d'
     end if

   end subroutine deallocate_arr_type_turbvar5d

   subroutine deallocate_type_version_ind(structure)

     implicit none

     type (type_version_ind) :: structure

     call deallocate_type_vecstring_type(structure%description)
     call deallocate_type_vecstring_type(structure%releasedate)
     call deallocate_arr_type_data_release(structure%data_release)

   end subroutine deallocate_type_version_ind

   subroutine deallocate_arr_type_version_ind(structure)

     implicit none

     type (type_version_ind), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_version_ind(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_version_ind'
     end if

   end subroutine deallocate_arr_type_version_ind

   subroutine deallocate_type_wall2d(structure)

     implicit none

     type (type_wall2d) :: structure

     call deallocate_type_identifier(structure%wall_id)
     call deallocate_type_wall_limiter(structure%limiter)
     call deallocate_type_wall_vessel(structure%vessel)
     call deallocate_arr_type_plasmaComplexType(structure%plasma)
     call deallocate_arr_type_wall_unitsComplexType(structure%wall_state)

   end subroutine deallocate_type_wall2d

   subroutine deallocate_arr_type_wall2d(structure)

     implicit none

     type (type_wall2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall2d'
     end if

   end subroutine deallocate_arr_type_wall2d

   subroutine deallocate_type_wall2d_mhd(structure)

     implicit none

     type (type_wall2d_mhd) :: structure

     call deallocate_arr_type_mhd_res_wall2d(structure%res_wall)
     call deallocate_type_mhd_ideal_wall2d(structure%ideal_wall)

   end subroutine deallocate_type_wall2d_mhd

   subroutine deallocate_arr_type_wall2d_mhd(structure)

     implicit none

     type (type_wall2d_mhd), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall2d_mhd(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall2d_mhd'
     end if

   end subroutine deallocate_arr_type_wall2d_mhd

   subroutine deallocate_type_wall3d(structure)

     implicit none

     type (type_wall3d) :: structure

     call deallocate_type_identifier(structure%wall_id)
     call deallocate_type_complexgrid(structure%grid)
     call deallocate_arr_type_plasmaComplexType(structure%plasma)
     call deallocate_arr_type_wall_unitsComplexType(structure%wall_state)

   end subroutine deallocate_type_wall3d

   subroutine deallocate_arr_type_wall3d(structure)

     implicit none

     type (type_wall3d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall3d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall3d'
     end if

   end subroutine deallocate_arr_type_wall3d

   subroutine deallocate_type_wall_blocks(structure)

     implicit none

     type (type_wall_blocks) :: structure

     call deallocate_arr_type_wall_blocks_unit(structure%blocks_unit)

   end subroutine deallocate_type_wall_blocks

   subroutine deallocate_arr_type_wall_blocks(structure)

     implicit none

     type (type_wall_blocks), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_blocks(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_blocks'
     end if

   end subroutine deallocate_arr_type_wall_blocks

   subroutine deallocate_type_wall_blocks_unit(structure)

     implicit none

     type (type_wall_blocks_unit) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_rz1D(structure%position)

   end subroutine deallocate_type_wall_blocks_unit

   subroutine deallocate_arr_type_wall_blocks_unit(structure)

     implicit none

     type (type_wall_blocks_unit), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_blocks_unit(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_blocks_unit'
     end if

   end subroutine deallocate_arr_type_wall_blocks_unit

   subroutine deallocate_type_wall_limiter(structure)

     implicit none

     type (type_wall_limiter) :: structure

     call deallocate_type_identifier(structure%limiter_id)
     call deallocate_arr_type_limiter_unit(structure%limiter_unit)

   end subroutine deallocate_type_wall_limiter

   subroutine deallocate_arr_type_wall_limiter(structure)

     implicit none

     type (type_wall_limiter), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_limiter(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_limiter'
     end if

   end subroutine deallocate_arr_type_wall_limiter

   subroutine deallocate_type_wall_types(structure)

     implicit none

     type (type_wall_types) :: structure

     call deallocate_type_vecstring_type(structure%label)
     call deallocate_arr_type_wall_types_layers(structure%layers)

   end subroutine deallocate_type_wall_types

   subroutine deallocate_arr_type_wall_types(structure)

     implicit none

     type (type_wall_types), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_types(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_types'
     end if

   end subroutine deallocate_arr_type_wall_types

   subroutine deallocate_type_wall_types_layers(structure)

     implicit none

     type (type_wall_types_layers) :: structure

     call deallocate_type_vecflt_type(structure%chem_comp)

   end subroutine deallocate_type_wall_types_layers

   subroutine deallocate_arr_type_wall_types_layers(structure)

     implicit none

     type (type_wall_types_layers), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_types_layers(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_types_layers'
     end if

   end subroutine deallocate_arr_type_wall_types_layers

   subroutine deallocate_type_wall_unitsComplexType(structure)

     implicit none

     type (type_wall_unitsComplexType) :: structure

     call deallocate_arr_type_wall_unitsComplexType_layers(structure%layers)
     call deallocate_type_complexgrid_scalar(structure%eta)
     call deallocate_type_complexgrid_scalar(structure%permeability)
     call deallocate_type_complexgrid_vector(structure%j)

   end subroutine deallocate_type_wall_unitsComplexType

   subroutine deallocate_arr_type_wall_unitsComplexType(structure)

     implicit none

     type (type_wall_unitsComplexType), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_unitsComplexType(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_unitsComplexType'
     end if

   end subroutine deallocate_arr_type_wall_unitsComplexType

   subroutine deallocate_type_wall_unitsComplexType_layers(structure)

     implicit none

     type (type_wall_unitsComplexType_layers) :: structure

     call deallocate_type_vecint_type(structure%elements)
     call deallocate_type_vecint_type(structure%gases)
     call deallocate_type_vecint_type(structure%compounds)
     call deallocate_type_matflt_type(structure%density)
     call deallocate_type_matflt_type(structure%dx)
     call deallocate_type_vecflt_type(structure%thickness)
     call deallocate_type_array3dflt_type(structure%roughness)
     call deallocate_type_array3dflt_type(structure%porosity)
     call deallocate_type_matflt_type(structure%dpa)
     call deallocate_type_matflt_type(structure%temperature)
     call deallocate_type_array3dflt_type(structure%element_frac)
     call deallocate_type_array3dflt_type(structure%chem_comp)
     call deallocate_type_array4dflt_type(structure%bulk_D)
     call deallocate_type_array4dflt_type(structure%surface_D)
     call deallocate_type_array4dflt_type(structure%bulk_solute)
     call deallocate_type_array4dflt_type(structure%surf_solute)
     call deallocate_type_array3dflt_type(structure%pore_content)
     call deallocate_arr_type_trap_type(structure%trap_type)

   end subroutine deallocate_type_wall_unitsComplexType_layers

   subroutine deallocate_arr_type_wall_unitsComplexType_layers(structure)

     implicit none

     type (type_wall_unitsComplexType_layers), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_unitsComplexType_layers(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_unitsComplexType_layers'
     end if

   end subroutine deallocate_arr_type_wall_unitsComplexType_layers

   subroutine deallocate_type_wall_vessel(structure)

     implicit none

     type (type_wall_vessel) :: structure

     call deallocate_type_identifier(structure%vessel_id)
     call deallocate_arr_type_wall_vessel_unit(structure%vessel_unit)

   end subroutine deallocate_type_wall_vessel

   subroutine deallocate_arr_type_wall_vessel(structure)

     implicit none

     type (type_wall_vessel), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_vessel(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_vessel'
     end if

   end subroutine deallocate_arr_type_wall_vessel

   subroutine deallocate_type_wall_vessel_annular(structure)

     implicit none

     type (type_wall_vessel_annular) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_rz1D(structure%inside)
     call deallocate_type_rz1D(structure%outside)

   end subroutine deallocate_type_wall_vessel_annular

   subroutine deallocate_arr_type_wall_vessel_annular(structure)

     implicit none

     type (type_wall_vessel_annular), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_vessel_annular(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_vessel_annular'
     end if

   end subroutine deallocate_arr_type_wall_vessel_annular

   subroutine deallocate_type_wall_vessel_unit(structure)

     implicit none

     type (type_wall_vessel_unit) :: structure

     call deallocate_type_wall_vessel_annular(structure%annular)
     call deallocate_type_wall_blocks(structure%blocks)
     call deallocate_type_wall_wall2d_vessel_radial_build(structure%radial_build)

   end subroutine deallocate_type_wall_vessel_unit

   subroutine deallocate_arr_type_wall_vessel_unit(structure)

     implicit none

     type (type_wall_vessel_unit), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_vessel_unit(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_vessel_unit'
     end if

   end subroutine deallocate_arr_type_wall_vessel_unit

   subroutine deallocate_type_wall_wall0d(structure)

     implicit none

     type (type_wall_wall0d) :: structure

     call deallocate_type_vecflt_type(structure%pumping_speed)
     call deallocate_type_vecflt_type(structure%gas_puff)
     call deallocate_type_vecflt_type(structure%wall_inventory)
     call deallocate_type_vecflt_type(structure%recycling_coefficient)
     call deallocate_type_wall_wall0d_plasma(structure%plasma)

   end subroutine deallocate_type_wall_wall0d

   subroutine deallocate_arr_type_wall_wall0d(structure)

     implicit none

     type (type_wall_wall0d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_wall0d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_wall0d'
     end if

   end subroutine deallocate_arr_type_wall_wall0d

   subroutine deallocate_type_wall_wall0d_plasma(structure)

     implicit none

     type (type_wall_wall0d_plasma) :: structure

     call deallocate_type_matint_type(structure%species_index)
     call deallocate_type_vecflt_type(structure%flux)
     call deallocate_type_vecflt_type(structure%energy)

   end subroutine deallocate_type_wall_wall0d_plasma

   subroutine deallocate_arr_type_wall_wall0d_plasma(structure)

     implicit none

     type (type_wall_wall0d_plasma), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_wall0d_plasma(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_wall0d_plasma'
     end if

   end subroutine deallocate_arr_type_wall_wall0d_plasma

   subroutine deallocate_type_wall_wall2d_vessel_radial_build(structure)

     implicit none

     type (type_wall_wall2d_vessel_radial_build) :: structure

     call deallocate_type_vecflt_type(structure%composition)

   end subroutine deallocate_type_wall_wall2d_vessel_radial_build

   subroutine deallocate_arr_type_wall_wall2d_vessel_radial_build(structure)

     implicit none

     type (type_wall_wall2d_vessel_radial_build), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_wall_wall2d_vessel_radial_build(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_wall_wall2d_vessel_radial_build'
     end if

   end subroutine deallocate_arr_type_wall_wall2d_vessel_radial_build

   subroutine deallocate_type_waveguides(structure)

     implicit none

     type (type_waveguides) :: structure

     call deallocate_type_vecint_type(structure%mask)
     call deallocate_type_vecflt_type(structure%e_phi)
     call deallocate_type_vecflt_type(structure%scl)

   end subroutine deallocate_type_waveguides

   subroutine deallocate_arr_type_waveguides(structure)

     implicit none

     type (type_waveguides), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waveguides(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waveguides'
     end if

   end subroutine deallocate_arr_type_waveguides

   subroutine deallocate_type_waves_global_param(structure)

     implicit none

     type (type_waves_global_param) :: structure

     call deallocate_type_vecstring_type(structure%name)
     call deallocate_type_vecstring_type(structure%type)
     call deallocate_type_vecint_type(structure%f_assumption)
     call deallocate_type_vecint_type(structure%ntor)
     call deallocate_type_vecflt_type(structure%p_frac_ntor)
     call deallocate_type_vecflt_type(structure%pow_i)
     call deallocate_type_matflt_type(structure%pow_z)
     call deallocate_type_vecflt_type(structure%pow_fi)
     call deallocate_type_matflt_type(structure%pow_fz)
     call deallocate_type_vecflt_type(structure%pow_ntor_e)
     call deallocate_type_matflt_type(structure%pow_ntor_i)
     call deallocate_type_array3dflt_type(structure%pow_ntor_z)
     call deallocate_type_vecflt_type(structure%pow_ntor_fe)
     call deallocate_type_matflt_type(structure%pow_ntor_fi)
     call deallocate_type_array3dflt_type(structure%pow_ntor_fz)
     call deallocate_type_vecflt_type(structure%cur_tor_ntor)
     call deallocate_type_rz0D(structure%mag_axis)
     call deallocate_type_b0r0(structure%toroid_field)

   end subroutine deallocate_type_waves_global_param

   subroutine deallocate_arr_type_waves_global_param(structure)

     implicit none

     type (type_waves_global_param), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_global_param(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_global_param'
     end if

   end subroutine deallocate_arr_type_waves_global_param

   subroutine deallocate_type_waves_grid_1d(structure)

     implicit none

     type (type_waves_grid_1d) :: structure

     call deallocate_type_vecflt_type(structure%rho_tor)
     call deallocate_type_vecflt_type(structure%rho_tor_norm)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%volume)
     call deallocate_type_vecflt_type(structure%area)

   end subroutine deallocate_type_waves_grid_1d

   subroutine deallocate_arr_type_waves_grid_1d(structure)

     implicit none

     type (type_waves_grid_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_grid_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_grid_1d'
     end if

   end subroutine deallocate_arr_type_waves_grid_1d

   subroutine deallocate_type_waves_grid_2d(structure)

     implicit none

     type (type_waves_grid_2d) :: structure

     call deallocate_type_matflt_type(structure%rho_tor_norm)
     call deallocate_type_matflt_type(structure%rho_tor)
     call deallocate_type_matflt_type(structure%psi)
     call deallocate_type_matflt_type(structure%theta)
     call deallocate_type_matflt_type(structure%r)
     call deallocate_type_matflt_type(structure%z)
     call deallocate_type_theta_info(structure%theta_info)

   end subroutine deallocate_type_waves_grid_2d

   subroutine deallocate_arr_type_waves_grid_2d(structure)

     implicit none

     type (type_waves_grid_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_grid_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_grid_2d'
     end if

   end subroutine deallocate_arr_type_waves_grid_2d

   subroutine deallocate_type_waves_profiles_1d(structure)

     implicit none

     type (type_waves_profiles_1d) :: structure

     call deallocate_type_vecflt_type(structure%powd_tot)
     call deallocate_type_vecflt_type(structure%powd_e)
     call deallocate_type_matflt_type(structure%powd_i)
     call deallocate_type_array3dflt_type(structure%powd_z)
     call deallocate_type_vecflt_type(structure%powd_fe)
     call deallocate_type_matflt_type(structure%powd_fi)
     call deallocate_type_array3dflt_type(structure%powd_fz)
     call deallocate_type_matflt_type(structure%powd_ntor)
     call deallocate_type_matflt_type(structure%powd_ntor_e)
     call deallocate_type_array3dflt_type(structure%powd_ntor_i)
     call deallocate_type_array4dflt_type(structure%powd_ntor_z)
     call deallocate_type_matflt_type(structure%powd_ntor_fe)
     call deallocate_type_array3dflt_type(structure%powd_ntor_fi)
     call deallocate_type_array4dflt_type(structure%powd_ntor_fz)
     call deallocate_type_vecflt_type(structure%curd_tor)
     call deallocate_type_matflt_type(structure%curd_torntor)
     call deallocate_type_vecflt_type(structure%pow_tot)
     call deallocate_type_vecflt_type(structure%pow_e)
     call deallocate_type_matflt_type(structure%pow_i)
     call deallocate_type_array3dflt_type(structure%pow_z)
     call deallocate_type_vecflt_type(structure%pow_fe)
     call deallocate_type_matflt_type(structure%pow_fi)
     call deallocate_type_array3dflt_type(structure%pow_fz)
     call deallocate_type_matflt_type(structure%pow_ntor)
     call deallocate_type_matflt_type(structure%pow_ntor_e)
     call deallocate_type_array3dflt_type(structure%pow_ntor_i)
     call deallocate_type_array3dflt_type(structure%pow_ntor_z)
     call deallocate_type_matflt_type(structure%pow_ntor_fe)
     call deallocate_type_array3dflt_type(structure%pow_ntor_fi)
     call deallocate_type_array3dflt_type(structure%pow_ntor_fz)
     call deallocate_type_vecflt_type(structure%curd_par)
     call deallocate_type_matflt_type(structure%curd_parntor)
     call deallocate_type_vecflt_type(structure%cur_tor)
     call deallocate_type_matflt_type(structure%cur_tor_ntor)
     call deallocate_type_matflt_type(structure%e_plus_ave)
     call deallocate_type_matflt_type(structure%e_minus_ave)
     call deallocate_type_matflt_type(structure%e_para_ave)
     call deallocate_type_matflt_type(structure%k_perp_ave)

   end subroutine deallocate_type_waves_profiles_1d

   subroutine deallocate_arr_type_waves_profiles_1d(structure)

     implicit none

     type (type_waves_profiles_1d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_profiles_1d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_profiles_1d'
     end if

   end subroutine deallocate_arr_type_waves_profiles_1d

   subroutine deallocate_type_waves_profiles_2d(structure)

     implicit none

     type (type_waves_profiles_2d) :: structure

     call deallocate_type_matflt_type(structure%powd_tot)
     call deallocate_type_matflt_type(structure%powd_e)
     call deallocate_type_array3dflt_type(structure%powd_i)
     call deallocate_type_array4dflt_type(structure%powd_z)
     call deallocate_type_matflt_type(structure%powd_fe)
     call deallocate_type_array3dflt_type(structure%powd_fi)
     call deallocate_type_array4dflt_type(structure%powd_fz)
     call deallocate_type_array3dflt_type(structure%powd_ntor)
     call deallocate_type_array3dflt_type(structure%powd_ntor_e)
     call deallocate_type_array4dflt_type(structure%powd_ntor_i)
     call deallocate_type_array5dflt_type(structure%powd_ntor_z)
     call deallocate_type_array3dflt_type(structure%powd_ntor_fe)
     call deallocate_type_array4dflt_type(structure%powd_ntor_fi)
     call deallocate_type_array5dflt_type(structure%powd_ntor_fz)
     call deallocate_type_array5dflt_type(structure%powd_iharm)

   end subroutine deallocate_type_waves_profiles_2d

   subroutine deallocate_arr_type_waves_profiles_2d(structure)

     implicit none

     type (type_waves_profiles_2d), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_profiles_2d(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_profiles_2d'
     end if

   end subroutine deallocate_arr_type_waves_profiles_2d

   subroutine deallocate_type_waves_rtposition(structure)

     implicit none

     type (type_waves_rtposition) :: structure

     call deallocate_type_vecflt_type(structure%r)
     call deallocate_type_vecflt_type(structure%z)
     call deallocate_type_vecflt_type(structure%phi)
     call deallocate_type_vecflt_type(structure%psi)
     call deallocate_type_vecflt_type(structure%theta)

   end subroutine deallocate_type_waves_rtposition

   subroutine deallocate_arr_type_waves_rtposition(structure)

     implicit none

     type (type_waves_rtposition), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_rtposition(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_rtposition'
     end if

   end subroutine deallocate_arr_type_waves_rtposition

   subroutine deallocate_type_waves_rtwavevector(structure)

     implicit none

     type (type_waves_rtwavevector) :: structure

     call deallocate_type_vecflt_type(structure%kr)
     call deallocate_type_vecflt_type(structure%kz)
     call deallocate_type_vecflt_type(structure%kphi)
     call deallocate_type_vecflt_type(structure%npar)
     call deallocate_type_vecflt_type(structure%nperp)
     call deallocate_type_vecflt_type(structure%ntor)

   end subroutine deallocate_type_waves_rtwavevector

   subroutine deallocate_arr_type_waves_rtwavevector(structure)

     implicit none

     type (type_waves_rtwavevector), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_waves_rtwavevector(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_waves_rtwavevector'
     end if

   end subroutine deallocate_arr_type_waves_rtwavevector

   subroutine deallocate_type_weighted_markers(structure)

     implicit none

     type (type_weighted_markers) :: structure

     call deallocate_arr_type_identifier(structure%variable_ids)
     call deallocate_type_matflt_type(structure%coord)
     call deallocate_type_vecflt_type(structure%weight)

   end subroutine deallocate_type_weighted_markers

   subroutine deallocate_arr_type_weighted_markers(structure)

     implicit none

     type (type_weighted_markers), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_weighted_markers(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_weighted_markers'
     end if

   end subroutine deallocate_arr_type_weighted_markers

   subroutine deallocate_type_whatref(structure)

     implicit none

     type (type_whatref) :: structure

     call deallocate_type_vecstring_type(structure%user)
     call deallocate_type_vecstring_type(structure%machine)

   end subroutine deallocate_type_whatref

   subroutine deallocate_arr_type_whatref(structure)

     implicit none

     type (type_whatref), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_whatref(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_whatref'
     end if

   end subroutine deallocate_arr_type_whatref

   subroutine deallocate_type_width(structure)

     implicit none

     type (type_width) :: structure

     call deallocate_type_vecflt_type(structure%dtheta)
     call deallocate_type_vecflt_type(structure%phi)

   end subroutine deallocate_type_width

   subroutine deallocate_arr_type_width(structure)

     implicit none

     type (type_width), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_width(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_width'
     end if

   end subroutine deallocate_arr_type_width

   subroutine deallocate_type_xpts(structure)

     implicit none

     type (type_xpts) :: structure

     call deallocate_type_rz1D(structure%position)
     call deallocate_type_vecstring_type(structure%source)
     call deallocate_type_vecflt_type(structure%weight)
     call deallocate_type_vecflt_type(structure%sigma)
     call deallocate_type_vecflt_type(structure%calculated)
     call deallocate_type_vecflt_type(structure%chi2)

   end subroutine deallocate_type_xpts

   subroutine deallocate_arr_type_xpts(structure)

     implicit none

     type (type_xpts), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_xpts(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_xpts'
     end if

   end subroutine deallocate_arr_type_xpts

   subroutine deallocate_type_xyz0D(structure)

     implicit none

     type (type_xyz0D) :: structure


   end subroutine deallocate_type_xyz0D

   subroutine deallocate_arr_type_xyz0D(structure)

     implicit none

     type (type_xyz0D), pointer :: structure(:)
     integer :: i

     if (associated(structure)) then
       do i = 1, size(structure)
         call deallocate_type_xyz0D(structure(i))
       end do
       deallocate(structure)
       if (verbose > 0) write(iu6, *)'deallocated array of type_xyz0D'
     end if

   end subroutine deallocate_arr_type_xyz0D


 end module deallocate_structures
