module ets_species_module

  use euitm_schemas

  implicit none

  type type_ets_species
     integer :: nion, nimp, natm,  nneut = 0, nnucl
     integer, allocatable :: nzimp(:), ntype(:), ncomp(:)
  end type type_ets_species

  type (type_ets_species), save :: ets_species

  type (type_compositions_type), save :: ets_composition

end module ets_species_module
