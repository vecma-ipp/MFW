# calculate te at neighboring +/-2 rho_tor grid points based on sample value of dTdrho at flux-tube
def update_te_grad(cpo_core, v, flux_index):
    # TODO verify that cpo_core has corprof base_type
    # v = Grad_Te at flux_index point
    b = cpo_core.te.value[flux_index] - v * cpo_core.rho_tor[flux_index]

    cpo_core.te.value[flux_index-2] = v * cpo_core.rho_tor[flux_index-2] + b
    cpo_core.te.value[flux_index-1] = v * cpo_core.rho_tor[flux_index-1] + b
    cpo_core.te.value[flux_index+1] = v * cpo_core.rho_tor[flux_index+1] + b
    cpo_core.te.value[flux_index+2] = v * cpo_core.rho_tor[flux_index+2] + b

# Calculate Ti at neighboring +/-2 rho_tor grid points based on sample value of dTdrho at flux-tube
def update_ti_grad(cpo_core, v, flux_index):
    # TODO verify that cpo_core has corprof base_type
    # v = Grad_Ti at flux_index point
    b = cpo_core.ti.value[flux_index][0] - v * cpo_core.rho_tor[flux_index]

    cpo_core.ti.value[flux_index-2][0] = v * cpo_core.rho_tor[flux_index-2] + b
    cpo_core.ti.value[flux_index-1][0] = v * cpo_core.rho_tor[flux_index-1] + b
    cpo_core.ti.value[flux_index+1][0] = v * cpo_core.rho_tor[flux_index+1] + b
    cpo_core.ti.value[flux_index+2][0] = v * cpo_core.rho_tor[flux_index+2] + b

# Calculate Te at neighboring +/-2 rho_tor grid points based on sample value of Te at flux-tube and
# value of dTdrho at flux-tube
def update_te(cpo_core, v, flux_index):
    # v = Te at flux_index point
    b = v - cpo_core.te.ddrho[flux_index] * cpo_core.rho_tor[flux_index]

    cpo_core.te.value[flux_index-2] = cpo_core.te.ddrho[flux_index] * cpo_core.rho_tor[flux_index-2] + b
    cpo_core.te.value[flux_index-1] = cpo_core.te.ddrho[flux_index] * cpo_core.rho_tor[flux_index-1] + b
    cpo_core.te.value[flux_index+1] = cpo_core.te.ddrho[flux_index] * cpo_core.rho_tor[flux_index+1] + b
    cpo_core.te.value[flux_index+2] = cpo_core.te.ddrho[flux_index] * cpo_core.rho_tor[flux_index+2] + b

# Calculate Ti at neighboring +/-2 rho_tor grid points based on sample value of Ti at flux-tube and
# value of dTdrho at flux-tube
def update_ti(cpo_core, v, flux_index):
    # v = Ti at flux_index point
    b = v - cpo_core.ti.ddrho[flux_index][0] * cpo_core.rho_tor[flux_index]

    cpo_core.ti.value[flux_index-2][0] = cpo_core.ti.ddrho[flux_index][0] * cpo_core.rho_tor[flux_index-2] + b
    cpo_core.ti.value[flux_index-1][0] = cpo_core.ti.ddrho[flux_index][0] * cpo_core.rho_tor[flux_index-1] + b
    cpo_core.ti.value[flux_index+1][0] = cpo_core.ti.ddrho[flux_index][0] * cpo_core.rho_tor[flux_index+1] + b
    cpo_core.ti.value[flux_index+2][0] = cpo_core.ti.ddrho[flux_index][0] * cpo_core.rho_tor[flux_index+2] + b
