abort "this is a configuration file to be used with the MUSCLE bootstrap utility" if __FILE__ == $0

KDIR = ENV["MUSCLE_KERNELS_DIR"]
INPUTDIR = ENV["CPO_INPUT_DIR"]

TAU = 0.01
SYS = "MARCONI"

INIT_K   = "PARTIAL_INIT"
TURB_K   = "GEM0"

case INIT_K
when "PARTIAL_INIT"
  init = NativeInstance.new('init', KDIR+'/partial_init_kernelB')
else
  abort "Unknown init kernel"
end

f2dv = NativeInstance.new('f2dv', KDIR+'/imp4dv_kernelB')

case TURB_K
  when "GEM0"
    turb = NativeInstance.new('turb', KDIR+'/gem0_kernelB')
  else
    abort "Unknown turbulence kernel"
  end

dupEquil = Instance.new('dupEquil', 'muscle.core.kernel.DuplicationMapper')
dupCorep = Instance.new('dupCorep', 'muscle.core.kernel.DuplicationMapper')

endWF = Terminal.new('endWF', 'muscle.core.conduit.terminal.NullSink')

$env['time_step'] = TAU
$env['init_step'] = 0
$env['target_step'] = 0

init['partial_init_path'] = INPUTDIR

f2dv['f_limit'] = false
f2dv['f_floor'] = 0.0

init.couple(dupEquil, 'equilibrium_out')
init.couple(dupCorep, 'coreprof_out')

dupCorep.couple(turb, {'corep1' => 'coreprof_in'})
dupEquil.couple(turb, {'equil1' => 'equilibrium_in'})

dupCorep.couple(f2dv, {'corep3' => 'coreprof_in'})
dupEquil.couple(f2dv, {'equil3' => 'equilibrium_in'})

turb.couple(f2dv, {'coretransp_out' => 'coretransp_in'})

f2dv.couple(endWF, 'coretransp_out')
