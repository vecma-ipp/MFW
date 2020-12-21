# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class neoclassic(KeepInOrder):
	'''
	class neoclassic
	Neoclassical quantities (including transport coefficients). Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- rho_tor_norm : numpy.ndarray 1D with float
	   Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m]; Vector (nrho). Time-dependent.
	- composition : class compositionstructurecomposition
	   Plasma composition (description of ion species). OBSOLESCENT.
	- desc_impur : class desc_impurstructuredesc_impur
	   Description of the impurities (list of ion species and possibly different charge states). OBSOLESCENT.
	- compositions : class compositionsstructurecompositions_type
	   Contains all the composition information for the simulation (main ions, impurities, neutrals, edge species).
	- ni_neo : class ni_neostructuretranscoefion
	   Neoclassical transport coefficients for ion density equation. Time-dependent.
	- ne_neo : class ne_neostructuretranscoefel
	   Neoclassical transport coefficients for electron density equation. Time-dependent.
	- nz_neo : class nz_neostruct_arraytranscoefimp: array of nz_neostruct_arraytranscoefimpObj objects
	   Neoclassical transport coefficients for impurity (multiple charge state) density equation. Time-dependent.
	- ti_neo : class ti_neostructuretranscoefion
	   Neoclassical transport coefficients for ion temperature equation. Time-dependent.
	- te_neo : class te_neostructuretranscoefel
	   Neoclassical transport coefficients for electron temperature equation. Time-dependent.
	- tz_neo : class tz_neostruct_arraytranscoefimp: array of tz_neostruct_arraytranscoefimpObj objects
	   Neoclassical transport coefficients for impurity (multiple charge state) temperature equation. Time-dependent.
	- mtor_neo : class mtor_neostructuretranscoefel
	   Neoclassical transport coefficients for total toroidal momentum equation. Time-dependent.
	- sigma : numpy.ndarray 1D with float
	   Neoclassical conductivity [ohm^-1.m^-1]. Time-dependent. Vector(nrho).
	- jboot : numpy.ndarray 1D with float
	   Bootstrap current density [A.m^-2]. Time-dependent. Vector(nrho).
	- er : numpy.ndarray 1D with float
	   Radial electric field [V/m]. Time-dependent. Vector(nrho).
	- vpol : numpy.ndarray 2D with float
	   Neoclassical poloidal rotation of each ion species [m/s]. Time-dependent. Matrix(nrho,nion).
	- vtor : numpy.ndarray 2D with float
	   Neoclassical toroidal rotation of each ion species [m/s]. Time-dependent. Matrix(nrho,nion).
	- mach : numpy.ndarray 2D with float
	   Mach number of each ion species. Time-dependent. Matrix(nrho,nion).
	- utheta_e : numpy.ndarray 1D with float
	   Electron poloidal flow [m/s]. Time-dependent. Vector(nrho).
	- utheta_i : numpy.ndarray 2D with float
	   Ion poloidal flow [m/s].  Time-dependent. Matrix(nrho,nion).
	- viscosity_par : numpy.ndarray 2D with float
	   Ion parallel viscosity [?].  Time-dependent. Matrix(nrho,nion).
	- impurity : class impuritystruct_arrayneoclassic_impurity: array of impuritystruct_arrayneoclassic_impurityObj objects
	   Array(nimp). Time-dependent
	- fext : numpy.ndarray 3D with float
	   Moments of parallel external force on each ion species [T.J.m^-3]. Time-dependent. Array3D(nrho,nion,nmoment).
	- jext : numpy.ndarray 1D with float
	   Current density response to fext [A.m^-2]. Time-dependent. Vector(nrho).
	- time : float
	   Time [s]; Time-dependent; Scalar.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self):
		self.base_path = 'neoclassic'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 5
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.composition = compositionstructurecomposition('composition')
		self.desc_impur = desc_impurstructuredesc_impur('desc_impur')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.ni_neo = ni_neostructuretranscoefion('ni_neo')
		self.ne_neo = ne_neostructuretranscoefel('ne_neo')
		self.nz_neo = nz_neostruct_arraytranscoefimp('nz_neo')
		self.ti_neo = ti_neostructuretranscoefion('ti_neo')
		self.te_neo = te_neostructuretranscoefel('te_neo')
		self.tz_neo = tz_neostruct_arraytranscoefimp('tz_neo')
		self.mtor_neo = mtor_neostructuretranscoefel('mtor_neo')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.jboot = numpy.zeros(0, numpy.float64, order='C')
		self.er = numpy.zeros(0, numpy.float64, order='C')
		self.vpol = numpy.zeros((0,0), numpy.float64, order='C')
		self.vtor = numpy.zeros((0,0), numpy.float64, order='C')
		self.mach = numpy.zeros((0,0), numpy.float64, order='C')
		self.utheta_e = numpy.zeros(0, numpy.float64, order='C')
		self.utheta_i = numpy.zeros((0,0), numpy.float64, order='C')
		self.viscosity_par = numpy.zeros((0,0), numpy.float64, order='C')
		self.impurity = impuritystruct_arrayneoclassic_impurity('impurity')
		self.fext = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.jext = numpy.zeros(0, numpy.float64, order='C')
		self.time = EMPTY_DOUBLE
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neoclassic\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute composition\n ' + self.composition.__str__(depth+1)
		ret = ret + space + 'Attribute desc_impur\n ' + self.desc_impur.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute ni_neo\n ' + self.ni_neo.__str__(depth+1)
		ret = ret + space + 'Attribute ne_neo\n ' + self.ne_neo.__str__(depth+1)
		ret = ret + space + 'Attribute nz_neo\n ' + self.nz_neo.__str__(depth+1)
		ret = ret + space + 'Attribute ti_neo\n ' + self.ti_neo.__str__(depth+1)
		ret = ret + space + 'Attribute te_neo\n ' + self.te_neo.__str__(depth+1)
		ret = ret + space + 'Attribute tz_neo\n ' + self.tz_neo.__str__(depth+1)
		ret = ret + space + 'Attribute mtor_neo\n ' + self.mtor_neo.__str__(depth+1)
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jboot.__str__()
		ret = ret + space + 'Attribute jboot\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.er.__str__()
		ret = ret + space + 'Attribute er\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vpol.__str__()
		ret = ret + space + 'Attribute vpol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vtor.__str__()
		ret = ret + space + 'Attribute vtor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mach.__str__()
		ret = ret + space + 'Attribute mach\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.utheta_e.__str__()
		ret = ret + space + 'Attribute utheta_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.utheta_i.__str__()
		ret = ret + space + 'Attribute utheta_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.viscosity_par.__str__()
		ret = ret + space + 'Attribute viscosity_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute impurity\n ' + self.impurity.__str__(depth+1)
		s = self.fext.__str__()
		ret = ret + space + 'Attribute fext\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jext.__str__()
		ret = ret + space + 'Attribute jext\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.composition.setExpIdx(idx)
		self.desc_impur.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
		self.ni_neo.setExpIdx(idx)
		self.ne_neo.setExpIdx(idx)
		self.nz_neo.setExpIdx(idx)
		self.ti_neo.setExpIdx(idx)
		self.te_neo.setExpIdx(idx)
		self.tz_neo.setExpIdx(idx)
		self.mtor_neo.setExpIdx(idx)
		self.impurity.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def getMaxOccurrences(self):
		return self.maxOccurrences

	def getCPOName(self):
		return self.base_path

	def putSlice(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.cpoTime = self.time
		status = ull.beginCPOPutSlice(self.idx, path)
		self.datainfo.cpoTime = self.cpoTime
		self.datainfo.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.composition.cpoTime = self.cpoTime
		self.composition.putSlice(path, cpopath)
		self.desc_impur.cpoTime = self.cpoTime
		self.desc_impur.putSlice(path, cpopath)
		self.compositions.cpoTime = self.cpoTime
		self.compositions.putSlice(path, cpopath)
		self.ni_neo.cpoTime = self.cpoTime
		self.ni_neo.putSlice(path, cpopath)
		self.ne_neo.cpoTime = self.cpoTime
		self.ne_neo.putSlice(path, cpopath)
		self.nz_neo.cpoTime = self.cpoTime
		self.nz_neo.putSlice(path, cpopath)
		self.ti_neo.cpoTime = self.cpoTime
		self.ti_neo.putSlice(path, cpopath)
		self.te_neo.cpoTime = self.cpoTime
		self.te_neo.putSlice(path, cpopath)
		self.tz_neo.cpoTime = self.cpoTime
		self.tz_neo.putSlice(path, cpopath)
		self.mtor_neo.cpoTime = self.cpoTime
		self.mtor_neo.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jboot', numpy.array(self.jboot).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'er', numpy.array(self.er).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vpol', numpy.array(self.vpol).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vtor', numpy.array(self.vtor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'mach', numpy.array(self.mach).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'utheta_e', numpy.array(self.utheta_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'utheta_i', numpy.array(self.utheta_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'viscosity_par', numpy.array(self.viscosity_par).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.impurity.cpoTime = self.cpoTime
		self.impurity.putSlice(path, cpopath)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'fext', numpy.array(self.fext).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jext', numpy.array(self.jext).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)
		status = ull.endCPOPutSlice(self.idx, path)

	def replaceLastSlice(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.cpoTime = self.time
		status = ull.beginCPOReplaceLastSlice(self.idx, path)
		self.datainfo.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		self.composition.replaceLastSlice(path, cpopath)
		self.desc_impur.replaceLastSlice(path, cpopath)
		self.compositions.replaceLastSlice(path, cpopath)
		self.ni_neo.replaceLastSlice(path, cpopath)
		self.ne_neo.replaceLastSlice(path, cpopath)
		self.nz_neo.replaceLastSlice(path, cpopath)
		self.ti_neo.replaceLastSlice(path, cpopath)
		self.te_neo.replaceLastSlice(path, cpopath)
		self.tz_neo.replaceLastSlice(path, cpopath)
		self.mtor_neo.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jboot', numpy.array(self.jboot).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'er', numpy.array(self.er).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vpol', numpy.array(self.vpol).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vtor', numpy.array(self.vtor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'mach', numpy.array(self.mach).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'utheta_e', numpy.array(self.utheta_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'utheta_i', numpy.array(self.utheta_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'viscosity_par', numpy.array(self.viscosity_par).astype(numpy.float64))
		check_status(status)
		self.impurity.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'fext', numpy.array(self.fext).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jext', numpy.array(self.jext).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)
		status = ull.endCPOReplaceLastSlice(self.idx, path)

	def putNonTimed(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.deleteData(occurrence)
		status = ull.beginCPOPutNonTimed(self.idx, path)
		self.datainfo.putNonTimed(path, cpopath)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), False)
		check_status(status)
		self.composition.putNonTimed(path, cpopath)
		self.desc_impur.putNonTimed(path, cpopath)
		self.compositions.putNonTimed(path, cpopath)
		self.ni_neo.putNonTimed(path, cpopath)
		self.ne_neo.putNonTimed(path, cpopath)
		self.nz_neo.putNonTimed(path, cpopath)
		self.ti_neo.putNonTimed(path, cpopath)
		self.te_neo.putNonTimed(path, cpopath)
		self.tz_neo.putNonTimed(path, cpopath)
		self.mtor_neo.putNonTimed(path, cpopath)
		self.impurity.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)
		status = ull.endCPOPutNonTimed(self.idx, path)

	def getSlice(self, inTime, interpolMode, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.cpoTime = self.time
		status = ull.beginCPOGetSlice(self.idx, path, inTime)
		self.datainfo.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_rho_tor_norm = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm')
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime
		self.composition.getSlice(path, cpopath, inTime, interpolMode)
		self.desc_impur.getSlice(path, cpopath, inTime, interpolMode)
		self.compositions.getSlice(path, cpopath, inTime, interpolMode)
		self.ni_neo.getSlice(path, cpopath, inTime, interpolMode)
		self.ne_neo.getSlice(path, cpopath, inTime, interpolMode)
		self.nz_neo.getSlice(path, cpopath, inTime, interpolMode)
		self.ti_neo.getSlice(path, cpopath, inTime, interpolMode)
		self.te_neo.getSlice(path, cpopath, inTime, interpolMode)
		self.tz_neo.getSlice(path, cpopath, inTime, interpolMode)
		self.mtor_neo.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_jboot, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jboot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jboot = ret_jboot
			self.cpoTime = retTime
		status, ret_er, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'er', inTime, interpolMode)
		check_status(status)
		if not status:
			self.er = ret_er
			self.cpoTime = retTime
		status, ret_vpol, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vpol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vpol = ret_vpol
			self.cpoTime = retTime
		status, ret_vtor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vtor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vtor = ret_vtor
			self.cpoTime = retTime
		status, ret_mach, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'mach', inTime, interpolMode)
		check_status(status)
		if not status:
			self.mach = ret_mach
			self.cpoTime = retTime
		status, ret_utheta_e, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'utheta_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.utheta_e = ret_utheta_e
			self.cpoTime = retTime
		status, ret_utheta_i, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'utheta_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.utheta_i = ret_utheta_i
			self.cpoTime = retTime
		status, ret_viscosity_par, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'viscosity_par', inTime, interpolMode)
		check_status(status)
		if not status:
			self.viscosity_par = ret_viscosity_par
			self.cpoTime = retTime
		self.impurity.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_fext, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'fext', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fext = ret_fext
			self.cpoTime = retTime
		status, ret_jext, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jext', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jext = ret_jext
			self.cpoTime = retTime
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)
		status = ull.endCPOGetSlice(self.idx, path)

	def build_non_resampled_data(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		status, nbslice = ull.beginCPOGet(self.idx, path, True)
		check_status(status)
		array=None
		if nbslice > 0:
			datainfoList = self.datainfo.build_non_resampled_data(path, cpopath, nbslice)
			status, rho_tor_normVal = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			compositionList = self.composition.build_non_resampled_data(path, cpopath, nbslice)
			desc_impurList = self.desc_impur.build_non_resampled_data(path, cpopath, nbslice)
			compositionsList = self.compositions.build_non_resampled_data(path, cpopath, nbslice)
			ni_neoList = self.ni_neo.build_non_resampled_data(path, cpopath, nbslice)
			ne_neoList = self.ne_neo.build_non_resampled_data(path, cpopath, nbslice)
			nz_neoList = self.nz_neo.build_non_resampled_data(path, cpopath, nbslice)
			ti_neoList = self.ti_neo.build_non_resampled_data(path, cpopath, nbslice)
			te_neoList = self.te_neo.build_non_resampled_data(path, cpopath, nbslice)
			tz_neoList = self.tz_neo.build_non_resampled_data(path, cpopath, nbslice)
			mtor_neoList = self.mtor_neo.build_non_resampled_data(path, cpopath, nbslice)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, jbootList = ull.getVect2DDouble(self.idx, path, cpopath + 'jboot')
			if len(jbootList) == 0:
				jbootList = numpy.resize(jbootList, (0,nbslice))
			check_status(status)
			status, erList = ull.getVect2DDouble(self.idx, path, cpopath + 'er')
			if len(erList) == 0:
				erList = numpy.resize(erList, (0,nbslice))
			check_status(status)
			status, vpolList = ull.getVect3DDouble(self.idx, path, cpopath + 'vpol')
			if len(vpolList) == 0:
				vpolList = numpy.resize(vpolList, (0,0,nbslice))
			check_status(status)
			status, vtorList = ull.getVect3DDouble(self.idx, path, cpopath + 'vtor')
			if len(vtorList) == 0:
				vtorList = numpy.resize(vtorList, (0,0,nbslice))
			check_status(status)
			status, machList = ull.getVect3DDouble(self.idx, path, cpopath + 'mach')
			if len(machList) == 0:
				machList = numpy.resize(machList, (0,0,nbslice))
			check_status(status)
			status, utheta_eList = ull.getVect2DDouble(self.idx, path, cpopath + 'utheta_e')
			if len(utheta_eList) == 0:
				utheta_eList = numpy.resize(utheta_eList, (0,nbslice))
			check_status(status)
			status, utheta_iList = ull.getVect3DDouble(self.idx, path, cpopath + 'utheta_i')
			if len(utheta_iList) == 0:
				utheta_iList = numpy.resize(utheta_iList, (0,0,nbslice))
			check_status(status)
			status, viscosity_parList = ull.getVect3DDouble(self.idx, path, cpopath + 'viscosity_par')
			if len(viscosity_parList) == 0:
				viscosity_parList = numpy.resize(viscosity_parList, (0,0,nbslice))
			check_status(status)
			impurityList = self.impurity.build_non_resampled_data(path, cpopath, nbslice)
			status, fextList = ull.getVect4DDouble(self.idx, path, cpopath + 'fext')
			if len(fextList) == 0:
				fextList = numpy.resize(fextList, (0,0,0,nbslice))
			check_status(status)
			status, jextList = ull.getVect2DDouble(self.idx, path, cpopath + 'jext')
			if len(jextList) == 0:
				jextList = numpy.resize(jextList, (0,nbslice))
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			array = []
			for i in range(nbslice):
				slice = neoclassic()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.rho_tor_norm = rho_tor_normVal
				slice.rho_tor = rho_torList[:,i]
				slice.composition = compositionList[i]
				slice.desc_impur = desc_impurList[i]
				slice.compositions = compositionsList[i]
				slice.ni_neo = ni_neoList[i]
				slice.ne_neo = ne_neoList[i]
				slice.nz_neo = nz_neoList[i]
				slice.ti_neo = ti_neoList[i]
				slice.te_neo = te_neoList[i]
				slice.tz_neo = tz_neoList[i]
				slice.mtor_neo = mtor_neoList[i]
				slice.sigma = sigmaList[:,i]
				slice.jboot = jbootList[:,i]
				slice.er = erList[:,i]
				slice.vpol = vpolList[:,:,i]
				slice.vtor = vtorList[:,:,i]
				slice.mach = machList[:,:,i]
				slice.utheta_e = utheta_eList[:,i]
				slice.utheta_i = utheta_iList[:,:,i]
				slice.viscosity_par = viscosity_parList[:,:,i]
				slice.impurity = impurityList[i]
				slice.fext = fextList[:,:,:,i]
				slice.jext = jextList[:,i]
				slice.time = timeList[i].copy().astype(float)
				slice.codeparam = codeparamList[i]
				array.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		status = ull.endCPOGet(self.idx, path)
		return array

	def deleteData(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.datainfo.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'rho_tor_norm')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		self.composition.deleteData(path, cpopath)
		self.desc_impur.deleteData(path, cpopath)
		self.compositions.deleteData(path, cpopath)
		self.ni_neo.deleteData(path, cpopath)
		self.ne_neo.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'nz_neo')
		self.ti_neo.deleteData(path, cpopath)
		self.te_neo.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'tz_neo')
		self.mtor_neo.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'jboot')
		ull.deleteData(self.idx, path, cpopath + 'er')
		ull.deleteData(self.idx, path, cpopath + 'vpol')
		ull.deleteData(self.idx, path, cpopath + 'vtor')
		ull.deleteData(self.idx, path, cpopath + 'mach')
		ull.deleteData(self.idx, path, cpopath + 'utheta_e')
		ull.deleteData(self.idx, path, cpopath + 'utheta_i')
		ull.deleteData(self.idx, path, cpopath + 'viscosity_par')
		ull.deleteData(self.idx, path, cpopath + 'impurity')
		ull.deleteData(self.idx, path, cpopath + 'fext')
		ull.deleteData(self.idx, path, cpopath + 'jext')
		ull.deleteData(self.idx, path, cpopath + 'time')
		self.codeparam.deleteData(path, cpopath)


class neoclassicArray:
	'''
	class neoclassicArray
	Neoclassical quantities (including transport coefficients). Time-dependent CPO

	Attributes:
	- array : list of neoclassic
	   Each list element correspond to one time slice.
	'''

	def __init__(self):
		self.array = []
		self.idx = EMPTY_INT
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neoclassicArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'neoclassic cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def put(self, occurrence=0):
		if (len(self.array)>0):
			self.array[0].putNonTimed(occurrence)
			for i in self.array:
				i.putSlice(occurrence)
		else:
			print('no time slice to be put')

	def get(self, occurrence=0):
		cpo = neoclassic()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(neoclassic())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = neoclassic()
		cpo.setExpIdx(self.idx)
		cpo.deleteData(occurrence)


class datainfostructuredatainfo(KeepInOrder):
	'''
	class datainfostructuredatainfo
	Generic information on a data item

	Attributes:
	- dataprovider : str
	   Name of the actual data provider (the person who filled the data)
	- putdate : str
	   Date at which the data has been put in the DB
	- source : str
	   Exact reference of the data source (e.g. original reference in the native machine data base)
	- comment : str
	   Any additional comment
	- cocos : int
	   COordinates COnventionS followed by this CPO
	- id : int
	   CPO id for checking its provenance in the workflow
	- isref : int
	   1 if the data can be found in the present data base entry; 2 if the data can be found in a parent data base entry; 0 if no data consistent with the present entry can be found.
	- whatref : class whatrefstructurewhatref
	   Structure defining a database entry and the CPO occurrence
	- putinfo : class putinfostructureputinfo
	   Level 2 information describing how to retrieve the actual data for the UAL. Not to be filled/used by the ITM user !
	'''

	def __init__(self, base_path_in='datainfo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dataprovider = ''
		self.putdate = ''
		self.source = ''
		self.comment = ''
		self.cocos = EMPTY_INT
		self.id = EMPTY_INT
		self.isref = EMPTY_INT
		self.whatref = whatrefstructurewhatref('whatref')
		self.putinfo = putinfostructureputinfo('putinfo')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class datainfostructuredatainfo\n'
		ret = ret + space + 'Attribute dataprovider: ' + str(self.dataprovider) + '\n'
		ret = ret + space + 'Attribute putdate: ' + str(self.putdate) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute comment: ' + str(self.comment) + '\n'
		ret = ret + space + 'Attribute cocos: ' + str(self.cocos) + '\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute isref: ' + str(self.isref) + '\n'
		ret = ret + space + 'Attribute whatref\n ' + self.whatref.__str__(depth+1)
		ret = ret + space + 'Attribute putinfo\n ' + self.putinfo.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.whatref.setExpIdx(idx)
		self.putinfo.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.whatref.cpoTime = self.cpoTime
		self.whatref.putSlice(path, cpopath)
		self.putinfo.cpoTime = self.cpoTime
		self.putinfo.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.whatref.replaceLastSlice(path, cpopath)
		self.putinfo.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'dataprovider', self.dataprovider)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'putdate', self.putdate)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'comment', self.comment)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'cocos', self.cocos)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'id', self.id)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'isref', self.isref)
		check_status(status)
		self.whatref.putNonTimed(path, cpopath)
		self.putinfo.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dataprovider = ull.getString(self.idx, path, cpopath + 'dataprovider')
		check_status(status)
		if not status:
			self.dataprovider = ret_dataprovider
		status, ret_putdate = ull.getString(self.idx, path, cpopath + 'putdate')
		check_status(status)
		if not status:
			self.putdate = ret_putdate
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_comment = ull.getString(self.idx, path, cpopath + 'comment')
		check_status(status)
		if not status:
			self.comment = ret_comment
		status, ret_cocos = ull.getInt(self.idx, path, cpopath + 'cocos')
		check_status(status)
		if not status:
			self.cocos = ret_cocos
		status, ret_id = ull.getInt(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_isref = ull.getInt(self.idx, path, cpopath + 'isref')
		check_status(status)
		if not status:
			self.isref = ret_isref
		self.whatref.getSlice(path, cpopath, inTime, interpolMode)
		self.putinfo.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dataproviderVal = ull.getString(self.idx, path, cpopath + 'dataprovider')
			check_status(status)
			status, putdateVal = ull.getString(self.idx, path, cpopath + 'putdate')
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, commentVal = ull.getString(self.idx, path, cpopath + 'comment')
			check_status(status)
			status, cocosVal = ull.getInt(self.idx, path, cpopath + 'cocos')
			check_status(status)
			status, idVal = ull.getInt(self.idx, path, cpopath + 'id')
			check_status(status)
			status, isrefVal = ull.getInt(self.idx, path, cpopath + 'isref')
			check_status(status)
			whatrefList = self.whatref.build_non_resampled_data(path, cpopath, nbslice)
			putinfoList = self.putinfo.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = datainfostructuredatainfo(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dataprovider = dataproviderVal
				slice.putdate = putdateVal
				slice.source = sourceVal
				slice.comment = commentVal
				slice.cocos = cocosVal
				slice.id = idVal
				slice.isref = isrefVal
				slice.whatref = whatrefList[i]
				slice.putinfo = putinfoList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'dataprovider') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'dataprovider', i, self.dataprovider)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putdate') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putdate', i, self.putdate)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'comment') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'comment', i, self.comment)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'cocos') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'cocos', i, self.cocos)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'isref') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'isref', i, self.isref)
		obj = self.whatref.putNonTimedElt(path, cpopath + 'whatref', i, obj)
		obj = self.putinfo.putNonTimedElt(path, cpopath + 'putinfo', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'dataprovider') 
			print ('obj = ' + str(obj))
		status, ret_dataprovider = ull.getStringFromObject(self.idx, obj, cpopath + 'dataprovider', i)
		check_status(status)
		if not status:
			self.dataprovider = ret_dataprovider
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putdate') 
			print ('obj = ' + str(obj))
		status, ret_putdate = ull.getStringFromObject(self.idx, obj, cpopath + 'putdate', i)
		check_status(status)
		if not status:
			self.putdate = ret_putdate
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'comment') 
			print ('obj = ' + str(obj))
		status, ret_comment = ull.getStringFromObject(self.idx, obj, cpopath + 'comment', i)
		check_status(status)
		if not status:
			self.comment = ret_comment
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'cocos') 
			print ('obj = ' + str(obj))
		status, ret_cocos = ull.getIntFromObject(self.idx, obj, cpopath + 'cocos', i)
		check_status(status)
		if not status:
			self.cocos = ret_cocos
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getIntFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'isref') 
			print ('obj = ' + str(obj))
		status, ret_isref = ull.getIntFromObject(self.idx, obj, cpopath + 'isref', i)
		check_status(status)
		if not status:
			self.isref = ret_isref
		self.whatref.getNonTimedElt(path, cpopath + 'whatref', i, obj)
		self.putinfo.getNonTimedElt(path, cpopath + 'putinfo', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dataprovider')
		ull.deleteData(self.idx, path, cpopath + 'putdate')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'comment')
		ull.deleteData(self.idx, path, cpopath + 'cocos')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'isref')
		self.whatref.deleteData(path, cpopath)
		self.putinfo.deleteData(path, cpopath)


class whatrefstructurewhatref(KeepInOrder):
	'''
	class whatrefstructurewhatref
	Structure defining a database entry and the CPO occurrence

	Attributes:
	- user : str
	   Name of the user if private data, public if public ITM database.
	- machine : str
	   Name of the device
	- shot : int
	   Shot number
	- run : int
	   Run number
	- occurrence : int
	   Occurrence number of the CPO in the reference entry
	'''

	def __init__(self, base_path_in='whatref'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.user = ''
		self.machine = ''
		self.shot = EMPTY_INT
		self.run = EMPTY_INT
		self.occurrence = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class whatrefstructurewhatref\n'
		ret = ret + space + 'Attribute user: ' + str(self.user) + '\n'
		ret = ret + space + 'Attribute machine: ' + str(self.machine) + '\n'
		ret = ret + space + 'Attribute shot: ' + str(self.shot) + '\n'
		ret = ret + space + 'Attribute run: ' + str(self.run) + '\n'
		ret = ret + space + 'Attribute occurrence: ' + str(self.occurrence) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'user', self.user)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'machine', self.machine)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'shot', self.shot)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'run', self.run)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'occurrence', self.occurrence)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_user = ull.getString(self.idx, path, cpopath + 'user')
		check_status(status)
		if not status:
			self.user = ret_user
		status, ret_machine = ull.getString(self.idx, path, cpopath + 'machine')
		check_status(status)
		if not status:
			self.machine = ret_machine
		status, ret_shot = ull.getInt(self.idx, path, cpopath + 'shot')
		check_status(status)
		if not status:
			self.shot = ret_shot
		status, ret_run = ull.getInt(self.idx, path, cpopath + 'run')
		check_status(status)
		if not status:
			self.run = ret_run
		status, ret_occurrence = ull.getInt(self.idx, path, cpopath + 'occurrence')
		check_status(status)
		if not status:
			self.occurrence = ret_occurrence

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, userVal = ull.getString(self.idx, path, cpopath + 'user')
			check_status(status)
			status, machineVal = ull.getString(self.idx, path, cpopath + 'machine')
			check_status(status)
			status, shotVal = ull.getInt(self.idx, path, cpopath + 'shot')
			check_status(status)
			status, runVal = ull.getInt(self.idx, path, cpopath + 'run')
			check_status(status)
			status, occurrenceVal = ull.getInt(self.idx, path, cpopath + 'occurrence')
			check_status(status)
			for i in range(nbslice):
				slice = whatrefstructurewhatref(self.base_path)
				slice.setExpIdx(self.idx)
				slice.user = userVal
				slice.machine = machineVal
				slice.shot = shotVal
				slice.run = runVal
				slice.occurrence = occurrenceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'user') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'user', i, self.user)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'machine') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'machine', i, self.machine)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'shot') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'shot', i, self.shot)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'run') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'run', i, self.run)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'occurrence') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'occurrence', i, self.occurrence)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'user') 
			print ('obj = ' + str(obj))
		status, ret_user = ull.getStringFromObject(self.idx, obj, cpopath + 'user', i)
		check_status(status)
		if not status:
			self.user = ret_user
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'machine') 
			print ('obj = ' + str(obj))
		status, ret_machine = ull.getStringFromObject(self.idx, obj, cpopath + 'machine', i)
		check_status(status)
		if not status:
			self.machine = ret_machine
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'shot') 
			print ('obj = ' + str(obj))
		status, ret_shot = ull.getIntFromObject(self.idx, obj, cpopath + 'shot', i)
		check_status(status)
		if not status:
			self.shot = ret_shot
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'run') 
			print ('obj = ' + str(obj))
		status, ret_run = ull.getIntFromObject(self.idx, obj, cpopath + 'run', i)
		check_status(status)
		if not status:
			self.run = ret_run
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'occurrence') 
			print ('obj = ' + str(obj))
		status, ret_occurrence = ull.getIntFromObject(self.idx, obj, cpopath + 'occurrence', i)
		check_status(status)
		if not status:
			self.occurrence = ret_occurrence

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'user')
		ull.deleteData(self.idx, path, cpopath + 'machine')
		ull.deleteData(self.idx, path, cpopath + 'shot')
		ull.deleteData(self.idx, path, cpopath + 'run')
		ull.deleteData(self.idx, path, cpopath + 'occurrence')


class putinfostructureputinfo(KeepInOrder):
	'''
	class putinfostructureputinfo
	Level 2 information describing how to retrieve the actual data for the UAL. Not to be filled/used by the ITM user !

	Attributes:
	- putmethod : str
	   Storage method for this data
	- putaccess : str
	   Instructions to access the data using this method
	- putlocation : str
	   Name of this data under this method
	- rights : str
	   Access rights to this data
	'''

	def __init__(self, base_path_in='putinfo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.putmethod = ''
		self.putaccess = ''
		self.putlocation = ''
		self.rights = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class putinfostructureputinfo\n'
		ret = ret + space + 'Attribute putmethod: ' + str(self.putmethod) + '\n'
		ret = ret + space + 'Attribute putaccess: ' + str(self.putaccess) + '\n'
		ret = ret + space + 'Attribute putlocation: ' + str(self.putlocation) + '\n'
		ret = ret + space + 'Attribute rights: ' + str(self.rights) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'putmethod', self.putmethod)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'putaccess', self.putaccess)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'putlocation', self.putlocation)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'rights', self.rights)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_putmethod = ull.getString(self.idx, path, cpopath + 'putmethod')
		check_status(status)
		if not status:
			self.putmethod = ret_putmethod
		status, ret_putaccess = ull.getString(self.idx, path, cpopath + 'putaccess')
		check_status(status)
		if not status:
			self.putaccess = ret_putaccess
		status, ret_putlocation = ull.getString(self.idx, path, cpopath + 'putlocation')
		check_status(status)
		if not status:
			self.putlocation = ret_putlocation
		status, ret_rights = ull.getString(self.idx, path, cpopath + 'rights')
		check_status(status)
		if not status:
			self.rights = ret_rights

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, putmethodVal = ull.getString(self.idx, path, cpopath + 'putmethod')
			check_status(status)
			status, putaccessVal = ull.getString(self.idx, path, cpopath + 'putaccess')
			check_status(status)
			status, putlocationVal = ull.getString(self.idx, path, cpopath + 'putlocation')
			check_status(status)
			status, rightsVal = ull.getString(self.idx, path, cpopath + 'rights')
			check_status(status)
			for i in range(nbslice):
				slice = putinfostructureputinfo(self.base_path)
				slice.setExpIdx(self.idx)
				slice.putmethod = putmethodVal
				slice.putaccess = putaccessVal
				slice.putlocation = putlocationVal
				slice.rights = rightsVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putmethod') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putmethod', i, self.putmethod)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putaccess') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putaccess', i, self.putaccess)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putlocation') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putlocation', i, self.putlocation)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'rights') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'rights', i, self.rights)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putmethod') 
			print ('obj = ' + str(obj))
		status, ret_putmethod = ull.getStringFromObject(self.idx, obj, cpopath + 'putmethod', i)
		check_status(status)
		if not status:
			self.putmethod = ret_putmethod
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putaccess') 
			print ('obj = ' + str(obj))
		status, ret_putaccess = ull.getStringFromObject(self.idx, obj, cpopath + 'putaccess', i)
		check_status(status)
		if not status:
			self.putaccess = ret_putaccess
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putlocation') 
			print ('obj = ' + str(obj))
		status, ret_putlocation = ull.getStringFromObject(self.idx, obj, cpopath + 'putlocation', i)
		check_status(status)
		if not status:
			self.putlocation = ret_putlocation
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'rights') 
			print ('obj = ' + str(obj))
		status, ret_rights = ull.getStringFromObject(self.idx, obj, cpopath + 'rights', i)
		check_status(status)
		if not status:
			self.rights = ret_rights

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'putmethod')
		ull.deleteData(self.idx, path, cpopath + 'putaccess')
		ull.deleteData(self.idx, path, cpopath + 'putlocation')
		ull.deleteData(self.idx, path, cpopath + 'rights')


class compositionstructurecomposition(KeepInOrder):
	'''
	class compositionstructurecomposition
	Plasma composition (description of ion species). OBSOLESCENT.

	Attributes:
	- amn : numpy.ndarray 1D with float
	   Atomic mass number (lumped ions are allowed); Vector (nion)
	- zn : numpy.ndarray 1D with float
	   Nuclear charge (lumped ions are allowed); Vector (nion)
	- zion : numpy.ndarray 1D with float
	   Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
	- imp_flag : numpy.ndarray 1D with int)
	   Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge state are considered and are described in impurity CPO; Vector (nion)
	- label : list of str
	   Label for the ions - note the charge state is not included; String Vector (nion)
	'''

	def __init__(self, base_path_in='composition'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.amn = numpy.zeros(0, numpy.float64, order='C')
		self.zn = numpy.zeros(0, numpy.float64, order='C')
		self.zion = numpy.zeros(0, numpy.float64, order='C')
		self.imp_flag = numpy.zeros(0, numpy.int32, order='C')
		self.label = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compositionstructurecomposition\n'
		s = self.amn.__str__()
		ret = ret + space + 'Attribute amn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zn.__str__()
		ret = ret + space + 'Attribute zn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zion.__str__()
		ret = ret + space + 'Attribute zion\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.imp_flag.__str__()
		ret = ret + space + 'Attribute imp_flag\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.label.__str__()
		ret = ret + space + 'Attribute label\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'amn', numpy.array(self.amn).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'zn', numpy.array(self.zn).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'zion', numpy.array(self.zion).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'imp_flag', numpy.array(self.imp_flag).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'label', self.label, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_amn = ull.getVect1DDouble(self.idx, path, cpopath + 'amn')
		check_status(status)
		if not status:
			self.amn = ret_amn
		status, ret_zn = ull.getVect1DDouble(self.idx, path, cpopath + 'zn')
		check_status(status)
		if not status:
			self.zn = ret_zn
		status, ret_zion = ull.getVect1DDouble(self.idx, path, cpopath + 'zion')
		check_status(status)
		if not status:
			self.zion = ret_zion
		status, ret_imp_flag = ull.getVect1DInt(self.idx, path, cpopath + 'imp_flag')
		check_status(status)
		if not status:
			self.imp_flag = ret_imp_flag
		status, ret_label = ull.getVect1DString(self.idx, path, cpopath + 'label')
		check_status(status)
		if not status:
			self.label = ret_label

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, amnVal = ull.getVect1DDouble(self.idx, path, cpopath + 'amn')
			check_status(status)
			status, znVal = ull.getVect1DDouble(self.idx, path, cpopath + 'zn')
			check_status(status)
			status, zionVal = ull.getVect1DDouble(self.idx, path, cpopath + 'zion')
			check_status(status)
			status, imp_flagVal = ull.getVect1DInt(self.idx, path, cpopath + 'imp_flag')
			check_status(status)
			status, labelVal = ull.getVect1DString(self.idx, path, cpopath + 'label')
			check_status(status)
			for i in range(nbslice):
				slice = compositionstructurecomposition(self.base_path)
				slice.setExpIdx(self.idx)
				slice.amn = amnVal
				slice.zn = znVal
				slice.zion = zionVal
				slice.imp_flag = imp_flagVal
				slice.label = labelVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'amn', i, numpy.array(self.amn).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zn', i, numpy.array(self.zn).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zion', i, numpy.array(self.zion).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'imp_flag', i, numpy.array(self.imp_flag).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		status, ret_zn = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zn', i)
		check_status(status)
		if not status:
			self.zn = ret_zn
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		status, ret_zion = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zion', i)
		check_status(status)
		if not status:
			self.zion = ret_zion
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		status, ret_imp_flag = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'imp_flag', i)
		check_status(status)
		if not status:
			self.imp_flag = ret_imp_flag
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'amn')
		ull.deleteData(self.idx, path, cpopath + 'zn')
		ull.deleteData(self.idx, path, cpopath + 'zion')
		ull.deleteData(self.idx, path, cpopath + 'imp_flag')
		ull.deleteData(self.idx, path, cpopath + 'label')


class desc_impurstructuredesc_impur(KeepInOrder):
	'''
	class desc_impurstructuredesc_impur
	Description of the impurities (list of ion species and possibly different charge states). OBSOLESCENT.

	Attributes:
	- amn : numpy.ndarray 1D with float
	   Atomic mass number of the impurity; Vector (nimp)
	- zn : numpy.ndarray 1D with int)
	   Nuclear charge of the impurity; Vector (nimp)
	- i_ion : numpy.ndarray 1D with int)
	   Index of the impurity species in the coreprof ion species ordering. Vector (nimp)
	- nzimp : numpy.ndarray 1D with int)
	   Number of charge states (or bundles) considered for each impurity species. Vector (nimp)
	- zmin : numpy.ndarray 2D with int
	   Minimum Z of impurity ionisation state bundle. Matrix (nimp,max_nzimp)
	- zmax : numpy.ndarray 2D with int
	   Maximum Z of impurity ionisation state bundle. If no bundle, zmax=zmin. Matrix (nimp,max_nzimp)
	- label : list of str
	   Label for the impurities - note that the charge state is not included; String Vector (nimp)
	'''

	def __init__(self, base_path_in='desc_impur'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.amn = numpy.zeros(0, numpy.float64, order='C')
		self.zn = numpy.zeros(0, numpy.int32, order='C')
		self.i_ion = numpy.zeros(0, numpy.int32, order='C')
		self.nzimp = numpy.zeros(0, numpy.int32, order='C')
		self.zmin = numpy.zeros((0,0), numpy.int32, order='C')
		self.zmax = numpy.zeros((0,0), numpy.int32, order='C')
		self.label = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class desc_impurstructuredesc_impur\n'
		s = self.amn.__str__()
		ret = ret + space + 'Attribute amn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zn.__str__()
		ret = ret + space + 'Attribute zn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.i_ion.__str__()
		ret = ret + space + 'Attribute i_ion\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.nzimp.__str__()
		ret = ret + space + 'Attribute nzimp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zmin.__str__()
		ret = ret + space + 'Attribute zmin\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zmax.__str__()
		ret = ret + space + 'Attribute zmax\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.label.__str__()
		ret = ret + space + 'Attribute label\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_impurstructuredesc_impur, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_impurstructuredesc_impur, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_impurstructuredesc_impur, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'amn', numpy.array(self.amn).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'zn', numpy.array(self.zn).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'i_ion', numpy.array(self.i_ion).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'nzimp', numpy.array(self.nzimp).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect2DInt(self.idx, path, cpopath + 'zmin', numpy.array(self.zmin).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect2DInt(self.idx, path, cpopath + 'zmax', numpy.array(self.zmax).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'label', self.label, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type desc_impurstructuredesc_impur, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_amn = ull.getVect1DDouble(self.idx, path, cpopath + 'amn')
		check_status(status)
		if not status:
			self.amn = ret_amn
		status, ret_zn = ull.getVect1DInt(self.idx, path, cpopath + 'zn')
		check_status(status)
		if not status:
			self.zn = ret_zn
		status, ret_i_ion = ull.getVect1DInt(self.idx, path, cpopath + 'i_ion')
		check_status(status)
		if not status:
			self.i_ion = ret_i_ion
		status, ret_nzimp = ull.getVect1DInt(self.idx, path, cpopath + 'nzimp')
		check_status(status)
		if not status:
			self.nzimp = ret_nzimp
		status, ret_zmin = ull.getVect2DInt(self.idx, path, cpopath + 'zmin')
		check_status(status)
		if not status:
			self.zmin = ret_zmin
		status, ret_zmax = ull.getVect2DInt(self.idx, path, cpopath + 'zmax')
		check_status(status)
		if not status:
			self.zmax = ret_zmax
		status, ret_label = ull.getVect1DString(self.idx, path, cpopath + 'label')
		check_status(status)
		if not status:
			self.label = ret_label

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type desc_impurstructuredesc_impur, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, amnVal = ull.getVect1DDouble(self.idx, path, cpopath + 'amn')
			check_status(status)
			status, znVal = ull.getVect1DInt(self.idx, path, cpopath + 'zn')
			check_status(status)
			status, i_ionVal = ull.getVect1DInt(self.idx, path, cpopath + 'i_ion')
			check_status(status)
			status, nzimpVal = ull.getVect1DInt(self.idx, path, cpopath + 'nzimp')
			check_status(status)
			status, zminVal = ull.getVect2DInt(self.idx, path, cpopath + 'zmin')
			check_status(status)
			status, zmaxVal = ull.getVect2DInt(self.idx, path, cpopath + 'zmax')
			check_status(status)
			status, labelVal = ull.getVect1DString(self.idx, path, cpopath + 'label')
			check_status(status)
			for i in range(nbslice):
				slice = desc_impurstructuredesc_impur(self.base_path)
				slice.setExpIdx(self.idx)
				slice.amn = amnVal
				slice.zn = znVal
				slice.i_ion = i_ionVal
				slice.nzimp = nzimpVal
				slice.zmin = zminVal
				slice.zmax = zmaxVal
				slice.label = labelVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_impurstructuredesc_impurObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_impurstructuredesc_impurObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_impurstructuredesc_impurObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'amn', i, numpy.array(self.amn).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'zn', i, numpy.array(self.zn).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'i_ion') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'i_ion', i, numpy.array(self.i_ion).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'nzimp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'nzimp', i, numpy.array(self.nzimp).astype(numpy.int32))
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'zmin', i, numpy.array(self.zmin).astype(numpy.int32))
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'zmax', i, numpy.array(self.zmax).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_impurstructuredesc_impurObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		status, ret_zn = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'zn', i)
		check_status(status)
		if not status:
			self.zn = ret_zn
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'i_ion') 
			print ('obj = ' + str(obj))
		status, ret_i_ion = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'i_ion', i)
		check_status(status)
		if not status:
			self.i_ion = ret_i_ion
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'nzimp') 
			print ('obj = ' + str(obj))
		status, ret_nzimp = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'nzimp', i)
		check_status(status)
		if not status:
			self.nzimp = ret_nzimp
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		status, ret_zmin = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'zmin', i)
		check_status(status)
		if not status:
			self.zmin = ret_zmin
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		status, ret_zmax = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'zmax', i)
		check_status(status)
		if not status:
			self.zmax = ret_zmax
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'amn')
		ull.deleteData(self.idx, path, cpopath + 'zn')
		ull.deleteData(self.idx, path, cpopath + 'i_ion')
		ull.deleteData(self.idx, path, cpopath + 'nzimp')
		ull.deleteData(self.idx, path, cpopath + 'zmin')
		ull.deleteData(self.idx, path, cpopath + 'zmax')
		ull.deleteData(self.idx, path, cpopath + 'label')


class compositionsstructurecompositions_type(KeepInOrder):
	'''
	class compositionsstructurecompositions_type
	Contains all the composition information for the simulation (main ions, impurities, neutrals, edge species).

	Attributes:
	- nuclei : class nucleistruct_arraynuclei: array of nucleistruct_arraynucleiObj objects
	   Array of nuclei considered.
	- ions : class ionsstruct_arrayions: array of ionsstruct_arrayionsObj objects
	   Array of main plasma ions.
	- impurities : class impuritiesstruct_arrayimpurities: array of impuritiesstruct_arrayimpuritiesObj objects
	   Array of impurities.
	- neutralscomp : class neutralscompstruct_arraycomposition_neutralscomp: array of neutralscompstruct_arraycomposition_neutralscompObj objects
	   Array of neutrals.
	- edgespecies : class edgespeciesstruct_arrayedgespecies: array of edgespeciesstruct_arrayedgespeciesObj objects
	   Array of edge species.
	- signature : class signaturestructureidentifier
	   Identifier for species choices. The goal of this is to uniquely capture the species blocks so that if the signatures are the same then the species blocks will also be the same.
	'''

	def __init__(self, base_path_in='compositions'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nuclei = nucleistruct_arraynuclei('nuclei')
		self.ions = ionsstruct_arrayions('ions')
		self.impurities = impuritiesstruct_arrayimpurities('impurities')
		self.neutralscomp = neutralscompstruct_arraycomposition_neutralscomp('neutralscomp')
		self.edgespecies = edgespeciesstruct_arrayedgespecies('edgespecies')
		self.signature = signaturestructureidentifier('signature')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compositionsstructurecompositions_type\n'
		ret = ret + space + 'Attribute nuclei\n ' + self.nuclei.__str__(depth+1)
		ret = ret + space + 'Attribute ions\n ' + self.ions.__str__(depth+1)
		ret = ret + space + 'Attribute impurities\n ' + self.impurities.__str__(depth+1)
		ret = ret + space + 'Attribute neutralscomp\n ' + self.neutralscomp.__str__(depth+1)
		ret = ret + space + 'Attribute edgespecies\n ' + self.edgespecies.__str__(depth+1)
		ret = ret + space + 'Attribute signature\n ' + self.signature.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.nuclei.setExpIdx(idx)
		self.ions.setExpIdx(idx)
		self.impurities.setExpIdx(idx)
		self.neutralscomp.setExpIdx(idx)
		self.edgespecies.setExpIdx(idx)
		self.signature.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.cpoTime = self.cpoTime
		self.nuclei.putSlice(path, cpopath)
		self.ions.cpoTime = self.cpoTime
		self.ions.putSlice(path, cpopath)
		self.impurities.cpoTime = self.cpoTime
		self.impurities.putSlice(path, cpopath)
		self.neutralscomp.cpoTime = self.cpoTime
		self.neutralscomp.putSlice(path, cpopath)
		self.edgespecies.cpoTime = self.cpoTime
		self.edgespecies.putSlice(path, cpopath)
		self.signature.cpoTime = self.cpoTime
		self.signature.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.replaceLastSlice(path, cpopath)
		self.ions.replaceLastSlice(path, cpopath)
		self.impurities.replaceLastSlice(path, cpopath)
		self.neutralscomp.replaceLastSlice(path, cpopath)
		self.edgespecies.replaceLastSlice(path, cpopath)
		self.signature.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.putNonTimed(path, cpopath)
		self.ions.putNonTimed(path, cpopath)
		self.impurities.putNonTimed(path, cpopath)
		self.neutralscomp.putNonTimed(path, cpopath)
		self.edgespecies.putNonTimed(path, cpopath)
		self.signature.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.getSlice(path, cpopath, inTime, interpolMode)
		self.ions.getSlice(path, cpopath, inTime, interpolMode)
		self.impurities.getSlice(path, cpopath, inTime, interpolMode)
		self.neutralscomp.getSlice(path, cpopath, inTime, interpolMode)
		self.edgespecies.getSlice(path, cpopath, inTime, interpolMode)
		self.signature.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			nucleiList = self.nuclei.build_non_resampled_data(path, cpopath, nbslice)
			ionsList = self.ions.build_non_resampled_data(path, cpopath, nbslice)
			impuritiesList = self.impurities.build_non_resampled_data(path, cpopath, nbslice)
			neutralscompList = self.neutralscomp.build_non_resampled_data(path, cpopath, nbslice)
			edgespeciesList = self.edgespecies.build_non_resampled_data(path, cpopath, nbslice)
			signatureList = self.signature.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = compositionsstructurecompositions_type(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nuclei = nucleiList[i]
				slice.ions = ionsList[i]
				slice.impurities = impuritiesList[i]
				slice.neutralscomp = neutralscompList[i]
				slice.edgespecies = edgespeciesList[i]
				slice.signature = signatureList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.nuclei.putNonTimedElt(path, cpopath + 'nuclei', i, obj)
		obj = self.ions.putNonTimedElt(path, cpopath + 'ions', i, obj)
		obj = self.impurities.putNonTimedElt(path, cpopath + 'impurities', i, obj)
		obj = self.neutralscomp.putNonTimedElt(path, cpopath + 'neutralscomp', i, obj)
		obj = self.edgespecies.putNonTimedElt(path, cpopath + 'edgespecies', i, obj)
		obj = self.signature.putNonTimedElt(path, cpopath + 'signature', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.nuclei.getNonTimedElt(path, cpopath + 'nuclei', i, obj)
		self.ions.getNonTimedElt(path, cpopath + 'ions', i, obj)
		self.impurities.getNonTimedElt(path, cpopath + 'impurities', i, obj)
		self.neutralscomp.getNonTimedElt(path, cpopath + 'neutralscomp', i, obj)
		self.edgespecies.getNonTimedElt(path, cpopath + 'edgespecies', i, obj)
		self.signature.getNonTimedElt(path, cpopath + 'signature', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nuclei')
		ull.deleteData(self.idx, path, cpopath + 'ions')
		ull.deleteData(self.idx, path, cpopath + 'impurities')
		ull.deleteData(self.idx, path, cpopath + 'neutralscomp')
		ull.deleteData(self.idx, path, cpopath + 'edgespecies')
		self.signature.deleteData(path, cpopath)


class nucleistruct_arraynuclei:
	'''
	class nucleistruct_arraynuclei
	Array of nuclei considered.

	Attributes:
	- array : list of nucleistruct_arraynucleiObj 
	'''

	def __init__(self, base_path_in='nuclei'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nucleistruct_arraynuclei\n'
		for i in range(len(self.array)):
			ret = ret + space + 'nucleistruct_arraynuclei[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(nucleistruct_arraynucleiObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(nucleistruct_arraynuclei(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = nucleistruct_arraynuclei(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class nucleistruct_arraynucleiObj(KeepInOrder):
	'''
	class nucleistruct_arraynucleiObj
	Array of nuclei considered.

	Attributes:
	- zn : float
	   Nuclear charge [units of elementary charge];
	- amn : float
	   Mass of atom [amu]
	- label : str
	   String identifying element (e.g. H, D, T, He, C, ...)
	'''

	def __init__(self, base_path_in='nuclei'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.zn = EMPTY_DOUBLE
		self.amn = EMPTY_DOUBLE
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nucleistruct_arraynucleiObj\n'
		ret = ret + space + 'Attribute zn: ' + str(self.zn) + '\n'
		ret = ret + space + 'Attribute amn: ' + str(self.amn) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nucleistruct_arraynucleiObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zn', i, self.zn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'amn', i, self.amn)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nucleistruct_arraynucleiObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		status, ret_zn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zn', i)
		check_status(status)
		if not status:
			self.zn = ret_zn
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class ionsstruct_arrayions:
	'''
	class ionsstruct_arrayions
	Array of main plasma ions.

	Attributes:
	- array : list of ionsstruct_arrayionsObj 
	'''

	def __init__(self, base_path_in='ions'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ionsstruct_arrayions\n'
		for i in range(len(self.array)):
			ret = ret + space + 'ionsstruct_arrayions[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(ionsstruct_arrayionsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(ionsstruct_arrayions(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = ionsstruct_arrayions(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class ionsstruct_arrayionsObj(KeepInOrder):
	'''
	class ionsstruct_arrayionsObj
	Array of main plasma ions.

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- zion : float
	   Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
	- imp_flag : int
	   Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge state are considered and are described in impurity CPO; Vector (nion)
	- label : str
	   String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
	'''

	def __init__(self, base_path_in='ions'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.zion = EMPTY_DOUBLE
		self.imp_flag = EMPTY_INT
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ionsstruct_arrayionsObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute zion: ' + str(self.zion) + '\n'
		ret = ret + space + 'Attribute imp_flag: ' + str(self.imp_flag) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ionsstruct_arrayionsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zion', i, self.zion)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'imp_flag', i, self.imp_flag)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ionsstruct_arrayionsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		status, ret_zion = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zion', i)
		check_status(status)
		if not status:
			self.zion = ret_zion
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		status, ret_imp_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'imp_flag', i)
		check_status(status)
		if not status:
			self.imp_flag = ret_imp_flag
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class impuritiesstruct_arrayimpurities:
	'''
	class impuritiesstruct_arrayimpurities
	Array of impurities.

	Attributes:
	- array : list of impuritiesstruct_arrayimpuritiesObj 
	'''

	def __init__(self, base_path_in='impurities'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class impuritiesstruct_arrayimpurities\n'
		for i in range(len(self.array)):
			ret = ret + space + 'impuritiesstruct_arrayimpurities[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(impuritiesstruct_arrayimpuritiesObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(impuritiesstruct_arrayimpurities(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = impuritiesstruct_arrayimpurities(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class impuritiesstruct_arrayimpuritiesObj(KeepInOrder):
	'''
	class impuritiesstruct_arrayimpuritiesObj
	Array of impurities.

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- i_ion : int
	   Index of the impurity species in the ions array of structures. Vector (nimp)
	- nzimp : int
	   Number of charge states (or bundles) considered for this impurity species.
	- zmin : numpy.ndarray 1D with float
	   Minimum Z of impurity ionisation state bundle. Vector (nzimp)
	- zmax : numpy.ndarray 1D with float
	   Maximum Z of impurity ionisation state bundle. If no bundle, zmax=zmin. Vector (nzimp)
	- label : list of str
	   String array (nzimp) identifying impurities (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
	'''

	def __init__(self, base_path_in='impurities'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.i_ion = EMPTY_INT
		self.nzimp = EMPTY_INT
		self.zmin = numpy.zeros(0, numpy.float64, order='C')
		self.zmax = numpy.zeros(0, numpy.float64, order='C')
		self.label = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class impuritiesstruct_arrayimpuritiesObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute i_ion: ' + str(self.i_ion) + '\n'
		ret = ret + space + 'Attribute nzimp: ' + str(self.nzimp) + '\n'
		s = self.zmin.__str__()
		ret = ret + space + 'Attribute zmin\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zmax.__str__()
		ret = ret + space + 'Attribute zmax\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.label.__str__()
		ret = ret + space + 'Attribute label\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritiesstruct_arrayimpuritiesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'i_ion') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'i_ion', i, self.i_ion)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nzimp') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nzimp', i, self.nzimp)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zmin', i, numpy.array(self.zmin).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zmax', i, numpy.array(self.zmax).astype(numpy.float64))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritiesstruct_arrayimpuritiesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'i_ion') 
			print ('obj = ' + str(obj))
		status, ret_i_ion = ull.getIntFromObject(self.idx, obj, cpopath + 'i_ion', i)
		check_status(status)
		if not status:
			self.i_ion = ret_i_ion
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nzimp') 
			print ('obj = ' + str(obj))
		status, ret_nzimp = ull.getIntFromObject(self.idx, obj, cpopath + 'nzimp', i)
		check_status(status)
		if not status:
			self.nzimp = ret_nzimp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		status, ret_zmin = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zmin', i)
		check_status(status)
		if not status:
			self.zmin = ret_zmin
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		status, ret_zmax = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zmax', i)
		check_status(status)
		if not status:
			self.zmax = ret_zmax
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class neutralscompstruct_arraycomposition_neutralscomp:
	'''
	class neutralscompstruct_arraycomposition_neutralscomp
	Array of neutrals.

	Attributes:
	- array : list of neutralscompstruct_arraycomposition_neutralscompObj 
	'''

	def __init__(self, base_path_in='neutralscomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutralscompstruct_arraycomposition_neutralscomp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'neutralscompstruct_arraycomposition_neutralscomp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(neutralscompstruct_arraycomposition_neutralscompObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(neutralscompstruct_arraycomposition_neutralscomp(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = neutralscompstruct_arraycomposition_neutralscomp(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class neutralscompstruct_arraycomposition_neutralscompObj(KeepInOrder):
	'''
	class neutralscompstruct_arraycomposition_neutralscompObj
	Array of neutrals.

	Attributes:
	- neutcomp : class neutcompstruct_arraycomposition_neutrals_neutcomp: array of neutcompstruct_arraycomposition_neutrals_neutcompObj objects
	   Array of components to the atom or molecule. Vector (ncomp)
	- type : class typestruct_arrayidentifier: array of typestruct_arrayidentifierObj objects
	   Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Vector (ntype) of identifiers
	- label : str
	   String identifying the atom or molecule (e.g. D2, DT, CD4, ...)
	'''

	def __init__(self, base_path_in='neutralscomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.neutcomp = neutcompstruct_arraycomposition_neutrals_neutcomp('neutcomp')
		self.type = typestruct_arrayidentifier('type')
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutralscompstruct_arraycomposition_neutralscompObj\n'
		ret = ret + space + 'Attribute neutcomp\n ' + self.neutcomp.__str__(depth+1)
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.neutcomp.setExpIdx(idx)
		self.type.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutralscompstruct_arraycomposition_neutralscompObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.neutcomp.putNonTimedElt(path, cpopath + 'neutcomp', i, obj)
		obj = self.type.putNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutralscompstruct_arraycomposition_neutralscompObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.neutcomp.getNonTimedElt(path, cpopath + 'neutcomp', i, obj)
		self.type.getNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class neutcompstruct_arraycomposition_neutrals_neutcomp:
	'''
	class neutcompstruct_arraycomposition_neutrals_neutcomp
	Array of components to the atom or molecule. Vector (ncomp)

	Attributes:
	- array : list of neutcompstruct_arraycomposition_neutrals_neutcompObj 
	'''

	def __init__(self, base_path_in='neutcomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutcompstruct_arraycomposition_neutrals_neutcomp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'neutcompstruct_arraycomposition_neutrals_neutcomp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(neutcompstruct_arraycomposition_neutrals_neutcompObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(neutcompstruct_arraycomposition_neutrals_neutcomp(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = neutcompstruct_arraycomposition_neutrals_neutcomp(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class neutcompstruct_arraycomposition_neutrals_neutcompObj(KeepInOrder):
	'''
	class neutcompstruct_arraycomposition_neutrals_neutcompObj
	Array of components to the atom or molecule. Vector (ncomp)

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- multiplicity : int
	   Multiplicity of the atom; int
	'''

	def __init__(self, base_path_in='neutcomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.multiplicity = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutcompstruct_arraycomposition_neutrals_neutcompObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute multiplicity: ' + str(self.multiplicity) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutcompstruct_arraycomposition_neutrals_neutcompObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'multiplicity') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'multiplicity', i, self.multiplicity)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutcompstruct_arraycomposition_neutrals_neutcompObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'multiplicity') 
			print ('obj = ' + str(obj))
		status, ret_multiplicity = ull.getIntFromObject(self.idx, obj, cpopath + 'multiplicity', i)
		check_status(status)
		if not status:
			self.multiplicity = ret_multiplicity


class typestruct_arrayidentifier:
	'''
	class typestruct_arrayidentifier
	Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Vector (ntype) of identifiers

	Attributes:
	- array : list of typestruct_arrayidentifierObj 
	'''

	def __init__(self, base_path_in='type'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class typestruct_arrayidentifier\n'
		for i in range(len(self.array)):
			ret = ret + space + 'typestruct_arrayidentifier[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(typestruct_arrayidentifierObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(typestruct_arrayidentifier(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = typestruct_arrayidentifier(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class typestruct_arrayidentifierObj(KeepInOrder):
	'''
	class typestruct_arrayidentifierObj
	Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Vector (ntype) of identifiers

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='type'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class typestruct_arrayidentifierObj\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type typestruct_arrayidentifierObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'description', i, self.description)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type typestruct_arrayidentifierObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		status, ret_description = ull.getStringFromObject(self.idx, obj, cpopath + 'description', i)
		check_status(status)
		if not status:
			self.description = ret_description


class edgespeciesstruct_arrayedgespecies:
	'''
	class edgespeciesstruct_arrayedgespecies
	Array of edge species.

	Attributes:
	- array : list of edgespeciesstruct_arrayedgespeciesObj 
	'''

	def __init__(self, base_path_in='edgespecies'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class edgespeciesstruct_arrayedgespecies\n'
		for i in range(len(self.array)):
			ret = ret + space + 'edgespeciesstruct_arrayedgespecies[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(edgespeciesstruct_arrayedgespeciesObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(edgespeciesstruct_arrayedgespecies(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = edgespeciesstruct_arrayedgespecies(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class edgespeciesstruct_arrayedgespeciesObj(KeepInOrder):
	'''
	class edgespeciesstruct_arrayedgespeciesObj
	Array of edge species.

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- zmin : float
	   Minimum Z of species charge state bundle
	- zmax : float
	   Maximum Z of species charge state bundle
	- label : str
	   String identifying the species (e.g. D0, D+, C0, C+, C+2, ...)
	'''

	def __init__(self, base_path_in='edgespecies'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.zmin = EMPTY_DOUBLE
		self.zmax = EMPTY_DOUBLE
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class edgespeciesstruct_arrayedgespeciesObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute zmin: ' + str(self.zmin) + '\n'
		ret = ret + space + 'Attribute zmax: ' + str(self.zmax) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type edgespeciesstruct_arrayedgespeciesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zmin', i, self.zmin)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zmax', i, self.zmax)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type edgespeciesstruct_arrayedgespeciesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		status, ret_zmin = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zmin', i)
		check_status(status)
		if not status:
			self.zmin = ret_zmin
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		status, ret_zmax = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zmax', i)
		check_status(status)
		if not status:
			self.zmax = ret_zmax
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class signaturestructureidentifier(KeepInOrder):
	'''
	class signaturestructureidentifier
	Identifier for species choices. The goal of this is to uniquely capture the species blocks so that if the signatures are the same then the species blocks will also be the same.

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='signature'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class signaturestructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'id', self.id)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'description', self.description)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_id = ull.getString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag
		status, ret_description = ull.getString(self.idx, path, cpopath + 'description')
		check_status(status)
		if not status:
			self.description = ret_description

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, idVal = ull.getString(self.idx, path, cpopath + 'id')
			check_status(status)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			status, descriptionVal = ull.getString(self.idx, path, cpopath + 'description')
			check_status(status)
			for i in range(nbslice):
				slice = signaturestructureidentifier(self.base_path)
				slice.setExpIdx(self.idx)
				slice.id = idVal
				slice.flag = flagVal
				slice.description = descriptionVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type signaturestructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type signaturestructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type signaturestructureidentifierObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'description', i, self.description)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type signaturestructureidentifierObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		status, ret_description = ull.getStringFromObject(self.idx, obj, cpopath + 'description', i)
		check_status(status)
		if not status:
			self.description = ret_description

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		ull.deleteData(self.idx, path, cpopath + 'description')


class ni_neostructuretranscoefion(KeepInOrder):
	'''
	class ni_neostructuretranscoefion
	Neoclassical transport coefficients for ion density equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 2D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- vconv_eff : numpy.ndarray 2D with float
	   Effective convection [m.s^-1]. Time-dependent. Matrix (nrho,nion)
	- exchange : numpy.ndarray 2D with float
	   Ion to electron energy exchange [W.m^-3]. Time-dependent. Matrix(nrho,nion).
	- qgi : numpy.ndarray 2D with float
	   Energy exchange term due to transport. [W.m^-3]. Time-dependent. Matrix (nrho,nion)
	- flux : numpy.ndarray 2D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
	- off_diagonal : class off_diagonalstructureoffdiagion
	   Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix. Scalar.
	'''

	def __init__(self, base_path_in='ni_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.vconv_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.exchange = numpy.zeros((0,0), numpy.float64, order='C')
		self.qgi = numpy.zeros((0,0), numpy.float64, order='C')
		self.flux = numpy.zeros((0,0), numpy.float64, order='C')
		self.off_diagonal = off_diagonalstructureoffdiagion('off_diagonal')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ni_neostructuretranscoefion\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.exchange.__str__()
		ret = ret + space + 'Attribute exchange\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.qgi.__str__()
		ret = ret + space + 'Attribute qgi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute off_diagonal\n ' + self.off_diagonal.__str__(depth+1)
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.off_diagonal.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ni_neostructuretranscoefion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'exchange', numpy.array(self.exchange).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'qgi', numpy.array(self.qgi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.off_diagonal.cpoTime = self.cpoTime
		self.off_diagonal.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ni_neostructuretranscoefion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'exchange', numpy.array(self.exchange).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'qgi', numpy.array(self.qgi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64))
		check_status(status)
		self.off_diagonal.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ni_neostructuretranscoefion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.off_diagonal.putNonTimed(path, cpopath)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type ni_neostructuretranscoefion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff_eff, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'diff_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
			self.cpoTime = retTime
		status, ret_vconv_eff, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
			self.cpoTime = retTime
		status, ret_exchange, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'exchange', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exchange = ret_exchange
			self.cpoTime = retTime
		status, ret_qgi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'qgi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qgi = ret_qgi
			self.cpoTime = retTime
		status, ret_flux, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux = ret_flux
			self.cpoTime = retTime
		self.off_diagonal.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type ni_neostructuretranscoefion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diff_effList = ull.getVect3DDouble(self.idx, path, cpopath + 'diff_eff')
			if len(diff_effList) == 0:
				diff_effList = numpy.resize(diff_effList, (0,0,nbslice))
			check_status(status)
			status, vconv_effList = ull.getVect3DDouble(self.idx, path, cpopath + 'vconv_eff')
			if len(vconv_effList) == 0:
				vconv_effList = numpy.resize(vconv_effList, (0,0,nbslice))
			check_status(status)
			status, exchangeList = ull.getVect3DDouble(self.idx, path, cpopath + 'exchange')
			if len(exchangeList) == 0:
				exchangeList = numpy.resize(exchangeList, (0,0,nbslice))
			check_status(status)
			status, qgiList = ull.getVect3DDouble(self.idx, path, cpopath + 'qgi')
			if len(qgiList) == 0:
				qgiList = numpy.resize(qgiList, (0,0,nbslice))
			check_status(status)
			status, fluxList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux')
			if len(fluxList) == 0:
				fluxList = numpy.resize(fluxList, (0,0,nbslice))
			check_status(status)
			off_diagonalList = self.off_diagonal.build_non_resampled_data(path, cpopath, nbslice)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			for i in range(nbslice):
				slice = ni_neostructuretranscoefion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff_eff = diff_effList[:,:,i]
				slice.vconv_eff = vconv_effList[:,:,i]
				slice.exchange = exchangeList[:,:,i]
				slice.qgi = qgiList[:,:,i]
				slice.flux = fluxList[:,:,i]
				slice.off_diagonal = off_diagonalList[i]
				slice.flag = flagVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ni_neostructuretranscoefionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'exchange', i, numpy.array(self.exchange).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'qgi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'qgi', i, numpy.array(self.qgi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		obj = self.off_diagonal.putTimedElt(path, cpopath + 'off_diagonal', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ni_neostructuretranscoefionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		status, ret_exchange = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'exchange', i)
		check_status(status)
		if not status:
			self.exchange = ret_exchange
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'qgi') 
			print ('obj = ' + str(obj))
		status, ret_qgi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'qgi', i)
		check_status(status)
		if not status:
			self.qgi = ret_qgi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		self.off_diagonal.getTimedElt(path, cpopath + 'off_diagonal', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ni_neostructuretranscoefionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.off_diagonal.putNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ni_neostructuretranscoefionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.off_diagonal.getNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff_eff')
		ull.deleteData(self.idx, path, cpopath + 'vconv_eff')
		ull.deleteData(self.idx, path, cpopath + 'exchange')
		ull.deleteData(self.idx, path, cpopath + 'qgi')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		self.off_diagonal.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flag')


class off_diagonalstructureoffdiagion(KeepInOrder):
	'''
	class off_diagonalstructureoffdiagion
	Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.

	Attributes:
	- d_ni : numpy.ndarray 3D with float
	   Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Array3d (nrho,nion,nion)
	- d_ti : numpy.ndarray 3D with float
	   Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Array3d (nrho,nion,nion)
	- d_ne : numpy.ndarray 2D with float
	   Off-Diagonal term coupling electron density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- d_te : numpy.ndarray 2D with float
	   Off-Diagonal term coupling electron temperature gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- d_epar : numpy.ndarray 2D with float
	   Off-Diagonal term coupling parallel electric field to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- d_mtor : numpy.ndarray 2D with float
	   Off-Diagonal term coupling total toroidal momentum to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	'''

	def __init__(self, base_path_in='off_diagonal'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.d_ni = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.d_ti = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.d_ne = numpy.zeros((0,0), numpy.float64, order='C')
		self.d_te = numpy.zeros((0,0), numpy.float64, order='C')
		self.d_epar = numpy.zeros((0,0), numpy.float64, order='C')
		self.d_mtor = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class off_diagonalstructureoffdiagion\n'
		s = self.d_ni.__str__()
		ret = ret + space + 'Attribute d_ni\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_ti.__str__()
		ret = ret + space + 'Attribute d_ti\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_ne.__str__()
		ret = ret + space + 'Attribute d_ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_te.__str__()
		ret = ret + space + 'Attribute d_te\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_epar.__str__()
		ret = ret + space + 'Attribute d_epar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_mtor.__str__()
		ret = ret + space + 'Attribute d_mtor\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'd_ni', numpy.array(self.d_ni).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'd_ti', numpy.array(self.d_ti).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd_ne', numpy.array(self.d_ne).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd_te', numpy.array(self.d_te).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd_epar', numpy.array(self.d_epar).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd_mtor', numpy.array(self.d_mtor).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'd_ni', numpy.array(self.d_ni).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'd_ti', numpy.array(self.d_ti).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd_ne', numpy.array(self.d_ne).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd_te', numpy.array(self.d_te).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd_epar', numpy.array(self.d_epar).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd_mtor', numpy.array(self.d_mtor).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_d_ni, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'd_ni', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_ni = ret_d_ni
			self.cpoTime = retTime
		status, ret_d_ti, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'd_ti', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_ti = ret_d_ti
			self.cpoTime = retTime
		status, ret_d_ne, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd_ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_ne = ret_d_ne
			self.cpoTime = retTime
		status, ret_d_te, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd_te', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_te = ret_d_te
			self.cpoTime = retTime
		status, ret_d_epar, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd_epar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_epar = ret_d_epar
			self.cpoTime = retTime
		status, ret_d_mtor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd_mtor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_mtor = ret_d_mtor
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, d_niList = ull.getVect4DDouble(self.idx, path, cpopath + 'd_ni')
			if len(d_niList) == 0:
				d_niList = numpy.resize(d_niList, (0,0,0,nbslice))
			check_status(status)
			status, d_tiList = ull.getVect4DDouble(self.idx, path, cpopath + 'd_ti')
			if len(d_tiList) == 0:
				d_tiList = numpy.resize(d_tiList, (0,0,0,nbslice))
			check_status(status)
			status, d_neList = ull.getVect3DDouble(self.idx, path, cpopath + 'd_ne')
			if len(d_neList) == 0:
				d_neList = numpy.resize(d_neList, (0,0,nbslice))
			check_status(status)
			status, d_teList = ull.getVect3DDouble(self.idx, path, cpopath + 'd_te')
			if len(d_teList) == 0:
				d_teList = numpy.resize(d_teList, (0,0,nbslice))
			check_status(status)
			status, d_eparList = ull.getVect3DDouble(self.idx, path, cpopath + 'd_epar')
			if len(d_eparList) == 0:
				d_eparList = numpy.resize(d_eparList, (0,0,nbslice))
			check_status(status)
			status, d_mtorList = ull.getVect3DDouble(self.idx, path, cpopath + 'd_mtor')
			if len(d_mtorList) == 0:
				d_mtorList = numpy.resize(d_mtorList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = off_diagonalstructureoffdiagion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.d_ni = d_niList[:,:,:,i]
				slice.d_ti = d_tiList[:,:,:,i]
				slice.d_ne = d_neList[:,:,i]
				slice.d_te = d_teList[:,:,i]
				slice.d_epar = d_eparList[:,:,i]
				slice.d_mtor = d_mtorList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'd_ni') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'd_ni', i, numpy.array(self.d_ni).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'd_ti') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'd_ti', i, numpy.array(self.d_ti).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd_ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd_ne', i, numpy.array(self.d_ne).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd_te') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd_te', i, numpy.array(self.d_te).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd_epar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd_epar', i, numpy.array(self.d_epar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd_mtor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd_mtor', i, numpy.array(self.d_mtor).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'd_ni') 
			print ('obj = ' + str(obj))
		status, ret_d_ni = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'd_ni', i)
		check_status(status)
		if not status:
			self.d_ni = ret_d_ni
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'd_ti') 
			print ('obj = ' + str(obj))
		status, ret_d_ti = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'd_ti', i)
		check_status(status)
		if not status:
			self.d_ti = ret_d_ti
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd_ne') 
			print ('obj = ' + str(obj))
		status, ret_d_ne = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd_ne', i)
		check_status(status)
		if not status:
			self.d_ne = ret_d_ne
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd_te') 
			print ('obj = ' + str(obj))
		status, ret_d_te = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd_te', i)
		check_status(status)
		if not status:
			self.d_te = ret_d_te
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd_epar') 
			print ('obj = ' + str(obj))
		status, ret_d_epar = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd_epar', i)
		check_status(status)
		if not status:
			self.d_epar = ret_d_epar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd_mtor') 
			print ('obj = ' + str(obj))
		status, ret_d_mtor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd_mtor', i)
		check_status(status)
		if not status:
			self.d_mtor = ret_d_mtor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'd_ni')
		ull.deleteData(self.idx, path, cpopath + 'd_ti')
		ull.deleteData(self.idx, path, cpopath + 'd_ne')
		ull.deleteData(self.idx, path, cpopath + 'd_te')
		ull.deleteData(self.idx, path, cpopath + 'd_epar')
		ull.deleteData(self.idx, path, cpopath + 'd_mtor')


class ne_neostructuretranscoefel(KeepInOrder):
	'''
	class ne_neostructuretranscoefel
	Neoclassical transport coefficients for electron density equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 1D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Vector (nrho)
	- vconv_eff : numpy.ndarray 1D with float
	   Effective convection [m.s^-1]. Time-dependent. Vector (nrho)
	- flux : numpy.ndarray 1D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
	- off_diagonal : class off_diagonalstructureoffdiagel
	   Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix. Scalar.
	'''

	def __init__(self, base_path_in='ne_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros(0, numpy.float64, order='C')
		self.vconv_eff = numpy.zeros(0, numpy.float64, order='C')
		self.flux = numpy.zeros(0, numpy.float64, order='C')
		self.off_diagonal = off_diagonalstructureoffdiagel('off_diagonal')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ne_neostructuretranscoefel\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute off_diagonal\n ' + self.off_diagonal.__str__(depth+1)
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.off_diagonal.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ne_neostructuretranscoefel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.off_diagonal.cpoTime = self.cpoTime
		self.off_diagonal.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ne_neostructuretranscoefel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64))
		check_status(status)
		self.off_diagonal.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ne_neostructuretranscoefel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.off_diagonal.putNonTimed(path, cpopath)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type ne_neostructuretranscoefel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff_eff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
			self.cpoTime = retTime
		status, ret_vconv_eff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
			self.cpoTime = retTime
		status, ret_flux, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux = ret_flux
			self.cpoTime = retTime
		self.off_diagonal.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type ne_neostructuretranscoefel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diff_effList = ull.getVect2DDouble(self.idx, path, cpopath + 'diff_eff')
			if len(diff_effList) == 0:
				diff_effList = numpy.resize(diff_effList, (0,nbslice))
			check_status(status)
			status, vconv_effList = ull.getVect2DDouble(self.idx, path, cpopath + 'vconv_eff')
			if len(vconv_effList) == 0:
				vconv_effList = numpy.resize(vconv_effList, (0,nbslice))
			check_status(status)
			status, fluxList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux')
			if len(fluxList) == 0:
				fluxList = numpy.resize(fluxList, (0,nbslice))
			check_status(status)
			off_diagonalList = self.off_diagonal.build_non_resampled_data(path, cpopath, nbslice)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			for i in range(nbslice):
				slice = ne_neostructuretranscoefel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff_eff = diff_effList[:,i]
				slice.vconv_eff = vconv_effList[:,i]
				slice.flux = fluxList[:,i]
				slice.off_diagonal = off_diagonalList[i]
				slice.flag = flagVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ne_neostructuretranscoefelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		obj = self.off_diagonal.putTimedElt(path, cpopath + 'off_diagonal', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ne_neostructuretranscoefelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		self.off_diagonal.getTimedElt(path, cpopath + 'off_diagonal', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ne_neostructuretranscoefelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.off_diagonal.putNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ne_neostructuretranscoefelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.off_diagonal.getNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff_eff')
		ull.deleteData(self.idx, path, cpopath + 'vconv_eff')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		self.off_diagonal.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flag')


class off_diagonalstructureoffdiagel(KeepInOrder):
	'''
	class off_diagonalstructureoffdiagel
	Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.

	Attributes:
	- d_ni : numpy.ndarray 2D with float
	   Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- d_ti : numpy.ndarray 2D with float
	   Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- d_ne : numpy.ndarray 1D with float
	   Off-Diagonal term coupling electron density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
	- d_te : numpy.ndarray 1D with float
	   Off-Diagonal term coupling electron temperature gradient to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
	- d_epar : numpy.ndarray 1D with float
	   Off-Diagonal term coupling parallel electric field to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
	- d_mtor : numpy.ndarray 1D with float
	   Off-Diagonal term coupling total toroidal momentum to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
	'''

	def __init__(self, base_path_in='off_diagonal'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.d_ni = numpy.zeros((0,0), numpy.float64, order='C')
		self.d_ti = numpy.zeros((0,0), numpy.float64, order='C')
		self.d_ne = numpy.zeros(0, numpy.float64, order='C')
		self.d_te = numpy.zeros(0, numpy.float64, order='C')
		self.d_epar = numpy.zeros(0, numpy.float64, order='C')
		self.d_mtor = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class off_diagonalstructureoffdiagel\n'
		s = self.d_ni.__str__()
		ret = ret + space + 'Attribute d_ni\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_ti.__str__()
		ret = ret + space + 'Attribute d_ti\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_ne.__str__()
		ret = ret + space + 'Attribute d_ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_te.__str__()
		ret = ret + space + 'Attribute d_te\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_epar.__str__()
		ret = ret + space + 'Attribute d_epar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d_mtor.__str__()
		ret = ret + space + 'Attribute d_mtor\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd_ni', numpy.array(self.d_ni).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd_ti', numpy.array(self.d_ti).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd_ne', numpy.array(self.d_ne).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd_te', numpy.array(self.d_te).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd_epar', numpy.array(self.d_epar).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd_mtor', numpy.array(self.d_mtor).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd_ni', numpy.array(self.d_ni).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd_ti', numpy.array(self.d_ti).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd_ne', numpy.array(self.d_ne).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd_te', numpy.array(self.d_te).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd_epar', numpy.array(self.d_epar).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd_mtor', numpy.array(self.d_mtor).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_d_ni, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd_ni', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_ni = ret_d_ni
			self.cpoTime = retTime
		status, ret_d_ti, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd_ti', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_ti = ret_d_ti
			self.cpoTime = retTime
		status, ret_d_ne, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd_ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_ne = ret_d_ne
			self.cpoTime = retTime
		status, ret_d_te, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd_te', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_te = ret_d_te
			self.cpoTime = retTime
		status, ret_d_epar, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd_epar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_epar = ret_d_epar
			self.cpoTime = retTime
		status, ret_d_mtor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd_mtor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d_mtor = ret_d_mtor
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type off_diagonalstructureoffdiagel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, d_niList = ull.getVect3DDouble(self.idx, path, cpopath + 'd_ni')
			if len(d_niList) == 0:
				d_niList = numpy.resize(d_niList, (0,0,nbslice))
			check_status(status)
			status, d_tiList = ull.getVect3DDouble(self.idx, path, cpopath + 'd_ti')
			if len(d_tiList) == 0:
				d_tiList = numpy.resize(d_tiList, (0,0,nbslice))
			check_status(status)
			status, d_neList = ull.getVect2DDouble(self.idx, path, cpopath + 'd_ne')
			if len(d_neList) == 0:
				d_neList = numpy.resize(d_neList, (0,nbslice))
			check_status(status)
			status, d_teList = ull.getVect2DDouble(self.idx, path, cpopath + 'd_te')
			if len(d_teList) == 0:
				d_teList = numpy.resize(d_teList, (0,nbslice))
			check_status(status)
			status, d_eparList = ull.getVect2DDouble(self.idx, path, cpopath + 'd_epar')
			if len(d_eparList) == 0:
				d_eparList = numpy.resize(d_eparList, (0,nbslice))
			check_status(status)
			status, d_mtorList = ull.getVect2DDouble(self.idx, path, cpopath + 'd_mtor')
			if len(d_mtorList) == 0:
				d_mtorList = numpy.resize(d_mtorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = off_diagonalstructureoffdiagel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.d_ni = d_niList[:,:,i]
				slice.d_ti = d_tiList[:,:,i]
				slice.d_ne = d_neList[:,i]
				slice.d_te = d_teList[:,i]
				slice.d_epar = d_eparList[:,i]
				slice.d_mtor = d_mtorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd_ni') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd_ni', i, numpy.array(self.d_ni).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd_ti') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd_ti', i, numpy.array(self.d_ti).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd_ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd_ne', i, numpy.array(self.d_ne).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd_te') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd_te', i, numpy.array(self.d_te).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd_epar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd_epar', i, numpy.array(self.d_epar).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd_mtor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd_mtor', i, numpy.array(self.d_mtor).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd_ni') 
			print ('obj = ' + str(obj))
		status, ret_d_ni = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd_ni', i)
		check_status(status)
		if not status:
			self.d_ni = ret_d_ni
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd_ti') 
			print ('obj = ' + str(obj))
		status, ret_d_ti = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd_ti', i)
		check_status(status)
		if not status:
			self.d_ti = ret_d_ti
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd_ne') 
			print ('obj = ' + str(obj))
		status, ret_d_ne = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd_ne', i)
		check_status(status)
		if not status:
			self.d_ne = ret_d_ne
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd_te') 
			print ('obj = ' + str(obj))
		status, ret_d_te = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd_te', i)
		check_status(status)
		if not status:
			self.d_te = ret_d_te
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd_epar') 
			print ('obj = ' + str(obj))
		status, ret_d_epar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd_epar', i)
		check_status(status)
		if not status:
			self.d_epar = ret_d_epar
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd_mtor') 
			print ('obj = ' + str(obj))
		status, ret_d_mtor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd_mtor', i)
		check_status(status)
		if not status:
			self.d_mtor = ret_d_mtor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type off_diagonalstructureoffdiagelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'd_ni')
		ull.deleteData(self.idx, path, cpopath + 'd_ti')
		ull.deleteData(self.idx, path, cpopath + 'd_ne')
		ull.deleteData(self.idx, path, cpopath + 'd_te')
		ull.deleteData(self.idx, path, cpopath + 'd_epar')
		ull.deleteData(self.idx, path, cpopath + 'd_mtor')


class nz_neostruct_arraytranscoefimp:
	'''
	class nz_neostruct_arraytranscoefimp
	Neoclassical transport coefficients for impurity (multiple charge state) density equation. Time-dependent.

	Attributes:
	- array : list of nz_neostruct_arraytranscoefimpObj 
	'''

	def __init__(self, base_path_in='nz_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nz_neostruct_arraytranscoefimp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'nz_neostruct_arraytranscoefimp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(nz_neostruct_arraytranscoefimpObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function putSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj_time = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		obj = ull.beginObject(self.idx, obj_time, 0, 'ALLTIMES', TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		obj_time = ull.putObjectInObject(self.idx, obj_time, 'ALLTIMES', 0, obj)
		if (dev()):
			print ('putObjectSlice('+path+', '+cpopath+self.base_path+', '+str(self.cpoTime)+', '+str(obj_time)+')')
		status = ull.putObjectSlice(self.idx, path, cpopath + self.base_path, self.cpoTime, obj_time)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function replaceLastSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('replaceLastObjectSlice('+path+', '+cpopath+self.base_path+', '+str(obj)+')')
		status = ull.replaceLastObjectSlice(self.idx, path, cpopath + self.base_path, obj)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function getSlice') 
		if (dev()):
			print ('getObjectSlice('+path+', '+cpopath+self.base_path+', '+str(inTime)+')')
		status, obj_time = ull.getObjectSlice(self.idx, path, cpopath + self.base_path, inTime)
		if status:
			print ('Failed to get slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ')')
			return
		status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', 0)
		if status:
			print ('No data found for slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj_time)
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		if (obj_size != 0):
			if (len(self.array) == 0):
				self.resize(obj_size)
			if (obj_size != len(self.array)):
				print ('error in getSlice: wrong size of object '+ path + '/' + cpopath +self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
			else:
				for i in range(obj_size):
					self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(nz_neostruct_arraytranscoefimp(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(nz_neostruct_arraytranscoefimp(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = nz_neostruct_arraytranscoefimp(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj_time)
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				if (obj_size != 0):
					if (len(list[n].array) == 0):
						list[n].resize(obj_size)
					if (obj_size != len(list[n].array)):
						print ('error in get: wrong size of object at '+path+'/'+cpopath+self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(list[n].array)) +')')
					else:
						for i in range(obj_size):
							if list[n]:
								list[n].array[i].getNonTimedElt(path, self.base_path, i, obj)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function putTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j)) 
			obj2 = self.array[j].putTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		if (dev()):
			print ('putObjectInObject('+str(obj)+', '+cpopath+', '+str(i)+', '+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function getTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (current part size =  ' + str(obj_size) + ') != (existing part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getTimedElt(path, self.base_path, j, obj2)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nz_neostruct_arraytranscoefimp, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class nz_neostruct_arraytranscoefimpObj(KeepInOrder):
	'''
	class nz_neostruct_arraytranscoefimpObj
	Neoclassical transport coefficients for impurity (multiple charge state) density equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 2D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Array2d (nrho,nzimp)
	- vconv_eff : numpy.ndarray 2D with float
	   Effective convection [m.s^-1]. Time-dependent. Array2d (nrho,nzimp)
	- exchange : numpy.ndarray 2D with float
	   Ion to electron energy exchange [W.m^-3]. Time-dependent. Array2d (nrho,nzimp)
	- flux : numpy.ndarray 2D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Array2d (nrho,nzimp)
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix (off-diagonal subtree not available for impurities for the moment). Scalar.
	'''

	def __init__(self, base_path_in='nz_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.vconv_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.exchange = numpy.zeros((0,0), numpy.float64, order='C')
		self.flux = numpy.zeros((0,0), numpy.float64, order='C')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nz_neostruct_arraytranscoefimpObj\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.exchange.__str__()
		ret = ret + space + 'Attribute exchange\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nz_neostruct_arraytranscoefimpObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'exchange', i, numpy.array(self.exchange).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nz_neostruct_arraytranscoefimpObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		status, ret_exchange = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'exchange', i)
		check_status(status)
		if not status:
			self.exchange = ret_exchange
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nz_neostruct_arraytranscoefimpObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nz_neostruct_arraytranscoefimpObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag


class ti_neostructuretranscoefion(KeepInOrder):
	'''
	class ti_neostructuretranscoefion
	Neoclassical transport coefficients for ion temperature equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 2D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Matrix (nrho,nion)
	- vconv_eff : numpy.ndarray 2D with float
	   Effective convection [m.s^-1]. Time-dependent. Matrix (nrho,nion)
	- exchange : numpy.ndarray 2D with float
	   Ion to electron energy exchange [W.m^-3]. Time-dependent. Matrix(nrho,nion).
	- qgi : numpy.ndarray 2D with float
	   Energy exchange term due to transport. [W.m^-3]. Time-dependent. Matrix (nrho,nion)
	- flux : numpy.ndarray 2D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
	- off_diagonal : class off_diagonalstructureoffdiagion
	   Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix. Scalar.
	'''

	def __init__(self, base_path_in='ti_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.vconv_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.exchange = numpy.zeros((0,0), numpy.float64, order='C')
		self.qgi = numpy.zeros((0,0), numpy.float64, order='C')
		self.flux = numpy.zeros((0,0), numpy.float64, order='C')
		self.off_diagonal = off_diagonalstructureoffdiagion('off_diagonal')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ti_neostructuretranscoefion\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.exchange.__str__()
		ret = ret + space + 'Attribute exchange\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.qgi.__str__()
		ret = ret + space + 'Attribute qgi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute off_diagonal\n ' + self.off_diagonal.__str__(depth+1)
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.off_diagonal.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ti_neostructuretranscoefion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'exchange', numpy.array(self.exchange).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'qgi', numpy.array(self.qgi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.off_diagonal.cpoTime = self.cpoTime
		self.off_diagonal.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ti_neostructuretranscoefion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'exchange', numpy.array(self.exchange).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'qgi', numpy.array(self.qgi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64))
		check_status(status)
		self.off_diagonal.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ti_neostructuretranscoefion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.off_diagonal.putNonTimed(path, cpopath)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type ti_neostructuretranscoefion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff_eff, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'diff_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
			self.cpoTime = retTime
		status, ret_vconv_eff, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
			self.cpoTime = retTime
		status, ret_exchange, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'exchange', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exchange = ret_exchange
			self.cpoTime = retTime
		status, ret_qgi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'qgi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qgi = ret_qgi
			self.cpoTime = retTime
		status, ret_flux, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux = ret_flux
			self.cpoTime = retTime
		self.off_diagonal.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type ti_neostructuretranscoefion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diff_effList = ull.getVect3DDouble(self.idx, path, cpopath + 'diff_eff')
			if len(diff_effList) == 0:
				diff_effList = numpy.resize(diff_effList, (0,0,nbslice))
			check_status(status)
			status, vconv_effList = ull.getVect3DDouble(self.idx, path, cpopath + 'vconv_eff')
			if len(vconv_effList) == 0:
				vconv_effList = numpy.resize(vconv_effList, (0,0,nbslice))
			check_status(status)
			status, exchangeList = ull.getVect3DDouble(self.idx, path, cpopath + 'exchange')
			if len(exchangeList) == 0:
				exchangeList = numpy.resize(exchangeList, (0,0,nbslice))
			check_status(status)
			status, qgiList = ull.getVect3DDouble(self.idx, path, cpopath + 'qgi')
			if len(qgiList) == 0:
				qgiList = numpy.resize(qgiList, (0,0,nbslice))
			check_status(status)
			status, fluxList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux')
			if len(fluxList) == 0:
				fluxList = numpy.resize(fluxList, (0,0,nbslice))
			check_status(status)
			off_diagonalList = self.off_diagonal.build_non_resampled_data(path, cpopath, nbslice)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			for i in range(nbslice):
				slice = ti_neostructuretranscoefion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff_eff = diff_effList[:,:,i]
				slice.vconv_eff = vconv_effList[:,:,i]
				slice.exchange = exchangeList[:,:,i]
				slice.qgi = qgiList[:,:,i]
				slice.flux = fluxList[:,:,i]
				slice.off_diagonal = off_diagonalList[i]
				slice.flag = flagVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_neostructuretranscoefionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'exchange', i, numpy.array(self.exchange).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'qgi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'qgi', i, numpy.array(self.qgi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		obj = self.off_diagonal.putTimedElt(path, cpopath + 'off_diagonal', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_neostructuretranscoefionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		status, ret_exchange = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'exchange', i)
		check_status(status)
		if not status:
			self.exchange = ret_exchange
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'qgi') 
			print ('obj = ' + str(obj))
		status, ret_qgi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'qgi', i)
		check_status(status)
		if not status:
			self.qgi = ret_qgi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		self.off_diagonal.getTimedElt(path, cpopath + 'off_diagonal', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_neostructuretranscoefionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.off_diagonal.putNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_neostructuretranscoefionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.off_diagonal.getNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff_eff')
		ull.deleteData(self.idx, path, cpopath + 'vconv_eff')
		ull.deleteData(self.idx, path, cpopath + 'exchange')
		ull.deleteData(self.idx, path, cpopath + 'qgi')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		self.off_diagonal.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flag')


class te_neostructuretranscoefel(KeepInOrder):
	'''
	class te_neostructuretranscoefel
	Neoclassical transport coefficients for electron temperature equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 1D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Vector (nrho)
	- vconv_eff : numpy.ndarray 1D with float
	   Effective convection [m.s^-1]. Time-dependent. Vector (nrho)
	- flux : numpy.ndarray 1D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
	- off_diagonal : class off_diagonalstructureoffdiagel
	   Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix. Scalar.
	'''

	def __init__(self, base_path_in='te_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros(0, numpy.float64, order='C')
		self.vconv_eff = numpy.zeros(0, numpy.float64, order='C')
		self.flux = numpy.zeros(0, numpy.float64, order='C')
		self.off_diagonal = off_diagonalstructureoffdiagel('off_diagonal')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class te_neostructuretranscoefel\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute off_diagonal\n ' + self.off_diagonal.__str__(depth+1)
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.off_diagonal.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type te_neostructuretranscoefel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.off_diagonal.cpoTime = self.cpoTime
		self.off_diagonal.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type te_neostructuretranscoefel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64))
		check_status(status)
		self.off_diagonal.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type te_neostructuretranscoefel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.off_diagonal.putNonTimed(path, cpopath)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type te_neostructuretranscoefel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff_eff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
			self.cpoTime = retTime
		status, ret_vconv_eff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
			self.cpoTime = retTime
		status, ret_flux, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux = ret_flux
			self.cpoTime = retTime
		self.off_diagonal.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type te_neostructuretranscoefel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diff_effList = ull.getVect2DDouble(self.idx, path, cpopath + 'diff_eff')
			if len(diff_effList) == 0:
				diff_effList = numpy.resize(diff_effList, (0,nbslice))
			check_status(status)
			status, vconv_effList = ull.getVect2DDouble(self.idx, path, cpopath + 'vconv_eff')
			if len(vconv_effList) == 0:
				vconv_effList = numpy.resize(vconv_effList, (0,nbslice))
			check_status(status)
			status, fluxList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux')
			if len(fluxList) == 0:
				fluxList = numpy.resize(fluxList, (0,nbslice))
			check_status(status)
			off_diagonalList = self.off_diagonal.build_non_resampled_data(path, cpopath, nbslice)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			for i in range(nbslice):
				slice = te_neostructuretranscoefel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff_eff = diff_effList[:,i]
				slice.vconv_eff = vconv_effList[:,i]
				slice.flux = fluxList[:,i]
				slice.off_diagonal = off_diagonalList[i]
				slice.flag = flagVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_neostructuretranscoefelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		obj = self.off_diagonal.putTimedElt(path, cpopath + 'off_diagonal', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_neostructuretranscoefelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		self.off_diagonal.getTimedElt(path, cpopath + 'off_diagonal', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_neostructuretranscoefelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.off_diagonal.putNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_neostructuretranscoefelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.off_diagonal.getNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff_eff')
		ull.deleteData(self.idx, path, cpopath + 'vconv_eff')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		self.off_diagonal.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flag')


class tz_neostruct_arraytranscoefimp:
	'''
	class tz_neostruct_arraytranscoefimp
	Neoclassical transport coefficients for impurity (multiple charge state) temperature equation. Time-dependent.

	Attributes:
	- array : list of tz_neostruct_arraytranscoefimpObj 
	'''

	def __init__(self, base_path_in='tz_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class tz_neostruct_arraytranscoefimp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'tz_neostruct_arraytranscoefimp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(tz_neostruct_arraytranscoefimpObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function putSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj_time = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		obj = ull.beginObject(self.idx, obj_time, 0, 'ALLTIMES', TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		obj_time = ull.putObjectInObject(self.idx, obj_time, 'ALLTIMES', 0, obj)
		if (dev()):
			print ('putObjectSlice('+path+', '+cpopath+self.base_path+', '+str(self.cpoTime)+', '+str(obj_time)+')')
		status = ull.putObjectSlice(self.idx, path, cpopath + self.base_path, self.cpoTime, obj_time)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function replaceLastSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('replaceLastObjectSlice('+path+', '+cpopath+self.base_path+', '+str(obj)+')')
		status = ull.replaceLastObjectSlice(self.idx, path, cpopath + self.base_path, obj)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function getSlice') 
		if (dev()):
			print ('getObjectSlice('+path+', '+cpopath+self.base_path+', '+str(inTime)+')')
		status, obj_time = ull.getObjectSlice(self.idx, path, cpopath + self.base_path, inTime)
		if status:
			print ('Failed to get slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ')')
			return
		status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', 0)
		if status:
			print ('No data found for slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj_time)
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		if (obj_size != 0):
			if (len(self.array) == 0):
				self.resize(obj_size)
			if (obj_size != len(self.array)):
				print ('error in getSlice: wrong size of object '+ path + '/' + cpopath +self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
			else:
				for i in range(obj_size):
					self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(tz_neostruct_arraytranscoefimp(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(tz_neostruct_arraytranscoefimp(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = tz_neostruct_arraytranscoefimp(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj_time)
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				if (obj_size != 0):
					if (len(list[n].array) == 0):
						list[n].resize(obj_size)
					if (obj_size != len(list[n].array)):
						print ('error in get: wrong size of object at '+path+'/'+cpopath+self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(list[n].array)) +')')
					else:
						for i in range(obj_size):
							if list[n]:
								list[n].array[i].getNonTimedElt(path, self.base_path, i, obj)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function putTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j)) 
			obj2 = self.array[j].putTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		if (dev()):
			print ('putObjectInObject('+str(obj)+', '+cpopath+', '+str(i)+', '+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function getTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (current part size =  ' + str(obj_size) + ') != (existing part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getTimedElt(path, self.base_path, j, obj2)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type tz_neostruct_arraytranscoefimp, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class tz_neostruct_arraytranscoefimpObj(KeepInOrder):
	'''
	class tz_neostruct_arraytranscoefimpObj
	Neoclassical transport coefficients for impurity (multiple charge state) temperature equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 2D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Array2d (nrho,nzimp)
	- vconv_eff : numpy.ndarray 2D with float
	   Effective convection [m.s^-1]. Time-dependent. Array2d (nrho,nzimp)
	- exchange : numpy.ndarray 2D with float
	   Ion to electron energy exchange [W.m^-3]. Time-dependent. Array2d (nrho,nzimp)
	- flux : numpy.ndarray 2D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Array2d (nrho,nzimp)
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix (off-diagonal subtree not available for impurities for the moment). Scalar.
	'''

	def __init__(self, base_path_in='tz_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.vconv_eff = numpy.zeros((0,0), numpy.float64, order='C')
		self.exchange = numpy.zeros((0,0), numpy.float64, order='C')
		self.flux = numpy.zeros((0,0), numpy.float64, order='C')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class tz_neostruct_arraytranscoefimpObj\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.exchange.__str__()
		ret = ret + space + 'Attribute exchange\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tz_neostruct_arraytranscoefimpObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'exchange', i, numpy.array(self.exchange).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tz_neostruct_arraytranscoefimpObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'exchange') 
			print ('obj = ' + str(obj))
		status, ret_exchange = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'exchange', i)
		check_status(status)
		if not status:
			self.exchange = ret_exchange
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tz_neostruct_arraytranscoefimpObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tz_neostruct_arraytranscoefimpObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag


class mtor_neostructuretranscoefel(KeepInOrder):
	'''
	class mtor_neostructuretranscoefel
	Neoclassical transport coefficients for total toroidal momentum equation. Time-dependent.

	Attributes:
	- diff_eff : numpy.ndarray 1D with float
	   Effective diffusivity [m^2.s^-1]. Time-dependent. Vector (nrho)
	- vconv_eff : numpy.ndarray 1D with float
	   Effective convection [m.s^-1]. Time-dependent. Vector (nrho)
	- flux : numpy.ndarray 1D with float
	   Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
	- off_diagonal : class off_diagonalstructureoffdiagel
	   Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
	- flag : int
	   Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport matrix. Scalar.
	'''

	def __init__(self, base_path_in='mtor_neo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff_eff = numpy.zeros(0, numpy.float64, order='C')
		self.vconv_eff = numpy.zeros(0, numpy.float64, order='C')
		self.flux = numpy.zeros(0, numpy.float64, order='C')
		self.off_diagonal = off_diagonalstructureoffdiagel('off_diagonal')
		self.flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mtor_neostructuretranscoefel\n'
		s = self.diff_eff.__str__()
		ret = ret + space + 'Attribute diff_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv_eff.__str__()
		ret = ret + space + 'Attribute vconv_eff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute off_diagonal\n ' + self.off_diagonal.__str__(depth+1)
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.off_diagonal.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mtor_neostructuretranscoefel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.off_diagonal.cpoTime = self.cpoTime
		self.off_diagonal.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mtor_neostructuretranscoefel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', numpy.array(self.diff_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', numpy.array(self.vconv_eff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64))
		check_status(status)
		self.off_diagonal.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mtor_neostructuretranscoefel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.off_diagonal.putNonTimed(path, cpopath)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mtor_neostructuretranscoefel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff_eff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'diff_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
			self.cpoTime = retTime
		status, ret_vconv_eff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vconv_eff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
			self.cpoTime = retTime
		status, ret_flux, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux = ret_flux
			self.cpoTime = retTime
		self.off_diagonal.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mtor_neostructuretranscoefel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diff_effList = ull.getVect2DDouble(self.idx, path, cpopath + 'diff_eff')
			if len(diff_effList) == 0:
				diff_effList = numpy.resize(diff_effList, (0,nbslice))
			check_status(status)
			status, vconv_effList = ull.getVect2DDouble(self.idx, path, cpopath + 'vconv_eff')
			if len(vconv_effList) == 0:
				vconv_effList = numpy.resize(vconv_effList, (0,nbslice))
			check_status(status)
			status, fluxList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux')
			if len(fluxList) == 0:
				fluxList = numpy.resize(fluxList, (0,nbslice))
			check_status(status)
			off_diagonalList = self.off_diagonal.build_non_resampled_data(path, cpopath, nbslice)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			for i in range(nbslice):
				slice = mtor_neostructuretranscoefel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff_eff = diff_effList[:,i]
				slice.vconv_eff = vconv_effList[:,i]
				slice.flux = fluxList[:,i]
				slice.off_diagonal = off_diagonalList[i]
				slice.flag = flagVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtor_neostructuretranscoefelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'diff_eff', i, numpy.array(self.diff_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vconv_eff', i, numpy.array(self.vconv_eff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		obj = self.off_diagonal.putTimedElt(path, cpopath + 'off_diagonal', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtor_neostructuretranscoefelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'diff_eff') 
			print ('obj = ' + str(obj))
		status, ret_diff_eff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'diff_eff', i)
		check_status(status)
		if not status:
			self.diff_eff = ret_diff_eff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vconv_eff') 
			print ('obj = ' + str(obj))
		status, ret_vconv_eff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vconv_eff', i)
		check_status(status)
		if not status:
			self.vconv_eff = ret_vconv_eff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		self.off_diagonal.getTimedElt(path, cpopath + 'off_diagonal', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtor_neostructuretranscoefelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.off_diagonal.putNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtor_neostructuretranscoefelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.off_diagonal.getNonTimedElt(path, cpopath + 'off_diagonal', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff_eff')
		ull.deleteData(self.idx, path, cpopath + 'vconv_eff')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		self.off_diagonal.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flag')


class impuritystruct_arrayneoclassic_impurity:
	'''
	class impuritystruct_arrayneoclassic_impurity
	Array(nimp). Time-dependent

	Attributes:
	- array : list of impuritystruct_arrayneoclassic_impurityObj 
	'''

	def __init__(self, base_path_in='impurity'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class impuritystruct_arrayneoclassic_impurity\n'
		for i in range(len(self.array)):
			ret = ret + space + 'impuritystruct_arrayneoclassic_impurity[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(impuritystruct_arrayneoclassic_impurityObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function putSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj_time = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		obj = ull.beginObject(self.idx, obj_time, 0, 'ALLTIMES', TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		obj_time = ull.putObjectInObject(self.idx, obj_time, 'ALLTIMES', 0, obj)
		if (dev()):
			print ('putObjectSlice('+path+', '+cpopath+self.base_path+', '+str(self.cpoTime)+', '+str(obj_time)+')')
		status = ull.putObjectSlice(self.idx, path, cpopath + self.base_path, self.cpoTime, obj_time)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function replaceLastSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('replaceLastObjectSlice('+path+', '+cpopath+self.base_path+', '+str(obj)+')')
		status = ull.replaceLastObjectSlice(self.idx, path, cpopath + self.base_path, obj)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function getSlice') 
		if (dev()):
			print ('getObjectSlice('+path+', '+cpopath+self.base_path+', '+str(inTime)+')')
		status, obj_time = ull.getObjectSlice(self.idx, path, cpopath + self.base_path, inTime)
		if status:
			print ('Failed to get slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ')')
			return
		status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', 0)
		if status:
			print ('No data found for slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj_time)
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		if (obj_size != 0):
			if (len(self.array) == 0):
				self.resize(obj_size)
			if (obj_size != len(self.array)):
				print ('error in getSlice: wrong size of object '+ path + '/' + cpopath +self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
			else:
				for i in range(obj_size):
					self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(impuritystruct_arrayneoclassic_impurity(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(impuritystruct_arrayneoclassic_impurity(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = impuritystruct_arrayneoclassic_impurity(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj_time)
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				if (obj_size != 0):
					if (len(list[n].array) == 0):
						list[n].resize(obj_size)
					if (obj_size != len(list[n].array)):
						print ('error in get: wrong size of object at '+path+'/'+cpopath+self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(list[n].array)) +')')
					else:
						for i in range(obj_size):
							if list[n]:
								list[n].array[i].getNonTimedElt(path, self.base_path, i, obj)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function putTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j)) 
			obj2 = self.array[j].putTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		if (dev()):
			print ('putObjectInObject('+str(obj)+', '+cpopath+', '+str(i)+', '+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function getTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (current part size =  ' + str(obj_size) + ') != (existing part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getTimedElt(path, self.base_path, j, obj2)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type impuritystruct_arrayneoclassic_impurity, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class impuritystruct_arrayneoclassic_impurityObj(KeepInOrder):
	'''
	class impuritystruct_arrayneoclassic_impurityObj
	Array(nimp). Time-dependent

	Attributes:
	- utheta_z : numpy.ndarray 2D with float
	   Ion poloidal flow for various charge states [m/s].  Time-dependent. Matrix(nrho,nzimp).
	'''

	def __init__(self, base_path_in='impurity'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.utheta_z = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class impuritystruct_arrayneoclassic_impurityObj\n'
		s = self.utheta_z.__str__()
		ret = ret + space + 'Attribute utheta_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritystruct_arrayneoclassic_impurityObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'utheta_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'utheta_z', i, numpy.array(self.utheta_z).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritystruct_arrayneoclassic_impurityObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'utheta_z') 
			print ('obj = ' + str(obj))
		status, ret_utheta_z = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'utheta_z', i)
		check_status(status)
		if not status:
			self.utheta_z = ret_utheta_z

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritystruct_arrayneoclassic_impurityObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritystruct_arrayneoclassic_impurityObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class codeparamstructurecodeparam(KeepInOrder):
	'''
	class codeparamstructurecodeparam
	Code parameters

	Attributes:
	- codename : str
	   Name of the code
	- codeversion : str
	   Version of the code (as in the ITM repository)
	- parameters : str
	   List of the code specific parameters, string expected to be in XML format.
	- output_diag : str
	   List of the code specific diagnostic/output, string expected to be in XML format.
	- output_flag : int
	   Output flag : 0 means the run is successful, other values meaning some difficulty has been encountered, the exact meaning is then code specific. Negative values mean the result shall not be used. Exact rules could discussed and implemented in the module wrapper. Time-dependent.
	'''

	def __init__(self, base_path_in='codeparam'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.codename = ''
		self.codeversion = ''
		self.parameters = ''
		self.output_diag = ''
		self.output_flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class codeparamstructurecodeparam\n'
		ret = ret + space + 'Attribute codename: ' + str(self.codename) + '\n'
		ret = ret + space + 'Attribute codeversion: ' + str(self.codeversion) + '\n'
		ret = ret + space + 'Attribute parameters: ' + str(self.parameters) + '\n'
		ret = ret + space + 'Attribute output_diag: ' + str(self.output_diag) + '\n'
		ret = ret + space + 'Attribute output_flag: ' + str(self.output_flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'output_flag', self.output_flag, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'output_flag', self.output_flag)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'codename', self.codename)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'codeversion', self.codeversion)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'parameters', self.parameters)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'output_diag', self.output_diag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_codename = ull.getString(self.idx, path, cpopath + 'codename')
		check_status(status)
		if not status:
			self.codename = ret_codename
		status, ret_codeversion = ull.getString(self.idx, path, cpopath + 'codeversion')
		check_status(status)
		if not status:
			self.codeversion = ret_codeversion
		status, ret_parameters = ull.getString(self.idx, path, cpopath + 'parameters')
		check_status(status)
		if not status:
			self.parameters = ret_parameters
		status, ret_output_diag = ull.getString(self.idx, path, cpopath + 'output_diag')
		check_status(status)
		if not status:
			self.output_diag = ret_output_diag
		status, ret_output_flag, retTime = ull.getIntSlice(self.idx, path, cpopath + 'output_flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.output_flag = ret_output_flag
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, codenameVal = ull.getString(self.idx, path, cpopath + 'codename')
			check_status(status)
			status, codeversionVal = ull.getString(self.idx, path, cpopath + 'codeversion')
			check_status(status)
			status, parametersVal = ull.getString(self.idx, path, cpopath + 'parameters')
			check_status(status)
			status, output_diagVal = ull.getString(self.idx, path, cpopath + 'output_diag')
			check_status(status)
			status, output_flagList = ull.getVect1DInt(self.idx, path, cpopath + 'output_flag')
			if len(output_flagList) == 0:
				output_flagList = numpy.resize(output_flagList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = codeparamstructurecodeparam(self.base_path)
				slice.setExpIdx(self.idx)
				slice.codename = codenameVal
				slice.codeversion = codeversionVal
				slice.parameters = parametersVal
				slice.output_diag = output_diagVal
				slice.output_flag = int(output_flagList[i].copy())
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'output_flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'output_flag', i, self.output_flag)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'output_flag') 
			print ('obj = ' + str(obj))
		status, ret_output_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'output_flag', i)
		check_status(status)
		if not status:
			self.output_flag = ret_output_flag

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'codename') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'codename', i, self.codename)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'codeversion') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'codeversion', i, self.codeversion)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'parameters') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'parameters', i, self.parameters)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'output_diag') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'output_diag', i, self.output_diag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'codename') 
			print ('obj = ' + str(obj))
		status, ret_codename = ull.getStringFromObject(self.idx, obj, cpopath + 'codename', i)
		check_status(status)
		if not status:
			self.codename = ret_codename
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'codeversion') 
			print ('obj = ' + str(obj))
		status, ret_codeversion = ull.getStringFromObject(self.idx, obj, cpopath + 'codeversion', i)
		check_status(status)
		if not status:
			self.codeversion = ret_codeversion
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'parameters') 
			print ('obj = ' + str(obj))
		status, ret_parameters = ull.getStringFromObject(self.idx, obj, cpopath + 'parameters', i)
		check_status(status)
		if not status:
			self.parameters = ret_parameters
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'output_diag') 
			print ('obj = ' + str(obj))
		status, ret_output_diag = ull.getStringFromObject(self.idx, obj, cpopath + 'output_diag', i)
		check_status(status)
		if not status:
			self.output_diag = ret_output_diag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'codename')
		ull.deleteData(self.idx, path, cpopath + 'codeversion')
		ull.deleteData(self.idx, path, cpopath + 'parameters')
		ull.deleteData(self.idx, path, cpopath + 'output_diag')
		ull.deleteData(self.idx, path, cpopath + 'output_flag')
