# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class coreprof(KeepInOrder):
	'''
	class coreprof
	Core plasma 1D profiles as a function of the toroidal flux coordinate, obtained by solving the core transport equations (can be also fitted profiles from experimental data). The codeparam element here describes the parameters of the transport equation solver and/or those of the fitting program. Time-dependent CPO.

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- rho_tor_norm : numpy.ndarray 1D with float
	   Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last radial grid point, which is quasi at the Last Closed Flux Surface); Time-dependent; Vector (nrho)
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m]; Vector (nrho). Time-dependent.
	- drho_dt : numpy.ndarray 1D with float
	   Time derivative of rho_tor [m/s]; Vector (nrho). Time-dependent.
	- toroid_field : class toroid_fieldstructuretoroid_field
	   Toroidal field information entering the definition of rho_tor, for reference only. The physical value of the toroidal field should be taken from the toroidfield CPO. Time-dependent.
	- composition : class compositionstructurecomposition
	   Plasma composition (description of ion species). OBSOLESCENT.
	- desc_impur : class desc_impurstructuredesc_impur
	   Description of the impurities (list of ion species and possibly different charge states). OBSOLESCENT.
	- compositions : class compositionsstructurecompositions_type
	   Contains all the composition information for the simulation (main ions, impurities, neutrals, edge species).
	- psi : class psistructurepsi
	   Poloidal magnetic flux [Wb]; Time-dependent;
	- te : class testructurecorefield
	   Electron temperature [eV]; (source term in [W.m^-3]). Time-dependent;
	- ti : class tistructurecorefieldion
	   Ion temperature [eV]; (source term in [W.m^-3]). Time-dependent;
	- ne : class nestructurecorefield
	   Electron density [m^-3]; (source term in [m^-3]).Time-dependent;
	- ni : class nistructurecorefieldion
	   Ion density [m^-3]; (source term in [m^-3]). Time-dependent;
	- vtor : class vtorstructurecorefieldion
	   Toroidal velocity of the various ion species [m.s^-1]; Time-dependent;
	- profiles1d : class profiles1dstructureprofiles1d
	   Profiles derived from the fields solved in the transport equations, or from experiment.
	- globalparam : class globalparamstructureglobalparam
	   Various global quantities calculated from the 1D profiles. Time-dependent
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'coreprof'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 10
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.drho_dt = numpy.zeros(0, numpy.float64, order='C')
		self.toroid_field = toroid_fieldstructuretoroid_field('toroid_field')
		self.composition = compositionstructurecomposition('composition')
		self.desc_impur = desc_impurstructuredesc_impur('desc_impur')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.psi = psistructurepsi('psi')
		self.te = testructurecorefield('te')
		self.ti = tistructurecorefieldion('ti')
		self.ne = nestructurecorefield('ne')
		self.ni = nistructurecorefieldion('ni')
		self.vtor = vtorstructurecorefieldion('vtor')
		self.profiles1d = profiles1dstructureprofiles1d('profiles1d')
		self.globalparam = globalparamstructureglobalparam('globalparam')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coreprof\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.drho_dt.__str__()
		ret = ret + space + 'Attribute drho_dt\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute toroid_field\n ' + self.toroid_field.__str__(depth+1)
		ret = ret + space + 'Attribute composition\n ' + self.composition.__str__(depth+1)
		ret = ret + space + 'Attribute desc_impur\n ' + self.desc_impur.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute psi\n ' + self.psi.__str__(depth+1)
		ret = ret + space + 'Attribute te\n ' + self.te.__str__(depth+1)
		ret = ret + space + 'Attribute ti\n ' + self.ti.__str__(depth+1)
		ret = ret + space + 'Attribute ne\n ' + self.ne.__str__(depth+1)
		ret = ret + space + 'Attribute ni\n ' + self.ni.__str__(depth+1)
		ret = ret + space + 'Attribute vtor\n ' + self.vtor.__str__(depth+1)
		ret = ret + space + 'Attribute profiles1d\n ' + self.profiles1d.__str__(depth+1)
		ret = ret + space + 'Attribute globalparam\n ' + self.globalparam.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.toroid_field.setExpIdx(idx)
		self.composition.setExpIdx(idx)
		self.desc_impur.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
		self.psi.setExpIdx(idx)
		self.te.setExpIdx(idx)
		self.ti.setExpIdx(idx)
		self.ne.setExpIdx(idx)
		self.ni.setExpIdx(idx)
		self.vtor.setExpIdx(idx)
		self.profiles1d.setExpIdx(idx)
		self.globalparam.setExpIdx(idx)
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'drho_dt', numpy.array(self.drho_dt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.toroid_field.cpoTime = self.cpoTime
		self.toroid_field.putSlice(path, cpopath)
		self.composition.cpoTime = self.cpoTime
		self.composition.putSlice(path, cpopath)
		self.desc_impur.cpoTime = self.cpoTime
		self.desc_impur.putSlice(path, cpopath)
		self.compositions.cpoTime = self.cpoTime
		self.compositions.putSlice(path, cpopath)
		self.psi.cpoTime = self.cpoTime
		self.psi.putSlice(path, cpopath)
		self.te.cpoTime = self.cpoTime
		self.te.putSlice(path, cpopath)
		self.ti.cpoTime = self.cpoTime
		self.ti.putSlice(path, cpopath)
		self.ne.cpoTime = self.cpoTime
		self.ne.putSlice(path, cpopath)
		self.ni.cpoTime = self.cpoTime
		self.ni.putSlice(path, cpopath)
		self.vtor.cpoTime = self.cpoTime
		self.vtor.putSlice(path, cpopath)
		self.profiles1d.cpoTime = self.cpoTime
		self.profiles1d.putSlice(path, cpopath)
		self.globalparam.cpoTime = self.cpoTime
		self.globalparam.putSlice(path, cpopath)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'drho_dt', numpy.array(self.drho_dt).astype(numpy.float64))
		check_status(status)
		self.toroid_field.replaceLastSlice(path, cpopath)
		self.composition.replaceLastSlice(path, cpopath)
		self.desc_impur.replaceLastSlice(path, cpopath)
		self.compositions.replaceLastSlice(path, cpopath)
		self.psi.replaceLastSlice(path, cpopath)
		self.te.replaceLastSlice(path, cpopath)
		self.ti.replaceLastSlice(path, cpopath)
		self.ne.replaceLastSlice(path, cpopath)
		self.ni.replaceLastSlice(path, cpopath)
		self.vtor.replaceLastSlice(path, cpopath)
		self.profiles1d.replaceLastSlice(path, cpopath)
		self.globalparam.replaceLastSlice(path, cpopath)
		self.codeparam.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
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
		self.toroid_field.putNonTimed(path, cpopath)
		self.composition.putNonTimed(path, cpopath)
		self.desc_impur.putNonTimed(path, cpopath)
		self.compositions.putNonTimed(path, cpopath)
		self.psi.putNonTimed(path, cpopath)
		self.te.putNonTimed(path, cpopath)
		self.ti.putNonTimed(path, cpopath)
		self.ne.putNonTimed(path, cpopath)
		self.ni.putNonTimed(path, cpopath)
		self.vtor.putNonTimed(path, cpopath)
		self.profiles1d.putNonTimed(path, cpopath)
		self.globalparam.putNonTimed(path, cpopath)
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
		status, ret_rho_tor_norm, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime
		status, ret_drho_dt, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'drho_dt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.drho_dt = ret_drho_dt
			self.cpoTime = retTime
		self.toroid_field.getSlice(path, cpopath, inTime, interpolMode)
		self.composition.getSlice(path, cpopath, inTime, interpolMode)
		self.desc_impur.getSlice(path, cpopath, inTime, interpolMode)
		self.compositions.getSlice(path, cpopath, inTime, interpolMode)
		self.psi.getSlice(path, cpopath, inTime, interpolMode)
		self.te.getSlice(path, cpopath, inTime, interpolMode)
		self.ti.getSlice(path, cpopath, inTime, interpolMode)
		self.ne.getSlice(path, cpopath, inTime, interpolMode)
		self.ni.getSlice(path, cpopath, inTime, interpolMode)
		self.vtor.getSlice(path, cpopath, inTime, interpolMode)
		self.profiles1d.getSlice(path, cpopath, inTime, interpolMode)
		self.globalparam.getSlice(path, cpopath, inTime, interpolMode)
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
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
			status, rho_tor_normList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			if len(rho_tor_normList) == 0:
				rho_tor_normList = numpy.resize(rho_tor_normList, (0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			status, drho_dtList = ull.getVect2DDouble(self.idx, path, cpopath + 'drho_dt')
			if len(drho_dtList) == 0:
				drho_dtList = numpy.resize(drho_dtList, (0,nbslice))
			check_status(status)
			toroid_fieldList = self.toroid_field.build_non_resampled_data(path, cpopath, nbslice)
			compositionList = self.composition.build_non_resampled_data(path, cpopath, nbslice)
			desc_impurList = self.desc_impur.build_non_resampled_data(path, cpopath, nbslice)
			compositionsList = self.compositions.build_non_resampled_data(path, cpopath, nbslice)
			psiList = self.psi.build_non_resampled_data(path, cpopath, nbslice)
			teList = self.te.build_non_resampled_data(path, cpopath, nbslice)
			tiList = self.ti.build_non_resampled_data(path, cpopath, nbslice)
			neList = self.ne.build_non_resampled_data(path, cpopath, nbslice)
			niList = self.ni.build_non_resampled_data(path, cpopath, nbslice)
			vtorList = self.vtor.build_non_resampled_data(path, cpopath, nbslice)
			profiles1dList = self.profiles1d.build_non_resampled_data(path, cpopath, nbslice)
			globalparamList = self.globalparam.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = coreprof()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.rho_tor_norm = rho_tor_normList[:,i]
				slice.rho_tor = rho_torList[:,i]
				slice.drho_dt = drho_dtList[:,i]
				slice.toroid_field = toroid_fieldList[i]
				slice.composition = compositionList[i]
				slice.desc_impur = desc_impurList[i]
				slice.compositions = compositionsList[i]
				slice.psi = psiList[i]
				slice.te = teList[i]
				slice.ti = tiList[i]
				slice.ne = neList[i]
				slice.ni = niList[i]
				slice.vtor = vtorList[i]
				slice.profiles1d = profiles1dList[i]
				slice.globalparam = globalparamList[i]
				slice.codeparam = codeparamList[i]
				slice.time = timeList[i].copy().astype(float)
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
		ull.deleteData(self.idx, path, cpopath + 'drho_dt')
		self.toroid_field.deleteData(path, cpopath)
		self.composition.deleteData(path, cpopath)
		self.desc_impur.deleteData(path, cpopath)
		self.compositions.deleteData(path, cpopath)
		self.psi.deleteData(path, cpopath)
		self.te.deleteData(path, cpopath)
		self.ti.deleteData(path, cpopath)
		self.ne.deleteData(path, cpopath)
		self.ni.deleteData(path, cpopath)
		self.vtor.deleteData(path, cpopath)
		self.profiles1d.deleteData(path, cpopath)
		self.globalparam.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class coreprofArray:
	'''
	class coreprofArray
	Core plasma 1D profiles as a function of the toroidal flux coordinate, obtained by solving the core transport equations (can be also fitted profiles from experimental data). The codeparam element here describes the parameters of the transport equation solver and/or those of the fitting program. Time-dependent CPO.

	Attributes:
	- array : list of coreprof
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
		ret = space + 'class coreprofArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'coreprof cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = coreprof()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(coreprof())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = coreprof()
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


class toroid_fieldstructuretoroid_field(KeepInOrder):
	'''
	class toroid_fieldstructuretoroid_field
	Toroidal field information entering the definition of rho_tor, for reference only. The physical value of the toroidal field should be taken from the toroidfield CPO. Time-dependent.

	Attributes:
	- b0 : float
	   Vacuum field at r0 [T]; Time-dependent. Scalar.
	- b0prime : float
	   Time derivative of the vacuum field at r0 [T/s]; Time-dependent. Scalar.
	- r0 : float
	   Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Scalar.
	- time : float
	   Time [s] (exact time slice used from the time array of the source signal, here the toroidfield CPO. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used); Time-dependent; Scalar.
	'''

	def __init__(self, base_path_in='toroid_field'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.b0 = EMPTY_DOUBLE
		self.b0prime = EMPTY_DOUBLE
		self.r0 = EMPTY_DOUBLE
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class toroid_fieldstructuretoroid_field\n'
		ret = ret + space + 'Attribute b0: ' + str(self.b0) + '\n'
		ret = ret + space + 'Attribute b0prime: ' + str(self.b0prime) + '\n'
		ret = ret + space + 'Attribute r0: ' + str(self.r0) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructuretoroid_field, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'b0', self.b0, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'b0prime', self.b0prime, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructuretoroid_field, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'b0', self.b0)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'b0prime', self.b0prime)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructuretoroid_field, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'r0', self.r0)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructuretoroid_field, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_b0, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'b0', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b0 = ret_b0
			self.cpoTime = retTime
		status, ret_b0prime, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'b0prime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b0prime = ret_b0prime
			self.cpoTime = retTime
		status, ret_r0 = ull.getDouble(self.idx, path, cpopath + 'r0')
		check_status(status)
		if not status:
			self.r0 = ret_r0
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructuretoroid_field, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, b0List = ull.getVect1DDouble(self.idx, path, cpopath + 'b0')
			if len(b0List) == 0:
				b0List = numpy.resize(b0List, (nbslice))
			check_status(status)
			status, b0primeList = ull.getVect1DDouble(self.idx, path, cpopath + 'b0prime')
			if len(b0primeList) == 0:
				b0primeList = numpy.resize(b0primeList, (nbslice))
			check_status(status)
			status, r0Val = ull.getDouble(self.idx, path, cpopath + 'r0')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = toroid_fieldstructuretoroid_field(self.base_path)
				slice.setExpIdx(self.idx)
				slice.b0 = b0List[i].copy().astype(float)
				slice.b0prime = b0primeList[i].copy().astype(float)
				slice.r0 = r0Val
				slice.time = timeList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructuretoroid_fieldObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b0') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b0', i, self.b0)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b0prime') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b0prime', i, self.b0prime)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructuretoroid_fieldObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b0') 
			print ('obj = ' + str(obj))
		status, ret_b0 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b0', i)
		check_status(status)
		if not status:
			self.b0 = ret_b0
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b0prime') 
			print ('obj = ' + str(obj))
		status, ret_b0prime = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b0prime', i)
		check_status(status)
		if not status:
			self.b0prime = ret_b0prime
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructuretoroid_fieldObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r0') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r0', i, self.r0)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructuretoroid_fieldObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r0') 
			print ('obj = ' + str(obj))
		status, ret_r0 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r0', i)
		check_status(status)
		if not status:
			self.r0 = ret_r0

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'b0')
		ull.deleteData(self.idx, path, cpopath + 'b0prime')
		ull.deleteData(self.idx, path, cpopath + 'r0')
		ull.deleteData(self.idx, path, cpopath + 'time')


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


class psistructurepsi(KeepInOrder):
	'''
	class psistructurepsi
	Poloidal magnetic flux [Wb]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value [Wb]; Time-dependent; Vector (nrho)
	- ddrho : numpy.ndarray 1D with float
	   Radial derivative (dvalue/drho_tor) [Wb.m^-1]; Time-dependent; Vector (nrho)
	- d2drho2 : numpy.ndarray 1D with float
	   Second order radial derivative (d2value/drho_tor2) [Wb.m^-2]; Time-dependent; Vector (nrho)
	- ddt_rhotorn : numpy.ndarray 1D with float
	   Time derivative of the poloidal flux at constant rho_tor_norm [V]. Time-dependent.
	- ddt_phi : numpy.ndarray 1D with float
	   Time derivative of the poloidal flux at constant toroidal flux [V]. Time-dependent.
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	- flag : int
	   Flag describing how the profile has been processed : 0-not calculated; 1-interpretative; 2-calculated by the transport solver; 3-calculated by a separate code : in that case only, description of the code provided in codeparam at the same level; 4-used value from the previous time step; Time-dependent; Scalar
	- boundary : class boundarystructureboundary
	   Boundary condition for the transport equation. Time-dependent.
	- jni : class jnistructurejni
	   Non-inductive parallel current density [A/m^2]; Time-dependent;
	- sigma_par : class sigma_parstructurecoreprofile
	   Parallel conductivity [ohm^-1.m^-1]. Time-dependent
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='psi'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.ddrho = numpy.zeros(0, numpy.float64, order='C')
		self.d2drho2 = numpy.zeros(0, numpy.float64, order='C')
		self.ddt_rhotorn = numpy.zeros(0, numpy.float64, order='C')
		self.ddt_phi = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.flag = EMPTY_INT
		self.boundary = boundarystructureboundary('boundary')
		self.jni = jnistructurejni('jni')
		self.sigma_par = sigma_parstructurecoreprofile('sigma_par')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class psistructurepsi\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddrho.__str__()
		ret = ret + space + 'Attribute ddrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d2drho2.__str__()
		ret = ret + space + 'Attribute d2drho2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt_rhotorn.__str__()
		ret = ret + space + 'Attribute ddt_rhotorn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt_phi.__str__()
		ret = ret + space + 'Attribute ddt_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute jni\n ' + self.jni.__str__(depth+1)
		ret = ret + space + 'Attribute sigma_par\n ' + self.sigma_par.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.jni.setExpIdx(idx)
		self.sigma_par.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type psistructurepsi, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddt_rhotorn', numpy.array(self.ddt_rhotorn).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddt_phi', numpy.array(self.ddt_phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'flag', self.flag, self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.jni.cpoTime = self.cpoTime
		self.jni.putSlice(path, cpopath)
		self.sigma_par.cpoTime = self.cpoTime
		self.sigma_par.putSlice(path, cpopath)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type psistructurepsi, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddt_rhotorn', numpy.array(self.ddt_rhotorn).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddt_phi', numpy.array(self.ddt_phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.jni.replaceLastSlice(path, cpopath)
		self.sigma_par.replaceLastSlice(path, cpopath)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type psistructurepsi, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.jni.putNonTimed(path, cpopath)
		self.sigma_par.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type psistructurepsi, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_ddrho, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
			self.cpoTime = retTime
		status, ret_d2drho2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
			self.cpoTime = retTime
		status, ret_ddt_rhotorn, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddt_rhotorn', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt_rhotorn = ret_ddt_rhotorn
			self.cpoTime = retTime
		status, ret_ddt_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddt_phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt_phi = ret_ddt_phi
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_flag, retTime = ull.getIntSlice(self.idx, path, cpopath + 'flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flag = ret_flag
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.jni.getSlice(path, cpopath, inTime, interpolMode)
		self.sigma_par.getSlice(path, cpopath, inTime, interpolMode)
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type psistructurepsi, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, ddrhoList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddrho')
			if len(ddrhoList) == 0:
				ddrhoList = numpy.resize(ddrhoList, (0,nbslice))
			check_status(status)
			status, d2drho2List = ull.getVect2DDouble(self.idx, path, cpopath + 'd2drho2')
			if len(d2drho2List) == 0:
				d2drho2List = numpy.resize(d2drho2List, (0,nbslice))
			check_status(status)
			status, ddt_rhotornList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddt_rhotorn')
			if len(ddt_rhotornList) == 0:
				ddt_rhotornList = numpy.resize(ddt_rhotornList, (0,nbslice))
			check_status(status)
			status, ddt_phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddt_phi')
			if len(ddt_phiList) == 0:
				ddt_phiList = numpy.resize(ddt_phiList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, flagList = ull.getVect1DInt(self.idx, path, cpopath + 'flag')
			if len(flagList) == 0:
				flagList = numpy.resize(flagList, (nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			jniList = self.jni.build_non_resampled_data(path, cpopath, nbslice)
			sigma_parList = self.sigma_par.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = psistructurepsi(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.ddrho = ddrhoList[:,i]
				slice.d2drho2 = d2drho2List[:,i]
				slice.ddt_rhotorn = ddt_rhotornList[:,i]
				slice.ddt_phi = ddt_phiList[:,i]
				slice.source = sourceVal
				slice.flag = int(flagList[i].copy())
				slice.boundary = boundaryList[i]
				slice.jni = jniList[i]
				slice.sigma_par = sigma_parList[i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type psistructurepsiObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddrho', i, numpy.array(self.ddrho).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd2drho2', i, numpy.array(self.d2drho2).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddt_rhotorn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddt_rhotorn', i, numpy.array(self.ddt_rhotorn).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddt_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddt_phi', i, numpy.array(self.ddt_phi).astype(numpy.float64))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.jni.putTimedElt(path, cpopath + 'jni', i, obj)
		obj = self.sigma_par.putTimedElt(path, cpopath + 'sigma_par', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type psistructurepsiObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		status, ret_ddrho = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddrho', i)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		status, ret_d2drho2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd2drho2', i)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddt_rhotorn') 
			print ('obj = ' + str(obj))
		status, ret_ddt_rhotorn = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddt_rhotorn', i)
		check_status(status)
		if not status:
			self.ddt_rhotorn = ret_ddt_rhotorn
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddt_phi') 
			print ('obj = ' + str(obj))
		status, ret_ddt_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddt_phi', i)
		check_status(status)
		if not status:
			self.ddt_phi = ret_ddt_phi
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.jni.getTimedElt(path, cpopath + 'jni', i, obj)
		self.sigma_par.getTimedElt(path, cpopath + 'sigma_par', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type psistructurepsiObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.jni.putNonTimedElt(path, cpopath + 'jni', i, obj)
		obj = self.sigma_par.putNonTimedElt(path, cpopath + 'sigma_par', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type psistructurepsiObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.jni.getNonTimedElt(path, cpopath + 'jni', i, obj)
		self.sigma_par.getNonTimedElt(path, cpopath + 'sigma_par', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'ddrho')
		ull.deleteData(self.idx, path, cpopath + 'd2drho2')
		ull.deleteData(self.idx, path, cpopath + 'ddt_rhotorn')
		ull.deleteData(self.idx, path, cpopath + 'ddt_phi')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		self.boundary.deleteData(path, cpopath)
		self.jni.deleteData(path, cpopath)
		self.sigma_par.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)


class boundarystructureboundary(KeepInOrder):
	'''
	class boundarystructureboundary
	Boundary condition for the transport equation. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-Wb, 2-A, 3-V]. For type 1 to 3, only the first position in the vector is used. For type 5, all three positions are used, meaning respectively a1, a2, a3. Time-dependent. Vector(3).
	- source : str
	   Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); String
	- type : int
	   Type of the boundary condition for the transport solver (in case flag = 2). 0- equation not solved; 1- edge value of poloidal flux; 2- total current inside boundary; 3- edge Vloop; 4- not defined; 5- generic boundary condition expressed as a1*(dpsi_drho_tor)+a2*psi=a3. . Time-dependent.Scalar
	- rho : float
	   Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the value of the data are considered to be prescribed. Scalar
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='boundary'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.type = EMPTY_INT
		self.rho = EMPTY_DOUBLE
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class boundarystructureboundary\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute type: ' + str(self.type) + '\n'
		ret = ret + space + 'Attribute rho: ' + str(self.rho) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundary, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'type', self.type, self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundary, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'type', self.type)
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundary, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'rho', self.rho)
		check_status(status)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundary, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_type, retTime = ull.getIntSlice(self.idx, path, cpopath + 'type', inTime, interpolMode)
		check_status(status)
		if not status:
			self.type = ret_type
			self.cpoTime = retTime
		status, ret_rho = ull.getDouble(self.idx, path, cpopath + 'rho')
		check_status(status)
		if not status:
			self.rho = ret_rho
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundary, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, typeList = ull.getVect1DInt(self.idx, path, cpopath + 'type')
			if len(typeList) == 0:
				typeList = numpy.resize(typeList, (nbslice))
			check_status(status)
			status, rhoVal = ull.getDouble(self.idx, path, cpopath + 'rho')
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = boundarystructureboundary(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				slice.type = int(typeList[i].copy())
				slice.rho = rhoVal
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'type', i, self.type)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getIntFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rho') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rho', i, self.rho)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rho') 
			print ('obj = ' + str(obj))
		status, ret_rho = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rho', i)
		check_status(status)
		if not status:
			self.rho = ret_rho
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'rho')
		self.codeparam.deleteData(path, cpopath)


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


class jnistructurejni(KeepInOrder):
	'''
	class jnistructurejni
	Non-inductive parallel current density [A/m^2]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Value of jni; Time-dependent; Vector (nrho)
	- integral : numpy.ndarray 1D with float
	   Integral from 0 to rho of jni. Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='jni'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.integral = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jnistructurejni\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.integral.__str__()
		ret = ret + space + 'Attribute integral\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurejni, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurejni, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurejni, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurejni, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_integral, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'integral', inTime, interpolMode)
		check_status(status)
		if not status:
			self.integral = ret_integral
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurejni, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, integralList = ull.getVect2DDouble(self.idx, path, cpopath + 'integral')
			if len(integralList) == 0:
				integralList = numpy.resize(integralList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = jnistructurejni(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.integral = integralList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurejniObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'integral', i, numpy.array(self.integral).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurejniObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		status, ret_integral = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'integral', i)
		check_status(status)
		if not status:
			self.integral = ret_integral

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurejniObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurejniObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'integral')
		ull.deleteData(self.idx, path, cpopath + 'source')


class sigma_parstructurecoreprofile(KeepInOrder):
	'''
	class sigma_parstructurecoreprofile
	Parallel conductivity [ohm^-1.m^-1]. Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='sigma_par'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class sigma_parstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sigma_parstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sigma_parstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sigma_parstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type sigma_parstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type sigma_parstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = sigma_parstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigma_parstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigma_parstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigma_parstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigma_parstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class testructurecorefield(KeepInOrder):
	'''
	class testructurecorefield
	Electron temperature [eV]; (source term in [W.m^-3]). Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- ddrho : numpy.ndarray 1D with float
	   Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Vector (nrho)
	- d2drho2 : numpy.ndarray 1D with float
	   Second order radial derivative (d2value/drho_tor^2) [signal_value_unit.m^-2]; Time-dependent; Vector (nrho)
	- ddt : numpy.ndarray 1D with float
	   Time derivative (dvalue/dtime) [signal_value_unit.s^-1]; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	- flag : int
	   Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-calculated by a separate code : in that case only, description of the code provided in codeparam at the same level; 4-used value from the previous time step; Time-dependent; Scalar
	- boundary : class boundarystructureboundaryel
	   Boundary condition for the transport equation. Time-dependent.
	- source_term : class source_termstructuresourceel
	   Total source term for the transport equation. Time-dependent.
	- transp_coef : class transp_coefstructurecoretransel
	   Total transport coefficients. Time-dependent.
	- flux : class fluxstructurefluxel
	   Fluxes of the quantity, two definitions. Time-dependent.
	- flux_dv_surf : numpy.ndarray 1D with float
	   Net flux through the magnetic surface, i.e. integral over the magnetic surface area of flux_dv. Time-dependent; Vector (nrho)
	- time_deriv : numpy.ndarray 1D with float
	   Integral of the time derivative term of the transport equation. Time-dependent. Vector (nrho)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='te'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.ddrho = numpy.zeros(0, numpy.float64, order='C')
		self.d2drho2 = numpy.zeros(0, numpy.float64, order='C')
		self.ddt = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.flag = EMPTY_INT
		self.boundary = boundarystructureboundaryel('boundary')
		self.source_term = source_termstructuresourceel('source_term')
		self.transp_coef = transp_coefstructurecoretransel('transp_coef')
		self.flux = fluxstructurefluxel('flux')
		self.flux_dv_surf = numpy.zeros(0, numpy.float64, order='C')
		self.time_deriv = numpy.zeros(0, numpy.float64, order='C')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class testructurecorefield\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddrho.__str__()
		ret = ret + space + 'Attribute ddrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d2drho2.__str__()
		ret = ret + space + 'Attribute d2drho2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt.__str__()
		ret = ret + space + 'Attribute ddt\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute source_term\n ' + self.source_term.__str__(depth+1)
		ret = ret + space + 'Attribute transp_coef\n ' + self.transp_coef.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		s = self.flux_dv_surf.__str__()
		ret = ret + space + 'Attribute flux_dv_surf\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.time_deriv.__str__()
		ret = ret + space + 'Attribute time_deriv\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.source_term.setExpIdx(idx)
		self.transp_coef.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type testructurecorefield, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'flag', self.flag, self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.source_term.cpoTime = self.cpoTime
		self.source_term.putSlice(path, cpopath)
		self.transp_coef.cpoTime = self.cpoTime
		self.transp_coef.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type testructurecorefield, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.source_term.replaceLastSlice(path, cpopath)
		self.transp_coef.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64))
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type testructurecorefield, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.source_term.putNonTimed(path, cpopath)
		self.transp_coef.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type testructurecorefield, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_ddrho, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
			self.cpoTime = retTime
		status, ret_d2drho2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
			self.cpoTime = retTime
		status, ret_ddt, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_flag, retTime = ull.getIntSlice(self.idx, path, cpopath + 'flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flag = ret_flag
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.source_term.getSlice(path, cpopath, inTime, interpolMode)
		self.transp_coef.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flux_dv_surf, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
			self.cpoTime = retTime
		status, ret_time_deriv, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'time_deriv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type testructurecorefield, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, ddrhoList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddrho')
			if len(ddrhoList) == 0:
				ddrhoList = numpy.resize(ddrhoList, (0,nbslice))
			check_status(status)
			status, d2drho2List = ull.getVect2DDouble(self.idx, path, cpopath + 'd2drho2')
			if len(d2drho2List) == 0:
				d2drho2List = numpy.resize(d2drho2List, (0,nbslice))
			check_status(status)
			status, ddtList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddt')
			if len(ddtList) == 0:
				ddtList = numpy.resize(ddtList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, flagList = ull.getVect1DInt(self.idx, path, cpopath + 'flag')
			if len(flagList) == 0:
				flagList = numpy.resize(flagList, (nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			source_termList = self.source_term.build_non_resampled_data(path, cpopath, nbslice)
			transp_coefList = self.transp_coef.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			status, flux_dv_surfList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux_dv_surf')
			if len(flux_dv_surfList) == 0:
				flux_dv_surfList = numpy.resize(flux_dv_surfList, (0,nbslice))
			check_status(status)
			status, time_derivList = ull.getVect2DDouble(self.idx, path, cpopath + 'time_deriv')
			if len(time_derivList) == 0:
				time_derivList = numpy.resize(time_derivList, (0,nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = testructurecorefield(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.ddrho = ddrhoList[:,i]
				slice.d2drho2 = d2drho2List[:,i]
				slice.ddt = ddtList[:,i]
				slice.source = sourceVal
				slice.flag = int(flagList[i].copy())
				slice.boundary = boundaryList[i]
				slice.source_term = source_termList[i]
				slice.transp_coef = transp_coefList[i]
				slice.flux = fluxList[i]
				slice.flux_dv_surf = flux_dv_surfList[:,i]
				slice.time_deriv = time_derivList[:,i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructurecorefieldObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddrho', i, numpy.array(self.ddrho).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd2drho2', i, numpy.array(self.d2drho2).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddt', i, numpy.array(self.ddt).astype(numpy.float64))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux_dv_surf', i, numpy.array(self.flux_dv_surf).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'time_deriv', i, numpy.array(self.time_deriv).astype(numpy.float64))
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructurecorefieldObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		status, ret_ddrho = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddrho', i)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		status, ret_d2drho2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd2drho2', i)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		status, ret_ddt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddt', i)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv_surf = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv_surf', i)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		status, ret_time_deriv = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'time_deriv', i)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructurecorefieldObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putNonTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructurecorefieldObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getNonTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'ddrho')
		ull.deleteData(self.idx, path, cpopath + 'd2drho2')
		ull.deleteData(self.idx, path, cpopath + 'ddt')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		self.boundary.deleteData(path, cpopath)
		self.source_term.deleteData(path, cpopath)
		self.transp_coef.deleteData(path, cpopath)
		self.flux.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flux_dv_surf')
		ull.deleteData(self.idx, path, cpopath + 'time_deriv')
		self.codeparam.deleteData(path, cpopath)


class boundarystructureboundaryel(KeepInOrder):
	'''
	class boundarystructureboundaryel
	Boundary condition for the transport equation. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-field, 2-field.m^-1, 3-m, 4-field.s^-1]. For type 1 to 4, only the first position in the vector is used. For type 5, all three positions are used, meaning respectively a1, a2, a3. Time-dependent. Vector(3).
	- source : str
	   Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); String
	- type : int
	   Type of the boundary condition for the transport solver (in case flag = 2). 0- equation not solved; 1- value of the field y; 2-radial derivative of the field (-dy/drho_tor); 3-scale length of the field y/(-dy/drho_tor); 4- flux; 5- generic boundary condition y expressed as a1y'+a2y=a3. Time-dependent. Scalar
	- rho_tor : float
	   Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the value of the data are considered to be prescribed. Time-dependent. Scalar
	'''

	def __init__(self, base_path_in='boundary'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.type = EMPTY_INT
		self.rho_tor = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class boundarystructureboundaryel\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute type: ' + str(self.type) + '\n'
		ret = ret + space + 'Attribute rho_tor: ' + str(self.rho_tor) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'type', self.type, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'rho_tor', self.rho_tor, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'type', self.type)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'rho_tor', self.rho_tor)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_type, retTime = ull.getIntSlice(self.idx, path, cpopath + 'type', inTime, interpolMode)
		check_status(status)
		if not status:
			self.type = ret_type
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, typeList = ull.getVect1DInt(self.idx, path, cpopath + 'type')
			if len(typeList) == 0:
				typeList = numpy.resize(typeList, (nbslice))
			check_status(status)
			status, rho_torList = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = boundarystructureboundaryel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				slice.type = int(typeList[i].copy())
				slice.rho_tor = rho_torList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'type', i, self.type)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, self.rho_tor)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getIntFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')


class source_termstructuresourceel(KeepInOrder):
	'''
	class source_termstructuresourceel
	Total source term for the transport equation. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Value of the source term; Time-dependent; Vector (nrho)
	- integral : numpy.ndarray 1D with float
	   Integral from 0 to rho of the source term. Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='source_term'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.integral = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class source_termstructuresourceel\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.integral.__str__()
		ret = ret + space + 'Attribute integral\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_integral, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'integral', inTime, interpolMode)
		check_status(status)
		if not status:
			self.integral = ret_integral
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, integralList = ull.getVect2DDouble(self.idx, path, cpopath + 'integral')
			if len(integralList) == 0:
				integralList = numpy.resize(integralList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = source_termstructuresourceel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.integral = integralList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'integral', i, numpy.array(self.integral).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		status, ret_integral = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'integral', i)
		check_status(status)
		if not status:
			self.integral = ret_integral

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'integral')
		ull.deleteData(self.idx, path, cpopath + 'source')


class transp_coefstructurecoretransel(KeepInOrder):
	'''
	class transp_coefstructurecoretransel
	Total transport coefficients. Time-dependent.

	Attributes:
	- diff : numpy.ndarray 1D with float
	   Diffusion coefficient [m^2.s^-1]. Time-dependent; Vector (nrho)
	- vconv : numpy.ndarray 1D with float
	   Convection coefficient [m.s^-1]. Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='transp_coef'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff = numpy.zeros(0, numpy.float64, order='C')
		self.vconv = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class transp_coefstructurecoretransel\n'
		s = self.diff.__str__()
		ret = ret + space + 'Attribute diff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv.__str__()
		ret = ret + space + 'Attribute vconv\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'diff', numpy.array(self.diff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vconv', numpy.array(self.vconv).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'diff', numpy.array(self.diff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vconv', numpy.array(self.vconv).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'diff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff = ret_diff
			self.cpoTime = retTime
		status, ret_vconv, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vconv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv = ret_vconv
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diffList = ull.getVect2DDouble(self.idx, path, cpopath + 'diff')
			if len(diffList) == 0:
				diffList = numpy.resize(diffList, (0,nbslice))
			check_status(status)
			status, vconvList = ull.getVect2DDouble(self.idx, path, cpopath + 'vconv')
			if len(vconvList) == 0:
				vconvList = numpy.resize(vconvList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = transp_coefstructurecoretransel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff = diffList[:,i]
				slice.vconv = vconvList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretranselObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'diff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'diff', i, numpy.array(self.diff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vconv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vconv', i, numpy.array(self.vconv).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretranselObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'diff') 
			print ('obj = ' + str(obj))
		status, ret_diff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'diff', i)
		check_status(status)
		if not status:
			self.diff = ret_diff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vconv') 
			print ('obj = ' + str(obj))
		status, ret_vconv = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vconv', i)
		check_status(status)
		if not status:
			self.vconv = ret_vconv

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretranselObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretranselObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff')
		ull.deleteData(self.idx, path, cpopath + 'vconv')
		ull.deleteData(self.idx, path, cpopath + 'source')


class fluxstructurefluxel(KeepInOrder):
	'''
	class fluxstructurefluxel
	Fluxes of the quantity, two definitions. Time-dependent.

	Attributes:
	- flux_dv : numpy.ndarray 1D with float
	   Flux of the field calculated from the transport coefficients. Time-dependent; Vector (nrho)
	- flux_interp : numpy.ndarray 1D with float
	   Interpretative flux deduced from measured data, the integral of the source term, and the time derivative of the field. Time-dependent; Vector (nrho)
	'''

	def __init__(self, base_path_in='flux'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.flux_dv = numpy.zeros(0, numpy.float64, order='C')
		self.flux_interp = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fluxstructurefluxel\n'
		s = self.flux_dv.__str__()
		ret = ret + space + 'Attribute flux_dv\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux_interp.__str__()
		ret = ret + space + 'Attribute flux_interp\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv', numpy.array(self.flux_dv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux_interp', numpy.array(self.flux_interp).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv', numpy.array(self.flux_dv).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux_interp', numpy.array(self.flux_interp).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_flux_dv, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv = ret_flux_dv
			self.cpoTime = retTime
		status, ret_flux_interp, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux_interp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_interp = ret_flux_interp
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, flux_dvList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux_dv')
			if len(flux_dvList) == 0:
				flux_dvList = numpy.resize(flux_dvList, (0,nbslice))
			check_status(status)
			status, flux_interpList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux_interp')
			if len(flux_interpList) == 0:
				flux_interpList = numpy.resize(flux_interpList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = fluxstructurefluxel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.flux_dv = flux_dvList[:,i]
				slice.flux_interp = flux_interpList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux_dv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux_dv', i, numpy.array(self.flux_dv).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux_interp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux_interp', i, numpy.array(self.flux_interp).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux_dv') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv', i)
		check_status(status)
		if not status:
			self.flux_dv = ret_flux_dv
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux_interp') 
			print ('obj = ' + str(obj))
		status, ret_flux_interp = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux_interp', i)
		check_status(status)
		if not status:
			self.flux_interp = ret_flux_interp

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'flux_dv')
		ull.deleteData(self.idx, path, cpopath + 'flux_interp')


class tistructurecorefieldion(KeepInOrder):
	'''
	class tistructurecorefieldion
	Ion temperature [eV]; (source term in [W.m^-3]). Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- ddrho : numpy.ndarray 2D with float
	   Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Matrix (nrho,nion)
	- d2drho2 : numpy.ndarray 2D with float
	   Second order radial derivative (d2value/drho_tor^2) [signal_value_unit.m^-2]; Time-dependent; Matrix (nrho,nion)
	- ddt : numpy.ndarray 2D with float
	   Time derivative (dvalue/dtime) [signal_value_unit.s^-1]; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	- flag : numpy.ndarray 1D with int)
	   Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-calculated by a separate code : in that case only, description of the code provided in codeparam at the same level; 4-used value from the previous time step; Time-dependent; Vector(nion)
	- boundary : class boundarystructureboundaryion
	   Boundary condition for the transport equation
	- source_term : class source_termstructuresourceion
	   Total source term for the transport equation. Time-dependent.
	- transp_coef : class transp_coefstructurecoretransion
	   Total transport coefficients. Time-dependent.
	- flux : class fluxstructurefluxion
	   Fluxes of the quantity, two definitions. Time-dependent.
	- flux_dv_surf : numpy.ndarray 2D with float
	   Net flux through the magnetic surface, i.e. integral over the magnetic surface area of flux_dv. Time-dependent; Matrix(nrho,nion)
	- time_deriv : numpy.ndarray 2D with float
	   Integral of the time derivative term of the transport equation. Time-dependent. Matrix (nrho,nion)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='ti'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.ddrho = numpy.zeros((0,0), numpy.float64, order='C')
		self.d2drho2 = numpy.zeros((0,0), numpy.float64, order='C')
		self.ddt = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']
		self.flag = numpy.zeros(0, numpy.int32, order='C')
		self.boundary = boundarystructureboundaryion('boundary')
		self.source_term = source_termstructuresourceion('source_term')
		self.transp_coef = transp_coefstructurecoretransion('transp_coef')
		self.flux = fluxstructurefluxion('flux')
		self.flux_dv_surf = numpy.zeros((0,0), numpy.float64, order='C')
		self.time_deriv = numpy.zeros((0,0), numpy.float64, order='C')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class tistructurecorefieldion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddrho.__str__()
		ret = ret + space + 'Attribute ddrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d2drho2.__str__()
		ret = ret + space + 'Attribute d2drho2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt.__str__()
		ret = ret + space + 'Attribute ddt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flag.__str__()
		ret = ret + space + 'Attribute flag\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute source_term\n ' + self.source_term.__str__(depth+1)
		ret = ret + space + 'Attribute transp_coef\n ' + self.transp_coef.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		s = self.flux_dv_surf.__str__()
		ret = ret + space + 'Attribute flux_dv_surf\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.time_deriv.__str__()
		ret = ret + space + 'Attribute time_deriv\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.source_term.setExpIdx(idx)
		self.transp_coef.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tistructurecorefieldion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'flag', numpy.array(self.flag).astype(numpy.int32), self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.source_term.cpoTime = self.cpoTime
		self.source_term.putSlice(path, cpopath)
		self.transp_coef.cpoTime = self.cpoTime
		self.transp_coef.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tistructurecorefieldion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'flag', numpy.array(self.flag).astype(numpy.int32))
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.source_term.replaceLastSlice(path, cpopath)
		self.transp_coef.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64))
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tistructurecorefieldion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.source_term.putNonTimed(path, cpopath)
		self.transp_coef.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type tistructurecorefieldion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_ddrho, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
			self.cpoTime = retTime
		status, ret_d2drho2, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
			self.cpoTime = retTime
		status, ret_ddt, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_flag, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flag = ret_flag
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.source_term.getSlice(path, cpopath, inTime, interpolMode)
		self.transp_coef.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flux_dv_surf, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
			self.cpoTime = retTime
		status, ret_time_deriv, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type tistructurecorefieldion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, ddrhoList = ull.getVect3DDouble(self.idx, path, cpopath + 'ddrho')
			if len(ddrhoList) == 0:
				ddrhoList = numpy.resize(ddrhoList, (0,0,nbslice))
			check_status(status)
			status, d2drho2List = ull.getVect3DDouble(self.idx, path, cpopath + 'd2drho2')
			if len(d2drho2List) == 0:
				d2drho2List = numpy.resize(d2drho2List, (0,0,nbslice))
			check_status(status)
			status, ddtList = ull.getVect3DDouble(self.idx, path, cpopath + 'ddt')
			if len(ddtList) == 0:
				ddtList = numpy.resize(ddtList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, flagList = ull.getVect2DInt(self.idx, path, cpopath + 'flag')
			if len(flagList) == 0:
				flagList = numpy.resize(flagList, (0,nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			source_termList = self.source_term.build_non_resampled_data(path, cpopath, nbslice)
			transp_coefList = self.transp_coef.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			status, flux_dv_surfList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux_dv_surf')
			if len(flux_dv_surfList) == 0:
				flux_dv_surfList = numpy.resize(flux_dv_surfList, (0,0,nbslice))
			check_status(status)
			status, time_derivList = ull.getVect3DDouble(self.idx, path, cpopath + 'time_deriv')
			if len(time_derivList) == 0:
				time_derivList = numpy.resize(time_derivList, (0,0,nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = tistructurecorefieldion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.ddrho = ddrhoList[:,:,i]
				slice.d2drho2 = d2drho2List[:,:,i]
				slice.ddt = ddtList[:,:,i]
				slice.source = sourceVal
				slice.flag = flagList[:,i]
				slice.boundary = boundaryList[i]
				slice.source_term = source_termList[i]
				slice.transp_coef = transp_coefList[i]
				slice.flux = fluxList[i]
				slice.flux_dv_surf = flux_dv_surfList[:,:,i]
				slice.time_deriv = time_derivList[:,:,i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistructurecorefieldionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ddrho', i, numpy.array(self.ddrho).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd2drho2', i, numpy.array(self.d2drho2).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ddt', i, numpy.array(self.ddt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'flag', i, numpy.array(self.flag).astype(numpy.int32))
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux_dv_surf', i, numpy.array(self.flux_dv_surf).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'time_deriv', i, numpy.array(self.time_deriv).astype(numpy.float64))
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistructurecorefieldionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		status, ret_ddrho = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ddrho', i)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		status, ret_d2drho2 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd2drho2', i)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		status, ret_ddt = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ddt', i)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv_surf = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv_surf', i)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		status, ret_time_deriv = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'time_deriv', i)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistructurecorefieldionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putNonTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistructurecorefieldionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getNonTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'ddrho')
		ull.deleteData(self.idx, path, cpopath + 'd2drho2')
		ull.deleteData(self.idx, path, cpopath + 'ddt')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		self.boundary.deleteData(path, cpopath)
		self.source_term.deleteData(path, cpopath)
		self.transp_coef.deleteData(path, cpopath)
		self.flux.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flux_dv_surf')
		ull.deleteData(self.idx, path, cpopath + 'time_deriv')
		self.codeparam.deleteData(path, cpopath)


class boundarystructureboundaryion(KeepInOrder):
	'''
	class boundarystructureboundaryion
	Boundary condition for the transport equation

	Attributes:
	- value : numpy.ndarray 2D with float
	   Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-field, 2-field.m^-1, 3-m, 4-field.s^-1]. For type 1 to 4, only the first position in the first dimension is used. For type 5, all three positions are used, meaning respectively a1, a2, a3. Time-dependent. Matrix(3,nion)
	- source : list of str
	   Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); Array of strings (nion)
	- type : numpy.ndarray 1D with int)
	   Type of the boundary condition for the transport solver (in case flag = 2). 0- equation not solved; 1- value of the field y; 2-radial derivative of the field (-dy/drho_tor); 3-scale length of the field y/(-dy/drho_tor); 4- flux; 5- generic boundary condition y expressed as a1y'+a2y=a3. Time-dependent. Vector(nion)
	- rho_tor : numpy.ndarray 1D with float
	   Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the value of the data are considered to be prescribed. Time-dependent. Vector(nion)
	'''

	def __init__(self, base_path_in='boundary'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']
		self.type = numpy.zeros(0, numpy.int32, order='C')
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class boundarystructureboundaryion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.type.__str__()
		ret = ret + space + 'Attribute type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'type', numpy.array(self.type).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'type', numpy.array(self.type).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_type, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'type', inTime, interpolMode)
		check_status(status)
		if not status:
			self.type = ret_type
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystructureboundaryion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, typeList = ull.getVect2DInt(self.idx, path, cpopath + 'type')
			if len(typeList) == 0:
				typeList = numpy.resize(typeList, (0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = boundarystructureboundaryion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.source = sourceVal
				slice.type = typeList[:,i]
				slice.rho_tor = rho_torList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'type', i, numpy.array(self.type).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystructureboundaryionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')


class source_termstructuresourceion(KeepInOrder):
	'''
	class source_termstructuresourceion
	Total source term for the transport equation. Time-dependent.

	Attributes:
	- value : numpy.ndarray 2D with float
	   Value of the source term; Time-dependent; Matrix (nrho,nion)
	- integral : numpy.ndarray 2D with float
	   Integral from 0 to rho of the source term. Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='source_term'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.integral = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class source_termstructuresourceion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.integral.__str__()
		ret = ret + space + 'Attribute integral\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_integral, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'integral', inTime, interpolMode)
		check_status(status)
		if not status:
			self.integral = ret_integral
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type source_termstructuresourceion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, integralList = ull.getVect3DDouble(self.idx, path, cpopath + 'integral')
			if len(integralList) == 0:
				integralList = numpy.resize(integralList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = source_termstructuresourceion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.integral = integralList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'integral', i, numpy.array(self.integral).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		status, ret_integral = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'integral', i)
		check_status(status)
		if not status:
			self.integral = ret_integral

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_termstructuresourceionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'integral')
		ull.deleteData(self.idx, path, cpopath + 'source')


class transp_coefstructurecoretransion(KeepInOrder):
	'''
	class transp_coefstructurecoretransion
	Total transport coefficients. Time-dependent.

	Attributes:
	- diff : numpy.ndarray 2D with float
	   Diffusion coefficient [m^2.s^-1]. Time-dependent; Matrix (nrho,nion)
	- vconv : numpy.ndarray 2D with float
	   Convection coefficient [m.s^-1]. Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='transp_coef'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.diff = numpy.zeros((0,0), numpy.float64, order='C')
		self.vconv = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class transp_coefstructurecoretransion\n'
		s = self.diff.__str__()
		ret = ret + space + 'Attribute diff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vconv.__str__()
		ret = ret + space + 'Attribute vconv\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'diff', numpy.array(self.diff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vconv', numpy.array(self.vconv).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'diff', numpy.array(self.diff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vconv', numpy.array(self.vconv).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_diff, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'diff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.diff = ret_diff
			self.cpoTime = retTime
		status, ret_vconv, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vconv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vconv = ret_vconv
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type transp_coefstructurecoretransion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, diffList = ull.getVect3DDouble(self.idx, path, cpopath + 'diff')
			if len(diffList) == 0:
				diffList = numpy.resize(diffList, (0,0,nbslice))
			check_status(status)
			status, vconvList = ull.getVect3DDouble(self.idx, path, cpopath + 'vconv')
			if len(vconvList) == 0:
				vconvList = numpy.resize(vconvList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = transp_coefstructurecoretransion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.diff = diffList[:,:,i]
				slice.vconv = vconvList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretransionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'diff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'diff', i, numpy.array(self.diff).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vconv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vconv', i, numpy.array(self.vconv).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretransionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'diff') 
			print ('obj = ' + str(obj))
		status, ret_diff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'diff', i)
		check_status(status)
		if not status:
			self.diff = ret_diff
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vconv') 
			print ('obj = ' + str(obj))
		status, ret_vconv = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vconv', i)
		check_status(status)
		if not status:
			self.vconv = ret_vconv

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretransionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transp_coefstructurecoretransionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'diff')
		ull.deleteData(self.idx, path, cpopath + 'vconv')
		ull.deleteData(self.idx, path, cpopath + 'source')


class fluxstructurefluxion(KeepInOrder):
	'''
	class fluxstructurefluxion
	Fluxes of the quantity, two definitions. Time-dependent.

	Attributes:
	- flux_dv : numpy.ndarray 2D with float
	   Flux of the field calculated from the transport coefficients. Time-dependent; Matrix (nrho,nion)
	- flux_interp : numpy.ndarray 2D with float
	   Interpretative flux deduced from measured data, the integral of the source term, and the time derivative of the field. Time-dependent; Matrix (nrho,nion)
	'''

	def __init__(self, base_path_in='flux'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.flux_dv = numpy.zeros((0,0), numpy.float64, order='C')
		self.flux_interp = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fluxstructurefluxion\n'
		s = self.flux_dv.__str__()
		ret = ret + space + 'Attribute flux_dv\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux_interp.__str__()
		ret = ret + space + 'Attribute flux_interp\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv', numpy.array(self.flux_dv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux_interp', numpy.array(self.flux_interp).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv', numpy.array(self.flux_dv).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux_interp', numpy.array(self.flux_interp).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_flux_dv, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv = ret_flux_dv
			self.cpoTime = retTime
		status, ret_flux_interp, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux_interp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_interp = ret_flux_interp
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructurefluxion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, flux_dvList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux_dv')
			if len(flux_dvList) == 0:
				flux_dvList = numpy.resize(flux_dvList, (0,0,nbslice))
			check_status(status)
			status, flux_interpList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux_interp')
			if len(flux_interpList) == 0:
				flux_interpList = numpy.resize(flux_interpList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = fluxstructurefluxion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.flux_dv = flux_dvList[:,:,i]
				slice.flux_interp = flux_interpList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux_dv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux_dv', i, numpy.array(self.flux_dv).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux_interp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux_interp', i, numpy.array(self.flux_interp).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux_dv') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv', i)
		check_status(status)
		if not status:
			self.flux_dv = ret_flux_dv
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux_interp') 
			print ('obj = ' + str(obj))
		status, ret_flux_interp = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux_interp', i)
		check_status(status)
		if not status:
			self.flux_interp = ret_flux_interp

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructurefluxionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'flux_dv')
		ull.deleteData(self.idx, path, cpopath + 'flux_interp')


class nestructurecorefield(KeepInOrder):
	'''
	class nestructurecorefield
	Electron density [m^-3]; (source term in [m^-3]).Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- ddrho : numpy.ndarray 1D with float
	   Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Vector (nrho)
	- d2drho2 : numpy.ndarray 1D with float
	   Second order radial derivative (d2value/drho_tor^2) [signal_value_unit.m^-2]; Time-dependent; Vector (nrho)
	- ddt : numpy.ndarray 1D with float
	   Time derivative (dvalue/dtime) [signal_value_unit.s^-1]; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	- flag : int
	   Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-calculated by a separate code : in that case only, description of the code provided in codeparam at the same level; 4-used value from the previous time step; Time-dependent; Scalar
	- boundary : class boundarystructureboundaryel
	   Boundary condition for the transport equation. Time-dependent.
	- source_term : class source_termstructuresourceel
	   Total source term for the transport equation. Time-dependent.
	- transp_coef : class transp_coefstructurecoretransel
	   Total transport coefficients. Time-dependent.
	- flux : class fluxstructurefluxel
	   Fluxes of the quantity, two definitions. Time-dependent.
	- flux_dv_surf : numpy.ndarray 1D with float
	   Net flux through the magnetic surface, i.e. integral over the magnetic surface area of flux_dv. Time-dependent; Vector (nrho)
	- time_deriv : numpy.ndarray 1D with float
	   Integral of the time derivative term of the transport equation. Time-dependent. Vector (nrho)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='ne'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.ddrho = numpy.zeros(0, numpy.float64, order='C')
		self.d2drho2 = numpy.zeros(0, numpy.float64, order='C')
		self.ddt = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.flag = EMPTY_INT
		self.boundary = boundarystructureboundaryel('boundary')
		self.source_term = source_termstructuresourceel('source_term')
		self.transp_coef = transp_coefstructurecoretransel('transp_coef')
		self.flux = fluxstructurefluxel('flux')
		self.flux_dv_surf = numpy.zeros(0, numpy.float64, order='C')
		self.time_deriv = numpy.zeros(0, numpy.float64, order='C')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nestructurecorefield\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddrho.__str__()
		ret = ret + space + 'Attribute ddrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d2drho2.__str__()
		ret = ret + space + 'Attribute d2drho2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt.__str__()
		ret = ret + space + 'Attribute ddt\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute source_term\n ' + self.source_term.__str__(depth+1)
		ret = ret + space + 'Attribute transp_coef\n ' + self.transp_coef.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		s = self.flux_dv_surf.__str__()
		ret = ret + space + 'Attribute flux_dv_surf\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.time_deriv.__str__()
		ret = ret + space + 'Attribute time_deriv\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.source_term.setExpIdx(idx)
		self.transp_coef.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructurecorefield, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'flag', self.flag, self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.source_term.cpoTime = self.cpoTime
		self.source_term.putSlice(path, cpopath)
		self.transp_coef.cpoTime = self.cpoTime
		self.transp_coef.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructurecorefield, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.source_term.replaceLastSlice(path, cpopath)
		self.transp_coef.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64))
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructurecorefield, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.source_term.putNonTimed(path, cpopath)
		self.transp_coef.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nestructurecorefield, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_ddrho, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
			self.cpoTime = retTime
		status, ret_d2drho2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'd2drho2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
			self.cpoTime = retTime
		status, ret_ddt, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ddt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_flag, retTime = ull.getIntSlice(self.idx, path, cpopath + 'flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flag = ret_flag
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.source_term.getSlice(path, cpopath, inTime, interpolMode)
		self.transp_coef.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flux_dv_surf, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
			self.cpoTime = retTime
		status, ret_time_deriv, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'time_deriv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nestructurecorefield, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, ddrhoList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddrho')
			if len(ddrhoList) == 0:
				ddrhoList = numpy.resize(ddrhoList, (0,nbslice))
			check_status(status)
			status, d2drho2List = ull.getVect2DDouble(self.idx, path, cpopath + 'd2drho2')
			if len(d2drho2List) == 0:
				d2drho2List = numpy.resize(d2drho2List, (0,nbslice))
			check_status(status)
			status, ddtList = ull.getVect2DDouble(self.idx, path, cpopath + 'ddt')
			if len(ddtList) == 0:
				ddtList = numpy.resize(ddtList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, flagList = ull.getVect1DInt(self.idx, path, cpopath + 'flag')
			if len(flagList) == 0:
				flagList = numpy.resize(flagList, (nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			source_termList = self.source_term.build_non_resampled_data(path, cpopath, nbslice)
			transp_coefList = self.transp_coef.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			status, flux_dv_surfList = ull.getVect2DDouble(self.idx, path, cpopath + 'flux_dv_surf')
			if len(flux_dv_surfList) == 0:
				flux_dv_surfList = numpy.resize(flux_dv_surfList, (0,nbslice))
			check_status(status)
			status, time_derivList = ull.getVect2DDouble(self.idx, path, cpopath + 'time_deriv')
			if len(time_derivList) == 0:
				time_derivList = numpy.resize(time_derivList, (0,nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = nestructurecorefield(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.ddrho = ddrhoList[:,i]
				slice.d2drho2 = d2drho2List[:,i]
				slice.ddt = ddtList[:,i]
				slice.source = sourceVal
				slice.flag = int(flagList[i].copy())
				slice.boundary = boundaryList[i]
				slice.source_term = source_termList[i]
				slice.transp_coef = transp_coefList[i]
				slice.flux = fluxList[i]
				slice.flux_dv_surf = flux_dv_surfList[:,i]
				slice.time_deriv = time_derivList[:,i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructurecorefieldObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddrho', i, numpy.array(self.ddrho).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'd2drho2', i, numpy.array(self.d2drho2).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ddt', i, numpy.array(self.ddt).astype(numpy.float64))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux_dv_surf', i, numpy.array(self.flux_dv_surf).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'time_deriv', i, numpy.array(self.time_deriv).astype(numpy.float64))
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructurecorefieldObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		status, ret_ddrho = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddrho', i)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		status, ret_d2drho2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'd2drho2', i)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		status, ret_ddt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ddt', i)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv_surf = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv_surf', i)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		status, ret_time_deriv = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'time_deriv', i)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructurecorefieldObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putNonTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructurecorefieldObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getNonTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'ddrho')
		ull.deleteData(self.idx, path, cpopath + 'd2drho2')
		ull.deleteData(self.idx, path, cpopath + 'ddt')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		self.boundary.deleteData(path, cpopath)
		self.source_term.deleteData(path, cpopath)
		self.transp_coef.deleteData(path, cpopath)
		self.flux.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flux_dv_surf')
		ull.deleteData(self.idx, path, cpopath + 'time_deriv')
		self.codeparam.deleteData(path, cpopath)


class nistructurecorefieldion(KeepInOrder):
	'''
	class nistructurecorefieldion
	Ion density [m^-3]; (source term in [m^-3]). Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- ddrho : numpy.ndarray 2D with float
	   Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Matrix (nrho,nion)
	- d2drho2 : numpy.ndarray 2D with float
	   Second order radial derivative (d2value/drho_tor^2) [signal_value_unit.m^-2]; Time-dependent; Matrix (nrho,nion)
	- ddt : numpy.ndarray 2D with float
	   Time derivative (dvalue/dtime) [signal_value_unit.s^-1]; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	- flag : numpy.ndarray 1D with int)
	   Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-calculated by a separate code : in that case only, description of the code provided in codeparam at the same level; 4-used value from the previous time step; Time-dependent; Vector(nion)
	- boundary : class boundarystructureboundaryion
	   Boundary condition for the transport equation
	- source_term : class source_termstructuresourceion
	   Total source term for the transport equation. Time-dependent.
	- transp_coef : class transp_coefstructurecoretransion
	   Total transport coefficients. Time-dependent.
	- flux : class fluxstructurefluxion
	   Fluxes of the quantity, two definitions. Time-dependent.
	- flux_dv_surf : numpy.ndarray 2D with float
	   Net flux through the magnetic surface, i.e. integral over the magnetic surface area of flux_dv. Time-dependent; Matrix(nrho,nion)
	- time_deriv : numpy.ndarray 2D with float
	   Integral of the time derivative term of the transport equation. Time-dependent. Matrix (nrho,nion)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='ni'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.ddrho = numpy.zeros((0,0), numpy.float64, order='C')
		self.d2drho2 = numpy.zeros((0,0), numpy.float64, order='C')
		self.ddt = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']
		self.flag = numpy.zeros(0, numpy.int32, order='C')
		self.boundary = boundarystructureboundaryion('boundary')
		self.source_term = source_termstructuresourceion('source_term')
		self.transp_coef = transp_coefstructurecoretransion('transp_coef')
		self.flux = fluxstructurefluxion('flux')
		self.flux_dv_surf = numpy.zeros((0,0), numpy.float64, order='C')
		self.time_deriv = numpy.zeros((0,0), numpy.float64, order='C')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nistructurecorefieldion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddrho.__str__()
		ret = ret + space + 'Attribute ddrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d2drho2.__str__()
		ret = ret + space + 'Attribute d2drho2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt.__str__()
		ret = ret + space + 'Attribute ddt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flag.__str__()
		ret = ret + space + 'Attribute flag\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute source_term\n ' + self.source_term.__str__(depth+1)
		ret = ret + space + 'Attribute transp_coef\n ' + self.transp_coef.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		s = self.flux_dv_surf.__str__()
		ret = ret + space + 'Attribute flux_dv_surf\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.time_deriv.__str__()
		ret = ret + space + 'Attribute time_deriv\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.source_term.setExpIdx(idx)
		self.transp_coef.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nistructurecorefieldion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'flag', numpy.array(self.flag).astype(numpy.int32), self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.source_term.cpoTime = self.cpoTime
		self.source_term.putSlice(path, cpopath)
		self.transp_coef.cpoTime = self.cpoTime
		self.transp_coef.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nistructurecorefieldion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'flag', numpy.array(self.flag).astype(numpy.int32))
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.source_term.replaceLastSlice(path, cpopath)
		self.transp_coef.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64))
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nistructurecorefieldion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.source_term.putNonTimed(path, cpopath)
		self.transp_coef.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nistructurecorefieldion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_ddrho, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
			self.cpoTime = retTime
		status, ret_d2drho2, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
			self.cpoTime = retTime
		status, ret_ddt, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_flag, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flag = ret_flag
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.source_term.getSlice(path, cpopath, inTime, interpolMode)
		self.transp_coef.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flux_dv_surf, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
			self.cpoTime = retTime
		status, ret_time_deriv, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nistructurecorefieldion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, ddrhoList = ull.getVect3DDouble(self.idx, path, cpopath + 'ddrho')
			if len(ddrhoList) == 0:
				ddrhoList = numpy.resize(ddrhoList, (0,0,nbslice))
			check_status(status)
			status, d2drho2List = ull.getVect3DDouble(self.idx, path, cpopath + 'd2drho2')
			if len(d2drho2List) == 0:
				d2drho2List = numpy.resize(d2drho2List, (0,0,nbslice))
			check_status(status)
			status, ddtList = ull.getVect3DDouble(self.idx, path, cpopath + 'ddt')
			if len(ddtList) == 0:
				ddtList = numpy.resize(ddtList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, flagList = ull.getVect2DInt(self.idx, path, cpopath + 'flag')
			if len(flagList) == 0:
				flagList = numpy.resize(flagList, (0,nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			source_termList = self.source_term.build_non_resampled_data(path, cpopath, nbslice)
			transp_coefList = self.transp_coef.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			status, flux_dv_surfList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux_dv_surf')
			if len(flux_dv_surfList) == 0:
				flux_dv_surfList = numpy.resize(flux_dv_surfList, (0,0,nbslice))
			check_status(status)
			status, time_derivList = ull.getVect3DDouble(self.idx, path, cpopath + 'time_deriv')
			if len(time_derivList) == 0:
				time_derivList = numpy.resize(time_derivList, (0,0,nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = nistructurecorefieldion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.ddrho = ddrhoList[:,:,i]
				slice.d2drho2 = d2drho2List[:,:,i]
				slice.ddt = ddtList[:,:,i]
				slice.source = sourceVal
				slice.flag = flagList[:,i]
				slice.boundary = boundaryList[i]
				slice.source_term = source_termList[i]
				slice.transp_coef = transp_coefList[i]
				slice.flux = fluxList[i]
				slice.flux_dv_surf = flux_dv_surfList[:,:,i]
				slice.time_deriv = time_derivList[:,:,i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistructurecorefieldionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ddrho', i, numpy.array(self.ddrho).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd2drho2', i, numpy.array(self.d2drho2).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ddt', i, numpy.array(self.ddt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'flag', i, numpy.array(self.flag).astype(numpy.int32))
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux_dv_surf', i, numpy.array(self.flux_dv_surf).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'time_deriv', i, numpy.array(self.time_deriv).astype(numpy.float64))
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistructurecorefieldionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		status, ret_ddrho = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ddrho', i)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		status, ret_d2drho2 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd2drho2', i)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		status, ret_ddt = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ddt', i)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv_surf = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv_surf', i)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		status, ret_time_deriv = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'time_deriv', i)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistructurecorefieldionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putNonTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistructurecorefieldionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getNonTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'ddrho')
		ull.deleteData(self.idx, path, cpopath + 'd2drho2')
		ull.deleteData(self.idx, path, cpopath + 'ddt')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		self.boundary.deleteData(path, cpopath)
		self.source_term.deleteData(path, cpopath)
		self.transp_coef.deleteData(path, cpopath)
		self.flux.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flux_dv_surf')
		ull.deleteData(self.idx, path, cpopath + 'time_deriv')
		self.codeparam.deleteData(path, cpopath)


class vtorstructurecorefieldion(KeepInOrder):
	'''
	class vtorstructurecorefieldion
	Toroidal velocity of the various ion species [m.s^-1]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- ddrho : numpy.ndarray 2D with float
	   Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Matrix (nrho,nion)
	- d2drho2 : numpy.ndarray 2D with float
	   Second order radial derivative (d2value/drho_tor^2) [signal_value_unit.m^-2]; Time-dependent; Matrix (nrho,nion)
	- ddt : numpy.ndarray 2D with float
	   Time derivative (dvalue/dtime) [signal_value_unit.s^-1]; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	- flag : numpy.ndarray 1D with int)
	   Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-calculated by a separate code : in that case only, description of the code provided in codeparam at the same level; 4-used value from the previous time step; Time-dependent; Vector(nion)
	- boundary : class boundarystructureboundaryion
	   Boundary condition for the transport equation
	- source_term : class source_termstructuresourceion
	   Total source term for the transport equation. Time-dependent.
	- transp_coef : class transp_coefstructurecoretransion
	   Total transport coefficients. Time-dependent.
	- flux : class fluxstructurefluxion
	   Fluxes of the quantity, two definitions. Time-dependent.
	- flux_dv_surf : numpy.ndarray 2D with float
	   Net flux through the magnetic surface, i.e. integral over the magnetic surface area of flux_dv. Time-dependent; Matrix(nrho,nion)
	- time_deriv : numpy.ndarray 2D with float
	   Integral of the time derivative term of the transport equation. Time-dependent. Matrix (nrho,nion)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='vtor'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.ddrho = numpy.zeros((0,0), numpy.float64, order='C')
		self.d2drho2 = numpy.zeros((0,0), numpy.float64, order='C')
		self.ddt = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']
		self.flag = numpy.zeros(0, numpy.int32, order='C')
		self.boundary = boundarystructureboundaryion('boundary')
		self.source_term = source_termstructuresourceion('source_term')
		self.transp_coef = transp_coefstructurecoretransion('transp_coef')
		self.flux = fluxstructurefluxion('flux')
		self.flux_dv_surf = numpy.zeros((0,0), numpy.float64, order='C')
		self.time_deriv = numpy.zeros((0,0), numpy.float64, order='C')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vtorstructurecorefieldion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddrho.__str__()
		ret = ret + space + 'Attribute ddrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.d2drho2.__str__()
		ret = ret + space + 'Attribute d2drho2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ddt.__str__()
		ret = ret + space + 'Attribute ddt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flag.__str__()
		ret = ret + space + 'Attribute flag\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute source_term\n ' + self.source_term.__str__(depth+1)
		ret = ret + space + 'Attribute transp_coef\n ' + self.transp_coef.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		s = self.flux_dv_surf.__str__()
		ret = ret + space + 'Attribute flux_dv_surf\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.time_deriv.__str__()
		ret = ret + space + 'Attribute time_deriv\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.source_term.setExpIdx(idx)
		self.transp_coef.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vtorstructurecorefieldion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'flag', numpy.array(self.flag).astype(numpy.int32), self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.source_term.cpoTime = self.cpoTime
		self.source_term.putSlice(path, cpopath)
		self.transp_coef.cpoTime = self.cpoTime
		self.transp_coef.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vtorstructurecorefieldion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', numpy.array(self.ddrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', numpy.array(self.d2drho2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', numpy.array(self.ddt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'flag', numpy.array(self.flag).astype(numpy.int32))
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.source_term.replaceLastSlice(path, cpopath)
		self.transp_coef.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', numpy.array(self.flux_dv_surf).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', numpy.array(self.time_deriv).astype(numpy.float64))
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vtorstructurecorefieldion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.source_term.putNonTimed(path, cpopath)
		self.transp_coef.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vtorstructurecorefieldion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_ddrho, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ddrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
			self.cpoTime = retTime
		status, ret_d2drho2, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'd2drho2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
			self.cpoTime = retTime
		status, ret_ddt, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ddt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_flag, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flag = ret_flag
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.source_term.getSlice(path, cpopath, inTime, interpolMode)
		self.transp_coef.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_flux_dv_surf, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flux_dv_surf', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
			self.cpoTime = retTime
		status, ret_time_deriv, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'time_deriv', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vtorstructurecorefieldion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, ddrhoList = ull.getVect3DDouble(self.idx, path, cpopath + 'ddrho')
			if len(ddrhoList) == 0:
				ddrhoList = numpy.resize(ddrhoList, (0,0,nbslice))
			check_status(status)
			status, d2drho2List = ull.getVect3DDouble(self.idx, path, cpopath + 'd2drho2')
			if len(d2drho2List) == 0:
				d2drho2List = numpy.resize(d2drho2List, (0,0,nbslice))
			check_status(status)
			status, ddtList = ull.getVect3DDouble(self.idx, path, cpopath + 'ddt')
			if len(ddtList) == 0:
				ddtList = numpy.resize(ddtList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, flagList = ull.getVect2DInt(self.idx, path, cpopath + 'flag')
			if len(flagList) == 0:
				flagList = numpy.resize(flagList, (0,nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			source_termList = self.source_term.build_non_resampled_data(path, cpopath, nbslice)
			transp_coefList = self.transp_coef.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			status, flux_dv_surfList = ull.getVect3DDouble(self.idx, path, cpopath + 'flux_dv_surf')
			if len(flux_dv_surfList) == 0:
				flux_dv_surfList = numpy.resize(flux_dv_surfList, (0,0,nbslice))
			check_status(status)
			status, time_derivList = ull.getVect3DDouble(self.idx, path, cpopath + 'time_deriv')
			if len(time_derivList) == 0:
				time_derivList = numpy.resize(time_derivList, (0,0,nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = vtorstructurecorefieldion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.ddrho = ddrhoList[:,:,i]
				slice.d2drho2 = d2drho2List[:,:,i]
				slice.ddt = ddtList[:,:,i]
				slice.source = sourceVal
				slice.flag = flagList[:,i]
				slice.boundary = boundaryList[i]
				slice.source_term = source_termList[i]
				slice.transp_coef = transp_coefList[i]
				slice.flux = fluxList[i]
				slice.flux_dv_surf = flux_dv_surfList[:,:,i]
				slice.time_deriv = time_derivList[:,:,i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vtorstructurecorefieldionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ddrho', i, numpy.array(self.ddrho).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'd2drho2', i, numpy.array(self.d2drho2).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ddt', i, numpy.array(self.ddt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'flag', i, numpy.array(self.flag).astype(numpy.int32))
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux_dv_surf', i, numpy.array(self.flux_dv_surf).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'time_deriv', i, numpy.array(self.time_deriv).astype(numpy.float64))
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vtorstructurecorefieldionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ddrho') 
			print ('obj = ' + str(obj))
		status, ret_ddrho = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ddrho', i)
		check_status(status)
		if not status:
			self.ddrho = ret_ddrho
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'd2drho2') 
			print ('obj = ' + str(obj))
		status, ret_d2drho2 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'd2drho2', i)
		check_status(status)
		if not status:
			self.d2drho2 = ret_d2drho2
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ddt') 
			print ('obj = ' + str(obj))
		status, ret_ddt = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ddt', i)
		check_status(status)
		if not status:
			self.ddt = ret_ddt
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux_dv_surf') 
			print ('obj = ' + str(obj))
		status, ret_flux_dv_surf = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux_dv_surf', i)
		check_status(status)
		if not status:
			self.flux_dv_surf = ret_flux_dv_surf
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'time_deriv') 
			print ('obj = ' + str(obj))
		status, ret_time_deriv = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'time_deriv', i)
		check_status(status)
		if not status:
			self.time_deriv = ret_time_deriv
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vtorstructurecorefieldionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.source_term.putNonTimedElt(path, cpopath + 'source_term', i, obj)
		obj = self.transp_coef.putNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vtorstructurecorefieldionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.source_term.getNonTimedElt(path, cpopath + 'source_term', i, obj)
		self.transp_coef.getNonTimedElt(path, cpopath + 'transp_coef', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'ddrho')
		ull.deleteData(self.idx, path, cpopath + 'd2drho2')
		ull.deleteData(self.idx, path, cpopath + 'ddt')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		self.boundary.deleteData(path, cpopath)
		self.source_term.deleteData(path, cpopath)
		self.transp_coef.deleteData(path, cpopath)
		self.flux.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'flux_dv_surf')
		ull.deleteData(self.idx, path, cpopath + 'time_deriv')
		self.codeparam.deleteData(path, cpopath)


class profiles1dstructureprofiles1d(KeepInOrder):
	'''
	class profiles1dstructureprofiles1d
	Profiles derived from the fields solved in the transport equations, or from experiment.

	Attributes:
	- pe : class pestructurecoreprofile
	   Electron pressure [Pa]; Time-dependent;
	- dpedt : class dpedtstructurecoreprofile
	   Time derivative of the electron pressure [Pa/s]; Time-dependent;
	- pi : class pistructurecoreprofion
	   Ion pressure [Pa]; Time-dependent;
	- pi_tot : class pi_totstructurecoreprofile
	   Total ion pressure (sum of the species) [Pa]; Time-dependent;
	- dpi_totdt : class dpi_totdtstructurecoreprofile
	   Time derivative of the total ion pressure [Pa/s]; Time-dependent;
	- pr_th : class pr_thstructurecoreprofile
	   Thermal pressure (electrons+ions) [Pa]; Time-dependent;
	- pr_perp : class pr_perpstructurecoreprofile
	   Total perpendicular pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;
	- pr_parallel : class pr_parallelstructurecoreprofile
	   Total parallel pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;
	- jtot : class jtotstructurecoreprofile
	   total parallel current density = average(jtot.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
	- jni : class jnistructurecoreprofile
	   non-inductive parallel current density = average(jni.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
	- jphi : class jphistructurecoreprofile
	   total toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent;
	- joh : class johstructurecoreprofile
	   ohmic parallel current density = average(joh.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
	- vloop : class vloopstructurecoreprofile
	   Toroidal loop voltage [V]. Time-dependent.
	- sigmapar : class sigmaparstructurecoreprofile
	   Parallel conductivity [ohm^-1.m^-1]. Time-dependent.
	- qoh : class qohstructuresourceel
	   ohmic heating [W/m^3]; Time-dependent;
	- qei : class qeistructurecoreprofile
	   Collisional heat transfer from electrons to ions (equipartition term) [W/m^3]; Time-dependent;
	- eparallel : class eparallelstructurecoreprofile
	   Parallel electric field = average(E.B) / B0, where B0 = coreprof/toroid_field/b0 [V.m^-1]. Time-dependent.
	- e_b : class e_bstructurecoreprofile
	   Average(E.B) [V.T.m^-1]. Time-dependent.
	- q : class qstructurecoreprofile
	   Safety factor profile; Time-dependent;
	- shear : class shearstructurecoreprofile
	   Magnetic shear profile; Time-dependent;
	- ns : class nsstructurecoreprofion
	   Density of fast ions, for the various ion species [m^-3]; Time-dependent;
	- mtor : class mtorstructurecoreprofion
	   Toroidal momentum of the various ion species [UNITS?]; Time-dependent;
	- wtor : class wtorstructurecoreprofion
	   Angular toroidal rotation frequency of the various ion species [s^-1]; Time-dependent;
	- vpol : class vpolstructurecoreprofion
	   Neoclassical poloidal rotation of each ion species [m/s]. Time-dependent.
	- zeff : class zeffstructurecoreprofile
	   Effective charge profile; Time-dependent;
	- bpol : class bpolstructurecoreprofile
	   Average poloidal magnetic field, defined as sqrt(ave(grad rho^2/R^2)).dpsi/drho [T]. Time-dependent.
	- dvprimedt : class dvprimedtstructurecoreprofile
	   Time derivative of the radial derivative of the volume enclosed in the flux surface, i.e. d/dt(dV/drho_tor) [m^2.s^-1]; Time-dependent.
	'''

	def __init__(self, base_path_in='profiles1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.pe = pestructurecoreprofile('pe')
		self.dpedt = dpedtstructurecoreprofile('dpedt')
		self.pi = pistructurecoreprofion('pi')
		self.pi_tot = pi_totstructurecoreprofile('pi_tot')
		self.dpi_totdt = dpi_totdtstructurecoreprofile('dpi_totdt')
		self.pr_th = pr_thstructurecoreprofile('pr_th')
		self.pr_perp = pr_perpstructurecoreprofile('pr_perp')
		self.pr_parallel = pr_parallelstructurecoreprofile('pr_parallel')
		self.jtot = jtotstructurecoreprofile('jtot')
		self.jni = jnistructurecoreprofile('jni')
		self.jphi = jphistructurecoreprofile('jphi')
		self.joh = johstructurecoreprofile('joh')
		self.vloop = vloopstructurecoreprofile('vloop')
		self.sigmapar = sigmaparstructurecoreprofile('sigmapar')
		self.qoh = qohstructuresourceel('qoh')
		self.qei = qeistructurecoreprofile('qei')
		self.eparallel = eparallelstructurecoreprofile('eparallel')
		self.e_b = e_bstructurecoreprofile('e_b')
		self.q = qstructurecoreprofile('q')
		self.shear = shearstructurecoreprofile('shear')
		self.ns = nsstructurecoreprofion('ns')
		self.mtor = mtorstructurecoreprofion('mtor')
		self.wtor = wtorstructurecoreprofion('wtor')
		self.vpol = vpolstructurecoreprofion('vpol')
		self.zeff = zeffstructurecoreprofile('zeff')
		self.bpol = bpolstructurecoreprofile('bpol')
		self.dvprimedt = dvprimedtstructurecoreprofile('dvprimedt')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles1dstructureprofiles1d\n'
		ret = ret + space + 'Attribute pe\n ' + self.pe.__str__(depth+1)
		ret = ret + space + 'Attribute dpedt\n ' + self.dpedt.__str__(depth+1)
		ret = ret + space + 'Attribute pi\n ' + self.pi.__str__(depth+1)
		ret = ret + space + 'Attribute pi_tot\n ' + self.pi_tot.__str__(depth+1)
		ret = ret + space + 'Attribute dpi_totdt\n ' + self.dpi_totdt.__str__(depth+1)
		ret = ret + space + 'Attribute pr_th\n ' + self.pr_th.__str__(depth+1)
		ret = ret + space + 'Attribute pr_perp\n ' + self.pr_perp.__str__(depth+1)
		ret = ret + space + 'Attribute pr_parallel\n ' + self.pr_parallel.__str__(depth+1)
		ret = ret + space + 'Attribute jtot\n ' + self.jtot.__str__(depth+1)
		ret = ret + space + 'Attribute jni\n ' + self.jni.__str__(depth+1)
		ret = ret + space + 'Attribute jphi\n ' + self.jphi.__str__(depth+1)
		ret = ret + space + 'Attribute joh\n ' + self.joh.__str__(depth+1)
		ret = ret + space + 'Attribute vloop\n ' + self.vloop.__str__(depth+1)
		ret = ret + space + 'Attribute sigmapar\n ' + self.sigmapar.__str__(depth+1)
		ret = ret + space + 'Attribute qoh\n ' + self.qoh.__str__(depth+1)
		ret = ret + space + 'Attribute qei\n ' + self.qei.__str__(depth+1)
		ret = ret + space + 'Attribute eparallel\n ' + self.eparallel.__str__(depth+1)
		ret = ret + space + 'Attribute e_b\n ' + self.e_b.__str__(depth+1)
		ret = ret + space + 'Attribute q\n ' + self.q.__str__(depth+1)
		ret = ret + space + 'Attribute shear\n ' + self.shear.__str__(depth+1)
		ret = ret + space + 'Attribute ns\n ' + self.ns.__str__(depth+1)
		ret = ret + space + 'Attribute mtor\n ' + self.mtor.__str__(depth+1)
		ret = ret + space + 'Attribute wtor\n ' + self.wtor.__str__(depth+1)
		ret = ret + space + 'Attribute vpol\n ' + self.vpol.__str__(depth+1)
		ret = ret + space + 'Attribute zeff\n ' + self.zeff.__str__(depth+1)
		ret = ret + space + 'Attribute bpol\n ' + self.bpol.__str__(depth+1)
		ret = ret + space + 'Attribute dvprimedt\n ' + self.dvprimedt.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.pe.setExpIdx(idx)
		self.dpedt.setExpIdx(idx)
		self.pi.setExpIdx(idx)
		self.pi_tot.setExpIdx(idx)
		self.dpi_totdt.setExpIdx(idx)
		self.pr_th.setExpIdx(idx)
		self.pr_perp.setExpIdx(idx)
		self.pr_parallel.setExpIdx(idx)
		self.jtot.setExpIdx(idx)
		self.jni.setExpIdx(idx)
		self.jphi.setExpIdx(idx)
		self.joh.setExpIdx(idx)
		self.vloop.setExpIdx(idx)
		self.sigmapar.setExpIdx(idx)
		self.qoh.setExpIdx(idx)
		self.qei.setExpIdx(idx)
		self.eparallel.setExpIdx(idx)
		self.e_b.setExpIdx(idx)
		self.q.setExpIdx(idx)
		self.shear.setExpIdx(idx)
		self.ns.setExpIdx(idx)
		self.mtor.setExpIdx(idx)
		self.wtor.setExpIdx(idx)
		self.vpol.setExpIdx(idx)
		self.zeff.setExpIdx(idx)
		self.bpol.setExpIdx(idx)
		self.dvprimedt.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles1dstructureprofiles1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pe.cpoTime = self.cpoTime
		self.pe.putSlice(path, cpopath)
		self.dpedt.cpoTime = self.cpoTime
		self.dpedt.putSlice(path, cpopath)
		self.pi.cpoTime = self.cpoTime
		self.pi.putSlice(path, cpopath)
		self.pi_tot.cpoTime = self.cpoTime
		self.pi_tot.putSlice(path, cpopath)
		self.dpi_totdt.cpoTime = self.cpoTime
		self.dpi_totdt.putSlice(path, cpopath)
		self.pr_th.cpoTime = self.cpoTime
		self.pr_th.putSlice(path, cpopath)
		self.pr_perp.cpoTime = self.cpoTime
		self.pr_perp.putSlice(path, cpopath)
		self.pr_parallel.cpoTime = self.cpoTime
		self.pr_parallel.putSlice(path, cpopath)
		self.jtot.cpoTime = self.cpoTime
		self.jtot.putSlice(path, cpopath)
		self.jni.cpoTime = self.cpoTime
		self.jni.putSlice(path, cpopath)
		self.jphi.cpoTime = self.cpoTime
		self.jphi.putSlice(path, cpopath)
		self.joh.cpoTime = self.cpoTime
		self.joh.putSlice(path, cpopath)
		self.vloop.cpoTime = self.cpoTime
		self.vloop.putSlice(path, cpopath)
		self.sigmapar.cpoTime = self.cpoTime
		self.sigmapar.putSlice(path, cpopath)
		self.qoh.cpoTime = self.cpoTime
		self.qoh.putSlice(path, cpopath)
		self.qei.cpoTime = self.cpoTime
		self.qei.putSlice(path, cpopath)
		self.eparallel.cpoTime = self.cpoTime
		self.eparallel.putSlice(path, cpopath)
		self.e_b.cpoTime = self.cpoTime
		self.e_b.putSlice(path, cpopath)
		self.q.cpoTime = self.cpoTime
		self.q.putSlice(path, cpopath)
		self.shear.cpoTime = self.cpoTime
		self.shear.putSlice(path, cpopath)
		self.ns.cpoTime = self.cpoTime
		self.ns.putSlice(path, cpopath)
		self.mtor.cpoTime = self.cpoTime
		self.mtor.putSlice(path, cpopath)
		self.wtor.cpoTime = self.cpoTime
		self.wtor.putSlice(path, cpopath)
		self.vpol.cpoTime = self.cpoTime
		self.vpol.putSlice(path, cpopath)
		self.zeff.cpoTime = self.cpoTime
		self.zeff.putSlice(path, cpopath)
		self.bpol.cpoTime = self.cpoTime
		self.bpol.putSlice(path, cpopath)
		self.dvprimedt.cpoTime = self.cpoTime
		self.dvprimedt.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles1dstructureprofiles1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pe.replaceLastSlice(path, cpopath)
		self.dpedt.replaceLastSlice(path, cpopath)
		self.pi.replaceLastSlice(path, cpopath)
		self.pi_tot.replaceLastSlice(path, cpopath)
		self.dpi_totdt.replaceLastSlice(path, cpopath)
		self.pr_th.replaceLastSlice(path, cpopath)
		self.pr_perp.replaceLastSlice(path, cpopath)
		self.pr_parallel.replaceLastSlice(path, cpopath)
		self.jtot.replaceLastSlice(path, cpopath)
		self.jni.replaceLastSlice(path, cpopath)
		self.jphi.replaceLastSlice(path, cpopath)
		self.joh.replaceLastSlice(path, cpopath)
		self.vloop.replaceLastSlice(path, cpopath)
		self.sigmapar.replaceLastSlice(path, cpopath)
		self.qoh.replaceLastSlice(path, cpopath)
		self.qei.replaceLastSlice(path, cpopath)
		self.eparallel.replaceLastSlice(path, cpopath)
		self.e_b.replaceLastSlice(path, cpopath)
		self.q.replaceLastSlice(path, cpopath)
		self.shear.replaceLastSlice(path, cpopath)
		self.ns.replaceLastSlice(path, cpopath)
		self.mtor.replaceLastSlice(path, cpopath)
		self.wtor.replaceLastSlice(path, cpopath)
		self.vpol.replaceLastSlice(path, cpopath)
		self.zeff.replaceLastSlice(path, cpopath)
		self.bpol.replaceLastSlice(path, cpopath)
		self.dvprimedt.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles1dstructureprofiles1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pe.putNonTimed(path, cpopath)
		self.dpedt.putNonTimed(path, cpopath)
		self.pi.putNonTimed(path, cpopath)
		self.pi_tot.putNonTimed(path, cpopath)
		self.dpi_totdt.putNonTimed(path, cpopath)
		self.pr_th.putNonTimed(path, cpopath)
		self.pr_perp.putNonTimed(path, cpopath)
		self.pr_parallel.putNonTimed(path, cpopath)
		self.jtot.putNonTimed(path, cpopath)
		self.jni.putNonTimed(path, cpopath)
		self.jphi.putNonTimed(path, cpopath)
		self.joh.putNonTimed(path, cpopath)
		self.vloop.putNonTimed(path, cpopath)
		self.sigmapar.putNonTimed(path, cpopath)
		self.qoh.putNonTimed(path, cpopath)
		self.qei.putNonTimed(path, cpopath)
		self.eparallel.putNonTimed(path, cpopath)
		self.e_b.putNonTimed(path, cpopath)
		self.q.putNonTimed(path, cpopath)
		self.shear.putNonTimed(path, cpopath)
		self.ns.putNonTimed(path, cpopath)
		self.mtor.putNonTimed(path, cpopath)
		self.wtor.putNonTimed(path, cpopath)
		self.vpol.putNonTimed(path, cpopath)
		self.zeff.putNonTimed(path, cpopath)
		self.bpol.putNonTimed(path, cpopath)
		self.dvprimedt.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles1dstructureprofiles1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pe.getSlice(path, cpopath, inTime, interpolMode)
		self.dpedt.getSlice(path, cpopath, inTime, interpolMode)
		self.pi.getSlice(path, cpopath, inTime, interpolMode)
		self.pi_tot.getSlice(path, cpopath, inTime, interpolMode)
		self.dpi_totdt.getSlice(path, cpopath, inTime, interpolMode)
		self.pr_th.getSlice(path, cpopath, inTime, interpolMode)
		self.pr_perp.getSlice(path, cpopath, inTime, interpolMode)
		self.pr_parallel.getSlice(path, cpopath, inTime, interpolMode)
		self.jtot.getSlice(path, cpopath, inTime, interpolMode)
		self.jni.getSlice(path, cpopath, inTime, interpolMode)
		self.jphi.getSlice(path, cpopath, inTime, interpolMode)
		self.joh.getSlice(path, cpopath, inTime, interpolMode)
		self.vloop.getSlice(path, cpopath, inTime, interpolMode)
		self.sigmapar.getSlice(path, cpopath, inTime, interpolMode)
		self.qoh.getSlice(path, cpopath, inTime, interpolMode)
		self.qei.getSlice(path, cpopath, inTime, interpolMode)
		self.eparallel.getSlice(path, cpopath, inTime, interpolMode)
		self.e_b.getSlice(path, cpopath, inTime, interpolMode)
		self.q.getSlice(path, cpopath, inTime, interpolMode)
		self.shear.getSlice(path, cpopath, inTime, interpolMode)
		self.ns.getSlice(path, cpopath, inTime, interpolMode)
		self.mtor.getSlice(path, cpopath, inTime, interpolMode)
		self.wtor.getSlice(path, cpopath, inTime, interpolMode)
		self.vpol.getSlice(path, cpopath, inTime, interpolMode)
		self.zeff.getSlice(path, cpopath, inTime, interpolMode)
		self.bpol.getSlice(path, cpopath, inTime, interpolMode)
		self.dvprimedt.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles1dstructureprofiles1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			peList = self.pe.build_non_resampled_data(path, cpopath, nbslice)
			dpedtList = self.dpedt.build_non_resampled_data(path, cpopath, nbslice)
			piList = self.pi.build_non_resampled_data(path, cpopath, nbslice)
			pi_totList = self.pi_tot.build_non_resampled_data(path, cpopath, nbslice)
			dpi_totdtList = self.dpi_totdt.build_non_resampled_data(path, cpopath, nbslice)
			pr_thList = self.pr_th.build_non_resampled_data(path, cpopath, nbslice)
			pr_perpList = self.pr_perp.build_non_resampled_data(path, cpopath, nbslice)
			pr_parallelList = self.pr_parallel.build_non_resampled_data(path, cpopath, nbslice)
			jtotList = self.jtot.build_non_resampled_data(path, cpopath, nbslice)
			jniList = self.jni.build_non_resampled_data(path, cpopath, nbslice)
			jphiList = self.jphi.build_non_resampled_data(path, cpopath, nbslice)
			johList = self.joh.build_non_resampled_data(path, cpopath, nbslice)
			vloopList = self.vloop.build_non_resampled_data(path, cpopath, nbslice)
			sigmaparList = self.sigmapar.build_non_resampled_data(path, cpopath, nbslice)
			qohList = self.qoh.build_non_resampled_data(path, cpopath, nbslice)
			qeiList = self.qei.build_non_resampled_data(path, cpopath, nbslice)
			eparallelList = self.eparallel.build_non_resampled_data(path, cpopath, nbslice)
			e_bList = self.e_b.build_non_resampled_data(path, cpopath, nbslice)
			qList = self.q.build_non_resampled_data(path, cpopath, nbslice)
			shearList = self.shear.build_non_resampled_data(path, cpopath, nbslice)
			nsList = self.ns.build_non_resampled_data(path, cpopath, nbslice)
			mtorList = self.mtor.build_non_resampled_data(path, cpopath, nbslice)
			wtorList = self.wtor.build_non_resampled_data(path, cpopath, nbslice)
			vpolList = self.vpol.build_non_resampled_data(path, cpopath, nbslice)
			zeffList = self.zeff.build_non_resampled_data(path, cpopath, nbslice)
			bpolList = self.bpol.build_non_resampled_data(path, cpopath, nbslice)
			dvprimedtList = self.dvprimedt.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = profiles1dstructureprofiles1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.pe = peList[i]
				slice.dpedt = dpedtList[i]
				slice.pi = piList[i]
				slice.pi_tot = pi_totList[i]
				slice.dpi_totdt = dpi_totdtList[i]
				slice.pr_th = pr_thList[i]
				slice.pr_perp = pr_perpList[i]
				slice.pr_parallel = pr_parallelList[i]
				slice.jtot = jtotList[i]
				slice.jni = jniList[i]
				slice.jphi = jphiList[i]
				slice.joh = johList[i]
				slice.vloop = vloopList[i]
				slice.sigmapar = sigmaparList[i]
				slice.qoh = qohList[i]
				slice.qei = qeiList[i]
				slice.eparallel = eparallelList[i]
				slice.e_b = e_bList[i]
				slice.q = qList[i]
				slice.shear = shearList[i]
				slice.ns = nsList[i]
				slice.mtor = mtorList[i]
				slice.wtor = wtorList[i]
				slice.vpol = vpolList[i]
				slice.zeff = zeffList[i]
				slice.bpol = bpolList[i]
				slice.dvprimedt = dvprimedtList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles1dstructureprofiles1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.pe.putTimedElt(path, cpopath + 'pe', i, obj)
		obj = self.dpedt.putTimedElt(path, cpopath + 'dpedt', i, obj)
		obj = self.pi.putTimedElt(path, cpopath + 'pi', i, obj)
		obj = self.pi_tot.putTimedElt(path, cpopath + 'pi_tot', i, obj)
		obj = self.dpi_totdt.putTimedElt(path, cpopath + 'dpi_totdt', i, obj)
		obj = self.pr_th.putTimedElt(path, cpopath + 'pr_th', i, obj)
		obj = self.pr_perp.putTimedElt(path, cpopath + 'pr_perp', i, obj)
		obj = self.pr_parallel.putTimedElt(path, cpopath + 'pr_parallel', i, obj)
		obj = self.jtot.putTimedElt(path, cpopath + 'jtot', i, obj)
		obj = self.jni.putTimedElt(path, cpopath + 'jni', i, obj)
		obj = self.jphi.putTimedElt(path, cpopath + 'jphi', i, obj)
		obj = self.joh.putTimedElt(path, cpopath + 'joh', i, obj)
		obj = self.vloop.putTimedElt(path, cpopath + 'vloop', i, obj)
		obj = self.sigmapar.putTimedElt(path, cpopath + 'sigmapar', i, obj)
		obj = self.qoh.putTimedElt(path, cpopath + 'qoh', i, obj)
		obj = self.qei.putTimedElt(path, cpopath + 'qei', i, obj)
		obj = self.eparallel.putTimedElt(path, cpopath + 'eparallel', i, obj)
		obj = self.e_b.putTimedElt(path, cpopath + 'e_b', i, obj)
		obj = self.q.putTimedElt(path, cpopath + 'q', i, obj)
		obj = self.shear.putTimedElt(path, cpopath + 'shear', i, obj)
		obj = self.ns.putTimedElt(path, cpopath + 'ns', i, obj)
		obj = self.mtor.putTimedElt(path, cpopath + 'mtor', i, obj)
		obj = self.wtor.putTimedElt(path, cpopath + 'wtor', i, obj)
		obj = self.vpol.putTimedElt(path, cpopath + 'vpol', i, obj)
		obj = self.zeff.putTimedElt(path, cpopath + 'zeff', i, obj)
		obj = self.bpol.putTimedElt(path, cpopath + 'bpol', i, obj)
		obj = self.dvprimedt.putTimedElt(path, cpopath + 'dvprimedt', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles1dstructureprofiles1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.pe.getTimedElt(path, cpopath + 'pe', i, obj)
		self.dpedt.getTimedElt(path, cpopath + 'dpedt', i, obj)
		self.pi.getTimedElt(path, cpopath + 'pi', i, obj)
		self.pi_tot.getTimedElt(path, cpopath + 'pi_tot', i, obj)
		self.dpi_totdt.getTimedElt(path, cpopath + 'dpi_totdt', i, obj)
		self.pr_th.getTimedElt(path, cpopath + 'pr_th', i, obj)
		self.pr_perp.getTimedElt(path, cpopath + 'pr_perp', i, obj)
		self.pr_parallel.getTimedElt(path, cpopath + 'pr_parallel', i, obj)
		self.jtot.getTimedElt(path, cpopath + 'jtot', i, obj)
		self.jni.getTimedElt(path, cpopath + 'jni', i, obj)
		self.jphi.getTimedElt(path, cpopath + 'jphi', i, obj)
		self.joh.getTimedElt(path, cpopath + 'joh', i, obj)
		self.vloop.getTimedElt(path, cpopath + 'vloop', i, obj)
		self.sigmapar.getTimedElt(path, cpopath + 'sigmapar', i, obj)
		self.qoh.getTimedElt(path, cpopath + 'qoh', i, obj)
		self.qei.getTimedElt(path, cpopath + 'qei', i, obj)
		self.eparallel.getTimedElt(path, cpopath + 'eparallel', i, obj)
		self.e_b.getTimedElt(path, cpopath + 'e_b', i, obj)
		self.q.getTimedElt(path, cpopath + 'q', i, obj)
		self.shear.getTimedElt(path, cpopath + 'shear', i, obj)
		self.ns.getTimedElt(path, cpopath + 'ns', i, obj)
		self.mtor.getTimedElt(path, cpopath + 'mtor', i, obj)
		self.wtor.getTimedElt(path, cpopath + 'wtor', i, obj)
		self.vpol.getTimedElt(path, cpopath + 'vpol', i, obj)
		self.zeff.getTimedElt(path, cpopath + 'zeff', i, obj)
		self.bpol.getTimedElt(path, cpopath + 'bpol', i, obj)
		self.dvprimedt.getTimedElt(path, cpopath + 'dvprimedt', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles1dstructureprofiles1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.pe.putNonTimedElt(path, cpopath + 'pe', i, obj)
		obj = self.dpedt.putNonTimedElt(path, cpopath + 'dpedt', i, obj)
		obj = self.pi.putNonTimedElt(path, cpopath + 'pi', i, obj)
		obj = self.pi_tot.putNonTimedElt(path, cpopath + 'pi_tot', i, obj)
		obj = self.dpi_totdt.putNonTimedElt(path, cpopath + 'dpi_totdt', i, obj)
		obj = self.pr_th.putNonTimedElt(path, cpopath + 'pr_th', i, obj)
		obj = self.pr_perp.putNonTimedElt(path, cpopath + 'pr_perp', i, obj)
		obj = self.pr_parallel.putNonTimedElt(path, cpopath + 'pr_parallel', i, obj)
		obj = self.jtot.putNonTimedElt(path, cpopath + 'jtot', i, obj)
		obj = self.jni.putNonTimedElt(path, cpopath + 'jni', i, obj)
		obj = self.jphi.putNonTimedElt(path, cpopath + 'jphi', i, obj)
		obj = self.joh.putNonTimedElt(path, cpopath + 'joh', i, obj)
		obj = self.vloop.putNonTimedElt(path, cpopath + 'vloop', i, obj)
		obj = self.sigmapar.putNonTimedElt(path, cpopath + 'sigmapar', i, obj)
		obj = self.qoh.putNonTimedElt(path, cpopath + 'qoh', i, obj)
		obj = self.qei.putNonTimedElt(path, cpopath + 'qei', i, obj)
		obj = self.eparallel.putNonTimedElt(path, cpopath + 'eparallel', i, obj)
		obj = self.e_b.putNonTimedElt(path, cpopath + 'e_b', i, obj)
		obj = self.q.putNonTimedElt(path, cpopath + 'q', i, obj)
		obj = self.shear.putNonTimedElt(path, cpopath + 'shear', i, obj)
		obj = self.ns.putNonTimedElt(path, cpopath + 'ns', i, obj)
		obj = self.mtor.putNonTimedElt(path, cpopath + 'mtor', i, obj)
		obj = self.wtor.putNonTimedElt(path, cpopath + 'wtor', i, obj)
		obj = self.vpol.putNonTimedElt(path, cpopath + 'vpol', i, obj)
		obj = self.zeff.putNonTimedElt(path, cpopath + 'zeff', i, obj)
		obj = self.bpol.putNonTimedElt(path, cpopath + 'bpol', i, obj)
		obj = self.dvprimedt.putNonTimedElt(path, cpopath + 'dvprimedt', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles1dstructureprofiles1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.pe.getNonTimedElt(path, cpopath + 'pe', i, obj)
		self.dpedt.getNonTimedElt(path, cpopath + 'dpedt', i, obj)
		self.pi.getNonTimedElt(path, cpopath + 'pi', i, obj)
		self.pi_tot.getNonTimedElt(path, cpopath + 'pi_tot', i, obj)
		self.dpi_totdt.getNonTimedElt(path, cpopath + 'dpi_totdt', i, obj)
		self.pr_th.getNonTimedElt(path, cpopath + 'pr_th', i, obj)
		self.pr_perp.getNonTimedElt(path, cpopath + 'pr_perp', i, obj)
		self.pr_parallel.getNonTimedElt(path, cpopath + 'pr_parallel', i, obj)
		self.jtot.getNonTimedElt(path, cpopath + 'jtot', i, obj)
		self.jni.getNonTimedElt(path, cpopath + 'jni', i, obj)
		self.jphi.getNonTimedElt(path, cpopath + 'jphi', i, obj)
		self.joh.getNonTimedElt(path, cpopath + 'joh', i, obj)
		self.vloop.getNonTimedElt(path, cpopath + 'vloop', i, obj)
		self.sigmapar.getNonTimedElt(path, cpopath + 'sigmapar', i, obj)
		self.qoh.getNonTimedElt(path, cpopath + 'qoh', i, obj)
		self.qei.getNonTimedElt(path, cpopath + 'qei', i, obj)
		self.eparallel.getNonTimedElt(path, cpopath + 'eparallel', i, obj)
		self.e_b.getNonTimedElt(path, cpopath + 'e_b', i, obj)
		self.q.getNonTimedElt(path, cpopath + 'q', i, obj)
		self.shear.getNonTimedElt(path, cpopath + 'shear', i, obj)
		self.ns.getNonTimedElt(path, cpopath + 'ns', i, obj)
		self.mtor.getNonTimedElt(path, cpopath + 'mtor', i, obj)
		self.wtor.getNonTimedElt(path, cpopath + 'wtor', i, obj)
		self.vpol.getNonTimedElt(path, cpopath + 'vpol', i, obj)
		self.zeff.getNonTimedElt(path, cpopath + 'zeff', i, obj)
		self.bpol.getNonTimedElt(path, cpopath + 'bpol', i, obj)
		self.dvprimedt.getNonTimedElt(path, cpopath + 'dvprimedt', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pe.deleteData(path, cpopath)
		self.dpedt.deleteData(path, cpopath)
		self.pi.deleteData(path, cpopath)
		self.pi_tot.deleteData(path, cpopath)
		self.dpi_totdt.deleteData(path, cpopath)
		self.pr_th.deleteData(path, cpopath)
		self.pr_perp.deleteData(path, cpopath)
		self.pr_parallel.deleteData(path, cpopath)
		self.jtot.deleteData(path, cpopath)
		self.jni.deleteData(path, cpopath)
		self.jphi.deleteData(path, cpopath)
		self.joh.deleteData(path, cpopath)
		self.vloop.deleteData(path, cpopath)
		self.sigmapar.deleteData(path, cpopath)
		self.qoh.deleteData(path, cpopath)
		self.qei.deleteData(path, cpopath)
		self.eparallel.deleteData(path, cpopath)
		self.e_b.deleteData(path, cpopath)
		self.q.deleteData(path, cpopath)
		self.shear.deleteData(path, cpopath)
		self.ns.deleteData(path, cpopath)
		self.mtor.deleteData(path, cpopath)
		self.wtor.deleteData(path, cpopath)
		self.vpol.deleteData(path, cpopath)
		self.zeff.deleteData(path, cpopath)
		self.bpol.deleteData(path, cpopath)
		self.dvprimedt.deleteData(path, cpopath)


class pestructurecoreprofile(KeepInOrder):
	'''
	class pestructurecoreprofile
	Electron pressure [Pa]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='pe'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pestructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pestructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pestructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pestructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pestructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pestructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = pestructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pestructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pestructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pestructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pestructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class dpedtstructurecoreprofile(KeepInOrder):
	'''
	class dpedtstructurecoreprofile
	Time derivative of the electron pressure [Pa/s]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='dpedt'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class dpedtstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dpedtstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dpedtstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dpedtstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type dpedtstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type dpedtstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = dpedtstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpedtstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpedtstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpedtstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpedtstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class pistructurecoreprofion(KeepInOrder):
	'''
	class pistructurecoreprofion
	Ion pressure [Pa]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='pi'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pistructurecoreprofion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pistructurecoreprofion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pistructurecoreprofion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pistructurecoreprofion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pistructurecoreprofion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pistructurecoreprofion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = pistructurecoreprofion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pistructurecoreprofionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pistructurecoreprofionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pistructurecoreprofionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pistructurecoreprofionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class pi_totstructurecoreprofile(KeepInOrder):
	'''
	class pi_totstructurecoreprofile
	Total ion pressure (sum of the species) [Pa]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='pi_tot'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pi_totstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pi_totstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pi_totstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pi_totstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pi_totstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pi_totstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = pi_totstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pi_totstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pi_totstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pi_totstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pi_totstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class dpi_totdtstructurecoreprofile(KeepInOrder):
	'''
	class dpi_totdtstructurecoreprofile
	Time derivative of the total ion pressure [Pa/s]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='dpi_totdt'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class dpi_totdtstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dpi_totdtstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dpi_totdtstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dpi_totdtstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type dpi_totdtstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type dpi_totdtstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = dpi_totdtstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpi_totdtstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpi_totdtstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpi_totdtstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dpi_totdtstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class pr_thstructurecoreprofile(KeepInOrder):
	'''
	class pr_thstructurecoreprofile
	Thermal pressure (electrons+ions) [Pa]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='pr_th'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pr_thstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_thstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_thstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_thstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pr_thstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pr_thstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = pr_thstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_thstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_thstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_thstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_thstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class pr_perpstructurecoreprofile(KeepInOrder):
	'''
	class pr_perpstructurecoreprofile
	Total perpendicular pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='pr_perp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pr_perpstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_perpstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_perpstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_perpstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pr_perpstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pr_perpstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = pr_perpstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_perpstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_perpstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_perpstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_perpstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class pr_parallelstructurecoreprofile(KeepInOrder):
	'''
	class pr_parallelstructurecoreprofile
	Total parallel pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='pr_parallel'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pr_parallelstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_parallelstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_parallelstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pr_parallelstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pr_parallelstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pr_parallelstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = pr_parallelstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_parallelstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_parallelstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_parallelstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pr_parallelstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class jtotstructurecoreprofile(KeepInOrder):
	'''
	class jtotstructurecoreprofile
	total parallel current density = average(jtot.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='jtot'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jtotstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jtotstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jtotstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jtotstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jtotstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jtotstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = jtotstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jtotstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jtotstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jtotstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jtotstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class jnistructurecoreprofile(KeepInOrder):
	'''
	class jnistructurecoreprofile
	non-inductive parallel current density = average(jni.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='jni'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jnistructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jnistructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = jnistructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jnistructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class jphistructurecoreprofile(KeepInOrder):
	'''
	class jphistructurecoreprofile
	total toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='jphi'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jphistructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jphistructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jphistructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jphistructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jphistructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jphistructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = jphistructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jphistructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jphistructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jphistructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jphistructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class johstructurecoreprofile(KeepInOrder):
	'''
	class johstructurecoreprofile
	ohmic parallel current density = average(joh.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='joh'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class johstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type johstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type johstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type johstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type johstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type johstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = johstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type johstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type johstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type johstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type johstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class vloopstructurecoreprofile(KeepInOrder):
	'''
	class vloopstructurecoreprofile
	Toroidal loop voltage [V]. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='vloop'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vloopstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vloopstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vloopstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vloopstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vloopstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vloopstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = vloopstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vloopstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vloopstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vloopstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vloopstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class sigmaparstructurecoreprofile(KeepInOrder):
	'''
	class sigmaparstructurecoreprofile
	Parallel conductivity [ohm^-1.m^-1]. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='sigmapar'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class sigmaparstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sigmaparstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sigmaparstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sigmaparstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type sigmaparstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type sigmaparstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = sigmaparstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigmaparstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigmaparstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigmaparstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sigmaparstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class qohstructuresourceel(KeepInOrder):
	'''
	class qohstructuresourceel
	ohmic heating [W/m^3]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Value of the source term; Time-dependent; Vector (nrho)
	- integral : numpy.ndarray 1D with float
	   Integral from 0 to rho of the source term. Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='qoh'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.integral = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class qohstructuresourceel\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.integral.__str__()
		ret = ret + space + 'Attribute integral\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qohstructuresourceel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qohstructuresourceel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'integral', numpy.array(self.integral).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qohstructuresourceel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type qohstructuresourceel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_integral, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'integral', inTime, interpolMode)
		check_status(status)
		if not status:
			self.integral = ret_integral
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type qohstructuresourceel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, integralList = ull.getVect2DDouble(self.idx, path, cpopath + 'integral')
			if len(integralList) == 0:
				integralList = numpy.resize(integralList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = qohstructuresourceel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.integral = integralList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qohstructuresourceelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'integral', i, numpy.array(self.integral).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qohstructuresourceelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'integral') 
			print ('obj = ' + str(obj))
		status, ret_integral = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'integral', i)
		check_status(status)
		if not status:
			self.integral = ret_integral

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qohstructuresourceelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qohstructuresourceelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'integral')
		ull.deleteData(self.idx, path, cpopath + 'source')


class qeistructurecoreprofile(KeepInOrder):
	'''
	class qeistructurecoreprofile
	Collisional heat transfer from electrons to ions (equipartition term) [W/m^3]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='qei'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class qeistructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qeistructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qeistructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qeistructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type qeistructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type qeistructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = qeistructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qeistructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qeistructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qeistructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qeistructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class eparallelstructurecoreprofile(KeepInOrder):
	'''
	class eparallelstructurecoreprofile
	Parallel electric field = average(E.B) / B0, where B0 = coreprof/toroid_field/b0 [V.m^-1]. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='eparallel'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class eparallelstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eparallelstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eparallelstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eparallelstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type eparallelstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type eparallelstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = eparallelstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eparallelstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eparallelstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eparallelstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eparallelstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class e_bstructurecoreprofile(KeepInOrder):
	'''
	class e_bstructurecoreprofile
	Average(E.B) [V.T.m^-1]. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='e_b'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_bstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_bstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_bstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_bstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_bstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_bstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = e_bstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_bstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_bstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_bstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_bstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class qstructurecoreprofile(KeepInOrder):
	'''
	class qstructurecoreprofile
	Safety factor profile; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='q'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class qstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type qstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type qstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = qstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class shearstructurecoreprofile(KeepInOrder):
	'''
	class shearstructurecoreprofile
	Magnetic shear profile; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='shear'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class shearstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type shearstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type shearstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type shearstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type shearstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type shearstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = shearstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shearstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shearstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shearstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shearstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class nsstructurecoreprofion(KeepInOrder):
	'''
	class nsstructurecoreprofion
	Density of fast ions, for the various ion species [m^-3]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='ns'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nsstructurecoreprofion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nsstructurecoreprofion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nsstructurecoreprofion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nsstructurecoreprofion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nsstructurecoreprofion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nsstructurecoreprofion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = nsstructurecoreprofion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nsstructurecoreprofionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nsstructurecoreprofionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nsstructurecoreprofionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nsstructurecoreprofionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class mtorstructurecoreprofion(KeepInOrder):
	'''
	class mtorstructurecoreprofion
	Toroidal momentum of the various ion species [UNITS?]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='mtor'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mtorstructurecoreprofion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mtorstructurecoreprofion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mtorstructurecoreprofion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mtorstructurecoreprofion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mtorstructurecoreprofion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mtorstructurecoreprofion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = mtorstructurecoreprofion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtorstructurecoreprofionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtorstructurecoreprofionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtorstructurecoreprofionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mtorstructurecoreprofionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class wtorstructurecoreprofion(KeepInOrder):
	'''
	class wtorstructurecoreprofion
	Angular toroidal rotation frequency of the various ion species [s^-1]; Time-dependent;

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='wtor'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wtorstructurecoreprofion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wtorstructurecoreprofion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wtorstructurecoreprofion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wtorstructurecoreprofion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type wtorstructurecoreprofion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type wtorstructurecoreprofion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = wtorstructurecoreprofion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wtorstructurecoreprofionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wtorstructurecoreprofionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wtorstructurecoreprofionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wtorstructurecoreprofionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class vpolstructurecoreprofion(KeepInOrder):
	'''
	class vpolstructurecoreprofion
	Neoclassical poloidal rotation of each ion species [m/s]. Time-dependent.

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix (nrho,nion)
	- source : list of str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array of strings (nion)
	'''

	def __init__(self, base_path_in='vpol'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.source = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vpolstructurecoreprofion\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.source.__str__()
		ret = ret + space + 'Attribute source\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vpolstructurecoreprofion, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vpolstructurecoreprofion, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vpolstructurecoreprofion, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'source', self.source, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vpolstructurecoreprofion, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getVect1DString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vpolstructurecoreprofion, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, sourceVal = ull.getVect1DString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = vpolstructurecoreprofion(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vpolstructurecoreprofionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vpolstructurecoreprofionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vpolstructurecoreprofionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vpolstructurecoreprofionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class zeffstructurecoreprofile(KeepInOrder):
	'''
	class zeffstructurecoreprofile
	Effective charge profile; Time-dependent;

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='zeff'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class zeffstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type zeffstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type zeffstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type zeffstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type zeffstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type zeffstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = zeffstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type zeffstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type zeffstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type zeffstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type zeffstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class bpolstructurecoreprofile(KeepInOrder):
	'''
	class bpolstructurecoreprofile
	Average poloidal magnetic field, defined as sqrt(ave(grad rho^2/R^2)).dpsi/drho [T]. Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='bpol'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bpolstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = bpolstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class dvprimedtstructurecoreprofile(KeepInOrder):
	'''
	class dvprimedtstructurecoreprofile
	Time derivative of the radial derivative of the volume enclosed in the flux surface, i.e. d/dt(dV/drho_tor) [m^2.s^-1]; Time-dependent.

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector (nrho)
	- source : str
	   Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); String
	'''

	def __init__(self, base_path_in='dvprimedt'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class dvprimedtstructurecoreprofile\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dvprimedtstructurecoreprofile, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dvprimedtstructurecoreprofile, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dvprimedtstructurecoreprofile, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type dvprimedtstructurecoreprofile, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type dvprimedtstructurecoreprofile, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			for i in range(nbslice):
				slice = dvprimedtstructurecoreprofile(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.source = sourceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dvprimedtstructurecoreprofileObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dvprimedtstructurecoreprofileObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dvprimedtstructurecoreprofileObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dvprimedtstructurecoreprofileObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'source')


class globalparamstructureglobalparam(KeepInOrder):
	'''
	class globalparamstructureglobalparam
	Various global quantities calculated from the 1D profiles. Time-dependent

	Attributes:
	- current_tot : float
	   Total plasma current [A]; Time-dependent; Scalar
	- current_bnd : float
	   Plasma current inside transport solver boundary rho_tor_bnd [A]; Time-dependent; Scalar
	- current_ni : float
	   Total non-inductive parallel current [A]; Time-dependent; Scalar
	- vloop : float
	   Toroidal loop voltage [V]; Time-dependent; Scalar
	- li : float
	   Internal inductance; Time-dependent; Scalar
	- beta_tor : float
	   toroidal beta; Time-dependent; Scalar
	- beta_normal : float
	   normalised beta; Time-dependent; Scalar
	- beta_pol : float
	   poloidal beta; Time-dependent; Scalar
	- w_dia : float
	   Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (pr_th + pr_perp). Time-dependent; Scalar
	- geom_axis : class geom_axisstructurerz0D
	   RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 and (Zmin+Zmax) / 2 of the boundary) [m]; Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='globalparam'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.current_tot = EMPTY_DOUBLE
		self.current_bnd = EMPTY_DOUBLE
		self.current_ni = EMPTY_DOUBLE
		self.vloop = EMPTY_DOUBLE
		self.li = EMPTY_DOUBLE
		self.beta_tor = EMPTY_DOUBLE
		self.beta_normal = EMPTY_DOUBLE
		self.beta_pol = EMPTY_DOUBLE
		self.w_dia = EMPTY_DOUBLE
		self.geom_axis = geom_axisstructurerz0D('geom_axis')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class globalparamstructureglobalparam\n'
		ret = ret + space + 'Attribute current_tot: ' + str(self.current_tot) + '\n'
		ret = ret + space + 'Attribute current_bnd: ' + str(self.current_bnd) + '\n'
		ret = ret + space + 'Attribute current_ni: ' + str(self.current_ni) + '\n'
		ret = ret + space + 'Attribute vloop: ' + str(self.vloop) + '\n'
		ret = ret + space + 'Attribute li: ' + str(self.li) + '\n'
		ret = ret + space + 'Attribute beta_tor: ' + str(self.beta_tor) + '\n'
		ret = ret + space + 'Attribute beta_normal: ' + str(self.beta_normal) + '\n'
		ret = ret + space + 'Attribute beta_pol: ' + str(self.beta_pol) + '\n'
		ret = ret + space + 'Attribute w_dia: ' + str(self.w_dia) + '\n'
		ret = ret + space + 'Attribute geom_axis\n ' + self.geom_axis.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.geom_axis.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type globalparamstructureglobalparam, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'current_tot', self.current_tot, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'current_bnd', self.current_bnd, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'current_ni', self.current_ni, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'vloop', self.vloop, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'li', self.li, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta_tor', self.beta_tor, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta_normal', self.beta_normal, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta_pol', self.beta_pol, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'w_dia', self.w_dia, self.cpoTime)
		check_status(status)
		self.geom_axis.cpoTime = self.cpoTime
		self.geom_axis.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type globalparamstructureglobalparam, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'current_tot', self.current_tot)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'current_bnd', self.current_bnd)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'current_ni', self.current_ni)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'vloop', self.vloop)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'li', self.li)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta_tor', self.beta_tor)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta_normal', self.beta_normal)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta_pol', self.beta_pol)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'w_dia', self.w_dia)
		check_status(status)
		self.geom_axis.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type globalparamstructureglobalparam, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geom_axis.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type globalparamstructureglobalparam, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_current_tot, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'current_tot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current_tot = ret_current_tot
			self.cpoTime = retTime
		status, ret_current_bnd, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'current_bnd', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current_bnd = ret_current_bnd
			self.cpoTime = retTime
		status, ret_current_ni, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'current_ni', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current_ni = ret_current_ni
			self.cpoTime = retTime
		status, ret_vloop, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'vloop', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vloop = ret_vloop
			self.cpoTime = retTime
		status, ret_li, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'li', inTime, interpolMode)
		check_status(status)
		if not status:
			self.li = ret_li
			self.cpoTime = retTime
		status, ret_beta_tor, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_tor = ret_beta_tor
			self.cpoTime = retTime
		status, ret_beta_normal, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta_normal', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_normal = ret_beta_normal
			self.cpoTime = retTime
		status, ret_beta_pol, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta_pol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_pol = ret_beta_pol
			self.cpoTime = retTime
		status, ret_w_dia, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'w_dia', inTime, interpolMode)
		check_status(status)
		if not status:
			self.w_dia = ret_w_dia
			self.cpoTime = retTime
		self.geom_axis.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type globalparamstructureglobalparam, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, current_totList = ull.getVect1DDouble(self.idx, path, cpopath + 'current_tot')
			if len(current_totList) == 0:
				current_totList = numpy.resize(current_totList, (nbslice))
			check_status(status)
			status, current_bndList = ull.getVect1DDouble(self.idx, path, cpopath + 'current_bnd')
			if len(current_bndList) == 0:
				current_bndList = numpy.resize(current_bndList, (nbslice))
			check_status(status)
			status, current_niList = ull.getVect1DDouble(self.idx, path, cpopath + 'current_ni')
			if len(current_niList) == 0:
				current_niList = numpy.resize(current_niList, (nbslice))
			check_status(status)
			status, vloopList = ull.getVect1DDouble(self.idx, path, cpopath + 'vloop')
			if len(vloopList) == 0:
				vloopList = numpy.resize(vloopList, (nbslice))
			check_status(status)
			status, liList = ull.getVect1DDouble(self.idx, path, cpopath + 'li')
			if len(liList) == 0:
				liList = numpy.resize(liList, (nbslice))
			check_status(status)
			status, beta_torList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta_tor')
			if len(beta_torList) == 0:
				beta_torList = numpy.resize(beta_torList, (nbslice))
			check_status(status)
			status, beta_normalList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta_normal')
			if len(beta_normalList) == 0:
				beta_normalList = numpy.resize(beta_normalList, (nbslice))
			check_status(status)
			status, beta_polList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta_pol')
			if len(beta_polList) == 0:
				beta_polList = numpy.resize(beta_polList, (nbslice))
			check_status(status)
			status, w_diaList = ull.getVect1DDouble(self.idx, path, cpopath + 'w_dia')
			if len(w_diaList) == 0:
				w_diaList = numpy.resize(w_diaList, (nbslice))
			check_status(status)
			geom_axisList = self.geom_axis.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = globalparamstructureglobalparam(self.base_path)
				slice.setExpIdx(self.idx)
				slice.current_tot = current_totList[i].copy().astype(float)
				slice.current_bnd = current_bndList[i].copy().astype(float)
				slice.current_ni = current_niList[i].copy().astype(float)
				slice.vloop = vloopList[i].copy().astype(float)
				slice.li = liList[i].copy().astype(float)
				slice.beta_tor = beta_torList[i].copy().astype(float)
				slice.beta_normal = beta_normalList[i].copy().astype(float)
				slice.beta_pol = beta_polList[i].copy().astype(float)
				slice.w_dia = w_diaList[i].copy().astype(float)
				slice.geom_axis = geom_axisList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type globalparamstructureglobalparamObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'current_tot') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'current_tot', i, self.current_tot)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'current_bnd') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'current_bnd', i, self.current_bnd)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'current_ni') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'current_ni', i, self.current_ni)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'vloop') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'vloop', i, self.vloop)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'li') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'li', i, self.li)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta_tor', i, self.beta_tor)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta_normal') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta_normal', i, self.beta_normal)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta_pol') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta_pol', i, self.beta_pol)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'w_dia') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'w_dia', i, self.w_dia)
		obj = self.geom_axis.putTimedElt(path, cpopath + 'geom_axis', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type globalparamstructureglobalparamObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'current_tot') 
			print ('obj = ' + str(obj))
		status, ret_current_tot = ull.getDoubleFromObject(self.idx, obj, cpopath + 'current_tot', i)
		check_status(status)
		if not status:
			self.current_tot = ret_current_tot
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'current_bnd') 
			print ('obj = ' + str(obj))
		status, ret_current_bnd = ull.getDoubleFromObject(self.idx, obj, cpopath + 'current_bnd', i)
		check_status(status)
		if not status:
			self.current_bnd = ret_current_bnd
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'current_ni') 
			print ('obj = ' + str(obj))
		status, ret_current_ni = ull.getDoubleFromObject(self.idx, obj, cpopath + 'current_ni', i)
		check_status(status)
		if not status:
			self.current_ni = ret_current_ni
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'vloop') 
			print ('obj = ' + str(obj))
		status, ret_vloop = ull.getDoubleFromObject(self.idx, obj, cpopath + 'vloop', i)
		check_status(status)
		if not status:
			self.vloop = ret_vloop
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'li') 
			print ('obj = ' + str(obj))
		status, ret_li = ull.getDoubleFromObject(self.idx, obj, cpopath + 'li', i)
		check_status(status)
		if not status:
			self.li = ret_li
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta_tor') 
			print ('obj = ' + str(obj))
		status, ret_beta_tor = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta_tor', i)
		check_status(status)
		if not status:
			self.beta_tor = ret_beta_tor
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta_normal') 
			print ('obj = ' + str(obj))
		status, ret_beta_normal = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta_normal', i)
		check_status(status)
		if not status:
			self.beta_normal = ret_beta_normal
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta_pol') 
			print ('obj = ' + str(obj))
		status, ret_beta_pol = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta_pol', i)
		check_status(status)
		if not status:
			self.beta_pol = ret_beta_pol
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'w_dia') 
			print ('obj = ' + str(obj))
		status, ret_w_dia = ull.getDoubleFromObject(self.idx, obj, cpopath + 'w_dia', i)
		check_status(status)
		if not status:
			self.w_dia = ret_w_dia
		self.geom_axis.getTimedElt(path, cpopath + 'geom_axis', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type globalparamstructureglobalparamObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geom_axis.putNonTimedElt(path, cpopath + 'geom_axis', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type globalparamstructureglobalparamObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.geom_axis.getNonTimedElt(path, cpopath + 'geom_axis', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'current_tot')
		ull.deleteData(self.idx, path, cpopath + 'current_bnd')
		ull.deleteData(self.idx, path, cpopath + 'current_ni')
		ull.deleteData(self.idx, path, cpopath + 'vloop')
		ull.deleteData(self.idx, path, cpopath + 'li')
		ull.deleteData(self.idx, path, cpopath + 'beta_tor')
		ull.deleteData(self.idx, path, cpopath + 'beta_normal')
		ull.deleteData(self.idx, path, cpopath + 'beta_pol')
		ull.deleteData(self.idx, path, cpopath + 'w_dia')
		self.geom_axis.deleteData(path, cpopath)


class geom_axisstructurerz0D(KeepInOrder):
	'''
	class geom_axisstructurerz0D
	RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 and (Zmin+Zmax) / 2 of the boundary) [m]; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='geom_axis'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geom_axisstructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'r', self.r, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'z', self.z, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'r', self.r)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect1DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (nbslice))
			check_status(status)
			status, zList = ull.getVect1DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = geom_axisstructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geom_axisstructurerz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r', i, self.r)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geom_axisstructurerz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geom_axisstructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geom_axisstructurerz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')
