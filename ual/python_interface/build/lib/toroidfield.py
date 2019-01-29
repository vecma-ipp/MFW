# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class toroidfield:
	'''
	class toroidfield
	Toroidal field. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- desc_tfcoils : class desc_tfcoilsstructuretf_desc_tfcoils
	   Description of the toroidal field coils
	- nturns : int
	   Number of total turns in the toroidal field coil
	- ncoils : int
	   Number of packets of coils
	- current : class currentstructureexp0D
	   Current in the toroidal field coils [A]; Time-dependent. Scalar.
	- bvac_r : class bvac_rstructureexp0D
	   Vacuum field times radius in the toroidal field magnet [T.m]. Positive sign means anti-clockwise when viewed from above. Time-dependent. Scalar.
	- r0 : float
	   Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Scalar. 
	- p_cryo : float
	   Total electric power consumed by the cryoplant system [W]; Time-dependent. Scalar.
	- wp_nh_max : float
	   Peak nuclear heating in winding pack [W*m^-3]. Time-dependent. Scalar
	- tfc_nh : float
	   Nuclear heating on the toroidal field coils [W]; Time-dependent. Scalar
	- neut_flux_inb : float
	   Neutron flux arriving at the inboard surface of the coil (on the plasma side) [neutron.s^-1.m^-2]; Time-dependent. Scalar.
	- neut_flux_outb : float
	   Neutron flux arriving at the ouboard surface of the coil (on the plasma side) [neutron.s^-1.m^-2]; Time-dependent. Scalar.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent. Scalar.
	'''

	def __init__(self):
		self.base_path = 'toroidfield'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 3
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.desc_tfcoils = desc_tfcoilsstructuretf_desc_tfcoils('desc_tfcoils')
		self.nturns = EMPTY_INT
		self.ncoils = EMPTY_INT
		self.current = currentstructureexp0D('current')
		self.bvac_r = bvac_rstructureexp0D('bvac_r')
		self.r0 = EMPTY_DOUBLE
		self.p_cryo = EMPTY_DOUBLE
		self.wp_nh_max = EMPTY_DOUBLE
		self.tfc_nh = EMPTY_DOUBLE
		self.neut_flux_inb = EMPTY_DOUBLE
		self.neut_flux_outb = EMPTY_DOUBLE
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class toroidfield\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute desc_tfcoils\n ' + self.desc_tfcoils.__str__(depth+1)
		ret = ret + space + 'Attribute nturns: ' + str(self.nturns) + '\n'
		ret = ret + space + 'Attribute ncoils: ' + str(self.ncoils) + '\n'
		ret = ret + space + 'Attribute current\n ' + self.current.__str__(depth+1)
		ret = ret + space + 'Attribute bvac_r\n ' + self.bvac_r.__str__(depth+1)
		ret = ret + space + 'Attribute r0: ' + str(self.r0) + '\n'
		ret = ret + space + 'Attribute p_cryo: ' + str(self.p_cryo) + '\n'
		ret = ret + space + 'Attribute wp_nh_max: ' + str(self.wp_nh_max) + '\n'
		ret = ret + space + 'Attribute tfc_nh: ' + str(self.tfc_nh) + '\n'
		ret = ret + space + 'Attribute neut_flux_inb: ' + str(self.neut_flux_inb) + '\n'
		ret = ret + space + 'Attribute neut_flux_outb: ' + str(self.neut_flux_outb) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.desc_tfcoils.setExpIdx(idx)
		self.current.setExpIdx(idx)
		self.bvac_r.setExpIdx(idx)
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
		self.desc_tfcoils.cpoTime = self.cpoTime
		self.desc_tfcoils.putSlice(path, cpopath)
		self.current.cpoTime = self.cpoTime
		self.current.putSlice(path, cpopath)
		self.bvac_r.cpoTime = self.cpoTime
		self.bvac_r.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'p_cryo', self.p_cryo, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'wp_nh_max', self.wp_nh_max, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'tfc_nh', self.tfc_nh, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'neut_flux_inb', self.neut_flux_inb, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'neut_flux_outb', self.neut_flux_outb, self.cpoTime)
		check_status(status)
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
		self.desc_tfcoils.replaceLastSlice(path, cpopath)
		self.current.replaceLastSlice(path, cpopath)
		self.bvac_r.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'p_cryo', self.p_cryo)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'wp_nh_max', self.wp_nh_max)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'tfc_nh', self.tfc_nh)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'neut_flux_inb', self.neut_flux_inb)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'neut_flux_outb', self.neut_flux_outb)
		check_status(status)
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
		self.desc_tfcoils.putNonTimed(path, cpopath)
		status = ull.putInt(self.idx, path, cpopath + 'nturns', self.nturns)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'ncoils', self.ncoils)
		check_status(status)
		self.current.putNonTimed(path, cpopath)
		self.bvac_r.putNonTimed(path, cpopath)
		status = ull.putDouble(self.idx, path, cpopath + 'r0', self.r0)
		check_status(status)
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
		self.desc_tfcoils.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_nturns = ull.getInt(self.idx, path, cpopath + 'nturns')
		check_status(status)
		if not status:
			self.nturns = ret_nturns
		status, ret_ncoils = ull.getInt(self.idx, path, cpopath + 'ncoils')
		check_status(status)
		if not status:
			self.ncoils = ret_ncoils
		self.current.getSlice(path, cpopath, inTime, interpolMode)
		self.bvac_r.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_r0 = ull.getDouble(self.idx, path, cpopath + 'r0')
		check_status(status)
		if not status:
			self.r0 = ret_r0
		status, ret_p_cryo, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'p_cryo', inTime, interpolMode)
		check_status(status)
		if not status:
			self.p_cryo = ret_p_cryo
			self.cpoTime = retTime
		status, ret_wp_nh_max, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'wp_nh_max', inTime, interpolMode)
		check_status(status)
		if not status:
			self.wp_nh_max = ret_wp_nh_max
			self.cpoTime = retTime
		status, ret_tfc_nh, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'tfc_nh', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tfc_nh = ret_tfc_nh
			self.cpoTime = retTime
		status, ret_neut_flux_inb, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'neut_flux_inb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.neut_flux_inb = ret_neut_flux_inb
			self.cpoTime = retTime
		status, ret_neut_flux_outb, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'neut_flux_outb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.neut_flux_outb = ret_neut_flux_outb
			self.cpoTime = retTime
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
			desc_tfcoilsList = self.desc_tfcoils.build_non_resampled_data(path, cpopath, nbslice)
			status, nturnsVal = ull.getInt(self.idx, path, cpopath + 'nturns')
			check_status(status)
			status, ncoilsVal = ull.getInt(self.idx, path, cpopath + 'ncoils')
			check_status(status)
			currentList = self.current.build_non_resampled_data(path, cpopath, nbslice)
			bvac_rList = self.bvac_r.build_non_resampled_data(path, cpopath, nbslice)
			status, r0Val = ull.getDouble(self.idx, path, cpopath + 'r0')
			check_status(status)
			status, p_cryoList = ull.getVect1DDouble(self.idx, path, cpopath + 'p_cryo')
			if len(p_cryoList) == 0:
				p_cryoList = numpy.resize(p_cryoList, (nbslice))
			check_status(status)
			status, wp_nh_maxList = ull.getVect1DDouble(self.idx, path, cpopath + 'wp_nh_max')
			if len(wp_nh_maxList) == 0:
				wp_nh_maxList = numpy.resize(wp_nh_maxList, (nbslice))
			check_status(status)
			status, tfc_nhList = ull.getVect1DDouble(self.idx, path, cpopath + 'tfc_nh')
			if len(tfc_nhList) == 0:
				tfc_nhList = numpy.resize(tfc_nhList, (nbslice))
			check_status(status)
			status, neut_flux_inbList = ull.getVect1DDouble(self.idx, path, cpopath + 'neut_flux_inb')
			if len(neut_flux_inbList) == 0:
				neut_flux_inbList = numpy.resize(neut_flux_inbList, (nbslice))
			check_status(status)
			status, neut_flux_outbList = ull.getVect1DDouble(self.idx, path, cpopath + 'neut_flux_outb')
			if len(neut_flux_outbList) == 0:
				neut_flux_outbList = numpy.resize(neut_flux_outbList, (nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = toroidfield()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.desc_tfcoils = desc_tfcoilsList[i]
				slice.nturns = nturnsVal
				slice.ncoils = ncoilsVal
				slice.current = currentList[i]
				slice.bvac_r = bvac_rList[i]
				slice.r0 = r0Val
				slice.p_cryo = p_cryoList[i].copy().astype(float)
				slice.wp_nh_max = wp_nh_maxList[i].copy().astype(float)
				slice.tfc_nh = tfc_nhList[i].copy().astype(float)
				slice.neut_flux_inb = neut_flux_inbList[i].copy().astype(float)
				slice.neut_flux_outb = neut_flux_outbList[i].copy().astype(float)
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
		self.desc_tfcoils.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'nturns')
		ull.deleteData(self.idx, path, cpopath + 'ncoils')
		self.current.deleteData(path, cpopath)
		self.bvac_r.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'r0')
		ull.deleteData(self.idx, path, cpopath + 'p_cryo')
		ull.deleteData(self.idx, path, cpopath + 'wp_nh_max')
		ull.deleteData(self.idx, path, cpopath + 'tfc_nh')
		ull.deleteData(self.idx, path, cpopath + 'neut_flux_inb')
		ull.deleteData(self.idx, path, cpopath + 'neut_flux_outb')
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class toroidfieldArray:
	'''
	class toroidfieldArray
	Toroidal field. Time-dependent CPO

	Attributes:
	- array : list of toroidfield
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
		ret = space + 'class toroidfieldArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'toroidfield cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = toroidfield()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(toroidfield())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = toroidfield()
		cpo.setExpIdx(self.idx)
		cpo.deleteData(occurrence)


class datainfostructuredatainfo:
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


class whatrefstructurewhatref:
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


class putinfostructureputinfo:
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


class desc_tfcoilsstructuretf_desc_tfcoils:
	'''
	class desc_tfcoilsstructuretf_desc_tfcoils
	Description of the toroidal field coils

	Attributes:
	- type : int
	   Type of coil, 0=circular coil, 1=plane coil with arbitrary shape.
	- phi : float
	   Toroidal angle of centre of coil 1, assuming all coils are identical and evenly distributed around the torus [rad]. Scalar
	- circularcoil : class circularcoilstructurecircularcoil
	   Circular coil description
	- planecoil : class planecoilstructureplanecoil
	   Plane coil description
	- inboard : class inboardstructuretf_structure
	   Description of TF inboard structure
	- outboard : class outboardstructuretf_structure
	   Description of TF outboard structure
	'''

	def __init__(self, base_path_in='desc_tfcoils'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = EMPTY_INT
		self.phi = EMPTY_DOUBLE
		self.circularcoil = circularcoilstructurecircularcoil('circularcoil')
		self.planecoil = planecoilstructureplanecoil('planecoil')
		self.inboard = inboardstructuretf_structure('inboard')
		self.outboard = outboardstructuretf_structure('outboard')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class desc_tfcoilsstructuretf_desc_tfcoils\n'
		ret = ret + space + 'Attribute type: ' + str(self.type) + '\n'
		ret = ret + space + 'Attribute phi: ' + str(self.phi) + '\n'
		ret = ret + space + 'Attribute circularcoil\n ' + self.circularcoil.__str__(depth+1)
		ret = ret + space + 'Attribute planecoil\n ' + self.planecoil.__str__(depth+1)
		ret = ret + space + 'Attribute inboard\n ' + self.inboard.__str__(depth+1)
		ret = ret + space + 'Attribute outboard\n ' + self.outboard.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.circularcoil.setExpIdx(idx)
		self.planecoil.setExpIdx(idx)
		self.inboard.setExpIdx(idx)
		self.outboard.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_tfcoilsstructuretf_desc_tfcoils, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.circularcoil.cpoTime = self.cpoTime
		self.circularcoil.putSlice(path, cpopath)
		self.planecoil.cpoTime = self.cpoTime
		self.planecoil.putSlice(path, cpopath)
		self.inboard.cpoTime = self.cpoTime
		self.inboard.putSlice(path, cpopath)
		self.outboard.cpoTime = self.cpoTime
		self.outboard.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_tfcoilsstructuretf_desc_tfcoils, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.circularcoil.replaceLastSlice(path, cpopath)
		self.planecoil.replaceLastSlice(path, cpopath)
		self.inboard.replaceLastSlice(path, cpopath)
		self.outboard.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_tfcoilsstructuretf_desc_tfcoils, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'type', self.type)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'phi', self.phi)
		check_status(status)
		self.circularcoil.putNonTimed(path, cpopath)
		self.planecoil.putNonTimed(path, cpopath)
		self.inboard.putNonTimed(path, cpopath)
		self.outboard.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type desc_tfcoilsstructuretf_desc_tfcoils, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_type = ull.getInt(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_phi = ull.getDouble(self.idx, path, cpopath + 'phi')
		check_status(status)
		if not status:
			self.phi = ret_phi
		self.circularcoil.getSlice(path, cpopath, inTime, interpolMode)
		self.planecoil.getSlice(path, cpopath, inTime, interpolMode)
		self.inboard.getSlice(path, cpopath, inTime, interpolMode)
		self.outboard.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type desc_tfcoilsstructuretf_desc_tfcoils, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, typeVal = ull.getInt(self.idx, path, cpopath + 'type')
			check_status(status)
			status, phiVal = ull.getDouble(self.idx, path, cpopath + 'phi')
			check_status(status)
			circularcoilList = self.circularcoil.build_non_resampled_data(path, cpopath, nbslice)
			planecoilList = self.planecoil.build_non_resampled_data(path, cpopath, nbslice)
			inboardList = self.inboard.build_non_resampled_data(path, cpopath, nbslice)
			outboardList = self.outboard.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = desc_tfcoilsstructuretf_desc_tfcoils(self.base_path)
				slice.setExpIdx(self.idx)
				slice.type = typeVal
				slice.phi = phiVal
				slice.circularcoil = circularcoilList[i]
				slice.planecoil = planecoilList[i]
				slice.inboard = inboardList[i]
				slice.outboard = outboardList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_tfcoilsstructuretf_desc_tfcoilsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_tfcoilsstructuretf_desc_tfcoilsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_tfcoilsstructuretf_desc_tfcoilsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'type', i, self.type)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'phi', i, self.phi)
		obj = self.circularcoil.putNonTimedElt(path, cpopath + 'circularcoil', i, obj)
		obj = self.planecoil.putNonTimedElt(path, cpopath + 'planecoil', i, obj)
		obj = self.inboard.putNonTimedElt(path, cpopath + 'inboard', i, obj)
		obj = self.outboard.putNonTimedElt(path, cpopath + 'outboard', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_tfcoilsstructuretf_desc_tfcoilsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getIntFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		self.circularcoil.getNonTimedElt(path, cpopath + 'circularcoil', i, obj)
		self.planecoil.getNonTimedElt(path, cpopath + 'planecoil', i, obj)
		self.inboard.getNonTimedElt(path, cpopath + 'inboard', i, obj)
		self.outboard.getNonTimedElt(path, cpopath + 'outboard', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		self.circularcoil.deleteData(path, cpopath)
		self.planecoil.deleteData(path, cpopath)
		self.inboard.deleteData(path, cpopath)
		self.outboard.deleteData(path, cpopath)


class circularcoilstructurecircularcoil:
	'''
	class circularcoilstructurecircularcoil
	Circular coil description

	Attributes:
	- centre : class centrestructurerz0D
	   Circular coil centre
	- hlength : float
	   Half length along coil axis [m]
	- radialhwidth : float
	   Half width, (outer radius-inner radius)/2 [m]
	'''

	def __init__(self, base_path_in='circularcoil'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.centre = centrestructurerz0D('centre')
		self.hlength = EMPTY_DOUBLE
		self.radialhwidth = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class circularcoilstructurecircularcoil\n'
		ret = ret + space + 'Attribute centre\n ' + self.centre.__str__(depth+1)
		ret = ret + space + 'Attribute hlength: ' + str(self.hlength) + '\n'
		ret = ret + space + 'Attribute radialhwidth: ' + str(self.radialhwidth) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.centre.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type circularcoilstructurecircularcoil, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.centre.cpoTime = self.cpoTime
		self.centre.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type circularcoilstructurecircularcoil, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.centre.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type circularcoilstructurecircularcoil, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.centre.putNonTimed(path, cpopath)
		status = ull.putDouble(self.idx, path, cpopath + 'hlength', self.hlength)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'radialhwidth', self.radialhwidth)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type circularcoilstructurecircularcoil, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.centre.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_hlength = ull.getDouble(self.idx, path, cpopath + 'hlength')
		check_status(status)
		if not status:
			self.hlength = ret_hlength
		status, ret_radialhwidth = ull.getDouble(self.idx, path, cpopath + 'radialhwidth')
		check_status(status)
		if not status:
			self.radialhwidth = ret_radialhwidth

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type circularcoilstructurecircularcoil, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			centreList = self.centre.build_non_resampled_data(path, cpopath, nbslice)
			status, hlengthVal = ull.getDouble(self.idx, path, cpopath + 'hlength')
			check_status(status)
			status, radialhwidthVal = ull.getDouble(self.idx, path, cpopath + 'radialhwidth')
			check_status(status)
			for i in range(nbslice):
				slice = circularcoilstructurecircularcoil(self.base_path)
				slice.setExpIdx(self.idx)
				slice.centre = centreList[i]
				slice.hlength = hlengthVal
				slice.radialhwidth = radialhwidthVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type circularcoilstructurecircularcoilObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type circularcoilstructurecircularcoilObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type circularcoilstructurecircularcoilObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.centre.putNonTimedElt(path, cpopath + 'centre', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'hlength') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'hlength', i, self.hlength)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'radialhwidth') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'radialhwidth', i, self.radialhwidth)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type circularcoilstructurecircularcoilObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.centre.getNonTimedElt(path, cpopath + 'centre', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'hlength') 
			print ('obj = ' + str(obj))
		status, ret_hlength = ull.getDoubleFromObject(self.idx, obj, cpopath + 'hlength', i)
		check_status(status)
		if not status:
			self.hlength = ret_hlength
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'radialhwidth') 
			print ('obj = ' + str(obj))
		status, ret_radialhwidth = ull.getDoubleFromObject(self.idx, obj, cpopath + 'radialhwidth', i)
		check_status(status)
		if not status:
			self.radialhwidth = ret_radialhwidth

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.centre.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'hlength')
		ull.deleteData(self.idx, path, cpopath + 'radialhwidth')


class centrestructurerz0D:
	'''
	class centrestructurerz0D
	Circular coil centre

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='centre'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class centrestructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type centrestructurerz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type centrestructurerz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type centrestructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'r', self.r)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type centrestructurerz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type centrestructurerz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = centrestructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type centrestructurerz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type centrestructurerz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type centrestructurerz0DObj, run function putNonTimedElt') 
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

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type centrestructurerz0DObj, run function getNonTimedElt') 
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')


class planecoilstructureplanecoil:
	'''
	class planecoilstructureplanecoil
	Plane coil description

	Attributes:
	- coordinates : class coordinatesstructurerz1D
	   Coordinate points of centre of conductor; vectors(nelements)
	- hlength : numpy.ndarray 1D with float
	   Half length perpendicular to plane where coil is defined; vector(nelements) [m].
	- radialhwidth : numpy.ndarray 1D with float
	   Half width, (outer contour-inner contour)/2; vector(nelements) [m].
	'''

	def __init__(self, base_path_in='planecoil'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.coordinates = coordinatesstructurerz1D('coordinates')
		self.hlength = numpy.zeros(0, numpy.float64, order='C')
		self.radialhwidth = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class planecoilstructureplanecoil\n'
		ret = ret + space + 'Attribute coordinates\n ' + self.coordinates.__str__(depth+1)
		s = self.hlength.__str__()
		ret = ret + space + 'Attribute hlength\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.radialhwidth.__str__()
		ret = ret + space + 'Attribute radialhwidth\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.coordinates.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type planecoilstructureplanecoil, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.cpoTime = self.cpoTime
		self.coordinates.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type planecoilstructureplanecoil, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type planecoilstructureplanecoil, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.putNonTimed(path, cpopath)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'hlength', numpy.array(self.hlength).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'radialhwidth', numpy.array(self.radialhwidth).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type planecoilstructureplanecoil, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_hlength = ull.getVect1DDouble(self.idx, path, cpopath + 'hlength')
		check_status(status)
		if not status:
			self.hlength = ret_hlength
		status, ret_radialhwidth = ull.getVect1DDouble(self.idx, path, cpopath + 'radialhwidth')
		check_status(status)
		if not status:
			self.radialhwidth = ret_radialhwidth

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type planecoilstructureplanecoil, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			coordinatesList = self.coordinates.build_non_resampled_data(path, cpopath, nbslice)
			status, hlengthVal = ull.getVect1DDouble(self.idx, path, cpopath + 'hlength')
			check_status(status)
			status, radialhwidthVal = ull.getVect1DDouble(self.idx, path, cpopath + 'radialhwidth')
			check_status(status)
			for i in range(nbslice):
				slice = planecoilstructureplanecoil(self.base_path)
				slice.setExpIdx(self.idx)
				slice.coordinates = coordinatesList[i]
				slice.hlength = hlengthVal
				slice.radialhwidth = radialhwidthVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type planecoilstructureplanecoilObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type planecoilstructureplanecoilObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type planecoilstructureplanecoilObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.coordinates.putNonTimedElt(path, cpopath + 'coordinates', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'hlength') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'hlength', i, numpy.array(self.hlength).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'radialhwidth') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'radialhwidth', i, numpy.array(self.radialhwidth).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type planecoilstructureplanecoilObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.coordinates.getNonTimedElt(path, cpopath + 'coordinates', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'hlength') 
			print ('obj = ' + str(obj))
		status, ret_hlength = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'hlength', i)
		check_status(status)
		if not status:
			self.hlength = ret_hlength
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'radialhwidth') 
			print ('obj = ' + str(obj))
		status, ret_radialhwidth = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'radialhwidth', i)
		check_status(status)
		if not status:
			self.radialhwidth = ret_radialhwidth

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'hlength')
		ull.deleteData(self.idx, path, cpopath + 'radialhwidth')


class coordinatesstructurerz1D:
	'''
	class coordinatesstructurerz1D
	Coordinate points of centre of conductor; vectors(nelements)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='coordinates'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coordinatesstructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurerz1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurerz1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurerz1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurerz1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getVect1DDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_z = ull.getVect1DDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurerz1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getVect1DDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, zVal = ull.getVect1DDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = coordinatesstructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurerz1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurerz1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurerz1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurerz1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')


class inboardstructuretf_structure:
	'''
	class inboardstructuretf_structure
	Description of TF inboard structure

	Attributes:
	- jcable : float
	   CICS cable in current density [A/m²]; Scalar
	- tisotf : float
	   Insulation thickness of TF conductor [m]; Scalar
	- efcasing : float
	   Thickness front casing [m]; Scalar
	- escasing : float
	   Thickness side casing [m]; Scalar
	- sigjackettf : float
	   Jacket stress limit [Pa]; Scalar
	- sigvaulttf : float
	   Vault stress limit  [Pa]; Scalar
	- ktf : float
	   Amplification factor for magnetic field
	- ritf : float
	   Internal TF coil radius [m]; Scalar
	- riitf : float
	   Internal vault TF coil radius [m]; Scalar
	- retf : float
	   External TF coil radius [m]; Scalar
	- he_fraction : float
	   Helium fraction (percentage) in TF structure infront of winding package  [-]; Scalar
	- ss_fraction : float
	   Stainless steel 316 fraction (percentage) in TF structure infront of winding package  [-]; Scalar
	- pow_dens_wp : float
	   Peak energy depostion in winding pack [W.m^-3]; Scalar
	'''

	def __init__(self, base_path_in='inboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.jcable = EMPTY_DOUBLE
		self.tisotf = EMPTY_DOUBLE
		self.efcasing = EMPTY_DOUBLE
		self.escasing = EMPTY_DOUBLE
		self.sigjackettf = EMPTY_DOUBLE
		self.sigvaulttf = EMPTY_DOUBLE
		self.ktf = EMPTY_DOUBLE
		self.ritf = EMPTY_DOUBLE
		self.riitf = EMPTY_DOUBLE
		self.retf = EMPTY_DOUBLE
		self.he_fraction = EMPTY_DOUBLE
		self.ss_fraction = EMPTY_DOUBLE
		self.pow_dens_wp = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class inboardstructuretf_structure\n'
		ret = ret + space + 'Attribute jcable: ' + str(self.jcable) + '\n'
		ret = ret + space + 'Attribute tisotf: ' + str(self.tisotf) + '\n'
		ret = ret + space + 'Attribute efcasing: ' + str(self.efcasing) + '\n'
		ret = ret + space + 'Attribute escasing: ' + str(self.escasing) + '\n'
		ret = ret + space + 'Attribute sigjackettf: ' + str(self.sigjackettf) + '\n'
		ret = ret + space + 'Attribute sigvaulttf: ' + str(self.sigvaulttf) + '\n'
		ret = ret + space + 'Attribute ktf: ' + str(self.ktf) + '\n'
		ret = ret + space + 'Attribute ritf: ' + str(self.ritf) + '\n'
		ret = ret + space + 'Attribute riitf: ' + str(self.riitf) + '\n'
		ret = ret + space + 'Attribute retf: ' + str(self.retf) + '\n'
		ret = ret + space + 'Attribute he_fraction: ' + str(self.he_fraction) + '\n'
		ret = ret + space + 'Attribute ss_fraction: ' + str(self.ss_fraction) + '\n'
		ret = ret + space + 'Attribute pow_dens_wp: ' + str(self.pow_dens_wp) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructuretf_structure, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructuretf_structure, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructuretf_structure, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'jcable', self.jcable)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tisotf', self.tisotf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'efcasing', self.efcasing)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'escasing', self.escasing)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sigjackettf', self.sigjackettf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sigvaulttf', self.sigvaulttf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ktf', self.ktf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ritf', self.ritf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'riitf', self.riitf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'retf', self.retf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_fraction', self.he_fraction)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ss_fraction', self.ss_fraction)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_wp', self.pow_dens_wp)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructuretf_structure, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_jcable = ull.getDouble(self.idx, path, cpopath + 'jcable')
		check_status(status)
		if not status:
			self.jcable = ret_jcable
		status, ret_tisotf = ull.getDouble(self.idx, path, cpopath + 'tisotf')
		check_status(status)
		if not status:
			self.tisotf = ret_tisotf
		status, ret_efcasing = ull.getDouble(self.idx, path, cpopath + 'efcasing')
		check_status(status)
		if not status:
			self.efcasing = ret_efcasing
		status, ret_escasing = ull.getDouble(self.idx, path, cpopath + 'escasing')
		check_status(status)
		if not status:
			self.escasing = ret_escasing
		status, ret_sigjackettf = ull.getDouble(self.idx, path, cpopath + 'sigjackettf')
		check_status(status)
		if not status:
			self.sigjackettf = ret_sigjackettf
		status, ret_sigvaulttf = ull.getDouble(self.idx, path, cpopath + 'sigvaulttf')
		check_status(status)
		if not status:
			self.sigvaulttf = ret_sigvaulttf
		status, ret_ktf = ull.getDouble(self.idx, path, cpopath + 'ktf')
		check_status(status)
		if not status:
			self.ktf = ret_ktf
		status, ret_ritf = ull.getDouble(self.idx, path, cpopath + 'ritf')
		check_status(status)
		if not status:
			self.ritf = ret_ritf
		status, ret_riitf = ull.getDouble(self.idx, path, cpopath + 'riitf')
		check_status(status)
		if not status:
			self.riitf = ret_riitf
		status, ret_retf = ull.getDouble(self.idx, path, cpopath + 'retf')
		check_status(status)
		if not status:
			self.retf = ret_retf
		status, ret_he_fraction = ull.getDouble(self.idx, path, cpopath + 'he_fraction')
		check_status(status)
		if not status:
			self.he_fraction = ret_he_fraction
		status, ret_ss_fraction = ull.getDouble(self.idx, path, cpopath + 'ss_fraction')
		check_status(status)
		if not status:
			self.ss_fraction = ret_ss_fraction
		status, ret_pow_dens_wp = ull.getDouble(self.idx, path, cpopath + 'pow_dens_wp')
		check_status(status)
		if not status:
			self.pow_dens_wp = ret_pow_dens_wp

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructuretf_structure, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, jcableVal = ull.getDouble(self.idx, path, cpopath + 'jcable')
			check_status(status)
			status, tisotfVal = ull.getDouble(self.idx, path, cpopath + 'tisotf')
			check_status(status)
			status, efcasingVal = ull.getDouble(self.idx, path, cpopath + 'efcasing')
			check_status(status)
			status, escasingVal = ull.getDouble(self.idx, path, cpopath + 'escasing')
			check_status(status)
			status, sigjackettfVal = ull.getDouble(self.idx, path, cpopath + 'sigjackettf')
			check_status(status)
			status, sigvaulttfVal = ull.getDouble(self.idx, path, cpopath + 'sigvaulttf')
			check_status(status)
			status, ktfVal = ull.getDouble(self.idx, path, cpopath + 'ktf')
			check_status(status)
			status, ritfVal = ull.getDouble(self.idx, path, cpopath + 'ritf')
			check_status(status)
			status, riitfVal = ull.getDouble(self.idx, path, cpopath + 'riitf')
			check_status(status)
			status, retfVal = ull.getDouble(self.idx, path, cpopath + 'retf')
			check_status(status)
			status, he_fractionVal = ull.getDouble(self.idx, path, cpopath + 'he_fraction')
			check_status(status)
			status, ss_fractionVal = ull.getDouble(self.idx, path, cpopath + 'ss_fraction')
			check_status(status)
			status, pow_dens_wpVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_wp')
			check_status(status)
			for i in range(nbslice):
				slice = inboardstructuretf_structure(self.base_path)
				slice.setExpIdx(self.idx)
				slice.jcable = jcableVal
				slice.tisotf = tisotfVal
				slice.efcasing = efcasingVal
				slice.escasing = escasingVal
				slice.sigjackettf = sigjackettfVal
				slice.sigvaulttf = sigvaulttfVal
				slice.ktf = ktfVal
				slice.ritf = ritfVal
				slice.riitf = riitfVal
				slice.retf = retfVal
				slice.he_fraction = he_fractionVal
				slice.ss_fraction = ss_fractionVal
				slice.pow_dens_wp = pow_dens_wpVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructuretf_structureObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructuretf_structureObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructuretf_structureObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'jcable') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'jcable', i, self.jcable)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tisotf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tisotf', i, self.tisotf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'efcasing') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'efcasing', i, self.efcasing)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'escasing') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'escasing', i, self.escasing)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigjackettf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigjackettf', i, self.sigjackettf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigvaulttf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigvaulttf', i, self.sigvaulttf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ktf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ktf', i, self.ktf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ritf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ritf', i, self.ritf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'riitf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'riitf', i, self.riitf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'retf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'retf', i, self.retf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_fraction') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_fraction', i, self.he_fraction)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ss_fraction') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ss_fraction', i, self.ss_fraction)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_wp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_wp', i, self.pow_dens_wp)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructuretf_structureObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'jcable') 
			print ('obj = ' + str(obj))
		status, ret_jcable = ull.getDoubleFromObject(self.idx, obj, cpopath + 'jcable', i)
		check_status(status)
		if not status:
			self.jcable = ret_jcable
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tisotf') 
			print ('obj = ' + str(obj))
		status, ret_tisotf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tisotf', i)
		check_status(status)
		if not status:
			self.tisotf = ret_tisotf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'efcasing') 
			print ('obj = ' + str(obj))
		status, ret_efcasing = ull.getDoubleFromObject(self.idx, obj, cpopath + 'efcasing', i)
		check_status(status)
		if not status:
			self.efcasing = ret_efcasing
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'escasing') 
			print ('obj = ' + str(obj))
		status, ret_escasing = ull.getDoubleFromObject(self.idx, obj, cpopath + 'escasing', i)
		check_status(status)
		if not status:
			self.escasing = ret_escasing
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigjackettf') 
			print ('obj = ' + str(obj))
		status, ret_sigjackettf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigjackettf', i)
		check_status(status)
		if not status:
			self.sigjackettf = ret_sigjackettf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigvaulttf') 
			print ('obj = ' + str(obj))
		status, ret_sigvaulttf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigvaulttf', i)
		check_status(status)
		if not status:
			self.sigvaulttf = ret_sigvaulttf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ktf') 
			print ('obj = ' + str(obj))
		status, ret_ktf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ktf', i)
		check_status(status)
		if not status:
			self.ktf = ret_ktf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ritf') 
			print ('obj = ' + str(obj))
		status, ret_ritf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ritf', i)
		check_status(status)
		if not status:
			self.ritf = ret_ritf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'riitf') 
			print ('obj = ' + str(obj))
		status, ret_riitf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'riitf', i)
		check_status(status)
		if not status:
			self.riitf = ret_riitf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'retf') 
			print ('obj = ' + str(obj))
		status, ret_retf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'retf', i)
		check_status(status)
		if not status:
			self.retf = ret_retf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_fraction') 
			print ('obj = ' + str(obj))
		status, ret_he_fraction = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_fraction', i)
		check_status(status)
		if not status:
			self.he_fraction = ret_he_fraction
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ss_fraction') 
			print ('obj = ' + str(obj))
		status, ret_ss_fraction = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ss_fraction', i)
		check_status(status)
		if not status:
			self.ss_fraction = ret_ss_fraction
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_wp') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_wp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_wp', i)
		check_status(status)
		if not status:
			self.pow_dens_wp = ret_pow_dens_wp

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'jcable')
		ull.deleteData(self.idx, path, cpopath + 'tisotf')
		ull.deleteData(self.idx, path, cpopath + 'efcasing')
		ull.deleteData(self.idx, path, cpopath + 'escasing')
		ull.deleteData(self.idx, path, cpopath + 'sigjackettf')
		ull.deleteData(self.idx, path, cpopath + 'sigvaulttf')
		ull.deleteData(self.idx, path, cpopath + 'ktf')
		ull.deleteData(self.idx, path, cpopath + 'ritf')
		ull.deleteData(self.idx, path, cpopath + 'riitf')
		ull.deleteData(self.idx, path, cpopath + 'retf')
		ull.deleteData(self.idx, path, cpopath + 'he_fraction')
		ull.deleteData(self.idx, path, cpopath + 'ss_fraction')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_wp')


class outboardstructuretf_structure:
	'''
	class outboardstructuretf_structure
	Description of TF outboard structure

	Attributes:
	- jcable : float
	   CICS cable in current density [A/m²]; Scalar
	- tisotf : float
	   Insulation thickness of TF conductor [m]; Scalar
	- efcasing : float
	   Thickness front casing [m]; Scalar
	- escasing : float
	   Thickness side casing [m]; Scalar
	- sigjackettf : float
	   Jacket stress limit [Pa]; Scalar
	- sigvaulttf : float
	   Vault stress limit  [Pa]; Scalar
	- ktf : float
	   Amplification factor for magnetic field
	- ritf : float
	   Internal TF coil radius [m]; Scalar
	- riitf : float
	   Internal vault TF coil radius [m]; Scalar
	- retf : float
	   External TF coil radius [m]; Scalar
	- he_fraction : float
	   Helium fraction (percentage) in TF structure infront of winding package  [-]; Scalar
	- ss_fraction : float
	   Stainless steel 316 fraction (percentage) in TF structure infront of winding package  [-]; Scalar
	- pow_dens_wp : float
	   Peak energy depostion in winding pack [W.m^-3]; Scalar
	'''

	def __init__(self, base_path_in='outboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.jcable = EMPTY_DOUBLE
		self.tisotf = EMPTY_DOUBLE
		self.efcasing = EMPTY_DOUBLE
		self.escasing = EMPTY_DOUBLE
		self.sigjackettf = EMPTY_DOUBLE
		self.sigvaulttf = EMPTY_DOUBLE
		self.ktf = EMPTY_DOUBLE
		self.ritf = EMPTY_DOUBLE
		self.riitf = EMPTY_DOUBLE
		self.retf = EMPTY_DOUBLE
		self.he_fraction = EMPTY_DOUBLE
		self.ss_fraction = EMPTY_DOUBLE
		self.pow_dens_wp = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class outboardstructuretf_structure\n'
		ret = ret + space + 'Attribute jcable: ' + str(self.jcable) + '\n'
		ret = ret + space + 'Attribute tisotf: ' + str(self.tisotf) + '\n'
		ret = ret + space + 'Attribute efcasing: ' + str(self.efcasing) + '\n'
		ret = ret + space + 'Attribute escasing: ' + str(self.escasing) + '\n'
		ret = ret + space + 'Attribute sigjackettf: ' + str(self.sigjackettf) + '\n'
		ret = ret + space + 'Attribute sigvaulttf: ' + str(self.sigvaulttf) + '\n'
		ret = ret + space + 'Attribute ktf: ' + str(self.ktf) + '\n'
		ret = ret + space + 'Attribute ritf: ' + str(self.ritf) + '\n'
		ret = ret + space + 'Attribute riitf: ' + str(self.riitf) + '\n'
		ret = ret + space + 'Attribute retf: ' + str(self.retf) + '\n'
		ret = ret + space + 'Attribute he_fraction: ' + str(self.he_fraction) + '\n'
		ret = ret + space + 'Attribute ss_fraction: ' + str(self.ss_fraction) + '\n'
		ret = ret + space + 'Attribute pow_dens_wp: ' + str(self.pow_dens_wp) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructuretf_structure, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructuretf_structure, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructuretf_structure, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'jcable', self.jcable)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tisotf', self.tisotf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'efcasing', self.efcasing)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'escasing', self.escasing)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sigjackettf', self.sigjackettf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sigvaulttf', self.sigvaulttf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ktf', self.ktf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ritf', self.ritf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'riitf', self.riitf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'retf', self.retf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_fraction', self.he_fraction)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ss_fraction', self.ss_fraction)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_wp', self.pow_dens_wp)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructuretf_structure, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_jcable = ull.getDouble(self.idx, path, cpopath + 'jcable')
		check_status(status)
		if not status:
			self.jcable = ret_jcable
		status, ret_tisotf = ull.getDouble(self.idx, path, cpopath + 'tisotf')
		check_status(status)
		if not status:
			self.tisotf = ret_tisotf
		status, ret_efcasing = ull.getDouble(self.idx, path, cpopath + 'efcasing')
		check_status(status)
		if not status:
			self.efcasing = ret_efcasing
		status, ret_escasing = ull.getDouble(self.idx, path, cpopath + 'escasing')
		check_status(status)
		if not status:
			self.escasing = ret_escasing
		status, ret_sigjackettf = ull.getDouble(self.idx, path, cpopath + 'sigjackettf')
		check_status(status)
		if not status:
			self.sigjackettf = ret_sigjackettf
		status, ret_sigvaulttf = ull.getDouble(self.idx, path, cpopath + 'sigvaulttf')
		check_status(status)
		if not status:
			self.sigvaulttf = ret_sigvaulttf
		status, ret_ktf = ull.getDouble(self.idx, path, cpopath + 'ktf')
		check_status(status)
		if not status:
			self.ktf = ret_ktf
		status, ret_ritf = ull.getDouble(self.idx, path, cpopath + 'ritf')
		check_status(status)
		if not status:
			self.ritf = ret_ritf
		status, ret_riitf = ull.getDouble(self.idx, path, cpopath + 'riitf')
		check_status(status)
		if not status:
			self.riitf = ret_riitf
		status, ret_retf = ull.getDouble(self.idx, path, cpopath + 'retf')
		check_status(status)
		if not status:
			self.retf = ret_retf
		status, ret_he_fraction = ull.getDouble(self.idx, path, cpopath + 'he_fraction')
		check_status(status)
		if not status:
			self.he_fraction = ret_he_fraction
		status, ret_ss_fraction = ull.getDouble(self.idx, path, cpopath + 'ss_fraction')
		check_status(status)
		if not status:
			self.ss_fraction = ret_ss_fraction
		status, ret_pow_dens_wp = ull.getDouble(self.idx, path, cpopath + 'pow_dens_wp')
		check_status(status)
		if not status:
			self.pow_dens_wp = ret_pow_dens_wp

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructuretf_structure, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, jcableVal = ull.getDouble(self.idx, path, cpopath + 'jcable')
			check_status(status)
			status, tisotfVal = ull.getDouble(self.idx, path, cpopath + 'tisotf')
			check_status(status)
			status, efcasingVal = ull.getDouble(self.idx, path, cpopath + 'efcasing')
			check_status(status)
			status, escasingVal = ull.getDouble(self.idx, path, cpopath + 'escasing')
			check_status(status)
			status, sigjackettfVal = ull.getDouble(self.idx, path, cpopath + 'sigjackettf')
			check_status(status)
			status, sigvaulttfVal = ull.getDouble(self.idx, path, cpopath + 'sigvaulttf')
			check_status(status)
			status, ktfVal = ull.getDouble(self.idx, path, cpopath + 'ktf')
			check_status(status)
			status, ritfVal = ull.getDouble(self.idx, path, cpopath + 'ritf')
			check_status(status)
			status, riitfVal = ull.getDouble(self.idx, path, cpopath + 'riitf')
			check_status(status)
			status, retfVal = ull.getDouble(self.idx, path, cpopath + 'retf')
			check_status(status)
			status, he_fractionVal = ull.getDouble(self.idx, path, cpopath + 'he_fraction')
			check_status(status)
			status, ss_fractionVal = ull.getDouble(self.idx, path, cpopath + 'ss_fraction')
			check_status(status)
			status, pow_dens_wpVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_wp')
			check_status(status)
			for i in range(nbslice):
				slice = outboardstructuretf_structure(self.base_path)
				slice.setExpIdx(self.idx)
				slice.jcable = jcableVal
				slice.tisotf = tisotfVal
				slice.efcasing = efcasingVal
				slice.escasing = escasingVal
				slice.sigjackettf = sigjackettfVal
				slice.sigvaulttf = sigvaulttfVal
				slice.ktf = ktfVal
				slice.ritf = ritfVal
				slice.riitf = riitfVal
				slice.retf = retfVal
				slice.he_fraction = he_fractionVal
				slice.ss_fraction = ss_fractionVal
				slice.pow_dens_wp = pow_dens_wpVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructuretf_structureObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructuretf_structureObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructuretf_structureObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'jcable') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'jcable', i, self.jcable)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tisotf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tisotf', i, self.tisotf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'efcasing') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'efcasing', i, self.efcasing)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'escasing') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'escasing', i, self.escasing)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigjackettf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigjackettf', i, self.sigjackettf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigvaulttf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigvaulttf', i, self.sigvaulttf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ktf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ktf', i, self.ktf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ritf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ritf', i, self.ritf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'riitf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'riitf', i, self.riitf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'retf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'retf', i, self.retf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_fraction') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_fraction', i, self.he_fraction)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ss_fraction') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ss_fraction', i, self.ss_fraction)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_wp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_wp', i, self.pow_dens_wp)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructuretf_structureObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'jcable') 
			print ('obj = ' + str(obj))
		status, ret_jcable = ull.getDoubleFromObject(self.idx, obj, cpopath + 'jcable', i)
		check_status(status)
		if not status:
			self.jcable = ret_jcable
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tisotf') 
			print ('obj = ' + str(obj))
		status, ret_tisotf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tisotf', i)
		check_status(status)
		if not status:
			self.tisotf = ret_tisotf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'efcasing') 
			print ('obj = ' + str(obj))
		status, ret_efcasing = ull.getDoubleFromObject(self.idx, obj, cpopath + 'efcasing', i)
		check_status(status)
		if not status:
			self.efcasing = ret_efcasing
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'escasing') 
			print ('obj = ' + str(obj))
		status, ret_escasing = ull.getDoubleFromObject(self.idx, obj, cpopath + 'escasing', i)
		check_status(status)
		if not status:
			self.escasing = ret_escasing
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigjackettf') 
			print ('obj = ' + str(obj))
		status, ret_sigjackettf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigjackettf', i)
		check_status(status)
		if not status:
			self.sigjackettf = ret_sigjackettf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigvaulttf') 
			print ('obj = ' + str(obj))
		status, ret_sigvaulttf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigvaulttf', i)
		check_status(status)
		if not status:
			self.sigvaulttf = ret_sigvaulttf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ktf') 
			print ('obj = ' + str(obj))
		status, ret_ktf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ktf', i)
		check_status(status)
		if not status:
			self.ktf = ret_ktf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ritf') 
			print ('obj = ' + str(obj))
		status, ret_ritf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ritf', i)
		check_status(status)
		if not status:
			self.ritf = ret_ritf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'riitf') 
			print ('obj = ' + str(obj))
		status, ret_riitf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'riitf', i)
		check_status(status)
		if not status:
			self.riitf = ret_riitf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'retf') 
			print ('obj = ' + str(obj))
		status, ret_retf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'retf', i)
		check_status(status)
		if not status:
			self.retf = ret_retf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_fraction') 
			print ('obj = ' + str(obj))
		status, ret_he_fraction = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_fraction', i)
		check_status(status)
		if not status:
			self.he_fraction = ret_he_fraction
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ss_fraction') 
			print ('obj = ' + str(obj))
		status, ret_ss_fraction = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ss_fraction', i)
		check_status(status)
		if not status:
			self.ss_fraction = ret_ss_fraction
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_wp') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_wp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_wp', i)
		check_status(status)
		if not status:
			self.pow_dens_wp = ret_pow_dens_wp

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'jcable')
		ull.deleteData(self.idx, path, cpopath + 'tisotf')
		ull.deleteData(self.idx, path, cpopath + 'efcasing')
		ull.deleteData(self.idx, path, cpopath + 'escasing')
		ull.deleteData(self.idx, path, cpopath + 'sigjackettf')
		ull.deleteData(self.idx, path, cpopath + 'sigvaulttf')
		ull.deleteData(self.idx, path, cpopath + 'ktf')
		ull.deleteData(self.idx, path, cpopath + 'ritf')
		ull.deleteData(self.idx, path, cpopath + 'riitf')
		ull.deleteData(self.idx, path, cpopath + 'retf')
		ull.deleteData(self.idx, path, cpopath + 'he_fraction')
		ull.deleteData(self.idx, path, cpopath + 'ss_fraction')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_wp')


class currentstructureexp0D:
	'''
	class currentstructureexp0D
	Current in the toroidal field coils [A]; Time-dependent. Scalar.

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='current'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class currentstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'value', self.value, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'value', self.value)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect1DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (nbslice))
			check_status(status)
			status, abserrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (nbslice))
			check_status(status)
			status, relerrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = currentstructureexp0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i].copy().astype(float)
				slice.abserror = abserrorList[i].copy().astype(float)
				slice.relerror = relerrorList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'value', i, self.value)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'abserror', i, self.abserror)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'relerror', i, self.relerror)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class bvac_rstructureexp0D:
	'''
	class bvac_rstructureexp0D
	Vacuum field times radius in the toroidal field magnet [T.m]. Positive sign means anti-clockwise when viewed from above. Time-dependent. Scalar.

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='bvac_r'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bvac_rstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureexp0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'value', self.value, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureexp0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'value', self.value)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureexp0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureexp0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect1DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (nbslice))
			check_status(status)
			status, abserrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (nbslice))
			check_status(status)
			status, relerrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = bvac_rstructureexp0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i].copy().astype(float)
				slice.abserror = abserrorList[i].copy().astype(float)
				slice.relerror = relerrorList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureexp0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'value', i, self.value)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'abserror', i, self.abserror)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'relerror', i, self.relerror)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureexp0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureexp0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class codeparamstructurecodeparam:
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
