# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class launchs:
	'''
	class launchs
	RF wave launch conditions. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- name : list of str
	   Antenna name, Vector of strings (nantenna)
	- type : list of str
	   Wave type (LH, EC, IC, ...), Vector of strings (nantenna)
	- frequency : numpy.ndarray 1D with float
	   Wave frequency [Hz], Vector (nantenna).
	- mode : numpy.ndarray 1D with int)
	   Incoming wave mode (+ 1 : slow wave only; -1 both slow and fast wave modes). Vector of integers (nantenna). Time-dependent
	- position : class positionstructurerzphi1D
	   Reference global position of the antenna. Time-dependent
	- spectrum : class spectrumstructurespectrum
	   Spectral properties of the wave.
	- beam : class beamstructurelaunchs_rfbeam
	   Beam characteristics
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'launchs'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 5
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.name = ['']
		self.type = ['']
		self.frequency = numpy.zeros(0, numpy.float64, order='C')
		self.mode = numpy.zeros(0, numpy.int32, order='C')
		self.position = positionstructurerzphi1D('position')
		self.spectrum = spectrumstructurespectrum('spectrum')
		self.beam = beamstructurelaunchs_rfbeam('beam')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class launchs\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.type.__str__()
		ret = ret + space + 'Attribute type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.frequency.__str__()
		ret = ret + space + 'Attribute frequency\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mode.__str__()
		ret = ret + space + 'Attribute mode\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute spectrum\n ' + self.spectrum.__str__(depth+1)
		ret = ret + space + 'Attribute beam\n ' + self.beam.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.position.setExpIdx(idx)
		self.spectrum.setExpIdx(idx)
		self.beam.setExpIdx(idx)
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
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'mode', numpy.array(self.mode).astype(numpy.int32), self.cpoTime)
		check_status(status)
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)
		self.spectrum.cpoTime = self.cpoTime
		self.spectrum.putSlice(path, cpopath)
		self.beam.cpoTime = self.cpoTime
		self.beam.putSlice(path, cpopath)
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
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'mode', numpy.array(self.mode).astype(numpy.int32))
		check_status(status)
		self.position.replaceLastSlice(path, cpopath)
		self.spectrum.replaceLastSlice(path, cpopath)
		self.beam.replaceLastSlice(path, cpopath)
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
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'type', self.type, False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'frequency', numpy.array(self.frequency).astype(numpy.float64), False)
		check_status(status)
		self.position.putNonTimed(path, cpopath)
		self.spectrum.putNonTimed(path, cpopath)
		self.beam.putNonTimed(path, cpopath)
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
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_type = ull.getVect1DString(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_frequency = ull.getVect1DDouble(self.idx, path, cpopath + 'frequency')
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		status, ret_mode, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'mode', inTime, interpolMode)
		check_status(status)
		if not status:
			self.mode = ret_mode
			self.cpoTime = retTime
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		self.spectrum.getSlice(path, cpopath, inTime, interpolMode)
		self.beam.getSlice(path, cpopath, inTime, interpolMode)
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
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, typeVal = ull.getVect1DString(self.idx, path, cpopath + 'type')
			check_status(status)
			status, frequencyVal = ull.getVect1DDouble(self.idx, path, cpopath + 'frequency')
			check_status(status)
			status, modeList = ull.getVect2DInt(self.idx, path, cpopath + 'mode')
			if len(modeList) == 0:
				modeList = numpy.resize(modeList, (0,nbslice))
			check_status(status)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			spectrumList = self.spectrum.build_non_resampled_data(path, cpopath, nbslice)
			beamList = self.beam.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = launchs()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.name = nameVal
				slice.type = typeVal
				slice.frequency = frequencyVal
				slice.mode = modeList[:,i]
				slice.position = positionList[i]
				slice.spectrum = spectrumList[i]
				slice.beam = beamList[i]
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
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'frequency')
		ull.deleteData(self.idx, path, cpopath + 'mode')
		self.position.deleteData(path, cpopath)
		self.spectrum.deleteData(path, cpopath)
		self.beam.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class launchsArray:
	'''
	class launchsArray
	RF wave launch conditions. Time-dependent CPO

	Attributes:
	- array : list of launchs
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
		ret = space + 'class launchsArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'launchs cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = launchs()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(launchs())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = launchs()
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


class positionstructurerzphi1D:
	'''
	class positionstructurerzphi1D
	Reference global position of the antenna. Time-dependent

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	- phi : numpy.ndarray 1D with float
	   Toroidal angle [rad]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerzphi1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime
		status, ret_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect2DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,nbslice))
			check_status(status)
			status, zList = ull.getVect2DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,nbslice))
			check_status(status)
			status, phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerzphi1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.phi = phiList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function getTimedElt') 
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
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'phi')


class spectrumstructurespectrum:
	'''
	class spectrumstructurespectrum
	Spectral properties of the wave.

	Attributes:
	- phi_theta : class phi_thetastructurelaunchs_phi_theta
	   Power spectrum as a function of the refractive index in the toroidal and poloidal directions.
	- parallel : class parallelstructurelaunchs_parallel
	   Power spectrum as a function of the parallel refractive index.
	'''

	def __init__(self, base_path_in='spectrum'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.phi_theta = phi_thetastructurelaunchs_phi_theta('phi_theta')
		self.parallel = parallelstructurelaunchs_parallel('parallel')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spectrumstructurespectrum\n'
		ret = ret + space + 'Attribute phi_theta\n ' + self.phi_theta.__str__(depth+1)
		ret = ret + space + 'Attribute parallel\n ' + self.parallel.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.phi_theta.setExpIdx(idx)
		self.parallel.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spectrumstructurespectrum, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.phi_theta.cpoTime = self.cpoTime
		self.phi_theta.putSlice(path, cpopath)
		self.parallel.cpoTime = self.cpoTime
		self.parallel.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spectrumstructurespectrum, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.phi_theta.replaceLastSlice(path, cpopath)
		self.parallel.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spectrumstructurespectrum, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.phi_theta.putNonTimed(path, cpopath)
		self.parallel.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type spectrumstructurespectrum, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.phi_theta.getSlice(path, cpopath, inTime, interpolMode)
		self.parallel.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type spectrumstructurespectrum, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			phi_thetaList = self.phi_theta.build_non_resampled_data(path, cpopath, nbslice)
			parallelList = self.parallel.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = spectrumstructurespectrum(self.base_path)
				slice.setExpIdx(self.idx)
				slice.phi_theta = phi_thetaList[i]
				slice.parallel = parallelList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spectrumstructurespectrumObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.phi_theta.putTimedElt(path, cpopath + 'phi_theta', i, obj)
		obj = self.parallel.putTimedElt(path, cpopath + 'parallel', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spectrumstructurespectrumObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.phi_theta.getTimedElt(path, cpopath + 'phi_theta', i, obj)
		self.parallel.getTimedElt(path, cpopath + 'parallel', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spectrumstructurespectrumObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.phi_theta.putNonTimedElt(path, cpopath + 'phi_theta', i, obj)
		obj = self.parallel.putNonTimedElt(path, cpopath + 'parallel', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spectrumstructurespectrumObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.phi_theta.getNonTimedElt(path, cpopath + 'phi_theta', i, obj)
		self.parallel.getNonTimedElt(path, cpopath + 'parallel', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.phi_theta.deleteData(path, cpopath)
		self.parallel.deleteData(path, cpopath)


class phi_thetastructurelaunchs_phi_theta:
	'''
	class phi_thetastructurelaunchs_phi_theta
	Power spectrum as a function of the refractive index in the toroidal and poloidal directions.

	Attributes:
	- nn_phi : numpy.ndarray 1D with int)
	   Number of points for the discretization of the spectrum in the toroidal direction, Vector of integers (nantenna).
	- nn_theta : numpy.ndarray 1D with int)
	   Number of points for the discretization of the spectrum in the poloidal direction, Vector of integers (nantenna).
	- n_phi : numpy.ndarray 2D with float
	   Refraction index in the toroidal direction, Matrix (nantenna,max_nn_phi).
	- n_theta : numpy.ndarray 2D with float
	   Refraction index in poloidal direction, Matrix (nantenna,max_nn_theta).
	- power : numpy.ndarray 3D with float
	   W/dNphi/dNtheta [W], Array (nantenna, max_nn_phi, max_nn_theta). Time-dependent
	'''

	def __init__(self, base_path_in='phi_theta'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nn_phi = numpy.zeros(0, numpy.int32, order='C')
		self.nn_theta = numpy.zeros(0, numpy.int32, order='C')
		self.n_phi = numpy.zeros((0,0), numpy.float64, order='C')
		self.n_theta = numpy.zeros((0,0), numpy.float64, order='C')
		self.power = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class phi_thetastructurelaunchs_phi_theta\n'
		s = self.nn_phi.__str__()
		ret = ret + space + 'Attribute nn_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.nn_theta.__str__()
		ret = ret + space + 'Attribute nn_theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.n_phi.__str__()
		ret = ret + space + 'Attribute n_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.n_theta.__str__()
		ret = ret + space + 'Attribute n_theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power.__str__()
		ret = ret + space + 'Attribute power\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phi_thetastructurelaunchs_phi_theta, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'power', numpy.array(self.power).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phi_thetastructurelaunchs_phi_theta, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'power', numpy.array(self.power).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phi_thetastructurelaunchs_phi_theta, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DInt(self.idx, path, cpopath + 'nn_phi', numpy.array(self.nn_phi).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'nn_theta', numpy.array(self.nn_theta).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'n_phi', numpy.array(self.n_phi).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'n_theta', numpy.array(self.n_theta).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type phi_thetastructurelaunchs_phi_theta, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nn_phi = ull.getVect1DInt(self.idx, path, cpopath + 'nn_phi')
		check_status(status)
		if not status:
			self.nn_phi = ret_nn_phi
		status, ret_nn_theta = ull.getVect1DInt(self.idx, path, cpopath + 'nn_theta')
		check_status(status)
		if not status:
			self.nn_theta = ret_nn_theta
		status, ret_n_phi = ull.getVect2DDouble(self.idx, path, cpopath + 'n_phi')
		check_status(status)
		if not status:
			self.n_phi = ret_n_phi
		status, ret_n_theta = ull.getVect2DDouble(self.idx, path, cpopath + 'n_theta')
		check_status(status)
		if not status:
			self.n_theta = ret_n_theta
		status, ret_power, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'power', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power = ret_power
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type phi_thetastructurelaunchs_phi_theta, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nn_phiVal = ull.getVect1DInt(self.idx, path, cpopath + 'nn_phi')
			check_status(status)
			status, nn_thetaVal = ull.getVect1DInt(self.idx, path, cpopath + 'nn_theta')
			check_status(status)
			status, n_phiVal = ull.getVect2DDouble(self.idx, path, cpopath + 'n_phi')
			check_status(status)
			status, n_thetaVal = ull.getVect2DDouble(self.idx, path, cpopath + 'n_theta')
			check_status(status)
			status, powerList = ull.getVect4DDouble(self.idx, path, cpopath + 'power')
			if len(powerList) == 0:
				powerList = numpy.resize(powerList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = phi_thetastructurelaunchs_phi_theta(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nn_phi = nn_phiVal
				slice.nn_theta = nn_thetaVal
				slice.n_phi = n_phiVal
				slice.n_theta = n_thetaVal
				slice.power = powerList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phi_thetastructurelaunchs_phi_thetaObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'power') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'power', i, numpy.array(self.power).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phi_thetastructurelaunchs_phi_thetaObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'power') 
			print ('obj = ' + str(obj))
		status, ret_power = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'power', i)
		check_status(status)
		if not status:
			self.power = ret_power

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phi_thetastructurelaunchs_phi_thetaObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'nn_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'nn_phi', i, numpy.array(self.nn_phi).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'nn_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'nn_theta', i, numpy.array(self.nn_theta).astype(numpy.int32))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'n_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'n_phi', i, numpy.array(self.n_phi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'n_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'n_theta', i, numpy.array(self.n_theta).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phi_thetastructurelaunchs_phi_thetaObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'nn_phi') 
			print ('obj = ' + str(obj))
		status, ret_nn_phi = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'nn_phi', i)
		check_status(status)
		if not status:
			self.nn_phi = ret_nn_phi
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'nn_theta') 
			print ('obj = ' + str(obj))
		status, ret_nn_theta = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'nn_theta', i)
		check_status(status)
		if not status:
			self.nn_theta = ret_nn_theta
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'n_phi') 
			print ('obj = ' + str(obj))
		status, ret_n_phi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'n_phi', i)
		check_status(status)
		if not status:
			self.n_phi = ret_n_phi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'n_theta') 
			print ('obj = ' + str(obj))
		status, ret_n_theta = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'n_theta', i)
		check_status(status)
		if not status:
			self.n_theta = ret_n_theta

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nn_phi')
		ull.deleteData(self.idx, path, cpopath + 'nn_theta')
		ull.deleteData(self.idx, path, cpopath + 'n_phi')
		ull.deleteData(self.idx, path, cpopath + 'n_theta')
		ull.deleteData(self.idx, path, cpopath + 'power')


class parallelstructurelaunchs_parallel:
	'''
	class parallelstructurelaunchs_parallel
	Power spectrum as a function of the parallel refractive index.

	Attributes:
	- nn_par : numpy.ndarray 1D with int)
	   Number of points for the discretization of the spectrum in the poloidal direction, Vector of integers (nantenna).
	- n_par : numpy.ndarray 2D with float
	   Refraction index in the parallel direction, Matrix (nantenna,max_nn_par).
	- power : numpy.ndarray 1D with float
	   W/dN_par [W], Matrix(nantenna, max_nn_par). Time-dependent
	'''

	def __init__(self, base_path_in='parallel'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nn_par = numpy.zeros(0, numpy.int32, order='C')
		self.n_par = numpy.zeros((0,0), numpy.float64, order='C')
		self.power = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class parallelstructurelaunchs_parallel\n'
		s = self.nn_par.__str__()
		ret = ret + space + 'Attribute nn_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.n_par.__str__()
		ret = ret + space + 'Attribute n_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power.__str__()
		ret = ret + space + 'Attribute power\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parallelstructurelaunchs_parallel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'power', numpy.array(self.power).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parallelstructurelaunchs_parallel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'power', numpy.array(self.power).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parallelstructurelaunchs_parallel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DInt(self.idx, path, cpopath + 'nn_par', numpy.array(self.nn_par).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'n_par', numpy.array(self.n_par).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type parallelstructurelaunchs_parallel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nn_par = ull.getVect1DInt(self.idx, path, cpopath + 'nn_par')
		check_status(status)
		if not status:
			self.nn_par = ret_nn_par
		status, ret_n_par = ull.getVect2DDouble(self.idx, path, cpopath + 'n_par')
		check_status(status)
		if not status:
			self.n_par = ret_n_par
		status, ret_power, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'power', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power = ret_power
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type parallelstructurelaunchs_parallel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nn_parVal = ull.getVect1DInt(self.idx, path, cpopath + 'nn_par')
			check_status(status)
			status, n_parVal = ull.getVect2DDouble(self.idx, path, cpopath + 'n_par')
			check_status(status)
			status, powerList = ull.getVect2DDouble(self.idx, path, cpopath + 'power')
			if len(powerList) == 0:
				powerList = numpy.resize(powerList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = parallelstructurelaunchs_parallel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nn_par = nn_parVal
				slice.n_par = n_parVal
				slice.power = powerList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parallelstructurelaunchs_parallelObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power', i, numpy.array(self.power).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parallelstructurelaunchs_parallelObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power') 
			print ('obj = ' + str(obj))
		status, ret_power = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power', i)
		check_status(status)
		if not status:
			self.power = ret_power

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parallelstructurelaunchs_parallelObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'nn_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'nn_par', i, numpy.array(self.nn_par).astype(numpy.int32))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'n_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'n_par', i, numpy.array(self.n_par).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parallelstructurelaunchs_parallelObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'nn_par') 
			print ('obj = ' + str(obj))
		status, ret_nn_par = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'nn_par', i)
		check_status(status)
		if not status:
			self.nn_par = ret_nn_par
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'n_par') 
			print ('obj = ' + str(obj))
		status, ret_n_par = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'n_par', i)
		check_status(status)
		if not status:
			self.n_par = ret_n_par

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nn_par')
		ull.deleteData(self.idx, path, cpopath + 'n_par')
		ull.deleteData(self.idx, path, cpopath + 'power')


class beamstructurelaunchs_rfbeam:
	'''
	class beamstructurelaunchs_rfbeam
	Beam characteristics

	Attributes:
	- spot : class spotstructurelaunchs_rfbeam_spot
	   Spot characteristics
	- phaseellipse : class phaseellipsestructurelaunchs_rfbeam_phaseellipse
	   Phase ellipse characteristics of the spot
	'''

	def __init__(self, base_path_in='beam'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.spot = spotstructurelaunchs_rfbeam_spot('spot')
		self.phaseellipse = phaseellipsestructurelaunchs_rfbeam_phaseellipse('phaseellipse')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamstructurelaunchs_rfbeam\n'
		ret = ret + space + 'Attribute spot\n ' + self.spot.__str__(depth+1)
		ret = ret + space + 'Attribute phaseellipse\n ' + self.phaseellipse.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.spot.setExpIdx(idx)
		self.phaseellipse.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamstructurelaunchs_rfbeam, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spot.cpoTime = self.cpoTime
		self.spot.putSlice(path, cpopath)
		self.phaseellipse.cpoTime = self.cpoTime
		self.phaseellipse.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamstructurelaunchs_rfbeam, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spot.replaceLastSlice(path, cpopath)
		self.phaseellipse.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamstructurelaunchs_rfbeam, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spot.putNonTimed(path, cpopath)
		self.phaseellipse.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type beamstructurelaunchs_rfbeam, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spot.getSlice(path, cpopath, inTime, interpolMode)
		self.phaseellipse.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type beamstructurelaunchs_rfbeam, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			spotList = self.spot.build_non_resampled_data(path, cpopath, nbslice)
			phaseellipseList = self.phaseellipse.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = beamstructurelaunchs_rfbeam(self.base_path)
				slice.setExpIdx(self.idx)
				slice.spot = spotList[i]
				slice.phaseellipse = phaseellipseList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurelaunchs_rfbeamObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.spot.putTimedElt(path, cpopath + 'spot', i, obj)
		obj = self.phaseellipse.putTimedElt(path, cpopath + 'phaseellipse', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurelaunchs_rfbeamObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.spot.getTimedElt(path, cpopath + 'spot', i, obj)
		self.phaseellipse.getTimedElt(path, cpopath + 'phaseellipse', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurelaunchs_rfbeamObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.spot.putNonTimedElt(path, cpopath + 'spot', i, obj)
		obj = self.phaseellipse.putNonTimedElt(path, cpopath + 'phaseellipse', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurelaunchs_rfbeamObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.spot.getNonTimedElt(path, cpopath + 'spot', i, obj)
		self.phaseellipse.getNonTimedElt(path, cpopath + 'phaseellipse', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spot.deleteData(path, cpopath)
		self.phaseellipse.deleteData(path, cpopath)


class spotstructurelaunchs_rfbeam_spot:
	'''
	class spotstructurelaunchs_rfbeam_spot
	Spot characteristics

	Attributes:
	- waist : numpy.ndarray 2D with float
	   Waist for the spot ellipse [m], Matrix (nantenna,2). Time-dependent
	- angle : numpy.ndarray 1D with float
	   Rotation angle for the spot ellipse [rd], Vector(nantenna). Time-dependent
	'''

	def __init__(self, base_path_in='spot'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.waist = numpy.zeros((0,0), numpy.float64, order='C')
		self.angle = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spotstructurelaunchs_rfbeam_spot\n'
		s = self.waist.__str__()
		ret = ret + space + 'Attribute waist\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.angle.__str__()
		ret = ret + space + 'Attribute angle\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurelaunchs_rfbeam_spot, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'waist', numpy.array(self.waist).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'angle', numpy.array(self.angle).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurelaunchs_rfbeam_spot, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'waist', numpy.array(self.waist).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'angle', numpy.array(self.angle).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurelaunchs_rfbeam_spot, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurelaunchs_rfbeam_spot, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_waist, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'waist', inTime, interpolMode)
		check_status(status)
		if not status:
			self.waist = ret_waist
			self.cpoTime = retTime
		status, ret_angle, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'angle', inTime, interpolMode)
		check_status(status)
		if not status:
			self.angle = ret_angle
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurelaunchs_rfbeam_spot, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, waistList = ull.getVect3DDouble(self.idx, path, cpopath + 'waist')
			if len(waistList) == 0:
				waistList = numpy.resize(waistList, (0,0,nbslice))
			check_status(status)
			status, angleList = ull.getVect2DDouble(self.idx, path, cpopath + 'angle')
			if len(angleList) == 0:
				angleList = numpy.resize(angleList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = spotstructurelaunchs_rfbeam_spot(self.base_path)
				slice.setExpIdx(self.idx)
				slice.waist = waistList[:,:,i]
				slice.angle = angleList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurelaunchs_rfbeam_spotObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'waist') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'waist', i, numpy.array(self.waist).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'angle', i, numpy.array(self.angle).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurelaunchs_rfbeam_spotObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'waist') 
			print ('obj = ' + str(obj))
		status, ret_waist = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'waist', i)
		check_status(status)
		if not status:
			self.waist = ret_waist
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		status, ret_angle = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'angle', i)
		check_status(status)
		if not status:
			self.angle = ret_angle

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurelaunchs_rfbeam_spotObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurelaunchs_rfbeam_spotObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'waist')
		ull.deleteData(self.idx, path, cpopath + 'angle')


class phaseellipsestructurelaunchs_rfbeam_phaseellipse:
	'''
	class phaseellipsestructurelaunchs_rfbeam_phaseellipse
	Phase ellipse characteristics of the spot

	Attributes:
	- invcurvrad : numpy.ndarray 2D with float
	   Inverse curvature radii for the phase ellipse [m-1], Matrix (nantenna,2). Time-dependent
	- angle : numpy.ndarray 1D with float
	   Rotation angle for the phase ellipse [rd], Vector(nantenna). Time-dependent
	'''

	def __init__(self, base_path_in='phaseellipse'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.invcurvrad = numpy.zeros((0,0), numpy.float64, order='C')
		self.angle = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class phaseellipsestructurelaunchs_rfbeam_phaseellipse\n'
		s = self.invcurvrad.__str__()
		ret = ret + space + 'Attribute invcurvrad\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.angle.__str__()
		ret = ret + space + 'Attribute angle\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurelaunchs_rfbeam_phaseellipse, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'invcurvrad', numpy.array(self.invcurvrad).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'angle', numpy.array(self.angle).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurelaunchs_rfbeam_phaseellipse, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'invcurvrad', numpy.array(self.invcurvrad).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'angle', numpy.array(self.angle).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurelaunchs_rfbeam_phaseellipse, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurelaunchs_rfbeam_phaseellipse, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_invcurvrad, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'invcurvrad', inTime, interpolMode)
		check_status(status)
		if not status:
			self.invcurvrad = ret_invcurvrad
			self.cpoTime = retTime
		status, ret_angle, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'angle', inTime, interpolMode)
		check_status(status)
		if not status:
			self.angle = ret_angle
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurelaunchs_rfbeam_phaseellipse, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, invcurvradList = ull.getVect3DDouble(self.idx, path, cpopath + 'invcurvrad')
			if len(invcurvradList) == 0:
				invcurvradList = numpy.resize(invcurvradList, (0,0,nbslice))
			check_status(status)
			status, angleList = ull.getVect2DDouble(self.idx, path, cpopath + 'angle')
			if len(angleList) == 0:
				angleList = numpy.resize(angleList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = phaseellipsestructurelaunchs_rfbeam_phaseellipse(self.base_path)
				slice.setExpIdx(self.idx)
				slice.invcurvrad = invcurvradList[:,:,i]
				slice.angle = angleList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurelaunchs_rfbeam_phaseellipseObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'invcurvrad') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'invcurvrad', i, numpy.array(self.invcurvrad).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'angle', i, numpy.array(self.angle).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurelaunchs_rfbeam_phaseellipseObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'invcurvrad') 
			print ('obj = ' + str(obj))
		status, ret_invcurvrad = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'invcurvrad', i)
		check_status(status)
		if not status:
			self.invcurvrad = ret_invcurvrad
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		status, ret_angle = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'angle', i)
		check_status(status)
		if not status:
			self.angle = ret_angle

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurelaunchs_rfbeam_phaseellipseObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurelaunchs_rfbeam_phaseellipseObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'invcurvrad')
		ull.deleteData(self.idx, path, cpopath + 'angle')


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
