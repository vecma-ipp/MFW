# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class magdiag(KeepInOrder):
	'''
	class magdiag
	Magnetic diagnostics. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- ip : class ipstructureexp0D
	   Plasma current [A]. Positive sign means anti-clockwise when viewed from above.  Time-dependent. Scalar
	- diamagflux : class diamagfluxstructureexp0D
	   Diamagnetic flux [Wb]; Time-dependent; Scalar
	- diamagener : class diamagenerstructureexp0D
	   Diamagnetic energy [J]; Time-dependent; Scalar
	- flux_loops : class flux_loopsstructureflux_loops
	   Poloidal flux loops RZ coordinates have 1 component for the full loop and two if there is a negative reference loop
	- bpol_probes : class bpol_probesstructurebpol_probes
	   Poloidal field probes
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'magdiag'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 1
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.ip = ipstructureexp0D('ip')
		self.diamagflux = diamagfluxstructureexp0D('diamagflux')
		self.diamagener = diamagenerstructureexp0D('diamagener')
		self.flux_loops = flux_loopsstructureflux_loops('flux_loops')
		self.bpol_probes = bpol_probesstructurebpol_probes('bpol_probes')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class magdiag\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute ip\n ' + self.ip.__str__(depth+1)
		ret = ret + space + 'Attribute diamagflux\n ' + self.diamagflux.__str__(depth+1)
		ret = ret + space + 'Attribute diamagener\n ' + self.diamagener.__str__(depth+1)
		ret = ret + space + 'Attribute flux_loops\n ' + self.flux_loops.__str__(depth+1)
		ret = ret + space + 'Attribute bpol_probes\n ' + self.bpol_probes.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.ip.setExpIdx(idx)
		self.diamagflux.setExpIdx(idx)
		self.diamagener.setExpIdx(idx)
		self.flux_loops.setExpIdx(idx)
		self.bpol_probes.setExpIdx(idx)
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
		self.ip.cpoTime = self.cpoTime
		self.ip.putSlice(path, cpopath)
		self.diamagflux.cpoTime = self.cpoTime
		self.diamagflux.putSlice(path, cpopath)
		self.diamagener.cpoTime = self.cpoTime
		self.diamagener.putSlice(path, cpopath)
		self.flux_loops.cpoTime = self.cpoTime
		self.flux_loops.putSlice(path, cpopath)
		self.bpol_probes.cpoTime = self.cpoTime
		self.bpol_probes.putSlice(path, cpopath)
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
		self.ip.replaceLastSlice(path, cpopath)
		self.diamagflux.replaceLastSlice(path, cpopath)
		self.diamagener.replaceLastSlice(path, cpopath)
		self.flux_loops.replaceLastSlice(path, cpopath)
		self.bpol_probes.replaceLastSlice(path, cpopath)
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
		self.ip.putNonTimed(path, cpopath)
		self.diamagflux.putNonTimed(path, cpopath)
		self.diamagener.putNonTimed(path, cpopath)
		self.flux_loops.putNonTimed(path, cpopath)
		self.bpol_probes.putNonTimed(path, cpopath)
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
		self.ip.getSlice(path, cpopath, inTime, interpolMode)
		self.diamagflux.getSlice(path, cpopath, inTime, interpolMode)
		self.diamagener.getSlice(path, cpopath, inTime, interpolMode)
		self.flux_loops.getSlice(path, cpopath, inTime, interpolMode)
		self.bpol_probes.getSlice(path, cpopath, inTime, interpolMode)
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
			ipList = self.ip.build_non_resampled_data(path, cpopath, nbslice)
			diamagfluxList = self.diamagflux.build_non_resampled_data(path, cpopath, nbslice)
			diamagenerList = self.diamagener.build_non_resampled_data(path, cpopath, nbslice)
			flux_loopsList = self.flux_loops.build_non_resampled_data(path, cpopath, nbslice)
			bpol_probesList = self.bpol_probes.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = magdiag()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.ip = ipList[i]
				slice.diamagflux = diamagfluxList[i]
				slice.diamagener = diamagenerList[i]
				slice.flux_loops = flux_loopsList[i]
				slice.bpol_probes = bpol_probesList[i]
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
		self.ip.deleteData(path, cpopath)
		self.diamagflux.deleteData(path, cpopath)
		self.diamagener.deleteData(path, cpopath)
		self.flux_loops.deleteData(path, cpopath)
		self.bpol_probes.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class magdiagArray:
	'''
	class magdiagArray
	Magnetic diagnostics. Time-dependent CPO

	Attributes:
	- array : list of magdiag
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
		ret = space + 'class magdiagArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'magdiag cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = magdiag()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(magdiag())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = magdiag()
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


class ipstructureexp0D(KeepInOrder):
	'''
	class ipstructureexp0D
	Plasma current [A]. Positive sign means anti-clockwise when viewed from above.  Time-dependent. Scalar

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='ip'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ipstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ipstructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type ipstructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type ipstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type ipstructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type ipstructureexp0D, run function build_non_resampled_data') 
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
				slice = ipstructureexp0D(self.base_path)
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
			print ('object of type ipstructureexp0DObj, run function putTimedElt') 
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
			print ('object of type ipstructureexp0DObj, run function getTimedElt') 
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
			print ('object of type ipstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ipstructureexp0DObj, run function getNonTimedElt') 
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


class diamagfluxstructureexp0D(KeepInOrder):
	'''
	class diamagfluxstructureexp0D
	Diamagnetic flux [Wb]; Time-dependent; Scalar

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='diamagflux'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class diamagfluxstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type diamagfluxstructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type diamagfluxstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type diamagfluxstructureexp0D, run function build_non_resampled_data') 
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
				slice = diamagfluxstructureexp0D(self.base_path)
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
			print ('object of type diamagfluxstructureexp0DObj, run function putTimedElt') 
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
			print ('object of type diamagfluxstructureexp0DObj, run function getTimedElt') 
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
			print ('object of type diamagfluxstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type diamagfluxstructureexp0DObj, run function getNonTimedElt') 
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


class diamagenerstructureexp0D(KeepInOrder):
	'''
	class diamagenerstructureexp0D
	Diamagnetic energy [J]; Time-dependent; Scalar

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='diamagener'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class diamagenerstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type diamagenerstructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type diamagenerstructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type diamagenerstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type diamagenerstructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type diamagenerstructureexp0D, run function build_non_resampled_data') 
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
				slice = diamagenerstructureexp0D(self.base_path)
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
			print ('object of type diamagenerstructureexp0DObj, run function putTimedElt') 
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
			print ('object of type diamagenerstructureexp0DObj, run function getTimedElt') 
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
			print ('object of type diamagenerstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type diamagenerstructureexp0DObj, run function getNonTimedElt') 
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


class flux_loopsstructureflux_loops(KeepInOrder):
	'''
	class flux_loopsstructureflux_loops
	Poloidal flux loops RZ coordinates have 1 component for the full loop and two if there is a negative reference loop

	Attributes:
	- setup_floops : class setup_floopsstructuresetup_floops
	   diagnostic setup information
	- measure : class measurestructureexp1D
	   Measured flux [Wb]; Time-dependent; Vector (nloops)
	'''

	def __init__(self, base_path_in='flux_loops'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.setup_floops = setup_floopsstructuresetup_floops('setup_floops')
		self.measure = measurestructureexp1D('measure')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class flux_loopsstructureflux_loops\n'
		ret = ret + space + 'Attribute setup_floops\n ' + self.setup_floops.__str__(depth+1)
		ret = ret + space + 'Attribute measure\n ' + self.measure.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.setup_floops.setExpIdx(idx)
		self.measure.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type flux_loopsstructureflux_loops, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_floops.cpoTime = self.cpoTime
		self.setup_floops.putSlice(path, cpopath)
		self.measure.cpoTime = self.cpoTime
		self.measure.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type flux_loopsstructureflux_loops, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_floops.replaceLastSlice(path, cpopath)
		self.measure.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type flux_loopsstructureflux_loops, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_floops.putNonTimed(path, cpopath)
		self.measure.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type flux_loopsstructureflux_loops, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_floops.getSlice(path, cpopath, inTime, interpolMode)
		self.measure.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type flux_loopsstructureflux_loops, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			setup_floopsList = self.setup_floops.build_non_resampled_data(path, cpopath, nbslice)
			measureList = self.measure.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = flux_loopsstructureflux_loops(self.base_path)
				slice.setExpIdx(self.idx)
				slice.setup_floops = setup_floopsList[i]
				slice.measure = measureList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flux_loopsstructureflux_loopsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.measure.putTimedElt(path, cpopath + 'measure', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flux_loopsstructureflux_loopsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.measure.getTimedElt(path, cpopath + 'measure', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flux_loopsstructureflux_loopsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.setup_floops.putNonTimedElt(path, cpopath + 'setup_floops', i, obj)
		obj = self.measure.putNonTimedElt(path, cpopath + 'measure', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flux_loopsstructureflux_loopsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.setup_floops.getNonTimedElt(path, cpopath + 'setup_floops', i, obj)
		self.measure.getNonTimedElt(path, cpopath + 'measure', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_floops.deleteData(path, cpopath)
		self.measure.deleteData(path, cpopath)


class setup_floopsstructuresetup_floops(KeepInOrder):
	'''
	class setup_floopsstructuresetup_floops
	diagnostic setup information

	Attributes:
	- name : list of str
	   Name of loop. Array of strings (nloops).
	- id : list of str
	   ID of loop. Array of strings (nloops).
	- position : class positionstructurerzphi2D
	   List of (R,Z,phi) points defining the position of the loop (see data structure documentation FLUXLOOPposition.pdf); Matrices (nloops, max_npoints)
	- npoints : numpy.ndarray 1D with int)
	   Number of points describing each loop in the "position" matrices. Vector (nloops)
	'''

	def __init__(self, base_path_in='setup_floops'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.id = ['']
		self.position = positionstructurerzphi2D('position')
		self.npoints = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class setup_floopsstructuresetup_floops\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.id.__str__()
		ret = ret + space + 'Attribute id\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		s = self.npoints.__str__()
		ret = ret + space + 'Attribute npoints\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setup_floopsstructuresetup_floops, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setup_floopsstructuresetup_floops, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setup_floopsstructuresetup_floops, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'id', self.id, False)
		check_status(status)
		self.position.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'npoints', numpy.array(self.npoints).astype(numpy.int32), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type setup_floopsstructuresetup_floops, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_id = ull.getVect1DString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_npoints = ull.getVect1DInt(self.idx, path, cpopath + 'npoints')
		check_status(status)
		if not status:
			self.npoints = ret_npoints

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type setup_floopsstructuresetup_floops, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, idVal = ull.getVect1DString(self.idx, path, cpopath + 'id')
			check_status(status)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, npointsVal = ull.getVect1DInt(self.idx, path, cpopath + 'npoints')
			check_status(status)
			for i in range(nbslice):
				slice = setup_floopsstructuresetup_floops(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.id = idVal
				slice.position = positionList[i]
				slice.npoints = npointsVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_floopsstructuresetup_floopsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_floopsstructuresetup_floopsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_floopsstructuresetup_floopsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'npoints', i, numpy.array(self.npoints).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_floopsstructuresetup_floopsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		status, ret_npoints = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'npoints', i)
		check_status(status)
		if not status:
			self.npoints = ret_npoints

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'id')
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'npoints')


class positionstructurerzphi2D(KeepInOrder):
	'''
	class positionstructurerzphi2D
	List of (R,Z,phi) points defining the position of the loop (see data structure documentation FLUXLOOPposition.pdf); Matrices (nloops, max_npoints)

	Attributes:
	- r : numpy.ndarray 2D with float
	   Major radius [m]
	- z : numpy.ndarray 2D with float
	   Altitude [m]
	- phi : numpy.ndarray 2D with float
	   Toroidal angle [rad]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')
		self.phi = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerzphi2D\n'
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
			print ('field '+self.base_path+' of type positionstructurerzphi2D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi2D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi2D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi2D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getVect2DDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_z = ull.getVect2DDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z
		status, ret_phi = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
		check_status(status)
		if not status:
			self.phi = ret_phi

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi2D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getVect2DDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, zVal = ull.getVect2DDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			status, phiVal = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerzphi2D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				slice.phi = phiVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi2DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi2DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi2DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi2DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi

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


class measurestructureexp1D(KeepInOrder):
	'''
	class measurestructureexp1D
	Measured flux [Wb]; Time-dependent; Vector (nloops)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='measure'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class measurestructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type measurestructureexp1D, run function getSlice') 
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
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type measurestructureexp1D, run function build_non_resampled_data') 
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
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = measurestructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestructureexp1DObj, run function getNonTimedElt') 
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


class bpol_probesstructurebpol_probes(KeepInOrder):
	'''
	class bpol_probesstructurebpol_probes
	Poloidal field probes

	Attributes:
	- setup_bprobe : class setup_bprobestructuresetup_bprobe
	   diagnostic setup information
	- measure : class measurestructureexp1D
	   Measured value [T]; Time-dependent; Vector (nprobes)
	'''

	def __init__(self, base_path_in='bpol_probes'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.setup_bprobe = setup_bprobestructuresetup_bprobe('setup_bprobe')
		self.measure = measurestructureexp1D('measure')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bpol_probesstructurebpol_probes\n'
		ret = ret + space + 'Attribute setup_bprobe\n ' + self.setup_bprobe.__str__(depth+1)
		ret = ret + space + 'Attribute measure\n ' + self.measure.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.setup_bprobe.setExpIdx(idx)
		self.measure.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpol_probesstructurebpol_probes, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_bprobe.cpoTime = self.cpoTime
		self.setup_bprobe.putSlice(path, cpopath)
		self.measure.cpoTime = self.cpoTime
		self.measure.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpol_probesstructurebpol_probes, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_bprobe.replaceLastSlice(path, cpopath)
		self.measure.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpol_probesstructurebpol_probes, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_bprobe.putNonTimed(path, cpopath)
		self.measure.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bpol_probesstructurebpol_probes, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_bprobe.getSlice(path, cpopath, inTime, interpolMode)
		self.measure.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bpol_probesstructurebpol_probes, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			setup_bprobeList = self.setup_bprobe.build_non_resampled_data(path, cpopath, nbslice)
			measureList = self.measure.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = bpol_probesstructurebpol_probes(self.base_path)
				slice.setExpIdx(self.idx)
				slice.setup_bprobe = setup_bprobeList[i]
				slice.measure = measureList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpol_probesstructurebpol_probesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.measure.putTimedElt(path, cpopath + 'measure', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpol_probesstructurebpol_probesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.measure.getTimedElt(path, cpopath + 'measure', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpol_probesstructurebpol_probesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.setup_bprobe.putNonTimedElt(path, cpopath + 'setup_bprobe', i, obj)
		obj = self.measure.putNonTimedElt(path, cpopath + 'measure', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpol_probesstructurebpol_probesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.setup_bprobe.getNonTimedElt(path, cpopath + 'setup_bprobe', i, obj)
		self.measure.getNonTimedElt(path, cpopath + 'measure', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.setup_bprobe.deleteData(path, cpopath)
		self.measure.deleteData(path, cpopath)


class setup_bprobestructuresetup_bprobe(KeepInOrder):
	'''
	class setup_bprobestructuresetup_bprobe
	diagnostic setup information

	Attributes:
	- name : list of str
	   Name of the probe.  Array of strings (nprobes).
	- id : list of str
	   ID of the probe.  Array of strings (nprobes).
	- position : class positionstructurerz1D
	   RZ of coil centre [m]; Vector (nprobes)
	- polangle : numpy.ndarray 1D with float
	   Poloidal angle of coil orientation (w.r.t. horizontal ?? to be checked) [rad]; Vector (nprobes)
	- torangle : numpy.ndarray 1D with float
	   Toroidal angle of coil orientation (0 if fully in the poloidal plane) [rad] ; Vector (nprobes)
	- area : numpy.ndarray 1D with float
	   Area of coil [m^2]; Vector (nprobes)
	- length : numpy.ndarray 1D with float
	   Length of coil [m]; Vector (nprobes)
	- turns : numpy.ndarray 1D with int)
	   Turns in the coil; Vector (nprobes)
	'''

	def __init__(self, base_path_in='setup_bprobe'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.id = ['']
		self.position = positionstructurerz1D('position')
		self.polangle = numpy.zeros(0, numpy.float64, order='C')
		self.torangle = numpy.zeros(0, numpy.float64, order='C')
		self.area = numpy.zeros(0, numpy.float64, order='C')
		self.length = numpy.zeros(0, numpy.float64, order='C')
		self.turns = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class setup_bprobestructuresetup_bprobe\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.id.__str__()
		ret = ret + space + 'Attribute id\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		s = self.polangle.__str__()
		ret = ret + space + 'Attribute polangle\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torangle.__str__()
		ret = ret + space + 'Attribute torangle\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.area.__str__()
		ret = ret + space + 'Attribute area\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.length.__str__()
		ret = ret + space + 'Attribute length\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.turns.__str__()
		ret = ret + space + 'Attribute turns\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setup_bprobestructuresetup_bprobe, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setup_bprobestructuresetup_bprobe, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setup_bprobestructuresetup_bprobe, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'id', self.id, False)
		check_status(status)
		self.position.putNonTimed(path, cpopath)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'polangle', numpy.array(self.polangle).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'torangle', numpy.array(self.torangle).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'length', numpy.array(self.length).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'turns', numpy.array(self.turns).astype(numpy.int32), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type setup_bprobestructuresetup_bprobe, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_id = ull.getVect1DString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_polangle = ull.getVect1DDouble(self.idx, path, cpopath + 'polangle')
		check_status(status)
		if not status:
			self.polangle = ret_polangle
		status, ret_torangle = ull.getVect1DDouble(self.idx, path, cpopath + 'torangle')
		check_status(status)
		if not status:
			self.torangle = ret_torangle
		status, ret_area = ull.getVect1DDouble(self.idx, path, cpopath + 'area')
		check_status(status)
		if not status:
			self.area = ret_area
		status, ret_length = ull.getVect1DDouble(self.idx, path, cpopath + 'length')
		check_status(status)
		if not status:
			self.length = ret_length
		status, ret_turns = ull.getVect1DInt(self.idx, path, cpopath + 'turns')
		check_status(status)
		if not status:
			self.turns = ret_turns

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type setup_bprobestructuresetup_bprobe, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, idVal = ull.getVect1DString(self.idx, path, cpopath + 'id')
			check_status(status)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, polangleVal = ull.getVect1DDouble(self.idx, path, cpopath + 'polangle')
			check_status(status)
			status, torangleVal = ull.getVect1DDouble(self.idx, path, cpopath + 'torangle')
			check_status(status)
			status, areaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'area')
			check_status(status)
			status, lengthVal = ull.getVect1DDouble(self.idx, path, cpopath + 'length')
			check_status(status)
			status, turnsVal = ull.getVect1DInt(self.idx, path, cpopath + 'turns')
			check_status(status)
			for i in range(nbslice):
				slice = setup_bprobestructuresetup_bprobe(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.id = idVal
				slice.position = positionList[i]
				slice.polangle = polangleVal
				slice.torangle = torangleVal
				slice.area = areaVal
				slice.length = lengthVal
				slice.turns = turnsVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_bprobestructuresetup_bprobeObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_bprobestructuresetup_bprobeObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_bprobestructuresetup_bprobeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'polangle') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'polangle', i, numpy.array(self.polangle).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torangle') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torangle', i, numpy.array(self.torangle).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'area', i, numpy.array(self.area).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'length') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'length', i, numpy.array(self.length).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'turns') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'turns', i, numpy.array(self.turns).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setup_bprobestructuresetup_bprobeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'polangle') 
			print ('obj = ' + str(obj))
		status, ret_polangle = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'polangle', i)
		check_status(status)
		if not status:
			self.polangle = ret_polangle
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torangle') 
			print ('obj = ' + str(obj))
		status, ret_torangle = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torangle', i)
		check_status(status)
		if not status:
			self.torangle = ret_torangle
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		status, ret_area = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'area', i)
		check_status(status)
		if not status:
			self.area = ret_area
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'length') 
			print ('obj = ' + str(obj))
		status, ret_length = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'length', i)
		check_status(status)
		if not status:
			self.length = ret_length
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'turns') 
			print ('obj = ' + str(obj))
		status, ret_turns = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'turns', i)
		check_status(status)
		if not status:
			self.turns = ret_turns

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'id')
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'polangle')
		ull.deleteData(self.idx, path, cpopath + 'torangle')
		ull.deleteData(self.idx, path, cpopath + 'area')
		ull.deleteData(self.idx, path, cpopath + 'length')
		ull.deleteData(self.idx, path, cpopath + 'turns')


class positionstructurerz1D(KeepInOrder):
	'''
	class positionstructurerz1D
	RZ of coil centre [m]; Vector (nprobes)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type positionstructurerz1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type positionstructurerz1D, run function build_non_resampled_data') 
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
				slice = positionstructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function putNonTimedElt') 
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
			print ('object of type positionstructurerz1DObj, run function getNonTimedElt') 
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
