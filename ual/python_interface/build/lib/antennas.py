# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class antennas:
	'''
	class antennas
	Antenna systems for heating and current drive in the electron cyclotron (EC), ion cylcotron (IC) and lower hybrid (LH) frequencies. Time-dependent CPO.

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- antenna_ec : class antenna_ecstruct_arrayantenna_ec: array of antenna_ecstruct_arrayantenna_ecObj objects
	   Vector of Electron Cyclotron antennas. Time-dependent
	- antenna_ic : class antenna_icstruct_arrayantenna_ic: array of antenna_icstruct_arrayantenna_icObj objects
	   Vector of Ion Cyclotron antennas. Time-dependent
	- antenna_lh : class antenna_lhstruct_arrayantenna_lh: array of antenna_lhstruct_arrayantenna_lhObj objects
	   Vector of Lower Hybrid antennas. Time-dependent
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'antennas'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 5
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.antenna_ec = antenna_ecstruct_arrayantenna_ec('antenna_ec')
		self.antenna_ic = antenna_icstruct_arrayantenna_ic('antenna_ic')
		self.antenna_lh = antenna_lhstruct_arrayantenna_lh('antenna_lh')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class antennas\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute antenna_ec\n ' + self.antenna_ec.__str__(depth+1)
		ret = ret + space + 'Attribute antenna_ic\n ' + self.antenna_ic.__str__(depth+1)
		ret = ret + space + 'Attribute antenna_lh\n ' + self.antenna_lh.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.antenna_ec.setExpIdx(idx)
		self.antenna_ic.setExpIdx(idx)
		self.antenna_lh.setExpIdx(idx)
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
		self.antenna_ec.cpoTime = self.cpoTime
		self.antenna_ec.putSlice(path, cpopath)
		self.antenna_ic.cpoTime = self.cpoTime
		self.antenna_ic.putSlice(path, cpopath)
		self.antenna_lh.cpoTime = self.cpoTime
		self.antenna_lh.putSlice(path, cpopath)
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
		self.antenna_ec.replaceLastSlice(path, cpopath)
		self.antenna_ic.replaceLastSlice(path, cpopath)
		self.antenna_lh.replaceLastSlice(path, cpopath)
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
		self.antenna_ec.putNonTimed(path, cpopath)
		self.antenna_ic.putNonTimed(path, cpopath)
		self.antenna_lh.putNonTimed(path, cpopath)
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
		self.antenna_ec.getSlice(path, cpopath, inTime, interpolMode)
		self.antenna_ic.getSlice(path, cpopath, inTime, interpolMode)
		self.antenna_lh.getSlice(path, cpopath, inTime, interpolMode)
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
			antenna_ecList = self.antenna_ec.build_non_resampled_data(path, cpopath, nbslice)
			antenna_icList = self.antenna_ic.build_non_resampled_data(path, cpopath, nbslice)
			antenna_lhList = self.antenna_lh.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = antennas()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.antenna_ec = antenna_ecList[i]
				slice.antenna_ic = antenna_icList[i]
				slice.antenna_lh = antenna_lhList[i]
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
		ull.deleteData(self.idx, path, cpopath + 'antenna_ec')
		ull.deleteData(self.idx, path, cpopath + 'antenna_ic')
		ull.deleteData(self.idx, path, cpopath + 'antenna_lh')
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class antennasArray:
	'''
	class antennasArray
	Antenna systems for heating and current drive in the electron cyclotron (EC), ion cylcotron (IC) and lower hybrid (LH) frequencies. Time-dependent CPO.

	Attributes:
	- array : list of antennas
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
		ret = space + 'class antennasArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'antennas cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = antennas()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(antennas())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = antennas()
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


class antenna_ecstruct_arrayantenna_ec:
	'''
	class antenna_ecstruct_arrayantenna_ec
	Vector of Electron Cyclotron antennas. Time-dependent

	Attributes:
	- array : list of antenna_ecstruct_arrayantenna_ecObj 
	'''

	def __init__(self, base_path_in='antenna_ec'):
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
		ret = space + 'class antenna_ecstruct_arrayantenna_ec\n'
		for i in range(len(self.array)):
			ret = ret + space + 'antenna_ecstruct_arrayantenna_ec[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(antenna_ecstruct_arrayantenna_ecObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function putSlice') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function getSlice') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(antenna_ecstruct_arrayantenna_ec(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(antenna_ecstruct_arrayantenna_ec(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = antenna_ecstruct_arrayantenna_ec(self.base_path)
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_ecstruct_arrayantenna_ec, run function getNonTimedElt') 
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


class antenna_ecstruct_arrayantenna_ecObj:
	'''
	class antenna_ecstruct_arrayantenna_ecObj
	Vector of Electron Cyclotron antennas. Time-dependent

	Attributes:
	- name : str
	   Antenna name
	- frequency : float
	   Frequency [Hz]
	- power : class powerstructureexp0D
	   Power [W]; Time-dependent
	- mode : int
	   Incoming wave mode (+ or -1 for O/X mode); Time-dependent
	- position : class positionstructurerzphi0D
	   Launching position in the global reference system; Time-dependent
	- launchangles : class launchanglesstructurelaunchangles
	   Launching angles of the beam
	- beam : class beamstructurerfbeam
	   Beam characteristics at the launching position
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='antenna_ec'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.frequency = EMPTY_DOUBLE
		self.power = powerstructureexp0D('power')
		self.mode = EMPTY_INT
		self.position = positionstructurerzphi0D('position')
		self.launchangles = launchanglesstructurelaunchangles('launchangles')
		self.beam = beamstructurerfbeam('beam')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class antenna_ecstruct_arrayantenna_ecObj\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute frequency: ' + str(self.frequency) + '\n'
		ret = ret + space + 'Attribute power\n ' + self.power.__str__(depth+1)
		ret = ret + space + 'Attribute mode: ' + str(self.mode) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute launchangles\n ' + self.launchangles.__str__(depth+1)
		ret = ret + space + 'Attribute beam\n ' + self.beam.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.power.setExpIdx(idx)
		self.position.setExpIdx(idx)
		self.launchangles.setExpIdx(idx)
		self.beam.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_ecstruct_arrayantenna_ecObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.power.putTimedElt(path, cpopath + 'power', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'mode') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'mode', i, self.mode)
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		obj = self.launchangles.putTimedElt(path, cpopath + 'launchangles', i, obj)
		obj = self.beam.putTimedElt(path, cpopath + 'beam', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_ecstruct_arrayantenna_ecObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.power.getTimedElt(path, cpopath + 'power', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'mode') 
			print ('obj = ' + str(obj))
		status, ret_mode = ull.getIntFromObject(self.idx, obj, cpopath + 'mode', i)
		check_status(status)
		if not status:
			self.mode = ret_mode
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		self.launchangles.getTimedElt(path, cpopath + 'launchangles', i, obj)
		self.beam.getTimedElt(path, cpopath + 'beam', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_ecstruct_arrayantenna_ecObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'frequency', i, self.frequency)
		obj = self.power.putNonTimedElt(path, cpopath + 'power', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		obj = self.launchangles.putNonTimedElt(path, cpopath + 'launchangles', i, obj)
		obj = self.beam.putNonTimedElt(path, cpopath + 'beam', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_ecstruct_arrayantenna_ecObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		status, ret_frequency = ull.getDoubleFromObject(self.idx, obj, cpopath + 'frequency', i)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		self.power.getNonTimedElt(path, cpopath + 'power', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		self.launchangles.getNonTimedElt(path, cpopath + 'launchangles', i, obj)
		self.beam.getNonTimedElt(path, cpopath + 'beam', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class powerstructureexp0D:
	'''
	class powerstructureexp0D
	Power [W]; Time-dependent

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='power'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class powerstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type powerstructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type powerstructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type powerstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type powerstructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type powerstructureexp0D, run function build_non_resampled_data') 
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
				slice = powerstructureexp0D(self.base_path)
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
			print ('object of type powerstructureexp0DObj, run function putTimedElt') 
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
			print ('object of type powerstructureexp0DObj, run function getTimedElt') 
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
			print ('object of type powerstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type powerstructureexp0DObj, run function getNonTimedElt') 
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


class positionstructurerzphi0D:
	'''
	class positionstructurerzphi0D
	Launching position in the global reference system; Time-dependent

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	- phi : float
	   Toroidal angle [rad]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE
		self.phi = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerzphi0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		ret = ret + space + 'Attribute phi: ' + str(self.phi) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function putSlice') 
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
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'phi', self.phi, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function replaceLastSlice') 
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
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'phi', self.phi)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function getSlice') 
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
		status, ret_phi, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function build_non_resampled_data') 
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
			status, phiList = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerzphi0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				slice.phi = phiList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r', i, self.r)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'phi', i, self.phi)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function getTimedElt') 
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
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function getNonTimedElt') 
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


class launchanglesstructurelaunchangles:
	'''
	class launchanglesstructurelaunchangles
	Launching angles of the beam

	Attributes:
	- alpha : float
	   Poloidal launching angle between the horizontal plane and the poloidal component of the nominal beam centerline [rad], Tan(alpha)=-k_z/k_R; Time-dependent
	- beta : float
	   Toroidal launching angle between the poloidal plane and the nominal beam centerline [rad], Sin(beta)=k_phi; Time-dependent
	'''

	def __init__(self, base_path_in='launchangles'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.alpha = EMPTY_DOUBLE
		self.beta = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class launchanglesstructurelaunchangles\n'
		ret = ret + space + 'Attribute alpha: ' + str(self.alpha) + '\n'
		ret = ret + space + 'Attribute beta: ' + str(self.beta) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type launchanglesstructurelaunchangles, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'alpha', self.alpha, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta', self.beta, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type launchanglesstructurelaunchangles, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'alpha', self.alpha)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta', self.beta)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type launchanglesstructurelaunchangles, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type launchanglesstructurelaunchangles, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_alpha, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'alpha', inTime, interpolMode)
		check_status(status)
		if not status:
			self.alpha = ret_alpha
			self.cpoTime = retTime
		status, ret_beta, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta = ret_beta
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type launchanglesstructurelaunchangles, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, alphaList = ull.getVect1DDouble(self.idx, path, cpopath + 'alpha')
			if len(alphaList) == 0:
				alphaList = numpy.resize(alphaList, (nbslice))
			check_status(status)
			status, betaList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta')
			if len(betaList) == 0:
				betaList = numpy.resize(betaList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = launchanglesstructurelaunchangles(self.base_path)
				slice.setExpIdx(self.idx)
				slice.alpha = alphaList[i].copy().astype(float)
				slice.beta = betaList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type launchanglesstructurelaunchanglesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'alpha') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'alpha', i, self.alpha)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta', i, self.beta)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type launchanglesstructurelaunchanglesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'alpha') 
			print ('obj = ' + str(obj))
		status, ret_alpha = ull.getDoubleFromObject(self.idx, obj, cpopath + 'alpha', i)
		check_status(status)
		if not status:
			self.alpha = ret_alpha
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta') 
			print ('obj = ' + str(obj))
		status, ret_beta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta', i)
		check_status(status)
		if not status:
			self.beta = ret_beta

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type launchanglesstructurelaunchanglesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type launchanglesstructurelaunchanglesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'alpha')
		ull.deleteData(self.idx, path, cpopath + 'beta')


class beamstructurerfbeam:
	'''
	class beamstructurerfbeam
	Beam characteristics at the launching position

	Attributes:
	- spot : class spotstructurespot
	   Spot characteristics
	- phaseellipse : class phaseellipsestructurephaseellipse
	   Phase ellipse characteristics
	'''

	def __init__(self, base_path_in='beam'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.spot = spotstructurespot('spot')
		self.phaseellipse = phaseellipsestructurephaseellipse('phaseellipse')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamstructurerfbeam\n'
		ret = ret + space + 'Attribute spot\n ' + self.spot.__str__(depth+1)
		ret = ret + space + 'Attribute phaseellipse\n ' + self.phaseellipse.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.spot.setExpIdx(idx)
		self.phaseellipse.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamstructurerfbeam, run function putSlice') 
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
			print ('field '+self.base_path+' of type beamstructurerfbeam, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type beamstructurerfbeam, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type beamstructurerfbeam, run function getSlice') 
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
			print ('field '+self.base_path+' of type beamstructurerfbeam, run function build_non_resampled_data') 
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
				slice = beamstructurerfbeam(self.base_path)
				slice.setExpIdx(self.idx)
				slice.spot = spotList[i]
				slice.phaseellipse = phaseellipseList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurerfbeamObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.spot.putTimedElt(path, cpopath + 'spot', i, obj)
		obj = self.phaseellipse.putTimedElt(path, cpopath + 'phaseellipse', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurerfbeamObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.spot.getTimedElt(path, cpopath + 'spot', i, obj)
		self.phaseellipse.getTimedElt(path, cpopath + 'phaseellipse', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurerfbeamObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.spot.putNonTimedElt(path, cpopath + 'spot', i, obj)
		obj = self.phaseellipse.putNonTimedElt(path, cpopath + 'phaseellipse', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamstructurerfbeamObj, run function getNonTimedElt') 
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


class spotstructurespot:
	'''
	class spotstructurespot
	Spot characteristics

	Attributes:
	- size : numpy.ndarray 1D with float
	   Size of the spot ellipse [m], Vector (2). Time-dependent
	- angle : float
	   Rotation angle for the spot ellipse [rd], Float. Time-dependent
	'''

	def __init__(self, base_path_in='spot'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.size = numpy.zeros(0, numpy.float64, order='C')
		self.angle = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spotstructurespot\n'
		s = self.size.__str__()
		ret = ret + space + 'Attribute size\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute angle: ' + str(self.angle) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurespot, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'size', numpy.array(self.size).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'angle', self.angle, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurespot, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'size', numpy.array(self.size).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'angle', self.angle)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurespot, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurespot, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_size, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'size', inTime, interpolMode)
		check_status(status)
		if not status:
			self.size = ret_size
			self.cpoTime = retTime
		status, ret_angle, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'angle', inTime, interpolMode)
		check_status(status)
		if not status:
			self.angle = ret_angle
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type spotstructurespot, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, sizeList = ull.getVect2DDouble(self.idx, path, cpopath + 'size')
			if len(sizeList) == 0:
				sizeList = numpy.resize(sizeList, (0,nbslice))
			check_status(status)
			status, angleList = ull.getVect1DDouble(self.idx, path, cpopath + 'angle')
			if len(angleList) == 0:
				angleList = numpy.resize(angleList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = spotstructurespot(self.base_path)
				slice.setExpIdx(self.idx)
				slice.size = sizeList[:,i]
				slice.angle = angleList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurespotObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'size') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'size', i, numpy.array(self.size).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'angle', i, self.angle)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurespotObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'size') 
			print ('obj = ' + str(obj))
		status, ret_size = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'size', i)
		check_status(status)
		if not status:
			self.size = ret_size
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		status, ret_angle = ull.getDoubleFromObject(self.idx, obj, cpopath + 'angle', i)
		check_status(status)
		if not status:
			self.angle = ret_angle

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurespotObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spotstructurespotObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'size')
		ull.deleteData(self.idx, path, cpopath + 'angle')


class phaseellipsestructurephaseellipse:
	'''
	class phaseellipsestructurephaseellipse
	Phase ellipse characteristics

	Attributes:
	- invcurvrad : numpy.ndarray 1D with float
	   Inverse curvature radii for the phase ellipse [m-1], positive/negative for divergent/convergent beams, Vector (2). Time-dependent
	- angle : float
	   Rotation angle for the phase ellipse [rd], Float. Time-dependent
	'''

	def __init__(self, base_path_in='phaseellipse'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.invcurvrad = numpy.zeros(0, numpy.float64, order='C')
		self.angle = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class phaseellipsestructurephaseellipse\n'
		s = self.invcurvrad.__str__()
		ret = ret + space + 'Attribute invcurvrad\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute angle: ' + str(self.angle) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurephaseellipse, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'invcurvrad', numpy.array(self.invcurvrad).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'angle', self.angle, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurephaseellipse, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'invcurvrad', numpy.array(self.invcurvrad).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'angle', self.angle)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurephaseellipse, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurephaseellipse, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_invcurvrad, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'invcurvrad', inTime, interpolMode)
		check_status(status)
		if not status:
			self.invcurvrad = ret_invcurvrad
			self.cpoTime = retTime
		status, ret_angle, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'angle', inTime, interpolMode)
		check_status(status)
		if not status:
			self.angle = ret_angle
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type phaseellipsestructurephaseellipse, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, invcurvradList = ull.getVect2DDouble(self.idx, path, cpopath + 'invcurvrad')
			if len(invcurvradList) == 0:
				invcurvradList = numpy.resize(invcurvradList, (0,nbslice))
			check_status(status)
			status, angleList = ull.getVect1DDouble(self.idx, path, cpopath + 'angle')
			if len(angleList) == 0:
				angleList = numpy.resize(angleList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = phaseellipsestructurephaseellipse(self.base_path)
				slice.setExpIdx(self.idx)
				slice.invcurvrad = invcurvradList[:,i]
				slice.angle = angleList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurephaseellipseObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'invcurvrad') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'invcurvrad', i, numpy.array(self.invcurvrad).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'angle', i, self.angle)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurephaseellipseObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'invcurvrad') 
			print ('obj = ' + str(obj))
		status, ret_invcurvrad = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'invcurvrad', i)
		check_status(status)
		if not status:
			self.invcurvrad = ret_invcurvrad
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		status, ret_angle = ull.getDoubleFromObject(self.idx, obj, cpopath + 'angle', i)
		check_status(status)
		if not status:
			self.angle = ret_angle

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurephaseellipseObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phaseellipsestructurephaseellipseObj, run function getNonTimedElt') 
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


class antenna_icstruct_arrayantenna_ic:
	'''
	class antenna_icstruct_arrayantenna_ic
	Vector of Ion Cyclotron antennas. Time-dependent

	Attributes:
	- array : list of antenna_icstruct_arrayantenna_icObj 
	'''

	def __init__(self, base_path_in='antenna_ic'):
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
		ret = space + 'class antenna_icstruct_arrayantenna_ic\n'
		for i in range(len(self.array)):
			ret = ret + space + 'antenna_icstruct_arrayantenna_ic[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(antenna_icstruct_arrayantenna_icObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function putSlice') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function getSlice') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(antenna_icstruct_arrayantenna_ic(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(antenna_icstruct_arrayantenna_ic(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = antenna_icstruct_arrayantenna_ic(self.base_path)
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_icstruct_arrayantenna_ic, run function getNonTimedElt') 
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


class antenna_icstruct_arrayantenna_icObj:
	'''
	class antenna_icstruct_arrayantenna_icObj
	Vector of Ion Cyclotron antennas. Time-dependent

	Attributes:
	- name : str
	   Antenna name; String
	- frequency : class frequencystructureexp0D
	   Frequency [Hz]; Time-dependent; Exp0d
	- power : class powerstructureexp0D
	   Power [W]; Time-dependent; Exp0d
	- ntor : numpy.ndarray 1D with int)
	   Toroidal mode numbers [-]; Time-dependent; Vector(n_ntor)
	- power_ntor : numpy.ndarray 1D with float
	   Power coupled in each toroidal mode [W]; Time-dependent; Vector(n_ntor)
	- setup : class setupstructureantennaic_setup
	   Detailed description of IC antenna hardware and internal settings
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='antenna_ic'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.frequency = frequencystructureexp0D('frequency')
		self.power = powerstructureexp0D('power')
		self.ntor = numpy.zeros(0, numpy.int32, order='C')
		self.power_ntor = numpy.zeros(0, numpy.float64, order='C')
		self.setup = setupstructureantennaic_setup('setup')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class antenna_icstruct_arrayantenna_icObj\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute frequency\n ' + self.frequency.__str__(depth+1)
		ret = ret + space + 'Attribute power\n ' + self.power.__str__(depth+1)
		s = self.ntor.__str__()
		ret = ret + space + 'Attribute ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_ntor.__str__()
		ret = ret + space + 'Attribute power_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute setup\n ' + self.setup.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.frequency.setExpIdx(idx)
		self.power.setExpIdx(idx)
		self.setup.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_icstruct_arrayantenna_icObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.frequency.putTimedElt(path, cpopath + 'frequency', i, obj)
		obj = self.power.putTimedElt(path, cpopath + 'power', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ntor', i, numpy.array(self.ntor).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_ntor', i, numpy.array(self.power_ntor).astype(numpy.float64))
		obj = self.setup.putTimedElt(path, cpopath + 'setup', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_icstruct_arrayantenna_icObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.frequency.getTimedElt(path, cpopath + 'frequency', i, obj)
		self.power.getTimedElt(path, cpopath + 'power', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		status, ret_ntor = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ntor', i)
		check_status(status)
		if not status:
			self.ntor = ret_ntor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_ntor') 
			print ('obj = ' + str(obj))
		status, ret_power_ntor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_ntor', i)
		check_status(status)
		if not status:
			self.power_ntor = ret_power_ntor
		self.setup.getTimedElt(path, cpopath + 'setup', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_icstruct_arrayantenna_icObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		obj = self.frequency.putNonTimedElt(path, cpopath + 'frequency', i, obj)
		obj = self.power.putNonTimedElt(path, cpopath + 'power', i, obj)
		obj = self.setup.putNonTimedElt(path, cpopath + 'setup', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_icstruct_arrayantenna_icObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		self.frequency.getNonTimedElt(path, cpopath + 'frequency', i, obj)
		self.power.getNonTimedElt(path, cpopath + 'power', i, obj)
		self.setup.getNonTimedElt(path, cpopath + 'setup', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class frequencystructureexp0D:
	'''
	class frequencystructureexp0D
	Frequency [Hz]; Time-dependent; Exp0d

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='frequency'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class frequencystructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type frequencystructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type frequencystructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type frequencystructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type frequencystructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type frequencystructureexp0D, run function build_non_resampled_data') 
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
				slice = frequencystructureexp0D(self.base_path)
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
			print ('object of type frequencystructureexp0DObj, run function putTimedElt') 
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
			print ('object of type frequencystructureexp0DObj, run function getTimedElt') 
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
			print ('object of type frequencystructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type frequencystructureexp0DObj, run function getNonTimedElt') 
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


class setupstructureantennaic_setup:
	'''
	class setupstructureantennaic_setup
	Detailed description of IC antenna hardware and internal settings

	Attributes:
	- straps : class strapsstruct_arraystraps: array of strapsstruct_arraystrapsObj objects
	   Properties of the IC antenna strap; Time-dependent; Vector(nstraps)
	- current : class currentstructurecurrent
	   Description of the IC surface currents on the antenna straps and on passive components.
	'''

	def __init__(self, base_path_in='setup'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.straps = strapsstruct_arraystraps('straps')
		self.current = currentstructurecurrent('current')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class setupstructureantennaic_setup\n'
		ret = ret + space + 'Attribute straps\n ' + self.straps.__str__(depth+1)
		ret = ret + space + 'Attribute current\n ' + self.current.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.straps.setExpIdx(idx)
		self.current.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennaic_setup, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.straps.cpoTime = self.cpoTime
		self.straps.putSlice(path, cpopath)
		self.current.cpoTime = self.cpoTime
		self.current.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennaic_setup, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.straps.replaceLastSlice(path, cpopath)
		self.current.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennaic_setup, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.straps.putNonTimed(path, cpopath)
		self.current.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennaic_setup, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.straps.getSlice(path, cpopath, inTime, interpolMode)
		self.current.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennaic_setup, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			strapsList = self.straps.build_non_resampled_data(path, cpopath, nbslice)
			currentList = self.current.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = setupstructureantennaic_setup(self.base_path)
				slice.setExpIdx(self.idx)
				slice.straps = strapsList[i]
				slice.current = currentList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennaic_setupObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.straps.putTimedElt(path, cpopath + 'straps', i, obj)
		obj = self.current.putTimedElt(path, cpopath + 'current', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennaic_setupObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.straps.getTimedElt(path, cpopath + 'straps', i, obj)
		self.current.getTimedElt(path, cpopath + 'current', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennaic_setupObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.straps.putNonTimedElt(path, cpopath + 'straps', i, obj)
		obj = self.current.putNonTimedElt(path, cpopath + 'current', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennaic_setupObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.straps.getNonTimedElt(path, cpopath + 'straps', i, obj)
		self.current.getNonTimedElt(path, cpopath + 'current', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'straps')
		self.current.deleteData(path, cpopath)


class strapsstruct_arraystraps:
	'''
	class strapsstruct_arraystraps
	Properties of the IC antenna strap; Time-dependent; Vector(nstraps)

	Attributes:
	- array : list of strapsstruct_arraystrapsObj 
	'''

	def __init__(self, base_path_in='straps'):
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
		ret = space + 'class strapsstruct_arraystraps\n'
		for i in range(len(self.array)):
			ret = ret + space + 'strapsstruct_arraystraps[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(strapsstruct_arraystrapsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function putSlice') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function getSlice') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(strapsstruct_arraystraps(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(strapsstruct_arraystraps(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = strapsstruct_arraystraps(self.base_path)
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type strapsstruct_arraystraps, run function getNonTimedElt') 
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


class strapsstruct_arraystrapsObj:
	'''
	class strapsstruct_arraystrapsObj
	Properties of the IC antenna strap; Time-dependent; Vector(nstraps)

	Attributes:
	- current : class currentstructureexp0D
	   Root mean square current flowing along the strap [A]; Time-Dependent; Float
	- phase : class phasestructureexp0D
	   Phase of strap current [rad]; Time-dependent; exp0D
	- phi_centre : float
	   Toroidal angle at the centre of the strap [rad]; Float
	- width : float
	   Width of strap in the toroidal direction [m]; Float
	- dist2wall : float
	   Distance to conducting wall or other conducter behind the antenna straps [m]; Float
	- coord_strap : class coord_strapstructurerz1D
	   Coordinates (R,z) of polygon describing the antenna in the poloidal plane; rz1d vector (ncoord_strap)
	'''

	def __init__(self, base_path_in='straps'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.current = currentstructureexp0D('current')
		self.phase = phasestructureexp0D('phase')
		self.phi_centre = EMPTY_DOUBLE
		self.width = EMPTY_DOUBLE
		self.dist2wall = EMPTY_DOUBLE
		self.coord_strap = coord_strapstructurerz1D('coord_strap')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class strapsstruct_arraystrapsObj\n'
		ret = ret + space + 'Attribute current\n ' + self.current.__str__(depth+1)
		ret = ret + space + 'Attribute phase\n ' + self.phase.__str__(depth+1)
		ret = ret + space + 'Attribute phi_centre: ' + str(self.phi_centre) + '\n'
		ret = ret + space + 'Attribute width: ' + str(self.width) + '\n'
		ret = ret + space + 'Attribute dist2wall: ' + str(self.dist2wall) + '\n'
		ret = ret + space + 'Attribute coord_strap\n ' + self.coord_strap.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.current.setExpIdx(idx)
		self.phase.setExpIdx(idx)
		self.coord_strap.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type strapsstruct_arraystrapsObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.current.putTimedElt(path, cpopath + 'current', i, obj)
		obj = self.phase.putTimedElt(path, cpopath + 'phase', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type strapsstruct_arraystrapsObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.current.getTimedElt(path, cpopath + 'current', i, obj)
		self.phase.getTimedElt(path, cpopath + 'phase', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type strapsstruct_arraystrapsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.current.putNonTimedElt(path, cpopath + 'current', i, obj)
		obj = self.phase.putNonTimedElt(path, cpopath + 'phase', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'phi_centre') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'phi_centre', i, self.phi_centre)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'width') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'width', i, self.width)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dist2wall') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dist2wall', i, self.dist2wall)
		obj = self.coord_strap.putNonTimedElt(path, cpopath + 'coord_strap', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type strapsstruct_arraystrapsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.current.getNonTimedElt(path, cpopath + 'current', i, obj)
		self.phase.getNonTimedElt(path, cpopath + 'phase', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'phi_centre') 
			print ('obj = ' + str(obj))
		status, ret_phi_centre = ull.getDoubleFromObject(self.idx, obj, cpopath + 'phi_centre', i)
		check_status(status)
		if not status:
			self.phi_centre = ret_phi_centre
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'width') 
			print ('obj = ' + str(obj))
		status, ret_width = ull.getDoubleFromObject(self.idx, obj, cpopath + 'width', i)
		check_status(status)
		if not status:
			self.width = ret_width
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dist2wall') 
			print ('obj = ' + str(obj))
		status, ret_dist2wall = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dist2wall', i)
		check_status(status)
		if not status:
			self.dist2wall = ret_dist2wall
		self.coord_strap.getNonTimedElt(path, cpopath + 'coord_strap', i, obj)


class currentstructureexp0D:
	'''
	class currentstructureexp0D
	Root mean square current flowing along the strap [A]; Time-Dependent; Float

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


class phasestructureexp0D:
	'''
	class phasestructureexp0D
	Phase of strap current [rad]; Time-dependent; exp0D

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='phase'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class phasestructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type phasestructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type phasestructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type phasestructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type phasestructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type phasestructureexp0D, run function build_non_resampled_data') 
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
				slice = phasestructureexp0D(self.base_path)
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
			print ('object of type phasestructureexp0DObj, run function putTimedElt') 
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
			print ('object of type phasestructureexp0DObj, run function getTimedElt') 
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
			print ('object of type phasestructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phasestructureexp0DObj, run function getNonTimedElt') 
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


class coord_strapstructurerz1D:
	'''
	class coord_strapstructurerz1D
	Coordinates (R,z) of polygon describing the antenna in the poloidal plane; rz1d vector (ncoord_strap)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='coord_strap'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coord_strapstructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_strapstructurerz1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_strapstructurerz1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_strapstructurerz1D, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type coord_strapstructurerz1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type coord_strapstructurerz1D, run function build_non_resampled_data') 
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
				slice = coord_strapstructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_strapstructurerz1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_strapstructurerz1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_strapstructurerz1DObj, run function putNonTimedElt') 
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
			print ('object of type coord_strapstructurerz1DObj, run function getNonTimedElt') 
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


class currentstructurecurrent:
	'''
	class currentstructurecurrent
	Description of the IC surface currents on the antenna straps and on passive components.

	Attributes:
	- mpol : numpy.ndarray 1D with int)
	   Poloidal modes, used to describe the spectrum of the antenna current. The poloidal angle is defined from the reference point rz_reference; the angle at a point (R,Z) is given by atan((Z-Zref)/(R-Rref)), where Rref=rz_reference/r and Zref=rz_reference/z. Time-Dependent; Integer(n_poloidal_modes)
	- ntor : numpy.ndarray 1D with int)
	   Toroidal modes, used to describe the spectrum of the antenna current. Time-Dependent; Integer(n_toroidal_modes)
	- spectrum : class spectrumstructureexp1D
	   Spectrum of the total surface current on the antenna strap and passive components expressed in poloidal and toroidal mode [A]. Calculated using a geometrical poloidal angle around the point rz_reference. Time-dependent; exp1D(n_poloidal_modes , n_toroidal_modes)
	- rz_reference : class rz_referencestructurerz0D
	   Reference point used to define the poloidal angle, e.g. the geometrical centre of the vacuum vessel. Time-dependent; rz0d
	'''

	def __init__(self, base_path_in='current'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mpol = numpy.zeros(0, numpy.int32, order='C')
		self.ntor = numpy.zeros(0, numpy.int32, order='C')
		self.spectrum = spectrumstructureexp1D('spectrum')
		self.rz_reference = rz_referencestructurerz0D('rz_reference')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class currentstructurecurrent\n'
		s = self.mpol.__str__()
		ret = ret + space + 'Attribute mpol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ntor.__str__()
		ret = ret + space + 'Attribute ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute spectrum\n ' + self.spectrum.__str__(depth+1)
		ret = ret + space + 'Attribute rz_reference\n ' + self.rz_reference.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.spectrum.setExpIdx(idx)
		self.rz_reference.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurecurrent, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spectrum.cpoTime = self.cpoTime
		self.spectrum.putSlice(path, cpopath)
		self.rz_reference.cpoTime = self.cpoTime
		self.rz_reference.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurecurrent, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spectrum.replaceLastSlice(path, cpopath)
		self.rz_reference.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurecurrent, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DInt(self.idx, path, cpopath + 'mpol', numpy.array(self.mpol).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'ntor', numpy.array(self.ntor).astype(numpy.int32), False)
		check_status(status)
		self.spectrum.putNonTimed(path, cpopath)
		self.rz_reference.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurecurrent, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_mpol = ull.getVect1DInt(self.idx, path, cpopath + 'mpol')
		check_status(status)
		if not status:
			self.mpol = ret_mpol
		status, ret_ntor = ull.getVect1DInt(self.idx, path, cpopath + 'ntor')
		check_status(status)
		if not status:
			self.ntor = ret_ntor
		self.spectrum.getSlice(path, cpopath, inTime, interpolMode)
		self.rz_reference.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurecurrent, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, mpolVal = ull.getVect1DInt(self.idx, path, cpopath + 'mpol')
			check_status(status)
			status, ntorVal = ull.getVect1DInt(self.idx, path, cpopath + 'ntor')
			check_status(status)
			spectrumList = self.spectrum.build_non_resampled_data(path, cpopath, nbslice)
			rz_referenceList = self.rz_reference.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = currentstructurecurrent(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mpol = mpolVal
				slice.ntor = ntorVal
				slice.spectrum = spectrumList[i]
				slice.rz_reference = rz_referenceList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurecurrentObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.spectrum.putTimedElt(path, cpopath + 'spectrum', i, obj)
		obj = self.rz_reference.putTimedElt(path, cpopath + 'rz_reference', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurecurrentObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.spectrum.getTimedElt(path, cpopath + 'spectrum', i, obj)
		self.rz_reference.getTimedElt(path, cpopath + 'rz_reference', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurecurrentObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'mpol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'mpol', i, numpy.array(self.mpol).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ntor', i, numpy.array(self.ntor).astype(numpy.int32))
		obj = self.spectrum.putNonTimedElt(path, cpopath + 'spectrum', i, obj)
		obj = self.rz_reference.putNonTimedElt(path, cpopath + 'rz_reference', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurecurrentObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'mpol') 
			print ('obj = ' + str(obj))
		status, ret_mpol = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'mpol', i)
		check_status(status)
		if not status:
			self.mpol = ret_mpol
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		status, ret_ntor = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ntor', i)
		check_status(status)
		if not status:
			self.ntor = ret_ntor
		self.spectrum.getNonTimedElt(path, cpopath + 'spectrum', i, obj)
		self.rz_reference.getNonTimedElt(path, cpopath + 'rz_reference', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'mpol')
		ull.deleteData(self.idx, path, cpopath + 'ntor')
		self.spectrum.deleteData(path, cpopath)
		self.rz_reference.deleteData(path, cpopath)


class spectrumstructureexp1D:
	'''
	class spectrumstructureexp1D
	Spectrum of the total surface current on the antenna strap and passive components expressed in poloidal and toroidal mode [A]. Calculated using a geometrical poloidal angle around the point rz_reference. Time-dependent; exp1D(n_poloidal_modes , n_toroidal_modes)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='spectrum'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spectrumstructureexp1D\n'
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
			print ('field '+self.base_path+' of type spectrumstructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type spectrumstructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type spectrumstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type spectrumstructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type spectrumstructureexp1D, run function build_non_resampled_data') 
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
				slice = spectrumstructureexp1D(self.base_path)
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
			print ('object of type spectrumstructureexp1DObj, run function putTimedElt') 
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
			print ('object of type spectrumstructureexp1DObj, run function getTimedElt') 
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
			print ('object of type spectrumstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spectrumstructureexp1DObj, run function getNonTimedElt') 
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


class rz_referencestructurerz0D:
	'''
	class rz_referencestructurerz0D
	Reference point used to define the poloidal angle, e.g. the geometrical centre of the vacuum vessel. Time-dependent; rz0d

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='rz_reference'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class rz_referencestructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rz_referencestructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type rz_referencestructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type rz_referencestructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type rz_referencestructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type rz_referencestructurerz0D, run function build_non_resampled_data') 
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
				slice = rz_referencestructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rz_referencestructurerz0DObj, run function putTimedElt') 
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
			print ('object of type rz_referencestructurerz0DObj, run function getTimedElt') 
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
			print ('object of type rz_referencestructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rz_referencestructurerz0DObj, run function getNonTimedElt') 
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


class antenna_lhstruct_arrayantenna_lh:
	'''
	class antenna_lhstruct_arrayantenna_lh
	Vector of Lower Hybrid antennas. Time-dependent

	Attributes:
	- array : list of antenna_lhstruct_arrayantenna_lhObj 
	'''

	def __init__(self, base_path_in='antenna_lh'):
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
		ret = space + 'class antenna_lhstruct_arrayantenna_lh\n'
		for i in range(len(self.array)):
			ret = ret + space + 'antenna_lhstruct_arrayantenna_lh[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(antenna_lhstruct_arrayantenna_lhObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function putSlice') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function getSlice') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(antenna_lhstruct_arrayantenna_lh(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(antenna_lhstruct_arrayantenna_lh(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = antenna_lhstruct_arrayantenna_lh(self.base_path)
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type antenna_lhstruct_arrayantenna_lh, run function getNonTimedElt') 
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


class antenna_lhstruct_arrayantenna_lhObj:
	'''
	class antenna_lhstruct_arrayantenna_lhObj
	Vector of Lower Hybrid antennas. Time-dependent

	Attributes:
	- name : str
	   Antenna name, String
	- frequency : float
	   Frequency [Hz]
	- power : class powerstructureexp0D
	   Power [W]; Exp0d. Time-dependent
	- n_par : float
	   Main parallel refractive index of the launched spectrum, for multi-junction antennas. Time-dependent
	- position : class positionstructurerzphi0D
	   Reference global antenna position. Time-dependent
	- setup : class setupstructureantennalh_setup
	   Detailed description of LH antennas.
	- plasmaedge : class plasmaedgestructureplasmaedge
	   Plasma edge characteristics in front of the antenna.
	- beam : class beamstructurerfbeam
	   Beam characteristics
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='antenna_lh'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.frequency = EMPTY_DOUBLE
		self.power = powerstructureexp0D('power')
		self.n_par = EMPTY_DOUBLE
		self.position = positionstructurerzphi0D('position')
		self.setup = setupstructureantennalh_setup('setup')
		self.plasmaedge = plasmaedgestructureplasmaedge('plasmaedge')
		self.beam = beamstructurerfbeam('beam')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class antenna_lhstruct_arrayantenna_lhObj\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute frequency: ' + str(self.frequency) + '\n'
		ret = ret + space + 'Attribute power\n ' + self.power.__str__(depth+1)
		ret = ret + space + 'Attribute n_par: ' + str(self.n_par) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute setup\n ' + self.setup.__str__(depth+1)
		ret = ret + space + 'Attribute plasmaedge\n ' + self.plasmaedge.__str__(depth+1)
		ret = ret + space + 'Attribute beam\n ' + self.beam.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.power.setExpIdx(idx)
		self.position.setExpIdx(idx)
		self.setup.setExpIdx(idx)
		self.plasmaedge.setExpIdx(idx)
		self.beam.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_lhstruct_arrayantenna_lhObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.power.putTimedElt(path, cpopath + 'power', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_par') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_par', i, self.n_par)
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		obj = self.setup.putTimedElt(path, cpopath + 'setup', i, obj)
		obj = self.plasmaedge.putTimedElt(path, cpopath + 'plasmaedge', i, obj)
		obj = self.beam.putTimedElt(path, cpopath + 'beam', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_lhstruct_arrayantenna_lhObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.power.getTimedElt(path, cpopath + 'power', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_par') 
			print ('obj = ' + str(obj))
		status, ret_n_par = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_par', i)
		check_status(status)
		if not status:
			self.n_par = ret_n_par
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		self.setup.getTimedElt(path, cpopath + 'setup', i, obj)
		self.plasmaedge.getTimedElt(path, cpopath + 'plasmaedge', i, obj)
		self.beam.getTimedElt(path, cpopath + 'beam', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_lhstruct_arrayantenna_lhObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'frequency', i, self.frequency)
		obj = self.power.putNonTimedElt(path, cpopath + 'power', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		obj = self.setup.putNonTimedElt(path, cpopath + 'setup', i, obj)
		obj = self.plasmaedge.putNonTimedElt(path, cpopath + 'plasmaedge', i, obj)
		obj = self.beam.putNonTimedElt(path, cpopath + 'beam', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type antenna_lhstruct_arrayantenna_lhObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		status, ret_frequency = ull.getDoubleFromObject(self.idx, obj, cpopath + 'frequency', i)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		self.power.getNonTimedElt(path, cpopath + 'power', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		self.setup.getNonTimedElt(path, cpopath + 'setup', i, obj)
		self.plasmaedge.getNonTimedElt(path, cpopath + 'plasmaedge', i, obj)
		self.beam.getNonTimedElt(path, cpopath + 'beam', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class setupstructureantennalh_setup:
	'''
	class setupstructureantennalh_setup
	Detailed description of LH antennas.

	Attributes:
	- modules : class modulesstructuremodules
	   Modules description. NB there are nmodules per antenna, distributed among nma_phi toroidal positions and nma_theta poloidal positions
	'''

	def __init__(self, base_path_in='setup'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.modules = modulesstructuremodules('modules')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class setupstructureantennalh_setup\n'
		ret = ret + space + 'Attribute modules\n ' + self.modules.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.modules.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennalh_setup, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.modules.cpoTime = self.cpoTime
		self.modules.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennalh_setup, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.modules.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennalh_setup, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.modules.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennalh_setup, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.modules.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type setupstructureantennalh_setup, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			modulesList = self.modules.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = setupstructureantennalh_setup(self.base_path)
				slice.setExpIdx(self.idx)
				slice.modules = modulesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennalh_setupObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.modules.putTimedElt(path, cpopath + 'modules', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennalh_setupObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.modules.getTimedElt(path, cpopath + 'modules', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennalh_setupObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.modules.putNonTimedElt(path, cpopath + 'modules', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type setupstructureantennalh_setupObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.modules.getNonTimedElt(path, cpopath + 'modules', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.modules.deleteData(path, cpopath)


class modulesstructuremodules:
	'''
	class modulesstructuremodules
	Modules description. NB there are nmodules per antenna, distributed among nma_phi toroidal positions and nma_theta poloidal positions

	Attributes:
	- nma_theta : int
	   Number of modules per antenna in the poloidal direction.
	- nma_phi : int
	   Number of modules per antenna in the toroidal direction.
	- ima_theta : numpy.ndarray 1D with int)
	   Position index of the module in the poloidal direction (from low theta to high theta, i.e. from bottom to top if the antenna is on LFS). Vector of integers (nmodules).
	- ima_phi : numpy.ndarray 1D with int)
	   Position index of the module in the toroidal direction (from low phi to high phi, counter-clockwise when seen from above). Vector of integers (nmodules).
	- sm_theta : float
	   Spacing between poloidally neighboring modules [m]
	- amplitude : class amplitudestructureexp1D
	   Amplitude of the TE10 mode injected in the module [W], Vector exp1d (nmodules). Time-dependent
	- phase : class phasestructureexp1D
	   Phase of the TE10 mode injected in the module [radians], Vector exp1d (nmodules). Time-dependent
	- waveguides : class waveguidesstructurewaveguides
	   Waveguides description
	'''

	def __init__(self, base_path_in='modules'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nma_theta = EMPTY_INT
		self.nma_phi = EMPTY_INT
		self.ima_theta = numpy.zeros(0, numpy.int32, order='C')
		self.ima_phi = numpy.zeros(0, numpy.int32, order='C')
		self.sm_theta = EMPTY_DOUBLE
		self.amplitude = amplitudestructureexp1D('amplitude')
		self.phase = phasestructureexp1D('phase')
		self.waveguides = waveguidesstructurewaveguides('waveguides')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class modulesstructuremodules\n'
		ret = ret + space + 'Attribute nma_theta: ' + str(self.nma_theta) + '\n'
		ret = ret + space + 'Attribute nma_phi: ' + str(self.nma_phi) + '\n'
		s = self.ima_theta.__str__()
		ret = ret + space + 'Attribute ima_theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ima_phi.__str__()
		ret = ret + space + 'Attribute ima_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute sm_theta: ' + str(self.sm_theta) + '\n'
		ret = ret + space + 'Attribute amplitude\n ' + self.amplitude.__str__(depth+1)
		ret = ret + space + 'Attribute phase\n ' + self.phase.__str__(depth+1)
		ret = ret + space + 'Attribute waveguides\n ' + self.waveguides.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.amplitude.setExpIdx(idx)
		self.phase.setExpIdx(idx)
		self.waveguides.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type modulesstructuremodules, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.amplitude.cpoTime = self.cpoTime
		self.amplitude.putSlice(path, cpopath)
		self.phase.cpoTime = self.cpoTime
		self.phase.putSlice(path, cpopath)
		self.waveguides.cpoTime = self.cpoTime
		self.waveguides.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type modulesstructuremodules, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.amplitude.replaceLastSlice(path, cpopath)
		self.phase.replaceLastSlice(path, cpopath)
		self.waveguides.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type modulesstructuremodules, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'nma_theta', self.nma_theta)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'nma_phi', self.nma_phi)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'ima_theta', numpy.array(self.ima_theta).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'ima_phi', numpy.array(self.ima_phi).astype(numpy.int32), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sm_theta', self.sm_theta)
		check_status(status)
		self.amplitude.putNonTimed(path, cpopath)
		self.phase.putNonTimed(path, cpopath)
		self.waveguides.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type modulesstructuremodules, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nma_theta = ull.getInt(self.idx, path, cpopath + 'nma_theta')
		check_status(status)
		if not status:
			self.nma_theta = ret_nma_theta
		status, ret_nma_phi = ull.getInt(self.idx, path, cpopath + 'nma_phi')
		check_status(status)
		if not status:
			self.nma_phi = ret_nma_phi
		status, ret_ima_theta = ull.getVect1DInt(self.idx, path, cpopath + 'ima_theta')
		check_status(status)
		if not status:
			self.ima_theta = ret_ima_theta
		status, ret_ima_phi = ull.getVect1DInt(self.idx, path, cpopath + 'ima_phi')
		check_status(status)
		if not status:
			self.ima_phi = ret_ima_phi
		status, ret_sm_theta = ull.getDouble(self.idx, path, cpopath + 'sm_theta')
		check_status(status)
		if not status:
			self.sm_theta = ret_sm_theta
		self.amplitude.getSlice(path, cpopath, inTime, interpolMode)
		self.phase.getSlice(path, cpopath, inTime, interpolMode)
		self.waveguides.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type modulesstructuremodules, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nma_thetaVal = ull.getInt(self.idx, path, cpopath + 'nma_theta')
			check_status(status)
			status, nma_phiVal = ull.getInt(self.idx, path, cpopath + 'nma_phi')
			check_status(status)
			status, ima_thetaVal = ull.getVect1DInt(self.idx, path, cpopath + 'ima_theta')
			check_status(status)
			status, ima_phiVal = ull.getVect1DInt(self.idx, path, cpopath + 'ima_phi')
			check_status(status)
			status, sm_thetaVal = ull.getDouble(self.idx, path, cpopath + 'sm_theta')
			check_status(status)
			amplitudeList = self.amplitude.build_non_resampled_data(path, cpopath, nbslice)
			phaseList = self.phase.build_non_resampled_data(path, cpopath, nbslice)
			waveguidesList = self.waveguides.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = modulesstructuremodules(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nma_theta = nma_thetaVal
				slice.nma_phi = nma_phiVal
				slice.ima_theta = ima_thetaVal
				slice.ima_phi = ima_phiVal
				slice.sm_theta = sm_thetaVal
				slice.amplitude = amplitudeList[i]
				slice.phase = phaseList[i]
				slice.waveguides = waveguidesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modulesstructuremodulesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.amplitude.putTimedElt(path, cpopath + 'amplitude', i, obj)
		obj = self.phase.putTimedElt(path, cpopath + 'phase', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modulesstructuremodulesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.amplitude.getTimedElt(path, cpopath + 'amplitude', i, obj)
		self.phase.getTimedElt(path, cpopath + 'phase', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modulesstructuremodulesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nma_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nma_theta', i, self.nma_theta)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nma_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nma_phi', i, self.nma_phi)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ima_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ima_theta', i, numpy.array(self.ima_theta).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ima_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ima_phi', i, numpy.array(self.ima_phi).astype(numpy.int32))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sm_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sm_theta', i, self.sm_theta)
		obj = self.amplitude.putNonTimedElt(path, cpopath + 'amplitude', i, obj)
		obj = self.phase.putNonTimedElt(path, cpopath + 'phase', i, obj)
		obj = self.waveguides.putNonTimedElt(path, cpopath + 'waveguides', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modulesstructuremodulesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nma_theta') 
			print ('obj = ' + str(obj))
		status, ret_nma_theta = ull.getIntFromObject(self.idx, obj, cpopath + 'nma_theta', i)
		check_status(status)
		if not status:
			self.nma_theta = ret_nma_theta
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nma_phi') 
			print ('obj = ' + str(obj))
		status, ret_nma_phi = ull.getIntFromObject(self.idx, obj, cpopath + 'nma_phi', i)
		check_status(status)
		if not status:
			self.nma_phi = ret_nma_phi
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ima_theta') 
			print ('obj = ' + str(obj))
		status, ret_ima_theta = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ima_theta', i)
		check_status(status)
		if not status:
			self.ima_theta = ret_ima_theta
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ima_phi') 
			print ('obj = ' + str(obj))
		status, ret_ima_phi = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ima_phi', i)
		check_status(status)
		if not status:
			self.ima_phi = ret_ima_phi
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sm_theta') 
			print ('obj = ' + str(obj))
		status, ret_sm_theta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sm_theta', i)
		check_status(status)
		if not status:
			self.sm_theta = ret_sm_theta
		self.amplitude.getNonTimedElt(path, cpopath + 'amplitude', i, obj)
		self.phase.getNonTimedElt(path, cpopath + 'phase', i, obj)
		self.waveguides.getNonTimedElt(path, cpopath + 'waveguides', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nma_theta')
		ull.deleteData(self.idx, path, cpopath + 'nma_phi')
		ull.deleteData(self.idx, path, cpopath + 'ima_theta')
		ull.deleteData(self.idx, path, cpopath + 'ima_phi')
		ull.deleteData(self.idx, path, cpopath + 'sm_theta')
		self.amplitude.deleteData(path, cpopath)
		self.phase.deleteData(path, cpopath)
		self.waveguides.deleteData(path, cpopath)


class amplitudestructureexp1D:
	'''
	class amplitudestructureexp1D
	Amplitude of the TE10 mode injected in the module [W], Vector exp1d (nmodules). Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='amplitude'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class amplitudestructureexp1D\n'
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
			print ('field '+self.base_path+' of type amplitudestructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type amplitudestructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type amplitudestructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type amplitudestructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type amplitudestructureexp1D, run function build_non_resampled_data') 
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
				slice = amplitudestructureexp1D(self.base_path)
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
			print ('object of type amplitudestructureexp1DObj, run function putTimedElt') 
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
			print ('object of type amplitudestructureexp1DObj, run function getTimedElt') 
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
			print ('object of type amplitudestructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type amplitudestructureexp1DObj, run function getNonTimedElt') 
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


class phasestructureexp1D:
	'''
	class phasestructureexp1D
	Phase of the TE10 mode injected in the module [radians], Vector exp1d (nmodules). Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='phase'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class phasestructureexp1D\n'
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
			print ('field '+self.base_path+' of type phasestructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type phasestructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type phasestructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type phasestructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type phasestructureexp1D, run function build_non_resampled_data') 
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
				slice = phasestructureexp1D(self.base_path)
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
			print ('object of type phasestructureexp1DObj, run function putTimedElt') 
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
			print ('object of type phasestructureexp1DObj, run function getTimedElt') 
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
			print ('object of type phasestructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type phasestructureexp1DObj, run function getNonTimedElt') 
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


class waveguidesstructurewaveguides:
	'''
	class waveguidesstructurewaveguides
	Waveguides description

	Attributes:
	- nwm_theta : int
	   Number of waveguides per module in the poloidal direction.
	- nwm_phi : int
	   Number of waveguides per module in the toroidal direction.
	- mask : numpy.ndarray 1D with int)
	   Mask of passive and active waveguides for an internal module; Vector of integers (nwm_phi)
	- npwbm_phi : int
	   Number of passive waveguide between modules in the toroidal direction
	- npwe_phi : int
	   Number of passive waveguides on each antenna edge in the toroidal direction
	- sw_theta : float
	   Spacing between poloidally neighboring waveguides [m]
	- hw_theta : float
	   Height of waveguides in the poloidal direction [m]
	- bwa : float
	   Width of active waveguides [m]; Float
	- biwp : float
	   Width of internal passive waveguides [m]; Float
	- bewp : float
	   Width of edge passive waveguides [m]; Float
	- e_phi : numpy.ndarray 1D with float
	   Thickness between waveguides in the toroidal direction [m], Vector (nthick_phi). Reminder : nthick_phi = nmp_phi*nwm_phi + (nmp_phi - 1)*npwbm_phi + 2*npwe_phi
	- scl : numpy.ndarray 1D with float
	   Short circuit length for passive waveguides [m], Vector (nshort_phi). Reminder : nshort _phi = nmp_phi* npwm_phi  + (nmp_phi - 1)*npwbm_phi + 2*npwe_phi
	'''

	def __init__(self, base_path_in='waveguides'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nwm_theta = EMPTY_INT
		self.nwm_phi = EMPTY_INT
		self.mask = numpy.zeros(0, numpy.int32, order='C')
		self.npwbm_phi = EMPTY_INT
		self.npwe_phi = EMPTY_INT
		self.sw_theta = EMPTY_DOUBLE
		self.hw_theta = EMPTY_DOUBLE
		self.bwa = EMPTY_DOUBLE
		self.biwp = EMPTY_DOUBLE
		self.bewp = EMPTY_DOUBLE
		self.e_phi = numpy.zeros(0, numpy.float64, order='C')
		self.scl = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class waveguidesstructurewaveguides\n'
		ret = ret + space + 'Attribute nwm_theta: ' + str(self.nwm_theta) + '\n'
		ret = ret + space + 'Attribute nwm_phi: ' + str(self.nwm_phi) + '\n'
		s = self.mask.__str__()
		ret = ret + space + 'Attribute mask\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute npwbm_phi: ' + str(self.npwbm_phi) + '\n'
		ret = ret + space + 'Attribute npwe_phi: ' + str(self.npwe_phi) + '\n'
		ret = ret + space + 'Attribute sw_theta: ' + str(self.sw_theta) + '\n'
		ret = ret + space + 'Attribute hw_theta: ' + str(self.hw_theta) + '\n'
		ret = ret + space + 'Attribute bwa: ' + str(self.bwa) + '\n'
		ret = ret + space + 'Attribute biwp: ' + str(self.biwp) + '\n'
		ret = ret + space + 'Attribute bewp: ' + str(self.bewp) + '\n'
		s = self.e_phi.__str__()
		ret = ret + space + 'Attribute e_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.scl.__str__()
		ret = ret + space + 'Attribute scl\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type waveguidesstructurewaveguides, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type waveguidesstructurewaveguides, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type waveguidesstructurewaveguides, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'nwm_theta', self.nwm_theta)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'nwm_phi', self.nwm_phi)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'mask', numpy.array(self.mask).astype(numpy.int32), False)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'npwbm_phi', self.npwbm_phi)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'npwe_phi', self.npwe_phi)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sw_theta', self.sw_theta)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'hw_theta', self.hw_theta)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'bwa', self.bwa)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'biwp', self.biwp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'bewp', self.bewp)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'e_phi', numpy.array(self.e_phi).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'scl', numpy.array(self.scl).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type waveguidesstructurewaveguides, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nwm_theta = ull.getInt(self.idx, path, cpopath + 'nwm_theta')
		check_status(status)
		if not status:
			self.nwm_theta = ret_nwm_theta
		status, ret_nwm_phi = ull.getInt(self.idx, path, cpopath + 'nwm_phi')
		check_status(status)
		if not status:
			self.nwm_phi = ret_nwm_phi
		status, ret_mask = ull.getVect1DInt(self.idx, path, cpopath + 'mask')
		check_status(status)
		if not status:
			self.mask = ret_mask
		status, ret_npwbm_phi = ull.getInt(self.idx, path, cpopath + 'npwbm_phi')
		check_status(status)
		if not status:
			self.npwbm_phi = ret_npwbm_phi
		status, ret_npwe_phi = ull.getInt(self.idx, path, cpopath + 'npwe_phi')
		check_status(status)
		if not status:
			self.npwe_phi = ret_npwe_phi
		status, ret_sw_theta = ull.getDouble(self.idx, path, cpopath + 'sw_theta')
		check_status(status)
		if not status:
			self.sw_theta = ret_sw_theta
		status, ret_hw_theta = ull.getDouble(self.idx, path, cpopath + 'hw_theta')
		check_status(status)
		if not status:
			self.hw_theta = ret_hw_theta
		status, ret_bwa = ull.getDouble(self.idx, path, cpopath + 'bwa')
		check_status(status)
		if not status:
			self.bwa = ret_bwa
		status, ret_biwp = ull.getDouble(self.idx, path, cpopath + 'biwp')
		check_status(status)
		if not status:
			self.biwp = ret_biwp
		status, ret_bewp = ull.getDouble(self.idx, path, cpopath + 'bewp')
		check_status(status)
		if not status:
			self.bewp = ret_bewp
		status, ret_e_phi = ull.getVect1DDouble(self.idx, path, cpopath + 'e_phi')
		check_status(status)
		if not status:
			self.e_phi = ret_e_phi
		status, ret_scl = ull.getVect1DDouble(self.idx, path, cpopath + 'scl')
		check_status(status)
		if not status:
			self.scl = ret_scl

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type waveguidesstructurewaveguides, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nwm_thetaVal = ull.getInt(self.idx, path, cpopath + 'nwm_theta')
			check_status(status)
			status, nwm_phiVal = ull.getInt(self.idx, path, cpopath + 'nwm_phi')
			check_status(status)
			status, maskVal = ull.getVect1DInt(self.idx, path, cpopath + 'mask')
			check_status(status)
			status, npwbm_phiVal = ull.getInt(self.idx, path, cpopath + 'npwbm_phi')
			check_status(status)
			status, npwe_phiVal = ull.getInt(self.idx, path, cpopath + 'npwe_phi')
			check_status(status)
			status, sw_thetaVal = ull.getDouble(self.idx, path, cpopath + 'sw_theta')
			check_status(status)
			status, hw_thetaVal = ull.getDouble(self.idx, path, cpopath + 'hw_theta')
			check_status(status)
			status, bwaVal = ull.getDouble(self.idx, path, cpopath + 'bwa')
			check_status(status)
			status, biwpVal = ull.getDouble(self.idx, path, cpopath + 'biwp')
			check_status(status)
			status, bewpVal = ull.getDouble(self.idx, path, cpopath + 'bewp')
			check_status(status)
			status, e_phiVal = ull.getVect1DDouble(self.idx, path, cpopath + 'e_phi')
			check_status(status)
			status, sclVal = ull.getVect1DDouble(self.idx, path, cpopath + 'scl')
			check_status(status)
			for i in range(nbslice):
				slice = waveguidesstructurewaveguides(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nwm_theta = nwm_thetaVal
				slice.nwm_phi = nwm_phiVal
				slice.mask = maskVal
				slice.npwbm_phi = npwbm_phiVal
				slice.npwe_phi = npwe_phiVal
				slice.sw_theta = sw_thetaVal
				slice.hw_theta = hw_thetaVal
				slice.bwa = bwaVal
				slice.biwp = biwpVal
				slice.bewp = bewpVal
				slice.e_phi = e_phiVal
				slice.scl = sclVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type waveguidesstructurewaveguidesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type waveguidesstructurewaveguidesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type waveguidesstructurewaveguidesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nwm_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nwm_theta', i, self.nwm_theta)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nwm_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nwm_phi', i, self.nwm_phi)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'mask') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'mask', i, numpy.array(self.mask).astype(numpy.int32))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'npwbm_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'npwbm_phi', i, self.npwbm_phi)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'npwe_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'npwe_phi', i, self.npwe_phi)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sw_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sw_theta', i, self.sw_theta)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'hw_theta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'hw_theta', i, self.hw_theta)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bwa') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bwa', i, self.bwa)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'biwp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'biwp', i, self.biwp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bewp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bewp', i, self.bewp)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'e_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'e_phi', i, numpy.array(self.e_phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scl', i, numpy.array(self.scl).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type waveguidesstructurewaveguidesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nwm_theta') 
			print ('obj = ' + str(obj))
		status, ret_nwm_theta = ull.getIntFromObject(self.idx, obj, cpopath + 'nwm_theta', i)
		check_status(status)
		if not status:
			self.nwm_theta = ret_nwm_theta
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nwm_phi') 
			print ('obj = ' + str(obj))
		status, ret_nwm_phi = ull.getIntFromObject(self.idx, obj, cpopath + 'nwm_phi', i)
		check_status(status)
		if not status:
			self.nwm_phi = ret_nwm_phi
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'mask') 
			print ('obj = ' + str(obj))
		status, ret_mask = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'mask', i)
		check_status(status)
		if not status:
			self.mask = ret_mask
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'npwbm_phi') 
			print ('obj = ' + str(obj))
		status, ret_npwbm_phi = ull.getIntFromObject(self.idx, obj, cpopath + 'npwbm_phi', i)
		check_status(status)
		if not status:
			self.npwbm_phi = ret_npwbm_phi
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'npwe_phi') 
			print ('obj = ' + str(obj))
		status, ret_npwe_phi = ull.getIntFromObject(self.idx, obj, cpopath + 'npwe_phi', i)
		check_status(status)
		if not status:
			self.npwe_phi = ret_npwe_phi
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sw_theta') 
			print ('obj = ' + str(obj))
		status, ret_sw_theta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sw_theta', i)
		check_status(status)
		if not status:
			self.sw_theta = ret_sw_theta
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'hw_theta') 
			print ('obj = ' + str(obj))
		status, ret_hw_theta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'hw_theta', i)
		check_status(status)
		if not status:
			self.hw_theta = ret_hw_theta
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bwa') 
			print ('obj = ' + str(obj))
		status, ret_bwa = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bwa', i)
		check_status(status)
		if not status:
			self.bwa = ret_bwa
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'biwp') 
			print ('obj = ' + str(obj))
		status, ret_biwp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'biwp', i)
		check_status(status)
		if not status:
			self.biwp = ret_biwp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bewp') 
			print ('obj = ' + str(obj))
		status, ret_bewp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bewp', i)
		check_status(status)
		if not status:
			self.bewp = ret_bewp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'e_phi') 
			print ('obj = ' + str(obj))
		status, ret_e_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'e_phi', i)
		check_status(status)
		if not status:
			self.e_phi = ret_e_phi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scl') 
			print ('obj = ' + str(obj))
		status, ret_scl = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scl', i)
		check_status(status)
		if not status:
			self.scl = ret_scl

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nwm_theta')
		ull.deleteData(self.idx, path, cpopath + 'nwm_phi')
		ull.deleteData(self.idx, path, cpopath + 'mask')
		ull.deleteData(self.idx, path, cpopath + 'npwbm_phi')
		ull.deleteData(self.idx, path, cpopath + 'npwe_phi')
		ull.deleteData(self.idx, path, cpopath + 'sw_theta')
		ull.deleteData(self.idx, path, cpopath + 'hw_theta')
		ull.deleteData(self.idx, path, cpopath + 'bwa')
		ull.deleteData(self.idx, path, cpopath + 'biwp')
		ull.deleteData(self.idx, path, cpopath + 'bewp')
		ull.deleteData(self.idx, path, cpopath + 'e_phi')
		ull.deleteData(self.idx, path, cpopath + 'scl')


class plasmaedgestructureplasmaedge:
	'''
	class plasmaedgestructureplasmaedge
	Plasma edge characteristics in front of the antenna.

	Attributes:
	- npoints : int
	   Number of points in the distance grid. Integer
	- distance : numpy.ndarray 1D with float
	   Grid for electron density, defined as the perpendicular distance to the antenna waveguide plane (the origin being described in the position sub-structure) [m]. Vector (npoints). Time-dependent.
	- density : numpy.ndarray 1D with float
	   Electron density in front of the antenna [m^-3]. Vector (npoints). Time-dependent.
	'''

	def __init__(self, base_path_in='plasmaedge'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.npoints = EMPTY_INT
		self.distance = numpy.zeros(0, numpy.float64, order='C')
		self.density = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class plasmaedgestructureplasmaedge\n'
		ret = ret + space + 'Attribute npoints: ' + str(self.npoints) + '\n'
		s = self.distance.__str__()
		ret = ret + space + 'Attribute distance\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.density.__str__()
		ret = ret + space + 'Attribute density\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmaedgestructureplasmaedge, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'distance', numpy.array(self.distance).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'density', numpy.array(self.density).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmaedgestructureplasmaedge, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'distance', numpy.array(self.distance).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'density', numpy.array(self.density).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmaedgestructureplasmaedge, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'npoints', self.npoints)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type plasmaedgestructureplasmaedge, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_npoints = ull.getInt(self.idx, path, cpopath + 'npoints')
		check_status(status)
		if not status:
			self.npoints = ret_npoints
		status, ret_distance, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'distance', inTime, interpolMode)
		check_status(status)
		if not status:
			self.distance = ret_distance
			self.cpoTime = retTime
		status, ret_density, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'density', inTime, interpolMode)
		check_status(status)
		if not status:
			self.density = ret_density
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type plasmaedgestructureplasmaedge, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, npointsVal = ull.getInt(self.idx, path, cpopath + 'npoints')
			check_status(status)
			status, distanceList = ull.getVect2DDouble(self.idx, path, cpopath + 'distance')
			if len(distanceList) == 0:
				distanceList = numpy.resize(distanceList, (0,nbslice))
			check_status(status)
			status, densityList = ull.getVect2DDouble(self.idx, path, cpopath + 'density')
			if len(densityList) == 0:
				densityList = numpy.resize(densityList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = plasmaedgestructureplasmaedge(self.base_path)
				slice.setExpIdx(self.idx)
				slice.npoints = npointsVal
				slice.distance = distanceList[:,i]
				slice.density = densityList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmaedgestructureplasmaedgeObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'distance') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'distance', i, numpy.array(self.distance).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'density', i, numpy.array(self.density).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmaedgestructureplasmaedgeObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'distance') 
			print ('obj = ' + str(obj))
		status, ret_distance = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'distance', i)
		check_status(status)
		if not status:
			self.distance = ret_distance
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		status, ret_density = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'density', i)
		check_status(status)
		if not status:
			self.density = ret_density

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmaedgestructureplasmaedgeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'npoints', i, self.npoints)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmaedgestructureplasmaedgeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		status, ret_npoints = ull.getIntFromObject(self.idx, obj, cpopath + 'npoints', i)
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
		ull.deleteData(self.idx, path, cpopath + 'npoints')
		ull.deleteData(self.idx, path, cpopath + 'distance')
		ull.deleteData(self.idx, path, cpopath + 'density')
