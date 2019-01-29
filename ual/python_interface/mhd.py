# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class mhd:
	'''
	class mhd
	MHD linear stability. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- toroid_field : class toroid_fieldstructureb0r0
	   Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to document the normalisation of rho and j in this CPO.
	- n : class nstruct_arraymhd_mode: array of nstruct_arraymhd_modeObj objects
	   Vector of toroidal mode numbers; Structure Array (ntor); Time-dependent
	- time : float
	   Time [s]; Time-dependent; Scalar.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self):
		self.base_path = 'mhd'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 10
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.toroid_field = toroid_fieldstructureb0r0('toroid_field')
		self.n = nstruct_arraymhd_mode('n')
		self.time = EMPTY_DOUBLE
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mhd\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute toroid_field\n ' + self.toroid_field.__str__(depth+1)
		ret = ret + space + 'Attribute n\n ' + self.n.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.toroid_field.setExpIdx(idx)
		self.n.setExpIdx(idx)
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
		self.toroid_field.cpoTime = self.cpoTime
		self.toroid_field.putSlice(path, cpopath)
		self.n.cpoTime = self.cpoTime
		self.n.putSlice(path, cpopath)
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
		self.toroid_field.replaceLastSlice(path, cpopath)
		self.n.replaceLastSlice(path, cpopath)
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
		self.toroid_field.putNonTimed(path, cpopath)
		self.n.putNonTimed(path, cpopath)
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
		self.toroid_field.getSlice(path, cpopath, inTime, interpolMode)
		self.n.getSlice(path, cpopath, inTime, interpolMode)
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
			toroid_fieldList = self.toroid_field.build_non_resampled_data(path, cpopath, nbslice)
			nList = self.n.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			array = []
			for i in range(nbslice):
				slice = mhd()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.toroid_field = toroid_fieldList[i]
				slice.n = nList[i]
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
		self.toroid_field.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'n')
		ull.deleteData(self.idx, path, cpopath + 'time')
		self.codeparam.deleteData(path, cpopath)


class mhdArray:
	'''
	class mhdArray
	MHD linear stability. Time-dependent CPO

	Attributes:
	- array : list of mhd
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
		ret = space + 'class mhdArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'mhd cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = mhd()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(mhd())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = mhd()
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


class toroid_fieldstructureb0r0:
	'''
	class toroid_fieldstructureb0r0
	Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to document the normalisation of rho and j in this CPO.

	Attributes:
	- r0 : float
	   Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Scalar. 
	- b0 : float
	   Vacuum field at r0 [T]; Positive sign means anti-clockwise when viewed from above. Scalar. Time-dependent. 
	'''

	def __init__(self, base_path_in='toroid_field'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r0 = EMPTY_DOUBLE
		self.b0 = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class toroid_fieldstructureb0r0\n'
		ret = ret + space + 'Attribute r0: ' + str(self.r0) + '\n'
		ret = ret + space + 'Attribute b0: ' + str(self.b0) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'b0', self.b0, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'b0', self.b0)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r0 = ull.getDouble(self.idx, path, cpopath + 'r0')
		check_status(status)
		if not status:
			self.r0 = ret_r0
		status, ret_b0, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'b0', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b0 = ret_b0
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, r0Val = ull.getDouble(self.idx, path, cpopath + 'r0')
			check_status(status)
			status, b0List = ull.getVect1DDouble(self.idx, path, cpopath + 'b0')
			if len(b0List) == 0:
				b0List = numpy.resize(b0List, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = toroid_fieldstructureb0r0(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r0 = r0Val
				slice.b0 = b0List[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b0') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b0', i, self.b0)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b0') 
			print ('obj = ' + str(obj))
		status, ret_b0 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b0', i)
		check_status(status)
		if not status:
			self.b0 = ret_b0

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r0') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r0', i, self.r0)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'r0')
		ull.deleteData(self.idx, path, cpopath + 'b0')


class nstruct_arraymhd_mode:
	'''
	class nstruct_arraymhd_mode
	Vector of toroidal mode numbers; Structure Array (ntor); Time-dependent

	Attributes:
	- array : list of nstruct_arraymhd_modeObj 
	'''

	def __init__(self, base_path_in='n'):
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
		ret = space + 'class nstruct_arraymhd_mode\n'
		for i in range(len(self.array)):
			ret = ret + space + 'nstruct_arraymhd_mode[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(nstruct_arraymhd_modeObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function putSlice') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function getSlice') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(nstruct_arraymhd_mode(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(nstruct_arraymhd_mode(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = nstruct_arraymhd_mode(self.base_path)
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type nstruct_arraymhd_mode, run function getNonTimedElt') 
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


class nstruct_arraymhd_modeObj:
	'''
	class nstruct_arraymhd_modeObj
	Vector of toroidal mode numbers; Structure Array (ntor); Time-dependent

	Attributes:
	- modenum : int
	   Toroidal mode number of the MHD mode; Scalar; Time-dependent.
	- growthrate : float
	   Linear growthrate of the mode [Hz]; Scalar; Time-dependent.
	- frequency : float
	   Frequency of the mode [Hz]; Scalar; Time-dependent.
	- plasma : class plasmastructuremhd_plasma
	   MHD modes in the confined plasma
	- vacuum : class vacuumstructuremhd_vacuum
	   External modes
	'''

	def __init__(self, base_path_in='n'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.modenum = EMPTY_INT
		self.growthrate = EMPTY_DOUBLE
		self.frequency = EMPTY_DOUBLE
		self.plasma = plasmastructuremhd_plasma('plasma')
		self.vacuum = vacuumstructuremhd_vacuum('vacuum')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nstruct_arraymhd_modeObj\n'
		ret = ret + space + 'Attribute modenum: ' + str(self.modenum) + '\n'
		ret = ret + space + 'Attribute growthrate: ' + str(self.growthrate) + '\n'
		ret = ret + space + 'Attribute frequency: ' + str(self.frequency) + '\n'
		ret = ret + space + 'Attribute plasma\n ' + self.plasma.__str__(depth+1)
		ret = ret + space + 'Attribute vacuum\n ' + self.vacuum.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.plasma.setExpIdx(idx)
		self.vacuum.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nstruct_arraymhd_modeObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'modenum') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'modenum', i, self.modenum)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'growthrate') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'growthrate', i, self.growthrate)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'frequency', i, self.frequency)
		obj = self.plasma.putTimedElt(path, cpopath + 'plasma', i, obj)
		obj = self.vacuum.putTimedElt(path, cpopath + 'vacuum', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nstruct_arraymhd_modeObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'modenum') 
			print ('obj = ' + str(obj))
		status, ret_modenum = ull.getIntFromObject(self.idx, obj, cpopath + 'modenum', i)
		check_status(status)
		if not status:
			self.modenum = ret_modenum
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'growthrate') 
			print ('obj = ' + str(obj))
		status, ret_growthrate = ull.getDoubleFromObject(self.idx, obj, cpopath + 'growthrate', i)
		check_status(status)
		if not status:
			self.growthrate = ret_growthrate
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		status, ret_frequency = ull.getDoubleFromObject(self.idx, obj, cpopath + 'frequency', i)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		self.plasma.getTimedElt(path, cpopath + 'plasma', i, obj)
		self.vacuum.getTimedElt(path, cpopath + 'vacuum', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nstruct_arraymhd_modeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.plasma.putNonTimedElt(path, cpopath + 'plasma', i, obj)
		obj = self.vacuum.putNonTimedElt(path, cpopath + 'vacuum', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nstruct_arraymhd_modeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.plasma.getNonTimedElt(path, cpopath + 'plasma', i, obj)
		self.vacuum.getNonTimedElt(path, cpopath + 'vacuum', i, obj)


class plasmastructuremhd_plasma:
	'''
	class plasmastructuremhd_plasma
	MHD modes in the confined plasma

	Attributes:
	- psi : numpy.ndarray 1D with float
	   Position in poloidal flux [Wb] (without 1/2pi and such that Bp=|grad psi| /R/2/pi). Time-dependent; Vector (npsi)
	- rho_tor_norm : numpy.ndarray 1D with float
	   Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point);  Time-dependent; Vector (nrho)
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m]; Vector (nrho). Time-dependent.
	- m : numpy.ndarray 2D with float
	   Poloidal mode number; Time-dependent; Array2D (npsi,nm)
	- disp_perp : numpy.ndarray 2D with  complex numbers
	   Perpendicular displacement of the mode (in Fourier space) [m]; Time-dependent; Array 2D (npsi,nm)
	- disp_par : numpy.ndarray 2D with  complex numbers
	   Parallel displacement of the mode (in Fourier space) [m]; Time-dependent; Array 2D (npsi,nm)
	- tau_alfven : numpy.ndarray 1D with float
	   Alven time=R/vA=R0 sqrt(mi ni(rho))/B0 [s]; Definitions of R0, BO, mi, ni to be clarified. rho grid should be included in the MHD CPO ? Time-dependent; Vector (npsi)
	- tau_res : numpy.ndarray 1D with float
	   Resistive time = mu_0 rho*rho/1.22/eta_neo [s]; Source of eta_neo to be clarified. Time-dependent; Vector (npsi)
	- coord_sys : class coord_sysstructurecoord_sys
	   flux surface coordinate system on a square grid of flux and angle
	- a_pert : class a_pertstructuremhd_vector
	   Pertubed vector potential (in Fourier space) [T.m]
	- b_pert : class b_pertstructuremhd_vector
	   Perturbed magnetic field (in Fourier space) [T]
	- v_pert : class v_pertstructuremhd_vector
	   Perturbed velocity (in Fourier space) [m/s]
	- p_pert : numpy.ndarray 2D with  complex numbers
	   Perturbed pressure (in Fourier space) [Pa]; Time-dependent; Array 2D (npsi,nm)
	- rho_mass_per : numpy.ndarray 2D with  complex numbers
	   Perturbed mass density (in Fourier space) [kg/m^3]; Time-dependent; Array 2D (npsi,nm)
	- temp_per : numpy.ndarray 2D with  complex numbers
	   Perturbed temperature (in Fourier space) [eV]; Time-dependent; Array 2D (npsi,nm)
	'''

	def __init__(self, base_path_in='plasma'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.m = numpy.zeros((0,0), numpy.float64, order='C')
		self.disp_perp = numpy.zeros((0,0), numpy.complex128, order='C')
		self.disp_par = numpy.zeros((0,0), numpy.complex128, order='C')
		self.tau_alfven = numpy.zeros(0, numpy.float64, order='C')
		self.tau_res = numpy.zeros(0, numpy.float64, order='C')
		self.coord_sys = coord_sysstructurecoord_sys('coord_sys')
		self.a_pert = a_pertstructuremhd_vector('a_pert')
		self.b_pert = b_pertstructuremhd_vector('b_pert')
		self.v_pert = v_pertstructuremhd_vector('v_pert')
		self.p_pert = numpy.zeros((0,0), numpy.complex128, order='C')
		self.rho_mass_per = numpy.zeros((0,0), numpy.complex128, order='C')
		self.temp_per = numpy.zeros((0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class plasmastructuremhd_plasma\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.m.__str__()
		ret = ret + space + 'Attribute m\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.disp_perp.__str__()
		ret = ret + space + 'Attribute disp_perp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.disp_par.__str__()
		ret = ret + space + 'Attribute disp_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.tau_alfven.__str__()
		ret = ret + space + 'Attribute tau_alfven\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.tau_res.__str__()
		ret = ret + space + 'Attribute tau_res\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute coord_sys\n ' + self.coord_sys.__str__(depth+1)
		ret = ret + space + 'Attribute a_pert\n ' + self.a_pert.__str__(depth+1)
		ret = ret + space + 'Attribute b_pert\n ' + self.b_pert.__str__(depth+1)
		ret = ret + space + 'Attribute v_pert\n ' + self.v_pert.__str__(depth+1)
		s = self.p_pert.__str__()
		ret = ret + space + 'Attribute p_pert\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_mass_per.__str__()
		ret = ret + space + 'Attribute rho_mass_per\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.temp_per.__str__()
		ret = ret + space + 'Attribute temp_per\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.coord_sys.setExpIdx(idx)
		self.a_pert.setExpIdx(idx)
		self.b_pert.setExpIdx(idx)
		self.v_pert.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructuremhd_plasma, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'm', numpy.array(self.m).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'disp_perp', numpy.array(self.disp_perp).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'disp_par', numpy.array(self.disp_par).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'tau_alfven', numpy.array(self.tau_alfven).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'tau_res', numpy.array(self.tau_res).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.coord_sys.cpoTime = self.cpoTime
		self.coord_sys.putSlice(path, cpopath)
		self.a_pert.cpoTime = self.cpoTime
		self.a_pert.putSlice(path, cpopath)
		self.b_pert.cpoTime = self.cpoTime
		self.b_pert.putSlice(path, cpopath)
		self.v_pert.cpoTime = self.cpoTime
		self.v_pert.putSlice(path, cpopath)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'p_pert', numpy.array(self.p_pert).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'rho_mass_per', numpy.array(self.rho_mass_per).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'temp_per', numpy.array(self.temp_per).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructuremhd_plasma, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'm', numpy.array(self.m).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'disp_perp', numpy.array(self.disp_perp).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'disp_par', numpy.array(self.disp_par).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'tau_alfven', numpy.array(self.tau_alfven).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'tau_res', numpy.array(self.tau_res).astype(numpy.float64))
		check_status(status)
		self.coord_sys.replaceLastSlice(path, cpopath)
		self.a_pert.replaceLastSlice(path, cpopath)
		self.b_pert.replaceLastSlice(path, cpopath)
		self.v_pert.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'p_pert', numpy.array(self.p_pert).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'rho_mass_per', numpy.array(self.rho_mass_per).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'temp_per', numpy.array(self.temp_per).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructuremhd_plasma, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coord_sys.putNonTimed(path, cpopath)
		self.a_pert.putNonTimed(path, cpopath)
		self.b_pert.putNonTimed(path, cpopath)
		self.v_pert.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructuremhd_plasma, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_psi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
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
		status, ret_m, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.m = ret_m
			self.cpoTime = retTime
		status, ret_disp_perp, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'disp_perp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.disp_perp = ret_disp_perp
			self.cpoTime = retTime
		status, ret_disp_par, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'disp_par', inTime, interpolMode)
		check_status(status)
		if not status:
			self.disp_par = ret_disp_par
			self.cpoTime = retTime
		status, ret_tau_alfven, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'tau_alfven', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tau_alfven = ret_tau_alfven
			self.cpoTime = retTime
		status, ret_tau_res, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'tau_res', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tau_res = ret_tau_res
			self.cpoTime = retTime
		self.coord_sys.getSlice(path, cpopath, inTime, interpolMode)
		self.a_pert.getSlice(path, cpopath, inTime, interpolMode)
		self.b_pert.getSlice(path, cpopath, inTime, interpolMode)
		self.v_pert.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_p_pert, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'p_pert', inTime, interpolMode)
		check_status(status)
		if not status:
			self.p_pert = ret_p_pert
			self.cpoTime = retTime
		status, ret_rho_mass_per, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'rho_mass_per', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_mass_per = ret_rho_mass_per
			self.cpoTime = retTime
		status, ret_temp_per, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'temp_per', inTime, interpolMode)
		check_status(status)
		if not status:
			self.temp_per = ret_temp_per
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructuremhd_plasma, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, psiList = ull.getVect2DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,nbslice))
			check_status(status)
			status, rho_tor_normList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			if len(rho_tor_normList) == 0:
				rho_tor_normList = numpy.resize(rho_tor_normList, (0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			status, mList = ull.getVect3DDouble(self.idx, path, cpopath + 'm')
			if len(mList) == 0:
				mList = numpy.resize(mList, (0,0,nbslice))
			check_status(status)
			status, disp_perpList = ull.getVect3DComplex(self.idx, path, cpopath + 'disp_perp')
			if len(disp_perpList) == 0:
				disp_perpList = numpy.resize(disp_perpList, (0,0,nbslice))
			check_status(status)
			status, disp_parList = ull.getVect3DComplex(self.idx, path, cpopath + 'disp_par')
			if len(disp_parList) == 0:
				disp_parList = numpy.resize(disp_parList, (0,0,nbslice))
			check_status(status)
			status, tau_alfvenList = ull.getVect2DDouble(self.idx, path, cpopath + 'tau_alfven')
			if len(tau_alfvenList) == 0:
				tau_alfvenList = numpy.resize(tau_alfvenList, (0,nbslice))
			check_status(status)
			status, tau_resList = ull.getVect2DDouble(self.idx, path, cpopath + 'tau_res')
			if len(tau_resList) == 0:
				tau_resList = numpy.resize(tau_resList, (0,nbslice))
			check_status(status)
			coord_sysList = self.coord_sys.build_non_resampled_data(path, cpopath, nbslice)
			a_pertList = self.a_pert.build_non_resampled_data(path, cpopath, nbslice)
			b_pertList = self.b_pert.build_non_resampled_data(path, cpopath, nbslice)
			v_pertList = self.v_pert.build_non_resampled_data(path, cpopath, nbslice)
			status, p_pertList = ull.getVect3DComplex(self.idx, path, cpopath + 'p_pert')
			if len(p_pertList) == 0:
				p_pertList = numpy.resize(p_pertList, (0,0,nbslice))
			check_status(status)
			status, rho_mass_perList = ull.getVect3DComplex(self.idx, path, cpopath + 'rho_mass_per')
			if len(rho_mass_perList) == 0:
				rho_mass_perList = numpy.resize(rho_mass_perList, (0,0,nbslice))
			check_status(status)
			status, temp_perList = ull.getVect3DComplex(self.idx, path, cpopath + 'temp_per')
			if len(temp_perList) == 0:
				temp_perList = numpy.resize(temp_perList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = plasmastructuremhd_plasma(self.base_path)
				slice.setExpIdx(self.idx)
				slice.psi = psiList[:,i]
				slice.rho_tor_norm = rho_tor_normList[:,i]
				slice.rho_tor = rho_torList[:,i]
				slice.m = mList[:,:,i]
				slice.disp_perp = disp_perpList[:,:,i].copy().astype(complex)
				slice.disp_par = disp_parList[:,:,i].copy().astype(complex)
				slice.tau_alfven = tau_alfvenList[:,i]
				slice.tau_res = tau_resList[:,i]
				slice.coord_sys = coord_sysList[i]
				slice.a_pert = a_pertList[i]
				slice.b_pert = b_pertList[i]
				slice.v_pert = v_pertList[i]
				slice.p_pert = p_pertList[:,:,i].copy().astype(complex)
				slice.rho_mass_per = rho_mass_perList[:,:,i].copy().astype(complex)
				slice.temp_per = temp_perList[:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructuremhd_plasmaObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor_norm', i, numpy.array(self.rho_tor_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'm', i, numpy.array(self.m).astype(numpy.float64))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'disp_perp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'disp_perp', i, numpy.array(self.disp_perp).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'disp_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'disp_par', i, numpy.array(self.disp_par).astype(numpy.complex128))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'tau_alfven') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'tau_alfven', i, numpy.array(self.tau_alfven).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'tau_res') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'tau_res', i, numpy.array(self.tau_res).astype(numpy.float64))
		obj = self.coord_sys.putTimedElt(path, cpopath + 'coord_sys', i, obj)
		obj = self.a_pert.putTimedElt(path, cpopath + 'a_pert', i, obj)
		obj = self.b_pert.putTimedElt(path, cpopath + 'b_pert', i, obj)
		obj = self.v_pert.putTimedElt(path, cpopath + 'v_pert', i, obj)
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'p_pert') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'p_pert', i, numpy.array(self.p_pert).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'rho_mass_per') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'rho_mass_per', i, numpy.array(self.rho_mass_per).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'temp_per') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'temp_per', i, numpy.array(self.temp_per).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructuremhd_plasmaObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor_norm = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor_norm', i)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		status, ret_m = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'm', i)
		check_status(status)
		if not status:
			self.m = ret_m
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'disp_perp') 
			print ('obj = ' + str(obj))
		status, ret_disp_perp = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'disp_perp', i)
		check_status(status)
		if not status:
			self.disp_perp = ret_disp_perp
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'disp_par') 
			print ('obj = ' + str(obj))
		status, ret_disp_par = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'disp_par', i)
		check_status(status)
		if not status:
			self.disp_par = ret_disp_par
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'tau_alfven') 
			print ('obj = ' + str(obj))
		status, ret_tau_alfven = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'tau_alfven', i)
		check_status(status)
		if not status:
			self.tau_alfven = ret_tau_alfven
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'tau_res') 
			print ('obj = ' + str(obj))
		status, ret_tau_res = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'tau_res', i)
		check_status(status)
		if not status:
			self.tau_res = ret_tau_res
		self.coord_sys.getTimedElt(path, cpopath + 'coord_sys', i, obj)
		self.a_pert.getTimedElt(path, cpopath + 'a_pert', i, obj)
		self.b_pert.getTimedElt(path, cpopath + 'b_pert', i, obj)
		self.v_pert.getTimedElt(path, cpopath + 'v_pert', i, obj)
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'p_pert') 
			print ('obj = ' + str(obj))
		status, ret_p_pert = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'p_pert', i)
		check_status(status)
		if not status:
			self.p_pert = ret_p_pert
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'rho_mass_per') 
			print ('obj = ' + str(obj))
		status, ret_rho_mass_per = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'rho_mass_per', i)
		check_status(status)
		if not status:
			self.rho_mass_per = ret_rho_mass_per
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'temp_per') 
			print ('obj = ' + str(obj))
		status, ret_temp_per = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'temp_per', i)
		check_status(status)
		if not status:
			self.temp_per = ret_temp_per

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructuremhd_plasmaObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.coord_sys.putNonTimedElt(path, cpopath + 'coord_sys', i, obj)
		obj = self.a_pert.putNonTimedElt(path, cpopath + 'a_pert', i, obj)
		obj = self.b_pert.putNonTimedElt(path, cpopath + 'b_pert', i, obj)
		obj = self.v_pert.putNonTimedElt(path, cpopath + 'v_pert', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructuremhd_plasmaObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.coord_sys.getNonTimedElt(path, cpopath + 'coord_sys', i, obj)
		self.a_pert.getNonTimedElt(path, cpopath + 'a_pert', i, obj)
		self.b_pert.getNonTimedElt(path, cpopath + 'b_pert', i, obj)
		self.v_pert.getNonTimedElt(path, cpopath + 'v_pert', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor_norm')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		ull.deleteData(self.idx, path, cpopath + 'm')
		ull.deleteData(self.idx, path, cpopath + 'disp_perp')
		ull.deleteData(self.idx, path, cpopath + 'disp_par')
		ull.deleteData(self.idx, path, cpopath + 'tau_alfven')
		ull.deleteData(self.idx, path, cpopath + 'tau_res')
		self.coord_sys.deleteData(path, cpopath)
		self.a_pert.deleteData(path, cpopath)
		self.b_pert.deleteData(path, cpopath)
		self.v_pert.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'p_pert')
		ull.deleteData(self.idx, path, cpopath + 'rho_mass_per')
		ull.deleteData(self.idx, path, cpopath + 'temp_per')


class coord_sysstructurecoord_sys:
	'''
	class coord_sysstructurecoord_sys
	flux surface coordinate system on a square grid of flux and angle

	Attributes:
	- grid_type : str
	   Type of coordinate system
	- grid : class gridstructurereggrid
	   Regular grid definition; Time-dependent
	- jacobian : numpy.ndarray 2D with float
	   Jacobian of the coordinate system; Time-dependent; Matrix (ndim1, ndim2)
	- g_11 : numpy.ndarray 2D with float
	   metric coefficients g_11; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_12 : numpy.ndarray 2D with float
	   metric coefficients g_12; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_13 : numpy.ndarray 2D with float
	   metric coefficients g_13; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_22 : numpy.ndarray 2D with float
	   metric coefficients g_22; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_23 : numpy.ndarray 2D with float
	   metric coefficients g_23; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_33 : numpy.ndarray 2D with float
	   metric coefficients g_33; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- position : class positionstructurerz2D
	   R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)
	'''

	def __init__(self, base_path_in='coord_sys'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_type = ''
		self.grid = gridstructurereggrid('grid')
		self.jacobian = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_11 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_12 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_13 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_22 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_23 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_33 = numpy.zeros((0,0), numpy.float64, order='C')
		self.position = positionstructurerz2D('position')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coord_sysstructurecoord_sys\n'
		ret = ret + space + 'Attribute grid_type: ' + str(self.grid_type) + '\n'
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		s = self.jacobian.__str__()
		ret = ret + space + 'Attribute jacobian\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_11.__str__()
		ret = ret + space + 'Attribute g_11\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_12.__str__()
		ret = ret + space + 'Attribute g_12\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_13.__str__()
		ret = ret + space + 'Attribute g_13\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_22.__str__()
		ret = ret + space + 'Attribute g_22\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_23.__str__()
		ret = ret + space + 'Attribute g_23\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_33.__str__()
		ret = ret + space + 'Attribute g_33\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid.setExpIdx(idx)
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.cpoTime = self.cpoTime
		self.grid.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'jacobian', numpy.array(self.jacobian).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_11', numpy.array(self.g_11).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_12', numpy.array(self.g_12).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_13', numpy.array(self.g_13).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_22', numpy.array(self.g_22).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_23', numpy.array(self.g_23).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_33', numpy.array(self.g_33).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'jacobian', numpy.array(self.jacobian).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_11', numpy.array(self.g_11).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_12', numpy.array(self.g_12).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_13', numpy.array(self.g_13).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_22', numpy.array(self.g_22).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_23', numpy.array(self.g_23).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_33', numpy.array(self.g_33).astype(numpy.float64))
		check_status(status)
		self.position.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'grid_type', self.grid_type)
		check_status(status)
		self.grid.putNonTimed(path, cpopath)
		self.position.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_grid_type = ull.getString(self.idx, path, cpopath + 'grid_type')
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.grid.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_jacobian, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'jacobian', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jacobian = ret_jacobian
			self.cpoTime = retTime
		status, ret_g_11, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_11', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_11 = ret_g_11
			self.cpoTime = retTime
		status, ret_g_12, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_12', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_12 = ret_g_12
			self.cpoTime = retTime
		status, ret_g_13, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_13', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_13 = ret_g_13
			self.cpoTime = retTime
		status, ret_g_22, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_22', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_22 = ret_g_22
			self.cpoTime = retTime
		status, ret_g_23, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_23', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_23 = ret_g_23
			self.cpoTime = retTime
		status, ret_g_33, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_33', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_33 = ret_g_33
			self.cpoTime = retTime
		self.position.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, grid_typeVal = ull.getString(self.idx, path, cpopath + 'grid_type')
			check_status(status)
			gridList = self.grid.build_non_resampled_data(path, cpopath, nbslice)
			status, jacobianList = ull.getVect3DDouble(self.idx, path, cpopath + 'jacobian')
			if len(jacobianList) == 0:
				jacobianList = numpy.resize(jacobianList, (0,0,nbslice))
			check_status(status)
			status, g_11List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_11')
			if len(g_11List) == 0:
				g_11List = numpy.resize(g_11List, (0,0,nbslice))
			check_status(status)
			status, g_12List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_12')
			if len(g_12List) == 0:
				g_12List = numpy.resize(g_12List, (0,0,nbslice))
			check_status(status)
			status, g_13List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_13')
			if len(g_13List) == 0:
				g_13List = numpy.resize(g_13List, (0,0,nbslice))
			check_status(status)
			status, g_22List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_22')
			if len(g_22List) == 0:
				g_22List = numpy.resize(g_22List, (0,0,nbslice))
			check_status(status)
			status, g_23List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_23')
			if len(g_23List) == 0:
				g_23List = numpy.resize(g_23List, (0,0,nbslice))
			check_status(status)
			status, g_33List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_33')
			if len(g_33List) == 0:
				g_33List = numpy.resize(g_33List, (0,0,nbslice))
			check_status(status)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = coord_sysstructurecoord_sys(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid_type = grid_typeVal
				slice.grid = gridList[i]
				slice.jacobian = jacobianList[:,:,i]
				slice.g_11 = g_11List[:,:,i]
				slice.g_12 = g_12List[:,:,i]
				slice.g_13 = g_13List[:,:,i]
				slice.g_22 = g_22List[:,:,i]
				slice.g_23 = g_23List[:,:,i]
				slice.g_33 = g_33List[:,:,i]
				slice.position = positionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'jacobian') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'jacobian', i, numpy.array(self.jacobian).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_11') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_11', i, numpy.array(self.g_11).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_12') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_12', i, numpy.array(self.g_12).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_13') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_13', i, numpy.array(self.g_13).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_22') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_22', i, numpy.array(self.g_22).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_23') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_23', i, numpy.array(self.g_23).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_33') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_33', i, numpy.array(self.g_33).astype(numpy.float64))
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'jacobian') 
			print ('obj = ' + str(obj))
		status, ret_jacobian = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'jacobian', i)
		check_status(status)
		if not status:
			self.jacobian = ret_jacobian
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_11') 
			print ('obj = ' + str(obj))
		status, ret_g_11 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_11', i)
		check_status(status)
		if not status:
			self.g_11 = ret_g_11
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_12') 
			print ('obj = ' + str(obj))
		status, ret_g_12 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_12', i)
		check_status(status)
		if not status:
			self.g_12 = ret_g_12
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_13') 
			print ('obj = ' + str(obj))
		status, ret_g_13 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_13', i)
		check_status(status)
		if not status:
			self.g_13 = ret_g_13
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_22') 
			print ('obj = ' + str(obj))
		status, ret_g_22 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_22', i)
		check_status(status)
		if not status:
			self.g_22 = ret_g_22
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_23') 
			print ('obj = ' + str(obj))
		status, ret_g_23 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_23', i)
		check_status(status)
		if not status:
			self.g_23 = ret_g_23
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_33') 
			print ('obj = ' + str(obj))
		status, ret_g_33 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_33', i)
		check_status(status)
		if not status:
			self.g_33 = ret_g_33
		self.position.getTimedElt(path, cpopath + 'position', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'grid_type', i, self.grid_type)
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		status, ret_grid_type = ull.getStringFromObject(self.idx, obj, cpopath + 'grid_type', i)
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'grid_type')
		self.grid.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'jacobian')
		ull.deleteData(self.idx, path, cpopath + 'g_11')
		ull.deleteData(self.idx, path, cpopath + 'g_12')
		ull.deleteData(self.idx, path, cpopath + 'g_13')
		ull.deleteData(self.idx, path, cpopath + 'g_22')
		ull.deleteData(self.idx, path, cpopath + 'g_23')
		ull.deleteData(self.idx, path, cpopath + 'g_33')
		self.position.deleteData(path, cpopath)


class gridstructurereggrid:
	'''
	class gridstructurereggrid
	Regular grid definition; Time-dependent

	Attributes:
	- dim1 : numpy.ndarray 1D with float
	   First dimension values; Vector (ndim1) 
	- dim2 : numpy.ndarray 1D with float
	   Second dimension values; Vector (ndim2) 
	'''

	def __init__(self, base_path_in='grid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dim1 = numpy.zeros(0, numpy.float64, order='C')
		self.dim2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class gridstructurereggrid\n'
		s = self.dim1.__str__()
		ret = ret + space + 'Attribute dim1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim2.__str__()
		ret = ret + space + 'Attribute dim2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dim1, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
			self.cpoTime = retTime
		status, ret_dim2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dim1List = ull.getVect2DDouble(self.idx, path, cpopath + 'dim1')
			if len(dim1List) == 0:
				dim1List = numpy.resize(dim1List, (0,nbslice))
			check_status(status)
			status, dim2List = ull.getVect2DDouble(self.idx, path, cpopath + 'dim2')
			if len(dim2List) == 0:
				dim2List = numpy.resize(dim2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = gridstructurereggrid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dim1 = dim1List[:,i]
				slice.dim2 = dim2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim1', i, numpy.array(self.dim1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim2', i, numpy.array(self.dim2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		status, ret_dim1 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim1', i)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		status, ret_dim2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim2', i)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dim1')
		ull.deleteData(self.idx, path, cpopath + 'dim2')


class positionstructurerz2D:
	'''
	class positionstructurerz2D
	R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)

	Attributes:
	- r : numpy.ndarray 2D with float
	   Major radius [m]
	- z : numpy.ndarray 2D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerz2D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,0,nbslice))
			check_status(status)
			status, zList = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerz2D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,:,i]
				slice.z = zList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function getTimedElt') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function getNonTimedElt') 
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


class a_pertstructuremhd_vector:
	'''
	class a_pertstructuremhd_vector
	Pertubed vector potential (in Fourier space) [T.m]

	Attributes:
	- coord1 : numpy.ndarray 2D with  complex numbers
	   Fourier components of first coordinate; Time-dependent; Array 2D (npsi,nm)
	- coord2 : numpy.ndarray 2D with  complex numbers
	   Fourier components of second coordinate; Time-dependent; Array 2D (npsi,nm)
	- coord3 : numpy.ndarray 2D with  complex numbers
	   Fourier components of third coordinate; Time-dependent; Array 2D (npsi,nm)
	'''

	def __init__(self, base_path_in='a_pert'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.coord1 = numpy.zeros((0,0), numpy.complex128, order='C')
		self.coord2 = numpy.zeros((0,0), numpy.complex128, order='C')
		self.coord3 = numpy.zeros((0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class a_pertstructuremhd_vector\n'
		s = self.coord1.__str__()
		ret = ret + space + 'Attribute coord1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord2.__str__()
		ret = ret + space + 'Attribute coord2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord3.__str__()
		ret = ret + space + 'Attribute coord3\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type a_pertstructuremhd_vector, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord1', numpy.array(self.coord1).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord2', numpy.array(self.coord2).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord3', numpy.array(self.coord3).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type a_pertstructuremhd_vector, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord1', numpy.array(self.coord1).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord2', numpy.array(self.coord2).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord3', numpy.array(self.coord3).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type a_pertstructuremhd_vector, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type a_pertstructuremhd_vector, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_coord1, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
			self.cpoTime = retTime
		status, ret_coord2, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2
			self.cpoTime = retTime
		status, ret_coord3, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord3', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord3 = ret_coord3
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type a_pertstructuremhd_vector, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, coord1List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord1')
			if len(coord1List) == 0:
				coord1List = numpy.resize(coord1List, (0,0,nbslice))
			check_status(status)
			status, coord2List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord2')
			if len(coord2List) == 0:
				coord2List = numpy.resize(coord2List, (0,0,nbslice))
			check_status(status)
			status, coord3List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord3')
			if len(coord3List) == 0:
				coord3List = numpy.resize(coord3List, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = a_pertstructuremhd_vector(self.base_path)
				slice.setExpIdx(self.idx)
				slice.coord1 = coord1List[:,:,i].copy().astype(complex)
				slice.coord2 = coord2List[:,:,i].copy().astype(complex)
				slice.coord3 = coord3List[:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type a_pertstructuremhd_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord1', i, numpy.array(self.coord1).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord2', i, numpy.array(self.coord2).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord3') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord3', i, numpy.array(self.coord3).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type a_pertstructuremhd_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		status, ret_coord1 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord1', i)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		status, ret_coord2 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord2', i)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord3') 
			print ('obj = ' + str(obj))
		status, ret_coord3 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord3', i)
		check_status(status)
		if not status:
			self.coord3 = ret_coord3

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type a_pertstructuremhd_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type a_pertstructuremhd_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'coord1')
		ull.deleteData(self.idx, path, cpopath + 'coord2')
		ull.deleteData(self.idx, path, cpopath + 'coord3')


class b_pertstructuremhd_vector:
	'''
	class b_pertstructuremhd_vector
	Perturbed magnetic field (in Fourier space) [T]

	Attributes:
	- coord1 : numpy.ndarray 2D with  complex numbers
	   Fourier components of first coordinate; Time-dependent; Array 2D (npsi,nm)
	- coord2 : numpy.ndarray 2D with  complex numbers
	   Fourier components of second coordinate; Time-dependent; Array 2D (npsi,nm)
	- coord3 : numpy.ndarray 2D with  complex numbers
	   Fourier components of third coordinate; Time-dependent; Array 2D (npsi,nm)
	'''

	def __init__(self, base_path_in='b_pert'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.coord1 = numpy.zeros((0,0), numpy.complex128, order='C')
		self.coord2 = numpy.zeros((0,0), numpy.complex128, order='C')
		self.coord3 = numpy.zeros((0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class b_pertstructuremhd_vector\n'
		s = self.coord1.__str__()
		ret = ret + space + 'Attribute coord1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord2.__str__()
		ret = ret + space + 'Attribute coord2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord3.__str__()
		ret = ret + space + 'Attribute coord3\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_pertstructuremhd_vector, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord1', numpy.array(self.coord1).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord2', numpy.array(self.coord2).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord3', numpy.array(self.coord3).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_pertstructuremhd_vector, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord1', numpy.array(self.coord1).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord2', numpy.array(self.coord2).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord3', numpy.array(self.coord3).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_pertstructuremhd_vector, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type b_pertstructuremhd_vector, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_coord1, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
			self.cpoTime = retTime
		status, ret_coord2, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2
			self.cpoTime = retTime
		status, ret_coord3, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord3', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord3 = ret_coord3
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type b_pertstructuremhd_vector, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, coord1List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord1')
			if len(coord1List) == 0:
				coord1List = numpy.resize(coord1List, (0,0,nbslice))
			check_status(status)
			status, coord2List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord2')
			if len(coord2List) == 0:
				coord2List = numpy.resize(coord2List, (0,0,nbslice))
			check_status(status)
			status, coord3List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord3')
			if len(coord3List) == 0:
				coord3List = numpy.resize(coord3List, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = b_pertstructuremhd_vector(self.base_path)
				slice.setExpIdx(self.idx)
				slice.coord1 = coord1List[:,:,i].copy().astype(complex)
				slice.coord2 = coord2List[:,:,i].copy().astype(complex)
				slice.coord3 = coord3List[:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_pertstructuremhd_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord1', i, numpy.array(self.coord1).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord2', i, numpy.array(self.coord2).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord3') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord3', i, numpy.array(self.coord3).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_pertstructuremhd_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		status, ret_coord1 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord1', i)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		status, ret_coord2 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord2', i)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord3') 
			print ('obj = ' + str(obj))
		status, ret_coord3 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord3', i)
		check_status(status)
		if not status:
			self.coord3 = ret_coord3

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_pertstructuremhd_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_pertstructuremhd_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'coord1')
		ull.deleteData(self.idx, path, cpopath + 'coord2')
		ull.deleteData(self.idx, path, cpopath + 'coord3')


class v_pertstructuremhd_vector:
	'''
	class v_pertstructuremhd_vector
	Perturbed velocity (in Fourier space) [m/s]

	Attributes:
	- coord1 : numpy.ndarray 2D with  complex numbers
	   Fourier components of first coordinate; Time-dependent; Array 2D (npsi,nm)
	- coord2 : numpy.ndarray 2D with  complex numbers
	   Fourier components of second coordinate; Time-dependent; Array 2D (npsi,nm)
	- coord3 : numpy.ndarray 2D with  complex numbers
	   Fourier components of third coordinate; Time-dependent; Array 2D (npsi,nm)
	'''

	def __init__(self, base_path_in='v_pert'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.coord1 = numpy.zeros((0,0), numpy.complex128, order='C')
		self.coord2 = numpy.zeros((0,0), numpy.complex128, order='C')
		self.coord3 = numpy.zeros((0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class v_pertstructuremhd_vector\n'
		s = self.coord1.__str__()
		ret = ret + space + 'Attribute coord1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord2.__str__()
		ret = ret + space + 'Attribute coord2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord3.__str__()
		ret = ret + space + 'Attribute coord3\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type v_pertstructuremhd_vector, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord1', numpy.array(self.coord1).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord2', numpy.array(self.coord2).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'coord3', numpy.array(self.coord3).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type v_pertstructuremhd_vector, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord1', numpy.array(self.coord1).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord2', numpy.array(self.coord2).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'coord3', numpy.array(self.coord3).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type v_pertstructuremhd_vector, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type v_pertstructuremhd_vector, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_coord1, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
			self.cpoTime = retTime
		status, ret_coord2, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2
			self.cpoTime = retTime
		status, ret_coord3, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'coord3', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord3 = ret_coord3
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type v_pertstructuremhd_vector, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, coord1List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord1')
			if len(coord1List) == 0:
				coord1List = numpy.resize(coord1List, (0,0,nbslice))
			check_status(status)
			status, coord2List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord2')
			if len(coord2List) == 0:
				coord2List = numpy.resize(coord2List, (0,0,nbslice))
			check_status(status)
			status, coord3List = ull.getVect3DComplex(self.idx, path, cpopath + 'coord3')
			if len(coord3List) == 0:
				coord3List = numpy.resize(coord3List, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = v_pertstructuremhd_vector(self.base_path)
				slice.setExpIdx(self.idx)
				slice.coord1 = coord1List[:,:,i].copy().astype(complex)
				slice.coord2 = coord2List[:,:,i].copy().astype(complex)
				slice.coord3 = coord3List[:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type v_pertstructuremhd_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord1', i, numpy.array(self.coord1).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord2', i, numpy.array(self.coord2).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'coord3') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'coord3', i, numpy.array(self.coord3).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type v_pertstructuremhd_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		status, ret_coord1 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord1', i)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		status, ret_coord2 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord2', i)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'coord3') 
			print ('obj = ' + str(obj))
		status, ret_coord3 = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'coord3', i)
		check_status(status)
		if not status:
			self.coord3 = ret_coord3

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type v_pertstructuremhd_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type v_pertstructuremhd_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'coord1')
		ull.deleteData(self.idx, path, cpopath + 'coord2')
		ull.deleteData(self.idx, path, cpopath + 'coord3')


class vacuumstructuremhd_vacuum:
	'''
	class vacuumstructuremhd_vacuum
	External modes

	Attributes:
	- m : numpy.ndarray 3D with float
	   Poloidal mode number; Time-dependent; Array2D (npsi,nm)
	- coord_sys : class coord_sysstructurecoord_sys
	   flux surface coordinate system on a square grid of flux and angle
	- a_pert : class a_pertstructuremhd_vector
	   Pertubed vector potential (in Fourier space) [T.m]
	- b_pert : class b_pertstructuremhd_vector
	   Perturbed magnetic field (in Fourier space) [T]
	'''

	def __init__(self, base_path_in='vacuum'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.m = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.coord_sys = coord_sysstructurecoord_sys('coord_sys')
		self.a_pert = a_pertstructuremhd_vector('a_pert')
		self.b_pert = b_pertstructuremhd_vector('b_pert')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vacuumstructuremhd_vacuum\n'
		s = self.m.__str__()
		ret = ret + space + 'Attribute m\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute coord_sys\n ' + self.coord_sys.__str__(depth+1)
		ret = ret + space + 'Attribute a_pert\n ' + self.a_pert.__str__(depth+1)
		ret = ret + space + 'Attribute b_pert\n ' + self.b_pert.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.coord_sys.setExpIdx(idx)
		self.a_pert.setExpIdx(idx)
		self.b_pert.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vacuumstructuremhd_vacuum, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'm', numpy.array(self.m).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.coord_sys.cpoTime = self.cpoTime
		self.coord_sys.putSlice(path, cpopath)
		self.a_pert.cpoTime = self.cpoTime
		self.a_pert.putSlice(path, cpopath)
		self.b_pert.cpoTime = self.cpoTime
		self.b_pert.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vacuumstructuremhd_vacuum, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'm', numpy.array(self.m).astype(numpy.float64))
		check_status(status)
		self.coord_sys.replaceLastSlice(path, cpopath)
		self.a_pert.replaceLastSlice(path, cpopath)
		self.b_pert.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vacuumstructuremhd_vacuum, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coord_sys.putNonTimed(path, cpopath)
		self.a_pert.putNonTimed(path, cpopath)
		self.b_pert.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vacuumstructuremhd_vacuum, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_m, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.m = ret_m
			self.cpoTime = retTime
		self.coord_sys.getSlice(path, cpopath, inTime, interpolMode)
		self.a_pert.getSlice(path, cpopath, inTime, interpolMode)
		self.b_pert.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vacuumstructuremhd_vacuum, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, mList = ull.getVect4DDouble(self.idx, path, cpopath + 'm')
			if len(mList) == 0:
				mList = numpy.resize(mList, (0,0,0,nbslice))
			check_status(status)
			coord_sysList = self.coord_sys.build_non_resampled_data(path, cpopath, nbslice)
			a_pertList = self.a_pert.build_non_resampled_data(path, cpopath, nbslice)
			b_pertList = self.b_pert.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = vacuumstructuremhd_vacuum(self.base_path)
				slice.setExpIdx(self.idx)
				slice.m = mList[:,:,:,i]
				slice.coord_sys = coord_sysList[i]
				slice.a_pert = a_pertList[i]
				slice.b_pert = b_pertList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vacuumstructuremhd_vacuumObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'm', i, numpy.array(self.m).astype(numpy.float64))
		obj = self.coord_sys.putTimedElt(path, cpopath + 'coord_sys', i, obj)
		obj = self.a_pert.putTimedElt(path, cpopath + 'a_pert', i, obj)
		obj = self.b_pert.putTimedElt(path, cpopath + 'b_pert', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vacuumstructuremhd_vacuumObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		status, ret_m = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'm', i)
		check_status(status)
		if not status:
			self.m = ret_m
		self.coord_sys.getTimedElt(path, cpopath + 'coord_sys', i, obj)
		self.a_pert.getTimedElt(path, cpopath + 'a_pert', i, obj)
		self.b_pert.getTimedElt(path, cpopath + 'b_pert', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vacuumstructuremhd_vacuumObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.coord_sys.putNonTimedElt(path, cpopath + 'coord_sys', i, obj)
		obj = self.a_pert.putNonTimedElt(path, cpopath + 'a_pert', i, obj)
		obj = self.b_pert.putNonTimedElt(path, cpopath + 'b_pert', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vacuumstructuremhd_vacuumObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.coord_sys.getNonTimedElt(path, cpopath + 'coord_sys', i, obj)
		self.a_pert.getNonTimedElt(path, cpopath + 'a_pert', i, obj)
		self.b_pert.getNonTimedElt(path, cpopath + 'b_pert', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'm')
		self.coord_sys.deleteData(path, cpopath)
		self.a_pert.deleteData(path, cpopath)
		self.b_pert.deleteData(path, cpopath)


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
