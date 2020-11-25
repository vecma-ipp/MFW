# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class turbulence(KeepInOrder):
	'''
	class turbulence
	Turbulence; Time-dependent CPO.

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- composition : class compositionstructureturbcomposition
	   Plasma composition (description of ion species).
	- coordsys : class coordsysstructureturbcoordsys
	   Decription of the coordinates and metric used by the codes.
	- var0d : class var0dstructureturbvar0d
	   Diagnostic fast time traces.
	- var1d : class var1dstructureturbvar1d
	   Dependent variable radial profile.
	- var2d : class var2dstructureturbvar2d
	   Dependent variable axisymmetric.
	- var3d : class var3dstructureturbvar3d
	   Dependent variable morphology. Grid is defined in coord_sys/turbgrid.
	- var4d : class var4dstructureturbvar4d
	   Gyrokinetic distribution function, axisymmetric component. Grid is defined in coord_sys/turbgrid.
	- var5d : class var5dstructureturbvar5d
	   Gyrokinetic distribution function. Grid is defined in coord_sys/turbgrid.
	- spec1d : class spec1dstructureturbspec1d
	   Toroidal mode number spectra.
	- env1d : class env1dstructureturbenv1d
	   Parallel fluctuation envelope.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar.
	'''

	def __init__(self):
		self.base_path = 'turbulence'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 1
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.composition = compositionstructureturbcomposition('composition')
		self.coordsys = coordsysstructureturbcoordsys('coordsys')
		self.var0d = var0dstructureturbvar0d('var0d')
		self.var1d = var1dstructureturbvar1d('var1d')
		self.var2d = var2dstructureturbvar2d('var2d')
		self.var3d = var3dstructureturbvar3d('var3d')
		self.var4d = var4dstructureturbvar4d('var4d')
		self.var5d = var5dstructureturbvar5d('var5d')
		self.spec1d = spec1dstructureturbspec1d('spec1d')
		self.env1d = env1dstructureturbenv1d('env1d')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class turbulence\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute composition\n ' + self.composition.__str__(depth+1)
		ret = ret + space + 'Attribute coordsys\n ' + self.coordsys.__str__(depth+1)
		ret = ret + space + 'Attribute var0d\n ' + self.var0d.__str__(depth+1)
		ret = ret + space + 'Attribute var1d\n ' + self.var1d.__str__(depth+1)
		ret = ret + space + 'Attribute var2d\n ' + self.var2d.__str__(depth+1)
		ret = ret + space + 'Attribute var3d\n ' + self.var3d.__str__(depth+1)
		ret = ret + space + 'Attribute var4d\n ' + self.var4d.__str__(depth+1)
		ret = ret + space + 'Attribute var5d\n ' + self.var5d.__str__(depth+1)
		ret = ret + space + 'Attribute spec1d\n ' + self.spec1d.__str__(depth+1)
		ret = ret + space + 'Attribute env1d\n ' + self.env1d.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.composition.setExpIdx(idx)
		self.coordsys.setExpIdx(idx)
		self.var0d.setExpIdx(idx)
		self.var1d.setExpIdx(idx)
		self.var2d.setExpIdx(idx)
		self.var3d.setExpIdx(idx)
		self.var4d.setExpIdx(idx)
		self.var5d.setExpIdx(idx)
		self.spec1d.setExpIdx(idx)
		self.env1d.setExpIdx(idx)
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
		self.composition.cpoTime = self.cpoTime
		self.composition.putSlice(path, cpopath)
		self.coordsys.cpoTime = self.cpoTime
		self.coordsys.putSlice(path, cpopath)
		self.var0d.cpoTime = self.cpoTime
		self.var0d.putSlice(path, cpopath)
		self.var1d.cpoTime = self.cpoTime
		self.var1d.putSlice(path, cpopath)
		self.var2d.cpoTime = self.cpoTime
		self.var2d.putSlice(path, cpopath)
		self.var3d.cpoTime = self.cpoTime
		self.var3d.putSlice(path, cpopath)
		self.var4d.cpoTime = self.cpoTime
		self.var4d.putSlice(path, cpopath)
		self.var5d.cpoTime = self.cpoTime
		self.var5d.putSlice(path, cpopath)
		self.spec1d.cpoTime = self.cpoTime
		self.spec1d.putSlice(path, cpopath)
		self.env1d.cpoTime = self.cpoTime
		self.env1d.putSlice(path, cpopath)
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
		self.composition.replaceLastSlice(path, cpopath)
		self.coordsys.replaceLastSlice(path, cpopath)
		self.var0d.replaceLastSlice(path, cpopath)
		self.var1d.replaceLastSlice(path, cpopath)
		self.var2d.replaceLastSlice(path, cpopath)
		self.var3d.replaceLastSlice(path, cpopath)
		self.var4d.replaceLastSlice(path, cpopath)
		self.var5d.replaceLastSlice(path, cpopath)
		self.spec1d.replaceLastSlice(path, cpopath)
		self.env1d.replaceLastSlice(path, cpopath)
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
		self.composition.putNonTimed(path, cpopath)
		self.coordsys.putNonTimed(path, cpopath)
		self.var0d.putNonTimed(path, cpopath)
		self.var1d.putNonTimed(path, cpopath)
		self.var2d.putNonTimed(path, cpopath)
		self.var3d.putNonTimed(path, cpopath)
		self.var4d.putNonTimed(path, cpopath)
		self.var5d.putNonTimed(path, cpopath)
		self.spec1d.putNonTimed(path, cpopath)
		self.env1d.putNonTimed(path, cpopath)
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
		self.composition.getSlice(path, cpopath, inTime, interpolMode)
		self.coordsys.getSlice(path, cpopath, inTime, interpolMode)
		self.var0d.getSlice(path, cpopath, inTime, interpolMode)
		self.var1d.getSlice(path, cpopath, inTime, interpolMode)
		self.var2d.getSlice(path, cpopath, inTime, interpolMode)
		self.var3d.getSlice(path, cpopath, inTime, interpolMode)
		self.var4d.getSlice(path, cpopath, inTime, interpolMode)
		self.var5d.getSlice(path, cpopath, inTime, interpolMode)
		self.spec1d.getSlice(path, cpopath, inTime, interpolMode)
		self.env1d.getSlice(path, cpopath, inTime, interpolMode)
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
			compositionList = self.composition.build_non_resampled_data(path, cpopath, nbslice)
			coordsysList = self.coordsys.build_non_resampled_data(path, cpopath, nbslice)
			var0dList = self.var0d.build_non_resampled_data(path, cpopath, nbslice)
			var1dList = self.var1d.build_non_resampled_data(path, cpopath, nbslice)
			var2dList = self.var2d.build_non_resampled_data(path, cpopath, nbslice)
			var3dList = self.var3d.build_non_resampled_data(path, cpopath, nbslice)
			var4dList = self.var4d.build_non_resampled_data(path, cpopath, nbslice)
			var5dList = self.var5d.build_non_resampled_data(path, cpopath, nbslice)
			spec1dList = self.spec1d.build_non_resampled_data(path, cpopath, nbslice)
			env1dList = self.env1d.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = turbulence()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.composition = compositionList[i]
				slice.coordsys = coordsysList[i]
				slice.var0d = var0dList[i]
				slice.var1d = var1dList[i]
				slice.var2d = var2dList[i]
				slice.var3d = var3dList[i]
				slice.var4d = var4dList[i]
				slice.var5d = var5dList[i]
				slice.spec1d = spec1dList[i]
				slice.env1d = env1dList[i]
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
		self.composition.deleteData(path, cpopath)
		self.coordsys.deleteData(path, cpopath)
		self.var0d.deleteData(path, cpopath)
		self.var1d.deleteData(path, cpopath)
		self.var2d.deleteData(path, cpopath)
		self.var3d.deleteData(path, cpopath)
		self.var4d.deleteData(path, cpopath)
		self.var5d.deleteData(path, cpopath)
		self.spec1d.deleteData(path, cpopath)
		self.env1d.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class turbulenceArray:
	'''
	class turbulenceArray
	Turbulence; Time-dependent CPO.

	Attributes:
	- array : list of turbulence
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
		ret = space + 'class turbulenceArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'turbulence cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = turbulence()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(turbulence())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = turbulence()
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


class compositionstructureturbcomposition(KeepInOrder):
	'''
	class compositionstructureturbcomposition
	Plasma composition (description of ion species).

	Attributes:
	- amn : numpy.ndarray 1D with float
	   Atomic mass number (lumped ions are allowed); Vector (nion)
	- zn : numpy.ndarray 1D with float
	   Nuclear charge (lumped ions are allowed); Vector (nion)
	- zion : numpy.ndarray 1D with float
	   Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
	- ie_mass : numpy.ndarray 1D with float
	   Ion to electron mass ratio as used in the code for each species. To be used only by models which keep electron inertia. Vector (nion)
	'''

	def __init__(self, base_path_in='composition'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.amn = numpy.zeros(0, numpy.float64, order='C')
		self.zn = numpy.zeros(0, numpy.float64, order='C')
		self.zion = numpy.zeros(0, numpy.float64, order='C')
		self.ie_mass = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compositionstructureturbcomposition\n'
		s = self.amn.__str__()
		ret = ret + space + 'Attribute amn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zn.__str__()
		ret = ret + space + 'Attribute zn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zion.__str__()
		ret = ret + space + 'Attribute zion\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ie_mass.__str__()
		ret = ret + space + 'Attribute ie_mass\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructureturbcomposition, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructureturbcomposition, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructureturbcomposition, run function putNonTimed') 
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
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'ie_mass', numpy.array(self.ie_mass).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructureturbcomposition, run function getSlice') 
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
		status, ret_ie_mass = ull.getVect1DDouble(self.idx, path, cpopath + 'ie_mass')
		check_status(status)
		if not status:
			self.ie_mass = ret_ie_mass

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructureturbcomposition, run function build_non_resampled_data') 
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
			status, ie_massVal = ull.getVect1DDouble(self.idx, path, cpopath + 'ie_mass')
			check_status(status)
			for i in range(nbslice):
				slice = compositionstructureturbcomposition(self.base_path)
				slice.setExpIdx(self.idx)
				slice.amn = amnVal
				slice.zn = znVal
				slice.zion = zionVal
				slice.ie_mass = ie_massVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructureturbcompositionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructureturbcompositionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructureturbcompositionObj, run function putNonTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 'ie_mass') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ie_mass', i, numpy.array(self.ie_mass).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructureturbcompositionObj, run function getNonTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 'ie_mass') 
			print ('obj = ' + str(obj))
		status, ret_ie_mass = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ie_mass', i)
		check_status(status)
		if not status:
			self.ie_mass = ret_ie_mass

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
		ull.deleteData(self.idx, path, cpopath + 'ie_mass')


class coordsysstructureturbcoordsys(KeepInOrder):
	'''
	class coordsysstructureturbcoordsys
	Decription of the coordinates and metric used by the codes.

	Attributes:
	- grid_type : str
	   Type of coordinate system.
	- turbgrid : class turbgridstructureturbgrid
	   Turbulence grid used by the codes; Time-dependent.
	- jacobian : numpy.ndarray 2D with float
	   Jacobian of the coordinate system; Time-dependent; Matrix (ndim1, ndim2).
	- g_11 : numpy.ndarray 2D with float
	   metric coefficients g_11; Time-dependent; Matrix (ndim1, ndim2).
	- g_12 : numpy.ndarray 2D with float
	   metric coefficients g_12; Time-dependent; Matrix (ndim1, ndim2).
	- g_13 : numpy.ndarray 2D with float
	   metric coefficients g_13; Time-dependent; Matrix (ndim1, ndim2).
	- g_22 : numpy.ndarray 2D with float
	   metric coefficients g_22; Time-dependent; Matrix (ndim1, ndim2).
	- g_23 : numpy.ndarray 2D with float
	   metric coefficients g_23; Time-dependent; Matrix (ndim1, ndim2).
	- g_33 : numpy.ndarray 2D with float
	   metric coefficients g_33; Time-dependent; Matrix (ndim1, ndim2).
	- position : class positionstructurerzphi3D
	   R Z phi positions of grid points; Time-dependent; Array3D (ndim1, ndim2, ndim3).
	'''

	def __init__(self, base_path_in='coordsys'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_type = ''
		self.turbgrid = turbgridstructureturbgrid('turbgrid')
		self.jacobian = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_11 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_12 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_13 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_22 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_23 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_33 = numpy.zeros((0,0), numpy.float64, order='C')
		self.position = positionstructurerzphi3D('position')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coordsysstructureturbcoordsys\n'
		ret = ret + space + 'Attribute grid_type: ' + str(self.grid_type) + '\n'
		ret = ret + space + 'Attribute turbgrid\n ' + self.turbgrid.__str__(depth+1)
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
		self.turbgrid.setExpIdx(idx)
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordsysstructureturbcoordsys, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.turbgrid.cpoTime = self.cpoTime
		self.turbgrid.putSlice(path, cpopath)
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
			print ('field '+self.base_path+' of type coordsysstructureturbcoordsys, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.turbgrid.replaceLastSlice(path, cpopath)
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
			print ('field '+self.base_path+' of type coordsysstructureturbcoordsys, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'grid_type', self.grid_type)
		check_status(status)
		self.turbgrid.putNonTimed(path, cpopath)
		self.position.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coordsysstructureturbcoordsys, run function getSlice') 
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
		self.turbgrid.getSlice(path, cpopath, inTime, interpolMode)
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
			print ('field '+self.base_path+' of type coordsysstructureturbcoordsys, run function build_non_resampled_data') 
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
			turbgridList = self.turbgrid.build_non_resampled_data(path, cpopath, nbslice)
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
				slice = coordsysstructureturbcoordsys(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid_type = grid_typeVal
				slice.turbgrid = turbgridList[i]
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
			print ('object of type coordsysstructureturbcoordsysObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
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
			print ('object of type coordsysstructureturbcoordsysObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
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
			print ('object of type coordsysstructureturbcoordsysObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'grid_type', i, self.grid_type)
		obj = self.turbgrid.putNonTimedElt(path, cpopath + 'turbgrid', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordsysstructureturbcoordsysObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		status, ret_grid_type = ull.getStringFromObject(self.idx, obj, cpopath + 'grid_type', i)
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.turbgrid.getNonTimedElt(path, cpopath + 'turbgrid', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'grid_type')
		self.turbgrid.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'jacobian')
		ull.deleteData(self.idx, path, cpopath + 'g_11')
		ull.deleteData(self.idx, path, cpopath + 'g_12')
		ull.deleteData(self.idx, path, cpopath + 'g_13')
		ull.deleteData(self.idx, path, cpopath + 'g_22')
		ull.deleteData(self.idx, path, cpopath + 'g_23')
		ull.deleteData(self.idx, path, cpopath + 'g_33')
		self.position.deleteData(path, cpopath)


class turbgridstructureturbgrid(KeepInOrder):
	'''
	class turbgridstructureturbgrid
	Turbulence grid used by the codes; Time-dependent.

	Attributes:
	- dim1 : numpy.ndarray 1D with float
	   First dimension values; Vector (ndim1).
	- dim2 : numpy.ndarray 1D with float
	   Second dimension values; Vector (ndim2).
	- dim3 : numpy.ndarray 1D with float
	   Third dimension values; Vector (ndim3).
	- dim_v1 : numpy.ndarray 1D with float
	   First v-space dimension values; Vector (ndim_v1).
	- dim_v2 : numpy.ndarray 1D with float
	   Second v-space dimension values; Vector (ndim_v2).
	'''

	def __init__(self, base_path_in='turbgrid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dim1 = numpy.zeros(0, numpy.float64, order='C')
		self.dim2 = numpy.zeros(0, numpy.float64, order='C')
		self.dim3 = numpy.zeros(0, numpy.float64, order='C')
		self.dim_v1 = numpy.zeros(0, numpy.float64, order='C')
		self.dim_v2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class turbgridstructureturbgrid\n'
		s = self.dim1.__str__()
		ret = ret + space + 'Attribute dim1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim2.__str__()
		ret = ret + space + 'Attribute dim2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim3.__str__()
		ret = ret + space + 'Attribute dim3\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim_v1.__str__()
		ret = ret + space + 'Attribute dim_v1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim_v2.__str__()
		ret = ret + space + 'Attribute dim_v2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type turbgridstructureturbgrid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type turbgridstructureturbgrid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type turbgridstructureturbgrid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dim3', numpy.array(self.dim3).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dim_v1', numpy.array(self.dim_v1).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dim_v2', numpy.array(self.dim_v2).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type turbgridstructureturbgrid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dim1 = ull.getVect1DDouble(self.idx, path, cpopath + 'dim1')
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
		status, ret_dim2 = ull.getVect1DDouble(self.idx, path, cpopath + 'dim2')
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
		status, ret_dim3 = ull.getVect1DDouble(self.idx, path, cpopath + 'dim3')
		check_status(status)
		if not status:
			self.dim3 = ret_dim3
		status, ret_dim_v1 = ull.getVect1DDouble(self.idx, path, cpopath + 'dim_v1')
		check_status(status)
		if not status:
			self.dim_v1 = ret_dim_v1
		status, ret_dim_v2 = ull.getVect1DDouble(self.idx, path, cpopath + 'dim_v2')
		check_status(status)
		if not status:
			self.dim_v2 = ret_dim_v2

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type turbgridstructureturbgrid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dim1Val = ull.getVect1DDouble(self.idx, path, cpopath + 'dim1')
			check_status(status)
			status, dim2Val = ull.getVect1DDouble(self.idx, path, cpopath + 'dim2')
			check_status(status)
			status, dim3Val = ull.getVect1DDouble(self.idx, path, cpopath + 'dim3')
			check_status(status)
			status, dim_v1Val = ull.getVect1DDouble(self.idx, path, cpopath + 'dim_v1')
			check_status(status)
			status, dim_v2Val = ull.getVect1DDouble(self.idx, path, cpopath + 'dim_v2')
			check_status(status)
			for i in range(nbslice):
				slice = turbgridstructureturbgrid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dim1 = dim1Val
				slice.dim2 = dim2Val
				slice.dim3 = dim3Val
				slice.dim_v1 = dim_v1Val
				slice.dim_v2 = dim_v2Val
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turbgridstructureturbgridObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turbgridstructureturbgridObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turbgridstructureturbgridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim1', i, numpy.array(self.dim1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim2', i, numpy.array(self.dim2).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim3') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim3', i, numpy.array(self.dim3).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim_v1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim_v1', i, numpy.array(self.dim_v1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim_v2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim_v2', i, numpy.array(self.dim_v2).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turbgridstructureturbgridObj, run function getNonTimedElt') 
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
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim3') 
			print ('obj = ' + str(obj))
		status, ret_dim3 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim3', i)
		check_status(status)
		if not status:
			self.dim3 = ret_dim3
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim_v1') 
			print ('obj = ' + str(obj))
		status, ret_dim_v1 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim_v1', i)
		check_status(status)
		if not status:
			self.dim_v1 = ret_dim_v1
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim_v2') 
			print ('obj = ' + str(obj))
		status, ret_dim_v2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim_v2', i)
		check_status(status)
		if not status:
			self.dim_v2 = ret_dim_v2

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dim1')
		ull.deleteData(self.idx, path, cpopath + 'dim2')
		ull.deleteData(self.idx, path, cpopath + 'dim3')
		ull.deleteData(self.idx, path, cpopath + 'dim_v1')
		ull.deleteData(self.idx, path, cpopath + 'dim_v2')


class positionstructurerzphi3D(KeepInOrder):
	'''
	class positionstructurerzphi3D
	R Z phi positions of grid points; Time-dependent; Array3D (ndim1, ndim2, ndim3).

	Attributes:
	- r : numpy.ndarray 3D with float
	   Major radius [m]
	- z : numpy.ndarray 3D with float
	   Altitude [m]
	- phi : numpy.ndarray 3D with float
	   Toroidal angle [rad]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.phi = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerzphi3D\n'
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
			print ('field '+self.base_path+' of type positionstructurerzphi3D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi3D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi3D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi3D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime
		status, ret_phi, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi3D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect4DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,0,0,nbslice))
			check_status(status)
			status, zList = ull.getVect4DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,0,0,nbslice))
			check_status(status)
			status, phiList = ull.getVect4DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerzphi3D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,:,:,i]
				slice.z = zList[:,:,:,i]
				slice.phi = phiList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi3DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi3DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi3DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi3DObj, run function getNonTimedElt') 
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


class var0dstructureturbvar0d(KeepInOrder):
	'''
	class var0dstructureturbvar0d
	Diagnostic fast time traces.

	Attributes:
	- dtime_type : str
	   Description of time trace e.g. last ndtime points.
	- dtime : numpy.ndarray 1D with float
	   Fast diagnostic time [s]; Time-dependent; Vector (ndtime).
	- en_exb : numpy.ndarray 1D with float
	   ExB energy [J/m^3]; Time-dependent; Vector (ndtime).
	- en_mag : numpy.ndarray 1D with float
	   Magnetic energy [J/m^3]; Time-dependent; Vector (ndtime).
	- en_el_th : numpy.ndarray 1D with float
	   electron thermal energy or free energy [J/m^3]; Time-dependent.
	- en_ion_th : numpy.ndarray 2D with float
	   Ion thermal energy or free energy [J/m^3]; Time-dependent; Matrix (ndtime, nion).
	- en_el_par : numpy.ndarray 1D with float
	   Electron parallel energy [J/m^3]; Time-dependent; Vector (ndtime).
	- en_ion_par : numpy.ndarray 2D with float
	   Ion parallel energy [J/m^3]; Time-dependent; Matrix (ndtime,nion).
	- en_tot : numpy.ndarray 1D with float
	   Total energy or free energy [J/m^3]; Time-dependent; Vector (ndtime).
	- fl_el : numpy.ndarray 1D with float
	   Electron flux [m^-2 s^-1]; Time-dependent; Vector (ndtime).
	- fl_heatel : numpy.ndarray 1D with float
	   Conductive electron heat flux [W.m^-2]; Time-dependent; Vector (ndtime).
	- fl_ion : numpy.ndarray 2D with float
	   Ion flux [m^-2 s^-1]; Time-dependent; Matrix (ndtime, nion).
	- fl_heation : numpy.ndarray 2D with float
	   Conductive ion heat flux [W.m^-2]; Time-dependent; Matrix (ndtime, nion).
	- fl_magel : numpy.ndarray 1D with float
	   Electron flux [m^-2 s^-1]; Time-dependent; Vector (ndtime).
	- fl_magheatel : numpy.ndarray 1D with float
	   Conductive electron heat flux [W.m^-2]; Time-dependent; Vector (ndtime).
	- fl_magion : numpy.ndarray 2D with float
	   Ion flux [m^-2 s^-1]; Time-dependent; Matrix (ndtime, nion).
	- flmagheation : numpy.ndarray 2D with float
	   Conductive ion heat flux [W.m^-2]; Time-dependent; Matrix (ndtime, nion).
	'''

	def __init__(self, base_path_in='var0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dtime_type = ''
		self.dtime = numpy.zeros(0, numpy.float64, order='C')
		self.en_exb = numpy.zeros(0, numpy.float64, order='C')
		self.en_mag = numpy.zeros(0, numpy.float64, order='C')
		self.en_el_th = numpy.zeros(0, numpy.float64, order='C')
		self.en_ion_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.en_el_par = numpy.zeros(0, numpy.float64, order='C')
		self.en_ion_par = numpy.zeros((0,0), numpy.float64, order='C')
		self.en_tot = numpy.zeros(0, numpy.float64, order='C')
		self.fl_el = numpy.zeros(0, numpy.float64, order='C')
		self.fl_heatel = numpy.zeros(0, numpy.float64, order='C')
		self.fl_ion = numpy.zeros((0,0), numpy.float64, order='C')
		self.fl_heation = numpy.zeros((0,0), numpy.float64, order='C')
		self.fl_magel = numpy.zeros(0, numpy.float64, order='C')
		self.fl_magheatel = numpy.zeros(0, numpy.float64, order='C')
		self.fl_magion = numpy.zeros((0,0), numpy.float64, order='C')
		self.flmagheation = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class var0dstructureturbvar0d\n'
		ret = ret + space + 'Attribute dtime_type: ' + str(self.dtime_type) + '\n'
		s = self.dtime.__str__()
		ret = ret + space + 'Attribute dtime\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_exb.__str__()
		ret = ret + space + 'Attribute en_exb\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_mag.__str__()
		ret = ret + space + 'Attribute en_mag\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_el_th.__str__()
		ret = ret + space + 'Attribute en_el_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_ion_th.__str__()
		ret = ret + space + 'Attribute en_ion_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_el_par.__str__()
		ret = ret + space + 'Attribute en_el_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_ion_par.__str__()
		ret = ret + space + 'Attribute en_ion_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.en_tot.__str__()
		ret = ret + space + 'Attribute en_tot\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_el.__str__()
		ret = ret + space + 'Attribute fl_el\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_heatel.__str__()
		ret = ret + space + 'Attribute fl_heatel\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_ion.__str__()
		ret = ret + space + 'Attribute fl_ion\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_heation.__str__()
		ret = ret + space + 'Attribute fl_heation\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_magel.__str__()
		ret = ret + space + 'Attribute fl_magel\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_magheatel.__str__()
		ret = ret + space + 'Attribute fl_magheatel\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fl_magion.__str__()
		ret = ret + space + 'Attribute fl_magion\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flmagheation.__str__()
		ret = ret + space + 'Attribute flmagheation\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var0dstructureturbvar0d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dtime', numpy.array(self.dtime).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'en_exb', numpy.array(self.en_exb).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'en_mag', numpy.array(self.en_mag).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'en_el_th', numpy.array(self.en_el_th).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'en_ion_th', numpy.array(self.en_ion_th).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'en_el_par', numpy.array(self.en_el_par).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'en_ion_par', numpy.array(self.en_ion_par).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'en_tot', numpy.array(self.en_tot).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'fl_el', numpy.array(self.fl_el).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'fl_heatel', numpy.array(self.fl_heatel).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'fl_ion', numpy.array(self.fl_ion).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'fl_heation', numpy.array(self.fl_heation).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'fl_magel', numpy.array(self.fl_magel).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'fl_magheatel', numpy.array(self.fl_magheatel).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'fl_magion', numpy.array(self.fl_magion).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'flmagheation', numpy.array(self.flmagheation).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var0dstructureturbvar0d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dtime', numpy.array(self.dtime).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'en_exb', numpy.array(self.en_exb).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'en_mag', numpy.array(self.en_mag).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'en_el_th', numpy.array(self.en_el_th).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'en_ion_th', numpy.array(self.en_ion_th).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'en_el_par', numpy.array(self.en_el_par).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'en_ion_par', numpy.array(self.en_ion_par).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'en_tot', numpy.array(self.en_tot).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'fl_el', numpy.array(self.fl_el).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'fl_heatel', numpy.array(self.fl_heatel).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'fl_ion', numpy.array(self.fl_ion).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'fl_heation', numpy.array(self.fl_heation).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'fl_magel', numpy.array(self.fl_magel).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'fl_magheatel', numpy.array(self.fl_magheatel).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'fl_magion', numpy.array(self.fl_magion).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'flmagheation', numpy.array(self.flmagheation).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var0dstructureturbvar0d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'dtime_type', self.dtime_type)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type var0dstructureturbvar0d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dtime_type = ull.getString(self.idx, path, cpopath + 'dtime_type')
		check_status(status)
		if not status:
			self.dtime_type = ret_dtime_type
		status, ret_dtime, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dtime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dtime = ret_dtime
			self.cpoTime = retTime
		status, ret_en_exb, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'en_exb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_exb = ret_en_exb
			self.cpoTime = retTime
		status, ret_en_mag, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'en_mag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_mag = ret_en_mag
			self.cpoTime = retTime
		status, ret_en_el_th, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'en_el_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_el_th = ret_en_el_th
			self.cpoTime = retTime
		status, ret_en_ion_th, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'en_ion_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_ion_th = ret_en_ion_th
			self.cpoTime = retTime
		status, ret_en_el_par, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'en_el_par', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_el_par = ret_en_el_par
			self.cpoTime = retTime
		status, ret_en_ion_par, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'en_ion_par', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_ion_par = ret_en_ion_par
			self.cpoTime = retTime
		status, ret_en_tot, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'en_tot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.en_tot = ret_en_tot
			self.cpoTime = retTime
		status, ret_fl_el, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'fl_el', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_el = ret_fl_el
			self.cpoTime = retTime
		status, ret_fl_heatel, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'fl_heatel', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_heatel = ret_fl_heatel
			self.cpoTime = retTime
		status, ret_fl_ion, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'fl_ion', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_ion = ret_fl_ion
			self.cpoTime = retTime
		status, ret_fl_heation, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'fl_heation', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_heation = ret_fl_heation
			self.cpoTime = retTime
		status, ret_fl_magel, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'fl_magel', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_magel = ret_fl_magel
			self.cpoTime = retTime
		status, ret_fl_magheatel, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'fl_magheatel', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_magheatel = ret_fl_magheatel
			self.cpoTime = retTime
		status, ret_fl_magion, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'fl_magion', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fl_magion = ret_fl_magion
			self.cpoTime = retTime
		status, ret_flmagheation, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'flmagheation', inTime, interpolMode)
		check_status(status)
		if not status:
			self.flmagheation = ret_flmagheation
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type var0dstructureturbvar0d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dtime_typeVal = ull.getString(self.idx, path, cpopath + 'dtime_type')
			check_status(status)
			status, dtimeList = ull.getVect2DDouble(self.idx, path, cpopath + 'dtime')
			if len(dtimeList) == 0:
				dtimeList = numpy.resize(dtimeList, (0,nbslice))
			check_status(status)
			status, en_exbList = ull.getVect2DDouble(self.idx, path, cpopath + 'en_exb')
			if len(en_exbList) == 0:
				en_exbList = numpy.resize(en_exbList, (0,nbslice))
			check_status(status)
			status, en_magList = ull.getVect2DDouble(self.idx, path, cpopath + 'en_mag')
			if len(en_magList) == 0:
				en_magList = numpy.resize(en_magList, (0,nbslice))
			check_status(status)
			status, en_el_thList = ull.getVect2DDouble(self.idx, path, cpopath + 'en_el_th')
			if len(en_el_thList) == 0:
				en_el_thList = numpy.resize(en_el_thList, (0,nbslice))
			check_status(status)
			status, en_ion_thList = ull.getVect3DDouble(self.idx, path, cpopath + 'en_ion_th')
			if len(en_ion_thList) == 0:
				en_ion_thList = numpy.resize(en_ion_thList, (0,0,nbslice))
			check_status(status)
			status, en_el_parList = ull.getVect2DDouble(self.idx, path, cpopath + 'en_el_par')
			if len(en_el_parList) == 0:
				en_el_parList = numpy.resize(en_el_parList, (0,nbslice))
			check_status(status)
			status, en_ion_parList = ull.getVect3DDouble(self.idx, path, cpopath + 'en_ion_par')
			if len(en_ion_parList) == 0:
				en_ion_parList = numpy.resize(en_ion_parList, (0,0,nbslice))
			check_status(status)
			status, en_totList = ull.getVect2DDouble(self.idx, path, cpopath + 'en_tot')
			if len(en_totList) == 0:
				en_totList = numpy.resize(en_totList, (0,nbslice))
			check_status(status)
			status, fl_elList = ull.getVect2DDouble(self.idx, path, cpopath + 'fl_el')
			if len(fl_elList) == 0:
				fl_elList = numpy.resize(fl_elList, (0,nbslice))
			check_status(status)
			status, fl_heatelList = ull.getVect2DDouble(self.idx, path, cpopath + 'fl_heatel')
			if len(fl_heatelList) == 0:
				fl_heatelList = numpy.resize(fl_heatelList, (0,nbslice))
			check_status(status)
			status, fl_ionList = ull.getVect3DDouble(self.idx, path, cpopath + 'fl_ion')
			if len(fl_ionList) == 0:
				fl_ionList = numpy.resize(fl_ionList, (0,0,nbslice))
			check_status(status)
			status, fl_heationList = ull.getVect3DDouble(self.idx, path, cpopath + 'fl_heation')
			if len(fl_heationList) == 0:
				fl_heationList = numpy.resize(fl_heationList, (0,0,nbslice))
			check_status(status)
			status, fl_magelList = ull.getVect2DDouble(self.idx, path, cpopath + 'fl_magel')
			if len(fl_magelList) == 0:
				fl_magelList = numpy.resize(fl_magelList, (0,nbslice))
			check_status(status)
			status, fl_magheatelList = ull.getVect2DDouble(self.idx, path, cpopath + 'fl_magheatel')
			if len(fl_magheatelList) == 0:
				fl_magheatelList = numpy.resize(fl_magheatelList, (0,nbslice))
			check_status(status)
			status, fl_magionList = ull.getVect3DDouble(self.idx, path, cpopath + 'fl_magion')
			if len(fl_magionList) == 0:
				fl_magionList = numpy.resize(fl_magionList, (0,0,nbslice))
			check_status(status)
			status, flmagheationList = ull.getVect3DDouble(self.idx, path, cpopath + 'flmagheation')
			if len(flmagheationList) == 0:
				flmagheationList = numpy.resize(flmagheationList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = var0dstructureturbvar0d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dtime_type = dtime_typeVal
				slice.dtime = dtimeList[:,i]
				slice.en_exb = en_exbList[:,i]
				slice.en_mag = en_magList[:,i]
				slice.en_el_th = en_el_thList[:,i]
				slice.en_ion_th = en_ion_thList[:,:,i]
				slice.en_el_par = en_el_parList[:,i]
				slice.en_ion_par = en_ion_parList[:,:,i]
				slice.en_tot = en_totList[:,i]
				slice.fl_el = fl_elList[:,i]
				slice.fl_heatel = fl_heatelList[:,i]
				slice.fl_ion = fl_ionList[:,:,i]
				slice.fl_heation = fl_heationList[:,:,i]
				slice.fl_magel = fl_magelList[:,i]
				slice.fl_magheatel = fl_magheatelList[:,i]
				slice.fl_magion = fl_magionList[:,:,i]
				slice.flmagheation = flmagheationList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var0dstructureturbvar0dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dtime') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dtime', i, numpy.array(self.dtime).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'en_exb') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'en_exb', i, numpy.array(self.en_exb).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'en_mag') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'en_mag', i, numpy.array(self.en_mag).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'en_el_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'en_el_th', i, numpy.array(self.en_el_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'en_ion_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'en_ion_th', i, numpy.array(self.en_ion_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'en_el_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'en_el_par', i, numpy.array(self.en_el_par).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'en_ion_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'en_ion_par', i, numpy.array(self.en_ion_par).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'en_tot') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'en_tot', i, numpy.array(self.en_tot).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'fl_el') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'fl_el', i, numpy.array(self.fl_el).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'fl_heatel') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'fl_heatel', i, numpy.array(self.fl_heatel).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'fl_ion') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'fl_ion', i, numpy.array(self.fl_ion).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'fl_heation') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'fl_heation', i, numpy.array(self.fl_heation).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'fl_magel') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'fl_magel', i, numpy.array(self.fl_magel).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'fl_magheatel') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'fl_magheatel', i, numpy.array(self.fl_magheatel).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'fl_magion') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'fl_magion', i, numpy.array(self.fl_magion).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flmagheation') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flmagheation', i, numpy.array(self.flmagheation).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var0dstructureturbvar0dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dtime') 
			print ('obj = ' + str(obj))
		status, ret_dtime = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dtime', i)
		check_status(status)
		if not status:
			self.dtime = ret_dtime
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'en_exb') 
			print ('obj = ' + str(obj))
		status, ret_en_exb = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'en_exb', i)
		check_status(status)
		if not status:
			self.en_exb = ret_en_exb
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'en_mag') 
			print ('obj = ' + str(obj))
		status, ret_en_mag = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'en_mag', i)
		check_status(status)
		if not status:
			self.en_mag = ret_en_mag
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'en_el_th') 
			print ('obj = ' + str(obj))
		status, ret_en_el_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'en_el_th', i)
		check_status(status)
		if not status:
			self.en_el_th = ret_en_el_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'en_ion_th') 
			print ('obj = ' + str(obj))
		status, ret_en_ion_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'en_ion_th', i)
		check_status(status)
		if not status:
			self.en_ion_th = ret_en_ion_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'en_el_par') 
			print ('obj = ' + str(obj))
		status, ret_en_el_par = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'en_el_par', i)
		check_status(status)
		if not status:
			self.en_el_par = ret_en_el_par
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'en_ion_par') 
			print ('obj = ' + str(obj))
		status, ret_en_ion_par = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'en_ion_par', i)
		check_status(status)
		if not status:
			self.en_ion_par = ret_en_ion_par
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'en_tot') 
			print ('obj = ' + str(obj))
		status, ret_en_tot = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'en_tot', i)
		check_status(status)
		if not status:
			self.en_tot = ret_en_tot
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'fl_el') 
			print ('obj = ' + str(obj))
		status, ret_fl_el = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'fl_el', i)
		check_status(status)
		if not status:
			self.fl_el = ret_fl_el
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'fl_heatel') 
			print ('obj = ' + str(obj))
		status, ret_fl_heatel = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'fl_heatel', i)
		check_status(status)
		if not status:
			self.fl_heatel = ret_fl_heatel
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'fl_ion') 
			print ('obj = ' + str(obj))
		status, ret_fl_ion = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'fl_ion', i)
		check_status(status)
		if not status:
			self.fl_ion = ret_fl_ion
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'fl_heation') 
			print ('obj = ' + str(obj))
		status, ret_fl_heation = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'fl_heation', i)
		check_status(status)
		if not status:
			self.fl_heation = ret_fl_heation
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'fl_magel') 
			print ('obj = ' + str(obj))
		status, ret_fl_magel = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'fl_magel', i)
		check_status(status)
		if not status:
			self.fl_magel = ret_fl_magel
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'fl_magheatel') 
			print ('obj = ' + str(obj))
		status, ret_fl_magheatel = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'fl_magheatel', i)
		check_status(status)
		if not status:
			self.fl_magheatel = ret_fl_magheatel
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'fl_magion') 
			print ('obj = ' + str(obj))
		status, ret_fl_magion = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'fl_magion', i)
		check_status(status)
		if not status:
			self.fl_magion = ret_fl_magion
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flmagheation') 
			print ('obj = ' + str(obj))
		status, ret_flmagheation = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flmagheation', i)
		check_status(status)
		if not status:
			self.flmagheation = ret_flmagheation

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var0dstructureturbvar0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'dtime_type') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'dtime_type', i, self.dtime_type)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var0dstructureturbvar0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'dtime_type') 
			print ('obj = ' + str(obj))
		status, ret_dtime_type = ull.getStringFromObject(self.idx, obj, cpopath + 'dtime_type', i)
		check_status(status)
		if not status:
			self.dtime_type = ret_dtime_type

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dtime_type')
		ull.deleteData(self.idx, path, cpopath + 'dtime')
		ull.deleteData(self.idx, path, cpopath + 'en_exb')
		ull.deleteData(self.idx, path, cpopath + 'en_mag')
		ull.deleteData(self.idx, path, cpopath + 'en_el_th')
		ull.deleteData(self.idx, path, cpopath + 'en_ion_th')
		ull.deleteData(self.idx, path, cpopath + 'en_el_par')
		ull.deleteData(self.idx, path, cpopath + 'en_ion_par')
		ull.deleteData(self.idx, path, cpopath + 'en_tot')
		ull.deleteData(self.idx, path, cpopath + 'fl_el')
		ull.deleteData(self.idx, path, cpopath + 'fl_heatel')
		ull.deleteData(self.idx, path, cpopath + 'fl_ion')
		ull.deleteData(self.idx, path, cpopath + 'fl_heation')
		ull.deleteData(self.idx, path, cpopath + 'fl_magel')
		ull.deleteData(self.idx, path, cpopath + 'fl_magheatel')
		ull.deleteData(self.idx, path, cpopath + 'fl_magion')
		ull.deleteData(self.idx, path, cpopath + 'flmagheation')


class var1dstructureturbvar1d(KeepInOrder):
	'''
	class var1dstructureturbvar1d
	Dependent variable radial profile.

	Attributes:
	- rho_tor_norm : numpy.ndarray 1D with float
	   Normalised toroidal flux  coordinate. Vector(nrho1d)
	- phi : numpy.ndarray 1D with float
	   Electrostatic potential [V]; Time-dependent; Vector (nrho1d).
	- er : numpy.ndarray 1D with float
	   Radial electric field [V/m]; Time-dependent; Vector (nrho1d).
	- vor : numpy.ndarray 1D with float
	   Vorticity [s^-1]; Time-dependent; Vector (nrho1d).
	- apl : numpy.ndarray 1D with float
	   Parallel magnetic potential divided by B [m]; Time-dependent; Vector (nrho1d).
	- jpl : numpy.ndarray 1D with float
	   Parallel current divided by B [A/m^2 per T]; Time-dependent; Vector (nrho1d).
	- ne : numpy.ndarray 1D with float
	   Electron density [m^-3]; Time-dependent; Vector (nrho1d).
	- te : numpy.ndarray 1D with float
	   Electron temperature [eV]; Time-dependent; Vector (nrho1d).
	- ni : numpy.ndarray 2D with float
	   Ion density [m^-3]; Time-dependent; Matrix (nrho1d,nion).
	- ti : numpy.ndarray 2D with float
	   Ion temperature [eV]; Time-dependent; Matrix (nrho1d,nion).
	- ui : numpy.ndarray 2D with float
	   Ion parallel velocity divided by B [m/s per T]; Time-dependent; Matrix (nrho1d,nion).
	'''

	def __init__(self, base_path_in='var1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.er = numpy.zeros(0, numpy.float64, order='C')
		self.vor = numpy.zeros(0, numpy.float64, order='C')
		self.apl = numpy.zeros(0, numpy.float64, order='C')
		self.jpl = numpy.zeros(0, numpy.float64, order='C')
		self.ne = numpy.zeros(0, numpy.float64, order='C')
		self.te = numpy.zeros(0, numpy.float64, order='C')
		self.ni = numpy.zeros((0,0), numpy.float64, order='C')
		self.ti = numpy.zeros((0,0), numpy.float64, order='C')
		self.ui = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class var1dstructureturbvar1d\n'
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.er.__str__()
		ret = ret + space + 'Attribute er\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vor.__str__()
		ret = ret + space + 'Attribute vor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.apl.__str__()
		ret = ret + space + 'Attribute apl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jpl.__str__()
		ret = ret + space + 'Attribute jpl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ne.__str__()
		ret = ret + space + 'Attribute ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.te.__str__()
		ret = ret + space + 'Attribute te\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ni.__str__()
		ret = ret + space + 'Attribute ni\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ti.__str__()
		ret = ret + space + 'Attribute ti\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ui.__str__()
		ret = ret + space + 'Attribute ui\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var1dstructureturbvar1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'er', numpy.array(self.er).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'apl', numpy.array(self.apl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ni', numpy.array(self.ni).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ui', numpy.array(self.ui).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var1dstructureturbvar1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'er', numpy.array(self.er).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'apl', numpy.array(self.apl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ni', numpy.array(self.ni).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ui', numpy.array(self.ui).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var1dstructureturbvar1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type var1dstructureturbvar1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_rho_tor_norm = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm')
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		status, ret_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_er, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'er', inTime, interpolMode)
		check_status(status)
		if not status:
			self.er = ret_er
			self.cpoTime = retTime
		status, ret_vor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vor = ret_vor
			self.cpoTime = retTime
		status, ret_apl, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'apl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.apl = ret_apl
			self.cpoTime = retTime
		status, ret_jpl, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
			self.cpoTime = retTime
		status, ret_ne, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ne = ret_ne
			self.cpoTime = retTime
		status, ret_te, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'te', inTime, interpolMode)
		check_status(status)
		if not status:
			self.te = ret_te
			self.cpoTime = retTime
		status, ret_ni, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ni', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ni = ret_ni
			self.cpoTime = retTime
		status, ret_ti, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ti', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ti = ret_ti
			self.cpoTime = retTime
		status, ret_ui, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ui', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ui = ret_ui
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type var1dstructureturbvar1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rho_tor_normVal = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			check_status(status)
			status, phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,nbslice))
			check_status(status)
			status, erList = ull.getVect2DDouble(self.idx, path, cpopath + 'er')
			if len(erList) == 0:
				erList = numpy.resize(erList, (0,nbslice))
			check_status(status)
			status, vorList = ull.getVect2DDouble(self.idx, path, cpopath + 'vor')
			if len(vorList) == 0:
				vorList = numpy.resize(vorList, (0,nbslice))
			check_status(status)
			status, aplList = ull.getVect2DDouble(self.idx, path, cpopath + 'apl')
			if len(aplList) == 0:
				aplList = numpy.resize(aplList, (0,nbslice))
			check_status(status)
			status, jplList = ull.getVect2DDouble(self.idx, path, cpopath + 'jpl')
			if len(jplList) == 0:
				jplList = numpy.resize(jplList, (0,nbslice))
			check_status(status)
			status, neList = ull.getVect2DDouble(self.idx, path, cpopath + 'ne')
			if len(neList) == 0:
				neList = numpy.resize(neList, (0,nbslice))
			check_status(status)
			status, teList = ull.getVect2DDouble(self.idx, path, cpopath + 'te')
			if len(teList) == 0:
				teList = numpy.resize(teList, (0,nbslice))
			check_status(status)
			status, niList = ull.getVect3DDouble(self.idx, path, cpopath + 'ni')
			if len(niList) == 0:
				niList = numpy.resize(niList, (0,0,nbslice))
			check_status(status)
			status, tiList = ull.getVect3DDouble(self.idx, path, cpopath + 'ti')
			if len(tiList) == 0:
				tiList = numpy.resize(tiList, (0,0,nbslice))
			check_status(status)
			status, uiList = ull.getVect3DDouble(self.idx, path, cpopath + 'ui')
			if len(uiList) == 0:
				uiList = numpy.resize(uiList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = var1dstructureturbvar1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.rho_tor_norm = rho_tor_normVal
				slice.phi = phiList[:,i]
				slice.er = erList[:,i]
				slice.vor = vorList[:,i]
				slice.apl = aplList[:,i]
				slice.jpl = jplList[:,i]
				slice.ne = neList[:,i]
				slice.te = teList[:,i]
				slice.ni = niList[:,:,i]
				slice.ti = tiList[:,:,i]
				slice.ui = uiList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var1dstructureturbvar1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'er') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'er', i, numpy.array(self.er).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vor', i, numpy.array(self.vor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'apl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'apl', i, numpy.array(self.apl).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'jpl', i, numpy.array(self.jpl).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ne', i, numpy.array(self.ne).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'te', i, numpy.array(self.te).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ni') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ni', i, numpy.array(self.ni).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ti', i, numpy.array(self.ti).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ui') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ui', i, numpy.array(self.ui).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var1dstructureturbvar1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'er') 
			print ('obj = ' + str(obj))
		status, ret_er = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'er', i)
		check_status(status)
		if not status:
			self.er = ret_er
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		status, ret_vor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vor', i)
		check_status(status)
		if not status:
			self.vor = ret_vor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'apl') 
			print ('obj = ' + str(obj))
		status, ret_apl = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'apl', i)
		check_status(status)
		if not status:
			self.apl = ret_apl
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		status, ret_jpl = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'jpl', i)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		status, ret_ne = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ne', i)
		check_status(status)
		if not status:
			self.ne = ret_ne
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		status, ret_te = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'te', i)
		check_status(status)
		if not status:
			self.te = ret_te
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ni') 
			print ('obj = ' + str(obj))
		status, ret_ni = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ni', i)
		check_status(status)
		if not status:
			self.ni = ret_ni
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		status, ret_ti = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ti', i)
		check_status(status)
		if not status:
			self.ti = ret_ti
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ui') 
			print ('obj = ' + str(obj))
		status, ret_ui = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ui', i)
		check_status(status)
		if not status:
			self.ui = ret_ui

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var1dstructureturbvar1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor_norm', i, numpy.array(self.rho_tor_norm).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var1dstructureturbvar1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor_norm = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor_norm', i)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'rho_tor_norm')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'er')
		ull.deleteData(self.idx, path, cpopath + 'vor')
		ull.deleteData(self.idx, path, cpopath + 'apl')
		ull.deleteData(self.idx, path, cpopath + 'jpl')
		ull.deleteData(self.idx, path, cpopath + 'ne')
		ull.deleteData(self.idx, path, cpopath + 'te')
		ull.deleteData(self.idx, path, cpopath + 'ni')
		ull.deleteData(self.idx, path, cpopath + 'ti')
		ull.deleteData(self.idx, path, cpopath + 'ui')


class var2dstructureturbvar2d(KeepInOrder):
	'''
	class var2dstructureturbvar2d
	Dependent variable axisymmetric.

	Attributes:
	- rho_tor_norm : numpy.ndarray 1D with float
	   Normalised toroidal flux  coordinate. Vector(nrho2d)
	- theta : numpy.ndarray 1D with float
	   Straight field line poloidal angle angle [rad]. Vector(ntheta2d)
	- phi : numpy.ndarray 2D with float
	   Electrostatic potential [V]; Time-dependent; Matrix (nrho2d,ntheta2d).
	- apl : numpy.ndarray 2D with float
	   Parallel magnetic potential divided by B [m]; Time-dependent; Matrix(nrho2d,ntheta2d).
	- jpl : numpy.ndarray 2D with float
	   Parallel current divided by B [A/m^2 per T]; Time-dependent; Matrix (nrho2d,ntheta2d).
	- vor : numpy.ndarray 2D with float
	   Vorticity [s^-1]; Time-dependent; Matrix(nrho2d,ntheta2d).
	- ne : numpy.ndarray 2D with float
	   Electron density [m^-3]; Time-dependent; Matrix (nrho2d,ntheta2d).
	- te : numpy.ndarray 2D with float
	   Electron temperature [eV]; Time-dependent; Matrix (nrho2d,ntheta2d).
	- ni : numpy.ndarray 3D with float
	   Ion density [m^-3]; Time-dependent; Array3D (nrho2d,ntheta2d,nion).
	- ti : numpy.ndarray 3D with float
	   Ion temperature [eV]; Time-dependent; Array3D (nrho2d,ntheta2d,nion).
	- ui : numpy.ndarray 3D with float
	   Ion parallel velocity divided by B [m/s per T]; Time-dependent; Array3D(nrho2d,ntheta2d,nion).
	'''

	def __init__(self, base_path_in='var2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.theta = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros((0,0), numpy.float64, order='C')
		self.apl = numpy.zeros((0,0), numpy.float64, order='C')
		self.jpl = numpy.zeros((0,0), numpy.float64, order='C')
		self.vor = numpy.zeros((0,0), numpy.float64, order='C')
		self.ne = numpy.zeros((0,0), numpy.float64, order='C')
		self.te = numpy.zeros((0,0), numpy.float64, order='C')
		self.ni = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.ti = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.ui = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class var2dstructureturbvar2d\n'
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.apl.__str__()
		ret = ret + space + 'Attribute apl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jpl.__str__()
		ret = ret + space + 'Attribute jpl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vor.__str__()
		ret = ret + space + 'Attribute vor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ne.__str__()
		ret = ret + space + 'Attribute ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.te.__str__()
		ret = ret + space + 'Attribute te\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ni.__str__()
		ret = ret + space + 'Attribute ni\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ti.__str__()
		ret = ret + space + 'Attribute ti\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ui.__str__()
		ret = ret + space + 'Attribute ui\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var2dstructureturbvar2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'apl', numpy.array(self.apl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'ni', numpy.array(self.ni).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'ui', numpy.array(self.ui).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var2dstructureturbvar2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'apl', numpy.array(self.apl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'ni', numpy.array(self.ni).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'ui', numpy.array(self.ui).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var2dstructureturbvar2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type var2dstructureturbvar2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_rho_tor_norm = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm')
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		status, ret_theta = ull.getVect1DDouble(self.idx, path, cpopath + 'theta')
		check_status(status)
		if not status:
			self.theta = ret_theta
		status, ret_phi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_apl, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'apl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.apl = ret_apl
			self.cpoTime = retTime
		status, ret_jpl, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'jpl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
			self.cpoTime = retTime
		status, ret_vor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vor = ret_vor
			self.cpoTime = retTime
		status, ret_ne, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ne = ret_ne
			self.cpoTime = retTime
		status, ret_te, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'te', inTime, interpolMode)
		check_status(status)
		if not status:
			self.te = ret_te
			self.cpoTime = retTime
		status, ret_ni, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'ni', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ni = ret_ni
			self.cpoTime = retTime
		status, ret_ti, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'ti', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ti = ret_ti
			self.cpoTime = retTime
		status, ret_ui, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'ui', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ui = ret_ui
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type var2dstructureturbvar2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rho_tor_normVal = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			check_status(status)
			status, thetaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'theta')
			check_status(status)
			status, phiList = ull.getVect3DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,0,nbslice))
			check_status(status)
			status, aplList = ull.getVect3DDouble(self.idx, path, cpopath + 'apl')
			if len(aplList) == 0:
				aplList = numpy.resize(aplList, (0,0,nbslice))
			check_status(status)
			status, jplList = ull.getVect3DDouble(self.idx, path, cpopath + 'jpl')
			if len(jplList) == 0:
				jplList = numpy.resize(jplList, (0,0,nbslice))
			check_status(status)
			status, vorList = ull.getVect3DDouble(self.idx, path, cpopath + 'vor')
			if len(vorList) == 0:
				vorList = numpy.resize(vorList, (0,0,nbslice))
			check_status(status)
			status, neList = ull.getVect3DDouble(self.idx, path, cpopath + 'ne')
			if len(neList) == 0:
				neList = numpy.resize(neList, (0,0,nbslice))
			check_status(status)
			status, teList = ull.getVect3DDouble(self.idx, path, cpopath + 'te')
			if len(teList) == 0:
				teList = numpy.resize(teList, (0,0,nbslice))
			check_status(status)
			status, niList = ull.getVect4DDouble(self.idx, path, cpopath + 'ni')
			if len(niList) == 0:
				niList = numpy.resize(niList, (0,0,0,nbslice))
			check_status(status)
			status, tiList = ull.getVect4DDouble(self.idx, path, cpopath + 'ti')
			if len(tiList) == 0:
				tiList = numpy.resize(tiList, (0,0,0,nbslice))
			check_status(status)
			status, uiList = ull.getVect4DDouble(self.idx, path, cpopath + 'ui')
			if len(uiList) == 0:
				uiList = numpy.resize(uiList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = var2dstructureturbvar2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.rho_tor_norm = rho_tor_normVal
				slice.theta = thetaVal
				slice.phi = phiList[:,:,i]
				slice.apl = aplList[:,:,i]
				slice.jpl = jplList[:,:,i]
				slice.vor = vorList[:,:,i]
				slice.ne = neList[:,:,i]
				slice.te = teList[:,:,i]
				slice.ni = niList[:,:,:,i]
				slice.ti = tiList[:,:,:,i]
				slice.ui = uiList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var2dstructureturbvar2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'apl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'apl', i, numpy.array(self.apl).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'jpl', i, numpy.array(self.jpl).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vor', i, numpy.array(self.vor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ne', i, numpy.array(self.ne).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'te', i, numpy.array(self.te).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'ni') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'ni', i, numpy.array(self.ni).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'ti', i, numpy.array(self.ti).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'ui') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'ui', i, numpy.array(self.ui).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var2dstructureturbvar2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'apl') 
			print ('obj = ' + str(obj))
		status, ret_apl = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'apl', i)
		check_status(status)
		if not status:
			self.apl = ret_apl
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		status, ret_jpl = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'jpl', i)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		status, ret_vor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vor', i)
		check_status(status)
		if not status:
			self.vor = ret_vor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		status, ret_ne = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ne', i)
		check_status(status)
		if not status:
			self.ne = ret_ne
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		status, ret_te = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'te', i)
		check_status(status)
		if not status:
			self.te = ret_te
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'ni') 
			print ('obj = ' + str(obj))
		status, ret_ni = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'ni', i)
		check_status(status)
		if not status:
			self.ni = ret_ni
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		status, ret_ti = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'ti', i)
		check_status(status)
		if not status:
			self.ti = ret_ti
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'ui') 
			print ('obj = ' + str(obj))
		status, ret_ui = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'ui', i)
		check_status(status)
		if not status:
			self.ui = ret_ui

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var2dstructureturbvar2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor_norm', i, numpy.array(self.rho_tor_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var2dstructureturbvar2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor_norm = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor_norm', i)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		status, ret_theta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta', i)
		check_status(status)
		if not status:
			self.theta = ret_theta

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'rho_tor_norm')
		ull.deleteData(self.idx, path, cpopath + 'theta')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'apl')
		ull.deleteData(self.idx, path, cpopath + 'jpl')
		ull.deleteData(self.idx, path, cpopath + 'vor')
		ull.deleteData(self.idx, path, cpopath + 'ne')
		ull.deleteData(self.idx, path, cpopath + 'te')
		ull.deleteData(self.idx, path, cpopath + 'ni')
		ull.deleteData(self.idx, path, cpopath + 'ti')
		ull.deleteData(self.idx, path, cpopath + 'ui')


class var3dstructureturbvar3d(KeepInOrder):
	'''
	class var3dstructureturbvar3d
	Dependent variable morphology. Grid is defined in coord_sys/turbgrid.

	Attributes:
	- phi : numpy.ndarray 3D with float
	   Electrostatic potential [V]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
	- vor : numpy.ndarray 3D with float
	   Vorticity [s^-1]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
	- jpl : numpy.ndarray 3D with float
	   Parallel current [A/m^2]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
	- ne : numpy.ndarray 3D with float
	   Electron density [m^-3]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
	'''

	def __init__(self, base_path_in='var3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.phi = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.vor = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.jpl = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.ne = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class var3dstructureturbvar3d\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vor.__str__()
		ret = ret + space + 'Attribute vor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jpl.__str__()
		ret = ret + space + 'Attribute jpl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ne.__str__()
		ret = ret + space + 'Attribute ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var3dstructureturbvar3d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var3dstructureturbvar3d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var3dstructureturbvar3d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type var3dstructureturbvar3d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_phi, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_vor, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'vor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vor = ret_vor
			self.cpoTime = retTime
		status, ret_jpl, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'jpl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
			self.cpoTime = retTime
		status, ret_ne, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ne = ret_ne
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type var3dstructureturbvar3d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, phiList = ull.getVect4DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,0,0,nbslice))
			check_status(status)
			status, vorList = ull.getVect4DDouble(self.idx, path, cpopath + 'vor')
			if len(vorList) == 0:
				vorList = numpy.resize(vorList, (0,0,0,nbslice))
			check_status(status)
			status, jplList = ull.getVect4DDouble(self.idx, path, cpopath + 'jpl')
			if len(jplList) == 0:
				jplList = numpy.resize(jplList, (0,0,0,nbslice))
			check_status(status)
			status, neList = ull.getVect4DDouble(self.idx, path, cpopath + 'ne')
			if len(neList) == 0:
				neList = numpy.resize(neList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = var3dstructureturbvar3d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.phi = phiList[:,:,:,i]
				slice.vor = vorList[:,:,:,i]
				slice.jpl = jplList[:,:,:,i]
				slice.ne = neList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var3dstructureturbvar3dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'vor', i, numpy.array(self.vor).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'jpl', i, numpy.array(self.jpl).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'ne', i, numpy.array(self.ne).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var3dstructureturbvar3dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		status, ret_vor = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'vor', i)
		check_status(status)
		if not status:
			self.vor = ret_vor
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		status, ret_jpl = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'jpl', i)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		status, ret_ne = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'ne', i)
		check_status(status)
		if not status:
			self.ne = ret_ne

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var3dstructureturbvar3dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var3dstructureturbvar3dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'vor')
		ull.deleteData(self.idx, path, cpopath + 'jpl')
		ull.deleteData(self.idx, path, cpopath + 'ne')


class var4dstructureturbvar4d(KeepInOrder):
	'''
	class var4dstructureturbvar4d
	Gyrokinetic distribution function, axisymmetric component. Grid is defined in coord_sys/turbgrid.

	Attributes:
	- fe : numpy.ndarray 4D with float
	   Electron distribution function times V-space volume element, axisymmetric component [m^-3]; Time-dependent; Array4D(ndim1,ndim2,ndim3,ndim_v1).
	- fi : numpy.ndarray 5D with float
	   Ion distribution function times V-space volume element, axisymmetric component [m^-3]; Time-dependent; Array5D(ndim1,ndim2,ndim3,ndim_v1,nion).
	'''

	def __init__(self, base_path_in='var4d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.fe = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.fi = numpy.zeros((0,0,0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class var4dstructureturbvar4d\n'
		s = self.fe.__str__()
		ret = ret + space + 'Attribute fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fi.__str__()
		ret = ret + space + 'Attribute fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var4dstructureturbvar4d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect5DDoubleSlice(self.idx, path, cpopath + 'fi', numpy.array(self.fi).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var4dstructureturbvar4d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect5DDoubleSlice(self.idx, path, cpopath + 'fi', numpy.array(self.fi).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var4dstructureturbvar4d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type var4dstructureturbvar4d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_fe, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fe = ret_fe
			self.cpoTime = retTime
		status, ret_fi, retTime = ull.getVect5DDoubleSlice(self.idx, path, cpopath + 'fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fi = ret_fi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type var4dstructureturbvar4d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, feList = ull.getVect5DDouble(self.idx, path, cpopath + 'fe')
			if len(feList) == 0:
				feList = numpy.resize(feList, (0,0,0,0,nbslice))
			check_status(status)
			status, fiList = ull.getVect6DDouble(self.idx, path, cpopath + 'fi')
			if len(fiList) == 0:
				fiList = numpy.resize(fiList, (0,0,0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = var4dstructureturbvar4d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.fe = feList[:,:,:,:,i]
				slice.fi = fiList[:,:,:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var4dstructureturbvar4dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'fe', i, numpy.array(self.fe).astype(numpy.float64))
		if (dev()):
			print ('putVect5DDoubleInObject : ' + cpopath + 'fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect5DDoubleInObject(self.idx, obj, cpopath + 'fi', i, numpy.array(self.fi).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var4dstructureturbvar4dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		status, ret_fe = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'fe', i)
		check_status(status)
		if not status:
			self.fe = ret_fe
		if (dev()):
			print ('getVect5DDoubleInObject : ' + cpopath + 'fi') 
			print ('obj = ' + str(obj))
		status, ret_fi = ull.getVect5DDoubleFromObject(self.idx, obj, cpopath + 'fi', i)
		check_status(status)
		if not status:
			self.fi = ret_fi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var4dstructureturbvar4dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var4dstructureturbvar4dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'fe')
		ull.deleteData(self.idx, path, cpopath + 'fi')


class var5dstructureturbvar5d(KeepInOrder):
	'''
	class var5dstructureturbvar5d
	Gyrokinetic distribution function. Grid is defined in coord_sys/turbgrid.

	Attributes:
	- fe : numpy.ndarray 5D with float
	   Electron distribution function times V-space volume element [m^-3]; Time-dependent; Array5D(ndim1,ndim2,ndim3,ndim_v1,ndim_v2).
	- fi : numpy.ndarray 6D with float
	   Ion distribution function times V-space volume element [m^-3]; Time-dependent; Array6D(ndim1,ndim2,ndim3,ndim_v1,ndim_v2,nion).
	'''

	def __init__(self, base_path_in='var5d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.fe = numpy.zeros((0,0,0,0,0), numpy.float64, order='C')
		self.fi = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class var5dstructureturbvar5d\n'
		s = self.fe.__str__()
		ret = ret + space + 'Attribute fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fi.__str__()
		ret = ret + space + 'Attribute fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var5dstructureturbvar5d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect5DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect6DDoubleSlice(self.idx, path, cpopath + 'fi', numpy.array(self.fi).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var5dstructureturbvar5d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect5DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect6DDoubleSlice(self.idx, path, cpopath + 'fi', numpy.array(self.fi).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type var5dstructureturbvar5d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type var5dstructureturbvar5d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_fe, retTime = ull.getVect5DDoubleSlice(self.idx, path, cpopath + 'fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fe = ret_fe
			self.cpoTime = retTime
		status, ret_fi, retTime = ull.getVect6DDoubleSlice(self.idx, path, cpopath + 'fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fi = ret_fi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type var5dstructureturbvar5d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, feList = ull.getVect6DDouble(self.idx, path, cpopath + 'fe')
			if len(feList) == 0:
				feList = numpy.resize(feList, (0,0,0,0,0,nbslice))
			check_status(status)
			status, fiList = ull.getVect7DDouble(self.idx, path, cpopath + 'fi')
			if len(fiList) == 0:
				fiList = numpy.resize(fiList, (0,0,0,0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = var5dstructureturbvar5d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.fe = feList[:,:,:,:,:,i]
				slice.fi = fiList[:,:,:,:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var5dstructureturbvar5dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect5DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect5DDoubleInObject(self.idx, obj, cpopath + 'fe', i, numpy.array(self.fe).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'fi', i, numpy.array(self.fi).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var5dstructureturbvar5dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect5DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		status, ret_fe = ull.getVect5DDoubleFromObject(self.idx, obj, cpopath + 'fe', i)
		check_status(status)
		if not status:
			self.fe = ret_fe
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'fi') 
			print ('obj = ' + str(obj))
		status, ret_fi = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'fi', i)
		check_status(status)
		if not status:
			self.fi = ret_fi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var5dstructureturbvar5dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type var5dstructureturbvar5dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'fe')
		ull.deleteData(self.idx, path, cpopath + 'fi')


class spec1dstructureturbspec1d(KeepInOrder):
	'''
	class spec1dstructureturbspec1d
	Toroidal mode number spectra.

	Attributes:
	- kperp : numpy.ndarray 1D with float
	   Perpendicular wavenumber [m^-1]; Vector (ndim_spec).
	- phi : numpy.ndarray 1D with float
	   Electrostatic potential [V^2 per mode]; Time-dependent; Vector (ndim_spec).
	- vor : numpy.ndarray 1D with float
	   Vorticity [s^-2 per mode]; Time-dependent; Vector (ndim_spec).
	- b : numpy.ndarray 1D with float
	   Magnetic energy [T^2 per mode]; Time-dependent; Vector (ndim_spec).
	- jpl : numpy.ndarray 1D with float
	   Current [A^2/m^4 per mode]; Time-dependent; Vector (ndim_spec).
	- ne : numpy.ndarray 1D with float
	   Electron density [m^-6 per mode]; Time-dependent; Vector (ndim_spec).
	- te : numpy.ndarray 1D with float
	   Electron temperature [eV^2 per mode]; Time-dependent; Vector (ndim_spec).
	- ti : numpy.ndarray 2D with float
	   Ion temperature [eV^2 per mode]; Time-dependent; Matrix (ndim_spec,nion).
	- fe : numpy.ndarray 1D with float
	   Electron particle flux [m^-2/s per mode]; Time-dependent; Vector (ndim_spec).
	- qe : numpy.ndarray 1D with float
	   Electron conductive heat flux [W.m^-2 per mode]; Time-dependent; Vector (ndim_spec).
	- qi : numpy.ndarray 2D with float
	   Ion conductive heat flux [W.m^-2 per mode]; Time-dependent; Matrix(ndim_spec,nion).
	- me : numpy.ndarray 1D with float
	   Magnetic electron heat flux [W.m^-2 per mode]; Time-dependent; Matrix (ndim_spec).
	- mi : numpy.ndarray 2D with float
	   Magnetic ion heat flux [W.m^-2 per mode]; Time-dependent; Matrix (ndim_spec,nion).
	'''

	def __init__(self, base_path_in='spec1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.kperp = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.vor = numpy.zeros(0, numpy.float64, order='C')
		self.b = numpy.zeros(0, numpy.float64, order='C')
		self.jpl = numpy.zeros(0, numpy.float64, order='C')
		self.ne = numpy.zeros(0, numpy.float64, order='C')
		self.te = numpy.zeros(0, numpy.float64, order='C')
		self.ti = numpy.zeros((0,0), numpy.float64, order='C')
		self.fe = numpy.zeros(0, numpy.float64, order='C')
		self.qe = numpy.zeros(0, numpy.float64, order='C')
		self.qi = numpy.zeros((0,0), numpy.float64, order='C')
		self.me = numpy.zeros(0, numpy.float64, order='C')
		self.mi = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spec1dstructureturbspec1d\n'
		s = self.kperp.__str__()
		ret = ret + space + 'Attribute kperp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vor.__str__()
		ret = ret + space + 'Attribute vor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b.__str__()
		ret = ret + space + 'Attribute b\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jpl.__str__()
		ret = ret + space + 'Attribute jpl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ne.__str__()
		ret = ret + space + 'Attribute ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.te.__str__()
		ret = ret + space + 'Attribute te\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ti.__str__()
		ret = ret + space + 'Attribute ti\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fe.__str__()
		ret = ret + space + 'Attribute fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.qe.__str__()
		ret = ret + space + 'Attribute qe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.qi.__str__()
		ret = ret + space + 'Attribute qi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.me.__str__()
		ret = ret + space + 'Attribute me\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mi.__str__()
		ret = ret + space + 'Attribute mi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spec1dstructureturbspec1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'b', numpy.array(self.b).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'qe', numpy.array(self.qe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'qi', numpy.array(self.qi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'me', numpy.array(self.me).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'mi', numpy.array(self.mi).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spec1dstructureturbspec1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'b', numpy.array(self.b).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'qe', numpy.array(self.qe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'qi', numpy.array(self.qi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'me', numpy.array(self.me).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'mi', numpy.array(self.mi).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spec1dstructureturbspec1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'kperp', numpy.array(self.kperp).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type spec1dstructureturbspec1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_kperp = ull.getVect1DDouble(self.idx, path, cpopath + 'kperp')
		check_status(status)
		if not status:
			self.kperp = ret_kperp
		status, ret_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_vor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vor = ret_vor
			self.cpoTime = retTime
		status, ret_b, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b = ret_b
			self.cpoTime = retTime
		status, ret_jpl, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
			self.cpoTime = retTime
		status, ret_ne, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ne = ret_ne
			self.cpoTime = retTime
		status, ret_te, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'te', inTime, interpolMode)
		check_status(status)
		if not status:
			self.te = ret_te
			self.cpoTime = retTime
		status, ret_ti, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ti', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ti = ret_ti
			self.cpoTime = retTime
		status, ret_fe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fe = ret_fe
			self.cpoTime = retTime
		status, ret_qe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'qe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qe = ret_qe
			self.cpoTime = retTime
		status, ret_qi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'qi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qi = ret_qi
			self.cpoTime = retTime
		status, ret_me, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'me', inTime, interpolMode)
		check_status(status)
		if not status:
			self.me = ret_me
			self.cpoTime = retTime
		status, ret_mi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'mi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.mi = ret_mi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type spec1dstructureturbspec1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, kperpVal = ull.getVect1DDouble(self.idx, path, cpopath + 'kperp')
			check_status(status)
			status, phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,nbslice))
			check_status(status)
			status, vorList = ull.getVect2DDouble(self.idx, path, cpopath + 'vor')
			if len(vorList) == 0:
				vorList = numpy.resize(vorList, (0,nbslice))
			check_status(status)
			status, bList = ull.getVect2DDouble(self.idx, path, cpopath + 'b')
			if len(bList) == 0:
				bList = numpy.resize(bList, (0,nbslice))
			check_status(status)
			status, jplList = ull.getVect2DDouble(self.idx, path, cpopath + 'jpl')
			if len(jplList) == 0:
				jplList = numpy.resize(jplList, (0,nbslice))
			check_status(status)
			status, neList = ull.getVect2DDouble(self.idx, path, cpopath + 'ne')
			if len(neList) == 0:
				neList = numpy.resize(neList, (0,nbslice))
			check_status(status)
			status, teList = ull.getVect2DDouble(self.idx, path, cpopath + 'te')
			if len(teList) == 0:
				teList = numpy.resize(teList, (0,nbslice))
			check_status(status)
			status, tiList = ull.getVect3DDouble(self.idx, path, cpopath + 'ti')
			if len(tiList) == 0:
				tiList = numpy.resize(tiList, (0,0,nbslice))
			check_status(status)
			status, feList = ull.getVect2DDouble(self.idx, path, cpopath + 'fe')
			if len(feList) == 0:
				feList = numpy.resize(feList, (0,nbslice))
			check_status(status)
			status, qeList = ull.getVect2DDouble(self.idx, path, cpopath + 'qe')
			if len(qeList) == 0:
				qeList = numpy.resize(qeList, (0,nbslice))
			check_status(status)
			status, qiList = ull.getVect3DDouble(self.idx, path, cpopath + 'qi')
			if len(qiList) == 0:
				qiList = numpy.resize(qiList, (0,0,nbslice))
			check_status(status)
			status, meList = ull.getVect2DDouble(self.idx, path, cpopath + 'me')
			if len(meList) == 0:
				meList = numpy.resize(meList, (0,nbslice))
			check_status(status)
			status, miList = ull.getVect3DDouble(self.idx, path, cpopath + 'mi')
			if len(miList) == 0:
				miList = numpy.resize(miList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = spec1dstructureturbspec1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.kperp = kperpVal
				slice.phi = phiList[:,i]
				slice.vor = vorList[:,i]
				slice.b = bList[:,i]
				slice.jpl = jplList[:,i]
				slice.ne = neList[:,i]
				slice.te = teList[:,i]
				slice.ti = tiList[:,:,i]
				slice.fe = feList[:,i]
				slice.qe = qeList[:,i]
				slice.qi = qiList[:,:,i]
				slice.me = meList[:,i]
				slice.mi = miList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spec1dstructureturbspec1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vor', i, numpy.array(self.vor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'b', i, numpy.array(self.b).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'jpl', i, numpy.array(self.jpl).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ne', i, numpy.array(self.ne).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'te', i, numpy.array(self.te).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ti', i, numpy.array(self.ti).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'fe', i, numpy.array(self.fe).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'qe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'qe', i, numpy.array(self.qe).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'qi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'qi', i, numpy.array(self.qi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'me') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'me', i, numpy.array(self.me).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'mi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'mi', i, numpy.array(self.mi).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spec1dstructureturbspec1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		status, ret_vor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vor', i)
		check_status(status)
		if not status:
			self.vor = ret_vor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'b') 
			print ('obj = ' + str(obj))
		status, ret_b = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'b', i)
		check_status(status)
		if not status:
			self.b = ret_b
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		status, ret_jpl = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'jpl', i)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		status, ret_ne = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ne', i)
		check_status(status)
		if not status:
			self.ne = ret_ne
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		status, ret_te = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'te', i)
		check_status(status)
		if not status:
			self.te = ret_te
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		status, ret_ti = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ti', i)
		check_status(status)
		if not status:
			self.ti = ret_ti
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		status, ret_fe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'fe', i)
		check_status(status)
		if not status:
			self.fe = ret_fe
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'qe') 
			print ('obj = ' + str(obj))
		status, ret_qe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'qe', i)
		check_status(status)
		if not status:
			self.qe = ret_qe
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'qi') 
			print ('obj = ' + str(obj))
		status, ret_qi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'qi', i)
		check_status(status)
		if not status:
			self.qi = ret_qi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'me') 
			print ('obj = ' + str(obj))
		status, ret_me = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'me', i)
		check_status(status)
		if not status:
			self.me = ret_me
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'mi') 
			print ('obj = ' + str(obj))
		status, ret_mi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'mi', i)
		check_status(status)
		if not status:
			self.mi = ret_mi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spec1dstructureturbspec1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'kperp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'kperp', i, numpy.array(self.kperp).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spec1dstructureturbspec1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'kperp') 
			print ('obj = ' + str(obj))
		status, ret_kperp = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'kperp', i)
		check_status(status)
		if not status:
			self.kperp = ret_kperp

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'kperp')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'vor')
		ull.deleteData(self.idx, path, cpopath + 'b')
		ull.deleteData(self.idx, path, cpopath + 'jpl')
		ull.deleteData(self.idx, path, cpopath + 'ne')
		ull.deleteData(self.idx, path, cpopath + 'te')
		ull.deleteData(self.idx, path, cpopath + 'ti')
		ull.deleteData(self.idx, path, cpopath + 'fe')
		ull.deleteData(self.idx, path, cpopath + 'qe')
		ull.deleteData(self.idx, path, cpopath + 'qi')
		ull.deleteData(self.idx, path, cpopath + 'me')
		ull.deleteData(self.idx, path, cpopath + 'mi')


class env1dstructureturbenv1d(KeepInOrder):
	'''
	class env1dstructureturbenv1d
	Parallel fluctuation envelope.

	Attributes:
	- theta : numpy.ndarray 1D with float
	   Straight field line poloidal angle [rad]; Vector (ntheta_env).
	- phi : numpy.ndarray 1D with float
	   Electrostatic potential [V^2]; Time-dependent; Vector (ntheta_env).
	- vor : numpy.ndarray 1D with float
	   Vorticity [coulomb^2/m^6]; Time-dependent; Vector (ntheta_env).
	- jpl : numpy.ndarray 1D with float
	   Parallel current [A^2/m^4]; Time-dependent; Vector (ntheta_env).
	- ne : numpy.ndarray 1D with float
	   Electron density [m^-6]; Time-dependent; Vector (ntheta_env).
	- he : numpy.ndarray 1D with float
	   Nonadiabatic electron density [m^-6]; Time-dependent; Vector (ntheta_env).
	- te : numpy.ndarray 1D with float
	   Electron temperature [eV^2]; Time-dependent; Vector (ntheta_env).
	- ni : numpy.ndarray 2D with float
	   Ion density [m^-6]; Time-dependent; Matrix(ntheta_env,nion).
	- ti : numpy.ndarray 2D with float
	   Ion temperature [eV^2]; Time-dependent; Matrix(ntheta_env,nion).
	- ui : numpy.ndarray 2D with float
	   Ion parallel velocity [m^2/s^2]; Time-dependent; Matrix (ntheta_env,nion).
	- fe : numpy.ndarray 1D with float
	   Electron particle flux [m^-2/s per mode]; Time-dependent; Vector (ntheta_env).
	- qe : numpy.ndarray 1D with float
	   Electron conductive heat flux [W.m^-2 per mode]; Time-dependent; Vector (ntheta_env).
	- qi : numpy.ndarray 2D with float
	   Ion conductive heat flux [W. m^-2 per mode]; Time-dependent; Matrix(ntheta_env,nion).
	- me : numpy.ndarray 1D with float
	   Magnetic electron heat flux [W.m^-2 per mode]; Time-dependent; Vector (ntheta_env).
	- mi : numpy.ndarray 2D with float
	   Magnetic ion heat flux [W. m^-2 per mode]; Time-dependent; Matrix(ntheta_env,nion).
	'''

	def __init__(self, base_path_in='env1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.theta = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.vor = numpy.zeros(0, numpy.float64, order='C')
		self.jpl = numpy.zeros(0, numpy.float64, order='C')
		self.ne = numpy.zeros(0, numpy.float64, order='C')
		self.he = numpy.zeros(0, numpy.float64, order='C')
		self.te = numpy.zeros(0, numpy.float64, order='C')
		self.ni = numpy.zeros((0,0), numpy.float64, order='C')
		self.ti = numpy.zeros((0,0), numpy.float64, order='C')
		self.ui = numpy.zeros((0,0), numpy.float64, order='C')
		self.fe = numpy.zeros(0, numpy.float64, order='C')
		self.qe = numpy.zeros(0, numpy.float64, order='C')
		self.qi = numpy.zeros((0,0), numpy.float64, order='C')
		self.me = numpy.zeros(0, numpy.float64, order='C')
		self.mi = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class env1dstructureturbenv1d\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vor.__str__()
		ret = ret + space + 'Attribute vor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jpl.__str__()
		ret = ret + space + 'Attribute jpl\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ne.__str__()
		ret = ret + space + 'Attribute ne\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.he.__str__()
		ret = ret + space + 'Attribute he\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.te.__str__()
		ret = ret + space + 'Attribute te\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ni.__str__()
		ret = ret + space + 'Attribute ni\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ti.__str__()
		ret = ret + space + 'Attribute ti\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ui.__str__()
		ret = ret + space + 'Attribute ui\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.fe.__str__()
		ret = ret + space + 'Attribute fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.qe.__str__()
		ret = ret + space + 'Attribute qe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.qi.__str__()
		ret = ret + space + 'Attribute qi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.me.__str__()
		ret = ret + space + 'Attribute me\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mi.__str__()
		ret = ret + space + 'Attribute mi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type env1dstructureturbenv1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'he', numpy.array(self.he).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ni', numpy.array(self.ni).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'ui', numpy.array(self.ui).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'qe', numpy.array(self.qe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'qi', numpy.array(self.qi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'me', numpy.array(self.me).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'mi', numpy.array(self.mi).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type env1dstructureturbenv1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vor', numpy.array(self.vor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', numpy.array(self.jpl).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ne', numpy.array(self.ne).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'he', numpy.array(self.he).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'te', numpy.array(self.te).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ni', numpy.array(self.ni).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ti', numpy.array(self.ti).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'ui', numpy.array(self.ui).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'fe', numpy.array(self.fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'qe', numpy.array(self.qe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'qi', numpy.array(self.qi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'me', numpy.array(self.me).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'mi', numpy.array(self.mi).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type env1dstructureturbenv1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type env1dstructureturbenv1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_theta = ull.getVect1DDouble(self.idx, path, cpopath + 'theta')
		check_status(status)
		if not status:
			self.theta = ret_theta
		status, ret_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_vor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vor = ret_vor
			self.cpoTime = retTime
		status, ret_jpl, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jpl', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
			self.cpoTime = retTime
		status, ret_ne, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ne', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ne = ret_ne
			self.cpoTime = retTime
		status, ret_he, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'he', inTime, interpolMode)
		check_status(status)
		if not status:
			self.he = ret_he
			self.cpoTime = retTime
		status, ret_te, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'te', inTime, interpolMode)
		check_status(status)
		if not status:
			self.te = ret_te
			self.cpoTime = retTime
		status, ret_ni, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ni', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ni = ret_ni
			self.cpoTime = retTime
		status, ret_ti, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ti', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ti = ret_ti
			self.cpoTime = retTime
		status, ret_ui, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'ui', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ui = ret_ui
			self.cpoTime = retTime
		status, ret_fe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.fe = ret_fe
			self.cpoTime = retTime
		status, ret_qe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'qe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qe = ret_qe
			self.cpoTime = retTime
		status, ret_qi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'qi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qi = ret_qi
			self.cpoTime = retTime
		status, ret_me, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'me', inTime, interpolMode)
		check_status(status)
		if not status:
			self.me = ret_me
			self.cpoTime = retTime
		status, ret_mi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'mi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.mi = ret_mi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type env1dstructureturbenv1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, thetaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'theta')
			check_status(status)
			status, phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,nbslice))
			check_status(status)
			status, vorList = ull.getVect2DDouble(self.idx, path, cpopath + 'vor')
			if len(vorList) == 0:
				vorList = numpy.resize(vorList, (0,nbslice))
			check_status(status)
			status, jplList = ull.getVect2DDouble(self.idx, path, cpopath + 'jpl')
			if len(jplList) == 0:
				jplList = numpy.resize(jplList, (0,nbslice))
			check_status(status)
			status, neList = ull.getVect2DDouble(self.idx, path, cpopath + 'ne')
			if len(neList) == 0:
				neList = numpy.resize(neList, (0,nbslice))
			check_status(status)
			status, heList = ull.getVect2DDouble(self.idx, path, cpopath + 'he')
			if len(heList) == 0:
				heList = numpy.resize(heList, (0,nbslice))
			check_status(status)
			status, teList = ull.getVect2DDouble(self.idx, path, cpopath + 'te')
			if len(teList) == 0:
				teList = numpy.resize(teList, (0,nbslice))
			check_status(status)
			status, niList = ull.getVect3DDouble(self.idx, path, cpopath + 'ni')
			if len(niList) == 0:
				niList = numpy.resize(niList, (0,0,nbslice))
			check_status(status)
			status, tiList = ull.getVect3DDouble(self.idx, path, cpopath + 'ti')
			if len(tiList) == 0:
				tiList = numpy.resize(tiList, (0,0,nbslice))
			check_status(status)
			status, uiList = ull.getVect3DDouble(self.idx, path, cpopath + 'ui')
			if len(uiList) == 0:
				uiList = numpy.resize(uiList, (0,0,nbslice))
			check_status(status)
			status, feList = ull.getVect2DDouble(self.idx, path, cpopath + 'fe')
			if len(feList) == 0:
				feList = numpy.resize(feList, (0,nbslice))
			check_status(status)
			status, qeList = ull.getVect2DDouble(self.idx, path, cpopath + 'qe')
			if len(qeList) == 0:
				qeList = numpy.resize(qeList, (0,nbslice))
			check_status(status)
			status, qiList = ull.getVect3DDouble(self.idx, path, cpopath + 'qi')
			if len(qiList) == 0:
				qiList = numpy.resize(qiList, (0,0,nbslice))
			check_status(status)
			status, meList = ull.getVect2DDouble(self.idx, path, cpopath + 'me')
			if len(meList) == 0:
				meList = numpy.resize(meList, (0,nbslice))
			check_status(status)
			status, miList = ull.getVect3DDouble(self.idx, path, cpopath + 'mi')
			if len(miList) == 0:
				miList = numpy.resize(miList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = env1dstructureturbenv1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.theta = thetaVal
				slice.phi = phiList[:,i]
				slice.vor = vorList[:,i]
				slice.jpl = jplList[:,i]
				slice.ne = neList[:,i]
				slice.he = heList[:,i]
				slice.te = teList[:,i]
				slice.ni = niList[:,:,i]
				slice.ti = tiList[:,:,i]
				slice.ui = uiList[:,:,i]
				slice.fe = feList[:,i]
				slice.qe = qeList[:,i]
				slice.qi = qiList[:,:,i]
				slice.me = meList[:,i]
				slice.mi = miList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type env1dstructureturbenv1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vor', i, numpy.array(self.vor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'jpl', i, numpy.array(self.jpl).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ne', i, numpy.array(self.ne).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'he') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'he', i, numpy.array(self.he).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'te', i, numpy.array(self.te).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ni') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ni', i, numpy.array(self.ni).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ti', i, numpy.array(self.ti).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'ui') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'ui', i, numpy.array(self.ui).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'fe', i, numpy.array(self.fe).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'qe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'qe', i, numpy.array(self.qe).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'qi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'qi', i, numpy.array(self.qi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'me') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'me', i, numpy.array(self.me).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'mi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'mi', i, numpy.array(self.mi).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type env1dstructureturbenv1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vor') 
			print ('obj = ' + str(obj))
		status, ret_vor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vor', i)
		check_status(status)
		if not status:
			self.vor = ret_vor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'jpl') 
			print ('obj = ' + str(obj))
		status, ret_jpl = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'jpl', i)
		check_status(status)
		if not status:
			self.jpl = ret_jpl
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ne') 
			print ('obj = ' + str(obj))
		status, ret_ne = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ne', i)
		check_status(status)
		if not status:
			self.ne = ret_ne
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'he') 
			print ('obj = ' + str(obj))
		status, ret_he = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'he', i)
		check_status(status)
		if not status:
			self.he = ret_he
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'te') 
			print ('obj = ' + str(obj))
		status, ret_te = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'te', i)
		check_status(status)
		if not status:
			self.te = ret_te
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ni') 
			print ('obj = ' + str(obj))
		status, ret_ni = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ni', i)
		check_status(status)
		if not status:
			self.ni = ret_ni
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ti') 
			print ('obj = ' + str(obj))
		status, ret_ti = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ti', i)
		check_status(status)
		if not status:
			self.ti = ret_ti
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'ui') 
			print ('obj = ' + str(obj))
		status, ret_ui = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'ui', i)
		check_status(status)
		if not status:
			self.ui = ret_ui
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'fe') 
			print ('obj = ' + str(obj))
		status, ret_fe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'fe', i)
		check_status(status)
		if not status:
			self.fe = ret_fe
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'qe') 
			print ('obj = ' + str(obj))
		status, ret_qe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'qe', i)
		check_status(status)
		if not status:
			self.qe = ret_qe
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'qi') 
			print ('obj = ' + str(obj))
		status, ret_qi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'qi', i)
		check_status(status)
		if not status:
			self.qi = ret_qi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'me') 
			print ('obj = ' + str(obj))
		status, ret_me = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'me', i)
		check_status(status)
		if not status:
			self.me = ret_me
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'mi') 
			print ('obj = ' + str(obj))
		status, ret_mi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'mi', i)
		check_status(status)
		if not status:
			self.mi = ret_mi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type env1dstructureturbenv1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type env1dstructureturbenv1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		status, ret_theta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta', i)
		check_status(status)
		if not status:
			self.theta = ret_theta

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'theta')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'vor')
		ull.deleteData(self.idx, path, cpopath + 'jpl')
		ull.deleteData(self.idx, path, cpopath + 'ne')
		ull.deleteData(self.idx, path, cpopath + 'he')
		ull.deleteData(self.idx, path, cpopath + 'te')
		ull.deleteData(self.idx, path, cpopath + 'ni')
		ull.deleteData(self.idx, path, cpopath + 'ti')
		ull.deleteData(self.idx, path, cpopath + 'ui')
		ull.deleteData(self.idx, path, cpopath + 'fe')
		ull.deleteData(self.idx, path, cpopath + 'qe')
		ull.deleteData(self.idx, path, cpopath + 'qi')
		ull.deleteData(self.idx, path, cpopath + 'me')
		ull.deleteData(self.idx, path, cpopath + 'mi')


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
