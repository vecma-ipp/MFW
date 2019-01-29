# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class wall:
	'''
	class wall
	General Wall representation. Time-dependent CPO.

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- wall0d : class wall0dstructurewall_wall0d
	   Simple 0D description of plasma-wall interaction
	- wall2d_mhd : class wall2d_mhdstructurewall2d_mhd
	   Simplified wall that encloses necessary information for RWM codes.
	- wall2d : class wall2dstruct_arraywall2d: array of wall2dstruct_arraywall2dObj objects
	   2D wall descriptions; Array of structures (number of wall descriptions). Replicate this element for each type of possible physics or engineering configurations necessary (gas tight vs wall with ports and holes, coarse vs fine representation, single contour limiter, disjoint gapped plasma facing components, ...). Time-dependent
	- wall3d : class wall3dstruct_arraywall3d: array of wall3dstruct_arraywall3dObj objects
	   3D wall descriptions; Array of structures (number of wall descriptions). Replicate this element for each type of possible physics or engineering configurations necessary (gas tight vs wall with ports and holes, coarse vs fine representation, ...). Time-dependent
	- wall_types : class wall_typesstruct_arraywall_types: array of wall_typesstruct_arraywall_typesObj objects
	   List of reference wall types (e.g. bulk tungsten, tungsten-coated CFC, ...) ; Array of structures (number of reference wall types)
	- compounds : class compoundsstruct_arraycompound_desc: array of compoundsstruct_arraycompound_descObj objects
	   Chemical compounds (e.g. solid tungsten, WC, CFC, ...) possibly present in the wall. Array of structure (number of compounds)
	- elements : class elementsstruct_arrayelement_desc: array of elementsstruct_arrayelement_descObj objects
	   Chemical elements present in the wall units, including elements from the plasma (gas + impurities). Use by compounds. Array of structures (number of elements)
	- compositions : class compositionsstructurecompositions_type
	   
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar.
	'''

	def __init__(self):
		self.base_path = 'wall'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 5
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.wall0d = wall0dstructurewall_wall0d('wall0d')
		self.wall2d_mhd = wall2d_mhdstructurewall2d_mhd('wall2d_mhd')
		self.wall2d = wall2dstruct_arraywall2d('wall2d')
		self.wall3d = wall3dstruct_arraywall3d('wall3d')
		self.wall_types = wall_typesstruct_arraywall_types('wall_types')
		self.compounds = compoundsstruct_arraycompound_desc('compounds')
		self.elements = elementsstruct_arrayelement_desc('elements')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute wall0d\n ' + self.wall0d.__str__(depth+1)
		ret = ret + space + 'Attribute wall2d_mhd\n ' + self.wall2d_mhd.__str__(depth+1)
		ret = ret + space + 'Attribute wall2d\n ' + self.wall2d.__str__(depth+1)
		ret = ret + space + 'Attribute wall3d\n ' + self.wall3d.__str__(depth+1)
		ret = ret + space + 'Attribute wall_types\n ' + self.wall_types.__str__(depth+1)
		ret = ret + space + 'Attribute compounds\n ' + self.compounds.__str__(depth+1)
		ret = ret + space + 'Attribute elements\n ' + self.elements.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.wall0d.setExpIdx(idx)
		self.wall2d_mhd.setExpIdx(idx)
		self.wall2d.setExpIdx(idx)
		self.wall3d.setExpIdx(idx)
		self.wall_types.setExpIdx(idx)
		self.compounds.setExpIdx(idx)
		self.elements.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
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
		self.wall0d.cpoTime = self.cpoTime
		self.wall0d.putSlice(path, cpopath)
		self.wall2d_mhd.cpoTime = self.cpoTime
		self.wall2d_mhd.putSlice(path, cpopath)
		self.wall2d.cpoTime = self.cpoTime
		self.wall2d.putSlice(path, cpopath)
		self.wall3d.cpoTime = self.cpoTime
		self.wall3d.putSlice(path, cpopath)
		self.wall_types.cpoTime = self.cpoTime
		self.wall_types.putSlice(path, cpopath)
		self.compounds.cpoTime = self.cpoTime
		self.compounds.putSlice(path, cpopath)
		self.elements.cpoTime = self.cpoTime
		self.elements.putSlice(path, cpopath)
		self.compositions.cpoTime = self.cpoTime
		self.compositions.putSlice(path, cpopath)
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
		self.wall0d.replaceLastSlice(path, cpopath)
		self.wall2d_mhd.replaceLastSlice(path, cpopath)
		self.wall2d.replaceLastSlice(path, cpopath)
		self.wall3d.replaceLastSlice(path, cpopath)
		self.wall_types.replaceLastSlice(path, cpopath)
		self.compounds.replaceLastSlice(path, cpopath)
		self.elements.replaceLastSlice(path, cpopath)
		self.compositions.replaceLastSlice(path, cpopath)
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
		self.wall0d.putNonTimed(path, cpopath)
		self.wall2d_mhd.putNonTimed(path, cpopath)
		self.wall2d.putNonTimed(path, cpopath)
		self.wall3d.putNonTimed(path, cpopath)
		self.wall_types.putNonTimed(path, cpopath)
		self.compounds.putNonTimed(path, cpopath)
		self.elements.putNonTimed(path, cpopath)
		self.compositions.putNonTimed(path, cpopath)
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
		self.wall0d.getSlice(path, cpopath, inTime, interpolMode)
		self.wall2d_mhd.getSlice(path, cpopath, inTime, interpolMode)
		self.wall2d.getSlice(path, cpopath, inTime, interpolMode)
		self.wall3d.getSlice(path, cpopath, inTime, interpolMode)
		self.wall_types.getSlice(path, cpopath, inTime, interpolMode)
		self.compounds.getSlice(path, cpopath, inTime, interpolMode)
		self.elements.getSlice(path, cpopath, inTime, interpolMode)
		self.compositions.getSlice(path, cpopath, inTime, interpolMode)
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
			wall0dList = self.wall0d.build_non_resampled_data(path, cpopath, nbslice)
			wall2d_mhdList = self.wall2d_mhd.build_non_resampled_data(path, cpopath, nbslice)
			wall2dList = self.wall2d.build_non_resampled_data(path, cpopath, nbslice)
			wall3dList = self.wall3d.build_non_resampled_data(path, cpopath, nbslice)
			wall_typesList = self.wall_types.build_non_resampled_data(path, cpopath, nbslice)
			compoundsList = self.compounds.build_non_resampled_data(path, cpopath, nbslice)
			elementsList = self.elements.build_non_resampled_data(path, cpopath, nbslice)
			compositionsList = self.compositions.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = wall()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.wall0d = wall0dList[i]
				slice.wall2d_mhd = wall2d_mhdList[i]
				slice.wall2d = wall2dList[i]
				slice.wall3d = wall3dList[i]
				slice.wall_types = wall_typesList[i]
				slice.compounds = compoundsList[i]
				slice.elements = elementsList[i]
				slice.compositions = compositionsList[i]
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
		self.wall0d.deleteData(path, cpopath)
		self.wall2d_mhd.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'wall2d')
		ull.deleteData(self.idx, path, cpopath + 'wall3d')
		ull.deleteData(self.idx, path, cpopath + 'wall_types')
		ull.deleteData(self.idx, path, cpopath + 'compounds')
		ull.deleteData(self.idx, path, cpopath + 'elements')
		self.compositions.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class wallArray:
	'''
	class wallArray
	General Wall representation. Time-dependent CPO.

	Attributes:
	- array : list of wall
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
		ret = space + 'class wallArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'wall cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = wall()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(wall())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = wall()
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


class wall0dstructurewall_wall0d:
	'''
	class wall0dstructurewall_wall0d
	Simple 0D description of plasma-wall interaction

	Attributes:
	- pumping_speed : numpy.ndarray 1D with float
	   pumping speed; Time-dependent. vector(nneut); [particles/s]
	- gas_puff : numpy.ndarray 1D with float
	   gas puff; vector(nneut); Time-dependent.  [particles/s]
	- wall_inventory : numpy.ndarray 1D with float
	   wall inventory; vector(nneut); Time-dependent. [particles]
	- recycling_coefficient : numpy.ndarray 1D with float
	   Recycling coefficient. Vector(nneut) Time-dependent.
	- wall_temperature : float
	   Wall temperature [K]. Time-dependent. Scalar
	- power_from_plasma : float
	   Power flowing from the plasma to the wall [W]. Time-dependent. Scalar
	- power_to_cooling : float
	   Power to cooling systems [W]. Time-dependent. Scalar
	- plasma : class plasmastructurewall_wall0d_plasma
	   
	'''

	def __init__(self, base_path_in='wall0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.pumping_speed = numpy.zeros(0, numpy.float64, order='C')
		self.gas_puff = numpy.zeros(0, numpy.float64, order='C')
		self.wall_inventory = numpy.zeros(0, numpy.float64, order='C')
		self.recycling_coefficient = numpy.zeros(0, numpy.float64, order='C')
		self.wall_temperature = EMPTY_DOUBLE
		self.power_from_plasma = EMPTY_DOUBLE
		self.power_to_cooling = EMPTY_DOUBLE
		self.plasma = plasmastructurewall_wall0d_plasma('plasma')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall0dstructurewall_wall0d\n'
		s = self.pumping_speed.__str__()
		ret = ret + space + 'Attribute pumping_speed\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gas_puff.__str__()
		ret = ret + space + 'Attribute gas_puff\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.wall_inventory.__str__()
		ret = ret + space + 'Attribute wall_inventory\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.recycling_coefficient.__str__()
		ret = ret + space + 'Attribute recycling_coefficient\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute wall_temperature: ' + str(self.wall_temperature) + '\n'
		ret = ret + space + 'Attribute power_from_plasma: ' + str(self.power_from_plasma) + '\n'
		ret = ret + space + 'Attribute power_to_cooling: ' + str(self.power_to_cooling) + '\n'
		ret = ret + space + 'Attribute plasma\n ' + self.plasma.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.plasma.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall0dstructurewall_wall0d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pumping_speed', numpy.array(self.pumping_speed).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gas_puff', numpy.array(self.gas_puff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'wall_inventory', numpy.array(self.wall_inventory).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'recycling_coefficient', numpy.array(self.recycling_coefficient).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'wall_temperature', self.wall_temperature, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'power_from_plasma', self.power_from_plasma, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'power_to_cooling', self.power_to_cooling, self.cpoTime)
		check_status(status)
		self.plasma.cpoTime = self.cpoTime
		self.plasma.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall0dstructurewall_wall0d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pumping_speed', numpy.array(self.pumping_speed).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gas_puff', numpy.array(self.gas_puff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'wall_inventory', numpy.array(self.wall_inventory).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'recycling_coefficient', numpy.array(self.recycling_coefficient).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'wall_temperature', self.wall_temperature)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'power_from_plasma', self.power_from_plasma)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'power_to_cooling', self.power_to_cooling)
		check_status(status)
		self.plasma.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall0dstructurewall_wall0d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.plasma.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type wall0dstructurewall_wall0d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_pumping_speed, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pumping_speed', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pumping_speed = ret_pumping_speed
			self.cpoTime = retTime
		status, ret_gas_puff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gas_puff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gas_puff = ret_gas_puff
			self.cpoTime = retTime
		status, ret_wall_inventory, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'wall_inventory', inTime, interpolMode)
		check_status(status)
		if not status:
			self.wall_inventory = ret_wall_inventory
			self.cpoTime = retTime
		status, ret_recycling_coefficient, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'recycling_coefficient', inTime, interpolMode)
		check_status(status)
		if not status:
			self.recycling_coefficient = ret_recycling_coefficient
			self.cpoTime = retTime
		status, ret_wall_temperature, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'wall_temperature', inTime, interpolMode)
		check_status(status)
		if not status:
			self.wall_temperature = ret_wall_temperature
			self.cpoTime = retTime
		status, ret_power_from_plasma, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'power_from_plasma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_from_plasma = ret_power_from_plasma
			self.cpoTime = retTime
		status, ret_power_to_cooling, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'power_to_cooling', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_to_cooling = ret_power_to_cooling
			self.cpoTime = retTime
		self.plasma.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type wall0dstructurewall_wall0d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, pumping_speedList = ull.getVect2DDouble(self.idx, path, cpopath + 'pumping_speed')
			if len(pumping_speedList) == 0:
				pumping_speedList = numpy.resize(pumping_speedList, (0,nbslice))
			check_status(status)
			status, gas_puffList = ull.getVect2DDouble(self.idx, path, cpopath + 'gas_puff')
			if len(gas_puffList) == 0:
				gas_puffList = numpy.resize(gas_puffList, (0,nbslice))
			check_status(status)
			status, wall_inventoryList = ull.getVect2DDouble(self.idx, path, cpopath + 'wall_inventory')
			if len(wall_inventoryList) == 0:
				wall_inventoryList = numpy.resize(wall_inventoryList, (0,nbslice))
			check_status(status)
			status, recycling_coefficientList = ull.getVect2DDouble(self.idx, path, cpopath + 'recycling_coefficient')
			if len(recycling_coefficientList) == 0:
				recycling_coefficientList = numpy.resize(recycling_coefficientList, (0,nbslice))
			check_status(status)
			status, wall_temperatureList = ull.getVect1DDouble(self.idx, path, cpopath + 'wall_temperature')
			if len(wall_temperatureList) == 0:
				wall_temperatureList = numpy.resize(wall_temperatureList, (nbslice))
			check_status(status)
			status, power_from_plasmaList = ull.getVect1DDouble(self.idx, path, cpopath + 'power_from_plasma')
			if len(power_from_plasmaList) == 0:
				power_from_plasmaList = numpy.resize(power_from_plasmaList, (nbslice))
			check_status(status)
			status, power_to_coolingList = ull.getVect1DDouble(self.idx, path, cpopath + 'power_to_cooling')
			if len(power_to_coolingList) == 0:
				power_to_coolingList = numpy.resize(power_to_coolingList, (nbslice))
			check_status(status)
			plasmaList = self.plasma.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = wall0dstructurewall_wall0d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.pumping_speed = pumping_speedList[:,i]
				slice.gas_puff = gas_puffList[:,i]
				slice.wall_inventory = wall_inventoryList[:,i]
				slice.recycling_coefficient = recycling_coefficientList[:,i]
				slice.wall_temperature = wall_temperatureList[i].copy().astype(float)
				slice.power_from_plasma = power_from_plasmaList[i].copy().astype(float)
				slice.power_to_cooling = power_to_coolingList[i].copy().astype(float)
				slice.plasma = plasmaList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall0dstructurewall_wall0dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pumping_speed') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pumping_speed', i, numpy.array(self.pumping_speed).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gas_puff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gas_puff', i, numpy.array(self.gas_puff).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'wall_inventory') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'wall_inventory', i, numpy.array(self.wall_inventory).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'recycling_coefficient') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'recycling_coefficient', i, numpy.array(self.recycling_coefficient).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'wall_temperature') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'wall_temperature', i, self.wall_temperature)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_from_plasma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_from_plasma', i, self.power_from_plasma)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_to_cooling') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_to_cooling', i, self.power_to_cooling)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall0dstructurewall_wall0dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pumping_speed') 
			print ('obj = ' + str(obj))
		status, ret_pumping_speed = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pumping_speed', i)
		check_status(status)
		if not status:
			self.pumping_speed = ret_pumping_speed
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gas_puff') 
			print ('obj = ' + str(obj))
		status, ret_gas_puff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gas_puff', i)
		check_status(status)
		if not status:
			self.gas_puff = ret_gas_puff
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'wall_inventory') 
			print ('obj = ' + str(obj))
		status, ret_wall_inventory = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'wall_inventory', i)
		check_status(status)
		if not status:
			self.wall_inventory = ret_wall_inventory
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'recycling_coefficient') 
			print ('obj = ' + str(obj))
		status, ret_recycling_coefficient = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'recycling_coefficient', i)
		check_status(status)
		if not status:
			self.recycling_coefficient = ret_recycling_coefficient
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'wall_temperature') 
			print ('obj = ' + str(obj))
		status, ret_wall_temperature = ull.getDoubleFromObject(self.idx, obj, cpopath + 'wall_temperature', i)
		check_status(status)
		if not status:
			self.wall_temperature = ret_wall_temperature
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_from_plasma') 
			print ('obj = ' + str(obj))
		status, ret_power_from_plasma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_from_plasma', i)
		check_status(status)
		if not status:
			self.power_from_plasma = ret_power_from_plasma
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_to_cooling') 
			print ('obj = ' + str(obj))
		status, ret_power_to_cooling = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_to_cooling', i)
		check_status(status)
		if not status:
			self.power_to_cooling = ret_power_to_cooling

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall0dstructurewall_wall0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.plasma.putNonTimedElt(path, cpopath + 'plasma', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall0dstructurewall_wall0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.plasma.getNonTimedElt(path, cpopath + 'plasma', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'pumping_speed')
		ull.deleteData(self.idx, path, cpopath + 'gas_puff')
		ull.deleteData(self.idx, path, cpopath + 'wall_inventory')
		ull.deleteData(self.idx, path, cpopath + 'recycling_coefficient')
		ull.deleteData(self.idx, path, cpopath + 'wall_temperature')
		ull.deleteData(self.idx, path, cpopath + 'power_from_plasma')
		ull.deleteData(self.idx, path, cpopath + 'power_to_cooling')
		self.plasma.deleteData(path, cpopath)


class plasmastructurewall_wall0d_plasma:
	'''
	class plasmastructurewall_wall0d_plasma
	

	Attributes:
	- species_index : numpy.ndarray 2D with int
	   Index of species into wall/compositions;  matrix(nspecies,3); 1st element indicates {1: main ions; 2:impurities; 3:neutrals; 4:edge species); 2nd element indicates index into that array; 3rd index indicates charge state if 1st element points to inpurities or neutral type if 1st element points to neutrals;
	- flux : numpy.ndarray 1D with float
	   flux of species indicated by species_index; array of nspecies; positive implies incoming onto wall; negative implies sent back into plasma; time-dependent; [particles/s]
	- energy : numpy.ndarray 1D with float
	   energy flux of species indicated by species_index; array of nspecies; positive implies incoming onto wall; negative implies sent back into plasma; 
time-dependent; [W]
	'''

	def __init__(self, base_path_in='plasma'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.species_index = numpy.zeros((0,0), numpy.int32, order='C')
		self.flux = numpy.zeros(0, numpy.float64, order='C')
		self.energy = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class plasmastructurewall_wall0d_plasma\n'
		s = self.species_index.__str__()
		ret = ret + space + 'Attribute species_index\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.energy.__str__()
		ret = ret + space + 'Attribute energy\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructurewall_wall0d_plasma, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructurewall_wall0d_plasma, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructurewall_wall0d_plasma, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DInt(self.idx, path, cpopath + 'species_index', numpy.array(self.species_index).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'flux', numpy.array(self.flux).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'energy', numpy.array(self.energy).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructurewall_wall0d_plasma, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_species_index = ull.getVect2DInt(self.idx, path, cpopath + 'species_index')
		check_status(status)
		if not status:
			self.species_index = ret_species_index
		status, ret_flux = ull.getVect1DDouble(self.idx, path, cpopath + 'flux')
		check_status(status)
		if not status:
			self.flux = ret_flux
		status, ret_energy = ull.getVect1DDouble(self.idx, path, cpopath + 'energy')
		check_status(status)
		if not status:
			self.energy = ret_energy

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastructurewall_wall0d_plasma, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, species_indexVal = ull.getVect2DInt(self.idx, path, cpopath + 'species_index')
			check_status(status)
			status, fluxVal = ull.getVect1DDouble(self.idx, path, cpopath + 'flux')
			check_status(status)
			status, energyVal = ull.getVect1DDouble(self.idx, path, cpopath + 'energy')
			check_status(status)
			for i in range(nbslice):
				slice = plasmastructurewall_wall0d_plasma(self.base_path)
				slice.setExpIdx(self.idx)
				slice.species_index = species_indexVal
				slice.flux = fluxVal
				slice.energy = energyVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructurewall_wall0d_plasmaObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructurewall_wall0d_plasmaObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructurewall_wall0d_plasmaObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'species_index') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'species_index', i, numpy.array(self.species_index).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'energy', i, numpy.array(self.energy).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastructurewall_wall0d_plasmaObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'species_index') 
			print ('obj = ' + str(obj))
		status, ret_species_index = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'species_index', i)
		check_status(status)
		if not status:
			self.species_index = ret_species_index
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'species_index')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		ull.deleteData(self.idx, path, cpopath + 'energy')


class wall2d_mhdstructurewall2d_mhd:
	'''
	class wall2d_mhdstructurewall2d_mhd
	Simplified wall that encloses necessary information for RWM codes.

	Attributes:
	- res_wall : class res_wallstruct_arraymhd_res_wall2d: array of res_wallstruct_arraymhd_res_wall2dObj objects
	   Resistive Wall(s).
	- ideal_wall : class ideal_wallstructuremhd_ideal_wall2d
	   Ideal wall
	'''

	def __init__(self, base_path_in='wall2d_mhd'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.res_wall = res_wallstruct_arraymhd_res_wall2d('res_wall')
		self.ideal_wall = ideal_wallstructuremhd_ideal_wall2d('ideal_wall')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall2d_mhdstructurewall2d_mhd\n'
		ret = ret + space + 'Attribute res_wall\n ' + self.res_wall.__str__(depth+1)
		ret = ret + space + 'Attribute ideal_wall\n ' + self.ideal_wall.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.res_wall.setExpIdx(idx)
		self.ideal_wall.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall2d_mhdstructurewall2d_mhd, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.res_wall.cpoTime = self.cpoTime
		self.res_wall.putSlice(path, cpopath)
		self.ideal_wall.cpoTime = self.cpoTime
		self.ideal_wall.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall2d_mhdstructurewall2d_mhd, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.res_wall.replaceLastSlice(path, cpopath)
		self.ideal_wall.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall2d_mhdstructurewall2d_mhd, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.res_wall.putNonTimed(path, cpopath)
		self.ideal_wall.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type wall2d_mhdstructurewall2d_mhd, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.res_wall.getSlice(path, cpopath, inTime, interpolMode)
		self.ideal_wall.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type wall2d_mhdstructurewall2d_mhd, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			res_wallList = self.res_wall.build_non_resampled_data(path, cpopath, nbslice)
			ideal_wallList = self.ideal_wall.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = wall2d_mhdstructurewall2d_mhd(self.base_path)
				slice.setExpIdx(self.idx)
				slice.res_wall = res_wallList[i]
				slice.ideal_wall = ideal_wallList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2d_mhdstructurewall2d_mhdObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2d_mhdstructurewall2d_mhdObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2d_mhdstructurewall2d_mhdObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.res_wall.putNonTimedElt(path, cpopath + 'res_wall', i, obj)
		obj = self.ideal_wall.putNonTimedElt(path, cpopath + 'ideal_wall', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2d_mhdstructurewall2d_mhdObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.res_wall.getNonTimedElt(path, cpopath + 'res_wall', i, obj)
		self.ideal_wall.getNonTimedElt(path, cpopath + 'ideal_wall', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'res_wall')
		self.ideal_wall.deleteData(path, cpopath)


class res_wallstruct_arraymhd_res_wall2d:
	'''
	class res_wallstruct_arraymhd_res_wall2d
	Resistive Wall(s).

	Attributes:
	- array : list of res_wallstruct_arraymhd_res_wall2dObj 
	'''

	def __init__(self, base_path_in='res_wall'):
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
		ret = space + 'class res_wallstruct_arraymhd_res_wall2d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'res_wallstruct_arraymhd_res_wall2d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(res_wallstruct_arraymhd_res_wall2dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function getSlice') 
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
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(res_wallstruct_arraymhd_res_wall2d(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = res_wallstruct_arraymhd_res_wall2d(self.base_path)
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
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type res_wallstruct_arraymhd_res_wall2d, run function getNonTimedElt') 
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


class res_wallstruct_arraymhd_res_wall2dObj:
	'''
	class res_wallstruct_arraymhd_res_wall2dObj
	Resistive Wall(s).

	Attributes:
	- walltype : class walltypestructureidentifier
	   Tag the type of wall to be used, 0 (conformal) or 1 (free)
	- delta : float
	   Wall thickness [m]; Scalar
	- eta : float
	   Wall resistivity [ohm.m]; Scalar
	- npoloidal : int
	   Number of poloidal coordinates for each wall (dimension of R and Z);
	- position : class positionstructurerz1D
	   RZ description of the wall; wall coordinates are defined at a middle line (line passing through the middle of the real wall as defined by thickness parameter delta)
	- holes : class holesstructureholes
	   Structure to describe the placing and properties of the holes
	'''

	def __init__(self, base_path_in='res_wall'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.walltype = walltypestructureidentifier('walltype')
		self.delta = EMPTY_DOUBLE
		self.eta = EMPTY_DOUBLE
		self.npoloidal = EMPTY_INT
		self.position = positionstructurerz1D('position')
		self.holes = holesstructureholes('holes')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class res_wallstruct_arraymhd_res_wall2dObj\n'
		ret = ret + space + 'Attribute walltype\n ' + self.walltype.__str__(depth+1)
		ret = ret + space + 'Attribute delta: ' + str(self.delta) + '\n'
		ret = ret + space + 'Attribute eta: ' + str(self.eta) + '\n'
		ret = ret + space + 'Attribute npoloidal: ' + str(self.npoloidal) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute holes\n ' + self.holes.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.walltype.setExpIdx(idx)
		self.position.setExpIdx(idx)
		self.holes.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type res_wallstruct_arraymhd_res_wall2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.walltype.putNonTimedElt(path, cpopath + 'walltype', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'delta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'delta', i, self.delta)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'eta', i, self.eta)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'npoloidal') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'npoloidal', i, self.npoloidal)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		obj = self.holes.putNonTimedElt(path, cpopath + 'holes', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type res_wallstruct_arraymhd_res_wall2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.walltype.getNonTimedElt(path, cpopath + 'walltype', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'delta') 
			print ('obj = ' + str(obj))
		status, ret_delta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'delta', i)
		check_status(status)
		if not status:
			self.delta = ret_delta
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		status, ret_eta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'eta', i)
		check_status(status)
		if not status:
			self.eta = ret_eta
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'npoloidal') 
			print ('obj = ' + str(obj))
		status, ret_npoloidal = ull.getIntFromObject(self.idx, obj, cpopath + 'npoloidal', i)
		check_status(status)
		if not status:
			self.npoloidal = ret_npoloidal
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		self.holes.getNonTimedElt(path, cpopath + 'holes', i, obj)


class walltypestructureidentifier:
	'''
	class walltypestructureidentifier
	Tag the type of wall to be used, 0 (conformal) or 1 (free)

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='walltype'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class walltypestructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type walltypestructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type walltypestructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type walltypestructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type walltypestructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type walltypestructureidentifier, run function build_non_resampled_data') 
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
				slice = walltypestructureidentifier(self.base_path)
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
			print ('object of type walltypestructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type walltypestructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type walltypestructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type walltypestructureidentifierObj, run function getNonTimedElt') 
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


class positionstructurerz1D:
	'''
	class positionstructurerz1D
	RZ description of the wall; wall coordinates are defined at a middle line (line passing through the middle of the real wall as defined by thickness parameter delta)

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


class holesstructureholes:
	'''
	class holesstructureholes
	Structure to describe the placing and properties of the holes

	Attributes:
	- n_holes : int
	   Number of holes on each wall;
	- coordinates : class coordinatesstructurecoordinates
	   Poloidal and Toroidal coordinates of the center of each hole;
	- width : class widthstructurewidth
	   Angular width of each in the poloidal and toroidal direction;
	- eta : numpy.ndarray 1D with float
	   Resistivity  of each hole [ohm.m]; Vector (n_holes)
	'''

	def __init__(self, base_path_in='holes'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.n_holes = EMPTY_INT
		self.coordinates = coordinatesstructurecoordinates('coordinates')
		self.width = widthstructurewidth('width')
		self.eta = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class holesstructureholes\n'
		ret = ret + space + 'Attribute n_holes: ' + str(self.n_holes) + '\n'
		ret = ret + space + 'Attribute coordinates\n ' + self.coordinates.__str__(depth+1)
		ret = ret + space + 'Attribute width\n ' + self.width.__str__(depth+1)
		s = self.eta.__str__()
		ret = ret + space + 'Attribute eta\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.coordinates.setExpIdx(idx)
		self.width.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type holesstructureholes, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.cpoTime = self.cpoTime
		self.coordinates.putSlice(path, cpopath)
		self.width.cpoTime = self.cpoTime
		self.width.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type holesstructureholes, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.coordinates.replaceLastSlice(path, cpopath)
		self.width.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type holesstructureholes, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'n_holes', self.n_holes)
		check_status(status)
		self.coordinates.putNonTimed(path, cpopath)
		self.width.putNonTimed(path, cpopath)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'eta', numpy.array(self.eta).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type holesstructureholes, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_n_holes = ull.getInt(self.idx, path, cpopath + 'n_holes')
		check_status(status)
		if not status:
			self.n_holes = ret_n_holes
		self.coordinates.getSlice(path, cpopath, inTime, interpolMode)
		self.width.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_eta = ull.getVect1DDouble(self.idx, path, cpopath + 'eta')
		check_status(status)
		if not status:
			self.eta = ret_eta

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type holesstructureholes, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, n_holesVal = ull.getInt(self.idx, path, cpopath + 'n_holes')
			check_status(status)
			coordinatesList = self.coordinates.build_non_resampled_data(path, cpopath, nbslice)
			widthList = self.width.build_non_resampled_data(path, cpopath, nbslice)
			status, etaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'eta')
			check_status(status)
			for i in range(nbslice):
				slice = holesstructureholes(self.base_path)
				slice.setExpIdx(self.idx)
				slice.n_holes = n_holesVal
				slice.coordinates = coordinatesList[i]
				slice.width = widthList[i]
				slice.eta = etaVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type holesstructureholesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type holesstructureholesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type holesstructureholesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'n_holes') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'n_holes', i, self.n_holes)
		obj = self.coordinates.putNonTimedElt(path, cpopath + 'coordinates', i, obj)
		obj = self.width.putNonTimedElt(path, cpopath + 'width', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'eta', i, numpy.array(self.eta).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type holesstructureholesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'n_holes') 
			print ('obj = ' + str(obj))
		status, ret_n_holes = ull.getIntFromObject(self.idx, obj, cpopath + 'n_holes', i)
		check_status(status)
		if not status:
			self.n_holes = ret_n_holes
		self.coordinates.getNonTimedElt(path, cpopath + 'coordinates', i, obj)
		self.width.getNonTimedElt(path, cpopath + 'width', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		status, ret_eta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'eta', i)
		check_status(status)
		if not status:
			self.eta = ret_eta

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'n_holes')
		self.coordinates.deleteData(path, cpopath)
		self.width.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'eta')


class coordinatesstructurecoordinates:
	'''
	class coordinatesstructurecoordinates
	Poloidal and Toroidal coordinates of the center of each hole;

	Attributes:
	- theta : numpy.ndarray 1D with float
	   Theta coordinate of holes center; Vector (n_holes)
	- phi : numpy.ndarray 1D with float
	   Toroidal coordinate of holes center; Vector (n_holes)
	'''

	def __init__(self, base_path_in='coordinates'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.theta = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coordinatesstructurecoordinates\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurecoordinates, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurecoordinates, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurecoordinates, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurecoordinates, run function getSlice') 
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
		status, ret_phi = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
		check_status(status)
		if not status:
			self.phi = ret_phi

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type coordinatesstructurecoordinates, run function build_non_resampled_data') 
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
			status, phiVal = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
			check_status(status)
			for i in range(nbslice):
				slice = coordinatesstructurecoordinates(self.base_path)
				slice.setExpIdx(self.idx)
				slice.theta = thetaVal
				slice.phi = phiVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurecoordinatesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurecoordinatesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurecoordinatesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coordinatesstructurecoordinatesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		status, ret_theta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta', i)
		check_status(status)
		if not status:
			self.theta = ret_theta
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
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
		ull.deleteData(self.idx, path, cpopath + 'theta')
		ull.deleteData(self.idx, path, cpopath + 'phi')


class widthstructurewidth:
	'''
	class widthstructurewidth
	Angular width of each in the poloidal and toroidal direction;

	Attributes:
	- dtheta : numpy.ndarray 1D with float
	   Angular poloidal width of holes; Vector (n_holes)
	- phi : numpy.ndarray 1D with float
	   Angular toroidal width of holes; Vector (n_holes)
	'''

	def __init__(self, base_path_in='width'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dtheta = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class widthstructurewidth\n'
		s = self.dtheta.__str__()
		ret = ret + space + 'Attribute dtheta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type widthstructurewidth, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type widthstructurewidth, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type widthstructurewidth, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dtheta', numpy.array(self.dtheta).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type widthstructurewidth, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dtheta = ull.getVect1DDouble(self.idx, path, cpopath + 'dtheta')
		check_status(status)
		if not status:
			self.dtheta = ret_dtheta
		status, ret_phi = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
		check_status(status)
		if not status:
			self.phi = ret_phi

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type widthstructurewidth, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dthetaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'dtheta')
			check_status(status)
			status, phiVal = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
			check_status(status)
			for i in range(nbslice):
				slice = widthstructurewidth(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dtheta = dthetaVal
				slice.phi = phiVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type widthstructurewidthObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type widthstructurewidthObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type widthstructurewidthObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dtheta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dtheta', i, numpy.array(self.dtheta).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type widthstructurewidthObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dtheta') 
			print ('obj = ' + str(obj))
		status, ret_dtheta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dtheta', i)
		check_status(status)
		if not status:
			self.dtheta = ret_dtheta
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
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
		ull.deleteData(self.idx, path, cpopath + 'dtheta')
		ull.deleteData(self.idx, path, cpopath + 'phi')


class ideal_wallstructuremhd_ideal_wall2d:
	'''
	class ideal_wallstructuremhd_ideal_wall2d
	Ideal wall

	Attributes:
	- walltype : class walltypestructureidentifier
	   Tag the type of wall to be used, 0 (conformal) or 1 (free)
	- position : class positionstructurerz1D
	   RZ description of the wall;
	'''

	def __init__(self, base_path_in='ideal_wall'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.walltype = walltypestructureidentifier('walltype')
		self.position = positionstructurerz1D('position')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ideal_wallstructuremhd_ideal_wall2d\n'
		ret = ret + space + 'Attribute walltype\n ' + self.walltype.__str__(depth+1)
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.walltype.setExpIdx(idx)
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ideal_wallstructuremhd_ideal_wall2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.walltype.cpoTime = self.cpoTime
		self.walltype.putSlice(path, cpopath)
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ideal_wallstructuremhd_ideal_wall2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.walltype.replaceLastSlice(path, cpopath)
		self.position.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ideal_wallstructuremhd_ideal_wall2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.walltype.putNonTimed(path, cpopath)
		self.position.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type ideal_wallstructuremhd_ideal_wall2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.walltype.getSlice(path, cpopath, inTime, interpolMode)
		self.position.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type ideal_wallstructuremhd_ideal_wall2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			walltypeList = self.walltype.build_non_resampled_data(path, cpopath, nbslice)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = ideal_wallstructuremhd_ideal_wall2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.walltype = walltypeList[i]
				slice.position = positionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ideal_wallstructuremhd_ideal_wall2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ideal_wallstructuremhd_ideal_wall2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ideal_wallstructuremhd_ideal_wall2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.walltype.putNonTimedElt(path, cpopath + 'walltype', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ideal_wallstructuremhd_ideal_wall2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.walltype.getNonTimedElt(path, cpopath + 'walltype', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.walltype.deleteData(path, cpopath)
		self.position.deleteData(path, cpopath)


class wall2dstruct_arraywall2d:
	'''
	class wall2dstruct_arraywall2d
	2D wall descriptions; Array of structures (number of wall descriptions). Replicate this element for each type of possible physics or engineering configurations necessary (gas tight vs wall with ports and holes, coarse vs fine representation, single contour limiter, disjoint gapped plasma facing components, ...). Time-dependent

	Attributes:
	- array : list of wall2dstruct_arraywall2dObj 
	'''

	def __init__(self, base_path_in='wall2d'):
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
		ret = space + 'class wall2dstruct_arraywall2d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'wall2dstruct_arraywall2d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(wall2dstruct_arraywall2dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function putSlice') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function getSlice') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(wall2dstruct_arraywall2d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(wall2dstruct_arraywall2d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = wall2dstruct_arraywall2d(self.base_path)
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type wall2dstruct_arraywall2d, run function getNonTimedElt') 
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


class wall2dstruct_arraywall2dObj:
	'''
	class wall2dstruct_arraywall2dObj
	2D wall descriptions; Array of structures (number of wall descriptions). Replicate this element for each type of possible physics or engineering configurations necessary (gas tight vs wall with ports and holes, coarse vs fine representation, single contour limiter, disjoint gapped plasma facing components, ...). Time-dependent

	Attributes:
	- wall_id : class wall_idstructureidentifier
	   Use this identifier to tag the type of 2d wall you are using. Use 0 for equilibrium codes (single closed limiter and vessel); 1 for gas-tight walls (disjoint PFCs with inner vessel as last limiter_unit; no vessel structure); 2 for free boundary codes (disjoint PFCs and vessel) 
	- limiter : class limiterstructurewall_limiter
	   Description of the immobile limiting surface(s) or plasma facing components for defining the Last Closed Flux Surface. Two representations are admitted : single contour or disjoint PFC. The limiter_id identifies the type of limiter set and code-specific representations derived from the official ones are also allowed if documented. Array of structures (nlimiter_type). Time-dependent
	- vessel : class vesselstructurewall_vessel
	   Mechanical structure of the vacuum vessel. Vessel assumed as set of nested layers with given physics properties; Two representations are admitted for each vessel unit : annular (two contours) or blocks. The vessel_id identifies the type of vessel_unit set one is using and code-specific representations derived from the official ones are also allowed if documented. Array of structures (nvessel_type)
	- plasma : class plasmastruct_arrayplasmaComplexType: array of plasmastruct_arrayplasmaComplexTypeObj objects
	   Description of incoming plasma for every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the limiter unit with index i in wall/wall2d/limiter/limiter_unit. Time-dependent
	- wall_state : class wall_statestruct_arraywall_unitsComplexType: array of wall_statestruct_arraywall_unitsComplexTypeObj objects
	   Dynamic wall state of every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the limiter unit with index i in wall/wall2d/limiter/limiter_unit. Time-dependent
	'''

	def __init__(self, base_path_in='wall2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.wall_id = wall_idstructureidentifier('wall_id')
		self.limiter = limiterstructurewall_limiter('limiter')
		self.vessel = vesselstructurewall_vessel('vessel')
		self.plasma = plasmastruct_arrayplasmaComplexType('plasma')
		self.wall_state = wall_statestruct_arraywall_unitsComplexType('wall_state')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall2dstruct_arraywall2dObj\n'
		ret = ret + space + 'Attribute wall_id\n ' + self.wall_id.__str__(depth+1)
		ret = ret + space + 'Attribute limiter\n ' + self.limiter.__str__(depth+1)
		ret = ret + space + 'Attribute vessel\n ' + self.vessel.__str__(depth+1)
		ret = ret + space + 'Attribute plasma\n ' + self.plasma.__str__(depth+1)
		ret = ret + space + 'Attribute wall_state\n ' + self.wall_state.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.wall_id.setExpIdx(idx)
		self.limiter.setExpIdx(idx)
		self.vessel.setExpIdx(idx)
		self.plasma.setExpIdx(idx)
		self.wall_state.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2dstruct_arraywall2dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.limiter.putTimedElt(path, cpopath + 'limiter', i, obj)
		obj = self.plasma.putTimedElt(path, cpopath + 'plasma', i, obj)
		obj = self.wall_state.putTimedElt(path, cpopath + 'wall_state', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2dstruct_arraywall2dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.limiter.getTimedElt(path, cpopath + 'limiter', i, obj)
		self.plasma.getTimedElt(path, cpopath + 'plasma', i, obj)
		self.wall_state.getTimedElt(path, cpopath + 'wall_state', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2dstruct_arraywall2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.wall_id.putNonTimedElt(path, cpopath + 'wall_id', i, obj)
		obj = self.limiter.putNonTimedElt(path, cpopath + 'limiter', i, obj)
		obj = self.vessel.putNonTimedElt(path, cpopath + 'vessel', i, obj)
		obj = self.plasma.putNonTimedElt(path, cpopath + 'plasma', i, obj)
		obj = self.wall_state.putNonTimedElt(path, cpopath + 'wall_state', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall2dstruct_arraywall2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.wall_id.getNonTimedElt(path, cpopath + 'wall_id', i, obj)
		self.limiter.getNonTimedElt(path, cpopath + 'limiter', i, obj)
		self.vessel.getNonTimedElt(path, cpopath + 'vessel', i, obj)
		self.plasma.getNonTimedElt(path, cpopath + 'plasma', i, obj)
		self.wall_state.getNonTimedElt(path, cpopath + 'wall_state', i, obj)


class wall_idstructureidentifier:
	'''
	class wall_idstructureidentifier
	Use this identifier to tag the type of 2d wall you are using. Use 0 for equilibrium codes (single closed limiter and vessel); 1 for gas-tight walls (disjoint PFCs with inner vessel as last limiter_unit; no vessel structure); 2 for free boundary codes (disjoint PFCs and vessel) 

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='wall_id'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall_idstructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_idstructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_idstructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_idstructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type wall_idstructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type wall_idstructureidentifier, run function build_non_resampled_data') 
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
				slice = wall_idstructureidentifier(self.base_path)
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
			print ('object of type wall_idstructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_idstructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_idstructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type wall_idstructureidentifierObj, run function getNonTimedElt') 
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


class limiterstructurewall_limiter:
	'''
	class limiterstructurewall_limiter
	Description of the immobile limiting surface(s) or plasma facing components for defining the Last Closed Flux Surface. Two representations are admitted : single contour or disjoint PFC. The limiter_id identifies the type of limiter set and code-specific representations derived from the official ones are also allowed if documented. Array of structures (nlimiter_type). Time-dependent

	Attributes:
	- limiter_id : class limiter_idstructureidentifier
	   Use this identifier to tag the type of limiter you are using. Use flag=0 for the official single contour limiter and 1 for the official disjoint PFC structure like first wall. Additional representations needed on a code-by-code basis follow same incremental pair tagging starting on flag=2
	- limiter_unit : class limiter_unitstruct_arraylimiter_unit: array of limiter_unitstruct_arraylimiter_unitObj objects
	   Array of ncomponents limiting surfaces making up the limiter type (single contour or disjoint PFC). Replicate this limiter_unit element ncomponents times. Each unit contains a plasma facing component that can have dedicated number of points. Array of structures (ncomponents). Time-dependent
	'''

	def __init__(self, base_path_in='limiter'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.limiter_id = limiter_idstructureidentifier('limiter_id')
		self.limiter_unit = limiter_unitstruct_arraylimiter_unit('limiter_unit')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class limiterstructurewall_limiter\n'
		ret = ret + space + 'Attribute limiter_id\n ' + self.limiter_id.__str__(depth+1)
		ret = ret + space + 'Attribute limiter_unit\n ' + self.limiter_unit.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.limiter_id.setExpIdx(idx)
		self.limiter_unit.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiterstructurewall_limiter, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.limiter_id.cpoTime = self.cpoTime
		self.limiter_id.putSlice(path, cpopath)
		self.limiter_unit.cpoTime = self.cpoTime
		self.limiter_unit.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiterstructurewall_limiter, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.limiter_id.replaceLastSlice(path, cpopath)
		self.limiter_unit.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiterstructurewall_limiter, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.limiter_id.putNonTimed(path, cpopath)
		self.limiter_unit.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type limiterstructurewall_limiter, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.limiter_id.getSlice(path, cpopath, inTime, interpolMode)
		self.limiter_unit.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type limiterstructurewall_limiter, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			limiter_idList = self.limiter_id.build_non_resampled_data(path, cpopath, nbslice)
			limiter_unitList = self.limiter_unit.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = limiterstructurewall_limiter(self.base_path)
				slice.setExpIdx(self.idx)
				slice.limiter_id = limiter_idList[i]
				slice.limiter_unit = limiter_unitList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiterstructurewall_limiterObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.limiter_unit.putTimedElt(path, cpopath + 'limiter_unit', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiterstructurewall_limiterObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.limiter_unit.getTimedElt(path, cpopath + 'limiter_unit', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiterstructurewall_limiterObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.limiter_id.putNonTimedElt(path, cpopath + 'limiter_id', i, obj)
		obj = self.limiter_unit.putNonTimedElt(path, cpopath + 'limiter_unit', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiterstructurewall_limiterObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.limiter_id.getNonTimedElt(path, cpopath + 'limiter_id', i, obj)
		self.limiter_unit.getNonTimedElt(path, cpopath + 'limiter_unit', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.limiter_id.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'limiter_unit')


class limiter_idstructureidentifier:
	'''
	class limiter_idstructureidentifier
	Use this identifier to tag the type of limiter you are using. Use flag=0 for the official single contour limiter and 1 for the official disjoint PFC structure like first wall. Additional representations needed on a code-by-code basis follow same incremental pair tagging starting on flag=2

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='limiter_id'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class limiter_idstructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiter_idstructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiter_idstructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiter_idstructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type limiter_idstructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type limiter_idstructureidentifier, run function build_non_resampled_data') 
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
				slice = limiter_idstructureidentifier(self.base_path)
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
			print ('object of type limiter_idstructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiter_idstructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiter_idstructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type limiter_idstructureidentifierObj, run function getNonTimedElt') 
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


class limiter_unitstruct_arraylimiter_unit:
	'''
	class limiter_unitstruct_arraylimiter_unit
	Array of ncomponents limiting surfaces making up the limiter type (single contour or disjoint PFC). Replicate this limiter_unit element ncomponents times. Each unit contains a plasma facing component that can have dedicated number of points. Array of structures (ncomponents). Time-dependent

	Attributes:
	- array : list of limiter_unitstruct_arraylimiter_unitObj 
	'''

	def __init__(self, base_path_in='limiter_unit'):
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
		ret = space + 'class limiter_unitstruct_arraylimiter_unit\n'
		for i in range(len(self.array)):
			ret = ret + space + 'limiter_unitstruct_arraylimiter_unit[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(limiter_unitstruct_arraylimiter_unitObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function putSlice') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function getSlice') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(limiter_unitstruct_arraylimiter_unit(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(limiter_unitstruct_arraylimiter_unit(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = limiter_unitstruct_arraylimiter_unit(self.base_path)
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type limiter_unitstruct_arraylimiter_unit, run function getNonTimedElt') 
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


class limiter_unitstruct_arraylimiter_unitObj:
	'''
	class limiter_unitstruct_arraylimiter_unitObj
	Array of ncomponents limiting surfaces making up the limiter type (single contour or disjoint PFC). Replicate this limiter_unit element ncomponents times. Each unit contains a plasma facing component that can have dedicated number of points. Array of structures (ncomponents). Time-dependent

	Attributes:
	- name : str
	   Name or description of the limiter_unit
	- closed : str
	   Identify whether the contour is closed (y) or open (n)
	- position : class positionstructurerz1D
	   Position (R,Z coordinates) of a limiting surface. No need to repeat first point for closed contours [m]; Vector(npoints)
	- eta : float
	   Wall resistivity [ohm.m]; Scalar
	- delta : float
	   Wall thickness [m] (Optional if a closed facing component is given but useful for simpler closed contour limiter); Time-dependent; Scalar
	- permeability : float
	   Vessel relative permeability; Scalar
	'''

	def __init__(self, base_path_in='limiter_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.closed = ''
		self.position = positionstructurerz1D('position')
		self.eta = EMPTY_DOUBLE
		self.delta = EMPTY_DOUBLE
		self.permeability = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class limiter_unitstruct_arraylimiter_unitObj\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute closed: ' + str(self.closed) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute eta: ' + str(self.eta) + '\n'
		ret = ret + space + 'Attribute delta: ' + str(self.delta) + '\n'
		ret = ret + space + 'Attribute permeability: ' + str(self.permeability) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiter_unitstruct_arraylimiter_unitObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'delta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'delta', i, self.delta)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiter_unitstruct_arraylimiter_unitObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'delta') 
			print ('obj = ' + str(obj))
		status, ret_delta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'delta', i)
		check_status(status)
		if not status:
			self.delta = ret_delta

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiter_unitstruct_arraylimiter_unitObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'closed') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'closed', i, self.closed)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'eta', i, self.eta)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'permeability') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'permeability', i, self.permeability)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limiter_unitstruct_arraylimiter_unitObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'closed') 
			print ('obj = ' + str(obj))
		status, ret_closed = ull.getStringFromObject(self.idx, obj, cpopath + 'closed', i)
		check_status(status)
		if not status:
			self.closed = ret_closed
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		status, ret_eta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'eta', i)
		check_status(status)
		if not status:
			self.eta = ret_eta
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'permeability') 
			print ('obj = ' + str(obj))
		status, ret_permeability = ull.getDoubleFromObject(self.idx, obj, cpopath + 'permeability', i)
		check_status(status)
		if not status:
			self.permeability = ret_permeability


class vesselstructurewall_vessel:
	'''
	class vesselstructurewall_vessel
	Mechanical structure of the vacuum vessel. Vessel assumed as set of nested layers with given physics properties; Two representations are admitted for each vessel unit : annular (two contours) or blocks. The vessel_id identifies the type of vessel_unit set one is using and code-specific representations derived from the official ones are also allowed if documented. Array of structures (nvessel_type)

	Attributes:
	- vessel_id : class vessel_idstructureidentifier
	   Use this identifier to tag the type of vessel you are using. Use flag=0 for the official single/multiple annular vessel and 1 for the official block element representation for each vessel unit. Additional representations needed on a code-by-code basis follow same incremental pair tagging starting on flag=2
	- vessel_unit : class vessel_unitstruct_arraywall_vessel_unit: array of vessel_unitstruct_arraywall_vessel_unitObj objects
	   Array of vacuum vessel units. Replicate this vessel_unit element ncomponents times. Each unit contains a mechanical structure of the vessel with distinct physics properties. Array of structures (ncomponents)
	'''

	def __init__(self, base_path_in='vessel'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.vessel_id = vessel_idstructureidentifier('vessel_id')
		self.vessel_unit = vessel_unitstruct_arraywall_vessel_unit('vessel_unit')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vesselstructurewall_vessel\n'
		ret = ret + space + 'Attribute vessel_id\n ' + self.vessel_id.__str__(depth+1)
		ret = ret + space + 'Attribute vessel_unit\n ' + self.vessel_unit.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.vessel_id.setExpIdx(idx)
		self.vessel_unit.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vesselstructurewall_vessel, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.vessel_id.cpoTime = self.cpoTime
		self.vessel_id.putSlice(path, cpopath)
		self.vessel_unit.cpoTime = self.cpoTime
		self.vessel_unit.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vesselstructurewall_vessel, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.vessel_id.replaceLastSlice(path, cpopath)
		self.vessel_unit.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vesselstructurewall_vessel, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.vessel_id.putNonTimed(path, cpopath)
		self.vessel_unit.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vesselstructurewall_vessel, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.vessel_id.getSlice(path, cpopath, inTime, interpolMode)
		self.vessel_unit.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vesselstructurewall_vessel, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			vessel_idList = self.vessel_id.build_non_resampled_data(path, cpopath, nbslice)
			vessel_unitList = self.vessel_unit.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = vesselstructurewall_vessel(self.base_path)
				slice.setExpIdx(self.idx)
				slice.vessel_id = vessel_idList[i]
				slice.vessel_unit = vessel_unitList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vesselstructurewall_vesselObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vesselstructurewall_vesselObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vesselstructurewall_vesselObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.vessel_id.putNonTimedElt(path, cpopath + 'vessel_id', i, obj)
		obj = self.vessel_unit.putNonTimedElt(path, cpopath + 'vessel_unit', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vesselstructurewall_vesselObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.vessel_id.getNonTimedElt(path, cpopath + 'vessel_id', i, obj)
		self.vessel_unit.getNonTimedElt(path, cpopath + 'vessel_unit', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.vessel_id.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'vessel_unit')


class vessel_idstructureidentifier:
	'''
	class vessel_idstructureidentifier
	Use this identifier to tag the type of vessel you are using. Use flag=0 for the official single/multiple annular vessel and 1 for the official block element representation for each vessel unit. Additional representations needed on a code-by-code basis follow same incremental pair tagging starting on flag=2

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='vessel_id'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vessel_idstructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vessel_idstructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vessel_idstructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vessel_idstructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type vessel_idstructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type vessel_idstructureidentifier, run function build_non_resampled_data') 
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
				slice = vessel_idstructureidentifier(self.base_path)
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
			print ('object of type vessel_idstructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vessel_idstructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vessel_idstructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type vessel_idstructureidentifierObj, run function getNonTimedElt') 
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


class vessel_unitstruct_arraywall_vessel_unit:
	'''
	class vessel_unitstruct_arraywall_vessel_unit
	Array of vacuum vessel units. Replicate this vessel_unit element ncomponents times. Each unit contains a mechanical structure of the vessel with distinct physics properties. Array of structures (ncomponents)

	Attributes:
	- array : list of vessel_unitstruct_arraywall_vessel_unitObj 
	'''

	def __init__(self, base_path_in='vessel_unit'):
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
		ret = space + 'class vessel_unitstruct_arraywall_vessel_unit\n'
		for i in range(len(self.array)):
			ret = ret + space + 'vessel_unitstruct_arraywall_vessel_unit[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(vessel_unitstruct_arraywall_vessel_unitObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function getSlice') 
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
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(vessel_unitstruct_arraywall_vessel_unit(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = vessel_unitstruct_arraywall_vessel_unit(self.base_path)
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
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type vessel_unitstruct_arraywall_vessel_unit, run function getNonTimedElt') 
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


class vessel_unitstruct_arraywall_vessel_unitObj:
	'''
	class vessel_unitstruct_arraywall_vessel_unitObj
	Array of vacuum vessel units. Replicate this vessel_unit element ncomponents times. Each unit contains a mechanical structure of the vessel with distinct physics properties. Array of structures (ncomponents)

	Attributes:
	- annular : class annularstructurewall_vessel_annular
	   Annular representation of a vessel layer by two free-hand contours.
	- blocks : class blocksstructurewall_blocks
	   Block element representation of vessel units. Each vessel unit is decomposed in elementary small units (blocks) caracyerized by a position, resistivity and relative permeability.
	- radial_build : class radial_buildstructurewall_wall2d_vessel_radial_build
	   Simple description of this vessel unit for the radial_build in system codes
	'''

	def __init__(self, base_path_in='vessel_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.annular = annularstructurewall_vessel_annular('annular')
		self.blocks = blocksstructurewall_blocks('blocks')
		self.radial_build = radial_buildstructurewall_wall2d_vessel_radial_build('radial_build')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vessel_unitstruct_arraywall_vessel_unitObj\n'
		ret = ret + space + 'Attribute annular\n ' + self.annular.__str__(depth+1)
		ret = ret + space + 'Attribute blocks\n ' + self.blocks.__str__(depth+1)
		ret = ret + space + 'Attribute radial_build\n ' + self.radial_build.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.annular.setExpIdx(idx)
		self.blocks.setExpIdx(idx)
		self.radial_build.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vessel_unitstruct_arraywall_vessel_unitObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.annular.putNonTimedElt(path, cpopath + 'annular', i, obj)
		obj = self.blocks.putNonTimedElt(path, cpopath + 'blocks', i, obj)
		obj = self.radial_build.putNonTimedElt(path, cpopath + 'radial_build', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vessel_unitstruct_arraywall_vessel_unitObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.annular.getNonTimedElt(path, cpopath + 'annular', i, obj)
		self.blocks.getNonTimedElt(path, cpopath + 'blocks', i, obj)
		self.radial_build.getNonTimedElt(path, cpopath + 'radial_build', i, obj)


class annularstructurewall_vessel_annular:
	'''
	class annularstructurewall_vessel_annular
	Annular representation of a vessel layer by two free-hand contours.

	Attributes:
	- name : str
	   Name or description of the vessel_unit
	- inside : class insidestructurerz1D
	   Inner Vessel wall outline (list of R,Z co-ordinates) [m]; Vector (npoints_inner)
	- outside : class outsidestructurerz1D
	   Outer Vessel wall outline (list of R,Z co-ordinates) [m]; Vector (npoints_outer)
	- eta : float
	   Vessel resistivity [ohm.m]; Scalar
	- permeability : float
	   Vessel relative permeability; Scalar
	'''

	def __init__(self, base_path_in='annular'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.inside = insidestructurerz1D('inside')
		self.outside = outsidestructurerz1D('outside')
		self.eta = EMPTY_DOUBLE
		self.permeability = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class annularstructurewall_vessel_annular\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute inside\n ' + self.inside.__str__(depth+1)
		ret = ret + space + 'Attribute outside\n ' + self.outside.__str__(depth+1)
		ret = ret + space + 'Attribute eta: ' + str(self.eta) + '\n'
		ret = ret + space + 'Attribute permeability: ' + str(self.permeability) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.inside.setExpIdx(idx)
		self.outside.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type annularstructurewall_vessel_annular, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inside.cpoTime = self.cpoTime
		self.inside.putSlice(path, cpopath)
		self.outside.cpoTime = self.cpoTime
		self.outside.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type annularstructurewall_vessel_annular, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inside.replaceLastSlice(path, cpopath)
		self.outside.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type annularstructurewall_vessel_annular, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'name', self.name)
		check_status(status)
		self.inside.putNonTimed(path, cpopath)
		self.outside.putNonTimed(path, cpopath)
		status = ull.putDouble(self.idx, path, cpopath + 'eta', self.eta)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'permeability', self.permeability)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type annularstructurewall_vessel_annular, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		self.inside.getSlice(path, cpopath, inTime, interpolMode)
		self.outside.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_eta = ull.getDouble(self.idx, path, cpopath + 'eta')
		check_status(status)
		if not status:
			self.eta = ret_eta
		status, ret_permeability = ull.getDouble(self.idx, path, cpopath + 'permeability')
		check_status(status)
		if not status:
			self.permeability = ret_permeability

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type annularstructurewall_vessel_annular, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getString(self.idx, path, cpopath + 'name')
			check_status(status)
			insideList = self.inside.build_non_resampled_data(path, cpopath, nbslice)
			outsideList = self.outside.build_non_resampled_data(path, cpopath, nbslice)
			status, etaVal = ull.getDouble(self.idx, path, cpopath + 'eta')
			check_status(status)
			status, permeabilityVal = ull.getDouble(self.idx, path, cpopath + 'permeability')
			check_status(status)
			for i in range(nbslice):
				slice = annularstructurewall_vessel_annular(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.inside = insideList[i]
				slice.outside = outsideList[i]
				slice.eta = etaVal
				slice.permeability = permeabilityVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type annularstructurewall_vessel_annularObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type annularstructurewall_vessel_annularObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type annularstructurewall_vessel_annularObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		obj = self.inside.putNonTimedElt(path, cpopath + 'inside', i, obj)
		obj = self.outside.putNonTimedElt(path, cpopath + 'outside', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'eta', i, self.eta)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'permeability') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'permeability', i, self.permeability)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type annularstructurewall_vessel_annularObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		self.inside.getNonTimedElt(path, cpopath + 'inside', i, obj)
		self.outside.getNonTimedElt(path, cpopath + 'outside', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		status, ret_eta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'eta', i)
		check_status(status)
		if not status:
			self.eta = ret_eta
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'permeability') 
			print ('obj = ' + str(obj))
		status, ret_permeability = ull.getDoubleFromObject(self.idx, obj, cpopath + 'permeability', i)
		check_status(status)
		if not status:
			self.permeability = ret_permeability

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		self.inside.deleteData(path, cpopath)
		self.outside.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'eta')
		ull.deleteData(self.idx, path, cpopath + 'permeability')


class insidestructurerz1D:
	'''
	class insidestructurerz1D
	Inner Vessel wall outline (list of R,Z co-ordinates) [m]; Vector (npoints_inner)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='inside'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class insidestructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type insidestructurerz1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type insidestructurerz1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type insidestructurerz1D, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type insidestructurerz1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type insidestructurerz1D, run function build_non_resampled_data') 
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
				slice = insidestructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type insidestructurerz1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type insidestructurerz1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type insidestructurerz1DObj, run function putNonTimedElt') 
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
			print ('object of type insidestructurerz1DObj, run function getNonTimedElt') 
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


class outsidestructurerz1D:
	'''
	class outsidestructurerz1D
	Outer Vessel wall outline (list of R,Z co-ordinates) [m]; Vector (npoints_outer)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='outside'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class outsidestructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outsidestructurerz1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outsidestructurerz1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outsidestructurerz1D, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type outsidestructurerz1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type outsidestructurerz1D, run function build_non_resampled_data') 
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
				slice = outsidestructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outsidestructurerz1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outsidestructurerz1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outsidestructurerz1DObj, run function putNonTimedElt') 
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
			print ('object of type outsidestructurerz1DObj, run function getNonTimedElt') 
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


class blocksstructurewall_blocks:
	'''
	class blocksstructurewall_blocks
	Block element representation of vessel units. Each vessel unit is decomposed in elementary small units (blocks) caracyerized by a position, resistivity and relative permeability.

	Attributes:
	- blocks_unit : class blocks_unitstruct_arraywall_blocks_unit: array of blocks_unitstruct_arraywall_blocks_unitObj objects
	   Vector of blocks that build of the vessel layer. Replicate this element nblocks times. Each unit contains a building block of the vessel and can have dedicated number of points. Array of structures (nblocks)
	'''

	def __init__(self, base_path_in='blocks'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.blocks_unit = blocks_unitstruct_arraywall_blocks_unit('blocks_unit')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class blocksstructurewall_blocks\n'
		ret = ret + space + 'Attribute blocks_unit\n ' + self.blocks_unit.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.blocks_unit.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type blocksstructurewall_blocks, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.blocks_unit.cpoTime = self.cpoTime
		self.blocks_unit.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type blocksstructurewall_blocks, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.blocks_unit.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type blocksstructurewall_blocks, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.blocks_unit.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type blocksstructurewall_blocks, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.blocks_unit.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type blocksstructurewall_blocks, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			blocks_unitList = self.blocks_unit.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = blocksstructurewall_blocks(self.base_path)
				slice.setExpIdx(self.idx)
				slice.blocks_unit = blocks_unitList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type blocksstructurewall_blocksObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type blocksstructurewall_blocksObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type blocksstructurewall_blocksObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.blocks_unit.putNonTimedElt(path, cpopath + 'blocks_unit', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type blocksstructurewall_blocksObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.blocks_unit.getNonTimedElt(path, cpopath + 'blocks_unit', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'blocks_unit')


class blocks_unitstruct_arraywall_blocks_unit:
	'''
	class blocks_unitstruct_arraywall_blocks_unit
	Vector of blocks that build of the vessel layer. Replicate this element nblocks times. Each unit contains a building block of the vessel and can have dedicated number of points. Array of structures (nblocks)

	Attributes:
	- array : list of blocks_unitstruct_arraywall_blocks_unitObj 
	'''

	def __init__(self, base_path_in='blocks_unit'):
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
		ret = space + 'class blocks_unitstruct_arraywall_blocks_unit\n'
		for i in range(len(self.array)):
			ret = ret + space + 'blocks_unitstruct_arraywall_blocks_unit[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(blocks_unitstruct_arraywall_blocks_unitObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function getSlice') 
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
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(blocks_unitstruct_arraywall_blocks_unit(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = blocks_unitstruct_arraywall_blocks_unit(self.base_path)
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
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type blocks_unitstruct_arraywall_blocks_unit, run function getNonTimedElt') 
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


class blocks_unitstruct_arraywall_blocks_unitObj:
	'''
	class blocks_unitstruct_arraywall_blocks_unitObj
	Vector of blocks that build of the vessel layer. Replicate this element nblocks times. Each unit contains a building block of the vessel and can have dedicated number of points. Array of structures (nblocks)

	Attributes:
	- name : str
	   Name or description of the blocks_unit
	- position : class positionstructurerz1D
	   Position (R,Z coordinates) of a vessel segment. No need to repeat first point for closed contours [m]; Vector(npoints)
	- eta : float
	   Resistivity of the vessel segment [ohm.m]; Scalar
	- permeability : float
	   Vessel relative permeability; Scalar
	- j_phi : float
	   induced currents inside the vessel; time dependent; [A]

	- resistance : float
	   resistance of block; [Ohm]
	'''

	def __init__(self, base_path_in='blocks_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.position = positionstructurerz1D('position')
		self.eta = EMPTY_DOUBLE
		self.permeability = EMPTY_DOUBLE
		self.j_phi = EMPTY_DOUBLE
		self.resistance = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class blocks_unitstruct_arraywall_blocks_unitObj\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute eta: ' + str(self.eta) + '\n'
		ret = ret + space + 'Attribute permeability: ' + str(self.permeability) + '\n'
		ret = ret + space + 'Attribute j_phi: ' + str(self.j_phi) + '\n'
		ret = ret + space + 'Attribute resistance: ' + str(self.resistance) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type blocks_unitstruct_arraywall_blocks_unitObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'eta', i, self.eta)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'permeability') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'permeability', i, self.permeability)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'j_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'j_phi', i, self.j_phi)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'resistance') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'resistance', i, self.resistance)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type blocks_unitstruct_arraywall_blocks_unitObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		status, ret_eta = ull.getDoubleFromObject(self.idx, obj, cpopath + 'eta', i)
		check_status(status)
		if not status:
			self.eta = ret_eta
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'permeability') 
			print ('obj = ' + str(obj))
		status, ret_permeability = ull.getDoubleFromObject(self.idx, obj, cpopath + 'permeability', i)
		check_status(status)
		if not status:
			self.permeability = ret_permeability
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'j_phi') 
			print ('obj = ' + str(obj))
		status, ret_j_phi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'j_phi', i)
		check_status(status)
		if not status:
			self.j_phi = ret_j_phi
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'resistance') 
			print ('obj = ' + str(obj))
		status, ret_resistance = ull.getDoubleFromObject(self.idx, obj, cpopath + 'resistance', i)
		check_status(status)
		if not status:
			self.resistance = ret_resistance


class radial_buildstructurewall_wall2d_vessel_radial_build:
	'''
	class radial_buildstructurewall_wall2d_vessel_radial_build
	Simple description of this vessel unit for the radial_build in system codes

	Attributes:
	- r1_inb : float
	   Inner radius (nearest to the plasma), in the global tokamak coordinate system of the vv measured at the equatorial plane (inboard side) [m]; Scalar
	- r2_inb : float
	   Outer radius (farest from the plasma), in the global tokamak coordinate system of the vv measured at the equatorial plane (inboard side) [m]; Scalar
	- r1_outb : float
	   Inner radius (nearest to the plasma), in the global tokamak coordinate system of the vv measured at the equatorial plane (outboard side) [m]; Scalar
	- r2_outb : float
	   Outer radius (farest from the plasma), in the global tokamak coordinate system of the vv measured at the equatorial plane (outboard side) [m]; Scalar
	- raddim : float
	   Radial thickness of the vacuum vessel; Scalar
	- nmat : float
	   Number of materials; Scalar
	- composition : numpy.ndarray 1D with float
	   Inboard shield radial build giving the percentage of each material respectively (Meaning of the material index 1: Eurofer, 2: Pb-15.7Li, 3: He, 4: Water, 5: Tungsten Carbide, 6: Boron, 7: Tungsten, 8: Stainless Steel 316) in %vol; Vector
	- pow_dens_inb : float
	   Peak energy depostion in vaccum vessel inboard [W.m^-3]; Scalar
	- pow_dens_outb : float
	   Peak energy depostion in vaccum vessel outboard [W.m^-3]; Scalar
	- fn_flux_inb : float
	   Fast neutron flux in vaccum vessel inboard [m^2.s^-1]; Scalar
	- fn_flux_outb : float
	   Fast neutron flux in vaccum vessel outboard [m^2.s^-1]; Scalar
	'''

	def __init__(self, base_path_in='radial_build'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r1_inb = EMPTY_DOUBLE
		self.r2_inb = EMPTY_DOUBLE
		self.r1_outb = EMPTY_DOUBLE
		self.r2_outb = EMPTY_DOUBLE
		self.raddim = EMPTY_DOUBLE
		self.nmat = EMPTY_DOUBLE
		self.composition = numpy.zeros(0, numpy.float64, order='C')
		self.pow_dens_inb = EMPTY_DOUBLE
		self.pow_dens_outb = EMPTY_DOUBLE
		self.fn_flux_inb = EMPTY_DOUBLE
		self.fn_flux_outb = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class radial_buildstructurewall_wall2d_vessel_radial_build\n'
		ret = ret + space + 'Attribute r1_inb: ' + str(self.r1_inb) + '\n'
		ret = ret + space + 'Attribute r2_inb: ' + str(self.r2_inb) + '\n'
		ret = ret + space + 'Attribute r1_outb: ' + str(self.r1_outb) + '\n'
		ret = ret + space + 'Attribute r2_outb: ' + str(self.r2_outb) + '\n'
		ret = ret + space + 'Attribute raddim: ' + str(self.raddim) + '\n'
		ret = ret + space + 'Attribute nmat: ' + str(self.nmat) + '\n'
		s = self.composition.__str__()
		ret = ret + space + 'Attribute composition\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pow_dens_inb: ' + str(self.pow_dens_inb) + '\n'
		ret = ret + space + 'Attribute pow_dens_outb: ' + str(self.pow_dens_outb) + '\n'
		ret = ret + space + 'Attribute fn_flux_inb: ' + str(self.fn_flux_inb) + '\n'
		ret = ret + space + 'Attribute fn_flux_outb: ' + str(self.fn_flux_outb) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type radial_buildstructurewall_wall2d_vessel_radial_build, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type radial_buildstructurewall_wall2d_vessel_radial_build, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type radial_buildstructurewall_wall2d_vessel_radial_build, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'r1_inb', self.r1_inb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r2_inb', self.r2_inb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r1_outb', self.r1_outb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r2_outb', self.r2_outb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'raddim', self.raddim)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'nmat', self.nmat)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'composition', numpy.array(self.composition).astype(numpy.float64), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_inb', self.pow_dens_inb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_outb', self.pow_dens_outb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flux_inb', self.fn_flux_inb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flux_outb', self.fn_flux_outb)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type radial_buildstructurewall_wall2d_vessel_radial_build, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r1_inb = ull.getDouble(self.idx, path, cpopath + 'r1_inb')
		check_status(status)
		if not status:
			self.r1_inb = ret_r1_inb
		status, ret_r2_inb = ull.getDouble(self.idx, path, cpopath + 'r2_inb')
		check_status(status)
		if not status:
			self.r2_inb = ret_r2_inb
		status, ret_r1_outb = ull.getDouble(self.idx, path, cpopath + 'r1_outb')
		check_status(status)
		if not status:
			self.r1_outb = ret_r1_outb
		status, ret_r2_outb = ull.getDouble(self.idx, path, cpopath + 'r2_outb')
		check_status(status)
		if not status:
			self.r2_outb = ret_r2_outb
		status, ret_raddim = ull.getDouble(self.idx, path, cpopath + 'raddim')
		check_status(status)
		if not status:
			self.raddim = ret_raddim
		status, ret_nmat = ull.getDouble(self.idx, path, cpopath + 'nmat')
		check_status(status)
		if not status:
			self.nmat = ret_nmat
		status, ret_composition = ull.getVect1DDouble(self.idx, path, cpopath + 'composition')
		check_status(status)
		if not status:
			self.composition = ret_composition
		status, ret_pow_dens_inb = ull.getDouble(self.idx, path, cpopath + 'pow_dens_inb')
		check_status(status)
		if not status:
			self.pow_dens_inb = ret_pow_dens_inb
		status, ret_pow_dens_outb = ull.getDouble(self.idx, path, cpopath + 'pow_dens_outb')
		check_status(status)
		if not status:
			self.pow_dens_outb = ret_pow_dens_outb
		status, ret_fn_flux_inb = ull.getDouble(self.idx, path, cpopath + 'fn_flux_inb')
		check_status(status)
		if not status:
			self.fn_flux_inb = ret_fn_flux_inb
		status, ret_fn_flux_outb = ull.getDouble(self.idx, path, cpopath + 'fn_flux_outb')
		check_status(status)
		if not status:
			self.fn_flux_outb = ret_fn_flux_outb

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type radial_buildstructurewall_wall2d_vessel_radial_build, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, r1_inbVal = ull.getDouble(self.idx, path, cpopath + 'r1_inb')
			check_status(status)
			status, r2_inbVal = ull.getDouble(self.idx, path, cpopath + 'r2_inb')
			check_status(status)
			status, r1_outbVal = ull.getDouble(self.idx, path, cpopath + 'r1_outb')
			check_status(status)
			status, r2_outbVal = ull.getDouble(self.idx, path, cpopath + 'r2_outb')
			check_status(status)
			status, raddimVal = ull.getDouble(self.idx, path, cpopath + 'raddim')
			check_status(status)
			status, nmatVal = ull.getDouble(self.idx, path, cpopath + 'nmat')
			check_status(status)
			status, compositionVal = ull.getVect1DDouble(self.idx, path, cpopath + 'composition')
			check_status(status)
			status, pow_dens_inbVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_inb')
			check_status(status)
			status, pow_dens_outbVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_outb')
			check_status(status)
			status, fn_flux_inbVal = ull.getDouble(self.idx, path, cpopath + 'fn_flux_inb')
			check_status(status)
			status, fn_flux_outbVal = ull.getDouble(self.idx, path, cpopath + 'fn_flux_outb')
			check_status(status)
			for i in range(nbslice):
				slice = radial_buildstructurewall_wall2d_vessel_radial_build(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r1_inb = r1_inbVal
				slice.r2_inb = r2_inbVal
				slice.r1_outb = r1_outbVal
				slice.r2_outb = r2_outbVal
				slice.raddim = raddimVal
				slice.nmat = nmatVal
				slice.composition = compositionVal
				slice.pow_dens_inb = pow_dens_inbVal
				slice.pow_dens_outb = pow_dens_outbVal
				slice.fn_flux_inb = fn_flux_inbVal
				slice.fn_flux_outb = fn_flux_outbVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type radial_buildstructurewall_wall2d_vessel_radial_buildObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type radial_buildstructurewall_wall2d_vessel_radial_buildObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type radial_buildstructurewall_wall2d_vessel_radial_buildObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r1_inb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r1_inb', i, self.r1_inb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r2_inb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r2_inb', i, self.r2_inb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r1_outb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r1_outb', i, self.r1_outb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r2_outb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r2_outb', i, self.r2_outb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'raddim') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'raddim', i, self.raddim)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nmat') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nmat', i, self.nmat)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'composition', i, numpy.array(self.composition).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_inb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_inb', i, self.pow_dens_inb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_outb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_outb', i, self.pow_dens_outb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flux_inb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flux_inb', i, self.fn_flux_inb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flux_outb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flux_outb', i, self.fn_flux_outb)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type radial_buildstructurewall_wall2d_vessel_radial_buildObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r1_inb') 
			print ('obj = ' + str(obj))
		status, ret_r1_inb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r1_inb', i)
		check_status(status)
		if not status:
			self.r1_inb = ret_r1_inb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r2_inb') 
			print ('obj = ' + str(obj))
		status, ret_r2_inb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r2_inb', i)
		check_status(status)
		if not status:
			self.r2_inb = ret_r2_inb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r1_outb') 
			print ('obj = ' + str(obj))
		status, ret_r1_outb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r1_outb', i)
		check_status(status)
		if not status:
			self.r1_outb = ret_r1_outb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r2_outb') 
			print ('obj = ' + str(obj))
		status, ret_r2_outb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r2_outb', i)
		check_status(status)
		if not status:
			self.r2_outb = ret_r2_outb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'raddim') 
			print ('obj = ' + str(obj))
		status, ret_raddim = ull.getDoubleFromObject(self.idx, obj, cpopath + 'raddim', i)
		check_status(status)
		if not status:
			self.raddim = ret_raddim
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nmat') 
			print ('obj = ' + str(obj))
		status, ret_nmat = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nmat', i)
		check_status(status)
		if not status:
			self.nmat = ret_nmat
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		status, ret_composition = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'composition', i)
		check_status(status)
		if not status:
			self.composition = ret_composition
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_inb') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_inb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_inb', i)
		check_status(status)
		if not status:
			self.pow_dens_inb = ret_pow_dens_inb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_outb') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_outb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_outb', i)
		check_status(status)
		if not status:
			self.pow_dens_outb = ret_pow_dens_outb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flux_inb') 
			print ('obj = ' + str(obj))
		status, ret_fn_flux_inb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flux_inb', i)
		check_status(status)
		if not status:
			self.fn_flux_inb = ret_fn_flux_inb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flux_outb') 
			print ('obj = ' + str(obj))
		status, ret_fn_flux_outb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flux_outb', i)
		check_status(status)
		if not status:
			self.fn_flux_outb = ret_fn_flux_outb

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r1_inb')
		ull.deleteData(self.idx, path, cpopath + 'r2_inb')
		ull.deleteData(self.idx, path, cpopath + 'r1_outb')
		ull.deleteData(self.idx, path, cpopath + 'r2_outb')
		ull.deleteData(self.idx, path, cpopath + 'raddim')
		ull.deleteData(self.idx, path, cpopath + 'nmat')
		ull.deleteData(self.idx, path, cpopath + 'composition')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_inb')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_outb')
		ull.deleteData(self.idx, path, cpopath + 'fn_flux_inb')
		ull.deleteData(self.idx, path, cpopath + 'fn_flux_outb')


class plasmastruct_arrayplasmaComplexType:
	'''
	class plasmastruct_arrayplasmaComplexType
	Description of incoming plasma for every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the limiter unit with index i in wall/wall2d/limiter/limiter_unit. Time-dependent

	Attributes:
	- array : list of plasmastruct_arrayplasmaComplexTypeObj 
	'''

	def __init__(self, base_path_in='plasma'):
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
		ret = space + 'class plasmastruct_arrayplasmaComplexType\n'
		for i in range(len(self.array)):
			ret = ret + space + 'plasmastruct_arrayplasmaComplexType[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(plasmastruct_arrayplasmaComplexTypeObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function putSlice') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function getSlice') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(plasmastruct_arrayplasmaComplexType(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(plasmastruct_arrayplasmaComplexType(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = plasmastruct_arrayplasmaComplexType(self.base_path)
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type plasmastruct_arrayplasmaComplexType, run function getNonTimedElt') 
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


class plasmastruct_arrayplasmaComplexTypeObj:
	'''
	class plasmastruct_arrayplasmaComplexTypeObj
	Description of incoming plasma for every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the limiter unit with index i in wall/wall2d/limiter/limiter_unit. Time-dependent

	Attributes:
	- species : numpy.ndarray 1D with int)
	   Definition of plasma species. Index into wall/compositions/edgespecies. Integer vector (number of plasma species).
	- flux : numpy.ndarray 2D with float
	   Plasma particle flux density from/to plasma facing wall surfaces [1/(m^2 s)]. Positive means incoming onto the wall, negative means sent back into the plasma. Time-dependent; Float matrix (number of plasma species, number of discretization elements in the subgrid)
	- b : numpy.ndarray 2D with float
	   Magnetic field vector at the surface [T]; Time-dependent; Float matrix (number of space dimensions, number of discretization elements in the subgrid). If two-dimensional: unit vectors with first coordinate perpendicular to the wall facing towards the plasma, second coordinate parallel to the surface (in the direction of the surface discretization), third dimension is zero. If three-dimensional: vector is relative to basis vectors stored in wall/wall3d/grid/basis 
with basis index as given in wall/wall3d/basis_index.
	- energy : numpy.ndarray 2D with float
	   Total energy flux density of incoming particles of given species [W/m^2]; Positive means incoming onto the wall, negative means sent back into the plasma. Time-dependent; Float matrix (number of plasma species, number of discretization elements in the subgrid)
	'''

	def __init__(self, base_path_in='plasma'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.species = numpy.zeros(0, numpy.int32, order='C')
		self.flux = numpy.zeros((0,0), numpy.float64, order='C')
		self.b = numpy.zeros((0,0), numpy.float64, order='C')
		self.energy = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class plasmastruct_arrayplasmaComplexTypeObj\n'
		s = self.species.__str__()
		ret = ret + space + 'Attribute species\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.flux.__str__()
		ret = ret + space + 'Attribute flux\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b.__str__()
		ret = ret + space + 'Attribute b\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.energy.__str__()
		ret = ret + space + 'Attribute energy\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastruct_arrayplasmaComplexTypeObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'flux', i, numpy.array(self.flux).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'b', i, numpy.array(self.b).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'energy', i, numpy.array(self.energy).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastruct_arrayplasmaComplexTypeObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'flux') 
			print ('obj = ' + str(obj))
		status, ret_flux = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'flux', i)
		check_status(status)
		if not status:
			self.flux = ret_flux
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'b') 
			print ('obj = ' + str(obj))
		status, ret_b = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'b', i)
		check_status(status)
		if not status:
			self.b = ret_b
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastruct_arrayplasmaComplexTypeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'species') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'species', i, numpy.array(self.species).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type plasmastruct_arrayplasmaComplexTypeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'species') 
			print ('obj = ' + str(obj))
		status, ret_species = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'species', i)
		check_status(status)
		if not status:
			self.species = ret_species


class wall_statestruct_arraywall_unitsComplexType:
	'''
	class wall_statestruct_arraywall_unitsComplexType
	Dynamic wall state of every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the limiter unit with index i in wall/wall2d/limiter/limiter_unit. Time-dependent

	Attributes:
	- array : list of wall_statestruct_arraywall_unitsComplexTypeObj 
	'''

	def __init__(self, base_path_in='wall_state'):
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
		ret = space + 'class wall_statestruct_arraywall_unitsComplexType\n'
		for i in range(len(self.array)):
			ret = ret + space + 'wall_statestruct_arraywall_unitsComplexType[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(wall_statestruct_arraywall_unitsComplexTypeObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function putSlice') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function getSlice') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(wall_statestruct_arraywall_unitsComplexType(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(wall_statestruct_arraywall_unitsComplexType(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = wall_statestruct_arraywall_unitsComplexType(self.base_path)
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type wall_statestruct_arraywall_unitsComplexType, run function getNonTimedElt') 
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


class wall_statestruct_arraywall_unitsComplexTypeObj:
	'''
	class wall_statestruct_arraywall_unitsComplexTypeObj
	Dynamic wall state of every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the limiter unit with index i in wall/wall2d/limiter/limiter_unit. Time-dependent

	Attributes:
	- wall_type : int
	   Definition of reference wall composition for every subgrid of the wall discretization. Vector of integers (number of subgrids). The indices point to wall/wall_types.
	- n_depo_layer : int
	   Number of deposited layers (in addition to the engineering layers)
	- layers : class layersstruct_arraywall_unitsComplexType_layers: array of layersstruct_arraywall_unitsComplexType_layersObj objects
	   Data on wall element layers; Array of structures (number of engineering layers + number of deposited layers); Layers can possibly be void (e.g. completely eroded), which is indicated by zero thickness. Time-dependent
	- eta : class etastructurecomplexgrid_scalar
	   Resitivity of wall element described by grid geometry [Ohm.m]
	- permeability : class permeabilitystructurecomplexgrid_scalar
	   Relative permeability of wall element described by grid geometry [-]
	- j : class jstructurecomplexgrid_vector
	   Current density vector in the element specified by the grid representation. [A/m^2]
	'''

	def __init__(self, base_path_in='wall_state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.wall_type = EMPTY_INT
		self.n_depo_layer = EMPTY_INT
		self.layers = layersstruct_arraywall_unitsComplexType_layers('layers')
		self.eta = etastructurecomplexgrid_scalar('eta')
		self.permeability = permeabilitystructurecomplexgrid_scalar('permeability')
		self.j = jstructurecomplexgrid_vector('j')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall_statestruct_arraywall_unitsComplexTypeObj\n'
		ret = ret + space + 'Attribute wall_type: ' + str(self.wall_type) + '\n'
		ret = ret + space + 'Attribute n_depo_layer: ' + str(self.n_depo_layer) + '\n'
		ret = ret + space + 'Attribute layers\n ' + self.layers.__str__(depth+1)
		ret = ret + space + 'Attribute eta\n ' + self.eta.__str__(depth+1)
		ret = ret + space + 'Attribute permeability\n ' + self.permeability.__str__(depth+1)
		ret = ret + space + 'Attribute j\n ' + self.j.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.layers.setExpIdx(idx)
		self.eta.setExpIdx(idx)
		self.permeability.setExpIdx(idx)
		self.j.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_statestruct_arraywall_unitsComplexTypeObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.layers.putTimedElt(path, cpopath + 'layers', i, obj)
		obj = self.j.putTimedElt(path, cpopath + 'j', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_statestruct_arraywall_unitsComplexTypeObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.layers.getTimedElt(path, cpopath + 'layers', i, obj)
		self.j.getTimedElt(path, cpopath + 'j', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_statestruct_arraywall_unitsComplexTypeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'wall_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'wall_type', i, self.wall_type)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'n_depo_layer') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'n_depo_layer', i, self.n_depo_layer)
		obj = self.layers.putNonTimedElt(path, cpopath + 'layers', i, obj)
		obj = self.eta.putNonTimedElt(path, cpopath + 'eta', i, obj)
		obj = self.permeability.putNonTimedElt(path, cpopath + 'permeability', i, obj)
		obj = self.j.putNonTimedElt(path, cpopath + 'j', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_statestruct_arraywall_unitsComplexTypeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'wall_type') 
			print ('obj = ' + str(obj))
		status, ret_wall_type = ull.getIntFromObject(self.idx, obj, cpopath + 'wall_type', i)
		check_status(status)
		if not status:
			self.wall_type = ret_wall_type
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'n_depo_layer') 
			print ('obj = ' + str(obj))
		status, ret_n_depo_layer = ull.getIntFromObject(self.idx, obj, cpopath + 'n_depo_layer', i)
		check_status(status)
		if not status:
			self.n_depo_layer = ret_n_depo_layer
		self.layers.getNonTimedElt(path, cpopath + 'layers', i, obj)
		self.eta.getNonTimedElt(path, cpopath + 'eta', i, obj)
		self.permeability.getNonTimedElt(path, cpopath + 'permeability', i, obj)
		self.j.getNonTimedElt(path, cpopath + 'j', i, obj)


class layersstruct_arraywall_unitsComplexType_layers:
	'''
	class layersstruct_arraywall_unitsComplexType_layers
	Data on wall element layers; Array of structures (number of engineering layers + number of deposited layers); Layers can possibly be void (e.g. completely eroded), which is indicated by zero thickness. Time-dependent

	Attributes:
	- array : list of layersstruct_arraywall_unitsComplexType_layersObj 
	'''

	def __init__(self, base_path_in='layers'):
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
		ret = space + 'class layersstruct_arraywall_unitsComplexType_layers\n'
		for i in range(len(self.array)):
			ret = ret + space + 'layersstruct_arraywall_unitsComplexType_layers[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(layersstruct_arraywall_unitsComplexType_layersObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function putSlice') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function getSlice') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(layersstruct_arraywall_unitsComplexType_layers(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(layersstruct_arraywall_unitsComplexType_layers(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = layersstruct_arraywall_unitsComplexType_layers(self.base_path)
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_unitsComplexType_layers, run function getNonTimedElt') 
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


class layersstruct_arraywall_unitsComplexType_layersObj:
	'''
	class layersstruct_arraywall_unitsComplexType_layersObj
	Data on wall element layers; Array of structures (number of engineering layers + number of deposited layers); Layers can possibly be void (e.g. completely eroded), which is indicated by zero thickness. Time-dependent

	Attributes:
	- elements : numpy.ndarray 1D with int)
	   List of elements present in the solid phase in this layer. Vector (number of elements). Holds indices pointing to wall/elements
	- gases : numpy.ndarray 1D with int)
	   List of gases present in this layer. Vector (number of gases). Holds indices pointing to wall/elements
	- compounds : numpy.ndarray 1D with int)
	   List of compounds present in the solid phase in this layer. Vector (number of compounds). Holds indices pointing to wall/compounds
	- density : numpy.ndarray 2D with float
	   Discretized density distribution in the layer of the discrete wall elements in the subgrid [kg/m^3]; Time-dependent; Float matrix (number of vertical cells in layer, number of discretization elements in the subgrid)
	- dx : numpy.ndarray 2D with float
	   Size of the vertical cells in the layer of the discrete wall elements in the subgrid [kg/m^3]; Time-dependent; Float matrix (number of vertical cells in layer, number of discretization elements in the subgrid)
	- thickness : numpy.ndarray 1D with float
	   Total size of the layer [m] (i.e. sum of dx over the number of vertical cells in the layer); Time-dependent; Vector (number of discretization elements in the subgrid)
	- roughness : numpy.ndarray 3D with float
	   Interface roughness description between the discrete elements and their top neighbour (i.e. towards the plasma); Time-dependent; Float 3d array (number of vertical cells in layer, number of discretization elements in the subgrid, index of roughness parameter); Roughness parameter 1: RMS height [m], parameter 2: wavelength along projection of B on the surface [m], parameter 3: wavelength perpendicular to projection of B on the surface [m]. If only two parameters are given the parameters are assumed to be isotropic
	- porosity : numpy.ndarray 3D with float
	   Discrete description of porosity of the layer. Time-dependent; Float 3d array (number of vertical cells in layer, number of discretization elements in the subgrid, index of porosity parameter); Porosity parameter 1: Volume fraction occupied by the pores [-], parameter 2: average size of the pores [m]
	- dpa : numpy.ndarray 2D with float
	   Discretized number of displacements per atom in the layer of the discrete wall elements in the subgrid [-]; Time-dependent; Float matrix (number of vertical cells in layer, number of discretization elements in the subgrid)
	- temperature : numpy.ndarray 2D with float
	   Discretized temperature distribution in the layer of the discrete wall elements in the subgrid [eV]; Time-dependent; Float matrix (number of vertical cells in layer, number of discretization elements in the subgrid)
	- element_frac : numpy.ndarray 3D with float
	   Fractional abundance of elements in the layer of the discrete wall elements in the subgrid [-]; Time-dependent; Float matrix (number of chemical elements as given in (local) elements, number of vertical cells in layer, number of discretization elements in the subgrid)
	- chem_comp : numpy.ndarray 3D with float
	   Fractional abundance of chemical compounds in the layer of the discrete wall elements in the subgrid [-]; Time-dependent; Float matrix (number of chemical compounds as given in (local) compounds, number of vertical cells in layer, number of discretization elements in the subgrid)
	- bulk_D : numpy.ndarray 4D with float
	   Diffusivity of gas species in bulks of different compounds [m^2/s]; Time-dependent; 4d float array. Dimensions: 1. index of compound (indexing as in (local) compounds), 2. index of gas element (indexing as in (local) gases), 3. cell index of 1d layer height discretization, 4. number of discretization elements in the subgrid
	- surface_D : numpy.ndarray 4D with float
	   Diffusivity of hydrogen species of surface of different compounds [m^2/s]; Time-dependent; Dimensions: see bulk_D
	- bulk_solute : numpy.ndarray 4D with float
	   Bulk mobile (solute) concentration [atoms/m^3]; Time-dependent; Dimensions: see bulk_D
	- surf_solute : numpy.ndarray 4D with float
	   Surface mobile (solute) concentration [atoms/m^2]; Time-dependent; Dimensions: see bulk_D
	- pore_content : numpy.ndarray 3D with float
	   Amount of gas species trapped in pores per cubic meter [1/m^3]; Time-dependent; 3d float array. Dimensions: 1. index of gas element (indexing as in (local) gases), 2. cell index of 1d layer height discretization, 3. number of discretization element in the subgrid
	- trap_type : class trap_typestruct_arraytrap_type: array of trap_typestruct_arraytrap_typeObj objects
	   Definition of trap types. Array of structures (number of trap types)
	'''

	def __init__(self, base_path_in='layers'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.elements = numpy.zeros(0, numpy.int32, order='C')
		self.gases = numpy.zeros(0, numpy.int32, order='C')
		self.compounds = numpy.zeros(0, numpy.int32, order='C')
		self.density = numpy.zeros((0,0), numpy.float64, order='C')
		self.dx = numpy.zeros((0,0), numpy.float64, order='C')
		self.thickness = numpy.zeros(0, numpy.float64, order='C')
		self.roughness = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.porosity = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.dpa = numpy.zeros((0,0), numpy.float64, order='C')
		self.temperature = numpy.zeros((0,0), numpy.float64, order='C')
		self.element_frac = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.chem_comp = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.bulk_D = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.surface_D = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.bulk_solute = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.surf_solute = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.pore_content = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.trap_type = trap_typestruct_arraytrap_type('trap_type')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class layersstruct_arraywall_unitsComplexType_layersObj\n'
		s = self.elements.__str__()
		ret = ret + space + 'Attribute elements\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gases.__str__()
		ret = ret + space + 'Attribute gases\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.compounds.__str__()
		ret = ret + space + 'Attribute compounds\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.density.__str__()
		ret = ret + space + 'Attribute density\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dx.__str__()
		ret = ret + space + 'Attribute dx\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.thickness.__str__()
		ret = ret + space + 'Attribute thickness\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.roughness.__str__()
		ret = ret + space + 'Attribute roughness\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.porosity.__str__()
		ret = ret + space + 'Attribute porosity\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dpa.__str__()
		ret = ret + space + 'Attribute dpa\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.temperature.__str__()
		ret = ret + space + 'Attribute temperature\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.element_frac.__str__()
		ret = ret + space + 'Attribute element_frac\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chem_comp.__str__()
		ret = ret + space + 'Attribute chem_comp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.bulk_D.__str__()
		ret = ret + space + 'Attribute bulk_D\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.surface_D.__str__()
		ret = ret + space + 'Attribute surface_D\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.bulk_solute.__str__()
		ret = ret + space + 'Attribute bulk_solute\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.surf_solute.__str__()
		ret = ret + space + 'Attribute surf_solute\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pore_content.__str__()
		ret = ret + space + 'Attribute pore_content\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute trap_type\n ' + self.trap_type.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.trap_type.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type layersstruct_arraywall_unitsComplexType_layersObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'density', i, numpy.array(self.density).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'dx') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'dx', i, numpy.array(self.dx).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'thickness') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'thickness', i, numpy.array(self.thickness).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'roughness') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'roughness', i, numpy.array(self.roughness).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'porosity') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'porosity', i, numpy.array(self.porosity).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'dpa') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'dpa', i, numpy.array(self.dpa).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'temperature') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'temperature', i, numpy.array(self.temperature).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'element_frac') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'element_frac', i, numpy.array(self.element_frac).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'chem_comp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'chem_comp', i, numpy.array(self.chem_comp).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'bulk_D') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'bulk_D', i, numpy.array(self.bulk_D).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'surface_D') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'surface_D', i, numpy.array(self.surface_D).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'bulk_solute') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'bulk_solute', i, numpy.array(self.bulk_solute).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'surf_solute') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'surf_solute', i, numpy.array(self.surf_solute).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pore_content') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pore_content', i, numpy.array(self.pore_content).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type layersstruct_arraywall_unitsComplexType_layersObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		status, ret_density = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'density', i)
		check_status(status)
		if not status:
			self.density = ret_density
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'dx') 
			print ('obj = ' + str(obj))
		status, ret_dx = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'dx', i)
		check_status(status)
		if not status:
			self.dx = ret_dx
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'thickness') 
			print ('obj = ' + str(obj))
		status, ret_thickness = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'thickness', i)
		check_status(status)
		if not status:
			self.thickness = ret_thickness
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'roughness') 
			print ('obj = ' + str(obj))
		status, ret_roughness = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'roughness', i)
		check_status(status)
		if not status:
			self.roughness = ret_roughness
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'porosity') 
			print ('obj = ' + str(obj))
		status, ret_porosity = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'porosity', i)
		check_status(status)
		if not status:
			self.porosity = ret_porosity
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'dpa') 
			print ('obj = ' + str(obj))
		status, ret_dpa = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'dpa', i)
		check_status(status)
		if not status:
			self.dpa = ret_dpa
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'temperature') 
			print ('obj = ' + str(obj))
		status, ret_temperature = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'temperature', i)
		check_status(status)
		if not status:
			self.temperature = ret_temperature
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'element_frac') 
			print ('obj = ' + str(obj))
		status, ret_element_frac = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'element_frac', i)
		check_status(status)
		if not status:
			self.element_frac = ret_element_frac
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'chem_comp') 
			print ('obj = ' + str(obj))
		status, ret_chem_comp = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'chem_comp', i)
		check_status(status)
		if not status:
			self.chem_comp = ret_chem_comp
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'bulk_D') 
			print ('obj = ' + str(obj))
		status, ret_bulk_D = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'bulk_D', i)
		check_status(status)
		if not status:
			self.bulk_D = ret_bulk_D
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'surface_D') 
			print ('obj = ' + str(obj))
		status, ret_surface_D = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'surface_D', i)
		check_status(status)
		if not status:
			self.surface_D = ret_surface_D
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'bulk_solute') 
			print ('obj = ' + str(obj))
		status, ret_bulk_solute = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'bulk_solute', i)
		check_status(status)
		if not status:
			self.bulk_solute = ret_bulk_solute
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'surf_solute') 
			print ('obj = ' + str(obj))
		status, ret_surf_solute = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'surf_solute', i)
		check_status(status)
		if not status:
			self.surf_solute = ret_surf_solute
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pore_content') 
			print ('obj = ' + str(obj))
		status, ret_pore_content = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pore_content', i)
		check_status(status)
		if not status:
			self.pore_content = ret_pore_content

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type layersstruct_arraywall_unitsComplexType_layersObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'elements') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'elements', i, numpy.array(self.elements).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'gases') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'gases', i, numpy.array(self.gases).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'compounds') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'compounds', i, numpy.array(self.compounds).astype(numpy.int32))
		obj = self.trap_type.putNonTimedElt(path, cpopath + 'trap_type', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type layersstruct_arraywall_unitsComplexType_layersObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'elements') 
			print ('obj = ' + str(obj))
		status, ret_elements = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'elements', i)
		check_status(status)
		if not status:
			self.elements = ret_elements
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'gases') 
			print ('obj = ' + str(obj))
		status, ret_gases = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'gases', i)
		check_status(status)
		if not status:
			self.gases = ret_gases
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'compounds') 
			print ('obj = ' + str(obj))
		status, ret_compounds = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'compounds', i)
		check_status(status)
		if not status:
			self.compounds = ret_compounds
		self.trap_type.getNonTimedElt(path, cpopath + 'trap_type', i, obj)


class trap_typestruct_arraytrap_type:
	'''
	class trap_typestruct_arraytrap_type
	Definition of trap types. Array of structures (number of trap types)

	Attributes:
	- array : list of trap_typestruct_arraytrap_typeObj 
	'''

	def __init__(self, base_path_in='trap_type'):
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
		ret = space + 'class trap_typestruct_arraytrap_type\n'
		for i in range(len(self.array)):
			ret = ret + space + 'trap_typestruct_arraytrap_type[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(trap_typestruct_arraytrap_typeObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function getSlice') 
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
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(trap_typestruct_arraytrap_type(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = trap_typestruct_arraytrap_type(self.base_path)
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
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type trap_typestruct_arraytrap_type, run function getNonTimedElt') 
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


class trap_typestruct_arraytrap_typeObj:
	'''
	class trap_typestruct_arraytrap_typeObj
	Definition of trap types. Array of structures (number of trap types)

	Attributes:
	- trap_id : class trap_idstructureidentifier
	   Identifier for the trap type
	- compound : int
	   Index of the compound doing the trapping. Refers to (local) ../compounds.
	- gas_species : int
	   Index of the gas species being trapped. Refers to (local) ../gases.
	- energy : float
	   Energy depth of the trap [eV]
	- fill_factor : numpy.ndarray 2D with float
	   Discretized filling fraction of traps in this layer (0...1) [-]. Dimensions: 1. index: cell index of depth discretization in this layer; 2. index: number of discretization elements in the subgrid
	- density : numpy.ndarray 2D with float
	   Discretized density of traps in this layer [1/m^3]. Dimensions: 1. index: cell index of depth discretization in this layer; 2. index: number of discretization elements in the subgrid
	'''

	def __init__(self, base_path_in='trap_type'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.trap_id = trap_idstructureidentifier('trap_id')
		self.compound = EMPTY_INT
		self.gas_species = EMPTY_INT
		self.energy = EMPTY_DOUBLE
		self.fill_factor = numpy.zeros((0,0), numpy.float64, order='C')
		self.density = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trap_typestruct_arraytrap_typeObj\n'
		ret = ret + space + 'Attribute trap_id\n ' + self.trap_id.__str__(depth+1)
		ret = ret + space + 'Attribute compound: ' + str(self.compound) + '\n'
		ret = ret + space + 'Attribute gas_species: ' + str(self.gas_species) + '\n'
		ret = ret + space + 'Attribute energy: ' + str(self.energy) + '\n'
		s = self.fill_factor.__str__()
		ret = ret + space + 'Attribute fill_factor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.density.__str__()
		ret = ret + space + 'Attribute density\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.trap_id.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trap_typestruct_arraytrap_typeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.trap_id.putNonTimedElt(path, cpopath + 'trap_id', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'compound') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'compound', i, self.compound)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'gas_species') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'gas_species', i, self.gas_species)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'energy', i, self.energy)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'fill_factor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'fill_factor', i, numpy.array(self.fill_factor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'density', i, numpy.array(self.density).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trap_typestruct_arraytrap_typeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.trap_id.getNonTimedElt(path, cpopath + 'trap_id', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'compound') 
			print ('obj = ' + str(obj))
		status, ret_compound = ull.getIntFromObject(self.idx, obj, cpopath + 'compound', i)
		check_status(status)
		if not status:
			self.compound = ret_compound
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'gas_species') 
			print ('obj = ' + str(obj))
		status, ret_gas_species = ull.getIntFromObject(self.idx, obj, cpopath + 'gas_species', i)
		check_status(status)
		if not status:
			self.gas_species = ret_gas_species
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'fill_factor') 
			print ('obj = ' + str(obj))
		status, ret_fill_factor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'fill_factor', i)
		check_status(status)
		if not status:
			self.fill_factor = ret_fill_factor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		status, ret_density = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'density', i)
		check_status(status)
		if not status:
			self.density = ret_density


class trap_idstructureidentifier:
	'''
	class trap_idstructureidentifier
	Identifier for the trap type

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='trap_id'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trap_idstructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trap_idstructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trap_idstructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trap_idstructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type trap_idstructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type trap_idstructureidentifier, run function build_non_resampled_data') 
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
				slice = trap_idstructureidentifier(self.base_path)
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
			print ('object of type trap_idstructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trap_idstructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trap_idstructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type trap_idstructureidentifierObj, run function getNonTimedElt') 
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


class etastructurecomplexgrid_scalar:
	'''
	class etastructurecomplexgrid_scalar
	Resitivity of wall element described by grid geometry [Ohm.m]

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='eta'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class etastructurecomplexgrid_scalar\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type etastructurecomplexgrid_scalar, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type etastructurecomplexgrid_scalar, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type etastructurecomplexgrid_scalar, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect3DDouble(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type etastructurecomplexgrid_scalar, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid = ull.getInt(self.idx, path, cpopath + 'griduid')
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		status, ret_subgrid = ull.getInt(self.idx, path, cpopath + 'subgrid')
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		status, ret_scalar = ull.getVect1DDouble(self.idx, path, cpopath + 'scalar')
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		status, ret_vector = ull.getVect2DDouble(self.idx, path, cpopath + 'vector')
		check_status(status)
		if not status:
			self.vector = ret_vector
		status, ret_matrix = ull.getVect3DDouble(self.idx, path, cpopath + 'matrix')
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type etastructurecomplexgrid_scalar, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidVal = ull.getInt(self.idx, path, cpopath + 'griduid')
			check_status(status)
			status, subgridVal = ull.getInt(self.idx, path, cpopath + 'subgrid')
			check_status(status)
			status, scalarVal = ull.getVect1DDouble(self.idx, path, cpopath + 'scalar')
			check_status(status)
			status, vectorVal = ull.getVect2DDouble(self.idx, path, cpopath + 'vector')
			check_status(status)
			status, matrixVal = ull.getVect3DDouble(self.idx, path, cpopath + 'matrix')
			check_status(status)
			for i in range(nbslice):
				slice = etastructurecomplexgrid_scalar(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = griduidVal
				slice.subgrid = subgridVal
				slice.scalar = scalarVal
				slice.vector = vectorVal
				slice.matrix = matrixVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type etastructurecomplexgrid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type etastructurecomplexgrid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type etastructurecomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type etastructurecomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class permeabilitystructurecomplexgrid_scalar:
	'''
	class permeabilitystructurecomplexgrid_scalar
	Relative permeability of wall element described by grid geometry [-]

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='permeability'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class permeabilitystructurecomplexgrid_scalar\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type permeabilitystructurecomplexgrid_scalar, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type permeabilitystructurecomplexgrid_scalar, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type permeabilitystructurecomplexgrid_scalar, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect3DDouble(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type permeabilitystructurecomplexgrid_scalar, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid = ull.getInt(self.idx, path, cpopath + 'griduid')
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		status, ret_subgrid = ull.getInt(self.idx, path, cpopath + 'subgrid')
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		status, ret_scalar = ull.getVect1DDouble(self.idx, path, cpopath + 'scalar')
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		status, ret_vector = ull.getVect2DDouble(self.idx, path, cpopath + 'vector')
		check_status(status)
		if not status:
			self.vector = ret_vector
		status, ret_matrix = ull.getVect3DDouble(self.idx, path, cpopath + 'matrix')
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type permeabilitystructurecomplexgrid_scalar, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidVal = ull.getInt(self.idx, path, cpopath + 'griduid')
			check_status(status)
			status, subgridVal = ull.getInt(self.idx, path, cpopath + 'subgrid')
			check_status(status)
			status, scalarVal = ull.getVect1DDouble(self.idx, path, cpopath + 'scalar')
			check_status(status)
			status, vectorVal = ull.getVect2DDouble(self.idx, path, cpopath + 'vector')
			check_status(status)
			status, matrixVal = ull.getVect3DDouble(self.idx, path, cpopath + 'matrix')
			check_status(status)
			for i in range(nbslice):
				slice = permeabilitystructurecomplexgrid_scalar(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = griduidVal
				slice.subgrid = subgridVal
				slice.scalar = scalarVal
				slice.vector = vectorVal
				slice.matrix = matrixVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type permeabilitystructurecomplexgrid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type permeabilitystructurecomplexgrid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type permeabilitystructurecomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type permeabilitystructurecomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class jstructurecomplexgrid_vector:
	'''
	class jstructurecomplexgrid_vector
	Current density vector in the element specified by the grid representation. [A/m^2]

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- label : str
	   Label describing the data
	- comp : class compstruct_arraycomplexgrid_scalar: array of compstruct_arraycomplexgrid_scalarObj objects
	   Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element
	- align : numpy.ndarray 1D with int)
	   Alignment flag for vector components. Integer vector (number of vector components).
	- alignid : list of str
	   Alignment id for vector components. String vector (number of vector components).
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	'''

	def __init__(self, base_path_in='j'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.label = ''
		self.comp = compstruct_arraycomplexgrid_scalar('comp')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']
		self.basis = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jstructurecomplexgrid_vector\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute comp\n ' + self.comp.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comp.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jstructurecomplexgrid_vector, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comp.cpoTime = self.cpoTime
		self.comp.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jstructurecomplexgrid_vector, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comp.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jstructurecomplexgrid_vector, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'label', self.label)
		check_status(status)
		self.comp.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'align', numpy.array(self.align).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'alignid', self.alignid, False)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'basis', self.basis)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jstructurecomplexgrid_vector, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid = ull.getInt(self.idx, path, cpopath + 'griduid')
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		status, ret_label = ull.getString(self.idx, path, cpopath + 'label')
		check_status(status)
		if not status:
			self.label = ret_label
		self.comp.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_align = ull.getVect1DInt(self.idx, path, cpopath + 'align')
		check_status(status)
		if not status:
			self.align = ret_align
		status, ret_alignid = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
		check_status(status)
		if not status:
			self.alignid = ret_alignid
		status, ret_basis = ull.getInt(self.idx, path, cpopath + 'basis')
		check_status(status)
		if not status:
			self.basis = ret_basis

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jstructurecomplexgrid_vector, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidVal = ull.getInt(self.idx, path, cpopath + 'griduid')
			check_status(status)
			status, labelVal = ull.getString(self.idx, path, cpopath + 'label')
			check_status(status)
			compList = self.comp.build_non_resampled_data(path, cpopath, nbslice)
			status, alignVal = ull.getVect1DInt(self.idx, path, cpopath + 'align')
			check_status(status)
			status, alignidVal = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
			check_status(status)
			status, basisVal = ull.getInt(self.idx, path, cpopath + 'basis')
			check_status(status)
			for i in range(nbslice):
				slice = jstructurecomplexgrid_vector(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = griduidVal
				slice.label = labelVal
				slice.comp = compList[i]
				slice.align = alignVal
				slice.alignid = alignidVal
				slice.basis = basisVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructurecomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructurecomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructurecomplexgrid_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		obj = self.comp.putNonTimedElt(path, cpopath + 'comp', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructurecomplexgrid_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
		self.comp.getNonTimedElt(path, cpopath + 'comp', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		status, ret_align = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'align', i)
		check_status(status)
		if not status:
			self.align = ret_align
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		status, ret_alignid = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'alignid', i)
		check_status(status)
		if not status:
			self.alignid = ret_alignid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'label')
		ull.deleteData(self.idx, path, cpopath + 'comp')
		ull.deleteData(self.idx, path, cpopath + 'align')
		ull.deleteData(self.idx, path, cpopath + 'alignid')
		ull.deleteData(self.idx, path, cpopath + 'basis')


class compstruct_arraycomplexgrid_scalar:
	'''
	class compstruct_arraycomplexgrid_scalar
	Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element

	Attributes:
	- array : list of compstruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='comp'):
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
		ret = space + 'class compstruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'compstruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(compstruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(compstruct_arraycomplexgrid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(compstruct_arraycomplexgrid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = compstruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class compstruct_arraycomplexgrid_scalarObj:
	'''
	class compstruct_arraycomplexgrid_scalarObj
	Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='comp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compstruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class wall3dstruct_arraywall3d:
	'''
	class wall3dstruct_arraywall3d
	3D wall descriptions; Array of structures (number of wall descriptions). Replicate this element for each type of possible physics or engineering configurations necessary (gas tight vs wall with ports and holes, coarse vs fine representation, ...). Time-dependent

	Attributes:
	- array : list of wall3dstruct_arraywall3dObj 
	'''

	def __init__(self, base_path_in='wall3d'):
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
		ret = space + 'class wall3dstruct_arraywall3d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'wall3dstruct_arraywall3d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(wall3dstruct_arraywall3dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function putSlice') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function getSlice') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(wall3dstruct_arraywall3d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(wall3dstruct_arraywall3d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = wall3dstruct_arraywall3d(self.base_path)
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type wall3dstruct_arraywall3d, run function getNonTimedElt') 
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


class wall3dstruct_arraywall3dObj:
	'''
	class wall3dstruct_arraywall3dObj
	3D wall descriptions; Array of structures (number of wall descriptions). Replicate this element for each type of possible physics or engineering configurations necessary (gas tight vs wall with ports and holes, coarse vs fine representation, ...). Time-dependent

	Attributes:
	- wall_id : class wall_idstructureidentifier
	   Identify the type of wall - 0 for gas tight and 1 for a wall with holes/open ports
	- grid : class gridstructurecomplexgrid
	   Grid description
	- plasma : class plasmastruct_arrayplasmaComplexType: array of plasmastruct_arrayplasmaComplexTypeObj objects
	   Description of incoming plasma for every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the corresponding subgrid with index i in wall/wall3d/grid. Time-dependent
	- wall_state : class wall_statestruct_arraywall_unitsComplexType: array of wall_statestruct_arraywall_unitsComplexTypeObj objects
	   Dynamic wall state of every wall component. Array of structures (number of wall components). The geometry of the wall component with index i is given by the corresponding subgrid with index i in wall/wall3d/grid. Time-dependent
	- basis_index : int
	   Index of basis vectors in wall/wall3d/grid/basis used to define vector quantities e.g. in plasma.
	'''

	def __init__(self, base_path_in='wall3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.wall_id = wall_idstructureidentifier('wall_id')
		self.grid = gridstructurecomplexgrid('grid')
		self.plasma = plasmastruct_arrayplasmaComplexType('plasma')
		self.wall_state = wall_statestruct_arraywall_unitsComplexType('wall_state')
		self.basis_index = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall3dstruct_arraywall3dObj\n'
		ret = ret + space + 'Attribute wall_id\n ' + self.wall_id.__str__(depth+1)
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		ret = ret + space + 'Attribute plasma\n ' + self.plasma.__str__(depth+1)
		ret = ret + space + 'Attribute wall_state\n ' + self.wall_state.__str__(depth+1)
		ret = ret + space + 'Attribute basis_index: ' + str(self.basis_index) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.wall_id.setExpIdx(idx)
		self.grid.setExpIdx(idx)
		self.plasma.setExpIdx(idx)
		self.wall_state.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall3dstruct_arraywall3dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.plasma.putTimedElt(path, cpopath + 'plasma', i, obj)
		obj = self.wall_state.putTimedElt(path, cpopath + 'wall_state', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall3dstruct_arraywall3dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
		self.plasma.getTimedElt(path, cpopath + 'plasma', i, obj)
		self.wall_state.getTimedElt(path, cpopath + 'wall_state', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall3dstruct_arraywall3dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.wall_id.putNonTimedElt(path, cpopath + 'wall_id', i, obj)
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.plasma.putNonTimedElt(path, cpopath + 'plasma', i, obj)
		obj = self.wall_state.putNonTimedElt(path, cpopath + 'wall_state', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis_index') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis_index', i, self.basis_index)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall3dstruct_arraywall3dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.wall_id.getNonTimedElt(path, cpopath + 'wall_id', i, obj)
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)
		self.plasma.getNonTimedElt(path, cpopath + 'plasma', i, obj)
		self.wall_state.getNonTimedElt(path, cpopath + 'wall_state', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis_index') 
			print ('obj = ' + str(obj))
		status, ret_basis_index = ull.getIntFromObject(self.idx, obj, cpopath + 'basis_index', i)
		check_status(status)
		if not status:
			self.basis_index = ret_basis_index


class gridstructurecomplexgrid:
	'''
	class gridstructurecomplexgrid
	Grid description

	Attributes:
	- uid : int
	   Unique index of this grid. Used for handling multiple grids
	- id : str
	   Name / identifier string for this grid
	- spaces : class spacesstruct_arraycomplexgrid_space: array of spacesstruct_arraycomplexgrid_spaceObj objects
	   Definitions of grid spaces. Array of structures (number of spaces)
	- subgrids : class subgridsstruct_arraycomplexgrid_subgrid: array of subgridsstruct_arraycomplexgrid_subgridObj objects
	   Definitions of subgrids. Array of structures (number of subgrids)
	- metric : class metricstructurecomplexgrid_metric
	   Metric coefficients
	- geo : class geostruct_arraycomplexgrid_geo_global: array of geostruct_arraycomplexgrid_geo_globalObj objects
	   Geometry data for implicit objects
	- bases : class basesstruct_arraycomplexgrid_vector: array of basesstruct_arraycomplexgrid_vectorObj objects
	   Vector bases. Used for aligned vector representation. Time-dependent (added systematically for the COMP child inheritance of that property). Array of structures (number of bases)
	'''

	def __init__(self, base_path_in='grid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.uid = EMPTY_INT
		self.id = ''
		self.spaces = spacesstruct_arraycomplexgrid_space('spaces')
		self.subgrids = subgridsstruct_arraycomplexgrid_subgrid('subgrids')
		self.metric = metricstructurecomplexgrid_metric('metric')
		self.geo = geostruct_arraycomplexgrid_geo_global('geo')
		self.bases = basesstruct_arraycomplexgrid_vector('bases')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class gridstructurecomplexgrid\n'
		ret = ret + space + 'Attribute uid: ' + str(self.uid) + '\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute spaces\n ' + self.spaces.__str__(depth+1)
		ret = ret + space + 'Attribute subgrids\n ' + self.subgrids.__str__(depth+1)
		ret = ret + space + 'Attribute metric\n ' + self.metric.__str__(depth+1)
		ret = ret + space + 'Attribute geo\n ' + self.geo.__str__(depth+1)
		ret = ret + space + 'Attribute bases\n ' + self.bases.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.spaces.setExpIdx(idx)
		self.subgrids.setExpIdx(idx)
		self.metric.setExpIdx(idx)
		self.geo.setExpIdx(idx)
		self.bases.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spaces.cpoTime = self.cpoTime
		self.spaces.putSlice(path, cpopath)
		self.subgrids.cpoTime = self.cpoTime
		self.subgrids.putSlice(path, cpopath)
		self.metric.cpoTime = self.cpoTime
		self.metric.putSlice(path, cpopath)
		self.geo.cpoTime = self.cpoTime
		self.geo.putSlice(path, cpopath)
		self.bases.cpoTime = self.cpoTime
		self.bases.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spaces.replaceLastSlice(path, cpopath)
		self.subgrids.replaceLastSlice(path, cpopath)
		self.metric.replaceLastSlice(path, cpopath)
		self.geo.replaceLastSlice(path, cpopath)
		self.bases.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'uid', self.uid)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'id', self.id)
		check_status(status)
		self.spaces.putNonTimed(path, cpopath)
		self.subgrids.putNonTimed(path, cpopath)
		self.metric.putNonTimed(path, cpopath)
		self.geo.putNonTimed(path, cpopath)
		self.bases.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_uid = ull.getInt(self.idx, path, cpopath + 'uid')
		check_status(status)
		if not status:
			self.uid = ret_uid
		status, ret_id = ull.getString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		self.spaces.getSlice(path, cpopath, inTime, interpolMode)
		self.subgrids.getSlice(path, cpopath, inTime, interpolMode)
		self.metric.getSlice(path, cpopath, inTime, interpolMode)
		self.geo.getSlice(path, cpopath, inTime, interpolMode)
		self.bases.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, uidVal = ull.getInt(self.idx, path, cpopath + 'uid')
			check_status(status)
			status, idVal = ull.getString(self.idx, path, cpopath + 'id')
			check_status(status)
			spacesList = self.spaces.build_non_resampled_data(path, cpopath, nbslice)
			subgridsList = self.subgrids.build_non_resampled_data(path, cpopath, nbslice)
			metricList = self.metric.build_non_resampled_data(path, cpopath, nbslice)
			geoList = self.geo.build_non_resampled_data(path, cpopath, nbslice)
			basesList = self.bases.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = gridstructurecomplexgrid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.uid = uidVal
				slice.id = idVal
				slice.spaces = spacesList[i]
				slice.subgrids = subgridsList[i]
				slice.metric = metricList[i]
				slice.geo = geoList[i]
				slice.bases = basesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.bases.putTimedElt(path, cpopath + 'bases', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.bases.getTimedElt(path, cpopath + 'bases', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'uid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'uid', i, self.uid)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		obj = self.spaces.putNonTimedElt(path, cpopath + 'spaces', i, obj)
		obj = self.subgrids.putNonTimedElt(path, cpopath + 'subgrids', i, obj)
		obj = self.metric.putNonTimedElt(path, cpopath + 'metric', i, obj)
		obj = self.geo.putNonTimedElt(path, cpopath + 'geo', i, obj)
		obj = self.bases.putNonTimedElt(path, cpopath + 'bases', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'uid') 
			print ('obj = ' + str(obj))
		status, ret_uid = ull.getIntFromObject(self.idx, obj, cpopath + 'uid', i)
		check_status(status)
		if not status:
			self.uid = ret_uid
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		self.spaces.getNonTimedElt(path, cpopath + 'spaces', i, obj)
		self.subgrids.getNonTimedElt(path, cpopath + 'subgrids', i, obj)
		self.metric.getNonTimedElt(path, cpopath + 'metric', i, obj)
		self.geo.getNonTimedElt(path, cpopath + 'geo', i, obj)
		self.bases.getNonTimedElt(path, cpopath + 'bases', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'uid')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'spaces')
		ull.deleteData(self.idx, path, cpopath + 'subgrids')
		self.metric.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'geo')
		ull.deleteData(self.idx, path, cpopath + 'bases')


class spacesstruct_arraycomplexgrid_space:
	'''
	class spacesstruct_arraycomplexgrid_space
	Definitions of grid spaces. Array of structures (number of spaces)

	Attributes:
	- array : list of spacesstruct_arraycomplexgrid_spaceObj 
	'''

	def __init__(self, base_path_in='spaces'):
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
		ret = space + 'class spacesstruct_arraycomplexgrid_space\n'
		for i in range(len(self.array)):
			ret = ret + space + 'spacesstruct_arraycomplexgrid_space[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(spacesstruct_arraycomplexgrid_spaceObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function getSlice') 
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(spacesstruct_arraycomplexgrid_space(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = spacesstruct_arraycomplexgrid_space(self.base_path)
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function getNonTimedElt') 
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


class spacesstruct_arraycomplexgrid_spaceObj:
	'''
	class spacesstruct_arraycomplexgrid_spaceObj
	Definitions of grid spaces. Array of structures (number of spaces)

	Attributes:
	- geotype : numpy.ndarray 1D with int)
	   Type of space geometry (id flags). Flags defining how the geometry (objects.geo) fields associated with; space objects are to be interpreted. Array (number of geometries defined for  this space),; first dimension: geometry index. A flag value of GRID_UNDEFINED=0 indicates the standard interpretation for; the given coordinates.
	- geotypeid : list of str
	   Type of space geometries (id string). See geotype.
	- coordtype : numpy.ndarray 2D with int
	   Type of coordinates describing the physical space. Vector (number of space dimensions); The size of coordtype defines the dimension of the space.; For predefined integer constants for standard coordinates see; the documentation of the grid service library.
	- objects : class objectsstruct_arrayobjects: array of objectsstruct_arrayobjectsObj objects
	   Definition of the space objects.; Array of structures (dimension of highest-dimensional objects).; First dimension: dimension of the objects (1=nodes, 2=edges, 3=faces, 4=cells/volumes, ...)
	- xpoints : numpy.ndarray 1D with int)
	   List of indices of all nodes which are x-points. Vector (number of x-points)
	'''

	def __init__(self, base_path_in='spaces'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geotype = numpy.zeros(0, numpy.int32, order='C')
		self.geotypeid = ['']
		self.coordtype = numpy.zeros((0,0), numpy.int32, order='C')
		self.objects = objectsstruct_arrayobjects('objects')
		self.xpoints = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spacesstruct_arraycomplexgrid_spaceObj\n'
		s = self.geotype.__str__()
		ret = ret + space + 'Attribute geotype\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.geotypeid.__str__()
		ret = ret + space + 'Attribute geotypeid\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coordtype.__str__()
		ret = ret + space + 'Attribute coordtype\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute objects\n ' + self.objects.__str__(depth+1)
		s = self.xpoints.__str__()
		ret = ret + space + 'Attribute xpoints\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.objects.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spacesstruct_arraycomplexgrid_spaceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'geotype', i, numpy.array(self.geotype).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'geotypeid', i, self.geotypeid)
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'coordtype', i, numpy.array(self.coordtype).astype(numpy.int32))
		obj = self.objects.putNonTimedElt(path, cpopath + 'objects', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'xpoints') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'xpoints', i, numpy.array(self.xpoints).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spacesstruct_arraycomplexgrid_spaceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		status, ret_geotype = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'geotype', i)
		check_status(status)
		if not status:
			self.geotype = ret_geotype
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		status, ret_geotypeid = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'geotypeid', i)
		check_status(status)
		if not status:
			self.geotypeid = ret_geotypeid
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		status, ret_coordtype = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'coordtype', i)
		check_status(status)
		if not status:
			self.coordtype = ret_coordtype
		self.objects.getNonTimedElt(path, cpopath + 'objects', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'xpoints') 
			print ('obj = ' + str(obj))
		status, ret_xpoints = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'xpoints', i)
		check_status(status)
		if not status:
			self.xpoints = ret_xpoints


class objectsstruct_arrayobjects:
	'''
	class objectsstruct_arrayobjects
	Definition of the space objects.; Array of structures (dimension of highest-dimensional objects).; First dimension: dimension of the objects (1=nodes, 2=edges, 3=faces, 4=cells/volumes, ...)

	Attributes:
	- array : list of objectsstruct_arrayobjectsObj 
	'''

	def __init__(self, base_path_in='objects'):
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
		ret = space + 'class objectsstruct_arrayobjects\n'
		for i in range(len(self.array)):
			ret = ret + space + 'objectsstruct_arrayobjects[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(objectsstruct_arrayobjectsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function getSlice') 
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(objectsstruct_arrayobjects(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = objectsstruct_arrayobjects(self.base_path)
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function getNonTimedElt') 
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


class objectsstruct_arrayobjectsObj:
	'''
	class objectsstruct_arrayobjectsObj
	Definition of the space objects.; Array of structures (dimension of highest-dimensional objects).; First dimension: dimension of the objects (1=nodes, 2=edges, 3=faces, 4=cells/volumes, ...)

	Attributes:
	- boundary : numpy.ndarray 2D with int
	   Lists of (n-1)-dimensional space objects defining the boundary of an n-dimensional space object.; Matrix(number of objects of dimension n, maximum number of boundary objects).; First dimension: object index, second dimension: boundary object index
	- neighbour : numpy.ndarray 3D with int
	   Connectivity information. Array (number of objects, maximum number of boundaries per object, maximum number of neighbours per boundary).; Stores the indices of the n-dimensional objects adjacent to the given n-dimensional object.;An object can possibly have multiple neighbours on every boundary.; First dimension: object index, second dimension: boundary index, third dimension: neighbour index on the boundary.
	- geo : numpy.ndarray 4D with float
	   Geometry data matrix associated with every object. Float array (number of objects, number of geometry coeff. 1, number of geometry coeff. 2, number of geometries).; The exact definition depends on the geometry type of the space (complexgrid_space.geotype).; First dimension: object index, second+third dimension: geometry coefficient matrix row+column, third dimension: geometry index (for definition of multiple geometries).
	- measure : numpy.ndarray 2D with float
	   Measure of space objects, i.e. physical size (length for 1d, area for 2d, volume for 3d objects,...). [m^dim].; First dimension: object index, second dimension: geometry index
	'''

	def __init__(self, base_path_in='objects'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.boundary = numpy.zeros((0,0), numpy.int32, order='C')
		self.neighbour = numpy.zeros((0,0,0), numpy.int32, order='C')
		self.geo = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.measure = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class objectsstruct_arrayobjectsObj\n'
		s = self.boundary.__str__()
		ret = ret + space + 'Attribute boundary\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.neighbour.__str__()
		ret = ret + space + 'Attribute neighbour\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.geo.__str__()
		ret = ret + space + 'Attribute geo\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.measure.__str__()
		ret = ret + space + 'Attribute measure\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type objectsstruct_arrayobjectsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'boundary') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'boundary', i, numpy.array(self.boundary).astype(numpy.int32))
		if (dev()):
			print ('putVect3DIntInObject : ' + cpopath + 'neighbour') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DIntInObject(self.idx, obj, cpopath + 'neighbour', i, numpy.array(self.neighbour).astype(numpy.int32))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'geo') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'geo', i, numpy.array(self.geo).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'measure') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'measure', i, numpy.array(self.measure).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type objectsstruct_arrayobjectsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'boundary') 
			print ('obj = ' + str(obj))
		status, ret_boundary = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'boundary', i)
		check_status(status)
		if not status:
			self.boundary = ret_boundary
		if (dev()):
			print ('getVect3DIntInObject : ' + cpopath + 'neighbour') 
			print ('obj = ' + str(obj))
		status, ret_neighbour = ull.getVect3DIntFromObject(self.idx, obj, cpopath + 'neighbour', i)
		check_status(status)
		if not status:
			self.neighbour = ret_neighbour
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'geo') 
			print ('obj = ' + str(obj))
		status, ret_geo = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'geo', i)
		check_status(status)
		if not status:
			self.geo = ret_geo
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'measure') 
			print ('obj = ' + str(obj))
		status, ret_measure = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'measure', i)
		check_status(status)
		if not status:
			self.measure = ret_measure


class subgridsstruct_arraycomplexgrid_subgrid:
	'''
	class subgridsstruct_arraycomplexgrid_subgrid
	Definitions of subgrids. Array of structures (number of subgrids)

	Attributes:
	- array : list of subgridsstruct_arraycomplexgrid_subgridObj 
	'''

	def __init__(self, base_path_in='subgrids'):
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
		ret = space + 'class subgridsstruct_arraycomplexgrid_subgrid\n'
		for i in range(len(self.array)):
			ret = ret + space + 'subgridsstruct_arraycomplexgrid_subgrid[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(subgridsstruct_arraycomplexgrid_subgridObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function getSlice') 
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(subgridsstruct_arraycomplexgrid_subgrid(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = subgridsstruct_arraycomplexgrid_subgrid(self.base_path)
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function getNonTimedElt') 
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


class subgridsstruct_arraycomplexgrid_subgridObj:
	'''
	class subgridsstruct_arraycomplexgrid_subgridObj
	Definitions of subgrids. Array of structures (number of subgrids)

	Attributes:
	- id : str
	   ID string (name) of the subgrid.
	- list : class liststruct_arraycomplexgrid_objectlist: array of liststruct_arraycomplexgrid_objectlistObj objects
	   List of object lists. Array of structures (number of object lists).
	'''

	def __init__(self, base_path_in='subgrids'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.list = liststruct_arraycomplexgrid_objectlist('list')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class subgridsstruct_arraycomplexgrid_subgridObj\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute list\n ' + self.list.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.list.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type subgridsstruct_arraycomplexgrid_subgridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		obj = self.list.putNonTimedElt(path, cpopath + 'list', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type subgridsstruct_arraycomplexgrid_subgridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		self.list.getNonTimedElt(path, cpopath + 'list', i, obj)


class liststruct_arraycomplexgrid_objectlist:
	'''
	class liststruct_arraycomplexgrid_objectlist
	List of object lists. Array of structures (number of object lists).

	Attributes:
	- array : list of liststruct_arraycomplexgrid_objectlistObj 
	'''

	def __init__(self, base_path_in='list'):
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
		ret = space + 'class liststruct_arraycomplexgrid_objectlist\n'
		for i in range(len(self.array)):
			ret = ret + space + 'liststruct_arraycomplexgrid_objectlist[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(liststruct_arraycomplexgrid_objectlistObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function getSlice') 
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(liststruct_arraycomplexgrid_objectlist(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = liststruct_arraycomplexgrid_objectlist(self.base_path)
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function getNonTimedElt') 
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


class liststruct_arraycomplexgrid_objectlistObj:
	'''
	class liststruct_arraycomplexgrid_objectlistObj
	List of object lists. Array of structures (number of object lists).

	Attributes:
	- cls : numpy.ndarray 1D with int)
	   Class tuple of the grid objects in this object list. Vector (number of grid spaces)
	- indset : class indsetstruct_arraycomplexgrid_indexlist: array of indsetstruct_arraycomplexgrid_indexlistObj objects
	   Implicit list of the object indices.;  Array of structures (number of grid spaces = length of index tuple). Every index of the index tuple is described by an index set, which defines either a list of index values or a range of index values.
	- ind : numpy.ndarray 2D with int
	   Explicit list of index tuples. Matrix (number of objects, number of spaces in grid).; First dimension: object index, second dimension: index tuple/space index.; If this field is defined and has nonzero size, the object list is understood to be explicit.
	'''

	def __init__(self, base_path_in='list'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.cls = numpy.zeros(0, numpy.int32, order='C')
		self.indset = indsetstruct_arraycomplexgrid_indexlist('indset')
		self.ind = numpy.zeros((0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class liststruct_arraycomplexgrid_objectlistObj\n'
		s = self.cls.__str__()
		ret = ret + space + 'Attribute cls\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute indset\n ' + self.indset.__str__(depth+1)
		s = self.ind.__str__()
		ret = ret + space + 'Attribute ind\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.indset.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type liststruct_arraycomplexgrid_objectlistObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'cls') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'cls', i, numpy.array(self.cls).astype(numpy.int32))
		obj = self.indset.putNonTimedElt(path, cpopath + 'indset', i, obj)
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'ind', i, numpy.array(self.ind).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type liststruct_arraycomplexgrid_objectlistObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'cls') 
			print ('obj = ' + str(obj))
		status, ret_cls = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'cls', i)
		check_status(status)
		if not status:
			self.cls = ret_cls
		self.indset.getNonTimedElt(path, cpopath + 'indset', i, obj)
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		status, ret_ind = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'ind', i)
		check_status(status)
		if not status:
			self.ind = ret_ind


class indsetstruct_arraycomplexgrid_indexlist:
	'''
	class indsetstruct_arraycomplexgrid_indexlist
	Implicit list of the object indices.;  Array of structures (number of grid spaces = length of index tuple). Every index of the index tuple is described by an index set, which defines either a list of index values or a range of index values.

	Attributes:
	- array : list of indsetstruct_arraycomplexgrid_indexlistObj 
	'''

	def __init__(self, base_path_in='indset'):
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
		ret = space + 'class indsetstruct_arraycomplexgrid_indexlist\n'
		for i in range(len(self.array)):
			ret = ret + space + 'indsetstruct_arraycomplexgrid_indexlist[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(indsetstruct_arraycomplexgrid_indexlistObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function getSlice') 
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(indsetstruct_arraycomplexgrid_indexlist(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = indsetstruct_arraycomplexgrid_indexlist(self.base_path)
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function getNonTimedElt') 
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


class indsetstruct_arraycomplexgrid_indexlistObj:
	'''
	class indsetstruct_arraycomplexgrid_indexlistObj
	Implicit list of the object indices.;  Array of structures (number of grid spaces = length of index tuple). Every index of the index tuple is described by an index set, which defines either a list of index values or a range of index values.

	Attributes:
	- range : numpy.ndarray 1D with int)
	   Defines an index range enumerating from range[1] to range[2] (with both range[1] and range[2] included). If additionally a third value range(3) is given, it is used as a stride. If it is omitted, a stride of 1 is assumed. Vector(3)
	- ind : numpy.ndarray 1D with int)
	   An explicit list of indices. If this member is defined and has nonzero size, the list is assumed to be explicit. Vector(length of explicit index list)
	'''

	def __init__(self, base_path_in='indset'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.range = numpy.zeros(0, numpy.int32, order='C')
		self.ind = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class indsetstruct_arraycomplexgrid_indexlistObj\n'
		s = self.range.__str__()
		ret = ret + space + 'Attribute range\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ind.__str__()
		ret = ret + space + 'Attribute ind\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type indsetstruct_arraycomplexgrid_indexlistObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'range') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'range', i, numpy.array(self.range).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ind', i, numpy.array(self.ind).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type indsetstruct_arraycomplexgrid_indexlistObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'range') 
			print ('obj = ' + str(obj))
		status, ret_range = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'range', i)
		check_status(status)
		if not status:
			self.range = ret_range
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		status, ret_ind = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ind', i)
		check_status(status)
		if not status:
			self.ind = ret_ind


class metricstructurecomplexgrid_metric:
	'''
	class metricstructurecomplexgrid_metric
	Metric coefficients

	Attributes:
	- measure : class measurestruct_arraycomplexgrid_scalar: array of measurestruct_arraycomplexgrid_scalarObj objects
	   Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects). [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)
	- g11 : class g11struct_arraycomplexgrid_scalar: array of g11struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g11. Array of structures (number of subgrids this information is stored on)
	- g12 : class g12struct_arraycomplexgrid_scalar: array of g12struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g12. Array of structures (number of subgrids this information is stored on)
	- g13 : class g13struct_arraycomplexgrid_scalar: array of g13struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g13. Array of structures (number of subgrids this information is stored on)
	- g22 : class g22struct_arraycomplexgrid_scalar: array of g22struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g22. Array of structures (number of subgrids this information is stored on)
	- g23 : class g23struct_arraycomplexgrid_scalar: array of g23struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g23. Array of structures (number of subgrids this information is stored on)
	- g33 : class g33struct_arraycomplexgrid_scalar: array of g33struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g33. Array of structures (number of subgrids this information is stored on)
	- jacobian : class jacobianstruct_arraycomplexgrid_scalar: array of jacobianstruct_arraycomplexgrid_scalarObj objects
	   Jacobian. Array of structures (number of subgrids this information is stored on)
	'''

	def __init__(self, base_path_in='metric'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measure = measurestruct_arraycomplexgrid_scalar('measure')
		self.g11 = g11struct_arraycomplexgrid_scalar('g11')
		self.g12 = g12struct_arraycomplexgrid_scalar('g12')
		self.g13 = g13struct_arraycomplexgrid_scalar('g13')
		self.g22 = g22struct_arraycomplexgrid_scalar('g22')
		self.g23 = g23struct_arraycomplexgrid_scalar('g23')
		self.g33 = g33struct_arraycomplexgrid_scalar('g33')
		self.jacobian = jacobianstruct_arraycomplexgrid_scalar('jacobian')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class metricstructurecomplexgrid_metric\n'
		ret = ret + space + 'Attribute measure\n ' + self.measure.__str__(depth+1)
		ret = ret + space + 'Attribute g11\n ' + self.g11.__str__(depth+1)
		ret = ret + space + 'Attribute g12\n ' + self.g12.__str__(depth+1)
		ret = ret + space + 'Attribute g13\n ' + self.g13.__str__(depth+1)
		ret = ret + space + 'Attribute g22\n ' + self.g22.__str__(depth+1)
		ret = ret + space + 'Attribute g23\n ' + self.g23.__str__(depth+1)
		ret = ret + space + 'Attribute g33\n ' + self.g33.__str__(depth+1)
		ret = ret + space + 'Attribute jacobian\n ' + self.jacobian.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.measure.setExpIdx(idx)
		self.g11.setExpIdx(idx)
		self.g12.setExpIdx(idx)
		self.g13.setExpIdx(idx)
		self.g22.setExpIdx(idx)
		self.g23.setExpIdx(idx)
		self.g33.setExpIdx(idx)
		self.jacobian.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.cpoTime = self.cpoTime
		self.measure.putSlice(path, cpopath)
		self.g11.cpoTime = self.cpoTime
		self.g11.putSlice(path, cpopath)
		self.g12.cpoTime = self.cpoTime
		self.g12.putSlice(path, cpopath)
		self.g13.cpoTime = self.cpoTime
		self.g13.putSlice(path, cpopath)
		self.g22.cpoTime = self.cpoTime
		self.g22.putSlice(path, cpopath)
		self.g23.cpoTime = self.cpoTime
		self.g23.putSlice(path, cpopath)
		self.g33.cpoTime = self.cpoTime
		self.g33.putSlice(path, cpopath)
		self.jacobian.cpoTime = self.cpoTime
		self.jacobian.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.replaceLastSlice(path, cpopath)
		self.g11.replaceLastSlice(path, cpopath)
		self.g12.replaceLastSlice(path, cpopath)
		self.g13.replaceLastSlice(path, cpopath)
		self.g22.replaceLastSlice(path, cpopath)
		self.g23.replaceLastSlice(path, cpopath)
		self.g33.replaceLastSlice(path, cpopath)
		self.jacobian.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.putNonTimed(path, cpopath)
		self.g11.putNonTimed(path, cpopath)
		self.g12.putNonTimed(path, cpopath)
		self.g13.putNonTimed(path, cpopath)
		self.g22.putNonTimed(path, cpopath)
		self.g23.putNonTimed(path, cpopath)
		self.g33.putNonTimed(path, cpopath)
		self.jacobian.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.getSlice(path, cpopath, inTime, interpolMode)
		self.g11.getSlice(path, cpopath, inTime, interpolMode)
		self.g12.getSlice(path, cpopath, inTime, interpolMode)
		self.g13.getSlice(path, cpopath, inTime, interpolMode)
		self.g22.getSlice(path, cpopath, inTime, interpolMode)
		self.g23.getSlice(path, cpopath, inTime, interpolMode)
		self.g33.getSlice(path, cpopath, inTime, interpolMode)
		self.jacobian.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			measureList = self.measure.build_non_resampled_data(path, cpopath, nbslice)
			g11List = self.g11.build_non_resampled_data(path, cpopath, nbslice)
			g12List = self.g12.build_non_resampled_data(path, cpopath, nbslice)
			g13List = self.g13.build_non_resampled_data(path, cpopath, nbslice)
			g22List = self.g22.build_non_resampled_data(path, cpopath, nbslice)
			g23List = self.g23.build_non_resampled_data(path, cpopath, nbslice)
			g33List = self.g33.build_non_resampled_data(path, cpopath, nbslice)
			jacobianList = self.jacobian.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = metricstructurecomplexgrid_metric(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measure = measureList[i]
				slice.g11 = g11List[i]
				slice.g12 = g12List[i]
				slice.g13 = g13List[i]
				slice.g22 = g22List[i]
				slice.g23 = g23List[i]
				slice.g33 = g33List[i]
				slice.jacobian = jacobianList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.measure.putNonTimedElt(path, cpopath + 'measure', i, obj)
		obj = self.g11.putNonTimedElt(path, cpopath + 'g11', i, obj)
		obj = self.g12.putNonTimedElt(path, cpopath + 'g12', i, obj)
		obj = self.g13.putNonTimedElt(path, cpopath + 'g13', i, obj)
		obj = self.g22.putNonTimedElt(path, cpopath + 'g22', i, obj)
		obj = self.g23.putNonTimedElt(path, cpopath + 'g23', i, obj)
		obj = self.g33.putNonTimedElt(path, cpopath + 'g33', i, obj)
		obj = self.jacobian.putNonTimedElt(path, cpopath + 'jacobian', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.measure.getNonTimedElt(path, cpopath + 'measure', i, obj)
		self.g11.getNonTimedElt(path, cpopath + 'g11', i, obj)
		self.g12.getNonTimedElt(path, cpopath + 'g12', i, obj)
		self.g13.getNonTimedElt(path, cpopath + 'g13', i, obj)
		self.g22.getNonTimedElt(path, cpopath + 'g22', i, obj)
		self.g23.getNonTimedElt(path, cpopath + 'g23', i, obj)
		self.g33.getNonTimedElt(path, cpopath + 'g33', i, obj)
		self.jacobian.getNonTimedElt(path, cpopath + 'jacobian', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measure')
		ull.deleteData(self.idx, path, cpopath + 'g11')
		ull.deleteData(self.idx, path, cpopath + 'g12')
		ull.deleteData(self.idx, path, cpopath + 'g13')
		ull.deleteData(self.idx, path, cpopath + 'g22')
		ull.deleteData(self.idx, path, cpopath + 'g23')
		ull.deleteData(self.idx, path, cpopath + 'g33')
		ull.deleteData(self.idx, path, cpopath + 'jacobian')


class measurestruct_arraycomplexgrid_scalar:
	'''
	class measurestruct_arraycomplexgrid_scalar
	Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects). [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of measurestruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='measure'):
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
		ret = space + 'class measurestruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'measurestruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(measurestruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(measurestruct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = measurestruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class measurestruct_arraycomplexgrid_scalarObj:
	'''
	class measurestruct_arraycomplexgrid_scalarObj
	Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects). [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='measure'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class measurestruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g11struct_arraycomplexgrid_scalar:
	'''
	class g11struct_arraycomplexgrid_scalar
	Metric coefficients g11. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g11struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g11'):
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
		ret = space + 'class g11struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g11struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g11struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g11struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g11struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g11struct_arraycomplexgrid_scalarObj:
	'''
	class g11struct_arraycomplexgrid_scalarObj
	Metric coefficients g11. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g11'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g11struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g11struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g11struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g12struct_arraycomplexgrid_scalar:
	'''
	class g12struct_arraycomplexgrid_scalar
	Metric coefficients g12. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g12struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g12'):
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
		ret = space + 'class g12struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g12struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g12struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g12struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g12struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g12struct_arraycomplexgrid_scalarObj:
	'''
	class g12struct_arraycomplexgrid_scalarObj
	Metric coefficients g12. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g12'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g12struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g12struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g12struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g13struct_arraycomplexgrid_scalar:
	'''
	class g13struct_arraycomplexgrid_scalar
	Metric coefficients g13. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g13struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g13'):
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
		ret = space + 'class g13struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g13struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g13struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g13struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g13struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g13struct_arraycomplexgrid_scalarObj:
	'''
	class g13struct_arraycomplexgrid_scalarObj
	Metric coefficients g13. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g13'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g13struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g13struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g13struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g22struct_arraycomplexgrid_scalar:
	'''
	class g22struct_arraycomplexgrid_scalar
	Metric coefficients g22. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g22struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g22'):
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
		ret = space + 'class g22struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g22struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g22struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g22struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g22struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g22struct_arraycomplexgrid_scalarObj:
	'''
	class g22struct_arraycomplexgrid_scalarObj
	Metric coefficients g22. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g22'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g22struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g22struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g22struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g23struct_arraycomplexgrid_scalar:
	'''
	class g23struct_arraycomplexgrid_scalar
	Metric coefficients g23. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g23struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g23'):
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
		ret = space + 'class g23struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g23struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g23struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g23struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g23struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g23struct_arraycomplexgrid_scalarObj:
	'''
	class g23struct_arraycomplexgrid_scalarObj
	Metric coefficients g23. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g23'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g23struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g23struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g23struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g33struct_arraycomplexgrid_scalar:
	'''
	class g33struct_arraycomplexgrid_scalar
	Metric coefficients g33. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g33struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g33'):
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
		ret = space + 'class g33struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g33struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g33struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g33struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g33struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g33struct_arraycomplexgrid_scalarObj:
	'''
	class g33struct_arraycomplexgrid_scalarObj
	Metric coefficients g33. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g33'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g33struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g33struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g33struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class jacobianstruct_arraycomplexgrid_scalar:
	'''
	class jacobianstruct_arraycomplexgrid_scalar
	Jacobian. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of jacobianstruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='jacobian'):
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
		ret = space + 'class jacobianstruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'jacobianstruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(jacobianstruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(jacobianstruct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = jacobianstruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class jacobianstruct_arraycomplexgrid_scalarObj:
	'''
	class jacobianstruct_arraycomplexgrid_scalarObj
	Jacobian. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='jacobian'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jacobianstruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jacobianstruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jacobianstruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class geostruct_arraycomplexgrid_geo_global:
	'''
	class geostruct_arraycomplexgrid_geo_global
	Geometry data for implicit objects

	Attributes:
	- array : list of geostruct_arraycomplexgrid_geo_globalObj 
	'''

	def __init__(self, base_path_in='geo'):
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
		ret = space + 'class geostruct_arraycomplexgrid_geo_global\n'
		for i in range(len(self.array)):
			ret = ret + space + 'geostruct_arraycomplexgrid_geo_global[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(geostruct_arraycomplexgrid_geo_globalObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function getSlice') 
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(geostruct_arraycomplexgrid_geo_global(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = geostruct_arraycomplexgrid_geo_global(self.base_path)
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function getNonTimedElt') 
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


class geostruct_arraycomplexgrid_geo_globalObj:
	'''
	class geostruct_arraycomplexgrid_geo_globalObj
	Geometry data for implicit objects

	Attributes:
	- geotype : int
	   Type of geometry (id flag). A flag defining how the geometry data associated with grid objects is to be interpreted. If the field is undefined (0=GRID_UNDEFINED), the standard interpretation for; the given coordinate types is assumed.
	- geotypeid : str
	   Type of geometry (id string). 
	- coordtype : numpy.ndarray 1D with int)
	   Type of coordinates describing the physical space. Vector (number of space dimensions); The size of coordtype defines the dimension of the space.; For predefined integer constants for standard coordinates see; the documentation of the grid service library.
	- geo_matrix : class geo_matrixstruct_arraycomplexgrid_scalar: array of geo_matrixstruct_arraycomplexgrid_scalarObj objects
	   Geometry data matrix associated with implicit objects. Array of structures (number of subgrids this information is stored on); The exact definition of the stored values depends on the geometry type of the geometry complexgrid_geo_global.geotype; 
	- measure : class measurestruct_arraycomplexgrid_scalar: array of measurestruct_arraycomplexgrid_scalarObj objects
	   Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects) in this geometry. [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)
	'''

	def __init__(self, base_path_in='geo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geotype = EMPTY_INT
		self.geotypeid = ''
		self.coordtype = numpy.zeros(0, numpy.int32, order='C')
		self.geo_matrix = geo_matrixstruct_arraycomplexgrid_scalar('geo_matrix')
		self.measure = measurestruct_arraycomplexgrid_scalar('measure')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geostruct_arraycomplexgrid_geo_globalObj\n'
		ret = ret + space + 'Attribute geotype: ' + str(self.geotype) + '\n'
		ret = ret + space + 'Attribute geotypeid: ' + str(self.geotypeid) + '\n'
		s = self.coordtype.__str__()
		ret = ret + space + 'Attribute coordtype\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute geo_matrix\n ' + self.geo_matrix.__str__(depth+1)
		ret = ret + space + 'Attribute measure\n ' + self.measure.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.geo_matrix.setExpIdx(idx)
		self.measure.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geostruct_arraycomplexgrid_geo_globalObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'geotype', i, self.geotype)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'geotypeid', i, self.geotypeid)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'coordtype', i, numpy.array(self.coordtype).astype(numpy.int32))
		obj = self.geo_matrix.putNonTimedElt(path, cpopath + 'geo_matrix', i, obj)
		obj = self.measure.putNonTimedElt(path, cpopath + 'measure', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geostruct_arraycomplexgrid_geo_globalObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		status, ret_geotype = ull.getIntFromObject(self.idx, obj, cpopath + 'geotype', i)
		check_status(status)
		if not status:
			self.geotype = ret_geotype
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		status, ret_geotypeid = ull.getStringFromObject(self.idx, obj, cpopath + 'geotypeid', i)
		check_status(status)
		if not status:
			self.geotypeid = ret_geotypeid
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		status, ret_coordtype = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'coordtype', i)
		check_status(status)
		if not status:
			self.coordtype = ret_coordtype
		self.geo_matrix.getNonTimedElt(path, cpopath + 'geo_matrix', i, obj)
		self.measure.getNonTimedElt(path, cpopath + 'measure', i, obj)


class geo_matrixstruct_arraycomplexgrid_scalar:
	'''
	class geo_matrixstruct_arraycomplexgrid_scalar
	Geometry data matrix associated with implicit objects. Array of structures (number of subgrids this information is stored on); The exact definition of the stored values depends on the geometry type of the geometry complexgrid_geo_global.geotype; 

	Attributes:
	- array : list of geo_matrixstruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='geo_matrix'):
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
		ret = space + 'class geo_matrixstruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'geo_matrixstruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(geo_matrixstruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(geo_matrixstruct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = geo_matrixstruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class geo_matrixstruct_arraycomplexgrid_scalarObj:
	'''
	class geo_matrixstruct_arraycomplexgrid_scalarObj
	Geometry data matrix associated with implicit objects. Array of structures (number of subgrids this information is stored on); The exact definition of the stored values depends on the geometry type of the geometry complexgrid_geo_global.geotype; 

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='geo_matrix'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geo_matrixstruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geo_matrixstruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geo_matrixstruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class basesstruct_arraycomplexgrid_vector:
	'''
	class basesstruct_arraycomplexgrid_vector
	Vector bases. Used for aligned vector representation. Time-dependent (added systematically for the COMP child inheritance of that property). Array of structures (number of bases)

	Attributes:
	- array : list of basesstruct_arraycomplexgrid_vectorObj 
	'''

	def __init__(self, base_path_in='bases'):
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
		ret = space + 'class basesstruct_arraycomplexgrid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'basesstruct_arraycomplexgrid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(basesstruct_arraycomplexgrid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(basesstruct_arraycomplexgrid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(basesstruct_arraycomplexgrid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = basesstruct_arraycomplexgrid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function getNonTimedElt') 
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


class basesstruct_arraycomplexgrid_vectorObj:
	'''
	class basesstruct_arraycomplexgrid_vectorObj
	Vector bases. Used for aligned vector representation. Time-dependent (added systematically for the COMP child inheritance of that property). Array of structures (number of bases)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- label : str
	   Label describing the data
	- comp : class compstruct_arraycomplexgrid_scalar: array of compstruct_arraycomplexgrid_scalarObj objects
	   Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element
	- align : numpy.ndarray 1D with int)
	   Alignment flag for vector components. Integer vector (number of vector components).
	- alignid : list of str
	   Alignment id for vector components. String vector (number of vector components).
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	'''

	def __init__(self, base_path_in='bases'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.label = ''
		self.comp = compstruct_arraycomplexgrid_scalar('comp')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']
		self.basis = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class basesstruct_arraycomplexgrid_vectorObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute comp\n ' + self.comp.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comp.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		obj = self.comp.putNonTimedElt(path, cpopath + 'comp', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
		self.comp.getNonTimedElt(path, cpopath + 'comp', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		status, ret_align = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'align', i)
		check_status(status)
		if not status:
			self.align = ret_align
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		status, ret_alignid = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'alignid', i)
		check_status(status)
		if not status:
			self.alignid = ret_alignid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis


class wall_typesstruct_arraywall_types:
	'''
	class wall_typesstruct_arraywall_types
	List of reference wall types (e.g. bulk tungsten, tungsten-coated CFC, ...) ; Array of structures (number of reference wall types)

	Attributes:
	- array : list of wall_typesstruct_arraywall_typesObj 
	'''

	def __init__(self, base_path_in='wall_types'):
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
		ret = space + 'class wall_typesstruct_arraywall_types\n'
		for i in range(len(self.array)):
			ret = ret + space + 'wall_typesstruct_arraywall_types[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(wall_typesstruct_arraywall_typesObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function getSlice') 
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
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(wall_typesstruct_arraywall_types(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = wall_typesstruct_arraywall_types(self.base_path)
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
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type wall_typesstruct_arraywall_types, run function getNonTimedElt') 
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


class wall_typesstruct_arraywall_typesObj:
	'''
	class wall_typesstruct_arraywall_typesObj
	List of reference wall types (e.g. bulk tungsten, tungsten-coated CFC, ...) ; Array of structures (number of reference wall types)

	Attributes:
	- label : str
	   Label for this reference wall type
	- layers : class layersstruct_arraywall_types_layers: array of layersstruct_arraywall_types_layersObj objects
	   Engineering layers composing the wall element; array of structures (number of engineering layers). First layer is facing the plasma, increasing index means moving away from the plasma facing surface
	'''

	def __init__(self, base_path_in='wall_types'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.label = ''
		self.layers = layersstruct_arraywall_types_layers('layers')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wall_typesstruct_arraywall_typesObj\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute layers\n ' + self.layers.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.layers.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_typesstruct_arraywall_typesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		obj = self.layers.putNonTimedElt(path, cpopath + 'layers', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wall_typesstruct_arraywall_typesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
		self.layers.getNonTimedElt(path, cpopath + 'layers', i, obj)


class layersstruct_arraywall_types_layers:
	'''
	class layersstruct_arraywall_types_layers
	Engineering layers composing the wall element; array of structures (number of engineering layers). First layer is facing the plasma, increasing index means moving away from the plasma facing surface

	Attributes:
	- array : list of layersstruct_arraywall_types_layersObj 
	'''

	def __init__(self, base_path_in='layers'):
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
		ret = space + 'class layersstruct_arraywall_types_layers\n'
		for i in range(len(self.array)):
			ret = ret + space + 'layersstruct_arraywall_types_layers[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(layersstruct_arraywall_types_layersObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function getSlice') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(layersstruct_arraywall_types_layers(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = layersstruct_arraywall_types_layers(self.base_path)
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type layersstruct_arraywall_types_layers, run function getNonTimedElt') 
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


class layersstruct_arraywall_types_layersObj:
	'''
	class layersstruct_arraywall_types_layersObj
	Engineering layers composing the wall element; array of structures (number of engineering layers). First layer is facing the plasma, increasing index means moving away from the plasma facing surface

	Attributes:
	- thickness : float
	   Thickness of layer [m]
	- chem_comp : numpy.ndarray 1D with float
	   Chemical composition of the layer in terms of the chemical compounds defined in wall/design_comp/compounds. Vector of fractional concentrations.
	'''

	def __init__(self, base_path_in='layers'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.thickness = EMPTY_DOUBLE
		self.chem_comp = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class layersstruct_arraywall_types_layersObj\n'
		ret = ret + space + 'Attribute thickness: ' + str(self.thickness) + '\n'
		s = self.chem_comp.__str__()
		ret = ret + space + 'Attribute chem_comp\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type layersstruct_arraywall_types_layersObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'thickness') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'thickness', i, self.thickness)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chem_comp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chem_comp', i, numpy.array(self.chem_comp).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type layersstruct_arraywall_types_layersObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'thickness') 
			print ('obj = ' + str(obj))
		status, ret_thickness = ull.getDoubleFromObject(self.idx, obj, cpopath + 'thickness', i)
		check_status(status)
		if not status:
			self.thickness = ret_thickness
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chem_comp') 
			print ('obj = ' + str(obj))
		status, ret_chem_comp = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chem_comp', i)
		check_status(status)
		if not status:
			self.chem_comp = ret_chem_comp


class compoundsstruct_arraycompound_desc:
	'''
	class compoundsstruct_arraycompound_desc
	Chemical compounds (e.g. solid tungsten, WC, CFC, ...) possibly present in the wall. Array of structure (number of compounds)

	Attributes:
	- array : list of compoundsstruct_arraycompound_descObj 
	'''

	def __init__(self, base_path_in='compounds'):
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
		ret = space + 'class compoundsstruct_arraycompound_desc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'compoundsstruct_arraycompound_desc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(compoundsstruct_arraycompound_descObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function getSlice') 
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
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(compoundsstruct_arraycompound_desc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = compoundsstruct_arraycompound_desc(self.base_path)
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
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type compoundsstruct_arraycompound_desc, run function getNonTimedElt') 
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


class compoundsstruct_arraycompound_descObj:
	'''
	class compoundsstruct_arraycompound_descObj
	Chemical compounds (e.g. solid tungsten, WC, CFC, ...) possibly present in the wall. Array of structure (number of compounds)

	Attributes:
	- label : str
	   Compound name/label
	- stochiometry : numpy.ndarray 1D with float
	   Fractional composition of the compound. Float vector, dimensions: 1. element number (numbering as in wall/elements array)
	- density : float
	   Compound density (molecules/m^3)
	- heat_cap : float
	   Specific heat capacity [J/(eV kg)]
	- heat_cond : numpy.ndarray 1D with float
	   Thermal conductivity [W/(m eV)]
	- surf_recrate : numpy.ndarray 2D with float
	   Recombination rate on surface (only for pure elements, not compounds) [molecules*m^2/s]; Dimensions: index 1: first recombining element, index 2: second recombining element (numbering as in wall/elements array)
	'''

	def __init__(self, base_path_in='compounds'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.label = ''
		self.stochiometry = numpy.zeros(0, numpy.float64, order='C')
		self.density = EMPTY_DOUBLE
		self.heat_cap = EMPTY_DOUBLE
		self.heat_cond = numpy.zeros(0, numpy.float64, order='C')
		self.surf_recrate = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compoundsstruct_arraycompound_descObj\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		s = self.stochiometry.__str__()
		ret = ret + space + 'Attribute stochiometry\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute density: ' + str(self.density) + '\n'
		ret = ret + space + 'Attribute heat_cap: ' + str(self.heat_cap) + '\n'
		s = self.heat_cond.__str__()
		ret = ret + space + 'Attribute heat_cond\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.surf_recrate.__str__()
		ret = ret + space + 'Attribute surf_recrate\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compoundsstruct_arraycompound_descObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'stochiometry') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'stochiometry', i, numpy.array(self.stochiometry).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'density', i, self.density)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'heat_cap') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'heat_cap', i, self.heat_cap)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'heat_cond') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'heat_cond', i, numpy.array(self.heat_cond).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'surf_recrate') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'surf_recrate', i, numpy.array(self.surf_recrate).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compoundsstruct_arraycompound_descObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'stochiometry') 
			print ('obj = ' + str(obj))
		status, ret_stochiometry = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'stochiometry', i)
		check_status(status)
		if not status:
			self.stochiometry = ret_stochiometry
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'density') 
			print ('obj = ' + str(obj))
		status, ret_density = ull.getDoubleFromObject(self.idx, obj, cpopath + 'density', i)
		check_status(status)
		if not status:
			self.density = ret_density
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'heat_cap') 
			print ('obj = ' + str(obj))
		status, ret_heat_cap = ull.getDoubleFromObject(self.idx, obj, cpopath + 'heat_cap', i)
		check_status(status)
		if not status:
			self.heat_cap = ret_heat_cap
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'heat_cond') 
			print ('obj = ' + str(obj))
		status, ret_heat_cond = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'heat_cond', i)
		check_status(status)
		if not status:
			self.heat_cond = ret_heat_cond
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'surf_recrate') 
			print ('obj = ' + str(obj))
		status, ret_surf_recrate = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'surf_recrate', i)
		check_status(status)
		if not status:
			self.surf_recrate = ret_surf_recrate


class elementsstruct_arrayelement_desc:
	'''
	class elementsstruct_arrayelement_desc
	Chemical elements present in the wall units, including elements from the plasma (gas + impurities). Use by compounds. Array of structures (number of elements)

	Attributes:
	- array : list of elementsstruct_arrayelement_descObj 
	'''

	def __init__(self, base_path_in='elements'):
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
		ret = space + 'class elementsstruct_arrayelement_desc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'elementsstruct_arrayelement_desc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(elementsstruct_arrayelement_descObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function getSlice') 
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
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(elementsstruct_arrayelement_desc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = elementsstruct_arrayelement_desc(self.base_path)
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
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type elementsstruct_arrayelement_desc, run function getNonTimedElt') 
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


class elementsstruct_arrayelement_descObj:
	'''
	class elementsstruct_arrayelement_descObj
	Chemical elements present in the wall units, including elements from the plasma (gas + impurities). Use by compounds. Array of structures (number of elements)

	Attributes:
	- nucindex : int
	   Index into list of nuclei in wall/compositions/nuclei if the element is present there. Otherwise it is 0 and zn, amn and label have to be set.

	- label : str
	   Element name/label
	- zn : float
	   Nuclear charge [units of elementary charge];
	- amn : float
	   Mass of atom [amu]
	'''

	def __init__(self, base_path_in='elements'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.label = ''
		self.zn = EMPTY_DOUBLE
		self.amn = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class elementsstruct_arrayelement_descObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute zn: ' + str(self.zn) + '\n'
		ret = ret + space + 'Attribute amn: ' + str(self.amn) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type elementsstruct_arrayelement_descObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zn', i, self.zn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'amn', i, self.amn)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type elementsstruct_arrayelement_descObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
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


class compositionsstructurecompositions_type:
	'''
	class compositionsstructurecompositions_type
	

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


class nucleistruct_arraynucleiObj:
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


class ionsstruct_arrayionsObj:
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


class impuritiesstruct_arrayimpuritiesObj:
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


class neutralscompstruct_arraycomposition_neutralscompObj:
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


class neutcompstruct_arraycomposition_neutrals_neutcompObj:
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


class typestruct_arrayidentifierObj:
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


class edgespeciesstruct_arrayedgespeciesObj:
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


class signaturestructureidentifier:
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
