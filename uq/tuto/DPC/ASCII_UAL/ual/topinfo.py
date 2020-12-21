# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class topinfo(KeepInOrder):
	'''
	class topinfo
	General info about the database entry. CPO.

	Attributes:
	- dataprovider : str
	   Name of the main data provider (the person who filled the original data)
	- description : str
	   Pulse/Entry description
	- firstputdate : str
	   Date of the original data submission
	- lastupdate : str
	   Date of the last data addition in the tree
	- source : str
	   Exact reference of the data source (e.g. original reference in the native machine data base)
	- comment : str
	   Any additional comment
	- dataversion : str
	   Version of the data structure
	- workflow : str
	   Workflow which has been used to produce the present entry. Exact format to be defined with the platform group. User-specific input files (if allowed) must be stored there as well.
	- entry : class entrystructureentry_def
	   Definition of this database entry
	- parent_entry : class parent_entrystructureentry_def
	   Definition of the entry of the direct parent (if any)
	- mdinfo : class mdinfostructuremdinfo
	   Information related to machine description for this entry
	'''

	def __init__(self):
		self.base_path = 'topinfo'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 1
		self.dataprovider = ''
		self.description = ''
		self.firstputdate = ''
		self.lastupdate = ''
		self.source = ''
		self.comment = ''
		self.dataversion = ''
		self.workflow = ''
		self.entry = entrystructureentry_def('entry')
		self.parent_entry = parent_entrystructureentry_def('parent_entry')
		self.mdinfo = mdinfostructuremdinfo('mdinfo')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class topinfo\n'
		ret = ret + space + 'Attribute dataprovider: ' + str(self.dataprovider) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		ret = ret + space + 'Attribute firstputdate: ' + str(self.firstputdate) + '\n'
		ret = ret + space + 'Attribute lastupdate: ' + str(self.lastupdate) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute comment: ' + str(self.comment) + '\n'
		ret = ret + space + 'Attribute dataversion: ' + str(self.dataversion) + '\n'
		ret = ret + space + 'Attribute workflow: ' + str(self.workflow) + '\n'
		ret = ret + space + 'Attribute entry\n ' + self.entry.__str__(depth+1)
		ret = ret + space + 'Attribute parent_entry\n ' + self.parent_entry.__str__(depth+1)
		ret = ret + space + 'Attribute mdinfo\n ' + self.mdinfo.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.entry.setExpIdx(idx)
		self.parent_entry.setExpIdx(idx)
		self.mdinfo.setExpIdx(idx)

	def getMaxOccurrences(self):
		return self.maxOccurrences

	def getCPOName(self):
		return self.base_path

	def put(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		status = ull.beginCPOPut(self.idx, path)
		status = ull.putString(self.idx, path, cpopath + 'dataprovider', self.dataprovider)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'description', self.description)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'firstputdate', self.firstputdate)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'lastupdate', self.lastupdate)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'comment', self.comment)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'dataversion', self.dataversion)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'workflow', self.workflow)
		check_status(status)
		self.entry.put(path, cpopath)
		self.parent_entry.put(path, cpopath)
		self.mdinfo.put(path, cpopath)
		status = ull.endCPOPut(self.idx, path)


	def get(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		status, nbslice = ull.beginCPOGet(self.idx, path, NON_TIMED)
		status, ret_dataprovider = ull.getString(self.idx, path, cpopath + 'dataprovider')
		check_status(status)
		if not status:
			self.dataprovider = ret_dataprovider
		status, ret_description = ull.getString(self.idx, path, cpopath + 'description')
		check_status(status)
		if not status:
			self.description = ret_description
		status, ret_firstputdate = ull.getString(self.idx, path, cpopath + 'firstputdate')
		check_status(status)
		if not status:
			self.firstputdate = ret_firstputdate
		status, ret_lastupdate = ull.getString(self.idx, path, cpopath + 'lastupdate')
		check_status(status)
		if not status:
			self.lastupdate = ret_lastupdate
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_comment = ull.getString(self.idx, path, cpopath + 'comment')
		check_status(status)
		if not status:
			self.comment = ret_comment
		status, ret_dataversion = ull.getString(self.idx, path, cpopath + 'dataversion')
		check_status(status)
		if not status:
			self.dataversion = ret_dataversion
		status, ret_workflow = ull.getString(self.idx, path, cpopath + 'workflow')
		check_status(status)
		if not status:
			self.workflow = ret_workflow
		self.entry.get(path, cpopath)
		self.parent_entry.get(path, cpopath)
		self.mdinfo.get(path, cpopath)
		status = ull.endCPOGet(self.idx, path)



class entrystructureentry_def(KeepInOrder):
	'''
	class entrystructureentry_def
	Definition of this database entry

	Attributes:
	- user : str
	   Name of the user if private data. Value should be ITM if stored in the official common ITM tree
	- machine : str
	   Name of the device
	- shot : int
	   Shot number
	- run : int
	   Run number
	'''

	def __init__(self, base_path_in='entry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.user = ''
		self.machine = ''
		self.shot = EMPTY_INT
		self.run = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class entrystructureentry_def\n'
		ret = ret + space + 'Attribute user: ' + str(self.user) + '\n'
		ret = ret + space + 'Attribute machine: ' + str(self.machine) + '\n'
		ret = ret + space + 'Attribute shot: ' + str(self.shot) + '\n'
		ret = ret + space + 'Attribute run: ' + str(self.run) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def put(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type entrystructureentry_def, run function put') 
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

	def get(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type entrystructureentry_def, run function get') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type entrystructureentry_defObj, run function putNonTimedElt') 
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
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type entrystructureentry_defObj, run function getNonTimedElt') 
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


class parent_entrystructureentry_def(KeepInOrder):
	'''
	class parent_entrystructureentry_def
	Definition of the entry of the direct parent (if any)

	Attributes:
	- user : str
	   Name of the user if private data. Value should be ITM if stored in the official common ITM tree
	- machine : str
	   Name of the device
	- shot : int
	   Shot number
	- run : int
	   Run number
	'''

	def __init__(self, base_path_in='parent_entry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.user = ''
		self.machine = ''
		self.shot = EMPTY_INT
		self.run = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class parent_entrystructureentry_def\n'
		ret = ret + space + 'Attribute user: ' + str(self.user) + '\n'
		ret = ret + space + 'Attribute machine: ' + str(self.machine) + '\n'
		ret = ret + space + 'Attribute shot: ' + str(self.shot) + '\n'
		ret = ret + space + 'Attribute run: ' + str(self.run) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def put(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parent_entrystructureentry_def, run function put') 
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

	def get(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parent_entrystructureentry_def, run function get') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parent_entrystructureentry_defObj, run function putNonTimedElt') 
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
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parent_entrystructureentry_defObj, run function getNonTimedElt') 
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


class mdinfostructuremdinfo(KeepInOrder):
	'''
	class mdinfostructuremdinfo
	Information related to machine description for this entry

	Attributes:
	- shot_min : int
	   Minimum shot number to which the machine description applies
	- shot_max : int
	   Maximum shot number to which the machine description applies
	- md_entry : class md_entrystructureentry_def
	   Entry of the machine description used. NB : just for information : for the moment, no guarantee that machine description data have not been modified with respect to the data in md_entry. Machine description data are written explicitely in each CPO.
	'''

	def __init__(self, base_path_in='mdinfo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.shot_min = EMPTY_INT
		self.shot_max = EMPTY_INT
		self.md_entry = md_entrystructureentry_def('md_entry')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mdinfostructuremdinfo\n'
		ret = ret + space + 'Attribute shot_min: ' + str(self.shot_min) + '\n'
		ret = ret + space + 'Attribute shot_max: ' + str(self.shot_max) + '\n'
		ret = ret + space + 'Attribute md_entry\n ' + self.md_entry.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.md_entry.setExpIdx(idx)

	def put(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mdinfostructuremdinfo, run function put') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'shot_min', self.shot_min)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'shot_max', self.shot_max)
		check_status(status)
		self.md_entry.put(path, cpopath)

	def get(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mdinfostructuremdinfo, run function get') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_shot_min = ull.getInt(self.idx, path, cpopath + 'shot_min')
		check_status(status)
		if not status:
			self.shot_min = ret_shot_min
		status, ret_shot_max = ull.getInt(self.idx, path, cpopath + 'shot_max')
		check_status(status)
		if not status:
			self.shot_max = ret_shot_max
		self.md_entry.get(path, cpopath)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mdinfostructuremdinfoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'shot_min') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'shot_min', i, self.shot_min)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'shot_max') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'shot_max', i, self.shot_max)
		obj = self.md_entry.putNonTimedElt(path, cpopath + 'md_entry', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mdinfostructuremdinfoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'shot_min') 
			print ('obj = ' + str(obj))
		status, ret_shot_min = ull.getIntFromObject(self.idx, obj, cpopath + 'shot_min', i)
		check_status(status)
		if not status:
			self.shot_min = ret_shot_min
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'shot_max') 
			print ('obj = ' + str(obj))
		status, ret_shot_max = ull.getIntFromObject(self.idx, obj, cpopath + 'shot_max', i)
		check_status(status)
		if not status:
			self.shot_max = ret_shot_max
		self.md_entry.getNonTimedElt(path, cpopath + 'md_entry', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'shot_min')
		ull.deleteData(self.idx, path, cpopath + 'shot_max')
		self.md_entry.deleteData(path, cpopath)


class md_entrystructureentry_def(KeepInOrder):
	'''
	class md_entrystructureentry_def
	Entry of the machine description used. NB : just for information : for the moment, no guarantee that machine description data have not been modified with respect to the data in md_entry. Machine description data are written explicitely in each CPO.

	Attributes:
	- user : str
	   Name of the user if private data. Value should be ITM if stored in the official common ITM tree
	- machine : str
	   Name of the device
	- shot : int
	   Shot number
	- run : int
	   Run number
	'''

	def __init__(self, base_path_in='md_entry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.user = ''
		self.machine = ''
		self.shot = EMPTY_INT
		self.run = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class md_entrystructureentry_def\n'
		ret = ret + space + 'Attribute user: ' + str(self.user) + '\n'
		ret = ret + space + 'Attribute machine: ' + str(self.machine) + '\n'
		ret = ret + space + 'Attribute shot: ' + str(self.shot) + '\n'
		ret = ret + space + 'Attribute run: ' + str(self.run) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def put(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type md_entrystructureentry_def, run function put') 
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

	def get(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type md_entrystructureentry_def, run function get') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type md_entrystructureentry_defObj, run function putNonTimedElt') 
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
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type md_entrystructureentry_defObj, run function getNonTimedElt') 
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
