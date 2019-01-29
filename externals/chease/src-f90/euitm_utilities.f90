! FORTRAN 90 type defs is text - It can be viewed as source in a browser and then saved
!  Version 1 - updated 5 September 2005, Jo Lister
!  Version 2 - updated 8 December 2005, Jo Lister

module euITM_utilities    ! declare the set of types common to all sub-trees

  type type_case  
     !    Synonym definitions
     character(80) :: name1  ! /header/synonymtable(i)/case(i)/name1 - Principal name
     character(80) :: name2  ! /header/synonymtable(i)/case(i)/name2 - Optional name
  endtype type_case

  type type_update  
     !    Modification made to this block
     character(80) :: author  ! /header/update(i)/author - Name of the modifier
     character(80) :: date  ! /header/update(i)/date - Date of the modification
     character(80) :: comment  ! /header/update(i)/comment - Modifications made
  endtype type_update

  type type_tags  
     !    List of tags in this block
     character(80) :: name1  ! /header/tags(i)/name1 - Principal name
     character(80) :: name2  ! /header/tags(i)/name2 - Optional name
  endtype type_tags

  type type_ref  
     !    Reference for online documentation
     character(80) :: descrip  ! /header/ref(i)/descrip - Short textual description of the reference
     character(100) :: url_type  ! /header/ref(i)/url_type - url-type of this reference (e.g. http)
     character(100) :: urlaccess  ! /header/ref(i)/urlaccess - url-access of this reference (e.g. http://...*.pdf)
  endtype type_ref

  type type_datainfo  
     !    Generic information on a data item
     character(80) :: sourcemethod  ! /datainfo/sourcemethod - Source method for this data
     character(200) :: sourceexpression  ! /datainfo/sourceexpression - Source expression to get this data
  endtype type_datainfo
  type type_datashorttext  
     !    
     character(80) :: dat  ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datashorttext

  type type_datadate  
     !    
     character(80) :: dat  ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datadate

  type type_datalongtext  
     !    
     character(200) :: dat  ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datalongtext

  type type_dataflt  
     !    
     real(kind=8)  :: dat  ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_dataflt

  type type_dataint  
     !    
     integer  :: dat  ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_dataint

  type type_datavecflt  
     !    
     real(kind=8),pointer  :: dat(:)      ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datavecflt

  type type_datavecint  
     !    
     integer,pointer  :: dat(:)       ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datavecint

  type type_datamatflt  
     !    
     real(kind=8),pointer  :: dat(:,:)      ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datamatflt

  type type_datamatint  
     !    
     integer,pointer  :: dat(:,:)      ! /dat - The data and its ASCII description
     type (type_datainfo) :: datainfo  ! /datainfo - 
  endtype type_datamatint

  type type_dataref  
     !    
     character(80) :: method  ! /method - Method for obtaining data
     character(200) :: source  ! /source - Expression for obtaining the data using the method
  endtype type_dataref

  type type_reggrid  
     !    
     type (type_dataint)  :: ndim1  ! /ndim1 - First dimension size
     type (type_dataint)  :: ndim2  ! /ndim2 - Second dimension size
     type (type_datavecflt)  :: dim1  ! /dim1 - First dimension values
     type (type_datavecflt)  :: dim2  ! /dim2 - Second dimension values
  endtype type_reggrid

  type type_rzvector  
     !    
     type (type_dataint)  :: npoints  ! /npoints - Number of points in the vector
     type (type_datamatflt)  :: rz  ! /rz - R,Z position [m]
  endtype type_rzvector

  type type_synonymtable  
     !    List of synonyms in this block
     character(80) :: title  ! /header/synonymtable(i)/title - Name of this synonym table
     type (type_dataint)  :: ncase  ! /header/synonymtable(i)/ncase - number of definitions
     type (type_case),pointer :: case(:)  ! /header/synonymtable(i)/case(i) - Synonym definitions
  endtype type_synonymtable

  type type_putinfo  
     !    Generic put information for a data item or structure
     character(80) :: rights  ! /putinfo/rights - Access rights to this data
     character(80) :: putmethod  ! /putinfo/putmethod - Storage method for this data
     character(80) :: putaccess  ! /putinfo/putaccess - Instructions to access the data using this method
     character(200) :: putlocation  ! /putinfo/putlocation - Name of this data under this method
     character(80) :: putby  ! /putinfo/putby - ID of the putter of the data
     character(80) :: putdate  ! /putinfo/putdate - Date of the put of this data
  endtype type_putinfo

  type type_header  
     !    Generic header
     character(80) :: title  ! /header/title - Title of this block, for display
     character(80) :: shortname  ! /header/shortname - Title in lowercase (one word only)
     character(80) :: briefdescrip  ! /header/briefdescrip - Brief text description of the block
     character(200) :: longdescrip  ! /header/longdescrip - Longer text description of the block
     type (type_dataint)  :: nupdate  ! /header/nupdate - number of updates
     type (type_update),pointer :: update(:)  ! /header/update(i) - Modification made to this block
     type (type_dataint)  :: nsynonymtable  ! /header/nsynonymtable - number of sysnonym tables
     type (type_synonymtable),pointer :: synonymtable(:)  ! /header/synonymtable(i) - List of synonyms in this block
     type (type_dataint)  :: ntags  ! /header/ntags - number of tag lists
     type (type_tags),pointer :: tags(:)  ! /header/tags(i) - List of tags in this block
     type (type_dataint)  :: nref  ! /header/nref - number of references
     type (type_ref),pointer :: ref(:)  ! /header/ref(i) - Reference for online documentation
     type (type_dataint)  :: nnorm  ! /header/nnorm - number of norms
     character(80) :: norm  ! /header/norm(i) - Norms which are respected by this block
  endtype type_header

  type type_signal  
     !    
     type (type_datashorttext)  :: signalname  ! /signalname - One of a set of signal types
     type (type_datashorttext)  :: signaltype  ! /signaltype - One of a set of signal types
     integer  :: id  ! /id - ID corresponding to the source definition
     type (type_datavecflt)  :: time  ! /time - Timebase for this signal [s]
     type (type_datamatflt)  :: values  ! /values - Data corresponding to the signal type
     type (type_datashorttext)  :: sourcemethod  ! /sourcemethod - Method for locally generating this data
     type (type_datashorttext)  :: sourceexpression  ! /sourceexpression - Expression which can be evaluated locally to generate the data using the method
     type (type_datashorttext)  :: errortype  ! /errortype - Type of error estimate, e.g. range, fractioneps, max([range,fractioneps])
     type (type_datavecflt)  :: range  ! /range - Upper and lower estimates of the error bar limits (1 sigma)
     type (type_datavecflt)  :: fractioneps  ! /fractioneps - Symmetric error derived as  [fraction(abs value), offset]
     type (type_datamatflt)  :: sigma  ! /sigma - Explicit estimate of the error to be used in fitting, must have the same timebase and columns as values
     type (type_datashorttext)  :: tobecompleted  ! /tobecompleted - 
  endtype type_signal

endmodule euITM_utilities
