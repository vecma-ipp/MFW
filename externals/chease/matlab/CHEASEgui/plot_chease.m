function varargout = plot_chease(varargin)
% plot_chease M-file for plot_chease.fig
%      plot_chease, by itself, creates a new plot_chease or raises the existing
%      singleton*.
%
%      H = plot_chease returns the handle to a new plot_chease or the handle to
%      the existing singleton*.
%
%      plot_chease('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in plot_chease.M with the given input arguments.
%
%      plot_chease('Property','Value',...) creates a new plot_chease or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before plot_chease_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to plot_chease_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
%      Inputs to plot_chease_OpeningFcn:
%	varargin{2}: path to folder containing cols files to plot. Default: current dir
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help plot_chease

% Last Modified by GUIDE v2.5 02-Mar-2009 10:25:19

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @plot_chease_OpeningFcn, ...
                   'gui_OutputFcn',  @plot_chease_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before plot_chease is made visible.
function plot_chease_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to plot_chease (see VARARGIN)

% Choose default command line output for plot_chease
handles.output = hObject;

% varargin{2} is the folder from which to start browsing for chease outputs
if nargin >= 2;
  dir_cheaseout = varargin{2};
else
  dir_cheaseout = pwd; % default
end

if nargin>=3 && ~isempty(varargin{3})
  fileToRead1 = varargin{3};
else
  [filename,pathname] = uigetfile({'*cols*','*cols* CHEASE output files'},'Select output CHEASE file to plot',[dir_cheaseout,'/*cols*']);
  fileToRead1 = fullfile(pathname,filename);
end
DELIMITER = ' ';
HEADERLINES = 1;
% Import the file
Data = importdata(fileToRead1, DELIMITER, HEADERLINES);

data_values=Data.data;
header_text=Data.textdata{1};
%put away the % symbol
header_text=header_text(2:end);
headers={};
entries=1;


start_index=find(header_text~=' ',1,'first');
end_index=start_index;

while end_index<size(header_text,2)
    while ~isequal(header_text(end_index),' ') && end_index<size(header_text,2)
        end_index=end_index+1;
    end
    headers{entries}=header_text(start_index:end_index-1);
    entries=entries+1;

    while isequal(header_text(end_index),' ')
        end_index=end_index+1;
    end

    start_index=end_index;
    end_index=end_index+1;
end

        
%disp(header_text);

set(handles.listbox1,'String',headers,'Value',1);
set(handles.listbox2,'String',headers,'Value',1);

handles.DATAMATRIX=data_values;			    
guidata(hObject,handles);

% UIWAIT makes plot_chease wait for user response (see UIRESUME)
% uiwait(handles.figure1);
% Update handles structure
guidata(hObject, handles);

% --- Outputs from this function are returned to the command line.
function varargout = plot_chease_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on selection change in listbox1.
function listbox1_Callback(hObject, eventdata, handles)
% hObject    handle to listbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox1


% --- Executes during object creation, after setting all properties.
function listbox1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in listbox2.
function listbox2_Callback(hObject, eventdata, handles)
% hObject    handle to listbox2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox2 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox2


% --- Executes during object creation, after setting all properties.
function listbox2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


x = get (handles.listbox1, 'Value') ;
y = get (handles.listbox2,'Value');


plot (handles.DATAMATRIX(:,x) , handles.DATAMATRIX(:,y));
