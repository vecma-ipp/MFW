function varargout = CHEASEgui(varargin)
% CHEASEGUI M-file for CHEASEgui.fig
%      CHEASEGUI, by itself, creates a new CHEASEGUI or raises the existing
%      singleton*.
%
%      H = CHEASEGUI returns the handle to a new CHEASEGUI or the handle to
%      the existing singleton*.
%
%      CHEASEGUI('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in CHEASEGUI.M with the given input arguments.
%
%      CHEASEGUI('Property','Value',...) creates a new CHEASEGUI or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before CHEASEgui_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to CHEASEgui_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help CHEASEgui

% Last Modified by GUIDE v2.5 04-May-2015 12:53:56

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @CHEASEgui_OpeningFcn, ...
    'gui_OutputFcn',  @CHEASEgui_OutputFcn, ...
    'gui_LayoutFcn',  [] , ...
    'gui_Callback',   []);

if nargin & isstr(varargin{1})
  gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
  [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
  gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before CHEASEgui is made visible.
function CHEASEgui_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to CHEASEgui (see VARARGIN)

% Choose default command line output for CHEASEgui
handles.output = hObject;
set(hObject,'toolbar','figure'); %enables toolbar
handles.n_points_bou=200; % number of elements for boundary (make a box at some point)
handles.n_points_rho=100; % number of elements for profiles (make a box at some point)
handles.CurrProf.nsttp=2; %default value for nsttp=2(as default Istar is given)
handles.PressProf.nppfun=4;
handles.PressProf.Pprofile=1; % 1 for pprime and 2 for pressure profile given
handles.CurrProf.nfunc=4;
set(handles.which_curr_prof, 'Value', handles.CurrProf.nsttp);  %set popup menu accordingly

%setting work directory
uname = getenv('USER');
handles.fpath = (['/tmp/',deblank(uname)]);
[c,olddir]=unix('pwd');
handles.olddir = deblank (olddir);
aspect_ratio_Callback(hObject, eventdata, handles); % calculate aspect ratio from a and R0

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes CHEASEgui wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = CHEASEgui_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;
%varargout{2} = handles.plotdatafile_H;

%----------------------------------------------------------------
% --- Executes during object creation, after setting all properties.
function set_Ip_chease_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_Ip_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

function set_Ip_chease_Callback(hObject, eventdata, handles)
% hObject    handle to set_Ip_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_Ip_chease as text
%        str2double(get(hObject,'String')) returns contents of
%        set_Ip_chease as a double

%
Ip_chease = str2num (get (handles.set_Ip_chease, 'String'));
mu0 = 4e-7*pi;
R0 = str2num (get (handles.set_R0exp, 'String'));
B0 = str2num (get (handles.set_B0exp, 'String'));
normaliz = mu0 / (B0*R0);
Ip_physic = Ip_chease / normaliz;
set (handles.set_Ip_physic, 'String', num2str(Ip_physic,'%.4e'));

% --- Executes during object creation, after setting all properties.
function set_pedge_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_a_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

function set_pedge_Callback(hObject, eventdata, handles)
% hObject    handle to set_pedge (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_pedge as text
%        str2double(get(hObject,'String')) returns contents of set_pedge as a double



%--------------------------------------------------------------------------

%SETTING AND PLOTTING PLASMA BOUNDARIES

% --- Executes during object creation, after setting all properties.
function set_R_chease_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_R_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_R_chease_Callback(hObject, eventdata, handles)
% hObject    handle to set_R_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_R_chease as text
%        str2double(get(hObject,'String')) returns contents of set_R_chease as a double
Plot_plasma_Callback(hObject, eventdata, handles);
aspect_ratio_Callback(hObject, eventdata, handles); %set the new aspect ratio:its value is changed!


% --- Executes during object creation, after setting all properties.
function set_Z_chease_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_a_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

function set_Z_chease_Callback(hObject, eventdata, handles)
% hObject    handle to set_Z_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_Z_chease as text
%        str2double(get(hObject,'String')) returns contents of set_Z_chease as a double
Plot_plasma_Callback(hObject, eventdata, handles);
chease = str2num (get (handles.set_Z_chease, 'String'));
normaliz= str2num (get (handles.set_R0exp, 'String'));
physic = chease * normaliz;
set (handles.set_Z_physic, 'String', num2str(physic));

% --- Executes during object creation, after setting all properties.
function set_a_chease_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_a_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_a_chease_Callback(hObject, eventdata, handles)
% hObject    handle to set_a_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_a_chease as text
%        str2double(get(hObject,'String')) returns contents of set_a_chease as a double
Plot_plasma_Callback(hObject, eventdata, handles);
aspect_ratio_Callback(hObject, eventdata, handles); %set the new aspect ratio:its value has changed!
chease = str2num (get (handles.set_a_chease, 'String'));
normaliz = str2num (get (handles.set_R0exp, 'String'));
physic = chease * normaliz;
set (handles.set_a_physic, 'String', num2str(physic));
% --- Executes during object creation, after setting all properties.


function set_k_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_k (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_k_Callback(hObject, eventdata, handles)
% hObject    handle to set_k (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_k as text
%        str2double(get(hObject,'String')) returns contents of set_k as a double
Plot_plasma_Callback(hObject, eventdata, handles);


% --- Executes during object creation, after setting all properties.
function set_delta_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_delta (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_delta_Callback(hObject, eventdata, handles)
% hObject    handle to set_delta (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_delta as text
%        str2double(get(hObject,'String')) returns contents of set_delta as a double
Plot_plasma_Callback(hObject, eventdata, handles);
% --- Executes during object creation, after setting all properties.


function set_xi_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_xi (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_xi_Callback(hObject, eventdata, handles)
% hObject    handle to set_xi (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_xi as text
%        str2double(get(hObject,'String')) returns contents of set_xi as a double
Plot_plasma_Callback(hObject, eventdata, handles);
% --- Executes on button press in Plot_plasma.

% --- Executes during object creation, after setting all properties.
function aspect_ratio_CreateFcn(hObject, eventdata, handles)
% hObject    handle to aspect_ratio (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function aspect_ratio_Callback(hObject, eventdata, handles)
% hObject    handle to aspect_ratio (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of aspect_ratio as text
%        str2double(get(hObject,'String')) returns contents of aspect_ratio as a double

R=str2num(get(handles.R0_chease,'String'));
amin=str2num(get(handles.set_a_chease,'String'));
aspect_ratio=amin/R;
set(handles.aspect_ratio, 'String', aspect_ratio);


function Plot_plasma_Callback(hObject, eventdata, handles)
% hObject    handle to Plot_plasma (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%aspect_ratio_Callback(hObject, eventdata, handles);  %calculate the aspect ratio
if isfield(handles,'PlasmaBndry')
  PlasmaBndry = handles.PlasmaBndry; % get existing structure
end

PlasmaBndry.R0 = str2num(get(handles.R0_chease, 'String'));
PlasmaBndry.Z0 = str2num(get(handles.set_Z_chease, 'String'));
PlasmaBndry.a = str2num(get(handles.set_a_chease, 'String'));
PlasmaBndry.kappa = str2num(get(handles.set_k, 'String'));
PlasmaBndry.delta = str2num(get(handles.set_delta, 'String'));
PlasmaBndry.xi = str2num(get(handles.set_xi, 'String'));
PlasmaBndry.epsilon = str2num(get(handles.aspect_ratio, 'String'));
PlasmaBndry.tensbnd = str2num(get(handles.set_tensbnd, 'String'));
PlasmaBndry.tensprof = str2num(get(handles.set_tensprof, 'String'));
PlasmaBndry.n_points_bou = handles.n_points_bou; % make this into a box at some point
PlasmaBndry = makePlasmaBndry_from_params(PlasmaBndry); % compute parameterized rz

%%%%%%%%%%% plot plasma boundary %%%%%%%%%%%%%%%%
subplot(handles.plasma_axes);
xlabel('R/R0'); ylabel('Z/R0');

% plot parameterized boundary, this is always possible
hPlasmaBndryPlot_params = plot(PlasmaBndry.rz_param(:,1),PlasmaBndry.rz_param(:,2),'b-');

if isfield(PlasmaBndry,'rzsmoothed') % also an externally provided boundary is available
  % hold off
  hPlasmaBndryPlot = plot(PlasmaBndry.rzsmoothed(:,1),PlasmaBndry.rzsmoothed(:,2),'b-', ...
          PlasmaBndry.rz_param(:,1),PlasmaBndry.rz_param(:,2),'r--', ...
          PlasmaBndry.rzexp(:,1),PlasmaBndry.rzexp(:,2),'k--', ...
          PlasmaBndry.R0,PlasmaBndry.Z0,'b+', 'LineWidth',2); 
  % switch line style depending on which will be used 
  useparambdry=get(handles.UseParameterizedBoundaryCheckBox, 'Value');
  if ~useparambdry
    set(hPlasmaBndryPlot(1),'linestyle','-','color','b'); 
    set(hPlasmaBndryPlot(2),'linestyle','--','color','r');
  else % use parameterized boundary
    set(hPlasmaBndryPlot(1),'linestyle','--','color','r'); 
    set(hPlasmaBndryPlot(2),'linestyle','-','color','b');
  end
  legend(hPlasmaBndryPlot(1:3),{'exp smooth','param','exp'});
end
axis equal; grid on; zoom on;

handles.PlasmaBndry=PlasmaBndry; %replace the handles structure PlasmaBndry 
guidata(hObject, handles); %update handles

% --- Executes on button press in add_plot_plasma.
function add_plot_plasma_Callback(hObject, eventdata, handles)
% hObject    handle to add_plot_plasma (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of add_plot_plasma
b = get (handles.add_plot_plasma, 'Value') ;
if (b==0)
  set (handles.plasma_axes, 'NextPlot', 'replace');
  set(handles.add_plot_plasma,'string','hold off')
else
  set (handles.plasma_axes, 'NextPlot', 'add');
  set(handles.add_plot_plasma,'string','hold on')
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%SETTING AND PLOTTING CURRENT PROFILE

function which_curr_prof_CreateFcn(hObject, eventdata, handles)
% hObject    handle to which_curr_prof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

%--- Executes on selection change in which_curr_prof.
function which_curr_prof_Callback(hObject, eventdata, handles)
% hObject    handle to which_curr_prof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns which_curr_prof contents as cell array
%        contents{get(hObject,'Value')} returns selected item from which_curr_prof

axes (handles.current_axes);
xlabel ('1-psi/psimin');

value=get(handles.which_curr_prof,'Value');
switch(value)
 case 1
  handles.CurrProf.nsttp=1; %TT' chosen, nstp=1
  ylabel('TTprime');
 case 2
  handles.CurrProf.nsttp=2; %I* chosen, nstp=2
  ylabel('I*');
 case 3
  handles.CurrProf.nsttp=3; % Iparallel chosen, nstp=3
  ylabel('Iparallel');
end

guidata (hObject, handles);

function current_prof_CreateFcn(hObject, eventdata, handles)
% hObject    handle to current_prof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


% --- Executes on selection change in current_prof.
function current_prof_Callback(hObject, eventdata, handles)
% hObject    handle to current_prof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns current_prof contents as cell array
%        contents{get(hObject,'Value')} returns selected item from current_prof
value=get(handles.current_prof,'Value');

switch(value)

 case 1
  %enable the user to set the points AND NOT the coefficients the polynomious
  set(handles.set_x_curr, 'Enable', 'on');
  set(handles.set_y_curr, 'Enable', 'on');
  set(handles.plot_current_points, 'Enable', 'on')
  set(handles.poly_curr,'Enable','off');
  handles.CurrProf.nfunc=4;    %the current profile is taken from expeq file, so nfunc=4
  set(handles.set_nfunc,'value',handles.CurrProf.nfunc);

case 2
  %disable some buttons (the user cannot set x and y of the points, and neither the coefficients of the poly
  set(handles.plot_current_points, 'Enable', 'on')
  set(handles.poly_curr,'Enable','off');
  handles.CurrProf.nfunc=4;    %the current profile is taken from expeq file, so nfunc=4
  set(handles.set_nfunc,'value',handles.CurrProf.nfunc);
  % get the points (clicking on the ases)and plot 
  xout=linspace(0,1,handles.n_points_rho);
  tensfit=str2num(get(handles.set_tension_fit_curr, 'String'));
  [x,y,yout]=get_xy_points(handles.current_axes,xout,tensfit);
  %save the Current Profile profile into the structure
  handles.CurrProf.x_clicked=x;
  handles.CurrProf.y_clicked=y;
  handles.CurrProf.x_forinput=xout;
  handles.CurrProf.y_forinput=yout;
  set(handles.set_x_curr, 'Enable', 'on');
  set(handles.set_y_curr, 'Enable', 'on');
  set(handles.set_x_curr,'string',num2str(reshape(x,1,length(x))));
  set(handles.set_y_curr,'string',num2str(reshape(y,1,length(y))));
  set(handles.set_x_curr, 'Enable', 'off');
  set(handles.set_y_curr, 'Enable', 'off');

case 3
  %enable the user to set the coefficients of the polynomious AND NOT the points        
  set(handles.poly_curr,'Enable','on');
  set(handles.plot_current_points, 'Enable', 'on')  ;
  set(handles.set_x_curr, 'Enable', 'off');
  set(handles.set_y_curr, 'Enable', 'off');
  handles.CurrProf.nfunc=4;    %the current profile is taken from expeq file, so nfunc=4
  set(handles.set_nfunc,'value',handles.CurrProf.nfunc);
  set(handles.plot_nfunc , 'Enable' , 'off');
end

guidata (hObject, handles);

% --- Executes during object creation, after setting all properties.
function poly_curr_CreateFcn(hObject, eventdata, handles)
% hObject    handle to poly_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function poly_curr_Callback(hObject, eventdata, handles)
% hObject    handle to poly_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of poly_curr as text
%        str2double(get(hObject,'String')) returns contents of poly_curr as a double
coefs = str2num(get(handles.poly_curr, 'String')); %get the coefficients of the polynomious
%plot the profile
x = linspace(0,1,handles.n_points_rho);
y = polyval (coefs(end:-1:1),x);
subplot (handles.current_axes); %set current axes
plot (x,y, 'LineWidth', 2); grid on; %plot the polynomious

%build the complete profile and save it into structure
handles.CurrProf.x_forinput = x;
handles.CurrProf.y_forinput = y;
guidata(hObject, handles);

% --- Executes during object creation, after setting all properties.
function set_x_curr_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_x_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_x_curr_Callback(hObject, eventdata, handles)
% hObject    handle to set_x_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_x_curr as text
%        str2double(get(hObject,'String')) returns contents of set_x_curr as a double


% --- Executes during object creation, after setting all properties.
function set_y_curr_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_y_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_y_curr_Callback(hObject, eventdata, handles)
% hObject    handle to set_y_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_y_curr as text
%        str2double(get(hObject,'String')) returns contents of set_y_curr as a double


% --- Executes on button press in plot_current_points.
function plot_current_points_Callback(hObject, eventdata, handles)
% hObject    handle to plot_current_points (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

subplot(handles.current_axes); %set the current axes
aa = get(handles.current_axes, 'NextPlot');

vcase=get(handles.current_prof,'value');
switch (vcase)
 case 1
  x = str2num(get(handles.set_x_curr, 'String')); 
  y = str2num(get(handles.set_y_curr, 'String'));
  plot (x, y,'r--','LineWidth',2);   grid on; %plot the current profile
  hold on
  %build the complete profile and save it into structure
  handles.CurrProf.x_forinput = linspace(0,1,handles.n_points_rho);
  % impose 1st derivative 0 at axis (d/drho=0)
  sigma=ones(size(x));
  if x(1)>0.
    x(2:end+1)=x;
    x(1)=0.;
    y(2:end+1)=y;
    sigma(2:end+1) = sigma;
    sigma(1)=1e3;
    disp('uses interpos for fun')
  end
  tensfit=str2num(get(handles.set_tension_fit_curr, 'String'));
  handles.CurrProf.y_forinput=interpos(x, y, handles.CurrProf.x_forinput, tensfit,[1 0],[0 0],sigma);
  tensprof=str2num(get(handles.set_tensprof, 'String'));
  handles.CurrProf.x_chease = handles.CurrProf.x_forinput;
  % compute what will be used in CHEASE from _forinput input
  handles.CurrProf.y_chease=interpos(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput,handles.CurrProf.x_chease, tensprof,[1 0],[0 0]);

  plot(handles.CurrProf.x_chease,handles.CurrProf.y_chease,'k-','linewidth',2);
  plot(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput,'b-','linewidth',2);

 case 2
  plot (handles.CurrProf.x_clicked,handles.CurrProf.y_clicked,'r--','LineWidth', 2);  grid on;
  hold on
  x=handles.CurrProf.x_clicked;
  y=handles.CurrProf.y_clicked;
  sigma=ones(size(x));
  if x(1)>0;
    x(2:end+1)=x;
    x(1)=0.;
    y(2:end+1)=y;
    sigma(2:end+1)=sigma;
    sigma(1)=1e3;
  end
  tensfit=str2num(get(handles.set_tension_fit_curr, 'String'));
  handles.CurrProf.y_forinput=interpos(x,y,handles.CurrProf.x_forinput, tensfit,[1 0],[0 0],sigma);
  handles.CurrProf.x_chease = linspace(0, 1, handles.n_points_rho);
  tensprof=str2num(get(handles.set_tensprof, 'String'));
  handles.CurrProf.y_chease=interpos(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput,handles.CurrProf.x_chease, tensprof,[1 0],[0 0]);

  plot(handles.CurrProf.x_chease,handles.CurrProf.y_chease,'k-','linewidth',2);
  plot(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput,'b-','linewidth',2);
  
 case 3
  
  poly_curr_Callback(hObject, eventdata, handles);
  handles.CurrProf.x_chease = linspace(0, 1, handles.n_points_rho);
  tensprof=str2num(get(handles.set_tensprof, 'String'));
  handles.CurrProf.y_chease=interpos(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput,handles.CurrProf.x_chease, tensprof,[1 0],[0 0]);

  plot(handles.CurrProf.x_chease,handles.CurrProf.y_chease,'k-','linewidth',2);
  hold on
  plot(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput,'b-','linewidth',2);
 
 otherwise
  
end

set(handles.current_axes, 'NextPlot',aa);
zoom on;

guidata (hObject, handles);

% --- Executes on button press in add_plot_current.
function add_plot_current_Callback(hObject, eventdata, handles)
% hObject    handle to add_plot_current (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of add_plot_current

b = get (handles.add_plot_current, 'Value') ;
if (b==0)
  set (handles.current_axes, 'NextPlot', 'replace');
  set(handles.add_plot_current,'string','hold off')
else
  set (handles.current_axes, 'NextPlot', 'add');
  set(handles.add_plot_current,'string','hold on')
end

%------------------------------------------------------------------------

%SETTING AND PLOTTING PRESSURE PROFILE

% --- Executes during object creation, after setting all properties.
function pressure_prof_CreateFcn(hObject, eventdata, handles)
% hObject    handle to pressure_prof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


% --- Executes on selection change in pressure_prof.
function pressure_prof_Callback(hObject, eventdata, handles)
% hObject    handle to pressure_prof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns pressure_prof contents as cell array
%        contents{get(hObject,'Value')} returns selected item from pressure_prof
value=get(handles.pressure_prof,'Value');
switch(value)

case 1 
  %enable the user to set the points AND NOT the coefficients of the polynomious     
  set(handles.set_x_press, 'Enable', 'on');
  set(handles.set_y_press, 'Enable', 'on');
  set(handles.plot_pressure_points, 'Enable', 'on');
  set(handles.poly_press , 'Enable' , 'off');
  set(handles.plot_ap , 'Enable' , 'off');
  handles.PressProf.nppfun=4; %pressure profile is specified in expeq file, so nppfun=4;
  set(handles.set_NPPFUN,'value',handles.PressProf.nppfun);

case 2 
  % give points by clicking
  set(handles.plot_pressure_points, 'Enable', 'on')
  set(handles.poly_press,'Enable','off');
  set(handles.plot_ap , 'Enable' , 'off');
  handles.PressProf.nppfun=4; %pressure profile is specified in expeq file, so nppfun=4;
  set(handles.set_NPPFUN,'value',handles.PressProf.nppfun);
  % get the points and plot
  xout=linspace(0,1,handles.n_points_rho);
  tensfit=str2num(get(handles.set_tension_fit_p, 'String'));
  [x,y,yout]=get_xy_points(handles.pressure_axes,xout,tensfit);
  %save the Pressure Profile profile into the structure
  handles.PressProf.x_clicked=x;
  handles.PressProf.y_clicked=y;
  handles.PressProf.x_forinput=xout;
  handles.PressProf.y_forinput=yout;
  % copy points to list of points, so can be changed easier
  set(handles.set_x_press, 'Enable', 'on');
  set(handles.set_y_press, 'Enable', 'on');
  set(handles.set_x_press,'string',num2str(reshape(x,1,length(x))));
  set(handles.set_y_press,'string',num2str(reshape(y,1,length(y))));
  set(handles.set_x_press, 'Enable', 'off');
  set(handles.set_y_press, 'Enable', 'off');

case 3
  %enable the user to set the coefficients of the polynomious AND NOT the points
  set(handles.poly_press,'Enable','on');
  set(handles.plot_pressure_points, 'Enable', 'on');
  set(handles.set_x_press, 'Enable', 'off');
  set(handles.set_y_press, 'Enable', 'off');
  handles.PressProf.nppfun=4;
  set(handles.set_NPPFUN,'value',handles.PressProf.nppfun);
  set(handles.plot_ap , 'Enable' , 'off');
end

guidata (hObject, handles);

% --- Executes during object creation, after setting all properties.
function set_x_press_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_x_curr_press (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_x_press_Callback(hObject, eventdata, handles)
% hObject    handle to set_x_curr_press (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_x_curr_press as text
%        str2double(get(hObject,'String')) returns contents of set_x_curr_press as a double


% --- Executes during object creation, after setting all properties.
function set_y_press_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_y_press (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_y_press_Callback(hObject, eventdata, handles)
% hObject    handle to set_y_press (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_y_press as text
%        str2double(get(hObject,'String')) returns contents of set_y_press as a double


% --- Executes on button press in plot_pressure_points.
function plot_pressure_points_Callback(hObject, eventdata, handles)
% hObject    handle to plot_pressure_points (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

subplot(handles.pressure_axes); %set current axes
aa = get(handles.pressure_axes, 'NextPlot');

vcase=get(handles.pressure_prof,'value');
switch (vcase)
 case 1
  x = str2num ( get ( handles.set_x_press, 'String'));
  y = str2num ( get ( handles.set_y_press, 'String'));
  plot (x,y,'r--','LineWidth', 2);  grid on;
  hold on
  %build the complete profile and save it into structure
  handles.PressProf.x_forinput = linspace(0, 1, handles.n_points_rho);
  % impose 1st derivative 0 at axis (d/drho=0)
  sigma=ones(size(x));
  if x(1)>0.
    x(2:end+1)=x;
    x(1)=0.;
    y(2:end+1)=y;
    sigma(2:end+1) = sigma;
    sigma(1)=1e3;
    disp('uses interpos for pprime')
  end
  tensfit=str2num(get(handles.set_tension_fit_p, 'String'));
  handles.PressProf.y_forinput=interpos(x, y, handles.PressProf.x_forinput, tensfit,[1 0],[0 0],sigma);
  tensprof=str2num(get(handles.set_tensprof, 'String'));
  handles.PressProf.x_chease = handles.PressProf.x_forinput;
  % compute what will be used in CHEASE from _forinput input
  handles.PressProf.y_chease=interpos(handles.PressProf.x_forinput,handles.PressProf.y_forinput,handles.PressProf.x_chease, tensprof,[1 0],[0 0]);

  plot(handles.PressProf.x_chease,handles.PressProf.y_chease,'k-','linewidth',2);
  plot(handles.PressProf.x_forinput,handles.PressProf.y_forinput,'b-','linewidth',2);

 case 2

  plot (handles.PressProf.x_clicked,handles.PressProf.y_clicked,'r--','LineWidth', 2);  grid on;
  hold on
  x=handles.PressProf.x_clicked;
  y=handles.PressProf.y_clicked;
  sigma=ones(size(x));
  if x(1)>0;
    x(2:end+1)=x;
    x(1)=0.;
    y(2:end+1)=y;
    sigma(2:end+1)=sigma;
    sigma(1)=1e3;
  end
  tensfit=str2num(get(handles.set_tension_fit_p, 'String'));
  handles.PressProf.y_forinput=interpos(x,y,handles.PressProf.x_forinput, tensfit,[1 0],[0 0],sigma);
  handles.PressProf.x_chease = linspace(0, 1, handles.n_points_rho);
  tensprof=str2num(get(handles.set_tensprof, 'String'));
  handles.PressProf.y_chease=interpos(handles.PressProf.x_forinput,handles.PressProf.y_forinput,handles.PressProf.x_chease, tensprof,[1 0],[0 0]);

  plot(handles.PressProf.x_chease,handles.PressProf.y_chease,'k-','linewidth',2);
  plot(handles.PressProf.x_forinput,handles.PressProf.y_forinput,'b-','linewidth',2);

 case 3

  poly_press_Callback(hObject, eventdata, handles);
  handles.PressProf.x_chease = linspace(0, 1, handles.n_points_rho);
  tensprof=str2num(get(handles.set_tensprof, 'String'));
  handles.PressProf.y_chease=interpos(handles.PressProf.x_forinput,handles.PressProf.y_forinput,handles.PressProf.x_chease, tensprof,[1 0],[0 0]);

  plot(handles.PressProf.x_chease,handles.PressProf.y_chease,'k-','linewidth',2);
  hold on
  plot(handles.PressProf.x_forinput,handles.PressProf.y_forinput,'b-','linewidth',2);
  
 otherwise
  
end

set(handles.pressure_axes, 'NextPlot',aa)
zoom on;

guidata (hObject, handles);

% --- Executes during object creation, after setting all properties.
function poly_press_CreateFcn(hObject, eventdata, handles)
% hObject    handle to poly_press (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function poly_press_Callback(hObject, eventdata, handles)
% hObject    handle to poly_press (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of poly_press as text
%        str2double(get(hObject,'String')) returns contents of poly_press as a double

coefs = str2num(get(handles.poly_press, 'String'));
%build the complete profile
x = linspace(0,1,handles.n_points_rho);
y = polyval (coefs(end:-1:1),x);
subplot (handles.pressure_axes); %set current axes
plot (x,y, 'LineWidth', 2); grid on; %plot the polynomious
%save the Pressure profile into structure
handles.PressProf.x_forinput=x;
handles.PressProf.y_forinput=y;
guidata(hObject, handles);

% --- Executes on button press in add_plot_pressure.
function add_plot_pressure_Callback(hObject, eventdata, handles)
% hObject    handle to add_plot_pressure (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of add_plot_pressure

b = get (handles.add_plot_pressure, 'Value') ;
if (b==0)
  set (handles.pressure_axes, 'NextPlot', 'replace');
  set(handles.add_plot_pressure,'string','hold off')
else
  set (handles.pressure_axes, 'NextPlot', 'add');
  set(handles.add_plot_pressure,'string','hold on')
end

%------------------------------------------------------------------------

%PREPARE EXPEQ AND NAMELIST (INPUT FOR CHEASE)

% --- Executes on button press in prepare_input.
function prepare_input_Callback(hObject, eventdata, handles)
% hObject    handle to prepare_input (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%check if the function receives all the parameters
f = isfield (handles.CurrProf, 'y_forinput'); %the field CurrProf always exists into handles (in Opening funciotn I set as default handels.CurrProf.nsttp=2)
if ~f  
  errordlg('No current profile saved' , 'Cannot prepare EXPEQ file');
end
b = isfield (handles, 'PlasmaBndry');
if ~b 
  errordlg('No Plasma Boundaries saved', 'Cannot prepare EXPEQ file');
end
m = isfield (handles, 'PressProf');
if ~m
  errordlg('No Pressure Profile saved', 'Cannot prepare EXPEQ file');
end

set (handles.which_curr_prof, 'Value', handles.CurrProf.nsttp);
% create the structure EXPEQ_data 

useparambdry=get(handles.UseParameterizedBoundaryCheckBox, 'Value');
if (isfield(handles.PlasmaBndry,'rzsmoothed')) && ~useparambdry
  handles.EXPEQdata.RZ_psi=handles.PlasmaBndry.rzsmoothed;
else
  handles.EXPEQdata.RZ_psi=handles.PlasmaBndry.rz_param;
end

handles.EXPEQdata.nsttp=handles.CurrProf.nsttp;
nfunrho = get (handles.set_nfunrho, 'Value') -1;
handles.EXPEQdata.nrhotype=nfunrho; 
handles.EXPEQdata.epsilon=handles.PlasmaBndry.epsilon;
handles.EXPEQdata.zgeom= str2num(get(handles.set_Z_chease, 'String'));
handles.EXPEQdata.pedge= str2num (get (handles.set_pedge, 'String'));
handles.EXPEQdata.rho=handles.PressProf.x_forinput;
handles.EXPEQdata.Pprime = handles.PressProf.y_forinput;
% give some extralines if they are not present
f = isfield (handles.EXPEQdata, 'extralines');
if ~f 
  handles.EXPEQdata.extralines{1} = '';
  handles.EXPEQdata.extralines{2}=' ';
  r0=get(handles.set_R0exp, 'String');
  handles.EXPEQdata.extralines{3}=[r0, ' R0 [M] USED FOR CONVERTING TO MKSA'];
  b0=get(handles.set_B0exp,'String');
  handles.EXPEQdata.extralines{4} = [b0, ' B0 [T] USED FOR CONVERTING TO MKSA'];
  qvalue=get(handles.set_q, 'String');
  q_position=get(handles.q_position, 'String');
  handles.EXPEQdata.extralines{5} = [qvalue, ' Q_SPEC at s= ',q_position];
  
end

%check what kind of current profile is given to EXPEQ file

if ( handles.EXPEQdata.nsttp==1) %nsttp=1, TTprime is given
  handles.EXPEQdata.TTprime=handles.CurrProf.y_forinput; 
elseif (handles.EXPEQdata.nsttp==2) % nsttp=2, Istar is given
  handles.EXPEQdata.Istar=handles.CurrProf.y_forinput; 
elseif (handles.EXPEQdata.nsttp==3)  %nsttp=3, Jparallel is given
  handles.EXPEQdata.Jparallel=handles.CurrProf.y_forinput;
end

%write the EXPEQ file
handles.runname_expeq = get(handles.run_name,'string');
handles.name_expeq=['EXPEQ_' handles.runname_expeq];
% check for abort
if isempty(handles.name_expeq); disp('aborted CHEASE input preparation'); return; end
% create dir if necessary
if ~exist(handles.fpath); disp(['creating ',handles.fpath]); [s,w] = unix(['mkdir ',handles.fpath]); end;

% full path and file
handles.fname_expeq = fullfile(handles.fpath,deblank(handles.name_expeq));

% write EXPEQ file
write_expeq(handles.EXPEQdata,handles.fname_expeq);

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHEASE NAMELIST
%%%%%%%%%%%%%%%%%%%%%%%%%
% create the structure datanamelist with the parameters that write_namelist needs
% Ideally should set it within the callback of each parameters, so it is in same place. But it is not performed when launching the GUI
% So do it here. In 2 groups, those which have buttons, etc to be set from GUI and the variables which cannot be set yet from GUI
%
% Parameters exisiting in GUI:
handles.datanamelist.ns=str2num(get(handles.set_NS, 'String'));
handles.datanamelist.nt=str2num(get(handles.set_NT, 'String'));
handles.datanamelist.npsi=str2num(get(handles.set_NPSI, 'String'));
handles.datanamelist.niso=handles.datanamelist.npsi;
handles.datanamelist.nchi=str2num(get(handles.set_NCHI, 'String'));
handles.datanamelist.solpdpol=str2num(get(handles.set_solpdpol, 'String'));
handles.datanamelist.nrbox=str2num(get(handles.set_nrbox, 'String'));
handles.datanamelist.nzbox=str2num(get(handles.set_nzbox, 'String'));
handles.datanamelist.r0exp=str2num (get (handles.set_R0exp, 'String'));
handles.datanamelist.b0exp=str2num(get(handles.set_B0exp, 'String'));
handles.datanamelist.aspct=str2num(get(handles.aspect_ratio, 'String'));
handles.datanamelist.ncscal=get(handles.set_ncscal, 'Value');
handles.datanamelist.currt=str2num (get (handles.set_Ip_chease, 'String'));
handles.datanamelist.csspec=str2num(get(handles.q_position, 'String'));
handles.datanamelist.qspec=str2num(get(handles.set_q, 'String'));
handles.datanamelist.cpress=str2num(get(handles.set_CPRESS,'String'));
handles.datanamelist.nsttp=handles.CurrProf.nsttp;
handles.datanamelist.npropt=get(handles.set_npropt,'Value');
handles.datanamelist.pedge=str2num(get(handles.set_pedge, 'String'));
if handles.PressProf.Pprofile==1 || handles.PressProf.nppfun~=4
  handles.datanamelist.nppfun=handles.PressProf.nppfun;
elseif handles.PressProf.nppfun==4 && handles.PressProf.Pprofile==2
  % with nppfun=4 and pressure profile given set nppfun=8
  % could use it woth nppfun <4 but needs to create array of points (later)
  handles.datanamelist.nppfun=8;
else
  error(' Problem in definition of nppfun')
end
handles.datanamelist.nfunc=handles.CurrProf.nfunc;

handles.datanamelist.nfunrho=nfunrho;
handles.datanamelist.nsurf=6;
handles.datanamelist.elong=str2num(get(handles.set_k, 'String'));
handles.datanamelist.delta=str2num(get(handles.set_delta, 'String'));
handles.datanamelist.xi=str2num(get(handles.set_xi, 'String'));
handles.datanamelist.tensbnd=str2num(get(handles.set_tensbnd, 'String'));
handles.datanamelist.tensprof=str2num(get(handles.set_tensprof, 'String'));
handles.datanamelist.signb0xp=str2num(get(handles.set_signB0, 'String'));
handles.datanamelist.signipxp=str2num(get(handles.set_signIp, 'String'));
handles.datanamelist.cocos_in=str2num(get(handles.set_COCOS_IN, 'String'));
handles.datanamelist.cocos_out=str2num(get(handles.set_COCOS_OUT, 'String'));

for i=1:7
  eval(['handles.datanamelist.ap2(i)=str2num(get(handles.set_ap' num2str(i) ', ''String''));']);
  eval(['handles.datanamelist.at2(i)=str2num(get(handles.set_at' num2str(i) ', ''String''));']);
end
for i=3:7
  handles.datanamelist.ap2(i) = - handles.datanamelist.ap2(i);
end

% parameters not set from GUI but as defaults
handles.datanamelist.ndiagop=1;


%write namelist
fname = [handles.fpath, '/namelist_CHEASE_',handles.runname_expeq];
handles.fname_namelist_CHEASE=fname;
if ~isfield(handles.datanamelist,'nitmopt')
  % ITM options not defined through GUI (yet) just read defaults:
  [customizelist_H] = customize_options('customize_list_ITM.data',0);
  handles.ITM_options=customizelist_H;
  handles.datanamelist.nitmopt = str2num(handles.ITM_options.values{handles.ITM_options.initmopt});
  handles.datanamelist.nitmshot = [str2num(handles.ITM_options.values{handles.ITM_options.initmshot_1}) ...
                    str2num(handles.ITM_options.values{handles.ITM_options.initmshot_2})];
  handles.datanamelist.nitmrun = [str2num(handles.ITM_options.values{handles.ITM_options.initmrun_1}) ...
                    str2num(handles.ITM_options.values{handles.ITM_options.initmrun_2})];
  handles.datanamelist.treeitm = [{handles.ITM_options.values{handles.ITM_options.itreeitm_1}} ...
                    {handles.ITM_options.values{handles.ITM_options.itreeitm_2}}];
end
write_namelist_chease(fname,handles.datanamelist,handles.datanamelist.cocos_in);
guidata (hObject,handles);

% --------------------------------------------------------------------
function File_Callback(hObject, eventdata, handles)
% hObject    handle to File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function RunTest_Callback(hObject, eventdata, handles)
% hObject    handle to RunTest (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
disp('in RunTest_Callback')

%cd(handles.fpath);%changing directory, in order to put all the files tmp folder
%[astat,aresult] = unix(['cp ', handles.fname_expeq, ' EXPEQ']); %copy the expeq of actual equilibrium to EXPEQ(file which chease uses)
% [bstat,bresult] = unix (['cp namelist_CHEASE chease_namelist']); %copy the namelist

% RUN CHEASE
[fname_out,outglobalsvalues]=run_chease_expeq(handles.fname_namelist_CHEASE,handles.fname_expeq)
handles.fname_output_chease=fullfile(handles.fpath,['/o.chease.' handles.runname_expeq]);
handles.fname_output_chease_cols=fullfile(handles.fpath,['/o.chease.' handles.runname_expeq '.cols']);
handles.fname_expeq_out=fullfile(handles.fpath,['/EXPEQ.OUT_' handles.runname_expeq]);
handles.fname_eqdsk_out=fullfile(handles.fpath,['/EQDSK.OUT_' handles.runname_expeq]);
handles.outglobalsvalues=outglobalsvalues;
handles.fname_out = fname_out;

guidata (hObject,handles);

outglobalstoGUI(handles); % so can use it after eqdsk run or load etc

uicontrol(handles.betaNout)

% --------------------------------------------------------------------
function plot_chease_Callback(hObject, eventdata, handles)
% hObject    handle to plot_chease (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%cd(handles.fpath);
% plot_chease([],handles.fpath,handles.fname_output_chease_cols); %call the GUI plot_chease

% to bbe able to zoom and make plots in local environment

plotdatafile_H=plotdatafile(handles.fname_output_chease_cols); %call the GUI plot_chease

handles.plotdatafile_H=plotdatafile_H;
guidata (hObject,handles);
%cd(handles.olddir); %uncomment this if you want to go back to work directory after this operation

% --------------------------------------------------------------------
function run_caxe_kinx_Callback(hObject, eventdata, handles)
% hObject    handle to run_caxe_kinx (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% setting Next Plot replace for all the axes
set (handles.add_plot_plasma, 'Value', 0);
set(handles.add_plot_plasma,'string','hold off')
set (handles.plasma_axes, 'NextPlot', 'replace');
set (handles.add_plot_current, 'Value', 0);
set(handles.add_plot_current,'string','hold off')
set (handles.current_axes, 'NextPlot', 'replace');
set (handles.add_plot_pressure, 'Value', 0);
set(handles.add_plot_pressure,'string','hold off')
set (handles.pressure_axes, 'NextPlot', 'replace');

%read the expeq file for CAXE
% handles = read_EXPEQ_Callback (hObject,0,handles);

if isfield(handles,'fname_expeq')
  expeqdata=read_expeq(handles.fname_expeq);
else
  expeqdata=read_expeq;
end

%preparing CAXE input

fname=[handles.fpath, '/namelist_CAXE_' handles.runname_expeq];
if isfield(handles,'set_q')
  q0_gui=str2double(get(handles.set_q,'string'));
end
if q0_gui < 0
  q0=abs(q0_gui);
else
  [stat,q0]=unix( ['cat ', handles.fname_expeq_out, ' |grep Q_ZERO |head -1 |awk ''{print $1}''']); %extracting qzero value
  q0=str2double(q0);
  if isnan(q0) && isfield(handles,'set_q')
    q0=q0_gui;
  elseif isnan(q0)
    q0=1.05; 
  end
end
q0
[acp,bcp]=unix('which cp'); % use full path to avoid local aliases (-i in particular)
if ~isempty(strfind(bcp,'alias'))
  itcp=findstr(bcp,'cp ');
  itspace=findstr(bcp,' ');
  ilastspace=find(itspace<itcp(1));
  bcp=bcp(itspace(ilastspace(end))+1:itcp(1)+1);
else
  bcp=bcp(1:end-1); % rm end of line
end
[stat,cpexpeq]=unix([bcp ' -f ' handles.fname_expeq_out ' ' fullfile(handles.fpath,'EXPEQ')]);

if isfield(handles,'set_NA11')
  Npsi=str2double(get(handles.set_NA11,'string'));
end
if isfield(handles,'set_NT11')
  Npol=str2double(get(handles.set_NT11,'string'));
end

%writing CAXE namelist
cpress=str2double(get(handles.set_CPRESS,'string'));
write_namelist_caxe(1, handles.EXPEQdata.nsttp,q0, fname,cpress,Npsi,Npol); 

%changing directory
%cd(handles.fpath); 

%copying some files needed by CAXE and KINX
aa=which('CHEASEgui');
[a,b,c]=fileparts(aa);
[s1,s2] = unix ([bcp ' -f ' fullfile(a,'xinp.dat') ' ' fullfile(handles.fpath,'xinp.dat')]);
[s3,s4] = unix ([bcp ' -f ' fullfile(a,'dkx1nw.wal') ' '  fullfile(handles.fpath,'dkx1nw.wal')]);

%Tranforming the expeq selected in something readable by CAXE
%[a1,b1]=unix('/home/pitzsch/bin/expeq2caxe > output_EXPEQ2CAXE');
[a1,b1]=unix(['cd ' handles.fpath ' ; /home/sauter/bin/xpq2cxa_2 > output_EXPEQ2CAXE ; cd -']);

%run CAXE
disp('Running CAXE...');
handles.out_caxe = [handles.fpath, '/o.caxe.',handles.runname_expeq];
handles.namelist_caxe = [handles.fpath, '/namelist_CAXE_',handles.runname_expeq];
[a2,b2]=unix(['cd ' handles.fpath ' ; /home/sauter/bin/caxe < ' handles.namelist_caxe ' | tee ' handles.out_caxe ' ; cd -']);

% creating KINX_namelist 
Ntoroidal = str2num (get(handles.set_toroidal_number, 'String'));
initial_guess=str2num(get(handles.set_initial_guess,'String'));
fname=[handles.fpath, '/namelist_KINX'];
if isfield(handles,'set_NA11')
  Npsi=str2double(get(handles.set_NA11,'string'));
end
if isfield(handles,'set_NT11')
  Npol=str2double(get(handles.set_NT11,'string'));
end
if isfield(handles,'set_IDW')
  IDW=str2double(get(handles.set_IDW,'string'));
end
if isfield(handles,'set_CRW')
  CRW=str2double(get(handles.set_CRW,'string'));
end

write_namelist_kinx (1,Ntoroidal,initial_guess,q0,fname,Npsi,Npol,IDW,CRW);

%create KINX output file
handles.out_kinx = [handles.fpath, '/o.kinx.',handles.runname_expeq];
%run KINX
disp('Running KINX....');
[a4,b4] = unix (['cd ' handles.fpath ' ; /home/sauter/bin/kinx < namelist_KINX | tee ', handles.out_kinx ' ; cd -'])

%do some check
if a2 == 0 & a4 == 0 
  disp (['CAXE and KINX run ok! output files : ',handles.out_caxe,' and ', handles.out_kinx]); 
  guidata (hObject, handles); %update handles
else
  disp ('Error running CAXE or KINX! Check inputs!');
end

%cd(handles.olddir); %uncomment this if you want to go back to work directory after this operation

% --------------------------------------------------------------------

% --------------------------------------------------------------------
function run_kinx_Callback(hObject, eventdata, handles)
% hObject    handle to run_kinx (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% creating KINX_namelist 
[stat,q0]=unix( ['cat EXPEQ.OUT |grep Q_ZERO |head -1 |awk ''{print $1}''']);
q0=str2double(q0);
Ntoroidal = str2num (get(handles.set_toroidal_number, 'String'));
initial_guess=str2num(get(handles.set_initial_guess,'String'));
fname=[handles.fpath, '/namelist_KINX'];
write_namelist_kinx (1,Ntoroidal,initial_guess,q0,fname);

%run KINX
disp('Running KINX....');
handles.out_kinx = [handles.fpath, '/o.kinx.',handles.name_expeq];
[a4,b4] = unix (['/home/pitzsch/bin/kinx2000 < namelist_KINX | tee ', handles.out_kinx]);

%[a4,b4] = unix ('/home/pitzsch/bin/kinx2000 < namelist_KINX | tee output_KINX');

%do some check
if a4==0 disp(['KINX run ok! output file is ', handles.out_kinx]);
else disp('Error running KINX!Check inputs!');
end


function plot_kinx_Callback(hObject, eventdata, handles)
% hObject    handle to plot_kinx (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

cd(handles.fpath);
plot_kinx; %calling the GUI plot_kinx
cd(handles.olddir);  %uncomment this if you want to go back to work directory after this operation

% --------------------------------------------------------------------
function open_chease_file_Callback(hObject, eventdata, handles)
% hObject    handle to open_chease_file (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ~isfield (handles , 'runname_expeq')
  errordlg ('Impossible to open file output CHEASE...Run CHEASE first');
else
  fname=[handles.fpath, '/o.chease.',handles.runname_expeq];
  open (fname);
end

% --------------------------------------------------------------------
function open_output_kinx_Callback(hObject, eventdata, handles)
% hObject    handle to open_output_kinx (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if isfield(handles, 'out_kinx')
  open (handles.out_kinx);
else errordlg('Impossible to open KINX output file : run KINX first!');
end
% --------------------------------------------------------------------
function Quit_Callback(hObject, eventdata, handles)
% hObject    handle to Quit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% $$$ button = questdlg('Are you sure?','Warning','Yes','No','no');
% $$$ if isequal (button, 'Yes' )
% $$$   cd(handles.olddir);
   closereq;
% $$$ elseif isequal(button, 'No')
% $$$   return;
% $$$ end


% --------------------------------------------------------------------
function Miscellaneous_Callback(hObject, eventdata, handles)
% hObject    handle to Miscellaneous (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


function Edit_namelist_Callback(hObject, eventdata, handles)
% hObject    handle to Edit_namelist (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


function handles=read_EXPEQ_Callback(hObject, eventdata, handles)
% hObject    handle to read_expeq (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

spwd=pwd;
if isfield(handles,'expeqdir')
  cd(handles.expeqdir);
end
[fname,fpath] = uigetfile([{'EXPEQ*;expeq*'},{'EXPEQ*/expeq* files'}; ...
  {'*EXPEQ*;*expeq*'},{'*EXPEQ*/*expeq* files'};{'*'},{'All files'}],'Select an EXPEQ file' );
handles.expeqdir=fpath;
handles.name_expeq = fname;
handles.fname_expeq = fullfile(fpath,fname);
cd(spwd)
% [w1,w2] = unix (['cp ' handles.expeqdir handles.expeq  ' EXPEQ']); %this operation is necessary if the user wants to run CAXE (so should be done in CAXE case)
handles.EXPEQdata=read_expeq(handles.fname_expeq,1);

%enable some buttons
set(handles.set_x_press, 'Enable', 'on');
set(handles.set_y_press, 'Enable', 'on');
set(handles.plot_pressure_points, 'Enable', 'on');
set(handles.set_x_curr, 'Enable', 'on');
set(handles.set_y_curr, 'Enable', 'on');
set(handles.plot_current_points, 'Enable', 'on');
%reading and setting new nsttp
handles.CurrProf.nsttp=handles.EXPEQdata.nsttp; %set nsttp equal to the nsttp of the EXPEQ loaded
set(handles.which_curr_prof, 'Value', handles.CurrProf.nsttp);%set the popup menu accordingly
%nfunrho=handles.EXPEQdata.nrhotype;
%set (handles.set_nfunrho, 'Value', (nfunrho+1));

%calculate boundaries parameters 
% save boundary data from EXPEQ to PlasmaBndry structure
tensbnd=str2num(get(handles.set_tensbnd, 'String'));
PlasmaBndry = makePlasmaBndry_from_exp(handles.EXPEQdata.RZ_psi,[],tensbnd);
handles.PlasmaBndry = PlasmaBndry;

% set edit boxes : Note keep values as is for those not included in EXPEQ (like R0EXP)
set(handles.set_k, 'String', num2str(PlasmaBndry.kappa));
set(handles.set_Z_chease, 'String',  num2str(PlasmaBndry.Z0chease));
set(handles.set_a_chease, 'String', num2str(PlasmaBndry.achease));
set(handles.aspect_ratio, 'String', num2str(PlasmaBndry.epsilon));
set(handles.set_delta, 'String', num2str(PlasmaBndry.delta));
set(handles.set_tensbnd, 'String', num2str(PlasmaBndry.tensbnd));
set(handles.set_pedge, 'String', num2str(handles.EXPEQdata.pedge));

% check if R0, B0, Ip given in extralines notes
R0EXP = str2num(get(handles.set_R0exp, 'String'));
B0EXP = str2num(get(handles.set_B0exp, 'String'));
if isfield(handles.EXPEQdata,'extralines') && ~isempty(handles.EXPEQdata.extralines)
  for ij=1:length(handles.EXPEQdata.extralines)
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'R0 [M]'))
      R0EXP=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.set_R0exp, 'String', num2str(R0EXP));
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'B0 [T]'))
      B0EXP=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.set_B0exp, 'String', num2str(B0EXP));
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'TOTAL CURRENT'))
      Ipchease=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'Q_ZERO'))
      Q_ZERO=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.q0out, 'String', ['q0: ' num2str(Q_ZERO,'%.2f')]);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'Q_EDGE'))
      Q_EDGE=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.qaout, 'String', ['q_a: ' num2str(Q_EDGE,'%.2f')]);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'LI'))
      LI=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.liout, 'String', ['li: ' num2str(LI,'%.2f')]);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'BETA_EXP'))
      BETA_EXP=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'POLOIDAL BETA'))
      betap=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.betapout, 'String', ['Vol[m^3]: ' num2str(betap,'%.2f')]);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'VOLUM'))
      VOLUM=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.volout, 'String', ['Vol[m^3]: ' num2str(VOLUM*R0EXP^3,'%.2f')]);
    end
    if ~isempty(strfind(handles.EXPEQdata.extralines{ij},'b/a'))
      kappaa=sscanf(handles.EXPEQdata.extralines{ij},'%f',1);
      set(handles.kappaaout, 'String', ['kappa_a: ' num2str(kappaa,'%.2f')]);
    end
  end
  if exist('Ipchease') &&  exist('R0EXP') &&  exist('B0EXP') &&  exist('BETA_EXP')
    Ipph=Ipchease/(4e-7*pi/R0EXP/B0EXP);
    set(handles.Ipout, 'String', ['Ip[MA]: ' num2str(Ipph/1e6,'%.2f')]);
    betaN = BETA_EXP*100 / (Ipph/1e6/PlasmaBndry.achease/R0EXP/B0EXP);
    set(handles.betaNout, 'String', ['beta_N: ' num2str(betaN,'%.2f')]);
  end
end
PlasmaBndry.R0 = R0EXP;
PlasmaBndry.a=PlasmaBndry.achease*R0EXP;
PlasmaBndry.Z0=PlasmaBndry.Z0chease*R0EXP;
set(handles.set_Z_physic, 'String', num2str (PlasmaBndry.Z0));
set(handles.set_a_physic, 'String', num2str(PlasmaBndry.a));
set(handles.aout, 'String', ['a[m]: ' num2str(PlasmaBndry.a,'%.2f')]);
set(handles.epsout, 'String', ['a/R0: ' num2str(PlasmaBndry.epsilon,'%.2f')]);
set(handles.deltaaout, 'String', ['delta_a: ' num2str(PlasmaBndry.delta,'%.2f')]);

% use boundary from file
set(handles.UseParameterizedBoundaryCheckBox, 'Value', 0);

%plot plasma of the selected EXPEQ
Plot_plasma_Callback(hObject, eventdata, handles);

%set edit boxes of pressure profile
x = handles.EXPEQdata.rho';
set (handles.set_x_press, 'String', num2str(x));
handles.PressProf.y_forinput = x;
ypress = handles.EXPEQdata.Pprime'; %convert into a column vector
set(handles.set_y_press, 'String', num2str(ypress));
handles.PressProf.y_forinput = ypress;

%plot pressure profile of the selected EXPEQ
subplot(handles.pressure_axes);
plot (handles.EXPEQdata.rho, handles.EXPEQdata.Pprime, '--r', 'LineWidth', 2); grid on;

%set edit boxes of current profile and plot current profile
set (handles.set_x_curr, 'String', num2str(x));
switch (handles.EXPEQdata.nsttp) 
case 1
  ycurr = handles.EXPEQdata.TTprime';
  set (handles.set_y_curr, 'String', num2str (ycurr));
  subplot(handles.current_axes);
  plot(handles.EXPEQdata.rho, handles.EXPEQdata.TTprime, '--r', 'LineWidth', 2); grid on;
  handles.CurrProf.y_forinput =  handles.EXPEQdata.TTprime';
case 2
  ycurr=handles.EXPEQdata.Istar';
  set (handles.set_y_curr, 'String', num2str (ycurr));
  subplot(handles.current_axes);
  plot(handles.EXPEQdata.rho, handles.EXPEQdata.Istar, '--r', 'LineWidth',2); grid on;
  handles.CurrProf.y_forinput =  handles.EXPEQdata.Istar'; %str2num (get(handles.set_y_curr, 'String'))
case 3
  ycurr = handles.EXPEQdata.Jparallel';
  set (handles.set_y_curr, 'String', num2str (ycurr));
  subplot(handles.current_axes);
  plot (handles.EXPEQdata.rho,handles.EXPEQdata.Jparallel, '--r', 'LineWidth', 2); grid on;
  handles.CurrProf.y_forinput =  handles.EXPEQdata.Jparallel';
end
%set Property 'nextplot' to add for all axes
set (handles.add_plot_plasma, 'Value', 1);
set(handles.add_plot_plasma,'string','hold on')
set (handles.plasma_axes, 'NextPlot', 'add');
set (handles.add_plot_current, 'Value', 1);
set(handles.add_plot_current,'string','hold on')
set (handles.current_axes, 'NextPlot', 'add');
set (handles.add_plot_pressure, 'Value', 1);
set(handles.add_plot_pressure,'string','hold on')
set (handles.pressure_axes, 'NextPlot', 'add');


%save the parameters for write_expeq and write_namelist
handles.n_points_bou = handles.EXPEQdata.n_psi; % 
handles.n_points_rho = handles.EXPEQdata.n_rho;%setting the right number of points for plasma and profiles

%the other parameters are either  are going to be set in the function prepare_input_chease(zgeom, rho_type), rhogrid
%see function prepare_input_chease_Callback 
guidata (hObject, handles); %save all the changes into handles

% --------------------------------------------------------------------
function read_eqdsk_Callback(hObject, eventdata, handles)
% hObject    handle to read_eqdsk (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

[fname,pathname]  = uigetfile([{'EQDSK*;eqdsk*'},{'EQDSK* or eqdsk* files'}; ...
  {'*EQDSK*;*eqdsk*'},{'*EQDSK* or *eqdsk* files'};{'*'},{'All files'}],'Select an EQDSK file');
if isequal(fname,0) || isequal(pathname,0); return; end

% get COCOS in, then change to COCOS=2 for CHEASE
figure('menubar','none','name','ENTER COCOS VALUE OF EQDSK (2 if not known)','NumberTitle','off');
pos=get(gcf,'pos');
pos(3)=min(300,pos(3));
pos(4)=min(200,pos(4));
set(gcf,'pos',[pos(1) pos(2) pos(3) pos(4)]);
cocos_values=[1:8 11:18];
clear strpop
for ii=1:length(cocos_values)
  icos=cocos_values(ii);
  strpop{ii}=['COCOS=' num2str(icos,'%2.2d')];
end
callback_str=['icos_in=get(gcbo,''value'');handles=guidata(' num2str(hObject,'%.18e') ');' ...
              'handles.hhicos_in=icos_in;guidata(' num2str(hObject,'%.18e') ',handles);close gcf'];
handles.set_hhcocos=uicontrol('style','popupmenu','string',strpop,'position',[0.05*pos(3) 0.4*pos(4) 0.9*pos(3) 0.3*pos(4)], ...
          'callback',callback_str); 
guidata(hObject, handles);
waitfor(handles.set_hhcocos);
handles=guidata(hObject);
cocos_in = cocos_values(handles.hhicos_in)

eqdskval_in=read_eqdsk(fullfile(pathname,fname),cocos_in);
[eqdskval,eqdskval_IpB0pos] = eqdsk_cocos_transform(eqdskval_in,[cocos_in 2]);

%enable some buttons
set(handles.set_x_press, 'Enable', 'on');
set(handles.set_y_press, 'Enable', 'on');
set(handles.plot_pressure_points, 'Enable', 'on');
set(handles.set_x_curr, 'Enable', 'on');
set(handles.set_y_curr, 'Enable', 'on');
set(handles.plot_current_points, 'Enable', 'on');
%setting new nsttp
handles.CurrProf.nsttp=1; %if EQDSK is used, nsttp =1, TTptime is given
set(handles.which_curr_prof, 'Value', handles.CurrProf.nsttp);%set the popup menu accordingly

%calculate boundaries parameters , make sure R0 from eqdsk is used to correctly normalize back/forth
tensbnd=str2num(get(handles.set_tensbnd, 'String'));
PlasmaBndry = makePlasmaBndry_from_exp([eqdskval.rplas,eqdskval.zplas],eqdskval.r0,tensbnd);
handles.PlasmaBndry = PlasmaBndry;

% set edit boxes 
set(handles.set_k, 'String', num2str(PlasmaBndry.kappa));
set(handles.set_R0exp, 'String', num2str(eqdskval.r0));
set(handles.set_Z_chease, 'String',  num2str(PlasmaBndry.Z0chease));
set(handles.set_Z_physic, 'String', num2str (eqdskval.zaxis));
set(handles.set_a_chease, 'String', num2str(PlasmaBndry.achease));
set(handles.set_a_physic, 'String', num2str(PlasmaBndry.a));
set(handles.aspect_ratio, 'String', num2str(PlasmaBndry.epsilon));
set(handles.set_delta, 'String', num2str(PlasmaBndry.delta));
set(handles.set_tensbnd, 'String', num2str(PlasmaBndry.tensbnd));

pedge_chease = eqdskval.p(end) * (4e-7*pi) / (eqdskval.b0^2); %calculate chease value of p
set(handles.set_pedge, 'String', num2str(pedge_chease));
set(handles.set_B0exp, 'String', num2str(eqdskval.b0));
set(handles.set_q, 'String', num2str (eqdskval.q(1)));
set(handles.q_position, 'String', num2str(0.));

% use boundary from file
set(handles.UseParameterizedBoundaryCheckBox, 'Value', 0);

%plot plasma of the selected EXPEQ
Plot_plasma_Callback(hObject, eventdata, handles);
%set edit boxes of pressure prime profile
set(handles.set_x_press, 'String', num2str(eqdskval.rhopsi'));
handles.PressProf.x_forinput = eqdskval.rhopsi';

% effective sign of psi
zpsinorm=1.;
if (eqdskval.psiaxis > eqdskval.psiedge) zpsinorm=-1.; end

if handles.PressProf.Pprofile==1;
  ypress_physic = eqdskval.pprime'; %convert into a column vector. EQDSKgives always PHYSICS values
  ypress_chease = zpsinorm .* ypress_physic * (4e-7*pi*PlasmaBndry.R0^2)/abs(eqdskval.b0); %extracting CHEASEvalues
else
  ypress_physic = eqdskval.p'; %convert into a column vector. EQDSKgives always PHYSICS values
  ypress_chease = ypress_physic * (4e-7*pi)/abs(eqdskval.b0).^2; %extracting CHEASEvalues
end
set(handles.set_y_press, 'String', num2str(ypress_chease));
handles.PressProf.y_forinput = ypress_chease;

%plot pressure profile of the selected EQDSK
subplot(handles.pressure_axes);
plot (eqdskval.rhopsi, ypress_chease , '--r', 'LineWidth', 2); grid on;

%set edit boxes of current profile and plot current profile
set (handles.set_x_curr, 'String', num2str(eqdskval.rhopsi'));
ycurr_physic = eqdskval.FFprime'; %convert into a column vector. EQDSKgives always PHYSIC values
ycurr_chease = zpsinorm .* ycurr_physic / abs(eqdskval.b0); %extracting CHEASEvalues
set (handles.set_y_curr, 'String', num2str (ycurr_chease));
handles.CurrProf.y_forinput =  ycurr_chease;

subplot(handles.current_axes);
plot(eqdskval.rhopsi, ycurr_chease , '--r', 'LineWidth', 2); grid on;

%set Property 'nextplot' to add for all axes
set (handles.add_plot_plasma, 'Value', 1);
set (handles.plasma_axes, 'NextPlot', 'add');
set (handles.add_plot_current, 'Value', 1);
set (handles.current_axes, 'NextPlot', 'add');
set (handles.add_plot_pressure, 'Value', 1);
set (handles.pressure_axes, 'NextPlot', 'add');

%save the parameters for write_expeq and write_namelist
%setting the right number of points for profiles
handles.n_points_bou = eqdskval.nbbound; %number points of plasma boundary
handles.n_points_rho = eqdskval.nr;%number of points of profiles

%the other parameters are either already set are going to be set in the function prepare_input_chease(zgeom, rho_type, rhogrid)
%see function prepare_input_chease_Callback
guidata (hObject, handles); %save all the changes into handles



% --------------------------------------------------------------------
function get_variables_Callback(hObject, eventdata, handles)
% hObject    handle to get_variables (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

Current=handles.CurrProf;
% save('CurrProf.mat', 'Current');
Pressure=handles.PressProf;
% save('PressProf.mat', 'Pressure');
Plasma=handles.PlasmaBndry;
% save('PlasmaBndry', 'Plasma');
assignin('base','cheasegui_current',Current);
assignin('base','cheasegui_pressure',Pressure);
assignin('base','cheasegui_plasma',Plasma);
disp('cheasegui_current, _pressure, _plasma added to workspace')

% --------------------------------------------------------------------
function number_points_Callback(hObject, eventdata, handles)
% hObject    handle to number_points (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles.number_points=[handles.number_points, untitled3];
guidata(hObject,handles);

%---------------------------------------------------------------------


% --- Executes during object creation, after setting all properties.

function set_Ip_physic_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_Ip_physic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_Ip_physic_Callback(hObject, eventdata, handles)
% hObject    handle to set_Ip_physic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_Ip_physic as text
%        str2double(get(hObject,'String')) returns contents of set_Ip_physic as a double

% in this case Ip_chease = Ip_physic * mu0 / (R0exp*B0exp)
Ip_physic = str2num (get (handles.set_Ip_physic, 'String'));
mu0 = 4e-7*pi;
R0 = str2num (get (handles.set_R0exp, 'String'));
B0 = str2num (get (handles.set_B0exp, 'String'));
normaliz = mu0 / (B0*R0);
Ip_chease = Ip_physic * normaliz;
set (handles.set_Ip_chease, 'String', num2str(Ip_chease));

% --- Executes during object creation, after setting all properties.
function set_R0exp_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_R0exp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','red');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_R0exp_Callback(hObject, eventdata, handles)
% hObject    handle to set_R0exp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_R0exp as text
%        str2double(get(hObject,'String')) returns contents of set_R0exp as a double
%set all the lenght parameters (chease values and physic values)
normaliz = str2num (get (handles.set_R0exp, 'String')); %normalization parameter
%set Z chease witrh the new normalization parameter
Zphysic = str2num (get(handles.set_Z_physic, 'String'));
Zchease = Zphysic/normaliz;
set ( handles.set_Z_chease, 'String', num2str(Zchease)); 
%set a with the new normalization parameter
a_physic= str2num (get(handles.set_a_physic, 'String'));
a_chease = a_physic/normaliz;
set (handles.set_a_chease, 'String', num2str(a_chease));

% --- Executes during object creation, after setting all properties.
function set_B0exp_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_B0exp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','red');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_B0exp_Callback(hObject, eventdata, handles)
% hObject    handle to set_B0exp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_B0exp as text
%        str2double(get(hObject,'String')) returns contents of set_B0exp as a double


% --- Executes during object creation, after setting all properties.
function set_Z_physic_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_Z_physic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


function set_Z_physic_Callback(hObject, eventdata, handles)
% hObject    handle to set_Z_physic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_Z_physic as text
%        str2double(get(hObject,'String')) returns contents of set_Z_physic as a double
%physic value of Z is changed, so also chease value must be changed 
physic = str2num (get(handles.set_Z_physic, 'String'));
normaliz = str2num (get (handles.set_R0exp, 'String'));
chease = physic /normaliz;
set (handles.set_Z_chease, 'String', num2str(chease));


% --- Executes during object creation, after setting all properties.
function set_a_physic_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_a_physic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_a_physic_Callback(hObject, eventdata, handles)
% hObject    handle to set_a_physic (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_a_physic as text
%        str2double(get(hObject,'String')) returns contents of set_a_physic as a double


normaliz = str2num (get (handles.set_R0exp, 'String'));
physic = str2num (get (handles.set_a_physic, 'String'));
chease = physic / normaliz;
set (handles.set_a_chease, 'String', num2str(chease));
set (handles.aspect_ratio, 'String', num2str(chease));

function set_R_normal_Callback(hObject, eventdata, handles)
% hObject    handle to set_R_normal (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_R_normal as text
%        str2double(get(hObject,'String')) returns contents of set_R_normal as a double


% --- Executes during object creation, after setting all properties.
function q_position_CreateFcn(hObject, eventdata, handles)
% hObject    handle to q_position (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','grey');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function q_position_Callback(hObject, eventdata, handles)
% hObject    handle to q_position (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of q_position as text
%        str2double(get(hObject,'String')) returns contents of q_position as a double


% --- Executes during object creation, after setting all properties.
function set_q_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_q (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_q_Callback(hObject, eventdata, handles)
% hObject    handle to set_q (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_q as text
%        str2double(get(hObject,'String')) returns contents of set_q as a double


% --- Executes during object creation, after setting all properties.
function set_nfunrho_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_nfunrho (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


% --- Executes on selection change in set_nfunrho.
function set_nfunrho_Callback(hObject, eventdata, handles)
% hObject    handle to set_nfunrho (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns set_nfunrho contents as cell array
%        contents{get(hObject,'Value')} returns selected item from set_nfunrho

% --- Executes during object creation, after setting all properties.
function set_ncscal_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ncscal (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


% --- Executes on selection change in set_ncscal.
function set_ncscal_Callback(hObject, eventdata, handles)
% hObject    handle to set_ncscal (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns set_ncscal contents as cell array
%        contents{get(hObject,'Value')} returns selected item from set_ncscal

value=get(hObject,'Value');
switch(value)
case 1
  set (handles.set_Ip_chease, 'Enable', 'Off');
  set (handles.set_Ip_physic, 'Enable', 'Off');
  set (handles.set_q, 'Enable', 'On');
  set (handles.q_position, 'Enable', 'On');
case 2
  set (handles.set_Ip_chease, 'Enable', 'On');
  set (handles.set_Ip_physic, 'Enable', 'On');
  set (handles.set_q, 'Enable', 'Off');
  set (handles.q_position, 'Enable', 'Off');
end

% --- Executes during object creation, after setting all properties.
function set_NS_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_NS_Callback(hObject, eventdata, handles)
% hObject    handle to set_NS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_NS as text
%        str2double(get(hObject,'String')) returns contents of set_NS as a double

% --- Executes during object creation, after setting all properties.
function set_NT_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NT (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_NT_Callback(hObject, eventdata, handles)
% hObject    handle to set_NT (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_NT as text
%        str2double(get(hObject,'String')) returns contents of set_NT as a double

% --- Executes during object creation, after setting all properties.
function set_NPSI_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NPSI (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_NPSI_Callback(hObject, eventdata, handles)
% hObject    handle to set_NPSI (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_NPSI as text
%        str2double(get(hObject,'String')) returns contents of set_NPSI as a double

% --- Executes during object creation, after setting all properties.
function set_NCHI_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NCHI (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
  set(hObject,'BackgroundColor','white');
else
  set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end



function set_NCHI_Callback(hObject, eventdata, handles)
% hObject    handle to set_NCHI (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_NCHI as text
%        str2double(get(hObject,'String')) returns contents of set_NCHI as a double

function set_toroidal_number_Callback(hObject, eventdata, handles)
% hObject    handle to set_toroidal_number (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_toroidal_number as text
%        str2double(get(hObject,'String')) returns contents of set_toroidal_number as a double


% --- Executes during object creation, after setting all properties.
function set_toroidal_number_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_toroidal_number (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_CPRESS_Callback(hObject, eventdata, handles)
% hObject    handle to set_CPRESS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_CPRESS as text
%        str2double(get(hObject,'String')) returns contents of set_CPRESS as a double

% --- Executes during object creation, after setting all properties.
function set_CPRESS_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_CPRESS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in set_NPPFUN.
function set_NPPFUN_Callback(hObject, eventdata, handles)
% hObject    handle to set_NPPFUN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns set_NPPFUN contents as cell array
%        contents{get(hObject,'Value')} returns selected item from set_NPPFUN
v = get (handles.set_NPPFUN, 'Value');
handles.PressProf.nppfun = v;

switch(v);
case 1
  set (handles.set_ap1, 'Enable', 'off');
  set (handles.set_ap2, 'Enable', 'off');
  set (handles.set_ap3, 'Enable', 'off');
  set (handles.set_ap4, 'Enable', 'off');
  set (handles.set_ap5, 'Enable', 'off');
  set (handles.set_ap6, 'Enable', 'off');
  set (handles.set_ap7, 'Enable', 'off');
  set(handles.plot_ap, 'Enable', 'off');
  set(handles.poly_press , 'Enable' , 'on');
  set(handles.plot_pressure_points, 'Enable', 'on');
  set(handles.set_x_press, 'Enable', 'off');
  set(handles.set_y_press, 'Enable', 'off');
case 2

  set (handles.set_ap1, 'Enable', 'on');
  set (handles.set_ap2, 'Enable', 'on');
  set (handles.set_ap3, 'Enable', 'on');
  set (handles.set_ap4, 'Enable', 'on');
  set (handles.set_ap5, 'Enable', 'on');
  set (handles.set_ap6, 'Enable', 'on');
  set (handles.set_ap7, 'Enable', 'on');
  set (handles.plot_ap, 'Enable', 'on');
  set(handles.plot_pressure_points, 'Enable', 'off');
  disp('Note that within CHEASE these are used to define -p'' so ap(3:7) have minus sign added')
  
case 3
  disp('not set yet')
  
case 4
  disp('profiles given with points')
  set(handles.plot_ap, 'Enable', 'off');
  set(handles.poly_press , 'Enable' , 'on');
  set(handles.plot_pressure_points, 'Enable', 'on');
  set(handles.set_x_press, 'Enable', 'on');
  set(handles.set_y_press, 'Enable', 'on');
  
end

guidata (hObject, handles); %save all the changes into handles

% --- Executes during object creation, after setting all properties.
function set_NPPFUN_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NPPFUN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap1_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap1 as text
%        str2double(get(hObject,'String')) returns contents of set_ap1 as a double


% --- Executes during object creation, after setting all properties.
function set_ap1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap2_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap2 as text
%        str2double(get(hObject,'String')) returns contents of set_ap2 as a double


% --- Executes during object creation, after setting all properties.
function set_ap2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap3_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap3 as text
%        str2double(get(hObject,'String')) returns contents of set_ap3 as a double


% --- Executes during object creation, after setting all properties.
function set_ap3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap4_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap4 as text
%        str2double(get(hObject,'String')) returns contents of set_ap4 as a double


% --- Executes during object creation, after setting all properties.
function set_ap4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap5_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap5 as text
%        str2double(get(hObject,'String')) returns contents of set_ap5 as a double


% --- Executes during object creation, after setting all properties.
function set_ap5_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap6_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap6 as text
%        str2double(get(hObject,'String')) returns contents of set_ap6 as a double


% --- Executes during object creation, after setting all properties.
function set_ap6_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_ap7_Callback(hObject, eventdata, handles)
% hObject    handle to set_ap7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_ap7 as text
%        str2double(get(hObject,'String')) returns contents of set_ap7 as a double


% --- Executes during object creation, after setting all properties.
function set_ap7_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_ap7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in plot_ap.
function plot_ap_Callback(hObject, eventdata, handles)
% hObject    handle to plot_ap (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

for i=1:7
  eval(['ap' num2str(i) '=str2num(get(handles.set_ap' num2str(i) ', ''String''));']);
end

if ap1<=0
  if ap1<0.
    disp(['ap1= ' num2str(ap1) ' should be >= 0, changed to 0'])
    ap1=0.;
  end
  x1=0.;
  y1=-ap5;
else
  [x1,y1] = linear_profile(0,ap1,-ap5,-ap4,1);
end

if ap2<ap1
  disp('warning, ap2<ap1, set to ap1')
  ap2=ap1;
end

xout=linspace(0,1,handles.n_points_rho);
if (ap2-ap1)<=0.
  % no 2nd interval
  if ap1<1.
    ap8=y1(end);
    [x3,y3] = quadratic_profile (ap1,1.0,ap8,-ap6,-ap7,1);
    tmpx=[x1(1:end-1),x3];
  else
    tmpx = x1;
    y3 = y1(end);
  end
  handles.PressProf.y_forinput=interpos(tmpx,-[y1(1:end-1),y3],xout);
else
  %ap8 is the value of the function in x=ap1
  ap8=y1(end);
  [x2,y2] = cubic_profile (ap1,ap2,ap8,-ap3,-ap4,-ap7);
  if ap2<1.
    ap8=y2(end);
    [x3,y3] = quadratic_profile (ap2,1.0,ap8,-ap6,-ap7,1);
  else
    if ap2>1.
      disp(['ap2= ' num2str(ap2) ' should be <= 1, changed to 1'])
      ap2=1.;
    end
    x3=1.;
    y3=y2(end);
  end
  tmpx=[x1,x2(2:end-1),x3];
  handles.PressProf.y_forinput=interpos(tmpx,-[y1,y2(2:end-1),y3],xout);
end

handles.PressProf.x_forinput = xout;
subplot(handles.pressure_axes);
plot(handles.PressProf.x_forinput,handles.PressProf.y_forinput);
guidata(hObject,handles);


% --- Executes on selection change in set_nfunc.
function set_nfunc_Callback(hObject, eventdata, handles)
% hObject    handle to set_nfunc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns set_nfunc contents as cell array
%        contents{get(hObject,'Value')} returns selected item from set_nfunc

v = get (handles.set_nfunc, 'Value');
handles.CurrProf.nfunc = v;

switch(v);
case 1
  set (handles.set_at1, 'Enable', 'off');
  set (handles.set_at2, 'Enable', 'off');
  set (handles.set_at3, 'Enable', 'off');
  set (handles.set_at4, 'Enable', 'off');
  set (handles.set_at5, 'Enable', 'off');
  set (handles.set_at6, 'Enable', 'off');
  set (handles.set_at7, 'Enable', 'off');
  set(handles.plot_nfunc, 'Enable', 'off');
  set(handles.poly_curr , 'Enable' , 'on');
  set(handles.plot_current_points, 'Enable', 'on');
  set(handles.set_x_curr, 'Enable', 'off');
  set(handles.set_y_curr, 'Enable', 'off');
case 2
  set (handles.set_at1, 'Enable', 'on');
  set (handles.set_at2, 'Enable', 'on');
  set (handles.set_at3, 'Enable', 'on');
  set (handles.set_at4, 'Enable', 'on');
  set (handles.set_at5, 'Enable', 'on');
  set (handles.set_at6, 'Enable', 'on');
  set (handles.set_at7, 'Enable', 'on');
  set(handles.plot_nfunc, 'Enable', 'on');
  set(handles.plot_current_points, 'Enable', 'off');

 case 3
  disp('not set yet')
  
 case 4
  disp('profiles given with points')
  set(handles.plot_nfunc, 'Enable', 'off');
  set(handles.poly_curr , 'Enable' , 'on');
  set(handles.plot_current_points, 'Enable', 'on');
  set(handles.set_x_curr, 'Enable', 'on');
  set(handles.set_y_curr, 'Enable', 'on');

end

guidata (hObject, handles); %save all the changes into handles

% --- Executes during object creation, after setting all properties.
function set_nfunc_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_nfunc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at1_Callback(hObject, eventdata, handles)
% hObject    handle to set_at1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at1 as text
%        str2double(get(hObject,'String')) returns contents of set_at1 as a double


% --- Executes during object creation, after setting all properties.
function set_at1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at2_Callback(hObject, eventdata, handles)
% hObject    handle to set_at2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at2 as text
%        str2double(get(hObject,'String')) returns contents of set_at2 as a double


% --- Executes during object creation, after setting all properties.
function set_at2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at3_Callback(hObject, eventdata, handles)
% hObject    handle to set_at3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at3 as text
%        str2double(get(hObject,'String')) returns contents of set_at3 as a double


% --- Executes during object creation, after setting all properties.
function set_at3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at4_Callback(hObject, eventdata, handles)
% hObject    handle to set_at4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at4 as text
%        str2double(get(hObject,'String')) returns contents of set_at4 as a double


% --- Executes during object creation, after setting all properties.
function set_at4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at5_Callback(hObject, eventdata, handles)
% hObject    handle to set_at5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at5 as text
%        str2double(get(hObject,'String')) returns contents of set_at5 as a double


% --- Executes during object creation, after setting all properties.
function set_at5_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at6_Callback(hObject, eventdata, handles)
% hObject    handle to set_at6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at6 as text
%        str2double(get(hObject,'String')) returns contents of set_at6 as a double


% --- Executes during object creation, after setting all properties.
function set_at6_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_at7_Callback(hObject, eventdata, handles)
% hObject    handle to set_at7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_at7 as text
%        str2double(get(hObject,'String')) returns contents of set_at7 as a double


% --- Executes during object creation, after setting all properties.
function set_at7_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_at7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



% --- Executes on button press in plot_nfunc.
function plot_nfunc_Callback(hObject, eventdata, handles)
% hObject    handle to plot_nfunc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

for i=1:7
  eval(['at' num2str(i) '=str2num(get(handles.set_at' num2str(i) ', ''String''));']);
end

if at1<=0
  if at1<0.
    disp(['at1= ' num2str(at1) ' should be >= 0, changed to 0'])
    at1=0.;
  end
  x1=at1;
  y1=at3;
else
  [x1,y1] = quadratic_profile (0, at1, at3, at4, at5, 3);
end

if at2<at1
  disp('warning, at2<at1, set to at1')
  at2=at1;
end

if at2<1.
  [x3,y3]=linear_profile(at2, 1, at6, at7, 3);
else
  if at2>1.
    disp(['at2= ' num2str(at2) ' should be <= 1, changed to 1'])
    at2=1.;
  end
  x3=1.;
  y3=at6;
end

%getting the value of the function in x=at1 and in x=at2
xout=linspace(0,1,handles.n_points_rho);
if (at2-at1)<=0.
  % no cubic interval
  if at1<1.
    xgrid=[x1(1:end-1),x3];
  else
    % no linear part
    xgrid = x1;
    y3=y1(end);
  end
  handles.CurrProf.y_forinput=interpos(xgrid,[y1(1:end-1),y3],xout,0);
else
  at8 = y1(end); %the value of the function in x=at1 is the last element of the vector y1
  at9 = y3(1) ;%the value of the function in x=at2 is the first element of the vector y3
  [x2,y2] = cubic_profile (at1, at2, at8, at9, at5, at7);
  xgrid=[x1,x2(2:end-1),x3];
  handles.CurrProf.y_forinput=interpos(xgrid,[y1,y2(2:end-1),y3],xout,0);
end

handles.CurrProf.x_forinput = xout;
subplot(handles.current_axes);
plot(handles.CurrProf.x_forinput,handles.CurrProf.y_forinput);
guidata(hObject,handles);

%at8=(at7-(at6/(at1-at2)))*(at2-at1);


% --- Executes on selection change in set_npropt.
function set_npropt_Callback(hObject, eventdata, handles)
% hObject    handle to set_npropt (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns set_npropt contents as cell array
%        contents{get(hObject,'Value')} returns selected item from set_npropt


% --- Executes during object creation, after setting all properties.
function set_npropt_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_npropt (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end



function set_initial_guess_Callback(hObject, eventdata, handles)
% hObject    handle to set_initial_guess (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_initial_guess as text
%        str2double(get(hObject,'String')) returns contents of set_initial_guess as a double


% --- Executes during object creation, after setting all properties.
function set_initial_guess_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_initial_guess (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
  set(hObject,'BackgroundColor','white');
end


% --- Executes when figure1 is resized.
function figure1_ResizeFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in UseParameterizedBoundaryCheckBox.
function UseParameterizedBoundaryCheckBox_Callback(hObject, eventdata, handles)
% hObject    handle to UseParameterizedBoundaryCheckBox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of UseParameterizedBoundaryCheckBox

value=get(hObject,'Value');

% Since it is a toggle button, if here means value has been changed
% reads and plots again plasma boundaries
Plot_plasma_Callback(hObject, eventdata, handles);


function set_tensbnd_Callback(hObject, eventdata, handles)
% hObject    handle to set_tensbnd (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_tensbnd as text
%        str2double(get(hObject,'String')) returns contents of set_tensbnd as a double

tensbnd=str2num(get(handles.set_tensbnd, 'String'));
if isfield(handles,'PlasmaBndry')
  PlasmaBndry = handles.PlasmaBndry; % get existing structure
  if isfield(PlasmaBndry,'rzexp')
    % recompute smoothed boundary
    PlasmaBndry = makePlasmaBndry_from_exp(PlasmaBndry,[],tensbnd);
  end
  handles.PlasmaBndry = PlasmaBndry;
end

guidata(hObject, handles);

% --- Executes during object creation, after setting all properties.
function set_tensbnd_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_tensbnd (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

%%%%%%%%%%%% OTHER FUNCTIONS %%%%%%%%%%%%%
function PlasmaBndry = makePlasmaBndry_from_params(PlasmaBndry)
% calculate and add rz_param from existing PlasmaBndry parameters
if ~all(isfield(PlasmaBndry,{'R0','Z0','a','kappa','delta','xi','epsilon'}))
  error('missing fields for boundary')
end

theta = linspace(0,2.*pi,PlasmaBndry.n_points_bou)';
r_params = PlasmaBndry.R0 + PlasmaBndry.a .* cos(theta + PlasmaBndry.delta*sin(theta) +...
    PlasmaBndry.xi*sin(2*theta) );
z_params = PlasmaBndry.Z0 + PlasmaBndry.a * ...
    PlasmaBndry.kappa * sin(theta);

PlasmaBndry.rz_param = [r_params, z_params]; 
return

function PlasmaBndry = makePlasmaBndry_from_exp(rzdata,r0exp,tensbnd_in)

if isstruct(rzdata)
  PlasmaBndry = rzdata;
else
  % calculates geometrical parameters from rz data, writes to PlasmaBndry structure
  R = rzdata(:,1); Z = rzdata(:,2);
  Rmax = max(R); Rmin=min(R); Zmax=max(Z); Zmin=min(Z);
  kappa=(Zmax-Zmin)/(Rmax-Rmin); %calculate kappa
  Rgeom=(Rmax+Rmin)/2.;
  if exist('r0exp') && ~isempty(r0exp)
    R0=r0exp;
  else
    R0=Rgeom;
  end
  Z0=(Zmax+Zmin)/2;%calculate Z0
  a = (Rmax-Rmin)/2; %calculate a
  epsilon=a/R0; %calculate the inverse aspect ratio
  [m,i] = max(Z); Rzmax = R(i); % R at maximum z
  delta = (Rgeom - Rzmax)/a;%calculate delta
  
  Z0chease = Z0/R0; achease = a/R0; % normalize to CHEASE units
  rzchease = rzdata/R0;
  
  xi = 1; 
  
  % assign to structure
  PlasmaBndry = [];
  PlasmaBndry.R0 = R0; 
  PlasmaBndry.Z0 = Z0; 
  PlasmaBndry.a = a;
  PlasmaBndry.Z0chease = Z0chease; 
  PlasmaBndry.achease = achease;
  PlasmaBndry.kappa = kappa; 
  PlasmaBndry.epsilon = epsilon; 
  PlasmaBndry.delta = delta; 
  PlasmaBndry.xi = xi;
  PlasmaBndry.rzexp = rzchease;
  PlasmaBndry.n_points_bou = length(rzdata(:,1));
end

% smooth plasma boundary if tensbnd_in non-zero
% Add rho(theta) of plasma boundary and 1st, 2nd derivatives for testing input
if exist('tensbnd_in') && ~isempty(tensbnd_in)
  tensbnd=tensbnd_in;
else
  tensbnd=0.;
end
if tensbnd ~= 0
  R0chease = 1.;
  Z0chease = PlasmaBndry.Z0chease;
  rho_LCFS=sqrt((PlasmaBndry.rzexp(:,1)-R0chease).^2 + (PlasmaBndry.rzexp(:,2)-Z0chease).^2);
  theta_LCFS=atan2(PlasmaBndry.rzexp(:,2)-Z0chease,PlasmaBndry.rzexp(:,1)-R0chease);
  [thetasorted_LCFS,isort]=sort(theta_LCFS);
  rhosorted_LCFS=rho_LCFS(isort);
  [rhofitted]=interpos(thetasorted_LCFS,rhosorted_LCFS,thetasorted_LCFS,tensbnd,[-1 -1],2.*pi);
  Rsmoothed = R0chease + rhofitted.*cos(thetasorted_LCFS);
  Zsmoothed = Z0chease + rhofitted.*sin(thetasorted_LCFS);
else
  Rsmoothed = PlasmaBndry.rzexp(:,1);
  Zsmoothed = PlasmaBndry.rzexp(:,2);
end
PlasmaBndry.tensbnd = tensbnd;
PlasmaBndry.rzsmoothed = [Rsmoothed,Zsmoothed];

PlasmaBndry = makePlasmaBndry_from_params(PlasmaBndry); 
% calculate new bdry from params, adds rz_params field

return

% --- Executes during object creation, after setting all properties.
function figure1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% --- Executes on mouse press over figure background.
function figure1_ButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% New function independant
function [xpoints,ypoints,yout]=get_xy_points(subplot_handle,xout,tension);

subplot(subplot_handle);
[x, y]=ginput;

if ~exist('tension')
  tenseff = -0.1;
else
  tenseff = tension;
end

%construct the complete current profile
% make sure in ascending order
[xsort,isort]=unique(x);
% suppress identical values
xpoints = xsort;
ypoints = y(isort);
if (xpoints(1) > 0) && (xpoints(end) < 1.)
  xeff = [0. ; xpoints ; 1.];
  yeff = [ypoints(1) ; ypoints ; ypoints(end)];
  sigma = [1e3 ; ones(size(xpoints)) ; 1e3];
elseif (xpoints(1) > 0)
  xeff = [0. ; xpoints ];
  yeff = [ypoints(1) ; ypoints];
  sigma = [1e3 ; ones(size(xpoints))];
else
  xeff = [xpoints ; 1.];
  yeff = [ypoints ; ypoints(end)];
  sigma = [ones(size(xpoints)) ; 1e3];
end
yout=interpos(xeff,yeff,xout,tenseff,[1 0],[0 0],sigma);

aa=get(subplot_handle,'nextplot');
hold off
plot (xout, yout , 'b-', 'LineWidth', 2); grid on;
hold on
plot (x, y , 'r--', 'LineWidth', 2); grid on;
set(subplot_handle,'nextplot',aa);
zoom on

return

% --- Executes during object creation, after setting all properties.
function plot_pressure_points_CreateFcn(hObject, eventdata, handles)
% hObject    handle to plot_pressure_points (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called



function set_signB0_Callback(hObject, eventdata, handles)
% hObject    handle to set_signB0 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_signB0 as text
%        str2double(get(hObject,'String')) returns contents of set_signB0 as a double


% --- Executes during object creation, after setting all properties.
function set_signB0_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_signB0 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_signIp_Callback(hObject, eventdata, handles)
% hObject    handle to set_signIp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_signIp as text
%        str2double(get(hObject,'String')) returns contents of set_signIp as a double


% --- Executes during object creation, after setting all properties.
function set_signIp_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_signIp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function run_name_Callback(hObject, eventdata, handles)
% hObject    handle to run_name (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of run_name as text
%        str2double(get(hObject,'String')) returns contents of run_name as a double


% --- Executes during object creation, after setting all properties.
function run_name_CreateFcn(hObject, eventdata, handles)
% hObject    handle to run_name (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in popupmenuPprofile.
function popupmenuPprofile_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenuPprofile (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns popupmenuPprofile contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenuPprofile
v = get (handles.popupmenuPprofile, 'Value');
handles.PressProf.Pprofile=get(handles.popupmenuPprofile,'value');
guidata(hObject,handles);


% --- Executes during object creation, after setting all properties.
function popupmenuPprofile_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenuPprofile (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

%
function outglobalstoGUI(handles)
% hObject    handle to q0out (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

outglobalsvalues=handles.outglobalsvalues;

set(handles.q0out, 'String', ['q0: ' num2str(outglobalsvalues.q0,'%.2f')]);
set(handles.qaout, 'String', ['q_a: ' num2str(outglobalsvalues.qedge,'%.2f')]);
set(handles.qminout, 'String', ['q_min: ' num2str(outglobalsvalues.qmin,'%.2f')]);
set(handles.rhopsiqminout, 'String', ['q_min: ' num2str(outglobalsvalues.rhopsiqmin,'%.2f')]);
set(handles.q95out, 'String', ['q95: ' num2str(outglobalsvalues.q95,'%.2f')]);
set(handles.liout, 'String', ['li: ' num2str(outglobalsvalues.li,'%.2f')]);
set(handles.betapout, 'String', ['betap: ' num2str(outglobalsvalues.betap,'%.2f')]);
set(handles.volout, 'String', ['Vol[m^3]: ' num2str(outglobalsvalues.volumchease*outglobalsvalues.r0exp^3,'%.2f')]);
set(handles.kappaaout, 'String', ['kappa_a: ' num2str(outglobalsvalues.kappa_edge,'%.2f')]);
set(handles.deltaaout, 'String', ['delta_a: ' num2str(outglobalsvalues.delta,'%.2f')]);
set(handles.Ipout, 'String', ['Ip[MA]: ' num2str(outglobalsvalues.ip_phys/1e6,'%.2f')]);
set(handles.betaNout, 'String', ['beta_N: ' num2str(outglobalsvalues.betan,'%.2f')]);
set(handles.aout, 'String', ['a[m]: ' num2str(outglobalsvalues.achease*outglobalsvalues.r0exp,'%.2f')]);
set(handles.epsout, 'String', ['a/R0: ' num2str(outglobalsvalues.epsilon_edge,'%.2f')]);



function set_COCOS_IN_Callback(hObject, eventdata, handles)
% hObject    handle to set_COCOS_IN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_COCOS_IN as text
%        str2double(get(hObject,'String')) returns contents of set_COCOS_IN as a double


% --- Executes during object creation, after setting all properties.
function set_COCOS_IN_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_COCOS_IN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_COCOS_OUT_Callback(hObject, eventdata, handles)
% hObject    handle to set_COCOS_OUT (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_COCOS_OUT as text
%        str2double(get(hObject,'String')) returns contents of set_COCOS_OUT as a double


% --- Executes during object creation, after setting all properties.
function set_COCOS_OUT_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_COCOS_OUT (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_CRW_Callback(hObject, eventdata, handles)
% hObject    handle to set_CRW (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_CRW as text
%        str2double(get(hObject,'String')) returns contents of set_CRW as a double


% --- Executes during object creation, after setting all properties.
function set_CRW_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_CRW (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_NA11_Callback(hObject, eventdata, handles)
% hObject    handle to set_NA11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_NA11 as text
%        str2double(get(hObject,'String')) returns contents of set_NA11 as a double


% --- Executes during object creation, after setting all properties.
function set_NA11_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NA11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_NT11_Callback(hObject, eventdata, handles)
% hObject    handle to set_NT11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_NT11 as text
%        str2double(get(hObject,'String')) returns contents of set_NT11 as a double


% --- Executes during object creation, after setting all properties.
function set_NT11_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_NT11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_IDW_Callback(hObject, eventdata, handles)
% hObject    handle to set_IDW (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_IDW as text
%        str2double(get(hObject,'String')) returns contents of set_IDW as a double


% --- Executes during object creation, after setting all properties.
function set_IDW_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_IDW (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --------------------------------------------------------------------
function read_namelist_Callback(hObject, eventdata, handles)
% hObject    handle to read_namelist (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

spwd=pwd;
if isfield(handles,'expeqdir')
  cd(handles.expeqdir);
end
[fname,fpath] = uigetfile([{'chease_namelist*'},{'chease_name* files'}; ...
  {'o.*'},{'o.* files'};{'*'},{'All files'}],'Select a file containing chease namelist at the top' );
handles.name_namelistin = fname;
handles.fname_namelistin = fullfile(fpath,fname);
handles.expeqdir=fpath;
cd(spwd);

nl=read_namelist_chease(handles.fname_namelistin);
set(handles.set_NS, 'String',num2str(nl.ns));
set(handles.set_NT, 'String',num2str(nl.nt));
set(handles.set_NPSI, 'String',num2str(nl.npsi));
set(handles.set_NCHI, 'String',num2str(nl.nchi));
set(handles.set_nrbox, 'String',num2str(nl.nrbox));
set(handles.set_nzbox, 'String',num2str(nl.nzbox));
set(handles.set_solpdpol, 'String',num2str(nl.solpdpol));
set (handles.set_R0exp, 'String',num2str(nl.r0exp));
set(handles.set_B0exp, 'String',num2str(nl.b0exp));
set(handles.aspect_ratio, 'String',num2str(nl.aspct));
set(handles.set_ncscal, 'Value',nl.ncscal);
set (handles.set_Ip_chease, 'String',num2str(nl.currt));
set(handles.q_position, 'String',num2str(nl.csspec));
set(handles.set_q, 'String',num2str(nl.qspec));
set(handles.set_CPRESS,'String',num2str(nl.cpress));
handles.CurrProf.nsttp = nl.nsttp;
set(handles.set_npropt,'Value',nl.npropt);
handles.PressProf.nppfun = nl.nppfun;
handles.PressProf.Pprofile=1;
if handles.PressProf.nppfun==8
  handles.PressProf.Pprofile=2;
  handles.PressProf.nppfun = 4;
end
set(handles.set_NPPFUN, 'Value',handles.PressProf.nppfun)

handles.CurrProf.nfunc = nl.nfunc;
set(handles.set_nfunc, 'Value',handles.CurrProf.nfunc)
set(handles.set_nfunrho,'Value',nl.nfunrho+1);
set(handles.set_k, 'String',num2str(nl.elong));
set(handles.set_delta, 'String',num2str(nl.delta));
set(handles.set_xi, 'String',num2str(nl.xi));
set(handles.set_signB0, 'String',num2str(nl.signb0xp));
set(handles.set_signIp, 'String',num2str(nl.signipxp));
set(handles.set_COCOS_IN, 'String',num2str(nl.cocos_in));
set(handles.set_COCOS_OUT, 'String',num2str(nl.cocos_out));
set(handles.set_tensbnd, 'String',num2str(nl.tensbnd));
set(handles.set_tensprof, 'String',num2str(nl.tensprof));

for i=1:7
  eval(['set(handles.set_at' num2str(i) ', ''String'',num2str(nl.at2(i)));']);
end
for i=1:2
  eval(['set(handles.set_ap' num2str(i) ', ''String'',num2str(nl.ap2(i)));']);
end
for i=3:7
  eval(['set(handles.set_ap' num2str(i) ', ''String'',num2str(-nl.ap2(i)));']);
end

guidata (hObject, handles); %save all the changes into handles
set_NPPFUN_Callback(hObject, eventdata, handles)
set_nfunc_Callback(hObject, eventdata, handles)


% --------------------------------------------------------------------
function Edit_ITM_options_Callback(hObject, eventdata, handles)
% hObject    handle to Edit_ITM_options (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ~isfield(handles,'datanamelist') || ~isfield(handles.datanamelist,'nitmopt')
  [customizelist_H] = customize_options('customize_list_ITM.data');
else
  % [customizelist_H_init] = customize_options('customize_list_ITM.data',0);
  customize_list_ITM_struct.keywords = handles.ITM_options.keywords;
  customize_list_ITM_struct.default_values{handles.ITM_options.initmopt} = num2str(handles.datanamelist.nitmopt);
  customize_list_ITM_struct.default_values{handles.ITM_options.initmshot_1} = num2str(handles.datanamelist.nitmshot(1));
  customize_list_ITM_struct.default_values{handles.ITM_options.initmrun_1} = num2str(handles.datanamelist.nitmrun(1));
  customize_list_ITM_struct.default_values{handles.ITM_options.initmshot_2} = num2str(handles.datanamelist.nitmshot(2));
  customize_list_ITM_struct.default_values{handles.ITM_options.initmrun_2} = num2str(handles.datanamelist.nitmrun(2));
  customize_list_ITM_struct.default_values{handles.ITM_options.itreeitm_1} = num2str(handles.datanamelist.treeitm{1});
  customize_list_ITM_struct.default_values{handles.ITM_options.itreeitm_2} = num2str(handles.datanamelist.treeitm{2});
  customize_list_ITM_struct.labels = handles.ITM_options.labels;
  [customizelist_H] = customize_options(customize_list_ITM_struct);
end

waitfor(customizelist_H.handles.ok);

handles.ITM_options=evalin('base','customizelist_H');
handles.datanamelist.nitmopt = str2num(handles.ITM_options.values{handles.ITM_options.initmopt});
handles.datanamelist.nitmshot = [str2num(handles.ITM_options.values{handles.ITM_options.initmshot_1}) ...
                    str2num(handles.ITM_options.values{handles.ITM_options.initmshot_2})];
handles.datanamelist.nitmrun = [str2num(handles.ITM_options.values{handles.ITM_options.initmrun_1}) ...
                    str2num(handles.ITM_options.values{handles.ITM_options.initmrun_2})];
handles.datanamelist.treeitm = [{handles.ITM_options.values{handles.ITM_options.itreeitm_1}} ...
                    {handles.ITM_options.values{handles.ITM_options.itreeitm_2}}];

guidata(hObject, handles);



function set_solpdpol_Callback(hObject, eventdata, handles)
% hObject    handle to set_solpdpol (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_solpdpol as text
%        str2double(get(hObject,'String')) returns contents of set_solpdpol as a double


% --- Executes during object creation, after setting all properties.
function set_solpdpol_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_solpdpol (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_nrbox_Callback(hObject, eventdata, handles)
% hObject    handle to set_nrbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_nrbox as text
%        str2double(get(hObject,'String')) returns contents of set_nrbox as a double


% --- Executes during object creation, after setting all properties.
function set_nrbox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_nrbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_nzbox_Callback(hObject, eventdata, handles)
% hObject    handle to set_nzbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_nzbox as text
%        str2double(get(hObject,'String')) returns contents of set_nzbox as a double


% --- Executes during object creation, after setting all properties.
function set_nzbox_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_nzbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --------------------------------------------------------------------
function check_eqdsk_out_Callback(hObject, eventdata, handles)
% hObject    handle to check_eqdsk_out (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function plot_all_check_eqdsk_out_Callback(hObject, eventdata, handles)
% hObject    handle to plot_all_check_eqdsk_out (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

ij=strfind(handles.fname_out,['EQDSK_COCOS_' num2str(handles.datanamelist.cocos_out,'%.2i')]);
for i=1:length(ij)
  % find valid name
  if ~isempty(ij{i})
    iok=i;
  end
end
fname_eqdskout=handles.fname_out{iok};
eqdsk=read_eqdsk(fname_eqdskout,handles.datanamelist.cocos_out,1);
plot_eqdsk(eqdsk);

handles.eqdsk_out = eqdsk;

guidata (hObject,handles);

% --------------------------------------------------------------------
function plot_gs_chec_eqdsk_out_Callback(hObject, eventdata, handles)
% hObject    handle to plot_gs_chec_eqdsk_out (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

ij=strfind(handles.fname_out,['EQDSK_COCOS_' num2str(handles.datanamelist.cocos_out,'%.2i')]);
for i=1:length(ij)
  % find valid name
  if ~isempty(ij{i})
    iok=i;
  end
end
fname_eqdskout=handles.fname_out{iok};
eqdsk=read_eqdsk(fname_eqdskout,handles.datanamelist.cocos_out,1);

% plot only contours of left- and right-hand side of Grad-Shafranov equation
plot_eqdsk(eqdsk,[],[],1);

handles.eqdsk_out = eqdsk;

guidata (hObject,handles);



function set_tensprof_Callback(hObject, eventdata, handles)
% hObject    handle to set_tensprof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_tensprof as text
%        str2double(get(hObject,'String')) returns contents of set_tensprof as a double



% --- Executes during object creation, after setting all properties.
function set_tensprof_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_tensprof (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_tension_fit_p_Callback(hObject, eventdata, handles)
% hObject    handle to set_tension_fit_p (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_tension_fit_p as text
%        str2double(get(hObject,'String')) returns contents of set_tension_fit_p as a double

x = str2num ( get ( handles.set_x_press, 'String'));
y = str2num ( get ( handles.set_y_press, 'String'));
sigma=ones(size(x));
if x(1)>0
  x(2:end+1)=x; x(1)=0.;
  y(2:end+1)=y;
  sigma(2:end+1)=sigma; sigma(1)=1e3;
end
tensfit=str2num(get(handles.set_tension_fit_p, 'String'));
handles.PressProf.y_forinput = interpos(x,y,handles.PressProf.x_forinput,tensfit,[1 0],[0 0],sigma);

plot_pressure_points_Callback(hObject, eventdata, handles)

% --- Executes during object creation, after setting all properties.
function set_tension_fit_p_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_tension_fit_p (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_tens_fit_bnd_Callback(hObject, eventdata, handles)
% hObject    handle to set_tens_fit_bnd (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_tens_fit_bnd as text
%        str2double(get(hObject,'String')) returns contents of set_tens_fit_bnd as a double


% --- Executes during object creation, after setting all properties.
function set_tens_fit_bnd_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_tens_fit_bnd (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function set_tension_fit_curr_Callback(hObject, eventdata, handles)
% hObject    handle to set_tension_fit_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of set_tension_fit_curr as text
%        str2double(get(hObject,'String')) returns contents of set_tension_fit_curr as a double

x = str2num ( get ( handles.set_x_curr, 'String'));
y = str2num ( get ( handles.set_y_curr, 'String'));
sigma=ones(size(x));
if x(1)>0
  x(2:end+1)=x; x(1)=0.;
  y(2:end+1)=y;
  sigma(2:end+1)=sigma; sigma(1)=1e3;
end
tensfit=str2num(get(handles.set_tension_fit_curr, 'String'));
handles.CurrProf.y_forinput = interpos(x,y,handles.CurrProf.x_forinput,tensfit,[1 0],[0 0],sigma);

plot_current_points_Callback(hObject, eventdata, handles);

% --- Executes during object creation, after setting all properties.
function set_tension_fit_curr_CreateFcn(hObject, eventdata, handles)
% hObject    handle to set_tension_fit_curr (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
