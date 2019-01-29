% minimalist demo to run chease with matlab scripts
clear; close all;

namelist_file = 'namelist_default';
EXPEQ_file = 'EXPEQ_default';

EXPEQ_default = write_expeq;
[EXPEQdataout,fclose_out] = write_expeq(EXPEQ_default,EXPEQ_file);

[~,ha] = plot_expeq(EXPEQdataout.fnamefull); % plot input EXPEQ file

nl.nsttp = EXPEQ_default.nsttp; % set correct nsttp in namelist
nl.npropt = nl.nsttp; % so EXPEQ OUT contains same profile as EXPEQ in
nl = write_namelist_chease(namelist_file,nl);

% Set verbosity level to 3 for demo
fname_out = run_chease(namelist_file,EXPEQ_file,2,'','','',3);

% Unneeded now with new revision of plot_expeq
% plot_expeq(EXPEQdataout.fnamefull,'b'); drawnow

%%
% find EXPEQ_OUT file
for ii=1:length(fname_out);
    if ~isempty(strfind(fname_out{ii},'EXPEQ.OUT_'))
        EXPEQ_OUT_file = fname_out{ii}; break
    end
end
% find cols file
for ii=1:length(fname_out);
    if ~isempty(strfind(fname_out{ii},'o.chease.default.cols'))
        ocolsfile = fname_out{ii}; break
    end
end

% plot output EXPEQ file in same plot
plot_expeq(ha.fig,EXPEQ_OUT_file,'r');
% it is normal that j|| is different

%%
cheasedata = read_ocols(ocolsfile);


