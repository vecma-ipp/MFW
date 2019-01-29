filename='chease_schema_choices.xsd';

fid=fopen(filename,'r');
clear ab
abc=1;
i=0; j=0;
while double(abc)>=0
  abc=fgets(fid);
  if double(abc)>=0
    i = i+1;
    ab{i} = abc;
    bb=strfind(ab{i},'<xs:element name=');
    if ~isempty(bb)
      bb=strfind(ab{i},'"');
      if length(bb)>=2
        j=j+1;
        keynames{j} = ab{i}(bb(1)+1:bb(2)-1);
        key_index(j) = bb(1)+1;
      end
    end
  end
end
fclose(fid);
length(ab)
length(keynames)

% extract tree structure
clear levelname islast
dlevels=unique(key_index);
nb_levels=length(dlevels)+1;
iprev(1:nb_levels)=0;
keyname_level=cell(nb_levels,1);
iprev(1) = key_index(1);
prev_level=1;
levelname{1,1}=keynames{1};
keyname_level{1}=keynames{1};
islast=zeros(length(keynames),1);
for j=2:length(keynames)
  %for j=2:20
  if key_index(j) > max(iprev);
    prev_level = prev_level + 1;
    if prev_level>nb_levels
      error(['prev_level=' num2str(prev_level) '>nb_levels= ' num2str(nb_levels)]);
    end
    iprev(prev_level)=key_index(j);
    levelname{prev_level,j}=keynames{j};
    for ij=1:prev_level-1
      levelname{ij,j}=keyname_level{ij};
    end
    keyname_level{prev_level}=keynames{j};
  elseif key_index(j) == max(iprev);
    % means that previous j was a leaf
    islast(j-1)=length(find(iprev>0));
    for ij=1:prev_level-1
      levelname{ij,j}=keyname_level{ij};
    end
    levelname{prev_level,j}=keynames{j};
    keyname_level{prev_level}=keynames{j};
  elseif key_index(j) < max(iprev);
    % means that previous j was a leaf
    islast(j-1)=length(find(iprev>0));
    dij = find(dlevels==key_index(j));
%    if strcmp(keynames{j},'second_source'); keyboard; end
    for kk=dij+1:prev_level
      keyname_level{kk}=[];
      iprev(kk)=0;
    end
    prev_level = dij;
    iprev(prev_level)=key_index(j);
    levelname{prev_level,j}=keynames{j};
    for ij=1:prev_level-1
      levelname{ij,j}=keyname_level{ij};
    end
    keyname_level{prev_level}=keynames{j};
  else
    disp('should not get there')
  end
end

ijk=find(islast>0);
for ii=1:length(ijk)
  j=ijk(ii);
  nb_levels = islast(j);
  var_names_ok{ii} = levelname{nb_levels,j};
  index_var_names_ok(ii) = j;
end

[var_names_ok_sorted,isorted]=sort(var_names_ok);
fid=fopen('testaaa_grouped','w');
for ii=1:length(isorted)
  j = index_var_names_ok(isorted(ii));
  nb_levels = islast(j);
  fprintf(fid,'%s  ',levelname{nb_levels,j});
  for ij=2:nb_levels-1
    fprintf(fid,'%s/',levelname{ij,j});
  end
  fprintf(fid,'%s\n',levelname{nb_levels,j});
end
fclose(fid);

[varname,var_treename]=textread('testaaa_grouped','%s%s\n');
[var_name_unique,ivar,iunique]=unique(varname);

fid=fopen('for_assign_chease_choices.f90','w');
for ii=1:length(ivar)
  igroup=strmatch(var_name_unique{ii},varname,'exact');
  nb_strpaths = length(igroup) + 1; % add single name
  for jj=1:length(igroup)
    j = igroup(jj);
    fprintf(fid,'    strpaths(%i) = ''%s''\n',jj,var_treename{j});
  end
  fprintf(fid,'    strpaths(%i) = ''%s''\n',nb_strpaths,varname{j});
  fprintf(fid,'    nb_strpaths = %i\n',nb_strpaths);
  fprintf(fid,'    do i=1,nb_strpaths\n');
  if strcmp(var_name_unique{ii},'comments') || strcmp(var_name_unique{ii},'treeitm')
    fprintf(fid,'      call xml2eg_get(doc , strpaths(i) , %s(1), error_flag)\n',varname{j});
  else
    fprintf(fid,'      call xml2eg_get(doc , strpaths(i) , %s, error_flag)\n',varname{j});
  end
  fprintf(fid,'      if (.not. error_flag) then\n');
  fprintf(fid,'        exit\n');
  fprintf(fid,'      endif\n');
  fprintf(fid,'    end do\n');
  if strcmp(var_name_unique{ii},'treeitm')
    fprintf(fid,'    treeitm(2)=treeitm(1)\n');
  end
  fprintf(fid,'    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),''%s = '',%s\n',varname{j},varname{j});
  fprintf(fid,'    \n');
end
fclose(fid);

