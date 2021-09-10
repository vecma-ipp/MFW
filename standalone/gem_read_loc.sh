
DIR='/marconi_scratch/userexternal/yyudin00/MFW_runs/cpo17/'
mkdir $DIR

mv gem-loop*.* $DIR
mv imp4dv_coretransp_0*.cpo $DIR
mv gem_coretransp_0*.cpo $DIR
mv fout_0* $DIR
mv stopped $DIR
mv *.dat $DIR
cp $DIR/t00.dat ./

cd  ../../uq/basicda/

