#!/bin/tcsh -ef

foreach file ( fc2k/*.xml )
$FC2K/bin/fc2k $file
end

