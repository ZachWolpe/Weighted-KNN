
* ___________________________________ Weighted KNN ___________________________________;

libname lib '/folders/myfolders/sasuser.v94/EKT 720/weighted_KNN/';

data xy;
set lib.yx;
run;

proc iml;
use xy; read all into xy;
n = nrow(xy); y=xy[,1]; x1=xy[,2]; x2=[,3];





















































