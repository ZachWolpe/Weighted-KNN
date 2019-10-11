
* ___________________________________ Weighted KNN ___________________________________;
libname lib '/folders/myfolders/sasuser.v94/EKT 720/weighted_KNN/';

data xy;
set lib.yx;
run;


proc iml;
use xy; read all into xy;
n = nrow(xy);


* ------------------------- Train Test Split -------------------------;
start train_test_split;
	in = uniform(J(n,1,1))<train_percentage;
	xy_train = xy[loc(in=1),];
	xy_test = xy[loc(in=0),];
finish train_test_split;
* ------------------------- Train Test Split -------------------------;


train_percentage = 0.8;
call train_test_split;



* ------------------------- Weighting Function -------------------------;






start weighting_function(points,distances);
	do i=1 to nrow(points);
		
	end;
	return
finish weighting_function;




















































