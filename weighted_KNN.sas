
* ___________________________________ Weighted KNN ___________________________________;
libname lib '/folders/myfolders/sasuser.v94/EKT 720/weighted_KNN/';

data xy;
set lib.yx;
run;
proc print data=xy (obs=4);




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



* ------------------------- K Nearest Neighbors Model -------------------------;
start K_nearest_neighbors(xy_train, xy_test, K);
	
	* compute distance matrix;
	dist = J(nrow(xy_train), nrow(xy_test), 99);
	do i=1 to nrow(xy_train);
		do j=1 to nrow(xy_test);
			dist[i,j] = sqrt((xy_train[i,2]-xy_test[j,2])**2 + (xy_train[i,3]-xy_test[j,3])**2);
		end;
	end;
	
	* sort data & collect K nearest;
	do i=1 to ncol(dist);
		col = dist[,i] || xy_train[,1];
		call sort(col, {1});
		pred = pred // col[1:k,2]`;
	end;

	print (nrow(pred)) (nrow(xy_test)) (nrow(xy_train));	
	print pred;

	return k;
finish K_nearest_neighbors;
* ------------------------- K Nearest Neighbors Model -------------------------;

t = K_nearest_neighbors(xy_train, xy_test, 3);







* --------------------- Create Visualization Grid ---------------------;
x1_max = max(xy_train[,2]) + 10;
x1_min = min(xy_train[,2]) - 10;
x2_max = max(xy_train[,3]) + 10;
x2_min = min(xy_train[,3]) - 10;

do i=x1_min to x1_max by 5;
	do j=x2_min to x2_max by 5;
		grid = grid // (888||i||j);
	end;
end;
* --------------------- Create Visualization Grid ---------------------;








* ------------------------- Weighting Function -------------------------;
start weighting_function(points,distances);
	weights = 1/distance;
	return weights;
finish weighting_function;
* ------------------------- Weighting Function -------------------------;






* ------------------------------ Bootstrap ------------------------------;
samp = sample(1:n,n,'replace');



* ------------------------------ Bootstrap ------------------------------;









































