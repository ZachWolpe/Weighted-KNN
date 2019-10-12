
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
	weightz = J(nrow(xy_test), K, 0);
	do i=1 to ncol(dist);
		* ---------- Weighting Function --------- ;
		* weight = 1/d;
		col = dist[,i] || (1/dist[,i]) || xy_train[,1];
		call sort(col, {1});
		* weight predictions;
		do j=1 to K;
			if col[j,3]=1 then; weightz[i,1] = weightz[i,1] + col[j,2];
			if col[j,3]=2 then; weightz[i,2] = weightz[i,2] + col[j,2];
			if col[j,3]=3 then; weightz[i,3] = weightz[i,3] + col[j,2];
		end;
	end;
	
	* predict;
	do i=1 to nrow(weightz);
		pred = pred // max(weightz[i,<:>]);
	end;
	
	return pred;
finish K_nearest_neighbors;
* ------------------------- K Nearest Neighbors Model -------------------------;


* run on testing data;
pred = K_nearest_neighbors(xy_train, xy_test, 3);

* run on grid;
grid_pred = K_nearest_neighbors(xy_train, grid, 3);


grid_results = (grid_pred || grid[,2:3] || J(nrow(grid_pred),1,1));
res = (xy_train || J(nrow(xy_train),1,2)) // grid_results;

create res from res[colname={'yh' 'x1' 'x2' 'set'}];
append from res;
quit;

proc sgplot data=res;
	scatter y=x2 x=x1 / group=yh;
run;















* ------------------------------ Bootstrap ------------------------------;
samp = sample(1:n,n,'replace');



* ------------------------------ Bootstrap ------------------------------;









































