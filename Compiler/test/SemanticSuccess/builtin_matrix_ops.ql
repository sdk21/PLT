def compute(): comp trial {
      
	int num_rows;
	int num_cols;
	comp val;
	mat m;

	m = [(1,2,3)(4,5,6)(7,8,9)];
	num_rows = rows(m);
	num_cols = cols(m);
	val = elem(m, 1,2);

	print(num_rows);
	print(num_cols);

	trial = val;
}
