# Generate the samples
# TODO: There should be a single-pass way to generate all samples at once

import sample_util

#file_size = file_len('train.csv')

file_size = 45840618

sample_size = 1000
sample_util.generate_sample('train.csv', 
				file_size, 
				sample_size, 
				"train_sample_%s.csv" % sample_size,
				True)

sample_size = 10000
sample_util.generate_sample('train.csv', 
				file_size, 
				sample_size, 
				"train_sample_%s.csv" % sample_size,
				True)

sample_size = 100000
sample_util.generate_sample('train.csv', 
				file_size, 
				sample_size, 
				"train_sample_%s.csv" % sample_size,
				True)

sample_size = 1000000
sample_util.generate_sample('train.csv', 
				file_size, 
				sample_size, 
				"train_sample_%s.csv" % sample_size,
				True)				