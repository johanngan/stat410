# Run cleaning on all the data, and place everything in one combined file

import cleaner

# Parameters
outfile = 'combined_data.csv'

input_suffix = '_course_evaluation.xls'
readers = [cleaner.CleanerFa05(), cleaner.CleanerSp05(), cleaner.Cleaner06(), cleaner.Cleaner()]

fa05_names = ['fa05']
sp05_names = ['sp05']

y06_names = [t + '06' for t in ['fa', 'sp']]

modern_names = ['fa' + format(i, '02') for i in range(7, 17)] + \
    ['sp' + format(i, '02') for i in range(7, 17)] + \
    ['su' + format(i, '02') for i in range(14, 18)]


names = [fa05_names, sp05_names, y06_names, modern_names]


# Clean all the data and output the file
first_year = names[0][0]
names[0] = names[0][1:]

print('')
print('Cleaning file: {}'.format(first_year + input_suffix))
readers[0].cleanWrite(first_year + input_suffix, outfile)
print('')
    
for yearname, r in zip(names, readers):
    for y in yearname:
        print('Cleaning file: {}'.format(y + input_suffix))
        r.cleanWrite(y + input_suffix, outfile, True)
        print('')