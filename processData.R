# Performs further operations on the data format and saves it to an RData file for use

# Read the data
setwd('/Users/johanngan/Desktop/My Stuff/Rice/Courses/Fall 2017/STAT 410/Project/Data')
dataName = 'combined_data.csv' # Base input file name from Python script
data = read.csv(dataName, header=TRUE)

# Convert level factor into a binary form: upper (300+) and lower (200-)
data$upperlower = rep("lower", length(data$level))
data$upperlower[grep("300", data$level)] = "upper"
data$upperlower[grep("400", data$level)] = "upper"
data$upperlower[grep("500", data$level)] = "upper"
data$upperlower = factor(data$upperlower)

# Convert the individual departments into broader school categories
# Humanities, Engineering, Business, Architecture, Music, Natural Science, Social Science, 
# Other, Cross-department
data$school = rep("other", length(data$depts))

# Returns a logical vector for whether each point in data contains a category listed
# in <cats>
is_in = function(data, cats)
{
    invect = rep(FALSE, length(data))
    for(c in cats)
    {
        invect[grep(c, data)] = TRUE
    }
    
    return(invect)
}

# Convert course department codes to appropriate academic school
arch = c('ARCH')
in_arch = is_in(data$depts, arch)
data$school[in_arch] = "architecture"

busi = c('ACCO', 'BUSI', 'MANA', 'MGMT', 'MGMW', 'MICO')
in_busi = is_in(data$depts, busi)
data$school[in_busi] = "business"

engi = c('BIOE', 'CAAM', 'CENG', 'CEVE', 'CHBE', 'COMP', 'ELEC', 'ENGI', 'GLHT', 'MECH', 'MSCI', 
         'MSNE', 'STAT')
in_engi = is_in(data$depts, engi)
data$school[in_engi] = "engineering"

huma = c('AMCI', 'ARAB', 'ARCR', 'ARTS', 'ARTV', 'ASIA', 'CHIN', 'CLAS', 'COMM', 'ENGL', 
         'FILM', 'FLAC', 'FOTO', 'FREN', 'GERM', 'GREE', 'HART', 'HEBR', 'HIND', 'HIST', 
         'HUMA', 'HURC', 'ITAL', 'JAPA', 'JWST', 'KORE', 'LASR', 'LATI', 'MDEM', 'MDST', 
         'PHIL', 'PLSH', 'PORT', 'RELI', 'RUSS', 'SLAV', 'SPAN', 'SPPO', 'SWGS', 'THEA', 
         'TIBT', 'WGST')
in_huma = is_in(data$depts, huma)
data$school[in_huma] = "humanities"

musi = c('MUSI')
in_musi = is_in(data$depts, musi)
data$school[in_musi] = "music"

nsci = c('APPL', 'ASTR', 'BIOC', 'BIOS', 'CHEM', 'EBIO', 'EMSP', 'ESCI', 'HEAL', 'KINE', 'MATH', 
         'NSCI', 'PHYS', 'SSPB')
in_nsci = is_in(data$depts, nsci)
data$school[in_nsci] = "natural_science"

soci = c('ANTH', 'CSCI', 'LING', 'ECON', 'NEIR', 'NEUR', 'PLST', 'POLI', 'POST', 'PSYC', 'SANS', 
         'SMGT', 'SOCI', 'SOSC')
in_soci = is_in(data$depts, soci)
data$school[in_soci] = "social_science"

other = c('BAKE', 'BROW', 'COLL', 'CSCS', 'DSRT', 'DUNC', 'EDUC', 'ENST', 'FSEM', 'FWIS', 'GLBL', 
          'HANS', 'HONS', 'JONE', 'KECK', 'LEAD', 'LOVE', 'LPAP', 'LPCR', 'MART', 'MILI', 'MLSC', 
          'MURT', 'NAVA', 'RICH', 'UNIV', 'WIES', 'WILL')
in_other = is_in(data$depts, other)

# Account for cross-departmental courses
num_in = in_arch + in_busi + in_engi + in_huma + in_musi + in_nsci + in_soci + in_other
data$school[num_in > 1] = "cross-departmental"

data$school = factor(data$school) # string -> factor



# Convert the term into a season variable and a year variable
data$season = rep("Fall", length(data$term))
data$season[grep("Spring", data$term)] = "Spring"
data$season[grep("Summer", data$term)] = "Summer"
data$season = factor(data$season)

data$year = rep("05", length(data$term))
for(y in 6:17)
{
    yearstr = sprintf("%02d", y)
    data$year[grep(yearstr, data$term)] = yearstr
}
data$year = factor(data$year)

save(data, file="course_data.RData") # Save to RData file