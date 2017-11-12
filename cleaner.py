# Reads a table of course evaluation summaries from the registrar

import xlrd
import sys

def roundDown(n, d):
    """Rounds n down to the nearest multiple of d"""

    return n - (n % d)

class SectionInfo(object):
    """Information about a section of a course read from one row."""

    def parseTerm(self, row):
        return str(row[0]).strip()

    def parseCourseName(self, row):
        """Returns (dept, cnumber, section)"""

        courseinfo = str(row[1]).split()
        return (courseinfo[0], int(courseinfo[1]), courseinfo[2])

    def parseIName(self, row):
        try:
            return row[3].strip()
        except(AttributeError):
            return str(row[3]).strip()

    def parseSize(self, row):
        if(str(row[6]).strip() != ''):
            return int(row[6])
        else:
            return int(row[4])

    def parseXlist(self, row):
        return str(row[5]).strip()

    def normalizeScore(self, score, factor, offset):
        """Renormalize scores for different scales"""

        return (score - offset)/factor + offset

    def parseScores(self, row):
        """Returns ([course_scores, nratings_c], [instructor_scores, n_i]), and ([], []) if invalid input
        course_scores, instructor_scores, and n_i are lists"""

        # Course info
        try:
            course_info = [ [float(row[i]) for i in range(16, 21)], float(row[33]) ]
        except:
            course_info = []

        # Instructor scores
        try:
            i_info = [ [float(row[i]) for i in range(7, 16)], [float(row[i]) for i in range(21, 30)] ]
        except:
            i_info = []

        return (course_info, i_info)

    def __init__(self, row):
        self.term = self.parseTerm(row) # str
        (self.dept, self.cnumber, self.section) = self.parseCourseName(row) # (str, int, str)
        self.iname = self.parseIName(row)   # str
        self.size = self.parseSize(row) # int
        self.xlist = self.parseXlist(row)   # str
        (self.course_info, self.instructor_info) = self.parseScores(row)  # ( [[float], float], [[float], [float]] )

class SectionInfo06(SectionInfo):
    """Section info for 06"""
    def parseIName(self, row):
        try:
            return (row[2].strip(), row[3].strip())
        except(AttributeError):
            return (str(row[2]).strip(), str(row[3]).strip())

    def parseSize(self, row):
        return int(row[4])

    def parseXlist(self, row):
        return str(row[5]).strip()

    def parseScores(self, row):
        # Course info
        try:
            course_info = [ [float(row[i]) for i in range(15, 20)], float(row[32]) ]
        except:
            course_info = []

        # Instructor scores
        try:
            i_info = [ [float(row[i]) for i in range(6, 15)], [float(row[i]) for i in range(20, 29)] ]
        except:
            i_info = []

        return (course_info, i_info)

class SectionInfoFa05(SectionInfo06):
    """Section info for fa05"""
    def parseCourseName(self, row):
        courseinfo = str(row[1]).replace('.', ' ').split()
        return (courseinfo[0], int(courseinfo[1]), courseinfo[2])

    def parseScores(self, row):
        # Renormalize from the 1-7 scale to the 1-5 scale
        offset = 1
        factor = 6.0/4.0

        # Course info
        try:
            course_info = [ [self.normalizeScore(float(row[i]), factor, offset) for i in range(15, 19)] + [float(row[19])], 
                float(row[32]) ]
        except:
            course_info = []

        # Instructor scores
        try:
            i_info = [ [self.normalizeScore(float(row[i]), factor, offset) for i in range(6, 15)], 
                [float(row[i]) for i in range(20, 29)] ]
        except:
            i_info = []

        return (course_info, i_info)

class SectionInfoSp05(SectionInfo):
    """Section info for sp05"""
    def parseIName(self, row):
        try:
            return row[2].strip()
        except(AttributeError):
            return str(row[2]).strip()

    def parseSize(self, row):
        return int(row[3])

    def parseXlist(self, row):
        return str(row[4]).strip()

    def parseScores(self, row):
        # Renormalize from the 1-7 scale to the 1-5 scale
        offset = 1
        factor = 6.0/4.0

        # Course info
        try:
            course_info = [ [self.normalizeScore(float(row[i]), factor, offset) for i in range(14, 18)] + [float(row[18])], 
                float(row[31]) ]
        except:
            course_info = []

        # Instructor scores
        try:
            i_info = [ [self.normalizeScore(float(row[i]), factor, offset) for i in range(5, 14)], 
                [float(row[i]) for i in range(19, 28)] ]
        except:
            i_info = []

        return (course_info, i_info)


class Section(object):
    """Information about a section of a course, including meta-info about the course"""

    def __init__(self, info, allsections=[]):
        self.term = info.term
        self.depts = set([info.dept])
        self.cnumbers = set([info.cnumber])
        self.zip_dept_cnum = set([(info.dept, info.cnumber)])
        self.section = info.section
        self.size = info.size
        self.xlist = info.xlist

        if(not(allsections)):
            self.allsections = set([info.section])
        else:
            self.allsections = set(allsections)
        self.instructors = set([info.iname])

        self.course_info = info.course_info
        self.instructor_info = info.instructor_info

    def __contains__(self, info):
        """Checks for whether an info is contained in a section"""

        # Same dept
        if((info.dept, info.cnumber) in self.zip_dept_cnum and info.section == self.section):
            return True

        return False

    def isRepeat(self, info):
        """Checks whether info is a crosslist repeat"""

        if(info.xlist != '' and info.xlist == self.xlist and info.iname in self.instructors):
            return True

        return False

    @staticmethod
    def isInvalid(info):
        """Checks whether a row of data should be considered invalid"""

        if(info.size == 0 or not(info.course_info) or not(info.instructor_info)):
            return True

        # Check the Ns
        if(info.course_info[1] == 0):
            return True

        for i in info.instructor_info[1]:
            if(i == 0):
                return True

        # Check the Xs
        for i in info.course_info[0] + info.instructor_info[0]:
            if(i < 1 or i > 5):
                return True


        return False

    def sameDept(self, info):
        """Checks whether info is in the same department"""

        if(info.dept in self.depts):
            return True

        return False

    def sameSection(self, info):
        """Checks whether the section number of info is the same"""

        if(info.section == self.section):
            return True

        return False

    def sameNumber(self, info):
        """Checks whether the course number of info is the same"""

        if(info.cnumber in self.cnumbers):
            return True

        return False

    def sameCourse(self, info):
        """Checks whether info is the same course"""

        # Same dept
        if(self.sameDept(info) and info.cnumber in self.cnumbers):
            return True

        # crosslist diff dept
        if(info.xlist != '' and info.xlist == self.xlist and not(self.sameDept(info))):
            return True

        # crosslist diff number
        if(info.xlist != '' and info.xlist == self.xlist and self.sameDept(info) and not(self.sameNumber(info))):
            return True

        return False


    def addInfo(self, info):
        """Update the instance with the newest info, if needed"""

        if(self.isInvalid(info)):
            return

        if(not(self.sameCourse(info))):
            return

        if(self.isRepeat(info)):
            self.zip_dept_cnum.add((info.dept, info.cnumber))
            self.cnumbers.add(info.cnumber)
            self.depts.add(info.dept)
        else:
            if(self.sameSection(info)):
                self.instructors.add(info.iname)

                # Update average instructor scores and instructor score numbers
                for i in range(len(self.instructor_info[0])):
                    selfn_i = self.instructor_info[1][i]
                    n_i = info.instructor_info[1][i]
                    self.instructor_info[0][i] = \
                        (self.instructor_info[0][i]*selfn_i + info.instructor_info[0][i]*n_i) / (selfn_i + n_i)

                    self.instructor_info[1][i] += n_i

            else:
                self.allsections.add(info.section)

    def nsections(self):
        return len(self.allsections)

    def ninstructors(self):
        return len(self.instructors)

    def levels(self):
        """Output a list of course levels"""

        levs = []

        for c in list(sorted(self.cnumbers)):
            l = roundDown(c, 100)
            if(not(l in levs)):
                levs += [l]

        return levs

    def departments(self):
        return list(sorted(self.depts))


class CourseList(object):
    """Collection of Section objects"""

    def __init__(self):
        self.sections = []

    def __contains__(self, info):
        for s in self.sections:
            if(info in s):
                return True

        return False

    def addSection(self, info):
        """Add a section to the course list, if needed"""

        allsects = []   # For retaining the section count for new sections of the same course
        for s in self.sections:
            s.addInfo(info)
            if(not(allsects) and s.sameCourse(info)):
                allsects = s.allsections

        if(not(info in self) and not(Section.isInvalid(info))):

            self.sections += [Section(info, allsects)]


class Cleaner(object):
    """Object to do the actual cleaning"""

    def __init__(self):
        # For reading
        self.startRow = 27
        self.courses = CourseList()

        # For output
        self.colNames = ['term', 'depts', 'level', 'size', 'ninstructors', 'nsections'] + \
            ['X' + str(i) + '_avg' for i in range(1, 10)] + ['X' + str(i) for i in range(10, 15)] + ['N8', 'N13']

    def reset(self):
        self.courses = CourseList()

    def genInfo(self, row):
        """Generates a SectionInfo object from row"""
        return SectionInfo(row)


    def clean(self, infile):
        """Input is a string with the file name, out_table is a table with section data. 
        Inner index is row index, outer index is column index"""
        self.reset()

        book = xlrd.open_workbook(infile)
        sh = book.sheet_by_index(0)

        for i in range(self.startRow, sh.nrows):
            try:
                self.courses.addSection( self.genInfo( sh.row_values(i) ) )
            except:
                print('Bad row {}: {}'.format(i, sh.row_values(i)))
                raise


    def writeTable(self, outfile, append=False, delim=','):
        """Writes a csv file with a given table"""

        mode = 'a' if append else 'w'
        with open(outfile, mode) as out:

            if(not(append)):
                for col in self.colNames[:-1]:
                    out.write(col + delim)
                out.write(self.colNames[-1] + '\n')

            for s in self.courses.sections:
                out.write(s.term + delim)

                for d in s.departments()[:-1]:
                    out.write(d + '/')
                out.write(s.departments()[-1] + delim)

                levs = s.levels()
                for l in levs[:-1]:
                    out.write(str(l) + '/')
                out.write(str(levs[-1]) + delim)

                out.write(str(s.size) + delim)
                out.write(str(s.ninstructors()) + delim)
                out.write(str(s.nsections()) + delim)

                for i_score in s.instructor_info[0]:
                    out.write(str(i_score) + delim)

                for c_score in s.course_info[0]:
                    out.write(str(c_score) + delim)

                out.write(str(s.instructor_info[1][7]) + delim)
                out.write(str(s.course_info[1]) + '\n')

            print('{} to file: {}'.format('Appended' if append else 'Wrote', outfile))

    def cleanWrite(self, infile, outfile, append=False, delim=','):
        """Cleans data and writes to a file"""
        self.clean(infile)
        self.writeTable(outfile, append, delim)



class CleanerFa05(Cleaner):
    def __init__(self):
        super(CleanerFa05, self).__init__()

        # For reading
        self.startRow = 28

    def genInfo(self, row):
        return SectionInfoFa05(row)

class CleanerSp05(CleanerFa05):
    def genInfo(self, row):
        return SectionInfoSp05(row)

class Cleaner06(Cleaner):
    def genInfo(self, row):
        return SectionInfo06(row)


if(__name__ == '__main__'):
    if(len(sys.argv) < 3):
        print("Error: Input at least two arguments, an input and output file name.")
        sys.exit(1)

    # Default 0 for modern format. Set to 1 for old format.
    mode = 'modern'

    if(len(sys.argv) >= 4):
        mode = sys.argv[3]

    if(mode == 'fa05'):
        reader = CleanerFa05()
    elif(mode == 'sp05'):
        reader = CleanerSp05()
    elif(mode == '06' or mode == 'fa06' or mode == 'sp06'):
        reader = Cleaner06()
    else:
        reader = Cleaner()

    reader.cleanWrite(sys.argv[1], sys.argv[2])