#########################
# Author: Christiann Hernandez(chrish47@uw.edu)
# Data: December, 2022
# Purpose: Converting .XLSX, .XLS, .TXT, .DTA, .SAV, and .H5 to CSV files
#########################
import os, pandas
import pyreadstat
import h5py
########################################################################################################
# Please make sure all your files are in the same destination or working directory. If you need help putting all your desired files into one single location, please see file_to_directory.py
# Your files can have and capitalized(upper-case) or non-capitalized(lower-case) file extension.
########################################################################################################

def xlsx_to_csv(pathdir, outputdir):
    os.chdir(pathdir)
    good = []
    bad = []
    files = os.listdir(pathdir)
    for eachfile in files: #XLSX Files
        try:
            if (eachfile.endswith((".xlsx")) or eachfile.endswith((".XLSX"))):
                cleanfilename = eachfile.replace(".XLSX","")
                excel_file = pandas.ExcelFile(eachfile)
                sheets = excel_file.sheet_names
                for sheet_unique in sheets:
                    sheet_data = excel_file.parse(sheet_unique)
                    csvname = outputdir + cleanfilename + "-" + sheet_unique + ".csv"
                    sheet_data.to_csv(csvname, index = False)
                    print('CONVERSION COMPLETED: ', eachfile, 'TO CSV')
            else:
                print('FILE:', eachfile,', IS NOT .XLSX')
        except:
            print(f"FOUND ISSUE WITH {eachfile}")
            bad.append(eachfile)
            
            
def xls_to_csv(pathdir,outputdir):
    os.chdir(pathdir)
    good = []
    bad = []
    files = os.listdir(pathdir)
    for eachfile in files: #XLS Files
        try:
            if (eachfile.endswith((".xls")) or eachfile.endswith((".XLS"))):
                cleanfilename = eachfile.replace(".XLS","")
                excel_file = pandas.ExcelFile(eachfile)
                sheets = excel_file.sheet_names
                for sheet_unique in sheets:
                    sheet_data = excel_file.parse(sheet_unique)
                    csvname = outputdir + cleanfilename + "-" + sheet_unique + ".csv"
                    sheet_data.to_csv(csvname, index = False)
                    print('CONVERSION COMPLETED: ', eachfile, 'TO CSV')
            else:
                print('FILE:', eachfile,', IS NOT .XLS')
        except:
            print(f"FOUND ISSUE WITH {eachfile}")
            bad.append(eachfile)
            
                
def sav_to_csv(pathdir,outputdir):
    os.chdir(pathdir)
    good = []
    bad = []
    files = os.listdir(pathdir)
    for eachfile in files: #SAV Files
        try:
            if (eachfile.endswith((".sav")) or eachfile.endswith((".SAV"))):
                cleanfilename = eachfile.replace(".SAV","")
                sav_file = pandas.read_spss(eachfile)
                csvname = outputdir + cleanfilename + ".csv"
                sav_file.to_csv(csvname, index = False)
                print('CONVERSION COMPLETED: ', eachfile, 'to CSV')
            else:
                print('FILE:', eachfile,', IS NOT .SAV')
        except:
            print(f"FOUND ISSUE WITH {eachfile}")
            bad.append(eachfile)        
        
def dta_to_csv(pathdir,outputdir):
    os.chdir(pathdir)
    good = []
    bad = []
    files = os.listdir(pathdir)
    for eachfile in files: #DTA Files
        try:
            if (eachfile.endswith((".dta")) or eachfile.endswith((".DTA"))):
                cleanfilename = eachfile.replace(".DTA","")
                dta_file = pandas.read_stata(eachfile)
                csvname = outputdir + cleanfilename + ".csv"
                dta_file.to_csv(csvname, index = False)
                print('CONVERSION COMPLETED: ', eachfile, 'to CSV')
            else:
                print('FILE:', eachfile,', IS NOT .DTA')
        except:
            print(f"FOUND ISSUE WITH {eachfile}")
            bad.append(eachfile)   
                
def txt_to_csv(pathdir,outputdir):
    os.chdir(pathdir)
    good = []
    bad = []
    files = os.listdir(pathdir)
    for eachfile in files: #TXT Files
        try:
            if (eachfile.endswith((".txt")) or eachfile.endswith((".TXT"))):
                cleanfilename = eachfile.replace(".TXT","")
                text_file = pandas.read_csv(eachfile, delim_whitespace=True)
                csvname = outputdir + cleanfilename + ".csv"
                text_file.to_csv(csvname, index = False)
                print('CONVERSION COMPLETED: ', eachfile, 'to CSV')
            else:
                print('FILE:', eachfile,', IS NOT .TXT')
        except:
            print(f"FOUND ISSUE WITH {eachfile}")
            bad.append(eachfile)     


def h5_to_csv(pathdir,outputdir):
    os.chdir(pathdir)
    good = []
    bad = []
    files = os.listdir(pathdir)
    for eachfile in files: #hdf Files
        try:
            if (eachfile.endswith((".h5")) or eachfile.endswith((".H5"))):
                cleanfilename = eachfile.replace(".h5","")
                hdf_file = pandas.read_hdf(eachfile)
                csvname = outputdir + cleanfilename + ".csv"
                hdf_file.to_csv(csvname, index = False)
                print('CONVERSION COMPLETED: ', eachfile, 'to CSV')
            else:
                print('FILE:', eachfile,', IS NOT .SAV')
        except:
            print(f"FOUND ISSUE WITH {eachfile}")
            bad.append(eachfile)    
            
########################################################################################################
# Example Call
#pathdir = "/ihme/homes/chrish47/Mortality_Screening_Landscape/test/"
#outputdir = "/ihme/homes/chrish47/Mortality_Screening_Landscape/test/output/"            
#xlsx_to_csv(pathdir,outputdir)
#print('DONE')