from __future__ import division
import pandas as pd
from get_draws.api import get_draws
from db_tools import ezfuncs as ez
import numpy as np
import sys
import os
from db_queries import (get_location_metadata, 
                        get_demographics,
                        get_covariate_estimates,
                        get_population)
import xlsxwriter as xl
import gbd.constants as gbd

location_names = [
    'Latin America and Caribbean',
    'Southeast Asia, East Asia, and Oceania',
    'Central Europe, Eastern Europe, and Central Asia',
    'North Africa and Middle East',
    'Sub-Saharan Africa',
    'High-income',
    'South Asia'
]

##############################################################################
# Please read the readme.txt in this repo. It will explain the whole strategy
#############################################################################
'''
epi_demographics = get_demographics("epi", release_id=release_id)
most_detailed_ages = epi_demographics['age_group_id']
most_detailed_locs = epi_demographics['location_id']

# Convert list to DataFrame
list_df = pd.DataFrame(most_detailed_locs, columns=['location_id'])

location = get_location_metadata(location_set_id = 35, release_id = 16)
filtered_location = location[location['super_region_name'] == 'South Asia'] #Change Super Region here#
#'Latin America and Caribbean'
#'Southeast Asia, East Asia, and Oceania'
#'Central Europe, Eastern Europe, and Central Asia'
#'North Africa and Middle East'
#'Sub-Saharan Africa'
#'High-income'
#'South Asia'

filtered_location = filtered_location[['location_id']]

# Merge DataFrames on 'location_id'
merged_df = pd.merge(list_df, filtered_location, on='location_id', how='right')

# Back to a list again
most_detailed_locs = merged_df['location_id'].tolist()
'''

class Base(object):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id, location_name):
        '''This class incorporates all the functions that all the specific
        causes use, but all in different sequence'''
        self.cluster_dir = cluster_dir
        self.year_id = year_id
        self.input_me = input_me
        self.output_me = output_me
        self.conn_def = "cod"
        self.gbd_round = gbd.GBD_ROUND_ID
        self.location_name = location_name
        #self.decomp_step = decomp_step
        self.release_id = release_id #16
        
        ###########################
        epi_demographics = get_demographics("epi", release_id=self.release_id)
        most_detailed_ages = epi_demographics['age_group_id']
        most_detailed_locs = epi_demographics['location_id']

        # Convert list to DataFrame
        list_df = pd.DataFrame(most_detailed_locs, columns=['location_id'])

        location = get_location_metadata(location_set_id = 35, release_id = 16)
        filtered_location = location[location['super_region_name'] == self.location_name] 

        #Change Super Region here#
        #'Latin America and Caribbean'
        #'Southeast Asia, East Asia, and Oceania'
        #'Central Europe, Eastern Europe, and Central Asia'
        #'North Africa and Middle East'
        #'Sub-Saharan Africa'
        #'High-income'
        #'South Asia'

        filtered_location = filtered_location[['location_id']]

        # Merge DataFrames on 'location_id'
        merged_df = pd.merge(list_df, filtered_location, on='location_id', how='left')

        # Back to a list again
        #most_detailed_locs = merged_df['location_id'].tolist()
        ###########################

        epi_demographics = get_demographics("epi", release_id=self.release_id)
        self.most_detailed_ages = epi_demographics['age_group_id']
        #self.most_detailed_locs = epi_demographics['location_id']
        self.most_detailed_locs = merged_df['location_id'].tolist()

        '''get_demographics should return the most detailed demographics for 
        the given round 
        set1 = set(get_demographics("epi", gbd_round_id=5)['location_id'])
        set2 = get_location_metadata(location_set_id=35, gbd_round_id=5)
        set2 = set2.loc[set2.most_detailed==1,'location_id'].tolist()
        set2 = set(set2)
        assert set1==set2'''

    def get_country_id(self, row):
        try:
            country_id = int(row.path_to_top_parent.split(',')[3])
        except IndexError:
            country_id = None
            
        return country_id

    def get_asfr(self):
        '''Pulls the age-specific fertility rate, which is used in live birth
        calculation'''
        asfr_id = 13
        asfr = get_covariate_estimates(covariate_id=asfr_id, 
                                        location_id=self.most_detailed_locs, 
                                        sex_id=2, 
                                        age_group_id=self.most_detailed_ages,
                                        year_id=self.year_id, 
                                        status='best',
                                        release_id = self.release_id)
        asfr.rename(columns={'mean_value':'asfr'}, inplace=True)
        keeps = ['location_id','year_id','age_group_id','sex_id','asfr']
        asfr = asfr[keeps]
        '''maternal age range is 10 to 54 years old. This corresponds to 
        age_group_ids 7 to 15 (inclusive). Set all age groups outside of 
        the maternal age range to zero '''
        asfr.loc[~asfr.age_group_id.isin(list(range(7,16))), 'asfr'] = 0.
        return asfr
              
    def div_draws(self, draw_df, other_df):
        '''Divides two sets of draws'''
        keep_cols, index_cols, draw_cols = self.get_cols()
        new_draws = draw_df.copy(deep=True)
        new_draws[draw_cols] = new_draws[draw_cols].div(other_df, axis=1)
        return new_draws                                
        

    def pull_draws(self, measure_id=6, age_group_list=None):
        '''Uses get_draws to pull draws of the ME for this class instance'''
        if age_group_list is None:
            age_group_list = [7,8,9,10,11,12,13,14,15]

        draws = get_draws(gbd_id_type='modelable_entity_id',
                            gbd_id=self.input_me,
                            source='epi',
                            measure_id=measure_id,
                            location_id=self.most_detailed_locs,
                            year_id=self.year_id,
                            age_group_id=self.most_detailed_ages,
                            sex_id=2,
                            release_id = self.release_id
                            )
        ''' most maternal models are age restricted but 
        the output from this script may fail the save_results_epi tests 
        if we don't include most_detailed ages in the uploaded data.
        set all ages not in the age_group_list list to zero '''
        keep_cols, index_cols, draw_cols = self.get_cols()
        draws.loc[~draws.age_group_id.isin(age_group_list), draw_cols] = 0.
        return draws

    def get_cols(self):
        '''Returns the important columns, for easy subsetting'''
        draw_cols = ['draw_{}'.format(i) for i in range(1000)]
        index_cols = ['location_id', 'year_id', 'age_group_id', 'sex_id']
        keep_cols = list(draw_cols)
        keep_cols.extend(index_cols)
        return keep_cols, index_cols, draw_cols

    def get_new_incidence(self, draw_df, asfr_df):
        '''Dismod models were run with live births as the denominator.
        This function reverts that, by multiplying by ASFR.'''
        keep_cols, index_cols, draw_cols = self.get_cols()
        # make sure dataframes match in terms of indexes
        new_draws = draw_df.copy(deep=True)
        new_draws = new_draws[keep_cols]
        asfr_cols = list(index_cols)
        asfr_cols.append('asfr')
        new_asfr = asfr_df.copy(deep=True)
        new_asfr = new_asfr[asfr_cols]
        # multiply incidence by asfr to get new incidence
        new_incidence = new_draws.merge(new_asfr, on=index_cols, how='left', 
            indicator=True)
        assert (new_incidence._merge=='both').all()
        new_incidence.drop('_merge',axis=1, inplace=True)
        for col in draw_cols:
            new_incidence[col] = (new_incidence[col] * new_incidence['asfr'])
        new_incidence.drop('asfr', axis=1, inplace=True)
        return new_incidence

    def mul_draws(self, draw_df, other_df):
        '''Multiplies two sets of draws'''
        keep_cols, index_cols, draw_cols = self.get_cols()
        new_draws = draw_df.copy(deep=True)
        new_draws[draw_cols] = new_draws[draw_cols].mul(other_df, axis=1)
        return new_draws

    def create_draws(self, mean, lower, upper):
        '''For the purpose of severity splits or duration'''
        seed = 100
        np.random.seed(seed)
        sd = (upper - lower) / (2 * 1.96)
        sample_size = mean * (1 - mean) / sd ** 2
        alpha = mean * sample_size
        beta = (1 - mean) * sample_size
        draws = np.random.beta(alpha, beta, size=1000)
        #Using the beta distribution
        # The Beta distribution is parameterized by two shape parameters, alpha and beta, which are derived from the mean and standard deviation.
        return draws
       
##########################################################
## Developed for PPCM, 2024
    def create_normal_draws(self, mean, lower, upper):
        '''Generates draws from a normal distribution for the purpose of severity splits or duration'''
        seed = 100
        np.random.seed(seed)
        # Assuming lower and upper bounds represent a 95% confidence interval, 
        # which spans 1.96 standard deviations from the mean in both directions
        sd = (upper - lower) / (2 * 1.96)
        # Generate draws from a normal distribution
        draws = np.random.normal(loc=mean, scale=sd, size=1000)
        #Using the normal distribution
        return draws
        '''The mean is the average value for the normal distribution.'''
        '''The lower and upper values are used to calculate the standard deviation (sd). It's assumed that lower and upper
        are the bounds of a 95% confidence interval for the normal distribution, which spans 1.96 standard deviations from
        the mean on each side.'''
        '''The np.random.normal function is used to generate 1000 random draws from a normal distribution with the specified
        mean (loc=mean) and standard deviation (scale=sd).'''
        
### Unlike the beta distribution which is bound between [0, 1], the normal distribution is not bounded and can theoretically take any real number.

    def create_log_normal_draws(self, mean, lower, upper):
        '''Generates draws from a log-normal distribution for the purpose of severity splits or duration'''
        seed = 100
        np.random.seed(seed)
    
        # Transform mean, lower, and upper bounds to the log scale
        log_lower = np.log(lower)
        log_upper = np.log(upper)
        log_mean = np.log(mean)

        # Assuming lower and upper bounds represent a 95% confidence interval, 
        # which spans 1.96 standard deviations from the mean in both directions
        # in the log scale
        log_sd = (log_upper - log_lower) / (2 * 1.96)

        # Generate draws from a log-normal distribution
        log_draws = np.random.normal(log_mean, log_sd, size=1000)
        
        #A log-normal distribution is a probability distribution of a random variable whose logarithm is normally distributed. This distribution is useful when modeling data that is positively skewed, meaning it cannot be negative and is multiplicative.

        # Transform back to the original scale
        draws = np.exp(log_draws)

        return draws


    def zero_locs(self, df):
        '''Given a dataframe obtained from get_draws zeros out all the draws 
        for the appropriate locations, and returns the zero'd out dataframe
        '''
        # keep SSA, SA, Afghanistan, Yemen, and Sudan 
        keep_df = pd.read_stata(("/mnt/team/rgud/pub/users/chrish47/"
            "fistula_locations_2019.dta"))
        keep_locs = keep_df.loc[keep_df.most_detailed==1,'location_id'].tolist()
        zero_df = df.copy(deep=True)
        keep_cols, index_cols, draw_cols = self.get_cols()
        for col in draw_cols:
            zero_df.loc[~zero_df.location_id.isin(keep_locs), col] = 0.
        return zero_df

    def squeeze_severity_splits(self, sev_df1, sev_df2, total=1):
        '''Given two severity dataframes and a total dataframe, all with
        the same index, squeezes so that the sum of the severity dataframes
        match the total.
        '''
        if type(sev_df1) is np.ndarray and type(sev_df2) is np.ndarray:
            ''' The numpy arrays passed through this function will not have
            zeros so there is no need to fill after division '''
            assert np.isnan(sev_df1).any() == False
            assert np.isnan(sev_df2).any() == False
            squeeze_frame = (total / (sev_df1 + sev_df2))
        else:
            ''' The inclusion of most detailed age groups with some set to zero
            creates NaNs when we divide by zero. Assert there are no NaNs in the 
            data set before division then set all the NaNs to zero after the 
            division. '''
            assert sev_df1.isnull().values.any() == False
            assert sev_df2.isnull().values.any() == False
            squeeze_frame = (total / (sev_df1 + sev_df2))
            squeeze_frame.fillna(0, inplace=True)
        
        sq_sev_df1 = sev_df1 * squeeze_frame
        sq_sev_df2 = sev_df2 * squeeze_frame
        return sq_sev_df1, sq_sev_df2

### PPCM, 4 severity splits
    def squeeze_severity_splits_PPCM(self, sev_df1, sev_df2, sev_df3, sev_df4, total=1):
        '''Given four severity dataframes and a total dataframe, all with
        the same index, squeezes so that the sum of the severity dataframes
        match the total.'''
        if all(isinstance(df, np.ndarray) for df in [sev_df1, sev_df2, sev_df3, sev_df4]):
            ''' The numpy arrays passed through this function will not have
            zeros so there is no need to fill after division '''
            assert all(not np.isnan(df).any() for df in [sev_df1, sev_df2, sev_df3, sev_df4])
            squeeze_frame = total / (sev_df1 + sev_df2 + sev_df3 + sev_df4)
        else:
            ''' The inclusion of most detailed age groups with some set to zero
            creates NaNs when we divide by zero. Assert there are no NaNs in the 
            data set before division then set all the NaNs to zero after the 
            division. '''
            assert all(not df.isnull().values.any() for df in [sev_df1, sev_df2, sev_df3, sev_df4])
            squeeze_frame = total / (sev_df1 + sev_df2 + sev_df3 + sev_df4)
            squeeze_frame.fillna(0, inplace=True)
    
        sq_sev_df1 = sev_df1 * squeeze_frame
        sq_sev_df2 = sev_df2 * squeeze_frame
        sq_sev_df3 = sev_df3 * squeeze_frame
        sq_sev_df4 = sev_df4 * squeeze_frame
        return sq_sev_df1, sq_sev_df2, sq_sev_df3, sq_sev_df4
##########################################################

    def export_negatives(self, df):
        """ Isolates negative data and exports the data for analysis. """
        
        # Find negative data and export
        negative_bool = (df < 0)
        neg_check = df.copy()
        neg_check = neg_check[negative_bool]
        if not neg_check.empty:
            # Create excel file
            negative_file = os.path.join(self.cluster_dir,
                "negatives_year{}.xlsx".format(self.year_id))
            workbook = xl.Workbook(negative_file)
            worksheet = workbook.add_worksheet()
            # Calculate dimensions of datataframe
            neg_check = neg_check.reset_index()
            neg_check.rename(columns={0:'negative_mean'},inplace=True)
            num_row, num_col = neg_check.shape
            # Write the data to a sequence of cells.
            worksheet.write_row(0,0,neg_check.columns)
            rowshift, colshift = 1, 1
            for i in range(num_row):
                worksheet.write_row(i+rowshift,0,neg_check.iloc[i])
            format_dict = {'text_wrap': True}
            desc_format = workbook.add_format(format_dict)
            desc_text = ("**Locations that appear in this file have a mean" 
                " incidence of severe preclampsia that is higher than" 
                " the mean incidence of total hypertensive disorders.")
            worksheet.merge_range(num_row+rowshift, 0, num_row+rowshift, 
                num_col-colshift, desc_text, desc_format)
            workbook.close()

    def replace_with_quantiles(self, numbers, lower_quantile, upper_quantile):
        '''Given a pandas series along with two numbers, replaces numbers
        below the lower quantile with the lower quantile, and replaces the
        numbers above the upper quantile with the upper quantile. Increments 
        lower and upper quantiles by .05 until negatives are removed. Final 
        lower and upper quatile values are exported to csv for reference.

        This function was made in order to prevent negative proportions. For
        the hypertensive class calculations, the data produced models where
        incidence of severe preclampsia was higher than total hypertensive
        disorders.

        TODO: Fix the actual model or implement a squeeze to prevent the 
        creation of negative values.
        '''
        self.export_negatives(numbers)
        negatives_exist = True
        try_lower = lower_quantile
        try_upper = upper_quantile
        
        while negatives_exist:
            lower = numbers.quantile(q=try_lower)
            upper = numbers.quantile(q=try_upper)
            replaced_numbers = numbers.copy()
            replaced_numbers = replaced_numbers.apply(
                lambda x: lower if x < lower else x)
            replaced_numbers = replaced_numbers.apply(
                lambda x: upper if x > upper else x)
            if (sum(replaced_numbers.apply(lambda x: x < 0)) != 0):
                print('Lower quantile tried: {}\n'.format(try_lower))
                print('Upper quantile tried: {}\n'.format(try_upper))
                try_lower += .05
                try_upper -= .05
            else:
                negatives_exist = False
        
        # export csv file
        quant_data = {
            'Year': [self.year_id],
            'Final lower quantile used': [try_lower],
            'Final upper quantile used': [try_upper]
        }
        quant_report = pd.DataFrame(data=quant_data)
        quant_report.to_csv(os.path.join(self.cluster_dir,
            'quantile_report_year{}.csv'.format(self.year_id)), index=False,
        encoding='utf-8')
        
        return replaced_numbers

    def scale_rows(self, df, scalars):
        '''Given a dataframe and a numpy array with the same index, returns
        a matrix with the rows scaled by the corresponding value in the
        numpy array
        '''
        scaled_df = df.multiply(scalars, axis=0)
        return scaled_df

    def data_rich_data_poor(self, df):
        '''Splits a given dataframe into two dataframes, based on
        data rich or data poor, and returns the two dfs. Can also use 
        get_location_metadata in place of SQL query here
        query = ('SELECT location_id, parent_id, location_set_version_id '
            'FROM shared.location_hierarchy_history '
            'WHERE location_set_version_id =('
            'SELECT location_set_version_id '
            'FROM shared.location_set_version_active '
            'WHERE location_set_id = 43 AND release_id = {})'.format(
                self.release_id))'''
                
        loc_meta = get_location_metadata(location_set_id=43, release_id = 16)
                
        #loc_df = ez.query(query=query, conn_def=self.conn_def)
        all = df.merge(loc_meta.loc[:,['location_id','parent_id']], 
            on='location_id', how='inner')
        data_rich = all.query("parent_id==44640")
        data_rich.drop('parent_id', axis=1, inplace=True)
        data_poor = all.query("parent_id==44641")
        data_poor.drop('parent_id', axis=1, inplace=True)
        
        '''
        # If the location hierarchy hasn't been updated, you might need to use
        # a flat file from COD team instead 
        data_poor_countries = pd.read_csv(('/home/j/WORK/03_cod/01_database/'
            '02_programs_nocode/demographics/star_system/'
            'countries_less_than_four_stars.csv'))
        data_poor_ids = data_poor_countries.location_id.tolist()
        loc_df = get_location_metadata(location_set_id=35, 
            gbd_round_id=self.gbd_round)
        loc_df = loc_df.loc[loc_df.most_detailed==1, ['location_id', 
            'path_to_top_parent']]
        loc_df['country_id'] = loc_df.apply(self.get_country_id, axis=1)
        loc_df.loc[~loc_df.country_id.isin(data_poor_ids),'parent_id']=44640
        loc_df.loc[loc_df.country_id.isin(data_poor_ids),'parent_id']=44641
        loc_df = loc_df[['location_id', 'parent_id']]
        
        all = df.merge(loc_df, on='location_id', how='inner')
        data_rich = all.query("parent_id==44640")
        data_rich.drop('parent_id', axis=1, inplace=True)
        data_poor = all.query("parent_id==44641")
        data_poor.drop('parent_id', axis=1, inplace=True)
        '''
        return data_rich, data_poor

    def output(self, df, output_me, measure):
        '''Outputs in the format required by save_results'''
        out_dir = '{}/{}'.format(self.cluster_dir, output_me)
        locations = df.location_id.unique()
        location = locations.astype(int)
        year = df.year_id.unique().item()
        year = int(year)
        for geo in locations:
            output = df[df.location_id == geo]
            geo_int = int(geo)  # Convert geo to integer for filename
            output.to_csv('{}/{}_{}_{}_2.csv'.format(out_dir, measure,
                                                 geo_int, year), index=False)
                                                 
###
'''Added for PPCM Dismod, 2024, same mechanics as with the the hemorrhage class. But different severity levels and duration'''
'''
# Define the parent directory path
parent_dir = '/mnt/team/rgud/pub/users/sunnypyl/for_Christian/Latin_America/'

# File names
file_names = {
    'control': 'NYHA1_baseline_draws_super_0806_linear.xlsx',
    'mild': 'NYHA2_baseline_draws_super_0806_linear.xlsx',
    'moderate': 'NYHA3_baseline_draws_super_0806_linear.xlsx',
    'severe': 'NYHA4_baseline_draws_super_0806_linear.xlsx'
}

# Dictionary to hold the NumPy arrays
data_dict = {}

for key, file_name in file_names.items():
    # Construct full file path
    file_path = os.path.join(parent_dir, file_name)
    
    # Read the Excel file into a Pandas DataFrame
    df = pd.read_excel(file_path)
    
    # Convert DataFrame to numeric only
    numeric_df = df.select_dtypes(include=[np.number])
    
    # Convert the numeric DataFrame to a NumPy array
    data_dict[key] = numeric_df.to_numpy()

# Access the arrays
control = data_dict['control']
mild = data_dict['mild']
moderate = data_dict['moderate']
severe = data_dict['severe']
'''
 
class PPCM(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, control_seq_me, mild_seq_me, mod_seq_me,
                 sev_seq_me, release_id, location_name):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id, location_name)
        self.control_seq_me = control_seq_me
        self.mild_seq_me = mild_seq_me
        self.mod_seq_me = mod_seq_me
        self.sev_seq_me = sev_seq_me
        
        print("control_seq_me:" + str(output_me)) #checking
        print("control_seq_me:" + str(control_seq_me)) #checking
        print("mild_seq_me:" + str(mild_seq_me)) #checking
        print("mod_seq_me:" + str(mod_seq_me)) #checking
        print("sev_seq_me:" + str(sev_seq_me)) #checking

    def run(self):
        # pull in incidence draws
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        ##########################
        
        # Define the parent directory path
        parent_dir = f'/mnt/team/rgud/pub/users/sunnypyl/for_Christian/postprocessing_1004update/PPCM_mrbrt_for_post_processing/{self.location_name.replace(" ", "_").replace(",","").replace("-","_")}/'
        print(parent_dir)

        # File names
        file_names = {
            'control': 'NYHA1_baseline_draws_hic_logit1004_linear.xlsx',
            'mild': 'NYHA2_baseline_draws_hic_logit1004_linear.xlsx',
            'moderate': 'NYHA3_baseline_draws_hic_logit1004_linear.xlsx',
            'severe': 'NYHA4_baseline_draws_hic_logit1004_linear.xlsx',
            'control_dur': 'duration_draws_hic_log1004_linear.xlsx',
            'mild_dur': 'duration_draws_hic_log1004_linear.xlsx',
            'moderate_dur': 'duration_draws_hic_log1004_linear.xlsx',
            'severe_dur': 'duration_draws_hic_log1004_linear.xlsx',
        }

        # Dictionary to hold the NumPy arrays
        data_dict = {}

        for key, file_name in file_names.items():
            # Construct full file path
            file_path = os.path.join(parent_dir, file_name)
    
            # Read the Excel file into a Pandas DataFrame
            df = pd.read_excel(file_path)
    
            # Convert DataFrame to numeric only
            numeric_df = df.select_dtypes(include=[np.number])
    
            # Convert the numeric DataFrame to a NumPy array
            data_dict[key] = numeric_df.to_numpy()

        # Access the arrays
        control = data_dict['control']
        mild = data_dict['mild']
        moderate = data_dict['moderate']
        severe = data_dict['severe']
        
        control_dur = data_dict['control_dur']
        mild_dur = data_dict['mild_dur']
        moderate_dur = data_dict['moderate_dur']
        severe_dur = data_dict['severe_dur']        
        ##########################
        
        # generate severity draws - PPCM: Controlled, Mild, Moderate, Severe
        control = control
        mild = mild
        moderate = moderate
        severe = severe
        # squeeze severities - PPCM, four severity squeeze.
        control, mild, moderate, severe = self.squeeze_severity_splits_PPCM(control, mild, moderate, severe)
        # generate duration draws for the four PPCM severity Duration groups.
        # control_dur = self.create_log_normal_draws(100/365, 80/365, 120/365) #365
        # mild_dur = self.create_log_normal_draws(152/365, 66/365, 238/365) #365
        # moderate_dur = self.create_log_normal_draws(729/365, 511/365, 947/365) #365
        # severe_dur = self.create_log_normal_draws(751/365, 67/365, 1435/365) #365
        # After speaking to Sunny, we are not hardcoding or generating duration draws. Sunny provided the duration file.
        control_dur = control_dur
        mild_dur = mild_dur
        moderate_dur = moderate_dur
        severe_dur = severe_dur
        
        # create moderate and severe incidence
        control_inc = self.mul_draws(new_inc, control)
        mild_inc = self.mul_draws(new_inc, mild)
        mod_inc = self.mul_draws(new_inc, moderate)
        sev_inc = self.mul_draws(new_inc, severe)
        # output moderate and severe incidence
        self.output(control_inc, control_seq_me, 6)
        self.output(mild_inc, mild_seq_me, 6)        
        self.output(mod_inc, mod_seq_me, 6)
        self.output(sev_inc, sev_seq_me, 6)
        
        # create control, mild, moderate and severe prevalence
        control_prev = self.mul_draws(control_inc, control_dur)
        mild_prev = self.mul_draws(mild_inc, mild_dur)
        mod_prev = self.mul_draws(mod_inc, moderate_dur)
        sev_prev = self.mul_draws(sev_inc, severe_dur)
        # output moderate and severe incidence and prevalence
        self.output(control_prev, control_seq_me, 5)
        self.output(mild_prev, mild_seq_me, 5)
        self.output(mod_prev, mod_seq_me, 5)
        self.output(sev_prev, sev_seq_me, 5)
###

if __name__ == "__main__":
    if len(sys.argv) < 7:
        raise Exception('''Need class_name, cluster_dir, year_id, input_MEs, 
                            output_MEs and release_id as args''')
    class_name = sys.argv[1]
    cluster_dir = sys.argv[2]
    year = int(sys.argv[3])
    input_mes = sys.argv[4].split(';')
    out_mes = sys.argv[5].split(';')
    release_id = int(sys.argv[6])

    if class_name == "PPCM":
        input_me = int(input_mes[0])
        output_me, control_seq_me, mild_seq_me, mod_seq_me, sev_seq_me = (int(out_mes[0]), int(out_mes[1]), int(out_mes[2]), int(out_mes[3]), int(out_mes[4]))
        
        for location_name in location_names:
            model = PPCM(cluster_dir, year, input_me, output_me, control_seq_me, mild_seq_me, mod_seq_me, sev_seq_me, release_id, location_name)
            model.run()
    else:
        raise ValueError('Class name must be PPCM')
        
    model.run()
