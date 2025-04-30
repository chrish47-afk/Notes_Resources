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

##############################################################################
# Please read the readme.txt in this repo. It will explain the whole strategy
#############################################################################

class Base(object):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        '''This class incorporates all the functions that all the specific
        causes use, but all in different sequence'''
        self.cluster_dir = cluster_dir
        self.year_id = year_id
        self.input_me = input_me
        self.output_me = output_me
        self.conn_def = "cod"
        self.gbd_round = gbd.GBD_ROUND_ID
        #self.decomp_step = decomp_step
        self.release_id = release_id #16

        epi_demographics = get_demographics("epi", release_id=self.release_id)
        self.most_detailed_ages = epi_demographics['age_group_id']
        self.most_detailed_locs = epi_demographics['location_id']

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
 ##########################################################      
 ### These functions were created in 2024 for GDM    
    def get_pregnant_pop(self):
        population_group_id = 16
        pregnant_population = get_population(population_group_id = 16,
                                    location_id=self.most_detailed_locs, 
                                    sex_id=2, 
                                    age_group_id=self.most_detailed_ages,
                                    year_id=self.year_id, 
                                    status='best',
                                    release_id = self.release_id)
        pregnant_population.rename(columns={'population':'pregnant_population'}, inplace=True)
        keeps = ['location_id','year_id','age_group_id','sex_id','pregnant_population']
        pregnant_population = pregnant_population[keeps]
        '''maternal age range is 10 to 54 years old. This corresponds to 
        age_group_ids 7 to 15 (inclusive). Set all age groups outside of 
        the maternal age range to zero '''
        pregnant_population.loc[~pregnant_population.age_group_id.isin(list(range(7,16))), 'pregnant_population'] = 0.
        return pregnant_population
        
    def get_female_pop(self):
        female_population = get_population(location_id=self.most_detailed_locs,
                                    sex_id=2,
                                    age_group_id=self.most_detailed_ages,
                                    year_id=self.year_id,
                                    status='best',
                                    release_id=self.release_id)
        female_population.rename(columns={'population':'female_population'}, inplace=True)
        keeps = ['location_id','year_id','age_group_id','sex_id','female_population']
        female_population = female_population[keeps]
        '''maternal age range is 10 to 54 years old. This corresponds to 
        age_group_ids 7 to 15 (inclusive). Set all age groups outside of 
        the maternal age range to zero '''
        female_population.loc[~female_population.age_group_id.isin(list(range(7,16))), 'female_population'] = 0.
        return female_population
        
    def get_sb_ratio(self):
    # Hardcoding the file path for the CSV file
        file_path = '/mnt/team/rgud/pub/users/chrish47/updated_stillbirths_duplicates_with_2023_2024.csv'
        #"R:\team\rgud\pub\users\chrish47\updated_stillbirths_duplicates_with_2023_2024.csv"
    
        # Loading the CSV file into a pandas DataFrame
        sb_ratio = pd.read_csv(file_path)
    
        # If self.year_id is an integer, apply the filtering accordingly
        if isinstance(self.year_id, int):
            sb_ratio = sb_ratio[(sb_ratio['year_id'] == self.year_id) & 
                                (sb_ratio['location_id'].isin(self.most_detailed_locs)) & 
                                (sb_ratio['sex_id'] == 2)]
        else:
            sb_ratio = sb_ratio[(sb_ratio['year_id'].isin(self.year_id)) & 
                                (sb_ratio['location_id'].isin(self.most_detailed_locs)) & 
                                (sb_ratio['sex_id'] == 2)]    
        # Renaming 'mean_value' to 'sb_ratio' to match the output
        sb_ratio.rename(columns={"mean_value": "sb_ratio"}, inplace=True)
    
        # Keeping only the necessary columns
        keeps = ['location_id', 'year_id', 'age_group_id', 'sex_id', 'sb_ratio']
        sb_ratio = sb_ratio[keeps]
    
        # Set 'sb_ratio' to zero for age groups outside the maternal age range (10-54 years old)
        # Age group IDs 7 to 15 correspond to ages 10 to 54
        sb_ratio.loc[~sb_ratio.age_group_id.isin(list(range(7, 16))), 'sb_ratio'] = 0.0
        return sb_ratio                                
    
    def pull_draws_gdm(self, measure_id=18, age_group_list=None):
        '''Uses get_draws to pull draws of the ME for this class instance'''
        if age_group_list is None:
            age_group_list = [7,8,9,10,11,12,13,14,15]

        draws_gdm = get_draws(gbd_id_type='modelable_entity_id',
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
        draws_gdm.loc[~draws_gdm.age_group_id.isin(age_group_list), draw_cols] = 0.
        return draws_gdm

    def get_new_incident_gdm(self, draw_df, pregnant_population_df, female_population_df):
        '''Dismod models were run with live births as the denominator.
        This function reverts that, by multiplying by ASFR.'''
        keep_cols, index_cols, draw_cols = self.get_cols()
        # Make sure dataframes match in terms of indexes
        # Proportional GDM Draws
        new_draws = draw_df.copy(deep=True)
        new_draws = new_draws[keep_cols]
        # Pregnant Population
        pregnant_population_cols = list(index_cols)
        pregnant_population_cols.append('pregnant_population')
        # Female Population
        female_population_cols = list(index_cols)
        female_population_cols.append('female_population')
        # Preparing Dataframes
        new_pregnant_population = pregnant_population_df.copy(deep=True)
        new_pregnant_population = new_pregnant_population[pregnant_population_cols]
        new_female_population = female_population_df.copy(deep=True)
        new_female_population = new_female_population[female_population_cols]
        # Multiply proportion by pregnant population to get new incidence
        # Merging all three dataframes
        new_incidence = new_draws.merge(new_pregnant_population, on=index_cols, how='left')
        new_incidence = new_incidence.merge(new_female_population, on=index_cols, how='left', indicator=True)        
        assert (new_incidence._merge=='both').all()
        new_incidence.drop('_merge',axis=1, inplace=True)
        # New prevalence
        for col in draw_cols:
            new_incidence[col] = (new_incidence[col] * (new_incidence['pregnant_population'] / new_incidence['female_population']))
        # Replace any null values with zeros
        new_incidence.fillna(0, inplace=True)
        # Clean-up
        new_incidence.drop(['pregnant_population', 'female_population'], axis=1, inplace=True)
        return new_incidence

    def get_new_incident_gdm_new(self, draw_df, asfr_df, sb_ratio_df, female_population_df):
        '''Dismod models were run with live births as the denominator.
        This function reverts that, by multiplying by ASFR.'''
        keep_cols, index_cols, draw_cols = self.get_cols()
        # Make sure dataframes match in terms of indexes
        # Proportional GDM Draws
        new_draws = draw_df.copy(deep=True)
        new_draws = new_draws[keep_cols]
        # asfr
        asfr_cols = list(index_cols)
        asfr_cols.append('asfr')
        # Female Population
        female_population_cols = list(index_cols)
        female_population_cols.append('female_population')
        # sb_ratio
        sb_ratio_cols = list(index_cols)
        sb_ratio_cols.append("sb_ratio")
        # Preparing Dataframes
        new_asfr = asfr_df.copy(deep=True)
        new_asfr = new_asfr[asfr_cols]
        new_female_population = female_population_df.copy(deep=True)
        new_female_population = new_female_population[female_population_cols]
        new_sb_ratio = sb_ratio_df.copy(deep=True)
        new_sb_ratio = new_sb_ratio[sb_ratio_cols]
                
        # Merging all three dataframes
        # Merging all dataframes: female_population, asfr, sb_ratio
        new_incidence = new_draws.merge(new_female_population, on=index_cols, how='left')
        new_incidence = new_incidence.merge(new_asfr, on=index_cols, how='left', indicator=True)
        new_incidence = new_incidence.merge(new_sb_ratio, on=index_cols, how='left')
        # Ensure all rows have matching data (for both sides of the merge)
        assert (new_incidence._merge=='both').all()
        new_incidence.drop('_merge',axis=1, inplace=True)
        
        # Multiply the new_draws by the expression you provided
        for col in draw_cols:
            new_incidence[col] = new_incidence[col] * (
            (new_incidence['asfr'] * (46/52) * new_incidence['female_population'] +
            new_incidence['sb_ratio'] * new_incidence['asfr'] * new_incidence['female_population'] * (26/52))
            / new_incidence['female_population'])
            
        # Replace any null values with zeros
        new_incidence.fillna(0, inplace=True)
        
        # Clean-up
        new_incidence.drop(['female_population', 'asfr', 'sb_ratio'], axis=1, inplace=True)
        #new_draws.to_csv('/mnt/team/rgud/pub/users/chrish47/output_new_draws.csv', index=False)
        return new_incidence
        
    def div_draws(self, draw_df, other_df):
        '''Divides two sets of draws'''
        keep_cols, index_cols, draw_cols = self.get_cols()
        new_draws = draw_df.copy(deep=True)
        new_draws[draw_cols] = new_draws[draw_cols].div(other_df, axis=1)
        return new_draws
        
##########################################################                                
        

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
        
    def mul_prev_pop(self, new_prev_df, female_population_df):
        # Get columns
        keep_cols, index_cols, draw_cols = self.get_cols()
    
        # Ensure the new_prev_df only has the necessary columns
        new_prev = new_prev_df.copy(deep=True)
        new_prev = new_prev[keep_cols]
    
        # Female Population
        female_population_cols = list(index_cols)
        female_population_cols.append('female_population')
    
        # Preparing female population dataframe
        new_female_population = female_population_df.copy(deep=True)
        new_female_population = new_female_population[female_population_cols]
    
        # Merge the dataframes
        new_prevalence = new_prev.merge(new_female_population, on=index_cols, how='left', indicator=True)
    
        # Ensure all rows have matching data
        assert (new_prevalence._merge == 'both').all()
        new_prevalence.drop('_merge', axis=1, inplace=True)
    
        # Multiply new_prev by female_population
        for col in draw_cols:
            new_prevalence[col] = new_prevalence[col] * new_prevalence['female_population']
    
        # Replace any null values with zeros
        new_prevalence.fillna(0, inplace=True)
    
        # Clean-up
        new_prevalence.drop(['female_population'], axis=1, inplace=True)
    
        return new_prevalence  

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
### These functions were created in 2024 for PPCM(Old)
### Reminder: A new PPCM live birth adjustment was created at the request of the modeler.    
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
                
        loc_meta = get_location_metadata(location_set_id=43, release_id = 33)
                
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

    def output_for_epiuploader(self, df, output_me):
        '''Outputs in the format required by the epi uploader'''
        out_dir = '{}/{}'.format(self.cluster_dir, output_me)
        year = df.year_start.unique().item()
        year = int(year)
        df.to_csv('{}/sepsis_inc_infertility_{}.csv'.format(out_dir, year),
                  index=False, encoding='utf-8')

    def format_for_epi_uploader(self, df, i_cols, me_id):
        '''Takes an unindexed dataframe and returns a dataframe for epi uploader

        Args:
            df (dataframe): unindexed dataframe with draws and GBD demographics
            i_cols: a list representing an index, most of the time use
            ['location_id', 'year_id', 'age_group_id', 'sex_id']
            me_id: The modelable entity id you want to save as

        return:
            A dataframe with draws transformed to mean, upper, and lower and
            epi uploader columns added.
        '''
        epi_df = df.copy()
        epi_df.set_index(i_cols, inplace=True)
        epi_df = epi_df.transpose().describe(
            percentiles=[.025, .975]).transpose()[['mean', '2.5%', '97.5%']]
        epi_df.rename(
            columns={'2.5%': 'lower', '97.5%': 'upper'}, inplace=True)
        epi_df.index.rename(i_cols, inplace=True)
        epi_df.reset_index(inplace=True)
        # get year_start, year_end
        epi_df['year_start'] = epi_df['year_id']
        epi_df['year_end'] = epi_df['year_id']
        # get age_start and age_end
        query = "SELECT age_group_id, age_group_name FROM shared.age_group"
        age_df = ez.query(query=query, conn_def='cod')
        age_filter = [7, 8, 9, 10, 11, 12, 13, 14, 15]
        #age_filter = self.most_detailed_ages
        criterion = age_df['age_group_id'].map(lambda x: x in age_filter)
        age_df = age_df[criterion]
        age_df = age_df[age_df.age_group_name.str.contains('to')]
        age_df['age_start'], age_df['age_end'] = list(zip(
            *age_df['age_group_name'].apply(lambda x: x.split(' to ', 1))))
        age_df['age_start'] = age_df['age_start'].astype(int)
        age_df['age_end'] = age_df['age_end'].astype(int)
        epi_df = epi_df.loc[epi_df.age_group_id.isin(age_filter),:]
        epi_df = epi_df.merge(age_df, on='age_group_id', how='left')
        # get sex
        epi_df['sex'] = epi_df['sex_id'].map({2: 'Female'})
        epi_df.drop(['year_id', 'sex_id', 'age_group_id',
                     'age_group_name'], axis=1, inplace=True)
        # add other necessary cols for the epi uploader
        epi_df['modelable_entity_id'] = me_id
        query = ('SELECT modelable_entity_name FROM epi.modelable_entity '
                 'WHERE modelable_entity_id = {}'.format(me_id))
        epi_df['modelable_entity_name'] = (ez.query(query, conn_def="epi")
                                           .loc[0, 'modelable_entity_name']) #replaced .ix with .loc
        # NID placeholder
        epi_df['nid'] = 293565
        empty_cols = ['seq', 'seq_parent', 'input_type', 'underlying_nid',
                      'underlying_field_citation_value', 'location_name'
                      'field_citation_value', 'file_path', 'page_num',
                      'table_num', 'ihme_loc_id', 'smaller_site_unit',
                      'site_memo', 'age_demographer', 'standard_error',
                      'effective_sample_size', 'cases', 'sample_size',
                      'design_effect', 'measure_adjustment',
                      'recall_type_value', 'sampling_type', 'response_rate',
                      'case_name', 'case_definition', 'case_diagnostics',
                      'group', 'specificity', 'group_review', 'note_modeler',
                      'note_SR', 'extractor', 'data_sheet_filepath']
        for col in empty_cols:
            epi_df['{}'.format(col)] = np.nan
        epi_df['sex_issue'] = 0
        epi_df['year_issue'] = 0
        epi_df['age_issue'] = 0
        epi_df['measure'] = "incidence"
        epi_df['measure_issue'] = 0
        epi_df['representative_name'] = "Unknown"
        epi_df['urbanicity_type'] = "Unknown"
        epi_df['unit_type'] = "Person"
        epi_df['unit_value_as_published'] = 1
        epi_df['is_outlier'] = 0
        epi_df['recall_type'] = "Point"
        epi_df['uncertainty_type'] = "Confidence interval"
        epi_df['uncertainty_type_value'] = 95
        epi_df['source_type'] = "Mixed or estimation"
        return epi_df


class Abortion(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        # get incidence draws

    def run(self):
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # create new prevalence
        duration = self.create_draws(0.0082, 0.0055, 0.0110)
        new_prev = self.mul_draws(new_inc, duration)
        self.output(new_prev, output_me, 5)
###
'''Added for GDM, 2024'''        
'''class GDM(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        # get incidence draws
        
    def run(self):
        draws = self.pull_draws_gdm()
        #create new incidence
        pregnant_population = self.get_pregnant_pop()
        female_population = self.get_female_pop()
        new_inc = self.get_new_incident_gdm(draws, pregnant_population, female_population)
        #naming convention was changed from get_new_prevalence_gdm to get_new_incident_gdm - Math stayed the same!
        self.output(new_inc, output_me, 6)
        #create new prevalence
        duration = self.create_draws(0.306849315, 0.230136986, 0.383561644) #16*7/365, 12*7/365-20*7/365 days
        new_prev = self.mul_draws(new_inc, duration) #Multiplying with duration, instead of dividing.
        self.output(new_prev, output_me, 5)'''

class GDM(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        # get incidence draws
        
    def run(self):
        draws = self.pull_draws_gdm()
        #create new incidence
        # Asfr*46/52*pop is FOR live birth
        # Stillbirth_livebirth_ratio*asfr*pop*26/52 is FOR stillbirth
        asfr = self.get_asfr() #new
        sb_ratio = self.get_sb_ratio() #new
        female_population = self.get_female_pop()
        
        # Create new Incidence
        new_inc = self.get_new_incident_gdm_new(draws, asfr, sb_ratio, female_population)
        self.output(new_inc, output_me, 6)
        # Create new Prevalence
        duration = self.create_draws(0.306849315, 0.230136986, 0.383561644) #16*7/365, 12*7/365-20*7/365 days
        new_prev = self.mul_draws(new_inc, duration) #Multiplying with duration, instead of dividing.
        #new_prev = self.mul_prev_pop(new_prev, female_population) #Muliplying with Female Population to get Point Prevalence
        self.output(new_prev, output_me, 5)
###

# the same as abortion but copied to avoid confusion
class Eptopic(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)

    def run(self):
        # get incidence draws
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # create new prevalence
        duration = self.create_draws(0.0082, 0.0055, 0.0110)
        new_prev = self.mul_draws(new_inc, duration)
        self.output(new_prev, output_me, 5)


class Hemorrhage(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, mod_seq_me,
                 sev_seq_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        self.mod_seq_me = mod_seq_me
        self.sev_seq_me = sev_seq_me

    def run(self):
        # pull in incidence draws
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # generate severity draws
        moderate = self.create_draws(0.85, 0.80, 0.90)
        severe = self.create_draws(0.15, 0.10, 0.20)
        # squeeze severities
        moderate, severe = self.squeeze_severity_splits(moderate, severe)
        # generate duration draws
        moderate_dur = self.create_draws(7 / 365, 4 / 365, 10 / 365)
        severe_dur = self.create_draws(14 / 365, 10 / 365, 18 / 365)
        # create moderate and severe incidence
        mod_inc = self.mul_draws(new_inc, moderate)
        sev_inc = self.mul_draws(new_inc, severe)
        # output moderate and severe incidence
        self.output(mod_inc, mod_seq_me, 6)
        self.output(sev_inc, sev_seq_me, 6)
        # create moderate and severe prevalence
        mod_prev = self.mul_draws(mod_inc, moderate_dur)
        sev_prev = self.mul_draws(sev_inc, severe_dur)
        # output moderate and severe incidence and prevalence
        self.output(mod_prev, mod_seq_me, 5)
        self.output(sev_prev, sev_seq_me, 5)
        
###
'''Added for PPCM Dismod, 2024, same mechanics as with the the hemorrhage class. But different severity levels and duration''' 
class PPCM(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, control_seq_me, mild_seq_me, mod_seq_me,
                 sev_seq_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
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
        # generate severity draws - PPCM: Controlled, Mild, Moderate, Severe
        control = self.create_draws(0.10, 0.08, 0.12)
        mild = self.create_draws(0.25, 0.23, 0.27)
        moderate = self.create_draws(0.33, 0.31, 0.35)
        severe = self.create_draws(0.32, 0.30, 0.34)
        # squeeze severities - PPCM, four severity squeeze.
        control, mild, moderate, severe = self.squeeze_severity_splits_PPCM(control, mild, moderate, severe)
        # generate duration draws for the four PPCM severity Duration groups.
        control_dur = self.create_log_normal_draws(100/365, 80/365, 120/365) #365
        mild_dur = self.create_log_normal_draws(152/365, 66/365, 238/365) #365
        moderate_dur = self.create_log_normal_draws(729/365, 511/365, 947/365) #365
        severe_dur = self.create_log_normal_draws(751/365, 67/365, 1435/365) #365
        
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

class Eclampsia(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, lt_seq_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        self.lt_seq_me = lt_seq_me

    def run(self):
        # pull in incidence draws
        draws = self.pull_draws()
        # create new incidence for Eclampsia Adjusted for Live Births 3635
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # create new prevalence for for Eclampsia Adjusted for Live Births 3635
        duration = self.create_draws(0.00274, 0.00137, 0.00548)
        new_prev = self.mul_draws(new_inc, duration)
        self.output(new_prev, output_me, 5)
        # create long term sequela severity draws for data rich/data poor
        data_rich_sev = self.create_draws(0.065, 0.0606, 0.0694)
        data_poor_sev = self.create_draws(0.114, 0.108, 0.120)
        # create long term sequela, by multiplying by severity draws
        dr_inc, dp_inc = self.data_rich_data_poor(new_inc)
        dr_inc = self.mul_draws(dr_inc, data_rich_sev)
        dp_inc = self.mul_draws(dp_inc, data_poor_sev)
        lt_seq_inc = pd.concat([dr_inc, dp_inc])
        # output long term sequela for 3931
        k_cols, i_cols, d_cols = self.get_cols()
        epi_df = self.format_for_epi_uploader(lt_seq_inc, i_cols, lt_seq_me)
        self.output_for_epiuploader(epi_df, lt_seq_me)


class Hypertension(Base):
    def __init__(self, cluster_dir, year_id, input_me, sev_input_me, output_me,
                 other_seq_me, sev_seq_me, lt_seq_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        self.other_seq_me = other_seq_me
        self.sev_seq_me = sev_seq_me
        self.lt_seq_me = lt_seq_me
        self.sev_input_me = sev_input_me

    def run(self):
        # pull in incidence draws
        total_inc = self.pull_draws()
        # create new incidence for maternal htn adj for live births
        asfr = self.get_asfr()
        LBA_total_inc = self.get_new_incidence(total_inc, asfr)
        self.output(LBA_total_inc, output_me, 6)
        # create new incidence for severe preclampsia
        self.input_me = self.sev_input_me
        sev_inc = self.pull_draws()
        LBA_sev_inc = self.get_new_incidence(sev_inc, asfr)
        self.output(LBA_sev_inc, sev_seq_me, 6)
        # use means to find age, location, year specific proportions
        # and scale to find other (simple subtraction introduced negatives)
        k_cols, i_cols, d_cols = self.get_cols()
        LBA_total_inc.set_index(i_cols, inplace=True)
        LBA_sev_inc.set_index(i_cols, inplace=True)
        LBA_total_inc_means = LBA_total_inc.mean(axis=1)
        LBA_sev_inc_means = LBA_sev_inc.mean(axis=1)
        # the inclusion of most detailed age groups with some set to zero
        # creates NaNs when we divide by zero
        # assert there are no NaNs in the data set before division
        # then set all the NaNs to zero after the division
        assert LBA_total_inc_means.isnull().values.any() == False
        proportion_other = (1 - (LBA_sev_inc_means / LBA_total_inc_means))
        proportion_other.fillna(0, inplace=True)
        proportion_other = self.replace_with_quantiles(proportion_other,
                                                       0.00,
                                                       1.00) #.05,.95
        LBA_other_inc = self.scale_rows(LBA_total_inc, proportion_other)
        # squeeze sev and other to total
        LBA_sev_inc, LBA_other_inc = (
            self.squeeze_severity_splits(LBA_sev_inc,
                                         LBA_other_inc,
                                         LBA_total_inc)
        )
        LBA_sev_inc.reset_index(inplace=True)
        LBA_other_inc.reset_index(inplace=True)
        self.output(LBA_other_inc, other_seq_me, 6)
        # multiply long term seq proportion by LBA_sev_inc
        # then save for epi-uploader
        longterm_prop = self.create_draws(0.62, 0.567, 0.673)
        lt_seq_inc = self.mul_draws(LBA_sev_inc, longterm_prop)
        epi_df = self.format_for_epi_uploader(lt_seq_inc, i_cols, lt_seq_me)
        self.output_for_epiuploader(epi_df, lt_seq_me)
        # create durations to get prevelence for other and sev
        other_htn_dur = self.create_draws(3 / 12, 2 / 12, 4 / 12)
        severe_preeclampsia_dur = self.create_draws(7 / 365, 5 / 365, 10 / 365)
        LBA_other_prev = self.mul_draws(LBA_other_inc, other_htn_dur)
        LBA_sev_prev = self.mul_draws(LBA_sev_inc, severe_preeclampsia_dur)
        self.output(LBA_other_prev, other_seq_me, 5)
        self.output(LBA_sev_prev, sev_seq_me, 5)


class Obstruct(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)

    def run(self):
        # get incidence draws
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # create new prevalence
        duration = self.create_draws(0.0137, 0.0082, 0.0192)
        new_prev = self.mul_draws(new_inc, duration)
        self.output(new_prev, output_me, 5)


class Fistula(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me,
                 recto_seq_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        self.recto_seq_me = recto_seq_me

    def run(self):
        k_cols, i_cols, d_cols = self.get_cols()
        #10-95+ for fistula
        custom_age_list = [7,8,9,10,11,12,13,14,15,16,17,18,19,20,30,31,32,235]
        # get incidence draws
        inc = self.pull_draws(age_group_list=custom_age_list)
        inc = inc[k_cols]
        # get prevalence draws
        prev = self.pull_draws(measure_id=5,age_group_list=custom_age_list)
        prev = prev[k_cols]
        # create severity splits
        vesi_prop = self.create_draws(0.95, 0.90, 0.99)
        recto_prop = self.create_draws(0.05, 0.01, 0.1)
        # squeeze proportions
        vesi_prop, recto_prop = (
            self.squeeze_severity_splits(vesi_prop, recto_prop))
        # create vesicovaginal fistula incidence and prevalence
        vesi_prev = self.mul_draws(prev, vesi_prop)
        vesi_inc = self.mul_draws(inc, vesi_prop)
        self.output(vesi_prev, output_me, 5)
        self.output(vesi_inc, output_me, 6)
        # create rectovaginal fistula incidence and prevalence
        recto_prev = self.mul_draws(prev, recto_prop)
        recto_inc = self.mul_draws(inc, recto_prop)
        self.output(recto_prev, recto_seq_me, 5)
        self.output(recto_inc, recto_seq_me, 6)


class Zero_Fistula(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)

    def run(self):
        k_cols, i_cols, d_cols = self.get_cols()
        # get incidence draws
        inc = self.pull_draws(age_group_list=self.most_detailed_ages)
        inc = inc[k_cols]
        # get prevalence draws
        prev = self.pull_draws(measure_id=5, 
            age_group_list=self.most_detailed_ages)
        prev = prev[k_cols]
        #zero draws
        zero_prev = self.zero_locs(prev)
        zero_inc = self.zero_locs(inc)
    
        self.output(zero_prev, output_me, 5)
        self.output(zero_inc, output_me, 6)


class Sepsis(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me,
                 infertile_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)
        self.infertile_me = infertile_me

    def run(self):
        k_cols, i_cols, d_cols = self.get_cols()
        # get incidence draws
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # create new prevalence
        duration = self.create_draws(0.01918, 0.0137, 0.0274)
        new_prev = self.mul_draws(new_inc, duration)
        self.output(new_prev, output_me, 5)
        # create incidence of infertility & output as input data for that model
        infertility_sev = self.create_draws(0.09, 0.077, 0.104)
        infert_inc = self.mul_draws(new_inc, infertility_sev)
        # format for epi_uploader
        epi_df = self.format_for_epi_uploader(infert_inc, i_cols, infertile_me)
        self.output_for_epiuploader(epi_df, infertile_me)


class SepsisOther(Base):
    def __init__(self, cluster_dir, year_id, input_me, output_me, release_id):
        Base.__init__(self, cluster_dir, year_id, input_me, output_me, release_id)

    def run(self):
        # get incidence draws
        draws = self.pull_draws()
        # create new incidence
        asfr = self.get_asfr()
        new_inc = self.get_new_incidence(draws, asfr)
        self.output(new_inc, output_me, 6)
        # create new prevalence
        duration = self.create_draws(0.082, 0.041, 0.123)
        new_prev = self.mul_draws(new_inc, duration)
        self.output(new_prev, output_me, 5)


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
    if class_name == "Abortion":
        input_me = int(input_mes[0])
        output_me = int(out_mes[0])
        model = Abortion(cluster_dir, year, input_me, output_me, release_id)
    elif class_name == "GDM":
        input_me = int(input_mes[0])
        output_me = int(out_mes[0])
        model = GDM(cluster_dir, year, input_me, output_me, release_id)
    elif class_name == "Hemorrhage":
        input_me = int(input_mes[0])
        output_me, mod_seq_me, sev_seq_me = (int(out_mes[0]), int(out_mes[1]),
                                             int(out_mes[2]))
        model = Hemorrhage(cluster_dir, year, input_me, output_me, mod_seq_me,
                           sev_seq_me, release_id)
    elif class_name == "PPCM":
        input_me = int(input_mes[0])
        output_me, control_seq_me, mild_seq_me, mod_seq_me, sev_seq_me = (int(out_mes[0]), int(out_mes[1]), int(out_mes[2]), int(out_mes[3]), int(out_mes[4]))
        model = PPCM(cluster_dir, year, input_me, output_me, control_seq_me, mild_seq_me, mod_seq_me, sev_seq_me, release_id)
    elif class_name == "Eclampsia":
        input_me = int(input_mes[0])
        output_me, lt_seq_me = int(out_mes[0]), int(out_mes[1])
        model = Eclampsia(cluster_dir, year, input_me, output_me, lt_seq_me, release_id)
    elif class_name == "Hypertension":
        input_me = int(input_mes[0])
        sev_preclamp_me = int(input_mes[1])
        output_me, other_seq_me, sev_seq_me, lt_seq_me = (int(out_mes[0]),
                                                          int(out_mes[1]),
                                                          int(out_mes[2]),
                                                          int(out_mes[3]))
        model = Hypertension(cluster_dir, year, input_me, sev_preclamp_me,
                             output_me, other_seq_me, sev_seq_me, lt_seq_me,
                             release_id)
    elif class_name == "Obstruct":
        input_me = int(input_mes[0])
        output_me = int(out_mes[0])
        model = Obstruct(cluster_dir, year, input_me, output_me, release_id)
    elif class_name == "Zero_Fistula":
        input_me = int(input_mes[0])
        output_me = int(out_mes[0])
        model = Zero_Fistula(cluster_dir, year, input_me, output_me, release_id)
    elif class_name == "Fistula":
        input_me = int(input_mes[0])
        output_me, recto_seq_me = int(out_mes[1]), int(out_mes[0])
        model = Fistula(cluster_dir, year, input_me, output_me, recto_seq_me, release_id)
    elif class_name == "Sepsis":
        input_me = int(input_mes[0])
        output_me, infertile_me = int(out_mes[0]), int(out_mes[1])
        model = Sepsis(cluster_dir, year, input_me, output_me, infertile_me, release_id)
    elif class_name == "SepsisOther":
        input_me = int(input_mes[0])
        output_me = int(out_mes[0])
        model = SepsisOther(cluster_dir, year, input_me, output_me, release_id)
    elif class_name == "Eptopic":
        input_me = int(input_mes[0])
        output_me = int(out_mes[0])
        model  = Eptopic(cluster_dir, year, input_me, output_me, release_id)
    else:
        raise ValueError('''Must be Abortion, Hemorrhage, Eclampsia,
                Hypertension, Obstruct, Fistula, Sepsis, SepsisOther, GDM, or PPCM''')

    model.run()
