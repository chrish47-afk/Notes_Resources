# this part of the code uploads the draws to the database

from save_results import save_results_epi
import gbd.constants as gbd
import argparse
import pandas as pd
import numpy as np
import os

release_id = 33 #16, 33# CHANGE THIS MANUALLY AS NECESSARY

parser = argparse.ArgumentParser()
parser.add_argument("me_id", help="The me_id to upload", type=int)
parser.add_argument("model_version_ids", 
    help="The model versions of the input me_ids used in this custom code", 
    type=str)
parser.add_argument("out_dir", help="upload directory", type=str)
parser.add_argument("decomp_step", help="decomp step", type=str)

args = parser.parse_args()
me_id = args.me_id
model_version_ids = args.model_version_ids
print(model_version_ids)
out_dir = args.out_dir
decomp_step = args.decomp_step
#release_id = args.release_id #16, 33# CHANGE THIS MANUALLY AS NECESSARY

year_ids = yearvals = [1990, 1995, 2000, 2005, 2010, 2015, 2020, 2022, 2023, 2024] #Replaced: gbd.ESTIMATION_YEARS

if (me_id == 3620) | (me_id == 3629) | (me_id == 28393):
    measids = 6
else:
    measids = [5,6]

sexes = [2]

file_pattern = "{measure_id}_{location_id}_{year_id}_{sex_id}.csv"

# Get bundle and crosswalk version IDs
# pull in dependency map
dep_map = pd.read_csv('{}/dependency_map_gdm.csv'.format(os.getcwd())) ############Change as needed ######################################
""" create new dataframe that converts all values in the dep_map to ints and 
matches each output_me with each of its input_mes """
columns = ['input_me','output_me']
extended_map = pd.DataFrame(columns=columns)
for index, row in dep_map.iterrows():
    ins = [int(x) for x in str(row.input_me).split(';')]
    outs = [int(x) for x in str(row.output_mes).split(';')]
    df = pd.DataFrame(columns=columns, data=list(zip(np.repeat(ins, len(outs)), 
        outs*len(ins)))) 
    extended_map = extended_map.append(df, ignore_index=True)

input_me = extended_map[extended_map.output_me==me_id]['input_me'].values[0]

epi_ids = pd.read_csv('{}/epi_ids_gdm.csv'.format(os.getcwd())) ################Change as needed #############################################
#print(epi_ids)
#print("Filtered result:", epi_ids[epi_ids.me_id == input_me]['bundle_id'])
#print("Filtered result:", epi_ids[epi_ids.me_id == input_me]['bundle_id'])

#bundle_id = epi_ids[epi_ids.me_id == input_me]['bundle_id'].values[0] # Fistula, Original
#### Fistula, New
filtered_df = epi_ids[epi_ids.me_id == input_me]
print(filtered_df)
if not filtered_df.empty:
    bundle_id = filtered_df['bundle_id'].values[0]
else:
    bundle_id = 78  # or some default value, or raise an exception

#### Fistula, New

#crosswalk_version_id = epi_ids[epi_ids.me_id == input_me]['crosswalk_version_id'].values[0] # Fistula, Original
### Fistula, New
filtered_df = epi_ids[epi_ids.me_id == input_me]
if not filtered_df.empty:
    crosswalk_version_id = filtered_df['crosswalk_version_id'].values[0]
else:
    crosswalk_version_id = 17186  # or some default value, or raise an exception

### Fistula, New

print(f'Using bundle {bundle_id} and crosswalk {crosswalk_version_id} for ME {me_id}')


if me_id == 16535:
    description = "Obstetric fistula DisMod {}".format(model_version_ids)
else:
    description = ("Applied live births to incidence; applied duration to"
        " prevalence. Used the following modelable_entity_ids and"
        " model_version_ids as inputs: {} and crosswalk_version_id: {}".format(model_version_ids, crosswalk_version_id))
        
description_gdm = ("Applied duration to incidence; applied (pregnant women/female population) to"
        " prevalence. Used the following modelable_entity_ids and"
        " model_version_ids as inputs: {} and crosswalk_version_id: {}".format(model_version_ids, crosswalk_version_id))        
print(description)

model_version_df = save_results_epi(input_dir=out_dir,
                                    input_file_pattern=file_pattern,
                                    modelable_entity_id=me_id,
                                    description=description,
                                    year_id=year_ids,
                                    sex_id=sexes,
                                    mark_best=True,
                                    measure_id=measids,
                                    metric_id=3,
                                    n_draws=1000,
                                    db_env='prod',
                                    release_id = release_id, # Added this to replace gbd_round_id and decomp_step
                                    bundle_id = bundle_id,
                                    crosswalk_version_id = crosswalk_version_id)