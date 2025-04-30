import os
import shutil, sys  

root = "Enter output filepath here" #Output Filepath#All Files will be copied here!#They will only get copied, not moved, so you don't have to worry corrupting any of the natives files.
my_list_of_file_paths = ["Enter your files here. If your entering multiples filepaths, please comma separate them."]

for file_path in my_list_of_file_paths:
    name = os.path.basename(file_path)
    save_to = os.path.join(root, name)
    try:
        shutil.copy(file_path, save_to)
        print("File copied successfully.","[",file_path,"]")
        # If source and destination are same
    except shutil.SameFileError:
        print("Source and destination represents the same file.","[",file_path,"]")
        # If there is any permission issue
    except PermissionError:
        print("Permission denied.","[",file_path,"]","[",file_path,"]")
        # For other errors
    except:
        print("Error occurred while copying file.","[",file_path,"]")
print('Done')